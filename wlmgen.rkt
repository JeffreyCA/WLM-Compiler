#lang racket
;;
;; ******************************************
;; Jeffrey C.
;; CS 241 Winter 2018
;; Assignment 09 & 10
;; ******************************************
;;

(define terminals (list "BECOMES" "BOF" "COMMA" "ELSE" "EOF" "EQ" "GE" "GT" "ID" "IF" "INT"
                        "LBRACE" "LE" "LPAREN" "LT" "MINUS" "NE" "NUM" "PCT" "PLUS" "RBRACE"
                        "RETURN" "RPAREN" "SEMI" "SLASH" "STAR" "WHILE"))

(define non-terminals (list "start" "params" "dcl" "dcls" "expr" "factor" "pcall" "procedures"
                            "procedure" "statement" "statements" "term" "test" "start"))

; Set as constant because literal is quite long
(define proc-rule
  "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE")

;; Struct definitions
;; Parse tree node
(define-struct node (data children) #:transparent)
;; Terminal tokens
(define-struct token (kind lexeme) #:transparent)
;; Procedure arity & symbol table pair
(define-struct proc (arity table) #:transparent)

; Default procedure table with getchar and putchar
(define default-proc-tbl
  (hash-set* (make-immutable-hash)
             "getchar" (make-proc 0 (make-immutable-hash))
             "putchar" (make-proc 1 (make-immutable-hash))))

;; Helper for printing to standard error and exiting gracefully.
(define (error-end msg)
  (eprintf "ERROR ~a~n" msg)
  (exit 0))

;; Scan input until EOF
(define (scan-input)
  (define input (read-line))
  (define (process-symbols symbol-lst)
    (cond [(empty? symbol-lst) empty]
          [(not (empty? symbol-lst))
           (define symbol (first symbol-lst))
           (cons (scan-input) (process-symbols (rest symbol-lst)))]))
  (cond
    ; End of input
    [(eof-object? input) empty]
    ; Empty line, scan next line
    [(= (string-length input) 0) (scan-input)]
    ; Otherwise add individual strings in line to list
    [else
     (define split-str (string-split input))
     (if (member (first split-str) terminals)
         (make-node (make-token (first split-str) (second split-str)) empty)
         (make-node (string-join split-str) (process-symbols (rest split-str))))]))

; Helper function to get procedure id of procedure node
(define (proc-id proc-node)
  (define id-token (node-data (list-ref (node-children proc-node) 1)))
  (token-lexeme id-token))

;; Table-related Functions:
;; Determine offset of id node in symbol table
(define (find-offset/id id-node symbol-table)
  (hash-ref symbol-table (token-lexeme (node-data id-node))))

;; Determine offset of dcl node in symbol table
(define (find-offset/dcl dcl-node symbol-table)
  (define children (node-children dcl-node))
  (find-offset/id (second children) symbol-table))

;; Produce immutable procedure table mapping procedure names to arity and symbol table
(define (build-proc-tbl node table)
  ; Determine arity of procedure
  (define (arity proc-children)
    (define params-node (list-ref proc-children 3))
    (define arity (length (node-children params-node)))
    (cond [(= arity 0) 0]
          [(= arity 1) 1]
          ; 3 children because of comma token
          [(= arity 3) 2]))
  ; Recursively call build-proc-tbl on each of children
  (define (build-proc-children children table)
    (cond [(empty? children) table]
          [else (build-proc-children (rest children) (build-proc-tbl (first children) table))]))
  ; Useful constants
  (define rule (node-data node))
  (define children (node-children node))
  (cond
    ; Current node is a procedure node
    [(and (string? rule) (string=? rule proc-rule))
     ; Procedure ID name
     (define name (token-lexeme (node-data (list-ref children 1))))
     (if (hash-has-key? table name)
         ; Procedure table already contains procedure name, throw error
         (error-end (string-append "Procedure " name " was already defined"))
         ; Otherwise add procedure to procedure table
         (hash-set table name (make-proc (arity children)
                                         (build-symbol-tbl node (make-immutable-hash)))))]
    ; Current is nonterminal, interate on children
    [(and (string? rule) (or (string=? rule "procedures procedure procedures")
                             (string=? rule "start BOF procedures EOF")))
     (build-proc-children children table)]
    [else table]))

;; Produce immutable symbol table with offset values
(define (build-symbol-tbl node table)
  ; Recursively call build-symbol-tbl on each of children
  (define (build-symbol-children children table)
    (cond [(empty? children) table]
          [else (build-symbol-children (rest children) (build-symbol-tbl (first children) table))]))
  ; Determine offset value
  (define (offset-val table)
    (* (+ (hash-count table) 1) -4))
  ; Useful constants
  (define rule (node-data node))
  (define children (node-children node))
  (cond
    ; Current rule is integer declaration, add to symbol table
    [(or (and (token? rule) (string=? (token-kind rule) "dcl INT ID"))
         (and (string? rule) (string=? rule "dcl INT ID")))
     (define name (token-lexeme (node-data (list-ref children 1))))
     (if (hash-has-key? table name)
         ; Symbol already contains ID, throw error
         (error-end (string-append "Symbol table already contains " name))
         ; Otherwise table with ID added to it
         (hash-set table name (number->string (offset-val table))))]
    ; Current is nonterminal, interate on children
    [(and (string? rule) (member (first (string-split rule)) non-terminals))
     (build-symbol-children children table)]
    [else table]))

;; Checking Functions:
;; Check valid procedure call (verify arity in procedure table and no variables are overshadowed)
(define (check-valid-pcall proc-id arity symbol-table)
  ;; Verify arity of procedure matches in the procedure table. Output error if mismatch occurs.
  (define (check-arity proc-id args)
    (define proc (hash-ref proc-tbl proc-id))
    (define arity (proc-arity proc))
    (if (not (= arity args))
        (error-end (string-append "Arity mismatch, " proc-id " requires " arity
                                  " arguments, but it was supplied " args "arguments"))
        (void)))
  ;; Check for local variable overshadowing in symbol table
  (define (check-shadow proc-id symbol-table)
    (if (hash-has-key? symbol-table proc-id)
        (error-end (string-append "Procedure " proc-id " is shadowed by local variable " proc-id))
        (void)))
  (check-arity proc-id arity)
  (check-shadow proc-id symbol-table))

;; Verify if input is number, that it is within integer range.
(define (check-num-val val)
  (cond [(number? (string->number val))
         (define num-val (string->number val))
         (if (and (>= num-val 0) (<= num-val 2147483647)) val
             (error-end (string-append val " is out of range.")))]
        [(string? val) val]))

;; Check undeclared variables in all procedures
(define (check-undeclared node)
  ; Useful constants
  (define rule (node-data node))
  (define children (node-children node))
  (cond
    [(not (string? rule)) (void)]
    ; Check for undelcared variables in current proc node in its symbol table
    [(string=? rule proc-rule)
     (define symbol-table (proc-table (hash-ref proc-tbl (proc-id node))))
     (check-undeclared/proc node symbol-table)]
    ; Recursively check each procedure
    [(or (string=? rule "start BOF procedures EOF")
         (string=? rule "procedures procedure procedures"))
     (for-each (lambda (x) (check-undeclared x)) children)]
    [else (void)]))

;; Check undeclared variables in parse tree in given symbol-table
(define (check-undeclared/proc node symbol-table)
  (define rule (node-data node))
  (define children (node-children node))
  (cond
    ; Current rule is integer declaration, check in symbol-table
    [(and (string? rule) (string=? rule "factor ID"))
     (define name (token-lexeme (node-data (list-ref children 0))))
     (if (not (hash-has-key? symbol-table name))
         ; Undeclared ID, throw error
         (error-end (string-append "ID " name " was not declared"))
         (void))]

    ; Current rule is procedure call, check in procedure table
    [(and (string? rule) (string-prefix? rule "pcall"))
     (define name (token-lexeme (node-data (list-ref children 0))))
     (if (not (hash-has-key? proc-tbl name))
         ; Undeclared procedure, throw error
         (error-end (string-append "Procedure " name " was not declared"))
         (void))]
    
    ; Current is nonterminal, iterate on children
    [(and (string? rule) (member (first (string-split rule)) non-terminals))
     (for-each (lambda (x) (check-undeclared/proc x symbol-table)) children)]))

;; Check wain procedure exists and has exactly two arguments.
(define (check-valid-wain proc-tbl)
  (cond [(not (hash-has-key? proc-tbl "wain")) (error-end "wain function not defined")]
        [else
         (if (not (= (proc-arity (hash-ref proc-tbl "wain")) 2))
             (error-end "wain function should have 2 arguments")
             (void))]))

;; Code Generation Functions:
;; Push register to stack
(define (push reg)
  (list (string-append "sw $" (number->string reg) ", -4($30)")
        "sub $30, $30, $4"))

;; Pop from stack to register
(define (pop reg)
  (list "add $30, $30, $4"
        (string-append "lw $" (number->string reg) ", -4($30)")))

;; Counters to generate unique else and while labels
(define else-count -1)
(define while-count -1)

;; Get else label
(define (get-else-label start?)
  ; Return either start or end else label
  (if start? (begin (set! else-count (+ else-count 1))
                    (string-append "elsestart" (number->string else-count)))
      (string-append "elseend" (number->string else-count))))

;; Get while label
(define (get-while-label start?)
  ; Return either start or end while label
  (if start? (begin (set! while-count (+ while-count 1))
                    (string-append "whilestart" (number->string while-count)))
      (string-append "whileend" (number->string while-count))))

;; Generate prologue code
(define (gen-prologue symbol-table)
  (list "; prologue"
        "lis $11"
        ".word 1"
        "lis $4"
        ".word 4"
        "add $29, $30, $0"
        "lis $5"
        (string-append ".word " (number->string (* (hash-count symbol-table) 4)))
        "sub $30, $30, $5"
        "sw $1, -4($29) ; push a"
        "sw $2, -8($29) ; push b"
        (push 31)))

;; Generate epilogue code
(define (gen-epilogue symbol-table)
  (list "; epilogue"
        (pop 31)
        "lis $5"
        (string-append ".word " (number->string (* (hash-count symbol-table) 4)))
        "add $30, $30, $5"
        "jr $31"))

;; ASM Code for getchar
(define getchar-code
  (list "getchar:"
        "sw $2, -4($30)"
        "lis $2"
        ".word 0xffff0004"
        "lw $3, 0($2)"
        "lw $2, -4($30)"
        "jr $31"))

;; ASM Code for putchar
(define putchar-code
  (list "putchar:"
        "sw $2, -4($30)"
        "lis $2"
        ".word 0xffff000c"
        "sw $1, 0($2)"
        "lw $2, -4($30)"
        "jr $31"))

;; Generate code for entire program
(define (generate-program-code parse-tree)
  (match parse-tree
    ; Expected first line of input
    [(node "start BOF procedures EOF" (list _ procs _))
     (list "beq $0, $0, wain" ; Execute wain first
           (generate-program-code procs)
           ; Put getchar/putchar at the end
           getchar-code
           putchar-code)]
    ; Generate code for all procedures using helper
    [(node "procedures procedure procedures" (list proc procs))
     (list
      (generate-proc-code proc (proc-table (hash-ref proc-tbl (proc-id proc))))
      (generate-program-code procs))]

    [(node "procedures" empty) empty]))

;; Generate code in a particular procedure, given its symbol table
(define (generate-proc-code parse-tree symbol-table)
  (match parse-tree
    [(node "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"
           (list _ _ _ params _ _ dcls stmts _ expr _ _))
     (list
      (string-append (proc-id parse-tree) ":")
      (gen-prologue symbol-table)
      (generate-proc-code params symbol-table)
      (generate-proc-code dcls symbol-table)
      (generate-proc-code stmts symbol-table)
      (generate-proc-code expr symbol-table)
      (gen-epilogue symbol-table))]

    [(node "params" empty) empty]

    [(node "params dcl" (list dcl))
     (generate-proc-code dcl symbol-table)]
    
    [(node "params dcl COMMA dcl" (list dcl1 _ dcl2))
     (list
      (generate-proc-code dcl1 symbol-table)
      (generate-proc-code dcl2 symbol-table))]

    [(node "dcls" empty) empty]

    [(node "dcls dcls dcl BECOMES NUM SEMI" (list dcls dcl _ num _))
     (list
      (generate-proc-code dcls symbol-table)
      "lis $3"
      (string-append ".word " (token-lexeme (node-data num)))
      (string-append "sw $3, " (find-offset/dcl dcl symbol-table) "($29)"))]
    
    [(node "dcl INT ID" (list _ id)) empty]

    [(node "statements" empty) empty]
    
    [(node "statements statements statement" (list stmts stmt))
     (list (generate-proc-code stmts symbol-table)
           (generate-proc-code stmt symbol-table))]

    [(node "statement ID BECOMES expr SEMI" (list id _ expr _))
     (list
      (generate-proc-code expr symbol-table)
      (string-append "sw $3, " (find-offset/id id symbol-table) "($29)"))]
    
    [(node "statement pcall SEMI" (list pcall _))
     (generate-proc-code pcall symbol-table)]

    ; While loop
    [(node "statement WHILE LPAREN test RPAREN LBRACE statements RBRACE"
           (list _ _ test _ _ stmts _))
     (define start-label (get-while-label #t))
     (define end-label (get-while-label #f))
     (list
      (string-append start-label ":")
      (generate-proc-code test symbol-table)
      (string-append "beq $3, $0, " end-label)
      (generate-proc-code stmts symbol-table)
      (string-append "beq $0, $0, " start-label)
      (string-append end-label ":"))]

    ; If/else statement
    [(node "statement IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE"
           (list _ _ test _ _ if-stmts _ _ _ else-stmts _))
     (define start-else-label (get-else-label #t))
     (define end-else-label (get-else-label #f))
     (list
      (generate-proc-code test symbol-table)
      (string-append "beq $3, $0, " start-else-label)
      (generate-proc-code if-stmts symbol-table)
      (string-append "beq $0, $0, " end-else-label)
      (string-append start-else-label ":")
      (generate-proc-code else-stmts symbol-table)
      (string-append end-else-label ":"))]
    
    [(node "pcall ID LPAREN RPAREN" (list id _ _))
     (define proc-id (token-lexeme (node-data id)))
     (check-valid-pcall proc-id 0 symbol-table)
     (list (push 29)
           (push 31)
           "lis $31"
           (string-append ".word " proc-id)
           "jalr $31"
           (pop 31)
           (pop 29))]
    
    [(node "pcall ID LPAREN expr RPAREN" (list id _ expr _))
     (define proc-id (token-lexeme (node-data id)))
     (check-valid-pcall proc-id 1 symbol-table)
     (list (generate-proc-code expr symbol-table)
           "add $1, $3, $0"
           (push 29)
           (push 31)
           "lis $31"
           (string-append ".word " proc-id)
           "jalr $31"
           (pop 31)
           (pop 29))]

    [(node "pcall ID LPAREN expr COMMA expr RPAREN" (list id _ expr1 _ expr2 _))
     (define proc-id (token-lexeme (node-data id)))
     (check-valid-pcall proc-id 2 symbol-table)
     (list (generate-proc-code expr1 symbol-table)
           (push 3)
           (generate-proc-code expr2 symbol-table)
           (pop 1)
           "add $2, $3, $0"
           (push 29)
           (push 31)
           "lis $31"
           (string-append ".word " proc-id)
           "jalr $31"
           (pop 31)
           (pop 29))]

    [(node "expr term" (list term))
     (generate-proc-code term symbol-table)]

    [(node "expr expr PLUS term" (list expr _ term))
     (list (generate-proc-code expr symbol-table)
           (push 3)
           (generate-proc-code term symbol-table)
           (pop 5)
           "add $3, $5, $3")]

    [(node "expr expr MINUS term" (list expr _ term))
     (list (generate-proc-code expr symbol-table)
           (push 3)
           (generate-proc-code term symbol-table)
           (pop 5)
           "sub $3, $5, $3")]

    [(node "term term STAR factor" (list term _ factor))
     (list (generate-proc-code term symbol-table)
           (push 3)
           (generate-proc-code factor symbol-table)
           (pop 5)
           "mult $5, $3"
           "mflo $3")]

    [(node "term term SLASH factor" (list term _ factor))
     (list (generate-proc-code term symbol-table)
           (push 3)
           (generate-proc-code factor symbol-table)
           (pop 5)
           "div $5, $3"
           "mflo $3")]

    [(node "term term PCT factor" (list term _ factor))
     (list (generate-proc-code term symbol-table)
           (push 3)
           (generate-proc-code factor symbol-table)
           (pop 5)
           "div $5, $3"
           "mfhi $3")]

    [(node "term factor" (list factor))
     (generate-proc-code factor symbol-table)]
    
    [(node "factor LPAREN expr RPAREN" (list _ expr _))
     (generate-proc-code expr symbol-table)]

    [(node "factor pcall" (list pcall))
     (generate-proc-code pcall symbol-table)]
    
    [(node "factor NUM" (list num))
     (list "lis $3"
           (string-append ".word " (generate-proc-code num symbol-table)))]
    
    [(node "factor ID" (list id))
     (list (string-append "lw $3, " (hash-ref symbol-table (token-lexeme (node-data id))) "($29)"))]

    ; expr 1 != expr 2
    [(node "test expr NE expr" (list expr1 _ expr2))
     (list
      (generate-proc-code expr1 symbol-table)
      (push 3)
      (generate-proc-code expr2 symbol-table)
      (pop 5)
      "slt $6, $3, $5"
      "slt $7, $5, $3"
      "add $3, $6, $7")]

    ; expr 1 == expr 2
    [(node "test expr EQ expr" (list expr1 _ expr2))
     (list
      (generate-proc-code expr1 symbol-table)
      (push 3)
      (generate-proc-code expr2 symbol-table)
      (pop 5)
      "slt $6, $3, $5"
      "slt $7, $5, $3"
      "add $3, $6, $7"
      "sub $3, $11, $3")]

    ; expr 1 < expr 2
    [(node "test expr LT expr" (list expr1 _ expr2))
     (list
      (generate-proc-code expr1 symbol-table)
      (push 3)
      (generate-proc-code expr2 symbol-table)
      (pop 5)
      "slt $3, $5, $3")]

    ; expr 1 <= expr 2
    [(node "test expr LE expr" (list expr1 _ expr2))
     (list
      (generate-proc-code expr1 symbol-table)
      (push 3)
      (generate-proc-code expr2 symbol-table)
      (pop 5)
      "slt $3, $3, $5"
      "sub $3, $11, $3")]

    ; expr 1 > expr 2
    [(node "test expr GT expr" (list expr1 _ expr2))
     (list
      (generate-proc-code expr1 symbol-table)
      (push 3)
      (generate-proc-code expr2 symbol-table)
      (pop 5)
      "slt $3, $3, $5")]

    ; expr 1 >= expr 2
    [(node "test expr GE expr" (list expr1 _ expr2))
     (list
      (generate-proc-code expr1 symbol-table)
      (push 3)
      (generate-proc-code expr2 symbol-table)
      (pop 5)
      "slt $3, $5, $3"
      "sub $3, $11, $3")]

    ; Node is a token, output its value
    [(node (token id val) empty)
     (check-num-val val)]
    
    ; For any other case, call generate on its child
    [(node _ (list child)) (generate-proc-code child symbol-table)]))

;; Output code (already flattened)
(define (print-code code)
  (cond [(empty? code)]
        [else (printf "~a~n" (first code)) 
              (print-code (rest code))]))

;; Read input into parse tree
(define tree (scan-input))
; Generate procedure table from parse tree
(define proc-tbl (build-proc-tbl tree default-proc-tbl))
; Check wain is present in table
(void (check-valid-wain proc-tbl))
; Check for any undeclared variables or undefined procedures
(void (check-undeclared tree))
; Generate code from parse tree
(define code (generate-program-code tree))
; Output unflattened code
(void (print-code (flatten code)))
