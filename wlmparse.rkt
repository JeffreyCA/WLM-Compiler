#lang racket
;;
;; ******************************************
;; Jeffrey C.
;; CS 241 Winter 2018
;; Assignment 08, Problem 5
;; ******************************************
;;

;; Read start symbol
(define start-symbol "start")
(define rules (file->lines "rules.txt"))
(define transitions (file->lines "transitions.txt"))

;; Set initial state to 0
(define initial-state "0")

;; Struct definitions
(define-struct node (data children) #:transparent)
(define-struct token (kind lexeme) #:transparent)

;; Scan input until EOF
(define (scan-input)
  (define input (read-line)) 
  (cond
    ; End of input
    [(eof-object? input) empty]
    ; Empty line, scan next line
    [(= (string-length input) 0) (scan-input)]
    ; Otherwise add individual strings in line to list
    [else
     (define split-str (string-split input))
     (if (= (length split-str) 2) (append (list (make-token (first split-str) (second split-str)))
                                          (scan-input))
         (append split-str (scan-input)))]))

;; goto state symbol -> state or #f if does not exist
(define (goto state symbol)
  (define (goto/helper state symbol transitions)
    (cond [(empty? transitions) #f]
          [(string-prefix? (first transitions) (string-join (list state symbol "shift")))
           (last (string-split (first transitions)))]
          [else (goto/helper state symbol (rest transitions))]))
  (if (token? symbol) (goto/helper state (token-kind symbol) transitions)
      (goto/helper state symbol transitions)))

;; reduce state symbol -> rule or #f if does not exist
(define (reduce state symbol)
  (define (reduced-state/fn state symbol transitions)
    (cond [(empty? transitions) #f]
          [(string-prefix? (first transitions) (string-join (list state symbol "reduce")))
           (string->number (last (string-split (first transitions))))]
          [else (reduced-state/fn state symbol (rest transitions))]))
  (define reduced-state (reduced-state/fn state (token-kind symbol) transitions))
  (if (false? reduced-state) #f (list-ref rules reduced-state)))

;; find-start-transition returns the starting transition of the LR(1).
(define (find-start-transition)
  (define (find-start-transition/rec rules)
    (cond [(empty? rules) #f]
          [(string-prefix? (first rules) start-symbol) (first rules)]
          [else (find-start-transition/rec (rest rules))]))
  (find-start-transition/rec rules))

;; reduce-loop represents the inner "while reduce" in pseudocode, reduces stack until no more rule
;; to reduce to.
(define (reduce-loop state-stack tree-stack c)
  (define reduced-rule (reduce (last state-stack) c))
  (cond [(string? reduced-rule)
         (define alpha-size (- (length (string-split reduced-rule)) 1))
         ; Stack with last 'alpha-size' popped
         (define popped-stack (take state-stack (- (length state-stack) alpha-size)))
         ; Value produced by goto
         (define goto-val (goto (last popped-stack) (first (string-split reduced-rule))))
         ; Stack with last 'alpha-size' popped and value of goto pushed
         (define pushed-stack (append popped-stack (list goto-val)))
         ; Items to pop from tree stack
         (define popped-tree-items (list-tail tree-stack (- (length tree-stack) alpha-size)))
         ; Result of popped tree stack
         (define popped-tree-stack (take tree-stack (- (length tree-stack) alpha-size)))
         ; Result of pushing 'A' to tree stack
         (define pushed-tree-stack
           (append popped-tree-stack (list (make-node reduced-rule popped-tree-items))))
         ; Continue to reduce
         (reduce-loop pushed-stack pushed-tree-stack c)]
        [else (cons state-stack tree-stack)]))

;; Parse input
(define (parse-input input state-stack tree-stack counter)
  (cond [(empty? input) tree-stack]
        [else
         ; Input word to be processed
         (define c (first input))
         ; Resulting stacks after reduction
         (define result-stack-pair (reduce-loop state-stack tree-stack c))
         ; Result is a pair where first stack is the state stack and second is tree stack
         (define result-state-stack (car result-stack-pair))
         (define result-tree-stack (cdr result-stack-pair))
         ; Result of Goto
         (define goto-result (goto (last result-state-stack) c))
         ; New stack to iterate on
         (define pushed-state-stack (append result-state-stack (list (goto (last result-state-stack) c))))
         (define pushed-tree-stack (append result-tree-stack (list (make-node c empty))))
         ; Output appropriate error message if invalid input
         (if (false? goto-result)
             ; Output error and return empty
             (begin (fprintf (current-error-port)
                             (string-append "ERROR at " (number->string (+ counter 1)) "\n")) empty)
             ; Otherwise continue parsing rest of input
             (parse-input (rest input) pushed-state-stack pushed-tree-stack (+ counter 1)))]))

;; Helper function to print token kind and lexeme
(define (print-token token)
  (printf "~a ~a~n" (token-kind token) (token-lexeme token)))

;; Print parse tree using preorder traversal
(define (preorder parse-tree)
  (define data (node-data parse-tree))
  (if (token? data) (print-token data) (printf "~a~n" (node-data parse-tree)))
  (cond [(not (empty? (node-children parse-tree)))
         ; Traverse each child
         (for-each (lambda (child) (preorder child)) (node-children parse-tree))]))

;; Helper function to output tree list
(define (print-tree-lst tree-lst)
  (cond [(empty? tree-lst) (void)]
        [else (define node (first tree-lst))
              (if (empty? (node-children node)) (print-token (node-data node)) (preorder node))
              (print-tree-lst (rest tree-lst))]))

;; Output left-most derivation from tree stack (which contains BOF, parse tree, EOF).
;; Does not output anything if input empty (error encountered)
(define (print-output tree-stack)
  (if (not (empty? tree-stack))
      (begin (printf "~a~n" (find-start-transition))
             (print-tree-lst tree-stack))
      (void)))

;; List of input strings to parse
(define input-lst (scan-input))
;; List of input followed by EOF
(define padded-lst (append input-lst (list (make-token "EOF" "EOF"))))

;; Get tree stack after running on input
(define tree-stack
  (parse-input padded-lst
               (list (goto initial-state (make-token "BOF" "BOF")))
               (list (make-node (make-token "BOF" "BOF") empty)) 0))

;; Output left-most derivation of tree
(print-output tree-stack)
