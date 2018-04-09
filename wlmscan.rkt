#lang racket
;;
;; ******************************************
;; Jeffrey C.
;; CS 241 Winter 2018
;; Assignment 06, Problem 4
;; ******************************************
;;

;; scan is the main function provided, which uses the data definitions
;; and helper functions that follow. Sample tests are at the bottom of the file.
(define (scan str)
  (scan-func str wlm-transition-lst 'start wlm-final))

;; scan-func: (listof char) trans-table symbol (listof symbol) -> (listof token)
(define (scan-func str trans start final)
  (scan-acc (string->list str) trans start final empty empty))

;; Next we specify the data definitions for tokens and the various components
;; of an FSM.
(define-struct token (kind lexeme) #:transparent)

;; A token is a (make-token k l), where k is a symbol
;;  and l is (union (list char) int).
(define-struct transition (state charset next) #:transparent)

;; A transition table is a list of transitions.
;; A transition is a (make-transition s cs ns), where s and ns are symbols,
;;  and cs is a function char->boolean that tests whether the transition applies.

;; The sample FSM provided is defined by (wlm-transition-lst, 'start, wlm-final).
;; Definitions of wlm-transition-lst and wlm-final follow.

;; Functions used in defining sample transition table:

;; Determine if character is a digit from 1-9
(define one-to-nine?
  (lambda (x) (and (char<=? #\1 x) (char<=? x #\9))))

;; Determine if character is equal to ch
(define (chartest ch)
  (lambda (x) (char=? x ch)))

;; Determine if character is alphabetic and not: i, e, w, r (the start of reserved keywords)
(define alpha/non-keyword?
  (lambda (x)
    (define keywords (list #\i #\e #\w #\r))
    (and (char-alphabetic? x) (not (member x keywords)))))

;; Determine if character is alphanumeric
(define char-alphanumeric?
  (lambda (x) (or (char-alphabetic? x) (char-numeric? x))))

;; Determine if character is alphanumeric and is not in excluded list of chars
(define (char-alphanumeric/exclude? exclude-lst)
  (lambda (x) (and (char-alphanumeric? x) (not (member x exclude-lst)))))

;; Determine if character is a compare symbbol (<, >, =, !)
(define char-cmp?
  (lambda (x) (member x (list #\< #\> #\= #\!))))

;; Determine if character is a compare symbbol (<, >, =, !) and is not in excluded list of chars
(define (char-cmp/exclude? exclude-lst)
  (lambda (x) (and (not (member x exclude-lst)) (member x (list #\< #\> #\= #\!)))))

;; WLM Transition table
(define wlm-transition-lst
  (list
   ;; Whitespace
   (make-transition 'start char-whitespace? 'whitespace)

   ;; IDs
   (make-transition 'start alpha/non-keyword? 'id)
   (make-transition 'id char-alphabetic? 'id)
   (make-transition 'id char-numeric? 'id)

   ;; Special keyword prefixes
   (make-transition 'start (chartest #\i) 'i)
   ;; int
   (make-transition 'i (chartest #\n) 'in)
   (make-transition 'in (chartest #\t) 'int)
   ;; if
   (make-transition 'i (chartest #\f) 'if)
   ;; IDs beginning with i-, if-, in-, int-
   (make-transition 'i (char-alphanumeric/exclude? (list #\n #\f)) 'id)
   (make-transition 'if char-alphanumeric? 'id)
   (make-transition 'in (char-alphanumeric/exclude? (list #\t)) 'id)
   (make-transition 'int char-alphanumeric? 'id)
   ;; else
   (make-transition 'start (chartest #\e) 'e)
   (make-transition 'e (chartest #\l) 'el)
   (make-transition 'el (chartest #\s) 'els)
   (make-transition 'els (chartest #\e) 'else)
   ;; IDs beginning with e-, el-, els-, else-
   (make-transition 'e (char-alphanumeric/exclude? (list #\l)) 'id)
   (make-transition 'el (char-alphanumeric/exclude? (list #\s)) 'id)
   (make-transition 'els (char-alphanumeric/exclude? (list #\e)) 'id)
   (make-transition 'else char-alphanumeric? 'id)
   ;; while
   (make-transition 'start (chartest #\w) 'w)
   (make-transition 'w (chartest #\h) 'wh)
   (make-transition 'wh (chartest #\i) 'whi)
   (make-transition 'whi (chartest #\l) 'whil)
   (make-transition 'whil (chartest #\e) 'while)
   ;; IDs beginning with w-, wh-, whi-, whil-, while-
   (make-transition 'w (char-alphanumeric/exclude? (list #\h)) 'id)
   (make-transition 'wh (char-alphanumeric/exclude? (list #\i)) 'id)
   (make-transition 'whi (char-alphanumeric/exclude? (list #\l)) 'id)
   (make-transition 'whil (char-alphanumeric/exclude? (list #\e)) 'id)
   (make-transition 'while char-alphanumeric? 'id)
   ;; return
   (make-transition 'start (chartest #\r) 'r)
   (make-transition 'r (chartest #\e) 're)
   (make-transition 're (chartest #\t) 'ret)
   (make-transition 'ret (chartest #\u) 'retu)
   (make-transition 'retu (chartest #\r) 'retur)
   (make-transition 'retur (chartest #\n) 'return)
   ;; IDs beginning with r-, re-, ret-, retu-, retur-, return-
   (make-transition 'r (char-alphanumeric/exclude? (list #\e)) 'id)
   (make-transition 're (char-alphanumeric/exclude? (list #\t)) 'id)
   (make-transition 'ret (char-alphanumeric/exclude? (list #\u)) 'id)
   (make-transition 'retu (char-alphanumeric/exclude? (list #\r)) 'id)
   (make-transition 'retur (char-alphanumeric/exclude? (list #\n)) 'id)
   (make-transition 'return char-alphanumeric? 'id)

   ;; Numbers
   (make-transition 'start (chartest #\0) 'zero)
   (make-transition 'start one-to-nine? 'num)
   (make-transition 'num char-numeric? 'num)
   (make-transition 'minus char-numeric? 'num)
   ;; Error states with numbers
   (make-transition 'num char-alphabetic? 'error-num-followed-by-alpha)
   (make-transition 'zero char-numeric? 'error-leading-zero)
   (make-transition 'zero char-alphabetic? 'error-num-followed-by-alpha)

   ;; Comparison transitions
   (make-transition 'start (chartest #\!) 'bang)
   (make-transition 'start (chartest #\=) 'becomes)
   (make-transition 'start (chartest #\<) 'lt)
   (make-transition 'start (chartest #\>) 'gt)
   (make-transition 'bang (chartest #\=) 'ne)
   (make-transition 'becomes (chartest #\=) 'eq)
   (make-transition 'lt (chartest #\=) 'le)
   (make-transition 'gt (chartest #\=) 'ge)
   ;; Error states with compare symbols: consecutive symbols must be separated by whitespace
   (make-transition 'eq char-cmp? 'error-missing-ws)
   (make-transition 'ne char-cmp? 'error-missing-ws)
   (make-transition 'lt (char-cmp/exclude? (list #\=)) 'error-missing-ws)
   (make-transition 'le char-cmp? 'error-missing-ws)
   (make-transition 'gt (char-cmp/exclude? (list #\=)) 'error-missing-ws)
   (make-transition 'ge char-cmp? 'error-missing-ws)
   (make-transition 'becomes (char-cmp/exclude? (list #\=)) 'error-missing-ws)

   ;; Braces and parentheses
   (make-transition 'start (chartest #\{) 'lbrace)
   (make-transition 'start (chartest #\}) 'rbrace)
   (make-transition 'start (chartest #\() 'lparen)
   (make-transition 'start (chartest #\)) 'rparen)

   ;; Operators
   (make-transition 'start (chartest #\+) 'plus)
   (make-transition 'start (chartest #\-) 'minus)
   (make-transition 'start (chartest #\*) 'star)
   (make-transition 'start (chartest #\/) 'slash)
   (make-transition 'start (chartest #\%) 'pct)

   ;; Other
   (make-transition 'start (chartest #\,) 'comma)
   (make-transition 'start (chartest #\;) 'semi)

   ;; Comments
   (make-transition 'slash (chartest #\/) 'comment)
   (make-transition 'comment (lambda (x) true) 'comment)))

;; List of final WLM states

(define wlm-final
  (list
   'id
   ; "Unfinished" keywords
   'i 'in
   'e 'el 'els
   'w 'wh 'whi 'whil
   'r 're 'ret 'retu 'retur
   'num 'zero
   'int
   'lparen 'rparen
   'lbrace 'rbrace
   'return 'if 'else 'while
   'becomes 'eq 'ne 'lt 'gt 'le 'ge
   'plus 'minus 'star 'slash 'pct
   'comma 'semi
   'error-missing-ws 'error-leading-zero 'error-num-followed-by-alpha
   'whitespace
   'comment))

;; scan-acc is the main workhorse of the lexer. It uses accumulative recursion
;; to run the FSM specified by (trans, state, final) on the list of characters cl.
;; acc accumulates the characters of the current token in reverse order, and
;; tacc accumulates the token list in reverse order.

;; Helper function for scan-acc
(define (found-trans? state ch tr)
  (and (symbol=? state (transition-state tr))
       ((transition-charset tr) ch)))

;; scan-acc: (listof char) trans-table symbol (listof symbol) (listof char) (listof token) -> (listof token)
(define (scan-acc cl trans state final acc tacc)
  (cond
    [(empty? cl)
     (if (member state final)
         (if (or (symbol=? state 'whitespace) (symbol=? state 'comment))
             (reverse tacc)
             (reverse (cons (finalize-token state (reverse acc)) tacc)))
         (error 'ERROR "unexpected end of string\n"))]
    [else
     (let ([trl (memf (lambda (x) (found-trans? state (first cl) x)) trans)])
       (cond
         [(and (boolean? trl) (member state final))
          (if (symbol=? state 'whitespace)
              (scan-acc cl trans 'start final empty tacc)
              (scan-acc cl trans 'start final empty (cons (finalize-token state (reverse acc)) tacc)))]
         [(boolean? trl)
          (error 'ERROR "left to parse:~a ~a\n" state (list->string cl))]
         [(symbol=? state 'comment)
          (reverse tacc)]
         [else
          (scan-acc (rest cl) trans (transition-next (first trl)) final (cons (first cl) acc) tacc)]))]))

;; Helper function for finalize-token to convert list to number
(define (list->number lst) (string->number (list->string lst)))

;; finalize-token symbol (listof char) -> token
(define (finalize-token state l)
  (define (unfinished-symbol? state)
    (member state (list 'i 'in 'e 'el 'els 'w 'wh 'whi 'whil 'r 're 'ret 'retu 'retur)))
  (define (throw-state-error msg)
    (error 'ERROR (string-append msg " " (~a l))))
  
  (cond
    [(symbol=? state 'num) (make-token 'num (list->number l))]
    [(or (unfinished-symbol? state) (symbol=? state 'id)) (make-token 'id (list->string l))]
    [(symbol=? state 'zero) (make-token 'num 0)]
    [(symbol=? state 'error-missing-ws)
     (throw-state-error "Expected a whitespace after compare symbol.")]
    [(symbol=? state 'error-leading-zero)
     (throw-state-error "Cannot have leading zero in multi-digit number.")]
    [(symbol=? state 'error-num-followed-by-alpha)
     (throw-state-error "Cannot have a letter following a number.")]
    [else (make-token state (list->string l))]))

;; Print token according to requirements
(define (print-token-lexeme scanned)
  (cond [(not (empty? scanned))
         (define token (first scanned))
         (define kind (token-kind token))
         (define lexeme (token-lexeme token))
         (printf "~a " (string-upcase (symbol->string kind)))
         (printf "~a~n" lexeme)
         (print-token-lexeme (rest scanned))]))

;; This file just uses scan to tokenize each line of the input
(define (scan-input)
  (define line (read-line))
  (cond
    [(eof-object? line) (void)]
    [(> (string-length line) 0) ; Ignore blank lines
     ; Ignore comment-only lines as well
     ; When a comment-only line is scanned, an empty struct is returned
     (define scanned (scan line))
     (cond
       [(empty? scanned) (scan-input)]
       [else (print-token-lexeme scanned) (scan-input)])]
    [else (scan-input)]))

(scan-input)
