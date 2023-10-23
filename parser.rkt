#lang racket
;Jacob Smith
;Username: jdsp4k
;Domain: UMKC
;Functional Programming - Let's Build a Parser
;10/22/2023
(require data/either)

;--BEGIN SCANNER--
;string->either(int, 'EOF)
;Takes a string and returns the location of the next delimiter character or whitespace (if count-whitespace is #t).
(define (get-next-delimiter-loc string [delim-list '(#\; #\: #\( #\) #\> #\< #\= #\+ #\- #\* #\/)] [loc 0] [count-whitespace? #t])
  (define (map-char char)
    (char=? (first (string->list string)) char))
  (if (empty? (string->list string))
      (failure 'EOF)
      (if
       (or
        (ormap map-char delim-list)
        (and
         (char-whitespace? (first (string->list string)))
         count-whitespace?))
       (success loc)
       (get-next-delimiter-loc (list->string (rest (string->list string))) delim-list (+ loc 1)))))

;string->bool
;Takes a string and returns true if it is "useless" ie is empty, or only whitespace, but not a newline.
(define (is-useless-string? string)
  (define (mapable-char=? char [target-char #\newline])
    (char=? char target-char))
  (or
   (empty? (string->list string))
   (and
    (ormap char-whitespace? (string->list string))
    (not (ormap mapable-char=? (string->list string))))))

;string->bool
;Inverts is-useless-string for filtering.
(define (is-not-useless-string? string)
  (not (is-useless-string? string)))

;string->list(string)
;Takes a string and returns a list of tokens.
(define (get-token-list string [token-list '()])
  (if (empty? (string->list string))
      (filter is-not-useless-string? token-list)
      (let ([token (get-next-delimiter-loc string)])
        (if (failure? token)
            (filter is-not-useless-string? (append token-list (list string)))
            (if (= (from-either token) 0)
                (get-token-list (substring string 1) (append token-list (list (substring string 0 1))))
                (get-token-list (substring string (from-either token)) (append token-list (list (substring string 0 (from-either token))))))))))

;--END SCANNER--
;--BEGIN LEXER--
;string->bool
;Takes a string and checks if it is a valid id.
(define (check-id string [first-char? #t])
  (if first-char?
      (and
       (char-alphabetic? (first (string->list string)))
       (or
        (empty? (rest (string->list string)))
        (check-id (list->string (rest (string->list string))) #f)))
      (and
       (or
        (char-alphabetic? (first (string->list string)))
        (char-numeric? (first (string->list string))))
       (or
        (empty? (rest (string->list string)))
        (check-id (list->string (rest (string->list string))) #f)))))

;string->bool
;Takes a string and checks if it is a valid num.
(define (check-num string [first-char? #t])
  (if first-char?
      (or
       (and
        (char=? #\0 (first (string->list string)))
        (empty? (rest (string->list string))))
       (and
        (not (char=? #\0 (first (string->list string))))
        (char-numeric? (first (string->list string)))
        (or
         (empty? (rest (string->list string)))
         (check-num (list->string (rest (string->list string))) #f))))
      (and
       (char-numeric? (first (string->list string)))
       (or
        (empty? (rest (string->list string)))
        (check-num (list->string (rest (string->list string))) #f)))))

;string->either((terminal, string), fail)
;Takes a string representing a token and returns a datum which matches if one exists.
(define (check-token string target-result-pair [fail #f] [word-chk string=?])
  (if (word-chk string (car target-result-pair))
      (success (cons (first (cdr target-result-pair)) string))
      (failure fail)))

;string->either((datum, string), string)
;Takes a token and appends the approprate tag to it, or fails if the token can't be matched.
(define (lex token [id-proc check-id] [num-proc check-num] [pairs '(("\n" newline)
                                                                        (":" colon)
                                                                        ("+" plus-sign)
                                                                        ("=" equal-sign)
                                                                        (";" semicolon)
                                                                        ("if" if)
                                                                        ("(" left-para)
                                                                        (")" right-para)
                                                                        ("while" while)
                                                                        ("endwhile" endwhile)
                                                                        ("read" read)
                                                                        ("write" write)
                                                                        ("goto" goto)
                                                                        ("gosub" gosub)
                                                                        ("return" return)
                                                                        ("break" break)
                                                                        ("end" end)
                                                                        ("true" true)
                                                                        ("false" false)
                                                                        ("<" lt-sign)
                                                                        (">" gt-sign)
                                                                        ("-" minus-sign)
                                                                        ("*" multiplication-sign)
                                                                        ("/" division-sign)
                                                                        ("$$" eof))])
  (if (empty? pairs)
      (if (id-proc token)
          (success (cons 'id token))
          (if (num-proc token)
              (success (cons 'digit token))
              (failure (cons "Unexpected token:" token))))
      (let ([lexeme (check-token token (first pairs))])
        (if (failure? lexeme)
            (lex token id-proc num-proc (rest pairs))
            lexeme))))

;token-list->int
;When the lexer fails, it will return the token it couldn't understand.
(define (get-current-line-number token-list [loc 1])
  (if (empty? token-list)
      loc
      (if (eq? (car (first token-list)) 'newline)
          (get-current-line-number (rest token-list) (+ loc 1))
          (get-current-line-number (rest token-list) loc))))

;token-list->lex-list
;Entry point for the lexer.
(define (lex-tokens token-list [lex-list '()])
  (if (empty? token-list)
      (success lex-list)
      (let ([lexed-token (lex (first token-list))])
        (if (failure? lexed-token)
            (failure (cons (get-current-line-number lex-list) (from-either lexed-token)))
            (lex-tokens (rest token-list) (append lex-list (list (from-either lexed-token))))))))

;--END LEXER--
;--BEGIN PARSER--
;proc-list->either
;Poorly named function which if meant to short-circuit when one of the procs in proc-list reutrns a
;success. It doesn't appear to work for reasons I can't understand.
(define (inv-chain proc-list [prev-func 'none])
  (cond
    [(empty? proc-list)
     (failure prev-func)]
    [else (let ([result (first proc-list)])
       (cond
         [(success? result)
          result]
         [else
          (inv-chain (rest proc-list) (first proc-list))]))]))

;token-list->token-list
;Helper function to get the first of this list ignoring the newlines.
(define (first-ignore-newline token-list)
  (if (eq? (car (first token-list)) 'newline)
      (first-ignore-newline (rest token-list))
      (first token-list)))

;token-list->token-list
;Helper function to get the rest of this list ignoring the newlines.
(define (rest-ignore-newline token-list)
  (if (eq? (car (first token-list)) 'newline)
      (rest-ignore-newline (rest token-list))
      (rest token-list)))

;result-tree, any, token-list->pair(result-tree, token-list)
;Just a helper function so my success datastructure stays consistent. Success-lex does nothing.
(define (build-success result-tree success-lex success-list)
  (cons (cons result-tree (first-ignore-newline success-list)) (rest-ignore-newline success-list)))

;token-list, result-tree, datum->either
;Checks if a terminal lexeme exists.
(define (terminal-lex? token-list result-tree lex)
  (cond
   [(eq? (car (first-ignore-newline token-list)) lex)
    (display (string-join (list "Found: " (symbol->string lex) " " (cdr (first-ignore-newline token-list)) (string #\newline)) "")) 
    (success (build-success result-tree lex token-list))]
   [else
    (display (string-join (list "Failed: Looking for " (symbol->string lex) ", found " (symbol->string (car (first-ignore-newline token-list))) " " (cdr (first-ignore-newline token-list)) (string #\newline)) ""))
    (failure (build-success result-tree #f token-list))]))

;token-list, result-tree, list of values to test against->either
;Entry point for the test-flag function. Checks to make sure that the right number of flags are there and adds them if needed.
(define (test token-list result-tree test-list [eps-flag-list '()])
  (define (get-false dummy-arg)
    #f)
  (cond
    [(= (length test-list) (length eps-flag-list)) (test-flag token-list result-tree test-list eps-flag-list)]
    [(empty? eps-flag-list) (test-flag token-list result-tree test-list (build-list (length test-list) get-false))]
    [else (error "Length mismatch in test-list and eps-flag-list")]))

;token-list, result-tree, list of values to test against, list indicating wether eps is a valid result for the matching value->either
;This function does the heavy lifting. It is passed a list of data or a proc datum pair. If a pair is found it runs the corresponding
;proc, and recieves a success or failure. If it is a success, the function recursively calls to check the next entry. If it is a failure
;The function will check if eps is a valid value, given the eps-flag-list. If so it will continue on to the next entry, otherwise it
;returns a failure. And yes I did realize halfway through writing this that I could've just made the grammar a data structure. Too late now.
(define (test-flag token-list result-tree test-list eps-flag-list)
  (cond
    [(empty? token-list) (success (cons result-tree token-list))]
    [(empty? test-list) (success (cons result-tree token-list))]
    [(cond
       [(pair? (first test-list))
        (let ([result ((car (first test-list)) token-list result-tree)])
          (cond
            [(and (failure? result) (not (first eps-flag-list)))
             (display (string-join (list "Critical Failure: Missing" (symbol->string (cdr (first test-list))) "\n")))
             (failure (build-success result-tree #f token-list))]
            [(and (failure? result) (first eps-flag-list))
             (display (string-join (list "Non-critical Failure: Missing" (symbol->string (cdr (first test-list))) "\n")))
             (test-flag (cdr (from-either result)) (car (from-either result)) (rest test-list) (rest eps-flag-list))]
            [(success? result) (test-flag (cdr (from-either result)) (car (from-either result)) (rest test-list) (rest eps-flag-list))]))]
       [else (let ([result (terminal-lex? token-list result-tree (first test-list))])
               (cond
                 [(and (failure? result) (not (first eps-flag-list)))
                  (display (string-join (list "Critical Failure: Missing" (symbol->string (first test-list)) "\n")))
                  (failure (build-success result-tree #f token-list))]
                 [(and (failure? result) (first eps-flag-list))
                  (display (string-join (list "Non-critical Failure: Missing" (symbol->string (first test-list)) "\n")))
                  (test-flag (cdr (from-either result)) (car (from-either result)) (rest test-list) (rest eps-flag-list))]
                 [(success? result) (test-flag (cdr (from-either result)) (car (from-either result)) (rest test-list) (rest eps-flag-list))]))])]))

;token-list, result-tree->either
;Looks for a num.
(define (num? token-list result-tree)
  (display "Testing: num?\n")
  (define (this-test test-list [eps-flag-list '()])
    (test token-list result-tree test-list eps-flag-list))
  (let ([res (inv-chain (list
                         (this-test (list 'plus-sign 'digit) (list #t #f))
                         (this-test (list 'minus-sign 'digit) (list #t #f))
                         (this-test (list 'digit) (list #f))))])
    (cond
      [(success? res) (display "Found num\n") res]
      [else (display "Failed num\n") (failure (cons result-tree token-list))])))
  
;token-list, result-tree->either
;Looks for an etail
(define (etail? token-list result-tree)
  (display "Testing: etail?\n")
  (define (this-test test-list [eps-flag-list '()])
    (test token-list result-tree test-list eps-flag-list))
  (let ([res (inv-chain (list
                         (this-test (list 'plus-sign (cons expr? 'expr)))
                         (this-test (list 'minus-sign (cons expr? 'expr)))
                         (this-test (list 'multiplication-sign (cons expr? 'expr)))
                         (this-test (list 'division-sign (cons expr? 'expr)))))])
        (cond
          [(success? res)(display "Found etail\n") res]
          [else (display "Failed etail\n") (failure (cons result-tree token-list))])))

;token-list, result-tree->either
;Looks for an expr.
(define (expr? token-list result-tree)
  (display "Testing: expr?\n")
  (define (this-test test-list [eps-flag-list '()])
    (test token-list result-tree test-list eps-flag-list))
  (let ([res (inv-chain (list 
                         (this-test (list 'id (cons etail? 'etail)) (list #f #t))
                         (this-test (list (cons num? 'num) (cons etail? 'etail)) (list #f #t))
                         (this-test (list 'left-para (cons expr? 'expr) 'right-para))))])
        (cond
          [(success? res) (display "Found expr\n") res]
          [else (display "Failed expr\n") (failure (cons result-tree token-list))])))

;token-list, result-tree->either
;Looks for a bool-op.
(define (bool-op? token-list result-tree)
  (display "Testing: bool-op?\n")
  (define (this-test test-list [eps-flag-list '()])
    (test token-list result-tree test-list eps-flag-list))
  (let ([res (inv-chain (list 
                         (this-test (list 'lt-sign))
                         (this-test (list 'gt-sign))
                         (this-test (list 'gt-sign 'eq-sign))
                         (this-test (list 'lt-sign 'eq-sign))
                         (this-test (list 'lt-sign 'gt-sign))
                         (this-test (list 'eq-sign))))])
        (cond
          [(success? res) (display "Found bool-op\n") res]
          [else (display "Failed bool-op\n") (failure (cons result-tree token-list))])))

;token-list, result-tree->either
;Looks for a boolean.
(define (boolean? token-list result-tree)
  (display "Testing: boolean\n")
  (define (this-test test-list [eps-flag-list '()])
    (test token-list result-tree test-list eps-flag-list))
  (let ([res (inv-chain (list
                         (this-test (list 'true))
                         (this-test (list 'false))
                         (this-test (list (cons expr? 'expr) (cons bool-op? 'bool-op) (cons expr? 'expr)))))])
    (cond
      [(success? res) (display "Found boolean\n") res]
      [else (display "Failed boolean\n") (failure (cons result-tree token-list))])))

;token-list, result-tree->either
;Looks for a statement
(define (stmt? token-list result-tree)
  (display "Testing: stmt\n")
  (define (this-test test-list [eps-flag-list '()])
    (test token-list result-tree test-list eps-flag-list))
  (let ([res (inv-chain (list
                         (this-test (list 'id 'equal-sign (cons expr? 'expr) 'semicolon))
                         (this-test (list 'if 'left-para (cons boolean? 'boolean) 'right-para (cons stmt? 'stmt) 'semicolon))
                         (this-test (list 'while 'left-para (cons boolean? 'boolean) 'right-para (cons linelist? 'linelist) 'endwhile 'semicolon) (list #f #f #f #f #t #f #f))
                         (this-test (list 'read 'id 'semicolon))
                         (this-test (list 'write (cons expr? 'expr) 'semicolon))
                         (this-test (list 'goto 'id 'semicolon))
                         (this-test (list 'gosub 'id 'semicolon))
                         (this-test (list 'return 'semicolon))
                         (this-test (list 'break 'semicolon))
                         (this-test (list 'end 'semicolon))))])
    (cond
      [(success? res) (display "Found stmt\n") res]
      [else (display "Failed stmt\n")(failure (cons result-tree token-list))])))
      
;token-list, result-tree->either
;Looks for a linetail
(define (linetail? token-list result-tree)
  (display "Testing: linetail?\n")
  (define (this-test test-list [eps-flag-list '()])
    (test token-list result-tree test-list eps-flag-list))
  (let ([res (inv-chain (list
                         (this-test (list (cons stmt? 'stmt)))))])
       (cond
         [(success? res) (display "Found linetail\n") res]
         [else (display "Failed linetail\n") (failure (cons result-tree token-list))])))

;token-list, result-tree->either
;Looks for a label.
(define (label? token-list result-tree)
  (display "Testing: label?\n")
  (define (this-test test-list [eps-flag-list '()])
    (test token-list result-tree test-list eps-flag-list))
  (let ([res (inv-chain (list
                         (this-test (list 'id 'colon))))])
    (cond
      [(success? res) (display "Found label\n") res]
      [else (display "Failed label\n") (failure (cons result-tree token-list))])))

;token-list, result-tree->either
;Looks for a line.
(define (line? token-list result-tree)
  (display "Testing: line?\n")
  (define (this-test test-list [eps-flag-list '()])
    (test token-list result-tree test-list eps-flag-list))
  (let ([res (inv-chain (list
                         (this-test (list (cons label? 'label) (cons stmt? 'stmt) (cons linetail? 'linetail)) (list #t #f #t))))])
    (cond
     [(success? res) (display "Found line\n") res]
     [else (display "Failed line\n") (failure (cons result-tree token-list))])))

;token-list, result-tree->either
;Looks for a linelist.
(define (linelist? token-list result-tree)
  (display "Testing: linelist?\n")
  (define (this-test test-list [eps-flag-list '()])
    (test token-list result-tree test-list eps-flag-list))
  (let ([res (inv-chain (list
                        (this-test (list (cons line? 'line) (cons linelist? 'linelist)) (list #f #t))))])
    (cond
     [(success? res) (display "Found linelist\n") res]
     [else (display "Failed linelist\n") (failure (cons result-tree token-list))])))

;either(token-list, unidentified lexeme)->either(big ol' structure)
;Entrypont. Looks for a program.
(define (program? token-list-either [result-tree (cons 'program 'null)])
  (display "Testing: Program?\n")
  (define (this-test test-list [eps-flag-list '()])
        (test (from-either token-list-either) result-tree test-list eps-flag-list))
  (cond
    [(failure? token-list-either) (display "Lex Error\n") token-list-either]
    [else
     (let ([res (inv-chain (list
                             (this-test (list (cons linelist? 'linelist) 'eof) (list #t #f))))])
       res)]))

;--END PARSER--
;--UTILITIES--
;list->int
;Counts the number of newlines in the list
(define (count-nl lex-list)
  (define (nl? lex)
    (eq? (car lex) 'newline))
  (length (filter nl? lex-list)))

;list, list-> int
;Subtracts the number of newlines left in the unconsumed queue from the total number of newlines.
(define (get-line lex-list result)
  (- (count-nl lex-list) (count-nl (cdr result))))

;--END UTILITIES--
;--MAIN PROGRAM--
;string->void
;Parse a file. Debug flag #t turns on debug.
(define (parse filename [debug-flag #f])
  (let ([out (current-output-port)])                                           ;Save the reference to the default terminal so we can turn it back on later
    (cond
      [(not debug-flag) (current-output-port (open-output-string))])           ;Set the defualt output to a dummy output port.
    (let* ([tokens (get-token-list (file->string filename #:mode 'text))]
           [lex (lex-tokens tokens)]
           [parse (program? lex)])
      (cond
        [(failure? lex) (current-output-port out) (display (string-join (list "Rejected\nInvalid token: Line" (number->string (car (from-either lex))) (cdr (cdr (from-either lex))))))]
        [(and (success? parse) (current-output-port out) (empty? (cdr (from-either parse)))) (display "Accepted\n")]
        [(success? parse) (current-output-port out) (display (string-join (list "Syntax Error: Line" (number->string (get-line (from-either lex) (from-either (from-either parse)))) "\nTokens after EOF\n")))]
        [else (current-output-port out) (display "Rejected\n") (display (string-join (list "Syntax Error: Line" (number->string (get-line (from-either lex) (from-either (from-either parse)))) "\n")))]))))

;--END MAIN PROGRAM--