#lang racket/base
#|
(require
  "../main.rkt"
  (for-syntax racket/base syntax/parse))

(module lexer-tokens racket/base
  (require racket/string)

  (provide (struct-out keyword-token)
           python-keyword-list)

  (struct keyword-token [name] #:transparent)

  (define python-keyword-list
    (string-split
      "
      False      await      else       import     pass
      None       break      except     in         raise
      True       class      finally    is         return
      and        continue   for        lambda     try
      as         def        from       nonlocal   while
      assert     del        global     not        with
      async      elif       if         or         yield
      +       -       **      *       /       //      %      @
      <<      >>      &       |       ^       ~
      <       >       <=      >=      ==      !=
      (       )       [       ]       {       }
      ,       :       .       ;       @       =       ->
      +=      -=      *=      /=      //=     %=      @=
      &=      |=      ^=      >>=     <<=     **=
      ")))

(require
  'lexer-tokens
  (for-syntax 'lexer-tokens))

(define (keyword expected-name)
  (lambda (t)
    (values
      (and (keyword-token? t) (equal? (keyword-token-name t) expected-name) expected-name)
      #f)))

(define-syntax #%peg-datum
  (peg-macro
    (lambda (stx)
      (syntax-parse stx
        [(_ s:string)
         (unless (member (syntax-e #'s) python-keyword-list)
           (raise-syntax-error #f "Invalid keyword token" #'s))
         #'(token (keyword 's))]))))


(struct return-ast [e] #:transparent)

(define-peg testlist-star-expr "True")

; with explicit `token`
(define-peg p1
  (=> (seq (token (keyword "return")) (? (: exp testlist-star-expr)))
      (return-ast exp)))

; with #%peg-datum
(define-peg p2
  (=> (seq "return" (? (: exp testlist-star-expr)))
      (return-ast exp)))

(module+ test
  (require rackunit)

  (check-equal?
    (parse-result-value (parse p1 (list (keyword-token "return") (keyword-token "True"))))
    (return-ast "True"))

  (check-equal?
    (parse-result-value (parse p2 (list (keyword-token "return") (keyword-token "True"))))
    (return-ast "True")))
|#
