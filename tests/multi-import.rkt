#lang racket/base

; testing the combining of multiple imports into a single import group

(require "../testing.rkt")

(syntax-spec
  (nonterminal/exporting defn
    ((~literal define) x:racket-var e:racket-expr)
    #:binding [(export x)])

  (host-interface/expression
    (double-local ([d1:defn ...] [d2:defn ...]) body:racket-expr)
    #:binding (scope (import d1) ... (import d2) ... body)
    #'(compile-expr ([d1 ...] [d2 ...]) body))

  (host-interface/expression
    (many-local ([d:defn ...] ...) body:racket-expr)
    ; this group is unnecessary, but we want to test the behavior of ellipsized groups with imports
    #:binding (scope [[[[(import d)]] ...] ...] body)
    #'(compile-expr ([d ...] ...) body)))

(define-syntax compile-expr
  (syntax-parser
    #:literals (define)
    [(_ ([(define x e1) ...] ...) body)
     #'(let ()
         (define x e1)
         ...
         ...
         body)]))

(check-equal?
 (double-local ([(define odd? (lambda (n) (if (zero? n) #f (even? (sub1 n)))))]
                [(define even? (lambda (n) (or (zero? n) (odd? (sub1 n)))))])
               (odd? 3))
 #t)
