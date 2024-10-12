#lang racket/base

; testing the conversion from (nest x y e) to (nest x (nest y e))

(require "../testing.rkt"
         syntax/macro-testing)

(syntax-spec
  (nonterminal my-expr
    (my-let* (b1:binding ...)
             (b2:binding)
      body:racket-expr)
    #:binding (nest b1 ... b2 body))
  (nonterminal/nesting binding (nested)
    [x:racket-var e:racket-expr]
    #:binding (scope (bind x) nested))
  (host-interface/expression
    (my-dsl e:my-expr)
    #'(compile-expr e)))

(define-syntax compile-expr
  (syntax-parser
    #:literals (my-let*)
    [(_ (my-let* ([x1 e1] ...)
                 ([x2 e2])
          body))
     #'(let* ([x1 e1] ... [x2 e2]) body)]))

(check-equal?
 (my-dsl (my-let* ([x 1] [y x]) ([z y]) z))
 1)
