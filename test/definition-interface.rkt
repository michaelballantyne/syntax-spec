#lang racket

(require "../main.rkt"
         rackunit)

(syntax-spec
  (binding-class dsl-var)
  (nonterminal dsl-e
    n:number)

  (host-interface/definition
    (dsl-def x:dsl-var rhs:dsl-e)
    #:binding (export x)

    #:lhs
    [#'x]
    #:rhs
    [#''rhs])

  (host-interface/expression
    (dsl-ref v:dsl-var)
    #'v))

(define f (lambda () (dsl-ref x)))
(dsl-def x 5)
(check-equal?
 (f)
 5)

(let ()
  (define f (lambda () (dsl-ref x)))
  (dsl-def x 6)
  (check-equal?
   (f)
   6))
