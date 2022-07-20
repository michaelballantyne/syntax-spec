#lang racket

(require "../main.rkt"
         rackunit)

(define-hosted-syntaxes
  (binding-class dsl-var)
  (nonterminal dsl-e
    n:number))

(define-host-interface/definition
  (dsl-def x:dsl-var rhs:dsl-e)
  #:binding (export x)
  ->
  (define
    [(compile-binder! #'x)]
    [#''rhs]))

(define-host-interface/expression
  (dsl-ref v:dsl-var)
  (compile-reference #'v))


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