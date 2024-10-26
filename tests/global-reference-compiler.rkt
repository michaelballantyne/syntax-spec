#lang racket/base

(require "../testing.rkt")

(syntax-spec
  (binding-class my-var #:reference-compiler immutable-reference-compiler)
  (host-interface/expression
    (my-let ([x:my-var e:racket-expr]) body:racket-expr ...)
    #:binding (scope (bind x) body ...)
    #'(let ([x e]) body ...)))

(check-equal?
 (my-let ([x 2]) x)
 2)

(check-exn
   #rx"cannot mutate identifier"
   (lambda ()
     (convert-compile-time-error
      (my-let ([x 2]) (set! x 3) x))))

(check-decl-error
 #rx"contract violation"
 (syntax-spec
   (binding-class dsl-var #:reference-compiler 2)
   (nonterminal expr
     [x:dsl-var x:dsl-var])))
