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

(syntax-spec
   (binding-class dsl-var #:reference-compiler 2)
   (host-interface/expression
     (bad-let x:dsl-var e:racket-expr)
     #:binding (scope (bind x) e)
     #'(let ([x 2]) e)))

(check-exn
 #rx"binding-class: contract violation"
 (lambda ()
   (convert-compile-time-error
    (bad-let x x))))
