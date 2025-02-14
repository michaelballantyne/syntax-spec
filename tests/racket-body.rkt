#lang racket/base

(require "../testing.rkt")

(syntax-spec
  (nonterminal my-expr
    (my-let ([x:racket-var e:racket-expr]) body:racket-body ...+)
    #:binding (scope (bind x) (import body) ...))
  (host-interface/expression
    (my-dsl e:my-expr)
    (syntax-parse #'e
      [(_ ([x e]) body ...+)
       #'(let ([x e]) body ...)])))

(check-equal?
 (my-dsl (my-let ([x 1]) x))
 1)
(check-equal?
 (my-dsl (my-let ([x 1]) (define y x) y))
 1)
(check-equal?
 (my-dsl (my-let ([x 1]) (define-syntax-rule (m) x) (m)))
 1)
