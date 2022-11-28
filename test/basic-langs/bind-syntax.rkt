#lang racket/base

; test bind-syntax

(require "../../testing.rkt")

(define-hosted-syntaxes
  (extension-class expr-macro)

  (binding-class var #:description "expr language variable")

  (nonterminal my-expr
    #:description "simple expr language expression"
    #:allow-extension (expr-macro racket-macro)

    n:number
    v:var
    (+ e1:my-expr e2:my-expr)

    (let ([v:var e:my-expr] ...) b:my-expr)
    #:binding [e {(bind v) b}]

    (let-syntax ([v:racket-macro e:expr] ...) b:my-expr)
    #:binding {(bind-syntax v e) b}))

(check-equal?
 (expand-nonterminal/datum my-expr
   (let-syntax ([double (syntax-rules () [(x e) (+ e e)])])
     (let ([x 1])
       (double x))))
 '(let-syntax ([double (syntax-rules () [(x e) (+ e e)])])
    (let ([x 1])
      (+ x x))))

(check-equal?
 (expand-nonterminal/datum my-expr
   (let-syntax ([double (expr-macro (syntax-rules () [(x e) (+ e e)]))])
     (let ([x 1])
       (double x))))
 '(let-syntax ([double (expr-macro (syntax-rules () [(x e) (+ e e)]))])
    (let ([x 1])
      (+ x x))))
