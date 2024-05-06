#lang racket/base

(require "../../testing.rkt")

(syntax-spec
  (binding-class var #:description "mylang variable")

  (extension-class mylang-macro)

  (nonterminal expr
    #:description "mylang expression"
    #:allow-extension mylang-macro

    v:var
    n:number
    (+ e1:expr e2:expr)
    
    (block d:def-or-expr ...)
    #:binding (nest d []))

  (nonterminal/nesting def-or-expr (tail)
    #:description "mylang definition context"
    #:allow-extension mylang-macro

    (begin d:def-or-expr ...)
    #:binding (nest d tail)
    
    (define*-values (v:var ...) e:expr)
    #:binding [e (scope (bind v) tail)]
    
    e:expr))

(define-syntax define*
  (mylang-macro
   (syntax-parser
     [(_ v e)
      #'(define*-values (v) e)])))

;; tests
(check-equal?
 (expand-nonterminal/datum expr
   (block
    (begin
      (define* x 5)
      (define* x (+ x 1)))
    (define* x (+ x 1))
    (+ x 1)))
 '(block
   (begin
     (define*-values (x) 5)
     (define*-values (x) (+ x 1)))
   (define*-values (x) (+ x 1))
   (+ x 1)))
