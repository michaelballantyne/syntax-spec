#lang racket/base

(require "../../main.rkt"
         rackunit
         (for-syntax racket/base syntax/parse racket/pretty))

(define-hosted-syntaxes
  (binding-class var "mylang variable")

  (extension-class mylang-macro)

  (nonterminal expr
    #:description "mylang expression"
    #:allow-extension mylang-macro

    v:var
    n:number
    (+ e1:expr e2:expr)
    
    (block d:def-or-expr ...)
    #:binding (nest d []))

  (nesting-nonterminal def-or-expr (tail)
    #:description "mylang definition context"
    #:allow-extension mylang-macro

    (begin d:def-or-expr ...)
    #:binding (nest d tail)
    
    (define*-values (v:var ...) e:expr)
    #:binding [e {(! v) tail}]
    
    e:expr))

;; simulated interface macro
(define-syntax mylang-expr
  (syntax-parser
    [(_ e) #`'#,((nonterminal-expander expr) #'e)]))

(define-syntax define*
  (mylang-macro
   (syntax-parser
     [(_ v e)
      #'(define*-values (v) e)])))

;; tests
(check-equal?
 (mylang-expr
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
