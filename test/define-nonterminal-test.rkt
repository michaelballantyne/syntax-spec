#lang racket/base

(require "../syntax/define-nonterminal.rkt"
         (for-syntax racket/base syntax/parse racket/pretty))

(define-binding-class mylang-var "mylang variable")

(define-nonterminal mylang-expr
  #:description "mylang expression"

  n:number
  
  x:mylang-var
  
  (cons e1:mylang-expr e2:mylang-expr)
  
  (let ([v:mylang-var e:mylang-expr])
    b:mylang-expr)
  #:binding {(! v) b}
  )

(define-syntax mylang
  (syntax-parser
    [(_ e)
     (pretty-display (syntax->datum ((nonterminal-expander mylang-expr) #'e)))
     #'(void)]))

(mylang
 (let ([x 5])
   (cons x (let ([y 6])
             (cons x y)))))