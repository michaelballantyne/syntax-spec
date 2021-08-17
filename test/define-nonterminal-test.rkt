#lang racket/base

(require "../syntax/define-nonterminal.rkt"
         (for-syntax racket/base syntax/parse ee-lib racket/pretty))

(define-binding-class mylang-var "mylang variable")

(define-nonterminal mylang-expr
  #:expander mylang-expand-expr
  #:else mylang-expand-other-expr
  
  (let ([v:mylang-var e:mylang-expr])
    b:mylang-expr)
  #:binding [e {(! v) b}]

  (cons e1:mylang-expr e2:mylang-expr)
  #:binding [e1 e2])

(begin-for-syntax
  (define (mylang-expand-other-expr stx)
    (syntax-parse stx
      [n:number #'n]
      [v:id
       (define binding (lookup #'v (binding-class-predicate mylang-var)))
       (when (not binding)
         (raise-syntax-error #f "not bound as a mylang variable" #'v))
       #'v]
      [_
       (raise-syntax-error #f "not a mylang expression" stx)])))

(define-syntax mylang
  (syntax-parser
    [(_ e)
     (pretty-display (syntax->datum (mylang-expand-expr #'e)))
     #'(void)]))

(mylang
 (let ([x 5])
   (cons x (let ([y 6])
             (cons x y)))))