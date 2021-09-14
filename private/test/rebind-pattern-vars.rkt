#lang racket/base

(require "../syntax/compile/pattern-var-reflection.rkt"
         syntax/stx
         rackunit)

(define (wrap-add1 stx)
  #`(+ #,stx 1))

(define transformed
  (syntax-case #'(let ([x 5] [y 6]) (+ x y)) ()
    [(form ([v e] ...) b)
     (rebind-pattern-vars
      (v e b)
      (values (pattern-var-value v)
              (stx-map wrap-add1 (pattern-var-value e))
              (wrap-add1 (pattern-var-value b)))
      #'(form ([v e] ...) b))]))

(check-equal?
 (syntax->datum transformed)
 '(let ([x (+ 5 1)] [y (+ 6 1)]) (+ (+ x y) 1)))

(check-equal?
 (eval transformed)
 14)