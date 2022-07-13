#lang racket/base

(require "../syntax/compile/pattern-var-reflection.rkt"
         syntax/parse
         syntax/stx
         rackunit)


(let ()
  (define/syntax-parse (x ...) #'(1 2 3))

  (check-equal?
   (syntax->datum
    (rebind-pattern-vars (x) #f
                         #'(~? (x ...) ())))
   '())

  (check-equal?
   (syntax->datum
    (rebind-pattern-vars (x) (attribute x)
                         #'(~? (x ...) ())))
   '(1 2 3)))

(let ()
  (define (wrap-add1 stx)
    #`(+ #,stx 1))

  (define transformed
    (syntax-parse #'(let ([x 5] [y 6]) (+ x y))
      [(form ([v e] ...) b)
       (rebind-pattern-vars
        (v e b)
        (values (attribute v)
                (map wrap-add1 (attribute e))
                (wrap-add1 (attribute b)))
        #'(form ([v e] ...) b))]))

  (check-equal?
   (syntax->datum transformed)
   '(let ([x (+ 5 1)] [y (+ 6 1)]) (+ (+ x y) 1)))

  (check-equal?
   (eval transformed)
   14))