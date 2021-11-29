#lang racket/base

(provide struct-error-as-expression
         dsl-error-as-expression)

(define (struct-error-as-expression message)
  (lambda (s stx)
    (raise-syntax-error
     #f
     message
     stx)))

(define (dsl-error-as-expression type)
  (struct-error-as-expression
   (string-append
    type
    " may not be used as a racket expression")))