#lang racket/base

(provide error-as-expression)

(define (error-as-expression message)
  (lambda (stx)
    (raise-syntax-error
     #f
     message)))