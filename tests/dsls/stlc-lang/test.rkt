#lang racket

; testing that programs in the #lang work as expected

(require "../simply-typed-lambda-calculus.rkt"
         "program.rkt")

(module+ test
  (require rackunit)
  (check-equal? (stlc/expr x) 2))
