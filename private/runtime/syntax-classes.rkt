#lang racket/base

(provide literal-in-space)

(require syntax/parse ee-lib)

(define-syntax-class (literal-in-space target-id binding-space)
  (pattern id:id
           #:when (same-binding? ((in-space binding-space) (attribute id))
                                 ((in-space binding-space) target-id))))