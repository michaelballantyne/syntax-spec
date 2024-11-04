#lang racket

(provide tech/reference)

(require scribble/manual)

(define (tech/reference str)
  (tech #:doc '(lib "scribblings/reference/reference.scrbl") str))