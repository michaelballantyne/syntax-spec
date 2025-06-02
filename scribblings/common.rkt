#lang racket

(provide (all-defined-out))

(require scribble/manual scriblib/render-cond scribble/core)

(define (tech/reference str)
  (tech #:doc '(lib "scribblings/reference/reference.scrbl") str))

(define (tech/guide str)
  (tech #:doc '(lib "scribblings/guide/guide.scrbl") str))

(define (seclink/reference sec str)
  (seclink sec #:doc '(lib "scribblings/reference/reference.scrbl") str))

(define (seclink/guide sec str)
  (seclink sec #:doc '(lib "scribblings/guide/guide.scrbl") str))


(define (mk-exact . items)
  (cond-element
    [latex (make-element (make-style #false '(exact-chars)) items)]
    [else ""]))

(define (tex-label label)
  (mk-exact (format "\\label{~a}" label)))