#lang racket

(require
  "../core.rkt")

(define-peg t1
  (=> (seq "a\n" (: r "b"))
      r))

(module+ test
  (parse t1 "a\nb"))
