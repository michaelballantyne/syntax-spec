#lang racket

(require "../main.rkt" rackunit)

(define-peg atoz
  (=> (: c (char-range #\a #\z))
      c))

(check-equal?
 (parse-result-value (parse atoz "a"))
 "a")