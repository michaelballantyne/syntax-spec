#lang racket

(require
  rackunit
  (submod "../compile-alt-str.rkt" case))

(define (f v)
  (int-case v
            [1 'a]
            [2 'b]
            [3 'c]
            [4 'd]
            [5 'e]
            [6 'f]
            [7 'g]
            [else #f]))

(define (g v)
  (char-case v
             [#\a 'a]
             [#\b 'b]
             [#\d 'd]
             [#\e 'e]
             [#\f 'f]
             [#\g 'g]
             [#\h 'h]
             [else #f]))

(check-equal?
 (map f '(1 2 3 4 5 6 7 0 9))
 '(a b c d e f g #f #f))

(check-equal?
 (map g (string->list "abcdefghi"))
 '(a b #f d e f g h #f))