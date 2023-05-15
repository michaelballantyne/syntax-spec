#lang racket/base

(require "../main.rkt")

(define-pegs
  [foo "foo"]
  [bar (=> (seq x:foo y:foo)
           (list x y))])

(module+ test
  (require rackunit)
  (check-equal? (parse-result-value (parse bar "foofoo"))
                '("foo" "foo")))
