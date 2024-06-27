#lang racket/base

(require "../main.rkt")

(define-peg t (=> (? (seq (: x "x") (: y "y")))
                  (list x y)))

(module+ test
  (require rackunit)

  (check-equal? (parse-result-value (parse t "xz"))
                (list #f #f)))
