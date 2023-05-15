#lang racket/base

(require "../main.rkt" rackunit)

(let ()
  (define-peg foo "foo")
  (define-peg foobar (seq foo "bar"))
  (check-equal? (parse-result-value (parse foobar "foobar"))
                "bar"))
