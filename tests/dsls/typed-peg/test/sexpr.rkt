#lang typed/racket
#|
(require "../main.rkt")

(define-peg digit (alt "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
(define-peg number (=> (: s (seq digit (* digit)))
                       (string->number s)))
(define-peg ws (* (text " ")))
(define-pegs
  [sexpr (alt number group)]
  [group (=> (seq (text "(")
                  ws
                  (* (seq (: e sexpr) ws))
                  (text ")"))
             e)])

(module+ test
  (require rackunit)
  (check-equal? (parse-result-value (parse number "123"))
                123)
  (check-equal? (parse-result-value (parse sexpr "123"))
                123)
  (check-equal? (parse-result-value (parse sexpr "()"))
                '())
  (check-equal? (parse-result-value (parse sexpr "(123)"))
                '(123))
  (check-equal? (parse-result-value (parse sexpr "(123 456)"))
                '(123 456))
  (check-equal? (parse-result-value (parse sexpr "(123 (456))"))
                '(123 (456)))
  (check-equal? (parse-result-value (parse sexpr "(123 ((456)))"))
                '(123 ((456))))
  (check-equal? (parse-result-value (parse sexpr "(123 ((456) 7))"))
                '(123 ((456) 7)))
  (check-equal? (parse-result-value (parse sexpr "(123 (456) ((789) 0))"))
                '(123 (456) ((789) 0))))

|#
