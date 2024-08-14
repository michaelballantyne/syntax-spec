#lang racket/base
#|
(require "../main.rkt"
         "define-peg-ast.rkt")

(use-literal-token-interpretation syntax-token)

; stub `test` production
(define-peg test (alt "e1" "e2"))

(define-peg-ast raise raise-ast
  (seq "raise" (? (seq (: exn test) (? (seq "from" (: from test)))))))

(module+ test
  (require rackunit racket/list syntax/srcloc)

  (define example-stx
    (syntax->list #'(raise e1 from e2)))

  (check-equal?
    (parse-result-value (parse raise example-stx))
    (raise-ast
      (apply build-source-location example-stx)
      (second example-stx)
      (fourth example-stx))))

|#
