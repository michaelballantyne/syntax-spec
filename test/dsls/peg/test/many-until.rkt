#lang racket/base

(require "../main.rkt"
         (for-syntax racket/base syntax/parse))

(define-syntax many-until
  (peg-macro
    (lambda (stx)
      (syntax-parse stx
        [(many-until p1:expr p2:expr)
         ;; parse many instances of p1 until the terminator p2 is reached
         #'(* (seq (! p2) p1))]))))

(define-peg comment
  (seq "#" (many-until any-char "\n")))

(define-peg string
  (=> (seq "\"" (many-until (: cs any-char) "\"") "\"")
      (list->string (map (lambda (c) (string-ref c 0)) cs))))

(define-peg whitespace " ")

(define-peg line
  (=> (seq (? whitespace) (: e string) (? whitespace) (? comment) "\n")
      e))

(module+ test
  (require rackunit)

  (check-equal? (parse-result-value (parse string "\"a string\""))
                "a string")

  (check-equal?
    (parse-result-value (parse line "\"a string\" # a comment\n"))
    "a string"))
