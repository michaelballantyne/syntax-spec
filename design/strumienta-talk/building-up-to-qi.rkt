#lang racket


;; (-> (U string? symbol?) string? (U string? symbol?))
(define (add-prefix name prefix)
  (cond
    [(string? name) (string-append prefix name)]
    [(symbol? name) (string->symbol
                     (string-append prefix
                                    (symbol->string name)))]))

(add-prefix 'x "1-")

;; Pattern: cond with predicates on same argument.

(define-syntax-rule
  (switch (e) [p b] ...)
  (let ([v e]) (cond [(p v) b] ...)))

(define (add-prefix2 name prefix)
  (switch (name)
    [string? (string-append prefix name)]
    [symbol? (string->symbol
                     (string-append prefix
                                    (symbol->string name)))]))

(add-prefix2 'x "2-")

;; Pattern: apply a sequence of transformations to a value

(require threading)
(define (add-prefix3 name prefix)
  (switch (name)
    [string? (string-append prefix name)]
    [symbol? (~>> name
                  symbol->string
                  (string-append prefix)
                  string->symbol)]))

(add-prefix3 'x "3-")

;; Notice common concept of a "flow"

(module qi-example racket
  (require qi)

  (define (add-prefix4 name prefix)
    (switch (name)
            [string? (string-append prefix)]
            [symbol? (~>> symbol->string
                          (string-append prefix)
                          string->symbol)]))

  (add-prefix4 'x "4-"))

(require 'qi-example)
