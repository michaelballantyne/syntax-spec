#lang racket/load
#|
(require (for-syntax syntax/parse) racket/pretty rackunit (only-in "../main.rkt" parse parse-result?))

; helper macros for printing the compiled code
(define-for-syntax expanded #f)
(define-syntax expand
  (syntax-parser
    [(_ m)
     (set! expanded (local-expand #'m 'top-level '()))
     expanded]))
(define-syntax (show-expanded stx)
  #`(pretty-print '#,expanded))

(expand
  (module example racket/base
    (require "../main.rkt")
    (provide comp-op)
    ; the production to be optimized
    (define-peg comp-op
                (alt "==" ">=" "<=" "<" ">" "!=" "in" "not" "is"))))

; print the compiled module, showing how the production is compliled.
(show-expanded)

(require 'example)

(for/list ([s '("==" ">=" "<=" "<" ">" "!=" "in" "not" "is")])
  (check-true
    (parse-result? (parse comp-op s))))

(check-exn #rx"parse failed"
           (lambda ()
             (parse comp-op "=")))

(check-exn #rx"parse failed"
           (lambda ()
             (parse comp-op "foo")))

|#
