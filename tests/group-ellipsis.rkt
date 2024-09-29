#lang racket/base

; testing edge cases with ellipses and groups

(require "../testing.rkt")

; the test is just that this compiles
(syntax-spec
  (nonterminal my-expr
    (my-let ([x:racket-var e:racket-expr] ...) body:racket-expr ...)
    #:binding [[e] ... (scope [(bind x)] ... [body] ...)]
    (my-imporing d:my-exporting ...)
    #:binding (scope [(import d)] ...))
 (nonterminal/exporting my-exporting
   (my-define-values (x:racket-var ...))
   #:binding [[(export x)] ...]))
