#lang racket/base

; testing edge cases with ellipses and groups

(require "../main.rkt")

; the test is just that this compiles
(syntax-spec
  (nonterminal my-expr
    (my-let ([x:racket-var e:racket-expr] ...) body:racket-expr ...)
    #:binding [[e] ... (scope [(bind x)] ... [body] ...)]
    (my-weird-let (d1:my-exporting ...) (d2:my-exporting ...) ([x:racket-var e:racket-expr] ...) b:racket-expr)
    #:binding (scope [(bind x) ... (import d2) ...] (import d1) ...)
    (my-imporing d:my-exporting ...)
    #:binding (scope [(import d)] ...))
 (nonterminal/exporting my-exporting
   (my-define-values (x:racket-var ...))
   #:binding [[(export x)] ...]))

; and this---here we're making sure ...+ works just like ... for depth checking
(syntax-spec
  (nonterminal/exporting my-def
    (my-def x:racket-var ...+ e:racket-expr)
    #:binding [(export x) ...]))
