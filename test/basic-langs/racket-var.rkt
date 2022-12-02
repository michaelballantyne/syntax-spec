#lang racket/base

; this tests the built-in racket-var binding class

(require "../../testing.rkt")

(define-hosted-syntaxes
  (nonterminal my-expr
   ((~literal let) ([x:racket-var e:expr] ...) b:expr)
   #:binding [(host e) {(bind x) (host b)}]))

(define-host-interface/expression
  (eval-my-expr e:my-expr)
  ; no with-reference-compilers
  #'e)

(check-equal?
 (eval-my-expr (let ([x 2]) (let ([y x] [z x]) (list y z))))
 '(2 2))
