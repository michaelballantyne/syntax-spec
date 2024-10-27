#lang racket/base

; this tests the built-in racket-var binding class

(require "../../testing.rkt")

(syntax-spec
  (nonterminal my-expr
   ((~literal let) ([x:racket-var e:racket-expr] ...) b:racket-expr)
   #:binding (scope (bind x) ... b))

  (host-interface/expression
    (eval-my-expr e:my-expr)
    #'e))

(check-equal?
 (eval-my-expr (let ([x 2]) (let ([y x] [z x]) (list y z))))
 '(2 2))
