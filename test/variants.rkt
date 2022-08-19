#lang racket/base

(require "../main.rkt" (for-syntax syntax/parse racket/base))

(define-hosted-syntaxes
  (nonterminal flow-expr 
    (fanout n:number)
    fanout
    (fanout n1:expr n2:number)))

(define-host-interface/expression
  (flow f:flow-expr)
  #''f)

(flow fanout)
(flow (fanout 5))
(flow (fanout 5 6))