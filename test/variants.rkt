#lang racket/base

(require "../main.rkt" (for-syntax racket/base))

(define-hosted-syntaxes
  (nonterminal flow-expr 
    (fanout n:number)
    (~literal fanout)
    ((~literal fanout) n1:number n2:number)))

(define-host-interface/expression
  (flow f:flow-expr)
  #''f)

(flow fanout)
(flow (fanout 5))
(flow (fanout 5 6))