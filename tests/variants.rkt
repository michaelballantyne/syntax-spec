#lang racket/base

(require "../main.rkt"
         (for-syntax syntax/parse racket/base)
         rackunit
         syntax/macro-testing)

(syntax-spec
  (nonterminal flow-expr
    fanout
    (fanout n:number)
    (fanout n1:expr n2:number)
    (~>/form (fanout . _)
             (raise-syntax-error #f "expected one of:\n    fanout\n    (fanout n:number)\n    (fanout n1:expr n2:number)" this-syntax)))

  (host-interface/expression
    (flow f:flow-expr)
    #''f))
  
(flow fanout)
(flow (fanout 5))
(flow (fanout 5 6))


(check-exn
 #rx"fanout: expected one of:\n    fanout"
 (lambda () (convert-compile-time-error
             (flow (fanout 5 6 7)))))
