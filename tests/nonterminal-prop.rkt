#lang racket/base

(require "../main.rkt"
         (for-syntax syntax/parse racket/base (only-in "../private/ee-lib/main.rkt" map-transform))
         rackunit
         syntax/macro-testing)

(syntax-spec
  (nonterminal flow-expr
    (thread f:flow-expr ...+)
    (partition [cond:flow-expr then:flow-expr] ...)
    fanout
    (fanout n:number)
    (fanout n1:expr n2:number)
    (~>/form (fanout . _)
             (raise-syntax-error #f "expected one of:\n    fanout\n    (fanout n:number)\n    (fanout n1:expr n2:number)" this-syntax)))

  (host-interface/expression
    (flow f:flow-expr)
    #`'#,(find-subexpr-positions #'f)))

(begin-for-syntax
  ;; Syntax -> ListOf Syntax
  (define (find-subexpr-positions stx)
    (define subexprs '())
    (map-transform
     (lambda (stx)
       (when (and (syntax? stx) (syntax-property stx 'nonterminal))
         (set! subexprs (cons stx subexprs)))
       stx)
     stx)
    subexprs))

(check-equal?
 (flow (thread fanout (fanout 1) (partition [fanout (fanout 1)])))
 '((thread fanout (fanout 1) (partition (fanout (fanout 1))))
   (partition (fanout (fanout 1)))
   (fanout 1)
   fanout
   (fanout 1)
   fanout))
