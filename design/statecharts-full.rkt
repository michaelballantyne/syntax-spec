#lang racket

(require "../main.rkt" (for-syntax syntax/parse))

(syntax-spec
  (binding-class statechart-name)
  (binding-class state-name)
  (binding-class var)
  (binding-class data-name)
  
  (nonterminal/exporting state-body
    (initial n:state-name)
    #:binding (scope n)
    
    e:event

    (data n:data-name e:racket-expr)
    #:binding (export n)

    (state n:state-name
      sb:state-body ...)
    #:binding [(export n) (scope (import sb))]

    (use scn:statechart-name #:as sn:state-name
         e:event ...))

  (nonterminal event
    (on (evt:id arg:var ...)
      ab:action ...+)
    #:binding (scope (bind arg) ab)

    (on-enter ab:action ...)
    (on-exit ab:action ...))
    
  (nonterminal action
    (-> s:state-name)
    
    (set n:data-name e:racket-expr)
    
    (emit (name:id arg:racket-expr ...))

    (let* (b:binding-group ...) body:action ...)
    #:binding (nest b body))

  (nonterminal/nesting binding-group (tail)
    [v:var e:racket-expr]
    #:binding (scope (bind v) tail))
  
  #;(host-interface/definition
      (define-statechart n:statechart-name
        sb:state-body)
      #:binding [(export n) (scope (import sb))])

  (host-interface/expression
    (machine st:statechart-name)
    #''TODO))

; (machine, any) -> (machine, (listof any))
(define (machine-step m event)
  'TODO)

; ... accessors ...
