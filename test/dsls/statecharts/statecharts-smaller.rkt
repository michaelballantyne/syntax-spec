#lang racket

(require bindingspec (for-syntax syntax/parse))

(define-hosted-syntaxes
  (binding-class statechart-name)
  (binding-class state-name)
  (binding-class var)
  (binding-class data-name)
  
  (two-pass-nonterminal state-body
    (initial n:state-name)
    #:binding {n}
    
    e:event

    (state n:state-name
           sb:state-body ...)
    #:binding [(export n) {(recursive sb)}]

    (use scn:statechart-name #:as sn:state-name
         e:event ...))

  (nonterminal event
    (on (evt:id arg:var ...)
        ab:action ...+)
    #:binding {(bind arg) ab})
    
  (nonterminal action
    (-> s:state-name)
    
    (set n:data-name e:expr)
    #:binding (host e)
    
    (emit (name:id arg:expr ...))
    #:binding (host arg)

    (let* (b:binding-group ...) body:action ...)
    #:binding (nest b body))

  (nesting-nonterminal binding-group (tail)
    [v:var e:expr]
    #:binding [(host e) {(bind v) tail}]))

#;(define-host-interface/definition
    (define-statechart n:statechart-name
      sb:state-body)
    #:binding [(export n) {(recursive sb)}])

(define-host-interface/expression
  (machine st:statechart-name)
  #''TODO)

; (machine, any) -> (machine, (listof any))
(define (machine-step m event)
  'TODO)

; ... accessors ...
