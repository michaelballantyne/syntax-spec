#lang racket

(provide machine state on)

(require "../../../main.rkt" "state-machine-compiler.rkt")

(syntax-spec
  (binding-class state-name)
  
  (nonterminal/exporting state-spec
    (state name:state-name
      ((~datum on-enter) action:racket-expr ...)
      e:event-spec ...)
    #:binding (export name))
  
  (nonterminal event-spec
    (on (name:id arg:id ...)
      action:racket-expr ...
      ((~datum ->) new-name:state-name)))

  (host-interface/expression
    (machine #:initial-state init:state-name
             #:states s:state-spec ...
             #:shared-events e:event-spec ...)
    #:binding (scope (import s) init e)
  
    #'(compile-machine (machine #:initial-state init
                                #:states s ...
                                #:shared-events e ...))))