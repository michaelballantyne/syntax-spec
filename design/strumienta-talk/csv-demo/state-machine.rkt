#lang racket

(provide machine state on)

(require bindingspec "state-machine-compiler.rkt")

(syntax-spec
  (binding-class state-name)
  
  (nonterminal/two-pass state-spec
    (state name:state-name
      ((~datum on-enter) action:expr ...)
      e:event-spec ...)
    #:binding (export name))
  
  (nonterminal event-spec
    (on (name:id arg:id ...)
      action:expr ...
      ((~datum ->) new-name:state-name)))

  (host-interface/expression
    (machine #:initial-state init:state-name
             #:states s:state-spec ...
             #:shared-events e:event-spec ...)
    #:binding { (recursive s) init e }
  
    #'(compile-machine (machine #:initial-state init
                                #:states s ...
                                #:shared-events e ...))))