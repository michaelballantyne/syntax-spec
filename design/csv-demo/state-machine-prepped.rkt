#lang racket

(provide machine state on)

(require bindingspec "state-machine-compiler.rkt")

(define-hosted-syntaxes
  (binding-class state-name)

  (two-pass-nonterminal state-spec
    (state name:state-name
      ((~datum on-enter) action:expr ...)
      e:event-spec ...)
    #:binding [(export name) action])
  
  (nonterminal event-spec
    (on (event-name:id arg:id ...)
      action:expr ...
      ((~datum ->) name:state-name))))


(define-host-interface/expression
  (machine #:initial-state initial-state:state-name
           #:states s:state-spec ...+
           #:shared-events e:event-spec ...)
  #:binding {(recursive s) initial-state e}

  #'(compile-machine (machine #:initial-state initial-state
                              #:states s ...
                              #:shared-events e ...)))
