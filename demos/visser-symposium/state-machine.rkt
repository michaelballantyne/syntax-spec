#lang racket

(provide machine state on on-enter)

(require syntax-spec "state-machine-compiler.rkt")

(syntax-spec
  (binding-class state-name)

  (host-interface/expression
    (machine #:initial-state s:state-name d:machine-decl ...)
    #:binding {(recursive d) s}
    #'(compile-machine s d ...))
  
  (nonterminal/two-pass machine-decl
    (state n:state-name
      e:event-decl ...)
    #:binding (export n)
    e:event-decl)
  
  (nonterminal event-decl
    (on-enter e:expr ...)
    (on (evt:id arg:racket-var ...)
      e:expr ...
      ((~datum ->) s:state-name))
    #:binding {(bind arg) (host e)}))