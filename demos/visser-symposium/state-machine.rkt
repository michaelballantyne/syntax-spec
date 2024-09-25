#lang racket

(provide machine state on on-enter)

(require "../../main.rkt" "state-machine-compiler.rkt")

(syntax-spec
  (binding-class state-name)

  (host-interface/expression
    (machine #:initial-state s:state-name d:machine-decl ...)
    #:binding (scope (import d) ... s)
    #'(compile-machine s d ...))
  
  (nonterminal/exporting machine-decl
    (state n:state-name
      e:event-decl ...)
    #:binding (export n)
    e:event-decl)
  
  (nonterminal event-decl
    (on-enter e:racket-expr ...)
    (on (evt:id arg:racket-var ...)
      e:racket-expr ...
      ((~datum ->) s:state-name))
    #:binding (scope (bind arg) ... e ...)))
