#lang racket/load

(require rackunit)

(define-syntax (test-error stx)
  (syntax-case stx ()
    [(_ regexp mod)
     #`(check-exn
         regexp
         (lambda ()
           #,(with-handlers ([(lambda (e) (exn:fail:syntax? e))
                              (lambda (e) #`(raise #,e))])
                            (local-expand #'mod 'top-level '())
                            #'#f)
           #t))]))


(test-error
  #rx"arith-expr-leftrec: left recursion through nonterminal"
  (module ex racket/base
    (require "../main.rkt")

    ; Check that nullability information from earlier modules is available
    ; to later modules.
    (module pre racket/base
      (require "../main.rkt")
      (provide term)

      (define-peg term "term"))

    (require 'pre)

    (struct binop-ast [e1 op e2])

    (define-peg arith-expr-leftrec
                (alt term
                     (=> (seq (: e1 arith-expr-leftrec)
                              (seq
                                (: op (alt "+" "-"))
                                (: e2 term)))
                         (binop-ast e1 op e2))))))
