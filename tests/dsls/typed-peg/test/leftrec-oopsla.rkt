#lang racket/load
#|
(require rackunit)

; capture compile-time errors for testing
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

; check that expanding the submodule generates a compile-time error with the expected messagae
(test-error
  #rx"arith-expr-leftrec: left recursion through nonterminal"
  (module example racket/base
    (require "../main.rkt")

    (struct binop-ast [lhs op rhs] #:transparent)

    (define (left-associate-binops e1 op* e*)
      (foldl (lambda (op e base) (binop-ast base op e))
             e1 op* e*))

    (define-peg term (predicate-token number?))

    (define-peg arith-expr-leftrec
                (alt term
                     (=> (seq (: e1 arith-expr-leftrec) (: op (alt "+" "-")) (: e2 term))
                         (binop-ast e1 op e2))))))
|#
