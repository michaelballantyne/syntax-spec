#lang racket/base

(require "../testing.rkt")

;; It is often easiest to use datum-literal matching in the DSL
;; compiler, especially if the DSL syntax uses binding spaces.

(provide dsl (for-space dsl +))

(syntax-spec
  (nonterminal expr
    #:binding-space dsl

    n:number
    (+ e1:expr e2:expr))

  (host-interface/expression
    (dsl e:expr)
    #'(compile-dsl e)))

(define-syntax (compile-dsl stx)
  (syntax-parse stx
    #:datum-literals (+)
    [(_ n:number)
     #'n]
    [(_ (+ e1 e2))
     #'(+ (compile-dsl e1) (compile-dsl e2))]))

(check-equal?
 (dsl (+ 1 2))
 3)


(module* test racket
  (require (rename-in (submod "..") [+ dsl-+])
           rackunit)

  (check-equal?
   (+ 1 2)
   3)

  ;; Check that the expander constructs output syntax with the
  ;; symbol used in the syntax-spec and not the symbol from the
  ;; input syntax; otherwise this example would not compile.
  (check-equal?
   (dsl (dsl-+ 1 2))
   3))
  
