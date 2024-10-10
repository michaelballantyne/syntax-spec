#lang racket/base

(require "../../testing.rkt")

(syntax-spec
  (binding-class var #:description "while-language variable")

  (extension-class expr-macro)
  (extension-class stmt-macro)

  (nonterminal expr
    #:description "while-language expression"

    n:number

    v:var
   
    (+ e1:expr e2:expr)
    (< e1:expr e2:expr)

    (vars (v:var ...) e:expr)
    #:binding (scope (bind v) ... e)
   
    (do s:stmt ... e:expr))

  (nonterminal stmt
    #:description "while-language statement"
    #:allow-extension stmt-macro

    (set v:var e:expr)

    (print e:expr)
   
    (while e:expr
           s:stmt ...)
   
    (stmts s:stmt ...)))

;; sugar
(define-syntax for
  (stmt-macro
   (syntax-parser
     [(_ [init cond incr] stmt ...)
      #'(stmts
         init
         (while cond
                stmt ...
                incr))])))
      
;; tests

(check-equal?
 (expand-nonterminal/datum expr
   (vars (i x)
         (do
             (set x 5)
           (for [(set i 0) (< i x) (set i (+ i 1))]
             (print i))
           i)))
 (expand-nonterminal/datum expr
  (vars (i x)
        (do
            (set x 5)

          (stmts
           (set i 0)
           (while (< i x)
                  (print i)
                  (set i (+ i 1))))
          
          i))))
