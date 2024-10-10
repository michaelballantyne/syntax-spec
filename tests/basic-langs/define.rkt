#lang racket/base

(require "../../testing.rkt")

(syntax-spec
  (binding-class var #:description "dsl variable")

  (extension-class dsl-macro)

  (nonterminal expr
    #:description "dsl expression"
    #:allow-extension dsl-macro

    v:var
    n:number
    (dsl-+ e1:expr e2:expr)
               
    (dsl-lambda (v:var ...) d:def-or-expr ...)
    #:binding (scope (bind v) ... (scope (import d) ...))

    (dsl-letrec-values ([(v:var ...) rhs:expr] ...) d:def-or-expr)
    #:binding (scope (bind v) ... ... rhs ... (scope (import d)))

    (dsl-let* (b:binding ...) e:expr)
    #:binding (nest b ... e)

    (v:var e:expr ...))

  (nonterminal/nesting binding (nested)
    #:description "dsl-let* binding group"
    
    [v:var e:expr]
    #:binding [e (scope (bind v) nested)])
  
  (nonterminal/exporting def-or-expr
    #:description "dsl definition context"
    #:allow-extension dsl-macro

    (dsl-begin d:def-or-expr ...)
    #:binding [(re-export d) ...]
    
    (dsl-define-values (v:var ...) e:expr)
    #:binding [(export v) ... e]
    
    e:expr))

(define-syntax dsl-define
  (dsl-macro
   (syntax-parser
     [(_ v e)
      #'(dsl-define-values (v) e)])))

;; tests

(check-equal?
 (expand-nonterminal/datum expr
   (dsl-lambda ()
               (dsl-define f (f))
               (f)))
 '(dsl-lambda ()
              (dsl-define-values (f) (f))
              (f)))

(check-equal?
 (expand-nonterminal/datum expr
   (dsl-lambda ()
               (dsl-define f (dsl-lambda (f) (f (g))))
               (dsl-begin
                (dsl-define g (dsl-lambda () (f))))
               (f)))
 '(dsl-lambda ()
              (dsl-define-values (f) (dsl-lambda (f) (f (g))))
              (dsl-begin
               (dsl-define-values (g) (dsl-lambda () (f))))
              (f)))

(check-equal?
 (expand-nonterminal/datum expr
   (dsl-letrec-values ([(a b c) (a b c d e f)]
                       [(d e f) (a b c d e f)])
     (a b c d e f)))
 '(dsl-letrec-values ([(a b c) (a b c d e f)]
                       [(d e f) (a b c d e f)])
     (a b c d e f)))

(check-exn
 #rx"dsl-define-values: identifier already defined"
 (lambda ()
   (expand-nonterminal/datum expr
     (dsl-lambda ()
                 (dsl-define x 5)
                 (dsl-define x 5)))))

