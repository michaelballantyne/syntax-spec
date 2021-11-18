#lang racket/base

(require "../../main.rkt"
         rackunit
         syntax/macro-testing
         (for-syntax racket/base syntax/parse racket/pretty))

(define-hosted-syntaxes
  (binding-class var "dsl variable")

  (extension-class dsl-macro)

  (nonterminal expr
    #:description "dsl expression"
    #:allow-extension dsl-macro

    v:var
    n:number
    (dsl-+ e1:expr e2:expr)
               
    (dsl-lambda (v:var ...) d:def-or-expr ...)
    #:binding {(! v) {(rec d)}}

    (v:var e:expr ...))

  (two-pass-nonterminal def-or-expr
    #:description "mylang definition context"
    #:allow-extension dsl-macro

    (dsl-begin d:def-or-expr ...)
    #:binding (rec d)
    
    (dsl-define-values (v:var ...) e:expr)
    #:binding [(^ v) e]
    
    e:expr))

;; simulated interface macro
(define-syntax mylang-expr
  (syntax-parser
    [(_ e) #`'#,((nonterminal-expander expr) #'e)]))

(define-syntax dsl-define
  (dsl-macro
   (syntax-parser
     [(_ v e)
      #'(dsl-define-values (v) e)])))

;; tests

(check-equal?
 (mylang-expr
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
   

(check-exn
 #rx"mylang-expr: identifier already defined"
 (lambda ()
   (convert-compile-time-error
    (mylang-expr
     (dsl-lambda ()
                 (dsl-define x 5)
                 (dsl-define x 5))))))

