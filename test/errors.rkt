#lang racket/base

(require "../main.rkt"
         (for-syntax racket/base syntax/parse)
         syntax/macro-testing
         racket/exn
         rackunit)

; Helpers

(define ((check-formatted-error-matches rx) exn)
  (regexp-match? rx (exn->string exn)))

(define (check-decl-error rx decl-stx)
  (check-exn
   (check-formatted-error-matches rx)
   (lambda ()
     (eval-syntax #`(module m racket/base
                      (require "../main.rkt")
                      #,decl-stx)))))

(define-syntax-rule (check-phase1-error rx e)
  (check-exn
   (check-formatted-error-matches rx)
   (lambda () (phase1-eval e #:catch? #t))))

; Syntax spec syntax errors

(check-decl-error
 #rx"nonterminal: not a valid syntax spec term"
 #'(define-hosted-syntaxes
     (nonterminal expr
                  1)))

(check-decl-error
 #rx"nonterminal: expected a reference to a binding class, syntax class, or nonterminal"
 #'(define-hosted-syntaxes
     (nonterminal expr
                  x:unbound-name)))

(check-decl-error
 #rx"nonterminal: duplicate pattern variable"
 #'(define-hosted-syntaxes
     (binding-class dsl-var "dsl-var")
     (nonterminal expr
                  [x:dsl-var x:dsl-var])))

; Binding spec syntax errors



; Valid definitions used to exercise errors

(define-hosted-syntaxes
  (binding-class dsl-var "DSL var")
  (nonterminal expr1
               n:number)
  (nonterminal expr2
               #:description "DSL expression"
               n:number)
  (nesting-nonterminal binding-group (tail)
                       [v:dsl-var e:expr1]
                       #:binding {(! v) tail}))

; Accessor syntax errors

(check-phase1-error
 #rx"binding-class-constructor: not bound as binding class"
 (binding-class-constructor unbound-name))

(check-phase1-error
 #rx"nonterminal-expander: not bound as nonterminal"
 (nonterminal-expander unbound-name))

(check-phase1-error
 #rx"nonterminal-expander: only simple non-terminals may be used as entry points"
 (nonterminal-expander binding-group))

; Runtime (wrt the meta-DSL) errors

(check-phase1-error
 #rx"foo: expected expr1"
 ((nonterminal-expander expr1) #'foo))

(check-phase1-error
 #rx"foo: expected DSL expression"
 ((nonterminal-expander expr2) #'foo))
