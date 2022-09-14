#lang racket/base

(require "../main.rkt"
         (for-syntax racket/base syntax/parse)
         syntax/macro-testing
         racket/exn
         rackunit
         (only-in "../testing.rkt" expand-nonterminal/datum))

;;
;; Helpers
;;

(define ((check-formatted-error-matches rx) exn)
  ;; I previously used exn->string, but that raised an error
  ;; re: writing special values when handling an ambiguous binding
  ;; error.
  (regexp-match? rx (exn-message exn)))

(define-syntax-rule (check-decl-error rx decl-stx)
  (check-exn
   (check-formatted-error-matches rx)
   (lambda ()
     (eval-syntax #`(module m racket/base
                      (require "../main.rkt")
                      decl-stx)))))

(define-syntax-rule (check-phase1-error rx e)
  (check-exn
   (check-formatted-error-matches rx)
   (lambda () (phase1-eval e #:catch? #t))))

(define-syntax-rule (check-syntax-error rx e)
  (check-exn
   (check-formatted-error-matches rx)
   (lambda () (convert-compile-time-error e))))

;;
;; Nonterminal declaration syntax errors
;;

(check-decl-error
 #rx"nonterminal: expected extension class name"
 (define-hosted-syntaxes
   (binding-class var #:description "var")
   (nonterminal expr
     #:allow-extension unbound-name
     v:var)))

(check-decl-error
 #rx"nesting-nonterminal: expected pattern variable binding for nested syntax"
 (define-hosted-syntaxes
   (nesting-nonterminal binding-group
     1)))

;;
;; Syntax spec syntax errors
;;

(check-decl-error
 #rx"nonterminal: expected a syntax spec term"
 (define-hosted-syntaxes
   (nonterminal expr
     1)))

(check-decl-error
 #rx"nonterminal: expected a reference to a binding class, syntax class, or nonterminal"
 (define-hosted-syntaxes
   (nonterminal expr
     x:unbound-name)))

(check-decl-error
 #rx"nonterminal: duplicate pattern variable"
 (define-hosted-syntaxes
   (binding-class dsl-var #:description "dsl-var")
   (nonterminal expr
     [x:dsl-var x:dsl-var])))

;;
;; Binding spec syntax errors
;;

(check-decl-error
 #rx"nonterminal: binding spec expected a reference to a pattern variable"
 (define-hosted-syntaxes
   (binding-class dsl-var #:description "DSL variable")
   (nonterminal expr
     x:dsl-var
     #:binding {y})))

(check-decl-error
 #rx"bind: expected a reference to a pattern variable"
 (define-hosted-syntaxes
   (binding-class dsl-var #:description "DSL variable")
   (nonterminal expr
     x:dsl-var
     #:binding {(bind y)})))

(check-decl-error
 #rx"nonterminal: nesting nonterminals may only be used with `nest`"
 (define-hosted-syntaxes
   (binding-class dsl-var #:description "DSL variable")
   (nonterminal expr
     b:binding-group
     #:binding b)
   (nesting-nonterminal binding-group (nested)
     [])))

(check-decl-error
 #rx"nest: expected pattern variable associated with a nesting nonterminal"
 (define-hosted-syntaxes
   (nonterminal expr
     (e:expr)
     #:binding (nest e []))))

(check-decl-error
 #rx"nest: expected more terms starting with binding spec term"
 (define-hosted-syntaxes
   (nonterminal expr
     b:expr
     #:binding (nest b))))

(check-decl-error
 #rx"bind: expected pattern variable associated with a binding class"
 (define-hosted-syntaxes
   (nonterminal expr
     b:expr
     #:binding (bind b))))

(check-decl-error
 #rx"recursive: expected pattern variable associated with a two-pass nonterminal"
 (define-hosted-syntaxes
   (nonterminal expr
     b:expr
     #:binding (recursive b))))


(check-decl-error
 #rx"nonterminal: exports may only occur at the top-level of a two-pass binding spec"
 (define-hosted-syntaxes
   (binding-class var #:description "var")
   (nonterminal expr
     v:var
     #:binding (export v))))

(check-decl-error
 #rx"nonterminal: variable binding must occur within a scope"
 (define-hosted-syntaxes
   (binding-class pvar)
   (nonterminal pat
     x:pvar
     #:binding (bind x))))

(check-decl-error
 #rx"nonterminal: recursive binding groups must occur within a scope"
 (define-hosted-syntaxes
   (binding-class var #:description "var")
   (two-pass-nonterminal def
     (define x:var e:expr)
     #:binding [(export x) e])
   (nonterminal expr
     (block d:def)
     #:binding (recursive d))))

(check-decl-error
 #rx"nonterminal: bindings must appear first within a scope"
 (define-hosted-syntaxes
   (binding-class var)
   (nonterminal expr
     (let x:var e:expr)
     #:binding {e (bind x)})))

(check-decl-error
 #rx"nonterminal: a recursive binding group must appear before references and subexpressions"
 (define-hosted-syntaxes
   (binding-class var)
   (two-pass-nonterminal def
     (define x:var e:expr)
     #:binding [(export x) e])
   (nonterminal expr
     (block d:def e:expr)
     #:binding {e (recursive d)})))

(check-decl-error
 #rx"nonterminal: only one recursive binding group may appear in a scope"
 (define-hosted-syntaxes
   (binding-class var)
   (two-pass-nonterminal def
     (define x:var e:expr)
     #:binding [(export x) e])
   (nonterminal expr
     (block d1:def d2:def)
     #:binding {(recursive d1) (recursive d2)})))

(check-decl-error
 #rx"exports must appear first in a two-pass spec"
 (define-hosted-syntaxes
   (binding-class var)
   (two-pass-nonterminal def
     (define x:var e:expr)
     #:binding [e (export x)])))

(check-decl-error
 #rx"re-exports must occur before references and subexpressions"
 (define-hosted-syntaxes
   (binding-class var)
   (two-pass-nonterminal def
     (define x:var d:def e:expr)
     #:binding [(export x) e (re-export d)])))

(check-decl-error
 #rx"nonterminal: each pattern variable must occur in the binding spec at most once"
 (define-hosted-syntaxes
   (nonterminal expr
     (begin e1:expr e2:expr)
     #:binding [e1 e1])))

;;
;; Valid definitions used to exercise errors
;;

(define-hosted-syntaxes
  (binding-class dsl-var1)
  (binding-class dsl-var2 #:description "DSL var")
  (extension-class dsl-macro1)
  (extension-class dsl-macro2 #:description "DSL macro")
  (nonterminal expr1
    #:allow-extension (dsl-macro1 dsl-macro2)
    n:number
    v:dsl-var2
    (dsl-begin e:expr1 ...+)
    [b:dsl-var2 e:expr1 ...+]
    #:binding {(bind b) e})
  (nonterminal expr2
    #:description "DSL expression"
    n:number)
  (nesting-nonterminal binding-group (tail)
    [v:dsl-var2 e:expr1]
    #:binding {(bind v) tail}))

(define-syntax m1
  (dsl-macro1
   (syntax-parser
     [(_ v e)
      #'[v e]])))

(define-syntax m2
  (dsl-macro2
   (syntax-parser
     [(_ v e)
      #'[v e]])))

(require (for-syntax racket/syntax))

(define-syntax (dsl-expr1 stx)
  (syntax-parse stx
    [(_ e)
     #`'#,((nonterminal-expander expr1) #'e)]))

(define-syntax (dsl-expr2 stx)
  (syntax-parse stx
    [(_ e)
     #`'#,((nonterminal-expander expr2) #'e)]))

;;
;; Improper use of phase1 names (nonterminals, binding class names, extension class names)
;;

(check-phase1-error
 #rx"expr1: nonterminals may only be referenced in nonterminal specifications"
 expr1)

(check-phase1-error
 #rx"dsl-var2: binding classes may only be referenced in nonterminal specifications"
 dsl-var2)

(check-phase1-error
 #rx"dsl-macro2: expected expression producing a macro transformer"
 dsl-macro2)

;;
;; Accessor syntax errors
;;

(check-phase1-error
 #rx"binding-class-constructor: expected a binding class name"
 (binding-class-constructor unbound-name))

(check-phase1-error
 #rx"nonterminal-expander: expected a nonterminal name"
 (nonterminal-expander unbound-name))

(check-phase1-error
 #rx"nonterminal-expander: only simple non-terminals may be used as entry points"
 (nonterminal-expander binding-group))

;;
;; Runtime (wrt the meta-DSL) errors
;;


;; Syntax not matching the nonterminal spec

;; Bad syntax
;; 1.The interface macro is named as the syntax raising the error
;; 2. In absence of description, the nonterminal name is used in the error
(check-syntax-error
 #rx"dsl-expr1: expected expr1"  
 (dsl-expr1 (foo)))

;; Bad reference given variable case
;; TODO: make reference errors in syntax productions appear to be raised by DSL expander
(check-syntax-error
 #rx"foo: not bound as DSL var"
 (dsl-expr1 foo))

;; Bad syntax in subexpression
(check-syntax-error
 #rx"dsl-expr1: expected expr1"
 (dsl-expr1 [foo (foo)]))

;; Bad reference in subexpression
(check-syntax-error
 #rx"bar: not bound as DSL var"
 (dsl-expr1 [foo bar]))

;; 1. Reference is bad syntax for nonterminal with no variable case
;; 2. Description is used when present
(check-syntax-error
 #rx"dsl-expr2: expected DSL expression"
 (dsl-expr2 foo))

;; No terms in ...+ position in form
(check-syntax-error
 ;; TODO: I should generate syntax classes corresponding to non-terminals
 ;; to improve errors like this. Ideally the message would be
 ;; "dsl-begin: expected more terms starting with dslexpr1"
 #rx"dsl-begin: expected more terms starting with any term"
 (dsl-expr1 (dsl-begin)))

;; No terms in ...+ position in syntax
;; TODO: this could be made nicer when there is only one possible syntax production that matches.
(check-syntax-error
 #rx"dsl-expr1: expected expr1"
 (dsl-expr1 [x]))

; expand-nonterminal/datum should be named as raising syntax
(check-syntax-error
 #rx"expand-nonterminal/datum: expected expr1"
 (expand-nonterminal/datum expr1 [x]))

(define-host-interface/expression
  (dsl-expr1-interface e:expr1)
  #`#,'#'e)

; interface macro should be named as raising syntax
(check-syntax-error
 #rx"dsl-expr1-interface: expected expr1"
 (dsl-expr1-interface [x]))

;; Use of DSL macro outside of DSL

;; Without description
(check-syntax-error
 #rx"m1: dsl-macro1 may not be used as a racket expression"
 (m1))

;; With description
(check-syntax-error
 #rx"m2: DSL macro may not be used as a racket expression"
 (m2))

;; Ensure expansion preserves source locations on the parens
;; of each form. (Note that it does not preserve source locations
;; of the parens within forms)
(require (for-syntax racket/syntax-srcloc))

(check-true
 (phase1-eval
  (let ([stx #'(dsl-begin 5)])
    (equal? (syntax-srcloc stx)
            (syntax-srcloc ((nonterminal-expander expr1) stx))))))

(check-true
 (phase1-eval
  (let* ([stx #'(dsl-begin (dsl-begin 5))]
         [expanded ((nonterminal-expander expr1) stx)])
    (equal? (syntax-srcloc (syntax-case stx () [(_ e) #'e]))
            (syntax-srcloc (syntax-case expanded () [(_ e) #'e]))))))

(define-hosted-syntaxes
  (nonterminal expr3
    #:binding-space dsl
    (foo)))

(let ()
  (define foo 5)
  (check-syntax-error
   #rx"foo: identifier's binding is ambiguous"
   (expand-nonterminal/datum expr3 (foo))))