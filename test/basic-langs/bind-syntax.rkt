#lang racket/base

; test bind-syntax

(require "../../testing.rkt")

; A simple expression language with expressions with an alternative let-syntax
; also includes numbers and + as non-host expressions.
; it uses racket macros
(syntax-spec
  (binding-class racket-var #:description "racket-like variable")
  
  (nonterminal racket-expr
    #:description "racket-like expr"
    #:allow-extension racket-macro

    n:number
    ((~literal +) e:racket-expr ...)
    (racket-let-syntax ([v:racket-macro e:expr] ...) b:racket-expr)
    #:binding {(bind-syntax v e) b}
    e:expr
    #:binding (host e))

  (host-interface/expression
    (eval-racket-expr e:racket-expr)
    (compile-racket-expr #'e)))

(begin-for-syntax
  (define (compile-racket-expr e)
    (syntax-parse e
      [((~literal racket-let-syntax) ([v e] ...) b)
       #`(let-syntax ([v e] ...) #,(compile-racket-expr #'b))]
      [_ this-syntax])))

; this dsl doesn't have host expressions. It only has numbers, variables, +, my-let, and my-let-syntax
; it only allows dsl macros
(syntax-spec
  (binding-class my-var #:description "my-expr variable")
  (extension-class my-macro)

  (nonterminal my-expr
    #:allow-extension my-macro

    n:number
    v:my-var
    ((~literal +) e:my-expr ...)

    (my-let ([v:my-var e:my-expr] ...) b:my-expr)
    #:binding {(bind v) b}

    (my-let-syntax ([v:my-macro e:expr] ...) b:my-expr)
    #:binding {(bind-syntax v e) b}

    ; this binds racket-macros, which cannot be used in my-exprs
    ; uses of bound macros should error.
    (bad-my-let-syntax ([v:racket-macro e:expr] ...) b:my-expr)
    #:binding {(bind-syntax v e) b}))

(test-equal?
 "local definition and use of a racket macro in a dsl expression"
 (expand-nonterminal/datum racket-expr
   (racket-let-syntax ([double (syntax-rules () [(double e) (+ e e)])])
                      (double 1)))
 '(racket-let-syntax ([double (syntax-rules () [(double e) (+ e e)])])
                     (+ 1 1)))

(test-equal?
 "local definition and use of a racket macro in a host expression"
 ; evaluation is necessary to resume host expansion.
 ; otherwise, we get a #%host-expression with (double x) not expanded yet.
 (eval-racket-expr
  (racket-let-syntax ([double (syntax-rules () [(double e) (+ e e)])])
                     (let ([x 1]) (double x))))
 2)

(test-equal?
 "local definition and use of a dsl macro"
 ; this tests that regular transformers are wrapped with my-macro automatically
 (expand-nonterminal/datum my-expr
   (my-let-syntax ([double (syntax-rules () [(double e) (+ e e)])])
                  (my-let ([x 1])
                          (double x))))
 '(my-let-syntax ([double (syntax-rules () [(double e) (+ e e)])])
                 (my-let ([x 1])
                         (+ x x))))

(test-exn
 "using a locally defined racket-macro where it is not allowed"
 #rx"expected my-expr"
 (lambda ()
   (expand-nonterminal/datum my-expr
     ; this form binds a racket-macro even though only my-macros are allowed in the body.
     (bad-my-let-syntax ([double (syntax-rules () [(double e) (+ e e)])])
                        (my-let ([x 1])
                                (double x))))))
