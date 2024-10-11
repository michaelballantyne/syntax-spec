#lang racket/base

; test bind-syntax

(require "../../testing.rkt")

; A simple expression language with expressions with an alternative letrec-syntax
; also includes numbers and + as non-host expressions.
; it uses racket macros
(syntax-spec
  (binding-class racket-var #:description "racket-like variable")
  
  (nonterminal racket-like-expr
    #:description "racket-like expr"
    #:allow-extension racket-macro

    n:number
    ((~literal +) e:racket-like-expr ...)
    (racket-letrec-syntax ([v:racket-macro e:expr] ...) b:racket-like-expr)
    #:binding (scope (bind-syntax v e) ... b)

    (racket-letrec-syntaxes ([(v:racket-macro ...) e:expr] ...) b:racket-like-expr)
    #:binding (scope (bind-syntaxes v ... e) ... b)
    e:racket-expr)

  (host-interface/expression
    (eval-racket-expr e:racket-like-expr)
    (compile-racket-expr #'e)))

(begin-for-syntax
  (define (compile-racket-expr e)
    (syntax-parse e
      [((~literal racket-letrec-syntax) ([v e] ...) b)
       #`(letrec-syntax ([v e] ...) #,(compile-racket-expr #'b))]
      [((~literal racket-letrec-syntaxes) ([(v ...) e] ...) b)
       #`(letrec-syntaxes ([(v ...) e] ...) #,(compile-racket-expr #'b))]
      [_ this-syntax])))

; this dsl doesn't have host expressions. It only has numbers, variables, +, my-let, and my-letrec-syntax
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
    #:binding (scope (bind v) ... b)

    (my-letrec-syntax ([v:my-macro e:expr] ...) b:my-expr)
    #:binding (scope (bind-syntax v e) ... b)

    (my-letrec-syntaxes ([(v:my-macro ...) e:expr] ...) b:my-expr)
    #:binding (scope (bind-syntaxes v ... e) ... b)

    ; this binds racket-macros, which cannot be used in my-exprs
    ; uses of bound macros should error.
    (bad-my-letrec-syntax ([v:racket-macro e:expr] ...) b:my-expr)
    #:binding (scope (bind-syntax v e) ... b)

    (bad-my-letrec-syntaxes ([(v:racket-macro ...) e:expr] ...) b:my-expr)
    #:binding (scope (bind-syntaxes v ... e) ... b))

  (host-interface/expression
    (my-lang e:my-expr)
    #''e))

(test-equal?
 "local definition and use of a racket macro in a dsl expression"
 (expand-nonterminal/datum racket-like-expr
   (racket-letrec-syntax ([double (syntax-rules () [(double e) (+ e e)])])
                      (double 1)))
 '(racket-letrec-syntax ([double (syntax-rules () [(double e) (+ e e)])])
                     (+ 1 1)))

(test-equal?
 "local definition and use of a racket macro in a dsl expression"
 (expand-nonterminal/datum racket-like-expr
   (racket-letrec-syntaxes ([(double triple)
                          (values (syntax-rules () [(double e) (+ e e)])
                                  (syntax-rules () [(triple e) (+ e e e)]))])
     (double (triple 1))))
 '(racket-letrec-syntaxes ([(double triple)
                          (values (syntax-rules () [(double e) (+ e e)])
                                  (syntax-rules () [(triple e) (+ e e e)]))])
                     (+ (+ 1 1 1) (+ 1 1 1))))

(test-equal?
 "local definition and use of a racket macro in a host expression"
 ; evaluation is necessary to resume host expansion.
 ; otherwise, we get a #%host-expression with (double x) not expanded yet.
 (eval-racket-expr
  (racket-letrec-syntax ([double (syntax-rules () [(double e) (+ e e)])])
                     (let ([x 1]) (double x))))
 2)

(test-equal?
 "local definition and use of a dsl macro"
 ; this tests that regular transformers are wrapped with my-macro automatically
 (expand-nonterminal/datum my-expr
   (my-letrec-syntax ([double (syntax-rules () [(double e) (+ e e)])])
                  (my-let ([x 1])
                          (double x))))
 '(my-letrec-syntax ([double (syntax-rules () [(double e) (+ e e)])])
                 (my-let ([x 1])
                         (+ x x))))

(test-equal?
 "check that a self-recursive macro works"
 (expand-nonterminal/datum my-expr
   (my-letrec-syntax ([my-let*
                        (syntax-rules ()
                          [(my-let* () b) b]
                          [(my-let* ([v e] binding ...) b)
                           (my-let ([v e])
                             (my-let* (binding ...) b))])])
                      (my-let* ([x 1] [x (+ x 1)]) x)))
 '(my-letrec-syntax ([my-let*
                       (syntax-rules ()
                         [(my-let* () b) b]
                         [(my-let* ([v e] binding ...) b)
                          (my-let ([v e])
                            (my-let* (binding ...) b))])])
                     (my-let ([x 1]) (my-let ([x (+ x 1)]) x))))

(test-equal?
 "local definition and use of a dsl macro"
 ; this tests that regular transformers are wrapped with my-macro automatically
 (expand-nonterminal/datum my-expr
                           (my-letrec-syntaxes ([(double triple)
                                              (values (syntax-rules () [(double e) (+ e e)])
                                                      (syntax-rules () [(triple e) (+ e e e)]))])
                             (my-let ([x 1])
                               (double (triple x)))))
 '(my-letrec-syntaxes ([(double triple)
                     (values (syntax-rules () [(double e) (+ e e)])
                             (syntax-rules () [(triple e) (+ e e e)]))])
    (my-let ([x 1])
            (+ (+ x x x) (+ x x x)))))

(test-exn
 "using a locally defined racket-macro where it is not allowed"
 #rx"expected my-expr"
 (lambda ()
   (expand-nonterminal/datum my-expr
     ; this form binds a racket-macro even though only my-macros are allowed in the body.
     (bad-my-letrec-syntax ([double (syntax-rules () [(double e) (+ e e)])])
                        (my-let ([x 1])
                                (double x))))))

(test-exn
 "using a locally defined racket-macro where it is not allowed"
 #rx"expected my-expr"
 (lambda ()
   (expand-nonterminal/datum my-expr
     ; this form binds a racket-macro even though only my-macros are allowed in the body.
     (bad-my-letrec-syntaxes ([(double) (syntax-rules () [(double e) (+ e e)])])
       (my-let ([x 1])
         (double x))))))
