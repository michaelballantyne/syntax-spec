#lang racket/base

;; `racket/block` implementation to test `export-syntax`.
;;
;; TODO: multiple macro definitions with single define-syntaxes.
;; TODO: implement begin?

(require "../../testing.rkt")

(syntax-spec
  (nonterminal/two-pass block-form
    #:allow-extension racket-macro

    ((~literal define-values) (x:racket-var ...) e:expr)
    #:binding [(export x) (host e)]

    ((~literal define-syntaxes) (x:racket-macro) e:expr)
    #:binding (export-syntax x e)

    e:expr
    #:binding (host e))

  (host-interface/expression
    (block body:block-form ...)
    #:binding {(recursive body)}
    #'(compile-block body ...)))

(define-syntax compile-block
  (syntax-parser
    #:literals (define-values define-syntaxes)
    [(_) #'(void)]
    [(_ body:expr ... (~and last ((~or define-values define-syntaxes) . _)))
     #'(compile-block body ... last (void))]
    [(_ (~alt (define-values (val-id ...) val-expr)
              (define-syntaxes (trans-id) trans-expr)
              expr)
        ...)
     #`(letrec-syntaxes+values ([(trans-id) trans-expr] ...)
         ([(val-id ...) val-expr] ...)
         expr
         ...)]))

(check-equal?
 (block 1)
 1)

(check-equal?
 (block)
 (void))

(check-equal?
 (block (define (f) (g))
        (define (g) 2)
        (f))
 2)

(check-equal?
 (block 1
        (define x 2))
 (void))

(check-equal?
 (block (define-syntax-rule (m) (f))
        (define (f) 2)
        (m))
 2)