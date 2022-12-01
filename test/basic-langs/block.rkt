#lang racket/base

; racket/block implementation to test for export-syntax
; ignores begin

(require "../../testing.rkt")

(define-hosted-syntaxes
  (binding-class rkt-var)
  (two-pass-nonterminal block-form
                        #:allow-extension racket-macro

                        ((~literal define-values) (x:rkt-var ...) e:expr)
                        #:binding [(export x) (host e)]

                        ; currently can only express single macro definitions
                        ((~literal define-syntaxes) (x:racket-macro) e:expr)
                        #:binding (export-syntax x e)

                        e:expr
                        #:binding (host e)))

; TODO begin?

; like a (non-splicing) begin, but the last form may be a definition
(define-host-interface/expression (block body:block-form ...)
  #:binding {(recursive body)}
  (displayln (attribute body))
  (let ([res (syntax-parse #'(body ...)
               ; empty block
               [() #'(void)]
               [(_ ... last)
                (define last-is-definition?
                  (syntax-parse #'last
                    #:literals (define-values define-syntaxes)
                    [(~or (define-values . _) (define-syntaxes . _)) #t]
                    [_ #f]))
                (syntax-parse #'(body ...)
                  #:literals (define-values define-syntaxes)
                  [((~alt (define-values (val-id ...) val-expr)
                          (define-syntaxes (trans-id) trans-expr)
                          expr)
                    ...)
                   #`(letrec-syntaxes+values
            ([(trans-id) trans-expr] ...)
            ([(val-id ...) val-expr] ...)
                       expr
                       ...
                       #,@(if last-is-definition?
                              ; this allows the last form to be a definition
                              ; last is a definition, put void at the end
                              #'((void))
                              ; last is an expr, leave it be
                              #'()))])])])
    (displayln res)
    res))

(check-equal?
 (block 1) 1)
;;; left off here debugging this. (map/trees f '(()))
;;; used to error from (transpose '()). After "fixing" that,
;;; we get a different error. Is this just a problem for (some-host-interface-macro) with no args?
;;; you should comprehend those first two map/trees unit tests too.
(check-equal? (block) (void))
(check-equal? (block (define (f) (g))
                     (define (g) 2)
                     (f))
              2)
(check-equal? (block 1 (define x 2))
              (void))
(check-equal? (block (define-syntax-rule (m) (f))
                     (define (f) 2)
                     (m))
              2)
