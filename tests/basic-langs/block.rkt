#lang racket/base

;; `racket/block` implementation to test `export-syntax`.
;;
;; TODO: multiple macro definitions with single define-syntaxes.
;; TODO: implement begin?

(require "../../testing.rkt"
         drracket/check-syntax)

(syntax-spec
  (nonterminal/exporting block-form
    #:allow-extension racket-macro

    ((~literal define-values) (x:racket-var ...) e:racket-expr)
    #:binding [(export x) ...]

    ((~literal define-syntaxes) (x:racket-macro ...) e:expr)
    #:binding (export-syntaxes x ... e)

    e:racket-expr)

  (host-interface/expression
    (block body:block-form ...)
    #:binding (scope (import body ...))
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

(check-equal?
 (block
  (define-syntaxes (one m) (values (syntax-rules () [(one) 1])
                                   (syntax-rules () [(m stx) stx])))
  (m (one)))
 1)

;; TODO this test only works when run without already having built with raco make. Disable for now.
#;(define-namespace-anchor a)
#;(test-case "disappeared props"
  (define (num-arrows-of check-syntax-result)
    (length (for/list ([vec check-syntax-result] #:when (equal? (vector-ref vec 0)
                                                                'syncheck:add-arrow/name-dup/pxpy))
              vec)))
  (define ns (namespace-anchor->namespace a))
  (check-equal? (num-arrows-of
                 (show-content (quote-syntax (block
                                               (define-syntax x #'a)
                                               (define-syntax m
                                                 (syntax-parser
                                                   [(_ name:id)
                                                    (define/syntax-parse actual-name (syntax-local-value #'name))
                                                    (syntax-property #'(define actual-name 42) 'disappeared-use (list (syntax-local-introduce #'name)))]))
                                               (m x)))
                               #:namespace ns))
                2))
