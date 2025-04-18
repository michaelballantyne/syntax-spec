#lang racket/base

;; Similar to private/test/simple-bsepc.rkt, but attempting to create an expander that
;; uses syntax classes for nonterminals.
;; This was created to research a potential solution for
;; https://github.com/michaelballantyne/syntax-spec/issues/62

(require
  (for-syntax
   racket/base
   syntax/parse
   "../ee-lib/main.rkt"
   "../runtime/binding-spec.rkt")

  "../ee-lib/define.rkt")

#;;; ANF language
(syntax-spec
  (binding-class my-var)
  (nonterminal a-expr
    e:c-expr
    (my-let ([x:my-var e:c-expr]) b:a-expr)
    #:binding (scope (bind x) b))
  (nonterminal c-expr
    e:i-expr
    (my-+ a:i-expr b:i-expr))
  (nonterminal i-expr
    n:number
    x:my-var))

(define-literal-forms anf-lits
  "mylang forms may only be used in mylang"
  (my-let my-+))

(begin-for-syntax
  (struct my-var-rep ())

  ;; hash from symbols to binding representations
  ;; to simulate the real expander environment
  (define pretend-binding-store (make-hash))
  (hash-set! pretend-binding-store 'bogus #f)

  (define-syntax-class a-expr
    #:literal-sets (anf-lits)
    (pattern e:c-expr
             #:attr expanded #'e.expanded)
    (pattern (my-let ([x:my-var-bind e:c-expr]) b:a-expr)
             #:attr expanded #'(my-let ([x.expanded e.expanded]) b.expanded)))

  (define-syntax-class c-expr
    #:literal-sets (anf-lits)
    (pattern e:i-expr
             #:attr expanded (attribute e.expanded))
    (pattern (my-+ a:i-expr b:i-expr)
             #:attr expanded #'(my-+ a.expanded b.expanded)))

  (define-syntax-class i-expr
    #:literal-sets (anf-lits)
    (pattern n:number
             #:attr expanded #'n)
    (pattern x:my-var-ref
             #:attr expanded #'x.expanded))

  (define-syntax-class my-var-bind
    (pattern x:id
             #:fail-when (hash-has-key? pretend-binding-store (syntax->datum #'x)) "duplicate binding"
             ;; we need the do because the bind! needs to happen before the body expands.
             ;; attrs are eagerly evaluated when a pattern with a syntax class is matched,
             ;; so the bind! needs to happen before the body is even matched.
             #:do [(hash-set! pretend-binding-store (syntax->datum #'x) (my-var-rep))]
             #:attr expanded #'x))

  (define-syntax-class my-var-ref
    (pattern x:id
             #:fail-unless (hash-has-key? pretend-binding-store (syntax->datum #'x)) "unbound variable"
             #:fail-unless (my-var-rep? (hash-ref pretend-binding-store (syntax->datum #'x))) "expected a my-var"
             #:attr expanded #'x)))

(define-syntax (mylang stx)
  (syntax-parse stx
    [(_ e:a-expr)
     #''e.expanded]))

(require rackunit syntax/macro-testing)

(define-syntax-rule (check-success e)
  (check-equal? (mylang e)
                'e))
(define-syntax-rule (check-failure e msg)
  (check-exn
   msg
   (lambda ()
     (convert-compile-time-error (mylang e)))))

(check-success 1)
(check-success (my-+ 1 2))
(check-success (my-let ([x 1]) x))
(check-failure
 y
 #rx"unbound var")
(check-failure
 (my-let ([a 1])
   (my-let ([a 2])
     3))
 #rx"duplicate binding")
(check-failure
 bogus
 #rx"expected a my-var")
(check-failure
 (my-+ (my-let ([z 1]) z) 2)
 #rx"expected number or expected identifier")
