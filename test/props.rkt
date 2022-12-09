#lang racket/base

(require "../testing.rkt")

(syntax-spec
  (extension-class dsl-macro)
  (nonterminal dsl-expr
    #:allow-extension dsl-macro
    form
    (form)))


(define-syntax attach-property-to-argument
  (dsl-macro
   (syntax-parser
     [(_ f)
      (syntax-property #'f 'foo 'bar)])))

(define-syntax identity-macro
  (dsl-macro
   (syntax-parser
     [(_ f)
      #'f])))

(syntax-spec
  (host-interface/expression
    (check-for-property e:dsl-expr)
    #`'#,(syntax-property #'e 'foo)))


(check-equal?
 (check-for-property (attach-property-to-argument (identity-macro (form))))
 'bar)

(check-equal?
 (check-for-property (attach-property-to-argument (identity-macro form)))
 'bar)