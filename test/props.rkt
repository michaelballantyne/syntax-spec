#lang racket/base

(require bindingspec
         rackunit
         (for-syntax racket/base syntax/parse))

(define-hosted-syntaxes
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

(define-host-interface/expression
  (check-for-property e:dsl-expr)
  #`'#,(syntax-property #'e 'foo))


(check-equal?
 (check-for-property (attach-property-to-argument (identity-macro (form))))
 'bar)

(check-equal?
 (check-for-property (attach-property-to-argument (identity-macro form)))
 'bar)