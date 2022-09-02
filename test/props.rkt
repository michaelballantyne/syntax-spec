#lang racket/base

(require bindingspec
         rackunit
         (for-syntax racket/base syntax/parse))

(define-hosted-syntaxes
  (extension-class dsl-macro)
  (nonterminal dsl-expr
    #:allow-extension dsl-macro
    (form)))

(define-syntax m
  (dsl-macro
   (syntax-parser
     [(_ f)
      (syntax-property #'f 'foo 'bar)])))

(define-syntax m2
  (dsl-macro
   (syntax-parser
     [(_ f)
      #'f])))

(define-host-interface/expression
  (dsl e:dsl-expr)
  (syntax-parse #'e
    #:literals (form)
    [(form)
     #`'#,(syntax-property this-syntax 'foo)]))

(check-equal?
 (dsl (m (m2 (form))))
 'bar)