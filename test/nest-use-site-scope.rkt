#lang racket/base

(require "../main.rkt" (for-syntax racket syntax/parse) rackunit)

(define-hosted-syntaxes
  (binding-class var)
  (extension-class pat-macro)
  
  (nonterminal dsl-expr
    v:var)
  
  (nesting-nonterminal pat (nested)
    #:allow-extension pat-macro
    
    v:var
    #:binding {(bind v) nested}))

(define-host-interface/expression
  (my-match [p:pat e:dsl-expr])
  #:binding (nest-one p e)
  #''success)

;; I'm not sure why, but the problem didn't occur at the module level. Perhaps
;; the racket/base module-begin doing something?
(let ()
  (define-syntax m (pat-macro (syntax-rules () [(_ a) a])))
  (check-equal?
   (my-match [(m x) x])
   'success))