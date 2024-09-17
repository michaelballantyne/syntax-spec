#lang racket/base

(require "../main.rkt" (for-syntax racket syntax/parse) rackunit)

(syntax-spec
  (binding-class var)
  (extension-class pat-macro)
  
  (nonterminal dsl-expr
    v:var)
  
  (nonterminal/nesting pat (nested)
    #:allow-extension pat-macro
    
    v:var
    #:binding (scope (bind v) nested))

  (host-interface/expression
    (my-match [p:pat e:dsl-expr])
    #:binding (nest p e)
    #''success))

;; I'm not sure why, but the problem didn't occur at the module level. Perhaps
;; the racket/base module-begin doing something?
(let ()
  (define-syntax m (pat-macro (syntax-rules () [(_ a) a])))
  (check-equal?
   (my-match [(m x) x])
   'success))

(syntax-spec
  (nonterminal my-expr
    (block d:my-def ...)
    #:binding (scope (import d ...)))
 
  (nonterminal/exporting my-def
    ((~literal define-syntax) x:pat-macro e:expr)
    #:binding (export-syntax x e)
    ((~literal my-match) [p:pat e:dsl-expr])
    #:binding (nest p e))
 
  (host-interface/expression
    (eval-my-expr e:my-expr)
    #''success))

; TODO: known bug with bind-syntax and nest.
#;(check-equal?
   (eval-my-expr (block (define-syntax m (syntax-rules () [(_ a) a]))
                        (my-match [(m x) x])))
   'success)
