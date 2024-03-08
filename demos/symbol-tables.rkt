#lang racket

(module lang racket
  (require "../main.rkt")
  (provide my-define my-ref Number List)

  (begin-for-syntax
    (define-persistent-symbol-table types))
  
  (syntax-spec
    (nonterminal type
      Number
      (List t:type))

    (binding-class my-var)
    
    (host-interface/definition
     (my-define x:my-var t:type)
     #:binding (export x)
     #:lhs [(symbol-table-set! types #'x (syntax->datum #'t))
            #'x]
     #:rhs [#'7])
    
    (host-interface/expression
     (my-ref x:my-var t:type)
     (when (not (equal? (symbol-table-ref types #'x) (syntax->datum #'t)))
       (raise-syntax-error #f "type error" #'x))
     #'x)))

(module def racket
  (require (submod ".." lang))
  (provide x)
  (my-define x Number)
  ;; =>
  #;(begin
      (define-syntax x (syntax-spec-binding my-var))
      (define x^ 7)))

(module ref racket
  (require (submod ".." lang) (rename-in (submod ".." def) [x y]))
  (my-ref y (List Number))
  ;; =>
  #;x^)

(require 'ref)

#;(
   (my-define x t)
   ->
   (begin
     (define x^ 7)
     (begin-for-syntax
       (free-id-table-set! ids #'x #'x^)))

   )