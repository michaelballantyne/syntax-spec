#lang racket/base

(provide (struct-out bindclass-rep)
         (struct-out extclass-rep)
         (struct-out nonterm-rep))

(require syntax/parse
         "errors.rkt"      
         (for-template racket/base))

(define (nonterm-lang-error-as-expression type)
  (error-as-expression
   (string-append
    type
    " may only be referenced in nonterminal specifications")))

;; When used as an expression, extension class names act as contructors
;; for macros belonging to the extension class.
(define (expand-as-constructor s stx)
  (syntax-parse stx
    [(_ e)
     (with-syntax ([constr (extclass-rep-constr s)])
       #`(constr e))]))

(struct bindclass-rep (description constr pred)
  #:property prop:procedure
  (nonterm-lang-error-as-expression "binding classes"))

(struct extclass-rep (constr pred acc)
  #:property prop:procedure
  expand-as-constructor)

(struct nonterm-rep (exp-proc)
  #:property prop:procedure
  (nonterm-lang-error-as-expression "nonterminals"))
