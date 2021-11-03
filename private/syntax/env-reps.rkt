#lang racket/base

(provide (struct-out bindclass-rep)
         (struct-out extclass-rep)
         (struct-out nonterm-rep)
         
         (struct-out simple-nonterm-info)
         (struct-out nesting-nonterm-info)
         (struct-out two-pass-nonterm-info)
         
         (struct-out nested-binding)
         (struct-out pvar-rep))

(require syntax/parse
         "../runtime/errors.rkt"      
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
       #'(constr e))]))

(struct bindclass-rep (description constr pred)
  #:property prop:procedure
  (nonterm-lang-error-as-expression "binding classes"))

(struct extclass-rep (constr pred acc)
  #:property prop:procedure
  expand-as-constructor)

(struct nonterm-rep (litset-ref variant-info)
  #:property prop:procedure
  (nonterm-lang-error-as-expression "nonterminals"))

(struct simple-nonterm-info (expander))
(struct nesting-nonterm-info (expander))
(struct two-pass-nonterm-info (pass1-expander pass2-expander))

; var-info is one of:
;   bindclass-rep
;   nonterm-rep
;   sequence-nonterm-rep
;   nested-binding
(struct pvar-rep (var-info))

(struct nested-binding [])