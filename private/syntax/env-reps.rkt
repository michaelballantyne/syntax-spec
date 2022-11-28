#lang racket/base

(provide (struct-out bindclass-rep)
         (struct-out extclass-rep)
         (struct-out nonterm-rep)
         
         (struct-out simple-nonterm-info)
         (struct-out nesting-nonterm-info)
         (struct-out two-pass-nonterm-info)
         
         (struct-out nested-binding)
         (struct-out pvar-rep)

         stxclass-rep?)

(require syntax/parse
         "../runtime/errors.rkt"
         racket/syntax
         ee-lib/syntax-category
         (only-in syntax/parse/private/residual-ct stxclass? has-stxclass-prop?)
         (for-template racket/base))

(define (nonterm-lang-error-as-expression type)
  (struct-error-as-expression
   (string-append
    type
    " may only be referenced in nonterminal specifications")))

;; When used as an expression, extension class names act as contructors
;; for macros belonging to the extension class.
(define (expand-as-constructor s stx)
  (syntax-parse stx
    [(_ e)
     (with-syntax ([constr (extclass-rep-constr s)])
       #'(constr e))]
    [_
     (wrong-syntax stx "expected expression producing a macro transformer")]))

#;(Any -> boolean?)
; Is the value an expander environment representative of a syntax class?
; Used to recognize something like e:expr
(define (stxclass-rep? v)
  (or (stxclass? v)
      (has-stxclass-prop? v)))

(struct bindclass-rep (description constr pred binding-space)
  #:property prop:procedure
  (nonterm-lang-error-as-expression "binding classes")
  #:property prop:not-racket-syntax #t)

(struct extclass-rep (constr pred acc binding-space)
  #:property prop:procedure
  expand-as-constructor
  #:property prop:not-racket-syntax #t)

(struct nonterm-rep (variant-info)
  #:property prop:procedure
  (nonterm-lang-error-as-expression "nonterminals")
  #:property prop:not-racket-syntax #t)

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
