#lang racket/base

(provide compile-bspec)

(require racket/match
         syntax/parse
         syntax/parse/class/paren-shape
         syntax/id-table
         syntax/id-set
         racket/list
         (only-in syntax/parse/private/residual-ct stxclass? has-stxclass-prop?)
         ee-lib
         "../env-reps.rkt"
         "../syntax-classes.rkt"
         (for-template racket/base
                       "../../runtime/binding-spec.rkt")
         )

(define (compile-bspec maybe-bspec sspec-pvars)
  (define/syntax-parse bspec (or maybe-bspec #'[]))

  (define compile-bspec-term
    (syntax-parser
      #:context 'compile-bspec-term
      #:datum-literals (! ^ nest)
      [v:nonref-id
       (define binding (lookup #'v pvar-rep?))
       (when (not binding)
         (raise-syntax-error #f "no corresponding pattern variable" #'v))
       (match (pvar-rep-var-info binding)
         [(nonterm-rep exp-proc _)
          #`(subexp 'v #,exp-proc)]
         [(bindclass-rep description _ pred)
          #`(ref 'v #,pred #,(string-append "not bound as " description))]
         [(continuation-binding rtvar)
          #`(continue #,rtvar)]
         [(? sequence-nonterm-rep?)
          (raise-syntax-error #f "sequence nonterminals may only be used with fold" #'v)]
         [(or (? stxclass?) (? has-stxclass-prop?))
          #`(group (list))])]
      [(! v:nonref-id ...+)
       (with-syntax
           ([(v-c ...)
             (for/list ([v (syntax->list #'(v ...))])
               (define binding (lookup v pvar-rep?))
               (when (not binding)
                 (raise-syntax-error #f "no corresponding pattern variable" v))
               (match binding
                 [(pvar-rep (bindclass-rep _ constr _))
                  #`(bind '#,v #,constr)]))])
         #'(group (list v-c ...)))]
      [(^ v:nonref-id ...+)
       (raise-syntax-error #f "^ not implemented yet" this-syntax)]
      [(nest v:nonref-id spec)
       (define binding (lookup (attribute v) pvar-rep?))
       (when (not binding)
         (raise-syntax-error #f "no corresponding pattern variable" #'v))
       (when (not (sequence-nonterm-rep? (pvar-rep-var-info binding)))
         (raise-syntax-error 'fold "not a sequence nonterminal" #'v))
       (with-syntax ([spec-c (compile-bspec-term (attribute spec))])
         #`(seq-fold 'v #,(sequence-nonterm-rep-exp-proc (pvar-rep-var-info binding)) spec-c))]
      [(~braces spec ...)
       (with-syntax ([(spec-c ...) (map compile-bspec-term (attribute spec))])
         #'(scope (group (list spec-c ...))))]
      [(~brackets spec ...)
       (with-syntax ([(spec-c ...) (map compile-bspec-term (attribute spec))])
         #'(group (list spec-c ...)))]))

  (define/syntax-parse (unreferenced-pvars ...)
    (bound-id-set->list
     (bound-id-set-subtract
      sspec-pvars
      (immutable-bound-id-set (bspec-referenced-pvars #'bspec)))))
      
  (compile-bspec-term #'[unreferenced-pvars ... bspec]))

(define bspec-referenced-pvars
  (syntax-parser
    #:context 'bspec-referenced-pvars
    #:datum-literals (! ^ nest)
    [v:nonref-id
     (list #'v)]
    [(! v:nonref-id ...+)
     (attribute v)]
    [(^ v:ref-id ...+)
     (attribute v)]
    [(nest v e)
     (cons (attribute v) (bspec-referenced-pvars (attribute e)))]
    [(~braces spec ...)
     (flatten (map bspec-referenced-pvars (attribute spec)))]
    [(~brackets spec ...)
     (flatten (map bspec-referenced-pvars (attribute spec)))]))