#lang racket/base

(provide compile-bspec)

(require racket/match
         syntax/parse
         syntax/parse/class/paren-shape
         syntax/id-table
         syntax/id-set
         racket/list
         "../env-reps.rkt"
         "../syntax-classes.rkt"
         (for-template racket/base
                       "../../runtime/binding-spec.rkt")
         )

(define (compile-bspec maybe-bspec varmap)
  (define/syntax-parse bspec (or maybe-bspec #'[]))

  (define compile-bspec-term
    (syntax-parser
      #:context 'compile-bspec-term
      #:datum-literals (! ^)
      [v:nonref-id
       (define binding (bound-id-table-ref varmap #'v #f))
       (when (not binding)
         (raise-syntax-error #f "no corresponding pattern variable" #'v))
       (match binding
         [(nonterm-rep exp-proc _)
          #`(subexp 'v #,exp-proc)]
         [(bindclass-rep description _ pred)
          #`(ref 'v #,pred #,(string-append "not bound as " description))])]
      [(! v:nonref-id ...+)
       (with-syntax
           ([(v-c ...)
             (for/list ([v (syntax->list #'(v ...))])
               (define binding (bound-id-table-ref varmap v #f))
               (when (not binding)
                 (raise-syntax-error #f "no corresponding pattern variable" v))
               (match binding
                 [(bindclass-rep _ constr _)
                  #`(bind '#,v #,constr)]))])
         #'(group (list v-c ...)))]
      [(^ v:ref-id ...+)
       (raise-syntax-error #f "^ not implemented yet" this-syntax)]
      [(~braces spec ...)
       (with-syntax ([(spec-c ...) (map compile-bspec-term (attribute spec))])
         #'(scope (group (list spec-c ...))))]
      [(~brackets spec ...)
       (with-syntax ([(spec-c ...) (map compile-bspec-term (attribute spec))])
         #'(group (list spec-c ...)))]))
            
  
      
  (define/syntax-parse (unreferenced-pvars ...)
    (bound-id-set->list
     (bound-id-set-subtract
      (immutable-bound-id-set (bound-id-table-keys varmap))
      (immutable-bound-id-set (bspec-referenced-pvars #'bspec)))))
      
  (compile-bspec-term #'[unreferenced-pvars ... bspec]))

(define bspec-referenced-pvars
  (syntax-parser
    #:context 'bspec-referenced-pvars
    #:datum-literals (! ^)
    [v:nonref-id
     (list #'v)]
    [(! v:nonref-id ...+)
     (attribute v)]
    [(^ v:ref-id ...+)
     (attribute v)]
    [(~braces spec ...)
     (flatten (map bspec-referenced-pvars (attribute spec)))]
    [(~brackets spec ...)
     (flatten (map bspec-referenced-pvars (attribute spec)))]))