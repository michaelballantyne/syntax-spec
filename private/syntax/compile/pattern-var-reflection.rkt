#lang racket/base

(provide
 ; (pattern-var-value v)
 ;
 ; Access the nested list of syntax objects assocated with pattern variable `v`.
 pattern-var-value

 ; (rebind-pattern-vars (v ...) e b)
 ;
 ; Rebind the pattern variables `v ...` to the values returned by `e`, for use in `b`.
 ;
 ; `e` must return a value for each pattern variable `v`, where the value has nested
 ; list structure matching the depth associated with the pattern variable.
 rebind-pattern-vars
 )

(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     (only-in racket/private/sc
                              make-syntax-mapping
                              syntax-pattern-variable?
                              syntax-mapping-depth
                              syntax-mapping-valvar)))

(define-syntax pattern-var-value
  (syntax-parser
    [(_ v:id)
     (syntax-mapping-valvar (get-pvar-info #'v))]))

(define-syntax rebind-pattern-vars
  (syntax-parser
    [(_ (var ...) rhs body)
     (with-syntax ([(depth ...)
                    (for/list ([v (syntax->list #'(var ...))])
                      (syntax-mapping-depth (get-pvar-info v)))]
                   [(new-val ...) (generate-temporaries #'(var ...))])
       #'(let-values ([(new-val ...) rhs])
           (let-syntaxes ([(var ...) (values (make-syntax-mapping 'depth #'new-val) ...)])
                         body)))]))

(define-for-syntax (get-pvar-info v)
  (define (unbound-error) (wrong-syntax v "expected pattern var"))
  (define binding (syntax-local-value v unbound-error))
  (when (not (syntax-pattern-variable? binding))
    (unbound-error))
  binding)