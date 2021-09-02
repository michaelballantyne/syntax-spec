#lang racket/base

(provide rebind-pattern-vars
         pattern-var-value)

(require (for-syntax racket/base
                     syntax/parse
                     (only-in racket/private/sc
                              make-syntax-mapping
                              syntax-pattern-variable?
                              syntax-mapping-depth
                              syntax-mapping-valvar)))


(define-for-syntax (get-pvar-info v)
  (define (unbound-error) (raise-syntax-error #f "not bound as pattern var" v))
  (define binding (syntax-local-value v unbound-error))
  (when (not (syntax-pattern-variable? binding))
                        (unbound-error))
  binding)

(define-syntax pattern-var-value
  (syntax-parser
    [(_ v:id)
     (syntax-mapping-valvar (get-pvar-info #'v))]))

; invariant: rhs must return a value for each var, where the value has nested list structure
; (not syntax lists) matching the pattern variable depth.
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