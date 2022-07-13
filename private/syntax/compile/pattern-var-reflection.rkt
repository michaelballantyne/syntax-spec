#lang racket/base

(provide
 ;; (rebind-pattern-vars (v ...) e b)
 ;;
 ;; Rebind the pattern variables `v ...` to the values returned by `e`, for use in `b`.
 ;; Pattern vars must be bound as syntax parse attributes, not just syntax-case pattern vars.

 ;; `e` must return a value for each pattern variable `v`, where the value has nested
 ;; list structure matching the depth associated with the pattern variable.
 rebind-pattern-vars)

(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     (only-in racket/private/sc
                              make-syntax-mapping
                              syntax-pattern-variable?
                              syntax-mapping-depth
                              syntax-mapping-valvar))
         (only-in racket/private/template
                  attribute-mapping
                  attribute-mapping?
                  attribute-mapping-name
                  attribute-mapping-var
                  attribute-mapping-depth
                  attribute-mapping-check)
         (only-in syntax/parse/private/residual
                  check-attr-value))

(define-syntax rebind-pattern-vars
  (syntax-parser
    [(_ (var ...) rhs body)
     #:with (new-val ...) (generate-temporaries #'(var ...))
     #:with (attr-var ...) (generate-temporaries #'(var ...))
     #:with (attr-mapping ...) (map reflect-attribute-mapping (attribute var) (attribute new-val))
     #:with (stx-mapping ...) (map reflect-syntax-mapping (attribute var) (attribute attr-var))

     #'(let-values ([(new-val ...) rhs])
         (letrec-syntaxes+values ([(attr-var) attr-mapping]
                                  ...) ()
           (letrec-syntaxes+values ([(var) stx-mapping]
                                    ...) ()
             body)))]))

(begin-for-syntax
  (define (reflect-attribute-mapping var new-var)
    (define stx-mapping (get-pvar-info var))

    (define (unbound-error) (wrong-syntax var "expected pattern variable with attribute binding"))
    
    (let ([attr-mapping (syntax-local-value (syntax-mapping-valvar stx-mapping) unbound-error)])
      #`(attribute-mapping
         (quote-syntax #,new-var)
         '#,(attribute-mapping-name attr-mapping)
         '#,(attribute-mapping-depth attr-mapping)
         ;; Have templates always check the contents of the rebinding
         (quote-syntax check-attr-value))))
    
  (define (reflect-syntax-mapping var new-valvar)
    (define existing-mapping (get-pvar-info var))
    #`(make-syntax-mapping
       '#,(syntax-mapping-depth existing-mapping)
       (quote-syntax #,new-valvar)))

  (define (get-pvar-info v)
    (define (unbound-error) (wrong-syntax v "expected pattern var"))
    (define binding (syntax-local-value v unbound-error))
    (when (not (syntax-pattern-variable? binding))
      (unbound-error))
    binding))

