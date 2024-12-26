#lang racket/base

(provide (all-defined-out)
         (for-space ml (all-defined-out)))

(require
  syntax-spec

  (for-syntax
    racket/base 
    syntax/parse
    (only-in syntax-spec/private/ee-lib/main lookup in-space)))

(syntax-spec
  (binding-class ml-var
    #:reference-compiler (make-variable-like-reference-compiler
                           compile-RM))
    
  (nonterminal ml-expr
    #:binding-space ml
    (~> x:id
        #:when (not (lookup #'x (binding-class-predicate ml-var)))
        (syntax/loc #'x (MR x)))

    x:ml-var
    n:number
    (app e1:ml-expr e2:ml-expr)
    (+ e1:ml-expr e2:ml-expr)
    (- e1:ml-expr e2:ml-expr)
    (if0 e1:ml-expr e2:ml-expr e3:ml-expr)
    (lambda ([x:ml-var t:ml-type]) e:ml-expr)
    #:binding (scope (bind x) e)

    (MR e:racket-expr)
    (: e:ml-expr t:ml-type)

    (~> (e1 e2)
        #'(app e1 e2))
    
    )

  (nonterminal ml-type
    #:binding-space ml
    Nat
    (-> t1:ml-type t2:ml-type))

  (host-interface/expression
    (RM e:ml-expr)
    (compile-RM #'e))

  (host-interface/expression
    (ml e:ml-expr)
    (define-values (e^ t) (infer-type #'e))
    #`(ml->racket #,e^)))

(struct ml-value [v t])

(define (seal e t)
  (ml-value e t))

(define (unseal e t)
  (unless (ml-value? e)
    (error 'MR "not an ML value"))
  (let ([v (ml-value-v e)]
        [t (ml-value-t e)])
    (if (equal? t t)
       v
      (error 'unseal "type mismatch"))))

(begin-for-syntax
  (define (compile-RM e)
    (define-values (e^ t) (infer-type e))
    #`(seal (ml->racket #,e^) #'#,t))

  ;; No type variables yet, so should just be datum equality.
  (define (assert-type-equal! actual expected term)
    (unless (equal? (syntax->datum actual) (syntax->datum expected))
      (raise-syntax-error 'ml 
                         (format "type mismatch: expected ~a, found ~a" 
                                (syntax->datum expected)
                                (syntax->datum actual))
                         term
                         #f
                         (list actual expected))))

  (define-local-symbol-table type-env)

  (define (type-env-ref x)
    (symbol-table-ref type-env x #'Nat))

  (define (type-env-extend! x t)
    (symbol-table-set! type-env x t))
  
  (define (infer-type e)
    (syntax-parse e
      #:datum-literals (app + - if0 lambda MR)
      [x:id
       (values #'x (type-env-ref #'x))]
      [_:number
       (values e #'Nat)]
      [(app e1 e2)
       (define-values (e1^ t1) (infer-type #'e1))
       (syntax-parse t1
         #:datum-literals (->)
         [(-> argt rett)
          (define e2^ (check-type! #'e2 #'argt))
          (values #`(app #,e1^ #,e2^) #'rett)]
         [_
          (raise-syntax-error 'ml "type mismatch: expected function type" this-syntax)])]
      [(+ e1 e2)
       (define e1^ (check-type! #'e1 #'Nat))
       (define e2^ (check-type! #'e2 #'Nat))
       (values #`(+ #,e1^ #,e2^) #'Nat)]
      [(- e1 e2)
       (define e1^ (check-type! #'e1 #'Nat))
       (define e2^ (check-type! #'e2 #'Nat))
       (values #`(- #,e1^ #,e2^) #'Nat)]
      [(if0 e1 e2 e3)
       (define e1^ (check-type! #'e1 #'Nat))
       (define-values (e2^ t2) (infer-type #'e2))
       (define e3^ (check-type! #'e3 t2))
       (values #`(if0 #,e1^ #,e2^ #,e3^) t2)]
      [(lambda ([x xt]) b)
       (type-env-extend! #'x #'xt)
       (define-values (b^ rett) (infer-type #'b))
       (values #`(lambda ([x xt]) #,b^) #`(-> xt #,rett))]
      [(MR e)
       (raise-syntax-error 'ml "Cannot infer type in this position" this-syntax)]
      [(: e t)
       (define e^ (check-type! #'e #'t))
       (values e^ #'t)]))

  (define (check-type! e t)
    (syntax-parse e
      #:datum-literals (app MR)
      [(app e1 e2)
       (define-values (e2^ t2) (infer-type #'e2))
       (define e1^ (check-type! #'e1 #`(-> #,t2 #,t)))
       #`(app #,e1^ #,e2^)]
      [(MR e)
       #`(MR e #,t)]
      [_ 
       (define-values (e^ inferred-t) (infer-type e))
       (assert-type-equal! inferred-t t e)
       e^]))
)

(define-syntax ml->racket
  (syntax-parser
    #:datum-literals (app + - if0 lambda)
    [(_ x:id)
     #'x]
    [(_ n:number)
     #'n]
    [(_ (app e1 e2))
     #'((ml->racket e1) (ml->racket e2))]
    [(_ (+ e1 e2))
     #'(+ (ml->racket e1) (ml->racket e2))]
    [(_ (- e1 e2))
     #'(max (- (ml->racket e1) (ml->racket e2)) 0)]
    [(_ (if0 e1 e2 e3))
     #'(if (zero? (ml->racket e1)) (ml->racket e2) (ml->racket e3))]
    [(_ (lambda ([x t]) b))
     #'(lambda (x) (ml->racket b))]
    [(_ (MR e t))
     #'(unseal e #'t)]))
  

(module+ test
  (require syntax/macro-testing
           rackunit)

  (define-syntax-rule (check-type-error expr expected-msg)
    (check-exn
     (λ (e) (regexp-match? expected-msg (exn-message e)))
     (λ () (convert-compile-time-error expr))))

  (check-equal? (ml 5) 5)
  (check-equal? (ml (+ 1 2)) 3)
  (check-equal? (ml ((lambda ([x Nat]) x) 42)) 42)
  (check-equal? (ml ((lambda ([x Nat]) (- x (MR (RM 1)))) 0)) 0)
  (check-equal? (ml ((lambda ([f (-> Nat Nat)]) (f 5)) (lambda ([x Nat]) x))) 5)

  (check-equal? (let ([f (RM (lambda ([x Nat]) x))])
                  (ml (+ (f 5) 1)))
                6)
              
  (check-equal? (ml ((lambda ([x Nat]) (+ (MR (let ([v x]) v)) 1)) 5))
                6)

  (ml (lambda ([x Nat]) (+ (MR (RM x)) 1)))
  
  (check-type-error
   (ml (app (lambda ([x Nat]) x) (lambda ([y Nat]) y)))
   #rx"type mismatch: expected Nat")
  
  (check-type-error
   (ml (app 5 3))
   #rx"type mismatch: expected function type")
  
  (check-type-error
   (ml (if0 0 5 (lambda ([x Nat]) x)))
   #rx"type mismatch: expected Nat")

  (check-type-error
   (ml (MR 5))
   #rx"Cannot infer type in this position")

  (check-exn
   #rx"MR: not an ML value"
   (lambda () (ml (+ (MR 5) 1))))
  
  )
