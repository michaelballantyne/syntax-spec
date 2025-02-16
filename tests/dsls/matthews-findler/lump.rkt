#lang racket/base

(require syntax-spec-v3 (for-syntax racket/base syntax/parse))

(syntax-spec
  (binding-class ml-var #:binding-space ml)
    
  (nonterminal ml-expr
    #:binding-space ml
    x:ml-var
    n:number
    (app e1:ml-expr e2:ml-expr)
    (+ e1:ml-expr e2:ml-expr)
    (- e1:ml-expr e2:ml-expr)
    (if0 e1:ml-expr e2:ml-expr e3:ml-expr)
    (lambda ([x:ml-var t:ml-type]) e:ml-expr)
    #:binding (scope (bind x) e)

    (MR e:racket-expr t:ml-type)

    (~> (e1 e2)
        #'(app e1 e2)))

  (nonterminal ml-type
    #:binding-space ml
    Nat
    (-> t1:ml-type t2:ml-type))

  (host-interface/expression
    (RM e:ml-expr t:ml-type)
    (check-type! #'e #'t)
    #'(seal (ml->racket e) #'t))

  (host-interface/expression
    (ml e:ml-expr t:ml-type)
    (check-type! #'e #'t)
    #'(ml->racket e)))

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
       (type-env-ref #'x)]
      [_:number
       #'Nat]
      [(app e1 e2)
       (define t1 (infer-type #'e1))
       (syntax-parse t1
         #:datum-literals (->)
         [(-> argt rett)
          (check-type! #'e2 #'argt)
          #'rett])]
      [(+ e1 e2)
       (check-type! #'e1 #'Nat)
       (check-type! #'e2 #'Nat)
       #'Nat]
      [(- e1 e2)
       (check-type! #'e1 #'Nat)
       (check-type! #'e2 #'Nat)
       #'Nat]
      [(if0 e1 e2 e3)
       (check-type! #'e1 #'Nat)
       (define t2 (infer-type #'e2))
       (check-type! #'e3 t2)
       t2]
      [(lambda ([x xt]) b)
       (type-env-extend! #'x #'xt)
       (define/syntax-parse rett (infer-type #'b))
       #'(-> xt rett)]
      [(MR e t)
       #'t]))

  (define (check-type! e t)
    (assert-type-equal! (infer-type e) t e)))

(define-syntax ml->racket
  (syntax-parser
    #:datum-literals (app + - if0 lambda MR)
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
  
(ml ((lambda ([x Nat]) (- x (MR (RM 1 Nat) Nat))) 0) Nat)