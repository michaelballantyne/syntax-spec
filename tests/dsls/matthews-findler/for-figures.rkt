(syntax-spec
  (binding-class ml-var
    #:reference-compiler
    (make-variable-like-reference-compiler
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
        #'(app e1 e2)))

  (nonterminal ml-type
    #:binding-space ml
    Nat
    L
    (-> t1:ml-type t2:ml-type))

  (host-interface/expression
    (RM e:ml-expr)
    (compile-RM #'e)))


;; Put runtime support in a separate figure.

(struct ml-value [v t])
(struct racket-value [v])

;; Compilation might also be a separate figure from grammar.



;; RM-translation : Any MLType -> Any
(define (RM-translation v t) #| elided ... |#)

;; MR-translation : Any MLType -> Any
(define (MR-translation v t) #| elided ... |#)

;; MLVar is Identifier
;; MLType is Syntax
;; MLExpr is Syntax

(begin-for-syntax
  ;; compile-RM : MLExpr -> Syntax
  (define (compile-RM e)
    (define-values (e^ t) (infer-type e))
    #`(RM-translation (ml->racket #,e^) #'#,t))

  ;; assert-type-equal! : MLType MLType MLExpr -> Void
  (define (assert-type-equal! actual expected term) #| elided ... |#)

  (define-local-symbol-table type-env)

  ;; type-env-ref : MLVar -> MLType
  (define (type-env-ref x)
    (symbol-table-ref type-env x #'Nat))

  ;; type-env-extend! : MLVar MLType -> Void
  (define (type-env-extend! x t)
    (symbol-table-set! type-env x t))
  
  ;; infer-type : MLExpr -> (Values MLExpr MLType)
  (define (infer-type e) #| elided ... |#)

  ;; check-type! : MLExpr MLType -> MLExpr
  (define (check-type! e t) #| elided ... |#))

(define-syntax ml->racket
  (syntax-parser
    #:datum-literals (app + - if0 lambda MR)
    #| elided cases ... |#
    [(_ (MR e t))
     #'(MR-translation e #'t)]))
  