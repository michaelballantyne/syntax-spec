#lang racket/base

;; A dsl with multiple passes
;; arithmetic + let -> ANF -> prune unused variables -> racket

(require "../../testing.rkt"
         (for-syntax racket/list rackunit (only-in ee-lib define/hygienic)))

(syntax-spec
  (binding-class var)
  (nonterminal expr
    n:number
    x:var
    ; need to use ~literal because you can't re-use let in the other non-terminals
    ((~literal let) ([x:var e:expr]) body:expr)
    #:binding (scope (bind x) body)
    ((~literal +) a:expr b:expr)
    ((~literal *) a:expr b:expr)
    ((~literal /) a:expr b:expr)
    (rkt e:racket-expr))
  (nonterminal anf-expr
    ((~literal let) ([x:var e:rhs-expr]) body:anf-expr)
    #:binding (scope (bind x) body)
    e:rhs-expr)
  (nonterminal rhs-expr
    ((~literal +) a:immediate-expr b:immediate-expr)
    ((~literal *) a:immediate-expr b:immediate-expr)
    ((~literal /) a:immediate-expr b:immediate-expr)
    ((~literal rkt) e:racket-expr)
    e:immediate-expr)
  (nonterminal immediate-expr
    x:var
    n:number)

  (host-interface/expression
   (eval-expr e:expr)
   #'(with-reference-compilers ([var immutable-reference-compiler])
       (compile-expr e))))

(begin-for-syntax
  (define local-expand-anf (nonterminal-expander anf-expr)))

(define-syntax compile-expr
  (syntax-parser
    [(_ e)
     ; I chose to use compile-time functions instead of macros because there is a lot
     ; of non-syntax data to pass around. But we still get hygiene with define/hygienic.

     ; need to expand to make sure everything is properly bound
     ; for the analysis pass, which uses symbol tables.
     (define e/anf (local-expand-anf (to-anf #'e) #:should-rename? #t))
     (define e/pruned (prune-unused-variables e/anf))
     ; this last local-expand-anf might be unnecessary for this compiler, but i'll leave it in
     ; since most compilers would need it.
     (define e/pruned^ (local-expand-anf e/pruned #:should-rename? #t))
     #`(compile-anf #,e/pruned^)]))

(begin-for-syntax
  ; expr -> anf-expr
  ; this doesn't really need to be hygienic, but in general, compiler passes often will.
  (define/hygienic (to-anf e)
    #:expression
    ; list of (list Identifier rhs-expr)
    ; most recent, and thus innermost, binding first
    (define bindings-rev '())
    ; Identifier rhs-expr -> Void
    ; ends up producing a let-binding of x to e in the result
    (define (bind! x e) (set! bindings-rev (cons (list x e) bindings-rev)))
    (define e^ (to-rhs e bind!))
    (wrap-lets e^ (reverse bindings-rev)))

  ; expr (Identifier rhs-expr -> Void) -> rhs-expr
  ; this doesn't need to be hygienic, only the whole pass.
  ; in other compilers, helpers may need to be hygienic too.
  (define (to-rhs e bind!)
    (syntax-parse e
      [((~literal let) ([x e]) body)
       (bind! #'x (to-rhs #'e bind!))
       (to-rhs #'body bind!)]
      [(op a b)
       #`(op #,(to-immediate #'a bind!)
             #,(to-immediate #'b bind!))]
      [_ this-syntax]))

  ; expr (Identifier rhs-expr -> Void) -> immediate-expr
  (define (to-immediate e bind!)
    (syntax-parse e
      [(_ . _)
       (define/syntax-parse (tmp) (generate-temporaries '(tmp)))
       (bind! #'tmp (to-rhs this-syntax bind!))
       #'tmp]
      [_ this-syntax]))

  ; rhs-expr (listof (list Identifier rhs-expr) )
  (define (wrap-lets e bindings)
    (foldr (lambda (binding e) #`(let ([#,(first binding) #,(second binding)]) #,e))
           e
           bindings)))

(begin-for-syntax
  ; anf-expr -> anf-expr
  (define/hygienic (prune-unused-variables e)
    #:expression
    (define var-used? (get-used-vars e))
    (remove-unused-vars e var-used?))

  ; anf-expr -> (Identifier -> Bool)
  ; non-hygienic because it's just an analysis pass
  (define (get-used-vars e)
    (define-local-symbol-table used-vars)
    (define (mark-as-used! x)
      (symbol-table-set! used-vars x #t))
    (define (var-used? x) (symbol-table-ref used-vars x #f))
    ; Go bottom-up, seeing references before their binders.
    ; The invariant is that we only traverse expressions that need
    ; to be evaluated.
    ; The innermost expression is needed, so we traverse it. From there,
    ; we only traverse expressions that are (transitively) needed.
    ; If we see a reference, mark it as used.
    ; If we see a binder that is marked as used,
    ; we need its rhs' referenced variables too, so recur on the rhs.
    ; If we see a binder that isn't marked as used, it was never referenced,
    ; so we don't traverse its rhs since it isn't needed.
    (let mark-used-variables! ([e e])
      (syntax-parse e
        [((~literal let) ([x e]) body)
         (mark-used-variables! #'body)
         (when (var-used? #'x)
           (mark-used-variables! #'e))]
        [(op a b)
         (mark-used-variables! #'a)
         (mark-used-variables! #'b)]
        [x:id
         (mark-as-used! #'x)]
        ; don't descent into racket expressions.
        ; this means we'll miss references like (rkt (eval-expr x)).
        ; TODO use free-variables once it supports host-expressions
        [_ (void)]))
    var-used?)

  ; anf-expr (Identifier -> Boolean) -> anf-expr
  (define (remove-unused-vars e var-used?)
    (let loop ([e e])
      (syntax-parse e
        [((~and let (~literal let)) ([x e]) body)
         (if (var-used? #'x)
             ; no need to recur on e since it's not a let
             #`(let ([x e])
                 #,(loop #'body))
             (loop #'body))]
        [_ this-syntax]))))

(define-syntax compile-anf
  (syntax-parser
    [(_ ((~literal let) ([x e]) body))
     #'(let ([x (compile-anf e)]) (compile-anf body))]
    [(_ (op a b)) #'(op a b)]
    [(_ ((~literal rkt) e))
     #'(let ([x e])
         (if (number? x)
             x
             (error 'rkt "expected a number, got ~a" x)))]
    [(_ e) #'e]))

(module+ test
  (begin-for-syntax
    (define-syntax-rule
      (check-anf e e/anf)
      (check-true
       (alpha-equivalent? (local-expand-anf (to-anf #'e) #:should-rename? #t)
                          (local-expand-anf #'e/anf #:should-rename? #t))))
    (check-anf 1 1)
    (check-anf (let ([x 1]) x)
               (let ([y 1]) y))
    (check-anf (+ 1 (+ 2 3))
               (let ([x (+ 2 3)])
                 (+ 1 x)))
    (check-anf
     (let ([x (let ([y 1]) y)])
       x)
     (let ([y 1])
       (let ([x y])
         x)))))

(define-syntax-rule (check-eval e) (check-equal? (eval-expr e) e))
(check-eval 1)
(check-eval (+ 1 1))
(check-eval (+ 1 (* 2 3)))
(check-eval (let ([x 1]) x))
(check-eval (let ([x (+ 3 4)]) x))
(check-eval
 (let ([x (let ([y 2])
            (+ 1 (* y 3)))])
   x))
(check-equal?
 (eval-expr
  (let ([unused (/ 1 0)])
    2))
 2)
(check-eval
 (let ([a 1])
   (let ([b a])
     (let ([unused a])
       b))))
; interfacing with racket
(check-equal?
 (eval-expr (rkt (add1 1)))
 2)
(test-equal? "use racket var in rkt"
 (let ([x 1])
   (eval-expr (rkt x)))
 1)
(test-equal? "use dsl var in rkt"
 (eval-expr
  (let ([x 1])
    (+ x
       (rkt x))))
 2)
(test-equal? "use outer dsl var in dsl in rkt"
 (eval-expr
  (let ([x 1])
    (+ x
       (rkt
        (eval-expr x)))))
 2)
(check-exn
 #rx"expected a number"
 (lambda ()
   (eval-expr (rkt "one"))))
(check-equal?
 (eval-expr
  (let ([unused (rkt (error "bad"))])
    1))
 1)
#;; since we don't descend into racket exprs, it thinks it's unused, so it removes it and we get an unbound reference
(check-equal?
 (eval-expr
  (let ([used-only-in-rkt 1])
    (let ([x (rkt used-only-in-rkt)])
      x))))
