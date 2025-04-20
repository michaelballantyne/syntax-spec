#lang racket/base

;; Similar to private/test/simple-bsepc.rkt, but attempting to create an expander that
;; uses syntax classes for nonterminals.
;; This was created to research a potential solution for
;; https://github.com/michaelballantyne/syntax-spec/issues/62

(require
  (for-syntax
   racket/base
   syntax/parse
   "../ee-lib/main.rkt"
   "../runtime/binding-spec.rkt")

  "../ee-lib/define.rkt")

#;;; ANF language
(syntax-spec
  (binding-class my-var)
  (nonterminal a-expr
    e:c-expr
    (my-let ([x:my-var e:c-expr]) b:a-expr)
    #:binding (scope (bind x) b)
    ;; body will expand before binding x
    (where b:a-expr
      ([x:my-var e:c-expr]))
    #:binding (scope (bind x) b))
  (nonterminal c-expr
    e:i-expr
    (my-+ a:i-expr b:i-expr)
    ;; application
    (a:i-expr b:i-expr c:i-expr))
  (nonterminal i-expr
    n:number
    x:my-var))

(define-literal-forms anf-lits
  "mylang forms may only be used in mylang"
  (my-let my-+ where))

(begin-for-syntax
  (struct my-var-rep ())

  ;; hash from symbols to binding representations
  ;; to simulate the real expander environment
  (define pretend-binding-store (make-hash (list (cons 'bogus #f))))

  (define-syntax-class a-expr
    #:literal-sets (anf-lits)
    (pattern e:c-expr
             #:attr expanded #'e.expanded)
    (pattern (my-let ([x:id ~! e:expr]) b:expr)
             #:with x^:my-var-bind #'x
             #:with e^:c-expr #'e
             #:with b^:a-expr #'b
             #:attr expanded #'(my-let ([x^.expanded e^.expanded]) b^.expanded))
    (pattern (where b:expr ([x:id ~! e:expr]))
             ;; we expand x and e BEFORE e because the binding spec is
             ;; [(scope (bind x) b) e]
             ;; expansion order is driven by the binding spec
             #:with x^:my-var-bind #'x
             #:with b^:a-expr #'b
             #:with e^:c-expr #'e
             #:attr expanded #'(where b^.expanded ([x^.expanded e^.expanded]))))

  (define-syntax-class c-expr
    #:literal-sets (anf-lits)
    (pattern e:i-expr
             #:attr expanded (attribute e.expanded))
    ;; my-+ should be first, but due to the fake binding store,
    ;; to get the my-+ shadowing test passing, this needed to be first.
    ;; in a real implementation with the real binding store, the my-+ pattern would
    ;; fail because the identifier wouldn't be referencing the literal, right?.
    ;; you'd need to be painting scopes on syntax as you go, but that could happen in
    ;; the pattern actions I guess.
    (pattern (a:expr b:expr c:expr)
             #:with a^:i-expr #'a
             #:with b^:i-expr #'b
             #:with c^:i-expr #'c
             #:attr expanded #'(#%app a^.expanded b^.expanded c^.expanded))
    (pattern (my-+ a:expr b:expr)
             #:with a^:i-expr #'a
             #:with b^:i-expr #'b
             #:attr expanded #'(my-+ a^.expanded b^.expanded)))

  (define-syntax-class i-expr
    #:literal-sets (anf-lits)
    (pattern n:number
             #:attr expanded #'n)
    (pattern x:my-var-ref
             #:attr expanded #'x.expanded))

  (define-syntax-class my-var-bind
    (pattern x:id
             #:fail-when (hash-has-key? pretend-binding-store (syntax->datum #'x)) "duplicate binding"
             ;; we need the do because the bind! needs to happen before the body expands.
             ;; attrs are eagerly evaluated when a pattern with a syntax class is matched,
             ;; so the bind! needs to happen before the body is even matched.
             #:do [(hash-set! pretend-binding-store (syntax->datum #'x) (my-var-rep))]
             #:attr expanded #'x))

  (define-syntax-class my-var-ref
    (pattern x:id
             #:fail-unless (hash-has-key? pretend-binding-store (syntax->datum #'x)) "unbound variable"
             #:fail-unless (my-var-rep? (hash-ref pretend-binding-store (syntax->datum #'x))) "expected a my-var"
             #:attr expanded #'x)))

(define-syntax (mylang stx)
  (set! pretend-binding-store (make-hash (list (cons 'bogus #f))))
  (syntax-parse stx
    [(_ e:a-expr)
     #''e.expanded]))

(require rackunit syntax/macro-testing)

(define-syntax-rule (check-success e)
  (check-equal? (mylang e)
                'e))
(define-syntax-rule (check-failure e msg)
  (check-exn
   msg
   (lambda ()
     (convert-compile-time-error (mylang e)))))

(check-success 1)
(check-success (my-+ 1 2))
(check-success (my-let ([x 1]) x))
(check-failure
 y
 #rx"unbound var")
(check-failure
 (my-let ([a 1])
   (my-let ([a 2])
     3))
 #rx"duplicate binding")
(check-failure
 bogus
 #rx"expected a my-var")
(check-failure
 (my-+ (my-let ([z 1]) z) 2)
 #rx"expected a-expr")
;; where.
;; this test fails if you expand left-to-right
(check-success
 (where x
   ([x 1])))
;; shadow my-+ in where.
;; this test fails if you expand left-to-right
(check-equal? (mylang (where (my-+ 1 2)
                        ([my-+ 3])))
              ;; should not treat my-+ as a literal in the where body
              '(where (#%app my-+ 1 2)
                 ([my-+ 3])))

#|
examples that broke the original eager design:
(where x
  ([x 1]))
this broke bc the reference parsed/expanded before the binding. syntax-parse parses (and thus expands bc of attr eagerness)
left to right.
(where (my-+ 1 2)
  ([my-+ 3]))
this broke for a similar reason. This shows that even "structural" parsing needs bindings to detect literal shadowing.

problems:
- if you do everything eager (and do binding class resolution in parsing), then where breaks because the body
gets parsed before the binding happens
- if you treat all ids as ids and ignore binding classes during parsing, then the my-+ shadow thing fails
because it will resolve to the literal instead of the shadowed thing
- the solution is to do binding-spec-driven expansion order, which requires promises and binding stuff has to happen in the post of the production.
but then, you'll still get the my-+ shadow problem because parsing needs to be driven by binding classes
to distinguish between shadowed literals and actual literals.

constraints:
- to resolve literals vs references to bindings that shadow literals, you need to bind as you parse
- to do that, you need to delay parsing. parsing and expansion order must be driven by the binding spec.
- to avoid backtracking over a binding, you need to commit when you bind

you can delay parsing with #:with

current desired semantics:
full backtracking (even over binding classes), except you commit when you bind a variable.
TODO does cut in a post commit the way you need it to?

example:

(nonterminal my-expr
  n:number
  x:a-var
  x:b-var
  (let ([x:a-var e:my-expr]) b:my-expr)
  #:binding (scope (bind x) b)
  (let ([x:b-var e:my-expr]) b:my-expr)
  #:binding (scope (bind x) b))

identifiers don't commit to a-var (which I'm pretty sure is the current syntax-spec behavior)
but (let ([x 1]) x) commits to the a-var let production
|#
