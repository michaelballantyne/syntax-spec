#lang typed/racket

; simply typed lambda calculus hosted on typed racket.

; simply typed lambda calculus, featuring racket integration, dsl macros, binding spaces,
; definition contexts, re-export, top-level definitions, persistent symbol tables,
; static analysis, rewrites, and custom reference compilers.

(provide (all-defined-out)
         (for-syntax (all-defined-out)))
(require (for-syntax racket/match)
         "../../testing.rkt"
         "simply-typed-lambda-calculus.rkt")

(syntax-spec
  (host-interface/expression
   (stlc/expr e:typed-expr)
   (infer-expr-type #'e)
   #'(compile-expr/top e))
  (host-interface/definitions
   (stlc body:typed-definition-or-expr ...+)
   #:binding (re-export body)
   (type-check-defn-or-expr/pass1 #'(begin body ...))
   (type-check-defn-or-expr/pass2 #'(begin body ...))
   #'(compile-defn-or-expr (begin body ...))))

(define-syntax compile-expr/top
  (syntax-parser
    [(_ e)
     ; you don't need an ann around this bc the expanded code will be inferred to have the right type
     #'(with-reference-compilers ([typed-var immutable-reference-compiler])
         (compile-expr e))]))

(define-syntax compile-expr
  (syntax-parser
    [(_ n:number) #'n]
    [(_ x:id) #'x]
    [(_ ((~datum #%lambda) ([x:id _ t-stx] ...) body))
     (define ts (map parse-type (attribute t-stx)))
     (define/syntax-parse (t ...) (map type->typed-racket-stx ts))
     #'(lambda ([x : t] ...) (compile-expr body))]
    [(_ ((~datum #%app) f arg ...))
     #'((compile-expr f) (compile-expr arg) ...)]
    [(_ ((~datum :) e _)) #'(compile-expr e)]
    [(_ ((~datum #%let) ([x e] ...) body))
     (define/syntax-parse (t ...) (map (compose type->typed-racket-stx get-identifier-type) (attribute x)))
     #'(let ([x : t (compile-expr e)] ...) (compile-expr body))]
    [(_ ((~datum rkt) e (~datum :) t-stx))
     (define/syntax-parse t (type->typed-racket-stx (parse-type #'t-stx)))
     #'(ann e t)]
    [(_ ((~datum block) d ... e))
     #'(let ()
         (compile-defn-or-expr d)
         ...
         (compile-expr e))]))

(begin-for-syntax
  ; Type -> Syntax
  (define (type->typed-racket-stx t)
    (match t
      [(number-type) #'Number]
      [(function-type arg-types return-type)
       (define/syntax-parse (t-arg ...) (map type->typed-racket-stx arg-types))
       (define/syntax-parse t-ret (type->typed-racket-stx return-type))
       #'(-> t-arg ... t-ret)])))

; inserts with-reference-compilers around exprs, and contract checks
(define-syntax compile-defn-or-expr/top
  (syntax-parser
    [(_ ((~datum #%define) x:id _ body))
     #`(define x (compile-expr/top body))]
    [(_ ((~datum begin) body ...+))
     #'(begin (compile-defn-or-expr/top body) ...)]
    [(_ e)
     #`(compile-expr/top e)]))

(define-syntax compile-defn-or-expr
  (syntax-parser
    [(_ ((~datum #%define) x:id _ body))
     #`(define x (compile-expr body))]
    [(_ ((~datum begin) body ...+))
     #'(begin (compile-defn-or-expr body) ...)]
    [(_ e)
     #'(compile-expr e)]))

(module+ test
  (require/typed rackunit
    [check-equal? (-> Any Any Any)]
    [check-exn (-> Any Any Any)])
  (check-equal?
   (stlc/expr (let ([x (lambda ([x : Number]) x)]) (rkt (x 1) : Number)))
   1)
  ; misuse dsl value in typed racket
  (assert-typecheck-fail ((stlc/expr (lambda ([x : Number]) x)) #t))
  ; misuse typed racket function in dsl
  (assert-typecheck-fail (stlc/expr ((rkt (lambda ([y : Boolean]) y) : (-> Number Number)) 1)))
  (check-equal?
   (let ()
     (stlc (define x : Number 1)
           x))
   1)
  (check-equal?
   (let ()
     (stlc (define (f [x : Number]) -> Number
             x)
           (f 1)))
   1))
