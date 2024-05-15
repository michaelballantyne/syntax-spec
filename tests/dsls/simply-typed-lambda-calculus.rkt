#lang racket/base

; simply typed lambda calculus, featuring racket integration, dsl macros, binding spaces,
; definition contexts, re-export, top-level definitions, persistent symbol tables,
; static analysis, rewrites, and custom reference compilers.

(require "../../testing.rkt"
         racket/contract
         (for-syntax racket/match syntax/transformer))

(syntax-spec
  (binding-class typed-var)
  (extension-class typed-macro #:binding-space stlc)
  (nonterminal typed-expr
    #:allow-extension typed-macro
    #:binding-space stlc

    x:typed-var
    n:number

    (#%lambda ([x:typed-var (~datum :) t:type] ...) body:typed-expr)
    #:binding (scope (bind x) body)
    (#%app fun:typed-expr arg:typed-expr ...)

    (#%let ([x:typed-var e:typed-expr] ...) body:typed-expr)
    #:binding (scope (bind x) body)

    ; type annotation
    (~> (e (~datum :) t)
        #'(: e t))
    ((~datum :) e:typed-expr t:type)

    (rkt e:racket-expr (~datum :) t:type)

    (block d:typed-definition-or-expr ... e:typed-expr)
    #:binding (scope (import d) e)

    ; rewrite for tagging applications
    (~> (fun arg ...)
        #'(#%app fun arg ...)))
  (nonterminal type
    Number
    ((~literal ->) arg-type:type ... return-type:type))
  (nonterminal/exporting typed-definition-or-expr
    #:allow-extension typed-macro
    #:binding-space stlc
    (#%define x:typed-var t:type e:typed-expr)
    #:binding (export x)
    (begin defn:typed-definition-or-expr ...+)
    #:binding (re-export defn)
    e:typed-expr)
  (host-interface/expression
   (stlc/expr e:typed-expr)
   (infer-expr-type #'e)
   #'(with-reference-compilers ([typed-var typed-var-reference-compiler])
       (compile-expr e)))
  (host-interface/expression
   (stlc/infer e:typed-expr)
   (define t (infer-expr-type #'e))
   (define t-datum (type->datum t))
   #`'#,t-datum)
  (host-interface/definitions
   (stlc body:typed-definition-or-expr ...+)
   #:binding (re-export body)
   (type-check-defn-or-expr/pass1 #'(begin body ...))
   (type-check-defn-or-expr/pass2 #'(begin body ...))
   #'(compile-defn-or-expr (begin body ...))))

(begin-for-syntax
  ; a Type is one of
  (struct number-type [] #:prefab)
  (struct function-type [arg-types return-type] #:prefab)
  ; arg-types is a (listof Type)

  ; maps identifiers to types
  (define-persistent-symbol-table types)

  ; Syntax -> Type
  (define (infer-expr-type e)
    (syntax-parse e
      [n:number (number-type)]
      [x:id (symbol-table-ref types #'x (lambda () (raise-syntax-error 'infer-expr-type "untyped identifier" #'x)))]
      [((~datum #%lambda) ([x:id _ t] ...) body)
       (define arg-types (map parse-type (attribute t)))
       (for ([x (attribute x)]
             [t arg-types])
         (symbol-table-set! types x t))
       (define body-type (infer-expr-type #'body))
       (function-type arg-types body-type)]
      [((~datum #%app) f arg ...)
       (define f-type (infer-expr-type #'f))
       (match f-type
         [(function-type expected-arg-types return-type)
          (unless (= (length expected-arg-types) (length (attribute arg)))
            (raise-syntax-error 'infer-expr-type
                                (format "arity error. expected ~a arguments, but got ~a"
                                        (length expected-arg-types)
                                        (length (attribute arg)))
                                this-syntax))
          (for ([expected-type expected-arg-types]
                [arg (attribute arg)])
            (check-expr-type arg expected-type))
          return-type]
         [_ (raise-syntax-error 'infer-expr-type
                                (format "type mismatch. expected a function type, but got ~a"
                                        (type->datum f-type))
                                #'f)])]
      [((~datum :) e t-stx)
       (define t (parse-type #'t-stx))
       (check-expr-type #'e t)
       t]
      [((~datum #%let) ([x e] ...) body)
       (for ([x (attribute x)]
             [e (attribute e)])
         (symbol-table-set! types x (infer-expr-type e)))
       (infer-expr-type #'body)]
      [((~datum rkt) e (~datum :) t)
       (parse-type #'t)]
      [((~datum block) d ... e)
       (type-check-defn-or-expr/pass1 #'(begin d ...))
       (type-check-defn-or-expr/pass2 #'(begin d ...))
       (infer-expr-type #'e)]))

  ; Syntax Type -> Void
  (define (check-expr-type e expected-type)
    (define actual-type (infer-expr-type e))
    (unless (equal? expected-type actual-type)
      (raise-syntax-error 'infer-expr-type
                          (format "type mismatch. expected ~a, but got ~a"
                                  (type->datum expected-type)
                                  (type->datum actual-type))
                          e)))

  ; Syntax -> Void
  (define (type-check-defn-or-expr/pass1 e)
    (syntax-parse e
      [((~datum #%define) x:id t _)
       (symbol-table-set! types #'x (parse-type #'t))]
      [((~datum begin) body ...)
       (for ([body (attribute body)])
         (type-check-defn-or-expr/pass1 body))]
      [_ (void)]))

  ; Syntax -> Void
  (define (type-check-defn-or-expr/pass2 e)
    (syntax-parse e
      [((~datum #%define) _ t e)
       (check-expr-type #'e (parse-type #'t))]
      [((~datum begin) body ...)
       (for ([body (attribute body)])
         (type-check-defn-or-expr/pass2 body))]
      [e (void (infer-expr-type #'e))]))

  ; Syntax -> Type
  (define (parse-type t-stx)
    (syntax-parse t-stx
      [(~datum Number) (number-type)]
      [((~datum ->) arg-type ... return-type)
       (function-type (map parse-type (attribute arg-type))
                      (parse-type #'return-type))]))

  ; Type -> any
  ; converts to simple s-expression for displaying
  (define (type->datum t)
    (match t
      [(number-type) 'Number]
      [(function-type arg-types return-type)
       (append (list '->)
               (map type->datum arg-types)
               (list (type->datum return-type)))])))

(define-syntax compile-expr
  (syntax-parser
    [(_ n:number) #'n]
    [(_ x:id) #'x]
    [(_ ((~datum #%lambda) ([x:id _ _] ...) body))
     #'(lambda (x ...) (compile-expr body))]
    [(_ ((~datum #%app) f arg ...))
     #'((compile-expr f) (compile-expr arg) ...)]
    [(_ ((~datum :) e _)) #'(compile-expr e)]
    [(_ ((~datum #%let) ([x e] ...) body))
     #'(let ([x (compile-expr e)] ...) (compile-expr body))]
    [(_ ((~datum rkt) e (~datum :) t))
     #`(contract #,(type->contract-stx (parse-type #'t))
                 e
                 'racket 'stlc
                 #f #'e)]
    [(_ ((~datum block) d ... e))
     #'(let ()
         (compile-defn-or-expr d)
         ...
         (compile-expr e))]))

(begin-for-syntax
  (define typed-var-reference-compiler
    ; TODO change to make-variable-like-reference-compiler
    (make-variable-like-transformer (lambda (x)
                                      #`(contract #,(type->contract-stx (symbol-table-ref types x))
                                                  #,x
                                                  'stlc 'racket
                                                  '#,x #'#,x))))

  ; Type -> Syntax
  ; emits syntax that specifies a contract equivalent to the given type
  (define (type->contract-stx t)
    (match t
      [(number-type) #'number?]
      [(function-type arg-types return-type)
       (define/syntax-parse (arg-type-stx ...) (map type->contract-stx arg-types))
       (define/syntax-parse return-type-stx (type->contract-stx return-type))
       #'(-> arg-type-stx ... return-type-stx)])))

(define-syntax compile-defn-or-expr
  (syntax-parser
    [(_ ((~datum #%define) x:id _ body))
     #'(define x (compile-expr body))]
    [(_ ((~datum begin) body ...+))
     #'(begin (compile-defn-or-expr body) ...)]
    [(_ e)
     #'(compile-expr e)]))

(define-syntax define-stlc-syntax
  (syntax-parser
    [(_ name:id trans:expr)
     #`(define-syntax #,((make-interned-syntax-introducer 'stlc) #'name 'add)
         (typed-macro trans))]))

(define-stlc-syntax let
  (syntax-parser
    [(_ ([x e] ...) body) #'(#%let ([x e] ...) body)]
    [(_ ([x e] ...) body ...+) #'(#%let ([x e] ...) (block body ...))]))

(define-stlc-syntax lambda
  (syntax-parser
    [(_ ([x (~datum :) t] ...) body) #'(#%lambda ([x : t] ...) body)]
    [(_ ([x (~datum :) t] ...) body ...+) #'(#%lambda ([x : t] ...) (block body ...))]))

(define-stlc-syntax let*
  (syntax-parser
    [(_ () body) #'(let () body)]
    [(_ ([x:id e] binding ...) body)
     #'(let ([x e]) (let* (binding ...) body))]))

(define-stlc-syntax define
  (syntax-parser
    [(_ x:id (~datum :) t e)
     #'(#%define x t e)]
    [(_ (f:id [arg:id (~datum :) arg-type] ...) (~datum ->) return-type body)
     #'(#%define f (-> arg-type ... return-type)
                 (lambda ([arg : arg-type] ...)
                   body))]))

; testing

(define-syntax-rule
  (check-eval e v)
  (check-equal? (let () (stlc e)) v))
(define-syntax-rule
  (check-infer e t)
  (check-equal? (stlc/infer e) 't))
(check-eval 1 1)
(check-eval ((lambda ([x : Number]) x) 1) 1)
(check-infer (lambda ([x : Number]) x)
             (-> Number Number))
(check-eval (let ([x 1]) x) 1)
(check-eval
 (let* ([second (lambda ([x : Number] [y : Number]) y)]
        [x (second 1 2)])
   x)
 2)
(check-exn
 #rx"expected a function type, but got Number"
 (lambda ()
   (convert-compile-time-error
    (stlc/expr (1 2)))))
(check-exn
 #rx"arity error. expected 0 arguments, but got 1"
 (lambda ()
   (convert-compile-time-error
    (stlc/expr ((lambda () 1) 2)))))
(check-exn
 #rx"type mismatch. expected Number, but got \\(-> Number\\)"
 (lambda ()
   (convert-compile-time-error
    (stlc/expr ((lambda ([x : Number]) x) (lambda () 1))))))
(check-exn
 #rx"type mismatch. expected Number, but got \\(-> Number\\)"
 (lambda ()
   (convert-compile-time-error
    (stlc/expr ((lambda () 1) : Number)))))
; racket integration
(check-infer (rkt 1 : Number)
             Number)
(check-infer (rkt (lambda (x) x) : (-> Number Number))
             (-> Number Number))
(check-eval (rkt 1 : Number)
            1)
(test-exn
 "racket expr is not of the correct type"
 #rx"promised: number\\?\n  produced: #t"
 (lambda ()
   (stlc/expr
    (rkt #t : Number))))
(test-exn
 "racket expr is a function which breaks its contract"
 #rx"promised: number\\?\n  produced: #t"
 (lambda ()
   (stlc/expr
    (let ([f (rkt (lambda (x) #t) : (-> Number Number))])
      (f 1)))))
(test-exn
 "typed var misused in racket expr"
 #rx"expected: number\\?\n  given: #t"
 (lambda ()
   (stlc/expr
    (let ([f (lambda ([x : Number]) x)])
      (rkt (f #t) : Number)))))
; definitions
(check-equal?
 (let ()
   (stlc
    (define x : Number 1)
    x))
 1)
; define at top-level
(stlc
 (define one : Number 1))
(check-eval
 one
 1)
(check-equal?
 (let ()
   (stlc
    (define (id [x : Number]) -> Number
      x)
    (id 2)))
 2)
(check-equal?
 (let ()
   (stlc
    (define (f) -> Number
      (g))
    (define (g) -> Number
      1)
    (f)))
 1)
(test-equal?
 "begin splices definitions"
 (let ()
   (stlc
    (begin (begin (define x : Number 1)))
    x))
 1)
; block
(check-eval
 (block 1)
 1)
(check-eval
 (block 1 2)
 2)
(check-eval
 (block
  (define x : Number 1)
  x)
 1)
; implicit block for multi-expression let
(check-eval
 (let ()
   (define x : Number 1)
   x)
 1)
(check-eval
 ((lambda () (define x : Number 1) 1))
 1)