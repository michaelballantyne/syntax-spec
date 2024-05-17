#lang racket/base

; test the behavior of a reference compiler that cares about the application case
; instead of just being a variable-like transformer.
; also make sure variable-like reference compilers support application

(require "../main.rkt"
         (for-syntax syntax/parse racket/base)
         rackunit
         syntax/macro-testing)

(begin-for-syntax
  ; maps each identifier to the number it's bound to
  (define-persistent-symbol-table number-vars))

(syntax-spec
 (binding-class mutable-var)
 (binding-class immutable-var)
 (binding-class weird-var)
 (binding-class number-var)
 (nonterminal expr
              (mutable-let ([x:mutable-var e:racket-expr]) body:racket-expr)
              #:binding (scope (bind x) body)
              (immutable-let ([x:immutable-var e:racket-expr]) body:racket-expr)
              #:binding (scope (bind x) body)
              (weird-let ([x:weird-var e:racket-expr]) body:racket-expr)
              #:binding (scope (bind x) body)
              (number-let ([x:number-var n:number]) body:racket-expr)
              #:binding (scope (bind x) body))

 (host-interface/expression
  (expression e:expr)
  #'(with-reference-compilers ([weird-var (syntax-parser
                                            [x:id #'x]
                                            [(x:id . args)
                                             #`(list x `#,(length (syntax-e #'args)))])]
                               [number-var (make-variable-like-reference-compiler
                                            (lambda (x)
                                              #`#,(symbol-table-ref number-vars x)))]
                               [mutable-var mutable-reference-compiler]
                               [immutable-var immutable-reference-compiler])
      (compile-expr e))))

(define-syntax compile-expr
  (syntax-parser
    [(_ ((~datum number-let) ([x n:number]) body))
     (symbol-table-set! number-vars #'x (syntax-e #'n))
     #'body]
    [(_ (_ ([x e]) body)) #'(let ([x e]) body)]))

(check-equal? (expression (weird-let ([x 'foo]) x)) 'foo)
(check-equal? (expression (weird-let ([x 'foo]) (x))) '(foo 0))
(check-equal? (expression (weird-let ([x 'foo]) (x 1))) '(foo 1))
(check-equal? (expression (weird-let ([x 'foo]) (x 1 2 3))) '(foo 3))

(check-equal? (expression (number-let ([x 2]) x)) 2)

(check-equal? (expression (immutable-let ([x (lambda (y) y)]) (x 2))) 2)
(check-equal? (expression (mutable-let ([x (lambda (y) y)]) (x 2))) 2)
