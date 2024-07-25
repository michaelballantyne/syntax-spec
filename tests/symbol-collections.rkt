#lang racket/base


(require "../testing.rkt")

; inserting, reading, and iterating over symbol collections
(syntax-spec
 (binding-class my-var)
 (nonterminal my-expr
   (my-let x:my-var e:my-expr)
   #:binding (scope (bind x) e)
   (ref x:my-var ...))
 (host-interface/expression
    (mutable e:my-expr)
    (define referenced-vars (local-symbol-set))
    (define var-to-symbol (local-symbol-table))
    (for ([x (get-referenced-vars #'e)])
      (symbol-set-add! referenced-vars x)
      (symbol-table-set! var-to-symbol x (syntax->datum x)))
    (define symbols
      (for/list ([x (in-symbol-set referenced-vars)])
        ; use an identifier returned from an interator as a key
        (symbol-table-ref var-to-symbol x)))
    #`'#,symbols)
 (host-interface/expression
    (immutable e:my-expr)
    (define-values (referenced-vars var-to-symbol)
      (for/fold ([referenced-vars (immutable-symbol-set)]
                 [var-to-symbol (immutable-symbol-table)])
                ([x (get-referenced-vars #'e)])
        (values (symbol-set-add referenced-vars x)
                (symbol-table-set var-to-symbol x (syntax->datum x)))))
    (define symbols
      (for/list ([x (in-symbol-set referenced-vars)])
        (symbol-table-ref var-to-symbol x)))
    #`'#,symbols))

(begin-for-syntax
  (define (get-referenced-vars expr)
    (syntax-parse expr
      #:datum-literals (my-let ref)
      [(my-let _ e) (get-referenced-vars #'e)]
      [(ref x ...) (attribute x)])))

(check-equal? (sort (mutable (my-let x (my-let y (ref x y)))) symbol<?)
              '(x y))
(check-equal? (sort (immutable (my-let x (my-let y (ref x y)))) symbol<?)
              '(x y))

; symbol set operations
(syntax-spec
 (nonterminal/exporting bind-expr
  (bind x:my-var ...)
  #:binding (export x))
 (nonterminal set-op-expr
   (intersection (x:my-var ...) (y:my-var ...))
   (union (x:my-var ...) (y:my-var ...))
   (subtract (x:my-var ...) (y:my-var ...)))
 (host-interface/definitions
  (my-define x:my-var ...)
  #:binding (export x)
  #'(begin (define x 1) ...))
 (host-interface/expression
  (set-op e:set-op-expr)
  (syntax-parse #'e
    [(op (x ...) (y ...))
     (define operation
       (syntax-parse #'op
         #:datum-literals (intersection union subtract)
         [intersection symbol-set-intersection]
         [union symbol-set-union]
         [subtract symbol-set-subtract]))
     (let ([xs (for/fold ([xs (immutable-symbol-set)])
                         ([x (attribute x)])
                 (symbol-set-add xs x))]
           [ys (for/fold ([ys (immutable-symbol-set)])
                         ([y (attribute y)])
                 (symbol-set-add ys y))])
       #`'#,(for/list ([x (in-symbol-set (operation xs ys))]) x))])))

(let ()
  (my-define a x y z)
  (check-equal? (sort (set-op (intersection (x y z) (a y z)))
                      symbol<?)
                '(y z))
  (check-equal? (sort (set-op (union (x y z) (a y z)))
                      symbol<?)
                '(a x y z))
  (check-equal? (sort (set-op (subtract (x y z) (a y z)))
                      symbol<?)
                '(x)))
