#lang racket/base

; (get-racket-referenced-vars (binding-class-id ...) e)
; returns a list of identifiers referenced in racket expressions from the specified binding classes in e.
(require racket/set
         "../testing.rkt")

(syntax-spec
  (binding-class a-var)
  (binding-class b-var)
  (binding-class c-var #:reference-compiler immutable-reference-compiler)
  (nonterminal my-expr
    (let/a x:a-var e:my-expr)
    #:binding (scope (bind x) e)
    (let/b x:b-var e:my-expr)
    #:binding (scope (bind x) e)
    (let/c x:c-var e:my-expr)
    #:binding (scope (bind x) e)
    (let/no-binding x:a-var e:my-expr)
    #:binding (scope (bind x) e)
    (rkt e:racket-expr))
  (host-interface/expression
   (my-dsl e:my-expr)
   #'(compile-expr e)))

(define-syntax compile-expr
  (syntax-parser
    #:datum-literals (let/a let/b let/c let/no-binding rkt)
    [(_ ((~or let/a let/b let/c) x:id e:expr))
     #'(let ([x 1]) (compile-expr e))]
    [(_ (let/no-binding x:id e:expr))
     #'(compile-expr e)]
    [(_ (rkt e:expr))
     (define/syntax-parse (x ...) (get-racket-referenced-identifiers (a-var b-var)
                                    #'e))
     #'(list 'x ...)]))

(check-equal? (my-dsl (let/a x (rkt 2)))
              '())
(check-equal? (my-dsl (let/a x (rkt (+ x x))))
              '(x))
(check-equal? (my-dsl (let/a x (rkt x)))
              '(x))
(check-equal? (my-dsl (let/a x (let/a y (rkt x))))
              '(x))
(check-equal? (list->seteq (my-dsl (let/a x (let/a y (rkt (+ x y))))))
              (seteq 'x 'y))
(check-equal? (my-dsl (let/b x (rkt (+ x x))))
              '(x))
(check-equal? (my-dsl (let/c x (rkt (+ x x))))
              '())
(check-equal? (list->seteq (my-dsl (let/c x
                                     (let/a y
                                       (let/b z
                                         (rkt (+ x y z)))))))
              (seteq 'y 'z))
(check-equal? (my-dsl (let/no-binding x (rkt (+ x x))))
              '(x))
