#lang racket/base

; (get-racket-referenced-vars (binding-class-id ...) e)
; returns a list of identifiers referenced in racket expressions from the specified binding classes in e.
(provide (for-syntax get-racket-referenced-vars))
(require racket/set
         "../testing.rkt")

(syntax-spec
  (binding-class a-var)
  (binding-class b-var)
  (binding-class c-var)
  (nonterminal my-expr
    (let/a x:a-var e:my-expr)
    #:binding (scope (bind x) e)
    (let/b x:b-var e:my-expr)
    #:binding (scope (bind x) e)
    (let/c x:c-var e:my-expr)
    #:binding (scope (bind x) e)
    (rkt e:racket-expr))
  (host-interface/expression
   (my-dsl e:my-expr)
   #'(with-reference-compilers ([c-var immutable-reference-compiler])
       (compile-expr e))))

(begin-for-syntax
  (define current-referenced-vars (make-parameter #f))
  (define-syntax-rule (get-racket-referenced-vars [binding-class ...] e)
    (parameterize ([current-referenced-vars (local-symbol-set)])
      (local-expand #`(with-reference-compilers ([binding-class recording-reference-compiler] ...)
                        #,e)
                    'expression
                    '())
      (for/list ([x (in-symbol-set (current-referenced-vars))]) x)))
  (define recording-reference-compiler
    (make-variable-like-reference-compiler
     (lambda (x) (symbol-set-add! (current-referenced-vars) x) x)
     (lambda (e)
       (syntax-parse e
         [(set! x _)
          (symbol-set-add! (current-referenced-vars) #'x)
          #'x])))))

(define-syntax compile-expr
  (syntax-parser
    #:datum-literals (let/a let/b let/c rkt)
    [(_ ((~or let/a let/b let/c) x:id e:expr))
     #'(let ([x 1]) (compile-expr e))]
    [(_ (rkt e:expr))
     (define/syntax-parse (x ...) (get-racket-referenced-vars (a-var b-var)
                                    #'e))
     #'(list 'x ...)]))

(check-equal? (my-dsl (let/a x (rkt 2)))
              '())
(check-equal? (my-dsl (let/a x (rkt (+ x x))))
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
