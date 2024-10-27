#lang racket/base

#|
This is a regression test.
The bug was that host exprs couldn't use set! on dsl-bound variables.

Previously, if a dsl's compiler bound variables using a regular 'let', using set! resulted in this error:
compile-reference: contract violation
  expected: identifier?
  given: #<syntax:compile-to-set!-binder.rkt:47:43 (set! x 2)>

Previously, if a dsl's compiler bound variables as set!-transformers, references worked fine,
but using the variable in set! form did not work.
It would result in an error like "set!: cannot mutate syntax identifier".
It's the same error as trying to set! an identifier defined by define-syntax.

In the old compiler, variables would be bound to simple transformers in the environment, rather than
set!-transformers.
|#

(require "../../testing.rkt")

(begin-for-syntax
  (struct my-var-rep []
    #:property prop:set!-transformer
    (syntax-parser
      [x:id #`(list x x)]
      [((~literal set!) x:id e:expr)
       #`(begin (set! x e) 'set!)]
      [(x:id arg ...)
       ;; ripped from syntax/transformer source code
       (let ([stx* (cons #'(#%expression x) (cdr (syntax-e this-syntax)))])
         (datum->syntax this-syntax stx* this-syntax))])))

(syntax-spec
  (binding-class my-var #:reference-compiler mutable-reference-compiler)
  (binding-class regular-var #:reference-compiler mutable-reference-compiler)
  (binding-class rep-var #:reference-compiler (my-var-rep))

  (nonterminal mylet-expr
    #:description "mylet expression"
    (mylet v:my-var body:racket-expr)
    #:binding (scope (bind v) body)
    (regular-let v:regular-var val:racket-expr body:racket-expr)
    #:binding (scope (bind v) body)
    (rep-let v:rep-var body:racket-expr)
    #:binding (scope (bind v) body)
    (mybegin def:mydef body:racket-expr)
    #:binding (scope (import def) body))

  (nonterminal/exporting mydef
    #:description "mylet definition"
    (mydefine-inner v:my-var)
    #:binding (export v)
    (regular-define-inner v:regular-var val:racket-expr)
    #:binding [(export v) val])

  (host-interface/expression
    (run le:mylet-expr)
    (compile-mylet-expr #'le)))

(define-for-syntax compile-mylet-expr
  (syntax-parser
    [((~literal mylet) v:id body:expr)
     #'(let-syntax ([v (make-set!-transformer
                        (syntax-parser
                          [(set! x:id val:expr) #'(list 'set! val)]
                          [x:id #''ref]))])
         body)]
    [((~literal regular-let) v:id val:expr body:expr)
     #'(let ([v val]) body)]
    [((~literal rep-let) v:id body:expr)
     #'(let ([v 1]) body)]
    [((~literal mybegin) ((~literal mydefine-inner) var) body) (compile-mylet-expr #'(mylet var body))]
    [((~literal mybegin) ((~literal regular-define-inner) var val) body) (compile-mylet-expr #'(regular-let var val body))]))

(syntax-spec
  ;; dsl-defined macros are not supported
  #;(host-interface/definition (mydefine v:var)
      #:binding [(export v)]
      ->
      ; this has to be 'define'
      (define-syntax
        [#'v]
        [(make-set!-transformer
          (syntax-parser
            [(set! x:id val:expr) #'(list 'set! val)]
            [x:id #''ref]))]))
 
  (host-interface/definition
    (regular-define v:regular-var val:racket-expr)
    #:binding [(export v) val]
  
    #:lhs
    [#'v]
    #:rhs
    [#'val])

  (host-interface/expression
    (my-set! v:my-var val:racket-expr)
    #'(set! v val)))

;; allows set! of dsl vars in regular racket expressions


;; this used to pass
(check-equal? (run (mylet x x)) 'ref)
;; this used to pass
(check-equal? (run (regular-let x 1 x)) 1)
(check-equal? (run (rep-let x x)) '(1 1))
;; this used to pass
(check-equal? (run (mybegin (mydefine-inner x) x)) 'ref)
;; this used to pass
#;(check-equal? (run (mybegin (regular-define-inner x 1) x)) 1)
#;(let ()
    (mydefine x)
    (check-equal? x 'ref))
;; this used to fail
(check-equal? (run (mylet x (set! x 2))) (list 'set! 2))
;; this used to fail
(check-equal? (run (regular-let x 1 (begin (set! x 2) x))) 2)
(check-equal? (run (rep-let x (list (set! x 2) x))) '(set! (2 2)))
;; this used to fail with the 'expected identifier' error
(check-equal? (run (mybegin (mydefine-inner x) (set! x 2))) (list 'set! 2))
;; this used to fail with the 'expected identifier' error
(check-equal? (run (mybegin (regular-define-inner x 1) (begin (set! x 2) x))) 2)
(test-equal? "using dsl-bound variable as a function"
             (run (regular-let x add1 (x 2)))
             3)

