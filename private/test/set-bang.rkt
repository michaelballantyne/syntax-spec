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

(define-hosted-syntaxes
  (binding-class var #:description "mylet variable")

  (nonterminal mylet-expr
               #:description "mylet expression"
               (mylet v:var body:expr)
               #:binding {(bind v) (host body)}
               (regular-let v:var val:expr body:expr)
               #:binding [(host val) {(bind v) (host body)}]
               (rep-let v:var body:expr)
               #:binding {(bind v) (host body)}
               (mybegin def:mydef body:expr)
               #:binding {(recursive def) (host body)})
  (two-pass-nonterminal mydef
               #:description "mylet definition"
               (mydefine-inner v:var)
               #:binding (export v)
               (regular-define-inner v:var val:expr)
               #:binding [(export v) (host val)]))

(define-host-interface/expression (run le:mylet-expr)
  (compile-mylet-expr #'le))

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

(define-for-syntax compile-mylet-expr
  (syntax-parser
    [((~literal mylet) v:id body:expr)
     #:with body^ #'(with-reference-compilers ([var mutable-reference-compiler]) body)
     #'(let-syntax ([v (make-set!-transformer
                           (syntax-parser
                             [(set! x:id val:expr) #'(list 'set! val)]
                             [x:id #''ref]))])
         body^)]
    [((~literal regular-let) v:id val:expr body:expr)
     #'(with-reference-compilers ([var mutable-reference-compiler])
         (let ([v val]) body))]
    [((~literal rep-let) v:id body:expr)
     #:with body^ #'(with-reference-compilers ([var (my-var-rep)]) body)
     #'(let ([v 1]) body^)]
    [((~literal mybegin) ((~literal mydefine-inner) var) body) (compile-mylet-expr #'(mylet var body))]
    [((~literal mybegin) ((~literal regular-define-inner) var val) body) (compile-mylet-expr #'(regular-let var val body))]))

;; dsl-defined macros are not supported
#;(define-host-interface/definition (mydefine v:var)
  #:binding [(export v)]
  ->
  ; this has to be 'define'
  (define-syntax
    [#'v]
    [(make-set!-transformer
      (syntax-parser
        [(set! x:id val:expr) #'(list 'set! val)]
        [x:id #''ref]))]))

(define-host-interface/definition (regular-define v:var val:expr)
  #:binding [(export v) (host val)]
  ->
  (define
    [#'v]
    [#'(with-reference-compilers ([var mutable-reference-compiler]) val)]))

(define-host-interface/expression (ref v:var)
  #'v)

;; allows set! of dsl vars in regular racket expressions
(define-host-interface/expression (my-set! v:var val:expr)
  #:binding (host val)
  #:with val^ #'(with-reference-compilers ([var mutable-reference-compiler]) val)
  #'(set! v val^))

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
;; this used to pass
(let ()
  (regular-define x 1)
  (check-equal? (ref x) 1))
;; this used to pass, but it doesn't really test set! behavior because it uses my-set!
(let ()
  (regular-define x 1)
  (my-set! x 2)
  (check-equal? (ref x) 2))
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

