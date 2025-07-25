#lang racket

;; A lambda calculus interpreter that operates on expanded syntax,
;; using syntax-spec.
;; Features normalization by evaluation
;;
;; Benefits:
;; - grammar and binding checking
;; - hygienic macros, syntactic sugar
;; - interpreter/static checks only have to worry about core forms
;; - static checks have access to binding information (this example language has no static checks)
;; - easy to report runtime error source location since we evaluate syntax
;; Drawbacks:
;; - not sure if substitution would work bc of scopes on expanded syntax. might be fine
;; - interpreter helpers need expanded syntax, which might make it hard to unit test them

(module+ test (require rackunit))
(require "../../testing.rkt"
         syntax/parse
         racket/syntax
         (for-syntax syntax/parse)
         racket/syntax-srcloc
         (for-template syntax-spec-dev)
         syntax/macro-testing)

(syntax-spec
  (binding-class lc-var #:binding-space lc)
  (extension-class lc-macro #:binding-space lc)
  (nonterminal lc-expr
    #:binding-space lc
    #:allow-extension lc-macro
    n:number
    (+ e1:lc-expr e2:lc-expr)
    x:lc-var
    (lambda (x:lc-var) e:lc-expr)
    #:binding (scope (bind x) e)
    (rkt e:expr)
    (~> (e1 e2)
        ;; this is necessary to preserve source location, properties, etc.
        (syntax/loc this-syntax (#%app e1 e2)))
    (#%app e1:lc-expr e2:lc-expr))

  (host-interface/expression
    (lc-expand e:lc-expr)
    #'#'e))

(define-dsl-syntax let lc-macro
  (syntax-rules ()
    [(let ([x rhs]) body)
     ((lambda (x) body) rhs)]))

(define-syntax-rule (lc e)
  (lc-eval (lc-expand e) empty-env))

;;; runtime

;; An Env is a (ImmutableBoundIdTable Value)
(define empty-env (immutable-symbol-table))
;; Env Identifier -> Value
(define (env-lookup env x)
  (if (symbol-table-has-key? env x)
      (symbol-table-ref env x)
      ;; neutral
      x))
;; Env Identifier Value -> Void
(define (env-extend env x v)
  (symbol-table-set env x v))

;; A Value is one of
;; a Number
;; a Value -> Value
;; a NeutralExpr

;; A NeutralExpr is one of
;; Identifier
;; (+ NeutralExpr Value)
;; (+ Value NeutralExpr)
;; (NeutralExpr Value)

(define-syntax-rule (normalize e)
  (lc-uneval (lc e)))

;; Syntax Env -> Value
(define (lc-eval stx env)
  (syntax-parse stx
    #:datum-literals (+ lambda #%app rkt)
    [n:number
     (syntax->datum #'n)]
    [(+ e1 e2)
     (define v1 (lc-eval #'e1 env))
     (define v2 (lc-eval #'e2 env))
     (cond
       [(or (syntax? v1) (syntax? v2))
        ;; neutral
        #`(+ #,v1 #,v2)]
       [else
        (unless (number? v1)
          (lc-error this-syntax "+ expects number"))
        (unless (number? v2)
          (lc-error this-syntax "+ expects number"))
        (+ v1 v2)])]
    [x:id
     (env-lookup env #'x)]
    [(lambda (x:id) e:expr)
     (lambda (v) (lc-eval #'e (env-extend env #'x v)))]
    [(#%app e1 e2)
     (match (lc-eval #'e1 env)
       [(? procedure? f)
        (f (lc-eval #'e2 env))]
       [(? syntax? f)
        #`(#,f #,(lc-eval #'e2 env))]
       [_
        (lc-error this-syntax "applied non-function")])]
    [(rkt e)
     (eval #'e)]))

;; Value -> Syntax
(define (lc-uneval v)
  (define count 0)
  (define (fresh)
    (begin0 (format-id #f "_.~a" count)
      (set! count (add1 count))))
  (let loop ([v v])
    (match v
      [(? number?)
       (datum->syntax #f v)]
      [(? procedure?)
       (define x (fresh))
       #`(lambda (#,x) #,(loop (v x)))]
      [(? syntax?)
       (syntax-parse v
         [((~datum +) a b)
          #`(+ #,(loop (attribute a)) #,(loop (attribute b)))]
         [(a b)
          #`(#,(loop (attribute a)) #,(loop (attribute b)))]
         [x:id #'x]
         [_
          ; 3D syntax of a value that got syntax'ed bc of quasiquote
          (loop (syntax->datum v))])])))

;; Syntax String -> Void
;; raise (runtime) error with source location reported
(define (lc-error stx msg)
  (define loc (syntax-srcloc stx))
  (if loc
      (raise-user-error (format "~a: ~a" (srcloc->string loc) msg))
      (raise-user-error 'lc msg)))

(define top-level-var 4)

(module+ test
  (define-syntax-rule (teval e) (check-equal? (lc e) e))
  (define-syntax-rule (tnormalize e e^) (check-equal? (syntax->datum (normalize e)) 'e^))
  (define-syntax-rule (t-runtime-error msg e)
    (check-exn
     msg
     (lambda ()
       (lc e))))
  (define-syntax-rule (t-expand-error msg e)
    (check-exn
     msg
     (lambda ()
       (convert-compile-time-error (lc e)))))
  (teval 1)
  (teval (+ 1 1))
  (teval ((lambda (x) x) 1))
  (teval (let ([x 1]) (+ x x)))
  (test-case "hygiene"
    ;; basic shadow
    (teval (let ([x 1])
             (let ([x 2])
               x)))
    ;; macro "shadows", should not actually shadow
    (define-dsl-syntax m lc-macro
      (syntax-rules ()
        [(m e)
         (let ([x 2]) e)]))
    (check-equal? (lc (let ([x 1]) (m x)))
                  1)
    ;; macro ref to macro binding not shadowable from use site
    (define-dsl-syntax m2 lc-macro
      (syntax-rules ()
        [(m2 ([x rhs]))
         (let ([y 1])
           (let ([x rhs])
             y))]))
    (check-equal? (lc (m2 ([y 2])))
                  1))
  ;; errors
  (t-expand-error
   #rx"not bound"
   x)
  (t-expand-error
   ;; actual decent grammatical error message
   #rx"lambda: unexpected term"
   (lambda (x y) x))
  (t-runtime-error
   ;; source location for runtime error
   #px".*\\.rkt:\\d*:\\d*: applied non-function"
   (1 2))
  (tnormalize 1 1)
  (tnormalize (lambda (x) x)
              (lambda (_.0) _.0))
  ;; evaluates in lambda bodies
  (tnormalize (lambda (x) (+ (+ 1 1) x))
              (lambda (_.0) (+ 2 _.0)))
  (tnormalize (lambda (x) (+ x (+ 1 1)))
              (lambda (_.0) (+ _.0 2)))
  (tnormalize (lambda (x) (x (lambda (y) y)))
              (lambda (_.0) (_.0 (lambda (_.1) _.1))))
  (check-equal? (lc (rkt (* 2 2))) 4)
  (check-equal? (lc (rkt top-level-var)) 4))
