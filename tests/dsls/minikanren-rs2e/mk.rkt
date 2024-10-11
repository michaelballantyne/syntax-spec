#lang racket/base

(provide (all-defined-out)
         quote cons
         (for-space mk (all-defined-out)))

(require "../../../main.rkt"
         (only-in "../../../private/ee-lib/errors.rkt" raise-argument-error/stx)
         (for-syntax
          racket/base
          racket/pretty
          syntax/parse
          syntax/id-table
          syntax/transformer
          (except-in "../../../private/ee-lib/main.rkt" racket-var)))

;;
;; Core syntax
;;

(syntax-spec
  (binding-class term-variable #:description "miniKanren term variable")
  (binding-class relation-name #:description "miniKanren relation name")
  
  (extension-class term-macro
                   #:binding-space mk)
  (extension-class goal-macro
                   #:binding-space mk)
  
  (nonterminal quoted
    #:description "quoted value"
    n:number
    s:id
    ()
    (a:quoted . d:quoted))

  (nonterminal term
    #:description "miniKanren term"
    #:allow-extension term-macro
    
    n:number
    x:term-variable
    ((~literal quote) t:quoted)
    ((~literal cons) t1:term t2:term))

  (nonterminal goal
    #:description "miniKanren goal"
    #:allow-extension goal-macro

    succeed
    fail
    
    (== t1:term t2:term)

    (disj2 g1:goal g2:goal)
    (conj2 g1:goal g2:goal)
  
    (fresh1 (x:term-variable) b:goal)
    #:binding (scope (bind x) b)

    (project (x:term-variable ...) e:racket-expr ...)
    #:binding [x ...]
    
    (ifte g1:goal g2:goal g3:goal)
    (once g:goal)

    (r:relation-name t:term ...+)))

;;
;; Surface syntax
;;

;; TODO: use syntax-parse and syntax classes for better errors.

(define-dsl-syntax quasiquote term-macro
  (syntax-parser 
    [(~describe
      "`<datum>"
      (_ q))
     (let recur ([stx #'q])
       (syntax-parse stx #:datum-literals (unquote)
         [(unquote e) #'e]
         [(unquote . rest)
          (raise-syntax-error 'unquote "bad unquote syntax" stx)]
         [(a . d) #`(cons #,(recur #'a) #,(recur #'d))]
         [(~or* v:identifier v:number) #'(quote v)]
         [() #'(quote ())]))]))

(define-dsl-syntax disj goal-macro
  (syntax-rules ()
    ((disj) fail)
    ((disj g) g)
    ((disj g0 g ...) (disj2 g0 (disj g ...)))))

(define-dsl-syntax conj goal-macro
  (syntax-rules ()
    ((conj) succeed)
    ((conj g) g)
    ((conj g0 g ...) (conj2 g0 (conj g ...)))))

(define-dsl-syntax fresh goal-macro
  (syntax-rules ()
    ((fresh () g ...) (conj g ...))
    ((fresh (x0 x ...) g ...)
     (fresh1 (x0)
       (fresh (x ...)
         g ...)))))

(define-dsl-syntax conde goal-macro
  (syntax-rules ()
    ((conde (g ...) ...)
     (disj (conj g ...) ...))))

(define-dsl-syntax conda goal-macro
  (syntax-rules ()
    ((conda (g0 g ...)) (conj g0 g ...))
    ((conda (g0 g ...) ln ...)
     (ifte g0 (conj g ...) (conda ln ...)))))

(define-dsl-syntax condu goal-macro
  (syntax-rules ()
    ((condu (g0 g ...) ...)
     (conda ((once g0) g ...) ...))))

;;
;; Interface macros
;;

(syntax-spec
  (host-interface/definition
    (core-defrel (name:relation-name x:term-variable ...) g:goal)
    #:binding [(export name) (scope (bind x) ... g)]
   
    #:lhs
    [(symbol-table-set!
      relation-arity
      #'name
      (length (syntax->list #'(x ...))))
     #'name]

    #:rhs
    [#`(lambda (x ...)
         (lambda (s)
           (lambda ()
             (#%app (compile-goal g) s))))])

  (host-interface/expression
    (core-run n:expr q:term-variable g:goal)
    #:binding (scope (bind q) g)
  
    #`(let ([q (var 'q)])
        (map (reify q)
             (run-goal n (compile-goal g)))))
  
  (host-interface/expression
    (goal-expression g:goal)
    #`(goal-value (compile-goal g))))

;;
;; Surface syntax for interface macros
;;

(define-syntax defrel
  (syntax-rules ()
    [(defrel (name x ...) g ...)
     (core-defrel (name x ...) (conj g ...))]))

(define-syntax run
  (syntax-rules ()
    [(run n (x0 x ...) g ...)
     (run n q (fresh (x0 x ...)
                (== `(,x0 ,x ...) q) g ...))]
    [(run n q g ...)
     (core-run n q (conj g ...))]))

(define-syntax run*
  (syntax-rules ()
    ((run* q g ...) (run #f q g ...))))

;;
;; Compiler
;;

(begin-for-syntax
  (define-persistent-symbol-table relation-arity))

(define-syntax compile-goal
  (syntax-parser
    #:literals (succeed fail == disj2 conj2 fresh1 project ifte once)
    [(_ succeed)
     #'succeed-rt]
    [(_ fail)
     #'fail-rt]
    [(_ (== t1 t2))
     #`(==-rt (compile-term t1) (compile-term t2))]
    [(_ (disj2 g1 g2))
     #`(disj2-rt (compile-goal g1) (compile-goal g2))]
    [(_ (conj2 g1 g2))
     #`(conj2-rt (compile-goal g1) (compile-goal g2))]
    [(_ (fresh1 (x) b))
     #`(call/fresh 'x (lambda (x) (compile-goal b)))]
      
    [(_ (project (x ...) e:expr ...))
     (define-local-symbol-table projected-names)
       
     (for ([x (attribute x)])
       (when (not (symbol-table-ref projected-names x #f))
         (symbol-table-set! projected-names x #t)))
       
     (define term-reference-compiler
       (make-variable-like-transformer
        (lambda (id)
          (if (symbol-table-ref projected-names id #f)
              id
              (raise-syntax-error #f "only projected logic variables may be used from Racket code" id)))))

     #`(lambda (s)
         (let ([x (walk* x s)] ...)
           (with-reference-compilers ([term-variable #,term-reference-compiler])
             ((conj-gen (check-goal e #'e) ...) s))))]
    [(_ (ifte g1 g2 g3))
     #`(ifte-rt (compile-goal g1) (compile-goal g2) (compile-goal g3))]
    [(_ (once g))
     #`(once-rt (compile-goal g))]
    [(_ (relname t ...))
     (let ([actual (length (attribute t))]
           [expected (symbol-table-ref relation-arity #'relname)])
       (when (not (= actual expected ))
         (raise-syntax-error
          #f
          (format "wrong number of arguments; actual ~a, expected ~a" actual expected)
          #'relname)))
       
     #'(relname (compile-term t) ...)]))
  
(define-syntax compile-term
  (syntax-parser
    #:literals (quote cons)
    [(_ n:number)
     #''n]
    [(_ x:id)
     #'x]
    [(_ (quote t))
     #''t]
    [(_ (cons t1 t2))
     #`(cons (compile-term t1) (compile-term t2))]))

;;
;; Runtime
;;

(struct goal-value [fn])

(define (check-goal val stx)
  (if (goal-value? val)
      (goal-value-fn val)
      (raise-argument-error/stx "project" "goal-value?" val stx)))

(define var (lambda (x) (vector x)))
(define var? (lambda (x) (vector? x)))

(define empty-s '())

(define (walk v s)
  (let ((a (and (var? v) (assv v s))))
    (cond
      ((pair? a) (walk (cdr a) s))
      (else v))))

(define (ext-s x v s)
  (cond
    ((occurs? x v s) #f)
    (else (cons `(,x . ,v) s))))

(define (occurs? x v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) (eqv? v x))
      ((pair? v) 
       (or (occurs? x (car v) s)
           (occurs? x (cdr v) s)))
      (else #f))))

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((eqv? u v) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s
              (unify (cdr u) (cdr v) s))))
      (else #f))))

(define (==-rt u v)
  (lambda (s)
    (let ((s (unify u v s)))
      (if s `(,s) '()))))

(define succeed-rt
  (lambda (s)
    `(,s)))
 
(define fail-rt
  (lambda (s)
    '()))

(define (disj2-rt g1 g2)
  (lambda (s)
    (append-inf (g1 s) (g2 s))))

(define (append-inf s-inf t-inf)
  (cond
    ((null? s-inf) t-inf)
    ((pair? s-inf) 
     (cons (car s-inf)
           (append-inf (cdr s-inf) t-inf)))
    (else (lambda () 
            (append-inf t-inf (s-inf))))))

(define (take-inf n s-inf)
  (cond
    ((and n (zero? n)) '())
    ((null? s-inf) '())
    ((pair? s-inf) 
     (cons (car s-inf)
           (take-inf (and n (sub1 n))
                     (cdr s-inf))))
    (else (take-inf n (s-inf)))))

(define (conj2-rt g1 g2)
  (lambda (s)
    (append-map-inf g2 (g1 s))))

(define-syntax conj-gen
  (syntax-rules ()
    ((conj-gen) succeed-rt)
    ((conj-gen g) g)
    ((conj-gen g0 g ...) (conj2-rt g0 (conj-gen g ...)))))

(define (append-map-inf g s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf)
     (append-inf (g (car s-inf))
                 (append-map-inf g (cdr s-inf))))
    (else (lambda () 
            (append-map-inf g (s-inf))))))

(define (call/fresh name f)
  (f (var name)))

(define (reify-name n)
  (string->symbol
   (string-append "_"
                  (number->string n))))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v)
       (cons
        (walk* (car v) s)
        (walk* (cdr v) s)))
      (else v))))

(define (reify-s v r)
  (let ((v (walk v r)))
    (cond
      ((var? v)
       (let ((n (length r)))
         (let ((rn (reify-name n)))
           (cons `(,v . ,rn) r))))
      ((pair? v)
       (let ((r (reify-s (car v) r)))
         (reify-s (cdr v) r)))
      (else r))))

(define (reify v)
  (lambda (s)
    (let ((v (walk* v s)))
      (let ((r (reify-s v empty-s)))
        (walk* v r)))))

(define (run-goal n g)
  (take-inf n (g empty-s)))

(define (ifte-rt g1 g2 g3)
  (lambda (s)
    (let loop ((s-inf (g1 s)))
      (cond
        ((null? s-inf) (g3 s))
        ((pair? s-inf)
         (append-map-inf g2 s-inf))
        (else (lambda ()
                (loop (s-inf))))))))

(define (once-rt g)
  (lambda (s)
    (let loop ((s-inf (g s)))
      (cond
        ((null? s-inf) '())
        ((pair? s-inf)
         (cons (car s-inf) '()))
        (else (lambda ()
                (loop (s-inf))))))))
