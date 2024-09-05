#lang racket

(provide compile-defrel compile-run)

(require
  "runtime.rkt"
  (for-syntax syntax/parse))

(define-syntax compile-defrel
  (syntax-parser
    [(_ name (x ...) g)
     #'(lambda (x ...)
         (lambda (s)
           (lambda ()
             (#%app (compile-goal g) s))))]))

(define-syntax compile-run
  (syntax-parser
    [(_ n (q) g)
     #'(let ([q (var 'q)])
        (map (reify q)
             (run-goal n (compile-goal g))))]))

(define-syntax compile-goal
  (syntax-parser
    #:datum-literals (succeed fail == disj2 conj2 fresh1)
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
    [(_ (relname t ...))      
     #'(relname (compile-term t) ...)]))
  
(define-syntax compile-term
  (syntax-parser
    #:datum-literals (quote cons)
    [(_ n:number)
     #''n]
    [(_ x:id)
     #'x]
    [(_ (quote t))
     #''t]
    [(_ (cons t1 t2))
     #`(cons (compile-term t1) (compile-term t2))]))