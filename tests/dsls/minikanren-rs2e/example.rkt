#lang racket/base

(require "mk.rkt" (except-in rackunit fail) syntax/macro-testing)

;; Cross module definition and use

(module a racket/base
  (require "mk.rkt")
  (provide appendo)

  (defrel (appendo l1 l2 l3)
    (conde
     [(== l1 '()) (== l2 l3)]
     [(fresh (first rest res)
        (== l1 (cons first rest))
        (== l3 (cons first res))
        (appendo rest l2 res))])))

(require 'a)
(check-equal?
 (run* (l1 l2)
   (appendo l1 l2 '(1 2 3 4)))
 '((() (1 2 3 4)) ((1) (2 3 4)) ((1 2) (3 4)) ((1 2 3) (4)) ((1 2 3 4) ())))


;; Use in Racket definition RHS before definiton

(let ()
  (define (f) (run 1 (q) (is-five q)))
  (defrel (is-five a)
    (== a 5))
    
  (check-equal?
   (f)
   '((5))))

;; Mutual recursion between relations

(let ()
  (defrel (eveno x)
    (conde
     [(== x 'z)]
     [(fresh (x-1)
        (== x `(s ,x-1))
        (oddo x-1))]))
    
  (defrel (oddo x)
    (fresh (x-1)
      (== x `(s ,x-1))
      (eveno x-1)))

  (check-equal?
   (run 4 (q)
     (eveno q))
   '((z) ((s (s z))) ((s (s (s (s z))))) ((s (s (s (s (s (s z))))))))))


;; `project`

(let ([x_1 #f])
  (check-equal?
   (run 1 (q)
     (fresh (x)
       (project (x)
                (let ()
                  (set! x_1 x)
                  ;; Goal expressions within project may refer to non-projected variables
                  (goal-expression (== q 5))))))
   '((5)))
  (check-true (vector? x_1)))

(check-exn
 #rx"q: only projected logic variables may be used from Racket code"
 (lambda ()
   (convert-compile-time-error
    (run 1 (q)
      (fresh (x)
        (project (x)
                 (let ()
                   ;; Racket expressions may only refer to the projected variables;
                   ;; this will be a compile-time error.
                   (displayln q)
                   (goal-expression (== x 1)))))))))
  
(let ([x_1 #f]
      [x_2 #f])
  (run 1 (q)
    (fresh (x)
      (project (x)
               (let ()
                 (set! x_1 x)
                 (goal-expression (== x 5)))
               (let ()
                 ;; Value will be same as before; it's frozen when we enter project.
                 (set! x_2 x)
                 (goal-expression succeed)))))
  (check-true (vector? x_1))
  (check-true (vector? x_2)))

(let ([x_1 #f]
      [x_2 #f])
  (run 1 (q)
    (fresh (x)
      (project (x)
               (let ()
                 (set! x_1 x)
                 (goal-expression (== x 5)))
               (goal-expression
                (project (x)
                         ;; Nested project yields an updated variable
                         (let ()
                           (set! x_2 x)
                           (goal-expression succeed)))))))
  (check-true (vector? x_1))
  (check-equal? x_2 5))

(check-exn
 #rx"q: only projected logic variables may be used from Racket code"
 (lambda ()
   (convert-compile-time-error
    (run 1 (q)
      (project (q)
               (goal-expression (== q 5)))
      (project ()
               (let ()
                 ;; This reference should be an error as the variable is not projected
                 ;; in a lexically-surrounding project.
                 (displayln q)
                 (goal-expression (== q 5))))))))


(check-exn
 #rx"q: only projected logic variables may be used from Racket code"
 (lambda ()
   (convert-compile-time-error
    (run 1 (q)
      (project (q)
               (goal-expression
                (project ()
                         ;; Currently only vars projected by the immediately-surrounding
                         ;; project may be referenced; this will be an error.
                         (let ()
                           (displayln q)
                           (goal-expression (== q 5))))))))))

