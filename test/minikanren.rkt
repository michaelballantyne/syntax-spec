#lang racket/base

(require "../syntax/define-nonterminal.rkt"
         (for-syntax racket/base syntax/parse racket/pretty))

(define-binding-class term-variable "miniKanren term variable")
(define-binding-class relation-name "miniKanren relation name")
(define-extension-class term-macro)
(define-extension-class goal-macro)

(define-nonterminal quoted
  #:description "quoted value"
  n:number
  s:id
  ()
  (a:quoted . d:quoted))


(define-nonterminal term
  #:description "miniKanren term"
  n:number
  x:term-variable
  (quote t:quoted)
  (cons t1:term t2:term))

(define-nonterminal relation-args
  #:description "relation arguments list"
  ()
  (t:term . a:relation-args))

(define-nonterminal goal
  #:description "miniKanren goal"
  #:allow-extension goal-macro

  (== t1:term t2:term)
  (=/= t1:term t2:term)
  (absento t1:term t2:term)
  (symbolo t:term)
  (numbero t:term)
  (stringo t:term)

  (disj2 g1:goal g2:goal)
  (conj2 g1:goal g2:goal)
  
  (fresh1 (x:term-variable ...) b:goal)
  #:binding {(! x) b}

  (r:relation-name . a:relation-args)
  )

; Simulated interface macros
(define-syntax mk
  (syntax-parser
    [(_ e)
     (pretty-display (syntax->datum ((nonterminal-expander goal) #'e)))
     #'(void)]))

(define-syntax define-relation
  (syntax-parser
    [(_ (name:id arg:id ...) b)
     #'(begin
        (define-syntax name ((binding-class-constructor relation-name)))
        (mk (fresh (arg ...) b)))]))

; Surface syntax
(define-syntax conj
  (goal-macro
   (syntax-parser
     [(_ g) #'g]
     [(_ g1 g* ...) #'(conj2 g1 (conj g* ...))])))

(define-syntax fresh
  (goal-macro
   (syntax-parser
     [(_ (x:id ...+) b ...+)
      #'(fresh1 (x ...) (conj b ...))])))

(define-syntax conde
  (goal-macro
   (syntax-parser
     [(_ [g0 g1 ...] c* ...)
      (syntax-parse #'(c* ...)
        [() #'(conj g0 g1 ...)]
        [_  #'(disj2
               (conj g0 g1 ...)
               (conde c* ...))])])))

(define-relation (appendo l1 l2 l3)
  (conde
   [(== l1 '()) (== l3 l2)]  ; base case
   [(fresh (head rest result) ; recursive case
      (== (cons head rest) l1)
      (== (cons head result) l3)
      (appendo rest l2 result))]))