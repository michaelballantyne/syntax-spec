#lang racket/base

(require "../../main.rkt"
         rackunit
         (for-syntax racket/base syntax/parse racket/pretty))

(define-hosted-syntaxes
  (binding-class term-variable #:description "miniKanren term variable")
  (binding-class relation-name #:description "miniKanren relation name")
  
  (extension-class term-macro)
  (extension-class goal-macro)
  
  (nonterminal quoted
    #:description "quoted value"
    n:number
    s:id
    ()
    (a:quoted . d:quoted))

  (nonterminal term
    #:description "miniKanren term"
    n:number
    x:term-variable
    (quote t:quoted)
    (cons t1:term t2:term))

  (nonterminal goal
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
    
    (r:relation-name t:term ...+)))
  
; Simulated interface macros
(define-syntax mk
  (syntax-parser
    [(_ e) #``#,((nonterminal-expander goal) #'e)]))

; Surface syntax
(define-syntax conj
  (goal-macro
   (syntax-parser
     [(_ g) #'g]
     [(_ g1 g2 g* ...) #'(conj (conj2 g1 g2) g* ...)])))

(define-syntax disj
  (goal-macro
   (syntax-parser
     [(_ g) #'g]
     [(_ g1 g* ...) #'(disj2 g1 (disj g* ...))])))

(define-syntax fresh
  (goal-macro
   (syntax-parser
     [(_ (x:id ...+) b ...+)
      #'(fresh1 (x ...) (conj b ...))])))

(define-syntax conde
  (goal-macro
   (syntax-parser
     [(_ [g ...+] ...+)
      #'(disj
         (conj g ...)
         ...)])))

(define-syntax appendo ((binding-class-constructor relation-name)))

(define expanded
  (mk (fresh (l1 l2 l3)
             (conde
              [(== l1 '()) (== l3 l2)]  ; base case
              [(fresh (head rest result) ; recursive case
                      (== (cons head rest) l1)
                      (== (cons head result) l3)
                      (appendo rest l2 result))]))))

(check-equal?
 expanded
 `(fresh1 (l1 l2 l3)
          (disj2
           (conj2 (== l1 '()) (== l3 l2))
           (fresh1 (head rest result)
                   (conj2 (conj2 (== (cons head rest) l1)
                                 (== (cons head result) l3))
                          (appendo rest l2 result))))))
