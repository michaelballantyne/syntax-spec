#lang racket/base

(require "../../testing.rkt")

(provide (all-defined-out) (for-syntax term-variable (all-defined-out)))

(syntax-spec
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
   #:allow-extension term-macro
    
   n:number
   (#%term-ref x:term-variable)
   (quote t:quoted)
   (cons t1:term t2:term)
    
   (rkt e:racket-expr)
   #:binding e

   (~> v:id
       (with-syntax ([#%term-ref (datum->syntax this-syntax '#%term-ref)])
         #'(#%term-ref v))))

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
   #:binding (scope (bind x) b)

   (#%rel-app r:relation-name t:term ...+)

   (~> (name:id arg ...)
       (with-syntax ([#%rel-app (datum->syntax this-syntax '#%rel-app)])
         #'(#%rel-app name arg ...)))))

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

(syntax-spec
  (host-interface/definition
    (define-relation/stub name:relation-name)
    #:binding (export name)

    #:lhs
    [#'name]
    #:rhs
    [#'(void)]))

(define-relation/stub appendo)

(define expanded
  (expand-nonterminal/datum goal
    (fresh (l1 l2 l3)
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
           (conj2 (== (#%term-ref l1) '()) (== (#%term-ref l3) (#%term-ref l2)))
           (fresh1 (head rest result)
                   (conj2 (conj2 (== (cons (#%term-ref head) (#%term-ref rest)) (#%term-ref l1))
                                 (== (cons (#%term-ref head) (#%term-ref result)) (#%term-ref l3)))
                          (#%rel-app appendo (#%term-ref rest) (#%term-ref l2) (#%term-ref result)))))))


; Test interposition point; separate submodule so we can rename-in the core #%rel-app.
(module* test racket
  (require "../../testing.rkt"
           (rename-in (submod "..") [#%rel-app core-#%rel-app])
           )

  (define-syntax #%rel-app
    (goal-macro
     (syntax-parser
       [(_ name arg ...)
        #'(fresh (foo)
            (core-#%rel-app name arg ...))])))

  (check-equal?
   (expand-nonterminal/datum goal
     (fresh (l1 l2 l3)
       (appendo l1 l2 l3)))
   `(fresh1 (l1 l2 l3)
            (fresh1 (foo)
                    (#%rel-app appendo (#%term-ref l1) (#%term-ref l2) (#%term-ref l3))))))

  
  
