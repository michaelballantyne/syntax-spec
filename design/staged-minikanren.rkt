#lang racket/base

(provide (all-defined-out)
         quote cons
         (for-space mk quasiquote))

(require "../main.rkt"
         "../private/ee-lib/errors.rkt"
         (for-syntax
          racket/base
          syntax/parse
          syntax/id-table
          (except-in "../private/ee-lib/main.rkt" racket-var)))

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

    (conj2 g1:goal g2:goal)
  
    (fresh1 (x:term-variable) b:goal)
    #:binding (scope (bind x) b)

    ; new reified call datatype
    (reify-call )
    (apply-reified )
    
    ; core staging extension
    (later g:goal)
    (now g:goal)

    (r:relation-name t:term ...+))

  (nonterminal staged-goal
    #:description "miniKanren goal"
    #:allow-extension goal-macro

    succeed
    fail
    
    (== t1:term t2:term)

    (condg
     fallback-goal:dynamic-goal
     staged-condg-clause ...)
    
    (conj2 g1:goal g2:goal)
  
    (fresh1 (x:term-variable) b:goal)
    #:binding (scope (bind x) b)

    ; new reified call datatype
    (lreify-call )
    (reify-call )
    (apply-reified )

    (l== t1:term t2:term) ; need equivalents for other constraints
    (ldisj g1:goal g2:goal)
    (lapp r:relation-name t:term ...+)

    ; do we really want specialized macros like all these? or should the
    ; staging-goals be more like surface goals.

    (r:relation-name t:term ...+))
  
  (nonterminal condg-clause
    ([x:term-variable ...] [guard:goal] [body:goal])
    #:binding (scope (bind x) guard body))

  (host-interface/definition
    (defrel/condg (r:relation-name arg:term-variable ...)
      clause:condg-clause ...)
    #:binding [(export r) (scope (bind arg) clause)]

    #:lhs [#'r]
    #:rhs [#'(void)]))


