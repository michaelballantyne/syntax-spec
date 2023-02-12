#lang racket/base

(provide
 lift-leftrec-check!
 expanded-defs)

(require
  syntax/id-table
  syntax/parse
  ee-lib/persistent-id-table
  (except-in ee-lib racket-var)

  (for-template
   "forms.rkt"))

(define-persistent-free-id-table expanded-defs)
(define checked-leftrec #f)
(define-persistent-free-id-table def-nullable?)
(define entered (make-free-id-table))

(define (nullable? stx)
  (syntax-parse stx
    #:literal-sets (peg-literals)
    [eps #t]
    [(seq e1 e2)
     (and (nullable? #'e1)
          (nullable? #'e2))]
    [(plain-alt e1 e2)
     (or (nullable? #'e1)
         (nullable? #'e2))]
    [(alt e1 e2)
     (or (nullable? #'e1)
         (nullable? #'e2))]
    [(* e) #t]
    [(! e)
     (not (nullable? #'e))]
    [(bind x e)
     (nullable? #'e)]
    [(=> pe e)
     (nullable? #'pe)]
    [(text t) #f]
    [(token f) #f]
    [(char f) #f]
    [name:id
     (nullable-nonterminal? #'name)]
    [(:src-span v e)
     (nullable? #'e)]
    [_ (raise-syntax-error #f "not a core peg form" this-syntax)]))

(define (nullable-nonterminal? id)
  (case (or (persistent-free-id-table-ref def-nullable? id (lambda () #f)) (free-id-table-ref entered id (lambda () #f)) 'unvisited)
    [(nullable) #t]
    [(not-nullable) #f]
    [(entered) (raise-syntax-error #f "left recursion through nonterminal" id)]
    [(unvisited)
     (free-id-table-set! entered id 'entered)
     (define res (nullable? (persistent-free-id-table-ref expanded-defs id)))
     (persistent-free-id-table-set! def-nullable? id (if res 'nullable 'not-nullable))
     res]))

(define (check-leftrec)
  (when (not checked-leftrec)
    (for ([(k v) (in-persistent-free-id-table expanded-defs)])
      (nullable-nonterminal? k))))


(module apply-for-syntax racket/base
  (require (for-syntax racket/base syntax/parse racket/syntax ee-lib/persistent-id-table))
  (provide apply-for-syntax)
  (define-syntax apply-for-syntax
    (wrap-persist
      (syntax-parser
        [(_ id)
         ((syntax-local-eval #'id))
         #'(begin)]))))
(require (for-template 'apply-for-syntax))

(define (lift-leftrec-check! name rhs)
  (persistent-free-id-table-set!
   expanded-defs
   (compiled-from (syntax-local-introduce name))
   (syntax-local-introduce rhs))
  
  (syntax-local-lift-module-end-declaration
   #'(apply-for-syntax check-leftrec)))