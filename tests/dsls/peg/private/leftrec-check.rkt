#lang racket/base

(provide
 run-leftrec-check!
 expanded-defs)

(require
  syntax/id-table
  syntax/parse
  "../../../../private/ee-lib/persistent-id-table.rkt"
  (except-in "../../../../private/ee-lib/main.rkt" racket-var)

  (for-template
   "forms.rkt"))

(define-local-symbol-table expanded-defs)
(define-persistent-symbol-table def-nullable?)
(define-local-symbol-table entered)

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
    [(? e) #t]
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
    [((~datum #%nonterm-ref) name:id)
     (nullable-nonterminal? #'name)]
    [(src-span v e)
     (nullable? #'e)]
    [_ (raise-syntax-error #f "not a core peg form" this-syntax)]))

;; MB note: I thought it might be a problem to raise an error when reaching a previously
;; entered nt, because we might record it as entered when checking one nonterminal and then
;; enter it again for another. But it's okay because if we check it successfully, then it's in
;; def-nullable? and we get 'nullable or 'not-nullable instead.
(define (nullable-nonterminal? id)
  (case (or (symbol-table-ref def-nullable? id #f)
            (symbol-table-ref entered id #f)
            'unvisited)
    [(nullable) #t]
    [(not-nullable) #f]
    [(entered) (raise-syntax-error #f "left recursion through nonterminal" id)]
    [(unvisited)
     (symbol-table-set! entered id 'entered)
     (define rhs (symbol-table-ref expanded-defs id))
     (define res (nullable? rhs))
     (symbol-table-set! def-nullable? id (if res 'nullable 'not-nullable))
     res]))

#;((listof identifier?) (listof syntax?) -> void?)
; run a leftrec check on the given block of mutually recursive peg defs
(define (run-leftrec-check! names pegs)
  (for ([name names] [rhs pegs])
    (symbol-table-set! expanded-defs name rhs))
  (for ([name names])
    (nullable-nonterminal? name)))
