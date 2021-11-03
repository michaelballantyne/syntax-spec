#lang racket

;; surface syntax (PEG)
;; two-pass-spec := (^ v:var-ref ...) ... (^ v:nonterm-ref ...) ... one-pass-spec
;; one-pass-spec := (! v:var-ref ...) ... (rec v:nonterm-ref ...) ... one-pass-spec-ref ...
;; one-pass-spec-ref := 


(define (elaborate-spec stx)
  (syntax-parse stx
    #:datum-literals (! ^ nest)
    [v:nonref-id
     ]
    [(! v:nonref-id ...+)
     ]
    [(^ v:ref-id ...+)
     ]
    [(nest v e)
     ]
    [(~braces spec ...)
     ]
    [(~brackets spec ...)
     ]))

;; Checks that the spec is well-ordered:
;; - No exports
;; - Bindings in each scope come first; then `rec`; then references, non-rec subexpressions, and nested scopes
(define (check-one-pass-order stx)
  (check-as-one-pass
   stx
   
  )

(define export-in-scope-error
  "")

(define (check-as-one-pass stx export-error)
  (define (bindings stx)
    (syntax-parse stx
      #:datum-literals (! ^ nest)
      [v:nonref-id
       ]
      [(! v:nonref-id ...+)
       ]
      [(^ v:ref-id ...+)
       ]
      [(nest v e)
       ]
      [(~braces spec ...)
       ]
      [(~brackets spec ...)
       ])))

  (define (recs stx)
    )

  (define (rest stx)

    )

  (bindings stx))

; - Exports all come first; exports of bindings, then exports of subexpressions.
; - Then the remainder of the spec is well-ordered as a one-pass spec.
(define (check-two-pass-order stx)
  (syntax-parse stx
    []))