#lang racket/base

(provide get-bound-vars
         get-free-vars)

(require racket/list
         syntax/parse
         ee-lib
         "./compile.rkt")

; all operations in this module ignore host expressions

; TODO symbol sets

; get the variables bound in this expression.
(define (get-bound-vars stx)
  ; you can just use a list bc of renaming
  (syntax-parse stx
    [((~literal #%host-expression) . _) (list)]
    [(a . b) (append (get-bound-vars #'a) (get-bound-vars #'b))]
    [x:id (if (compiled-binder? #'x)
              (list #'x)
              (list))]
    [_ (list)]))
; for the match pattern (not x), this will think x is a bound var.

; get the free variables found in stx (deduplicated modulo free-identifier=?).
(define (get-free-vars stx)
  (define refs (deduplicate-references (get-compiled-references stx)))
  (define binders (get-bound-vars stx))
  (subtract-identifiers refs binders))

; get all compiled references
(define (get-compiled-references stx)
  (syntax-parse stx
    [((~literal #%host-expression) . _) (list)]
    [(a . b) (append (get-compiled-references #'a) (get-compiled-references #'b))]
    [x:id (if (compiled-reference? #'x)
              (list #'x)
              (list))]
    [_ (list)]))

(define (deduplicate-references ids)
  (remove-duplicates ids same-identifier?))

(define (same-identifier? x y) (free-identifier=? (compiled-from x) (compiled-from y)))

(define (subtract-identifiers xs ys)
  (for/list ([x xs]
             #:unless (member x ys same-identifier?))
    x))
