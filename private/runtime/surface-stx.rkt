#lang racket/base

(provide syntax-surface-stx
         annotate-surface-stx)

(define surface-prop (gensym))

;; Future, efficient implementation idea: have syntax-spec interface
;; macros cache a version of the initial syntax object they receive with
;; the lexical context removed, and make this
;; object available to the expander via a parameter.
;;
;; Then, the surface syntax annotation will store that top-level syntax
;; object plus a source location. The accessor will search the syntax object
;; for a portion that matches the source location.
;;
;; Overall, this should mean that we only need to store a constant size annotation
;; for each AST node, plus the original syntax object for the source program.
;; Looking up the appropriate part could be linear in the size of the source program
;; or at least its depth, but would only happen when we look up the surface syntax
;; to raise an error, or generate code for a contract. So, occasionally.


;; (-> Syntax Syntax))
;; The returned syntax always has an empty lexical context.
;; THROWS if applied to syntax that does not have an annotated surface syntax
;;   applied by syntax-spec.
(define (syntax-surface-stx stx)
  (define prop-val (syntax-property stx surface-prop))
  (unless prop-val
    (error 'syntax-surface-stx "Requires syntax annotated with surface stx"))
  (datum->syntax #f (syntax->datum prop-val) prop-val))

(define (annotate-surface-stx stx-expanded stx-surface)
  (let ([prop-on-surface (syntax-property stx-surface surface-prop)])
    ;; When a macro expands to a subexpression, throwing itself away, the
    ;; result will be syntax-original and we don't want to call the macro
    ;; that expanded to it its surface-stx. But we do want to record the
    ;; original unexpanded version of that form, before sub-forms get traversed.
    (if (syntax-original? (syntax-local-introduce stx-expanded))
       (syntax-property stx-expanded surface-prop stx-expanded)
       (syntax-property
        stx-expanded surface-prop
        (or prop-on-surface stx-surface)))))