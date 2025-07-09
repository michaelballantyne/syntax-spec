#lang racket/base

(provide free-identifiers
         binding-identifiers
         identifier=?
         alpha-equivalent?
         subst
         get-racket-referenced-identifiers
         (rename-out [identifier=? compiled-identifier=?]))

(require racket/list
         racket/dict
         racket/sequence
         syntax/parse
         syntax/id-table
         "../ee-lib/main.rkt"
         (for-template racket/base "./compile.rkt"))

;; Currently we use this as the notion of identifier equality:
(define (identifier=? x y) (free-identifier=? (compiled-from x) (compiled-from y)))

;; Some drawbacks:
;;   When we add a renaming operation, free-identifiers won't work properly for syntax
;;   containing a mix of renamed and un-renamed names.
;;
;;   I'm not sure how name generation in backend compilers and generate-prog can work
;;   together with this. Do we need a name generation operation that also generates
;;   fresh compiled-from identities? This feels like the wrong thing.
;;
;;       It looks like minikanren-ee alpha=? currently uses free-identifier=? equality,
;;       but generate-prog uses generate-temporaries. I think that combination is broken.
;;
;;           The symbols resulting from generate-temporaries may collide with existing symbols,
;;           and when the names are unbound they could be free-identifier=?.
;;
;;           We use phase1-eval for all our testing, so I could generate bindings via
;;           syntax-local-bind-syntaxes.
;;
;; Problems with other kinds of equality:
;;   bound-identifier=?: compile-reference applies syntax-local-get-shadower, so our
;;   references have scopes that the binders do not.
;;
;;   free-identifer=?: compiled names are not yet bound (well, they may or may not be
;;   bound) and do not have unique symbols.
;;
;; We don't use syntax/id-set because it's plenty simple enough without,
;; and it's nice to have fewer dependencies to load.

;; Syntax, [#:allow-host? Boolean] -> (ListOf Identifier)
(define (free-identifiers stx #:allow-host? [allow-host? #f])
  (define refs (deduplicate-references (all-references stx allow-host?)))
  (define binders (binding-identifiers stx #:allow-host? allow-host?))
  (subtract-identifiers refs binders))

;; Syntax, Boolean -> (ListOf Identifier)
;; The result list may have duplicates.
(define (all-references stx allow-host?)
  (syntax-parse stx
    [((~literal #%host-expression) . _)
     (raise-host-expression-error-or-value
      'free-identifiers
      allow-host?
      (list))]
    [(a . b) (append (all-references #'a allow-host?)
                     (all-references #'b allow-host?))]
    [x:id (if (compiled-reference? #'x)
              (list #'x)
              (list))]
    [_ (list)]))

;; Syntax, [#:allow-host? Boolean] -> (ListOf Identifier)
;; The result list has no duplicates as renaming ensures all binders are distinct.
(define (binding-identifiers stx #:allow-host? [allow-host? #f])
  (syntax-parse stx
    [((~literal #%host-expression) . _)
     (raise-host-expression-error-or-value
      'free-identifiers
      allow-host?
      (list))]
    [(a . b)
     (append (binding-identifiers #'a #:allow-host? allow-host?)
             (binding-identifiers #'b #:allow-host? allow-host?))]
    [x:id (if (compiled-binder? #'x)
              (list #'x)
              (list))]
    [_ (list)]))

(define (raise-host-expression-error-or-value who-sym allow-host? value-if-allowed)
  (if allow-host?
      value-if-allowed
      (error who-sym "can't enter a #%host-expression")))

(define (deduplicate-references ids)
  (remove-duplicates ids identifier=?))

(define (subtract-identifiers xs ys)
  (for/list ([x xs]
             #:unless (member x ys identifier=?))
    x))

; Syntax, Syntax [#:allow-host? Boolean] -> Boolean
; Are the two expressions alpha-equivalent?
(define (alpha-equivalent? stx-a stx-b #:allow-host? [allow-host? #f])
  (define bound-reference=? (syntaxes->bound-reference=? stx-a stx-b allow-host?))
  (and bound-reference=?
       (alpha-equivalent?/references stx-a stx-b bound-reference=? allow-host?)))

; Syntax Syntax Boolean -> (or/c #f (Identifier Identifier -> Boolean))
; if the two terms have corresponding binders, build bound-reference=?
; If they have different numbers of binders, return #f
; bound-reference=? answers "do these two references refer to corresponding binders?"
(define (syntaxes->bound-reference=? stx-a stx-b allow-host?)
  (define table-a (make-free-id-table))
  (define table-b (make-free-id-table))
  ;; associate both binders with the same gensym
  (define (bind! identifier-a identifier-b)
    (define x (gensym))
    (free-id-table-set! table-a (compiled-from identifier-a) x)
    (free-id-table-set! table-b (compiled-from identifier-b) x))
  (define binders-a (binding-identifiers stx-a #:allow-host? allow-host?))
  (define binders-b (binding-identifiers stx-b #:allow-host? allow-host?))
  ; must traverse binders before references
  ; in case a variable is referenced before it is bound,
  ; like mutual recursion
  (for ([binder-a binders-a]
        [binder-b binders-b])
    (bind! binder-a binder-b))
  (and (= (length binders-a) (length binders-b))
       (substitutions->bound-reference=? table-a table-b)))

;; FreeIdTable FreeIdTable -> (Identifier Identifier -> Boolean)
;; Do these two references refer to corresponding binders?
;; table-a and table-b should map corresponding binders to the same, unique value
(define ((substitutions->bound-reference=? table-a table-b) identifier-a identifier-b)
  (and (dict-has-key? table-a (compiled-from identifier-a))
       (dict-has-key? table-b (compiled-from identifier-b))
       (eq? (free-id-table-ref table-a (compiled-from identifier-a))
            (free-id-table-ref table-b (compiled-from identifier-b)))))

; Syntax Syntax (Identifier Identifier -> Boolean) Boolean -> Boolean
; check that the references are alpha-equivalent.
(define (alpha-equivalent?/references stx-a stx-b bound-reference=? allow-host?)
  (let loop ([stx-a stx-a] [stx-b stx-b])
    (syntax-parse (list stx-a stx-b)
      [(~or (((~literal #%host-expression) . _) _)
            (_ ((~literal #%host-expression) . _)))
       (raise-host-expression-error-or-value
        'alpha-equivalent?
        allow-host?
        #f)]
      [(a:id b:id)
       (cond
         [(and (compiled-binder? #'a)
               (compiled-binder? #'b))
          ; bindings assumed to be equivalent
          #t]
         [(and (compiled-reference? #'a)
               (compiled-reference? #'b))
          (or (bound-reference=? #'a #'b)
              ; if they're free references
              (identifier=? #'a #'b))]
         [else (free-identifier=? #'a #'b)])]
      [(() ()) #t]
      [((a-car . a-cdr) (b-car . b-cdr))
       (and (loop #'a-car #'b-car)
            (loop #'a-cdr #'b-cdr))]
      [(a b) (equal? (syntax->datum #'a)
                     (syntax->datum #'b))])))

;; Syntax Syntax Syntax -> Syntax
;; Replace all occurrences of target (by alpha equivalence) with replacement in stx.
;; Leaves host expressions unchanged.
(define (subst stx target replacement)
  (let loop ([stx stx])
    (if (if (compiled-binder? target)
            (and (compiled-reference? stx) (identifier=? stx target))
            (alpha-equivalent? stx target))
        replacement
        (syntax-parse stx
          ;; ignore host expressions
          [((~literal #%host-expression) . _) this-syntax]
          [(a . b)
           (quasisyntax/loc this-syntax (#,(loop #'a) . #,(loop #'b)))]
          [_ stx]))))

(define current-referenced-vars (make-parameter #f))

; get the racket vars referenced in e of the provided binding classes
(define-syntax-rule (get-racket-referenced-identifiers [binding-class ...] e)
  (get-racket-referenced-identifiers/proc (list #'binding-class ...) e))

; (listof Identifier) Syntax -> (listof Identifier)
(define (get-racket-referenced-identifiers/proc binding-classes e)
  (define/syntax-parse (binding-class ...) binding-classes)
  (parameterize ([current-referenced-vars (local-symbol-set)])
    (local-expand #`(with-reference-compilers ([binding-class recording-reference-compiler] ...)
                      #,e)
                  'expression
                  '())

    (sequence->list (in-symbol-set (current-referenced-vars)))))

(define recording-reference-compiler
  (make-variable-like-reference-compiler
   ;; emit syntax instead of raw reference.
   ;; this is necessary for languages that don't emit bindings for
   ;; racket-reference-able identifiers.
   (lambda (x) (symbol-set-add! (current-referenced-vars) x) #`#'#,x)
   (lambda (e)
     (syntax-parse e
       [(set! x _)
        (symbol-set-add! (current-referenced-vars) #'x)
        #'#'x]))))
