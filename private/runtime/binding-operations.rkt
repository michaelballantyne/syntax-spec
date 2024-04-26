#lang racket/base

(provide free-identifiers
         alpha-equivalent?)

(require racket/list
         syntax/parse
         syntax/id-table
         ee-lib
         (for-template "./compile.rkt"))

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
  (define binders (all-binders stx allow-host?))
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

;; Syntax, Boolean -> (ListOf Identifier)
;; The result list has no duplicates as renaming ensures all binders are distinct.
(define (all-binders stx allow-host?)
  (syntax-parse stx
    [((~literal #%host-expression) . _)
     (raise-host-expression-error-or-value
      'free-identifiers
      allow-host?
      (list))]
    [(a . b)
     (append (all-binders #'a allow-host?)
             (all-binders #'b allow-host?))]
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
  (define table-a (make-free-id-table))
  (define table-b (make-free-id-table))
  (define (bind! identifier-a identifier-b)
    (define x (gensym))
    (free-id-table-set! table-a identifier-a x)
    (free-id-table-set! table-b identifier-b x))
  (define (reference=? identifier-a identifier-b)
    (eq? (free-id-table-ref table-a identifier-a (gensym))
         (free-id-table-ref table-b identifier-b (gensym))))
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
          (bind! #'a #'b)
          #t]
         [(and (compiled-reference? #'a)
               (compiled-reference? #'b))
          (or (reference=? #'a #'b)
              ; if they're free references
              (identifier=? #'a #'b))]
         [else (free-identifier=? #'a #'b)])]
      [(() ()) #t]
      [((a-car . a-cdr) (b-car . b-cdr))
       (and (loop #'a-car #'b-car)
            (loop #'a-cdr #'b-cdr))]
      [(a b) (equal? (syntax->datum #'a)
                     (syntax->datum #'b))])))
