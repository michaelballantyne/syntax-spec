#lang racket/base

(require (for-syntax racket/base racket/list "flip-intro-scope.rkt"))

(provide
 (for-syntax
  lift-from-properties!
  lift-disappeared-uses!
  lift-disappeared-bindings!))

(begin-for-syntax
  (define lifted? #f)
  (define lifted-properties (make-hasheq))

  (define (ensure-module-end!)
    (when (not lifted?)
      (syntax-local-lift-expression #'(emit-lifteds)))
    (set! lifted? #t))

  ; Lift properties conveying information to the IDE from stx. Return stx without these props.
  (define (lift-from-properties! stx)
    (for/fold ([stx stx])
              ([prop-key '(disappeared-use disappeared-binding sub-range-binders mouse-over-tooltips)])
      (cond
        [(syntax-property stx prop-key)
         (lift-property! prop-key (syntax-property stx prop-key))
         (syntax-property-remove stx prop-key)]
        [else stx])))

  (define (lift-property! prop-key prop-val)
    (ensure-module-end!)
    (hash-update! lifted-properties prop-key (lambda (old-val) (cons prop-val old-val)) '()))

  (define (lift-disappeared-uses! . ids)
    (lift-property! 'disappeared-use (map flip-intro-scope ids)))

  (define (lift-disappeared-bindings! . ids)
    (lift-property! 'disappeared-binding (map flip-intro-scope ids))))

(define-syntax (emit-lifteds stx)
  (define result
    (for/fold ([result #'(void)])
              ([(prop-key prop-val) (in-hash lifted-properties)])
      (syntax-property result prop-key (flatten prop-val))))
  ; Reset so another end declaration lift is triggered if macros
  ; that are themselves end declarations lift disappeareds.
  (set! lifted? #f)
  (set! lifted-properties (make-hasheq))

  result)
