#lang racket/base

(require
  rackunit
  (for-syntax racket/base "../main.rkt"))

(define-syntax (m stx)
  (define ctx (syntax-local-make-definition-context))
  (define id^ (car (syntax-local-bind-syntaxes (list #'x) #'5 ctx)))
  #``([unbound/unbound #,(same-binding? #'x #'x)]
      [local/local #,(same-binding? id^ id^)]
      [unbound/local #,(same-binding? #'x id^)]))

(check-equal?
 (m)
 '([unbound/unbound #f]
   [local/local #t]
   [unbound/local #f]))

; top level, unbound
(check-equal?
 (eval '(begin
          (require "../main.rkt")
          (same-binding? #'x #'x))
       (make-base-namespace))
 #f)

; top level, bound
(check-equal?
 (eval '(begin
          (require "../main.rkt")
          (define x 5)
          (same-binding? #'x #'x))
       (make-base-namespace))
 #t)