#lang racket/base

(provide flip-intro-scope)

(require racket/private/check)

(define (make-intro-scope-introducer)
  (define no-scope (datum->syntax #f 'foo))
  (define intro-scope
    (syntax-local-identifier-as-binding
     (syntax-local-introduce
      no-scope)))
  (make-syntax-delta-introducer
   intro-scope
   no-scope))

(define/who (flip-intro-scope stx)
  (check who syntax? stx)
  ((make-intro-scope-introducer) stx 'flip))
