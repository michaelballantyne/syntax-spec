#lang racket/base

(provide raise-argument-error/stx (struct-out exn:fail:contract:srcloc))

(define-struct (exn:fail:contract:srcloc
                exn:fail:contract)
  (srcloc)
  #:property prop:exn:srclocs
  (lambda (s)
    (list (exn:fail:contract:srcloc-srcloc s))))

(define (syntax->srcloc blame-stx)
  (srcloc
   (syntax-source blame-stx)
   (syntax-line blame-stx)
   (syntax-column blame-stx)
   (syntax-position blame-stx)
   (syntax-span blame-stx)))

(define (raise-argument-error/stx name expected v stx)
  (raise (exn:fail:contract:srcloc
          (format "~a: ~a: contract violation\n  expected: ~a\n  given:  ~a"
                  (srcloc->string (syntax->srcloc stx))
                  name
                  expected
                  v)
          (current-continuation-marks)
          (syntax->srcloc stx))))


