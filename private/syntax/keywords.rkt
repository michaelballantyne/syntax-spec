#lang racket/base

(provide ~>)

(require (for-syntax racket/base))

(define-syntax ~>
  (lambda (stx) (raise-syntax-error #f "keyword used out of context" stx)))