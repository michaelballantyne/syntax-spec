#lang racket/base

(provide (all-defined-out))
(require "cmdline.rkt" (for-syntax racket/base syntax/parse))

(define-option-syntax switch/o
  (syntax-parser
    [(_ flags:flag-names desc:string)
     #'(choice/o #:default #f [flags desc #t])]))

(define-option-syntax list/o
  (syntax-parser
    [(_ names:flag-names arg:arg-spec desc:string)
     #'(multi/o '()
              [names arg desc (lambda (acc) (append acc (list arg.name)))])]))

(define (int-range/p min max)
  (lambda (s)
    (define n (string->number s))
    (unless (and (integer? n) (>= n min) (<= n max))
      (raise-user-error (format "expected integer between ~a and ~a" min max)))
    n))

(define-option-syntax optional/o
  (syntax-parser
    [(_ #:default default-expr rest ...)
     #'(choice/o #:default default-expr [rest ...])]))

(define-option-syntax required/o
  (syntax-parser
    [(_ rest ...)
     #'(choice/o #:required [rest ...])]))

(define-flag-syntax numbered-flags/f
  (syntax-parser
    [(_ flags:flag-names [min:number max:number] desc:string)
     (define/syntax-parse (f ...)
       (for/list ([n (in-range (syntax-e #'min) (+ 1 (syntax-e #'max)))])
         (define/syntax-parse names (for/list ([s (syntax->datum #'flags.names)]) (format "~a~a" s n)))
         (define/syntax-parse this-desc (format "set ~a to ~a" (syntax-e #'desc) n))
         #`[names this-desc #,n]))
     #'(begin f ...)]))
