#lang racket/base

(require "../../main.rkt"
         racket/list
         (only-in racket/cmdline parse-command-line)
         rackunit
         (for-syntax racket/base syntax/parse))

(begin-for-syntax
  (define-syntax-class flag-name
    #:description #f
    (pattern _:string
             #:cut
             #:fail-unless
             (regexp-match? #rx"^([-+][^-+]$|(--|[+][+])[^-+])" (syntax-e this-syntax))
             "bad flag string"
             #:fail-when
             (regexp-match? #rx"^[-+][0-9]$" (syntax-e this-syntax))
             "number flag not allowed"
             #:fail-when
             (regexp-match? #rx"^(-h|--help)$" (syntax-e this-syntax))
             "pre-defined flag not allowed"))

  (define-syntax-class flag-names
    #:description #f
    #:attributes [names]
    (pattern s:flag-name
             #:attr names #'(s))
    (pattern (s:flag-name ...+)
             #:attr names #'(s ...)))

  (define-syntax-class string-stx
    (pattern _:string)))

(define-hosted-syntaxes
  (extension-class option-macro)
  (extension-class flag-macro)

  (nonterminal option
    #:allow-extension option-macro
    (choice/required f:flag ...+)
    (choice/default default:expr f:flag ...+)
    (multi init:expr f:flag ...+))

  (nonterminal flag
    (flag-begin f:flag ...+)
    [names:flag-names [name:id parser:expr] desc:string-stx e:expr]))

;; TODO lots, see ee-lib version
