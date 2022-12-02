#lang racket/base

; This is a regression test.
; Previously, if a dsl was used in a host position of another dsl, bindings from the outer dsl use would be
; unavailable in host positions of the inner dsl use since the reference compilers would be lost.

(require "../testing.rkt")

; blue language
(define-hosted-syntaxes
  (binding-class blue-var)
  (nonterminal blue-expr
               ((~literal let) ([x:blue-var e:expr] ...) b:expr)
               #:binding [(host e) {(bind x) (host b)}]))

(define-host-interface/expression
  (blue e:blue-expr)
  #'(with-reference-compilers ([blue-var immutable-reference-compiler])
      e))

; red language
(define-hosted-syntaxes
  (binding-class red-var)
  (nonterminal red-expr
               ((~literal let) ([x:red-var e:expr] ...) b:expr)
               #:binding [(host e) {(bind x) (host b)}]))

(define-host-interface/expression
  (red e:red-expr)
  #'(with-reference-compilers ([red-var immutable-reference-compiler])
      e))

; sanity check
(check-equal? (red (let ([x 2]) x)) 2)
(check-equal? (blue (let ([x 2]) x)) 2)

; nested usage
; The old behavior is the following error:
; x: red-var may not be used as a racket expression
(check-equal? (red (let ([x 2])
                     (blue (let ([y 3])
                             (list x y)))))
              '(2 3))
