#lang racket/base

(require "../main.rkt"
         rackunit
         (for-syntax racket/base syntax/parse racket/pretty))

(define-binding-class var "expr language variable")

(define-nonterminals
  [expr
   #:description "simple expr language expression"

   n:number

   v:var

   (+ e1:expr e2:expr)

   (let ([v:var e:expr] ...) b:expr)
   #:binding [e {(! v) b}]

   (let* (b:binding ...) e:expr)
   #:binding (nest b e)]
  [binding
   (nested)
   #:description "binding group"
   [v:var e:expr]
   #:binding [e {(! v) nested}]
   ]
  )


(define-syntax exprlang
  (syntax-parser
    [(_ e) #`'#,((nonterminal-expander expr) #'e)]))

(exprlang
 (let ([x 5]) (let ([x (+ x 1)]) x)))

(exprlang
 (let* ([x 5] [x (+ x 1)]) x))