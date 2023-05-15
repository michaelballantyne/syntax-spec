#lang racket/base

; ensures that you can use colon as the name of a form

(require "../testing.rkt")

(syntax-spec
 (nonterminal bt
   (: l:bt r:bt)
   leaf)

 (host-interface/expression
  (make-bt e:bt)
  (syntax-parse #'e
    [((~literal :) l r)
     #'(list (make-bt l) (make-bt r))]
    [(~literal leaf)
     #''()])))

(check-equal? (make-bt (: leaf (: leaf leaf)))
              '(() (() ())))
