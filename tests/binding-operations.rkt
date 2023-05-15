#lang racket/base

(require "../testing.rkt")

(syntax-spec
 (binding-class var)
 (nonterminal expr
   n:number
   v:var
   (+ e1:expr e2:expr)
   (lambda (x:var) e:expr)
   #:binding {(bind x) e})

 (host-interface/definition
  (define-var v:var)
  #:binding (export v)
  #:lhs [#'v]
  #:rhs [#'#f]))

(syntax-spec
 (host-interface/expression
  (free-vars-as-symbols e:expr)

  #`(list #,@(map (lambda (id) #`'#,id) (get-free-vars #'e)))))

(define-var x)
(define-var y)
(define-var z)

(check-equal?
 (free-vars-as-symbols
  (+
   (+ x
      (lambda (y)
        (+ y z)))
   z))
 '(x z))