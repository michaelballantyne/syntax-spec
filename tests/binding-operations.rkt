#lang racket/base

(require "../testing.rkt")

(syntax-spec
 (binding-class var)
 (nonterminal expr
   #:binding-space dsl
   n:number
   v:var
   (+ e1:expr e2:expr)
   (host e:racket-expr)
   
   (lambda (x:var) e:expr)
   #:binding {(bind x) e})

 (host-interface/definition
  (define-var v:var)
  #:binding (export v)
  #:lhs [#'v]
  #:rhs [#'#f]))

(syntax-spec
 (host-interface/expression
  (expr/free-vars-as-symbols e:expr)

  #`(list #,@(map (lambda (id) #`'#,id) (free-identifiers #'e)))))


(define-var x)
(define-var y)
(define-var z)

(check-equal?
 (expr/free-vars-as-symbols
  (+
   (+ x
      (lambda (y)
        (+ y z)))
   z))
 '(x z))

(check-exn
 #rx"free-identifiers: can't compute the free identifiers of a #%host-expression"
 (lambda ()
   (convert-compile-time-error
    (expr/free-vars-as-symbols
     (host x)))))

(syntax-spec
 (host-interface/expression
  (expr/free-vars-as-symbols/ignore-host e:expr)

  #`(list #,@(map (lambda (id) #`'#,id) (free-identifiers #'e #:allow-host? #t)))))

(check-equal?
 (expr/free-vars-as-symbols/ignore-host
  (+
   (+ (host x)
      (lambda (y)
        (+ y z)))
   z))
 '(z))

