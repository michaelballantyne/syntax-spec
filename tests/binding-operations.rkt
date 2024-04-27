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
   #:binding (scope (bind x) e)

   (letrec ([x:var e:expr] ...) body:expr)
   #:binding (scope (bind x) e (scope body)))

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
 #rx"free-identifiers: can't enter a #%host-expression"
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

(syntax-spec
 (host-interface/expression
  (expr/alpha-equivalent? a:expr b:expr)
  #`#,(alpha-equivalent? #'a #'b #:allow-host? #f)))

(check-true
 (expr/alpha-equivalent?
  1
  1))

(check-false
 (expr/alpha-equivalent?
  1
  2))

(check-true
 (expr/alpha-equivalent?
  x
  x))

(check-false
 (expr/alpha-equivalent?
  x
  y))

(check-true
 (expr/alpha-equivalent?
  (+ x y)
  (+ x y)))

(check-false
 (expr/alpha-equivalent?
  (+ x y)
  (+ x z)))

(check-true
 (expr/alpha-equivalent?
  (lambda (a) (+ x a))
  (lambda (b) (+ x b))))

(check-false
 (expr/alpha-equivalent?
  (lambda (a) x)
  (lambda (x) x)))

(check-true
 (expr/alpha-equivalent?
  (letrec ([f g]
           [g f])
    f)
  (letrec ([g f]
           [f g])
    g)))

(check-exn
 ; for some reason, including "alpha-equivalent?: " causes the test to fail
 #rx"can't enter a #%host-expression"
 (lambda ()
   (convert-compile-time-error
    (expr/alpha-equivalent?
     (+ x (host PI))
     (+ x (host PI))))))

(syntax-spec
 (host-interface/expression
  (expr/alpha-equivalent?/ignore-host a:expr b:expr)
  #`#,(alpha-equivalent? #'a #'b #:allow-host? #t)))

(check-false
 (expr/alpha-equivalent?/ignore-host
  (+ x (host PI))
  (+ x (host PI))))
