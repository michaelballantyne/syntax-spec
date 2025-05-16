#lang racket/base

(require "../testing.rkt"
         ;; for runtime syntax-interpreter testing, we want this at phase 0
         "../private/runtime/binding-operations.rkt")

(syntax-spec
 (binding-class var)
 (extension-class macro #:binding-space dsl)
 (nonterminal expr
   #:binding-space dsl
   #:allow-extension macro
   n:number
   v:var
   (+ e1:expr e2:expr)
   (host e:racket-expr)

   (lambda (x:var) e:expr)
   #:binding (scope (bind x) e)
   (e1:expr e2:expr)

   (letrec ([x:var e:expr] ...) body:expr)
   #:binding (scope (bind x) ... e ... (scope body)))

 (host-interface/definition
  (define-var v:var)
  #:binding (export v)
  #:lhs [#'v]
  #:rhs [#'#f]))

(syntax-spec
 (host-interface/expression
  (expr/free-vars-as-symbols e:expr)
  #`(list #,@(map (lambda (id) #`'#,id) (free-identifiers #'e))))

 (host-interface/expression
  (expr/binding-vars-as-symbols e:expr)
  #`(list #,@(map (lambda (id) #`'#,id) (binding-identifiers #'e)))))


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

(check-equal?
 (expr/binding-vars-as-symbols
  (lambda (w)
    (+
     (+ x
        (lambda (y)
          (+ y z)))
     z)))
 '(w y))

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

(check-false
 (expr/alpha-equivalent?
  (lambda (x) (lambda (x) x))
  (lambda (x) (lambda (y) x))))

;; alpha equivalence should respect hygiene
(define-dsl-syntax m macro
  (syntax-parser
    [(m y:id) #'(lambda (x) y)]))
;; fails bc the binding equivalence uses a free id table even though there are no bindings on expanded syntax,
;; so it ends up not being hygienic
(check-false
 (expr/alpha-equivalent?
   ;; (lambda (x1) (lambda (x2) x1)) because macro introduction scope
   (lambda (x) (m x))
   ;; (lambda (x1) (lambda (x2) x2))
   (lambda (x) (lambda (x) x))))

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

(syntax-spec
  (host-interface/expression
    (subst-expr e:expr target:expr replacement:expr)
    #`'#,(subst #'e #'target #'replacement)))

;; substitute whole expression
(check-equal? (subst-expr (lambda (x) x) (lambda (y) y) 1)
              1)
;; substitute sub-expression
(check-equal? (subst-expr (letrec ([x (lambda (x) x)]) x)
                          (lambda (y) y)
                          1)
              '(letrec ([x 1]) x))
;; substitute sub-expression multiple times
(check-equal? (subst-expr (letrec ([x (lambda (x) x)]) (lambda (z) z))
                          (lambda (y) y)
                          1)
              '(letrec ([x 1]) 1))

(check-equal? (subst-expr (lambda (x) (+ 1 1)) (+ 1 1) (+ 2 2))
               '(lambda (x) (+ 2 2)))

;; substitution of a free variable
(syntax-spec
  (host-interface/expression (beta/subst app:expr)
    (syntax-parse #'app
      [(((~datum lambda) (x) body) arg)
       #`'#,(subst #'body #'x #'arg)])))

(check-equal?
 (beta/subst ((lambda (x) (+ x x))
              1))
 '(+ 1 1))

(check-equal?
 (beta/subst ((lambda (x) (lambda (y) (+ x y)))
              1))
 '(lambda (y) (+ 1 y)))

;;; tests using binding operations at runtime, with a syntax-interpreter style

(syntax-spec
  (binding-class rt-var #:binding-space rt)
  (extension-class rt-macro #:binding-space rt)
  (nonterminal rt-expr
    #:allow-extension rt-macro
    #:binding-space rt
    x:rt-var
    (lambda (x:rt-var) e:rt-expr)
    #:binding (scope (bind x) e)
    (f:rt-expr x:rt-expr))
  (host-interface/expression
    (expand-rt e:rt-expr)
    #'#'e))

(define-dsl-syntax let rt-macro
  (syntax-rules ()
    [(let ([x rhs]) body)
     ((lambda (x) body)
      rhs)]))

(check-true (alpha-equivalent? (expand-rt (lambda (x) x))
                               (expand-rt (lambda (x) x))))
(check-true (alpha-equivalent? (expand-rt (lambda (x) x))
                               (expand-rt (lambda (y) y))))
(check-true (alpha-equivalent? (expand-rt (lambda (x) (lambda (y) x)))
                                (expand-rt (lambda (x) (lambda (y) x)))))
(check-false (alpha-equivalent? (expand-rt (lambda (x) (lambda (y) x)))
                                (expand-rt (lambda (x) (lambda (y) y)))))
(check-true (alpha-equivalent? (expand-rt (let ([x (lambda (y) y)]) (x x)))
                                (expand-rt (let ([x (lambda (x) x)]) (x x)))))

(check-equal? (syntax->datum (subst (expand-rt (lambda (x) x))
                                    (expand-rt (lambda (x) x))
                                    (expand-rt (lambda (y) y))))
              '(lambda (y) y))
(check-equal? (syntax->datum (subst (expand-rt (lambda (_) (lambda (x) x)))
                                    (expand-rt (lambda (x) x))
                                    (expand-rt (lambda (y) y))))
              '(lambda (_) (lambda (y) y)))
(check-true (alpha-equivalent? (subst (expand-rt (lambda (x) x))
                                      (expand-rt (lambda (x) x))
                                      (expand-rt (lambda (y) y)))
                               (expand-rt (lambda (y) y))))
(check-true (alpha-equivalent? (subst (expand-rt (lambda (_) (lambda (x) x)))
                                      (expand-rt (lambda (x) x))
                                      (expand-rt (lambda (y) y)))
                               (expand-rt (lambda (_) (lambda (y) y)))))

