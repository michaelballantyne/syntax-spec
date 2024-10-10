#lang racket/base

; testing the combining of multiple imports into a single import group

(require "../testing.rkt"
         syntax/macro-testing)

(syntax-spec
  (nonterminal/exporting defn
    ((~literal define) x:racket-var e:racket-expr)
    #:binding [(export x)])

  (host-interface/expression
    (double-local ([d1:defn ...] [d2:defn ...]) body:racket-expr)
    #:binding (scope (import d1) ... (import d2) ... body)
    #'(compile-expr ([d1 ...] [d2 ...]) body))

  (host-interface/expression
    (many-local ([d:defn ...] ...) body:racket-expr)
    ; this group is unnecessary, but we want to test the behavior of ellipsized groups with imports
    #:binding (scope [[[[(import d)]] ...] ...] body)
    #'(compile-expr ([d ...] ...) body)))

(define-syntax compile-expr
  (syntax-parser
    #:literals (define)
    [(_ ([(define x e1) ...] ...) body)
     #'(let ()
         (define x e1)
         ...
         ...
         body)]))

(check-equal?
 (double-local ([(define odd? (lambda (n) (if (zero? n) #f (even? (sub1 n)))))]
                [(define even? (lambda (n) (or (zero? n) (odd? (sub1 n)))))])
               (odd? 3))
 #t)

; another test

(syntax-spec
  (nonterminal/exporting def
    #:allow-extension racket-macro
    (mylet (d:def ...) (d2:def ...))
    #:binding (scope (import d) ... (import d2) ...)
    (mylet2 (d:def ...) (d2:def ...))
    #:binding (scope [(import d) (import d2)] ...)

    (mydef x:racket-var e:racket-expr)
    #:binding (export x)

    (mydefsyntax x:racket-macro e:expr)
    #:binding (export-syntax x e)

    (myexpr e:racket-expr))
  (host-interface/expression
    (mylang d:def)
    #:binding (scope (import d))
    #''d))

(test-case "these expand"
  (mylang
   (mylet
    [(myexpr 1)
     (mydefsyntax mydef2 (syntax-rules () [(_ x e) (mydef x e)]))]
    [(mydef2 x 5)
     (myexpr 2)]))

  (mylang
   (mylet2
    [(myexpr 2)
     (mydef2 x 5)]
    [(mydefsyntax mydef2 (syntax-rules () [(_ x e) (mydef x e)]))
     (myexpr 1)]))

  (void))

(test-case "these don't expand"
  (check-exn
   #rx"expected def"
   (lambda ()
     (convert-compile-time-error
      (mylang
       (mylet2
        [(myexpr 1)
         (mydefsyntax mydef2 (syntax-rules () [(_ x e) (mydef x e)]))]
        [(mydef2 x 5)
         (myexpr 2)])))))

  (check-exn
   #rx"expected def"
   (lambda ()
     (convert-compile-time-error
      (mylang
       (mylet
        [(myexpr 2)
         (mydef2 x 5)]
        [(mydefsyntax mydef2 (syntax-rules () [(_ x e) (mydef x e)]))
         (myexpr 1)])))))

  (void))
