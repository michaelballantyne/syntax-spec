#lang racket

(require "class.rkt")

(require drracket/check-syntax
         (for-syntax syntax/parse)
         rackunit
         syntax/macro-testing
         racket/match)

(begin-for-syntax (define v (box 0)))

(test-case "simple class"
           (define foo%
             (class
                 (field x)
               (define (add y) (+ x y))))
           (define foo (new foo% 1))
           (check-equal? (send foo add 2) 3))
#;(
(test-case "empty class"
           (define foo% (class))
           (new foo%)
           (void))
(test-case "internal method call"
           (define foo%
             (class
                 (define (f x)
                   (send this g x))
               (define (g x)
                 (add1 x))))
           (define foo (new foo%))
           (check-equal? (send foo f 1) 2))
(test-case "mutually recursive methods"
           (define parity%
             (class
                 (define (even? n)
                   (if (= n 0)
                       #t
                       (send this odd? (sub1 n))))
               (define (odd? n)
                 (if (= n 0)
                     #f
                     (send this even? (sub1 n))))))
           (define parity (new parity%))
           (check-equal? (send parity even? 10) #t)
           (check-equal? (send parity even? 11) #f))
(test-case "mutating a field"
           (define counter%
             (class
                 (field count)
               (define (inc) (set! count (add1 count)))
               (define (get) count)))
           (define counter (new counter% 0))
           (send counter inc)
           (send counter inc)
           (send counter inc)
           (check-equal? (send counter get) 3))
(test-case "mutating a field in the constructor"
           (define counter%
             (class
                 (field count)
               (set! count (add1 count))
               (define (get) count)))
           (check-equal? (send (new counter% 0) get) 1))
(test-case "use a macro that expands to a definition"
           (define-syntax-rule (m x) (define x (lambda () 2)))
           (define foo%
             (class
                 (m x)))
           (define foo (new foo%))
           (check-equal? (send foo x) 2))
(test-case "use a macro that expands to field"
           (define-syntax-rule (m x) (field x))
           (define foo%
             (class
                 (m x)
               (define (f) (add1 x))))
           (define foo (new foo% 1))
           (check-equal? (send foo f) 2))
(test-case "define and use a macro inside of a class"
           (define foo%
             (class
                 (define-syntax-rule (m x) (field x))
               (define-syntax-rule (my-add1 x) (add1 x))
               (m x)
               (define (f) (my-add1 x))))
           (define foo (new foo% 1))
           (check-equal? (send foo f) 2))
(test-case "defines in a begin"
           (define foo%
             (class
                 (begin
                   (define-syntax-rule (m x) (field x))
                   (define-syntax-rule (my-add1 x) (add1 x))
                   (m x)
                   (define (f) (my-add1 x)))))
           (define foo (new foo% 1))
           (check-equal? (send foo f) 2))
(test-case "defines in a nested begin"
           (define foo%
             (class
                 (begin
                   (begin
                     (define-syntax-rule (m x) (field x))
                     (define-syntax-rule (my-add1 x) (add1 x))
                     (m x)
                     (define (f) (my-add1 x))))))
           (define foo (new foo% 1))
           (check-equal? (send foo f) 2))
(test-case "shadow method name"
           (define foo%
             (let* ([f 42]
                    [foo% (class (define (f) 2))])
               foo%))
           (define foo (new foo%))
           (let ([f 42])
             (check-equal? (send foo f) 2)))
(test-case "macro expands to a reference to a locally defined value"
           (define foo%
             (class
                 (define-syntax-rule (call-g) (g))
               (define (g) 2)
               (define (f) (call-g))))
           (define foo (new foo%))
           (check-equal? (send foo f) 2))
(test-case "internal-definition-context-add-scopes regression test"
           (define-syntax-rule (x) 'good)
           (define-syntax-rule (m f) (define (f) (x)))
           (define c%
             (class
                 (define-syntax-rule (x) 'bad2)
               (m f)))
           (check-equal? (send (new c%) f) 'good))
(test-case "duplicate method name error"
           (check-exn #rx"defined"
                      (lambda ()
                        (convert-syntax-error
                         (class
                             (define (f) 2)
                           (define (f) 2))))))
(test-case "duplicate method name error, macro introduced"
           (check-exn #rx"defined"
                      (lambda ()
                        (convert-syntax-error
                         (let ()
                           (define-syntax-rule (m) (define (f) 2))
                           (class
                               (m)
                             (define (f) 2)))))))
(test-case "method used as procedure"
           (define foo%
             (class(define-syntax-rule
  (test-case desc body ...)
  (their-test-case desc (convert-compile-time-error (let () body ...))))
                 (define (f) 2)
               (define (g) (f))))
           (define foo (new foo%))
           (check-equal? (send foo g) 2))
(test-case "method used as procedure with args"
           (define foo%
             (class
                 (define (f x y) (+ x y))
               (define (g) (f 1 2))))
           (define foo (new foo%))
           (check-equal? (send foo g) 3))
(test-case "method used as reference"
           (define foo%
             (class
                 (define (f) 2)
               (define (g) f)))
           (define foo (new foo%))
           (check-pred procedure? (send foo g)))
(test-case "method used in apply"
           (define foo%
             (class
                 (define (f x y) (+ x y))
               (define (g) (apply f (list 1 2)))))
           (define foo (new foo%))
           (check-equal? (send foo g) 3))
(test-case "class-level expr"
           (define v 'bad)
           (define foo%
             (class
                 (set! v 'good)))
           (new foo%)
           (check-equal? v 'good))
(test-case "multiple class-level exprs"
           (define v 0)
           (define foo%
             (class
                 (set! v (add1 v))
               (set! v (add1 v))))
           (new foo%)
           (check-equal? v 2))
(test-case "multiple class-level exprs"
           (define v 0)
           (define foo%
             (class
                 (set! v (add1 v))
               (set! v (add1 v))))
           (new foo%)
           (check-equal? v 2))
(test-case "class-level field usage"
           (define foo%
             (class
                 (field v)
               (set! v 'good)
               (define (get-v) v)))
           (define foo (new foo% #f))
           (check-equal? (send foo get-v) 'good))
(test-case "class-level method usage"
           (define v 'bad)
           (define foo%
             (class
                 (define (m) (set! v 'good))
               (m)))
           (new foo%)
           (check-equal? v 'good))
(test-case "method shadows macro"
           (define v #f)
           (define-syntax-rule (m e ...) (set! v 'bad))
           (define foo%
             (class
                 (define (m) (set! v 'good))
               (m)))
           (new foo%)
           (check-equal? v 'good))
(test-case "method shadows macro after use"
           ; I was surprised, but racket class works this way too!
           ; The expansion depends on the order
           ; Makes sense since the expr is actually expanded before the method is bound
           (define v #f)
           (define-syntax-rule (m e ...) (set! v 'good))
           (define foo%
             (class
                 (m)
               (define (m) (set! v 'bad))))
           (new foo%)
           (check-equal? v 'good))
(test-case "local macro use before definition"
           (define v 'bad)
           (new (class
                    (m)
                  (define-syntax-rule (m) (set! v 'good))))
           (check-equal? v 'good))
(test-case "class-level 'this'"
           (define v 'bad)
           (define foo%
             (class
                 (define (f) (set! v 'good))
               (send this f)))
           (new foo%)
           (check-equal? v 'good))
(test-case "class-level 'this' alone"
           (define v 'bad)
           (define foo%
             (class
                 (define (f) (set! v 'good))
               this))
           (new foo%)
           (void))
(test-case "local macro emits this"
           (define v #f)
           (define foo
             (new (class
                      (define-syntax-rule (m) this)
                    (set! v (m)))))
           (check-equal? foo v))
(test-case "transformers only get evaluated once"
           (class
               (define-syntax m
                 (let ()
                   (set-box! v (add1 (unbox v)))
                   (lambda (stx) #'42)))
             (m))
           (check-equal? (phase1-eval (unbox v)) 1))
(test-case "define multiple syntaxes"
           (define v #f)
           (new
            (class
                (define-syntaxes (double one) (values (syntax-rules () [(double e) (list e e)])
                                                      (syntax-rules () [(one) 1])))
              (set! v (double (one)))))
           (check-equal? v '(1 1)))
;; TODO this test only works when run without already having built with raco make. Disable for now.
#;(define-namespace-anchor a)
#;(test-case "disappeared props"
           (define (num-arrows-of check-syntax-result)
             (length (for/list ([vec check-syntax-result] #:when (equal? (vector-ref vec 0)
                                                                         'syncheck:add-arrow/name-dup/pxpy))
                       vec)))
           (define ns (namespace-anchor->namespace a))
           (check-equal? (num-arrows-of
                          (show-content (quote-syntax (class
                                                          (define-syntax x #'a)
                                                        (define-syntax my-fields
                                                          (syntax-parser
                                                            [(_ name:id)
                                                             (define/syntax-parse field-name (syntax-local-value #'name))
                                                             (syntax-property #'(field field-name) 'disappeared-use (list (syntax-local-introduce #'name)))]))
                                                        (my-fields x)))
                                        #:namespace ns))
                         2))
(test-case "define in method"
  (define v
    (new
     (class
       (define (f)
         (define x 2)
         x))))
  (check-equal?
   (send v f)
   2))
(test-case "match-define"
  (define v
    (new
     (class
       (define/match (fact n)
         [(0) 1]
         [(n) (* n (fact (sub1 n)))]))))
  (check-equal? (send v fact 4)
                24))
#;
(test-case "non-method definition"
  (define v
    (new
     (class
      (define x 2)
      (define (f) x))))
  (check-equal? (send v f) 2))

)