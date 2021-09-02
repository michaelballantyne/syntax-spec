#lang racket/base

(require
  (for-syntax
   racket/base
   syntax/parse
   ee-lib
   "../runtime/binding-spec.rkt")
  
  ee-lib/define)

(define-literal-forms mylang-lits
  "mylang forms may only be used in mylang"
  (mylang-let))

(begin-for-syntax
  (struct mylang-binding ())
  
  (define/hygienic (mylang-expand-expr stx) #:expression
    (syntax-parse stx
      #:literal-sets (mylang-lits)
      [n:number
       #'n]
      [v:id
       (hash-ref
        (simple-expand
         (ref 'v mylang-binding? "unbound mylang var reference")
         (hash
          'v #'v))
        'v)]
      [(mylang-let ([v e]) b)
       (define res
         (simple-expand
          (group
           (list
            (subexp 'e mylang-expand-expr)
            (scope
             (group
              (list
               (bind 'v mylang-binding)
               (subexp 'b mylang-expand-expr))))))
          (hash
           'v #'v
           'e #'e
           'b #'b)))
       #`(mylang-let ([#,(hash-ref res 'v)
                       #,(hash-ref res 'e)])
                     #,(hash-ref res 'b))])))

(define-syntax (mylang stx)
  (syntax-parse stx
    [(_ e)
     #`#'#,(mylang-expand-expr #'e)]))

(require rackunit syntax/macro-testing)

(check-equal?
 (syntax->datum
  (mylang (mylang-let ([x 5]) x)))
 '(mylang-let ((x 5)) x))

(check-exn
 #rx"y: unbound mylang var reference"
 (lambda ()
   (convert-compile-time-error
    (mylang (mylang-let ([x 5]) y)))))