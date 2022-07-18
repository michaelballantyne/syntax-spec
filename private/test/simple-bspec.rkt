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
  
  (define (mylang-expand-expr stx)
    (syntax-parse stx
      #:literal-sets (mylang-lits)
      [n:number
       #'n]
      [v:id
       (expand-function-return
        (list (ref 'v #f mylang-binding? "unbound mylang var reference"))
        (hash
         'v #'v)
        (lambda (env)
          (hash-ref env 'v)))]
      [(mylang-let ([v e]) b)
       (expand-function-return
        (list
         (group
          (list
           (subexp 'e mylang-expand-expr)
           (scope
            (group
             (list
              (bind 'v #f mylang-binding)
              (subexp 'b mylang-expand-expr)))))))
        (hash
         'v #'v
         'e #'e
         'b #'b)
        (lambda (env)
          #`(mylang-let ([#,(hash-ref env 'v)
                          #,(hash-ref env 'e)])
                        #,(hash-ref env 'b))))])))

(define-syntax (mylang stx)
  (syntax-parse stx
    [(_ e)
     #`#'#,(simple-expand-single-exp mylang-expand-expr #'e)]))

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
