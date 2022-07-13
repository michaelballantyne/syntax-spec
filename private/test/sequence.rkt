#lang racket/base

(require
  (for-syntax
   racket/base
   syntax/parse
   ee-lib
   "../runtime/binding-spec.rkt"
   "../syntax/compile/pattern-var-reflection.rkt")
  
  ee-lib/define)

(define-literal-forms mylang-lits
  "mylang forms may only be used in mylang"
  (mylang-let*))

(begin-for-syntax
  (struct mylang-binding ())

  (define (mylang-expand-binding-group stx)
    (syntax-parse stx
      [[v:id e]
       (define bspec
         (group (list (subexp 'e mylang-expand-expr)
                      (scope
                       (group
                        (list
                         (bind 'v #f mylang-binding)
                         (nested)))))))
       (expand-function-return
        bspec
        (hash
         'v (attribute v)
         'e (attribute e))
        (lambda (env)
          #`[#,(hash-ref env 'v) #,(hash-ref env 'e)]))]))
  
  (define (mylang-expand-expr stx)
    (syntax-parse stx
      #:literal-sets (mylang-lits)
      [n:number
       #'n]
      [v:id
       (expand-function-return
        (ref 'v #f mylang-binding? "unbound mylang var reference")
        (hash
         'v #'v)
        (lambda (env) (hash-ref env 'v)))]
      [(mylang-let* (b ...) e)
       ; #:binding (fold b e)
       (define bspec
         (nest 'b mylang-expand-binding-group
               (subexp 'e mylang-expand-expr)))

       (expand-function-return
        bspec
        (hash
         'b (attribute b)
         'e (attribute e))
        (lambda (env)
          #`(mylang-let* (#,@(hash-ref env 'b))
                         #,(hash-ref env 'e))))])))

(define-syntax (mylang stx)
  (syntax-parse stx
    [(_ e)
     #`#'#,(simple-expand-single-exp mylang-expand-expr #'e)]))

(require rackunit syntax/macro-testing)

(check-equal?
 (syntax->datum
  (mylang (mylang-let* ([x 5] [y x]) y)))
 '(mylang-let* ((x 5) (y x)) y))

(check-exn
 #rx"y: unbound mylang var reference"
 (lambda ()
   (convert-compile-time-error
    (mylang (mylang-let* ([x 5]) y)))))
