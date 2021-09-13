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

  (define (mylang-expand-binding-group stx k)
    (syntax-parse stx
      [[v:id e]
       (define bspec
         (group (list (subexp 'e mylang-expand-expr)
                      (scope
                       (group
                        (list
                         (bind 'v mylang-binding)
                         (continue k)))))))
       (define res
         (simple-expand
          bspec
          (hash
           'v (pattern-var-value v)
           'e (pattern-var-value e))))
       #`[#,(hash-ref res 'v) #,(hash-ref res 'e)]]))
  
  (define (mylang-expand-expr stx)
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
      [(mylang-let* (b ...) e)
       ; #:binding (fold b e)
       (define bspec
         (seq-fold 'b mylang-expand-binding-group
                   (subexp 'e mylang-expand-expr)))

       (define res
         (simple-expand
          bspec
          (hash
           'b (pattern-var-value b)
           'e (pattern-var-value e)
           )))
       #`(mylang-let* (#,@(hash-ref res 'b))
                      #,(hash-ref res 'e))])))

(define-syntax (mylang stx)
  (syntax-parse stx
    [(_ e)
     #`#'#,(mylang-expand-expr #'e)]))

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