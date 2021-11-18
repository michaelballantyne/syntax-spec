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

  (define (mylang-expand-binding-group stx nest-st)
    (syntax-parse stx
      [[v:id e]
       (define bspec
         (group (list (subexp 'e mylang-expand-expr)
                      (scope
                       (group
                        (list
                         (bind 'v mylang-binding)
                         (nested)))))))
       (define-values (res nest-st^)
         (simple-expand
          bspec
          (hash
           'v (pattern-var-value v)
           'e (pattern-var-value e))
          nest-st))
       (values #`[#,(hash-ref res 'v) #,(hash-ref res 'e)] nest-st^)]))
  
  (define (mylang-expand-expr stx)
    (syntax-parse stx
      #:literal-sets (mylang-lits)
      [n:number
       #'n]
      [v:id
       (define-values (res _)
         (simple-expand
          (ref 'v mylang-binding? "unbound mylang var reference")
          (hash
           'v #'v)
          #f))
       (hash-ref
        res
        'v)]
      [(mylang-let* (b ...) e)
       ; #:binding (fold b e)
       (define bspec
         (nest 'b mylang-expand-binding-group
               (subexp 'e mylang-expand-expr)))

       (define-values (res _)
         (simple-expand
          bspec
          (hash
           'b (pattern-var-value b)
           'e (pattern-var-value e))
          #f))
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