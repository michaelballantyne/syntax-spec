#lang racket/base

(require
  (for-syntax
   racket/base
   syntax/parse
   ee-lib
   "../binding-spec/spec.rkt"
   "../binding-spec/expand.rkt"
   "../syntax/private/rebind-pattern-vars.rkt")
  
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
          'v (pattern-var-value v)))
        'v)]
      [(mylang-let ([v e] ...) b)
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
           'v (pattern-var-value v)
           'e (pattern-var-value e)
           'b (pattern-var-value b))))
       
       (rebind-pattern-vars ([(v e b) (values (hash-ref res 'v) (hash-ref res 'e) (hash-ref res 'b))])
                            #`(mylang-let ([v e] ...)
                                          b))])))

(define-syntax (mylang stx)
  (syntax-parse stx
    [(_ e)
     #`#'#,(mylang-expand-expr #'e)]))

(mylang (mylang-let ([x 5] [y 6])
                    x))
