#lang racket/base

(require ee-lib/define
         
         (for-syntax racket/base
                     syntax/parse
                     ee-lib
                     "../syntax/simple-syntax.rkt"))

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
       (simple-spec-expand
        stx
        v
        (~ v mylang-binding?))]
      [(mylang-let ([v:id e]) b)
       (simple-spec-expand
        stx
        (_ ([v e]) b)
        [(: e mylang-expand-expr)
         {(! v mylang-binding)
          (: b mylang-expand-expr)}])])))

(define-syntax (mylang stx)
  (syntax-parse stx
    [(_ e)
     #`#'#,(mylang-expand-expr #'e)]))

(mylang (mylang-let ([x 5])
                    x))