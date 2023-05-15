#lang racket

(require "../../testing.rkt"
         "minikanren-binding-space.rkt"
         (for-syntax syntax/id-table))

(begin-for-syntax
  (define compiled-var (make-free-id-table))
  
  (define compile-goal
    (syntax-parser
      #:datum-literals (fresh1 ==)
      [(fresh1 (v ...) b)
       #`(let ([v (gensym)] ...)
           #,(compile-goal #'b))]
      [(== t1 t2)
       #`(list #,(compile-term #'t1) #,(compile-term #'t2))]))
  
  (define compile-term
    (syntax-parser
      #:datum-literals (rkt #%term-ref)
      [n:number
       #'n]
      [(#%term-ref x)
       #'x]
      [(rkt e)
       #'(with-reference-compilers ([term-variable immutable-reference-compiler])
         e)])))

(syntax-spec
  (host-interface/expression
    (run n:expr (qvar:term-variable ...)
      g:goal)
    #:binding {(bind qvar) g}

    #`(let ([qvar (gensym)] ...)
        #,(compile-goal #'g))))

(check-true
 (pair?
  (run 3 (q)
    (fresh (x)
      (== q x)))))


