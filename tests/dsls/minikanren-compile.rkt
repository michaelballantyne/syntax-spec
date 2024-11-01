#lang racket

(require "../../testing.rkt"
         "minikanren.rkt"
         (for-syntax syntax/id-table))

(begin-for-syntax
  (define compile-goal
    (syntax-parser
      #:literals (== fresh)
      [(fresh1 (v ...) b)
       #`(let ([v (gensym)] ...)
           #,(compile-goal #'b))]
      [(== t1 t2)
       #`(list #,(compile-term #'t1) #,(compile-term #'t2))]))
  
  (define compile-term
    (syntax-parser
      #:literals (rkt)
      [n:number
       #'n]
      [(rkt e)
       #'e])))

(syntax-spec
  (host-interface/expression
    (run n:expr (qvar:term-variable ...)
      g:goal ...)
    #:binding (scope (bind qvar) ... g ...)

    #'(void))


  (host-interface/expression
    (mk-compile g:goal)

    (compile-goal #'g)))


(mk-compile
 (fresh (x)
   (== 1 (rkt x))))

(run 3 (q)
  (fresh (x)
    (== q x)))


