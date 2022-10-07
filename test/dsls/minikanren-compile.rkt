#lang racket

(require "../../testing.rkt"
         "minikanren.rkt"
         (for-syntax syntax/id-table))

(begin-for-syntax
  (define compile-goal
    (syntax-parser
      #:literals (== fresh)
      [(fresh1 (v ...) b)
       #:with (v-c ...) (for/list ([v (attribute v)])
                          (compile-binder! v))
       #`(let ([v-c (gensym)] ...)
           #,(compile-goal #'b))]
      [(== t1 t2)
       #`(displayln (list #,(compile-term #'t1) #,(compile-term #'t2)))]))
  
  (define compile-term
    (syntax-parser
      #:literals (rkt)
      [n:number
       #'n]
      [(rkt e)
       #'(with-reference-compilers ([term-variable immutable-reference-compiler])
         e)])))

(define-host-interface/expression
  (run n:expr (qvar:term-variable ...)
    g:goal ...)
  #:binding {(bind qvar) g}

  (displayln #'(g ...))
  #'(void))


(define-host-interface/expression
  (mk-compile g:goal)

  (compile-goal #'g))


(mk-compile
 (fresh (x)
   (== 1 (rkt x))))

(run 3 (q)
  (fresh (x)
    (== q x)))


