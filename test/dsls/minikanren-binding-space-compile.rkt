#lang racket

(require "../../testing.rkt"
         "minikanren-binding-space.rkt"
         (for-syntax syntax/id-table))

(begin-for-syntax
  (define compiled-var (make-free-id-table))
  
  (define compile-goal
    (syntax-parser
      [((~space-literal fresh1 mk) (v ...) b)
       #`(let ([v (gensym)] ...)
           #,(compile-goal #'b))]
      [((~space-literal == mk) t1 t2)
       #`(displayln (list #,(compile-term #'t1) #,(compile-term #'t2)))]))
  
  (define compile-term
    (syntax-parser
      [n:number
       #'n]
      [((~space-literal rkt mk) e)
       #'(with-reference-compilers ([term-variable immutable-reference-compiler])
         e)])))

(syntax-spec
  (host-interface/expression
    (run n:expr (qvar:term-variable ...)
      g:goal ...)
    #:binding {(bind qvar) g}
  
    (displayln #'(g ...))
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


