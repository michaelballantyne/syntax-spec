#lang racket

(require "../../testing.rkt"
         "minikanren.rkt"
         (for-syntax syntax/id-table))

(begin-for-syntax
  (define compiled-var (make-free-id-table))
  
  (define compile-goal
    (syntax-parser
      #:literals (== fresh)
      [(fresh1 (v ...) b)
       #:with (v-c ...) (for/list ([v (attribute v)])
                          (compile-binder! compiled-var v))
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
       (resume-host-expansion #'e)])))

(define-host-interface/expression
  (mk-compile g:goal)

  (with-binding-compilers ([term-variable
                            (lambda (id) (compile-reference compiled-var id))])
    (displayln #'g)
    (compile-goal #'g)))

#;(define-syntax mk-compile
    (syntax-parser
      [(_ e)
       (define expanded ((nonterminal-expander goal) #'e))
     
       (with-binding-compilers ([term-variable
                                 (lambda (id) (compile-reference compiled-var id))])
         (compile-goal expanded))]))

(let ()
  (mk-compile
   (fresh (x)
          (== 1 (rkt x)))))

(mk-compile
 (fresh (x)
        (== 1 (rkt x))))
