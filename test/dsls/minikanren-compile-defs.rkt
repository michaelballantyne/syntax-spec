#lang racket

(provide run (all-from-out "minikanren.rkt") mk-defs define-relation define-relation2)

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
       (resume-host-expansion
        #'e
        #:reference-compilers ([term-variable compile-reference]))])))

(define-host-interface/expression
  (run n:expr (qvar:term-variable ...)
    g:goal ...)
  #:binding {(bind qvar) g}
  
  (displayln #'(g ...))
  #'(void))




(define-host-interface/expression
  (mk-compile g:goal)

  (compile-goal #'g))

(define-host-interface/definitions
  (define-relation (name:relation-name arg:term-variable ...) body:goal)
  #:binding [(export name) {(bind arg) body}]
  #'(define tmp 5))

(define-hosted-syntaxes
  (two-pass-nonterminal mk-def
    (define-relation2 (name:relation-name arg:term-variable ...) body:goal)
    #:binding [(export name) {(bind arg) body}]))

(define-host-interface/definitions
  (mk-defs d:mk-def ...)
  #:binding (re-export d)
  #'(define tmp 5))


(define-relation (appendo2 l1 l2 l3)
  (== l1 '()))

(run 1 (q) (appendo2 '() '() '()))

(#%expression (run 1 (q) (oddo 5) (eveno 4)))

(mk-defs
 (define-relation2 (eveno n)
   (oddo n))
 (define-relation2 (oddo n)
   (eveno n)))



(mk-compile
 (fresh (x)
   (== 1 (rkt x))))

(run 3 (q)
  (fresh (x)
    (== q x)))


