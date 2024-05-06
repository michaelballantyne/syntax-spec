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
       #'(with-reference-compilers ([term-variable immutable-reference-compiler])
           e)])))

(syntax-spec
  (host-interface/expression
    (run n:expr (qvar:term-variable ...)
      g:goal ...)
    #:binding {(bind qvar) g}
  
    #'(void))

  (host-interface/expression
    (mk-compile g:goal)

    (compile-goal #'g))

  (host-interface/definitions
    (define-relation (name:relation-name arg:term-variable ...) body:goal)
    #:binding [(export name) {(bind arg) body}]
    #'(define tmp 5)))


(syntax-spec
 (nonterminal/exporting mk-def
   (define-relation2 (name:relation-name arg:term-variable ...) body:goal)
   #:binding [(export name) {(bind arg) body}])

 (host-interface/definitions
   (mk-defs d:mk-def ...)
   #:binding (re-export d)
   #'(define tmp 5)))


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


