#lang racket/base

(require "../testing.rkt"
         (for-syntax racket/base (except-in ee-lib racket-var) syntax/parse))

(syntax-spec
  (binding-class dsl-name)
  (nonterminal dsl-expr       
    n:dsl-name
    #:binding [n]
    
    (dsl-let n:dsl-name e:dsl-expr)
    #:binding {(bind n) e}

    ;; Introduce binding; need to ensure it does not capture references in e...
    (~> ((~datum a) e)
        #'(dsl-let x e))

    ;; ... particularly references introduced by another ~>.
    (~> ((~datum b))
        #'(dsl-let x (a x))))

  (host-interface/expression
    (check-test e:dsl-expr)
    (syntax-case #'e (dsl-let)
      [(dsl-let b1 (dsl-let b2 r))
       #`(list #,(same-binding? (compiled-from #'b1) (compiled-from #'r))
               #,(same-binding? (compiled-from #'b2) (compiled-from #'r)))])))

(check-equal?
 (check-test (b))
 (list #t #f))

