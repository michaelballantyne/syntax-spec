#lang racket

(require
  "../core.rkt"
  syntax/srcloc
  (for-syntax syntax/parse))

(define-peg t1
  (=> (seq "a\n" (:src-span src (: r " b\ncd")))
      (list src r)))

(define-syntax stx
  (peg-macro
   (syntax-parser
     [(_ x:id)
      #'(token (lambda (s)
                 (if (eq? 'x (syntax-e s))
                     (values s (build-source-location s))
                     (values #f #f))))])))

(define-peg t2
  (=> (seq (stx x) (:src-span src (seq (: r1 (stx y)) (: r2 (stx z)))))
      (list src r1 r2)))

(module+ test
  (require rackunit)
  
  (check-equal?
   (parse-result-value (parse t1 (make-text "a\n b\ncd" 'foo)))
   (list (srcloc 'foo 2 0 2 5) " b\ncd"))

  (define input (syntax->list #'(x y z)))


  (check-equal?
   (first (parse-result-value (parse t2 input)))
   (build-source-location (second input) (third input)))

  )
