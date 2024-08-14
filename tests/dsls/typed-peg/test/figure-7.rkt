#lang racket
#|
(require "../main.rkt")

(use-literal-token-interpretation string-token)

(struct binop-ast [lhs op rhs] #:transparent)

(define (left-associate-binops e1 op* e*)
  (foldl (lambda (op e base) (binop-ast base op e))
         e1 op* e*))

(define-peg term (predicate-token number?))

(define-peg arith-expr
  (=> (seq (: e1 term) (* (seq (: op* (alt "+" "-")) (: e* term))))
      (left-associate-binops e1 op* e*)))

;; (parse arith-expr '(1 "+" 2 "-" 3))
;; evaluates to:
;; (binop-ast (binop-ast 1 "+" 2) "-" 3)

(module+ test
  (require rackunit)

  (check-equal?
    (parse-result-value (parse arith-expr '(1 "+" 2 "-" 3)))
    (binop-ast (binop-ast 1 "+" 2) "-" 3)))
|#
