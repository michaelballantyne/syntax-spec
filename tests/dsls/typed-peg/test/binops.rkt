#lang racket/base

#|
(require "../main.rkt"
         (for-syntax racket/base syntax/parse))

(use-literal-token-interpretation string-token)

(struct binop-ast [lhs op rhs] #:transparent)
(struct prefix-ast [op rhs] #:transparent)

(define (left-associate-binops e1 op* e*)
  (foldl (lambda (op e base) (binop-ast base op e))
         e1 op* e*))

(define-syntax binops
  (peg-macro
   (lambda (stx)
     (syntax-parse stx
       [(_ op-e subexpr-e)
        #'(=> (seq (: e1 subexpr-e) (* (seq (: op* op-e) (: e* subexpr-e))))
              (left-associate-binops e1 op* e*))]))))

(define-syntax prefix
  (peg-macro
   (lambda (stx)
     (syntax-parse stx
       [(_ op-e subexpr-e)
        #'(alt
            (=> (seq (: op op-e) (: arg subexpr-e))
                (prefix-ast op arg))
            subexpr-e)]))))

(define-peg expr (alt "True" "False" (predicate-token number?)))

(define-peg comp-op
  (alt "<" ">" "==" ">=" "<=" "!=" "in" (seq "not" "in") (seq "is" "not") "is"))

(define-peg or-test
  (binops "or"
    (binops "and"
      (prefix "not"
        (binops comp-op
          expr)))))

(module+ test
  (require rackunit)

  (check-equal?
    (parse-result-value (parse expr '(1)))
    1)

  (check-equal?
    (parse-result-value (parse or-test '(1 "and" 1 "<" 2)))
    (binop-ast 1 "and" (binop-ast 1 "<" 2)))

  (check-equal?
    (parse-result-value (parse or-test '(1 "<" 2 "and" 2)))
    (binop-ast (binop-ast 1 "<" 2) "and" 2))

  (check-equal?
    (parse-result-value (parse or-test '("True" "==" "False" "and" 1 "<" 2)))
    (binop-ast (binop-ast "True" "==" "False") "and" (binop-ast 1 "<" 2)))

  (check-equal?
    (parse-result-value (parse or-test '("not" "True" "==" "False" "or" 1 "<" 2)))
    (binop-ast (prefix-ast "not" (binop-ast "True" "==" "False")) "or" (binop-ast 1 "<" 2)))
)

|#
