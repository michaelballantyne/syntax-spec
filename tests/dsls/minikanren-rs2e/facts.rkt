#lang racket/base

(provide define-facts-table assert-fact query-facts)

(require "mk.rkt" db sql
         (for-syntax racket/base syntax/parse))

(struct facts-table [conn insert query])

(define (connect-and-create create-statement)
  (define conn (sqlite3-connect #:database 'memory))
  (query-exec conn create-statement)
  conn)

(define-syntax-rule
  (define-facts-table name [field ...])
  (define name
    (let ([conn (connect-and-create
                 (create-table #:temporary name #:columns [field text] ...))])
      (facts-table
       conn
       (insert #:into name #:set [field ?] ...)
       (select field ... #:from name)))))

(define (assert-fact ft . args)
  (define c (facts-table-conn ft))
  (define insert-stmt (facts-table-insert ft))
  (apply query-exec c insert-stmt args))

(define (do-query ft args)
  (define c (facts-table-conn ft))
  (define query (facts-table-query ft))
  (query-rows c query))

(define (unify-query-results query-res args)
  (if (null? query-res)
      (goal-expression fail)
      (goal-expression
       (conde
         [(== (rkt-term (vector->list (car query-res))) (rkt-term args))]
         [(project () (unify-query-results (cdr query-res) args))]))))

(define (query-facts-rt ft . args)
  (define query-res (do-query ft args))
  (unify-query-results query-res args))

(define-syntax query-facts
  (goal-macro
   (syntax-parser
     [(_ ft args ...)
      #'(project (args ...)
          (query-facts-rt ft args ...))])))

(module+ test
  (define-facts-table flights [flightfrom flightto])
  (assert-fact flights "bos" "slc"))
