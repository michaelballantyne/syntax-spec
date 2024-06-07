#lang racket/base

(provide syntax-datum?)

(require racket/prefab)

(define (syntax-datum? v)
  (or (null? v)
      (symbol? v)
      (boolean? v)
      (number? v)
      (and (pair? v) (syntax-datum? (car v)) (syntax-datum? (cdr v)))
      (and (vector? v) (for/and ([el v]) (syntax-datum? el)))
      (and (box? v) (syntax-datum? (unbox v)))
      (and (hash? v) (for/and ([(k v) v]) (and (syntax-datum? k) (syntax-datum? v))))
      (and (immutable-prefab-struct-key v) (for/and ([el (in-vector (struct->vector v) 1)])
                                             (syntax-datum? el)))))