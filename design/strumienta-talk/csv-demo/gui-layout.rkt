#lang racket/base

(provide gui-layout)

(require racket/class
         (for-syntax racket/base syntax/parse racket/syntax syntax/parse/class/paren-shape))

(define-syntax gui-layout
  (syntax-parser
    [(_ parent-name:id
        (class:id (~optional (~seq #:as element-name:id)
                             #:defaults ([element-name (generate-temporary 'element)]))
                  [~brackets arg-name:id arg-expr:expr] ...
                  child ...)
        ...)
     #'(begin
         (define element-name
           (new class
                [parent parent-name]
                [arg-name arg-expr] ...))
         ...
         (begin
           (gui-layout element-name child ...)
           ...))]))