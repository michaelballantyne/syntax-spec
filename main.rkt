#lang racket/base

(provide (all-from-out "private/syntax/interface.rkt")
         (for-syntax
          number
          expr
          id

          in-space
          ~space-literal

          resume-host-expansion
          
          compiled-ids
          compile-reference
          compile-binder!
          compile-binders!

          mutable-reference-compiler
          immutable-reference-compiler

          define-persistent-free-id-table
          persistent-free-id-table?
          syntax-datum?
          identifier-with-binding?
          persistent-free-id-table-has-key?
          persistent-free-id-table-set!
          persistent-free-id-table-ref))

(require "private/syntax/interface.rkt"
         "private/runtime/compile.rkt"
         (for-syntax syntax/parse
                     ee-lib
                     ee-lib/persistent-id-table
                     ee-lib/private/binding
                     "private/runtime/syntax-classes.rkt"))
