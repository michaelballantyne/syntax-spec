#lang racket/base

(provide (all-from-out "private/syntax/interface.rkt")
         with-reference-compilers
         (for-syntax
          number
          expr
          id

          in-space
          ~space-literal

          compiled-ids
          compile-reference
          compile-binder!
          compile-binders!

          mutable-reference-compiler
          immutable-reference-compiler

          syntax-datum?
          identifier-with-binding?
          define-symbol-table
          symbol-table-set!
          symbol-table-ref))

(require "private/syntax/interface.rkt"
         "private/runtime/compile.rkt"
         (for-syntax syntax/parse
                     ee-lib
                     ee-lib/persistent-id-table
                     ee-lib/private/binding
                     "private/runtime/syntax-classes.rkt"))
