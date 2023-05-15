#lang racket/base

(provide (all-from-out "private/syntax/interface.rkt")
         with-reference-compilers
         (for-syntax
          number
          id

          mutable-reference-compiler
          immutable-reference-compiler

          define-persistent-symbol-table
          define-local-symbol-table
          syntax-datum?
          symbol-table-set!
          symbol-table-ref

          free-identifiers))

(require "private/syntax/interface.rkt"
         "private/runtime/compile.rkt"
         (for-syntax syntax/parse
                     (except-in ee-lib racket-var)
                     ee-lib/persistent-id-table
                     ee-lib/private/binding
                     "private/runtime/binding-operations.rkt"
                     "private/runtime/syntax-classes.rkt"))
