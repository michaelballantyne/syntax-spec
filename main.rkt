#lang racket/base

(provide (all-from-out "private/syntax/interface.rkt")
         with-reference-compilers
         (for-syntax
          number
          id

          mutable-reference-compiler
          immutable-reference-compiler

          make-variable-like-reference-compiler

          symbol-table?
          mutable-symbol-table?
          define-persistent-symbol-table
          ; deprecated
          define-local-symbol-table
          local-symbol-table

          symbol-table-set!
          symbol-table-ref
          symbol-table-has-key?

          symbol-set?
          mutable-symbol-set?
          define-persistent-symbol-set
          local-symbol-set

          symbol-set-add!
          symbol-set-member?

          immutable-symbol-table?
          immutable-symbol-table

          symbol-table-set
          symbol-table-remove

          immutable-symbol-set?
          immutable-symbol-set

          symbol-set-add
          symbol-set-remove
          symbol-set-union
          symbol-set-intersection
          symbol-set-subtract

          in-symbol-table
          in-symbol-set

          compiled-identifier=?
          free-identifiers
          alpha-equivalent?))

(require "private/syntax/interface.rkt"
         "private/runtime/compile.rkt"
         (for-syntax syntax/parse
                     (except-in "private/ee-lib/main.rkt" racket-var)
                     "private/ee-lib/persistent-id-table.rkt"
                     "private/ee-lib/binding.rkt"
                     "private/runtime/binding-operations.rkt"
                     "private/runtime/syntax-classes.rkt"))
