#lang racket/base

(provide (all-from-out "private/syntax/interface.rkt")
         with-reference-compilers
         (for-syntax
          number
          expr
          id

          in-space
          ~space-literal

          mutable-reference-compiler
          immutable-reference-compiler

          define-persistent-symbol-table
          define-local-symbol-table
          syntax-datum?
          symbol-table-set!
          symbol-table-ref))

(require "private/syntax/interface.rkt"
         "private/runtime/compile.rkt"
         (for-syntax syntax/parse
                     (except-in ee-lib racket-var racket-var?)
                     ee-lib/persistent-id-table
                     ee-lib/private/binding
                     "private/runtime/syntax-classes.rkt"))
