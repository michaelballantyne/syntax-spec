A version of the PEG DSL from `../peg` ported to Typed Racket by Michael Delmonaco.

Currently it is not usable outside of the `core.rkt` module that defines the syntax because of this limitation of Typed Racket macros: https://github.com/michaelballantyne/syntax-spec/issues/24

Thus, we have not ported the tests from the untyped PEG implementation. If the above issue is fixed, it should be possible.
