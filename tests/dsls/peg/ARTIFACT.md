We created this DSL from scratch. The components are as follows:

| Component | Location |
| --------- | -------- |
| Runtime | `private/runtime.rkt` |
| Environment representations for nonterminals and macros | `private/env-rep.rkt` |
| Core form literals definitions | `private/forms.rkt` |
| Expander | `private/expand.rkt` |
| Left-recursion static check | `private/leftrec-check.rkt` |
| Compiler | `private/compile.rkt` |
| Optimization for text alternatives | `private/compile-alt-str.rkt` |
| Interface macros | `core.rkt` |
| Syntactic sugar | `main.rkt` |

