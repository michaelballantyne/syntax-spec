# syntax-spec

A metalanguage for creating sophisticated DSLs in Racket. You provide a grammar and binding rules, and the metalanguage provides a front-end that checks binding, expands macros to your DSL core langauge, and provides tools for working with binding in your DSL's compiler.

This is still a prototype: it is not yet stable, and the documentation is incomplete.

There are a few relatively-complete example DSL implementations: [miniKanren](tests/dsls/minikanren-rs2e), [miniclass](tests/dsls/miniclass), and [TinyHDL](tests/dsls/tiny-hdl). There is also a small [state machine DSL](demos/visser-symposium).

The package is not yet available on the Racket package server. You can install it by checking out the Git repository, changing directory into it, and running:

```
raco pkg install
```

Once installed, you can access the documentation via:

```
raco docs syntax-spec
```



