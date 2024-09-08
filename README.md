# syntax-spec

A metalanguage for creating sophisticated DSLs in Racket. You provide a grammar and binding rules, and the metalanguage provides a front-end that checks binding, expands macros to your DSL core langauge, and provides tools for working with binding in your DSL's compiler.

There are a few relatively-complete example DSL implementations: [miniKanren](tests/dsls/minikanren-rs2e), [miniclass](tests/dsls/miniclass), and [TinyHDL](tests/dsls/tiny-hdl). There is also a small [state machine DSL](demos/visser-symposium).

If you'd like to learn more about syntax-spec, you can check out our [ICFP 2024 paper](https://mballantyne.net/publications/icfp2024.pdf) and associated [15-minute talk](https://youtu.be/F70QZaMoYJQ?t=10756), or my longer [demo](demos/mk-workshop-2024) at the 2024 miniKanren workshop.

A release is available on the Racket package server as [`syntax-spec-v1`](https://pkgs.racket-lang.org/package/syntax-spec-v1). 

This is still a prototype: future releases will likely contain breaking changes, and the documentation is incomplete. However, breaking changes will be released under an updated package name.

To use the released package, install via:

```
raco pkg install syntax-spec-v1
```

and import as

```
(require syntax-spec-v1)
```


To use the latest, unstable version, check out the Git repository, change directory into it, and run:


```
raco pkg install
```

Then import as

```
(require syntax-spec)
```

Once installed, you can access the documentation via:

```
raco docs syntax-spec
```



