# syntax-spec

A metalanguage for creating sophisticated DSLs in Racket. You provide a grammar and binding rules, and the metalanguage provides a front-end that checks binding, expands macros to your DSL core langauge, and provides tools for working with binding in your DSL's compiler.

`syntax-spec` is used in the [Qi](https://github.com/drym-org/qi), [hosted-minikanren](https://github.com/michaelballantyne/hosted-minikanren), and [ocular-patdown](https://docs.racket-lang.org/ocular-patdown/Pattern-based_Updating.html) projects.
This repository also includes a number of example DSL implementations: [peg](tests/dsls/peg), [cmdline](tests/dsls/cmdline), [miniKanren](tests/dsls/minikanren-rs2e), [miniclass](tests/dsls/miniclass), and [TinyHDL](tests/dsls/tiny-hdl). There is also a small [state machine DSL](demos/visser-symposium).

If you'd like to learn more about syntax-spec, you can check out our [ICFP 2024 paper](https://mballantyne.net/publications/icfp2024.pdf) and associated [15-minute talk](https://youtu.be/F70QZaMoYJQ?t=10756), or my longer [demo](demos/mk-workshop-2024) at the 2024 miniKanren workshop.


## Installing the release

A release is available on the Racket package server as [`syntax-spec-v2`](https://pkgs.racket-lang.org/package/syntax-spec-v2). 

This is still a prototype: future releases will likely contain breaking changes, and the documentation is incomplete. However, breaking changes will be released under an updated package name.

To use the released package, install via:

```
raco pkg install syntax-spec-v2
```

and import as

```
(require syntax-spec-v2)
```

Its documentation is available on [the Racket documentation site](https://docs.racket-lang.org/syntax-spec-v2).


## Installing the development version

To use the latest, unstable version, check out the Git repository, change directory into it, and run:


```
raco pkg install
```

Then import as

```
(require syntax-spec-dev)
```

Once installed, you can access the documentation via:

```
raco docs syntax-spec-dev
```

Note that the package name when installed this way is based on the directory name, so if you checked out this repository as the directory `syntax-spec`, you would use

```
raco pkg remove syntax-spec
```

to uninstall the package.


