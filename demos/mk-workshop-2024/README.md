# `syntax-spec` and `hosted-minikanren` demo at the 2024 miniKanren workshop

This folder contains the [slides](mk-workshop-2024.pdf) and code examples from my talk "Hosted miniKanren: reconciling optimizing compilation and extensibility" ([video][https://www.youtube.com/watch?t=18443&v=rLHUYRWgIGg]) at the 2024 miniKanren workshop.

The talk steps through the process of building a miniKanren implementation with syntax-spec, and shows off the way that our [hosted-miniKanren](https://github.com/michaelballantyne/hosted-minikanren) implementation benefits from the combination of macro-extensibility, optimizing compilation, and host-language interaction that syntax-spec enables.

The files are named according to their order in the demo. Those from `12-example-matche.rkt` rely on having [hosted-miniKanren](https://github.com/michaelballantyne/hosted-minikanren) installed, as well.