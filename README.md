# ef3r

[![codecov](https://codecov.io/github/Sintrastes/ef3r/graph/badge.svg?token=2A1XMWGOSH)](https://codecov.io/github/Sintrastes/ef3r)

Work in progress.

Ef3r, prononced "effer", as in "effervescent", (will be) a

 - Minimalistic
 - Dynamically / gradually typed
 - Lazily evaluated (or not, IDK at this point)
 - Functional programming language

with:

 - First-class language support for (pull) Functional Reactive Programming.
 - An innovative visual debugger with "expression tracing" capabilities.
 - Universal Function Call Syntax.
 - Extensible records and variants.

# Philosophy

Ef3r seeks to distill the concepts of modern software development into their simplest, most fundamental and orthogonal building blocks. As such, we reject a monolithic all-encompasing programming ontology based on _classes_, which conflate many seperate concerns (nominal typing, construction, encapsulation, state, data bundling) into a single mechanism. Coming from other more mainstream languages, this leads to some interesting design descisions, such as:

 - No distinction between "functions" and "methods" (Universal Function Call Syntax).
 - Lazy evaluation (provides the mechanism for easy encapsulation, by allowing data to implicitly act like a closure).
 - No distinction between functions and properties.
 - All data is inherently extensible. (Data modeling is based off of extensible records and variants)

However, despite many of these concepts having a basis in academic programming language theory, we have no desire to be an academic language. As such, we also aim to focus on usability, and first-class tooling, and take inspiration from the following prior art:

 - [Smalltalk](https://en.wikipedia.org/wiki/Smalltalk)
 - [Whyline](https://www.cs.cmu.edu/~NatProg/whyline.html)
 - [Eve](https://witheve.com/)

We also take inspiration from:

 - [Kotlin](https://kotlinlang.org/) (lambda / "block" syntax, "when" expressions)
 - [Haskell](https://www.haskell.org/) ("lambda case" syntax, binary operator backtick syntax, `where` expressions).
 - [Purescript](https://www.purescript.org/) / [OCaml](https://ocaml.org/) (Extensible records and variants / OCaml object system).
 - [Julia](https://julialang.org/) (multiple dispatch, strong dynamic typing system).
 - [Python](https://www.python.org/) (minimal syntax for variable declarations).
 - [Rust](https://www.rust-lang.org/) (Functions are private-by-default in modules, no need for a specail `private` syntax, `?` for unwrapping effectful values).
 - [Idris](https://www.idris-lang.org/) (Types are just values, no need for a specail syntax for type application v.s. term application.

# Acronym

Experimental Functional Reactive Runtime in Rust
