# language-webassembly

[![Build Status](https://travis-ci.org/TerrorJack/language-webassembly.svg)](https://travis-ci.org/TerrorJack/language-webassembly) [![Build status](https://ci.appveyor.com/api/projects/status/github/TerrorJack/language-webassembly?svg=true)](https://ci.appveyor.com/project/TerrorJack/language-webassembly)

A Haskell port of the WebAssembly [spec](https://github.com/WebAssembly/spec). Work in progress.

## Notable differences from the OCaml implementation

* Name convention: when several different OCaml modules share a common identifier, a prefix is added to that name for disambiguation. e.g. `IntAdd`/`FloatAdd`, `IntBinOp`/`FloatBinOp`. Also, `underscore_case`s are turned to `camelCase`s.
* Annotating the AST: the OCaml implementation uses a `phrase` datatype to bind source code position to every AST node. Here, I use an `Annotated` datatype for similar purpose, but it's a polymorphic binder and can annotate the AST nodes with any other type. For more on this topic, check out [this blog post](http://blog.ezyang.com/2013/05/the-ast-typing-problem/). I'm choosing the low-tech approach (compared to fixpoint functors, etc) here, since it already works well.
* Error handling: the OCaml implementation uses exceptions heavily; here I use `MonadError` with a single error type whenever possible.
* `Vector`s replace lists.

## Todo

* Phase 1: port the WebAssembly AST/kernel AST
    * Modeling in Haskell
    * Implement checker/desugarer/interpreter
    * Implement tests
    * Implement S-expression parser
* Phase 2: implement missing utilities
    * Binary encoder/decoder
    * Pretty-printer
    * REPL-mode of interpreter
* Phase 3: in case I really get here
    * More test suites, benchmark, documentation, etc
    * Call 3rd-party WebAssembly implementations (SpiderMonkey, V8, etc) for cross-testing
    * Upload to Hackage/Stackage Nightly
