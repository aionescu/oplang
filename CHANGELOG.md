<!-- markdownlint-disable first-line-h1 -->

## v0.5.0.0 \[2024-04-28\]

* Use `putchar`/`getchar` instead of `printf`/`scanf` for I/O
* Optimizer improvements
  * Can now optimize more complex loops into constant-time arithmetic
  * Performs better constant propagation

## v0.4.0.1 \[2023-12-28\]

* Improve error messages, warnings, and `--help` text
* Fix warnings not being shown if there were also compilation errors
* Update dependencies to allow compiling with GHC 9.8

## v0.4.0.0 \[2023-05-06\]

* Add command-line options for printing the AST and IR, ignoring warnings, and skipping C compilation
* Require GHC 9.2 or newer

## v0.3.0.1 \[2022-08-12\]

* Update [`base`](https://hackage.haskell.org/package/base) version bound to require GHC 9.0 (`base` 4.15) or newer

## v0.3.0.0 \[2022-08-12\]

* Upgrade to [`text`](https://hackage.haskell.org/package/text) v2 and use [`text-builder-linear`](https://hackage.haskell.org/package/text-builder-linear), improving compilation performance
* Optimizer overhaul
  * Now uses a separate IR
  * Performs more optimizations, runs in a single pass
  * Generates smaller and slightly faster C code

## v0.2.0.0 \[2022-03-19\]

* Parser error message improvements
* Codegen improvements
* **\[Breaking\]** Tailcalls are no longer optimized

## v0.1.0.0 \[2020-10-10\]

* Initial release
