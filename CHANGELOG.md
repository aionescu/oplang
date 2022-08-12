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
