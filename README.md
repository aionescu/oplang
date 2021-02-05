# oplang

[![Hackage](https://img.shields.io/hackage/v/oplang?style=for-the-badge)](https://hackage.haskell.org/package/oplang)

OpLang is a stack-based esoteric programming language based on [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck).

## Installing

The compiler is available on [Hackage](https://hackage.haskell.org/package/oplang), and can be installed via [`cabal`](https://www.haskell.org/cabal/).

```sh
cabal install oplang
```

## Usage

```sh
oplang code.op
./code.out
```

For a list of available command-line arguments, use `oplang --help`.

## Building from source

To build the compiler from source, you will need the [`cabal`](https://www.haskell.org/cabal/) build tool.

```sh
cabal new-build
cabal new-run . -- <args>
```

## Language Description

OpLang is a strict superset of Brainfuck that lets you define custom procedures called "operators", which use a stack for passing arguments and returning values.

The memory model of OpLang is similar to that of Brainfuck: There is a "memory tape", consisting of an array of 1-byte cells, and a pointer that keeps track of the current cell.

The big difference from Brainfuck is that each custom operator invocation gets its own tape. In a way, tapes are like the stack frames of functions in languages like C.

The only way for operators to communicate (e.g. pass arguments or "return" things) is through the `stack`, a global resource available to all operators.

## Syntax

OpLang has 10 "intrinsic" operators (8 of which are the Brainfuck operators, with the same semantics):

* `+`: Increment the current cell
* `-`: Decrement the current cell
* `<`: Move the current cell pointer to the left
* `>`: Move the current cell pointer to the right
* `;`: Pop a value from the stack and store it into the current cell
* `:`: Push the value of the current cell onto the stack
* `,`: Read a character from stdin and store its ASCII value in the current cell
* `.`: Interpret the current cell as an ASCII character and print it to stdout
* `[`: Begin a loop (i.e. if the value at the current cell is zero, jump to the next `]`)
* `]`: End a loop (i.e. if the value at the current cell is non-zero, jump to the previous `[`)

An OpLang program consists of a series of custom operator definitions, followed by the actual program, here called the "toplevel" (similar to a `main()` function in other languages).

An operator definition consists of the new operator's name, followed by the operator's body, enclosed in `{`/`}`.

Single-line comments are supported, and are introduced by the `#` character.

Here's a simple program that reads a character from stdin, adds 3 to it, and prints it back:

```op
a { ; +++ : }
,: a ;.
```

For more example programs, see the [Examples](Examples/) folder.

## License

This repository is licensed under the terms of the GNU General Public License v3.
For more details, see [the license file](LICENSE.txt).
