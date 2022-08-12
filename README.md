# oplang

[![Hackage](https://img.shields.io/hackage/v/oplang?style=for-the-badge)](https://hackage.haskell.org/package/oplang)

OpLang is a stack-based esoteric programming language based on [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck).

## Installing

The compiler is available on [Hackage](https://hackage.haskell.org/package/oplang), and can be installed via [cabal](https://www.haskell.org/cabal/) (which can itself be installed via [ghcup](https://www.haskell.org/ghcup/)).

```sh
cabal install oplang
```

## Usage

To compile an OpLang file with default options, use:

```sh
oplang code.op
./code
```

For a list of available command-line options, use `oplang --help`.

## Building from source

Prerequisites:

* `GHC` >=9.0
* `cabal` >=3.0

(Both can be installed via [ghcup](https://www.haskell.org/ghcup/))

```sh
cabal build
cabal run . -- <args>
```

## Language Features

OpLang is a strict superset of Brainfuck.

Its main improvement is the addition of user-defined operators, which are analogous to user-defined functions in languages like C or Python.

The "memory tape" in OpLang is specific to each operator invocation (akin to the stack space allocated for functions in C), and there is a separate "stack" which persists across operator invocations.

Each cell in the tape(s) and stack is 1 byte, and overflow/underflow is allowed.

The default size of the stack is 4KB, and the default size for each memory tape is 64KB. These can be modified via the command-line.

## Syntax

OpLang has 10 "intrinsic" operators, 8 of which are the Brainfuck operators, with the same semantics:

* `+`: Increment the current cell
* `-`: Decrement the current cell
* `<`: Move the current cell pointer to the left
* `>`: Move the current cell pointer to the right
* `,`: Read a character from stdin and store its ASCII value in the current cell
* `.`: Interpret the current cell as an ASCII character and print it to stdout
* `[`: Begin a loop (i.e. if the value at the current cell is zero, jump to the next `]`)
* `]`: End a loop (i.e. if the value at the current cell is non-zero, jump to the previous `[`)

And 2 of them are new operators that modify the stack:

* `;`: Pop a value from the stack and store it into the current cell
* `:`: Push the value of the current cell onto the stack

An OpLang program consists of a series of custom operator definitions, followed by the "toplevel" (similar to a `main()` function in other languages).

An operator definition consists of the operator's name, followed by the operator's body, enclosed in `{`/`}`.

Single-line comments are supported, and are introduced by the `#` character.

Here's a simple program that reads a character from stdin, adds 3 to it, and prints it back:

```oplang
a { ; +++ : }
,: a ;.
```

For more example programs, see the [examples](examples/) folder.

## License

This repository is licensed under the terms of the GNU General Public License v3.

For more details, see [the license file](LICENSE.txt).
