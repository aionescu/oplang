# opc

The compiler for `OpLang`, a stack-based esoteric programming language inspired by [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck).

## OpLang Basics

In short, OpLang (**Op**erator **Lang**uage) is an extended version of Brainfuck that supports defining custom operators, which communicate through a stack.

The memory model of OpLang is similar to that of Brainfuck: There is a "memory tape", consisting of an array of cells, and a "cursor" that keeps track of a cell (the "current cell"), initially set to the first cell in the tape.
The language's primitive instructions, called "intrinsics", modify either the cursor, or the current cell.

In OpLang, each operator (similar to functions or procedures in other languages) has its own tape, that is reset before each invocation of that operator.
The only way for operators to communicate (e.g. pass arguments or return values) is through the `stack`, a special "global" tape which does not get reset, and which all operators have access to.

## Syntax

OpLang has 10 "intrinsic" operators (8 of which are the Brainfuck operators, with the same semantics):

* `+`: Increments the value at the current cell
* `-`: Decrements the value at the current cell
* `<`: Move the current cell to the left
* `>`: Move the current cell to the right
* `;`: Pop a value from the stack and move it into the current cell
* `:`: Push the value of the current cell onto the stack
* `,`: Read a character from stdin and store its ASCII value in the current cell
* `.`: Interpret the current cell as an ASCII character and print it to stdin
* `[`: Begin a loop (i.e. if the value at the current cell is zero, jump to the next `]`)
* `]`: End a loop (i.e. if the value at the current cell is non-zero, jump to the previous `[`)

An OpLang program consists of a (potentially empty) series of `operator definitions`, followed by a series of operator calls, called the `toplevel` (similar to a `main()` function in other languages).

An operator definition consists of an operator name, which is any character that is not a reserved operator, whitespace, or any of `{}`, followed by a series of operator calls delimited between `{` and `}`.

OpLang supports single-line comments, introduced by `#`.

Here's an example of a simple OpLang program:

```op
a { ; +++ : }
++++ : a a a ; .
```

The above program defines a custom operator `a` that pops a value from the stack, adds 3 to it, then pushes it back onto the stack.
The program's toplevel (i.e. `main()` function) initializes the first cell to 0, pushes it to the stack, then calls `a` 3 times, then pops the value from the stack and prints it.

For more sample programs, see the [Samples](Samples/) folder.

## Compiler Internals

The compiler works by translating `OpLang` code into C, then calling the system's C compiler (`cc`).
The compiler also performs a series of optimizations on the code. For more details about optimizations, see [this](https://github.com/Oldpug/Bfi#ast-and-optimizations).

## Building from source

In order to build the compiler, simply run `stack build` in the repo's root directory. You will need the `Stack` build tool, which you can download [here](https://www.haskell.org/platform/).

## License

This repository is licensed under the terms of the MIT License.
For more details, see [the license file](LICENSE.txt).