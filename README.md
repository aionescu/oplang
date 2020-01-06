# oplangc

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
* `.`: Interpret the current cell as an ASCII character and print it to stdout
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

The recommended file extension for OpLang programs is `*.op`.

## Compiler Settings

The compiler can be passed additional configuration via command-line arguments. In order to see the available options, run the compiler with the `-h` or `--help` argument.

## Compiler Internals

The compiler works by translating `OpLang` code into C, then calling the system's C compiler (`cc`).
The compiler also performs a series of optimizations on the code.

### Optimizations

The compiler performs a series of optimizations on OpLang source code.
Some of the optimizations are simply Brainfuck optimizations, which carry over to OpLang.

Here's a list of the performed optimizations:

#### Instruction Merging

Multiple instructions of the same kind (e.g. + and -, or < and >) are merged into a single instruction.

```op
+++ --- ++ => Add 2
```

```op
> <<<< => Move -3
```

```op
+++ --- => # Nothing
```

#### Efficient cell zeroing

Cell zeroing is usually achieved by using `[-]`.
The compiler recognizes this pattern, and transforms it into a single assignment.

```op
[-] => Set 0
```

Note that because cells can overflow, `[+]` achieves the same behavior.

```op
[+] => Set 0
```

#### Dead Code Elimination

In some cases (such as consecutive loops), the compiler can statically determine that particular instructions have no effect, so they are removed.

```op
[+>][<-] => [+>] => Loop [Add 1, Move 1] # Second loop is not compiled
```

Also, operators that are defined but never used do not get compiled.
They are still checked for correctness.

```op
n { ... } # Unused
m { ... }           => m { ... }
... m                  ... m
```

#### Strength reduction

In some cases, certain instructions can be replaced with cheaper ones.
For example, adding after a loop can be replaced with a `Set` instruction, as the value of the cell is 0 (since it exited the loop), so the 2 instructions are equivalent.

```op
[-]+++ => Set 0, Add 3 => Set 3
```

Also, setting a value after adding to it (or subtracting from it) will overwrite the result of the additions, so they are removed.

```op
+++[-] => Add 3, Set 0 => Set 0
```

Consecutive `Set`s also behave similarly: Only the last `Set` is compiled, previous ones are elided.

#### Offseted instructions

A common pattern that arises in Brainfuck/OpLang code is the following structure: `[>>>+<<<-]` (the number of `>`s, `<`s and `+`s may vary, and `+` may be replaced by another instruction)

Instead of generating the following code for such structures: `Loop [Move 3, Add 1, Move -3, Add -1]`, the compiler generates the following code: `Loop [AtOffset 3 (Add 1), Add -1]`, thus performing less arithmetic operations.

#### Tail Call Optimization*

While not implementing full tail call optimization (due to using C as a backend, which does not support TCO), the compiler detects functions that are called in tail position and instead of allocating a new tape for them, it passes the caller's tape to the tail-called function (i.e. the caller and the callee share a single tape).

## Building from source

In order to build the compiler, simply run `stack build` in the repo's root directory. You will need the `Stack` build tool, which you can download [here](https://docs.haskellstack.org/en/stable/README/).

Once built, run `stack run -- <args>` to run the compiler. (e.g. `stack run -- -o a.out code.op`)

## License

This repository is licensed under the terms of the MIT License.
For more details, see [the license file](LICENSE.txt).