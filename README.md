# blackMAMBO
A Tiger compiler, for a **X64 target-machine**, implemented in Standard ML, a "strict, statically typed functional programming language with modular structure".

# About Tiger
Tiger is a simple, statically-typed, programming language defined in Andrew W. Appel's book *Modern Compiler Implementation in ML*.
You can find a fine specification in [Semantic Scholar](https://goo.gl/icf7Np).

# Platform
Linux

# Compiler Phases
- [x] **Lex**. Break the source file into individual words, or tokens.
- [x] **Parse**. Analyze the phrase structure of the program.
- [x] **Semantic Actions**. Builda a piece of abstract syntax tree corresponding to each phrase.
- [x] **Frame Layout**. Place variables, function-parameters, etc, into activation records (stack frames) in a machine-dependent way.
- [x] **Translate**. Produce intermediate representation trees (IR trees), a notation that is not tied to any particular source language or target-machine architecture.
- [x] **Canonicalize**. Hoist side effects out of expressions, and clean up conditional branches, for the convenience of the next phases.
- [x] **Instruction Selection**. Group the IR-tree nodes into clumps that correspond to the actions of target-machine instructions.
- [x] **Control Flow Analysis**. Analyze the sequence of instructions into a control flow graph that shows all the possible flows of control the program might follow when it executes.
- [x] **Liveness Analysis**. Gather info about the flow of info through variables of the the program: calculates the places where each program variable holds a still-needed value (is live).
- [x] **Register Allocation**. Choose a register to hold each of the variables and temporary values used by the program; variables not live at the same time can share the same register.
- [x] **Code Emission**. Replace the temporary names in each machine instruction with machine registers.

# Usage
First of all:

1. Clone repo.
2. Download and install [Moscow ML](http://mosml.org/). Moscow ML is a "light-weight implementation of Standard ML (SML), a strict functional language used in teaching and research".
3. Open `Makefile` from blackMAMBO, and update `HOME` path with the path where you installed Moscow ML.
4. Open a terminal, change directory to blackMAMBO folder, do the following: ```make clean && make depend && make```

Now, let's say you want to compile a Tiger program `program.tig`. The simplest way to carry out this operation is by doing the following: ```./tiger program.tig```.

There exist several options available to display information when compiling a Tiger program using blackMAMBO: ```./tiger -option program.tig```, where `option` can be

* `ir`: displays IR-tree.
* `canon`: displays canon code.
* `liveout`: displays live-out info in an instruction-graph way.
* `assembly`: displays program assembly.

# Bibliography
*Modern Compiler Implementation in ML*, Andrew W. Appel.

# More Info
* *Compilers* final class project. Computer Science. National University of Rosario.
* Some code comments are written in Spanish, although the majority of them are written in English. Sorry about that.

> Yes! I did it! :metal:

*Project presented on December 6, 2017. National University of Rosario*