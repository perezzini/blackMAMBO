# blackMAMBO
A Tiger compiler, for a **X64 target-machine**, implemented in Standard ML, a "strict, statically typed functional programming language with modular structure".

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