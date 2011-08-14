
Experimental Scheme-like compiled language 
------------------------------------------

  - Optimized
  - Statically typed
  - Garbage collected
  - Retargettable
  - C code generation
  - Erlang-style message-passing multitasking
  - Generally for embeded programming purposes

Currently under hard development, this is even not an alpha version yet.

The nearest goal is to get compact, optimized as possible closure-converted
C code to run on MSP430 MCU (16 bits, 55Kb ROM, 4 - 16 Kb of RAM)

The main goals to get an easy-to-modify compiler with relatively small codebase
to try different opportinities that currently are not very common because of total
C hegemony. 

Furthermore, I have just tired of writing code in C for embedded devices.

Hopefully compact syntax and static typing will bring a lot of possibilities 
of automatic static code analysis.

This project is mostly inspired by BitC, PICBIT, Staapl, Embedded ML and MinCaml
projects.

If your share similar ideas and wish to contribute, I will very appreciated to any help 
in the following directions:

### Typing ###

The H&M type inference is basically implemented, but so far is not used in the compiler.
Also complex data types are waiting for implementation.

### Injecting Hoopl ###

### Error handling ###

The error handling is almost missed at the moment.

### Macro system ###

There is almost no any macro system or ideas how to implement it at the moment.


ROADMAP
-------

### Beta release ###

  - Static types
  - C code generation
  - HOF, Closures
  - Int, String, List, Tuple, Record, Array data types
  - C code generation with basic optimization: alpha conversion, 
    beta reduction, closure elimination, constant folding, let flattening

### Nearest future ###

  - Using Hoopl for dataflow analysis 
  - Assembly generation

### Far future ###

???


FAQ
---


Q: Why not to use LLVM?
A: It's too complicated in it's backend aspects. It has a huge codebase. It takes a lot of time to get into it.
   But, there is no problems to use this compiler with LLVM as a backend. But this compiler is going to be
   small, simple and easy to hack and use

Q: When it's going to be ready for production?
A: I don't know. Personbally, I'm going to start to use it in production in Sep-2011


