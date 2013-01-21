# The Stage compiler

Stage is an agent language with support for concurrency. The compiler is split
in two: an OCaml compiler that reads Stage source code and generates a custom
bytecode, and a series of code generators that translate the Stage bytecode to
other platforms like CLR, JVM, or (why not?) JavaScript. Currently only the
OCaml bit is implemented.