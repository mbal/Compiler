X is the programming language that cares about your sanity. Well, for
a start it doesn't offer any feature that can cause hair loss, such as
monads. To be sure, it doesn't even offer anything beside basic
arithmetic!

X borrows features from Haskell, Python and J, but:
* J calls for a mixture of interpreting and parsing, because the
  meaning of some sentence is fully defined only at runtime.
* Haskell is very near to the *almost perfect* language, but there are
  a few points I don't like. In particular, Haskell has a lot of
  libraries, but these libraries tend to offer a specific DSL: this
  often means that, to learn how to use a library, you have to learn a
  new language. This language is almost always well integrated in the
  Haskell language, but, for a newcomer to Haskell, this can be rather
  confusing. 
* Python gets a lot right, in my opinion. 

# Notes
* Currently the code is not really organized.
* Suspected Py3k incompatibilities are marked with the tag `333`,
  explaining the reason.
* We currently don't do any optimizations to the code.

## Python bytecode
X compiles to Python bytecode, because it's a simple format, quite
simpler than the JVM's. The main idea behind it is the same: they're
both stack based, meaning that all the operands are pushed on a stack,
in order to operate on them. However, the choice to compile X to the
CPython's bytecode shows the main limitation for this approach: the
bytecode is thought for the Python language. The same is true for
Scala, Clojure, and so on.
In particular:
* the `let` instruction in X should introduce a lexical binding, but
  the only way to do it in Python is to apply the `let` to `def`
  translation (as shown in SICP). 

    LOAD 4
    LOAD 2
    +-------------+
    |      4      |
    +-------------+
    |      2      |
    +-------------+
    |      1      |  ----> 1 was on the stack before
    +-------------+

    BINARY_ADD
    +-------------+
    |      6      |
    +-------------+
    |      1      | ----> 1 remains on the stack
    +-------------+

All the constants are saved in a special area of the bytecode file,
and they're referred to by the index in this area. This means that all
the instructions occupy either 1 byte (if they have no operands) or 3
bytes (1 byte for the instruction, 2 bytes for the index of the
constant).
The names of the variables are saved in another area, and even these
are referred to by their index.

## Known bugs
* Some code makes Python segfault. I don't have tracked the
  motivation, yet. It's probably the stack size, that is wrong.
  `print(4+4/3*3)` doesn't work (when the stack size is set to 2). It's
  a bit strange, though, since it crashes after all the computations,
  while it **should** crash when pushing the first 3 on the stack...
  Yes, it's the stack size. After reading the CPython source code I
  have a rough idea about how to compute it.
