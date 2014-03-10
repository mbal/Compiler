# Compiler

## Organization
The compiler itself is quite simple:

* Main
* Parser: creates an AST from the program source
* Definitions: parses the AST, collecting the definitions and
  propagates some basic info.
* Compiler: compiles the AST.
* Emitter: emits the bytecode.

### Definitions

This module does a pass over the AST to collect informations about
definitions of functions and assignments of variables to functions,
such as `let foo = bar;` where `bar` is a function (either defined
before or after).
This allows to perform basic inference on function arguments: we can
signal if a function is used with the wrong number of arguments, or is
not defined.

### Compiler
Transforms the AST into a high level representation of the assembly of
the Python's virtual machine.

### Emitter
