## Python's code object

The python code object is defined as follows:

    PyCode {
            argcount :: Word32
            , nlocals :: Word32
            , stackSize :: Word32
            , flags :: Word32
            , code :: PyString 
            , consts :: PyTuple
            , varnames :: PyTuple
            , names :: PyTuple
            }

where:
* `argcount`: count of formal parameters of the function (0 if the
  code object refers to the top-level definition)

* `nlocals`: number of local variables used in the definition of the
  function (basically number of formal parameters + internal
  declarations)

* `stacksize`: the minimum size of the stack needed to perform the
  computation. Note that this value is the source of most problems in
  the compiler, since a value too low will result in a probable
  segfault.

* `flags`
* `code`: the bytecode represented as binary string.
* `consts`: the constants used in the code. It always contains `None`.
* `varnames`: the names of all the variables, both formal parameters
  of the function and variables declared.

* `names`: a tuple of the variables imported from the outer scope

For example:

    def foo(a, b, c):
        x = a + b - c
        y = x + z
        return y

produces the following code object:

* `argcount = 3` since there are 3 formal parameters, `a, b` and `c`.
* `nlocals = 5`, 3 formal parameters and 2 variables defined (`x` and `y`).
* `stackSize` will be 2.
* `code` will contain all the bytecode
* `consts = (None, )`
* `varnames = (a, b, c, x, y)`
* `names = (z, )`

Note that `z` is in the `names` tuple even if it's not defined.
        
