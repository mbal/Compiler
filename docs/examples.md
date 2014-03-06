## Examples of X

Probably none of this example will work right now: they're just an
idea of what I want for X.

    function fact(n) =
        if n == 0 then 1 else n * fact(n-1)

Using the agenda operator and a hook:

    function fact(n) = ([1', (* fact . dec)] @ (n > 0))(n)
    
    function dec(k) = k - 1

Or, equivalently, using the hook (f g)(x) = f(k, g(k))

    function dec(k) = (- 1')(k)

where ' is a suffix that transforms constants in (anonymous) functions that return
the constant. 1' === function 1'() = 1 

    function fact(n as int) as int =
        if n == 0 then 1 else n * fact(n-1)

The ' operator is a generalization of what is found in J, which provides the constants
functions _9:, _8:, ... 0:, 1:, ... 9: that return the number (from -9 to 9)
