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

where ' is a suffix that transforms constants in function that return
the constant. 1' === function 1'() = 1
However, this would need a lot of changes in the grammar (and in the semantics).

    function fact(n as int) as int =
        if n == 0 then 1 else n * fact(n-1)

