## Examples of X

Probably none of this example will work right now: they're just an
idea of what I want for X.

    function fact(n) =
        if n == 0 then 1 else n * fact(n-1)

Using the agenda operator and a hook:

    function fact(n) = ([1', (* fact . dec)] @ (n > 0))(n)
    
    function dec(k) = k - 1

Or, equivalently, using the hook (f g)(x) = f(k, g(k)). Unluckily, if
we introduce a hook, we open up some ambiguities in the grammar, for
example:
`(h f . g)` is parsed as `(Hook h (Compose f g))`, but
`(h (f . g))` is parsed as `h(f . g)`.
The problem is, the second interpretation may be correct, sometimes.

An alternative notation I considered is `? f g` (or the infix
`f ? g`, even though the question mark is kind of misleading, it may
seem a conditional, but it's the symbol that most resembles an hook).
Another approach is to change the application notation, using some
FL-like syntax, for example `f:(arg1, arg2, ...)`. This slight
modification would solve the problem, but I am not too convinced.

`'` is a suffix that transforms constants in (anonymous) functions
that return the constant. `1' === function 1'() = 1`. It's similar to
FL's `~` operator, except that `~` is prefix.

The main problem with this approach is how to allow the call of the
primed function with any number of arguments. The simplest way to
solve it is to use Python's *args, but it will be needlessly slow.
Solving the problem at compile time is tricky: there are two situations
in which a @ is used:

* `let k = [a', g]@cond; k(arg1, arg2, ...)`
* `(([a', g])@cond)(arg1, arg2, ...)`

The first case is difficult to treat (impossible, I think), since we
don't know at compile-time the value of the condition (should we push
the operands on the stack or not?)
The second case is simpler because we can translate it as:

    if cond == False
        (a')()
    else
        g(arg1, arg2, ...)

And we know if a function is 0-ary (well, varargs). This
transformation is not trivial, but neither hard. However, we cannot
apply the translation only sometimes, since it would not be consisted.

The ' operator is a generalization of what is found in J, which
provides the constants functions _9:, _8:, ... 0:, 1:, ... 9: that
return the number (from -9 to 9)
