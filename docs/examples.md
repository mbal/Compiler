# Examples of X
Probably none of this examples will work right now (or ever): they're
just an idea of what I want for X.
The most basic program is:

    function fact(n) =
        if n == 0 then 1 else n * fact(n-1);

This currently works.

    function fact(n) =
        n == 0 -> 1 | n * fact(n-1); //(X)

While this is nice, I don't think it's a real improvement, it's simply
a rewrite of the `if` in the first example.

    function fact(n) =
        (<1' ? <mul, <fact . dec>>>@(n > 0))(n); // (X)

This is longer and a lot less readable than the previous version. It
has some nice concepts, such as the conditional or function
composition, but they don't add anything: the angles notation, needed
to avoid ambiguities, clutters the definition.

    function fact(n) =
        ([1, (mul fact . dec)] @ (n > 0))(n);

Is nicer, but terribly ambiguous: is `mul fact . dec` equivalent to
`mul (fact . dec)`? This second form is a lot like a function
application, and even this second interpretation could be perfectly
reasonable.

The next idea is therefore to use a different notation to call
functions. The choices considered are the `|` (from the mathematical
notation used with integrals) or `:`.

    function fact(n) =
        ([1, (mul fact . dec)] @ (n > 0)): n

    fact:4 // == 24

    function fact(n) =
        ([1, (mul fact . dec)] @ (n > 0))| n

    fact|4 // == 24

The pipe doesn't look very good.
In this case, what should we do when there are two or more arguments?
`foo:1,2` is the simplest answer, but, consider `[0, foo:1,2, 3]`. So,
we add parenthesis, too: `foo:(1,2)`, similar to Backus' FP.

While this is a slight modification to the grammar, I'm not too
convinced, because it needs more characters. This could seem a
nitpick, but it's not: basically, if the arguments for the function
are enclosed in parenthesis, then `:` is noise.




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

To solve the ambiguity problem, we employed the concepts of special
forms: in X, a special form is a expression enclosed in `<` and `>`.
Special forms are a way to define a new function, composing existing
functions. 
* `<f, g>(x)` which means f(x, g(x))
* `<f . g . h ... >` represents the composition
* `<f ? g ? h ...>` is a conditional, which executes the function corresponding to the indexing expression supplied.

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
