# Examples of X

## Factorial (so, IF and FUNCALL)
The factorial example was choosen because it's simple, but, at the
same time, it allows to get a feel of the language, since it contains
conditionals, function applications, and some basic operations.

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
        ([1, (mul fact . dec)] @ (n > 0))(n); // (X)

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

    function fact(n) =
        if n == 0 then 1 else hook(mul, compose(fact, dec))(n)

In the next few examples I'll use a lisp like syntax, in order to make
the transformations clearer.

    (function fact [n]
        (if (= n 0)
            1
            (* n (fact (- n 1)))))


    (function fact [n]
        (if (= n 0)
            1
            ((lambda(x) (* x (fact (- x 1)))) n)))

    (function fact [n]
        (if (= n 0)
            1
            ({x: (* x (fact (- x 1)))} x))) // {x: ...} == (lambda (x) ...)
            // is a nice shorthand, but it looks like a set comprehension :(

    function fact(n) =
        if n == 0 then 1 else {x: (x * fact(x-1))}(n);

    (function fact [n]
        (if (= n 0)
            1
            ((<* {x: fact (- x 1)}>) n))) // very unclear: too many symbols

    (function fact [n]
        (if (= n 0)
            1
            ((<*, <fact . dec>>) n))) // a bit better

    function factA(n) =
        if n == 0 then 1 else <*, <fact.dec>>(n); // OR, define a priority ==>

    function factB(n) =
        if n == 0 then 1 else {*, fact . dec}(n);

I kind of like the last and the penultimate. While `factA` is easier
to parse as the priority is specified using `<` and `>`, `factB` makes
clear that the expression `*, fact . dec` is just an anonymous
function (if we choose to use that notation for lambdas).

## Lambdas (anonymous functions)

The `'` (prime) operator is a suffix that transform constants in
(anonymous) functions that return said constant. `1' == lambda: 1`.
It's similar to FL's `~` operator, except that `~` is prefix.

The main problem with this approach is how to allow the call of the
primed function with any number of arguments. The simplest way to
solve it is to use Python's *args, but it will be slower (you have to
compile the arguments and push them on the stack).
Solving the problem at compile time is tricky: there are two situations
in which a @ is used:

* `let k = [a', g]@cond; k(arg1, arg2, ...)`
* `(([a', g])@cond)(arg1, arg2, ...)`

The second case is probably solvable; but the first cannot be solved
easily: it's impossible to know at compile time whether `k` is the
primed constant or the function.

To introduce an anonymous function, we can either use:
* `{arg1, arg2, ... argn: exp}`, however, this makes difficult or a
  bit ugly adding type annotations: `{x:int, y:string -> string: string(x) + y}`
* `{x, y (int string -> string): string(x) + y}` --> acceptable
* `{int x, string y: string string(x) + y}` --> ugly
* `{x^int, y^int: string(x) + y}^string` --> not very nice
* `(int, string -> string){x, y: string(x) + y}` --> has the plus that
  is very easy to make the signature optional.
* `{x, y: string(x) + y} as int string -> string` something like Haskell's.
        function foo(x, y) as int string -> string -> int =
            {y: x}
   This notation is easy and quite nice to extend in case of
   non-anonymous function.
