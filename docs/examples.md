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

To introduce an anonymous function, we can use: `{args: exp}`. Note
that `:` is mandatory, even if the lambda accept no arguments.
`{: 4}` is the anonymous function that returns 4.
`{x, y: x + y}` is the anonymous function that returns the sum of its
arguments.
Lambda introduce closure over their enclosing function:

    function foo(a, b, c) =
        {x: a * x * x + b * x + c}

`foo` returns a function that evaluates polynomials.

Another concept is the special form. A special form is enclosed in `{`
and `}` (like a lambda), but has a slightly different semantics: it
composes existing functions, and it basically syntactic sugar.

There are two categories of special forms:

* trains
* and composition

### Composition

The composition special form is a special form to denote function
composition; in the general case, it's:
`{f1 . f2 . ... fk}`, the `.` (dot) operator is associative to the
right, so it's equivalent to: `{x: f1(f2(...fk(x)))}`.

### Train
Trains are taken from the J programming language, with some minor
syntactic differences.

A train is a sequence of functions, separated by `,`. There are 3 trains:
* the hook. A hook is in the form: `{f, g}` (two functions), and it's
  equivalent to: `{x: f(x, g(x))}`. `{mul dec}(k)` returns `k * (k -1)`

* the fork. The fork is in the form `{f, g, h}` (three functions), and
  it's equivalent to: `{x: g(f(x), h(x))}`. For example,
  `{sum div len}([1,2,3])` is the average of the array (2). For two
  arguments (dyad) function, it would be nice to have
  `{f, g, h} == {x, y: g(f(x, y), h(x, y))}`. This can be enforced at
  compile time, probably.

* the generic train `{f1, f2, ... fk}`. In general, longer trains are
  decomposed in hooks and forks.
  If there is an even number of functions,
  `{f1, f2, ... fk}` is the hook between `f1` and `{f2, ... fk}`
  If there is an odd number:
  `{f1, f2, ... fk}` is the fork: `{f1, f2, {f3, ... fk}}`.
