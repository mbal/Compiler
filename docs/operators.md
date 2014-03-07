# Operators
## Unary operators

* `-` negate an expression. PREFIX
* `+` makes an expression positive. PREFIX
* `'` (prime operator): creates a 0-ary function from the constant
  supplied. This function can be called with any number of arguments,
  it simply ignores them (arguments are not compiled). POSTFIX
* `not` negates a boolean expression. PREFIX

## Binary operators
* `+`, `-`, `*`, `\` standard arithmetic
* `==`, `<`, `>`, `<=`, `>=`, `!=` standard comparison
* `@` (at operator): index an array. INFIX (e.g. `[1,2,3] @ 2 == 3`)
* `(f g)` (hook): the `(f g)(x)` hook defines a function `h(x)` such
  that `h(x) = f(x, g(x))`. Parenthesis are mandatory, `f` should be a
  dyad, and `g` a monad. Example: `(mul dec)(5) = 5 * (5-1) = 20`
* `f . g` (composition). `(f . g)(x)` defines `h(x)` such that `h(x) =
  f(g(x))`. `.` is right associative: `f . g . h = f . (g . h)`.
