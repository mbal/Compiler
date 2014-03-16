function dec(x) = x - 1;
function mul(a,b) = a*b;

function fact1(x) =
    if x == 0 then
       1
    else
       ({mul, fact1.dec})(x);

function fact2(x) =
    if x == 0 then 1 else x * fact2(x-1);

// the factorial is not a good example: function calls are expensive
// in python and the fact1 implementation of factorial is 4 times
// slower than the usual recursive version.
// We can probably improve a bit with inlining, currently not implemented.
print(fact1(6));

// fact2, instead, is quite aligned, in terms of performance (~10%
// slower). We could improve the performance a little bit.
// currently, the disassembly has 2 jumps (1 for the IF, and one for
// the THEN branch, to jump to the RETURN at the end), while python
// compiles the same code with only a JUMP.

print(fact2(6));
