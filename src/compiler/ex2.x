function mul(a,b) = a * b;
function dec(a) = a - 1;

/*function fact(n) =
    if n == 0 then 1 else n * ({fact.dec})(n);*/

function test(x) =
    dec(dec(x));

let f  = {x : dec ( dec(x))};
function foo() = {dec . dec};
print(f(3));

//print(fact(100));
