function factorial(n) =
   if n == 0 then 1 else n * factorial(n-1);

/*function fact2(n) = factIter(n, 0)
   where factIter(n, acc) =
       n == 0 -> acc ; factIter(n-1, n*acc);
*/

/*function fact2(n):
   n == 0 -> 1 ; <mul <fact . dec>>(n);*/

function fibo(n) =
   if n == 1 or n == 2 then
       1
   else
       fibo(n-1) + fibo(n-2);

print(factorial(10));
print(fibo(10));

let foo = factorial;

print(foo(5)); //-- prints 120 (5!)
// print(foo(5,1)); -- error! Two arguments to function that needs 1
// print(foo()); -- error


