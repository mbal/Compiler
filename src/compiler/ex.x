function factorial(n) =
   if n == 0 then 1 else n * factorial(n-1);

/*function fact2(n):
   n == 0 -> 1 ; <mul fact . dec>(n);*/

function fibo(n) =
   if n == 1 or n == 2 then
       1
   else
       fibo(n-1) + fibo(n-2);

print(factorial(100));

print(fibo(10));
