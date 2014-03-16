function poly(a,b,c) =
    {x: a*x*x + b*x + c};

// should print <function <lambda> at ...>
print(poly(1,5,6));

let p = poly(1,5,6);

// should print 6
print(p(0));

// 1*1*1+5*1+6 = 1+5+6 = 12
print(p(1));
