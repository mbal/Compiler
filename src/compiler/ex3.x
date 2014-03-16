function poly(a,b,c) =
    {x: a*x*x + b*x + c};

// should print <function <lambda> at ...>
print(poly(1,5,6));

// well, this can be a problem. How can we infer that poly(1,2,3) is a
// function? For now, let's just turn off the propagation of the defs.
let p = poly(1,5,6);

// should print 6
print(p(0));

// 1*1*1+5*1+6 = 1+5+6 = 12
// currently doesn't work.
print(p(1));
