#define f(x) x
#define g(x) f(x) f(x
#define i(x) g(x)) g(x
#define h(x) i(x))) i(x
#define k(x) i(x))) i(x))))
f(x)
g(x))
i(x)))
h(x))))
k(x))))
