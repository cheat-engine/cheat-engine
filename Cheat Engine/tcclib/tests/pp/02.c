#define x 3
#define f(a) f(x * (a))
#undef x
#define x 2
#define g f
#define z z[0]
#define h g(~
#define m(a) a(w)
#define w 0,1
#define t(a) a
#define p() int
#define q(x) x
#define r(x,y) x ## y
#define str(x) # x
f(y+1) + f(f(z)) % t(t(g)(0) + t)(1);
g(x+(3,4)-w) | h 5) & m
(f)^m(m);
char c[2][6] = { str(hello), str() };
/*
 * f(2 * (y+1)) + f(2 * (f(2 * (z[0])))) % f(2 * (0)) + t(1);
 * f(2 * (2+(3,4)-0,1)) | f(2 * (~ 5)) & f(2 * (0,1))^m(0,1);
 * char c[2][6] = { "hello", "" };
 */
#define L21 f(y+1) + f(f(z)) % t(t(g)(0) + t)(1);
#define L22 g(x+(3,4)-w) | h 5) & m\
(f)^m(m);
L21
L22
