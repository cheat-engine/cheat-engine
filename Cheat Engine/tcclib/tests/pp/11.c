#define D1(s, ...) s
#define D2(s, ...) s D1(__VA_ARGS__)
#define D3(s, ...) s D2(__VA_ARGS__)
#define D4(s, ...) s D3(__VA_ARGS__)

D1(a)
D2(a, b)
D3(a, b, c)
D4(a, b, c, d)

x D4(a, b, c, d) y
x D4(a, b, c) y
x D4(a, b) y
x D4(a) y
x D4() y

#define GNU_COMMA(X,Y...) X,## Y

x GNU_COMMA(A,B,C) y
x GNU_COMMA(A,B) y
x GNU_COMMA(A) y
x GNU_COMMA() y

#define __sun_attr___noreturn__ __attribute__((__noreturn__))
#define ___sun_attr_inner(__a) __sun_attr_##__a
#define __sun_attr__(__a) ___sun_attr_inner __a
#define __NORETURN __sun_attr__((__noreturn__))
__NORETURN
#define X(...)
#define Y(...)  1 __VA_ARGS__ 2
Y(X X() ())
