X1 __COUNTER__
X2 __COUNTER__
#if __COUNTER__
X3 __COUNTER__
#endif
#define pass(x) x
#define a x __COUNTER__ y
#define a2 pass(__COUNTER__)
#define f(c) c __COUNTER__
#define apply(d) d d __COUNTER__ x2 f(d) y2 __COUNTER__
#define _paste(a,b) a ## b
#define paste(a,b) _paste(a,b)
#define _paste3(a,b,c) a ## b ## c
#define doublepaste(a,b) _paste3(a,b,b)
#define str(x) #x
X4 a
X5 f(a)
X6 f(b)
X7 f(__COUNTER__)
X8 apply(a)
X9 apply(f(a))
X10 apply(__COUNTER__)
X11 apply(a2)
X12 str(__COUNTER__)
X13 paste(x,__COUNTER__)
X14 _paste(x,__COUNTER__)
X15 doublepaste(x,__COUNTER__)
