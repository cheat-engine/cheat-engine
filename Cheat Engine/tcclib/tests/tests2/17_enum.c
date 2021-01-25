#include <stdio.h>

enum fred
{
   a,
   b,
   c,
   d,
   e = 54,
   f = 73,
   g,
   h
};

/* All following uses of enum efoo should compile
   without warning.  While forward enums aren't ISO C,
   it's accepted by GCC also in strict mode, and only warned
   about with -pedantic.  This happens in the real world.  */
/* Strict ISO C doesn't allow this kind of forward declaration of
   enums, but GCC accepts it (and gives only pedantic warning), and
   it occurs in the wild.  */
enum efoo;
struct Sforward_use {
    int (*fmember) (enum efoo x);
};

extern enum efoo it_real_fn(void);
enum efoo {
  ONE,
  TWO,
};
struct S2 {
  enum efoo (*f2) (void);
};
void should_compile(struct S2 *s)
{
  s->f2 = it_real_fn;
}

enum efoo it_real_fn(void)
{
  return TWO;
}

static unsigned int deref_uintptr(unsigned int *p)
{
  return *p;
}

enum Epositive {
    epos_one, epos_two
};

int main()
{
   enum fred frod;
   enum Epositive epos = epos_two;

   printf("%d %d %d %d %d %d %d %d\n", a, b, c, d, e, f, g, h);
   /* printf("%d\n", frod); */
   frod = 12;
   printf("%d\n", frod);
   frod = e;
   printf("%d\n", frod);

   /* Following should compile without warning.  */
   printf ("enum to int: %u\n", deref_uintptr(&epos));

   return 0;
}

/* vim: set expandtab ts=4 sw=3 sts=3 tw=80 :*/
