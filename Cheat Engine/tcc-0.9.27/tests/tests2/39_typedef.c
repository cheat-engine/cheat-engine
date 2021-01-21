#include <stdio.h>

typedef int MyInt;

struct FunStruct
{
   int i;
   int j;
};

typedef struct FunStruct MyFunStruct;

typedef MyFunStruct *MoreFunThanEver;

int main()
{
   MyInt a = 1;
   printf("%d\n", a);

   MyFunStruct b;
   b.i = 12;
   b.j = 34;
   printf("%d,%d\n", b.i, b.j);

   MoreFunThanEver c = &b;
   printf("%d,%d\n", c->i, c->j);

   return 0;
}

/* "If the specification of an array type includes any type qualifiers,
   the element type is so-qualified, not the array type." */

typedef int A[3];
extern A const ca;
extern const A ca;
extern const int ca[3];

typedef A B[1][2];
extern B const cb;
extern const B cb;
extern const int cb[1][2][3];

extern B b;
extern int b[1][2][3];

/* Funny but valid function declaration.  */
typedef int functype (int);
extern functype func;
int func(int i)
{
   return i + 1;
}

/* Even funnier function decl and definition using typeof.  */
int set_anon_super(void);
int set_anon_super(void)
{
   return 42;
}
typedef int sas_type (void);
extern typeof(set_anon_super) set_anon_super;
extern sas_type set_anon_super;

/* vim: set expandtab ts=4 sw=3 sts=3 tw=80 :*/
