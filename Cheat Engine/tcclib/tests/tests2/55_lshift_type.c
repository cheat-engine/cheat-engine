/* $Id: lshift-type.c 53089 2012-07-06 11:18:26Z vinc17/ypig $

Tests on left-shift type, written by Vincent Lefevre <vincent@vinc17.net>.

ISO C99 TC3 says: [6.5.7#3] "The integer promotions are performed on
each of the operands. The type of the result is that of the promoted
left operand."
*/

#include <stdio.h>

#define PTYPE(M) ((M) < 0 || -(M) < 0 ? -1 : 1) * (int) sizeof((M)+0)
#define CHECK(X,T) check(#X, PTYPE(X), PTYPE((X) << (T) 1))
#define TEST1(X,T) do { CHECK(X,T); CHECK(X,unsigned T); } while (0)
#define TEST2(X)                 \
  do                             \
    {                            \
      TEST1((X),short);          \
      TEST1((X),int);            \
      TEST1((X),long);           \
      TEST1((X),long long);      \
    }                            \
  while (0)
#define TEST3(X,T) do { TEST2((T)(X)); TEST2((unsigned T)(X)); } while (0)
#define TEST4(X)                 \
  do                             \
    {                            \
      TEST3((X),short);          \
      TEST3((X),int);            \
      TEST3((X),long);           \
      TEST3((X),long long);      \
    }                            \
 while (0)

static int debug, nfailed = 0;

static void check (const char *s, int arg1, int shift)
{
  int failed = arg1 != shift;
  if (debug || failed)
    printf ("%s %d %d\n", s, arg1, shift);
  nfailed += failed;
}

int main (int argc, char **argv)
{
  debug = argc > 1;
  TEST4(1);
  TEST4(-1);
  printf ("%d test(s) failed\n", nfailed);
  return nfailed != 0;
}
