#include <stdio.h>

static int fred = 1234;
static int joe;

void henry()
{
   static int fred = 4567;

   printf("%d\n", fred);
   fred++;
}

int main()
{
   printf("%d\n", fred);
   henry();
   henry();
   henry();
   henry();
   printf("%d\n", fred);
   fred = 8901;
   joe = 2345;
   printf("%d\n", fred);
   printf("%d\n", joe);

   return 0;
}

/* vim: set expandtab ts=4 sw=3 sts=3 tw=80 :*/
