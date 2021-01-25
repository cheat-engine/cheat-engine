#include <stdio.h>

int main()
{
   int x, y, z;

   for (x = 0; x < 2; x++)
   {
      for (y = 0; y < 3; y++)
      {
         for (z = 0; z < 3; z++)
         {
            printf("%d %d %d\n", x, y, z);
         }
      }
   }

   return 0;
}

/* vim: set expandtab ts=4 sw=3 sts=3 tw=80 :*/
