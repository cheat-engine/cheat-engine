#include <stdio.h>

int main() 
{
   int Count;
   int Array[10];

   for (Count = 1; Count <= 10; Count++)
   {
      Array[Count-1] = Count * Count;
   }

   for (Count = 0; Count < 10; Count++)
   {
      printf("%d\n", Array[Count]);
   }

   return 0;
}

// vim: set expandtab ts=4 sw=3 sts=3 tw=80 :
