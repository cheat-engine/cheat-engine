#include <stdio.h>

int array[16];

//Swap integer values by array indexes
void swap(int a, int b)
{
   int tmp  = array[a];
   array[a] = array[b];
   array[b] = tmp;
}

//Partition the array into two halves and return the
//index about which the array is partitioned
int partition(int left, int right)
{
   int pivotIndex = left;
   int pivotValue = array[pivotIndex];
   int index = left;
   int i;

   swap(pivotIndex, right);
   for(i = left; i < right; i++)
   {
      if(array[i] < pivotValue)
      {
         swap(i, index);
         index += 1;
      }
   }
   swap(right, index);

   return index;
}

//Quicksort the array
void quicksort(int left, int right)
{
   if(left >= right)
      return;

   int index = partition(left, right);
   quicksort(left, index - 1);
   quicksort(index + 1, right);
}

int main()
{
   int i;

   array[0] = 62;
   array[1] = 83;
   array[2] = 4;
   array[3] = 89;
   array[4] = 36;
   array[5] = 21;
   array[6] = 74;
   array[7] = 37;
   array[8] = 65;
   array[9] = 33;
   array[10] = 96;
   array[11] = 38;
   array[12] = 53;
   array[13] = 16;
   array[14] = 74;
   array[15] = 55;

   for (i = 0; i < 16; i++)
      printf("%d ", array[i]);

   printf("\n");

   quicksort(0, 15);

   for (i = 0; i < 16; i++)
      printf("%d ", array[i]);

   printf("\n");

   return 0;
}

/* vim: set expandtab ts=4 sw=3 sts=3 tw=80 :*/
