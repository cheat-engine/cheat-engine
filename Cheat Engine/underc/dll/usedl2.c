/* usedl2.c
 * This second example uses uc_compile_fn() to generate
 * C-calleable function wrappers around UC expressions.
 * Note that the resulting functions use the __stdcall calling
 * convention, which makes it possible for this code to work
 * with other languages.
 */
#include <stdio.h>
#include <math.h>
#include "ucdl.h"

// note where __stdcall goes...
typedef double (__stdcall *DFN)(double);   
typedef int (__stdcall *IFN)(int,int);

int main()
{
 double res, maxdiff = 0.0;
 DFN pf;
 IFN pfi;
 int i;
 uc_init(NULL,0);
 pf = uc_compile_fn("double x","x*x");
 for(i = 0; i < 1000; i++) {
   res = fabs(pf(2.0)-4.0);
   if (res > maxdiff) maxdiff = res;
 } 
 printf("max diff was %lf\n",maxdiff);
 pfi = uc_compile_fn("int x, int y","x - y");
 printf("next call was %d\n",pfi(20,13));
 uc_finis();
}
