/* In this example, our program exports two functions
 * which are accessed via a UC script run from the DLL
 * (It is a curious and useful fact that Win32 programs
 * can export functions like DLLs)
 */
#include <stdio.h>
#include "ucdl.h"

#define EXPORT __declspec(dllexport)

EXPORT double sqr(double x)
{
 return x*x;
}

EXPORT int sum(int x, int y)
{
 return x+y;
}

int main()
{
// This ensures that the usual standard headers are included
// before any scripts are loaded
 uc_init(NULL,1);
 for(;;) {
   printf(">>> ");
   if (getch() == '.') break;
   uc_include("script.uc");
 }
 uc_finis();
}
