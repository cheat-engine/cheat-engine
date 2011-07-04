/* This shows the basic calling sequence for
 * the UnderC extension DLL
 */
#include <stdio.h>
#include "ucdl.h"

int main()
{
 char buff[128];
 int SZ = 128;
 uc_init(NULL,0);
 uc_exec("20*2.3;");
 uc_result(buff,SZ);
 printf("result was %s\n",buff);
 uc_finis();
}
