/* usedl7.c
 * Demonstrates redirection of DLL output
 * to a specified callback function in the main program
*/
#include <stdio.h>
#include "ucdl.h"

/* Under Win32, we use the __stcall calling convention to make
   it easier for other languages like Delphi to use the library.
   Under Linux, all the functions use the usual cdecl calling convention
*/

/* the actual callback must have this form */
void STD dump_fn(char *buff)
{
 printf("'%s'",buff);
}

int main()
{
  uc_init(NULL,1);
  /* here we are telling UC to force the import of our callback,
     but with the specific name _dout_callback
  */
  uc_import("void _dout_callback(char*)",&dump_fn);

  /* any output sent to cout will be sent to our callback,
     as soon as each line is flushed (which endl always does)
  */
  uc_exec("cout << 20.3 << \" and \" << 20 << endl;");
  uc_exec("cout << 'x' << endl;");

//  uc_exec("#lib $self");
  uc_finis();
}
