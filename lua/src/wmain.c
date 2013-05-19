#include <windows.h>
#include <stdlib.h> /* declaration of __argc and __argv */

extern int main(int, char **);

int PASCAL WinMain(HINSTANCE hinst, HINSTANCE hprev, LPSTR cmdline, int ncmdshow)
{
  int rc;
  
  extern int __argc;     /* this seems to work for all the compilers we tested, except Watcom compilers */
  extern char** __argv;
  
  rc = main(__argc, __argv);
  
  return rc;
}
