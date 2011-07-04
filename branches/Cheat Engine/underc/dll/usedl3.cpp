/* The UnderC extension DLL can be conveniently called via
 * this function exec(), which does all the checking necessary.
 * Do note that any UC headers must be explicitly loaded,
 * and everything is usually in the std namespace.
 */
#include <iostream.h>
#include "ucdl.h"

char buff[256];
#define OUT buff,256

void exec(char *exp)
{
  if (uc_exec(exp)) {
    uc_result(buff,256);
    if (*buff != '\0')
      cout << "result: " << buff << endl;
  } else {
    uc_error(buff,256);
    cout << "error: " << buff << endl;
    int line = uc_error_pos(buff);
    cout << "at " << line << ": " << buff << endl;
  }
}

int main()
{
 uc_init(NULL,0);
 // *** exec() will take all UC commands and C++ statements,
 // *** whether or not they return an actual value.
 // *** (you can archieve the same effect here by uc_init(NULL,1) )
 /*
 uc_exec("#include <iostream>");
 uc_exec("using namespace std;");
 // *** this little macro uses the stringizing operator to
 // *** make inputing quoted strings easier!
 uc_exec("#define S(x) #x");

 exec("ofstream(S(tmp.txt)) << S(Hello Dolly);");
 */
 // *** this will give an error message ***
 exec("20+2");

 uc_finis();
}
