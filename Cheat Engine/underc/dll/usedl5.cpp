/* This uses the UCRI reflection interface
 * of the UC DLL
 */

#include <iostream.h>  
#include "mstring.h"

ostream& operator << (ostream& out, string s)
{
  out << s.c_str();
  return out;
}

#include "../src/ucri.h"
#include "ucdl.h"

UCDll session;

int main()
{ 
  static string s;

  // all the methods of class std::string
  XClass *ps = uc_std()->lookup_class("string");
  XFunctions& fl = ps->functions();
  for(XFunctions::iterator fli = fl.begin(); fli != fl.end(); ++fli) {
    (*fli)->as_str(s);
    cout << s << endl;
  }
  
 }