// C++ By Example, Chapter 9
// Generating XML/HTML
#include "attrib.h"
using namespace std;

string int2str(int i)
{ 
  char buff[30];
  return itoa(i,buff,10);
}

string quotes(string s)
{ return "\"" + s + "\""; }


