#ifdef NO_FAKE
#include <iostream.h>
#include <strstream.h>
#else
#include "iostrm.h"
#endif

const int BUFFSIZE = 128;
char buff[BUFFSIZE];

int main()
{
  ostrstream out(buff,BUFFSIZE);
  out << "hello" << ends;  // MUST have explicit ends (not needed for fake iostreams)
  // at this point buff is 'hello'
  cout << buff << endl;
  out.seekp(0);
  out << "dolly" << ends;
  // shd be "dolly"
  cout << buff << endl;
}