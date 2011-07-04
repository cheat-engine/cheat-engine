#include <iostream>
#include <string>
#include <uc_except.h>
using namespace std;

void do_something()
{
  throw string("help");
}

bool test_except( )
{
 bool ok = false;
 try {
   throw(1);
 } catch(int val) {
   ok = val==1;
 }
 try {
   do_something();
 } catch(string s) {
   ok = s=="help";
 }
 try {
   int i = 0, k = 20/i;
 } catch(Exception& e) {
   cerr << "what!\n";
 }
 return ok;
}

int main()
{
 if (! test_except()) {
   cerr << "Exception test failed" << endl;
   return -1;
 }
 return 0;
}