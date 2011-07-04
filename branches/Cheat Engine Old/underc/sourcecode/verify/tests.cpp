// tests.cpp
#include "html.h"

#include <iostream>
#include <fstream>
#include <string>
#include <list>
#include <algorithm>
using namespace std;

#define N 512

void compare(const char *f1, const char *f2)
{
  char buff1[N],buff2[N];
  ifstream in1(f1), in2(f2);
  while (! in1.eof()) {
     in1.getline(buff1,N);
     in2.getline(buff2,N);
     if (string(buff1) != buff2) 
       cout << "mismatch: " << f1 << endl
        << buff1 << endl << buff2 << endl;
  }
}

void test_string(ostream& out)
{
 string s = "hello dolly";
 s += " you're so fine!";
 out << "first:" << s << endl;
 out << s.substr(1,3) << ' ' << s.substr(1,string::npos) << endl;
 out << "d = " << s[s.find("doll")] << endl;
 s.replace(0,4,"heaven");
 out << s + " and that's all" << endl;
}

void test_output(ostream& out)
{
  int i = 1;
  float f = 2.0;
  double d = 2.0;
  char *ps = " text ";
  out << i << ' ' << f << ' ' << d << ps << endl;
}

typedef list<string> LS;
typedef list<double> LD;

LS& operator<< (LS& ls, string s)
{
  ls.push_back(s);
  return ls;
}

ostream& operator<< (ostream& os, LS& ls)
{
  LS::iterator li;
  for(li = ls.begin(); li != ls.end(); ++li)
     os << *li << ' ';
  return os;
} 

void test_list(ostream& out)
{
  LS ls;
  ls << "one" << "two" << "three" << "four";

// insert before 'three'
  LS::iterator li = find(ls.begin(), ls.end(),"three");
  ls.insert(li,"three-half");
  out << ls << endl;

  ls.remove("two");
  out << ls << endl;

  LD ld;
  double d[] = {2.2, 5.3, 6.7, 4.1};
  copy(d,d+4,back_inserter(ld));
  LD::iterator di;
  for(di = ld.begin(); di != ld.end(); ++di)
     out << *di << ' ';
  out << endl;
  while (ld.size() > 0) {
     out << ld.back() << ' ';
     ld.pop_back();
  }
  out << endl;     
} 

void test_html()
{
  exercise();  // from html.cpp
}

int main()
{
  test_string(ofstream("t1.txt"));
  compare("t1.txt","t1c.txt");
  test_output(ofstream("t2.txt")); 
  compare("t2.txt","t2c.txt");
  test_list(ofstream("t3.txt"));
  compare("t3.txt","t3c.txt");

  test_html();
  compare("first.htm","first_c.htm");
  return 0;
}