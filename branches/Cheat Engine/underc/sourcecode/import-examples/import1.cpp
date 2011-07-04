#include "mstring.h"
#include "import1.h"

EXPORT int sz(FatString fs)
{
  return fs.size();
}

EXPORT int add(int a, int b)
{
  return a + b; 
}

EXPORT double sum(double x, double y)
{
  return x + y;
}

EXPORT int length_of(string s)
{
  return s.size();
}

EXPORT void combine(string s1, string s2, string& res)
{
  res = s1 + s2;
}

namespace std {
  struct SS { int a; int b; };
}

EXPORT void stest(std::SS s, string s1, const string& s2,
 string *ps, string& rs)
{  }

EXPORT string name()
{ return "Hello Dolly"; }

EXPORT string copy1(string s)
{
  return s;
}

EXPORT string copy2(const string& s)
{
  return s;
}

FatString::FatString(char *p) : m_s(p),m_ref(0) { }
FatString::~FatString() { }
FatString::FatString(const FatString& fs)
{  m_s = fs.m_s; m_ref = fs.m_ref; }

string FatString::str()
{
  return m_s; 
}

int FatString::size()
{
  return m_s.size();
}

Fred::Fred() { set(10,20); }
void Fred::set(int a, int b) { m_a = a; m_b = b; }
int Fred::a()     { return m_a; }
int Fred::b()     { return m_b; }
int Fred::sum()   { return m_a + m_b; }



