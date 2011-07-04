// Mstring.c
// testing string class
/*
  Yes, I know it's bad to reinvent this particular wheel but
  I don't like the templatized standard string. An extra 30%
  (at least) size penalty for using the char traits templatized
  iostreams and strings, hassle debugging those long names,
  plus longer compile times. A longer version of this rave
  is available at 
    home.mweb.co.za/sd/sdonovan/templates.html

  Masochists and other diehard supporters of the standard
  can switch to using <iostream> and <string> in classlib.h.

  Raves aside, this class could certainly do with improvements.
*/
#include <string.h>
#include "classlib.h"  // includes i/o and mstring headers...

const int MAX_LINE = 1024;

int nCreated = 0;

STR
string::alloc(int size)
{ STR ss = new char[size+1];
   return ss;
}

string::string (const char *ss /*=NULL*/)
//*acts as our default constructor
{
 if (!ss) ss = "";
 m_length = strlen((char *)ss);
 m_data = alloc (m_length);
 strcpy (m_data, (char *)ss);
 nCreated++;
}

string::string (int size)
{
  m_length = size;
  m_data = alloc (size);
  m_data[0] = 0;
  nCreated++;
}

// and (NB) a copy constructor....
string::string (const string& s)
{
  m_length = s.m_length;
  m_data = alloc(m_length);
  strcpy(m_data,s.m_data);
  nCreated++;
}

string::~string()
{
  delete m_data;
  m_data = NULL;
  nCreated--;
}

// assignment operators
string&
string::operator = (const string &s)
{
  copy(s.c_str(),s.length());
  return *this;
}

string&
string::operator = (const char *str)
{
  if (!str) str = "";
  copy (str, strlen(str));
  return *this;
}


int string::compare(const string& s) const
{
    if(!m_data || !s.m_data) return -1; else
    return strcmp(m_data,s.m_data);
}

int string::compare(const char *s) const
{   return strcmp(m_data,s);  }

string
string::substr (int start, int n)
{
   STR in,out;
   int i;
 
   if (!n || start + n > length()) n = length()-start;
   
   string T(n);
   in = c_str() + start;
   out = T.c_str();
   //strcpy(out,in);
   for (i = 0; i < n; i++) *out++ = *in++;
   *out = 0;
   return T;

}
int
string::find(STR ps)
{
   STR ss = strstr(c_str(),ps);
   return !ss ? -1 :long(ss) - long(c_str());
}

int
string::find(string& s)
{
   return find(s.c_str());
}

int
string::find(char ch)
{
   STR ss = strchr(c_str(),ch);
   return !ss ? -1 :long(ss) - long(c_str());
}

int
string::rfind(char ch)
{
  STR ss = strrchr(c_str(),ch);
  return !ss ? -1 :long(ss) - long(c_str());
}

string &
string::operator += (const string &s)
{
  m_length += s.length();
  resize (m_length);
  strcat(m_data, s.m_data);
  return *this;
}

string&
string::operator += (const char *str)
{
  if (!str) str = "";
  m_length += strlen(str);
  resize (m_length);
  strcat(m_data, str);
  return *this;
}

string& 
string::operator += (char ch)
{ 
  char tmp[] = {ch,'\0'};  // very crude!!
  return (*this) += tmp;
}



void string::copy(const char*str, int sz)
{
  if (sz > length() || sz == 0) resize(sz);
  m_length = sz;
  strcpy (m_data,str);
}

int string::resize (int size)
{
   STR new_str = alloc (size);
   if (m_data) strncpy(new_str,m_data,size+1);
   if (m_length > 0) { delete m_data; m_data = NULL; }
   m_data = new_str;
   return size; 
}

string operator + (string s1, string s2)
{
   string temps(s1);
   temps += s2;
   return temps;
}

//#include <iostream.h>
char temp_buff[MAX_LINE];

ostream& operator << (ostream& os, string s)
{
  return os << s.c_str();
}


//ostream& operator << (ostream& os, const string& s)
//{
//  return os << s.c_str();
//}


istream& operator >> (istream& is, string& s)
{
  is >> temp_buff;
  s = temp_buff;
  return is;
}

/*  test code
void do_it()
{
  string s("hello"), b("and"), c(80), d("and");
  char *ss;

  cout << s.length() << "," << b.length() << endl;

  c = s;
  c += b;
  c = s + "hm?" + b + "and what else?";

  cout << " and " << c << endl;

  cout << '*' << s.c_str() << s.substr(1,3) << endl;

  cout << c.find("what") << s.substr(s.find("h"),3) << endl;

  
 }


int main()
{
  string in,key;
  char buff[80];
  int test;

  cout.set_outspace('|');

  do_it();

  cout << nCreated << endl;

  return 0;

}

*/


