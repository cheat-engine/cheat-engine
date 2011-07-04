// UnderC Development Project, 2001
// A simplified standard string class
#include "string_imp.h"
#include <string.h>

void string::resize(size_type sz)
{
  if(m_len > sz) {
   m_len = sz;
   return;  // buffer is quite big enough....
  }	  
  char *tmp = new char[sz+1];
  //printf("alloc %x\n",tmp);
  if (m_str) {
    strcpy(tmp,m_str);
    //delete m_str;
  }
  m_str = tmp;
  m_len = sz;
 }

 void string::append(char *s)
 {
  resize(m_len + strlen(s));
  strcat(m_str,s);
 }

 void string::push_back(char ch)
 {
  char st[2];
  st[0] = ch; st[1] = '\0';
  append(st);
 }

 void string::copy(char *str)
 {	
   resize(strlen(str));
   strcpy(m_str,str);
 }

// *fix 1.2.9 allowing NULL to be passed is necessary for our
// map container to work (see map() in <map>)
 string::string(const char *str)
 {
  char *ls = (char *)str;
  if (!ls) ls = "";  // UCW bugge...
  m_len = 0;
  m_str = NULL;
  copy(ls);
 }
 
 string::string(const char *str, int sz)
 {
   resize(sz);
   strcpy(m_str,str); 
 }

 string::string()
 {
  m_len = 0;
  m_str = NULL;
  copy("");
 }

 string::string(size_type sz, char ch)
 {
   resize(sz);
   for(size_type i = 0; i < sz; i++) m_str[i] = ch;
   m_str[sz] = '\0';
 }  

 string::string(const string& s)
 {
  m_str = NULL;
  m_len = 0;
  char *ptr = s.m_str;
  //printf("copy %s\n",ptr);
  copy(s.m_str);
 }

 string& string::operator= (const string& s)
 {
  copy(s.m_str);  return *this;
 }

 string& string::operator= (char *str)
 {
   copy(str);
   return *this;
 }
 
 string& string::operator+= (char *str)
 {
   append(str);  return *this;
 }

 string& string::operator+= (const string& s)
 {
   append(s.c_str());  return *this;
 }

 string& string::operator+= (char ch)
 {
   push_back(ch);
   return *this;
 }

string:: ~string()
 {
   delete m_str; 
   m_str = NULL;
 }

 string::size_type string::find(char *str) const
 {
   char *ss = strstr(m_str,str);
   if (ss) return (size_type)(ss) - (size_type)(m_str);
      else return npos;
 }

 string::size_type string::find(const string& s) const
 {
   return find(s.m_str);
 }

 string::size_type string::find(char ch) const
 { 
  char str[2]; // init didn't work?
  str[0] = ch;
  str[1] = '\0';
  return find(str);
 }

string::size_type string::rfind(char ch) const
 {
  char *ss = strrchr(m_str,ch);
  if (ss) return (size_type)(ss) - (size_type)(m_str);
      else return npos;
 }

 string::size_type string::bound(size_type n) const
 {
 // *hack 0.9.5 necessary while we have trouble w/ unsigned integers
  if (n > m_len || n == npos) n = m_len;
  return n;
 }

 string string::substr(size_type start, size_type n) const
 {
  string ts;
  n = bound(n);
  ts.resize(n);
  char *s1 = ts.m_str;
  char *s2 = m_str+start;
  for(size_type i = 0; i < n; i++) {
      *s1++ = *s2++;
  }
  *s1 = '\0'; 
 // strncpy(s1,s2,n);
  return ts;
 }

 string& string::replace(size_type is, size_type n, char *repl)
 {
  n = bound(n);
  size_type rsz = strlen(repl);
  size_type nextra = rsz - n;
  size_type oldsz = m_len;
  if (nextra != 0) {
    resize(m_len+nextra);
    char *pend = m_str + is + n; // just past target
    memmove(pend+nextra,pend,oldsz-is-n);
    m_str[m_len] = '\0';
  }
  memmove(m_str+is,repl,rsz);
  return *this;
 }

 string& string::replace(size_type is, size_type n, const string& repl)
 {
   return replace(is,n,repl.m_str);
 }
 
 void string::insert(size_type idx, const string& repl)
 {
   replace(idx,0,repl); 
 }

 char& string::operator[] (size_type i)
 {
   return m_str[i];
 }
 
 char string::operator[] (size_type i) const
 {
   return m_str[i];
 }

 int string::compare(const string& s) const
 {
   return strcmp(m_str, s.m_str); 
 }

 int string::compare(const char *s) const
 {
   return strcmp(m_str, s); 
 }

// *hack 0.9.7 used to be non-member, but that won't work for now!
 bool string::operator== (const string& s2) const
 {
  return compare(s2) == 0;
 }

 bool string::operator== (const char* c1) const
 {
   return compare(c1) == 0;
 }

 EXPORT bool operator != (const string& s1, const string& s2)
 {
  return s1.compare(s2) != 0;
}

 EXPORT bool operator != (const string& s1, const char* c1)
 {
  return s1.compare(c1) != 0;
}

EXPORT  bool operator> (const string& s1, const string& s2)
 {
  return s1.compare(s2) > 0;
 }

EXPORT  bool operator< (const string& s1, const string& s2)
 {
  return s1.compare(s2) < 0;
 }

EXPORT  string operator+ (const string& s1, const string& s2)
 {
   string out = s1;
   out.append(s2.c_str());
   return out;
 }

EXPORT string operator+ (const string& s1, char *str2)
 {
   string out = s1;
   out.append(str2);
   return out;
 }




 
