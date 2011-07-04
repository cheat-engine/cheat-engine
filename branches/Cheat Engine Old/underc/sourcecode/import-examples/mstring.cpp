// UnderC Development Project, 2001
// A simplified standard string class

#ifndef __STRING_
#define __STRING_

#define EXPORT __declspec(dllexport)
#include <string.h>

//namespace std {

class EXPORT string {
protected:
 char *m_str;
 unsigned int m_len;
public:
 typedef unsigned long size_type;
 typedef char *iterator;
 typedef const char *const_iterator;
 enum { npos = 0xFFFFFFFF };

 char *c_str() const { return m_str; }
 size_type  length() const { return m_len; }
 size_type  size()   const { return m_len; }
 // *hack 0.9.5 These aren't really const methods (but!)
 iterator begin() const { return m_str; }
 iterator end() const  { return m_str + m_len; }


 void resize(size_type sz)
 {
  if(m_len > sz) {
   m_len = sz;
   return;  // buffer is quite big enough....
  }	  
  char *tmp = new char[sz+1];
  if (m_str) {
    strcpy(tmp,m_str);
    //delete m_str;
  }
  m_str = tmp;
  m_len = sz;
 }

 void append(char *s)
 {
  resize(m_len + strlen(s));
  strcat(m_str,s);
 }

 void push_back(char ch)
 {
  char st[2];
  st[0] = ch; st[1] = '\0';
  append(st);
 }

 void copy(char *str)
 {	
#ifdef STRING_TESTING
   printf("copy %x\n",str);
#endif
   resize(strlen(str));
   strcpy(m_str,str);
 }

 string(char *str)
 {
  if (!str) str = "";
  m_len = 0;
  m_str = NULL;
  //printf("str %s\n",str);
  copy(str);
 }

 string()
 {
  m_len = 0;
  m_str = NULL;
  copy("");
 }

 string(size_type sz, char ch)
 {
   resize(sz);
   for(size_type i = 0; i < sz; i++) m_str[i] = ch;
   m_str[sz] = '\0';
 }  

 string(const string& s)
 {
  m_str = NULL;
  m_len = 0;
  char *ptr = s.m_str;
  //printf("copy %s\n",ptr);
  copy(s.m_str);
 }

 string& operator= (const string& s)
 {
  copy(s.m_str);  return *this;
 }

 string& operator= (char *str)
 {
   copy(str);
   return *this;
 }
 
 string& operator+= (char *str)
 {
   append(str);  return *this;
 }

 string& operator+= (const string& s)
 {
   append(s.c_str());  return *this;
 }

 string& operator+= (char ch)
 {
   push_back(ch);
   return *this;
 }

 ~string()
 {
#ifdef STRING_TESTING
   printf("killing... '%s' %x\n",m_str,this); 
   if (m_str == NULL) puts("already dead!");
#endif
   delete m_str; 
//#endif
   m_str = NULL;
 }

 size_type find(char *str) const
 {
   char *ss = strstr(m_str,str);
   if (ss) return (size_type)(ss) - (size_type)(m_str);
      else return npos;
 }

 size_type find(const string& s) const
 {
   return find(s.m_str);
 }

 size_type find(char ch) const
 { 
  char str[2]; // init didn't work?
  str[0] = ch;
  str[1] = '\0';
  return find(str);
 }

 size_type rfind(char ch) const
 {
  char *ss = strrchr(m_str,ch);
  if (ss) return (size_type)(ss) - (size_type)(m_str);
      else return npos;
 }

 size_type bound(size_type n) const
 {
 // *hack 0.9.5 necessary while we have trouble w/ unsigned integers
  if (n > m_len || n == npos) n = m_len;
  return n;
 }

 string substr(size_type start, size_type n = npos) const
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

 string& replace(size_type is, size_type n, char *repl)
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

 string& replace(size_type is, size_type n, const string& repl)
 {
   return replace(is,n,repl.m_str);
 }

 char& operator[] (size_type i)
 {
   return m_str[i];
 }

 int compare(const string& s) const
 {
   return strcmp(m_str, s.m_str); 
 }

// *hack 0.9.7 used to be non-member, but that won't work for now!
 bool operator== (const string& s2) const
 {
  return compare(s2) == 0;
 }

 };

// } // namespace std

 EXPORT bool operator != (const string& s1, const string& s2)
 {
  return s1.compare(s2) != 0;
 }

 EXPORT bool operator> (const string& s1, const string& s2)
 {
  return s1.compare(s2) > 0;
 }

 EXPORT bool operator< (const string& s1, const string& s2)
 {
  return s1.compare(s2) < 0;
 }

 EXPORT string operator+ (const string& s1, const string& s2)
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

EXPORT string name_of()
{
  return "hello";
}

#ifdef _IOSTREAM_H

EXPORT ostream& operator<< (ostream& os, const string& s)
{
  os << s.c_str();
  return os;
}

EXPORT istream& operator>> (istream& is, string& s)
{ char buff[512 /*MAX_LINE*/]; 
  is >> buff;
  s = buff; 
  return is;
}

 EXPORT istream& getline(istream& is, string& s)
 { 
   char buff[512 /*MAX_LINE*/ ];
   is.getline(buff,MAX_LINE);
   s = buff;
   return is;
 }

#endif

#endif


 
