// rx++
// A simple C++ regexp class based on the 
// GNU RX library

#ifndef __RXPLUS_H
#define __RXPLUS_H

#include "regexp.h"

const int _NM_ = 20;

class Regexp {
private:
  regex_t m_rx;
  int m_ret;
  char* m_str;
  regmatch_t* m_rm;
  bool m_advance;

public:
  Regexp(char *pat = NULL, int flags = 0) {
     m_advance = false;
     m_ret = regcomp(&m_rx,pat,flags);
     m_rm = new regmatch_t[_NM_];
  }

  ~Regexp() {
     regfree(&m_rx);
     delete m_rm;
  }

  bool is_ok()                          { return m_ret == 0; }
  void set_str(char *str)               { m_str = str; m_advance = false; }

  bool match(char *str, int flags = 0)
  {
    m_str = str;
    m_ret = regexec(&m_rx,m_str,_NM_,m_rm,flags);
    return m_ret == 0;
  }

  bool match(const string& s, int flags = 0)
  { return match(s.c_str(),flags); }

  int index(int idx = 0) {
     return m_rm[idx].rm_so;
  }

  int end_match(int idx = 0) {
     return m_rm[idx].rm_eo;
  }

  bool next_match(int flags = 0)
  {
    if (m_advance) m_str += end_match(0);
    m_ret = regexec(&m_rx,m_str,_NM_,m_rm,flags);
    m_advance = m_ret == 0;
    return m_advance;
  }

  bool operator= (char *p)            { return match(p); }
  bool operator= (const string& s)    { return match(s); }

  string matched(int idx = 0)
  {
    int start = index(idx);
    int finish = end_match(idx);
    if (start == -1) return "";
    string s = m_str;
    return s.substr(start,finish - start);
  }
  
  void get_error(string& err)
  {
   int len = regerror(m_ret,&m_rx,NULL,0); // how big is error string?
   char *buff = new char[len];
   regerror(m_ret,&m_rx,buff,len); // fetch the error...
   err = buff;
   delete buff;
  }
};

#endif
