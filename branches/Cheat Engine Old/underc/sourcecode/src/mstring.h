/* MSTRING.H
 * A simple string class which implements a reasonable subset
 * of the standard template basic_string class.
 * Part of the MLIB library
*/

#ifndef _MSTRING_H
#  define _MSTRING_H
typedef char *STR;

const int S_TEMP = 1, S_LOAN = 2;

class string {
//------------
 private:
   STR m_data;
   int m_length;

 public:
  string (const char *str=0);
  string (int size);
  string (const string& s);
  string (const string& s,int flags);
  ~string ();

  typedef char *iterator;
  iterator begin()  { return m_data; }
  iterator end()    { return m_data+m_length; }

  int  length () const
    { return m_length; }
  int  size   () const
    { return m_length; }

  STR  c_str () const
    { return m_data; }

  string& operator = (const string &s);
  string& operator = (const char *str);
  char& operator [] (int i) { return m_data[i]; }

  void copy(const char *str, int sz);
  int compare(const string& s) const;
  int compare(const char *s) const;

  string& operator += (const string &s);
  string& operator += (const char *str);
  string& operator += (char ch);

  string substr (int start, int n=0);
  int     find   (STR ps);
  int     find   (string& s);
  int     find   (char ch);
  int     rfind  (char ch);

 STR alloc (int size);
 int resize(int size);

};

 string operator + (string s1, string s2);

 // *change 1.2.8 (string,char*) comparisons no longer involve temporary creation
 inline int operator == (const string& s1, const string& s2) { return s1.compare(s2)==0; }

 inline int operator == (const string& s1, char *ps2) { return s1.compare((const char*)ps2)==0; }
 inline int operator != (const string& s1, const string& s2) { return s1.compare(s2)!=0; }
 inline int operator != (const string& s1, const char*ps2) { return s1.compare(ps2)!=0; }
 inline int operator <  (const string& s1, const string& s2) { return s1.compare(s2)<0;  }
 //inline int operator <  (string s1, string& s2) { return s1.compare(s2)<0;  }
 inline int operator >  (string& s1, string& s2) { return s1.compare(s2)>0;  }
 
 
 //#ifdef __IOSTRM_H
   ostream& operator << (ostream&, string);
  // ostream& operator << (ostream&, const string&);
   istream& operator >> (istream&, string&);
 //#endif
 

 #endif

