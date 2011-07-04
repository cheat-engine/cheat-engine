// UnderC Development Project, 2001
// A simplified standard string class

#ifdef __UNDERC__
#lib mstring.dll
#endif

#ifndef __STRING_
#define __STRING_

#define NULL 0
// #include <iostream>

//namespace std {

class string {
protected:
 char *m_str;
 unsigned int m_len;
public:
 typedef unsigned long size_type;
 typedef char *iterator;
 typedef const char *const_iterator;
 enum { npos = 0xFFFFFFFF };

 char *c_str() const;
 size_type  length() const;
 size_type  size()   const;
 iterator begin() const;
 iterator end() const;

 void resize(size_type sz);
 void append(char *s); 
 void push_back(char ch); 
 void copy(char *str);
 string(char *str);
 string();
 string(size_type sz, char ch);
 string(const string& s);
 string& operator= (const string& s);
 string& operator= (char *str);
 string& operator+= (char *str);
 string& operator+= (const string& s);
 string& operator+= (char ch);
 ~string();
 size_type find(char *str) const;
 size_type find(const string& s) const;
 size_type find(char ch) const;
 size_type rfind(char ch) const;
 size_type bound(size_type n) const;
 string substr(size_type start, size_type n = npos) const;
 string& replace(size_type is, size_type n, char *repl);
 string& replace(size_type is, size_type n, const string& repl);
 char& operator[] (size_type i);
 int compare(const string& s) const;
 bool operator== (const string& s2) const;
 };

//} // namespace std

 bool operator != (const string& s1, const string& s2);
 bool operator> (const string& s1, const string& s2);
 bool operator< (const string& s1, const string& s2);
 string operator+ (const string& s1, const string& s2);
 string operator+ (const string& s1, char *str2);
 string name_of();


#ifdef _IOSTREAM_H
ostream& operator<< (ostream& os, const string& s);
istream& operator>> (istream& is, string& s);
istream& getline(istream& is, string& s);
#endif

#endif


 
