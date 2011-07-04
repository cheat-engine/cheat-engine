// C++ By Example, Chapter 9
// Generating XML/HTML
#ifndef __XML_H
#define __XML_H
#include <string>
#include <fstream>
#include "tag.h"

class XMLDoc {
  TagList m_tstack;
  std::ofstream m_out;  
public:
// Tag stack management
  void push_tag(const Tag& tag);
  Tag current() const;
  void push(const Tag& tag);
  bool empty() const;
  bool pop();
 
  virtual void outs(const char *str);
  void outs(const string& s);
  void open(const string& name);
  void close();

  ~XMLDoc()
  { close(); }

 }; // class XMLDoc

XMLDoc& operator<<(XMLDoc& doc, char *s);
XMLDoc& operator<<(XMLDoc& doc, const string& s);
XMLDoc& operator<<(XMLDoc& doc, int val);
XMLDoc& operator<<(XMLDoc& doc, const Tag& tag);

#endif
