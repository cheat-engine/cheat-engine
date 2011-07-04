#ifndef __TAG_H
#define __TAG_H
#include "attrib.h"
using std::string;

class Tag: public NamedObject {
  bool m_open,m_temp;
  AttribList *m_attribs;
  mutable AttribList::iterator m_iter;
public:
  Tag(string n="", bool temp=true)
   : NamedObject(n),m_open(true),
     m_temp(temp),m_attribs(NULL)
  { }

  Tag& set(bool on_off);
  void clear();
  bool closed() const       { return ! m_open; }
  bool is_temporary() const { return m_temp; }
  void add_attrib(string a, string v);
  void add_attrib(string a, int v);
  bool    has_attributes() const;
  Attrib *next_attribute() const;
};

typedef std::list<Tag> TagList;

#endif

