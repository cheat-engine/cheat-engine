// C++ By Example, Chapter 9
// Generating XML/HTML
#include "tag.h"
using std::string;

Tag& Tag::set(bool on_off)
 { m_open = on_off; return *this; }

void Tag::clear()
{ 
 delete m_attribs;
 m_attribs = NULL;
}  

void Tag::add_attrib(string a, string v)
{ 
    if (m_attribs == NULL) m_attribs = new AttribList;
    m_attribs->push_back(new Attrib(a,v));
}

void Tag::add_attrib(string a, int v)
{
     add_attrib(a,int2str(v));
}

bool Tag::has_attributes() const
{
    if (m_attribs == NULL) return false;
    m_iter = m_attribs->begin();
    return true;
}

Attrib *Tag::next_attribute() const
{
    if (m_iter == m_attribs->end()) return NULL;
    Attrib *a = *m_iter;
    ++m_iter;
    return a;
}


