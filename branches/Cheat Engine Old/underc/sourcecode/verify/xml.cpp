// C++ By Example, Chapter 9
// Generating XML/HTML
#include "xml.h"
using std::string;

void XMLDoc::push_tag(const Tag& tag)  {  m_tstack.push_back(tag); }
bool XMLDoc::empty()   const { return m_tstack.size()==0; }

void XMLDoc::push(const Tag& tag)
{
     Attrib *a;
     m_out << '<' << tag.name();
     if (tag.closed()) m_out << '/';
     else if (tag.has_attributes()) {
        m_out << ' ';
        while ((a = tag.next_attribute()) != NULL)
          m_out << a->name() << '=' << quotes(a->value()) << ' ';
     }
     m_out << '>';
     push_tag(tag);   
} 

bool XMLDoc::pop()
{
    if (empty()) return false;
    Tag& tag = m_tstack.back();
    m_out << "</" << tag.name() << "> ";
    m_tstack.pop_back();
    return ! empty();
}

void XMLDoc::outs(const char *str)
{ 
    m_out << str;
    while (!empty() && m_tstack.back().is_temporary()) { 
        pop() ;       
    }
}
void XMLDoc::outs(const string& s)
{  outs(s.c_str()); }

void XMLDoc::open(const string& name)
{ m_out.open(name.c_str()); }

void XMLDoc::close()
{
   // close out ALL pending tags
    while (pop())   ;
    m_out.close();
}

XMLDoc& operator<<(XMLDoc& doc, char *s)
{
  doc.outs(s);
  return doc;
}

XMLDoc& operator<<(XMLDoc& doc, const string& s)
{
  doc.outs(s);
  return doc;
}

XMLDoc& operator<<(XMLDoc& doc, int val)
{
  doc.outs(int2str(val));
  return doc;
}

XMLDoc& operator<<(XMLDoc& doc, const Tag& tag)
{
   doc.push(tag);
   return doc;
}

