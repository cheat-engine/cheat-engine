// C++ By Example, Chapter 9
// Generating XML/HTML
#ifndef __ATTRIB_H
#define __ATTRIB_H
#include <string>
#include <list>
//using std::string;
using namespace std;

string int2str(int i);
string quotes(string s);

class NamedObject {
  string m_name;
public:
 NamedObject(string name="") 
  : m_name(name) {}

 string name() const { return m_name; }
 void   name(string s) { m_name = s; }

};

class Attrib: public NamedObject {
    string m_value;
public:
  Attrib (string nm="", string vl="")
  : NamedObject(nm), m_value(vl)
  {}
  string value() const { return m_value; }
};

typedef std::list<Attrib *> AttribList;
typedef AttribList::iterator ALI;



#endif
