// C++ By Example, Chapter 9
// Generating XML/HTML, etc
#include "xml.h"
using namespace std;

Tag bold("BOLD");
Tag italic("ITALIC");
Tag link_tag("A");
Tag para("P");
Tag head_tag("");


class HTMLDoc: public XMLDoc {
public:
   void open(string name, string doc_title="", string clr="#FFFFFF")
   {
     if (name.find(".") == string::npos) name += ".htm";
     XMLDoc::open(name);
     if (doc_title=="") doc_title = name;
     
     push(Tag("HTML",false));
      push(Tag("HEAD"));
      push(Tag("TITLE"));
      XMLDoc::outs(doc_title);
      XMLDoc::outs("\n");
 
      Tag body_tag("BODY",false);
      body_tag.add_attrib("bgcolor",clr);
      push(body_tag);
      XMLDoc::outs("\n");
   }

   HTMLDoc(string name, string doc_title="", string clr="#FFFFFF")
   {
     open(name,doc_title,clr);
   }

   void outs(const char *str)  // override
   {
   // calls original method to do output
     if (strchr(str,'\n') != NULL) {
	  char buff[256];
	  strcpy(buff,str);
      for(char *t = strtok(buff,"\n");  t != NULL; t = strtok(NULL,"\n"))
      { 
        push(para);
        XMLDoc::outs(t);  
        XMLDoc::outs("\n");
      }
     } else XMLDoc::outs(str);
   }
};


Tag& head(int level = 0)
{ 
   head_tag.name("H" + int2str(level));
   return head_tag.set(level > 0);
}

Tag& link(string fname)
{
   link_tag.clear();
   link_tag.add_attrib("HREF",fname);
   return link_tag;
}

void exercise()
{
  int n = 58;
  HTMLDoc doc("first","An introduction");
  doc << head(1) << "Here is a Title";
  doc << "There have been " << n << " visitors\n";
  doc << bold << "whatever\n";
  doc.close();
}  

int main(int argc, char **argv)
{
 exercise();
 return 0;
}
