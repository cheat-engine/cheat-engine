/* KEYWORDS.CPP
 * Looking up keywords (contains full list)
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
*/
// get rid of nasty & irrevalent messages about debug map names
#pragma warning(disable:4786)
#include "common.h"  
#include "tparser.h"
#include "classlib.h"
#include <map>

typedef std::map<string,int> IntMap;

namespace Keywords {

IntMap kmap;
IntMap::iterator kmap_end;

int  lookup(const string& name)
{  
  //kmap[name]; not a good idea - _always_ adds an entry!
  IntMap::iterator iim = kmap.find(name);
  return (iim != kmap_end) ? iim->second : 0;
}

string find(int id)
{
// the opposite operation!
 IntMap::iterator iim;
 for(iim = kmap.begin(); iim != kmap_end; ++iim)
  if (iim->second == id) return iim->first;
 return "<unknown>";
}

void add(char *name, int val)
{ 
    kmap[name] = val;
}

void init()
{
 add("unsigned",UNSIGNED);
 add("char",CHAR);
 add("int",INT);
 add("short",SHORT);
 add("long",LONG);
 add("float",FLOAT);
 add("double",DOUBLE);
 add("bool",BOOL);
 add("void",VOID);
 add("const",CONST);
 add("static",STATIC);
 add("virtual",VIRTUAL);
 add("typedef",TYPEDEF);
 add("class",CLASS);
 add("struct",STRUCT);
 add("private",PRIVATE);
 add("protected",PROTECTED);
 add("public",PUBLIC);
 add("enum",ENUM);
 add("new",NEW);
 add("delete",DELETE);
 add("sizeof",SIZEOF);
 add("if",IF);
 add("else",ELSE);
 add("while",WHILE);
 add("do",DO);
 add("for",FOR);
 add("switch",SWITCH);
 add("case",CASE);
 add("default",DEFAULT);
 add("return",RETURN);
 add("continue",CONTINUE);
 add("break",BREAK);
 add("operator",OPERATOR);
 add("static_cast",STATIC_CAST);
 add("const_cast",CONST_CAST);
 add("dynamic_cast",DYNAMIC_CAST);
 add("reinterpret_cast",REINTERPRET_CAST);
 add("namespace",NAMESPACE);
 add("using",USING);
 add("try",TRY);
 add("catch",CATCH);
 add("throw",THROW);
 add("template",TEMPLATE);
 add("extern",EXTERN);
 add("explicit",EXPLICIT);
 add("friend",FRIEND);
 add("typeof",TYPEOF);
 add("__stdcall",STDCALL);
 add("__API",API);
 add("__lambda",LAMBDA);
 add("goto",GOTO);     // *add 1.2.5 
 add("union",UNION);   // *add 1.2.6
 add("__init_block__",FAKE_INIT_LIST); // *add 1.2.7 A syntactical hack; see ParserState::handle_method_body


 kmap_end = kmap.end();
}
  

};
