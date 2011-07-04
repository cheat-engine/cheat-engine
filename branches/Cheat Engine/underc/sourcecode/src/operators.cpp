/* Operators.cpp
 * Fast lookup for operators
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
*/
#include <stdarg.h>
#include "common.h"
#define NEEDS_LOOKUP
#include "operators.h"
#include "tparser.h"

void Operators::init()
{
  init_lookup(); 
  add("<",LESS_THAN,"<<",LSHIFT,"<=",LEQ,
      "<<=",SHL_A,0);
  add(">",GREATER,">>",RSHIFT,">=",GEQ,
       ">>=",SHR_A,0);
  add("=",ASSIGN,"==",EQUAL,0);
  add("!",LOG_NOT,"!=",NOT_EQUAL,0);
  add("*",STAR,"*=",MUL_A,0);
  add(",",COMMA,0);
  add("+",PLUS,"++",INCR,"+=",ADD_A,0);
  add("-",MINUS,"--",DECR,"-=",MINUS_A,
      "->",ARROW,"->*",MEMBER_ARROW, 0);
  add("%",MODULO,"%=",MOD_A,0);
  add("/",DIVIDE,"/=",DIV_A,0);
  add("|",BIN_OR,"||",LOG_OR,"|=",BOR_A,0);
  add("&",ADDR,"&&",LOG_AND,"&=",BAND_A,0);
  add(":",':',"::",BINARY_SCOPE,0);
  add("~",BIN_NOT,0);
  // bit of a fiddle to get ellipsis recognized...
  add(".",DOT,".*",MEMBER_DOT,"..",DOT,"...",THREEDOT,0);
  add("?",ARITH_IF,0);
  add("^",BIN_XOR,"^=",XOR_A,0);
//*Note shd pass through!!
}

// private to this module...
#include <map>
typedef std::map<int,char *> NameMap;

namespace {
 struct LookupItem {
   char ch;
   int  op;
 };

 struct LookupStruct { 
   int op;                    // op corresp to first char, e.g. ASSIGN
   LookupItem *second_char;   // list of two-letter ops, e.g. "<<","<=". Term. by nul char!
   char ch3;                  // third char of op name, e.g. '=' in "<<="
   int op3;                   // op corresponding
 };

 LookupStruct *ch_lookup[256];
 NameMap name_map;
};

namespace Operators {

void init_lookup()
{
 memset(ch_lookup,0,sizeof(void *)*256);
}

void add(char *first, int id,...)
{
 char *str;
 va_list ap;
 va_start(ap,id);
 static LookupItem ibuff[6];
 // single char must be first!
 LookupStruct *ls = new LookupStruct;
 ch_lookup[first[0]] = ls;
 ls->op = id;
 name_map[id] = first;
 // pick up the second chars, if any!!
 int i = 0;
 while ((str = va_arg(ap,char *)) != NULL) {
   id = va_arg(ap,int);
   name_map[id] = str;
   if (str[2] != '\0') break;   // we hit a 3-char op!
   ibuff[i].ch = str[1];
   ibuff[i].op = id;
   i++;
 }
 ibuff[i].ch = '\0';  // terminate the 2-char list

 if (str != NULL) { ls->ch3 = str[2];  ls->op3 = id; } else ls->ch3 = -1; // wuz 0

 // always add an extra item, even if no 2-char ops!
 ls->second_char = new LookupItem[i+1];
 memcpy(ls->second_char,ibuff,(i+1)*sizeof(LookupItem));
 va_end(ap);
}


int lookup(int ch, TokenStream& ts)
{
 LookupStruct *ls = ch_lookup[ch];
 if (!ls) return ch;  // pass through
 LookupItem *li = ls->second_char;
 char chr, nextch = ts.look_ahead();
 while ((chr = li->ch) != '\0') {
   if (chr == nextch) { 
     ts.next();
     if (ls->ch3 == ts.look_ahead()) { 
        ts.next();
        return ls->op3;
     }
     else return li->op;
   }
   li++;
  }
  return ls->op;
 }           

string name_from_id(int id)
{
  NameMap::iterator nmi = name_map.find(id);
  if (nmi != name_map.end()) return nmi->second; // "operator" + string(nmi->second);
  // otherwise, is either a 'token' operator or a formal operator
  char *s;
  switch(id) {
  case NEW: s = "new"; break;
  case DELETE: s =  "delete"; break;
  case SIZEOF: s =  "sizeof"; break;
  case ARRAY: s =  "[]"; break;
  case FUN_CALL: s =  "()"; break;
  case UMINUS: s =  "-"; break;
  case UPLUS:  s =  "+"; break;
  case BIN_AND: s =  "&"; break;
  case DEREF:  s =  "*"; break;
  case ',':    s = ",";  break;  // often it just _passes through_ depending on comma flag!
  default: s =  ""; break;
  }
  return s; //"operator"+string(s);
}   

} // namespace Operators

