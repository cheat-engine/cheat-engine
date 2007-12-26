/* Lexical scanner for bison grammar in parser.y;  supplies yylex()
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 *
 * There are a number of fairly hectic lexical tie-ins which I found necessary, no doubt
 * because my grammar is dubious.  However, I was not trying to make the grammar elegant.
 */
#include "common.h"
#include "tparser.h"
#include "keywords.h"
#define NEEDS_LOOKUP
#include "operators.h"
#include "input.h"
#include "uc_tokens.h"
#include "errors.h"
#include <ctype.h>

bool interactive_mode(); // in main.cpp
bool check_error();  // in parser.y
char *substitute(TokenStream& tok, char *buff, char **actual_args, char *subst); // from subst

UCTokenStream input(NULL);

// this looks at the return code from the tokenizer and generates entries
// depending on whether it's a number, string, etc.  Do note that all constants
// use the same mechanism as variables; they are allocated space in the data section.
// This simplifies the instruction set w/out much impact on performance.
static int do_constant(int c)
{
 static char buff[512];
 char *ptr = buff;
 char *endp;
 PEntry pe = new Entry; 
 pe->rmode = DIRECT;
 pe->size = 1;  // by default; for arrays it's > 1!!
 pe->m_typename = false;
 yylval.entry = pe;
 switch(c) {
 case T_STRING:
    ptr = input.get_string(); 
    pe->size = strlen(ptr)+1; 
    //HACK01 Char array of size 1 is miscoded!
    if (pe->size == 1) { pe->size=2; ptr[1] = '\0'; }
    pe->type = t_char_ptr;
    break;
 case T_CHAR:
    input.get_str(ptr);
    pe->type = t_char;
    break;
 case T_INT:
     *(int *)ptr = input.get_int();
    if (*(int *)ptr == 0) pe->type = t_zero;
    else pe->type = t_int;
    break;
 case T_HEX:
 case T_OCT:
     input.get_str(ptr);
     *(unsigned long *)ptr = strtoul(ptr,(char **)&endp,c==T_HEX ? 16 : 8);
     pe->type = t_int;
/* Can't understand this one today!
	 if (endp[1] != '\0') {
	    error("Bad hex or oct constant");
     } 
*/
     break;
 case T_FLOAT:
    *(float *)ptr = input.get_float();
    pe->type = t_float;
    break;
 case T_DOUBLE:
    *(double *)ptr = input.get_float();
    pe->type = t_double;
    break;
  } 
  // note that string literals are _not_ const!!
  if (c != T_STRING) pe->type.make_const();
  int sz = pe->type != t_char_ptr ? pe->size*pe->type.size() : pe->size;
  pe->data = Parser::global().alloc(sz, ptr);
  return CONSTANT;
}


// *fix 1.2.4 This nasty look-ahead is trying to distinguish between
// 'C<T> c' (a declaration) and 'C<T>::c' (an expression). As long as we need 
// these nasty things, we need to get it right. I'm now excluding any '::' found
// within parens, as well as within angle brackets
// *fix 1.2.7 This was broken for '(C<T> c, A::B)'. So I'm hopping out on ','
static bool hunt_for_scope()
{
  int i = 0, level = 0;   // i wuz 1!!
  while (input.peek_ahead(i) != 0) {
    char c1 = input.peek_ahead(i), c2 = input.peek_ahead(i+1);
    if (c1=='<' || c1=='(') level++; else
    if (c1=='>' || c1==')') level--;
    if (level == 0 && c1==',') return false;
    if (level == 0 && c1==':' && c2==':') return true;
    i++;
  }
  return false;
}

static int count(char *p, char ch, bool in_brackets)
{
	int blevel = 0, ncount = 0;
	while (*p) {
		if (*p == '{') blevel++; else
        if (*p == '}') blevel--; else
		if (*p == ch) { 
			if (blevel > 0   && in_brackets) ncount++;
			if (blevel == 0  && ! in_brackets) ncount++;
        } else
        if (*p == '/' && *(p+1) == '/') break;  // ignore line comments!
		p++;
    }
	return ncount;
}

extern bool in_template;
extern bool no_new_line; // *HACK* *HACK*

// *fix 1.2.3L GCC2.96 doesn't like local functions
// just like strchr() but returns pointer to end of 'str' when not found
static char *ch_search(char *str, char c) {
  while (*str && *str != c) ++str;
  return str;
}

// *add 1.2.7 Finds the first character in buff which is not alphabetic,
// a digit, or '_'
static char *first_non_alphanum(char* p)
{
  while (*p && (isspace(*p) || *p == '_' || isalnum(*p))) ++p;
  return p;
}

// Bison parser calls this for each new token!!
// In some ways this is the most frightening code I've written,
// because of the lexical tie-in hacks that were necessary to
// get the grammar to moreorless behave.  If some kind soul
// would sort out the grammar, then this would become sensible
// code we could be proud of.
int yylex()
//---------
{
  static string tok;
  static string INT_ = "int";
  static bool conversion_op = false;
  static char token_buff[80];
  int ival;
  static PEntry class_entry;
  char *pos;
  PEntry pe;
  bool not_in_dcl = !Parser::is_in_declaration();
  int c = input.next();  
  if (c == T_TOKEN) {
    tok = input.get_token();
    ival = Keywords::lookup(tok);
    if (ival != 0) {
      if (ival == OPERATOR) {
         pos = input.current();
         // [] and () have no equivalent as tokens...
         if (input.next_two("[]")) tok = "[]"; else
         if (input.next_two("()")) tok = "()"; 
         else {
           ival = yylex();
           tok = Operators::name_from_id(ival);
           if (tok == "") { // can only be a type conversion operator
              input.current(pos);    // so reset stream position
			  conversion_op = true;  // remember we're in a conversion operator...
              return OPERATOR;       // and let OPERATOR pass through
           } 
         }
         goto token_lookup;
      } else // virtual destructor syntax currently requires this look-ahead...
      if (ival == VIRTUAL && input.look_ahead(true)=='~') {
          Parser::state.modifier = Virtual;
          return yylex();
      } else // *hack 1.1.0 support 'unsigned long int' etc
	  if ((ival == LONG || ival == SHORT) && input.peek_next_token() == INT_) {
         input.next();  // throw away 'int'
      }	  
	  else
	  // *hack 1.1.4 'struct _A a;' or 'class Bonzo;' - collides w/ regular use.
	  // this extremely nasty code is looking for 'token token' after the keyword
	  // _on the same line_
      // *1.2.2 (Eric) An improvement (SJD) But strip line comments!
      // *1.2.7 A simplification, which also fixes broken 'struct A a = {1,2}'
      // In a struct defn, the tagname is followed by a ':' or a '{'.
      // Otherwise, it's used in a declarative sense.
	  if (ival == STRUCT || ival == CLASS || ival == UNION) { 
	      char *p = first_non_alphanum(input.current());                     
          if (! strchr(":{/<>,",*p)) {
               if (ival == STRUCT) ival = STRUCT_Y; 
               else if (ival == UNION)  ival = UNION_Y;
     	 		                  else  ival = CLASS_Y;
          } 
      }

	  // *hack 1.1.4 class template syntax is confused by new declarative use of class...	  
	  if (in_template) {
	   if (ival == STRUCT) ival = STRUCT_X; else
	   if (ival == CLASS)  ival = CLASS_X;
       in_template = false;
      }
	  
	  conversion_op = false;
      yylval.val = ival;
      return ival;
    }   
  token_lookup:
    pe = Parser::symbol_lookup(tok);
    bool not_label = (pe==NULL) ? input.peek_ahead(0) != ':' || input.peek_ahead(1) == ':' : true;
    // _not_ an error if we can't find the token in declaration mode, or if followed by a single ':'
    if (!pe && not_in_dcl && ! Parser::state.in_typedef && not_label) {
      error("Cannot find '" + tok + "'");
      // *SJD* Shd return 0 to terminate if NOT in interactive mode!
      yylval.val = 0;
      return TOKEN;
    } 

    // _always_ let typenames through...
    yylval.entry = pe;
    if (pe && pe->is_typename()) {
	    if (conversion_op) { // any typenames in a conversion op. shd NOT be TYPENAME_FUNCTION
		   conversion_op = false;
		   return TYPENAME;
        }
        //REF:TEMPLATE [
        if (pe->type == t_template_type) {
          TemplateInstance *ti = Parser::state.in_template;
          if (ti == NULL || input.look_ahead(true)=='<') {
             if (hunt_for_scope()) return TEMPLATE_NAME_EXPR;
             else return TEMPLATE_NAME;
          } else { // unqualified template name within a template, 
            // w/in template instantiation the qualification is _implied_
            if (ti->name() == pe->name) { // but only if this is the template class...
                pe = ti->entry();
                yylval.entry = pe;
            }   
          }
        }  //]

        // Because of the conflict between declarations and expressions, some nasty
		// lexical tie-ins take place here.
        bool parens_follows = input.look_ahead(true)=='(';
		bool scope_follows = input.next_two("::",false);
        if (parens_follows || scope_follows) {
		// is this the name of the current class?
          if(parens_follows && (Parser::is_class_entry(pe) || pe == class_entry )) {  
               yylval.classptr = pe->type.as_class();
			   class_entry = NULL;
               return THIS_CLASSNAME; 
          } else
		  if (tok==input.peek_next_token()) {
               c = yylex();   // shd be ::
			   Parser::state.class_dcl = pe->type;
			   class_entry = pe;
			   return yylex();
		  }
		  else
		  if (parens_follows) { // *hack 1.1.2 Function ptr declarations
             input.skip_whitespace();
			 if (input.next_two("(*",false)) return TYPENAME;
             // *hack 1.1.3 Pointer to method *EVEN MORE NASTY*
             if (strstr(input.current(),"::*")) return TYPENAME;
          } 
          return TYPENAME_FUNCTION;
        }
        else return TYPENAME; 
    }
    if (not_in_dcl && not_label) return IDEN; 
    else { // only emit TOKEN in declarative mode!
       strcpy(token_buff,tok.c_str());
       yylval.str = token_buff;
       return TOKEN;
    }
  } else
  if (c < T_LAST) { //...must be a CONSTANT of some sort
      if (c == T_END) {
// *hack 1.2.2b I'm often getting trouble shutting the session down
// when running a program in batch mode. 
//#ifdef _WCON
//          return 0;   // c'est fini!
//#else
         if (interactive_mode()) return 0; else throw 1;
//#endif
      }
      return do_constant(c);
  }
  if (c == ';') return c; 
  if (c == ',' && in_arg_list()) return c;
  ival = Operators::lookup(c,input);
  yylval.val = ival;
  return ival;
}

// this is exported (via directcall.cpp) to give UC programmers access to #commands
void uc_hash_cmd(char *s)
{
 char *cmd = strtok(s," ");
 char *rest = strtok(NULL,"");     // *fix 1.2.4 this fn wd fail if there were no args for the cmd! 
 if (rest) input.insert_string(rest);  // push the rest into the token string!
 input.user_cmd(cmd);
 //input.discard_line();
 input.insert_string(" ");          // *hack 1.2.4 discard_line() messes w/ UC's mind when executing code.
}

// from subst.cpp
void macro_substitute(TokenStream& tok,char *str, char *buff);


// *add 1.2.2 Can now do macro substitutions on arbitrary strings
void uc_macro_subst(const char *str, char *buff, int sz)
{
  // *NB* this routine can't check whether the buffer will overrun...
  macro_substitute(input,(char *)str,buff);
}

// two useful functions which can be used anywhere to tell where we are in the compilation...
int Input::lineno()
{ return input.lineno(); }

string Input::filename()
{ return input.file(); }

bool Input::open(const char *fname)
{
// *fix 1.0.1 Tokenizer::open() throws an exception!
 try {
  return input.open(fname);
 } catch(...) {
	return false;
 }
}

void Input::clear()
{
  input.clear();   // clear the file stack
}

void Input::insert_stream(istream *pos, const char *name, int start_line)
{
 input.insert_stream(pos,name,start_line);
}

// *add 1.2.7 Used to insert stuff into the input stream
// It suffers from the limitations of TokenStream::insert_string(), i.e. it destroys existing buffer!
void Input::insert_string(char* str)
{
 input.insert_string(str);
}

void Input::grab_next_line(char *buff)
{ 
// *I am quite dubious about this*
//  strcpy(buff, input.get_upto(0,false));
//  input.fetch_line();
  input.grab_line(buff);
}

char *Input::next_token(bool first) 
{
  // *fix 1.1.4L Watch out for whitespace and (NB) ignore \r!
    return strtok(first ? input.get_upto(0,false) : NULL," \r\t");
}

void Input::set_open_restore(INTFN module_open, INTFN module_close)
{
    input.set_open_op((RESTOREFN)module_open);
    input.set_restore_op((RESTOREFN)module_close);
}

PEntry Input::lookup_next_symbol()
{
 Errors::reset_error_state();
 int tok = yylex();
 if (tok == TYPENAME_FUNCTION && input.next_two("::")) {
    if (!Parser::state.begin_scope(yylval.entry->type)) return NULL;
    tok = yylex();
    Parser::state.pop_context();
 }
 if (check_error()) return NULL;
 if (tok != IDEN && tok != TYPENAME) return NULL;
 return yylval.entry;
}







