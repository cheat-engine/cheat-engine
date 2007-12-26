/* uc_tokens.cpp
 * Specialized tokenizer for C++ lexical scanner
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 *
 * UCTokenStream specializes the general C tokenizer TokenStream
 * by overriding things like macro handling and the interactive prompt.
 * The user_cmd() virtual method is overriden in main.cpp
 */

#include <stdlib.h>
#include <time.h>
#include "common.h"
#include "module.h"
#include "uc_tokens.h"
#include "version.h"
#include "errors.h"

// *fix 1.2.7 full strftime() flags not supported with MSC++ 6.0
#ifndef _MSC_VER
#define DATE_FORMAT "%b %e %G"
#define TIME_FORMAT "%T"
#else
#define DATE_FORMAT "%x"
#define TIME_FORMAT "%X"
#endif

bool check_error();  // in parser.y
bool interactive_mode(); // in main.cpp

enum {BM_LINE=1, BM_FILE, BM_UNDERC, BM_NOVALUE, BM_DATE, BM_TIME, BM_EMPTY};

void add_builtin_macro(const char* name)
{
    TokenStream::macro_builtin(name,BM_NOVALUE);
}

void UCTokenStream::init()
{
  macro_builtin("__LINE__",BM_LINE);
  macro_builtin("__FILE__",BM_FILE);
  macro_builtin("__DATE__",BM_DATE);
  macro_builtin("__TIME__",BM_TIME);
  macro_builtin("__UNDERC__",BM_UNDERC);
  macro_builtin("__cplusplus",BM_NOVALUE);
#ifdef _CONSOLE
  macro_builtin("_CONSOLE",BM_NOVALUE);
#endif
#ifdef _USRDLL
  macro_builtin("_USRDLL",BM_NOVALUE);
#endif
#ifdef __unix__
# ifdef __linux__
   macro_builtin("__linux__",BM_NOVALUE);
# endif
# ifdef __FreeBSD__
   macro_builtin("__FreeBSD__",BM_NOVALUE);
# endif   
  macro_builtin("__unix__",BM_NOVALUE);
#else
# ifdef __BEOS__
  macro_builtin("__beos__",BM_NOVALUE);
  macro_builtin("__BEOS__",BM_NOVALUE); /* that's the official one */
# else
   macro_builtin("__win32__",BM_NOVALUE);
# endif
#endif

#ifdef _MSC_VER
  macro_builtin("__MS_BUILT",BM_NOVALUE);
#endif

  if (Parser::debug.range_check)
      macro_builtin("_RANGE_CHECK",BM_NOVALUE);

// *hack 1.0.0 Until these keywords are implemented...
  macro_builtin("mutable", BM_EMPTY);
  macro_builtin("inline",  BM_EMPTY);
  macro_builtin("typename",BM_EMPTY);
  macro_builtin("register",BM_EMPTY);
  macro_builtin("volatile",BM_EMPTY);
}

void UCTokenStream::handle_builtin_macro(char *tbuff, int id)
{
    time_t t;
    char buff[120];
    switch(id) {
    case BM_LINE:
      itoa(lineno(),tbuff,10);
      break;
    case BM_FILE:
      quote_str(tbuff,file().c_str());
      break;
    case BM_DATE: // *fix 1.2.6 same format as CPP
      time(&t);      
      strftime(buff,sizeof(buff),DATE_FORMAT,localtime(&t));      
      quote_str(tbuff,buff);
      break; 
    case BM_TIME:
      time(&t);      
      strftime(buff,sizeof(buff),TIME_FORMAT,localtime(&t));      
      quote_str(tbuff,buff);
      break;
    case BM_EMPTY:
      strcpy(tbuff," ");
      break;
    case BM_UNDERC:
      quote_str(tbuff,mUCVersion); 
      break;
    case BM_NOVALUE:
      strcpy(tbuff,"1");
      break;
   }
}

void UCTokenStream::on_add_macro(char *name, PMEntry pme)
{
 Module::current()->add_macro(name);
}


// used to explicitly flag the statement/command as finished and not 'hanging'
// so we can change the prompt (*add 1.1.0)
bool s_hanging_statement;

void next_statement()
{
  s_hanging_statement = false;
}

int tstack_depth(); // *DEBUG*

void UCTokenStream::do_prompt()
{
 Parser::state.context().add_line_no(file(),lineno());
 if(is_interactive()) {
 #ifdef _USE_READLINE   
    ostrstream out (get_prompt_buffer(),MAX_PROMPT_SIZE);
 #else
    ostream& out = cout;
 #endif   
  int bdepth = Parser::block_depth();
  out << ';'; 
  if (is_in_comment()) out << "*/";
#ifdef _DEBUG
  else if (Parser::is_in_declaration()) out << "DCL> "; 
#endif
  else if (bdepth > 0) out << bdepth << '}';
  else if (s_hanging_statement) out << ';'; 
  else { out << '>'; s_hanging_statement = true; }
  out << ' ';
#ifdef _USE_READLINE
  out << ends;
#else  
  cout.flush(); // iostream.h requires this!
#endif  
 }
}

void UCTokenStream::on_hash_cmd()
{
  next_statement();
}

// *add 1.2.5 support for #if expressions
int _uc_exec(char* s, void* cntxt, char* filename, int line); // in main.cpp
static int sResult;

static void eval_expr(PExpr e)
{
    sResult = Parser::const_int_expr(e);
}

int  UCTokenStream::eval_const_expr(const char* expr)
{
    char buff[160];
    strcpy(buff,expr);
    strcat(buff,";");
    sResult = 1;
    Parser::set_expression_handler(eval_expr);
    int ret = _uc_exec(buff,0,0,0);
    Parser::set_expression_handler(NULL);
    if (ret != 0) {
        cerr << "Can't evaluate " << expr << " in #if" << endl;
        return 1;
    } else 
    return sResult;
}

static std::list<string> s_file_targets;
static std::list<RESTOREFN> s_restore_fn;

void UCTokenStream::set_restore_op(RESTOREFN fn) { 
    s_file_targets.push_back(m_original_file);
	s_restore_fn.push_back(m_restore);
    m_restore = fn;
    m_original_file = file();
}

void UCTokenStream::set_open_op(RESTOREFN fn) {
    m_openfn = fn;
}


void UCTokenStream::on_open()
{
 Module::set_current(file(),true);
 if (m_openfn) {
   (*m_openfn)();
   m_openfn = NULL;
 } 
}

void UCTokenStream::on_restore()
{
 Module::set_current(file(),false);
 if (m_restore && file() == m_original_file) {
    (*m_restore)();
    Errors::check_output();
	m_original_file = s_file_targets.back();
	m_restore = s_restore_fn.back();
	s_file_targets.pop_back();
	s_restore_fn.pop_back();
 }
}

// *add 1.2.9 the module traceback list
struct FileRef {
   string filename;
   int lineno;
};

typedef std::list<FileRef> FileRefList;
static FileRefList mModuleTraceList;

void dump_module_traceback()
{
    while (mModuleTraceList.size() > 0) {
        FileRef& fr = mModuleTraceList.front();
        cmsg << fr.filename << ' ' << fr.lineno << ": (included from)" << endl;
        mModuleTraceList.pop_front();
    }
}

void UCTokenStream::on_clear(const string& filename, int line)
{
 if (! Parser::debug.interactive_debugging)
   cerr << "included from " << filename << ' ' << line << endl;
 else
 {
   FileRef fr;
   fr.filename = filename;
   fr.lineno = line;
   mModuleTraceList.push_back(fr);
 }
 Module* pm = Module::from_name(filename);
 if (pm) pm->clean_macros_and_typedefs();
}

void UCTokenStream::on_error(const char *msg, bool is_error)
{
 if (is_error) { 
     error(msg);
     check_error();
     throw string(msg);
 }
 else  warning(msg);
}

bool UCTokenStream::next_two(char *ts, bool skip)
{
  if (look_ahead()==ts[0] && peek_ahead(1)==ts[1]) {
            if (skip) { next(); next(); }
            return true;
  } else return false;
}

bool UCTokenStream::is_interactive_mode()
{
	return interactive_mode();
}
