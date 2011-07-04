/*
December 1 2007:
Dark Byte is continuing work on this project.
const int EXPR_BUFF_SIZE = 1024; ... luckely not for exec which ce uses else it'd be a headshot to the original writer



*/

#include "common.h"
#include "engine.h"
#include "main.h"
#include "errors.h"
#include "directcall.h"
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include "module.h"
#include "input.h"

#define WITHIN_UC 1
#include "ucri.h"

#ifdef _WCON
#include "wcon.h"
#include "threads.h"
#endif


// from main.cpp
int uc_eval(char *expr, bool append_semicolon=true, bool synchronous=false, char *name=NULL, int lineno=0);
int ext_uc_eval(char *expr, char *output, int sz);
int redirected_eval(char *buffer,bool semicolon, char *name=NULL, int lineno=0);

const int EXPR_BUFF_SIZE = 1024;

#ifdef _WCON
int __stdcall WinMain (void *hInstance, void *hPrevInstance, char *lpszCmdLine, int nCmdShow);

class ConsoleThread: public Thread {
public:

  virtual int execute()
  {
    Thread::local(0);
    WinMain(NULL,NULL,"",1);
	Main::banner();
    Input::open("CON");
	Main::interactive_loop();
	return 0;
  }
};

static ConsoleThread *sConThread;
#endif


void set_main_thread(unsigned long id);

CEXPORT void XAPI uc_interactive_loop()
{
#ifdef _WCON
   Thread::local(1);   
   sConThread = new ConsoleThread();
   sConThread->resume();
   set_main_thread((long)sConThread->handle());
#endif
}

void wcon_set_parent(void *defs_file);

CEXPORT int XAPI uc_init(char *defs_file, bool use_defs)
{
// *fix 1.2.2b Change in signature of process_command_line()
    int argc = 1;
    char **argv = NULL;
    int ret = Main::process_command_line(argc,argv);

#ifdef _WCON
	wcon_set_parent((void *)defs_file);
	uc_interactive_loop();
#endif

	Main::initialize();

    // *add 1.1.3 Make sure std exists for UCRI and initialize it
    Parser::state.add_namespace("std");
    Parser::state.pop_context();
   	uc_ucri_init();

    if (use_defs) {
#ifdef _WCON
        sConThread->suspend();
#endif	  
      redirected_eval("#include <classlib.h>",false);
      redirected_eval("using namespace std;",false);
#ifdef _WCON	  
      sConThread->resume();
#endif      
    } 

	return ret;
}

int pgm_main(int argc,char **argv);

CEXPORT int XAPI uc_main(int argc, char **argv)
{
  return pgm_main(argc,argv);
}

CEXPORT void XAPI uc_finis()
{
	Main::finalize();
}


CEXPORT void *XAPI uc_main_window()
{
#ifdef _WCON
 return wcon_window_handle();
#else
 return NULL;
#endif
}
	
// UC_exec() can be passed _any_ UC command or
// expression
CEXPORT int XAPI uc_exec(char* buffer)
{
    int ret = redirected_eval(buffer,false);
    return ret == OK;
}

CEXPORT int XAPI uc_include(char *path)
{
 char buff[128];
 sprintf(buff,"#include \"%s\"",path);
 return uc_exec(buff);
}

void strip_n_copy(char *dest, char *src, int sz)
{
	int len = strlen(src);
	if (src[len-1]=='\n') src[len-1] = '\0';
	strncpy(dest,src,sz);
}

CEXPORT void XAPI uc_result(char *buffer, int sz)
{
	char *out = Errors::get_redirect_buffer(1);
    strip_n_copy(buffer,out,sz);
}

CEXPORT void XAPI uc_error(char *buffer, int sz)
{
	char *out = Errors::get_redirect_buffer(2);
	strip_n_copy(buffer,out,sz);
}

CEXPORT int XAPI uc_error_pos(char *filename)
{
 return	Errors::get_stop_position(filename);
}

// *fix 1.2.4 Extra leading space and quotes around strings stripped out
CEXPORT int XAPI uc_eval(char *expr, char *res, int sz)
{
    char buff[EXPR_BUFF_SIZE];
	int iret = ext_uc_eval(expr,res,sz) != FAIL;
    if (*res=='(') {  // *add 1.1.4 strip out type!
       char *p = res;
       while(*p && *p != ')') p++;
	   p++;
       // skip leading space, if found
       if (*p == ' ') p++;
       char* eql = strchr(p,'=');
       if (eql) p = eql+2;
	   strcpy(buff,p);
	   strcpy(res,buff);
    }
	// *fix 1.1.4 nasty extra line feed!
	int len = strlen(res);
    if (res[len-1]=='\n') { res[len-1] = '\0'; len--; }    

    // removed quoted strings...
    if (*res == '\'' || *res == '\"') {
        res[len-1] = '\0';
        strcpy(buff,res+1);
        strcpy(res,buff);
    }
	return iret;
}

CEXPORT void XAPI uc_set_quote(char *var, char *val)
{
 char buff[EXPR_BUFF_SIZE];
 sprintf(buff,"%s = \"%s\";",var,val);
 uc_exec(buff);
}

CEXPORT void XAPI uc_init_ref(char *type, char *var, void *addr)
{
 char buff[EXPR_BUFF_SIZE];
 sprintf(buff,"%s& %s = *(%s *)0x%X;",type,var,type,addr);
 uc_exec(buff);
}

//1.12.ce:
//Might speed up with precompiling functions 
CEXPORT void * XAPI uc_get_function(const char *functionname)
{
	return Function::lookup(functionname);
}

static Function * XAPI make_function(const char *args, const char *expr)
{
 static int mFn = 1;
 char buff[EXPR_BUFF_SIZE];
 char fn_name[10];
 sprintf(fn_name,"__T%03d",++mFn);
 sprintf(buff,"__declare %s(%s) { return %s; }",fn_name,args,expr);
 if (uc_exec(buff)) {  // function defined successfully!
	 return Function::lookup(fn_name);
 } else return NULL;
}

CEXPORT void * XAPI uc_compile(const char *args, const char *expr)
{
  return make_function(args,expr);
}

// *change 1.1.3  uc_eval_exp() is implemented by uc_eval_method() in UCRI
CEXPORT int XAPI uc_eval_exp(void *sc, void *arguments, void *result)
{
 return uc_eval_method(sc,NULL,arguments,result);
}

CEXPORT int uc_eval_args(void *sc, void *result, ...)
{
 int res;
 va_list arguments;
 va_start(arguments,result);
 res = uc_eval_exp(sc,arguments,result);
 va_end(arguments);
 return res;
}

// *add 1.1.4 convenient way to use uc_eval_method()
CEXPORT int uc_eval_method_args(void *sc, void *obj, void *result, ...)
{
 int res;
 va_list arguments;
 va_start(arguments,result);
 res = uc_eval_method(sc,obj,arguments,result);
 va_end(arguments);
 return res;
}


// e.g. UC_compile_fn("double x","sin(x)/x");
CEXPORT void * XAPI uc_compile_fn(char *parm, char *expr)
{
  Function *fun_ptr = make_function(parm,expr);
  return Builtin::generate_native_stub(fun_ptr);
}

// *add 1.1.4 
CEXPORT int  XAPI uc_load(char *path)
{
 char buff[EXPR_BUFF_SIZE];
 sprintf(buff,"#l %s",path);
 return uc_exec(buff);
}

CEXPORT int  XAPI uc_run()
{
  return uc_exec("#r"); 
}

CEXPORT int XAPI uc_import(char *dcl, void *fn)
{
    int res;
    Builtin::set_direct_import(fn);
    try {
       res = redirected_eval(dcl,true);
    } catch(...) {
       res = FAIL;
    }
    Builtin::set_direct_import(0);
    return res == OK;
}


