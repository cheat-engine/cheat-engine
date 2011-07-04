/* program.cpp
 * Running a Program. Manages separate program threads (only for GUI version currently)
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 */

#include "common.h"
#include "module.h"
#include "threads.h"
#include "program.h"
#include "errors.h"
#include "utils.h"
#include "loaded_module_list.h"
#include "engine.h"

// from main.cpp
void get_function_module(Function *pf, char *pathname);
char *uc_get_title();

// from engine
void  exec_message(char *msg, bool was_error, bool dont_unwind=false);
void reset_stacks();

// interface to temporary code generation in common.cpp
// *change 1.1.2 These guys are now generally available as short-cuts.
int Program::run_int_function(int flags, int& retval)
{
 ArgBlock args;
 args.no = 0;
 flags += Engine::RETURN_32 + Engine::ARGS_PASSED;
 int retcode = Parser::exec_temp_code(flags,&args);
 retval = args.ret1;
 return retcode;
}

// *change 1.2.8 We're now explicitly calling initialization code before main()
static int run_main_function(int flags, int& retval)
{
  Parser::init_lib();
  LoadedModuleList::run_program_initialization();
  int rt = Program::run_int_function(flags,retval);
  LoadedModuleList::run_program_finalization();
  return rt;
}

void Program::compile_function(void *e)
{
 Parser::code().compile((PExpr)e);
 Parser::finalize_temp_code(false, t_int);
}


#ifdef _WCON
// Currently only the GUI build supports a separate program thread

#include "ide.h"

// from wcon
void wcon_bring_to_front();
void* wcon_handle();

// defined at end of this module
void main_state(int state);
int get_main_state();
Event s_start_event, s_debug_event;

class ExecThread: public Thread {
private:
  char *m_title;
  int m_id;
public:
  ExecThread(char *title) : m_title(strdup(title)) {}

  bool finished() { return m_title == NULL; }
  int  console_id()       { return m_id; }

  void kill_console_properly() {
    Lock kill_console;  // the lock shd go in wcon, methinks...
    wcon_destroy(m_id);
  }

  void kill()
  {
    kill_console_properly();
    Thread::kill();
  }


  int execute() 
  {
    char temp[40];
    s_start_event.reset();
    Thread::local(0); // mark this thread as being the program thread
    // Create a separate console window for I/O
	//wcon_bring_to_front();                 // bring program window to front... 
    m_id = wcon_create(1000,80,true);   // w/ 1000 lines buffered; create defered
    wcon_set_size(20,350,400,200);
    wcon_set_title(m_title);
	wcon_bring_to_front();                 // bring program window to front... 
	IDE::bring_to_front(wcon_handle());
    start_time();

    // and go...
    // *add 0.9.8 synchronization with remote debugger
    int flags = 0, retval;
    s_start_event.set();
    main_state(RUNNING);    
    IDE::message();  // *add 1.2.8 tell the IDE that we've begun execution
    while (run_main_function(flags,retval)==HALTED) {
      if (Errors::check_output()) IDE::breakpoint();     // Inform the IDE, if it's a remote command
      main_state(HALTED);
      wcon_pause();                          // pause secondary window until freed 
      main_state(RUNNING);              
      IDE::message();
      wcon_bring_to_front();                 // bring program window to front... 
      flags = Engine::RESUME;                // keep looping and resume execution 
    }   
	Engine::reset_single_stepping();         // *fix 1.2.8
  
    // wait for return...
	// *add 0.9.5 close pgm console if no output
    if (wcon_lines() > 0) {  // there was output...
      char buff[256];
      long et = elapsed_time(); // in millisec
      strcpy(buff,m_title);
      if (et > 100) { // only bother to give time if it took more than 100 ms        
        sprintf(temp,"(%d msec)",et);
        strcat(buff,temp);
      }
      main_state(FINISHED);
      // *add 1.2.8 put return value on caption if being interactively debugged...
      if (Parser::debug.interactive_debugging) {
          sprintf(temp, " return %d ", retval);
          strcat(buff,temp);
      }
      strcat(buff," [Press Any Key]");
      wcon_set_title(buff);
      wcon_getch();
    }
    kill_console_properly();
    m_title = NULL;
    main_state(OK);
	// *fix 1.1.0 Actual return code from main()
    if (! Parser::debug.interactive_debugging) 
      cmsg << "Program returned " << retval << endl;
    // *add 0.9.8 Program console error messages can be redirected
    //if (Errors::check_output())
		IDE::finished();  // Inform the IDE...
    return 0;
  }

  void cleanup() { delete this; }
};

static ExecThread *pet = NULL;

bool Program::in_main_thread()
{ 
    return Thread::local();
}

void Program::pause_thread()
{
   s_debug_event.reset();
   s_debug_event.wait();
}

bool Program::thread_is_paused()
{
    return ! s_debug_event.is_set();
}

void Program::release_thread()
{
    s_debug_event.set();
}

void  Program::stop_main(bool is_error)
{
//*fix 1.2.8 Always clean out function stack and restore context when stopping,
// even if the program window isn't up anymore.
  if (pet != NULL && ! pet->finished()) {
    pet->kill();
  }
  if (is_error) exec_message("Stopped",true);  
  main_state(OK);
  Parser::restore_global_context(); // see below...
  // *fix 1.2.8 this ensures that the engine is no longer in 'paused' state.
  reset_stacks();
  // *add 1.2.8 Inform the IDE and reset any single-stepping state
  // *issue: this will also stop any tracing operations.
  IDE::finished(); 
  Engine::reset_single_stepping();         
}
#else

bool Program::in_main_thread()
{ return true; }

void Program::pause_thread()
{ }

#endif

int  Program::call_main(int argc, char **argv,Program::RunType how)
{
 using namespace Expressions;
 int retval = 0; // meaning success!

 // *fix 1.1.4 We push local context while debugging; make sure it's popped before restarting...
 Parser::restore_global_context();

 Type t_char_ptr_ptr = t_char_ptr;
 t_char_ptr_ptr.incr_pointer();
 PExpr e1 = constant_op(t_int,argc);
 PExpr e2 = constant_op(t_char_ptr_ptr,(long)argv);
 PExprList pel = expr_list(e1,e2);
 Function *fn = Function::lookup("main");
 if (!fn) return -2;
 int nargs = fn->signature()->size();
 if (nargs==0) pel = NULL;
 else if (nargs!=2) { error("Wrong no of args for main()"); return 2; }
 compile_function(new Expr(FUNCTION,t_int,fn,pel)); 
#ifdef _WCON
 // *add 1.2.2a optionally run program in same thread as console
 if (how != same_window) {
 // *add 0.9.4 title of new program console is full path of main module
   char pathname[255];
   get_function_module(fn,pathname);
 // exec main program in its own thread...
   Thread::local(1);  // this is the default value anyhow....
   pet = new ExecThread(pathname);
   if (how == new_thread) {
     pet->resume();
     s_start_event.reset();
     s_start_event.wait();
   } else // new_window
   pet->execute();
 } else
 run_main_function(0,retval);
#else
 run_main_function(0,retval); // *fix 1.1.0 Actual return code from main()  
#endif
 return retval;
}

static int mMainState = OK;

void main_state(int state)
{
 mMainState = state;  
#ifdef _WCON
 char *msg;
 switch(state) {
 case OK:       msg = "";            break;
 case HALTED:   msg = " [break]";    break;
 case RUNNING:  msg = " [running]";  break;
 case CRASHED:  msg = " [crashed]";  break;
 case FINISHED: msg = " [finished]"; break;
 default:       msg = " [unknown]";  break;
 }
 char buff[256];
 strcpy(buff,uc_get_title());
 strcat(buff,msg);
 wcon_set_title(buff,true);
#endif
}

int get_main_state()
{ return mMainState; }

void get_function_module(Function *pf, char *pathname)
{
  int imod = pf->line_nos()->module();
  if (Module::from_id(imod) != NULL)
    strcpy(pathname, Module::from_id(imod)->name().c_str());
  else *pathname = '\0';
}

bool Program::run(char *cmdline, bool same_thread) 
{
#ifdef _WCON
    if (get_main_state()==FINISHED) Program::stop_main(false);
#endif
    Errors::reset_error_state();
    if (Engine::paused()) {  // we were at a breakpoint, so resume execution
#ifdef _WCON
      if (wcon_paused()) {
         wcon_resume();
      } else {
// *fix 1.2.8 tell the IDE that we've resumed; if the program thread was
// paused, we have to let it go again....
        IDE::message();  
        if (thread_is_paused()) 
           release_thread();
        else           
          Engine::execute(NULL,Engine::RESUME);        
      }
#else
      Engine::execute(NULL,Engine::RESUME);
#endif
      return true;
    } else  { // normal case ... there may be arguments
      int argc; 
      static char *argv[20];
      static char pathname[127];
      char *tok;
      Function *fn = Function::lookup("main");
      if (fn == NULL) return false;
      // *add 0.9.4 argv[0] is now full path of module containing main()
      get_function_module(fn,pathname);
      argc = 0;   argv[argc++] = strdup(pathname);
      tok = Utils::quote_strtok(cmdline);
      while (tok != NULL) { 
          argv[argc++] = strdup(tok);
           tok = Utils::quote_strtok(NULL);
      }	 
      int retval = call_main(argc,argv,same_thread ? same_window : new_thread);      
	  if (retval == -2) return false;
#ifdef _CONSOLE
	  cout << "Program returned " << retval << endl;
#endif
	  return true;
    }
    return true;
 }


struct ODL_entry {
    void* addr;
    FBlock* fb;
};
typedef std::list<ODL_entry> ODLEntryList;

struct MEntry {
    int idx;
    FBlock* fb;
    ODLEntryList* odl;
};
typedef std::list<MEntry> MEntryList;

static ODLEntryList *s_module_odl = NULL;
static ODLEntryList s_global_odl;
static MEntryList s_module_list;
static int s_mod_idx;

static void exec_dtor_list(ODLEntryList* odl)
{
    ODLEntryList::iterator oeli;
    for(oeli = odl->begin(); oeli != odl->end(); ++oeli) {
       Engine::object_ptr(oeli->addr);
       Engine::execute(oeli->fb);
    }
}

namespace LoadedModuleList {

void init()
{
    s_module_list.clear();
}

void init_module(int idx)
{
  s_mod_idx = idx;
  if (idx != -1) 
    s_module_odl = new ODLEntryList();
  else
    s_module_odl = NULL;  // this flags us as NOT compiling program code...
}

void finish_module(Instruction* pi)
{
  FunctionEntry* pfe;
  Function* fn;
  PEntry pe;

  if (pi) {
      // we need the file part of the path, without the extension...
      string name = Module::from_id(s_mod_idx)->name();
      name = Utils::get_filepart(name,true);
      // shd really change _all_ exotic characters to the usu C identifier chars...
      for(int i = 0; i < name.length(); i++)
          if (name[i] == '-') name[i] = '_';
      name = "__" + name + "_init_";
      // is the module init function already defined?
      pe = Parser::global().lookup(name);
      if (pe && pe->type.is_function()) {
          pfe = (FunctionEntry*)pe->data;
          fn = pfe->back();
          FBlock* fb = fn->fun_block();
          delete fb->pstart;
          fb->pstart = pi;
          // ok, let's go look for this in the loaded module list (module id is unique)
          MEntryList::iterator meli;
          for(meli = s_module_list.begin(); meli != s_module_list.end(); ++meli) {
              MEntry& me = *meli;
              if (me.idx == s_mod_idx) {
                  delete me.odl;
                  me.odl = s_module_odl;
              }
          }
      } else { 
          Signature* sig = new Signature(); // this defaults to void f(void)....
          pfe = Parser::create_function_entry(sig,name,NULL);
          fn = new Function(sig,pfe);
          pfe->push_back(fn);
          MEntry mod_entry;
          mod_entry.idx = s_mod_idx;
          mod_entry.fb = fn->fun_block();      
          mod_entry.fb->pstart = pi;
          mod_entry.fb->nlocal = 200; //*HACK*
          mod_entry.odl = s_module_odl;
          s_module_list.push_back(mod_entry);
      } 
  }
  s_module_odl = NULL;
}

void ODL_add(void* ptr, Function* dtor)
{
    ODL_entry entry;
    entry.addr = ptr;
    entry.fb = dtor->fun_block();
    if (s_module_odl)
        s_module_odl->push_front(entry);
    else
        s_global_odl.push_front(entry);
}

// before main() is called, we initialize all the load modules by calling
// their XXX_init_ functions
void run_program_initialization()
{
    MEntryList::iterator meli;
    for(meli = s_module_list.begin(); meli != s_module_list.end(); ++meli) {     
       Engine::execute(meli->fb);
    }
}

// at the end of main(), we need to call the destructors of all objects
// defined in 'program scope'.
void run_program_finalization()
{
    MEntryList::reverse_iterator meli;
    for(meli = s_module_list.rbegin(); meli != s_module_list.rend(); ++meli) {
       exec_dtor_list(meli->odl);
    }
}

// this is called at the end of the session.
void run_global_finalization()
{
  exec_dtor_list(&s_global_odl);
}

};



