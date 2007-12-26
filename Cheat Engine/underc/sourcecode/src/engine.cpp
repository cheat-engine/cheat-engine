/* ENGINE.CPP
 * The stackcode engine
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 */
#include "classlib.h"
#include <list>
#include <memory.h>
#include "stack.h"
#include "opcodes.h"
#include "directcall.h"
#include "hard_except.h"
#include "breakpoints.h"
#include "common.h"
#include "errors.h"
#include "program.h"
#include "loaded_module_list.h"

int lo_byte(int data)
{
   return data & 0xFF;
}

int hi_byte(int data)
{
	return (data & 0xFF00) >> 8;
}

// *change 1.2.2 Switch off ODS debugging by default in debug mode
#ifdef _DEBUG
//#define DEBUG_ODS
//#define TRACK_ODS
extern int gDebugBreak;
extern FBlock* gFunBlock;
void __break(int);
 void check_fun_block(FBlock* fb)
 {
  if (gFunBlock == fb)
      __break(3);
 }
#else
#define check_fun_block(x)
#endif

// *change 1.2.3 We now by default put all global objects requiring destruction
// onto a static ODL, which is cleared at the end of the session.
// Uncomment the next line if you don't want this!
// #define NO_STATIC_ODS

// from common.cpp
void unpack_bitfield(int data, int& offs, int& bit_offs, int& bit_size);

CEXPORT char *sh_fun();  // debug function

// later in this file....
int current_ip();  
void  exec_message(char *msg, bool was_error, bool dont_unwind = false);
bool get_curent_exec_pos(int& ip_offs, string& fname,  LineInfo& li);
int get_current_exec_line();

namespace Builtin {
  PClass imported_class_from_function(void *pfn);
}

// useful macro for tracing...
#define OUT(v) cout << #v " = " << v << endl

const int ODS_STACKSIZE = 200, ODS_TEMP_BUFFSIZE = 400;
const int MAX_CODE_LEVELS = 2;
const int MAX_FUNCTION_DEPTH = 8000;
const int EXEC_STACKSIZE = 100000, OBJECT_STACKSIZE = 9000;
#define START_FB (FBlock *)0x0a

void
CodeGenerator::out(Instruction *is)
{
 *pcode = *is;
 pcode++;
 NC++;
 mTotal++;
}

void
CodeGenerator::emitc(int opc, RMode rm, int data)
//------------------------------------------
{
 pcode->opcode = opc;
 pcode->rmode  = rm;
 pcode->data   = data;
 pcode++;
 NC++;
 mTotal++;
}

int
CodeGenerator::total_instructions()
{ return mTotal; }

int
CodeGenerator::ip_offset()
{ return NC; }   // *because it counts instructions!*

Instruction *
CodeGenerator::current_pi()
{ return pcode; }


Instruction *
CodeGenerator::last_pi()
{ return pcode-1; }

void
CodeGenerator::begin_code()
{
 NC = 0;
}

void
CodeGenerator::backspace()
{
 pcode--;
 NC--;
 mTotal--;
}


Instruction *
CodeGenerator::end_code()
//---------------------
{
 if (NC) {
   pcode->opcode = 0; NC++; // flag end-of-code!
   Instruction *pi = new Instruction[NC];
   memcpy(pi,_code_buff_,sizeof(Instruction)*NC);
   pcode = _code_buff_;
   NC = 0;
   mTotal = 0;  // *fix 1.1.2 Was not reset
   return pi;
 } 
 else return NULL;
}

// *add 1.2.3a Returns non-NULL ptr to instruction if this function
// has exactly one instruction
Instruction* 
CodeGenerator::has_one_instruction(Function* pf)
{
  Instruction* ip = pf->fun_block()->pstart;
  // look past the RETx instruction to see the end-of-code marker
  if (ip && (ip+2)->opcode == 0) return ip;
  else return NULL;
}

Instruction *
CodeGenerator::instruction_with_pointer(int opcode, void *ptr)
{
    typedef void *pvoid;
    Instruction *pi = new Instruction();
    pi->opcode = opcode;
    pi->rmode = DIRECT;
    pi->data = Parser::global().alloc(sizeof(pvoid),NULL);
    *(pvoid *)Parser::global().addr(pi->data) = ptr;
    return pi;
}



Instruction *
x_copy_code(Instruction *iptr)
{
 Instruction *pi = iptr;
 while (pi->opcode != 0) pi++;
 int sz = ((int)pi - (int)iptr)/sizeof(Instruction);
 pi = new Instruction[sz];
 memcpy(pi,iptr,sizeof(Instruction)*sz);
 return pi;
}


const int FUNCTION_STACK_DEPTH = 3*MAX_FUNCTION_DEPTH;
static Stack<void *,FUNCTION_STACK_DEPTH> fs;
char *mDataSeg;
static char *mDataPtr = mDataSeg;

void Engine::set_data_seg(void *ptr)
{ mDataSeg = (char *)ptr; }

// for reasons of efficiency, the execution stack
// is statically allocated.  Fine if thread-safe isn't
// a piority.
// (note: I know this should be in an anonymous namespace,
// but the Visual C++ debugger can't see them then...until
// then!)
static bool ptr_check;
static int _stack_[EXEC_STACKSIZE];
static int *mSP = _stack_ + EXEC_STACKSIZE - 1;
static int *mEndStack = _stack_;

void reset_execution_stack()
{ // currently only called after stack overflow....
  mSP = _stack_ + EXEC_STACKSIZE - 1;
}

// double-word stack operations

inline void push(int val) {
    *(--mSP) = val;
}

inline int  pop() {
    return *(mSP++);
}

int popp() { // specialized pointer-checking pop!
  int p = pop();
  if (ptr_check) Builtin::bad_ptr_check((void *)p);
  return p;
}

inline int& tos() {
    return *mSP;
}

inline void drop() {
    ++mSP;
}

inline void pushf(float v) {
     *((float *)--mSP) = v;
}

inline float popf() { 
    return *((float *)mSP++);
}

// quadword stack operations

inline void push2(double fval) {
    mSP -= 2; *((double *)mSP) = fval; 
}

inline double pop2() {
	double tmp = *(double *)mSP; mSP += 2; return tmp;  
}

inline double& tos2() {
       return *(double *)mSP;
}

inline void   drop2() {
    mSP += 2;
}

int stack_depth() { return  (int)_stack_ - (int)mSP; }

void stack_report(ostream& os,int *base)
{
    os << "STACK: ";
    if (base == NULL) base = _stack_;
    if (base == mSP) { os << "(Empty)" << endl; return; }
    while (base != mSP-1) {
        os << *base++ << ' ';
    } 
    os << '(' << *base << ')' << endl;
}

int Engine::retval()
{  return tos(); }

//----------------------- object-stack ------------------------------------
static Stack<char *,OBJECT_STACKSIZE> ostack;
static char *mOP;

// these functions define the Virtual Method Table structure of UC

inline PFBlock virtual_method_lookup(int slot)
{
  return PFBlock( (*VMT(mOP))[slot] );
}

PFBlock virtual_method_imported_lookup(int slot)
{
// *add 1.2.0 Imported objects may have associated VMTs!
    return PFBlock( (Class::find_VMT(mOP))[slot] );
}

PClass get_class_object(void *p)
{
// *change 1.2.0 In general, dynamic casts need to look in the VMT map
// because objects may have been imported without a VMT in the usual position
    PPClass pc = Class::find_VMT(p);
    if (pc) return pc[0];
    else return (*VMT(p))[0];
}

PPClass& get_object_VMT(void* p)
{
   return *VMT(p);
}

inline void opop() 
{ 
  mOP = ostack.pop();
}

void chk_ods()
{
  if (ostack.depth() > OBJECT_STACKSIZE) 
       ostack.clear();

}

inline void opush(void *data)
{ 
    if (ptr_check) Builtin::bad_ptr_check(data);
    ostack.push(mOP);
    mOP = (char *)data;
}

inline char *otos()
{
  return ostack.TOS();
}

void Engine::object_ptr(void *data)
{
 mOP = (char *)data;
}

void *Engine::object_ptr()
{
 return mOP;
}

// potentially quite dubious - at the mo., only used so we can use JINC to move mOP
// for object array construction.
char **Engine::object_ptr_addr()
{
 return &mOP;
}

  
//----------------------- THE ACTUAL ENGINE ---------------------------------
Instruction *ppcode;
FBlock *fb;
int *baseSP;

struct ExecutionState {
  Instruction *m_ppcode;
  FBlock* m_fb;
  int *m_baseSP;
  bool m_cntxt_pushed;
  ArgBlock *m_xargs;  

  void save(Instruction *_ppcode, bool cpushed)
  { 
      m_ppcode = _ppcode;
      m_fb = fb; 
      m_baseSP = baseSP;
      m_cntxt_pushed = cpushed;
  }

  void restore(bool& cpushed, ArgBlock *&args)
  {
      ppcode = m_ppcode;
      fb = m_fb;
      baseSP = m_baseSP;
      cpushed = m_cntxt_pushed;
      args = m_xargs;
  }

};

ExecutionState resume_state;

const Instruction *end_of_code = (Instruction *)4;

//----------------------- object destructor stack ---------------------------
// *change 1.0.0 The ODS is now a stack of <class,object> records
// *change 1.2.3 The stack is now backed by a vector, so we don't get
// unpleasant suprises with large numbers of objects.

template <class T, int N>
class VStack {
private:
    std::vector<T> m_vec;
public:
    VStack()
    { m_vec.reserve(N); m_vec.resize(0);}

    void push(const T& val)
    {
      m_vec.push_back(val);
    }

    T pop()
    { 
      T val = m_vec.back();
      m_vec.pop_back();
      return val;
    }

    bool empty()
    { 
      return m_vec.size() = 0;
    }

    void clear()
    {
      m_vec.resize(0);
    }

    int depth()
    {
      return m_vec.size();
    }

    T& get(int i)
    { return m_vec[i]; }
};

struct ODS_Record {
  PClass m_class;
  void  *m_obj;
#ifdef DEBUG_ODS
  Function *m_fn;  //DEBUG fields
  int m_offs;
#endif
};

class ODStack: public VStack<ODS_Record,ODS_STACKSIZE>  // wuz Stack<...
{
private:
    ODS_Record orec;
public:

 void push_object(PClass pc, void* obj)
 {
    orec.m_class = pc;
    orec.m_obj = obj;
    #ifdef DEBUG_ODS
    if (fb == Parser::temp_fun_block()) orec.m_fn = NULL;
    else orec.m_fn = Function::from_fun_block(fb);
    orec.m_offs = current_ip();
    #endif
    push(orec);
 }

 void pop_object(PClass& pc, void*& obj)
 {
   orec = pop();
   pc = orec.m_class;
   obj = orec.m_obj;
 }

 void fetch_object(int idx, PClass& pc, void*& obj)
 {
   orec = get(idx);
   //orec = *ref_to_TOS(-idx);
   pc = orec.m_class;
   obj = orec.m_obj;
 }

 #ifdef DEBUG_ODS
 void current_context(PClass& pc, PFunction& fn, int& offs)
 { // assumes basically that either pop_object() or fetch_object() has been called...
    fn = orec.m_fn;
    offs = orec.m_offs;
	pc = orec.m_class;
 }
 #endif

};

static ODStack mODS;

void end_function();  // forward...

#ifdef DEBUG_ODS
void destruct_error(char *msg)
{
     PFunction fn;
     int ofs;
	 PClass pc;
     mODS.current_context(pc,fn,ofs);
     string name = fn ? fn->name() : string("<temp>");
	 string cname = pc ? pc->name() : string("<none>");
     cerr << "bad ODS entry: class " << cname << " fn " << name << '(' << ofs << ')' << endl;
	 exec_message(msg,true);
}
#else
void destruct_error(char *msg) { exec_message(msg,true); }
#endif

void do_unwind(void *p, ODStack& o_ds)
{
// unwind the Object Destruction Stack (ODS)
  FBlock * old_fb = fb;
  Instruction *old_ppcode = ppcode;
  int *old_baseSP = baseSP;
  int *old_mSP = mSP;
  PClass pc;

  // *fix 0.9.5 Before unwinding, we must check whether this object is in fact on the ODS
  if (p != NULL) {
    bool found = false;
    int n  = o_ds.depth();
    if (!n) return; // ODS empty....
    for(int i = 0; i < n; i++)
    {
      void* s_obj; 
      o_ds.fetch_object(i,pc,s_obj);
      if (s_obj == p) {
        found = true;
        break;
      }
    }
    if (! found)
      return;    
  }

  // the stack unwinding loop
  char *lastOP = mOP;
  void *obj;
  if (o_ds.depth() > 0)
  do {
   o_ds.pop_object(pc,obj);
   mOP = (char *)obj;    
   Function *dfn;
   try {
     dfn = pc->destructor();
   } catch(...) {
       destruct_error("bad ODS entry ");
       break;
   }
   if (dfn != NULL) try {
     if (Engine::execute(dfn->fun_block())==CRASHED) 
	      destruct_error("destructor failed");
   } catch(...) {
     destruct_error("bad destructor");
     return;
   }
  } while (obj != p && o_ds.depth() > 0);
  mOP = lastOP;    
  fb = old_fb;
 // check_fun_block(fb);
  ppcode = old_ppcode;
  baseSP = old_baseSP;
  mSP = old_mSP;
}

#define DEFER_MARKER ((PClass *)(0x777))

#ifdef DEBUG_ODS
void check_ODS()
{
 int n  = mODS.depth();
 if (!n) return;
 for(int i = 0; i < n; i++)
 {
   PClass pc;
   void *obj;
   PFunction fn;
   int ofs;
   
   mODS.fetch_object(i,pc,obj);
   bool fooked = false;
   if (pc->has_VMT()) try {
     PClass *vmt = *VMT(obj);
     if (vmt != DEFER_MARKER) fooked = pc != *vmt;	 
   }
   catch(...) { fooked = true; }
   if (fooked) {
       mODS.current_context(pc,fn,ofs);
       string name = fn ? fn->name() : string("<temp>");
       cerr << "bad ODS: class " << pc->name() << " fn "
	        << name << '(' << ofs << ')' << "ptr " << obj << endl;
       exec_message("bad ODS entry ",true);
     break;
   }  
 }
}
#define CHECK_ODS check_ODS()
#else
#define CHECK_ODS
#endif


int throw_exception(Type t, void *thrown_obj)
// returns a valid offset into the catching function if successful;
// -1 otherwise
{
 try {
 static CatchHandler *curr_except = NULL;
 if (t == t_void) {
  // we are re-raising this exception! (see Parser::do_throw())
   // we assume that the stack has already been unwound past the 
   // try block marker we hit last...
   t = curr_except->thrown_type();
   thrown_obj = curr_except->thrown_object();
 } else curr_except = NULL;
 int n  = mODS.depth();
 if (!n) return -1;
 for(int i = 0; i < n; i++)
 {
   PClass pc;
   void *obj;
   mODS.fetch_object(i,pc,obj);
   if (pc == Parser::try_block_class()) {
     // first field is our catch handler
     curr_except = (CatchHandler *) *(unsigned int *)obj;
     int ip = curr_except->match_thrown_object(t,thrown_obj);
     if (ip != -1) {
        do_unwind(obj,mODS);  // unwind up to (&including) the try block marker  
        while (fb != curr_except->fun_block()) end_function();
        return ip;
     }// if (...we matched a catch block...) 
   } // if (...we hit a try block marker ...)
 } // for(...all entries in ODS .....
 } catch(...) {
   cerr << "bomb out handling exception" << endl;
 }
 // *fix 0.9.5 Will attempt to unwind the stack if the exception is uncaught
 do_unwind(NULL,mODS);
 return -1; // nobody wanted to catch this type!!
}

void *ventry(int slot, int offs)
{
  return (*(void ***)(mOP+offs))[slot];
}


//--------------- THE STACK ENGINE --------------------------
#define REF(T,p)    (*(T *)(p))
#define REFS(T)     (*(T *)pop())
#define REFA(T,p,a) (*((T *)(p)+(a)))
#define REFP(T,p)   (*(T **)(p))
#define NEXT(p)     ((int *)p)+1


int switch_jump(int *sb, int val)
// Switch jump tables look like this:
//  (num. entries) (default jump) (1st val) (1st jmp) .....
{
  int sz = *sb++, def = *sb++;
  while (sz && *sb != val) { sb++; sb++; sz--; }
  if (sz)  return *(sb+1);
      else return def;
}

void Engine::kill(int retcode)
{ 
  ppcode = (Instruction *)end_of_code;
}

void reset_stacks()
{
 if (fs.depth() > 0) {
   cerr << "reseting stacks\n";
   reset_execution_stack();
   fs.clear();   
 }
 // *fix 1.2.8 reset_stacks() will ALWAYS clear out the resume state
 resume_state.m_fb = NULL;  // flag us as NOT paused    
}

// *change 1.2.4 separated out code for determining full function name
string fname_from_fblock(FBlock* fun_block)
{
   if (fun_block == Parser::temp_fun_block()) return "<temp>";
   return Function::from_fun_block(fun_block)->as_str();
}

// *change 1.2.3 Function tracing is now controlled by the XTrace object
// *add 1.2.4 Can switch off default exit behaviour
// *change 1.2.4 Now dumps out full function name when tracing
// *change 1.2.4 No return value for enter() and leave() methods
XTrace::XTrace(bool on_exit)
  : m_on_entry(true), m_on_exit(on_exit) { }

void XTrace::enter(XExecState* xs)
{
   cerr << "*ENTER " << fname_from_fblock(xs->fb) << endl;
}

void XTrace::leave(XExecState* xs)
{
   cerr << "*LEAVE " << fname_from_fblock(xs->fb) << endl;
}

void get_stack_frame(int fi, ExecutionState& xs);

XExecState* get_exec_state()
{
  static XExecState xs;
  ExecutionState exs;
  get_stack_frame(1,exs);
  xs.fb = fb;
  xs.ip = ppcode;
  xs.op = mOP;
  xs.sp = mSP;
  xs.bp = baseSP;
  xs.last_fb = exs.m_fb;
  xs.last_ip = exs.m_ppcode;
  xs.last_bp = exs.m_baseSP;
  return &xs;
}

// *add 1.2.4 Can switch tracing on & off globally
static bool gCanTrace = true;

void engine_set_tracing(bool yesno)
{ gCanTrace = yesno; }

void start_function(FBlock *_fb)
{
#ifdef _DEBUG
 if (gFunBlock == fb)
      __break(3);
#endif
  fs.push(ppcode);
  if (fs.depth() > FUNCTION_STACK_DEPTH) {
      reset_stacks();
      throw Exception("Function Stack Overflow");
  }
  fs.push(fb);
  ppcode = _fb->pstart;
  fb = _fb;
  fs.push(baseSP);  
  if (ppcode == NULL) { // *add 1.2.4 report full function name
      static string tmps = fname_from_fblock(_fb) + " is not defined yet";
      throw Exception(tmps.c_str());   // shd be cool for a _static_ string...
  }
  if (resume_state.m_fb == NULL) baseSP = mSP;
  else if (_fb->nlocal == 100) { baseSP = resume_state.m_baseSP; return; } //fiddle!
  else baseSP = mSP;
  mSP -= _fb->nlocal;
  // *add 0.9.4 Stack Overflow check in start_function
  if ((long)mSP < (long)_stack_) throw Exception("Exec Stack overflow");

  ptr_check = Parser::debug.ptr_check;

// *add 1.2.4 Can control default entry behaviour for trace
  if (gCanTrace && _fb->trace && _fb->trace->do_enter()) {
     // suppress tracing while in custom trace method
	  XExecState* xs = get_exec_state();
      gCanTrace = false;
      _fb->trace->enter(xs);
	  gCanTrace = true;
     // and copy function state back to allow for re-dispatching
	  fb = xs->fb;
	  ppcode = (Instruction*)xs->ip;
	  mSP = xs->sp;
      baseSP = xs->bp;
  }
}

// *add 1.2.4 Can switch off default exit behaviour for trace
void end_function()
{
#ifdef _DEBUG
 if (gFunBlock == fb)
      __break(3);
#endif
 if (fb->trace && gCanTrace && fb->trace->do_leave()) {
      gCanTrace = false;
      fb->trace->leave(get_exec_state());
	  gCanTrace = true;
 }
 baseSP = (int *)fs.pop();
 fb = (FBlock *)fs.pop();
 ppcode = (Instruction *)fs.pop();  
}

void *fs_ptr(int offs)
{
    return *fs.ref_to_TOS(offs);
}


// *add 1.1.1 Dumping the stack frame after a breakpoint halt
void get_stack_frame(int fi, ExecutionState& xs)
{
  if (fi == 0) {
    xs.m_baseSP = baseSP;
    xs.m_fb = fb;
    xs.m_ppcode = ppcode;
  } else {
    int offs = -3*(fi-1);
    xs.m_baseSP = (int *)fs_ptr(offs);
    xs.m_fb     = (FBlock *)fs_ptr(offs-1);
    xs.m_ppcode = (Instruction *)fs_ptr(offs-2);
  }
}

int stack_frame_depth()
{ return fs.depth()/3 - 1; }  // i.e. not interested in <temp>!

// *change 1.2.8 write out full name of function....
// the report is now in a more parser-friendly format.
void dump_fun_frame(int i, const ExecutionState& xs)
{  
	LineInfo li;
    FBlock *this_fb = xs.m_fb;
    if (this_fb == START_FB) cmsg << i << " <DLL> 0 <function>" << endl;
    else {
      FunctionContext *fcxt = (FunctionContext *)this_fb->context;
      li.ip_offset = (xs.m_ppcode - this_fb->pstart)/sizeof(Instruction);
      if (fcxt && fcxt->ip_to_line(li)) 
          ;           
      else {
          li.file = "<none>";
          li.line = -2;
      }
      cmsg << i << ' ' << li.file << ' ' << li.line+2 << ' '
           << fname_from_fblock(this_fb) << endl;
    }
}

void dump_fun_stack()
{
    ExecutionState xs;

    for(int i = 0, n = stack_frame_depth(); i < n; i++) {
       get_stack_frame(i,xs);
	   dump_fun_frame(i,xs);
    }
}

int *find_main_baseSP(char *fct)
{
   ExecutionState xs;

   for(int i = 0, n = stack_frame_depth(); i < n; i++) {
       get_stack_frame(i,xs);
	   if (xs.m_fb->function->name()==fct) {
		   get_stack_frame(i-1,xs);
		   cout << "called from main: " << xs.m_fb->function->name() << endl;
		   return xs.m_baseSP;
       }
   }
   return NULL;
}

void Engine::attach_main_context(char *fct)
{
	Table *cntxt = Function::lookup(fct)->context();
	Parser::state.push_context(cntxt);
	baseSP = find_main_baseSP(fct);
}

// *add 1.1.1 Selecting a stack frame while halted
void Engine::set_frame(int i, bool verbose)
{
    ExecutionState xs;
    LineInfo li;
    Parser::state.pop_context();
    get_stack_frame(i, xs);    
    Parser::state.push_context(xs.m_fb->context);
    resume_state.m_baseSP = xs.m_baseSP;
	if (verbose) dump_fun_frame(i,xs);
}

static int
 s_stepping = 0,
 s_current_exec_line = -1,
 s_old_exec_line = -1;

static Breakpoint* s_brk = NULL;

const int STEP_WAIT=1,STEP_MAIN=2,STEPPING=3,STEPPING_OVER=4;
static FBlock* s_main_fb;
static FBlock* s_initial_fb;
static int s_initial_stack_depth;

// *add 1.2.4 Switches profiling mode; returns IC.
unsigned long instr_count = 0;
const int STEP_PROFILE = 5;

unsigned long* set_instruction_counter(bool do_profiling)
{
   s_stepping = do_profiling ? STEP_PROFILE : 0;
   return &instr_count;  
}


#ifdef _DEBUG
int sh_offs() {
   return (int)mSP-(int)_stack_;
}

CEXPORT char *sh_fun() {
  static string tmp;
  try {
    tmp = fname_from_fblock(fb);  // *change 1.2.4
    return tmp.c_str();
  }
  catch(...) {
    return "<dud>";
  }
}

char *sh_var(char *sym)
{
    static char buff[40];
    PEntry pe = Parser::symbol_lookup(sym);
    if (pe) {
        unsigned int res = *(unsigned int *)pe->global_ptr();
        sprintf(buff,"%x %d",res,res);
        return buff;
    }
    else return "not found";
}
#endif

int current_ip()
{
  return ((int)ppcode - (int)fb->pstart)/4;
}

bool start_exec()
//---------------------------
{
  static int rmode,rdata,val,nargs,tmp;
  static Opcodes opcode;
  static double fval, gval;
  static void *data;
  static int buff[TMP_BUFF_SIZE];
  NFBlock *pfb; // *fix 1.2.4 Must _not_ be static!

  while (ppcode != end_of_code) {
top:
    opcode = (Opcodes)ppcode->opcode;
    rmode  = ppcode->rmode;
    if (rmode) {
      rdata = ppcode->data;
      if (rmode == DIRECT) data = mDataSeg + rdata; else
      if (rmode == SREL)   data = baseSP + rdata;    
      else data = mOP + rdata;
    }

    switch(opcode) {
        //...pushing effective address.
    case PEA:  push((int)data);         break;
    case PERA: push(rdata);             break;

    //.....pushing onto the exec stack...........................
    case PUSHC: push(REF(char,data));   break;
    case PUSHW: push(REF(short,data));  break;
    case PUSHI: push(REF(int,data));    break;
    case PUSHF: push2(REF(float,data));     break; // push as qword 
    case PUSHD: push2(REF(double,data));    break; 
    case PUSHS: pushf(REF(float,data));     break; // push as dword
    
    //....popping off the exec stack into a variable.............
    case POPC:  REF(char,data) = pop(); break;
    case POPW:  REF(short,data) = pop();    break;  
    case POPI:  REF(int,data) = pop();  break;
    case POPF:  REF(float,data) = pop2();   break;
    case POPD:  REF(double,data) = pop2();  break;
    //....stack-relative pushes/pops
    case PUSHSC: push(REF(char,popp()));    break;
    case PUSHSW: push(REF(short,popp()));   break;
    case PUSHSI: push(REF(int,popp())); break;
    case PUSHSF: push2(REF(float,popp()));  break;
    case PUSHSD: push2(REF(double,popp())); break;
    case POPSC:  val=popp(); REF(char,val) = pop(); break;
    case POPSW:  val=popp(); REF(short,val) = pop(); break;
    case POPSI:  val=popp(); REF(int,val) = pop(); break;
    case POPSD:  val=popp();REF(double,val) = pop2(); break;

    case DROP:   drop();            break;
    case DROP2:  drop2();           break;  // *add 1.2.3b missing opcode!
    case DUP:    push(tos());       break;
    case DUP2:   push2(tos2());     break;  // *fix 1.2.4 was taken out by mistake in 1.2.3b!
    case SWAP: //....swap top two dwords on stack.... 
          val = pop();
          tmp = tos();
          tos() = val;
          push(tmp);
          break;
    case SWAPD:  // swap top qword with next dword...
	      fval = pop2();
		  val = pop();
		  push2(fval);
		  push(val);
	      break;
    //.....integer arithmetric operations.......................
    case NEG:    tos() = -tos();        break;
    case ADD:    val=pop();  tos() += val;  break;
    case SUB:    val=pop();  tos() -= val;  break;
    case MUL:    val=pop();  tos() *= val;  break;
    case DIV:    val=pop();  tos() /= val;  break;
    case IDIV:   val=pop();  (unsigned&)tos() /= val;  break;
    case AND:    val=pop();  tos() &= val;  break;  // binary and
    case OR:     val=pop();  tos() |= val;  break;  // binary or
    case XOR:    val=pop();  tos() ^= val;  break;
    case SHL:    val=pop();  tos() <<= val; break;  
    case SHR:    val=pop();  tos() >>= val; break;
    case NOT:    tos() = ! tos();           break;
    case BNOT:   tos() = ~ tos();           break;
    case EQ:     val=pop();  tos() = tos() == val;  break;
    case NEQ:     val=pop();  tos() = tos() != val; break;
    case MOD:    val=pop();  tos() = tos() % val;   break;
    case LESS:   val=pop();  tos() = tos() < val;   break;
    case GREAT:  val=pop();  tos() = tos() > val;   break;
    case LE:   val=pop();  tos() = tos() <= val;   break;
    case GE:  val=pop();  tos() = tos() >= val;   break;

    //....floating-point arithmetric
    case FNEG:   tos2() = -tos2();          break;
    case FADD: 
        fval=pop2();
        tos2() += fval;
        break;
    case FSUB:   fval=pop2(); tos2() -= fval;  break;       
    case FMUL:   fval=pop2(); tos2() *= fval;   break;      
    case FDIV:   fval=pop2(); tos2() /= fval;   break;
    case FEQ:    fval=pop2(); push(pop2() == fval); break;
    case FNEQ:    fval=pop2(); push(pop2() != fval);    break;
    case FLESS:  fval=pop2(); push(pop2() < fval);  break;
    case FGREAT: fval=pop2(); push(pop2() > fval);  break;   
    case FLE:  fval=pop2(); push(pop2() <= fval);   break;
    case FGE: fval=pop2(); push(pop2() >= fval);  break;     


    //...array operations
    case ADDCC:  val=pop();  push((int)data + sizeof(char)*val);    break;
    case ADDCW:  val=pop();  push((int)data + sizeof(short)*val);   break;
    case ADDCI:  val=pop();  push((int)data + sizeof(int)*val);     break;
    case ADDCD:  val=pop();  push((int)data + sizeof(double)*val);  break;
    case ADDPC:  val=pop();  push(REF(int,data) + sizeof(char)*val);    break;
    case ADDPW:  val=pop();  push(REF(int,data) + sizeof(short)*val);   break;
    case ADDPI:  val=pop();  push(REF(int,data) + sizeof(int)*val);     break;
    case ADDPD:  val=pop();  push(REF(int,data) + sizeof(double)*val);  break;
    // ADDSC is ADD!
    case ADDSW:  val=pop();  push(val + sizeof(short)*pop());           break;  
    case ADDSI:  val=pop();  push(val + sizeof(int)*pop());             break;
    case ADDSD:  val=pop();  push(val + sizeof(double)*pop());          break;
    case ADDSN:  val=pop();  push(val + pop()*pop());                   break;

    case COPY:   memcpy((void *)pop(),(void *)pop(),ppcode->data); break;

    //...jumps (all relative) and calls (pcode and native)........
    //...These set ppcode directly, and so we jump to the end of the loop....
    case CCALLX: // *add 1.2.0 Associate VMT with imported ptr
        PFBlock(data)->class_ptr->attach_VMT(mOP);
        goto call_the_function;
    case CCALL: //....patch the hidden VMT pointer......
        *VMT(mOP) = PFBlock(data)->class_ptr->get_VMT();
        // and fall through....
call_the_function:
    case CALL:  
        start_function(PFBlock(data));
        continue;
    case VCALL: //....lookup the proc in the VMT table....
        start_function(virtual_method_lookup(rdata));
        continue;
    case VCALLX: //....lookup proc in associated VMT table (for imports)
        start_function(virtual_method_imported_lookup(rdata));
        continue;
    case CALLN:  // native code interface
        pfb = (NFBlock *)data;
        nargs = pfb->nargs;
        if (nargs == -1) nargs = pop();
        callfn(pfb->pfn,mSP-1,nargs,mOP,pfb->flags,buff);
        mSP += nargs;
        if (pfb->flags & DC_QWORD) push2(*(double *)buff); else
        if (pfb->flags & DC_NOWORD) ;
        else push(*buff);
        break;
    case CCALLV: {// vector constructor/destructor call
        ConstructBlock *pcb = (ConstructBlock *)data;
        PPClass vtable = pcb->get_VMT();
		PClass pc = pcb->class_ptr();
        int num;
        if (pcb->dynamic()) { // i.e new[] has generated a ptr for us; 
           char *ptr = mOP - sizeof(void *);
           num = Builtin::alloc_size(ptr);
        } else num = pcb->num();
        char *old_mOP = mOP;
        for(int i = 0, sz = pcb->size(); i < num; i++) {
          if (vtable != NULL) { // patch VMT, if needed
                if (! pc->has_true_VMT()) pc->attach_VMT(vtable);
                else *VMT(mOP) = vtable;             
          }
          // push  ODS, if needed
          // *add 1.2.3 static objects get their own ODS
          // *change 1.2.9 static objects are handled at compile-time w/ the ODL 
          // (see LoadedModuleList in program.cpp)
		  if (pcb->load_ODS()) 
              mODS.push_object(pc, mOP);     
          if (Engine::execute(pcb->fblock())==CRASHED)
            throw Exception("Construction failed");
          mOP += sz;
        }
        mOP = old_mOP;
        if (pcb->dynamic()) push((int)mOP);  // leave obj on XS
        opop();                              // drop the OS
      }
      break;
  // *NOTE* The continue seems strange, but it is important to skip ppcode++!
    case CALLS:
        start_function(PFBlock(pop()));
        continue;
    case VCALLS:
        start_function(virtual_method_lookup((int)pop()));
        continue;
      JMP_case:
    case JMP:    ppcode = fb->pstart + ppcode->data; goto top;              break;
    case JZ:     if(!pop()) goto JMP_case; break;
    case JNZ:    if(pop())  goto JMP_case; break;
    case JZND:   if(!tos()) goto JMP_case; else drop(); break; 
    case JNZND:  if(tos())  goto JMP_case; else drop(); break;
    case JSWITCH: ppcode = fb->pstart + switch_jump((int *)data,pop()); goto top; 
    case RET:   
          mSP = baseSP;
          mSP += fb->nargs;
          end_function();
          break;    
    case RETI: //......returning dword..........
          val = pop();      // pop the return value
          mSP = baseSP;     // restore stack frame
          mSP += fb->nargs; // drop the args (STDCALL!)
          push(val);        // push return value
          end_function();
          break;
    case RETD: //.....returning qword............
          fval = pop2();
          mSP = baseSP;
          mSP += fb->nargs;
          push2(fval);
          end_function();
          break;
    case ADDSP: // *add 1.2.3b True cdecl calling convention implemented
		{
          int data = ppcode->data;
		  int tp = lo_byte(data);
		  int sz = hi_byte(data);
		  if (tp==1) val = pop();
		  else if (tp==2) fval = pop2();
		  mSP += sz;
          if (tp==1) push(val); 
		  else if (tp==2) push2(fval);
		}
		break;
    case UNWIND:
         {
         int* old_sp = mSP;  
         char *old_op = mOP;
         do_unwind((char *)data, mODS);
         if (old_sp != mSP || old_op != mOP) 
            cerr << "SP/OP changed!\n";
         #ifdef TRACK_ODS
         cout << "unwind " << data << ' ' << current_ip() << ' ' << sh_fun() <<endl;
         #endif
         }
         break;
    case DCAST: { // *add 0.9.5 dynamic cast
           Type t = *(Type *)data;
           Class *tc = t.as_class();  // hm...what about the (void *) possibility?
           void *ptr = (void *)pop();
           Class *oc = get_class_object(ptr);
           if (! oc->inherits_from(tc)) {
              if (t.is_pointer()) push(0);
              else {
                int ip = throw_exception(t_char_ptr,(char *)"bad cast"); // for now!!
                if (ip == -1) throw Exception("bad dynamic typecast");
                else { ppcode = fb->pstart + ip; continue; }
              }
           } else push((int)ptr);
         }
         break;
    // INC and DEC
    case INCC:   REF(char,data)++;  break;
    case INCW:   REF(short,data)++; break;
    case INCI:   REF(int,data)++;   break;
    case INCS:   tos()++;           break;
    case DECC:   REF(char,data)--;  break;
    case DECW:   REF(short,data)--; break;
    case DECI:   REF(int,data)--;   break;
    case DECS:   tos()--;           break;

   // INCP and DECP
    case INCPC:   REFP(char,data)++;    break;
    case INCPW:   REFP(short,data)++;   break;
    case INCPI:   REFP(int,data)++;     break;
    case INCPD:   REFP(double,data)++;  break;
    case DECPC:   REFP(char,data)--;    break;
    case DECPW:   REFP(short,data)--;   break;
    case DECPI:   REFP(int,data)--;     break;
    case DECPD:   REFP(double,data)--;  break;

   // INCS and DECS
    case INCSC:   REFS(char)++;     break;
    case INCSW:   REFS(short)++;        break;
    case INCSI:   REFS(int)++;      break;
    case DECSC:   REFS(char)--;     break;
    case DECSW:   REFS(short)--;        break;
    case DECSI:   REFS(int)--;      break;
  
    //...conversions...............................................
    case D2I:   push((int)pop2());          break;
    case I2D:   push2((double)pop());       break;  
    case F2D:   push2((double)popf());      break;
    case D2F:   pushf((float)pop2());       break;
    case I2F:   pushf((float)pop());        break;
    case F2I:   push((int)popf());          break;
    case I2B:   push(0xFF & pop());         break;   // *add 1.2.9 32-bit to 8-bit bool

    //...object-stack manipulation
    case LOS:
        //depth = ostack.depth();
        opush(data);
        break;
    case LOSS:
        opush((void *)pop());
        break;
    case DOS: 
        opop(); 
        break;  
    case TOSX:
        push((int)mOP);
        opop();
        break;
    case TOSD:
        mODS.push_object(*(PClass *)data, mOP);
        #ifdef TRACK_ODS
        cout << "TOSD " << (void *)mOP << ' ' << current_ip() << ' ' << sh_fun() << endl;
        #endif
        opop();
        break;
    case TPODS: // pushing temporary for functions returning object values
        {
         PClass pc = *(PClass *)data;
#ifdef DEBUG_ODS
          if (pc->has_VMT()) *VMT(mOP) = DEFER_MARKER;
#endif
          mODS.push_object(pc,mOP);
          push((int)mOP);
        #ifdef TRACK_ODS
        cout << "TPODS " << mOP << ' ' << sh_fun() << endl;
        #endif
          opop();
        }
        break;

    case PUSH_THIS:
        push((int)mOP);
        break;

    case STALC: // allocate object directly on stack (needed for MS object-passing)
    // *fix 1.2.0 The UC stack now grows downwards!
		mSP -= ppcode->data;
   	    opush(mSP);
		break;

    case THROW_EX:   {
        Type et = *(Type *)data;
        void *obj = (void *)pop();
        int ip = throw_exception(et,obj);
        if (ip == -1) { // couldn't catch the exception
        // *add 1.2.4 if the thrown type is derived from Exception,
        // then pass on the message! (Warning: this depends on Exception defined in <uc_except.h>
        // having precisely this layout, w/ a char pointer as the first field)
            if (match(Parser::mEException->type,et) != NO_MATCH) 
              throw Exception(*(char **)obj);
            else 
              throw Exception("uncaught exception");
        } // otherwise, can continue excuting at the given catch block address...
        else { ppcode = fb->pstart + ip; continue; }
       }

    case CHKVMT: // *NOTE64*
      push((int)(*(PClass *)(data))->check_object_pointer((char *)pop()));
      break;

    case VTABLE_PATCH:	   
       // *fix 1.2.8 in classes derived indirectly from imported classes,
	  // Class::update_vtable() was _not_ being called for the specific
      // class in question!
	   //  get_class_object(mOP)->update_vtable(mOP);
	   (*(PClass *)(data))->update_vtable(mOP);
      break;

    case POPIF:
    case PUSHIF: {
		int offs,bit_offs,bit_size, mask;
		unpack_bitfield(ppcode->data,offs,bit_offs,bit_size);
		mask = (1 << bit_size)-1;
		data = mOP + offs;
		if (opcode == PUSHIF) {
           val = *(unsigned int *)data;
		   val >>= bit_offs;
		   push(val & mask);
        } else {
		   unsigned word = *(unsigned *)data;	 
           val = pop();
		   mask <<= bit_offs; 
		   mask = ~ mask;
		   word &= mask;
		   val <<= bit_offs;
		   word |= val;
		   *(unsigned *)data = word;
        } 
	 } break;

    //...Miscelaneous (not likely to remain forever!)
    case SHOWI:
    case SHOWV:
    {
        Type t, te;
        PEntry ve;
        if (opcode==SHOWI) {
            t = *(Type *)data; ve = NULL;
            te = t;
        } else { 
            t = t_void;  ve = *(PEntry *)data;
            te = ve->type;
        }
        bool is_double = te.is_double() && !te.is_pointer();
        void *ptr = is_double ? (void *)&tos2() : (void *)&tos();
        Parser::dump_expression(t,ptr,ve);
        if(is_double) pop2(); else pop();
    } break;
    case HALT:
	// *change 1.2.4 if the operand is out of range, it's a NOP instruction!
	    if (ppcode->data < MAX_BREAKPOINTS) {
          if (Breakpoint::from_id(ppcode->data)->execute()) return false;
          continue; // don't move to next instruction!
         } 
		 break;
    case SHOWD:  cout << "(double) " << (rmode ? *(double *)data : pop2()) << endl; break;
    } // switch(opcode)
    ppcode++;   
    CHECK_ODS;
    if (s_stepping) {
	 // *add 1.2.4 Profiling support
     if (s_stepping == STEP_PROFILE) { instr_count++; continue; }	 
     if (ppcode == end_of_code) return true; // finished...
    // *add 1.2.7 working single-stepping 
    // while in this state, we're just looking for the current function block fb
    // to switch back to the initial fb
     if (s_stepping==STEPPING_OVER) {
      if (s_initial_fb == fb) {
        s_stepping = 0;
        s_current_exec_line = get_current_exec_line();        
        if (s_current_exec_line != -1 && s_current_exec_line != s_old_exec_line) return false;
        // it's possible we're back in the original fn, but at its end. So keep going.
        else s_stepping = STEPPING;
      }
     } else
    // normal state while we're single-stepping; jump out if the current line num
    // changes.
     if (s_stepping==STEPPING) {
      int nl = get_current_exec_line();
      ///  cout << "l = " << nl << " ip = " << current_ip() << "fb = " << fb << endl;
      if (nl != -1 && nl != s_current_exec_line) {
           s_old_exec_line = s_current_exec_line;
	       s_current_exec_line = nl;
          // we want to step over; has the current function changed?
          if (s_initial_fb && s_initial_fb != fb) {
             int sdepth = stack_frame_depth();
             if (sdepth > s_initial_stack_depth) {
                // we're entering a new function - let's step over it
                s_stepping = STEPPING_OVER;
             } else {
                // we're leaving a function; continue single-stepping
                s_stepping = STEPPING;
                s_initial_fb = fb;
                s_initial_stack_depth = sdepth;
             }
          } else {
	        s_stepping = 0;
	        return false;  // we are halted on the next line
          }
      }
     } else if (s_stepping == STEP_WAIT) {
		  s_current_exec_line = get_current_exec_line();
		  s_stepping = STEPPING;    
     } 
    }
  } // while(true)
  return true;  // cool, everything ok.
} 

bool Engine::set_single_stepping(bool single_step)
{
   Function* mfn = Function::lookup("main"); 
   s_main_fb = mfn->fun_block();
  // check to see if we are already halted at a breakpoint....
   if (paused() && s_brk != NULL) {
       s_stepping = STEPPING; //STEP_WAIT; 
   	   s_current_exec_line = get_current_exec_line();
   } else s_stepping = STEPPING; 
   s_initial_stack_depth = stack_frame_depth();
   if (single_step) s_initial_fb = NULL;
   else if (paused()) s_initial_fb = resume_state.m_fb;    
   else { // we're starting afresh w/ main
       s_initial_fb = s_main_fb;
       s_stepping = STEPPING_OVER;
   }	  
  return true;
}

//*fix 1.2.8 must explicitly reset single stepping
void Engine::reset_single_stepping()
{
	s_stepping = 0; 
}

FBlock *mStartFB;

extern FBlock mCodeBlock;
Instruction *old_immediate_code;

namespace Parser {
Instruction *immediate_code_ptr();
void immediate_code_ptr(Instruction *pi);
FBlock *immediate_code_block();
};

void dissemble(PFBlock fb); // in DISSEM

bool Engine::paused()   { return resume_state.m_fb != NULL; }
bool Engine::running()  { return fb != START_FB;          } 

bool get_current_exec_pos(int& ip_offs, string& fname,  LineInfo& li)
{
   FunctionContext *fcxt = NULL;
   if (fb) {
     fcxt = (FunctionContext *)fb->context;      
     fname = fname_from_fblock(fb);  // *change 1.2.4
   }
   ip_offs = current_ip();
   if (! fcxt) return false;
   li.ip_offset = ip_offs;
   return fcxt->ip_to_line(li); 
}


Table* context_of_function(LocalContext* lc)
{
   FunctionEntry* pfe = lc->function()->fun_entry();
   return pfe->reference()->context;
}

bool function_is_in_std(LocalContext* lc)
{
    return context_of_function(lc) == Parser::std_namespace();
}

// add 1.2.7 when single-stepping, we can avoid going
// into any function w/in the std namespace.
// *NOTE* 
// 1. wd it apply to methods of classes w/in std?
// 2. shd be a Function method - need it elsewhere.
int get_current_exec_line()
{
	LineInfo li;
	int ip_offs;
	string fname;
	bool ret = get_current_exec_pos(ip_offs,fname,li);
    if (ret && !(Parser::debug.no_trace_std 
        && function_is_in_std((LocalContext*)li.cntxt))) return li.line;
	else return -1;
}

void  exec_message(char *msg, bool was_error, bool dont_unwind_error)
{
 if (fb == START_FB) {
   cerr << msg << endl << "poppped!!" << endl;
   return;
 }
 int ip_offs;
 string fname;
 LineInfo li;
 bool trace_function = Parser::debug.function_trace;
 bool tracing_lines = true; // !trace_function;
 bool back_trace = false;
 
 if (trace_function && Parser::debug.verbose) {
  cerr << "*CRASH " << (void *)mOP << ' ' << (void *)mSP 
       << ' ' << (void *)baseSP << endl;
  }
  // set the error state!

 int kount = 0; // limit the number of trace back levels...
 try {
     do {
        bool have_info = get_current_exec_pos(ip_offs,fname,li);
        if (back_trace && ppcode != NULL) {
          if (have_info) cerr << li.file << ' ' << li.line << ": " << fname << endl;
          else cerr << "(" << ip_offs << ") " << fname << endl;
          if (++kount > 50) {
             cerr << "Too many trace messages" << endl;
             reset_stacks();
             return;
          }
        } else
        if(tracing_lines && have_info) {
            if (!was_error || dont_unwind_error) { 
                // *fix 1.2.8 set the local context _before_ we tell the IDE it's a break....
                Parser::state.push_context(li.cntxt);                          
                resume_state.save(ppcode,true);
                old_immediate_code = Parser::immediate_code_ptr();
                Errors::set_halt_state(msg,fname,li.file,li.line, was_error); 
                return;             
            }
            Errors::set_halt_state(msg,fname,li.file,li.line, was_error);           
            // Halted at breakpoint - prepare to resume, set up context
            // *add 1.2.8 or an error, but we want to preserve the context...
            // *add 0.9.2 Can stop as soon as we have a line no, unless we're tracing...
            mODS.clear();
            if (! trace_function) { reset_stacks(); return; } 
            else {
               cerr << "in " << fname << ":\n";
               back_trace = true;
            }
        }
        if (fs.depth()==0) break;  // *fix 0.9.6 Function Stack Overflow has already cleared this
        end_function();
        check_fun_block(fb);
     } while (fb != START_FB);
     reset_stacks();
     // *fix 1.2.4 Dump out final error message if we haven't found a line number...
     if (! tracing_lines || ! back_trace)
         Errors::set_halt_state(msg,fname,"",ip_offs,true);   
 } catch(...) {
  cerr << "Unknown crash" << endl;
 // *add 0.9.4 Must restore execution stack, esp. for stack overflow errors 
   reset_stacks();
 }  
}

// *change 1.2.9 the 'static ODS' has been retired.
void Engine::global_unwind()
{
  //do_unwind(NULL,mStaticODS);
   LoadedModuleList::run_global_finalization();
}

int handle_exception(PEntry epe, char *msg);

// *add 1.2.8 this is the entry point used by native code to call the interpreter
// so that we can pause the thread for debugging.
int __STDCALL Engine::stub_execute(FBlock* _fb, int flags, ArgBlock *xargs)
//-------------------------------------------------------------------------
{
    int ret;
    do {
     ret = execute(_fb,flags | FROM_OUTSIDE,xargs);
     if (ret == HALTED) {
         if (! Program::in_main_thread()) {
             Program::pause_thread();
             flags = RESUME;
         } 
         else ret = OK;
     }
    } while (ret == HALTED);
    return ret;
}

int __STDCALL Engine::execute(FBlock *_fb, int flags, ArgBlock *xargs)
//--------------------------------------------------------------------
{
  FBlock * old_fb = fb;
  Instruction *old_ppcode = ppcode;
  int *old_mSP = mSP;
  PEntry epe;
  char *except_msg;
resume_execution:
  try {
   if (flags & ARGS_PASSED) {              // only if execute() is called from outside UC
     xargs->flags = flags;                   // *fix 1.1.1 save flags and arg block so we can resume properly!
	 resume_state.m_xargs = xargs;
     if (flags & METHOD_CALL) opush(xargs->OPtr);  // the 'this' pointer 
     for(int i=0,n = xargs->no; i < n; i++)  // these are set by the native stub!!
         push(xargs->values[i]);
   }
   if (flags & RESUME) {     
	 bool context_pushed;
	 set_frame(0,false);                                 // *fix 1.1.1 ensure we're in proper frame!
     resume_state.restore(context_pushed,xargs);
	 if (xargs) flags = xargs->flags;                 // *fix 1.1.1 restore flags and arg block as well!
	 if (context_pushed) Parser::state.pop_context(); // i.e, we were halted by a breakpoint, which pushed the context..
     resume_state.m_fb = NULL;                          // flag us as NOT paused    
     Parser::immediate_code_ptr(old_immediate_code);  // and restore immediate execution context!
   } else { 
    mStartFB = _fb;
    // *fix 1.2.8 we only push START_FB if execute() is called from
    // w/in UCW; its only purpose is to give a 'false bottom' to
    // the function stack.
    if (flags & FROM_OUTSIDE) {
       fs.push(ppcode);
       fs.push(fb);
       fs.push(mSP);
    }
    fb = START_FB;
    ppcode = NULL;
    start_function(_fb);
   }

   // *fix 1.0.0L Some platforms need this to translate signals into C++ exceptions
   // See hard_except.h for the definition and hard_except.cpp for explanation
   CATCH_SIGNALS
   for(;;) {
    if (! start_exec()) { exec_message("halted at ",false); return HALTED; }
     else {
        if (flags & FROM_OUTSIDE) {
          old_mSP = (int*)fs.pop();
          old_fb = (FBlock*)fs.pop();
          old_ppcode = (Instruction*)fs.pop();
        }
        fb = old_fb;		
        ppcode = old_ppcode;        
        if (flags & ARGS_PASSED) {
           if (flags & METHOD_CALL) opop();
           if (flags & RETURN_32) xargs->ret1 = pop(); else
           if (flags & RETURN_64) xargs->ret2 = pop2();
        } else mSP = old_mSP;
        return OK; 
     }
   }
  }
  catch(Exception& e) {
  // map onto the corresp UC exception types. Note that uc_except.h
  // must be included for these to operate properly. (see Parser::init_lib())

    switch (e.type()) {
    case Exception::INT_DIV0:    epe = Parser::mEIntDivByZero;      break;
    case Exception::ACCESS:      epe = Parser::mEAccessViolation;   break;
    case Exception::FLOAT_DIV0:  epe = Parser::mEFloatDivByZero;    break;
    case Exception::RANGE_ERROR: epe = Parser::mERangeError;        break;
	case Exception::UNKNOWN:     epe = Parser::mEException;         break;
    default: epe = NULL;
    }
    except_msg = strdup(e.what()); //*??
  }
  catch(...) {
    // *fix 1.1.0 Translate as an unknown exception....
	epe = Parser::mEException;
    except_msg = "unknown";
  }
  // managing exceptions
  // *add 1.2.8 evaluating stuff which explodes during a debug session
  if (Engine::paused()) return HALTED;
   int ip = -1;
   if (epe != NULL) ip = throw_exception(epe->type,epe->global_ptr());
   if (ip == -1) { // plain uncaught exception message...
       // *add 1.2.8 treating errors as a break is now an option (doesn't unwind fun stack)
         bool break_on_error = Parser::debug.errors_as_break;
         exec_message(except_msg,true,break_on_error);
         return break_on_error ? HALTED : CRASHED;
   } else { // force execution to resume in catch block...!
     // *fix 0.9.4 Point is that throw_exception() will have dropped us 
     // into the right function context...
     // *fix 1.1.0 In this case we are _not_ pushing the local context!
      resume_state.save(fb->pstart+ip,false);
      flags = Engine::RESUME;
      goto resume_execution;
    }
}

int handle_exception(PEntry epe, char *msg)
{
    int ip = -1;
    if (epe != NULL) ip = throw_exception(epe->type,epe->global_ptr());
    if (ip == -1) { // plain uncaught exception message...
       try {
         exec_message(msg,true); return CRASHED;
       } catch(...) {
         cerr << "Disturbed....\n";   return CRASHED;
       }
    } else { // force execution to resume in catch block...!
     // *fix 0.9.4 Point is that throw_exception() will have dropped us 
     // into the right function context...
     // *fix 1.1.0 In this case we are _not_ pushing the local context!
      resume_state.save(fb->pstart+ip,false);
      return Engine::execute(NULL,Engine::RESUME);
    }
}

CatchHandler::CatchHandler(FBlock *fb)
  : m_fb(fb)
{
  m_end_label = new Label(&Parser::code());
}

void CatchHandler::add_catch_block(Type t, int ip_offs)
{
  CatchBlock *pcb = new CatchBlock;
  pcb->type = t;
  pcb->ip_offset = ip_offs;
  m_catch_blocks.push_back(pcb);
}

int CatchHandler::match_thrown_object(Type t, void *obj)
{
  CatchBlockList::iterator cbli;
  FORALL(cbli,m_catch_blocks) {
    Type ct = (*cbli)->type; 
 // *fix 0.9.5. Catch-block must match any derived class..
    if (ct == t || ct == t_void || t.inherits_from(ct)) {
      m_thrown_object = obj;
      m_thrown_type = t;
      *(void **)(Parser::mExceptObj->global_ptr()) = obj;
      return (*cbli)->ip_offset;
    }
  }
 return -1;
}

