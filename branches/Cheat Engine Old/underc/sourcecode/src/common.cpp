/* common.cpp
 * Support logic for grammar
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 */
 #include "common.h"
 #include "function_match.h"
 #include "tparser.h"
 #include "code.h"
 #include "opcodes.h"
 #include "engine.h"
#include "directcall.h"
// for Module stuff.....
#include "module.h"
#include "input.h"
#include "tokens.h"

Table* gScopeContext;

#ifdef _DEBUG
extern int gDebugBreak;
extern FBlock* gFunBlock;
void __break(int);
#endif

Function* gLastFunction;  //*DEBUG
void* gObjectReturnPtr;

void next_statement();  // in uc_tokens.cpp
void skip_function_body(int brace_count=1); // *TEMP* in templates.cpp

// these are temporary places for stuff that must go in headers!
 void dissemble(PFBlock fb); // in DISSEM
 void dissembler_init();

 bool interactive_mode(); 

 // found in parser code - don't muck w/ in_declaration directly!
 extern bool IEF;
 void force_comma_flag();

 // found in type.cpp (of all places!)
 Signature *unique_signature(Signature *sig);

void *Entry::global_ptr()
{
    return Parser::global().addr(data);
}

char *Entry::object_ptr(char *obj)
{
    return obj + data;
}

// *add 1.1.4 Bit field management. Note the assumption here is that
// for a 32-bit word, the info can be packed into 20-bits, which is
// the operand size for our instruction set.
struct _BitField {
	int offs; //: 10;
	int bit_offs; //: 5;
	int bit_size; //: 5;
	int rest; // : 12;    // pack up to 32-bits
};

void Entry::set_bit_field(int offs, int bit_offs, int bit_size)
{
 int sz = sizeof(_BitField);
 data =  Parser::global().alloc(sz,NULL); 
 rmode = OREL_F;
 _BitField *bf = (_BitField *)Parser::global().addr(data);
 bf->offs = offs;
 bf->bit_offs = bit_offs;
 bf->bit_size = bit_size;
 bf->rest = 0;
// unsigned int rdata;
// memmove(&rdata,&bf,4); 
// data = rdata; 
}

void unpack_bitfield(int data, int& offs, int& bit_offs, int& bit_size)
{
 _BitField *bf = (_BitField *)Parser::global().addr(data);
 offs = bf->offs;
 bit_offs = bf->bit_offs;
 bit_size = bf->bit_size;
}

void Entry::get_bit_field(int& offs, int& bit_offs, int& bit_size)
{
  unpack_bitfield(data,offs,bit_offs,bit_size);
}

string itos(int i)
{
 char buff[20];
 itoa(i,buff,10);
 return buff;
}

string as_str(Type t)
{ static string str;
  t.as_string(str);
  return str;
}

void fail(const string& msg)
{ 
    throw msg;
}

 //... private to this module
 namespace {
  const int DATA_SIZE = 1000000;
  Global mGlobal(DATA_SIZE);
  FBlock mCodeBlock;
  LocalContext *mpTempContext=NULL;
  PClass mCatchHandlerObj=NULL;
  const int CONTINUEABLE = 1, IN_METHOD = 2, CONTEXT_PUSHED = 4,
            IN_SWITCH = 8, BREAKABLE = 16, FUN_BLOCK = 32, LOOP_SCOPE = 64;

 }
 //.......................

 void clear_global_namespace()
 {
  mGlobal.clear();
 }

int yyerror(const char *s);  

 void error(string msg)
 {
 using Parser::state;
 // this flag forces an orderly exit from the parser!
  if (state.err != "") yyerror(state.err.c_str());
  state.err = msg;
  state.file = Input::filename();
  state.lineno = Input::lineno();
 }

 void warning(string _msg)
 {
   cmsg << Input::filename() << ' ' << Input::lineno() << ": "
       << _msg << endl;
 }

 string make_temp_name()
 {
  static int k = 0;
  return "$" + itos(k++);
 }

 int tstack_depth()
 { return Parser::state.tstack.depth(); }

 static Parser::ExprFun s_expr_handler;

 // shared w/ parser,for the mo!
 namespace Parser {
  
  ParserState state; 
  PEntry mEIntDivByZero=NULL,mEAccessViolation=NULL,mEFloatDivByZero=NULL,
         mEException=NULL,mERangeError=NULL;
  PExpr mOCout=NULL;
  PExpr mObjectReturnExpr;
  Namespace* sStdNamespace = NULL;

  int s_array_size = 1;
  int s_twodim_array = 1;

  DebugFlags debug;

  Global& global() { return mGlobal; }

  Table*  std_namespace() 
  {
    return (Table*) sStdNamespace;
  }

  bool set_alignment(int ipack)
  {
    if (ipack != 1 && ipack != 4 && ipack != 8) return false;
	Parser::debug.class_dword_align = ipack;
    return true;
  }
 
// temporary stuff used by the grammar to Express Itself.
void out(char *s)
 { cout << s; }
void outln(char *s)
 { cout << s << endl; }

string quotes(const string& s)
{ return "'" + s + "'"; }

// type stack stuff
void  tpush(Type t)
{ 
  state.tstack.push(t);
  if (state.tstack.depth() >= state.tstack.capacity())
	  state.tstack.pop();
}

Type& tots()
 { return state.tstack.TOS(); }

void  stots(TType tt)
{ state.tstack.TOS() = AsType(tt); }

Type  tpop()
 { return state.tstack.pop(); }
TType ttots()
 { return AsTType(tots()); }
TType make_ref(TType t)
 { AsType(t).make_reference(); return t; }
TType incr_ptr(TType t)
 { AsType(t).incr_pointer();   return t; }
TType make_unsigned(TType t)
 { AsType(t).make_unsigned(); return t; }

int block_depth()
{
 return state.block_stack.depth();
}

void *ExprToPtr(Expression e)
{
// *SJD* This is specialized to operate with _globally allocated_ data
 if (!e->is_entry()) return NULL;
 return e->entry()->global_ptr();
}

// *fix 1.2.3a (Eric) Can now have expressions as array sizes
TType make_array(TType t, Expression psz)
{
 int sz=0; // defaults to zero so we know to infer size from initializer...
 if (psz) {
    try { sz = const_int_expr(psz); }
    catch(string msg) { error(msg); return t; }
 }
 AsType(t).make_array(sz);
 if (s_array_size > 1) s_twodim_array = sz;
 else s_array_size = sz;
 return t;
}

void reset_array_size()
{
  s_array_size = 1; //*SJD* Temporary hack!
  s_twodim_array = 1;
}


LocalContext *temp_context()
{ return mpTempContext; }

PClass try_block_class()
{ return mCatchHandlerObj; }

void set_function_code(bool is_fn)
{
 state.m_PCode = is_fn ? &state.m_FContext : &state.m_SContext;
}

UCContext& static_code() { return state.m_SContext; }

bool is_function_code() { return state.m_PCode == &state.m_FContext; }
UCContext& code()       { return *state.m_PCode; }

FBlock *temp_fun_block() { return &mCodeBlock; }


// support code for grammar starts here....

bool is_class(Table *pc)        { return pc->type()==IS_STRUCT; }
bool is_namespace(Table *pc)    { return pc->type()==IS_NAMESPACE;}
bool in_class_context()         { return is_class(&state.context()); }
bool is_local(Table *fc)        { return fc->type()==FUNCTION;}
bool is_class_entry(PEntry pe)
 { return in_class_context() && pe == PClass(&state.context())->entry(); }

bool is_global(Table* pc)       { return pc == &mGlobal; }
bool is_global_entry(PEntry pe) { return is_global(pe->context);}

Function *current_function(Table *context)
{
 if (context==NULL) context = &state.context();
 if (is_local(context)) return PLocalContext(context)->function();
 else return NULL;
}

int a;

PEntry symbol_lookup(const string& name)
{
  Table *context = &state.context() ;
  PEntry pe = context->lookup(name);
  if (!pe) return NULL;
  if (debug.no_access_control) return pe;  
  if (!is_class(pe->context)) {
    return pe; // interactive!!
  }

  // Access control - this entry came from a class....  
  PClass pe_class = (Class *)pe->context;

  // if we are in a function, see if it is a friend!
  Function *function = current_function(context);
  if (function && pe_class->is_friend_function(function)) return pe;

  // What is our current class context?
  PClass this_class;
  if (is_class(context)) this_class = (Class *)context; else
  if (function) this_class = function->class_context();
  else this_class = NULL;
  if (this_class == pe_class) return pe;  // trivial case!

  int access = pe->access(), base_access = pe_class->base_access_mode();
  if (base_access != Public) {
    if (base_access == Private) access = Private; else
    if (base_access == Protected && access == Public) access = Protected;
  }
  if (access==Public) return pe; 

  if (this_class) {
   if (access == Protected && this_class->inherits_from(pe_class)) return pe;
   if (pe_class->is_friend_class(this_class)) return pe;

  //*SJD* Does this entry come from an _enclosing_ class?
  // (can tell this if we do have the right parent but it isn't a base class!)
   if (this_class->parent_context() == pe_class && this_class->base_class()==NULL)
         return pe;

  }
  // sorry, no can do.
  warning("cannot access " + quotes(pe->name));
  return pe; ///NULL;  *Behaviour is different in 'console mode'

}

// *add 0.9.4 support for extern "C".  Returns int because we may need to be explicit.
bool in_extern()
{ return state.extern_flag; }

bool in_extern_C()
{ return state.extern_flag_C; }

// parser state management
void ParserState::reset()
{
  in_match = false;
  was_fun_ptr = false;
  in_declaration = false;
  in_typedef = false;
  in_class = false;
  in_method = false;
  in_switch = false;  
  class_dcl = t_void;
  scope_context = NULL;
  in_loop = false;
  in_construct_destruct = IsPlain;
  extern_flag = false;
  extern_flag_C = false;
  modifier = None;
  err = "";
  arg_list.clear(); 
  // NB to restore any stacks!
  // note this mucks up the state while we are paused...need
  // to think this one through HACK04
  if (!Engine::paused()) {
    context_stack.clear();
    push_context(&global());
  }
//  mCodeBlock.pstart = code().end_code();  // will clear static code buffer 
  if (is_function_code()) {
    code().end_code();          // clear the function code buffer   
    set_function_code(false);   // so we're back in static code 
  }
  block_stack.clear();         // clear any pending blocks
  tstack.clear();             // clear the type stack
  IEF=false;

  s_expr_handler = NULL;

  dcl_init_list.clear();  
}

void ParserState::push_context(Table *tbl) 
{
  context_stack.push(tbl);
}

Table& ParserState::context() 
{   
    Table *cntxt = context_stack.TOS(); 
    if (!cntxt) { // *DEBUG*
       warning("Restoring lost context...");
       cntxt = &global();
       push_context(cntxt);
    } 
    return *cntxt;  
}

void ParserState::add_to_arg_list(Type t, const string& name, Expression init)
{
 arg_list.push_back(ArgEntry(t,name,init));
}

static ArgList tmp_arg_list;

void ParserState::begin_args()
{
 tmp_arg_list = arg_list;
 arg_list.clear();
}

void ParserState::end_args()
{
 arg_list = tmp_arg_list;
}

void clear_parse_flags()
{
 force_comma_flag(); // *hack 0.9.6 the dcl stack is getting out of order...
 state.token_stack.clear();
}

void restore_global_context()
{
 if (is_local(&state.context())) state.pop_context();
}

bool mFakeInitBlock = false;

// *add 1.2.7 Method bodies are cached for later compilation

bool ParserState::handle_method_body(Class* pc, bool plain_method)
{
  // With imported methods, the idea is to ignore the inline code and link to the exported fn.  
    if (Builtin::get_dll_handle() != NULL) {
  // *fix 1.2.0L GCC under Linux doesn't export these guys, so compile them as usual!
// *ch 1.2.9 patch
#ifndef _WIN32
      return false;
#else
     // (ignore case of methods outside class defn...)
     if (pc != NULL) pop_context();
     // but force a dynamic link to those w/in the class body
     else  declare_function(tpop(),token_stack.pop(),NULL);  
     //  and ignore the method body
     Template::grab_function_body(plain_method,NULL); 
     // *fix 1.2.8 see 'hack of the week' in the next block - we gotta fool the parser!
     if (! plain_method) {
         Input::insert_string("__init_block__ {}");
         mFakeInitBlock = true;
     }
#endif
  } else {
    // Only method bodies _inside_ class definitions are cached. Or caching can be switched off...
     if (! debug.skip_method_bodies || pc != NULL) return false;  
    // otherwise, we want to cache these bodies for later compilation when the class is finalized;
    // make sure they're declared      
     TemplateInstance* ti = in_template;
     in_template = NULL;
     Function* pf = declare_function(tpop(),token_stack.pop(),NULL);            
     in_template = ti;
     char* body_buff = Template::generate_function_header(pf,plain_method,false);
     Template::grab_function_body(plain_method,body_buff);
     pf->class_context()->add_defered_method_body(pf,body_buff);
     // *hack 1.2.7 hack of the week: we have grabbed the init list, but need to push
     // something into the input stream which the parser recognizes as 'class_init_list block'
     if (! plain_method) {
         Input::insert_string("__init_block__ {}");
         mFakeInitBlock = true;
     } else {
         block_stack.push(0);
         Input::insert_string("}");
     }   
  }
  dcl_reset();                                // and ensure that the state is properly reset!
  while (tstack_depth() > 2) tpop();                
  clear_parse_flags();
  return true;    
}

/// Managing blocks!
// also used to start function definitions....
void ParserState::init_block(int type)
{
// parser can ask us to ignore the next init_block(); used w/
// constructor initialization lists to prematurely start the function
// definition.  That is, we break the usual function init_block up into
// two separate operations: constructing the function and its block,
// and then initializing it!
 if (in_method) {
   in_method = false; 
   // if we're inside a class body, then skip this initialization
   if (debug.skip_method_bodies && (class_dcl != t_void || mFakeInitBlock)) {
       block_stack.push(0);
       mFakeInitBlock = false;
       return;
   }
   if (!is_local(&context())) { 
       error("Context not local!");
       return;
   }
   FunctionContext& fc = static_cast<FunctionContext&>(context());
   FBlock *fb = fc.fun_block();
   int nargs = fb->nargs;  // *hack 0.9.8 was being reset to zero...MUST OVERHAUL!
   fc.initialize();
   fb->nargs = nargs;
   return;
 }

 bool is_continueable = in_loop, is_breakable = in_loop;
 int flags = 0;
 if (in_declaration) { // _only_ if this is a function block!
   Table *cntxt = &context();
   PClass pc = NULL;
   // *fix 1.1.3 If this was a template method, then don't muck up the IC!
   // Careful to exclude the case of instantiating methods of a template class.
   bool template_instantiating =
       cntxt->type() == INSTANTIATION_CONTEXT  
    && ! in_template->get_template()->get_entry()->is_method();
   flags = CONTEXT_PUSHED;
   if (class_dcl != t_void) { // nested Class:: notation...
     if (!class_dcl.is_class()) { error("Not a class name"); return; }
     pc = class_dcl.as_class();
     // class context may _already_ be pushed in the case of constructor
     // initialization lists - but we flag it as a method anyhow.
     if (cntxt != pc  /*&& ! template_instantiating*/) { 
         push_context(pc);
         cntxt = pc;
     }
     if (! template_instantiating) flags |= IN_METHOD;  
   }

   // *add 1.1.0 Expressly forbid nested functions! Otherwise we get nasty brace mismatch issues...
   if (is_local(cntxt)) {
       error("Cannot have functions within functions"); return;
   }
   string name = token_stack.TOS();

   // *add 1.1.0 Skip the bodies of any inline imported methods, except for ctors w/ init lists
   if (is_class(cntxt)) {
       if (handle_method_body(pc,type != CONSTRUCTOR_BLOCK)) return;
   }
   start_function(tpop(),token_stack.pop(), type != CONSTRUCTOR_BLOCK);

   dcl_reset();

   // *change 1.2.3 The return label is now backed by a stack
   m_ret_label = new Label(&code());
   m_ret_label_stack.push(m_ret_label);
   flags |= FUN_BLOCK;
 } else
 if (in_switch) {
   flags = IN_SWITCH;    
   is_breakable = true;
 } else
 if (in_class) {
   flags = CONTEXT_PUSHED;
   in_class = false;
 } else
 if (type==LOOP_BLOCK) {
   flags = LOOP_SCOPE;
 }

 if (is_breakable) {
   flags |= BREAKABLE;
   break_stack.push(label_stack.TOS());
   if (in_switch) { // *fix 0.9.4 switch left label stack pushed - mucked w/ while!
      label_stack.pop();
      in_switch = false;
   }
 }
 if (is_continueable) {
   flags |= CONTINUEABLE;
   continue_stack.push(new Label(&code()));  
   in_loop = false;
 }
 block_stack.push(flags); 
 clear_parse_flags();
}

bool ParserState::context_generated()
{
  return block_stack.depth() == 0 || block_stack.TOS() & CONTEXT_PUSHED;
}

void ParserState::check_context(int mode)
{
// if a fresh context has been pushed, cool...
 if (context_generated()) return; 
 Function *function = current_function();
 // function==NULL is a C++ error, but is allowed in interactive mode
 push_context(new LocalContext(&context(),function));
 block_stack.TOS() |= CONTEXT_PUSHED;
}

bool ParserState::in_loop_dcl()
{
  return block_stack.TOS() & LOOP_SCOPE;
}

void end_switch();

void ParserState::finalize_block()
{
 if (block_stack.depth() < 1) 
	 block_stack.push(CONTEXT_PUSHED);

  int flags = block_stack.pop();
  if (flags & IN_SWITCH) end_switch();
  if (flags & CONTINUEABLE) {
      m_continue = continue_stack.pop();
      m_continue->here();     
  } else m_continue = NULL;

  if (flags & BREAKABLE) break_stack.pop();
  if (flags & FUN_BLOCK) { // *fix 0.9.5 UCW can fall over if braces are mismatched..
     if (m_ret_label_stack.empty()) error("misplaced braces");
     else m_ret_label->here(); 

  // *hack 1.1.0 It's difficult to skip the body of a imported ctor w/ init list,
  // so ensure that declare_function() is called to import this function!
    if (Builtin::get_dll_handle() && is_class(&context())) {
       string name;
       if (!Builtin::add_dll_function(current_function(),0,name))
          cerr << "cannot link to ctor " + quotes(name) << endl;
//	   fn->builtin(Function::CDECL); //?
	}
  }
  if (flags & CONTEXT_PUSHED) {
    context().finalize();
    pop_context();
    class_dcl = t_void;
  }   

  // If we were in a method body, then the class context must be dropped
  if (flags & IN_METHOD) pop_context();
  // can now dispose of the continue/return label
  if (flags & CONTINUEABLE) delete m_continue;
  if (flags & FUN_BLOCK) {
      delete m_ret_label;
      m_ret_label = NULL;
      m_ret_label_stack.pop();

  } 
  state.in_construct_destruct = IsPlain; //*fix 1.2.4
}

Label *ParserState::return_label()
{ 
  return m_ret_label;
}

//---------------doing control statements---------------

// *fix 1.2.7 In 'A::B<C>', C was looked up in A's context.
// The fix involves detecting whether we're in a scoped context,
// and popping and pushing the context.
void ParserState::begin_templ_list()
{
 if (scope_context != NULL) pop_context();
}

void ParserState::end_templ_list()
{
 if (scope_context != NULL) push_context(scope_context);
}


void push_label_stack()
{  state.label_stack.push(new Label(&code())); }

void label_jump(int jtype, bool do_push)
{
 if (do_push) push_label_stack();
 code().jump(jtype,state.label_stack.TOS());
 if (!do_push) { delete state.label_stack.pop(); }
}

void label_here()
{
 push_label_stack();
 state.label_stack.TOS()->here();
}

void compile_condition(PExpr cond)
{
  code().compile(Expressions::cast_to_bool_op(cond));
}

bool do_loop_start(PExpr cond, bool jump_back)
{
 // skip the controlled statement if not true!
 // *fix 0.9.4 Push a non-zero value to ensure for(;;) keeps going..
 if (!cond) cond = Expressions::sizeof_op(1);
 compile_condition(cond);
 // Quite NB to dispose of any temporaries before we continue...
 check_temp_context();
 /*if (jump_back)*/ label_jump(JZ,true);
 return true;
}

bool do_loop_end(bool is_jump, bool is_forward)
{
 Label *l1 = state.label_stack.pop(); 
 // now either jump forward (if-else) or back (while-do-for)
 if (is_jump) label_jump(JMP, is_forward);   
 l1->here(); // the forward jump!
 return true;
}

bool do_for_end(PExpr e)
{ 
 if (e) code().compile(e,DROP_VALUE);
 do_loop_end(true);
 return true;
}

// *fix 1.2.3 break inside a do-while loop was broken!
bool do_do_end(PExpr cond)
{
// this will be the break label
 Label *l1 = state.label_stack.pop(); 
 compile_condition(cond);
 check_temp_context();
 // if condition fails, we jump back to the top label;
 code().jump(JNZ,state.label_stack.pop());
 // break will jump to here
 l1->here();
 return true;
} 

void check_enclosing_scopes(LocalContext *outer)
// Given a local context, look at the enclosing scopes until we hit
// some outer context (usually the outermost function context). If the
// outer context has auto objects, then do nothing, since the jump will
// go to a proper unwind.  Otherwise, find the first enclosing scope
// that does have auto objects.
// note: must only be called from within a local function context
{
 LocalContext *cntxts[BLOCK_DEPTH];
 LocalContext *lc = (LocalContext *)&state.context();
 int ic = 0;
 while ((cntxts[ic++] = lc) != outer)  
   lc = (LocalContext *)lc->parent_context(); 
 while (--ic >= 0) 
   if (cntxts[ic] != outer && cntxts[ic]->first_obj() != NULL) {
		 cntxts[ic]->check_objects();
		 return;
   }
}

bool do_return(PExpr e)
{
  Label *ret_label = state.return_label();
  if (!ret_label) { error("return only in function"); return false; }
  Function *fn = ((LocalContext&)state.context()).function(); 
  Type rt = fn->return_type();
//  if (fn->name()=="bh_begin")
//     ret_label = 0;     
   // *add 1.1.1 support for __declare; uses first return type!
  if (rt == t_null) { // t_undefined
	  rt = e->type();
	  rt.strip_const();
	  fn->adjust_return_type(rt);
  } 
  // *change 1.1.0 Void functions can return void 'values' (ISO standard behaviour)
  if (rt == t_void && e != NULL && e->type() != t_void) { 
      error("void function cannot return value");
      return false;
  }
  if (e) {
      PExpr ret_exp = Expressions::return_op(fn,rt,e); 
      if (ret_exp->is_nil()) { 
          error("Could not copy return object");
          return false;
      }  
      code().compile(ret_exp);
  }
  // *fix 1.1.0 Must ensure that a proper UNWIND operation occurs for any locals
  check_enclosing_scopes(fn->context());
  code().jump(JMP,ret_label);
  return true;
}

// *fix 1.2.4 When breaking out of a loop inside a local context, do check that 
// there _actually is_ a context local to the loop!  We were trashing objects!
// *fix 1.2.5 The new 'false' param for LocalContext::check_objects() prevents
//  the system for considering this context to be fully unwound. We were losing
//  normal unwind at the end of loops.
bool check_local_unwind()
{
 if (is_local(&state.context()) && state.context_generated())
     return ((LocalContext&)state.context()).check_objects(false);
 else return false;
}

bool do_break_continue(int keyword)
{
  LabelStack &lstack = (keyword==BREAK) ? state.break_stack : state.continue_stack;
  if (lstack.depth()==0) { error("break/continue not allowed here"); return false; }
  if (keyword==BREAK) check_local_unwind();
  code().jump(JMP,lstack.TOS());
  return true;
}

///// implementing switch statements

void do_switch(PExpr e)
{
 state.in_switch = true;
 push_label_stack(); // for the break....
 code().compile(e);
 state.switch_stack.push(new SwitchBlock(&code()));
 code().emit(JSWITCH,DIRECT,0);
}

bool do_case(PExpr e)
{
 if (e) {
   void *cnst_ptr = ExprToPtr(e);
   int val;
   if (cnst_ptr==NULL) { error("Case label cannot be an expression"); return false; }
   switch(e->type().size()) {
   case sizeof(long):  val = *(long *)cnst_ptr;  break;
   case sizeof(short): val = *(short*)cnst_ptr;  break;
   case sizeof(char):  val = *(char *)cnst_ptr;  break;
   default: { error("Case label must be an integer type"); return false; }
   }
   state.switch_stack.TOS()->add_jump(val);
 }else state.switch_stack.TOS()->add_default();
 return true; // for now
}

void end_switch()
{
 state.break_stack.TOS()->here();
 delete state.switch_stack.pop()->construct();
}

////////// implementing goto
static Label* create_goto_label(char* label_name)
{
// NB to create the label in the overall function context, not some local block
 Table* cntxt = ((LocalContext&)state.context()).function()->context();
 PEntry label_entry = cntxt->add(label_name);  
 label_entry->type = t_label;
 label_entry->size = 0;
 Label* label = new Label(&code());
 label_entry->data = (int)label;
 return label;
}

static Label* check_goto_label(PEntry label_entry)
{
  if (! state.return_label())
     fail("goto only allowed in functions");
  if (label_entry->type != t_label)
     fail(label_entry->name + " is already defined");
  return (Label*)label_entry->data;    
}

void do_goto(char* label_name)
{
 try {
  Label* label = NULL;
  Table* cntxt = &state.context();
  PEntry label_entry = cntxt->lookup(label_name);
  if (label_entry) label = check_goto_label(label_entry);
  if (! label) {           // ----not declared yet; forward jump----
     label = create_goto_label(label_name);  
     // if there were local objects needing destruction, keep track of this!
     if (check_local_unwind()) label->context(cntxt);
  } else {                 // ----backward jump---------------------
      // only try to unwind local objects if the actual label was outside our context
      // (if the label doesn't have an address, then we haven't found it yet)
     if (label->context() != cntxt || ! label->addr_is_set()) check_local_unwind();
  }
  // and do the jump!
  code().jump(JMP,label);
 } catch(string s) {
   error(s);
 }
}

void goto_label_new(char* name)
{
 Label* l = create_goto_label(name);
 l->here();
}

void goto_label_existing(PEntry pe)
{
 try {
   Label* l = check_goto_label(pe);
   l->here();
   if (l->context() == &state.context()) l->patch_with_NOP();
 } catch(string s) {
   error(s);
 }
}


string name_of(Type t)
{ 
 static string s;
 t.as_string(s);
 return s;
}

bool ParserState::begin_scope(Type t)
{
 if (!t.is_class()) {
    error(quotes(name_of(t)) + " is not a class, struct or namespace");
    return false;
 } else {
    PClass pc = t.as_class();  
  //* this forces instantiation of templates when we need to access their members
    if (! t.is_namespace()) pc->make_available();
    push_context(pc);
    scope_context_stack.push(scope_context);
    scope_context = &context(); 
    gScopeContext = scope_context;
    return true;
 }
}

// *add 1.2.7 backing scope_context with a stack...
void ParserState::end_scope()
{
  pop_context();
  scope_context = scope_context_stack.pop();
}

PClass class_context()
{
 return is_class(&state.context()) ? PClass(&state.context()) : NULL;
}

void error_already_defined(const string& name)
{
 if (! interactive_mode())
  fail(quotes(name) + " is already defined");
}


// *add 1.2.6 union
Type ParserState::add_class(int s_or_c, const string& pname, int deriv_access, Type base_class)
{
 bool is_struct = s_or_c != CLASS;
try {
 Table *instantiation_context = NULL;
 Table *cntxt = &context();
 // *add 1.1.4 
 // * tagless structs are given a temporary name
 // * in C mode, structs and unions are made distinct so they don't conflict w/ variable names ('struct tm tm')
 string name = pname;
 if (name == "") name = make_temp_name();
 else if (debug.c_mode && is_struct) name = "$_" + name; 
 PEntry pe = context().lookup(name);
 bool already_defined = pe != NULL;

  //*SJD* ISSUE Policy is a little loose here;  I'm allowing redefinition
 // but really it is another concession to interactive mode.
// if(already_defined && context().is_local_entry(pe)) error_already_defined(name);
 if(!already_defined) {
 // *fix 0.9.6 a template class is w/in an 'instantiation context' - watch out for nested case!
 // *fix 0.9.8 the IC is _injected_ into the class context - parent trick breaks down w/ derivation
    // if (in_template == NULL || context().type() != INSTANTIATION_CONTEXT)
    //      pe = context().add(name);
    // else pe = context().parent_context()->add(name); // the enclosing namespace

    if (in_template != NULL && context().type() == INSTANTIATION_CONTEXT) {
       instantiation_context = pop_context();
       cntxt = instantiation_context->parent_context();
    }
    pe = cntxt->add(name);
	//if (deriv_access == ForwardClass && in_typedef)
 } 
 Class *base = NULL;
 if (deriv_access != NotDerived && deriv_access != ForwardClass) {
   string nb = quotes(as_str(base_class));
   if (!base_class.is_class())   fail(nb + " is not a class");
   base = base_class.as_class();
   if (! base->make_available()) fail(nb + " is not fully defined");
 }
 if (deriv_access == Default) deriv_access = is_struct ? Public : Private;
 Class *new_class;
 if (! already_defined) {
   new_class = new Class(base ? base : cntxt /*&context()*/,deriv_access);
   new_class->entry(pe);
   // *add 1.2.3 Class now keeps track of whether it's declared as struct or not
   if (is_struct) new_class->make_struct();
   new_class->set_access_mode(is_struct ? Public : Private);
   if (s_or_c == UNION) new_class->set_anonymous_union();
   pe->type = Type(new_class);
   pe->size = 0;
   pe->data = 0;  // which distinguishes us from Namespaces!!
   pe->set_class();  // meaning: we are not a typedef!
   pe->m_typename = true;
   if (instantiation_context != NULL) {
     new_class->inject_namespace((Namespace *)instantiation_context);
     push_context(instantiation_context);
   }
  } else if (deriv_access != ForwardClass) {
   // *fix 1.1.2 A forward class definition must not clear out an existing class
   // redefinition or full definition.
    new_class = pe->type.as_class();
   // two cases: full defn of forward class, 
   //  and redefinition of existing class (an extension!)
   //*SJD* Absolutely need to call the base-setting code for derived classes
    new_class->set_base_class(base,deriv_access);
 }

 if (deriv_access != ForwardClass) {
      if (state.in_template != NULL)
        state.in_template->instantiated(true);

     Module::current()->add_typename(pe);
     push_context(new_class);
     state.in_class = true;
     state.class_dcl = t_void;  

  }
  return pe->type;
 } catch(string msg) { error(msg); }
 return t_void;
}

void ParserState::set_access_mode(int val)
{
 if (!is_class(&context())) error("Access mode not allowed here");
 else ((Class&)context()).set_access_mode(val);
}

void ParserState::add_friend_class(const string& name)
{
 if (!is_class(&context())) error("Friend not allowed here"); 
 else {
  PEntry pe = symbol_lookup(name);
  if (!pe->type.is_class()) error("Must be a class context");
  else ((Class&)context()).add_friend_object(pe->type.as_class(),true);
 }
}

/*REF:NAMESPACE*/

void ParserState::add_namespace(string name)
{
 Namespace *ns;
 // *fix 1.2.0 Change in grammar for 'class_name' broke anonymous namespaces.
 // *fix 1.2.3 Nasty code that was too free with string char data caused anon namespaces
 // to break in debug mode.
 bool is_anonymous = name == "";
 if (is_anonymous) name = Module::anonymous_namespace_name();
 PEntry pe = context().lookup(name);
 bool already_defined = pe != NULL;
 if(!already_defined) { 
     pe = context().add(name);
     ns = new Namespace(&context());
     pe->data = (int)ns;
	 pe->set_class();
     ns->entry(pe);
     pe->type = Type(ns);
     pe->m_typename = true;
 } else ns = (Namespace *)pe->data;
 if(is_anonymous) context().inject_namespace(ns);
 else  Module::add_namespace(ns);
 push_context(ns);
}


Signature *ParserState::get_signature(Type t, bool clear_arglist) 
{
// 'clear_arglist' now (apart from doing what it says afterwards) 
// also attaches the list of parameter names to the signature. 
 TypeList ts;
 int sz = 0;
 bool was_stdarg = false;
 ArgList::iterator ali;
 StringList sl;
 if (arg_list.back().type == t_void) { // means '...'!!
    arg_list.pop_back();
    was_stdarg = true;
 }
 for(ali = arg_list.begin(); ali != arg_list.end();  ++ali) {
    ts.push_back(ali->type);
    // *fix 1.2.2 Displayed prototypes of imported functions had extra '*' symbols;
    // they were probably put in to signify 'no name'
    if (ali->name != "*")  sl.push_back(ali->name);  
                    else   sl.push_back("");
    sz += (ts.back().is_double() ? sizeof(double) : sizeof(int));
 }
 Table *pc = &context();
 // *fix 1.1.3 Member function template being instantiated!
 if (pc->type() == INSTANTIATION_CONTEXT) pc = pc->parent_context();
 Class *cptr = is_class(pc) ? (Class *)pc : NULL;

 //HACK15  If this is a method pointer declaration, then use 
 // state.class_dcl which is set in the declaration grammar
 if (was_fun_ptr && state.class_dcl != t_void) {
   cptr = state.class_dcl.as_class();
   state.class_dcl = t_void;
 }

 Signature *sig = new Signature(t,ts,cptr); 
 sig->byte_size(sz);
 sig->stdarg(was_stdarg);
 if (member_is_const) sig->set_const();
 member_is_const = false;
 // *fix 1.1.2 Argument names are now always saved by get_signature()
 sig->set_arg_names(sl);
 if (clear_arglist) end_args();
 
 //*HACK* This can be set by array args...
 // (usually reset by add_variable)
 s_array_size = 1;
 s_twodim_array = 1;

 return sig;
}

// *fix 1.1.3 This is only called by do_function_template(), but 
// there are two cases (i) a method of a template class declared outside,
// and (ii) a genuine method template.
Signature *get_prototype(bool& outside_class)
{
 string name = state.token_stack.pop();
 Type rtype  = Parser::tpop(); //??
 dcl_reset();  
 
 Signature::set_fun_name(name);
 Signature *sig = state.get_signature(rtype,true); 
 outside_class = false;
 if (state.class_dcl != t_void) {
   sig->set_class_ptr(state.class_dcl.as_class());
   state.class_dcl = t_void;
   outside_class = true;
 }
 return sig;
}

Type ParserState::signature_type(Type rt)
{
   // *fix 1.1.0 Building up function ptr type (they are declared like any other var...)
 // function pointer declaration!!
	was_fun_ptr = true;
    Signature *sig = get_signature(rt,true);
  // *fix 0.9.4  NB that signature ptrs are unique in fun ptr declarations.
	rt = Type(unique_signature(sig));
    rt.incr_pointer();
    was_fun_ptr = false;
	return rt;
}

PExprList get_default_arguments()
{
 PExprList pel = new ExprList;
 bool gotcha = false;
 ArgList::iterator ali;
 for(ali = state.arg_list.begin(); ali != state.arg_list.end();  ++ali)
    if (ali->init) { pel->push_back(ali->init); gotcha = true; }
    else if (gotcha) error("Default arguments must be at the end");
    else pel->push_back(NULL);
 if (!gotcha) { 
   delete pel; return NULL;
 } else return pel; 
}

static bool mIsConversion = false;

FunctionEntry *
create_function_entry(Signature *sig, const string& name, PEntry pe) 
{
   if (pe==NULL) pe = state.context().add(name);
   mIsConversion = name == CONVERSION_OPNAME;
   pe->type = Type(sig);
   pe->name = name;
   pe->rmode = DIRECT;
   pe->m_typename = false;
   FunctionEntry *pfe = new FunctionEntry(pe);
   pe->data = (int)pfe;
   return pfe;
}

// *add 1.2.8 convenient function for working with signatures
string sig_as_str(Signature* sig, const string& name)
{
    char buff[255];
    Signature::set_fun_name(name);
    ostrstream out(buff,255);
    out << *sig;
    return buff;
}

string class_name(Type t)
{ return t.is_class() ? t.as_class()->name() : string("<unknown>"); }

void error_not_in_class(const string& name, Type class_type)
{
 fail(quotes(name) + " not found in class " +  quotes(class_name(class_type)));
}

// *change 1.2.9 template functions in enclosing scopes are NOT handled specially
// any more. We just have to be careful for the case where a function entry has
// no functions.
// *fix 1.2.9 member templates now work.
FunctionEntry *
lookup_function_entry(Signature *sig, const string& name,bool fn_defn,
                                     Function **fn, int& slot_id)
{ 
 Type method_scope = state.class_dcl;
 state.class_dcl = t_void;
 bool method_body = method_scope != t_void;

 PEntry pe = symbol_lookup(name);  
 mIsConversion = name == CONVERSION_OPNAME;
 FunctionEntry *pfe = NULL, *local_entry = NULL;
 slot_id = 0;
 *fn = NULL;
 if (pe) { // already is an entry
    bool is_function = pe->type.is_signature();   
    Table* cntxt = &state.context(); 
    pfe = (FunctionEntry *)pe->data;

    // but is it local?  In which case we try to overload 
    if (cntxt->is_local_entry(pe)){
     // *add 1.2.8 'main' can't be overloaded...
     if (name == "main") return NULL;
     if (is_function) { 
        *fn = pfe->simple_match(sig);
        if (*fn) { // but not if this signature is already present!
           // if this function is already complete, we can only
           // redeclare it, not redefine it.           
           if ((*fn)->context() && fn_defn) {
               if (!interactive_mode()) { error_already_defined(name); return NULL; }
             else (*fn)->clear(); // ISSUE: again, interactive mode is more lenient
           } 
           // *issue 0.9.5 allows us to actually change the return type; when shd it be an error?
           if ((*fn)->return_type() != sig->return_type()) { *fn = NULL; return NULL; }
           return pfe;  // reuse entry...
        } else // another case: the set is virtual - in which case, this entry
               // is hiding the original virtual entry!
        if (state.in_template != NULL && pfe->size() > 0 && pfe->back()->is_virtual()) {
           pe = cntxt->parent_context()->lookup(name);
           if (pe==NULL) return pfe; // this set was created in the current class...
           if (pe->type.is_signature()) { // continue to find virtual slot....
               local_entry = pfe;                //save local entry
               pfe = (FunctionEntry *)pe->data;  //but look at inherited entry!
           } else return NULL;  // wasn't a function, so ditto
        } else {  // cool - can now try to overload....
        // *fix 1.2.8 except of course, if this is a method body definition;
        // there must be a corresponding method declaration!
        // *fix 1.2.9 unless this is a member template instantiation; the function
        // doesn't exist at this point!
            if (method_body && state.in_template == NULL)
                error_not_in_class(sig_as_str(sig,name),method_scope);
            else return pfe;          
        }
     } else { error_already_defined(name); return NULL; }// and it wasn't a function!
   }
   // *fix 1.2.4 Qualified method name (C::name(...)) where 'name' is not found directly in 'C'
   else if (method_body) error_not_in_class(name,method_scope);

   // an entry in some enclosing scope
   if (is_class(pe->context)) {
       *fn = pfe->simple_match(sig);
       // if a virtual method, get its slot id
       // but always redeclare (hence hiding the original entry)
       if (*fn != NULL) {
         if ((*fn)->is_virtual()) { 
			 slot_id = (*fn)->slot_id();
			 if (slot_id > 512) slot_id = 1;
		 }
         //*HACK* this was very irritating - switch it off while importing...
        // else warning(quotes(name) + " will be hidden");   
       } 
   }
   *fn = NULL; //*????*
   // *fix 1.2.9 The _specific case_ where we're instantiating a function template;
   // the entry already exists!
   if (state.in_template && 
       state.in_template->get_template()->get_entry()->entry() == pe) return pfe;
   else  return local_entry ? local_entry : NULL; // NULL create a new entry please!
 } else // not found!
 if (method_body) {
  error_not_in_class(name,method_scope);
  return NULL;
 } else return NULL;
}   


void ParserState::set_construct_destruct(int ftype)
{
  in_construct_destruct = ftype;
}

Function *generate_function(Signature *sig, FunctionEntry *pfe, int slot_id, bool imported)
{
  PClass pclass = class_context();
  Function *fn = new Function(sig,pfe); 
  //*NOTE* This is somewhat awkward, and needs to be done up 
  // before _anybody else_ sees it!
  if (pclass != NULL && state.modifier != Static && state.in_construct_destruct == IsPlain)
       state.in_construct_destruct = 3;
  fn->set_construct_destruct(state.in_construct_destruct, state.modifier == Explicit);
  state.in_construct_destruct = IsPlain;
  pfe->push_back(fn);
  fn->builtin(imported ? Function::CDECL : Function::UCFN);
  // *change 1.2.3 Flag how this function must be exported as a callback
  fn->export_as_cdecl(state.modifier != Stdcall);

  if (state.modifier==Virtual || slot_id > 0) {     
     if (!pclass) fail("'virtual' not allowed here");
     /// A new virtual method gets a new (non-zero) slot id
     if (slot_id == 0) slot_id = pclass->next_slot();
     fn->slot_id(slot_id); 
	 // *fix 1.1.0 add the slot to the class...
	 pclass->set_slot(fn->slot_id(),fn);             
  }
  state.modifier = None; 

  if (mIsConversion) { 
     if (!pclass) fail("conversion operator must be a method");
     pclass->set_conversion_to(fn);
  }
  //REF:TEMPLATE
  // *fix 0.9.3 Didn't check to see that this was the same template....
  // (got confused during class instantiation)
  if (pfe->get_template() != NULL && state.in_template != NULL
         && ! state.in_template->get_template()->is_class()) {
       fn->set_template(state.in_template);
       fn->get_template()->data(fn);
  }
  return fn;
}

void set_default_args(Function *fn, bool do_clear, bool is_defn=false)
{
  // set any default arguments - will overwrite any currently defined ones!
  PExprList pel = get_default_arguments();
  if (pel) { 
     // *fix 0.9.4 Now complains about default args in fn. defn 
     // *NOTE* We should move this 'not defined' test to Function...
     if (is_defn && fn->fun_block()->pstart == NULL
        && fn->has_default_args()) fail("Cannot redefine default arguments");
     fn->set_default_args(pel);
     delete pel;
  }
  if (do_clear) state.arg_list.clear(); 
}


Table* enclosing_namespace(Table* pt)
{
  while (pt->type() != IS_NAMESPACE && pt != &global())
    pt = pt->parent_context();
  return pt;
}

Function *ParserState::declare_function(Type t,const string& name, PExpr poss_init)
{
 int slot_id, modifier_flags = modifier;  
 in_declaration = false;
 Function* fn = NULL;
 Table* fcntext = NULL;
try {
  Signature *sig = get_signature(t);

  bool was_import = Builtin::get_dll_handle() && poss_init == NULL;
  PClass  class_ptr = class_context();

 // string cname = class_ptr ? class_ptr->name() : ""; //*DEBUG

  if (in_friend_dcl) {  // *add 0.9.5 Friend function declaration
      if (class_ptr == NULL) fail("Friend declaration only allowed in class declaration");
      pop_context(); // to ensure that friend dcl lookup is outside this scope...
  } else if (context().type()==FUNCTION) { // *fix 1.2.6 declaration within function scope
     fcntext = enclosing_namespace(&context());
     push_context(fcntext);
  }

  FunctionEntry *pfe = lookup_function_entry(sig,name,false,&fn, slot_id); 

  if (in_friend_dcl) {  // Parser::state.in_friend_dcl
      push_context(class_ptr);
  } 

  if (!pfe) pfe = create_function_entry(sig,name);
  if (!fn) { // add a new function to the overloaded set!
   fn = generate_function(sig,pfe,slot_id,was_import); 
  }

  if (fcntext) pop_context();

  set_default_args(fn,true);

  if (in_friend_dcl) { // *fix 1.1.0 friend declarations act as forward declarations
     class_ptr->add_friend_object(fn,false);
     in_friend_dcl = false;
     return fn;
  }

  if (state.in_template && class_ptr) { // w/in class template instantiation
   // *add 1.1.0 Any externally defined method template entries must be attached
   // to corresp methods
    TemplateInstance *ti = state.in_template;  // i.e. the class template instance
    const EntryList *pfl = ti->get_template()->get_entry()->method_entries();
    if (pfl) {
       EntryList::const_iterator eli;
       FORALL(eli,*pfl) {
		  PEntry pe = *eli;
		  if (pe->name == name)  // attach the method templates to the methods...
		    pfe->set_template(PFunctionEntry(pe->data)->get_template()); 	      
       }	 
    } 
  }
 
  if (poss_init != NULL) { // *add 0.9.5 pure virtual methods
     if (class_ptr == NULL) fail("must be in class context");
     if (!fn->is_virtual()) fail("method must be virtual");
     if (!poss_init->type().is_zero()) fail("must be zero for pure virtual method");
     class_ptr->make_abstract();
  } else {
  // This prototype may represent a dynamic link to a DLL
  // *fix 1.2.3 Don't try to import a pure virtual method
    if (was_import) {
      string name;
   // *fix 0.9.5 state.modifier is reset - this CONTINUOUSLY gives trouble...
   // *hack 1.1.4 Some libraries have a number of entries which can't be found.
      if (!Builtin::add_dll_function(fn,modifier_flags,name) && ! debug.suppress_link_errors)
        cerr << "cannot link to " + quotes(name) << endl;
    }
  }
 } catch(string msg) { error(msg); }
 // NB to reset state!!
 modifier = None;  
 in_friend_dcl = false;
 return fn;
}

Function *ParserState::start_function(Type t, const string& name,bool init_context, int ftype)
{
 int slot_id, modifier_flags = modifier;
 PEntry pe;
 Function *fn = NULL;
 ArgList by_val_list;
 
 try {  
  PClass  class_ptr = class_context();

  Signature *sig = get_signature(t);
  FunctionEntry *pfe = lookup_function_entry(sig,name,true,&fn,slot_id);
  if (!pfe)  pfe = create_function_entry(sig,name);
  if (!fn)  // add a new function to the overloaded set!
     fn = generate_function(sig,pfe,slot_id,false); 
 
  fn->builtin(ftype);
  if (fn->builtin()) fn->import_scheme(Import::scheme());
  set_default_args(fn,false,true);

#if _DEBUG
  gLastFunction = fn; //*DEBUG*
  if (gDebugBreak) {
      if (gDebugBreak == 2) 
          __break(2);
      else
         gFunBlock = fn->fun_block();
      gDebugBreak = 0;
  }
#endif

   // Create a function context and make it current!
  FunctionContext *fcntxt;
  Table *cntxt = &context();
  if (fn->is_constructor()) fcntxt = new ConstructorContext(cntxt,fn); else
  if (fn->is_destructor())  fcntxt = new DestructorContext(cntxt,fn);
  else fcntxt = new FunctionContext(cntxt,fn);
  if (init_context) fcntxt->initialize();
  else  {
      fn->clear();
      set_function_code(true); // *fix 0.9.5 was not called before constructor init. lists
  }
  push_context(fcntxt);  

  if (class_ptr) { // methods need a 'this' variable!
     Type cpt = class_ptr->entry()->type;
     cpt.incr_pointer();
     //     cpt.make_const();  // *fix 1.2.7
     PEntry p_this = fcntxt->add("this");
     p_this->type = cpt;
     p_this->data = THIS_OFFSET;
  }

  dcl_init_size = 0; 

  // Add any args, ignoring the initializers
  // (1) Passing objects _by value_ is achieved by a dummy ref. argument,
  // which is then used to initialize a proper local object.
  // *add 1.1.0 unless we're importing a function which does need true by value passing! (like MSC++)
  // *add 1.2.6 which may well be conditional on the object size and type (like GCC 3)
  // (2) We pass a hidden first arg for routines returning object values
  // *add 1.2.6 unless this is a function which does true by value return (like GCC 3)

  ImportScheme* import = fn->import_scheme();
  if (t.is_object() && ! (import && import->true_return_by_value(t))) {
     t.make_reference();
     fn->return_object(add_variable(t,"*",NULL,ARG));
  }
  ArgList::iterator ali;
  bool passing_obj_by_value = false;
  // *fix 0.9.2 Template class arguments may be instantiated during add_variable()
  //  The state is restored ok, but the arg_list iterator was no longer valid.
  for(; arg_list.size() > 0; arg_list.pop_front()) {
     Type t = arg_list.front().type;
     string name = arg_list.front().name;
     // *fix 1.2.2b if this name is empty, replace by '*', which means 'create temporary name'
     if (name == "") name = "*";
     if (t.is_object() && ! (import && import->true_pass_by_value(t))) {
       by_val_list.push_back(ArgEntry(t,name,NULL));
       t.make_reference();
       name = "*";
       passing_obj_by_value = true;
     }

     pe = add_variable(t,name,NULL,ARG); 

     if (passing_obj_by_value) {
       by_val_list.back().init = Expressions::entry_op(pe);
       passing_obj_by_value = false;
     }  
  }
  // any objects passed by value are now declared in the function context; these
  // are initialized using the reference parameter created above.
  // *add 1.1.0 except if this was a native call...
  if (by_val_list.size() > 0 && ! fn->builtin()) {
    for(ali = by_val_list.begin(); ali != by_val_list.end(); ++ali)
      add_variable(ali->type,ali->name,ali->init);
    by_val_list.clear();
  }

 } catch(string msg) { error(msg); }  
 class_dcl = t_void;  // just in case...
 return fn;
}

PEntry ParserState::add_typedef(Type t, const string& name)
{
 in_typedef = true;
 PEntry pe = add_variable(t,name,NULL,None);
 in_typedef = false;
 return pe;
}

// this structure makes sure that the state is _always_ properly sorted out
// when we leave add_variable(), even if we just return.
// *fix 1.2.7 this sorts out a problem with extern arrays followed by plain const defs:
//  extern char obuff[BUFFSIZE];
//  const int TOPLEFT = 1;  // blew up because s_array_size was NOT reset

struct PopAtEnd {
    bool m_context_pushed;

    PopAtEnd() : m_context_pushed(false) { }
    void set() { m_context_pushed = true; }
    ~PopAtEnd() {
        if (m_context_pushed) state.pop_context();
        reset_array_size();
    }
};


PEntry ParserState::add_variable(Type t, string name, Expression init, int mode)
{
 PEntry pe;
 bool is_class_context;
 Table* alloc_context;
 static int offset=0, bit_offs=0, field_size;
 if (name=="") return NULL;
 if (name=="obuff") //*DEBUG*
     name = "obuff";
 try {
  PopAtEnd restore_cntxt;
  int size;
// *add 1.2.3 Support for 'static' applied within file scope
// The trick here is to explicitly load this module's anonymous namespace
  bool is_static = mode == ENUM_INIT; 
  if (mode == Static) {
      is_static = true;
      if (is_global(&context())) {
          add_namespace("");
          restore_cntxt.set();
      }
  }
  if (name=="*") name = make_temp_name(); // assign a temporary name...
  if (t == t_null  && ! in_typedef) { // *add 1.1.1 support for __declare    
    if (init == NULL) fail("must initialize a __declare variable");
    t = init->type();
	t.strip_const();   // i.e. we want the base type
	t.make_zero_int(); // *fix 1.1.2 the zero bit must go!
	if (t.is_function()) t.incr_pointer(); // *fix 1.2.0 (Eric) otherwise we crash	
  }
  // _cannot_ use a namespace name as if it were a plain type!
  if (t.is_namespace()) fail("Namespace not allowed here");

  // is our context ready?
  check_context(mode);
  PClass pc = t.is_class() ? t.as_class() : NULL;

  //issue here is that pc->make_available() _usually_ fails because a template class
  //instantiation failed, setting the error(). We need a way around this...
  if (pc != NULL && /*! in_typedef &&*/ ! pc->make_available()) return NULL; //fail("Class not fully defined");

  // *fix 1.1.2 Watch out for bad array init here, except for char arrays
  // *fix 1.2.0 Deduce the size of the array in declarations like 'char p[] = "hello"'
  if (t.is_pointer() && s_array_size != 1) { // a fiddle: means we have an array!     
     bool is_elist = init && init->is_expr_list();     
     if (init && ! is_elist) {
		 if (! t.is_char() || init->type() != t_char_ptr || ! init->is_entry())
		   error("bad array initializer");
		 else if (s_array_size==0) {
		   s_array_size = strlen((char *)init->entry()->global_ptr())+1;
		 }		 
	 } else 
     if (s_array_size == 0) { // hm, may be asking us to guess the array size!
       if (init && is_elist) s_array_size = init->expr_list()->size();       
       else fail("arrays of size zero not allowed"); // *add 1.2.9
     } 
  }
  size = s_array_size*s_twodim_array;  // will be > 1 for arrays!

  // Has this variable already been added?
  pe = context().lookup(name);
  bool already_declared = pe != NULL;
  if (already_declared) { 
     if (context().is_local_entry(pe)) {
      // *fix 0.9.4 Redefinition is not an error if 'extern'; we reuse an entry unless it will be larger
      // *fix 1.2.2 or will be differently initialized...   
       // *fix 1.2.3b If we've already allocated in an extern declaration, then now initialize...
       if (pe->m_access == NotInitialized) {
          pe->rmode = DIRECT;
          is_class_context = false;
          goto rest_of_init;
       }
       if (! in_extern()) error_already_defined(name); 
       else return pe;  // *fix 1.2.3b Don't redeclare extern variables....
       if (pe->type == t && pe->size >= size && !t.is_class() && !init) {
          reset_array_size();
          return pe;
       }
     }
    //*SJD* This is very irritating  - I think it should be optional
    //if (pe->type == t) warning(quotes(name) + ": hidden variable");
  }

  pe = context().add(name);  
  // put any non-tempory variables in the init list
  if (name[0] != '$') dcl_init_list.push_back(pe);
  pe->type = t;
  pe->size = size;

  // extract the base type, if we have an array (1- or 2-D only)
  // *fix 1.2.8 We weren't doing the 2D case properly; double [3][3] had the wrong size!
  if (t.is_pointer() && s_array_size != 1) {
      if (s_array_size < 0)
          error("array size cannot be negative");
      t.decr_pointer(); 
      if (s_twodim_array > 1)
        t.decr_pointer();
  }
  // use the reference size, not the size of what it's refering to!
  if (t.is_reference()) size = sizeof(void *)*pe->size;
                   else size = t.size()*pe->size;
  pe->rmode = is_static ? DIRECT : context().refmode();
  is_static = pe->rmode == DIRECT;  // *fix 1.0.0
  pe->m_typename = false;

  
  //*HACK* 2D arrays are _formally_ allowed in class defs...
  if(pe->rmode == OREL) s_twodim_array = 1;

  alloc_context = NULL;
  if (in_typedef) {
    pe->data = 0;
    pe->m_typename = true;
	if (Module::current()) Module::current()->add_typename(pe);
    return pe;  //*NOTE* not perfect! will mess w/ array_size!
  } else
  if (mode != ARG) { // i.e. default!
    if(s_twodim_array > 1) { // two-dimensional array
       if(pe->rmode != DIRECT) error("can only define static two-dim arrays");
       // leave room for the pointer table...
      size += sizeof(void *)*s_array_size;
    }
    alloc_context = is_static ? &global() : &context(); 
    is_class_context = alloc_context ? is_class(alloc_context) : false;

    // *add 1.1.4 Bit fields are now allowed in structures
    // *fix 1.2.0 Now allocated to ensure 4-byte alignment for the next field, to be consistent with GCC
    // *fix 1.2.3 Bit fields will now fit properly in structures one word in size
    if (init && init->is_const()) {
        if(pe->rmode != OREL || ! pe->type.is_int()) 
            fail("Bit fields must be integer and are only allowed in structs");
        int bit_size = const_int_expr(init->entry());
        if (bit_offs == 0) {
            size = 4 - alloc_context->size() % 4; // room available upto next 4-byte boundary
            offset = alloc_context->alloc(size,NULL);
            field_size = 8*size; // bits left in this field
        } 
        pe->set_bit_field(offset,bit_offs,bit_size);
        bit_offs += bit_size;
        if (bit_offs > field_size) bit_offs = 0;  // have we gone over a boundary?
        init = NULL;
    } else {
   // *add 1.2.0 Support for 8-byte alignment of doubles in structures
       if (is_class_context && debug.class_dword_align == 8 && t == t_double)
         pe->data = alloc_context->alloc_qword();
       else 
         pe->data = alloc_context->alloc(size,NULL);
	   bit_offs = 0;
    }
	// *change 1.1 Stack now grows downwards, so locals have -ve offset.
    // *fix 1.2.2  Important to round up size to nearest dword bounary!
    if (is_local(alloc_context)) pe->data = -pe->data - Allocator::dword_align(size)/sizeof(int);
    if(s_twodim_array > 1) { // two-dimensional array
      void **ptr = (void **)pe->global_ptr();
      void **ptable_begin = ptr + s_array_size;
      // row length in dwords
      int row_size = t.size()*s_twodim_array/sizeof(void**);
      for(int i = 0; i < s_array_size; i++)
        ptr[i] = ptable_begin + row_size*i;
    } else
    if (t.is_class()) {
     if(t.is_object() && pc->get_VMT() != NULL) {    // i.e first dword is for the VMT entry
       (pe->data) += (pe->is_stack_relative() ? 1 : sizeof(int));
     }
    }
  } else 
  if (is_local(&context())) {
    PFBlock fb = ((FunctionContext&)context()).fun_block();
    pe->data = fb->nargs;
    fb->nargs += Table::dword_align(size)/sizeof(int); // ie. in dwords
	return pe;
  } else fail("how is ARG possible here?");
  pe->context = &context();

  // *fix 1.2.3b Don't initialize variables in extern declarations
  if (in_extern()) {
      pe->m_access = NotInitialized;
      return pe;
  }
rest_of_init:

  // no default initialization for class members!
  if (init || !is_class_context) {
   // even if no init, we pass objects through this in case they
   // _may_ need default construction. Don't try to construct in typedefs! 
   if (init || (t.is_object() && mode != DEFER_TEMP/* && !in_typedef*/)) { 
    if (init && (!alloc_context || (is_class_context && mode != ENUM_INIT)))
      fail("initialization not allowed here");
   //*SJD* The context flag needs some work....
      if (mode != ENUM_INIT || !init->is_entry() || !init->type().is_const()) {
        PExpr e = Expressions::initialize_op(pe,init,NULL,mode==TEMP ? 1 : 0);
        if (e != NULL && ! e->is_nil())
			   (is_static ? static_code() : code())
			             .compile(e,in_loop_dcl() ? 0 : DROP_VALUE);       
      } else if (init->is_entry()) {
        pe->data = init->entry()->data;      
      } else fail("bad init");
    }
    else if (t.is_reference() && (mode != ARG && !in_typedef))
       fail("uninitialized reference");
   }
  } catch(string msg) { error(msg); }
  reset_array_size();
  return pe;
}

// This is called after every declaration; used to detect anonymous unions
void ParserState::check_dcl_init(Type t)
{ 
    if (dcl_init_list.size() > 0) { 
        dcl_init_list.clear();
        return;  // there were declarations
    }

    if (! t.is_class()) return;   // only interested in empty class declarations...
    PClass pc = t.as_class();
    if (pc->is_anonymous_union()) { // which are anonymous unions
    // iterate over all non-static members of the union
    // and insert them into the enclosing context.
    // First add a temp variable of the union type to allocate some space...
        PEntry union_entry = add_variable(t,"*");
        Table* cntxt = &context();
        EntryList el;
        EntryList::iterator eli;
        pc->list_entries(el,NON_STATIC | FIELDS);
        for(eli = el.begin(); eli != el.end(); ++eli) {
           PEntry entry = *eli;
           string name = entry->name;
           PEntry new_pe = cntxt->new_entry(name); 
           new_pe->type = entry->type;
           new_pe->size = entry->size;
           new_pe->data = union_entry->data;
           new_pe->rmode = is_class(cntxt) ? OREL : DIRECT;  
           cntxt->add_entry(name,new_pe);           
        }       
    }
}

int array_size(PEntry pe)
{
 return pe->size;
}

int size_of_entry(PEntry pe)
{
 Type t = pe->type;
 // for arrays, use the size of the base type...
 if (array_size(pe) > 1 && t.is_pointer()) { 
      t.decr_pointer(); 
      return array_size(pe)*t.size();
 } else return t.size();
}

int const_int_expr(PEntry pe)
{
// *fix 1.2.1 (Eric) Named enums are also valid integer constants!
// *fix 1.2.1 (Eric) Constants may be short
// *fix 1.2.1 Watch out for unsigned shorts...
  void *ptr = pe->global_ptr();
  Type t = pe->type;
  if (t.is_enum()) return *(int *)ptr;
  if (!t.is_const() || !t.is_int())
     fail("const expr expected, not: " + pe->name);
  if (t.is_short()) {
      if (t.is_unsigned()) return *(unsigned short *)ptr;
      else return *(short *)ptr;
  }
  if (t.is_int())
      return *(int *)ptr;
  else return 0;
}

int const_int_expr(Expression e)
{ 
// *add 1.1.4 Some limited ability to fold compile-time constants
// *add 1.2.5 Added relational ops for #if expressions
 if (! e->is_entry()) {
     if (e->is_function()) fail("Constant expressions cannot contain functions");
	 int op = e->op();
	 int v1 = e->arg1() ? const_int_expr(e->arg1()) : 0;
	 int v2 = e->arg2() ? const_int_expr(e->arg2()) : 0;
	 switch (op) {
	 case LSHIFT: return  v1 << v2;
     case RSHIFT: return  v1 >> v2;
	 case STAR:   return  v1 * v2;
	 case PLUS:   return  v1 + v2;
     case MINUS:  return  v1 - v2;
	 case DIVIDE: return  v1 / v2;
     case BIN_OR: return  v1 | v2;
     case ADDR:   return  v1 & v2;
     case BIN_NOT: return ~v1;
     case LOG_OR:  return v1 || v2;
     case LOG_AND: return v1 && v2;
     case EQUAL:   return v1 == v2;
     case NOT_EQUAL: return v1 != v2;
     case LESS_THAN: return v1 < v2;
     case LEQ:       return v1 <= v2;
     case GREATER:   return v1 > v2;
     case GEQ:       return v1 >= v2;
	 default: fail("Must be a simple constant integer expression");	 
     }  
 }
 // plain entry case...
 return const_int_expr(Expressions::ExprToEntry(e));
}

void *create_const_entry(Type t, PEntry& rpe)
{ 
  PEntry pe = new Entry;
  pe->rmode = DIRECT;
  t.make_const();
  pe->type = t;
  pe->size = 1;  // by default; for arrays it's > 1!!
  pe->m_typename = false;
  pe->data = global().alloc(t.size(),NULL);
  rpe = pe;
  return pe->global_ptr();
}

//////////////////////// Enumerations /////////////////////////////////
// *fix 1.1.4
// * Enums can now be parts of declarations, and I've now made
// it possible for the optional initialization to be an earlier enum constant,
// so that 'enum {ONE,TWO=ONE+1}' will now work.
// * C mode works like w/ structs!

static int gVal;
static Enum* gEnum;

Type ParserState::do_enum(string name)
{
try {
  Enum *enum_obj;
  Type t_const_int = t_int;   t_const_int.make_const();
  Type et = t_const_int;
  gVal = 0;
  // True nameless enums are like 'enum {...}' and just introduce a set of constants.
  // If however we get 'typedef enum {...} tag' then it is _not_ nameless
  // and we need a distinct name, rather like the equivalent w/ struct.
  bool nameless = name=="" && ! in_typedef;
  if (nameless) name="?enum";                 // entry name of nameless enum
  else if (name=="") name = make_temp_name(); // assign a temporary name... 
  else if (debug.c_mode) name = "$_" + name; 
  //PEntry pe = symbol_lookup(name);
  // *fix 1.2.7 _only_ look in local context; 
  // a previously defined enum wd collide w/ a definition w/in a class
  PEntry pe = context().lookup(name,false);
  // *hack 1.1.4 this test in C mode interferes w/ old-fashioned 'enum Name var'
  if (! debug.c_mode && pe && !nameless) fail("already defined"); 
  if (! pe) {
     pe = context().add(name);
     pe->rmode = DIRECT;
     pe->m_typename = true;
     enum_obj = new Enum(name);
	 enum_obj->entry(pe);          // *add 1.2.7 Enum now carries entry...
     pe->data = global().alloc(sizeof(Enum),enum_obj);
     if (!nameless) { 
         et = Type(enum_obj); 
         Module::current()->add_typename(pe);  // will need to clean these entries out...
     }
     pe->type = et;
  } else 
 // *fix 1.2.7 The enum object is via a _direct_ reference!
      enum_obj = (Enum *)global().addr(pe->data); 
   gEnum = enum_obj;

   return pe->type;
  } catch(string msg) { error(msg); }
  return t_void;
}


void ParserState::add_enum(Type et, string name, PExpr init)  
{
  // Add a constant to the current enumeration!
  // *fix 1.2.1 (Eric) Simplified, and no longer mistreats shorts...
  bool was_in_typedef = in_typedef;  
  PEntry pe_entry,entry;
  in_typedef = false;
  try {
   if (init) 
      gVal = const_int_expr(init);
   *(int *)create_const_entry(t_int, pe_entry) = gVal++;
   entry = add_variable(et,name,Expressions::entry_op(pe_entry),ENUM_INIT);  
   gEnum->add_entry(entry);
  } catch(string msg) 
  { error(msg); }   
  in_typedef = was_in_typedef;
}

/////////////////////////////// Exception handling....//////////////////////////////
void do_start_try_block()
{
 Function *curr_fn = current_function();
 if (curr_fn == NULL){ error("try..catch only allowed in functions"); return; }

 CatchHandler *pch = new CatchHandler(curr_fn->fun_block());
 state.mTryStack.push(pch);
 // we now create an auto object in this block, using the catch handler type
 Type tbc (try_block_class());
 PEntry pe = state.add_variable(tbc,"_tbo_",
	                          Expressions::constant_op(t_int,(int)pch));
 pe->context->first_obj(pe);
}

void do_end_try_catch_block(bool is_final)
{
 Label *end = state.mTryStack.TOS()->label();
 if (!is_final) code().jump(JMP,end);
 else {
 // Get rid of the JMP from the last block
  if (code().last_pi()->opcode == JMP) 
     code().backspace();
  end->here();
  state.mTryStack.pop();
 }
}

PEntry mExceptObj;

void do_start_catch_block()
{
using namespace Expressions;
 Signature *sig = state.get_signature(t_void);
 bool catch_all = sig->stdarg();
 if ( (sig->size() != 1 && !catch_all) || (sig->size() != 0 && catch_all))
     error("Catch block has only one parameter!");
 else {
  Type t;
  if (!catch_all) t = *sig->begin(); else t = t_void;
  CatchHandler *pch = state.mTryStack.TOS();
  t.strip_qualifiers();
  pch->add_catch_block(t,code().ip_offset());
  if(t.is_object()) t.make_reference(); // a hack - we relax usu. strict semantics here
  if (!catch_all) {
    PExpr eco = entry_op(mExceptObj);
    // HACK05 - force the exception data to behave like a _pointer_
    if (t.is_class() && !t.is_pointer()) {
        Type pt = t;  pt.incr_pointer();
        eco = deref_op(typecast_op(0,pt,eco),false);
    }
    state.add_variable(t,state.arg_list.begin()->name,eco);
  }
 }
 state.end_args();
}

void do_throw(PExpr e)
{
 Type t; 
 if (e != NULL) {
  t = e->type();
  // Here's the big fiddle: when we throw objects we create them
  // dynamically! We copy references as well since they are often
  // dubious. HACK05
  if (t.is_class() && !t.is_pointer()) 
     e = Expressions::new_op(t,NULL,Expressions::expr_list(e));
  code().compile(e);    
 } else { // if we re-raise the exception, then we mark it w/ t_void!
   code().emit_push_int(0);
   t = t_void;
 }  
 t.strip_qualifiers();
 code().emit_except_throw(t);
}


static PExpr mXInsert = NULL, mXEndl = NULL;

// *add 1.2.5 Support for immediate evaluation of constant expressions (for #if)

void set_expression_handler(ExprFun f)
{
  s_expr_handler = f;
}

bool expression_end(PExpr e)
{
try {
 // only execute immediately if not a controlled expression statement,
 // and not within a function.
 if (s_expr_handler) (*s_expr_handler)(e);
 else
 if (!IEF && block_depth() == 0) {

   if (debug.dump_expr) { 
     Expressions::dump(cmsg,e);  cmsg << endl;
   }
   // insert an appropriate dump instruction if non-void   
   Type t = e->type();
//  /*
   // *SJD* This feature is irritating and needs a rethink!!
   // Basic idea is to build up the symbolic equiv of 'cout << obj << endl'
   // *fix 1.1.2 The usual error is now silent, and a newline is output.
   // We don't try to do class ptrs for the mo. I am not sure whether
   // this is the best default behaviour, so I've left it in this ugly 
   // state!
   // *change 1.2.8 don't do this when the debugger is driven externally.
   if (mOCout != NULL && (t.is_class() || t.is_enum()) && ! debug.interactive_debugging) {
     PExpr pout;
     if (t.is_class() &&  t.as_class()->name() != "string") {
       pout = Expressions::function_op(mXInsert,
                                  Expressions::expr_list(mOCout,e),true);
       if (t.is_pointer()) {
		    Type t2 = pout->arg1()->arg_list()->back()->type();
			if (t2.is_void()) goto default_case;
       }  
       if (pout && ! pout->is_nil()) { // append '<< endl' expression
		   pout = Expressions::append_op (pout,
			       Expressions::function_op(mXInsert,
			         Expressions::expr_list(mOCout,mXEndl)));                     
         code().compile(pout,DROP_VALUE);
         return true;
       }
     }
   }
default_case:
   // *fix 1.2.1 (Eric's double evaluation bug) Expressions with object values were compiled twice!
   code().compile(e); 
   if (t != t_void) {
       int tinfo, op;
       if (e->is_variable()) { op = SHOWV; tinfo = (int)e->entry(); }
       else  {  op = SHOWI;  tinfo = AsTType(t); }
       code().emit(op,DIRECT,global().alloc_int(tinfo));
   }
 } else // ensure that this statement expression does not leave stack droppings!
   code().compile(e,DROP_VALUE);
 }
 catch(string msg) { error(msg); }
 catch(...) { fail("expr compile failed"); }
 return true;
}

Instruction *immediate_code_ptr()        { return mCodeBlock.pstart; }
void immediate_code_ptr(Instruction *pi) { mCodeBlock.pstart = pi; }
FBlock *immediate_code_block() { return &mCodeBlock; }

bool check_temp_context()
{
  if (mpTempContext->size() > 0) {
    mpTempContext->finalize();
    mpTempContext->clear();
    return true;
   }
   return false;
}

void finalize_temp_code(bool do_dissemble, Type rt)
{
// *add 1.1.0 can (optionally) return an integer so that
// the result of main() can be passed back...
	code().emit_return(rt);
    if (mCodeBlock.pstart && ! Engine::paused()) 
    try { // sometimes (utils.uc) we get trouble doing this...
      delete mCodeBlock.pstart;
    } catch(...) {
     mCodeBlock.pstart = 0;
    }
    mCodeBlock.pstart = code().end_code();
    if (do_dissemble) dissemble(&mCodeBlock);    
}

int exec_temp_code(int flags,ArgBlock *xargs)
{
  return Engine::execute(&mCodeBlock,flags,xargs);
}

bool statement_end()
{
  bool code_generated = code().total_instructions() > 0;
  state.modifier = None; // *fix 0.9.4 Was not reset at end of statement!
  if (code_generated) {
      check_temp_context();
  } else return true;

  // *fix 1.2.8 need to allow temporary code to execute before
  // main() is called in batch mode.
  // *change 1.2.9 we use the loaded module list to execute 
  // initialization code, controlled by this flag
  // interactive_mode() ?
 if (! Parser::debug.compile_program && block_depth() == 0) {
    finalize_temp_code(debug.auto_dissemble,t_void);
    exec_temp_code();
	next_statement();
  }
  return true;
}

void dump_function(PEntry pe, bool do_dissemble, int idx, ostream& os); // later...

void dump_expression(Type t, void *ptr, PEntry pe)
{
  if (t==t_void) t = pe->type;
  // *change 1.2.8 functions are dumped as full prototypes
  if (t.is_signature() && ! t.is_pointer()) {
    dump_function(pe,false,-1,cmsg);
  } else {
    cmsg << '(' << t << ") ";
    if (pe) cmsg << pe->name << " = ";
    try {
      cmsg << t.value_as_string(ptr);
	} catch(...) {
      cmsg << "???";   
	}
	cmsg << endl;
  }  
}

PExpr get_object_return_expr()
{
  return mObjectReturnExpr;
}

Namespace* lookup_namespace(char *name)
{
 PEntry pe = state.context().lookup(name);
 if (pe && pe->type.is_class()) return (Namespace*)pe->data;
 else return NULL;
}

PEntry add_std_exception(char *name)
{
  PEntry pe = state.context().lookup(name);
  if (pe && pe->type.is_class()) { 
     Type t = pe->type.as_class();
     return state.add_variable(t,"*");
  }                       
  else return NULL;
}

PExpr get_entry_expr(char *name)
{
 PEntry pe = state.context().lookup(name);
 if (pe != NULL) return Expressions::entry_op(pe);
            else return NULL;
}

void init_lib()
// this is called after defs.h is loaded
// if <uc_except.h> (in the usu <classlib.h> is not included,
// then these guys are not found and you cannot catch hardware exceptions.
{
 mEIntDivByZero = add_std_exception("IntDivByZero");
 mEFloatDivByZero = add_std_exception("FloatDivByZero");
 mEAccessViolation = add_std_exception("AccessViolation");
 mEException = add_std_exception("Exception");
 mERangeError = add_std_exception("RangeError");
 mOCout = get_entry_expr("cout");
 mXInsert = get_entry_expr("<<");
 mXEndl = get_entry_expr("endl");

 
}

// *hack 1.1.4 C programmers will of course use any available keywords,
// whether C++ likes it or not.
// *fix 1.2.0 only actually switches mode if necessary
void set_c_mode(bool yesno)
{
  if (yesno == debug.c_mode) return;  
  debug.c_mode = yesno;
  if (yesno) {
	  TokenStream::macro_subst("delete","delete_");
      TokenStream::macro_subst("new","new_");
  } else {
	  TokenStream::macro_delete("delete");
      TokenStream::macro_delete("new");
  }
}

void init()
{
 mpTempContext = new LocalContext(&mGlobal,NULL);
 mpTempContext->set_unwind(1); // i.e. use temporary unwind stack
 state.class_dcl = t_void;
 state.scope_context = NULL;
 state.scope_context_stack.clear();
 state.context_stack.push(NULL); 
 state.context_stack.push(&mGlobal);

 state.token_stack.push("*"); // so we know if we've gone over!
 s_array_size = 1; //*SJD* Temporary hack!

 state.reset();

 debug.dump_expr = false;
 debug.c_mode = false;
 debug.use_typedef_names = false;
 debug.suppress_link_errors = false;
 debug.auto_dissemble = false;
 debug.auto_exec = true;
 debug.strict = false;
 debug.skip_method_bodies = true; 
 #ifdef _DEBUG
 debug.function_trace = true;
 debug.ptr_check = true;  
 #else
 debug.function_trace = true; // *change 1.2.4 Now controls stack trace on crash!
 debug.ptr_check = false; 
 #endif
 debug.class_dword_align = 4;
 debug.verbose = false;

 debug.no_access_control = false;
 debug.do_overalloc = true;
 debug.attempt_inline = false; // *add 1.2.3a New Optimization flag
 debug.range_check = false; // *add 1.2.5 range checking
 debug.no_trace_std = false;  // *add 1.2.7  Can choose not to trace through any std:: function
 debug.interactive_debugging = false;  // *add 1.2.8 switch on if running remotely
 debug.errors_as_break = false;        // *add 1.2.8 override usual function stack unwind
 debug.compile_program = false;        // *add 1.2.9

 mCodeBlock.pstart = NULL;
 mCodeBlock.nlocal = 100; //*SJD* A complete _fiddle_
 mCodeBlock.ntotal = mCodeBlock.nlocal;
 PEntry pe = new Entry;
 pe->name = "<temp>";
 pe->data = 0;   // the _only_ FBlock with an empty FunctionEntry!
 mCodeBlock.entry = pe;

 //temporary
 dissembler_init();
 UCContext::init();
 Builtin::init();
 global().alloc(sizeof(int),NULL); // skip first word of DIRECT address space
 mExceptObj = state.add_variable(t_int,"_xo_",NULL,None);
// *add 1.2.6 this points to two words used for object return-by-value, if required.
 PEntry obj_ret = state.add_variable(t_double,"_obj_",NULL,None);
 gObjectReturnPtr = mGlobal.addr(obj_ret->data);
 mObjectReturnExpr = Expressions::entry_op(obj_ret);

 Engine::set_data_seg(mGlobal.addr(0));
 Module::add_namespace(&mGlobal);
 set_function_code(false);

 mCatchHandlerObj = Class::generate_try_block_handler();

 // *add 1.2.7 Keep track of std namespace explicitly, by pre-creating it
 // and storing its context.
 state.add_namespace("std");
 state.pop_context();
 sStdNamespace = lookup_namespace("std");

 // block_stack.push(CONTEXT_PUSHED);

 // Add the bool constants true & false 
 // *SJD* Strictly speaking, these guys should be keywords, but what the hell
 Type t_const_bool = t_bool;
 t_const_bool.make_const();
 state.add_variable(t_const_bool,"true",Expressions::constant_op(t_int,1),ENUM_INIT);
 state.add_variable(t_const_bool,"false",Expressions::constant_op(t_int,0),ENUM_INIT);

 // *add 1.1.1 support for __declare
 state.add_typedef(t_null,"__declare");
}

// ***** DIAGNOSTICS *******
void test_match()
{
 state.in_match = false;
}

void dump_function(PEntry pe, bool do_dissemble, int idx, ostream& os)
{
   FunctionEntry *pfe = (FunctionEntry *)pe->data;
   FBlock *fb = NULL;
   // *fix 1.2.9 we were blowing up when showing function templates with no instances
   if (pfe->size()==0) os << "<no instances" << endl;
   else {
     if (idx > -1) fb = pfe->nth_fun(idx)->fun_block();
     if (!do_dissemble) pfe->dump(os);
     if (do_dissemble && fb && fb->pstart) {
       os << "nargs = " << fb->nargs << " nlocal = " << fb->nlocal << endl;
       dissemble(fb);
       os << "at address " << fb << endl;
     }
   }
}

void dump_var(PEntry pe)
{
// *fix 1.2.0 Cleaned up function display
 if (!pe) {
    state.context().dump_entries(cout,1);
    return;
 } 
 
 Type t = pe->type;
 bool class_entry = is_class(pe->context);
 if (class_entry && pe->m_typename) {
    t.as_class()->dump_entries(cout,1);
    return;   
 }
 string prefix = class_entry ? PClass(pe->context)->name() + "::" : string("");
 cout << "VAR ";
 if (t.is_signature()) Signature::set_fun_name("");
 if (! t.is_function()) {
  try {
    cout << pe->type; 
  } catch(...) {
   cout << "<void>";
  }
 }
 cout << ' ' << prefix << pe->name 
      << " size " << size_of_entry(pe)
      << " offset " << pe->data << endl;

 if (t.is_function()) {
   dump_function(pe,debug.auto_dissemble,1,cout);
   return;
 }
 string res;
 bool do_dump = false;
 try {
 // *LINUX* CATCH_SIGNALS
    if (pe->type.is_class()) {
         cout << "CLASS(" << pe->type.as_class()->name() << ") ";
         pe->type.as_class()->dump_entries(cout,0); 
    } else
    if (pe->rmode==DIRECT) {
     void *ptr = pe->global_ptr();
     // *fix 1.0.0 dumping arrays of characters should now work...
     if (size_of_entry(pe) > 1) ptr = (void *) &ptr;
     res = t.value_as_string(ptr);
     if (res != "") { 
       cout << res;
       do_dump = true; 
     }
   }
  if (do_dump) cout << " was value" << endl;
 } catch(...) { warning("problem with dumping value...."); }
}
////        
} // namespace Parser

