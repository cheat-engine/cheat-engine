/* COMMON.H
 * common stuff between grammar and support code
*/
#ifndef __COMMON_H
#define __COMMON_H
#include "types.h"
#include "table.h"
#include "templates.h"
#include "stack.h"

// in parser.y
void dcl_set(bool yes = true, bool comma_flag = true);
void dcl_reset();
bool in_arg_list();

// in common.cpp
void error(string msg);
void fail(const string& msg);
void warning(string msg);
string itos(int);

// in lexer.cpp
int current_lineno();
string current_filename();

#include "expressions.h"
#include "code.h"

const int ARG = 101, ENUM_INIT = 102, TEMP = 103, DEFER_TEMP = 104;
const int FAIL=-1, OK=0, PENDING=1, HALTED=2, CRASHED=3, RUNNING=1, FINISHED=4;
const int THIS_OFFSET = -999;
// was $t
#define CONVERSION_OPNAME "__T__"

enum { IsPlain,IsConstructor,IsDestructor };
enum { None, Static, Virtual, Stdcall, Api, Explicit, Immediate, UseAddr };

namespace Parser {

typedef PExpr Expression;

typedef Stack<string,20> TokenStack;
typedef Stack<Table *,40> ContextStack;

enum { PLAIN_BLOCK, CONSTRUCTOR_BLOCK, CLASS_BLOCK, LOOP_BLOCK };

struct ArgEntry {
   Type type; string name; Expression init;
   ArgEntry(Type _type, string _name, Expression _init)
    : type(_type),name(_name),init(_init)
    {}
};
typedef std::list<ArgEntry> ArgList;

const int BLOCK_DEPTH = 40;
typedef Stack<Label *,BLOCK_DEPTH> LabelStack;
typedef Stack<UCContext*,10> CodeStack;
typedef Stack<Table*,5> ScopeStack;

typedef void (*ExprFun)(PExpr e);

struct ParserState { 
  bool in_match;
  bool was_fun_ptr;
  bool in_declaration;
  bool in_typedef;
  bool in_friend_dcl,in_explicit_constructor;
  bool in_class;
  bool in_method;
  bool in_loop;
  bool in_switch;
  int in_construct_destruct;
  int modifier;
  bool member_is_const;
  bool extern_flag;
  bool extern_flag_C;
  Type class_dcl;
  TemplateInstance *in_template;
  ArgList arg_list;
  UCContext m_FContext,m_SContext;
  UCContext *m_PCode;
  CodeStack m_code_stack;
  EntryList dcl_init_list;
  ScopeStack scope_context_stack;
  int dcl_init_size;

  // error info
  string err;
  string file;
  int    lineno;

  Stack<Type,50> tstack;
  Stack<int,BLOCK_DEPTH> block_stack;
  Stack<CatchHandler *,10> mTryStack;
  Stack<SwitchBlock *,10> switch_stack;
  LabelStack label_stack, continue_stack, break_stack;

  Table *scope_context;
  TokenStack token_stack;
  ContextStack context_stack;
  Label *m_continue;
  Label *m_ret_label;
  LabelStack m_ret_label_stack;
  // methods
  ParserState() { reset(); }
  void reset();
  Label *return_label();
  void push_context(Table *t);
  Table *pop_context()             { return context_stack.pop();   } 
  Table& context();
  
  //*NOTE* the 'int init' will be something like 'Expr init'
  void init_block(int type);
  void finalize_block();
  bool context_generated();
  void check_context(int mode);
  bool handle_method_body(Class* pc, bool plain_method);
  bool in_loop_dcl();
  bool begin_scope(Type t);
  void end_scope();
  void add_to_arg_list(Type t, const string& name, Expression init);
  Signature *get_signature(Type t, bool clear_arglist=false);
  Type signature_type(Type rt);
  void begin_args();
  void end_args();
  PEntry add_typedef(Type t, const string& name);
  PEntry add_variable(Type t, string name, Expression init=NULL, int mode=Immediate);
  void check_dcl_init(Type t);
  //Type type_of_class(string name);
  Type add_class(int s_or_c, const string& name, int deriv_access, Type base_class);
  void add_friend_class(const string& name);
  void add_namespace(string name);
  void set_access_mode(int val);
  void set_construct_destruct(int ftype);
  Function *declare_function(Type t, const string& name, PExpr poss_init = NULL);
  Function *start_function(Type t, const string& name, bool init_context=true, int ftype=Function::UCFN); 
  Type do_enum(string name);
  void add_enum(Type et, string name, PExpr init);  
  void begin_templ_list();
  void end_templ_list();


};

int const_int_expr(Expression e);
int const_int_expr(PEntry pe);


extern ParserState state;
extern PEntry mEIntDivByZero,mEAccessViolation,mEFloatDivByZero,mEException;
extern PEntry mExceptObj,mERangeError;

struct DebugFlags {
   bool dump_expr;
   bool auto_dissemble;
   bool auto_exec;
   bool function_trace;
   int  class_dword_align;
   bool verbose;
   bool strict;
   bool ptr_check;
   bool no_access_control;   
   bool do_overalloc;
   bool c_mode;
   bool use_typedef_names;
   bool suppress_link_errors; 
   bool attempt_inline;         // *add 1.2.3a New Optimization flag - inlines one-instruction functions
   bool range_check;            // *add 1.2.5  range checking: _range_check
   bool skip_method_bodies;     // *add 1.2.7  Defering compilation of method bodies
   bool no_trace_std;           // *add 1.2.7  Can choose not to trace through any std:: function
   bool interactive_debugging;  // *add 1.2.8  switch on if running remotely
   bool errors_as_break;        // *add 1.2.8  override usual function stack unwind
   bool compile_program;        // *add 1.2.9  true program compile mode (uses program ODL etc)
 };
extern DebugFlags debug;
bool set_alignment(int ipack);

// We cannot use the Type class directly as a BISON expression type,
// but here shamelessly exploit its equivalence to a long value.
typedef long TType;
inline Type& AsType(const TType& t) { return *(Type *)&t; }
inline TType& AsTType(const Type& t) { return *(TType *)&t; }
void           tpush(Type t);
Type&          tots();
Type           tpop();
TType          ttots();
void           stots(TType tt);
TType          make_ref(TType t);
TType          incr_ptr(TType t);
TType          make_unsigned(TType t);
TType          make_array(TType t, Expression psz);
void *         ExprToPtr(Expression e);

void out(char *s); 
void outln(char *s); 

Global&        global();
Table*         std_namespace();
LocalContext*  temp_context();
FBlock*        temp_fun_block();
Function*      current_function(Table *context=NULL);
FunctionEntry* create_function_entry(Signature *sig, const string& name, PEntry pe = NULL);
PExpr          get_object_return_expr();
PClass         try_block_class();

void init_lib();
void init();
void set_c_mode(bool yesno);
int block_depth();
const int C = 1;
bool in_extern();
bool in_extern_C();
bool is_global_entry(PEntry pe);
bool is_class_entry(PEntry pe);
bool is_class(Table *pc);
bool is_namespace(Table *pc);
int array_size(PEntry pe); 
int size_of_entry(PEntry pe);
void *create_const_entry(Type t, PEntry& rpe);
string quotes(const string& s);
PEntry symbol_lookup(const string& name);
Signature *get_prototype(bool& outside_class);
inline bool is_in_declaration()
 { return state.in_declaration; }

Instruction *immediate_code_ptr();
void immediate_code_ptr(Instruction *pi);

void restore_global_context();
bool in_class_context();
void finalize_temp_code(bool do_dissemble, Type rt);
int  exec_temp_code(int flags=0, ArgBlock *xargs=NULL);
bool expression_end(PExpr e);
bool check_temp_context();
bool statement_end();
bool check_temp_context();
void dump_expression(Type t, void *ptr, PEntry pe=NULL);
void set_function_code(bool is_fn);
bool is_function_code();
UCContext& code();
void push_label_stack();
void label_here();
bool do_loop_start(PExpr cond, bool jump_back);
bool do_loop_end(bool is_jump, bool is_forward=false);
bool do_for_end(PExpr e);
bool do_do_end(PExpr cond);
bool do_return(PExpr e);
bool do_break_continue(int keyword);
void do_switch(PExpr e);
bool do_case(PExpr e);
void label_jump(int jtype, bool do_push);

void do_start_try_block();
void do_end_try_catch_block(bool is_final);
void do_start_catch_block();
void do_throw(PExpr e);

void do_goto(char* label_name);
void goto_label_new(char* name);
void goto_label_existing(PEntry pe);




void test_match();
void dump_var(PEntry pe);
void dump_function(PEntry pe, bool do_dissemble, int idx, ostream& os);

void set_expression_handler(ExprFun f);
}


#endif

