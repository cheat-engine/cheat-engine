/* FUNCTION.CPP
 * Support for functions: classes: Function,FunctionEntry,LocalContext
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
*/
#include "common.h"
#include "opcodes.h"
#include "module.h"
#include <ctype.h>
#include "directcall.h"
#include "input.h"

#ifdef __GNUC__
#include <strstream.h>
#endif

const int MAX_FUN_NAME = 256;

//#include "function.h"
//#include "expressions.h"

namespace { // private stuff
 string s_fun_name = "<none>";
 bool s_ctor = false, s_dtor = false;

 inline int max(int i, int j)
{ return i > j ? i : j; }

};

// Line Number Management

typedef std::list<LineInfo> LineInfoList;

// this overrides the virtual base class
class LineNumbers: public LineNumbersB {
private:
    LineInfoList m_li;
    int m_module;
public:
   void add(const LineInfo& li) // override
   {
     m_li.push_back(li);
   }

   // *fix 1.2.7 necessary to look for the next line, if we match exactly
   bool lookup_line(LineInfo& li, bool find_line) // override
   {
       LineInfoList::iterator lili, lili_end = m_li.end();
       for(lili = m_li.begin(); lili != lili_end; lili++)
           if (find_line && lili->ip_offset >= li.ip_offset) {
               if (lili->ip_offset == li.ip_offset) {
                   lili++;
                   if (lili == lili_end) return false; //*ALWAYS?*
               }
               li = *lili;
               return true;
           } else
           if (!find_line && lili->line >= li.line) {
               li = *lili;
               return true;
           }         
       return false;
   }

   // *add 1.2.7    
   void dump(ostream& out)
   {
       LineInfoList::iterator lili;
       for(lili = m_li.begin(); lili != m_li.end(); lili++)
           out << lili->line << ' ' << lili->ip_offset << ' ' << lili->file << endl;
   }

   
   int  lookup_ip(int line) // override
   {
    LineInfo li;
    li.line = line;
    if (lookup_line(li,false)) return li.ip_offset;
    else return 0;
   }

   void lookup_range(int& l1, int& l2)
   {
       LineInfoList::iterator lili;
       if (m_li.size()==0) 
	   {
		   l1 = l2 = -1; 
	   }
	   l1 = m_li.front().line;
	   l2 = m_li.back().line;
   } 

   LineNumbers() : m_module(-1) {}

   void module(int id)     { m_module = id;   }
   int  module()           { return m_module; }

};

static bool s_full_names = true;

void Signature::set_fun_name(const string& name, bool is_ctor, bool is_dtor)
{
  s_ctor = is_ctor;
  s_dtor = is_dtor;
  s_fun_name = name;
}

string Signature::get_fun_name()
{ return s_fun_name; }

void Signature::write_qualified_names(bool yesno)
{ s_full_names = yesno; }

// miscelaneous output routines
ostream& operator << (ostream& os, Signature& sig)
{
 Signature::iterator si;
 StringList::iterator sli;
 StringList *args = sig.get_arg_names();
 if (args && args->size()==0) args = NULL;
 if (!s_ctor && !s_dtor)  // *fix 1.1.2 these don't have return types!
   os << sig.return_type() << ' ';  
 string na = s_fun_name;
 char first = na[0];
 // *fix 1.2.7 wasn't outputing 'operator' in front of '()'
 if (!isalpha(first) && first != '_' /*&& first != '('*/) na = "operator" + na;
 PClass pc = sig.class_ptr();
 if (pc != NULL) {
    string classn = pc->name();
    if (! s_full_names && pc->is_template()) classn = pc->get_template()->name();
    if (s_ctor)  na = classn; else
	if (s_dtor)  na = "~" + classn;
    if (s_full_names) na = classn + "::" + na;
 }
 os << na << '(';
 if (args != NULL) sli = args->begin();
 for(si = sig.begin(); si != sig.end(); ) {
   os << *si++;
   if (args != NULL) os << ' ' << *sli++;
   if (si != sig.end()) os << ',';
 }
 os << ')';
 if (sig.is_const()) os << " const";
 return os;
}

ostream& operator <<(ostream& os, Function& f)
{
  f.dump(os);
  return os;
}


LocalContext::LocalContext(Table *parent, Function *f) 
  : Table(parent,SREL,0),m_function(f), m_no_auto_dtor(false)
{
    m_type = FUNCTION;
    set_mem_unit(4);     // we work in DWORDS
    set_dword_align(4);  // *fix 1.2.3 Local contexts have 32-bit alignment _explicitly_
    set_parent(parent);
}

int LocalContext::alloc(int sz, void *data)
{
// remember to keep the parent updated!
 if (m_parent->type()==FUNCTION) m_parent->alloc(sz,NULL);
 return Table::alloc(sz,data);
}

void LocalContext::set_parent(Table *parent)
{
    m_parent = parent;  // *fix 0.9.5 m_parent was set _after_ calling update_parent!

   // start off where our parent ended....
    if (parent->type()== FUNCTION) {
       if (m_data == 0) m_data = (char *)parent->size();
    }
}

bool LocalContext::check_objects(bool do_clear)
{
    // Any local objects requiring destruction must be unwound...
    if (m_first_obj != NULL) {
      Parser::code().emit(UNWIND,SREL,m_first_obj->data);
      if (do_clear) m_first_obj = NULL;
      return true;
    } else return false;
}

void LocalContext::finalize()
{
	check_objects();
}

void LocalContext::add_line_no(const string& file, int line) // override
{
  LineInfo li;
  if (function() == NULL) return;  // local context in interactive mode
  li.file = file;
  li.cntxt = this;
  li.line = line;
  li.ip_offset = Parser::code().ip_offset();
  // *fix 1.2.7 We were adding line numbers in interactive debugging mode.
  // So check that the module id has been set (which only happens on finalization of context)
  LineNumbersB* lnb = function()->line_nos();
  if (lnb->module() == -1) lnb->add(li);
}

FunctionContext::FunctionContext(Table *parent, Function *f)
 : LocalContext(parent,f)
{
}

FBlock *
FunctionContext::fun_block()
{
 return function()->fun_block();
}

void FunctionContext::initialize()
{
 Function *fn = function();
 fn->attach_context(this);
 
 FBlock *fb = fun_block();
 if (fb->pstart) delete fb->pstart;  // may be previously defined!
 fb->pstart = 0;
 fb->nargs = 0;                      // crucial to clear this!

// and switch to the function compile context!
 Parser::set_function_code(true);

 // *change 1.2.9 This is now retired; this is called explicitly before main()
 // *fix 1.2.5 Ensure that any library initializations occur before a program is run!
 //if (fn->name()=="main") {
 //  Parser::code().compile(Expressions::function_call("_init_lib",NULL),DROP_VALUE);
 //}
}


void FunctionContext::finalize()
{
 UCContext& code = Parser::code();
 // if the last instruction was a trival RETURN jump, ignore it!
 int op = code.last_pi()->opcode;
 Label *ret_lbl = Parser::state.return_label();
 if (op == JMP && code.last_pi()->data == code.ip_offset()) {
   ret_lbl->remove(code.last_pi());  // take out this JMP
   code.backspace();                 // skip back
   ret_lbl->here();                  // repatch all other instances of return jump
 } else
 if (ret_lbl && op == UNWIND) ret_lbl->here(-1);

 LocalContext::finalize();

 // *fix 1.1.0 Finalizing the context often generates an UNWIND;
 // ensure that all returns jump to it first!
 if (ret_lbl && code.last_pi()->opcode == UNWIND) ret_lbl->here(-1);
 
 // appropriate RETx instruction
 code.emit_return(m_function->return_type());

 fun_block()->finalize(size());

  // and switch to back to the static compile context...
 Parser::set_function_code(false);

 // update the module structure with our line number range
 int lstart, lend;
 function()->line_nos()->lookup_range(lstart,lend);
 if (lstart < 0 || lend < 0)
     lstart = 0;
 Module *pm = Module::current();
 if(pm) {
     pm->add_function(function(),lstart,lend);
     function()->line_nos()->module(pm->id());
 } else function()->line_nos()->module(0);
}

bool  FunctionContext::ip_to_line(LineInfo& li)
{
    return function()->line_nos()->lookup_line(li,true);
}

ConstructorContext::ConstructorContext(Table *parent, Function *fun)
 : FunctionContext(parent,fun)
{
}

void ConstructorContext::initialize()
{
 FunctionContext::initialize();
 if (!function()->builtin()) function()->class_context()->auto_construct(false);
}

void ConstructorContext::finalize()
{
  Parser::check_temp_context();  // *fix 0.9.5 force this where there's no code in constructor body
  FunctionContext::finalize();
}

DestructorContext::DestructorContext(Table *parent, Function *fun)
 : FunctionContext(parent,fun)
{
}

void DestructorContext::finalize()
{
  if (!function()->builtin()) function()->class_context()->auto_destruct(false);
  FunctionContext::finalize();  // *fix 0.9.4 auto-generated destructor code wasn't written to function!
}

FBlock *FBlock::create(PEntry pe, PClass pc)
{
 int mfb = Parser::global().alloc(sizeof(FBlock),NULL);
 FBlock *pf = (FBlock *)Parser::global().addr(mfb);
 pf->context = NULL;
 pf->nlocal = 0;
 pf->nargs = 0;
 pf->ntotal = 0;
 pf->entry = pe;
 pf->pstart = NULL;
 pf->class_ptr = pc;
 pf->data = NULL;
 pf->trace = NULL;
 return pf;
}

void FBlock::finalize(int sz)
{
 pstart = Parser::code().end_code();
 nlocal = sz/sizeof(int);     // our _total_ context size in _dwords_!
 ntotal = nlocal + nargs; // add args to get total size
 //class_ptr = NULL; // for now!!
}

int NFBlock::create(Function *pfn, CALLFN fn)
{
 PFBlock pfb = pfn->fun_block();

// set up native code call block;
 NFBlock fblk;
 fblk.pfn = fn;
 fblk.nargs = pfn->stdarg() ? -1 : pfb->nargs;
 fblk.flags = pfn->is_cdecl() ? DC_CDECL : 0;

// *add 1.1.0 Sometimes imported methods use the cdecl calling convention;
// the 'this' pointer becomes a hidden extra argument.
  if (pfn->is_method() && pfn->is_cdecl())
      fblk.nargs++;

 Type rt = pfn->return_type(); //sig.m_type;

 // Some schemes use half-assed cdecl convention for returning objects
 // *add 1.2.6 add support for true return by value (GCC3)
 ImportScheme* import = pfn->import_scheme(); 
 if (import && rt.is_object() && import->ret_obj_popped()) {
     if (! import->true_return_by_value(rt))
        fblk.flags += DC_RET_OBJ;
     else
        fblk.flags += DC_RET_VAL | DC_NOWORD;
 }
 else
 if (! rt.is_pointer()) {
   fblk.flags += rt.is_double() ? DC_QWORD : 0;
   fblk.flags += rt.is_void() ? DC_NOWORD : 0;
 } // otherwise a pointer is of course the default: 4 bytes.

 return Parser::global().alloc(sizeof(fblk),&fblk);
}


////
Function::Function(Signature *sig, FunctionEntry *pfe)
 : m_sig(sig),m_context(NULL),m_slot_id(0),m_fun_entry(pfe),
   m_default_args(NULL),m_stdarg(false),m_builtin(false),
   m_line_nos(NULL),m_templ_info(NULL),m_import_scheme(NULL)
{
 m_fun_block = FBlock::create(pfe->reference(),class_context());
 m_fun_block->function = this;
 m_line = Input::lineno();
 Module* pm = Module::current();
 m_mod = pm ? pm->id() : -1;
}

Function::~Function()
{
    delete m_line_nos;
}

bool Function::undefined()
{
  return m_fun_block->pstart == NULL;
}

void 
Function::set_construct_destruct(int ftype, bool no_implicit_type_conversion)
{
 m_ftype = ftype;
 Class *pc = class_context();
 //if (!is_plain() && pc) fail("must be within class definition");
 if (is_destructor()) {
  if(signature()->size() > 0) fail("destructor cannot be passed parameters");
  pc->destructor(this);
 } else
 if (is_constructor()) pc->add_constructor(this,no_implicit_type_conversion);
}

string
Function::name()
{
 return fun_entry()->reference()->name;
}

void Function::attach_context(FunctionContext *context)
{
 // since this happens whenever a function is being redefined, we call clear()!!
  clear();
  m_context = context;
  m_fun_block->context = m_context;
}

void Function::clear()
// this is used when we are interactively redefining a function;
// an opportunity to dispose of code, etc
{
// *NOTE* blew up trying to delete this....how was it alloc?
 //delete m_fun_block->context; 

 // line number info
 delete m_line_nos; 
 m_line_nos = new LineNumbers;

 // function block
 m_fun_block->nargs = 0;
}

void Function::slot_id(int i)
{
 m_slot_id = i;

}

void
Function::set_default_args(PExprList pel)
// ONLY called if there _were_ any default values for arguments
// Hmm...we must watch out here!
{
 int k=0;
 m_default_args = new ExprArray(0 /*pel->size()*/);
 m_default_index = -1;
 ExprList::iterator eli;
  for (eli = pel->begin(); eli != pel->end(); ++eli,++k)
    if (*eli != NULL) {
      if (m_default_index == -1) m_default_index = k; 
      m_default_args->push_back(*eli);
    } 
}

bool
Function::can_match(int size)
// can this function in principle match an argument list of a particular size?
{
  int asz = signature()->size();
  if (!m_default_args) {
    if (stdarg()) return size >= asz;
    else return size == asz; 
  } else 
  return size >= m_default_index && size <= asz;
}

PExprList
Function::complete_arg_list(PExprList args)
{
 if (!m_default_args) return NULL;
 if (args->size() < signature()->size()) {
   PExprList pdef = new ExprList(m_default_args->begin() + args->size()-m_default_index,
                                 m_default_args->end());
   args->insert(args->end(),pdef->begin(),pdef->end());
   return pdef;
 } else return NULL;
}

void Function::dump(ostream& os)
{
 Signature::set_fun_name(name(),is_constructor(),is_destructor()); 
 os << *signature();
}

string Function::as_str()
{
 char buffer[MAX_FUN_NAME];
 ostrstream out(buffer,MAX_FUN_NAME);
 dump(out);
 out << ends;
 return buffer;
}

Function *
Function::from_fun_block(FBlock *pfb)
{
  FunctionEntry *pfe = (FunctionEntry *) pfb->entry->data;
  return pfe->fun_from_fblock(pfb);
}

// do note that this version only works for non-overloaded functions!
Function *
Function::lookup(const string& nm)
{
 PEntry pe = Parser::symbol_lookup(nm);
 if (!pe || !pe->type.is_signature()) return NULL;
 FunctionEntry *pfe = (FunctionEntry *)pe->data;   // shd be a method, really
 return pfe->back();
}


void FunctionEntry::dump (ostream& os)
{
 iterator ife;
 int idx = 1;
 for (ife = begin(); ife != end(); ++ife) {
   Function *pf = *ife;
   os << idx++ << ' ';
   pf->dump(os);
   if (pf->is_virtual()) os << " - slot " << pf->slot_id();
   os << endl;
 }
 Signature::set_fun_name("");
}


Function *
FunctionEntry::nth_fun(int idx)
{
   iterator fei = begin();
   Function *fn;
   if (idx == 0) idx = 1; 
   if (idx >= size()) idx = size();
   for(int i = 1; i <= size(); ++i, ++fei)
       if (i == idx) { fn = *fei; break; }
   return fn;
}

Function *
FunctionEntry::fun_from_fblock(FBlock *pfb)
{
   for(iterator fei = begin(); fei != end(); ++fei)
       if ((*fei)->fun_block()==pfb) return *fei;
   return NULL;
}


// these odd little functions are useful when debugging the system, 
// since Visual Studio and GDB will happily execute them for you on request.
static char buff[60];

char *sh_sig(Signature& s)
{
 *buff = 0;
 ostrstream outs(buff,60);
 outs << s << ends;
 return buff;
}

char *sh_type(Type& t)
{
 *buff = 0;
 ostrstream outs(buff,60);
 outs << t << ends;
 return buff;
}


// *change 1.2.3a 'do_strict' insists that the signatures match exactly; otherwise
// we allow for looser matches (particularly method signatures)
bool Signature::match(const Signature& sig, bool do_strict) const
{
// Comparing signatures 
// *fix 1.2.2b (Eric) Functions w/ no args can be incorrectly matched
 if (this == &sig) return true;
 if (size() != sig.size() || !class_ptr() != !sig.class_ptr()) return false;
 // *fix 1.2.3 Method signatures made tighter in 'exact match' mode
 if (class_ptr() != NULL && sig.class_ptr() != NULL) {
     if ((do_strict && class_ptr() != sig.class_ptr())
        || !sig.class_ptr()->inherits_from(class_ptr()))
     return false; // *fix 1.2.1 Member function signatures are distinct!
 }
 if (is_const() != sig.is_const())
     return false; // *fix 0.9.6 constness makes signatures distinct
 if (stdarg() != sig.stdarg())
     return false; // *fix 1.2.3 (Eric) And so does whether this is stdarg or not
 
 Signature::iterator ti,tio;
 try {
 for(ti = begin(), tio = sig.begin(); ti != end(); ++ti,++tio)
   if (!(*ti == *tio)) return false;
 } catch(...) {
   return false;
 }
 //* return tio == sig.end() && ti == end(); //true;
 return true;
}

#pragma optimize( "", off )
static Function *_fn;

Function *
FunctionEntry::simple_match(Signature *sig)
{
 iterator ife;
 for (ife = begin(); ife != end(); ++ife) {
   _fn = *ife;
   if (_fn == NULL)
       return NULL;
   if (_fn->signature()->match(*sig)) return *ife;
 }
 return NULL; // for now...
}

Function *
FunctionEntry::full_match(Signature *)
{
// basically not implemented!
 return back();
}



