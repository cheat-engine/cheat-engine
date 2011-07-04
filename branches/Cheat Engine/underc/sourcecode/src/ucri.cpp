/* UnderC Reflection Interface (UCRI)
 * UnderC C++ interpreter
 * Steve Donovan, 2001,2002
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 *
 * 
 * 
 */
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include "module.h"
#include "directcall.h"
#include "program.h"
#include "imports.h"
#include <map>

#include "ucri.h"

// found in directcall.cpp
void *_new_ex(int sz);
void _delete_ex(char *ptr, int sz);

// found in main.cpp
int uc_eval(char *expr, bool append_semicolon=true, bool synchronous=false, char *name=NULL, int lineno=0);
bool do_str_to_val(Type t,char *buff,void *ptr); // at end of this module

// found in code.cpp
bool is_double_number(const Type& t); 

// found in engine.cpp
void engine_set_tracing(bool yesno);
unsigned long* set_instruction_counter(bool do_profiling);
PClass get_class_object(void* p);
PPClass& get_object_VMT(void* p);

const char *TEMP_VAR = "__T0_tmp_";
static XNTable *mUC_std = 0, *mUC_glob = 0;
static ImportScheme *mUC_import_scheme;

template <class T1, class T2>
 class Wrapper {
   std::map<T2*,T1*> m_map;
 public:
    T1* operator()(T2* p)
	{
	  if (p==NULL) return NULL; // NULL maps onto NULL...
	  T1* pw = m_map[p];
	  if (! pw) {
	    pw = new T1(p);
		m_map[p] = pw;
	  }
	  return pw;
	 }
   };

static Wrapper<XFunction,Function> mXFunctionWrap;
static Wrapper<XType,Type> mXTypeWrap;
static Wrapper<XClass,Class> mXClassWrap;
static Wrapper<XEntry,Entry> mXEntryWrap;

XEntry* wrap_XEntry(Entry* pe)
{
  return mXEntryWrap(pe);
}

XFunction* wrap_XFunction(Function *fun)
{
  return mXFunctionWrap(fun);
}

XType* wrap_XType(Type *t)
{
  return mXTypeWrap(t);
}

XClass* wrap_XClass(Class *pc)
{
  return mXClassWrap(pc);
}

XType::XType(Type* t)
{
	m_type = t;
}

// Typically most compilers implement 'bool' as 
// single unsigned byte, whereas in UC bools are
// 32-bit integers. In MS at least, functions returning
// byte values are not guaranteed to clear out the
// rest of the EAX register!

bool bool4(unsigned long ul)
{ return (bool) ul; }
 
bool XType::is_const() const
{
	return bool4(m_type->is_const());
}
 
bool XType::is_reference() const
{
	return bool4(m_type->is_reference());
}
 
bool XType::is_pointer() const
{
	return bool4(m_type->is_pointer());
}
 
bool XType::is_array() const
{
	return bool4(m_type->is_array());
}
 
bool XType::is_unsigned() const
{
	return bool4(m_type->is_unsigned());
}
 
bool XType::is_number() const
{
	return bool4(m_type->is_number());
}
 
bool XType::is_int() const
{
	return bool4(m_type->is_int());
}
 
bool XType::is_float() const
{
	return bool4(m_type->is_float());
}
 
bool XType::is_single() const
{
	return bool4(m_type->is_single());
}
 
bool XType::is_long() const
{
	return bool4(m_type->is_long());
}
 
bool XType::is_short() const
{
	return bool4(m_type->is_short());
}
 
bool XType::is_char() const
{
	return bool4(m_type->is_char());
}
 
bool XType::is_double() const
{
	return bool4(m_type->is_double());
}
 
bool XType::is_signature() const
{
	return bool4(m_type->is_signature());
}
 
bool XType::is_function() const
{
	return bool4(m_type->is_function());
}
 
bool XType::is_class() const
{
	return bool4(m_type->is_class());
}
 
bool XType::is_object() const
{
	return bool4(m_type->is_object());
}

bool XType::is_bool() const
{
	return bool4(m_type->is_bool());
}
 
bool XType::is_void() const
{
	return bool4(m_type->is_void());
}
 
bool XType::is_namespace() const
{
	return bool4(m_type->is_namespace());
}
 
int XType::pointer_depth() const
{
	return m_type->pointer_depth();
}
 
int XType::size() const
{
	return m_type->size();
}
 
XClass* XType::as_class() const
{
	return wrap_XClass(m_type->as_class());
}

char*   XType::as_str() const
{  
	static string s;
	m_type->as_string(s);
	return s.c_str();
}

void XType::val_as_str(string& s, void *ptr) const
{
  s = m_type->value_as_string(ptr,false);
} 

void  XType::str_to_val(char *buff, void *ptr)
{
 do_str_to_val(*m_type,buff,ptr);
}

Type*  XType::type()
{ return m_type; }

XType*  XType::from_str(char *str)
{
 char buff[200];
 sprintf(buff,"%s %s;",str,TEMP_VAR);
 uc_eval(buff,false,true);  
 XEntry *xe = mUC_glob->lookup((char *)TEMP_VAR);
 return xe->type();
}

XTList& XType::typelist(XType* t1,...)
{
// copy type parms into a type list...
 va_list args;
 va_start(args,t1);
 static XTList tl;
 tl.clear();
 tl.push_back(t1);
 XType *xt; 
 while (xt = va_arg(args,XType*))
	 tl.push_back(xt);
 va_end(args);
 return tl;
}

XEntry::XEntry(PEntry pe) 
  : m_entry(pe) { }


XEntry *XEntry::clone()
{
  PEntry pe = new Entry;
  *pe = *m_entry;  
  return wrap_XEntry(pe);
}

XEntry *XEntry::base_entry()
{
// generate a copy of an array entry
// appropriate for accessing its elements
 XEntry* xe = clone();
 Type t = *type()->type();
 t.strip_array();
 t.decr_pointer();
 xe->m_entry->type = t;
 return xe;
}

char* XEntry::name()
{ return m_entry->name.c_str(); }

int XEntry::data()
{ return m_entry->data; }


void* XEntry::ptr(void *base)
{
 if (base == NULL) return Parser::global().addr(m_entry->data); 
 else return (char *)base + m_entry->data;
}

void   XEntry::set_data(int x)
{
  m_entry->data = x;
}

void   XEntry::set_ptr(void *p, void *base)
{
 if (base == NULL)
	m_entry->data = Parser::global().offset(p);
 else
	m_entry->data = (char *)p - (char *)base;
}

int XEntry::size()
{ 
   return m_entry->size;
}   

XType* XEntry::type()
{
  return wrap_XType(&m_entry->type);
}

void *XEntry::entry()
{ return m_entry; }


void XEntry::val_as_str(string& s, void *base)
{
  type()->val_as_str(s,ptr(base));
}

void XEntry::str_to_val(char *buff, void *base)
{
  type()->str_to_val(buff,ptr(base));
}

int XEntry::nfun()
{
  Type t = m_entry->type;
  if (! t.is_function()) return 0;  
  return reinterpret_cast<FunctionEntry *>(m_entry->data)->size();
}

XFunction *XEntry::function(int idx)
{
   Function *pf = reinterpret_cast<FunctionEntry *>(m_entry->data)
	   ->nth_fun(idx);
   return wrap_XFunction(pf);
}

int XEntry::addr_mode()
{ return (int)m_entry->rmode; }

XNTable::XNTable(NamedTable *tbl)
: m_table(tbl) { }

XEntry* XNTable::lookup(char *name,bool in_parent)
{
  PEntry pe = m_table->lookup(name,in_parent);
  if (!pe) return NULL;   // not found!
  return wrap_XEntry(pe);
}

XClass* XNTable::lookup_class(char *name, bool in_parent)
{
  PEntry pe = m_table->lookup(name,in_parent);
  if (!pe) return NULL;  // not found!
  if (pe->is_class()) return wrap_XClass(pe->type.as_class());
  else return NULL;
}

XTemplateFun* XNTable::lookup_template(char *name, bool in_p)
{
  PEntry pe = m_table->lookup(name,in_p);
  if (!pe) return NULL;  // not found!
  if (pe->type.is_signature()) {
	  FunctionEntry *pfe = (FunctionEntry *)pe->data;
	  if (pfe->get_template())
		  return new XTemplateFun(pfe->get_template());
	  else return NULL; // just a normal function
  } else return NULL;  // not a function at all
}

char* XNTable::name()
{
  return m_table->entry()->name.c_str();
}

//* add 1.2.4 Given a pointer allocated by a table, find its offset 
// (useful for globally allocated stuff like fblocks, etc)
int XNTable::offset(void* p)
{
   return m_table->offset(p);
}

NamedTable* XNTable::table()
{
    return m_table;
}

// this creates an entry with a given name and (opt)
// type. Just as with variables added with Parser::add_variable(),
// we allocate some direct data space as well.
XEntry* XNTable::create(char *nm, XType *xt)
{
  PEntry pe = m_table->new_entry(nm);
  if (xt) pe->type = *xt->type();
  pe->data = Parser::global().alloc(pe->type.size(),NULL);
  return wrap_XEntry(pe);
}

// issue: who deallocates these lists of ptrs?

void XNTable::get_functions(XFunctions& flist, int flags, char *pattern)
{
  EntryList ls;
  EntryList::iterator eli;
  flags |= FUNCTIONS;
  if (pattern == NULL) m_table->list_entries(ls,flags);
  else m_table->search_entries(pattern,&ls,flags);
  flist.clear();
  for(eli = ls.begin(); eli != ls.end(); ++eli) {
   FunctionEntry *pfe = reinterpret_cast<FunctionEntry *>((*eli)->data);
   FunctionEntry::iterator fei;
   for(fei = pfe->begin(); fei != pfe->end(); ++fei)
	   flist.push_back(wrap_XFunction((Function *)*fei));
  }
}

void XNTable::get_variables(XEntries& vlist, int flags, char *pattern)
{
  EntryList ls;
  EntryList::iterator eli;
  if (pattern == NULL) m_table->list_entries(ls,flags);
  else m_table->search_entries(pattern,&ls,flags);
  vlist.clear();
  for(eli = ls.begin(); eli != ls.end(); ++eli)
	  vlist.push_back(wrap_XEntry(*eli));
}

XFunctions& XNTable::functions(int flags)
{
  static XFunctions flist;
  get_functions(flist,flags,NULL);
  return flist;
}

XEntries&  XNTable::variables(int flags)
{
  static XEntries vlist;
  get_variables(vlist,flags,NULL);
  return vlist;
}

void XNTable::dispose_of_entries(XEntries& vars)
{
 vars.clear();
}

XClass::XClass(NamedTable *tbl)
: XNTable(tbl)
{}

Class *XClass::class_obj()
{ 
  return static_cast<Class *>(m_table);
}

XClass *XClass::base_class()
{
  Class *bc = class_obj()->base_class();
  if (bc) return wrap_XClass(bc);	
  else return NULL;
}

bool XClass::has_VMT()
{
  return class_obj()->has_VMT();
}

void * XClass::create()
{
 // Allocate properly (this overallocates!)
  void *ptr = _new_ex(class_obj()->size());
 // Find the entry for all class ctors
  PEntry pe = class_obj()->get_constructor();
  if (! pe) return ptr;  // no ctors 
 // Find the default ctor by iterating thru 
 // all the ctors, looking for the first one which
 // can match zero arguments.
  FunctionEntry *pfe = (FunctionEntry *)pe->data;
  FunctionEntry::iterator fei;
  Function *pf = NULL;
  for(fei = pfe->begin(); fei != pfe->end(); ++fei) {
     pf = *fei;
	 if (pf->can_match(0)) break;
  }
  if (! pf) return ptr; // no default ctor
  // If our class has a VMT, poke this into the object.
  // This goes just before the start...
  if (class_obj()->has_VMT())
	  *((Class ***)ptr - 1) = class_obj()->get_VMT();
  // now execute the default ctor directly!
  ArgBlock args;
  int flags = Engine::ARGS_PASSED | Engine::METHOD_CALL;
  args.OPtr = (char *)ptr;
  args.no = 0;
  int retval = Engine::execute(pf->fun_block(),flags,&args);
  if (retval != OK) return NULL; // exec failed!
  return ptr;
}

int XClass::inherits_from(XClass *xc)
{
  return class_obj()->inherits_from(xc->class_obj());
}

int XClass::no_template_parms()
{
 TemplateInstance* ti = class_obj()->get_template();
 if (ti) return ti->type_parms().size();
 else return 0;
}

XType* XClass::template_parm(int idx) // 0 to begin w/
{
 TemplateInstance* ti = class_obj()->get_template();
 static Type t;
 t = ti->type_parms().front();
 return wrap_XType(new Type(t));
}

XTemplateFun* XClass::get_template()
{
 TemplateInstance* ti = class_obj()->get_template();
 if (ti) return new XTemplateFun(ti->get_template()->get_entry());
 else return NULL;
}

// *add 1.2.4 Accessing RTTI; setting and getting the class of an object
XClass* XClass::get_class_of(void* p)
{
  return wrap_XClass(get_class_object(p));
}

void    XClass::set_class_of(void* p)
{
  get_object_VMT(p) = class_obj()->get_VMT();
}

XFunction::XFunction(Function *fun)
: m_fun(fun),m_ref(fun->fun_entry()->reference())
{

}

char* XFunction::name()
{
  return m_ref->name.c_str();
}

void XFunction::as_str(string& s)
{
  s = m_fun->as_str();
}

XType* XFunction::ret_type()
{
  static Type t;
  t = m_fun->return_type();
  return wrap_XType(&t);
}

XTList& XFunction::args()
{
  static XTList arglist;
  string s;
  arglist.clear();
  Signature *sig = m_fun->signature();
  Signature::iterator si;
  for(si = sig->begin(); si != sig->end(); ++si) {
      Type* pt = new Type(*si);
	  arglist.push_back(wrap_XType(pt));
  }
  return arglist;
}

void XFunction::get_args(XTList* tl, XStringList* sl)
{
 *tl = args();
 if (sl) {
	 ArgList& arg_names = *m_fun->signature()->get_arg_names();
	 ArgList::iterator ali;
	 sl->clear();
	 for(ali = arg_names.begin(); ali != arg_names.end(); ++ali)
		 sl->push_back(*ali);
 }

}

int XFunction::where(string& filename)
{
	LineNumbersB* lnb = m_fun->line_nos();
	if (lnb) {
  	  int l1,l2;
	  lnb->lookup_range(l1,l2);
      if (lnb->module() != 0) {
	    filename = Module::from_id(lnb->module())->name();
        return l1;
      } else return 0;
    } else return 0; 
}

int XFunction::ip_to_line(void* ip)
{
  LineNumbersB* lnb = m_fun->line_nos();
  if (lnb) {
      LineInfo li;
      li.ip_offset = ((Instruction*)ip - m_fun->fun_block()->pstart);
      if (lnb->lookup_line(li,true)) return li.line;
      else return 0;
  } else return 0;
}

int XFunction::module()
{
	if (m_fun->line_nos()) return m_fun->line_nos()->module();
	else return 0;
}

void *XFunction::fblock()
{
	return m_fun->fun_block();
}

// *add 1.1.3 fun() always returns an _executable_ pointer;
// the assumption that if we're a DLL we have been called
// by a C++ program; otherwise, from a UC script itself.
void *XFunction::fun()
{
#ifndef _USRDLL
    return fblock();
#else
    if (m_fun->import_scheme() == NULL) {
      m_fun->import_scheme(mUC_import_scheme);
	  if (m_fun->is_method())
	     m_fun->class_context()->set_import_scheme(mUC_import_scheme);  
    }
	void *fptr = Builtin::generate_native_stub(m_fun);
	m_fun->import_scheme(NULL);
	return fptr;
#endif
}

// *add 1.2.4 Can access the function's pcode
XInstruction* XFunction::pcode()
{
 return (XInstruction*)m_fun->fun_block()->pstart;
}

// *add 1.1.3 Evaluate a general function or method
int XFunction::eval(void *args, void *result, void *obj)
{
  return uc_eval_method(m_fun,obj,args,result);
}

// *add 1.2.3 Customizing a function's tracing behaviour
void    XFunction::set_trace(XTrace* tr)
{
  m_fun->fun_block()->trace = tr;
}

XTrace* XFunction::get_trace()
{
  return m_fun->fun_block()->trace; 
}

// *add 1.2.4 Can switch all tracing on or off
void XFunction::set_tracing(bool yesno)
{
 engine_set_tracing(yesno);
}

// *add 1.2.4 Get an XFunction from a Fblock
XFunction* XFunction::from_fb(void* fb)
{
  Function* pf = ((FBlock*)fb)->function;
  return pf ? wrap_XFunction(pf) : NULL;
}

XEntry* XFunction::lookup_local(char* name)
{
  return wrap_XEntry(m_fun->context()->lookup(name,false));
}


XModule::XModule(Module *pm)
: m_mod(pm) { }

static XModule*  wrap_XModule(Module *pm)
{
  return pm ?  new XModule(pm) : NULL;
}

XModule*    XModule::from_id(int id)
{ 
	return wrap_XModule(Module::from_id(id));
}

XModule*    XModule::from_name(char* filename)
{ 
	return wrap_XModule(Module::from_name(filename));
}

XModules&   XModule::lists()
{
 static XModules mlist;
 ModuleList ml;
 Module::get_modules(ml);
 mlist.clear();
 ModuleList::iterator mli;
 FORALL(mli,ml)
	 mlist.push_back(wrap_XModule(*mli));
 return mlist;   
}

char*       XModule::filename()
{
  static string s;
  s = m_mod->name();
  return s.c_str();
}

XFunctions& XModule::functions()
{
 static XFunctions flist;
 flist.clear();
 Module::entry_iterator mei,mend = m_mod->entry_end();
 for(mei = m_mod->entry_begin(); mei != mend; ++mei)
   if (mei->type()==FUNCTION)
	 flist.push_back(wrap_XFunction(mei->function()));
 return flist;
}

XClasses& XModule::classes()
{
 static XClasses clist;
 clist.clear();
 Module::entry_iterator mei,mend = m_mod->entry_end();
 for(mei = m_mod->entry_begin(); mei != mend; ++mei)
   if (mei->type()==IS_STRUCT)
	 clist.push_back(wrap_XClass(mei->as_class()));
 return clist;
}


XTemplateFun::XTemplateFun(TemplateEntry *te)
: m_templ(te) {}

static void  copy_to_tlist(TypeList& type_parms, const XTList& tl)
{
 XTList::iterator xtli;
 for(xtli = tl.begin(); xtli != tl.end(); ++xtli)
	 type_parms.push_back(*(*xtli)->type());
}

static void *fblock_from_templ_inst(TemplateInstance *ti)
{
   Function *pf = (Function *)ti->data();
   // a UC function ptr is the FBlock...
   return pf->fun_block();
}

// match_instantiate() does what UC will normally
// do when instantiating a template function. You pass
// the types of the arguments to the function, and 
// match() deduces the actual type parameters involved.

void* XTemplateFun::match_instantiate(const XTList& tl)
{
 TypeList type_parms;
 copy_to_tlist(type_parms,tl);

// and pass it onto TemplateEntry::match()
 if (m_templ->match(type_parms)) {
   // which makes an actual template instance available
   return fblock_from_templ_inst(m_templ->match_instance());
 }
 else return NULL; // no match possible
}

// whereas, you pass instantiate() the type parameters
// directly. This does a number of things which are
// usually the responsibility of TemplateEntry::match(),
// but the procedure is actually simpler because the
// type parms don't have to be deduced (see Template::match())
// from the actual arg types
void* XTemplateFun::instantiate(const XTList& tl)
{
 TypeList type_parms;
 copy_to_tlist(type_parms,tl);

 // do we already have such an instance?
 int idx = m_templ->simple_match(type_parms,false);
 if (idx != -1) { // we do indeed - use it!
	 m_templ->set_index(idx);
     return fblock_from_templ_inst(m_templ->match_instance());   
 } else {
   // have to instantiate for these type parms...
   // Arbitrarily pick the first template in the list
   Template *templ = m_templ->templates(0);

   // Create an instance, & fill in the type parameters
   TemplateInstance *inst = new TemplateInstance(templ,type_parms);
   copy_type_list(inst->type_parms(),type_parms);
   m_templ->add_instance(inst);

   // bind the formal dummy types to actual types
   const TypeList& fa = templ->formal_parms(); 
   TypeList::const_iterator tlif,tlia;
   for(tlif = fa.begin(), tlia = type_parms.begin(); tlif != fa.end(); ++tlif, ++tlia) {
      Type tf = *tlif, ta = *tlia;
	  if (tf.is_dummy())
        tf.as_dummy()->bind_to(ta);
   } 

   // and voila
   templ->instantiate(inst);
   return fblock_from_templ_inst(inst);
 }
}

char* XTemplateFun::name()
{
  return m_templ->entry()->name.c_str();
}

EXPORT XNTable* uc_global()
{
	return mUC_glob;
}

EXPORT XNTable* uc_std()
{
    return mUC_std;
}

EXPORT void uc_ucri_init()
{
   if (mUC_std == NULL) {
      PEntry pe = Parser::symbol_lookup("std");
	  mUC_std = new XNTable(pe->type.as_class());
	  mUC_glob = new XNTable(&Parser::global());
	  mUC_import_scheme = Import::create_compat_scheme();
   }
}

EXPORT unsigned long* ucri_instruction_counter(bool do_profiling)
{
  return set_instruction_counter(do_profiling);
}

// *change 1.1.3 uc_eval_exp() moved here from dll_entry.cpp, making
// it part of the main system. I've generalized it to do method calls
// as well, and it will be available as part of both the UCRI and the 
// DLL interface.
CEXPORT int XAPI uc_eval_method(void *sc, void *obj, void *arguments, void *result)
{
 Function *pf = (Function *)sc; // should come from uc_compile()! 
 Type rt = pf->return_type();
 // copy the arguments into a buffer 
 Signature *sig = pf->signature();
 ArgBlock args;
 Signature::iterator is;
 args.no = 0;
 int temp_args[50], *argo = temp_args, *argi = (int *)arguments;
 for(is = sig->begin(); is != sig->end(); ++is) { 
	 if (is_double_number(*is)) {
       args.no += 2;
	   *((double *&)argo)++ = *((double *&)argi)++;
     } else {
       ++args.no;    
	   *argo++ = *argi++;
     }
 }	 
 // and now copy the buffer into the args array, backwards!
 int *args_ptr = args.values;
 for (int i = 0, n = args.no; i < n; i++) 
	 args.values[n-i-1] = temp_args[i];
 int flags = Engine::ARGS_PASSED;
 bool returns_qword = is_double_number(rt);
 if (returns_qword) flags += Engine::RETURN_64; else
 if (! rt.is_void()) flags += Engine::RETURN_32;
 if (obj != NULL) {  // i.e. a method call!
   args.OPtr = (char *)obj;
   flags += Engine::METHOD_CALL;
 }
 int retcode = Engine::execute(pf->fun_block(),flags,&args);
 if (retcode == FAIL) return 0;
 if (result) {
  if (returns_qword)
    *(double *)result = args.ret2;
  else
   *(unsigned long *)result = args.ret1;
 }
 return 1;
}


bool do_str_to_val(Type t,char *buff,void *ptr)
{
 istrstream in(buff);
 if (t.is_pointer()) {
   if (t.is_char()) strcpy((char *)ptr,buff);
    else {
     unsigned int ui;
     in >> ui;
     *(void**)ptr = (void *)ui;
    }
 } else
 if (t.is_int()) {
  if (t.is_char()) {
     char ch;
     in >> ch;
     if (! t.is_unsigned()) *(char *)ptr = ch; 
                   else     *(unsigned char *)ptr = ch;
  } else 
  if (! t.is_unsigned()) {
    int val;
    in >> val;  
    if (t.is_long()) *(long *)ptr = val;
	else if (t.is_short()) *(short *)ptr = val;
	else if (t.is_enum())  *(int *)ptr = val;
    // *fix 1.2.3 our bools are now 8-bit unsigned
	else if (t.is_bool())  *(unsigned char *)ptr = val; 
    //* else if (t.is_bool())  *(int *)ptr = val; 
	else *(int *)ptr = val;
  } else {
    unsigned int val;
	in >> val;
    if (t.is_char()) *(unsigned char *)ptr = val; 
    else if (t.is_long()) *(unsigned long *)ptr = val;
	else if (t.is_short()) *(unsigned short *)ptr = val;
	else *(unsigned int *)ptr = val;	
  }
 } else
 if (t.is_float()) {
	 if (t.is_double()) in >> *(double *)ptr;
	 else in >> *(float *)ptr;
 } else
 if (t.is_class()) {
	 PClass pc = t.as_class();
	 if (pc->name()=="string") in >> *(string *)ptr;
	 else return false;
 }
 return true;
}
