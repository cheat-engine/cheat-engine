/* TEMPLATES.CPP
 * Support for C++ templates
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
*/
#include "common.h"
#include "templates.h"
#include "std_utils.h"
#include "tparser.h"  // for CLASS token
#include <ctype.h>
#include "input.h"
#include "directcall.h"

const int TEMPL_BUFFSIZE = 20000;

 // in main.cpp
 
int uc_eval(char *expr, bool append_semicolon, bool synchronous, char *name, int lineno);

#define YYEMPTY (-2)
extern int yychar;
int	save_yychar;
YYSTYPE	save_yylval;	

void save_parser_state()
{
 save_yychar = yychar;
 save_yylval = yylval;
 yychar = YYEMPTY;
}

void restore_parser_state()
{
 yychar = save_yychar;
 yylval = save_yylval;
}

int count(char *, char);  // at end of this file.
string build_qualified_name(const string& name, const TypeList& tl);
ostream& operator<< (ostream& outs, const TypeList& tl);
void massage_type_list(TypeList& tl);
void copy_type_list(TypeList& tlp, const TypeList& tlt);

typedef TypeList::iterator TLI;
typedef TypeList::const_iterator TLCI;

static char mIBuffer[TEMPL_BUFFSIZE];

bool DummyType::bind_to(Type t)
{
// Plain dummies bind to types in a straightforward manner; they only complain when
// they are already bound and an attempt is made to bind them to a different type
// Binding a constant dummy to a variable dummy involves some copying, which will
// only work for simple types at the mo.
 PEntry pe = entry() != NULL && t.is_dummy() ? t.as_dummy()->entry() : NULL;
 if (t == t_null) m_type = t_null;  // that is, unbound us (NB for us to _reuse_ this)
 else if (pe != NULL && pe->name == "") {  // constant dummy...
   memcpy(entry()->global_ptr(),pe->global_ptr(),pe->type.size());
   // *fix 1.2.9 indicates that we are properly bound! 
   m_type = entry()->type;  
   // *change 1.2.4 no longer function_trace
   if (Parser::debug.verbose) cout << "*bound (const) " << name() << ' ' << *(int *)pe->global_ptr() << endl;
 } else {
   if (! unbound() && t != m_type) return false;
   m_type = t;
   if (entry()) entry()->type = t;
   if (Parser::debug.verbose) cout << "*bound " << name() << ' ' << t << endl;
 }
 return true;
}

string DummyType::value_as_string()
{
  string s;
  if (name() != "") s += name();
  else { 
    if (entry()==NULL) s += "unbound";
    else s += type().value_as_string(entry()->global_ptr());
  }
  return s;
}

IContext::IContext(Table *parent)
 : Table(parent,DIRECT,0)
{ m_type = INSTANTIATION_CONTEXT; }

Template::Template(TemplateEntry *te, const TypeList& tl, IContext *cntxt)
 : m_context(cntxt), m_templ_entry(te), m_formal_args(tl)
{
  m_file = Input::filename();
  m_lineno = Input::lineno();
}

bool Template::is_class()
{ return m_templ_entry->is_class(); }

string Template::name()
{ return m_templ_entry->name();     }

void Template::instantiate(TemplateInstance *instance)
{
  using Parser::state;
  const TypeList& tl = instance->get_template()->formal_parms();

  TLCI ti;
 // don't try to instantiate templates using dummy types!
  int unbound_count = 0;
  FORALL(ti,tl) if (ti->is_dummy()){
    Type t = *ti;
	if (t.is_dummy()  && (t.is_unbound() || t.as_dummy()->type().is_dummy()) )
	    unbound_count++;
  }
  string inst_err = "";
  bool success = true;
  if (unbound_count > 0) success = false; 
  else {  
    Parser::ParserState old_state = state;
    // *hack 1.2.7 Can't use defered method compilation for instantiating template classes.
    bool old_skip = Parser::debug.skip_method_bodies;
    Parser::debug.skip_method_bodies = false;
    state.reset();
    state.push_context(m_context);
    state.in_template = instance;
    dcl_set(false,false); // save the DCL & ALS stacks...
    save_parser_state();  // save the BISON state...
	// *fix 1.1.0 Don't try to instantiate while in importing mode...
	void *slib = Builtin::get_dll_handle();
	Builtin::set_dll_handle(NULL);
  
    try {
      success = uc_eval(m_buffer,false,true,m_file.c_str(),m_lineno) == 0;
    } catch(string s) {
      inst_err = s;  
    } catch(...) {
      //inst_err = "unknown instantiation error";
     success = false;
    }

	Builtin::set_dll_handle(slib);
    state.in_template = NULL;
    state.pop_context();
    restore_parser_state();
    // *fix 1.2.7 lines swopped around...
    state = old_state;
   // dcl_reset();
    Parser::debug.skip_method_bodies = old_skip;

    // NB to _unbind_ the dummy parameters, ready for the next instantiation!
    FORALL(ti,tl) if (ti->is_dummy()) ti->as_dummy()->unbind();
  } 

  if (! success) {
     instance->instantiated(false);
     fail(inst_err);
  } else {
    instance->instantiated(true);
	// can't call type() until the data is set!
    if (Parser::debug.verbose) { //*change 1.2.4 no longer function_trace
	  cout << "instantiated: ";
	  Signature::set_fun_name(instance->name()); // only for function templates, of course!
	  try {
	   if (instance->data() != NULL) cout << instance->type();
      } catch(...) {
        cout << "<unknown>";
	  }
	  cout << endl;
	}
  }
}

TypeList& Template::formal_parms()
{ return (TypeList&)m_context->formal_parms(); }

const TypeList& Template::formal_args()
{ return m_formal_args; }

// static support routines for the parser
Type Template::get_template_type(PEntry te, TypeList *ptl)
{
 // resolving the actual type, given an expression like Type <int,20> etc.
 TemplateEntry *pte = (TemplateEntry *)te->data;
 if(pte->match(*ptl)) {
   return pte->match_instance()->type();
  } else error("Cannot match this template");
 return t_null;
}

bool in_template = false;

void Template::do_template_header(TypeList *ptl)
{
 using Parser::state;
 IContext *ic = new IContext(&state.context());
 state.push_context(ic);
 TLI tli;
 FORALL(tli,*ptl) {
    Type t = *tli;
    DummyType *pd = tli->as_dummy();
	string name = pd->name();
	bool is_dummy = tli->is_unbound();
	Type type;
	// the 'true' dummy types are put in as is; the constants are put in as
	// their type value.
    if (is_dummy) type =  *tli;
    else type = pd->type();
	
	PEntry pe;
	if (is_dummy) pe = state.add_typedef(type,name);
    else pe = state.add_variable(type,name,NULL,None);

	// We bind the actual entry to any dummies
	tli->as_dummy()->entry(pe);
 }
   
 ic->set_formal_parms(*ptl);
 delete ptl;

 // *hack 1.1.4 lexical tie-in 
 in_template = true;

}

void Template::do_function_template()
{
// appears immediately after the function prototype;
// *add 1.1.0 special case of method template; keep track of any entries created.
// *fix 1.1.3 exclude genuine member function templates!
 // *hack 1.1.4 lexical tie-in 
 in_template = false;

 try {
  bool was_outside_class;
  Signature *sig = Parser::get_prototype(was_outside_class);
  string name    = Signature::get_fun_name();
  TypeList formal_args = sig->type_list();

  IContext *ic = (IContext *)Parser::state.pop_context();
 
  PEntry pe;
  Table *cntxt;
  bool was_method = false;
  if (sig->class_ptr() != NULL && was_outside_class) {
    cntxt = sig->class_ptr();
    if (PClass(cntxt)->get_template()==NULL) fail("must be a class template here");
    was_method = true;
  } else cntxt = &Parser::state.context();

  pe = cntxt->lookup(name);
  FunctionEntry *pfun;
  TemplateEntry *ptmpl;  
  if (pe == NULL) { 
 // Create a new FunctionEntry with a TemplateEntry
 	pe = cntxt->add(name);
	pfun = Parser::create_function_entry(sig,name,pe);
	ptmpl = new TemplateEntry(pe,false);
	if (was_method) ptmpl->set_method();
	pfun->set_template(ptmpl);
	pe->type = Type(sig);
    pe->data = (int)pfun;
    if (was_method) 
       PClass(cntxt)->get_template()->get_template()->get_entry()->add_method_entry(pe);
  } else if (! pe->type.is_function()) {
     fail("already defined as a non-function");
  } else {
   // was previously defined as a function (which might not have been a template!)
    pfun = (FunctionEntry *)pe->data;
    ptmpl = pfun->get_template();
 // Not really an error...but watch this carefully!  (especially w/ methods!)
    if (ptmpl == NULL) { //    fail("previously defined as a function");
      ptmpl = new TemplateEntry(pe,false);
	  pfun->set_template(ptmpl);
    }
  }

 // add a new template to the TemplateEntry
  Template *pnt = new Template(ptmpl,formal_args,ic);
  ptmpl->add_template(pnt);

 // Lippman 183, 2nd ed.  formal args must contain all formal template type parms.
 // (Not applicable to class template members, which don't need matching)
  if (! was_method) {
    bool all_present = true;
    TLCI tlip, tlia;
    const TypeList& formal_parms = pnt->formal_parms();
    if (!ptmpl->is_method()) {
    FORALL(tlip,formal_parms)  {
      int times_present = 0;
      FORALL(tlia,formal_args) if (type_contains(*tlia,*tlip)) times_present++;
      all_present = all_present && times_present > 0;
    }
    if (!all_present) { fail("All formal template type parms must be present at least once"); return; }
   }
  }

// and grab the function!! 
  generate_function_header(name,sig,true);
  pnt->grab();

  //*fix 1.2.0L Should not have deleted the signature! 
  // delete sig;
} catch(string msg) {
  error(msg);
}

}

char* Template::generate_function_header(Function* pf, bool plain_method, bool qualified_names)
{ 
 *mIBuffer = 0;
  ostrstream outs(mIBuffer,TEMPL_BUFFSIZE);
  Signature::write_qualified_names(qualified_names);
  pf->dump(outs);
  outs << ' ' << (plain_method ? '{' : ':') << ends;
  Signature::write_qualified_names(true); // maintain the default setting!
  return mIBuffer;
}

char* Template::generate_function_header(string name, Signature* sig, bool plain_method)
{
  *mIBuffer = 0;
  ostrstream outs(mIBuffer,TEMPL_BUFFSIZE);
  Signature::set_fun_name(name);
  outs << *sig << " {" << ends;
  return mIBuffer;
}

void Template::do_class_template(int s_or_c, string name, int look_ahead, TypeList *ptl)
{
 using Parser::state;
 IContext *ic = (IContext *)state.pop_context();
 // *hack 1.1.4 lexical tie-in 
 in_template = false;
 
// *fix 0.9.8 Doing class template specializations properly
 TypeList formal_args;
 PEntry pe = state.context().lookup(name);
 if (pe && pe->type != t_template_type) { error("Not a template class " + Parser::quotes(name)); return; }
 if (ptl != NULL) { // formal_args will only be non-trivial if this is a specialization;
    if (pe == NULL) { error("Can only specialize a class template if it already exists"); return; }    
    formal_args = *ptl;
 } else
    formal_args = ic->formal_parms();

 TemplateEntry *pte;
 
 if (pe == NULL) {  
   pe = state.context().add(name);
   pe->type = t_template_type;
   pte = new TemplateEntry(pe,true);
   pe->data = (int)pte;
   pe->m_typename = true;
 } else pte = (TemplateEntry *)pe->data;

 Template *pct = new Template(pte,formal_args,ic);
 pte->add_template(pct);

 *mIBuffer = 0;
 ostrstream outs(mIBuffer,TEMPL_BUFFSIZE);
 outs << (s_or_c == CLASS_X ? "class " : "struct ") << name;
 //* -2 (Obscure Bison number) means that there's no lookahead token...    
 //* in this case the look-ahead can be either '{' or ':'  
 if (look_ahead != -2) outs << (char)look_ahead;
 outs << endl;
 outs << ends;

 pct->grab();

}

bool Template::type_contains(Type t1, Type t2)
// does the dummy type t1 contain t2?  Two cases:
// (a) t1 is a qualified form of t2, e.g const T&
// (b) t1 is a template containing t2, e.g. list<T,A>&
{
 if (t1.is_dummy() && t2.is_dummy()) {
    DummyType *d1 = t1.as_dummy(), *d2 = t2.as_dummy();
	if (d1 == d2 || d1 && d2 && d1->name() == d2->name()) return true;  // case (a)
    if (t1.is_class()) {
	  Class *pc = t1.as_class();
	  const TypeList& tl = pc->get_template()->type_parms();
	  TLCI tli;
	  FORALL(tli,tl) {
	   Type tp = *tli;
	   if (tp == t2) // || d2 && tp.is_dummy() && d2->name() == tp.as_dummy()->name())
	      return true; 
      }
	  return false;
	  //return utils::find(tl,t2);  // case (b)
	}
 }
 return false;	    
}

void massage_type_list(TypeList& tl)
{
 TLI tli;
 FORALL(tli,tl) {
  Type t = *tli;
  if (t.is_const() && ! t.is_ref_or_ptr()) t.strip_const();  // e.g. 'const int' -> 'int'
 // 'variable references'! (see Expressions::entry_op())
  else if (t.is_reference()) t.strip_reference();   //t.is_variable()
  // the appearance of a function name means an _implied_ function pointer
  if (t.is_signature() && !t.is_pointer()) t.incr_pointer();
  *tli = t;
 }
}

// support for things like Stack<int,20> and template<class T, int n>
// these guys will grab the four possibilities in such lists.
// For example , these would be:
//  1) class T     - a named dummy
//  2) int N       - a variable parameter
//  3) int         - a type
//  4) 20          - a constant
// These are represented by dummy types, except for (3), which just pass through.
// (See the end of parser.y)
Type Template::dummy(Type t, string name)
{ 
 if (t != t_null) {
	 if (name == "") { // e.g. just plain 'int' passes through  (3)
	   return t;  
     }  else { // *fix 1.2.9 force the const type
        t.make_const();  
        // 'int n' -> unbound variable dummy (2)
        return Type::make_dummy(name,t,NULL);  
     }
 } else 
  // 'class T' -> classic type dummy (1)
  return Type::make_dummy(name,t_null,NULL); 
}

Type Template::dummy(PEntry pe)
{
  return Type::make_dummy("",pe->type,pe); // constant entry dummy (4)
}

char *get_method_name(char *line);
const int MAX_LINE_SIZE = 256;

void Template::grab()
{
 char buff[MAX_LINE_SIZE];
// in general stuff will already be written into the buffer; may (or not) include the ':' or '{'
 int already_written = strlen(mIBuffer);
 int brace_count = already_written > 0 ? count(mIBuffer,'{') : 0;
 char *ptr = mIBuffer + already_written; // - 1;  *NOTE*
 strcpy(ptr,"\n"); // seems to make a difference...
 ptr ++;
 do {
   Input::grab_next_line(buff);
   brace_count += count(buff,'{') - count(buff,'}'); 

   if (*buff != '\0') strcpy(ptr,buff);
   ptr += strlen(buff);
   strcpy(ptr,"\n"); //*I suspect we need this because we cannot handle very long lines!
   ptr ++;
  //brace_count += count(buff,'{') - count(buff,'}'); 
 } while (brace_count > 0);      

if (Parser::debug.verbose) cout << "*buff: " << mIBuffer << endl;
 m_buffer = strdup(mIBuffer);
 *mIBuffer = '\0'; 
}

// *add 1.1.0 This is obviously very similar to the above; next time I want to combine
// the functionality, but for now I just want to know that (a) it works and (b) doesn't 
// interfere with stuff that already works.
// *fix 1.2.4 If this is a ctor with an init list, handle the special case by always
// grabbing everything up to '{'
void Template::grab_function_body(bool plain_method, char* body)
{
 int brace_count = plain_method ? 1 : 0;
 bool grabbing_init = ! plain_method;
 char* ptr;
 char buff[MAX_LINE_SIZE];
 int open_kount, close_kount;
 if (body) ptr = body + strlen(body);
 do { 
   Input::grab_next_line(buff);
   open_kount = count(buff,'{');
   close_kount = count(buff,'}');
   brace_count += open_kount - close_kount; 
   if (open_kount) grabbing_init = false;
   if (body) {
     if (*buff != '\0') strcpy(ptr,buff);
     ptr += strlen(buff);
     strcpy(ptr,"\n"); //*I suspect we need this because we cannot handle very long lines!
     ptr ++;
   }
 } while (brace_count > 0 || grabbing_init);  
}

Template *Template::as_template(Type t)
{
  if (t.is_template_class()) // peculiar expr gets the actual template,
                             // not just the particular instance! 
    return t.as_class()->get_template()->get_template();
  else return NULL; //*NOTE* until we can sort this out!
}

static string mErrStr;

int err(const string& s)
{ mErrStr = s; return 0; }

int Template::match(const TypeList& tl)
{
  TLCI tlif, tlia;
  if (tl == m_formal_args) return 999; // *fix 1.0.0 full specialization
  int effort = 0;
  for(tlif = m_formal_args.begin(), tlia = tl.begin(); tlia != tl.end(); ++tlif, ++tlia) {
    Type tf = *tlif, ta = *tlia;
	if (! tf.is_dummy()) { 
		// *fix 1.1.1 must be able to match all plain args of template functions
		if (::match(tf,ta)==NO_MATCH) return 0;
		continue;  // NB rest is not for plain arguments!!
    }
    if (tf.as_dummy() != NULL) tf.as_dummy()->unbind();

    if (tf.is_const()) { // Quite safe to remove const even if the actual arg isn't...
            ta.strip_const(); tf.strip_const(); effort++; 
    } 
    if (tf.is_reference()) {
           /*if (ta.is_reference())*/
		    { ta.strip_reference(); tf.strip_reference(); effort++; }
    } 
    while (tf.is_pointer()) {
		   if (ta.is_pointer()) { ta.decr_pointer();  tf.decr_pointer(); effort++; }
		   else break;
   }

   // at this point, the formal arg type is either 'bare'
   // or a template class.
   if (!tf.is_bare()) return err("cannot match");
   if (ta.is_const() && ta.is_ref_or_ptr()) return err("parameter is not const");
   effort++;  
   if (tf.is_template_class()) {
     if(ta.is_template_class()) {
        TemplateInstance * titf = tf.as_class()->get_template(); 
 		TemplateInstance * tita = ta.as_class()->get_template(); 
		// but do they belong to the same template?
		if (titf->get_template() == tita->get_template()) {
	      effort++;
          TypeList& tlf = titf->type_parms();
          TypeList& tla = tita->type_parms();
          TLI tlif,tlia;
          for(tlif=tlf.begin(),tlia=tla.begin(); tlif != tlf.end(); tlif++,tlia++) {
            if (!tlif->as_dummy()->bind_to(*tlia)) return err("already bound to a diff. type");
          }
        } else return 0;  // no way we can match different templates!
     } else return 0;  // cannot match template w/ a non-template class! 	  
   } else {
     if (! tf.as_dummy()->bind_to(ta)) return err("already bound");   
   }
  }
  return effort;
}

Type TemplateInstance::type()
{
  if (m_template->is_class()) return Type((Class *)data());
                      else return Type(((Function *)data())->signature());

}

string TemplateInstance::name()
{ return m_template->name(); }

TemplateInstance::TemplateInstance(Template *templ, const TypeList& tl)
 : m_template(templ), m_type_parms(templ->formal_parms()), m_type_args(tl),m_instantiated(false),
   m_data(NULL)
 { }


bool TemplateEntry::instantiate_method(Function *fn)
{
  if (fn->get_template()) return true; // fine - already instantiated!
// This function doesn't have a TemplateInstance initially
// *hack 1.1.0 The hack here is to find the template corresponding to this function
// in the overloaded set. The assumption of course is that they have been 
// defined in the same order as they were declared!
  FunctionEntry *pfe = fn->fun_entry();
  FunctionEntry::iterator fei;
  TemplateList::iterator tli;
  Template *templ;
  for(fei = pfe->begin(), tli = m_templates.begin(); fei != pfe->end(); ++fei,++tli)
     if (*fei == fn) templ = *tli;

  PClass pc = fn->class_context();
  TemplateInstance *c_inst = pc->get_template();

  TemplateInstance *tinst = new TemplateInstance(templ,c_inst->type_list()); 

  try {
     copy_type_list(tinst->type_parms(),c_inst->type_parms());
	 TLCI tlif = tinst->get_template()->formal_parms().begin(), tlia = c_inst->type_parms().begin();
	 for(; tlia != c_inst->type_parms().end();  ++tlif, ++tlia)
	    (*tlif).as_dummy()->bind_to(*tlia);
     templ->instantiate(tinst);
     tinst->instantiated(true);
	 // and mark as instantiated...
     fn->set_template(tinst);
     tinst->data(fn);
     return true;
   } catch(string s) {
     error(s);
	 return false;
   } catch(...) {
     error("unknown error");
	 return false;
   }
 }


int TemplateEntry::simple_match(const TypeList& tl, bool use_args)
{	
 TemplateInstanceList::iterator tili;
  int i = 0;
  FORALL(tili,m_instances) {
    TemplateInstance *ti = *tili;
    const TypeList& tlinst = use_args ? ti->type_list() : ti->type_parms();
	if (Parser::debug.verbose) cout << "*cmp " << tl << ' ' << tlinst << endl;
    if (tl == tlinst) return i;
	i++;
  }
 return -1;
}

int TemplateEntry::no_instances()
// returns the number of _instantiated_ instances
{
  TemplateInstanceList::iterator tili;
  int i = 0;
  FORALL(tili,m_instances) if ((*tili)->instantiated()) i++;
  return i;
}

TemplateInstance *TemplateEntry::match_instance()
{
  if (m_match_idx == -1) return NULL;
  return utils::list_item(m_instances,m_match_idx);
}

string TemplateEntry::last_error()
{
 string ret = mErrStr;
 mErrStr = "";
 return ret;
}

string TemplateEntry::name()
{
 return m_entry->name;
}

void TemplateEntry::add_template(Template *templ)
{
 utils::add_unique(m_templates,templ);
}

Template*  TemplateEntry::templates(int i)
{
  return utils::list_item(m_templates,i);
}

void TemplateEntry::add_method_entry(PEntry pe)
{
  if (m_method_entries == NULL) m_method_entries = new EntryList();
  m_method_entries->push_back(pe);
}

const EntryList *TemplateEntry::method_entries()
{
  return m_method_entries;
}

void copy_type_list(TypeList& tlp, const TypeList& tlt)
{
  TLCI ti;
  TLI tip; 
  for(tip=tlp.begin(),ti=tlt.begin();  ti != tlt.end();  ++ti, ++tip) {
    if (ti->is_dummy()) {
	   DummyType *dt = ti->as_dummy();
       *tip = dt->type();
    } else *tip = *ti;
  }
}

bool TemplateEntry::match(const TypeList& actual_parms)
{
  if (is_method()) return false;

  // ensure that variables lose their implicit referenceness, etc.
  TypeList tl = actual_parms;
  massage_type_list(tl);
  set_index(-1);

  using Parser::state;
// Do we have an existing instance which will match exactly?
  int idx = simple_match(tl);
  if (idx != -1) {
    set_index(idx);
	return true;
  }
     
// Build up a candidate list of templates which could theoretically match this list
  TemplateList tel;
  int no_args = tl.size();
  TemplateList::iterator teli;	
  FORALL(teli, m_templates)
    if ((*teli)->formal_args().size() == no_args) tel.push_back(*teli);
  if (tel.size()==0) return err("Wrong number of type arguments"); 

  // now find the template that scores the highest! Even if there is only one
  // candidate, we go through this to see if we can actually do the match and
  // (most importantly) to do the type binding
  Template *templ;
  int score,max_score = 0;
  FORALL(teli,tel) {
    score = (*teli)->match(tl);
	if (score > max_score) { templ = *teli; max_score = score; }
  }
  if (max_score == 0) { 
	  string error = last_error();
	  return err(error != "" ? error : "Cannot match any template");
  }

  // at this point the template parameters should be successfully bound
  // and a new instance of this template can officially exist (although not yet 
  // instantiated, of course.)
  TemplateInstance *instance = new TemplateInstance(templ,tl);

  // Meanwhile, we fill in the instance's type parameters
  copy_type_list(instance->type_parms(),templ->formal_parms());

  if (m_is_class) {
     TemplateInstance *old_inst = state.in_template;
  // Generate an 'empty' class for the template instance (like one declared forwardly)
     string qualified_name = build_qualified_name(name(),tl);
	 state.push_context(templ->context());
	 state.in_template = instance;
     state.add_class(CLASS,qualified_name,ForwardClass,t_void);
	 state.in_template = old_inst;   // *fix 0.9.7 restore this for nested instantiations
	 state.pop_context();
  //*fix 1.2.9 this is the actual parent context of this template
     NamedTable* tbl = static_cast<NamedTable*>(templ->context()->parent_context());
	 PEntry pe = tbl->lookup(qualified_name);   // wuz state.context().
	 Class *pc = pe->type.as_class();
     instance->data(pc);
	 instance->entry(pe);
	 pc->set_template(instance);
   } else { // strictly speaking, this is not correct in _all cases_ but this will do for now.
    try {
     templ->instantiate(instance); 
    } catch(string msg) {
      return err(msg);
	}
  }
  add_instance(instance);
  return true;
}

void TemplateEntry::add_instance(TemplateInstance* ti)
{
  int ni = m_instances.size();
  m_instances.push_back(ti);
  set_index(ni);
}

int count(char *p, char ch)
{
 int k = 0;
 while (*p != '\0') if (*p++ == ch) k++;
 return k;
}

char *get_method_name(char *line)
{
  char buff[MAX_LINE_SIZE];
  strcpy(buff,line);
  char *p = strstr(buff,"(");
  if (p==NULL) return NULL;  
  p--;
  while(isspace(*p)) p--;
  *(p+1) = '\0';
  while(!isspace(*p)) p--;
  p++;
  if (strncmp(p,"operator",8)==0) p+=8;
  return p;
}


ostream& operator<< (ostream& outs, const TypeList& tl)
{
  outs << '<';
  TLCI tli, tl_last = tl.end();
  --tl_last;
  FORALL(tli,tl) {
	outs << *tli; 
	if (tli != tl_last) outs << ',';
  }
  outs << '>';
  return outs;
}

string build_qualified_name(const string& name, const TypeList& tl)
{
  char buff[512];
  ostrstream outs(buff,512);
  outs << name << tl;
  outs << ends;
  return buff;
}

char *sh_tlistc(const TypeList& tl)  /*DEBUG*/
{
 static char buff[128];
 ostrstream outs(buff,128);
 outs << tl << ends;
 return buff;
}

char *sh_tlist(TypeList& tl)
{ 
 return sh_tlistc((const TypeList&)tl); 
}


