/* CLASS.CPP
 * The type Class
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 */

//#define _HAS_ITERATOR_DEBUGGING 0
//#define _SECURE_VALIDATION_DEFAULT 0
#pragma warning (disable:4786)
#define IN_TABLE_CPP
#include "common.h"
#include "directcall.h"
#include "operators.h"
#include "opcodes.h"
#include "tparser.h"
#include "std_utils.h"
#include "module.h"
#include "input.h"

void error(string msg);  // shd be in common...
 // in main.cpp
int uc_eval(char *expr, bool append_semicolon, bool synchronous, char *name, int lineno);

namespace {// private utility stuff
  bool grab_list(const TypeList& tl1, TypeList& tl2);
  typedef std::map<PEntry,PExprList> Entry2ExprMap;
  FunctionContext *mFe;

  PEntry begin_method(string name, Type rtype, char *arg_name, Type atype, int ftype);
  void declare_method(string name, Type rtype, char *arg_name, Type atype, int ftype);
  void   end_method();
  Function *get_std_match(const TypeList& tl, const FunctionList& fl, Type t);

  PFBlock mVMT[MAX_VMT_ENTRIES];

  Entry2ExprMap s_entry_map;

  char *TRY_BLOCK_HANDLER_CLASS = "_tbhc_";
};

void *mVtable[MAX_VMT_ENTRIES];

// HACK ALERT!
#define m_base_ctor m_finalized

Class::Class(Table *parent,int access) 
        : NamedTable(parent,OREL,0),m_base_access(access),m_simple_struct(true),
          m_default_constructor(NULL),m_copy_constructor(NULL),m_destructor(NULL),
          m_has_constructors(false),m_base_entry(NULL),m_templ_info(NULL),
          m_vtable_size(0),m_is_union(false),
          m_abstract(false),m_base_ctor(false), m_import(NULL),m_imported(false),m_struct(false)
{
    m_type = IS_STRUCT;
    m_VMT = NULL;
  
    set_base_class(base_class(),access);
}

void Class::set_base_class(Class *base, int access)
{
  using namespace Parser;
  clear();
  if (base) m_parent = base;
  m_base_access = access;

  // *fix 1.1.2 Field alignment moved here from ctor, in case of redefinition.
  set_dword_align(Parser::debug.class_dword_align);

  if (base) {
        PPClass vmt;        
        m_VMT = NULL;  // hm, really shd clear out if NOT null...
        // *NB* use the actual size, not the VMT-adjusted size!
        m_data = (char *)m_parent->size();
        m_slot_id = base->last_slot();
        vmt = base->get_VMT();
        int vmt_size = sizeof(void *)*(m_slot_id+1);
        if (vmt) //...copy our parent's VMT into the local buffer
            memcpy(mVMT,vmt,vmt_size);

        copy_convert_to_list();


        // if we are derived from an imported class w/ a vtable,
        // then we will have to generate a vtable for these objects.
        // *fix 1.2.0 Use the inherited vtable size
        // *fix 1.2.7 Such classes must have the _same_ kind of import scheme
        // as their bases; this was not working if there was a subsequent DLL load.
        if((base->imported() && vmt) || (base->vtable_size() > 0)) { 
           m_vtable_size = base->vtable_size(); 
		   m_import = Import::create_scheme(base->import_scheme()->scheme_type());
		   m_imported = Builtin::get_dll_handle() != NULL;
		   m_import->set_first_virtual_class(base->m_import->vmt_offset());
        } 
        // *hack 0.9.8 A forward declaration of ctor; hack for factory functions. 
        // *OUT*this interferes w/ normal default ctor rules.
        //declare_method(constructor_name(),t_void,NULL,t_void,IsConstructor);
        //m_base_ctor = true;
   } else {
        m_slot_id = 0;
   }
}

Function *Class::default_constructor() { return m_default_constructor; }
Function *Class::copy_constructor()    { return m_copy_constructor;    }
Function *Class::destructor()          { return m_destructor;          }
bool  Class::has_constructors()
{ 
    return m_has_constructors;
}

// defines our convention for internally naming the constructor/destructor
// cf. parser.y line 110...
string Class::constructor_name()
{ return "__C__"; }

string Class::destructor_name()
{ return "__D__"; }

bool Class::is_anonymous_union()
{
  return m_is_union && name()[0] == '$';
}

void Class::set_anonymous_union()
{
 switch_off_allocator_advance();
 m_is_union = true;
}

PEntry
Class::add(const string& name)
{
  PEntry pe = Table::add(name);
  pe->set_access(get_access_mode());
  m_entries.push_back(pe);
  return pe;
}

// compare this to Table::list_entries(), which uses the underlying map.
// Class keeps an explicit list of entries so it can deliver entries
// in _order of declaration_.  No doubt there is more than one way to
// do this
void Class::list_entries(EntryList &el, int flags)
{
	if (flags & DO_PARENT && base_class() != NULL) base_class()->list_entries(el,flags);
	EntryList::iterator eli;
	for(eli = m_entries.begin(); eli != m_entries.end(); ++eli) 
		if (check_entry(*eli,flags)) el.push_back(*eli);
}

void 
Class::add_constructor(Function *fn, bool no_implicit_type_conversion)
{
  if (fn->can_match(0)) { 
      m_default_constructor = fn;
  }  else
  if (fn->can_match(1)) {
    Type t = entry()->type;     t.make_const_reference();
    Type at = *(fn->signature()->begin());  // the type of the argument

    // was this a copy constructor?
    if (at == t) m_copy_constructor = fn;
    else if (! no_implicit_type_conversion) {
     // defines a Conversion From - _except_ for copy constructor and if marked 'explicit'!
      m_from_list.push_back(at);
      m_from_fn.push_back(fn);
    }
  }
  m_has_constructors = true;
  m_simple_struct = false;
}

void Class::destructor(Function *fn) 
{
 m_destructor = fn;  // *change 1.0.0 does NOT imply this class has to have VMT
 // *fix 1.2.0 This caused enormous grief - I'm not at all sure _why_ it was here!
 // construct_VMT() shd _only_ be called when all the class' virtual slots are filled,
 // i.e. at class finalization.
 //construct_VMT();    // *temporary - depends on _gForceVMT
}

void Class::construct_VMT()
{
  if (m_VMT) return;  // already built one!
  if (m_slot_id > 0) {
    // increase our size to accomodate VMT 
    // - but only if this is the first class to have a VMT!
    //if (!is_derived() || is_derived() && PClass(m_parent)->last_slot() == 0)  m_data += 4;  
    mVMT[0] = PFBlock(this);  // our class pointer!
    m_VMT = new PClass [m_slot_id+1];
    memcpy(m_VMT, mVMT, sizeof(PFBlock)*(m_slot_id+1));
 }
}

void Class::clear()
{
 Table::clear();  // just to test - not sure about inheritance!
 m_has_constructors = false; // NB to reset state fully!
 m_default_constructor = m_copy_constructor = m_destructor = NULL;
 s_entry_map.clear();
 m_obj_list.clear();
 m_field_list.clear();
 m_entries.clear();
 m_vtable_slots.clear();
 m_vtable_funs.clear();
 m_base_entry = NULL;
 m_VMT = NULL;
 m_slot_id = 0;
}

Namespace *Class::inside_namespace()
{
 if (Parser::is_namespace(entry()->context)) return (Namespace *)entry()->context;
 else return NULL;
}

bool not_locally_defined(Class *pc, PEntry pe)
{ 
 return !pe || !pc->is_local_entry(pe);
}  

Type ref_to(Class *pc, bool is_const=true)
{
  Type t_oref = pc->entry()->type; //??
  t_oref.make_reference();
  if (is_const) t_oref.make_const(); 
  return t_oref;
}

PEntry Class::get_constructor()
{
 return  lookup(constructor_name());
}

static int s_first_line = -1, s_last_line;

void Class::add_line_no(const string& file, int line)
{
  if (s_first_line == -1) s_first_line = line;
  else s_last_line = line;
}

// *add 1.2.7 support for the defered compilation of method bodies

void Class::add_defered_method_body(Function* fn, char* body_buff)
{
  m_DeferedBodies.push_back(new MethodBody(fn,body_buff));
}

void save_parser_state();
void restore_parser_state();

string s;
Class* mClass;

void* _copy_body(char* buff)
{
  strcpy(buff,s.c_str());
  return mClass;
}

void Class::compile_methods()
{
 if (Parser::debug.skip_method_bodies) {
      using namespace Parser;
      Module *pm = Module::current();
      //string s;
      bool success;
      int line;
      mClass = this;
      string file = pm->name(); 
      debug.skip_method_bodies = false;
      ParserState old_state = state;
      save_parser_state();  // save the BISON state...
      while (m_DeferedBodies.size() > 0) {
          MethodBody* mb = m_DeferedBodies.front();
          s = mb->body;
          line = pm->get_function_line(mb->pf);
          try {
            success = uc_eval(s.c_str(),false,true,file.c_str(),line) == 0;
          } catch(string msg) {
            success = false;
          }
          if (! success) {
             cerr << s << "*\n";        
             debug.skip_method_bodies = true;
             m_DeferedBodies.clear();
             return;
          }
          delete mb;
          m_DeferedBodies.pop_front();
      }
      m_DeferedBodies.clear();
      Input::insert_string(";");
      state = old_state;
      debug.skip_method_bodies = true;
      restore_parser_state();
      Parser::state.in_declaration = false; //*HACK*
  }
}


void Class::finalize()
{
  Module::current()->add_class(this,s_first_line,s_last_line);
  s_last_line = -1;

  construct_VMT();
  // not entirely the whole story.....
  m_simple_struct = !has_constructors();
  m_finalized = true;

  if (imported()) {
      // *fix 1.2.0 Ensure that imported classes have a proper vtable
      m_vtable_size = m_slot_id;
     // *add 1.2.3 __vftable is used to reserve the class layout
     // of an imported class with virtual methods, but only if it's the first such class.
      if (! base_class() && m_vtable_size > 0) {          
        PEntry pe;
        // in case of the vtable being at the begining, we have to shuffle entries along
        if (Import::scheme()->vtable_at_begining()) {
           build_obj_list();   // to construct m_field_list
           EntryList::iterator eli;
           FORALL(eli, m_field_list) {              
              pe = *eli;      
              pe->data += sizeof(void *);
           }  
           pe = Parser::state.add_variable(t_void_ptr,"__vftable");         
           pe->data = 0;  
        }
        else Parser::state.add_variable(t_void_ptr,"__vftable");        
      }
   //   return;  // *fix 1.2.0 we always try to force import ctors & dtors (issue: but if they are not complete?)
  }

  // this class requires construction and destruction!
  // *fix 1.1.0 or maybe not...these guys will not generate default ctors etc if it isn't necessary
  // *fix 1.2.6 Insist on not generating ctors and dtors if it really isn't necessary.
  if (!has_constructors()) {
      if (! auto_construct(true)) goto finish;
  }
  if (!imported()) {
    if (!copy_constructor())   auto_assign_copy(false);
    // Do note that we _never_ inherit operator= directly!
    if (not_locally_defined(this,lookup("="))) auto_assign_copy(true);
  }
  if (!destructor())  auto_destruct(true);

  m_simple_struct =  ! has_constructors();

finish:
 // finally, we can compile the method bodies
  compile_methods();

  Parser::state.set_construct_destruct(IsPlain);
}

bool Class::make_available()
{
// force a full instantiation of this template class
  TemplateInstance *ti = get_template();
  if (ti != NULL) {
    if(! ti->instantiated()) {
        try {
           ti->get_template()->instantiate(ti);
        } catch(string msg) {
             error(msg); return false;
        } 
        return true;
      } else return true;
  } else return true;  
}

int Class::size()
{
 //...add a word to class size if we have a VMT!!
    int sz = Allocator::size(); 
    return sz + ((m_VMT) ? sizeof(int) : 0) + ((m_vtable_size) ? sizeof(int) : 0);
}

int Class::distance_from(Class *pc)
{
    if (pc == this) return 0;  // on the nose...
    Class *bc = base_class();
    if (bc) return bc->distance_from(pc)+1;
    else return NOT_RELATED;
}   

bool Class::inherits_from(Class *pc)
{
    return distance_from(pc) < NOT_RELATED ? true : false;
}

Class *Class::base_class()
{
 // Watch out here for _nested classes_
   return (m_parent->type()==IS_STRUCT && m_base_access != NotDerived)
           ? (Class *)m_parent : NULL;
}

// ------------------ Managing conversion to and from this class -----------------------

void Class::set_conversion_to(Function *fn)
{
  m_to_list.push_back(fn->return_type());
  m_to_fn.push_back(fn);
}

bool Class::get_from_type_list(TypeList& tl)
{  return grab_list(m_from_list,tl); }

bool Class::get_to_type_list(TypeList& tl)
{  return grab_list(m_to_list,tl);   }

void
Class::copy_convert_to_list()
{
 Class *pb = base_class();
 m_to_list = pb->m_to_list;
 m_to_fn = pb->m_to_fn;
}

// *fix 0.9.7 No longer require exact match for getting conversion operators
// *note* there will be an issue in the cases where a standard conversion requires code!
// *fix 1.1.0 the utility get_std_match() tries to match a type trivially, by promotion,
// and then by standard conversion.  Point here is that it will favour the trivial match
// even where a standard match was possible.  See end of this module.

Function *
Class::get_conversion_to(Type t)
 {
  return get_std_match(m_to_list,m_to_fn,t);
 }

 Function *
 Class::get_conversion_from(Type t)
 {
  return get_std_match(m_from_list,m_from_fn,t);
 }


//.---------------------.._safe_ ways to manipulate the class VMT....-----------------
void  Class::set_slot(int id, Function *fn)
{
    FBlock *pfb = fn->fun_block();
    if (id < 1 && id > m_slot_id) error("class: slot out of range");
    if (m_VMT) m_VMT[id] = (PClass)pfb; else
               mVMT[id] = pfb;

 // if our class is derived from an imported class, then we have
 // to create a native stub for this method, and add it to the native vtable!
 // Note: the vtable slot indices are _zero-based_!!
 // We refuse to add any virtual methods beyond those already present in
 // in the original export!
    if (!fn->builtin() && vtable_size() > 0 && id <= vtable_size()) {
        m_vtable_slots. push_back(id-1);
        m_vtable_funs.  push_back((Function *)Builtin::generate_native_stub(fn));
       // cerr << "update slot " << id << ' ' << fn->name() << endl; 
    }
}
   
PFBlock Class::get_slot(int id)
{
    if (id < 1 && id > m_slot_id) error("class: slot out of range");
    if (m_VMT) return (PFBlock)m_VMT[id]; else
               return (PFBlock)mVMT[id];
}

// pc->last_slot() > 0 is a more reliable way of telling whether a class has a VMT
// The issue here is that the VMT will sometimes only be generated when the class is finalized.

bool Class::has_VMT()
{ 
 return last_slot() > 0 || get_VMT() != NULL;
}

// *add 1.2.0 Imported objects may avoid overallocation problems and attach
// the VMT to the object pointer using a map....
typedef std::map<void *,void *> PointerMap;
static PointerMap s_vmt_map;

void Class::attach_VMT(void *obj, PPClass vmt)
{
    s_vmt_map[obj] = (vmt==NULL) ? get_VMT() : vmt;
}

PPClass Class::find_VMT(void *obj)
{
 // might be in the cache...
    /*
    PCacheEntry pcache = s_vmt_cache
    for(int i = 0; i < n_vmt_cache; i++,pcache++)
        if (pcache->first==obj) return PPClass(pcache->second);
     */

 // otherwise, look in the map
    PointerMap::iterator pmi = s_vmt_map.find(obj);
    if (pmi != s_vmt_map.end()) {
       // s_vmt_cache[0] = *pmi;
        return PPClass(pmi->second);
    } else
        return NULL;
}

bool Class::has_true_VMT()
{
  return has_VMT() && !(m_import && ! m_import->cpp_friendly());
}

//--------------------- imported classes - managing vtables --------------------
bool Class::imported()
 { return m_imported; }  

int  Class::vtable_size()
 { return m_vtable_size; }

bool Class::derived_from_import()  // this is dubious! 
 { return vtable_size() > 0; }

void Class::set_imported()
{ 
 if (! m_import) {
   m_import = Import::create_scheme();
   m_imported = true;
   // Schemes like GCC2 put the hidden vtable pointer at the end of the first base class
   // with virtual methods
   if (! base_class() && ! m_import->vtable_at_begining())
       m_import->set_first_virtual_class(Allocator::size()/sizeof(int));
 }
}

void Class::update_vtable(char *obj)
{
// This implements VTABLE_PATCH, which is found in the ctors of imported classes w/ vtables
    if (m_vtable_slots.size() == 0) return;   // no modifications!
	if (m_import->patched())
	  m_import->vtable_write(obj);     // write our new vtable into the object
	else {                             // unless new vtable must be constructed.....
	  m_import->vtable_size(vtable_size());
      m_import->vtable_read(obj);      //   read the original vtable from object    
	  m_import->clone_vtable();        //   make a copy...

     // and patch any slots that we have overrided....
      IntList::iterator ili = m_vtable_slots.begin();
      FunctionList::iterator fli = m_vtable_funs.begin();
      for(; ili != m_vtable_slots.end(); ++ili, ++fli) {
        int id = *ili;
       // cerr << "patch slot " << id << ' ' << *fli << endl; 
        m_import->vtable_write_slot(id,(CALLFN)*fli);
      }
	  m_import->now_patched();
	  m_import->vtable_write(obj);
    }
}

char *
Class::check_object_pointer(char *p)
{
 // Given a imported C++ object, ensure that it's got a proper UC VMT
 // We are given the base class - have to hunt to find the exact derived class!
 // The strategy is to look at the classes of all the methods we find in the vtable
 // and pick the class furthest from the base.
 // This does mean that we need to see ALL the headers, not just the interface header
 // for the base class - otherwise, we can't tell what class an imported method belongs
 // to.
 // This implements CHKVMT

   if (! p) return NULL;  // *fix 1.2.4 doesn't apply to null pointers
   if ( (has_true_VMT() && *VMT(p) == NULL) || find_VMT(p)==NULL) {       
      PFBlock provisional_VMT[MAX_VMT_ENTRIES]; // constraint enforced elsewhere
      PClass furthest_class = this;
      int tdist,max_tdist = 0, indefinites = 0, k = 1; // our slots begin at one...  
	  m_import->vtable_read(p); 
      // for all entries in the base's VMT
      for (int i = 0,n = last_slot(); i < n; i++) {  // wuz -1?
	     CALLFN pf = m_import->vtable_read_slot(i);
         PFBlock pfb = Builtin::imported_fblock_from_function((void *)pf);
		 if (pfb != NULL) { // this method has been already imported...
		   PClass pc = pfb->class_ptr;
           if (pc != NULL) { // and we have seen its class defn
             tdist = pc->distance_from(this);        
             if (tdist != NOT_RELATED && tdist > max_tdist) { 
				 max_tdist = tdist;
				 furthest_class = pc;
			 }
		   } else ++indefinites;
		 } else { // haven't seen this one yet; import it! (Slot ids are one-based!)
			 pfb = Builtin::import_vmethod(pf,get_slot(i+1));
			 ++indefinites;
         }
         provisional_VMT[k++] = pfb;
      } // for all slots
	  PPClass vmt;
	  m_import->vtable_size(m_slot_id);
	  if (indefinites == 0) vmt = furthest_class->get_VMT();
	  else vmt = (PPClass)m_import->hunt_for_matching_VMT(provisional_VMT,furthest_class);

      // and finally, actually patch in the VMT
      // *add 1.2.0 'unfriendly' imported classes use the VMT association strategy
      if (! has_true_VMT()) attach_VMT(p,vmt);
      else *VMT(p) = vmt;
     // if (! Parser::debug.suppress_link_errors) cmsg << "Type was " << furthest_class->name() << endl;
   }
   return p;
 }
//----------------------managing class initialization---------------------------

void Class::add_class_init_list(PEntry pe, PExprList pel)
{
 if (pe->m_typename && pe->type.is_class()) {
  PClass pce = pe->type.as_class();
  if (m_base_entry == NULL) build_obj_list();
  if (pce==base_class()) pe = m_base_entry;
  else error("not a base class");
 }
 s_entry_map[pe] = pel;
}

Class *as_class(PEntry pe)
{
  return pe->type.as_class();
}

// constructing the list of _members which are objects_
bool Class::build_obj_list()
{
    if (m_base_entry != NULL) return true; //*just remember always to reset m_base_entry*
    // find all our non-static member fields, particularly
    // looking for objects 
    m_field_list.clear(); // *for now*
    m_obj_list.clear(); //*just in case*
    EntryMap::iterator ei;
    // *NOTE* there MUST be a more elegant way to find proper allocated _fields_
    for(ei = m_map->begin(); ei != m_map->end(); ++ei) {
      PEntry pe = ei->second;
      if (pe->m_typename) continue;
      Type t = pe->type;
      // *fix 0.9.4 Include named enumeration member variables!
      if (!t.is_function() && pe->rmode!=DIRECT) {    /*t.is_enum()*/
        m_field_list.push_back(pe);
        if(t.is_object()) m_obj_list.push_back(pe);
      }
    }
    // if we have a base class, then create a fake entry
    Class *bc = base_class();
    if (bc) {
     PEntry pe = new Entry;
     *pe =  *(bc->entry());
     pe->rmode = OREL;
     pe->data = 0;
     //  m_obj_list->push_back(pe);
     m_base_entry = pe;
    }

    // *fix 1.1.0 Returns false if there is nothing requiring construction
    return m_obj_list.size() > 0 || m_base_entry != NULL;
}

void compile(PExpr ex)
{
// *fix 0.9.6 Is necessary to check the temp context after every expression compile!
 Parser::code().compile(ex,DROP_VALUE);
 Parser::check_temp_context();
}

PExpr call_function(Function *pf, PExpr e=NULL)
// shortcut for calling functions of one or none arguments;
// always forces call of actual function, virtual or not (see compile_function_call() in code.cpp)
{
 return new Expr(DCALL,pf->return_type(),pf,Expressions::expr_list(e));
}

// auto contruction/destruction code generation
// do note that the _base class_ is treated specially; we have already physically
// constructed the object, so all we do is call the inherited method!
bool Class::auto_construct(bool make_method)
{
 using namespace Expressions;
 bool copy_ctor = false;
 if (!make_method) { // if we were called to initialize an existing constructor...
   FunctionContext *fc = (FunctionContext *)&Parser::state.context();
   // which was a copy constructor, a special case; (no default operation!)
   // *fix 1.0.0 But we do need to use any explicit initializations in the ctor init list
   copy_ctor = fc->function() == copy_constructor();
 } else  if (imported()) { // *change 1.2.0 Special case of importing a class moved here....
    declare_method(constructor_name(),t_void,NULL,t_void,IsConstructor);
    return true;
 }

 if (m_field_list.size() == 0) { // then build the list of fields....
   bool needs_construction = build_obj_list(); 
   // *fix 1.1.0 Don't interfere with attempts to finalize existing ctors or dtors.
   if (! make_method) needs_construction = true;   
   // *fix 1.1.0 Create a default ctor if there's a VMT, even if nothing needs to be constructed
   // *fix 1.2.6 I've uncommented this line, having forgotten why it was commented ;)
   // for GCC3 imports, we really do need to know if something's a 'plain old data struct'!
   if (! needs_construction && ! has_VMT()) return false;
 }

 try {

 if (make_method) {
    begin_method(constructor_name(),t_void,NULL,t_void,IsConstructor);  
 } 

 // *fix 0.9.5 One place where you can construct an abstract base class
 if (!copy_ctor && m_base_entry != NULL && !base_class()->simple_struct()) {
   PExpr ec = construct_op(base_class(), s_entry_map[m_base_entry],false);
   if (ec->is_nil()) throw "cannot construct base class";
   compile(ec);

 // Imported constructors of classes w/ vtables must be followed by 
 // code to (optionally) patch the vtable, if a derived UC class is
 // overriding any methods!
 // *fix 1.2.0 We must emit VTABLE_PATCH for _any_ class which has modified the vtable
 // of some imported ancestor class. (This was only working for directly derived classes!)
    if (vtable_size() > 0)
       Parser::code().out(CodeGenerator::instruction_with_pointer(VTABLE_PATCH,this));
  }

 EntryList::iterator eli;
 FORALL(eli, m_field_list) {
   PEntry pe = *eli;
   bool obj_has_ctor = pe->type.is_object() && as_class(pe)->has_constructors();
   // if there was a class init list, this map has been filled in.
   PExprList pel = s_entry_map[pe];
   if ((!copy_ctor && obj_has_ctor) || pel != NULL) {
       PExpr ec = initialize_op(pe,NULL,pel,0);
       if (ec->is_nil()) throw "cannot initialize member object";
       compile(ec);
   }
 }
 } catch(char *msg) {
   error (msg);
 }
 // *fix 0.9.5 Absolutely essential that this ALWAYS happens!
 s_entry_map.clear();
 if (make_method) end_method();
 return true;
}

void Class::auto_destruct(bool make_method)
{
 using namespace Expressions;
 if (m_obj_list.size() == 0 && !build_obj_list()) return;

 // *fix 1.2.3 Derived objects may have needed the building of a ctor,
 // but that doesn't mean they need a dtor.
 if (m_obj_list.size() == 0 && base_class()->destructor() == NULL) return;
 if (imported()) { // *add 1.2.0 needing to force the import of a class dtor
    declare_method(destructor_name(),t_void,NULL,t_void,IsDestructor);
    return;
 }
 // Build up a list of member fields which have dtors
 EntryList::iterator eli;
 EntryList ls;
 FORALL(eli,m_obj_list)
   if (as_class(*eli)->destructor()) ls.push_back(*eli);
 // *fix 0.9.6 we can bail out if there's no such member fields AND no base dtor
 bool base_has_dtor = m_base_entry != NULL && base_class()->destructor() != NULL;
 if (ls.size()==0 && ! base_has_dtor) return;
 if (make_method) begin_method(destructor_name(),t_void,NULL,t_void,IsDestructor);
 if (base_has_dtor) {
    compile(call_function(base_class()->destructor()));
 }
 // and call the dtor for all such member objects...
 for(eli = ls.begin(); eli != ls.end(); eli++) {
   Function *pd = as_class(*eli)->destructor();
   compile(method_call(pd,entry_op(*eli),NULL));
 }
 if (make_method) end_method();
}

void Class::auto_assign_copy(bool is_assign)
// either creates a default assignment operator,
// or a copy constructor, using memberwise
// assigment/initialization
// Construct these in all cases!
{
 using namespace Expressions;
 // *fix 1.2.7 Ensure that the field list has been updated.
 if (m_field_list.size() == 0) build_obj_list();
  
  char *arg_name = "$obj";
  Type t_const_oref = ref_to(this,true);
  
  PEntry pe;
  PExpr ex,em,ea,eo; 
  Class *bc = base_class();
  if (is_assign) {
   PEntry pe = lookup("="); // *fix 0.9.5 auto-assign calls _inherited_ operator=
   // ea is the formal parameter (const T&)
   ea = entry_op(begin_method("=",ref_to(this,false),arg_name,t_const_oref,IsPlain));
   if (bc && pe) {  // but do we have operator=(const T&) ??
       PExpr efn = function_op(entry_op(pe),expr_list(ea));
       if (!efn->is_nil()) compile(efn);
   } 
  } else { 
   ea = entry_op(begin_method(constructor_name(),t_void,arg_name,t_const_oref,IsConstructor));
   if (bc && bc->copy_constructor()) compile(call_function(bc->copy_constructor(),ea));
  }

  // over all non-function non-static members of this class...
  EntryList::iterator eli;
  FORALL(eli,m_field_list){
     pe = *eli;
     em = entry_op(pe);
     eo = make_op(DOT,pe->type,ea,em);
	 // *fix 1.1.4 Don't enforce constness check when generating these assignments!
     if (is_assign) ex = assign_op(em,eo,false);
     else {
		 // *hack 1.1.4 Ignore arrays when building copy ctor for now
         if (Parser::array_size(pe) > 1) continue; 
         else ex = initialize_op(pe,eo,NULL,0);
     }
     if (ex && !ex->is_nil()) compile(ex);
	 else if (m_obj_list.size() != 0) warning("cannot build assign/copy ctor");
  } 
  // operator= must return its reference argument...
  // Crucial not to _drop_ the stack at this point!
  if (is_assign) Parser::code().compile(ea,0);
  end_method();
}

///// Friendship mangement /////////////
bool Class::is_friend_class(Class *pc)
{
  return is_friend_function(pc);
}

bool Class::is_friend_function(void *fn)
{
 return utils::find(m_friend_objects,(Function *)fn);
}

void Class::add_friend_object(void *pc, bool is_class)
{ 
 m_friend_objects.push_back((Function *)pc);
}

PClass Class::generate_try_block_handler()
{
 using namespace Parser;
 using namespace Expressions;
 // Construct a class with a single 32-bit field and a constructor
 // for initializing same...NB that it has a VMT so that it gets put
 // on the ODS, where we will use it as a unique marker.
  Type  tc = state.add_class(STRUCT,TRY_BLOCK_HANDLER_CLASS,ForwardClass,t_void);  
  Class *pc = tc.as_class();
  pc->next_slot();     // a fiddle, since we don't have any v. methods!
  pc->construct_VMT();
  state.push_context(pc);
  PEntry field = state.add_variable(t_int,"_field_",NULL,None);

  PEntry arg = begin_method(pc->constructor_name(),t_void,"_arg_",t_int,IsConstructor);
  PExpr ea = assign_op(entry_op(field),entry_op(arg));
  code().compile(ea,DROP_VALUE);
  end_method();

  state.pop_context();
  return pc;
}


// *add 1.2.6 the Enum object now carries the list of its members
void Enum::add_entry(PEntry pe)
{
 m_entries.push_back(pe);
}

Table* Enum::context()
{
// we can assume that the entries have been added to the enclosing context
  return m_entries.size() > 0 ? m_entries.front()->context : NULL;
}

string Enum::lookup_value(int val)
{
    EntryList::iterator eli;
    for (eli = m_entries.begin(); eli != m_entries.end(); ++eli) {
        PEntry pe = *eli;
        int entry_val = *(int *)pe->global_ptr();
        if (val == entry_val) return pe->name;
    }
    return "<undef>";
}


namespace {
// private utilities!
bool grab_list(const TypeList& tl1, TypeList& tl2)
 {
   if (tl1.size()==0) return false;
   tl2 = tl1;
   return true;
 }

// these guys provide a simplified way of generating a class method,
// of one or none arguments.  Used to construct default constructors, etc.
PEntry begin_method(string name, Type rtype, char *arg_name, Type atype, int ftype)
{
 using Parser::state;
 Class *ccntxt;
 Sig sig(rtype);
 if (arg_name) sig << arg_name << atype;
 bool wuz_function = state.context().type() == FUNCTION;
 if (wuz_function) ccntxt = (Class *)state.pop_context();
 Function *pfn = Parser::state.start_function(sig.m_type,name.c_str());
 Parser::state.pop_context(); // usually done by block-end!
 if (wuz_function) state.push_context(ccntxt);
 pfn->set_construct_destruct(ftype);
 mFe = pfn->context();
 Parser::state.push_context(pfn->context());
 return mFe->lookup(arg_name);
}

void declare_method(string name, Type rtype, char *arg_name, Type atype, int ftype)
{
 Sig sig(rtype);
 if (arg_name) sig << arg_name << atype;
 Parser::state.set_construct_destruct(ftype);
 Parser::state.declare_function(sig.m_type,name.c_str());
}

void end_method()
{
  mFe->finalize();  
  Parser::state.pop_context(); // see ParserState::finalize_block()
  Parser::state.set_construct_destruct(IsPlain); // *fix 1.2.3b
}

typedef TypeDistance (*TypeMatch) (Type,Type);

Function *get_match(const TypeList& tl, const FunctionList& fl, Type t, TypeMatch tmatch)
{
  TypeList::const_iterator tli;
  FunctionList::const_iterator fli;
  t.strip_qualifiers();
  for(tli = tl.begin(), fli = fl.begin(); tli != tl.end();  ++tli,++fli)
        if (tmatch(*tli,t) != NO_MATCH) return *fli; // *fix 1.1.3 wuz (*tli,t)
  return NULL;
}

Function *get_std_match(const TypeList& tl, const FunctionList& fl, Type t)
{
  Function *pf = get_match(tl,fl,t,trivial_match);
  if (pf) return pf;
  pf = get_match(tl,fl,t,promote_match);
  if (pf) return pf;
  pf = get_match(tl,fl,t,std_match);
  return pf;
}

}

