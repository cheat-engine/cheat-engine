/* BREAKPOINTS.CPP
 * implements Module and Breakpoint classes
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
*/

#include "module.h"
#include "breakpoints.h"
#include "opcodes.h"
#include "tokens.h"
#include "std_utils.h"
#include "os.h"
// for remove_if()
#include <ctype.h>
#include <algorithm>

// for file times....
//#include <time.h>
//#include <sys/types.h>
//#include <sys/stat.h>
// *ch 1.2.9 patch
#ifdef __BEOS__
/* where did you get this one ??? */
#define _stricmp strcasecmp
#endif

struct NamespaceEntry {
    Namespace *nspace;
    InjectState *istate;
};

typedef std::list<NamespaceEntry> NamespaceEntryList;

namespace {
  Breakpoint *mg_bkpr_arr[MAX_BREAKPOINTS];
  //Module *mg_current_module;
  ModuleList mg_module_list;
  int mg_last_id = 0;
  BreakpointList mg_temp_list;

  NamespaceEntryList mg_nspace_entry_list;
}

static Module *mg_current_module;


Module::Module(const string& name, int id)
: m_name(name), m_id(id), m_rc(1), m_modified(false)
{
  m_modified_time = get_file_time(name.c_str());
}


Function *
Module::function_from_file(const string& file, int lineno)
  {
    Module *pm = from_name(file);
    if (pm == NULL) return NULL;
    return pm->function_at(lineno);
  }

 
  Function *
  Module::function_at(int lineno)
  {  
    ModuleEntryList::iterator meli;
    FORALL(meli,m_entry_list)
      if (meli->type()==FUNCTION && meli->contains(lineno))
		  return meli->function();
    return NULL;         
  }

  Module *
  Module::from_id(int id) {
    ModuleList::iterator mli;
    FORALL(mli, mg_module_list)
     if ((*mli)->id() == id) return *mli;
    return NULL;
  }

// *fix 1.1.2 Non-win32 file comparisons are (usually) case-sensitive.
// *ch 1.2.9 patch
#ifndef _WIN32
  bool is_equal(const string& s1, const string& s2)
  { return s1 == s2; }
#else
  bool is_equal(const string& s1, const string& s2)
  //*shouldn't be case sensitive for Win32....
  {
    return _stricmp(s1.c_str(),s2.c_str()) == 0;
  }
#endif

  void
  Module::remove(Module *pm)
  {
    mg_module_list.remove(pm);
  }

  Module *
  Module::from_name(const string& file) {
  
    ModuleList::iterator mli;
    FORALL(mli, mg_module_list)
     if (is_equal((*mli)->name(),(string&)file)) return *mli; 
    return NULL;
  }

  Module *
  Module::create(const string& file) {
     Module *pm = from_name(file);
     if (pm == NULL) {
       pm = new Module(file,mg_last_id++);
       mg_module_list.push_back(pm);
     } else pm->inc_refcount();
     return pm;
  }

  Module *
  Module::current() { return mg_current_module; }

  void
  Module::set_current(const string& file, bool is_entering)
  {
    Module *pm = create(file);
    if (is_entering && mg_current_module)
        mg_current_module->m_dependencies.push_back(pm);
    mg_current_module = pm;
  }

  int
  Module::file_from_function(Function *pf, string& file)
  {
    ModuleList::iterator mli;
    FORALL(mli, mg_module_list) {
      Module *pm = *mli;
      ModuleEntryList::iterator meli;
      FORALL(meli,pm->m_entry_list)
        if (meli->function() == pf) {
           file = pm->name();
           return meli->lstart();
        }
    }  
    return 0;  // meaning, no can do.
  }

  string
  Module::anonymous_namespace_name()
  {
   return "$AN" + itos(mg_current_module->id());
  }

  void
  Module::add_namespace(Namespace *ns)
  {
// *fix 1.2.3 Keep the namespace list entries unique...
      NamespaceEntryList::iterator neli;
      FORALL(neli,mg_nspace_entry_list) 
          if (neli->nspace == ns) return;

      NamespaceEntry ne;
      ne.istate = NULL;
      ne.nspace = ns;
      mg_nspace_entry_list.push_back(ne);
  }

  void
  Module::clean_namespaces(bool do_save)
  {
   NamespaceEntryList::iterator neli;
   FORALL(neli,mg_nspace_entry_list) {
     Namespace   *nspace = neli->nspace;
     if (nspace == NULL) continue;  // *NOTE* how can this be zero?
     // *add 0.9.4 Global Namespace only cleaned out in strict mode
     // *gets rid of hard-to-explain 'using namespace std'!
     if (!Parser::debug.strict && nspace->name()=="$G$") continue;
     // note: InjectState::clone() will return NULL if the 
     // state was _trivial_.  We save a copy because clean()
     // will also clear out the injection state.
     if (do_save) neli->istate = nspace->inject_state()->clone(); 
     nspace->clean();
   }     
  }

  void
  Module::restore_namespaces()
  {
  // get everything back to the clean state (the loaded module
  // may well have messed with the namespaces) But don't interfere 
  // with previously save state!
   clean_namespaces(false);
  // and restore ourselves to the (presumably) messed state
  // we were in before loading the new module.
   NamespaceEntryList::iterator neli;
   FORALL(neli,mg_nspace_entry_list) {
     InjectState *istate = neli->istate;
     Namespace   *nspace = neli->nspace;
     if (istate) {
         *nspace->inject_state() = *istate;
         nspace->restore();
     }
    }
  }


  void 
  Module::dump_entries(ostream& os, int flags)
  {
    ModuleList::iterator mli;
    FORALL(mli, mg_module_list) {
      Module *pm = *mli;
      pm->dump(os,0);
    }
  } 

  void Module::dump(ostream& os, int flags)
  {
   os << name() << ' ' << id() << ' ' << refcount() << endl;
   if (flags & FUNS) {
    entry_iterator ei;
    os << "Functions:\n";
    FORALL(ei, m_entry_list)
	  if (ei->type()==FUNCTION)	os << ei->function()->name() << endl;
   }
   if (flags & DEPEND) {
       ModuleList::iterator mli;
       FORALL(mli,m_dependencies) os << (*mli)->name() << endl;
   }
  }

  void  Module::get_modules(ModuleList& ml)
  { ml = mg_module_list;  }

  void  Module::get_dependencies(ModuleList& ml)
  { ml = m_dependencies;  }

  Module::entry_iterator
  Module::find(void *pf)
  {
    entry_iterator ei;
    FORALL(ei, m_entry_list)
      if (ei->object() == pf) return ei;
    return entry_end();
  }

  ModuleEntry::ModuleEntry(void *pf, int l1, int l2, int type)
  {
    m_pf = pf;  m_start = l1;  m_end = l2;
	m_type = type;
  }

  void Module::add_entry(ModuleEntry& me)
  {
    entry_iterator ei = find(me.object());
    if (ei != entry_end()) *ei = me;
    else m_entry_list.push_back(me); 
  }

 void
 Module:: add_function(Function *pf, int l1, int l2)
 {
   ModuleEntry me(pf,l1,l2,FUNCTION);
   add_entry(me);
 }

 void
 Module::add_class(Class *pc, int l1, int l2)
 {
   ModuleEntry me(pc,l1,l2,IS_STRUCT);
   add_entry(me);
 }

 void
 Module::add_typename(PEntry pe)
 {
    utils::add_unique(m_typenames,pe);
 }

 bool
 Module::is_inside(Function *pf, int l)
  {
    entry_iterator meli;
    FORALL(meli,m_entry_list)
      if(meli->type()==FUNCTION && meli->contains(l)) return true;
    return false;
  }

 // *add 1.2.7 given a function, on what line is it?
 int
 Module::get_function_line(Function* pf)
 {
    if (pf->undefined()) return pf->line(); // *add 1.2.7

    entry_iterator ei = find(pf);
    if (ei != entry_end()) return ei->lstart();
    else return -1;
 }

  static bool was_typedef(PEntry pe)
  { 
  // *fix 0.9.4 Both typedef and enum symbols must be cleaned out
  // (otherwise they cause syntax errors when recompiled)
    if (pe->is_typedef() || pe->type.is_enum()) {
      pe->context->remove(pe);
      return true;
    }
    return false;
  }

  void
  Module::clean_macros_and_typedefs()
  {
    std::remove_if(m_typenames.begin(), m_typenames.end(), was_typedef);
    StringList::iterator sli;
    FORALL(sli,m_macros) TokenStream::macro_delete(sli->c_str());
    m_macros.clear();
  }

  bool
  Module::is_modified()
  {
   m_visited = true;
   if (m_modified) return true;
   long file_time = get_file_time(name().c_str());
   bool trace = Parser::debug.verbose;
   if (file_time > m_modified_time) {
      if (trace) cmsg << "changed " << name() << endl;
      m_modified_time = file_time;
      m_modified = true;

      // prepare for a fresh compile....
      clean_macros_and_typedefs();
      return true;
   } else {
     if (trace) cmsg << "unchanged " << name() << endl;
     return false;
   }   
  }

  bool
  Module::needs_rebuilding()
  {
   if (m_visited) return false;
   bool changed =  is_modified();

   // go through the dependency list....
   // we _still_ have to do this, even if the module is obviously
   // changed, because is_modified() has the crucial side-effect
   // of stripping out macros and typedefs from any module.
   ModuleList::iterator mli;
   bool check;
   FORALL(mli,m_dependencies) {
     check = (*mli)->needs_rebuilding();
     changed = changed || check;
   }
   
   return changed;
  }

  void
  Module::reset_flags()
  {
   m_modified = false;
   m_visited = false;
   ModuleList::iterator mli;
   FORALL(mli,m_dependencies) {
     (*mli)->m_modified = false;   
     (*mli)->m_visited = false;
   }
  }

  void
  Module::reset_modify_flags()
  {
    ModuleList::iterator mli;
    FORALL(mli, mg_module_list) {
      (*mli)->m_modified = false;
      (*mli)->m_visited = false;
    }
  }


  Instruction *ip_at_line(Function *pf, int lineno)
  {
     int ip = pf->line_nos()->lookup_ip(lineno);
     Instruction *ip_ptr = ip + pf->fun_block()->pstart;  
     // NB that our instruction does actually have a successor; 
     // this checks to see that we aren't the last instruction in the routine...
     // in general, we have to avoid _any_ instruction which can divert flow of control!
     if ((ip_ptr+1)->opcode == 0) ip_ptr--;
     //cout << (void *)ip_ptr << ' ' << (int)ip_ptr->opcode << endl;  //*DEBUG*
     return ip_ptr;
  }

  
  Breakpoint::Breakpoint(int id, bool persist, Function *pf, int lineno)
 : m_id(id),m_line(lineno),m_pf(pf), m_persistent(persist), m_paused(false), m_set(false)
  {
     // m_pi = ip_at_line(pf,lineno);
     set_line(lineno);
  }

  Breakpoint *
  Breakpoint::create(const string& filename, int lineno, bool persist) {
    Function *pf = Module::function_from_file(filename,lineno);
    if (pf == NULL) return NULL;  // didn't succeed....

    //...find an id 
    int i;
    for(i = 0; i < MAX_BREAKPOINTS; i++)
      if (mg_bkpr_arr[i] == NULL) break; 
     
    Breakpoint *pb = new Breakpoint(i,persist,pf,lineno);
    mg_bkpr_arr[i] = pb;
    return pb;
  }

  Breakpoint::iterator
  Breakpoint::find_in_function(Function *pf)
  {
     mg_temp_list.clear();
     for(int i = 0; i < MAX_BREAKPOINTS; i++)
      if (mg_bkpr_arr[i] != NULL && mg_bkpr_arr[i]->function() == pf)
           mg_temp_list.push_back (mg_bkpr_arr[i]);
     return mg_temp_list.begin();
  }

  Breakpoint::iterator
  Breakpoint::end_list()
  {
    return mg_temp_list.end();
  }

  void add_breakpoint_in_order(BreakpointList& bpl, Breakpoint *bp)
  { 
      Breakpoint::iterator bpli;
      for(bpli = bpl.begin();  bpli != bpl.end();  bpli++)
          if (bp->line() < (*bpli)->line()) { bpl.insert(bpli,bp); return; }
      bpl.push_back(bp);   
  }

  Breakpoint::iterator
  Breakpoint::find_in_file(const string& file)
  {
     mg_temp_list.clear();
     Module *pm = Module::from_name(file);
     if (pm == NULL) return end_list();
     int id = pm->id();   
     for(int i = 0; i < MAX_BREAKPOINTS; i++)
         if (mg_bkpr_arr[i] != NULL) {
           Breakpoint *bp = mg_bkpr_arr[i];
           if (bp->function()->line_nos()->module() == id)
              //mg_temp_list.push_back (bp);
              add_breakpoint_in_order(mg_temp_list,bp);
         }
     return mg_temp_list.begin();
  }

  Breakpoint *
  Breakpoint::exists_at(const string& filename, int lineno)
  {
    Function *pf = Module::function_from_file(filename,lineno);
    if (pf == NULL) return NULL;  // didn't succeed....

    iterator bli = find_in_function(pf);
    while (bli != end_list()) {
        if((*bli)->line()==lineno) return *bli;
        ++bli;
    }
    return NULL;
  }

  Breakpoint *
  Breakpoint::from_id(int id) { return mg_bkpr_arr[id]; }

  Breakpoint::~Breakpoint() {
     // retire the id
     mg_bkpr_arr[m_id] = NULL;
     restore_instruction();
  }

  bool
  Breakpoint::set_line(int l)
  {
      m_line = l;
      // we actually need the code at the end of the _previous_ line!
      m_pi = ip_at_line(function(),line()-1);
      if (m_pi->opcode == HALT) return false;  // *fix 0.9.8 don't try to reset existing BP
      m_set = false;
      set_break();
      return true;
  }

  void
 Breakpoint:: set_break()
  {
   // if (m_set) return;
  //s   m_set = true;
    m_saved_instruction = *m_pi;
    Instruction instr;
    instr.opcode = HALT;
    instr.data   = m_id;
    *m_pi = instr;
  }

void
Breakpoint:: restore_instruction()
  {     *m_pi = m_saved_instruction;   } 

static Breakpoint *mCurrentBreak = NULL;
Breakpoint *current_break() { return mCurrentBreak; }

bool
Breakpoint:: execute() {
    restore_instruction();
    if (m_persistent) {
     if (!m_paused) { // set a trap at the next instruction
        m_pi++;
        set_break();
        m_paused = true;
        mCurrentBreak = this;
        return true;    // and break!
     } else {        // which we use to _reset_ the breakpoint!
       m_pi--;
       set_break();
       m_paused = false;
       mCurrentBreak = NULL;
       return false;  // and continue!
     }
    } else {
      delete this;
      return true;  // and break!
    }
 }
  
void
Breakpoint::toggle(char *file, int lineno, bool is_persistent, ostream& out)
{
 if (Module::from_name(file) == NULL) {
        out << "module '" << file << "' not found\n";
        return;
 }
 Breakpoint *pb = exists_at(file,lineno);
// cout << file << ' ' << lineno << endl;
 out << "breakpoint ";
 if (is_persistent) { // persistent breakpoint
        if (pb) {
          int line = pb->line();
          delete pb;  // if it already exists, toggle it off
          out << line << " unset\n";
        } else {
          pb = create(file,lineno,true);       
          if (pb != NULL) out << pb->line() << " set\n";
          else out << "0 unset\n"; // some mysterious error...
        }  
  } else {
      if (pb == NULL) {
         pb = create(file,lineno,false);
      } 
     out << pb->line() << " temp set\n";
  }
}

void
Breakpoint::group(char *file, int *lines, int& sz, bool do_get)
{
    int i = 0;
    iterator bli = find_in_file(file);
    while (bli != end_list()){
       Breakpoint* bp = *bli;
       if(do_get) lines[i] = bp->line();      // *fix 1.2.7 list _all_ breakpoints, but...   
       else  if (! bp->m_paused) {            // *fix 0.9.8 don't fool with any paused breakpoint
             bp->set_line(lines[i]);            
          } 
        i++;
        bli++;
    }
    if (do_get) sz = i;
}


void
Breakpoint::remove_all()
{
   for(int i = 0; i < MAX_BREAKPOINTS; i++)
     if (mg_bkpr_arr[i] != NULL) delete mg_bkpr_arr[i];
       
}



