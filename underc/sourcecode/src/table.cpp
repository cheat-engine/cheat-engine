/* TABLE.CPP
 * Lookup tables
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 *
 * Describes general lookup tables, from which all
 * contexts, such as namespaces, function blocks, classes etc
 * are derived.
 */

// get rid of nasty & irrevalent messages about debug map names
#pragma warning(disable:4786)
#define IN_TABLE_CPP
#include "table.h"
#include "std_utils.h"

//------------------ CLASS Allocator

int Allocator::dword_align(int sz)
//--------------------------------
{ 
  if (sz % 4 == 0) return sz;
  else return sz + 4 - sz%4;
}

// *add 1.2.0 Support for 8-byte alignment of doubles (MS default)

int Allocator::qword_align(int sz)
//--------------------------------
{ 
  if (sz % 8 == 0) return sz;
  else return sz + 8 - sz%8;
}

Allocator::Allocator(void *buff, int sz)
: m_dword_align(true),m_reserved(0),m_own_ptr(false),m_mem_unit(1),m_move_to_next(-1)
//-------------------------------------------------------------------------------------
{ 
  m_buff = (char *)buff;
  if(sz)   { m_buff = new char [sz];  m_own_ptr = true; }
  m_data  = m_buff;
}

Allocator::~Allocator()
{
 if (m_own_ptr) delete [] m_buff;
}

inline int max(int i, int j)
{ return i > j ? i : j; }

int Allocator::alloc(int sz, void *data)
//--------------------------------------
{
  int ret;
  char *start;
 // *add 1.2.6 allocators for unions are reset on each new allocation
  if (m_move_to_next > -1) {
	  m_move_to_next = max(sz,m_move_to_next); 
	  m_data = (char*) m_move_to_next;
	  return 0;
  } else {
    start = m_data;
    // *fix 1.2.3 Since the alignment flag redefinition, we've not been respecting pack(1)
    if (m_dword_align > 1) sz = dword_align(sz);
    if (data && m_data) memcpy(m_data, data, sz); 
    m_data += sz;
    ret = (long)start - (long)m_buff;
    return ret/m_mem_unit;
  } 
}

int Allocator::alloc_qword()
//---------------------------
{
  if (m_move_to_next > -1) return alloc(8,NULL);
// *add 1.2.0 this is specialized to return proper 8-byte aligned offsets...
  m_data = (char *)qword_align((int)m_data);
  char *start = m_data;
  m_data += 8;
  return (int)start;
}

int Allocator::current_offset()
//-----------------------------
{
  return ((long)m_data - (long)m_buff)/m_mem_unit;
}


////------------------ CLASS Table
Table::Table(Table *parent, RMode mode, int sz)
: Allocator(NULL,sz),m_parent(parent),m_mode(mode),
  m_entries(0), m_first_obj(NULL),m_unwind_flag(0)
{
 m_map = new EntryMap;
 m_type = TABLE;
}

bool
Table::is_local_entry(PEntry pe)
//-----------------------------
{ return pe ? pe->context == this : false; }

PEntry
Table::lookup(const string& name, bool look_in_parent)
//---------------------------------------------------
{
 PEntry se;
 /// Look in any injected namespaces....
  NamespaceList::iterator nli;
  if (m_injected_namespaces.size() > 0)
   FORALL(nli,m_injected_namespaces)
    if((se = (*nli)->lookup(name,false)) != NULL) return se;

 /// Try to look up this symbol locally
 try {
  EntryMap::iterator iem = m_map->find(name);
  se = (iem != m_map->end()) ? iem->second : NULL;
  if (se) return se; 
 } catch(...) {
   return NULL;
 }

 /// If not found, then ask our parent context (if it exists)
 if(m_parent && look_in_parent) {
       se = m_parent->lookup(name); 
       if (se) return se; 
 } 
 return NULL;  // no can do!!
} 

                       
void
Table::add_entry(const string& name, PEntry pe)
{
 (*m_map)[name] = pe;
}

bool 
Table::remove(PEntry pe)
{
 EntryMap::iterator iem = m_map->find(pe->name);
 if (iem == m_map->end()) return false;
 m_map->erase(iem);
 return true;
}

Entry *
Table::new_entry(const string& name)
{
 PEntry se = new Entry;
 se->set_access(Public);
 se->type = t_void;
 se->name = (string)name;  // pasop w/ MSTRING.H
 se->size = 1;
 se->set_access(Public); // by default!
 se->m_typename = false; 
 se->context = this;
 return se;
}

Entry *
Table::add(const string& name)
{
 PEntry se = new_entry(name);
 add_entry(name,se);
 m_entries++;
 return se;
}

void Table::dump_entries(ostream& os,int flags)
{
  EntryMap::iterator ei;
  const char *sep = (flags & SEMICOLON_SEP) ? "; " : " ";
  bool do_vars = flags & VARS;
  int i = 1;
  for(ei = m_map->begin(); ei != m_map->end(); ++ei,++i) {
    PEntry pe = ei->second;
    // note; we exclude _temporaries_ in this enumeration!
    if (do_vars && (pe->type.is_const() || pe->type.is_signature() || pe->m_typename==true || pe->name[0]=='$')) continue;
    os << (string)ei->first << sep;
    if (i % 4 == 0) os << endl;
  }
  os << endl;
  // for now...idea is to have more control later
  if ((flags & ALL) && m_parent && m_parent->m_type == m_type) m_parent->dump_entries(os,flags);
}

// This defines how the search flags are used to filter symbol table entries
// *add 1.2.3 Can now specifically look for imported and undefined functions
bool Table::check_entry(PEntry pe, int flags)
{
	 Type t = pe->type;
	 if (t.is_function()) {
		 if (! (flags & FUNCTIONS))  return false; // only if requested explicitly!
		 // Assume here that all members of an overloaded set have the same distinguishing characteristics!
		 FunctionEntry *pfe = (FunctionEntry *)pe->data;
         // *fix 1.2.3 Template function entries are often empty
         if (pfe->size() == 0) return false;
         Function *pf = pfe->back() ;
		 if (flags & NON_STATIC && ! pf->is_method())   return false;
		 if (flags & VIRTUALS   && ! pf->is_virtual())  return false;
		 if (flags & BUILTINS   && ! pf->builtin())     return false;
		 if (flags & CTORS      && ! pf->is_constructor()) return false;
		 if (flags & DTORS      && ! pf->is_destructor())  return false;  
         if (flags & CONSTS     && ! pf->is_const())       return false;
         if (flags & IMPORTS    && ! pf->import_scheme())  return false;
         if (flags & UNDEFINED  && ! pf->undefined())      return false;
		 return true;  // plain ordinary function...
     } else { 
		 if (flags & FUNCTIONS)  return false; // this isn't a function!
		 if (pe->is_typename()) {
		   if (flags & TYPEDEFS   && pe->is_typedef())   return true;
		   if (flags & NAMESPACES && pe->is_namespace()) return true;
		   if (flags & CLASSES    && pe->is_class())     return true;
		   return false;
		 }
		 else if (flags & NON_STATIC && pe->is_direct()) return false;
		 else {
          if (! (flags & FIELDS)) return false;
	      if (flags & TEMPS  && pe->name[0] != '$')  return false;
	      if (flags & CONSTS && ! t.is_const())      return false;
		  return true;
         }
     } 	 
}

void Table::list_entries(EntryList &el,int flags)
{
  search_entries((TableSearcher *)NULL,&el,flags);
}

class PostMatcher: public TableSearcher {
private:
	const char *m_t;

public:
	PostMatcher(const char *t)
	: m_t(t) {}

   virtual bool match(PEntry pe) 
   {
     const char *s = pe->name.c_str();
     return strstr(s,m_t) != NULL;
   }

};

class PreMatcher: public TableSearcher {
private:
	const char *m_t;
	int m_sz;
public:
	PreMatcher(const char *t, int sz)
	: m_t(t), m_sz(sz) {}

   virtual bool match(PEntry pe) 
   {
     const char *s = pe->name.c_str();
     return strncmp(s,m_t,m_sz) == 0; 
   }
};

// *add 1.2.4 Can now list all symbols which _exactly_ match a name
class ExactMatcher: public TableSearcher {
private:
    string m_target;
public:
    ExactMatcher(string t)
        : m_target(t)
    {}

    virtual bool match(PEntry pe)
    {
        return pe->name == m_target;
    }
};


// *fix 1.2.4 Passing a non-wildcarded pattern will return all entries with that exact name.
PEntry Table::search_entries(const char *pat,EntryList* el,int flags)
{
  TableSearcher *tsearch;
  if (pat[0]=='*') {
      tsearch = new PostMatcher(pat+1);
  } else {
	  char *buff = strdup(pat);
	  char *s = strchr(buff,'*');
      if (s != NULL)  {
        *s = '\0';
        tsearch = new PreMatcher(buff,strlen(buff));
      } else 
        tsearch = new ExactMatcher(buff);
   }
  return search_entries(tsearch,el,flags);
}
    

// *add 1.1.4 This is a general-purpose entry searcher and extractor,
//   which will fill a list with matching entries, if the list is defined.
//   If not, then we return the first matching entry.
//   If the search object doesn't exist, then we match anything that satisfies the flags.
//   It follows the same search order as Table::lookup().
//   This function will always return a valid entry on success (that is, at least
//   one entry found) and otherwise NULL.
// *fix 1.2.0 we were bailing out immediately if we found stuff in the parent, etc
// and were collecting all entries (el != NULL)
PEntry Table::search_entries(TableSearcher *search,EntryList* el,int flags)
{
  PEntry pe;  
  // look in the parent context if requested
  if (flags & DO_PARENT && m_parent != NULL) {
	  pe = m_parent->search_entries(search,el,flags);
	  if (pe && ! el) return pe;
  }  
   /// Look in any injected namespaces....
  NamespaceList::iterator nli;
  if (m_injected_namespaces.size() > 0)
    FORALL(nli,m_injected_namespaces) {
      pe = (*nli)->search_entries(search,el,flags);   
      if(pe && ! el) return pe;
    }
  /// and finally traverse the entries...
  EntryMap::iterator ei;
  for(ei = m_map->begin(); ei != m_map->end(); ++ei) {
     pe = ei->second;
	 if (check_entry(pe,flags) && (search && search->match(pe) || ! search)) {
		 if (el) el->push_back(pe);
		 else return pe;
     }
  }
  bool success = el && el->size() > 0;
  return success ? el->front() : NULL;  
}


void Table::reclaim(string s)
{
}

void Table::clear()
{ 
 Allocator::clear();
 m_map->clear();
}

void Table::first_obj(PEntry pe)
{
 if (m_first_obj == NULL) m_first_obj = pe;
}

/*REF:NAMESPACE*/

PEntry Table::inject_entry(PEntry pe)
{
 // Add a new entry, copying the relevant information (note: the _access rights_ may change!)
 PEntry new_pe = add(pe->name);
 new_pe->type = pe->type;
 new_pe->data = pe->data;
 new_pe->rmode = pe->rmode;
 new_pe->size = pe->size;
 new_pe->m_typename = pe->m_typename;
 return new_pe;
}

void error(string msg);

bool Table::inject_namespace(Namespace *tbl)
{
    if (tbl==NULL) { error("Not a namespace!"); return false; }
    return utils::add_unique(m_injected_namespaces,tbl);
}

// *add 1.2.7 Can query whether a particular namespace has been injected into a context...
bool Table::is_injected(Namespace *tbl)
{
	return utils::find(m_injected_namespaces,tbl);
}

//------------------ CLASS NamedTable
NamedTable::NamedTable(Table *parent, RMode mode, int sz)
: Table(parent,mode,sz)
{
}

string
NamedTable::name()
{ 
 return m_entry->name;
}
//------------------ CLASS InjectState
InjectState *
InjectState::clone()
{
 if (injected_entries().size()==0 && injected_namespaces().size()==0 && 
     masked_entries().size()==0) return NULL;  // this was a TRIVIAL state!
 else {
   InjectState *is = new InjectState;
   is->injected_entries() = injected_entries();
   is->masked_entries() = masked_entries();
   is->injected_namespaces() = injected_namespaces();
   return is;
 }
}

void
InjectState::clear()
{
 injected_entries().clear();
 injected_namespaces().clear();
 masked_entries().clear();
}

//------------------ CLASS Namespace
Namespace::Namespace(Table *parent, int sz)
: NamedTable(parent,DIRECT,sz),m_inject_state(NULL)
{
 m_type = IS_NAMESPACE;
}

Namespace::~Namespace()
{
 delete m_inject_state;
}

InjectState *
Namespace::inject_state()
{
  if (m_inject_state==NULL) m_inject_state = new InjectState;
  return m_inject_state;
}

PEntry Namespace::inject_entry(PEntry pe)
{
// Are we masking an existing _local_ entry with this injected entry?
 PEntry masked_pe = lookup(pe->name);
 if (masked_pe != NULL && is_local_entry(masked_pe))
     inject_state()->add_masked_entry(masked_pe);

 PEntry new_pe = Table::inject_entry(pe);
 // keep tabs on what's been injected!
 inject_state()->add_injected_entry(new_pe);
 return new_pe;
}

bool Namespace::inject_namespace(Namespace *ns)
{
 if (!Table::inject_namespace(ns)) return false;
 inject_state()->add_injected_namespace(ns);
 return true;
}

void Namespace::clean()
{
  if (is_clean()) return;

  EntryList::iterator eli;
  // remove injected entries
  EntryList& elj = inject_state()->injected_entries();
  FORALL(eli,elj) remove(*eli);
  // restore any masked entries
  EntryList& elm = inject_state()->masked_entries();
  FORALL(eli,elm) add_entry((*eli)->name , (*eli));
  m_injected_namespaces.clear();

  inject_state()->clear();

}

void Namespace::restore()
{
  if (is_clean()) return;

  EntryList::iterator eli;
  // re-insert injected entries
  EntryList& elj = inject_state()->injected_entries();
  FORALL(eli,elj) Table::inject_entry(*eli);

  m_injected_namespaces = inject_state()->injected_namespaces();

}

//------------------ CLASS Global

// Global namespace!!
const int GLOBAL = TABLE+1;

Global::Global(int sz)
 : Namespace(NULL,sz) //Table(NULL,DIRECT,sz)
{
  m_type = GLOBAL;
  PEntry pe = new Entry;
  pe->name = "$G$";
  entry(pe);
}

void Global::finalize()
{
}



