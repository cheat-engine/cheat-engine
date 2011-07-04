/* TABLE.H
 */
#ifndef __TABLE_H
#define __TABLE_H
#include "classlib.h"
//#define NULL 0

#include <list>
typedef std::list<string> StringList;

class Allocator {
//---------------
protected:
 char *m_buff, *m_data;
 bool  m_own_ptr;
 int   m_mem_unit;  // usually 1, but some like 4 bytes!
 int m_reserved;
 unsigned char m_dword_align;
 int m_move_to_next;
public:
  Allocator(void *buff=NULL, int sz=0);
  ~Allocator();

  static int dword_align(int sz);
  static int qword_align(int sz);

  void set_dword_align(unsigned char ipack) 
  { m_dword_align = ipack; }

  unsigned char get_alignment() 
  { return m_dword_align; }

  void set_mem_unit(int unit) 
   { m_mem_unit = unit; }

  void switch_off_allocator_advance()
  {  m_move_to_next = 0; }

  int current_offset();
  
  int size()
  {  return (int)m_data - (int)m_buff;  }

  void clear()
  { m_data = m_buff; m_reserved = 0; }

  void reserved_space(int sz)
  { m_reserved = sz; }

  virtual int alloc(int sz, void *data);
  int alloc_qword();

  int alloc_int(int val)
  { return alloc(sizeof(int),&val); }

  int alloc_double(double fval)
  { return alloc(sizeof(double),&fval); }

  void *addr(int offs)
  { return (void *)(m_buff + offs); }

  int offset(void *ptr)
  { return (int)ptr - (int)m_buff; }

  void set_base(void *ptr)
  { m_buff = (char *)ptr; }
};

struct Entry;
typedef Entry *PEntry;
typedef std::list<PEntry> EntryList;

#ifdef IN_TABLE_CPP
  #include <map>
  typedef std::map<string,Entry *> EntryMap;
#else
  typedef int EntryMap;
#endif

enum RMode { // address modes
  NONE,
  DIRECT,  // 'direct' here means 22bit offset into data area
  SREL,    // stack-relative (auto variables)  
  OREL,     // object-relative (class members)
  OREL_F    // bit-field (will also be object-relative)
};

const int TABLE = 100, IS_NAMESPACE = 105;

// *add 1.2.3b Extra enum to flag entries which are not yet defined...
enum { NotDerived,Public,Protected,Private,Default,ForwardClass, NotInitialized };

class Namespace;
typedef std::list<Namespace *> NamespaceList;

enum { 
    TEMPS=1, CONSTS=2, FUNCTIONS=4, NON_STATIC=8, VIRTUALS=16, BUILTINS=32,
    CTORS=64, DTORS=128, TYPEDEFS=256, NAMESPACES=512, CLASSES=1024, FIELDS=4096, DO_PARENT=2048,
    IMPORTS=8192, UNDEFINED=16384, ALL_ENTRIES= ~(2*FIELDS) 
 };

class TableSearcher {
public:
	virtual bool match(PEntry pe) = 0;
};

class Table: public Allocator {
//-----------------------------
 protected:
  EntryMap *m_map;
  Table *m_parent;  
  RMode m_mode;
  int m_entries;
  int m_type;
  int m_unwind_flag;
  void *m_user_data;
  PEntry m_first_obj;
  NamespaceList m_injected_namespaces;
  
 public:
  enum { SEMICOLON_SEP = 1, ALL_LOCAL = 2, ALL = 4, VARS = 8, TYPENAMES = 16 };

  Table(Table *parent=NULL, RMode mode=DIRECT, int sz = 0);
  PEntry lookup(const string& name, bool look_in_parent = true);
  void   add_entry(const string& name, PEntry pe);
  PEntry new_entry(const string& name);
  bool   remove(PEntry pe);
  virtual PEntry add(const string& name);
  bool   is_local_entry(PEntry pe);

  virtual void add_line_no(const string& file, int line) { }
  virtual void finalize() { }

  virtual PEntry search_entries(const char *pat,EntryList *el,int flags);
  virtual PEntry search_entries(TableSearcher *search,EntryList *el,int flags);
  virtual void dump_entries(ostream& os, int depth);
  virtual void list_entries(EntryList &el, int flags);
  static bool check_entry(PEntry pe, int flags);
  void reclaim(string s);

  Table *parent_context()        { return m_parent; }
  void   set_parent(Table *t)    { m_parent = t;    }
  RMode refmode()                { return m_mode; } 
  int count()                    { return m_entries; }
  int type()                     { return m_type; }
  void *data()                   { return m_user_data; }
  void set_data(void *p)         { m_user_data = p; }
  void set_unwind(int u)         { m_unwind_flag = u; }
  void clear();

// Object destructor management
  PEntry first_obj()      { return m_first_obj; }
  void first_obj(PEntry pe);

// Namespace management
 virtual PEntry inject_entry(PEntry pe);
 virtual bool inject_namespace(Namespace *tbl);
 virtual bool is_injected(Namespace *tbl);
};  

class NamedTable: public Table {
private:
  PEntry m_entry;
public:
  NamedTable(Table *parent, RMode mode, int sz);
  string name();

  PEntry entry()             { return m_entry; } 
  void   entry(PEntry entry) { m_entry = entry; }
};

class InjectState {
private:
  EntryList m_masked_entries, m_injected_entries;
  NamespaceList m_injected_namespaces;
public:
    InjectState *clone();
    void         clear();
    void add_masked_entry(PEntry pe)           { m_masked_entries.push_back(pe); }
    void add_injected_entry(PEntry pe)         { m_injected_entries.push_back(pe); } 
    void add_injected_namespace(Namespace *ns) { m_injected_namespaces.push_back(ns); }
    EntryList&     masked_entries()            { return m_masked_entries; }
    EntryList&     injected_entries()          { return m_injected_entries; }   
    NamespaceList& injected_namespaces()       { return m_injected_namespaces; }
};


class Namespace: public NamedTable {
private:
    InjectState *m_inject_state;
public:
    Namespace(Table *parent, int sz = 20);
    ~Namespace();
    InjectState *inject_state();
    bool is_clean()                          { return m_inject_state == NULL; }
    void clean();
    void restore();


    // override!
   PEntry inject_entry(PEntry pe);
   bool inject_namespace(Namespace *tbl);
};

class Global: public Namespace {
public:
    Global(int sz);
    void finalize(); //override
};

#include "entry.h"



#endif

