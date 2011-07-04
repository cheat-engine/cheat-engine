// Module.h
#ifndef __MODULE_H
#define __MODULE_H
#include "classlib.h"
#include "common.h"
#include "engine.h"
#include <list>

class ModuleEntry {
private:
  int m_start, m_end;  // line number range
  int m_type;
  void *m_pf;      // for a given function, class, etc

public:
  ModuleEntry(void *pf, int l1, int l2, int type);

  bool contains(int l)
  {
     return l >= m_start && l <= m_end;
  }

  void  *    object()     { return m_pf; }
  Class *    as_class()   { return (Class *)m_pf; }
  Function * function()   { return (Function *)m_pf; }
  int type()   { return m_type;  }
  int lstart() { return m_start; }
  int lend()   { return m_end;   }
};
class Module;
typedef  std::list<ModuleEntry> ModuleEntryList;
typedef std::list<Module *> ModuleList;

class Module {
private:
  string m_name;
  int m_id, m_rc;
  long m_modified_time;
  bool m_modified, m_visited;
  ModuleEntryList m_entry_list;
  ModuleList m_dependencies;
  EntryList m_typenames;
  StringList m_macros;
public:
    enum { FUNS = 1, DEPEND = 2, ALL = 3 };
  typedef ModuleEntryList::iterator entry_iterator;
  Module(const string& name, int id);
  Function *function_at(int lineno);
  string name() { return m_name; }
  int    id()   { return m_id;   }
  int    inc_refcount() { return m_rc++; }
  int    refcount()     { return m_rc;   }

  static Function *function_from_file(const string& file, int lineno);
  static int       file_from_function(Function *pf, string& file);
  static Module *create(const string& file);
  static Module *current(); 
  static void remove(Module *pm);
  static Module *from_id(int id);
  static Module *from_name(const string& file);
  static void add_namespace(Namespace *ns);
  static void clean_namespaces(bool do_save = true);
  static void restore_namespaces();
  static string anonymous_namespace_name();
  static void set_current(const string& file, bool is_entering);
  static void dump_entries(ostream& os, int flags);
  static void reset_modify_flags();
  static void get_modules(ModuleList& ml);
  void  get_dependencies(ModuleList& ml);
  void reset_flags();
  void dump(ostream& os, int flags);

  entry_iterator  entry_begin() { return m_entry_list.begin(); }
  entry_iterator  entry_end() { return m_entry_list.end(); }  
  entry_iterator  find(void *pf);

  void add_entry(ModuleEntry& me);
  void add_function(Function *pf, int l1, int l2);
  void add_class(Class *pf, int l1, int l2);
  void add_typename(PEntry pe);
  void add_macro(const string& s)  { m_macros.push_back(s);     } 
  int  get_function_line(Function* pf); 
  void clean_macros_and_typedefs();
  bool is_inside(Function *pf, int l);
  bool is_modified();
  bool needs_rebuilding();
};
#endif
