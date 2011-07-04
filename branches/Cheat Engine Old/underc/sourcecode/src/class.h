/* CLASS.H
 */

#ifndef __CLASS_H
#define __CLASS_H
#include "table.h"
#include "imports.h"
#include "function.h"  

class NamedObject {
 string m_name;
public:
  NamedObject(string name) : m_name(name) { }
  string name()
   { return m_name; }
  void set_name(string s)
  { m_name = s; }
};

class Enum: public NamedObject {
  EntryList m_entries;
  PEntry m_entry;
public: 
  Enum(string name) : NamedObject(name) { }
  void add_entry(PEntry);
  Table* context();
  string lookup_value(int val);
  PEntry entry()   { return m_entry; }
  void   entry(PEntry pe) { m_entry = pe; }
};

const int MAX_VMT_ENTRIES = 250, IS_STRUCT=TABLE+10;

class Function; // forward...

struct MethodBody {
    Function* pf;
    string body;

    MethodBody(Function* _pf, string _body)
        : pf(_pf),body(_body) { }
};

class Class: public NamedTable {
protected:
  TypeList m_from_list, m_to_list;
  std::list<Function *>   m_friend_objects;
  std::list<Function *> m_from_fn, m_to_fn;
  std::list<int>      m_vtable_slots;
  FunctionList        m_vtable_funs;
  std::list<MethodBody*> m_DeferedBodies;
  int m_vtable_size;
  EntryList m_obj_list,m_field_list,m_entries;
  Class **m_VMT;
  int m_slot_id;
  PEntry m_base_entry;
  TemplateInstance *m_templ_info;
  int m_base_access, m_access_mode;
  bool m_simple_struct, m_has_constructors, m_abstract,m_finalized, m_imported, m_struct, m_is_union;
  Function *m_default_constructor, *m_copy_constructor, *m_destructor;
  ImportScheme *m_import;

public:
  enum { NOT_RELATED = 999 };

  Class(Table *parent, int access) ;
  void Class::set_base_class(Class *base, int access);

  PEntry add(const string& name); // override Table::add()
  void list_entries(EntryList &el, int flags);  // and Table::list_entries()
  Namespace *inside_namespace();
  Function *default_constructor();
  Function *copy_constructor();
  Function *destructor();
  bool  has_constructors();
  void  add_constructor(Function *fn, bool no_implicit_type_conversion);
  void  destructor(Function *fn);
  void  construct_VMT();
  void  clear();

  string constructor_name();
  string destructor_name();
  bool is_anonymous_union(); // *add 1.2.6 support for anonymous unions
  void set_anonymous_union();

  bool  simple_struct() { return m_simple_struct; }
  bool  is_struct()     { return m_struct; }
  void  make_struct()   { m_struct = true; }

  // stuff to do with imported classes
  ImportScheme *import_scheme() { return m_import; }
  // *add 1.1.3 Directly fooling w/ the class; hack alert!
  void set_import_scheme(ImportScheme *is) { m_import = is; }  
  bool  imported();
  int   vtable_size();
  bool  derived_from_import();
  void  set_imported();
  void  update_vtable(char *obj);
  char *check_object_pointer(char *p);

  // generating the lists of _non-static members_
  bool build_obj_list();

  // auto contruction/destruction code generation
  void compile_methods();
  void add_defered_method_body(Function* fn, char* body_buff);
  bool auto_construct(bool make_method);
  void auto_destruct(bool make_method);
  void auto_assign_copy(bool is_assign); 
  void add_class_init_list(PEntry pe, PExprList pel);
   
  // inheritance relationships 
  bool inherits_from(Class *pc);
  int  distance_from(Class *pc);
  Class *base_class();
  
  // finishing off class creation 
  PEntry get_constructor();
  void finalize();
  int size();    
      
  //*TEMP*
  void set_from_type_list(TypeList& tl) { m_from_list = tl; }
  void set_to_type_list(TypeList& tl) { m_to_list = tl; }

  // user-defined conversions to & from this class
  void set_conversion_to(Function *fn);
  bool get_from_type_list(TypeList& tl);
  bool get_to_type_list(TypeList& tl);
  void copy_convert_to_list();
  Function *get_conversion_to(Type t);
  Function *get_conversion_from(Type t);

  //access control
  void  set_access_mode(int mode)     { m_access_mode = mode; }
  int   get_access_mode()             { return m_access_mode; }
  int   base_access_mode()            { return m_base_access; }
  bool is_friend_class(Class *pc);
  bool is_friend_function(void *fn); 
  void add_friend_object(void *pc, bool is_class);
  
  // virtual method management
  Class**   get_VMT()       { return m_VMT; }
  int       last_slot()     { return m_slot_id; }
  int       next_slot()     { return ++m_slot_id; }
  bool      has_VMT();
  void      set_slot(int id, Function *data);
  PFBlock   get_slot(int id);
  void      make_abstract() { m_abstract = true; }
  bool      is_abstract()   { return m_abstract; }

  void      attach_VMT(void *obj, PPClass vmt=NULL);
  static PPClass find_VMT(void *obj);
  bool      has_true_VMT();

  // general utilities & template info
  //*SJD* No longer override base routine...
 // void dump_entries(ostream& os, int depth);
 void add_line_no(const string& file, int line);
 static Class *generate_try_block_handler();
 bool make_available();
 bool is_template()                       { return m_templ_info != NULL; }
 TemplateInstance *get_template()         { return m_templ_info;  }
 void set_template(TemplateInstance *pti) { m_templ_info = pti; }
};

typedef Class *PClass, **PPClass;


#endif
