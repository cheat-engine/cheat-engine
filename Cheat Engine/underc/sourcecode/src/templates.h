// TEMPLATES.H

#ifndef __TEMPLATES_H
#define __TEMPLATES_H

class TemplateEntry; // forward
class TemplateInstance;

const int INSTANTIATION_CONTEXT = 805;

class DummyType: public NamedObject {
private:
  Type m_type;
  PEntry m_entry;
public:
  DummyType(const string& name, Type t=t_null, PEntry pe=NULL)
   : NamedObject(name), m_type(t),m_entry(pe) { }

  Type   type()             { return m_type; }
  PEntry entry()            { return m_entry; }
  void   entry(PEntry pe)   { m_entry = pe; }
  bool   bind_to(Type t);
  string value_as_string();
  bool   unbound()          { return m_type == t_null; }
  void   unbind()           { bind_to(t_null);         }
};

class IContext: public Table {
private:
   TypeList m_formal_parms; 
public:
  IContext(Table *parent);
  const TypeList& formal_parms() { return m_formal_parms; }
  void set_formal_parms(const TypeList& tl) { m_formal_parms = tl; }
};

class Template {
private:
   IContext *m_context;
   char *m_buffer;
   TemplateEntry *m_templ_entry;
   TypeList m_formal_args;  // do we need this?
   string m_file;
   StringList m_methods;
   int m_lineno;
public:
   Template(TemplateEntry *te, const TypeList& tl, IContext *cntxt);

   int match(const TypeList& tl);
   void grab();
   void instantiate(TemplateInstance *inst);
   bool is_class();
   string name();
   TemplateEntry *get_entry() { return m_templ_entry; }
   IContext *context()        { return m_context; }
   StringList& methods()      { return m_methods; }
   const TypeList& formal_args();
   TypeList& formal_parms();
   static Type dummy(Type t, string name); 
   static Type dummy(PEntry pe); 
   static bool type_contains(Type t1, Type t2);
   static Template *as_template(Type t);
   static Type get_template_type(PEntry te, TypeList *ptl);
   static void do_template_header(TypeList *ptl);
   static void do_function_template();
   static void do_class_template(int s_or_c, string name, int deriv, TypeList *ptl);
   static void grab_function_body(bool plain_method, char* body);
   static char* generate_function_header(string name, Signature* sig, bool plain_method);
   static char* generate_function_header(Function* pf, bool plain_method, bool qualified_names);
};

class TemplateInstance {
private:
   TypeList m_type_parms;
   TypeList m_type_args;
   Template *m_template;
   void *m_data;
   PEntry m_entry;
   bool  m_instantiated;
public:
   TemplateInstance(Template *templ, const TypeList& parms);

   TypeList& type_parms()      { return m_type_parms; }
   Template *      get_template() { return m_template;   }
   const TypeList& type_list() { return m_type_args;        }
   void *data()                { return m_data; }
   void  data(void *d)         { m_data = d;    }
   PEntry entry()              { return m_entry; }
   void   entry(PEntry pe)     { m_entry = pe; }
   bool   instantiated()       { return m_instantiated; }
   void   instantiated(bool y) { m_instantiated = y;    } 
   string name();
   Type  type();
};

typedef std::list<Template *> TemplateList;
typedef std::list<TemplateInstance *> TemplateInstanceList;

class TemplateEntry {
private:
  TemplateList m_templates;
  TemplateInstanceList m_instances;
  bool m_is_class;    // we DO actually need it...
  bool m_template_method;
  int  m_match_idx;
  PEntry m_entry;
  EntryList *m_method_entries;
public:
  TemplateEntry(PEntry pe, bool is_class)
    :  m_entry(pe), m_is_class(is_class), m_template_method(false), m_method_entries(NULL)
    {}
  PEntry entry()          { return m_entry; }
  void   entry(PEntry pe) { m_entry = pe; }
  bool   is_class()       { return m_is_class; }
  bool   is_method()      { return m_template_method; }
  void   set_method()     { m_template_method = true; }
  string            name();
  void              add_template(Template *templ); 
  int               match_index()      { return m_match_idx; }     
  void              set_index(int i)   { m_match_idx = i; }
  string            last_error();
  void              add_method_entry(PEntry pe);
  const EntryList*  method_entries();
  TemplateInstance* match_instance();
  Template*         templates(int i);
  int               simple_match(const TypeList& tl, bool use_args = true); // returns index into TIL...
  bool              match(const TypeList& tl);
  int               no_instances();
  bool              instantiate_method(Function *pf);
  void              add_instance(TemplateInstance* ti);
};

void copy_type_list(TypeList& tlp, const TypeList& tlt);


#endif
