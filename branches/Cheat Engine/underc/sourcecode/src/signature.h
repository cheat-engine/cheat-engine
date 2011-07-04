/* SIGNATURE.H
 */

#ifndef _SIGNATURE_H
#define _SIGNATURE_H

// for now, Signature acts as a list of types
// and also has a return type and a (maybe NULL) class ptr.
// *fix 1.2.3 (Eric) Ensure that all fields are properly initialized
#include <list>
typedef std::list<string> ArgList;

class Signature {
protected:
  Type m_ret_type;
  Class *m_class_ptr;
  TypeArray m_arg_types;
  int m_byte_size;
  bool m_stdarg;
  bool m_is_const, m_is_static;  // only for methods
  ArgList *m_arg_names;
public:
  typedef TypeArray::const_iterator iterator;

  Signature(Type rtype=t_void, Class *cptr=NULL)
   : m_ret_type(rtype),m_class_ptr(cptr),m_arg_names(NULL),m_is_const(false),
     m_is_static(false),m_byte_size(0),m_stdarg(false)
    {}

  Signature(Type rtype, const TypeArray& tl, Class *cptr=NULL)
   : m_ret_type(rtype),m_class_ptr(cptr),m_is_const(false), m_is_static(true),m_arg_names(NULL),
     m_stdarg(false),m_byte_size(0)
  {
    m_arg_types.assign(tl.begin(),tl.end());
  }

  void   set_arg_names(const ArgList& sl) 
  {  m_arg_names = new ArgList(sl);  }

  ArgList *get_arg_names() const { return m_arg_names; }

  static void set_fun_name(const string& name, bool ctor=false, bool dtor=false);
  static string get_fun_name();
  static void write_qualified_names(bool yesno);

  Type   return_type()     const    { return m_ret_type; }
  void   adjust_return_type(Type t) { m_ret_type = t;    }
  Class *class_ptr()       const    { return m_class_ptr; }
  void   set_class_ptr(Class *pc)   { m_class_ptr = pc;   }
  int    byte_size()       const    { return m_byte_size; }
  void   byte_size(int sz)          { m_byte_size = sz;  }
  void   set_const()                { m_is_const = true; }
  bool   is_const()        const    { return m_is_const; }
  void   set_not_static()           { m_is_static = false; }
  bool   is_static()       const    { return m_is_static; } 
  bool   stdarg()          const    { return m_stdarg; }
  void   stdarg(bool stdarg)        { m_stdarg = stdarg; }  

  bool   match(const Signature& sig, bool do_strict=false) const;

  // list-like interface!
  void push_back(Type t) { m_arg_types.push_back(t); }
  int size()       const { return m_arg_types.size(); }
  iterator begin() const { return m_arg_types.begin(); }
  iterator end()   const { return m_arg_types.end(); }

 // this naive impl. is poss. because TypeArray _really_ is a TypeList!!
  const TypeList& type_list() const { return m_arg_types; }

};


#endif
