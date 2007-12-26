
// imports.h

#ifndef __IMPORTS_H
#define __IMPORTS_H

class Function;
class Class;
struct FBlock;
typedef FBlock *PFBlock;
typedef PFBlock *VMTArray;
typedef std::list<VMTArray> VMTList;
typedef int (*CALLFN) ();
typedef CALLFN *VTable;
typedef VTable *PVTable;

class ImportScheme {
protected:
  VTable m_vtable;
  bool   m_patched;
  bool   m_cpp_friendly;
  int    m_vtable_size;
  int    m_vmt_ofs;
  int    m_scheme_type;  // *add 1.2.7
  VMTList *m_ghost_classes;
  virtual int index(int id)  { return id; }

public:
 ImportScheme();
 bool patched()                { return m_patched; }
 void now_patched()            { m_patched = true; }
 int  vmt_offset()             { return m_vmt_ofs;  }
 void vtable_size(int sz)      { m_vtable_size = sz; }
 int  vtable_size()            { return m_vtable_size; }
 bool cpp_friendly()           { return m_cpp_friendly; }
 void scheme_type(int st)      { m_scheme_type = st;   }
 int  scheme_type()            { return m_scheme_type; }

// overrideables
 virtual bool uses_stdmethod() { return false; }
 virtual bool pre_construct()  { return true;  }
 virtual bool ret_obj_popped() { return false; }
 virtual bool true_pass_by_value(Type) { return false; }
 virtual bool true_return_by_value(Type) { return false; }
 virtual void set_first_virtual_class(int) { }
 virtual int       vtable_full_size() { return m_vtable_size; }
 virtual VTable    clone_vtable();
 virtual void      vtable_write_slot(int i, CALLFN pf);
 virtual CALLFN    vtable_read_slot(int i);
 virtual void      vtable_write(char *obj);
 virtual VTable    vtable_read(char *obj);
 virtual bool      vtable_at_begining() { return true; }
 VMTArray hunt_for_matching_VMT(VMTArray vmt, Class *pc);
};

namespace Import {
  ImportScheme *create_scheme(int scm=-1);
  ImportScheme *create_compat_scheme();
  ImportScheme *scheme();
  void          reset(bool new_dl);
  bool          set_scheme(string keyword);
  void*         load_method_entry(Function *pfn, string& mangled_name);
  bool          cpp_friendly();
}

#endif

