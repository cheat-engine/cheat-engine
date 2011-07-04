// DIRECTCALL.H
#ifndef __DIRECTCALL_H
#define __DIRECTCALL_H

typedef int (*CALLFN) (void);

// flags for calling callfn()
const int DC_CDECL = 1, DC_QWORD = 2, DC_NOWORD = 4, DC_RET_OBJ = 8, DC_RET_VAL = 16 + 8;       

class Function;

struct NFBlock {
  CALLFN pfn;   // the function to be called    
  int nargs;    // dwords in call (-1 means use TOS)
  int flags;   // cdecl or stdcall? returns dword or qword?
  static int create(Function *pfn, CALLFN fn);
};

void callfn(CALLFN fn, int args[], int argc, void *optr, int flags, void *buff);

#include "types.h"
struct Sig {
 Type m_type;
 char *m_arg_name;
 Sig(Type t) : m_type(t),m_arg_name(NULL) {}
 Sig& operator << (Type t);
 Sig& operator << (char *arg_name);
 void set_const(bool t);
};

#define IMPLICIT_LINK ((void *)-1)

namespace Builtin {  
  void init();
  int alloc_size(void *p);
  void bad_ptr_check(void *p);
  FBlock *imported_fblock_from_function(void *pfn);
  FBlock *import_vmethod(CALLFN fn, PFBlock pf);
  bool add_dll_function(Function *pfn, int modifier, string& name);
  void *get_dll_handle();
  void set_direct_import(void* p);
  void set_dll_handle(void *dl);
  void unload_lib(void *hlib);
  bool set_current_lib_file(char *file); 
  string get_current_lib_file();
  void finis();
  void *generate_native_stub(Function *pfn);
  int range_check_function();

  bool using_ordinal_lookup();
  bool lookup_is_ordinal();
  int lookup_ordinal(const char *name);
  bool generate_ordinal_lookup(const char *index_file);
  void cleanup_ordinal_lookup();
}


#endif

