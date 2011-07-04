/* IMPORTS.CPP
 * Managing the importing of classes with virtual methods, etc
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 *
 * I've extracted the commonality between the two schemes (MS and GCC)
 * and implemented that as ImportScheme, which is then specialized
 * for the specific cases. 
 */

#include "common.h"
#include "mangle.h"
#include "os.h"
#include "directcall.h"
#include "imports.h"
#include <algorithm>

// found in directcall.cpp - these are used to allocate and deallocate C++ objects in the DLL
void *_new_ex(int sz);
void _delete_ex(char *ptr,int sz); 
void *_new_vect_ex(int n,int sz);

ImportScheme::ImportScheme()
 : m_vmt_ofs(0),m_patched(false),m_vtable(NULL),m_vtable_size(0),m_ghost_classes(NULL)
{
// *add 1.2.0 "cpp_friendly' means that the imported DLL uses the same overallocation strategy
// as UC, so that there's always room for a proper VMT.  Otherwise, we have to associate the VMT
// with the pointer using a map
    m_cpp_friendly = Import::cpp_friendly();
}

VTable
ImportScheme::clone_vtable()
{
    VTable ppf = new CALLFN[vtable_full_size()+5];  
    memcpy(ppf,m_vtable,sizeof(CALLFN)*vtable_full_size());
	m_vtable = ppf;	
    return ppf;
}

void
ImportScheme::vtable_write_slot(int id, CALLFN pf)
{
    m_vtable[index(id)] = pf;
}

CALLFN
ImportScheme::vtable_read_slot(int id)
{
    return m_vtable[index(id)];
}

void
ImportScheme::vtable_write(char *obj)
{
  ((VTable *)obj)[m_vmt_ofs] = m_vtable; 
}

VTable
ImportScheme::vtable_read(char *obj)
{
   m_vtable = ((VTable *)obj)[m_vmt_ofs];
   return m_vtable;
}

// MS scheme: hidden ptr always at offset 0,
// vtable is a simple table of functions begining at 0.
// default method calling convention is 'stdmethod' (obj ptr in ecx register)
// any objects are passed by pushing them directly on the stack

class MSImportScheme: public ImportScheme {
public:
  static ImportScheme *New() { return new MSImportScheme(); }

 // overrides
  bool uses_stdmethod()
   { return true; }

  bool true_pass_by_value(Type)
   { return true; }
  
};

#ifdef _WIN32
// GCC scheme: hidden ptr at the end of the last non-virtual base class
// vtable is composed of 2-word records begining at 1.
// default method calling is just cdecl.
// objects w/ copy ctors are passed by reference (like UC) but they are pre-created!
// When returning objects, the return object arg is popped. (RET 4!)

class GCCImportScheme: public ImportScheme {
public:
  static ImportScheme *New() { return new GCCImportScheme(); }

// overrides
  bool ret_obj_popped()
   { return true; }

  int index(int i)
   { return 2*(i+1)+1; }

  int vtable_full_size()
   { return index(m_vtable_size); }

  void set_first_virtual_class(int ofs) 
   { m_vmt_ofs = ofs;  }

  bool vtable_at_begining()
  { return false; }

};
#else
// GCC scheme for 2.96
// hidden ptr as before,
// vtable is composed of word records begining at 2 (at 0, 0? at 1, RTTI)
// otherwise like 2.95 MinGW
class GCCImportScheme: public ImportScheme {
public:
  static ImportScheme *New() { return new GCCImportScheme(); }

// overrides
  bool ret_obj_popped()
   { return true; }

  int index(int i)
   { return i+2; }

  int vtable_full_size()
   { return index(m_vtable_size); }

  void set_first_virtual_class(int ofs) 
   { m_vmt_ofs = ofs;  }

  bool vtable_at_begining()
  { return false; }

};
#endif

// *add 1.2.6 GCC 3
// The GCC 3.2 scheme is laid out like the MS scheme; hidden pointer at offset 0
// pointing to a simple table of methods.
// It does true pass by value, like MS, except only for objects with size 4 or 8 bytes.
// it also does true object return by value, with the same constraint.
// For larger objects, appears to behave like GCC 2.96

static bool is_plain_struct(Type t)
{
  PClass pc = t.as_class();
  return pc->simple_struct();
};

class GCC3ImportScheme: public ImportScheme {
public:
   static ImportScheme *New() { return new GCC3ImportScheme(); }

  bool ret_obj_popped()
   { return true; }

  bool true_pass_by_value(Type t)
  { 
      return is_plain_struct(t); 
  }

  bool true_return_by_value(Type t)
  {
      return is_plain_struct(t) ? t.size() < 3*sizeof(int) : false;
  }

};
//----------------------import namespace----------------------

typedef ImportScheme * (*SCHEME_FACTORY)();
typedef char * (*MANGLER)(Function *);
struct FACTORY { SCHEME_FACTORY scheme; MANGLER mangler; };
typedef void * (*ALLOCATOR)(int);
typedef void (*DEALLOCATOR)(char *, int);
typedef void *(*VECTOR_ALLOCATOR)(int,int);
typedef void (*ALLOCATOR_ENTRY)(ALLOCATOR,DEALLOCATOR,VECTOR_ALLOCATOR);

// add all the available scheme factories here
// *add 1.2.6 GCC 3 scheme added
const int MS=0,GCC=1, GCC3=2, UNDECIDED=-1;
FACTORY mFactory[] = {
     { &MSImportScheme::New,  &Mangle::microsoft },
	 { &GCCImportScheme::New, &Mangle::GCC2 },
     { &GCC3ImportScheme::New,&Mangle::GCC3 }        
};
const int NUM_SCHEME = sizeof(mFactory)/sizeof(FACTORY);

int mCurrent = UNDECIDED;
ImportScheme *mScheme = NULL;
bool mCppFriendly;

static int compat_scheme_index()
{
//*PASOP* This must reflect the _compiler_ compatible scheme
#ifndef __GNUC__
  return MS;
#else
  return GCC;
#endif
}

// *add 1.2.7 Import scheme now carries its scheme type, which is the index
// of its factory in the table.  create_scheme() now takes an optional
// scheme index to force this.

static ImportScheme * generate_scheme(int idx)
{
 ImportScheme* scheme;
 scheme = (*mFactory[idx].scheme)();
 scheme->scheme_type(idx);
 return scheme;
}

static void *load_dl_entry(const char *name)
{
  return get_proc_address(Builtin::get_dll_handle(),name);
}

ImportScheme * Import::create_scheme(int scm)
{
    return generate_scheme(scm == -1 ? mCurrent : scm);
}

ImportScheme * Import::create_compat_scheme()
{
  return generate_scheme(compat_scheme_index());
}

void Import::reset(bool new_dl)
{
  if (Builtin::get_dll_handle() != IMPLICIT_LINK ) mCurrent = UNDECIDED;
  else { // *fix 1.1.4L Don't try any dynamic loads if it's a self-link! 
     mCurrent = compat_scheme_index();
     return;
  }
  ALLOCATOR_ENTRY alloc_entry = (ALLOCATOR_ENTRY)load_dl_entry("_ucdl_set_allocator");
  if (new_dl) {
// *ch 1.2.9 patch
  #ifdef _WIN32  
    if (alloc_entry) {
      alloc_entry(&_new_ex,&_delete_ex,&_new_vect_ex);
      mCppFriendly = true;
    } else mCppFriendly = false;
  #else
    mCppFriendly = Parser::debug.do_overalloc;    
  #endif  
 // delete mScheme; *can't do this because standalone functions carry it...
  } else {
    if (alloc_entry) 
		alloc_entry(NULL,NULL,NULL);
  }
  mScheme = NULL;
}

ImportScheme *Import::scheme()
{ 
  return mScheme;
}

bool Import::set_scheme(string keyword)
{
  if (keyword == "GNU") mCurrent = GCC; else
  if (keyword == "MS")  mCurrent = MS;  else
  if (keyword == "GNU3") mCurrent = GCC3;
  else mCurrent = UNDECIDED;
  if (mCurrent == UNDECIDED) return false;
  mScheme = create_scheme();
  return true;
}

bool Import::cpp_friendly()
{ 
  return mCppFriendly;
}

static char *mName;

void *try_load_method(Function *pfn, int imethod)
{
   mName = (*mFactory[imethod].mangler)(pfn);
   return load_dl_entry(mName);
}

void *Import::load_method_entry(Function *pfn, string& mangled_name)
{
 void *proc;
 if (mCurrent == UNDECIDED) {
   int i = 0;
   do {
    proc = try_load_method(pfn,i++);
   } while (i < NUM_SCHEME && proc == NULL);
   if (proc==NULL) return NULL;  // no can do!

   // we've successfully deduced the scheme!!
   mCurrent = i-1; 
   mScheme = create_scheme();  
  }
  else proc = try_load_method(pfn,mCurrent);
  mangled_name = mName;
  return proc;
}

VMTArray ImportScheme::hunt_for_matching_VMT(VMTArray vmt, PClass pc)
{
  if (m_ghost_classes == NULL) m_ghost_classes = new VMTList();
  int vsize = vtable_size()+1;   // includes the class ptr!
  VMTList::iterator vli = m_ghost_classes->begin(), vli_end = m_ghost_classes->end();
  for(; vli != vli_end; ++vli)
	 if (std::equal(*vli+1, *vli + vsize, vmt+1)) return *vli;

 // otherwise, we have to create a new VMT and add it to our list of ghosts
  VMTArray v = new PFBlock[vsize];
  memcpy(v,vmt,sizeof(PFBlock)*vsize);
  v[0] = (PFBlock)pc;
  m_ghost_classes->push_back(v);
  return v;
}
 

