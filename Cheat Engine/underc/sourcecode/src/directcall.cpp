/* directcall.cpp
 * Direct access to native code, exported 'builtin' functions
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "common.h"
#include "opcodes.h"
#include "directcall.h"
#include "mangle.h"
#include "hard_except.h"
#include "os.h"
#include "ex_vfscanf.h"
#include "main.h"
#include <map>

#ifdef _WCON
#include "uc_graphics.h"
int exec(char *, int, bool);  // in twl.cpp
#define fprintf wcon_fprintf
#define fgets wcon_fgets
#else
#define fprintf con_fprintf
#define fgets con_fgets
#define wcon_fscanf con_fscanf
#endif

void* _copy_body(char* buff); // debug

// from lexer.cpp!
void uc_hash_cmd(char *s); 
void uc_macro_subst(const char *str, char *buff, int sz);

// *add 1.2.4 uc_exec(), uc_result() from main.cpp
int _uc_exec(char* s, void* cntxt, char* filename, int line);
void _uc_result(int ret, char *output, int sz, char* filename, int* line);

int _uc_exec_1(char* s) { return _uc_exec(s,0,0,0); }

void __mangle(); //*DEBUG*

// from tokens.cpp; exported as uc_include_path()
int _uc_include_path(const char *fname, char* buff, int sz);

extern void* gObjectReturnPtr;

typedef int (* CALL_FUN)(int, int);
#define STDCALL __stdcall
typedef int (*CALLFN) (void);

#ifndef __GNUC__

void callfn(CALLFN fn, int args[], int argc, void *optr, int flags, void *buff)
{
   int sz = sizeof(int)*argc;
  __asm {
    mov ecx, argc
    mov ebx, args
    // push the arguments onto the stack, backwards
a_loop:
    cmp ecx,0
    jz a_out
    mov eax, [ebx + 4*ecx]
    push eax
    dec ecx
    jmp a_loop

a_out:
    mov ecx,optr // thiscall calling convention (MS only)
    call fn
    // Cleanup stack ptr if this was a cdecl call
    mov ecx, flags
    test ecx,DC_CDECL
    jz  a_over
    add  esp,sz
a_over:
    test ecx,DC_RET_OBJ
    jz a_again
// these cases are peculiar to GCC
    cmp ecx,DC_RET_VAL
    jl  a_skip
    mov ebx, gObjectReturnPtr
    mov [ebx],eax
    mov [ebx+4],edx
    jmp a_finish
a_skip:
	sub  esp,4
a_again:
    mov ebx,buff
    test ecx,DC_QWORD
    jnz  a_dbl
    mov  dword ptr[ebx],eax
    jmp a_finish
a_dbl:
    fstp qword ptr[ebx]
a_finish:
  } 
}


// 'bare' inline functions w/ no usual prolog/epilog!
#define PROC(name) __declspec(naked) void name() { __asm {
#define ENDP }}

static ArgBlock *pArgs;  // only used for type info below!

// *fix 1.2.6 don't use edi! ecx is always safe.
PROC(copy_array)
// edi contains ptr to ArgBlock, eax contains no of args  
  pop [ecx]pArgs.ret_addr   // return addr!
  mov [ecx]pArgs.esi_ptr, esi
  mov [ecx]pArgs.no, eax
  lea esi,[ecx]pArgs.values
 l1:
  pop edx
  dec eax
  mov [esi + 4*eax],edx
  cmp eax,0
  jnz l1
  mov esi,[ecx]pArgs.esi_ptr
  push [ecx]pArgs.ret_addr
  ret
ENDP

#else 

//  *fix 0.9.9c  Two issues here: GCC does not emit locals,
// and 'const int' vars are optimized away.
// *fix 1.0.0L ELF does not use underscores in front of symbols 
// used in inline asm.
#ifndef _WIN32
#define DC_CDECL_ _DC_CDECL_
#define DC_QWORD_ _DC_QWORD_
#define DC_RET_OBJ_ _DC_RET_OBJ_
#define DC_RET_VAL_ _DC_RET_VAL_
#endif
static int DC_CDECL_ = DC_CDECL, DC_QWORD_ = DC_QWORD, DC_RET_OBJ_ = DC_RET_OBJ, DC_RET_VAL_ = DC_RET_VAL;

void callfn(CALLFN fn, int args[], int argc, void *optr, int flags, void *buff)
{
  // *fix 1.2.0  The RET_OBJ correction was wrong; it shd be subl $4,esp!
  // *fix 1.2.4  Using static variables leads to disasters. Here's the offsets
  // of the parameters:
  //  fn 8
  //  args 12
  //  argc 16
  //  optr 20
  // flags 24
  // buff 28
asm(
      "movl 12(%ebp),%ebx\t\n"
      "movl 16(%ebp),%eax\t\n"
      "imul $4,%eax\t\n"
      "movl %eax,16(%ebp)\t\n"
      "movl %eax,%ecx\t\n"
      "a_loop: cmpl $0,%ecx\t\n"
      "jz a_out\t\n"
      "movl (%ebx,%ecx),%eax\t\n" 
      "pushl %eax\t\n"
      "addl $-4,%ecx\t\n"
      "jmp a_loop\t\n"
      "a_out:  movl 20(%ebp),%ecx\t\n"
      "movl 8(%ebp),%eax\t\n"
      "call *%eax\t\n"
      "movl 24(%ebp),%ecx\t\n"
      "testl _DC_CDECL_,%ecx\t\n"
      "jz a_over\t\n"
      "addl 16(%ebp),%esp\t\n"
      "a_over:  testl _DC_RET_OBJ_,%ecx\t\n"
      "jz a_again\t\n"
      "cmpl _DC_RET_VAL_,%ecx\t\n"
      "jl a_skip\t\n"
      "movl _gObjectReturnPtr,%ebx\t\n"
      "movl (%ebx),%eax\t\n"
      "movl 4(%ebx),%edx\t\n"
      "jmp a_finish\t\n"
      "a_skip: subl $4,%esp\t\n"
      "a_again:   movl 28(%ebp),%ebx\t\n"
      "testl _DC_QWORD_,%ecx\t\n"
      "jnz a_dbl\t\n"
      "movl %eax,(%ebx)\t\n" 
      "jmp a_finish\t\n"
      "a_dbl:    fstpl (%ebx)\t\n"
      "a_finish:");
}
// *fix 1.2.9 sorted out a nasty in the above code! We were
// testing against EDX  and trying to move stuff from [ECX]

// I know this looks like portable code, but it isn't!
void copy_array(int sz, ArgBlock *xargs)
{
  int k,i;
  int *args = xargs->values;
  int *p = (int *)&xargs + 1;
  xargs->no = sz;
  // *fix 1.2.0L Copy these args backwards!
  for(k = sz-1,i=0; k >= 0; k--,i++)
    args[i] = p[k];
}

#endif

using namespace Parser;

//namespace { // private stuff
typedef std::list<Handle> HandleList;
HandleList lib_list;
Handle s_lib = NULL;

void *_get_std_stream(int i)
 {
  switch(i) {
   case 1: return stdin;
   case 2: return stdout;
   case 3: return stderr;
   case 4: return _str_in;   //See ex_vfscanf.cpp for this hack!
   case 5: return _str_out;
  }
  return stdout;
 }

void __break(int icode)
// use this builtin when debugging the system;
// *add 1.2.1 (Eric) Now takes an integer argument
{
 int i = 10; // set your breakpoint here!
}

// Sig is a hack that directly manipulates the Parser state so that when
// start_function() is called, it believes that a prototype has been found.

Sig& Sig::operator << (Type t)
{
 char *name;
 if(m_arg_name) { name = m_arg_name; m_arg_name = NULL; }
 else name = "*";
 Parser::state.add_to_arg_list(t,name,NULL);
 return *this;
}

Sig& Sig::operator << (char *arg_name)
{
 m_arg_name = arg_name;
 return *this;
}

void Sig::set_const(bool t)
{
  Parser::state.member_is_const = t;
}


void dissemble(PFBlock fb); // from dissem.cpp

int _dissem(FBlock *pfb) {
  try {
    dissemble(pfb);
   } catch(...) {
     return 0;
   }
   return 1;
}

typedef std::map<void *,int> PointerMap;
PointerMap mPtrMap;
bool gPtrCheckStart = false;

void *_new(int sz)
{
#ifdef _WIN32  
  void *ptr = (void *)new char[sz];
#else
  void *ptr = malloc(sz);
#endif  
  
if (Parser::debug.ptr_check) mPtrMap[ptr] = 1;
// cerr << "new " << sz << ' ' << ptr << endl;
return ptr;
}
// do note that "builtin" wrappers reverse their arg lists,
// so these guys are done backwards. (They will _never_ be called
// directly)
void *_new_vect(int n,int sz)
{
#ifdef _WIN32  
  void *p = (void *)new char[sz*n];
#else  
 void *p = malloc(sz*n);
#endif  
 if (Parser::debug.ptr_check) mPtrMap[p] = n;
 return p;
}

void _delete(char *ptr,int sz)
{  
  if (Parser::debug.ptr_check && Builtin::alloc_size(ptr) == 0) {
      if (! Parser::debug.suppress_link_errors && gPtrCheckStart)
	     cerr << (void *)ptr << " is not allocated by us!\n";
  } else
// *ch 1.2.9 patch
#ifdef _WIN32    
  delete ptr;
#else  
 free(ptr);
#endif 
}

// *change 1.1.0 Overallocation to make room for the VMT now done by builtins...
void* _new_ex(int sz)
{
 int *p = (int *)_new(sz+sizeof(int)); 
 *p = 0;   // to flag the VMT as NOT being created...
 return p+1;
}

void* _new_vect_ex(int n, int sz)
{
 int *p = (int *)_new_vect(n,sz+sizeof(int));
 *p = 0;
 return p+1;
}

void _delete_ex(char *ptr, int sz)
{
  if (ptr != NULL) { _delete(ptr-sizeof(int),sz); }
}

// *ch 1.2.9 patch
#ifndef _WIN32
void* operator new(size_t sz)
{
  return _new_ex(sz);
}

void operator delete(void *p)
{
 _delete_ex((char *)p,0);  
}

void* operator new[](size_t sz)
{
  return _new_ex(sz);
}

void operator delete[](void *p)
{
  _delete_ex((char *)p,0);
}

#endif

const int CDECL = Function::CDECL;

namespace Builtin {

void add(const Sig& sig,char *name, CALLFN fn, bool is_stdarg = false, int ftype=CDECL);

// this is used to find out how big allocated object arrays were....
int alloc_size(void *p)
{
 PointerMap::iterator pmi = mPtrMap.find(p);
 if (pmi != mPtrMap.end()) return pmi->second;
 // issue here is that when we're checking ALL alloc ptrs get an entry in this map!
 if (Parser::debug.ptr_check) return 0;
 else return 1;
}

// *add 0.9.4 Pointer sanity check if #opt p+
void bad_ptr_check(void *data)
{
  if (data == NULL)
      throw BadPointer("NULL pointer");
  // other checks (dangling, invalid, etc) go here....
}

typedef int (*TWOFN)(int,int);

int _wrap(char *name)
{
 Function *pf = Function::lookup(name);
 if (pf == NULL) return 0;
 TWOFN pfn = (TWOFN)generate_native_stub(pf);
 return (*pfn)(10,20);
}

// *add 1.0.0 These exported functions support the fast specialization map<string,int>
// You can of course use any other associative container for this implementation.

typedef std::map<string,int> MSI;
typedef MSI *PMSI;
typedef MSI::iterator IMSI;
typedef IMSI *PIMSI;

struct StrPair {
     string first;
     int second;
};
typedef StrPair* PSP;

void* _map_create()
{ return new MSI;    }

void  _map_destroy(void *pm)
{ delete PMSI(pm);}

int   _map_size(void *pm)
{ return PMSI(pm)->size(); }

int*  _map_find(const string& key,void *pm) {
   MSI::iterator msi = PMSI(pm)->find(key);
   return msi != PMSI(pm)->end() ? &msi->second : 0;
}

int*  _map_insert(const string& key,int val,void *pm) {  
  int& res = (*PMSI(pm))[key] = val;
  return &res;
}

// *ad 1.2.9 corresponding iterator stuff
void* _map_iter_ep(void* pm, int which)
{
    MSI::iterator it;
    if (which==0) it = PMSI(pm)->begin();
            else  it = PMSI(pm)->end();
    return new IMSI(it);
}

void* _map_iter_find(void* pm, const string& key)
{
    MSI::iterator it = PMSI(pm)->find(key);
    return new IMSI(it);
}

int  _map_iter_equal(void* i1, void* i2)
{
  return *PIMSI(i1) == *PIMSI(i2);
}

void _map_iter_fetch(void* pi, void* dat)
{
  PSP(dat)->first =  (*PIMSI(pi))->first;
  PSP(dat)->second = (*PIMSI(pi))->second;
}

void _map_iter_next(void* pi, int wdir)
{
  ++(*PIMSI(pi));
}

void* _native_stub(FBlock *pfb)
{
//*fix 1.2.3a The function may well be NULL...
   if (pfb == NULL) return NULL;
   if (pfb->function->builtin()) return pfb->native_addr();
   else return generate_native_stub(pfb->function);
}

// *add 1.2.5 range checking: _range_check
int _range_check(int sz, int i)
{
    if (i >= sz || i < 0) {
        char buff[80];
        if (i >= sz) sprintf(buff,"Range Error: %d >= %d",i,sz);
               else  sprintf(buff,"Range Error: %d < 0",i);
#ifdef WIN32	
        throw RangeError(buff);
#else   
        throw_range_error(buff);
#endif
    }
    return i;
}

static int mRangeCheck;

//by dark byte, I can't seem to solve this overload-function to pointer thing
double sin2(double x)
{
	return sin(x);
}

double cos2(double x)
{
	return cos(x);
}

double tan2(double x)
{
	return tan(x);
}

double atan22(double x, double y)
{
	return atan2(x,y);
}

double pow2(double x, double y)
{
	return pow(x,y);
}

double exp2(double x)
{
	return exp(x);
}

double log2(double x)
{
	return log(x);
}

double sqrt2(double x)
{
	return sqrt(x);
}

char* strstr2(char *x, const char *y)
{
	return strstr(x,y);
}

char* strchr2(char *x, int _ch)
{
	return strchr(x,_ch);
}

char* strrchr2(char *x, int _ch)
{
	return strrchr(x,_ch);
}





void init()
{
 Type t_ccp = t_char_ptr;
 Type t_int_ptr = t_int;
 t_ccp.make_const();
 t_int_ptr.incr_pointer();
 Signature *sig = new Signature(t_int);
 sig->push_back(t_int);
 sig->push_back(t_int);
 Type st(sig);
 st.incr_pointer();

 add(Sig(t_void_ptr) << t_void_ptr,"_native_stub",(CALLFN)_native_stub);
 add(Sig(t_void_ptr) << t_int,          "_new",(CALLFN)&_new);
 add(Sig(t_void_ptr) << t_int << t_int, "_new_vect",(CALLFN)&_new_vect);
 add(Sig(t_void) << t_void_ptr << t_int,"_delete",(CALLFN)&_delete);
 add(Sig(t_void_ptr) << t_int,          "_new_ex",(CALLFN)&_new_ex);
 add(Sig(t_void_ptr) << t_int << t_int, "_new_vect_ex",(CALLFN)&_new_vect_ex);
 add(Sig(t_void) << t_void_ptr << t_int,"_delete_ex",(CALLFN)&_delete_ex);




 add(Sig(t_double) << t_double,"sin",(CALLFN)&sin2);
 add(Sig(t_double) << t_double,"cos",(CALLFN)&cos2);
 add(Sig(t_double) << t_double,"tan",(CALLFN)&tan2);
 add(Sig(t_double) << t_double << t_double,"atan2",(CALLFN)&atan22);
 add(Sig(t_double) << t_double << t_double,"pow",(CALLFN)&pow2);
 add(Sig(t_double) << t_double,"exp",(CALLFN)&exp2);
 add(Sig(t_double) << t_double,"log",(CALLFN)&log2);
 add(Sig(t_double) << t_double,"sqrt",(CALLFN)&sqrt2);
 add(Sig(t_double) << t_ccp,"atof",(CALLFN)&atof);
 add(Sig(t_long) << t_ccp,"atoi",(CALLFN)&atoi);
 add(Sig(t_char_ptr) << t_int << t_char_ptr << t_int, "itoa", (CALLFN)&_itoa);
 add(Sig(t_long),"rand",(CALLFN)&rand);

 add(Sig(t_char_ptr) << t_char_ptr << t_ccp,"strcpy",(CALLFN)&strcpy);
 add(Sig(t_char_ptr) << t_char_ptr << t_ccp << t_int,"strncpy",(CALLFN)&strncpy);
 add(Sig(t_char_ptr) << t_char_ptr << t_ccp,"strcat",(CALLFN)&strcat);
 add(Sig(t_int) << t_ccp << t_ccp,"strcmp",(CALLFN)&strcmp);
 add(Sig(t_char_ptr) << t_ccp,"strdup",(CALLFN)&_strdup);
 add(Sig(t_char_ptr) << t_char_ptr << t_ccp,"strtok",(CALLFN)&strtok);
 add(Sig(t_char_ptr) << t_char_ptr << t_char_ptr,"strstr",(CALLFN)&strstr2);
 add(Sig(t_char_ptr) << t_char_ptr << t_int,"strchr",(CALLFN)&strchr2);
 add(Sig(t_char_ptr) << t_char_ptr << t_int,"strrchr",(CALLFN)&strrchr2);
 add(Sig(t_int) << t_ccp,"strlen",(CALLFN)&strlen);
 add(Sig(t_void_ptr) << t_void_ptr << t_void_ptr << t_int,
      "memmove",(CALLFN)&memmove);

 add(Sig(t_void_ptr) << t_void_ptr << t_int, "_str_cpy", (CALLFN)&str_cpy);
 add(Sig(t_int) << t_int, "_str_eof", (CALLFN)&str_eof);

 // stdio builtins (first entry used to get stdin, etc)
 add(Sig(t_void_ptr) << t_int,"_get_std_stream",(CALLFN)&_get_std_stream);
 add(Sig(t_int) << t_ccp,"puts",(CALLFN)&puts);
 add(Sig(t_int) << t_ccp,"printf",(CALLFN)&printf,true);
 add(Sig(t_int) << t_ccp << t_ccp,"sprintf",(CALLFN)&sprintf,true);
 // *change 1.2.3 GCC has finally persuaded me not to export puts().
 // add(Sig(t_char_ptr) << t_char_ptr,"gets",(CALLFN)&gets);
 add(Sig(t_char_ptr) << t_char_ptr << t_int << t_void_ptr,"fgets",(CALLFN)&fgets);
 add(Sig(t_int) << t_void_ptr << t_ccp,"fprintf",(CALLFN)&fprintf,true);
 add(Sig(t_int) << t_void_ptr << t_ccp,"fscanf",(CALLFN)&fscanf,true);
 add(Sig(t_int) << t_int << t_void_ptr, "ungetc",(CALLFN)&ungetc,true);

 add(Sig(t_int) << t_void_ptr << t_ccp,"wcon_fscanf",(CALLFN)&wcon_fscanf,true);

 add(Sig(t_int) << t_void_ptr << t_int << t_int << t_void_ptr,"fread",(CALLFN)&fread);
 add(Sig(t_int) << t_void_ptr << t_int << t_int << t_void_ptr,"fwrite",(CALLFN)&fwrite);
 add(Sig(t_int) << t_void_ptr,"feof",(CALLFN)&feof);
 add(Sig(t_void_ptr) << t_char_ptr << t_char_ptr,"fopen",(CALLFN)&fopen);
 add(Sig(t_int) << t_void_ptr, "fclose",(CALLFN)&fclose);
 add(Sig(t_int) << t_void_ptr, "fflush",(CALLFN)&fflush);
 add(Sig(t_int) << t_char_ptr << t_char_ptr, "rename", (CALLFN)&rename);
 add(Sig(t_int) << t_void_ptr, "fgetc", (CALLFN)&fgetc);
 add(Sig(t_int) << t_int << t_void_ptr, "fputc", (CALLFN)&fputc);
 add(Sig(t_int) << t_void_ptr << t_long << t_int, "fseek", (CALLFN)&fseek);
 add(Sig(t_long) << t_void_ptr, "ftell", (CALLFN)&ftell);

 add(Sig(t_void) << t_ccp,"uc_cmd",(CALLFN)&uc_hash_cmd);
 add(Sig(t_void) << t_ccp << t_char_ptr << t_int,"uc_macro_subst",(CALLFN)&uc_macro_subst);
 add(Sig(t_int) << t_void_ptr, "_dissem",(CALLFN)&_dissem);
 //add(Sig(t_void), "_init_lib",(CALLFN)&Parser::init_lib);
 // *add 1.2.5 range checking: _range_check
 add(Sig(t_int) << t_int << t_int,"_range_check",(CALLFN)&_range_check);

 // *add 1.2.4 uc_exec(), uc_result()
 add(Sig(t_int) << t_ccp,"uc_exec",(CALLFN)&_uc_exec_1);
 add(Sig(t_int) << t_ccp << t_void_ptr << t_ccp << t_int,"uc_exec",(CALLFN)&_uc_exec);
 add(Sig(t_void) << t_int << t_char_ptr << t_int << t_char_ptr << t_void_ptr,"uc_result_pos",(CALLFN)&_uc_result);
 add(Sig(t_void_ptr) << t_char_ptr,"copy_body",(CALLFN)&_copy_body);

 // *add 1.2.6 uc_include_path() will find a system file using the UC include paths.
 add(Sig(t_int) << t_ccp << t_char_ptr << t_int,"uc_include_path",(CALLFN)&_uc_include_path);

// These support the specialization map<string,int> (see <map> in the pocket library)
 add(Sig(t_void_ptr),                                     "_map_create",  (CALLFN)&_map_create);
 add(Sig(t_void)    << t_void_ptr,                        "_map_destroy", (CALLFN)&_map_destroy);
 add(Sig(t_int)     << t_void_ptr,                        "_map_size",    (CALLFN)&_map_size);
 add(Sig(t_int_ptr) << t_void_ptr << t_void_ptr,          "_map_find",    (CALLFN)&_map_find);
 add(Sig(t_int_ptr) << t_void_ptr << t_int << t_void_ptr, "_map_insert",  (CALLFN)&_map_insert);
 // *add 1.2.9 iterator support for the fast specialization
 add(Sig(t_void_ptr) << t_void_ptr << t_int,              "_map_iter_ep", (CALLFN)&_map_iter_ep);
 add(Sig(t_void_ptr) << t_void_ptr << t_void_ptr,         "_map_iter_find",(CALLFN)&_map_iter_find);
 add(Sig(t_int)      << t_void_ptr << t_void_ptr,         "_map_iter_equal",(CALLFN)&_map_iter_equal);
 add(Sig(t_void)     << t_void_ptr << t_void_ptr,         "_map_iter_fetch",(CALLFN)&_map_iter_fetch);
 add(Sig(t_void)     << t_void_ptr << t_int,              "_map_iter_next", (CALLFN)&_map_iter_next);
 
 add(Sig(t_void) << t_int,"__break",(CALLFN)__break);
 add(Sig(t_void),"__mangle",(CALLFN)__mangle);
 
 // UCW Graphics
 #ifdef _WCON
 //unsigned long ucw_rgb(int r, int g, int b);
 //void ucw_set_colour(UCWin win, unsigned long clr, bool fg);

 add(Sig(t_int) << t_char_ptr << t_int << t_int << t_int << t_int,
         "ucw_create_window",(CALLFN)&ucw_create_window);
 add(Sig(t_int) << t_int << t_char_ptr,"ucw_title",(CALLFN)&ucw_title);
 add(Sig(t_int) << t_int << t_char_ptr,"ucw_cmd",(CALLFN)&ucw_cmd);
 add(Sig(t_int) << t_int << t_int << t_int << t_int << t_int,"ucw_size_window",(CALLFN)&ucw_size_window);         
 add(Sig(t_int) << t_int << t_int << t_int,"ucw_move_to",(CALLFN)&ucw_move_to);
 add(Sig(t_int) << t_int << t_int << t_int,"ucw_line_to",(CALLFN)&ucw_line_to);
 add(Sig(t_int) << t_int << t_char_ptr,"ucw_text_out",(CALLFN)&ucw_text_out);
 add(Sig(t_int) << t_int << t_char_ptr << t_int,"ucw_font",(CALLFN)&ucw_font);
 add(Sig(t_int) << t_int << t_float << t_float << t_float,"ucw_fcolour",(CALLFN)&ucw_fcolour);
 add(Sig(t_int) << t_int << t_float << t_float << t_float,"ucw_bcolour",(CALLFN)&ucw_bcolour);
 add(Sig(t_int) << t_int << t_int << t_int << t_int << t_int,"ucw_rectangle",(CALLFN)&ucw_rectangle);         
 add(Sig(t_int) << t_int << t_int << t_int << t_int << t_int,"ucw_ellipse",(CALLFN)&ucw_ellipse);         
 add(Sig(t_int) << t_char_ptr << t_int << t_bool,"exec",(CALLFN)&exec);
 add(Sig(t_long) << t_int << t_int << t_int,"ucw_rgb",(CALLFN)&ucw_rgb);
 add(Sig(t_void) << t_int << t_long << t_int,"ucw_set_colour",(CALLFN)&ucw_set_colour);
#endif

 Function* pf = Function::lookup("_range_check");
 mRangeCheck = NFBlock::create(pf,(CALLFN)pf->fun_block()->native_addr());
}

//---------------------- adding builtin functions ---------------------------
// We keep a map of imported functions to quickly tell what they are imported as!
typedef std::map<CALLFN,FBlock *> ClassImportMap;
ClassImportMap mImportMap;

// *hack 1.2.0 The optimized Win32 MS build blows up here occaisionally - no idea why!
#pragma optimize( "", off )
static FBlock *_pfb;
static Function *_pfn;
static int _kount = 0;
void insert_ptr_map(CALLFN fn, Function *pfn)
{
  try {
  _pfn = pfn;
  _pfb = pfn->fun_block();
  mImportMap[fn] = _pfb; 
  } catch(...) {
     ++_kount;
  }
}
#pragma optimize( "", on ) 

void add(const Sig& sig,char *name, CALLFN fn, bool is_stdarg, int ftype)
// Builtin::add() assumes that the Signature has been collected in the proper way.
// It's usually called from state.declare_function(), but also from add_dll_function().
// In both cases the class Sig acts as a interface to the common signature stuff.
{
 Function *pfn = state.start_function(sig.m_type,name,true,ftype);
 FunctionContext *fe = pfn->context();
 int nargs = pfn->fun_block()->nargs;
 if (is_stdarg) pfn->stdarg(true);
 
 Parser::code().emit_native_function_call(pfn,fn);
 

#ifdef __GNUC__ 
 mImportMap[fn] = pfn->fun_block();
#else
 insert_ptr_map(fn,pfn);
#endif

 fe->finalize();
 state.pop_context(); // usually done by block-end!
}

// *add 1.2.5 Offset of range checking function
int range_check_function()
{
  return mRangeCheck;
}

FBlock *import_vmethod(CALLFN fn, PFBlock pfb)
// this is passed a native routine, and a UC function block which acts as a prototype
// for the _anonymous method import_
{
  Parser::set_function_code(true);  // enter function code context
  UCContext& code = Parser::code();
  Function *pf = pfb->function;
  FBlock *fb;
  try {
	fb = FBlock::create(pfb->entry,NULL);
	mImportMap[fn] = fb;
    code.emit_native_function_call(pf,fn);
    code.emit_return(pf->return_type());
    
	fb->finalize(0);
  } catch(...) { error("error when importing function"); }  
  Parser::set_function_code(false); // go back to static code context 
  return fb;
}

//----------------------- importing functions and methods from DLLs -------------


FBlock *imported_fblock_from_function(void *pfn)
{
  return mImportMap[(CALLFN)pfn];
}


CALLFN lookup_self_link(PClass pc, const string& name);  // later in this module...


// *add    1.2.3  Support for uc_import() - the UseAddr modifier
static void* mDirectAddr = NULL;

void set_direct_import(void* p)
{ 
// A Two-Part Hack.
// It's NB to call this function with a NULL argument afterwards to reset the state.
  if (p) {
      // (1)the current DLL handle needs to be non-NULL, but doesn't have to be sensible
      // This is just to force Parser::declare_function() to call add_dll_function()
      // below
      set_dll_handle((void*)-1);
      // (2)The modifier flag will be passed to add_dll_function(), so we give it
      // a special value so it will pick up the pointer directly....
      Parser::state.modifier = UseAddr;
      mDirectAddr = p;
  } else {
      set_current_lib_file(0);
      mDirectAddr = NULL;
  }
}

bool add_dll_function(Function *pfn, int modifier, string& name)
{
  PClass pc = pfn->class_context();
  CALLFN proc;

  // *change 0.9.4  Need explicit extern "C" now - (used to simply be class context)
  // *add    1.1.4  Implicit self-link for those systems which need it...
  if (modifier == UseAddr) proc = (CALLFN) mDirectAddr;
#ifdef CANNOT_SELF_LINK
  else if (get_dll_handle()==IMPLICIT_LINK) {
    name = pfn->name(); // *fix 1.2.4 pass back name in case we can't find     
    proc = lookup_self_link(pc,name);
  }
#endif  
  else if(! Parser::in_extern_C()) { // mangled C++ name 
    proc = (CALLFN)Import::load_method_entry(pfn,name);      
	if (name=="") name = "?" + pfn->name();
  } else { // extern "C"
    name = pfn->name();
    if (modifier == Stdcall) name = "_" + name + "@" + itos(pfn->signature()->byte_size());     
    proc = (CALLFN)get_proc_address(s_lib,name.c_str());
  }
  if (!proc) return false; 

  if (pc != NULL) pc->set_imported();

  Sig ssig(pfn->return_type());
  Signature *sig = pfn->signature();
  Signature::iterator tli;
  for(tli = sig->begin(); tli != sig->end(); ++tli) ssig << (Type)*tli;
  ssig.set_const(sig->is_const());  // *fix 1.1.0  Must respect constness of signature!

  // *fix 1.2.3 Important also to mke the signature reflect any extra unnamed arguments....
 if (sig->stdarg()) ssig << t_void;  // the convention used by the parser...


// *fix 1.1.0 We can now distinguish between ordinary cdecl method calls (GCC)
// and so-called '__thiscall' calls (MS)
// *add 1.2.3 Note that direct imports into the DLL are NOT stdcall under Linux!
  int calling_convention = Function::CDECL;
  if (modifier == Stdcall || modifier == Api 
#ifdef _WIN32
      || modifier == UseAddr
#endif
   ) 
      calling_convention = Function::_STDCALL;

  if(pfn->is_method()) {
    if (pc->import_scheme()->uses_stdmethod()) calling_convention = Function::STDMETHOD;
  }

  add(ssig,pfn->name().c_str(),proc,false,calling_convention);

  return true;
}



//----------------------- generating native stubs -------------------------
// Basically this is a mad (and v. limited) x86 macro assembler.

typedef unsigned long ulong;
typedef char *pchar;
typedef short *& pshort;
typedef ulong *& pulong;

// Disable some silly warnings:
// truncation of const value and 'int' to 'short'.
#pragma warning(disable:4309) 
#pragma warning(disable:4305)

void emit1(pchar& pc, char ch)      {  *pc++ = ch; }
void emit2(pchar& pc, short s)      {  *pshort(pc)++ = s; }
void emit4(pchar& pc, ulong l)      {  *pulong(pc)++ = l;}
void pushc(pchar& pc, ulong val)    { emit1(pc,0x68); emit4(pc,val); }
void pushv(pchar& pc, void *ptr)    { emit2(pc,0x35FF); emit4(pc,(ulong)ptr); }     
void popv(pchar& pc, void *ptr)     { emit2(pc,0x058F); emit4(pc,(ulong)ptr); }     
void callf(pchar& pc, void *pfn) {
  emit1(pc,0xBA);   emit4(pc,(ulong)pfn);     // mov edx, offset copy array
  emit2(pc,0xD2FF);                                   // call edx
}
void mov_acc(pchar& pc, ulong val)  { emit1(pc,0xB8);   emit4(pc,val);  }    // mov eax,...
void mov_ptr(pchar& pc, void *val)  { emit1(pc,0xB9);   emit4(pc,(ulong)val);  } // mov ecx..
void copy_acc(pchar& pc, void *ptr) { emit1(pc,0xA1);   emit4(pc,(ulong)ptr);  }
void copy_cx(pchar& pc, void *ptr)  { emit2(pc,0x0D89); emit4(pc,(ulong)ptr); }               
void sub_esp(pchar& pc, int val)    { emit2(pc,0xEC81);   emit4(pc,(ulong)val); }
void push_qword(pchar& pc, void *ptr) { emit2(pc,0x05DD); emit4(pc,(ulong)ptr); }
void ret(pchar& pc)                 { emit1(pc,(char)0xC3);   }

void *generate_native_stub(Function *pfn)
{
// The stub must copy the arguments by calling copy_array(), and then calls Engine::execute().
  char cde_buff[200];
  Signature *sig = pfn->signature();
  
  int no_args = sig->byte_size()/sizeof(int);
  void *rra = new ulong;
  ArgBlock *xargs = new ArgBlock;
  int flags = Engine::ARGS_PASSED;
  char *pc = cde_buff;
  // *fix 1.1.4     Plain function callbacks are often cdecl...but think about this!
  // *change 1.2.3  Functions export as cdecl, unless __stdcall is used;
  //                MS methods don't and GCC methods do; determined by is_cdecl().
  bool fun_is_cdecl;
  if (pfn->is_method()) {
      fun_is_cdecl = pfn->is_cdecl();
  } else 
      fun_is_cdecl = pfn->export_as_cdecl();
  
  popv(pc,rra);                                                    // save return addr
  if (pfn->is_method())  {
      if (pfn->class_context()->import_scheme()->uses_stdmethod()) // is this a stdmethod call?
           copy_cx(pc,&xargs->OPtr);                                // obj ptr was in ecx
      else {
         popv(pc,&xargs->OPtr);                                     // obj ptr was on the stack
         no_args++;                                                // as an _extra_ arg
	  }
      flags = flags | Engine::METHOD_CALL;
  }
  Type rt = pfn->return_type();
  if (rt.is_double()) flags = flags | Engine::RETURN_64; else
  if (!rt.is_void()) flags = flags | Engine::RETURN_32;
  if (no_args > 0) {
  // two different strategies here, depending on the compiler
#ifndef __GNUC__
    mov_ptr(pc,xargs);
    mov_acc(pc,no_args);                                          // call copy_array
    callf(pc,(void *)&copy_array);                                
  // copy_array mucks w/ ESP by explicitly popping the arguments;
  // cdecl calls assume that caller will sort out ESP.
 
   if (fun_is_cdecl) 
     sub_esp(pc, sizeof(int)*no_args);                            // restore esp if cdecl
  #else
    pushc(pc,(unsigned long)xargs);
    pushc(pc,no_args);
    callf(pc,(void *)&copy_array);
    sub_esp(pc, -8);                  // because copy_array is cdecl...
    if (! fun_is_cdecl)              // if we're NOT cdecl then clean up like a good boy!
      sub_esp(pc, -sizeof(int)*no_args);
    
  #endif				 
  }
  pushc(pc,(ulong)xargs);
  pushc(pc,flags);
  pushc(pc,(ulong)pfn->fun_block());
  callf(pc,(void *)&Engine::stub_execute);                        // and call the stack engine!
  if (flags & Engine::RETURN_32) copy_acc(pc,&xargs->ret1);       // put result in eax, if needed
  else
  if (flags & Engine::RETURN_64)  push_qword(pc,&xargs->ret2);    // or onto fp stack *add 1.1.1
  pushv(pc,rra);                                                  // restore return addr & return
  ret(pc);

  // and copy the code block
  int sz = (ulong)pc - (ulong)cde_buff;
  char *cp = new char[sz+1];
  memcpy(cp,cde_buff,sz+1);
  return cp;
}


// *add 1.1.4 The Linux tool chain currently doesn't allow you to link to self,
//            so we have to explicitly specify self-exports. 
#ifndef CANNOT_SELF_LINK
 static const int sNoSelfLink = false;
#else
 static const int sNoSelfLink = true;
#endif

//-------------------- shared library management ----------------
static string s_file;

bool set_current_lib_file(char *file)
{
 if (!file) {
   s_lib = NULL;
   cleanup_ordinal_lookup();
 } else {
// *add 1.1.2 Linking to #self finds the actual full path of UC pgm
  // *fix 1.1.4 It's now #pragma dlink $self,etc! (otherwise argues with prepro)
  if (file[0]=='#') file[0] = '$';
  bool explicit_link = true;
  if (strcmp(file,"$self")==0) {// self-linking case!
// *ch 1.2.9 patch
#ifdef _WIN32    
    file = Main::uc_exec_name();
#else
    file = "";
#endif    
	explicit_link = ! sNoSelfLink;
  } else
    if (strcmp(file,"$caller")==0) {
      file = NULL;
    }
  // *add 1.1.2 Try looking in the UC_LIB directory!
   string sfile = file ? file : "$caller";
   if (explicit_link) {
     s_lib = load_library(file);
     if (!s_lib) {
	   sfile = Main::uc_lib_dir() + sfile;
	   s_lib = load_library(sfile.c_str());
	 }
   } else s_lib = IMPLICIT_LINK;  
   lib_list.push_back(s_lib);
   if (!s_lib) return false;
// *fix 1.1.4 Subsequent #lib where the loaded DLL is the same - don't reset!
   if (sfile != s_file) { 
	   Import::reset(true);
	   s_file = sfile;
   }
 }
 return true;
}

string get_current_lib_file()
{ return s_file; }

void unload_library(Handle hlib)
{
    set_dll_handle(hlib);
	Import::reset(false);
    if (hlib != IMPLICIT_LINK && hlib != NULL) {
        free_library(hlib);
        cleanup_ordinal_lookup();
    } 
}

// *fix 1.2.4 if passed NULL (i.e. no current DLL _importing_ taking place)
// this function will try to unload the last handle loaded.
void unload_lib(Handle hlib)
{ 
  if (hlib == NULL) {
    hlib = lib_list.back();
  }
  unload_library(hlib);
  lib_list.remove(hlib);
  s_lib = NULL;
} 

void *get_dll_handle() { return (void *)s_lib; }

void set_dll_handle(void *dl)
{ 
	s_lib = (Handle)dl;
}

// Looking up DLL entries using ordinal lookup
typedef std::map<string,int> SIMap;
static SIMap *s_ord_lookup;
static bool s_lookup_is_ordinal;

bool using_ordinal_lookup()
{ return s_ord_lookup != NULL; }

int lookup_ordinal(const char *name)
{
    SIMap::iterator simi = s_ord_lookup->find(name);
    if (simi != s_ord_lookup->end()) return simi->second;
    else return 0;
}

// *change 1.2.2 (Eric) imp file format is now more relaxed;
// (a) ignore any line that begins with a "# ", and 
// (b) everything after the mangled name
// *add 1.2.4 Will look in UC LIB directory a la .DLLs
// *add 1.2.4 UC2 type means that value is not ordinal but address
int convert_ordinal(char *buf)
{
  if (s_lookup_is_ordinal)
    return atoi(buf);
  else {
    unsigned long l;
    sscanf(buf,"%x",&l);
    return (int)l;  
  }
}

bool lookup_is_ordinal()
{ return s_lookup_is_ordinal; }

bool generate_ordinal_lookup(const char *index_file)
{
  string name,magic,compiler;
  char buf[1024];
  ifstream in;
  in.open(index_file);
  if (! in || in.eof()) {
    string  sfile = Main::uc_lib_dir() + index_file;
    in.open(sfile.c_str());
    if (! in || in.eof()) {
      cerr << "cannot find '" << sfile << "'\n";
      return false;
    }  
  }  
  in >> magic >> compiler;
  if (magic != "UC1" && magic != "UC2") return false;
  s_lookup_is_ordinal = magic == "UC1";
  if (! Import::set_scheme(compiler)) return false;
  s_ord_lookup = new SIMap;
  // *fix 1.2.3a (Eric) Attempted to read ordinal twice
  while (! in.eof()) {
    in >> buf;
    if (*buf && *buf != '#') {
      in >> name;
      (*s_ord_lookup)[name] = convert_ordinal(buf);
    }
    in.getline(buf,sizeof(buf));
  }
  return true;
}

void cleanup_ordinal_lookup()
{
    delete s_ord_lookup;
    s_ord_lookup = NULL;
}

// *fix 1.2.3 Under Win32, it is a Bad Idea to try free the process handle.
void finis()
{
 Handle process_handle = get_process_handle();
 HandleList::iterator ili;
 for(ili = lib_list.begin(); ili != lib_list.end(); ++ili) {
      Handle lib = *ili;
      if (lib != process_handle) 
         unload_library(lib);
 }
}

CALLFN lookup_self_link(PClass pc,const string& name) 
{
	return NULL; 
}



}  // namespace Builtin

