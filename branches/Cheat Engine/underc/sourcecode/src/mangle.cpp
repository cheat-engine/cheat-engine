/* mangle.cpp
 * Mangling C++ external symbols
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 *
 * 2nd version, January 2002
 * Exports the Mangle namespace
 * The symbol manglers are all classes derived from Mangler, which
 * supplies basic common policy, and scheme-specific methods which are
 * overriden by MSMangler, GCC2Mangler, etc. (The 'template pattern')
 **/
 
#include "common.h"
#include "std_utils.h"
#include "mangle.h"
#include <stdio.h>

template <class T>
int find_index(const std::list<T>& ls, T t)
{
  int i;
  typename std::list<T>::const_iterator li = ls.begin();
  for(i = 0; li != ls.end(); li++) {
     if (t == *li) return i;
     i++;
  }
  return -1;
}

// *fix 1.2.0L Change in the encoding of GCC dtors from 2.95 to 2.96
// (this assumes that the GCC 2 compiler under Win32 is MinGW 1)
#ifndef _WIN32
#if __GNUC__ == 2 && _GNUC_MINOR__ == 95
#define GCC_DTOR "_$_"
#else
#define GCC_DTOR "_._"
#endif
#else
#define GCC_DTOR "_$_"
#endif

const int MAX_SIG_LEN = 25, BUFF_SIZE = 256;

//namespace {
 struct OpNames { char* cname; char* mname; }; 
 char *i2a(int i);  // at the end of this file...
 char *i2ax(int i);
 char *lookup_opname(OpNames *popn, const char *n);

 static char buff[BUFF_SIZE];
 std::list<Class *> mClassList;
 std::list<Type> mArgTypes;
//}

class OutputBuffer {
protected:
private:
  char *m_p;
  char *m_buff;
  char *m_mark_p;
public:
  OutputBuffer()
  {
   m_buff = buff;
   reset();
  }
 
  void reset()
  {
    m_p = m_buff;
    mClassList.clear();
    mArgTypes.clear();
  }

  void end()
  { 
    *m_p = 0;
  }

  void set_mark()
  {
      m_mark_p = m_p;
  }

  void restore_mark()
  {
      m_p = m_mark_p;
  }

  char read_mark(int i)
  {
      return m_mark_p[i];
  }

  void outs(const char *p)
  {
      strcpy(m_p,p);
      m_p += strlen(p);
  }

  void out(char ch)
  {
      *m_p++ = ch;
  }

  void outi(int i)
  {
    outs(i2a(i));
  }

  void outx(int i)
  { 
    outs(i2ax(i));
  }

  char *ptr()
  {
    return m_buff;
  }

};
	
class Mangler: public OutputBuffer {
protected:
  char *m_begin_name;
  char *m_rpt_type;
  char *m_void_args;  
  char *m_end_stdarg; 
  char *m_end_args;
  char *m_plain_function;
  char *m_function_ptr;
  char *m_method_ptr;
  char *m_enum;
  char *m_end_function_ptr;
  bool m_use_return_type;
  char *m_plain_obj;
  char *m_const_attrib;
  char *m_plain_attrib;
  char  m_ref_chr;
  char  m_array_char;
  char  m_ptr_char;
  bool m_method_type_first;
  bool m_convention_first;
  char *m_namespace;
  char *m_collect_repeats;
  char *m_template_name;
  char *m_template_args;
  char *m_end_class;
  char *m_method_ptr_type;
  Function *m_fn;
  bool  m_dont_lookup_class;
  bool m_within_signature;
  bool m_discard_sig_types;
public:

  virtual void out_method_type(int) {}
  virtual char type_encode(Type t) { return 0; }
  virtual bool simple_type(Type t) { return false; }
  virtual void do_calling_convention() {}
  virtual void out_simple_classname(const string& name) {}
  virtual void out_fun_name(string fun_name) {};

  Mangler()
  :  m_dont_lookup_class(false) { }
  
  virtual void out_class_ref(Type t)
  {
   out_ref_ptr(t);
   out_classname(t.as_class());
  }

  void out_classname(Class *pc) 
  {
    string name=pc->name();
    if (pc->get_template()) {
	   outs(m_template_name);
	   name = pc->get_template()->name();
    }
    out_simple_classname(name);
    if (pc->get_template()) {
          TypeList& tl = pc->get_template()->type_parms();
	  Type t = tl.front();
	  outs(m_template_args);
	  // *fix 1.1.2 Always put out template type parms in full....
	  m_dont_lookup_class = true;
          out_type(t);
	  m_dont_lookup_class = false;
     }
     if (m_namespace && pc->inside_namespace()) {
          outs(m_namespace);
          outs(pc->inside_namespace()->name().c_str());
     }
     outs(m_end_class);
 }


  void out_type(Type t) 
  { 
      if (t.is_signature()) {
	   if (t.as_signature()->class_ptr() == NULL) { // NOT a method
          outs(m_function_ptr);
       } else {
          outs(m_method_ptr);
          out_classname(t.as_signature()->class_ptr());
          outs(m_method_ptr_type);
       }  
       m_within_signature = true;
	   do_signature(t.as_signature());
       m_within_signature = false;
       outs(m_end_function_ptr);
      } 
      else if (t.is_class()) out_class_ref(t);
      else if (t.is_enum()) {  // *add 1.2.1 Mangling enum arguments
         out_ref_ptr(t);       // *fix 1.2.3 (Eric) handle pointers to enums, etc 
         outs(m_enum);
         out_simple_classname(t.as_enum()->name());
         outs(m_end_class);
      }
	  else {
	    out_ref_ptr(t);
		out(type_encode(t));
      }
   }

 void out_attrib(Type t) 
 {
    outs(t.is_const()  ? m_const_attrib : m_plain_attrib);
 }


  void out_ref_ptr(Type t)
  {
  // put out ref/ptr attributes 
      if (t.is_ref_or_ptr()) {
         if(t.is_reference())  {
           out(m_ref_chr);  out_attrib(t);
         } 
         if(t.is_pointer()) {
		   char pc = m_array_char;
		   if (!t.is_array()) pc = m_ptr_char;
           for(int i = 0; i < t.pointer_depth(); i++) {
                 out(pc);
      //..the type system can't currently represent _multiple const attributes_
                 if (i==0) out_attrib(t);   else outs(m_plain_attrib);
         }
       }
      } else // GCC requires a class to have at least one attrib.
      if (t.is_class() && ! m_dont_lookup_class) outs(m_plain_obj);     
  }

  virtual void do_void_args()
  {
    outs(m_void_args);   // (void)
  }

  // *fix 1.2.1 The two passes (collection and output) have been made into one. This simplifies
  // matters and ensures that types inside function pointer signatures are found in the correct
  // order. The special case of repeat codes is handled by setting a marker in the output.
  // *fix 1.2.1 GCC mangling doesn't do this compression trick on any embedded function pointer signatures.
  void do_signature(Signature *sig)
  {
      bool stdarg = sig->stdarg();
      if (m_use_return_type) { 
	  // bit of a hack - this applies to MS only...
      //*fix 1.2.3a Ctors taking method ptr arguments were miscoded.....   
        if ((m_fn->is_constructor() || m_fn->is_destructor()) && !m_within_signature) out('@');
        else {
            // *fix 1.2.3 (Eric) Functions returning enums are encoded like those returning objects
           Type rtype = sig->return_type();
	   if ((rtype.is_class() || rtype.is_enum()) && !rtype.is_ref_or_ptr()) outs("?A");
	   out_type(rtype);
        }
      } 
      if (sig->begin() == sig->end()) do_void_args();
      else {
        // encode each type in turn;  any non-simple repeated types are kept in a list and
        // refered to by index.  Some schemes (e.g. GCC) will collect repeated types together
        int last_idx=-1, idx, n_repeats;
        bool add_to_list;
        Signature::iterator sigi;
	// *fix 1.2.4 GCC regards the class of a member function as
	// a type to be considered in the type list...
	if (m_collect_repeats && m_fn->class_context()) {
	  Type ct(m_fn->class_context());	  
	  mArgTypes.push_back(ct);
	}
	for(sigi = sig->begin(); sigi != sig->end(); sigi++) {
           Type t = *sigi;
           idx = -1;
           add_to_list = false;
           if (! simple_type(t) && ! (m_within_signature && m_discard_sig_types)) { 
               // does this arglist already have this type?
               idx = find_index(mArgTypes,t);
               // if not, put it in the list of argument types
               add_to_list = (idx == -1);
            } 
            if (idx == -1) {
                out_type(t); 
                if (add_to_list) mArgTypes.push_back(t);
                last_idx = -1;
            } else { // a repeated type is represented by its index
                if (idx == last_idx && m_collect_repeats) { // matches last repeated type!
                  if (read_mark(0)==*m_rpt_type) n_repeats = 1;
                  else n_repeats = int(read_mark(1)-'0');
                  ++n_repeats;
                  restore_mark();
                  outs(m_collect_repeats);
                  outi(n_repeats);
                  outi(idx);
                } else { // write out a type reference, setting the mark for later!
                  set_mark();
                  outs(m_rpt_type);
                  outi(idx);
                }
                last_idx = idx;
            }
       }
       // cdecl-style arbitrary arguments
      if(stdarg) outs(m_end_stdarg); else outs(m_end_args); 
      }
   }


 char *mangle(Function *pfn)
 {
   reset();
   m_fn = pfn;
   outs(m_begin_name);
   out_fun_name(pfn->name());
   PClass pc = pfn->class_context();
   if (pc != NULL) { // is a method
     int access = m_fn->fun_entry()->reference()->access();
	 if (m_method_type_first)   out_method_type(access);
     out_classname(pc);
     if (! m_method_type_first) out_method_type(access);
	 mClassList.push_back(pc);
   } else
   outs(m_plain_function); 

 // any arguments
   if (m_convention_first)   do_calling_convention();
   do_signature(m_fn->signature());
   if (! m_convention_first) do_calling_convention();

 // end output string
   end();
   return ptr();
 }

};

OpNames ms_funs[] = {
     {"=","4"},
     {"+","H"},
     {"-","G"},
	 {"==","8"},
	 {"!=","9"},
     {"++","E"},
     {"--","F"},
     {"<<","6"},
     {">>","5"},
     {"[]","A"},
     {"<","M"},
     {">","O"},
     {"+=","Y"},
     // *add 1.2.1 Peter H's extra operators...
     {"-=","Z"},
     {"*=","X"},
     {"/=","_0"},
     // *add 1.2.3a Conversion operators
     {"__T__","B"},
     {NULL,NULL}
};

class MSMangler: public Mangler {
public:
 MSMangler()
 {
  m_begin_name = "?";
  m_rpt_type = "";
  m_void_args = "XZ";  
  m_end_stdarg = "ZZ"; 
  m_end_args = "@Z";
  m_plain_function = "@Y";
  m_function_ptr = "P6A"; // cdecl! Our problem is that it isn't
  	                      // part of the signature!!
  // *fix 1.2.3a 
  m_method_ptr = "P8";    
  m_method_ptr_type = "AE";  // a hack - wd be "BE" for const signatures...

  m_end_function_ptr = "";
  m_use_return_type = true;
  m_plain_obj = "";
  m_const_attrib = "B";
  m_plain_attrib = "A";
  m_ref_chr = 'A';
  m_array_char = 'Q';
  m_ptr_char = 'P';
  m_method_type_first = false;
  m_convention_first = true;
  m_collect_repeats = NULL;

  m_end_class = "@@";
  m_template_name = "?$";
  m_template_args = "@";
  m_namespace = NULL; // "@"; for now!

  m_discard_sig_types = false;
  m_enum = "W4";

}

 bool simple_type(Type t) // MSMangler
 {
     return t.is_number() && !t.is_reference();
 }

 void out_simple_classname(const string& name) // MSMangler
 {
    outs(name.c_str());
 }

 void do_calling_convention() // MSMangler
 {
    // if (m_fn->is_cdecl()) out('A');  else 
     if (m_fn->is_method()) out('E');  else
	 out('A');    
     //if (m_fn->is_stdcall()) out('G'); 
 }

 char type_encode(Type t) // MSMangler
 {
   	char ch;
	 if (t.is_void()) ch = 'X'; else
	 if (t.is_bool()) { out('_'); ch = 'N'; } // weird encoding!
	 else
     if (t.is_int()) {
         if(t.is_unsigned()) {
            if (t.is_char()) ch = 'E'; else
            if (t.is_short()) ch = 'G'; else
            if (t.is_long()) ch = 'K';
            else ch = 'I'; // plain 'int'
         } else {
            if (t.is_char()) ch = 'D'; else
            if (t.is_short()) ch = 'F'; else
            if (t.is_long()) ch = 'J';
            else ch = 'H';
         }
      } else
      if (t.is_float()) {
         if (t.is_double()) ch = 'N';
         else ch = 'M';
      } else //if(t.is_class()) ch = 'V';
         return 0;
      return ch;      
    }

  void out_fun_name(string fun_name) // MSMangler
  {
     const char *name = fun_name.c_str();
     if (m_fn->is_constructor()) outs("?0");  else
     if (m_fn->is_destructor()) outs("?1"); else 
     {
      char *mn = lookup_opname(ms_funs,name);
	  if (mn != NULL) { out('?'); outs(mn); }
	  else { 
	  // methods (as op. to operators & constructors) refer to the class as #1, not #0.
	  // presumably there's something already in the list!
	    mClassList.push_back(NULL);
        outs(name);
        out('@');
      }
     }
   }

   void out_method_type(int access) // MSMangler
   {
	char ch;
	char *pch;
	switch(access) {
	case Public:    pch = "SUQ"; break;
	case Protected: pch = "KMI"; break;
	case Private:   pch = "CEA"; break;
	default:        pch = "SUQ"; break;
	}
	if (m_fn->is_static()) ch = pch[0]; else
	if (m_fn->is_virtual()) ch = pch[1]; 
    else ch = pch[2];
    out(ch);
    if (!m_fn->is_static()) out(m_fn->is_const()  ? 'B' : 'A');
   }

   void out_class_ref(Type t) // MSMangler
   {
   // MS optimizes the usual pattern for repeated class refs.
       PClass pc = t.as_class(); 
       int idx = find_index(mClassList,pc);
	   if (m_dont_lookup_class) idx = -1;
       // *fix 1.2.3 Structs are now mangled correctly, since we keep a struct flag
       char cc = pc->is_struct() ? 'U' : 'V';
       if (idx != -1) {    // we matched a _previously emitted_ class...
			out_ref_ptr(t);
			out(cc);  
			outi(idx);
			out('@');
       } else {  
			out_ref_ptr(t);
			out(cc);  
			out_classname(pc);
  		    if (! m_dont_lookup_class) mClassList.push_back(pc);
       } 
   }

};

OpNames gcc2_fns[] = {
    {"=", "as"},
    {"==","eq"},
    {"!=","ne"},  // *fix 1.2.4 added
    {"[]","vc"},
    {"<<","ls"},
    {">>","rs"},
    {"+","pl"},
    {"-","mi"},
    {"+=","apl"},
    {"-=","ami"},
    {"*","ml"},
    {"++","pp"},
    {"--","mm"},
    {"<","lt"},
    {">","gt"},
    {"&&","aa"},
    {"||","oo"},
    {"&","ad"},
    {"|","or"},
    {NULL,NULL}
   };

class GCC2Mangler: public Mangler {
public:
 GCC2Mangler() 
 {
  m_begin_name = "";
  m_rpt_type = "T";
  m_void_args = "v";  

  m_end_stdarg = "e"; 
  m_end_args = "";
  m_plain_function = "F";
  m_function_ptr = "PF"; 
  m_method_ptr = "";  // ?que           
  m_end_function_ptr = "_v";
  m_method_ptr_type = "";   // *add 1.2.3
  m_use_return_type = false;
  m_plain_obj = "G";
  m_const_attrib = "C";
  m_plain_attrib = "";
  m_ref_chr = 'R';
  m_array_char = 'P';
  m_ptr_char = 'P';
  m_method_type_first = true;
  m_convention_first = false;
  m_collect_repeats = "N";

  m_end_class = "";
  m_template_name = "t";
  m_template_args = "1Z";
  m_namespace = NULL;

  m_discard_sig_types = true;
  m_enum = "";
 }

 bool simple_type(Type t) // GCC2Mangler
 {
//     return ! t.is_class() && ! t.is_signature();
     return t.is_number() && !t.is_ref_or_ptr();
 }

 void out_simple_classname(const string& name) // GCC2Mangler
 {
    outi(name.length());
    outs(name.c_str());
 }

 void do_void_args()  // GCC2Mangler
 {
 // a curious exception - class member functions don't get a special void args
 // *fix 1.2.4 class_context() is non-null if a function is a member (static!)
   if (! m_fn->class_context()) Mangler::do_void_args();
 }
 
 char type_encode(Type t) // GCC2Mangler
 {
   	 char ch;
	 if (t.is_void()) ch = 'v'; else
	 if (t.is_bool()) ch = 'b'; 
	 else
     if (t.is_int()) {
         if(t.is_unsigned()) out('U');
         if (t.is_char())  ch = 'c'; else
         if (t.is_short()) ch = 's'; else
         if (t.is_long())  ch = 'l';
         else ch = 'i'; // plain 'int'
      } else
      if (t.is_float()) {
         if (t.is_double()) ch = 'd';
         else ch = 'f';
      } else 
         return 0;
      return ch;      
  }



  void out_fun_name(string fun_name) // GCC2Mangler
  {
     const char *name = fun_name.c_str();
     if (m_fn->is_constructor()) outs("__");  else
     if (m_fn->is_destructor()) outs(GCC_DTOR);
     else {
   	   char *mn = lookup_opname(gcc2_fns,name);
	   if (mn != NULL) { outs("__"); outs(mn); }
	   else { 
         outs(name);
       }
       outs("__");
     }
   }

  void do_calling_convention() // GCC2Mangler
  {
   //  if (m_fn->is_stdcall()) outs("@0"); 
  }


  void out_method_type(int access) // GCC2Mangler
  {
   if (!m_fn->is_static() && m_fn->is_const()) out('C');
  } 


};

//////// GCC 3 mangler ////////////////////////

struct ArgTypeEntry {
    Type m_t;
    int m_arg_idx;
};

typedef std::list<ArgTypeEntry> ArgTypeList;

ArgTypeList mArgTypeList;

struct TypeObjEntry {
    void* m_pt;
	Type  m_t;
    int m_type_idx;
};

typedef std::list<TypeObjEntry> TypeObjList;

TypeObjList mTypeObjList;

const int NOT_FOUND = -2;
int match_exact_arg(Type t);
int match_type_arg(Type& t);


OpNames gcc3_fns[] = {
    {"__C__","C1"},
    {"__D__","C2"},
    {"=", "aS"},
    {"==","eq"},
    {"!=","ne"},  
    {"[]","ix"},
    {"<<","ls"},
    {">>","rs"},
    {"+","pl"},
    {"-","mi"},
    {"+=","pL"},
    {"-=","mI"},
    {"*","ml"},
    {"++","pp"},
    {"--","mm"},
    {"<","lt"},
    {">","gt"},
    {"&&","aa"},
    {"||","oo"},
    {"&","an"},
    {"|","or"},
    {NULL,NULL}
   };



class GCC3Mangler: public Mangler {
    int m_no_attrib;
    int m_type_idx;
	Type m_t;
    bool m_no_namespace;
public:

    char type_encode(Type t)
    {
        char ch;
        if (t.is_void()) ch = 'v'; else
        if (t.is_bool()) ch = 'b'; 
        else 
        if (t.is_int()) {
          if (! t.is_unsigned()) {
            if (t.is_char())  ch = 'c'; else
            if (t.is_short()) ch = 's'; else
            if (t.is_long())  ch = 'l';
            else ch = 'i'; // plain 'int'
          } else {
            if (t.is_char())  ch = 'h'; else
            if (t.is_short()) ch = 't'; else
            if (t.is_long())  ch = 'm';
            else ch = 'j'; 
          }
        } else
        if (t.is_float()) {
         if (t.is_double()) ch = 'd';
         else ch = 'f';
        } else 
         return 0;
        return ch;      
    }

   bool simple_type(Type t) // GCC3Mangler
   {
     return t.is_number() && !t.is_ref_or_ptr();
   }

   void out_attrib_char(char ch)
   {
       ++m_no_attrib;
       out(ch);
   }


   void out_ref_ptr(Type t)
   {
  // put out ref/ptr attributes 
     if (t.is_ref_or_ptr()) {
         if(t.is_reference())  out_attrib_char('R');  
         if(t.is_pointer()) {
           for(int i = 0; i < t.pointer_depth(); i++)  out_attrib_char('P');
         }
         //..the UnderC type system can't currently represent _multiple const attributes_
         if (t.is_const()) out_attrib_char('K');
     }
   }

   void out_name(const string& name)
   {
    outi(name.length());
    outs(name.c_str());
   }

   bool out_namespace(Table* pc)
   {
      if (! pc || m_no_namespace) return false;
      Namespace* ns = pc->type()==IS_NAMESPACE ? (Namespace*)pc : 0;
      //dynamic_cast<Namespace*>(pc);
      if (ns) {
          out('N');
          out_name(ns->name());
          add_type(ns);
          return true;
      }
      else return false;
   }

   void add_type(void* pt)
   {
       TypeObjEntry toe;
	   toe.m_pt = pt;
	   toe.m_type_idx = m_type_idx;
	   toe.m_t = m_t;       
       mTypeObjList.push_back(toe);
       ++m_type_idx; 
   }

   void out_classname(Class *pc) 
   {
    string name; 
    bool wuz_inside_ns = out_namespace (pc->inside_namespace());
    add_type(pc);
    if (pc->get_template()) {	   
	   out_name(pc->get_template()->name());
       out('I');
       TypeList& tl = pc->get_template()->type_parms();
       TypeList::iterator tli;
       for (tli = tl.begin(); tli != tl.end(); ++tli)
           out_type(*tli);
       out('E');
    } else {
        out_name(pc->name());
    }    
    if (wuz_inside_ns) out('E');
   }


    void out_type(Type t) 
    { 
      // encoding function ptrs
      if (t.is_signature()) {
       Signature* sig = t.as_signature();
      //bool wuz_inside_ns = out_namespace(sig->context());
	   if (sig->class_ptr() == NULL) { // NOT a method
          outs("PF");
       } else {
          out('M');
          out_classname(sig->class_ptr());
          out('F');
       }  
       out_type(sig->return_type());
	   do_signature(sig);
       out('E');
       //if (wuz_inside_ns) out('E');
      } 
      // classes and enums are output simularly
      else if (t.is_class()) out_classname(t.as_class());
      else if (t.is_enum())  { 
          Enum* e = t.as_enum();
          bool wuz_inside_ns = out_namespace(e->context());
          out_name(e->name());
          add_type(e);
          if (wuz_inside_ns) out('E');
      }
      else out(type_encode(t));    
   }


   void out_ref(int idx)
   {
   // references to previous types are of the form Sx_,
   // where x is either empty or a hexadecimal digit.
     out('S');
     if (idx > -1) outx(idx);
     out('_');
   }

   void do_signature(Signature *sig)
   {
      if (sig->begin() == sig->end()) out('v');
      else {
        int arg_idx, type_idx,start_type_idx,start_arg_idx = 0;
        Signature::iterator sigi;
        for (sigi = sig->begin(); sigi != sig->end(); ++sigi) {
           m_t = *sigi;           
         // simple types encode simply according to the above rules
           if (simple_type(m_t)) out(type_encode(m_t));
           else
         // has this (non-simple) type already appeared in the arglist?
           if ((arg_idx = match_exact_arg(m_t)) != NOT_FOUND) out_ref(arg_idx);               
           else {
            // how many attributes (const, pointer, etc) in this type?
            m_no_attrib = 0;
            // has the type already appeared in the arglist?
            if ((type_idx = match_type_arg(m_t)) != NOT_FOUND) {
               out_ref_ptr(m_t);
               out_ref(type_idx);
            } else {
               out_ref_ptr(m_t);
               out_type(m_t);
            }  
            start_arg_idx   = m_type_idx + m_no_attrib - 1;
            ArgTypeEntry ate;
            ate.m_t = m_t;
            ate.m_arg_idx = start_arg_idx;
            mArgTypeList.push_back(ate);
            m_type_idx = start_arg_idx + 1;
           }
        }
      }
   }


  void out_fun_name(string fun_name) // GCC3Mangler
  {
     const char *name = fun_name.c_str();
     bool wuz_inside_ns = false, wuz_inside_class = false;
     outs("_Z");
     PClass pc = m_fn->class_context();
     if (pc != NULL) { // is a class member function
       out('N');
       if (m_fn->is_const()) out('K');
       out_classname(pc);
       wuz_inside_class = true;
     } else { // may well be inside a namespace...
       Table* cntxt = m_fn->context();
       if (cntxt)
         wuz_inside_ns = out_namespace(cntxt->parent_context());
     }
     // the actual name
     char *mn = lookup_opname(gcc3_fns,name);
     if (mn != NULL) outs(mn);
     else 
         out_name(name);
     
     if (wuz_inside_class || wuz_inside_ns) out('E');
   }

  char *mangle(Function *pfn)  // GCC3Mangler
  {
   reset();
   m_no_namespace = true;
   m_t = t_void;
   m_fn = pfn;
   m_type_idx = -1;
   mArgTypeList.clear();
   mTypeObjList.clear();
   out_fun_name(pfn->name());

 // any arguments
   do_signature(m_fn->signature());

 // end output string
   end();
   return ptr();
 }

};

int match_exact_arg(Type t)
{
     ArgTypeList::iterator ali;
     for(ali = mArgTypeList.begin(); ali != mArgTypeList.end(); ++ali)
         if (t == ali->m_t) return ali->m_arg_idx;
     return NOT_FOUND;
}

int match_type_arg(Type& t)
{
     void *pt;
     if (t.is_class()) pt = t.as_class(); else
     if (t.is_enum()) pt = t.as_enum();
     else return NOT_FOUND;
     TypeObjList::iterator ali;
     for (ali = mTypeObjList.begin(); ali != mTypeObjList.end(); ++ali)
       if (ali->m_pt == pt) { // a class or enum has been repeated!
	     int idx = ali->m_type_idx;
         Type at = ali->m_t;
         // we must allow for _partial_ matches, where this argument already has
         // some of the necessary qualifiers.
         if (t.is_const()) {
           if (at.is_const()) {
             t.strip_const();
             idx++;
            } else return idx; // t has more qualifiers
         }
         int ad = at.is_pointer() ? at.pointer_depth() : 0;
         int  d =  t.is_pointer() ?  t.pointer_depth() : 0;
         if (ad > 0 && d > 0 && ad > d) {
           for(int i = 0, n = ad - d; i < n; i++) {
              t.decr_pointer();
              idx++;
           }
         }
         if (at.is_reference() && t.is_reference()) {
            t.strip_reference();
            idx++;
         }                
	     return idx;
       }
     return NOT_FOUND;
}

static MSMangler ms_mangler;
static GCC2Mangler gcc2_mangler;
static GCC3Mangler gcc3_mangler;

char *Mangle::microsoft(Function *pf)
{
  return ms_mangler.mangle(pf);
}

char *Mangle::GCC2(Function *pf)
{
  return gcc2_mangler.mangle(pf);
}

char *Mangle::GCC3(Function *pf)
{
  return gcc3_mangler.mangle(pf);
}

char *i2a(int i)
{
     static char buff[15];
     sprintf(buff,"%d",i);
     return buff;
}

char *i2ax(int i)
{
     static char buff[15];
     sprintf(buff,"%X",i);
     return buff;
}

char *lookup_opname(OpNames *popn, const char *n)
{
  for(; popn->cname != NULL; popn++) 
    if (strcmp(popn->cname,n)==0) return popn->mname;
  return NULL;
}

