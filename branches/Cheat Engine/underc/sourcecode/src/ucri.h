/* UnderC Reflection Interface (UCRI)
 * UnderC C++ interpreter
 * Steve Donovan, 2001,2002
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 * 
 * ***UCRI version 1.0***
 */
/*! \file ucri.h
 * UCRI supplies a dynamic interface to the underlying
 * UnderC rich type system, beyond standard RTTI. 
 * You can lookup symbols, extract all entries in a given
 * context (namespace or class), inspect functions and
 * instantiate templates dynamically. Objects may be created
 * using the class object.
 *
 * These classes are essentially thin wrappers over the 
 * existing internal implementation classes,
 * e.g. XClass is a wrapper around a Class object pointer.
 * I also have attempted to give a simplified view of 
 * UnderC internals.
 */
#ifndef __UCRI_H
#define __UCRI_H

#ifdef WITHIN_UC
# include "common.h"
#else
// I'm using forward class declarations to minimise the
// link mismatches you would get from simple typedefs.
   struct Entry;
   typedef Entry *PEntry;
   class NamedTable;
   class Function;
   class Class;
   class Module;
   class Type;
   class TemplateEntry;

   // these guys are needed for fblock.h
   class Table;
   class Instruction;
   typedef Instruction* PInstruction;
   class XTrace;
# include "fblock.h"

typedef unsigned int uint;

enum { // exactly as they appear in table.h
	   TEMPS=1, CONSTS=2, FUNCTIONS=4, NON_STATIC=8, 
       VIRTUALS=16, BUILTINS=32,
       CTORS=64, DTORS=128, TYPEDEFS=256,
	   NAMESPACES=512, CLASSES=1024, FIELDS=4096,
       IMPORTS=8192, UNDEFINED=16384,
	   DO_PARENT=2048 };

enum RMode { // address modes
  NONE,
  DIRECT,  // 'direct' here means 22bit offset into data area
  SREL,    // stack-relative (auto variables)  
  OREL,     // object-relative (class members)
  OREL_F    // bit-field (will also be object-relative)
};

#endif

// *add 1.2.4 Access to underlying pcode
// just like Instruction in engine.h
// 32-bit instruction record
struct XInstruction {
  uint opcode: 8;
  uint rmode: 2;
  int data: 22;
};

// listx looks like the UC pocket list class, and
// uses the correct allocator. If you use the typedefs,
// there should be no future problems when I can use 
// a proper std::list.
#include "listx.h"
#include "export.h"
#include "xtrace.h"

class XFunction;
class XClass;
class XTemplateFun;
class XType;
typedef listx<XType *> XTList;
typedef listx<string> XStringList;

/// XType wraps Type (see types.h).
/// I've left out the means to modify the underlying type.
class EXPORT XType {
private:
	Type* m_type;
public:
	XType(Type* pt);
	bool is_const() const;
	bool is_reference() const;
	bool is_pointer() const;
    bool is_array() const;
    bool is_unsigned() const;
    bool is_number() const;
    bool is_int() const;   
    bool is_float() const;
    bool is_single() const; 
    bool is_long() const;    
    bool is_short() const;   
    bool is_char() const;    
    bool is_double() const;  
    bool is_signature() const; 
    bool is_function() const;  
    bool is_class() const;     
    bool is_object() const;    
    bool is_bool() const;      
    bool is_void() const;    
    bool is_namespace() const;
    int  pointer_depth() const; 
    int     size() const;
/// if is_class() is true, then this is the class object.
    XClass* as_class() const;
/// convert to a string representation, eg. "const char*".
    char*   as_str() const;
/// convert a value to a string.
/// \param s An output string
/// \param ptr A pointer to a data value
    void    val_as_str(string& s, void *ptr) const;
/// convert an ASCII representation into data.
/// \param buff a null-terminated char buffer
/// \param ptr a pointer to the converted value
	void    str_to_val(char *buff, void *ptr);
	Type*   type();
/// convert an ASCII representation (e.g. "int") into a type.
	static XType*  from_str(char *str);
/// convenient way to create a list of types.
	static  XTList& typelist(XType* t1,...);
};

/// XEntry wraps Entry, which is what all symbol tables
/// (classes, namespaces, function contexts, etc) contain.
/// Use it as a proxy for a variable.  If it was a function
/// entry, then you can extract all members of an overloaded
/// set using nfun() and function().
class EXPORT XEntry {
private:
	PEntry m_entry;
public:
	XEntry(PEntry pe = NULL);
    /// create a copy of the entry.
    XEntry *clone();
   /// given an array or pointer entry, then this is an entry representing an element.
	XEntry *base_entry();
   /// the symbol's name.
  	char*  name();
   /// pointer data, as an offset; the interpretation depends on the address mode.
	int    data();
   ///  converts the data into an actual pointer (the default is to use the global address space).
	void*  ptr(void *base=NULL);
    void   set_data(int x);
    void   set_ptr(void *p, void *base=NULL);
   /// size of the entry - it's usually 1, more for arrays.
	int    size();
   /// type of entry
	XType* type();
   /// the actual underlying Entry object which has been wrapped.
	void*  entry();
   /// convert the data to a string.
    void val_as_str(string& s, void *base=NULL);
   /// convert a string to valid data for this entries' type.
    void str_to_val(char *buff, void *base=NULL);
	// for Function entries only...
   /// number of functions in this overloaded set.
   /// (It will be non-zero if this is a function entry)
	int        nfun();                
   /// fetch each function in the set, starting with 1
	XFunction *function(int idx=1);   
	/// Address mode of entry.
    /// This can be DIRECT (for global data), SREL (for stack-relative)
    /// and OREL (for object-relative, i.e. non-static fields)
	int addr_mode();
};


/// XFunction wraps Function; You can get the function declaration as
/// a string, the types of the args, the return type, and location info.
/// set_trace() will attach an XTrace-derived object to the function;
/// this will be executed whenever entering or leaving the function.
class EXPORT XFunction {
private:
	Function* m_fun;
    Entry*    m_ref;
public:
	XFunction(Function *fun = NULL);
   /// short name of function.
    char*   name();
   /// get the fully qualified name of this function.
	void    as_str(string& s);
   /// the return type of the function
	XType*  ret_type();
   /// the function arguments, as a list of XType*
	XTList& args();
   /// An alternative way to get function arguments.
   /// (this will also get you the parameter names, if defined)
   /// \param tl  List of types
   /// \param sl  List of parameter names
	void    get_args(XTList* tl, XStringList* sl);
   /// Where this function is defined.
   /// \param filename A string to receive the filename
   /// \return The line number
	int     where(string& filename);
  /// Convert an address into a line number
  /// \param ip A pointer to an instruction
  /// \return The corresponding line number
    int     ip_to_line(void* ip);
  /// This function's module id
  /// \return A module id 
  /// \sa XModule
	int     module();
  /// The function block
	void*   fblock();
  /// The underlying Function object
    void*   fun();
  /// Evaluate this function.
  /// \param args  Pointer to an array of arguments
  /// \param result Pointer to the result
  /// \param obj  Optional object pointer, if this is a method
  /// \return OK if fine
	int     eval(void *args, void *result, void *obj=NULL);
  /// Attach a trace object to the function
  /// \param tr  A trace object
  /// \sa XTrace
    void    set_trace(XTrace* tr);
  /// Get the current trace object, if any.
  /// \return The trace object
    XTrace* get_trace();

 /// Get the function's pcode.
  /// This is the underlying bytecode compiled by UnderC; accessing
  /// it is useful if you are looking for specific references.
  /// \return A pointer to the pcode
    XInstruction* pcode();
  /// Globally set whether we are tracing functions or not.
    static void set_tracing(bool yesno);
  /// Given a function block, find its function
    static XFunction* from_fb(void* fb);
  /// Look up a variable name in the function's context
  /// \param Name of Variable (usually a formal parameter)
  /// \return The corresponding entry
  /// \sa XEntry
	XEntry* lookup_local(char* name);
};

typedef listx<XFunction *> XFunctions;
typedef listx<XEntry *> XEntries;

/// XNTable wraps NamedTable, and is the base class for
/// XClass; You can look up symbols, or get a list of 
/// all available symbols.
/// do note that get_functions() etc can be given optional
/// mask patterns (Not general regular expressions - just simple wildcards
/// either like '*_init' or like 'get_*')
class EXPORT XNTable {
protected:
	NamedTable *m_table;
public:
	XNTable(NamedTable *tbl);
  /// Look up a name in this context
  /// \param name  Symbol name
  /// \param in_parent  True if you want to keep looking in the parent context 
  /// \return The symbol table entry, or NULL if not found.
  /// \sa XEntry
	virtual XEntry* lookup(char *name,bool in_parent=false);
 /// Look up a class in this context
 /// \return A pointer to the class object, NULL if not found or not a class
 /// \sa XClass
	XClass* lookup_class(char *name, bool in_parent=false);
 /// Look up a template in this context
	XTemplateFun* lookup_template(char *name, bool in_p=false);
 /// The underlying NamedTable object
    NamedTable* table();
 /// Name of this symbol table
	char*  name();	
 /// The offset of a given pointer in this context's data space
    int offset(void* p);
 /// Create a new entry in this table
 /// \param nm Name of new symbol
 /// \param xt  Type of entry
 /// \sa XEntry
    XEntry *create(char *nm = "", XType *xt = NULL);
 /// A list of all functions in this context
 /// \param flags Can control whether you want non-static, etc.
 /// \return a function list
	XFunctions& functions(int flags=0);
 /// a list of all variables in this context
	XEntries&   variables(int flags=FIELDS);

 /// \c get_functions() is the prefered way to get a list of functions in this context. \a flags can be:
 /// \li \c  NON_STATIC  non-static member functions
 /// \li \c VIRTUALS virtual member functions
 /// \li \c BUILTINS  UnderC builtin functions
 /// \li \c CTORS   Class constructors
 /// \li \c DTORS   Class destructors
 /// \li \c IMPORTS Functions imported from a DLL
 /// \li \c UNDEFINED Functions which have been declared but not defined yet. 
 ///
 /// You can combine this with \c DO_PARENT to extract entries from the parent
 /// context, and so on.
 ///
 /// \param fns A list to receive the functions
 /// \param flags Controls type of functions required
 /// \param pattern Can specify a wildcard
	void get_functions(XFunctions& fns, int flags = 0,char *pattern=NULL);

 /// \c get_variables() is the prefered way to get a list of variables.
 /// It can be used to fetch \em any kind of entry,
 /// depending on the flags used.  For types, use these flag values:
 /// \li \c TYPEDEFS  any typedefs
 /// \li \c NAMESPACES  any namespaces
 /// \li  \c CLASSES any classes
 ///
 /// For variables, you can combine FIELDS with one of the following flags:
 /// \li \c TEMPS  temporary variables
 /// \li \c CONSTS  constants (\em not macros)
 /// \li \c NON_STATIC  non-static members of a struct
 /// 
 /// Again, \a flags can contain DO_PARENT as before.
 	void get_variables(XEntries& vars, int flags=FIELDS,char *pattern=NULL);

	static void dispose_of_entries(XEntries& vars);
};

class XTemplateFun ;

class EXPORT XClass: public XNTable {
public:
	XClass(NamedTable *tbl);
 /// underlying Class object. 
	Class*      class_obj();
 /// base class of this class.
 /// \return Base class if defined; NULL otherwise
    XClass*     base_class();
 /// Create an object of this type.
 /// It is assumed that this class has a suitable default constructor.
	void*        create();
 /// Does this class inherit from a given class?
 /// \param xc Another class
 /// \return A measure of inheritance depth; zero if not related.
	int            inherits_from(XClass *xc);
 /// If we are a template class, the number of template parameters
 /// \return Zero for ordinary classes
    int            no_template_parms();
 /// The type parameters of a template class
 /// \return A list of types;  NULL if not a template
    XType*     template_parm(int idx); 
	XTemplateFun*  get_template();
    // *add 1.2.4 Accessing RTTI; setting and getting the class of an object
 /// Has this class got a VMT (Virtual Method Table)? .
    bool           has_VMT();
  /// Given an object, what is its dynamic class?.
    static XClass* get_class_of(void* p);
  /// Set the class of an object dynamically.
    void           set_class_of(void* p);
};

class XModule;
typedef listx<XModule *> XModules;
typedef listx<XClass *> XClasses;

/// XModule wraps Module, which is the UC representation
/// of a loaded source file (of any extension). Note that
/// the name lookup is exact. You can also use the module
/// id from XFunction::module() for access.  The lists()
/// function will give you all currently loaded modules.
class EXPORT XModule {
private:
	Module* m_mod;
public:
	XModule(Module *pm);
 /// This translates a module index into a module object
 /// \param id Index
 /// \return XModule object; NULL if the index is out of range
	static XModule*   from_id(int id);
 /// Likewise, except using a filename (must be exact match).
	static XModule*   from_name(char* filename); 
 /// All available modules.
	static XModules&  lists();
 /// What is the filename of this module?
	char*       filename();
 /// List of functions defined in this module.
	XFunctions& functions();
 /// List of classes defined in this module.
	XClasses&   classes();
};

/// XTemplateFun wraps TemplateEntry. You can use 
/// XNTable::lookup_template() to get a ptr to your template,
/// and then dynamically instantiate it. match_instantiate()
/// is passed argument types (which is the usual way it's
/// done in C++) and instantiate() is passed formal type
/// parameters (which is more convenient)
class EXPORT XTemplateFun {
private:
	TemplateEntry* m_templ;
public:
	XTemplateFun(TemplateEntry *te);
  /// Instantiate this template function, given argument types
  /// \param tl Formal argument types
    void* match_instantiate(const XTList& tl);
  /// Instantiate this template function, given type parameters
  /// \param tl Formal template type parameters
	void* instantiate(const XTList& tl);
  /// Name of this template
	char* name();
};

// Access to the important namespaces.
// Please remember to call uc_ucri_init() to make
// sure these are available!
/// The global symbol table.
EXPORT XNTable* uc_global();
/// The std standard namespace.
EXPORT XNTable* uc_std();
/// To be called before accessing any UCRI function.
EXPORT void uc_ucri_init();

// *add 1.2.4 Profiling support
EXPORT unsigned long* ucri_instruction_counter(bool do_profiling);

///  Evaluate a UnderC function ptr, optionally with object pointer.
#ifdef __UNDERC__
CEXPORT XAPI int uc_eval_method(void *sc, void *obj, void *arguments, void *result);
#else
CEXPORT int XAPI uc_eval_method(void *sc, void *obj, void *arguments, void *result);
#endif

#endif 



