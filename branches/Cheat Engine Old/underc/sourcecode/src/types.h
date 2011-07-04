/* TYPES.H
 * Interface to UC Type System
 */
#ifndef __TYPES_H
#define __TYPES_H

#include "classlib.h"
#include <list>
#include <vector>

enum TypeEnum {
 TT_VOID=0,
 TT_UNSIGNED=1,TT_LONG=2,TT_PTR=4,
 TT_CHAR,TT_SHORT,TT_INT,TT_FLOAT, 
 TT_ZERO,TT_SIGNATURE,TT_CLASS,TT_ENUM,TT_BOOL,TT_NAMESPACE,
 TT_DUMMY, TT_CONSTANT, TT_NULL,
 TT_END
};

typedef unsigned int uint;
typedef unsigned char Bit;
typedef unsigned char Byte;
enum TypeDistance {
  EXACT_MATCH, FUN_PTR_MATCH, REFERENCE_MATCH, TRIVIAL_MATCH,
  PROMOTE_MATCH, REF_PROMOTE_MATCH,
  STD_MATCH,  REF_STD_MATCH,
  CONVERT_FROM_MATCH, CONVERT_TO_MATCH, 
  NO_MATCH=9999
};

class Signature;  // forward!!
class Class;
class Enum;
class Namespace;
class DummyType;
struct Entry;

class Type {

// 16-bit bitfield plus extra index representation
 Bit  m_const:1, m_ref:1;
 Byte m_ptr:2;
 Bit  m_class:1, m_signature:1, m_enum:1;
 Bit  m_int:1, m_float:1;
 Bit  m_long:1, m_short:1, m_unsigned:1, m_char:1;
 Bit  m_bool:1;
 Bit m_zero:1;  
 Bit m_array:1;// up to 16 bits for fields
 unsigned short m_extra;
 
 public:
    Type(TypeEnum t=TT_VOID, int extra=TT_VOID);    
    Type(Class *pc)
       { complex_init(TT_CLASS,(void *)pc); }
    Type(Signature *ps)
       { complex_init(TT_SIGNATURE,(void *)ps); }
    Type(Enum *pe)
       { complex_init(TT_ENUM,(void *)pe); }
    Type(Namespace *ns)
       { complex_init(TT_NAMESPACE, (void *)ns); }

    void complex_init(TypeEnum t, void *type_data);

    bool operator == (Type t) const;
    bool operator != (Type t) const
    { return !( (*this)==t ); }

 // querying the precise type
    bool is_const() const      { return m_const; }
    bool is_reference() const  { return m_ref;   }
    bool is_pointer() const    { return m_ptr;   }
    bool is_ref_or_ptr() const { return m_ref || m_ptr; } 
    bool is_array() const      { return m_ptr && m_array;  }   //*ARRAY*
    bool is_unsigned() const   { return m_unsigned; } 
    bool is_number() const     { return (m_int || m_float) && !is_pointer(); }
    bool is_int() const        { return m_int;   }
    bool is_float() const      { return m_float; }
    bool is_single() const     { return m_float && !m_long; }
    bool is_long() const       { return m_long;  }
    bool is_short() const      { return m_short && !m_enum; }
    bool is_char() const       { return m_char && m_int;  }
    bool is_double() const     { return m_float && m_long; }
    bool is_signature() const  { return m_signature; }
    bool is_function() const   { return is_signature() && !is_pointer(); }
    bool is_class() const      { return m_class; }
    bool is_object() const     { return is_class() & ! is_ref_or_ptr(); }
    bool is_enum() const       { return m_enum && !m_short; }
    bool is_bool() const       { return m_bool; } 
    bool is_void() const    
      { return !m_int && m_char; }
    bool is_zero() const       { return m_zero && !m_ref; }
    bool is_namespace() const  { return m_class && m_zero; }  // should never be confused since it's a TYPENAME!

    bool is_dummy() const;
    bool is_null() const;
    bool is_bare() const;
    int  pointer_depth() const { return m_ptr;  }
    bool is_unbound() const;
    bool is_template_class() const;

 // building up types
    void make_const()          { m_const = 1; }
    void make_unsigned()       { m_unsigned = 1; }
    void make_zero_int()       { m_zero = 0; }
    void incr_pointer()        { m_ptr++;   }
    void decr_pointer()        { m_ptr--;   }  // any checking??
    void make_reference()      { m_ref = 1; }
    void make_const_reference(){ m_ref = 1; m_const = 1; }
    void make_array(int sz)    { incr_pointer(); m_array = 1; } // for now...
    void strip_const()         { m_const = 0; }
    void strip_reference()     { m_ref = 0; m_zero = 0; }
    void strip_array()         { m_array = 0; }
    void strip_qualifiers()    { strip_reference(); strip_const();  strip_array(); }

    // distinguishing between 'true' and 'variable' references!
    // Should be safe: is_zero() is only used in Type::std_match(),
    // and its other use is for distinguishing namespaces, which are not variable types.
    void make_variable()            { make_reference(); m_zero = 1; }
    bool is_variable() const        { return is_reference() && m_zero; }
    bool is_plain_reference() const { return is_reference() && ! is_variable(); }

 // general operations on types
    Type           promote() const;
    void           as_string(string& s) const;
    string         value_as_string(void *ptr, bool do_quotes = true) const;

 // accessing details of complex types
    Signature *as_signature() const;
    Class     *as_class() const;
    Enum      *as_enum() const;
    DummyType *as_dummy() const { return (DummyType *)as_enum(); }
     
    int       array_size() { return 0; } // for now...


 // direct access to common complex type operations
    int      inherits_from(Type t) const;  // depth of derivation; 0 for no relation
    int      size() const;                 // implements sizeof()
    static   Type make_dummy(const string& name, Type t, Entry *pe);
};

extern const Type 
  t_void, t_char, t_uchar, t_int, t_uint,
  t_short, t_ushort, t_long, t_ulong,
  t_float, t_double, t_bool,
  t_void_ptr, t_char_ptr, t_zero, t_template_type, t_null, t_label;

// can NOW properly define Signature and class!!
//typedef std::vector<Type> TypeArray;
typedef std::list<Type> TypeArray;
typedef std::list<Type> TypeList;

#include "signature.h"
#include "class.h"

ostream& operator << (ostream& os, Type t);
ostream& operator << (ostream& os, TypeDistance td);

// type matching functions
typedef TypeDistance (*MatchFunction)(Type t1,Type t2);
TypeDistance  match(Type t1,Type t2);
TypeDistance  trivial_match(Type t1, Type t2);
TypeDistance  promote_match(Type t1, Type t2);
TypeDistance  std_match(Type t1, Type t2);

#endif                  
