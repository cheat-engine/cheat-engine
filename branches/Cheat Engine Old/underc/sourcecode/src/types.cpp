/* TYPES.CPP
 * The UC type system interface
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 */

#include "types.h"

#include "common.h"
#include "templates.h"

 ostream& operator << (ostream&, string);
 istream& operator >> (istream&, string&);


const Type 
  t_bool(TT_BOOL), t_null(TT_NULL,0), t_template_type((Class *)-1),
  t_void, t_char(TT_CHAR), t_uchar(TT_CHAR,TT_UNSIGNED), t_int(TT_INT), t_uint(TT_INT,TT_UNSIGNED),
  t_short(TT_SHORT), t_ushort(TT_SHORT,TT_UNSIGNED), t_long(TT_INT,TT_LONG),
  t_ulong(TT_INT,TT_LONG | TT_UNSIGNED), 
  t_float(TT_FLOAT), t_double(TT_FLOAT,TT_LONG),
  t_void_ptr(TT_VOID,TT_PTR), t_char_ptr(TT_CHAR,TT_PTR),
  t_zero(TT_ZERO),t_label(TT_NULL,1);

namespace { // local to this module
 std::vector<Class *>      _classes_;
 std::vector<Signature *>  _signatures_;
 std::vector<Enum *>       _enums_;
}

template <class T>
int index_into(std::vector<T>& v, T val)
{
 for(int i = 0; i < v.size(); i++)
  if (v[i] == val) return i;

 // otherwise it ain't here - add to vector!
 v.push_back(val);
 return v.size()-1;
}

// *fix 0.9.4 Looks through the signature list for matches...
// *fix 1.1.0 Insist that they are completely equal, down to the return type
// Should this not be part of what it means for signatures to match?
Signature *unique_signature(Signature *sig)
{
  std::vector<Signature *>::iterator is = _signatures_.begin(),
                                     ie = _signatures_.end();
  for(; is != ie; ++is) {
    Signature *s = *is;  
    // *change 1.2.3a Signature::match() is here called in "exact match" mode!
    if (s->return_type()==sig->return_type() && s->match(*sig,true)) return *is;
  }
  return sig;
}

Signature *find_signature(void *type_data)
{
   Signature *orig = (Signature *)type_data;
   Signature *uniq = unique_signature(orig);
   return uniq ? uniq : orig;
}


Type::Type(TypeEnum t, int extra)
{
  *((long *)this) = 0;  //*SJD* Nasty but effective!
  if(t == TT_NULL) {
    m_extra = extra;   
    return;
  }
// hmm...are bools considered a kind of integer?
 if (t != TT_VOID && t != TT_FLOAT && t != TT_BOOL) m_int = 1;
 else m_int = 0;
 if (t==TT_CHAR)  m_char = 1;   else
 if (t==TT_SHORT) m_short = 1;  else
 if (t==TT_VOID) m_char = 1;    else  // a fiddle....
 if (t==TT_FLOAT) m_float = 1;  else
 if (t==TT_ZERO) { m_zero = 1; m_const = 1; } else
 if (t==TT_BOOL) {
    m_bool = 1;
 }

 if (extra & TT_LONG) m_long = 1;
 if (extra & TT_UNSIGNED) m_unsigned = 1;
 if (extra & TT_PTR) m_ptr = 1;

 m_extra = 0;
}

void Type::complex_init(TypeEnum t, void *type_data)
{
  *((long *)this) = 0;  //*SJD* Nasty but effective!
  if (t == TT_CLASS || t == TT_NAMESPACE) {
    m_class = 1;       m_extra = index_into(_classes_,(Class *)type_data);
  // this might seem like asking for trouble, but namespace entries
  // are always TYPENAMEs not IDENs.  So the chances of confusion are slim.
    if (t == TT_NAMESPACE) m_zero = 1;
  } else
  if (t == TT_SIGNATURE) {
 // *change 1.2.2 (Eric) Always try to keep signatures unique
    m_signature = 1;   m_extra = index_into(_signatures_,find_signature(type_data));
  } else
  if (t == TT_ENUM || t == TT_DUMMY) {
    if (t == TT_DUMMY) m_short = 1;
    m_enum = 1;     
    // for dummies , _identical names_ means equality!
    // (can get away with this because both types derive from NamedType)
    Enum *ndata = (Enum *)type_data;
    int sz = _enums_.size();
    if (ndata->name() != "" || t != TT_DUMMY) {
      for(int i = 0; i < sz; i++)
        if (_enums_[i]==ndata 
          /*|| t == TT_DUMMY && _enums_[i]->name() == ndata->name()*/)
            { m_extra = i;    return; }
    } else {
      //*change 1.2.9 To support numerical template parameters properly,
      // we have to keep dummies with the same value identical;  this
      // code will only work with integer types (you're on your own
      // with other types)
       DummyType* dt = (DummyType*)type_data;
       int val = Parser::const_int_expr(dt->entry());
       for(int i = 0; i < sz; i++) {
           if (_enums_[i]->name() == "") {
               DummyType* de = (DummyType*)_enums_[i];
               if (val == Parser::const_int_expr(de->entry())) {
                   m_extra = i; return;
               }
           }
       }
    }
    // otherwise, add to the end!!
    m_extra = _enums_.size();
    _enums_.push_back(ndata);
  }
 
}

bool Type::operator == (Type t)  const
{
// a complete kludge!
// again, a basic assumption that the Type object fits snugly into a 32-bit word.
 return *((long *)this) == *((long *)&t);
}

bool Type::is_null() const
{ return *((long *)this) == 0; }

bool Type::is_bare() const
{ 
  return !is_const() && !is_pointer() && !is_reference();
}

bool Type::is_unbound() const
{
if (is_null()) return false;   // null!
if (!is_dummy()) return true;  // everything else has a definite type value
return as_dummy()->unbound();
}

bool Type::is_template_class() const
{
 return is_class() && as_class()->get_template() != NULL;
}


Type Type::make_dummy(const string& name, Type t, PEntry pe)
{
 Type rt;
 DummyType *pdum = new DummyType(name,t,pe);
 rt.complex_init(TT_DUMMY,pdum);
 return rt;
}

bool Type::is_dummy() const
{
  if (m_enum && m_short) return true; // plain dummy
  if (is_template_class()) { // otherwise it may have at least one dummy parameter!
    Class *pc = as_class();
    const TypeList& tl = pc->get_template()->type_parms();
    TypeList::const_iterator tli;
    int kount = 0;
    FORALL(tli,tl)
      if (tli->is_dummy()) kount++;
    return kount > 0;
  }
  return false;
}


Signature *Type::as_signature() const
{
 if (m_signature) return _signatures_[m_extra]; else return NULL;
}

Class     *Type::as_class() const
{
 if (m_class) return _classes_[m_extra]; else return NULL;
}

Enum      *Type::as_enum() const
{
 return _enums_[m_extra]; 
}

//----- depth of derivation; 0 for no relation (delegated to class object)
int      Type::inherits_from(Type t) const
{
 if (is_class() && t.is_class()) return as_class()->inherits_from(t.as_class());
 else return 0;
}

int Type::size() const                 // implements sizeof()
{
  if (is_pointer()) return sizeof(void *);
  else 
  if (is_enum()) return sizeof(int);   else;  // for now....
  if (is_int()) {
   if (is_short()) return sizeof(short);  else
   if (is_long())  return sizeof(long);  else
   if (is_char())  return sizeof(char);  
   else return sizeof(int);
  } else
  if (is_float()) {
   if (is_double()) return sizeof(double); else return sizeof(float);
  } else
  if (is_class()) return as_class()->size();
  // *fix 1.2.3 Our bools now have standard size (8-bits)
  else if (is_bool()) return sizeof(bool);  
  else return sizeof(int);
}

class TypeSearch: public TableSearcher {
private:
	Class* m_class;
	Type m_type;
	Type m_dummy;
	Type m_result;
	
public:
	TypeSearch() {
		m_dummy = Type::make_dummy("_*_",t_void,NULL);
    }
	void set_type(Type t) {
      m_type = t;
	  m_class = t.as_class();
    }

	Type result() { return m_result; }

    virtual bool match(PEntry pe)
    {
    Type tt = pe->type;
    if (tt.is_class() && tt.as_class() == m_class) {
	 Type ta = m_type, td = m_dummy;
	 if (ta.is_const() && ! tt.is_const()) {
		 ta.strip_const();
		 td.make_const();
     }
	 while (ta.is_pointer() && ! tt.is_pointer()) {
		 ta.decr_pointer();
		 td.incr_pointer();
     }
	 if (ta.is_reference() &&  ! tt.is_reference()) {
		 ta.strip_reference();
		 td.make_reference();
     }
	 if (ta == tt) {
		 td.as_dummy()->set_name(pe->name);
		 m_result = td;
		 return true;
     }
    }
    return false;
   }

};

// *fix 1.2.7 We dump out the fully qualified names of classes and enums,
// which was necessary for delayed compilation of method bodies.
// *add 1.2.7 debug.use_typedef_names not only forces a global search for
// a typedef for a particular type, but leaves off 'std::' when std has
// been injected.  Not likely to be a popular feature with the ISO committee.

NamedTable* nested_context(PEntry pe)
{
  int type = pe->context->type();
  if (type == IS_NAMESPACE || type == IS_STRUCT) {
	  if (Parser::debug.use_typedef_names && pe->context == Parser::std_namespace()
		  && Parser::global().is_injected((Namespace*)pe->context)) return NULL;
	  else return (NamedTable*) pe->context;
  }
  else return NULL;
}

string enum_as_str(Enum *em)
{
 string s = em->name();
 // the issue here is that in C mode 'enum Fred' actually generates
 // a slightly mangled name $_Fred.
 if (s[0]=='$' && s[1] == '_') s = s.substr(2,s.length()-2);
 NamedTable* parent = nested_context(em->entry());
 if (parent) s = parent->name() + "::" + s;
 return s;
}

bool lookup_typedef_name(Type t, string& s)
{
  static TypeSearch type_search;
  type_search.set_type(t);
  PEntry pe = Parser::global().search_entries(&type_search,NULL,TYPEDEFS);
  if (pe != NULL) {
	 type_search.result().as_string(s);
	 return true;
  } else return false;
}

string class_as_str(Class *pc)
{
 string s;
 NamedTable* parent = nested_context(pc->entry());
 if (parent) s = parent->name() + "::";
 s += pc->name();
 return s;
}

void Type::as_string(string& s) const
{ 
 if (Parser::debug.use_typedef_names && lookup_typedef_name(*this,s)) return;
 s = "";
 if (is_enum()) { s = enum_as_str(as_enum()); return; }
 if (is_const()) s += "const ";
 if (is_unsigned()) s += "unsigned ";
 if (is_long() && is_int()) s += "long ";
 if (is_char())   s += "char"; else
 if (is_short())  s += "short"; else
 if (is_double()) s += "double"; else
 if (is_float())  s += "float"; else
 if (is_void())   s += "void"; else
 if (is_int())    s += "int"; else
 if (is_bool())   s += "bool"; else
 if (is_class())  s += class_as_str(as_class());
 else
 if (is_signature()) {
   char buff[256];
   ostrstream out(buff,256);
   if (is_pointer()) Signature::set_fun_name("(*)");
   out << *as_signature();
   if (is_pointer()) Signature::set_fun_name("");
   out << ends;
   s = buff;   
   return;
 } else 
 if (is_dummy()) s += as_dummy()->value_as_string();

 if (is_pointer()) {
    for(int i = 0; i < pointer_depth(); i++) s += "*";
 }
 #ifdef _DEBUG
 if (is_variable()) s += "!"; else  
 #endif
 if (is_reference() && !is_variable()) s += "&";
 
}

ostream& operator << (ostream& os, Type t)
{
 string stemp="";
 t.as_string(stemp);
 os << stemp;
 return os;
}

// in these match functions, the first arg represents the formal
// parameter, and the second the actual argument.

// Must remove the zero bit, in case we had a variable! (see note(3), Expressions::entry_op)
inline Type make_const(Type t)
{ Type tc = t; tc.make_const(); tc.make_zero_int(); return tc; }

inline Type strip_qualifiers(Type t)
{ Type tc = t; tc.strip_qualifiers(); return tc; }

inline Type make_const_ref(Type t)
{ Type tc = t; tc.make_const_reference(); tc.make_zero_int(); return tc; }

inline Type make_ref(Type t)
{ Type tc = t; tc.make_reference(); tc.make_zero_int(); return tc; }


TypeDistance
 trivial_match(Type t1, Type t2)
 //-----------------------------
{
// trivial match between t2 (actual) and t1 (formal) argument.
 if (t1.is_reference()) {
    if (t1.is_variable() && strip_qualifiers(t1) == strip_qualifiers(t2))
       return TRIVIAL_MATCH;    //const T& => T! (variable type) is fine...
    t2.make_zero_int();  // clear VARIABLE type
    t1.make_zero_int();

 // these are really exact or trivial matches but we need to know for arg passing
   if (t1 == t2) return REFERENCE_MATCH;     
   if (t1.is_const()) {
     // a non-const reference converts trivially to its const equivalent; 
     if(t1 == make_const(t2)) return REFERENCE_MATCH;  //T& => const T&   
     // T => const T& -- always cool because our constants have addresses
     // *fix 1.2.1 Must however exclude const T => T&!
     Type t2r = t2;
     t2r.make_reference();
     if (t2.is_const()) {
        if (t1 == t2r) return REFERENCE_MATCH;  // const T => const T&
     } else {  // exclude const T => T& !
        t2r.make_const();
        if (t1 == t2r) return REFERENCE_MATCH;  // T => const T&
     }
   }
 } 
 t2.make_zero_int(); 
 t1.make_zero_int();  
 if (!t1.is_ref_or_ptr()) t2.strip_const();
 if (t1 == t2) return EXACT_MATCH;
 
 // T * => const T *;  T[] => T*, T* => T[]
 if (t1.is_pointer()) {
     if (t1.is_const()) t1.strip_const();
     if (t1.is_array()) t1.strip_array();
     if (t2.is_array()) t2.strip_array();
     if (t2 == t1)  return TRIVIAL_MATCH;
 }
 //T  => T&,const T&
 // *fix 1.2.1 Must however exclude const T => T&!
// if (!t2.is_ref_or_ptr()) t2.strip_const();
 if (t2.is_reference() && strip_qualifiers(t2) == t1) return TRIVIAL_MATCH;
 //if (t1.is_reference() && strip_qualifiers(t1) == t2) return TRIVIAL_MATCH;
 return NO_MATCH;
}

TypeDistance 
 promote_match(Type t1, Type t2)
{
// promotions between t2 (actual) and t1 (formal) argument.
// *fix 0.9.7 Allow for t2 => const t1&, if we can promote t2 to t1.
 bool is_ref = t1.is_reference() & t1.is_const();
 if (is_ref) t1.strip_qualifiers();
 Type pt = t2.promote();
 if (pt == t1) return is_ref ? REF_PROMOTE_MATCH : PROMOTE_MATCH;
 return NO_MATCH;
}

TypeDistance
 std_match(Type t1, Type t2)
{
// standard conversions between t2 (actual) and t1 (formal) argument.
//cout << "std: f= " << t1 << " a= " << t2 << endl;
 bool ref_argument = t1.is_plain_reference();
 // *add 1.1.4 C is less fussy about integer conversions to enumerations...
 if (Parser::debug.c_mode && t1.is_enum() && t2.is_int()) return STD_MATCH;
 if (t1.is_number() && !ref_argument) {
  // in strict mode, there are no implicit bool conversions...
  // *change 1.2.3 Retired until we can think this through.....
  // if (t2==t_bool && Parser::debug.strict) return NO_MATCH;
   if (t2.is_number() || t2.is_enum()) return STD_MATCH;
   if (t2.is_zero()) return STD_MATCH;
  } else
 if (t2.is_pointer()) {
    if (t1.is_void() && t1.pointer_depth()==1) 
        return ref_argument ? REF_STD_MATCH : STD_MATCH;
    if (!t1.is_pointer()) return NO_MATCH;
 } 

 if (t1 == t_bool && t2.is_int()) {       
    return STD_MATCH;
 } 
 if (t1.is_pointer()) {
   if(t2.is_zero()) return STD_MATCH;
   if(t1.is_signature() && t2.is_signature() /*&& t1.as_signature()==t2.as_signature()*/)
       return FUN_PTR_MATCH; //*note* t2 is not a ptr in 'pf = sin' etc!
   if(!t2.is_pointer()) return NO_MATCH;
  }
// *fix 0.9.5 Standard reference matches score lower than ordinary reference matches
 if (t2.inherits_from(t1)) return (ref_argument && ! t2.is_plain_reference()) ?
                                      REF_STD_MATCH : STD_MATCH;
                              
 return NO_MATCH; // for now
}

TypeDistance  
 match(Type t1,Type t2)
{
 TypeDistance td = trivial_match(t1,t2);
 if (td != NO_MATCH) return td;
 td = promote_match(t1,t2);
 if (td != NO_MATCH) return td;
 td = std_match(t1,t2);
 if (td != NO_MATCH) return td;

 // for now
 return NO_MATCH;
}

Type Type::promote()  const
{
// from Lippman (2nd, 174)
 if (is_pointer() || is_reference() && ! is_variable()) return *this; //* doesn't apply!! 
 if (is_char() || is_short()) return t_int;
 if (is_short() && is_unsigned()) return t_int; // sizeof(int) > sizeof(short)
 if (is_float()) return t_double;
 if (is_bool()) return t_int; 
 return *this;
}

typedef unsigned long unsigned_t;

string Type::value_as_string(void *ptr, bool do_quotes) const
{
// *add 1.1.2 Putting quotes around strings is now optional.
 static char buff[512];
 memset(buff,0,512);
 ostrstream out(buff,512);  
 if (is_pointer()) {
     //*fix 1.2.6 Was attempting to dump char** etc as character constants
    if (is_char() && pointer_depth() == 1){  
    char *cp = *(char **)ptr;
    if (cp) {
		if (do_quotes) out << '"';
		out	<< cp;
		if (do_quotes) out << '"'; 
    } else out << "NULL";
   } else
   out << (void *)*(int **)ptr; 
 }  else
 if (is_enum()) { // *add 1.2.6 show enumeration constants, if possible
    int val = *(int *)ptr;
    string name = as_enum()->lookup_value(val);
    out << val << ' ' << name;
 }  else  
 if (is_bool()) out << (*(int *)ptr ? "true" : "false"); else
 if (is_int()) {
     bool unsign = is_unsigned();
     if(is_char()) {
        out << '\'' << *(char *)ptr << '\'';
        if (unsign) out << " (" << (unsigned_t)*(unsigned char *)ptr << ')';
     } else if(is_short()) {
        if (unsign) out << (unsigned_t)*(unsigned short *)ptr;
        else        out << *(short *)ptr;
     } else {
       if (unsign)  out << (unsigned_t)*(unsigned int *)ptr;
       else         out << *(int *)ptr;
     }
 } else 
 if (is_double()) out << *(double *)ptr;   else
 if (is_float()) out << *(float *)ptr; else
 if (is_class()) {
   PClass pc = as_class();
   // HACK02 -- strings outputed directly - assume first member field is char *
   if (pc->name()=="string") {
	   if (do_quotes) out << '\'';
	   out << **(char ***)ptr;
	   if (do_quotes) out << '\'';
   } else out << pc->name() << " {}"; 
 }
 out << ends;
 return buff;
}

ostream& operator << (ostream& os, TypeDistance td)
 {
   char *txt;
   switch(td) {
   #define CASE(x) case x: txt = #x; break;
   CASE(EXACT_MATCH)
   CASE(REFERENCE_MATCH)
   CASE(TRIVIAL_MATCH)
   CASE(PROMOTE_MATCH)
   CASE(STD_MATCH)
   CASE(CONVERT_FROM_MATCH)
   CASE(CONVERT_TO_MATCH)
   CASE(FUN_PTR_MATCH)
   CASE(NO_MATCH)
   #undef CASE
   }
   os << txt;
   return os;
 }

  



