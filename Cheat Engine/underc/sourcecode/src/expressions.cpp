/* Expressions.cpp
 * Code to manipulate expressions - the type PExpr
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 *
 * Expr is a fairly basic type, consisting of an operation type (e.g. FUNCTION)
 * and up to two operands. C++ expressions are represented by binary trees built out of
 * Expr pointers.
 */

#include "expressions.h"
#include "common.h"
#include "function_match.h"
#include "tparser.h"
#include "operators.h"
#include "directcall.h"
#include <assert.h>
#include <map>
typedef std::map<int,int> IntMap;

extern Table* gScopeContext; // from common.cpp (yes, a hack)

// found in type.cpp (of all places!)
 Signature *unique_signature(Signature *sig);
// found in common.cpp
string as_str(Type t);

namespace {
  IntMap assign_equiv_op, float_op;
};

bool
Expr::is_function()
{
 return m_op == FUNCTION || m_op == METHOD_CALL || m_op == DCALL;
}

string 
Expr::name()
{
 if (is_entry()) {
    if (!entry()->is_constant()) return entry()->name;
    if (!entry()->type.is_int()) return "?";
    return itos(Parser::const_int_expr(entry()));
 } else
 if (is_function()) return function()->name(); else
 if (is_expr()) return "(...)"; 
 else { // we assume it's a specific operator!
   return Operators::name_from_id(op());
 }
}

bool
Expr::is_variable()
{
 return is_entry() && type().is_reference() && ! entry()->type.is_reference();
}

bool   Expr::is_expr_list()
{ 
 return is_function() && function()==NULL;
}

bool  Expr::is_brace_list()
{ 
  return is_expr_list() && m_type == t_int;
}

namespace Expressions {

// *add 1.2.3 A mechanism for defering function errors when looking at operators
FunctionEntry* mErrPfe;
string mErrMatch; 

PExpr expr_error(string msg); // forward

void set_function_error(FunctionEntry* pfe, string str)
{
    mErrPfe = pfe;
    mErrMatch = str;
}

PExpr function_error()
{
    if (mErrPfe) {
       cerr << "Candidates were\n";
       mErrPfe->dump(cerr);
    }
    PExpr e = expr_error(mErrMatch);
    mErrPfe = NULL;
    mErrMatch = "";
    return e;
}

PExpr function_call(const char *name, PExprList pel)
{
 Function *fn = Function::lookup(name);
 return new Expr(FUNCTION,fn->return_type(),fn,pel);
}

PExpr bcast_op(Type t, PExpr e)
{
 if (t == e->type()) return e;
 else {
  if (e->type().is_plain_reference()) t.make_reference();   // *fix 0.9.7 made var casts to be references!
  return new Expr(BCAST,t,e);
 }  
}

bool is_lvalue(Type t)
{
 return t.is_reference() && !t.is_const();  
}

Type type_of(PExpr e)
{
// this peculiar logic is the inverse of that in entry_op() below!
 if (e->is_entry()) return e->entry()->type;
 else return e->type();
}

// *add 0.9.3 The (non-standard) typeof operator is still experimental. 
// So far, I have to strip reference types to make __typeof(*p)
// work properly.
Type typeof_op(PExpr e)
{
 Type t = type_of(e);
 t.strip_reference();
 return t;
}

bool is_true_reference(PExpr e)
{
 return type_of(e).is_reference();
}

PExpr nil() 
{
 return new Expr(NIL,t_void);
}

PExpr expr_error(string msg)
{
 error(msg);
 return nil();
}

string quoted_op_name(int op) { return Parser::quotes(Operators::name_from_id(op)); }

PExpr not_for_float(int op)
{
 return expr_error(quoted_op_name(op)+" does not take floating point arguments");
}

PExprList expr_list(PExpr e1, PExpr e2)
{
  PExprList pel = new ExprList;
  if (e1) pel->push_back(e1);
  if (e2) pel->push_back(e2);
  return pel;
}

const int NTYPES = 9;

PExpr pointer_addition_op(int op, PExpr e1, PExpr e2)
{
 if (op == MINUS) e2 = make_op(UMINUS,e2->type(),e2);
 return make_op(ARRAY,e1->type(),e1,e2);
}

// here are the functions which the parser uses to generate expressions
PExpr arith_op(int op, PExpr e1, PExpr e2)
{
  static Type types[NTYPES]
   = {t_double,t_ulong,t_long, t_uint,t_int,t_ushort,t_short,t_uchar,t_char};   
  Type t1 = e1->type(), t2 = e2->type();
  t1.strip_qualifiers();
  t2.strip_qualifiers();
  if (t1.is_number() && t2.is_number()) { 
   if (t1 == t_float) { e1 = bcast_op(t_double,e1); t1 = t_double; }
   if (t2 == t_float) { e2 = bcast_op(t_double,e2); t2 = t_double; }
   for(int i = 0; i < NTYPES; i++) { 
    Type tt = types[i];
    if (t1 == tt || t2 == tt) {
      if (tt == t_double && float_op[op] == 0) return not_for_float(op);
      if (t1 != tt) e1 = bcast_op(tt,e1);
      if (t2 != tt) e2 = bcast_op(tt,e2);
      return make_op(op,tt,e1,e2);
    }
   }
 } else
  if (op == PLUS || op == MINUS) {
     // *add 0.9.3 Subtracting two pointers should give the number of elements...
     // *fix 1.2.1 Wrong! p1-p2 should be equiv. to (int)p1 - (int)p2; also, it was wrong for int types!
     // *fix 1.2.4 Sorry, wrong again!  I was right for 0.9.3.
      if (t1.is_pointer() && t2.is_pointer()) {
          if (op == MINUS) {
              t1.decr_pointer();
              PExpr ae = make_op(MINUS,t_int,e1,e2);
              return make_op(DIVIDE,t_int,ae,sizeof_op(t1.size()));
          } 
          else return expr_error("Cannot add pointers");
      }

     if (t1.is_pointer() && t2.is_int()) return pointer_addition_op(op,e1,e2); else
     if (t2.is_pointer() && t1.is_int()) return pointer_addition_op(op,e2,e1); 
  }

  // otherwise, try for user-defined operator!  
  return bin_op(op,e1,e2);
}

PExpr dot_function_op(void *pfn, PExpr obj)
{
  return make_op(DOT,obj->type(),obj,entry_op((PEntry)pfn));
}

PExpr method_op(PEntry pef, PExpr obj, PExprList pel)
{
// *NOTE* are we having returning-object problems w/ this one?
// used for bin_op() methods, also operator().
 return function_op(dot_function_op(pef,obj),pel);
}   

PExpr construct_context_op(PExpr e1, PExpr e2=NULL, int ctype=0); // forward
PEntry add_temporary(Type t, PExpr e, bool always_construct, bool dont_do_dtor = false);  // forward
void set_first_object(PExpr e1);  // forward;
PExpr init_op(PExpr el, int arr_sz, PExpr er, PExprList pel, int ctype); // forward;

void fn_return_object(Type rt, PExprList args)
{
 // Functions returning objects are passed a temporary reference; 
 // if it's an object requiring destruction, we also push the ODS!
   PExpr et = entry_op(add_temporary(rt,NULL,false));
   if (rt.as_class()->destructor()) et = make_op(FCONTEXT,rt,et,0);
   args->push_front(et);	
}

// *add 1.2.6 true return-by-value requires a different coding;
PExpr fn_return_object_as_value(Type rt, PExpr er)
{
   PExpr et = entry_op(add_temporary(rt,NULL,false));
   PExpr ret_obj = Parser::get_object_return_expr();
   Type init_rt = rt;
//   init_rt.make_reference();
//   init_rt.make_const();
   ret_obj->entry()->type = init_rt;
   ret_obj->set_type(init_rt);
   return append_op(er,                              // the function expresssion
           append_op(init_op(et,1,ret_obj,NULL,1),   // initializing the temporary
		   et,true)                                      // pushing the temporary
	   );
}

PExpr method_call(PFunction fn, PExpr obj, PExprList pel)
// note: will mess w/ pel!!
{
  Type rt = fn->return_type();
  if (!pel) pel = new ExprList;  /// empty arg list
  if (rt.is_object()) fn_return_object(rt,pel);
  pel->push_back(obj);
  return make_op(METHOD_CALL,rt,(PExpr)fn,(PExpr)pel);
}

typedef bool (TypeCheck)(Type t);

bool to_number(Type t)  { return t.is_number() ; }
bool to_pointer(Type t) { return t.is_pointer(); }

static Type mType;

bool to_specified_type(Type t)
{
  return match(mType,t) != NO_MATCH;
}   

PExpr try_convert(PExpr e, TypeCheck type_check)
{
  if (!e) return NULL;
  Type t = e->type();  
  if (type_check(t)) return e;
  if (t.is_class()) { 
    TypeList tl;
    PClass pc = t.as_class();
    if (pc->get_to_type_list(tl)) {
       TypeList::iterator tli;
       for (tli = tl.begin(); tli != tl.end(); ++tli)
         if (type_check(*tli)) { 
           PFunction pf = pc->get_conversion_to(*tli);
           return method_call(pf,e,NULL);
         }
     }
   }
   return NULL;
}

PExpr try_convert_to(PExpr e, Type t)
{
   mType = t;
   return try_convert(e,to_specified_type);
}

bool is_assign_op(int op)  // note: dependent on these values being continguous.
{
 return op >= ASSIGN && op <= XOR_A;
}

bool is_object(PExpr e)
// note: Type::is_object() fails for class _references_.
// Not sure if that is sensible, but here goes...
{ 
 if (e==NULL) return false;
 Type t = e->type();
 return t.is_class() && !t.is_pointer();
}

string type_as_str(PExpr e)
{
  return as_str(e->type());
}

PExpr cannot_convert_error(PExpr e1, PExpr e2)
{
 return expr_error("cannot convert " + type_as_str(e2) + " to " + type_as_str(e1));
}

PExpr bin_op(int op, PExpr e1, PExpr e2)
{
 string name = Operators::name_from_id(op);
 PEntry pe = NULL;
 bool class_args = is_object(e1) || is_object(e2);
 if (class_args) { 
   // first see if it's a method of an object
   if (is_object(e1)) pe = e1->type().as_class()->lookup(name);
   // then if it's global (or at least injected into global)
   if(!pe) pe = Parser::global().lookup(name);
   if (pe && Parser::is_class(pe->context)) { // was a method operator
     PExpr er = method_op(pe,e1,expr_list(e2));
     return er;
   }
 }
 if (pe) { // plain old operator - treat as function!  
 // *fix 1.1.0 If we can't match the bin op, pass thru and try conversions...
    PExpr efn = function_op(entry_op(pe),expr_list(e1,e2),true);
	if (efn) return efn;
 }
  
 // no entry was found, or no match possible; 
 if (is_assign_op(op)) { // *fix 0.9.6 try for assignment conversions 
   Type t = e1->type();
   PExpr ce = try_convert_to(e2,t);
   if (ce) {
     if (op == ASSIGN) return make_op(ASSIGN,t,e1,ce);
                 else  return compound_assign_op(op,e1,ce);
   } else return cannot_convert_error(e1,e2); 
 } else
 if (op != COMMA) { // try for numerical/pointer conversions
   PExpr ce1,ce2;
   ce1 = try_convert(e1,to_number);
   ce2 = try_convert(e2,to_number);
   if (!e2 && ce1) return make_op(op,ce1->type(),ce1,NULL);  // unary case!
   else if (ce1 && ce2) return arith_op(op,ce1,ce2);

   if (op == PLUS || op == MINUS) { // pointer arithmetric
     if (ce2) ce1 = try_convert(e1,to_pointer);
     if (ce1) ce2 = try_convert(e2,to_pointer);
     if (ce1 && ce2) return arith_op(op,ce1,ce2);
   }
 }
 if (class_args) {
  // *hack 0.9.7 list<> needs op== at the mo, whether or not it's used.
  // This seems arbitrary but is nicer than shoving a spurious op== into every class!
  // *add 1.2.3  Now give a more informative error message
     if (op == EQUAL) return constant_op(t_int,0);
     else return function_error(); 
 }
 else return make_op(op,e1->type(),e1,e2);
}

PExpr append_op(PExpr e1, PExpr e2, bool drop_values)
{ 
  return make_op(drop_values ? COMMA : APPEND, e1->type(),e1,e2); 
}

void set_first_object(PExpr e1)
{
   if (e1->is_entry() && e1->entry()->is_stack_relative()) {
	  // *fix 1.0.0 The first object on an auto var frame is the first one put on the ODS... 
      e1->entry()->context->first_obj(e1->entry()); 
   }
}

PExpr construct_context_op(PExpr e1, PExpr e2, int ctype)
{
 Type t;
 // default is a normal auto var, ctype==1 means a temporary
 if (ctype == 0) t = t_void; else t = t_int;

 return make_op(CCONTEXT,t,e2,e1); //*NOTE* order has switched for DOT compat!
}

PExpr dynamic_context_op(PExpr efn)
{
  return make_op(DCONTEXT,t_void,efn,NULL);
}

PExpr vector_context_op(PExpr efn, PExpr e = NULL)
{
  if (e) set_first_object(e);
  return make_op(VCONTEXT,t_void,e,efn);
}

PExpr add_const_op(Type t, int val, PExpr e)
{
 PExpr ec = constant_op(t_int,val);
 return make_op(PLUS,t,e,ec); 
}

PExpr delete_op(PExpr e1, bool is_arr)
{
// destructing the object, if necessary, and freeing the memory
// the pointer includes the VMT entry, if any, which means subtracting
// a word to get the actual alloc pointer.
// *change 1.1.0 This is handled by the builtin delete_ex, unless delete is overloaded.
 Type t = e1->type();
 t.decr_pointer();          
 PClass pc = t.is_class() ? t.as_class() : NULL;
 bool overalloc = Parser::debug.do_overalloc || (pc != NULL ? pc->has_true_VMT() : false);
 bool overloaded = Function::lookup("delete") != NULL;
 const char *delete_fn = overloaded ? "delete" : (overalloc ? "_delete_ex" : "_delete"); 
 PExpr er,ed=NULL,ez = constant_op(t_int,t.size());  
 if (pc != NULL) {
	if (pc->destructor()) {
      PEntry pe = pc->lookup(pc->destructor_name());
      ed = function_op(entry_op(pe),expr_list());
	  if (is_arr) ed = vector_context_op(ed);
	         else ed = dynamic_context_op(ed);  
      if (overloaded && overalloc) ed = add_const_op(t,-sizeof(void *),ed);
      ed = append_op(e1,ed);
    } else ed = e1;
    er = function_call(delete_fn,expr_list(ed,ez));
 } else
 er = function_call(delete_fn,expr_list(e1,ez));
 return er;
}

PExpr construct_op(PClass pc, PExprList pel, bool check_abstract)
{
 if (pc->is_abstract() && check_abstract) return expr_error("class contains abstract methods");
// call the constructor, passing the arguments
 PEntry pec = pc->get_constructor();
 if (pec==NULL) return expr_error("cannot create constructor");
 return function_op(entry_op(pec),pel);
}

PExpr init_ref_op(PExpr el, PExpr er)
{
// NOTE(4) we are fed a _reference entry expression_, which is (* e)
 return new Expr(INIT_REF,t_void,el->arg1(),er);
}

static PExpr array_init_op(PEntry pe, PExprList pel)
{
	 ExprList::iterator eli = pel->begin();
	 PExpr arr_expr, assign_expr;
	 int mem_unit = (pe->is_stack_relative()) ? sizeof(int) : 1; // stack-relative counts in words...
	 for(int i = 0; eli != pel->end(); ++eli,++i) {
		 PEntry aie = new Entry;  *aie = *pe;	   // construct an Array Item Entry
		 aie->type.decr_pointer();				   // using base type of array
		 aie->size = 1; 						   // (which is not an array!)
		 aie->data += (i*aie->type.size())/mem_unit;
		 assign_expr = initialize_op(aie,*eli,NULL,0);	
		 if (i > 0) arr_expr = append_op(arr_expr,assign_expr,true); // i.e normal COMMA op!
		 else arr_expr = assign_expr;
	 }
	 return arr_expr;
}

static PExpr dot_op(PExpr obj, PEntry pe)
{
  PExpr ef = entry_op(pe);
  return make_op(DOT,ef->type(),obj,ef); 
}


static PExpr struct_init_op(PExpr obj, PClass pc, PExprList pel)
{
	// *add 1.1.1 Explicit initialization of structs.
	// *fix 1.1.2 Handling of array fields & simplification.
   EntryList fields;
   EntryList::iterator ei;
   ExprList::iterator eli;
   PExpr init_expr, assign_expr;
   pc->list_entries(fields,FIELDS | NON_STATIC);
   if (fields.size() != pel->size()) return expr_error("init list wrong size for this struct");
   for(ei = fields.begin(), eli = pel->begin(); ei != fields.end(); ++ei, ++eli) {
      assign_expr = init_op(entry_op(*ei),Parser::array_size(*ei),*eli,NULL,0); 
	  if (assign_expr->is_nil()) return assign_expr;  // couldn't assign to this entry!
	  if (ei != fields.begin()) init_expr = append_op(init_expr,assign_expr,true);
	  else init_expr = assign_expr;
   }
   return make_op(DOT,t_void,obj,init_expr);
}


PExpr init_op(PExpr el, int arr_sz, PExpr er, PExprList pel, int ctype)
{
// initializing an auto object, if necessary calling the constructor
// Only one of er and pel is non-NULL! 
  Type t = el->type();
  bool brace_init = false;
  if (er && er->is_expr_list()) {
       pel = er->expr_list();
	   brace_init = er->is_brace_list();
       er = NULL;
  }
  bool is_ref =  t.is_plain_reference();
  bool is_ref_or_ptr = is_ref && t.is_pointer();
  bool is_array = arr_sz > 1;
  bool is_non_init_array = is_array && pel == NULL;
  // *fix 0.9.4 Now does both initialized and non-initialized arrays of objects.
  if (t.is_class() && !is_ref && (is_non_init_array || !t.is_pointer())) { 
     PClass pc = t.as_class(); 
	 if (brace_init) return struct_init_op(el,pc,pel); // initialized struct!
     if (t.as_class()->has_constructors()) { // an object or an uninitialized array of objects...          
       if (er) pel = expr_list(er);    // object initialized w/ assign syntax
       if (!pel) pel = expr_list();    // i.e, call default constructor!
       PExpr ec = construct_op(pc,pel);
       if (ec->is_nil()) {
	     if (pel->size()==0) return expr_error("no default constructor");
	     return expr_error("cannot find constructor");
	   }
	   // either object (initialized or not), or uninitialized array.
       if (! is_non_init_array) return construct_context_op(ec, el, ctype);
       else  return vector_context_op(ec, el);
     } else
      // *fix 1.2.6 plain simple structs are just initialized with a copy operation
       if (er != NULL) return make_op(COPY_BLOCK,t,el,er);   
       else return NULL;  // no operation..
  } else { // reference or simple initialization
	 if (pel) er = pel->front();      // scalar init. w/ object syntax
	 if (!er) return expr_error("bad init");
	 if (is_ref) return init_ref_op(el,er);       // init. reference
	 else if (is_array) {
// *add 1.1.2 special case 'char p[] = "hello"'
// *fix 1.1.4 pointer test because 'char *a[] = {"one","two"}' was broken
// *add 1.2.0L special case implemented
         if (t.is_char() && t.pointer_depth()==1  && er) { 
		   return function_call("strcpy",expr_list(el,er));	   
         } else
		 return array_init_op(el->entry(),pel); // initialized array of scalars, or objects.
     }   
     else return assign_op(el,er,false);          // simple initialization
 }
}


// *change 1.2.9 When compiling in proper 'program mode', then integer
// constant expressions are directly evaluated.  If we can't fold the constant,
// then the usual defered evaluation is carried out.  We do this to
// support arrays being sized by constants.
PExpr initialize_op(PEntry pe, PExpr er, PExprList pel, int ctype)
{
  Type t = pe->type;
  if (Parser::debug.compile_program && pe->is_direct()
        && ! t.is_pointer() && t.is_int() && t.is_const()) {
      int val;
      bool succeeded = true;
      try {
         val = Parser::const_int_expr(er);
      } catch(string msg) {
       // it wasn't a simple constant expression; pass through 
       // force it to be zero, however, so it will cause a clean error if
       // if used to declare an array.
          val = 0;
          succeeded = false;
      }
      *(int*)(pe->global_ptr()) = val;
      if (succeeded) return NULL;
  } 
  return init_op(entry_op(pe), Parser::array_size(pe),er,pel,ctype);
}

PExpr expr_list_op(PExprList pel, bool is_list)
{ 
   PExpr ex = function_op(NULL,pel);
   if (! is_list) ex->set_type(t_int);
   return ex;
}

void add_to_arg_list(PExpr ec, PExpr arg)
{
  PExprList pel = (PExprList) ec->arg2();  
  pel->push_front(arg);
}

Type base_type(Type t)
{
  t.decr_pointer();
  return t;
}

PExpr new_op(Type t, PExpr e, PExprList pel)
{
// e represents the (opt.)_number_ of objects, pel the (opt.) _arguments_ to the constructor
// *change 1.1.0 we switch to the overallocating version of new when there's a VMT
// unless the operator was overloaded.
// *fix 1.2.4 Make sure that the base type of t is an object, not itself a pointer.
 PClass pc = t.is_class() ? t.as_class() : NULL;
 bool overalloc = Parser::debug.do_overalloc || (pc != NULL ? pc->has_true_VMT() : false);    
 bool is_scalar = e == NULL;
 // *NOTE* Do we do new[] yet?
 bool overload = Function::lookup(is_scalar ? "new" : "new[]") != NULL;
 const char *new_fn;
 if (is_scalar) new_fn =  overload ? "new" :   (overalloc ? "_new_ex" : "_new");
          else  new_fn =  overload ? "new[]" : (overalloc ? "_new_vect_ex" : "_new_vect");
 PExpr er, ez = constant_op(t_int,t.size());
 t.incr_pointer();
 // allocating the memory
 if (is_scalar) er = function_call(new_fn,expr_list(ez));
           else er = function_call(new_fn,expr_list(e,ez));

 // a result of T* needs construction if T::T() exists;  but T** etc doesn't!
 if (pc && t.pointer_depth() == 1 && pc->has_constructors()) {
    if (!pel) pel = expr_list();     
    PExpr ector = construct_op(pc,pel);  
    if (ector->is_nil()) return ector;
	PExpr ec;
	// wrap construction in its object context	
	if (is_scalar) ec = dynamic_context_op(ector);
	          else ec = vector_context_op(ector);
    // NB to skip first word if we're putting a VMT there!
    if (overalloc && overload) er = add_const_op(t,sizeof(void *),er);

    add_to_arg_list(ector,er);
	er = ec;
    //er = append_op(er,ec);	
 } else if (pel && pel->size() == 1){ 
  // *fix 1.1.0 a plain scalar initialization - make sure it's typecast properly!
   er = append_op(er,assign_op(NULL,typecast_op(STATIC_CAST,base_type(t),pel->front())));
 }
 er->set_type(t); // i.e. make it T *
 return er;
}

bool is_object(Type t)
{ return t.is_class() && !t.is_pointer(); }  //*NOTE* Type::is_object 

PExpr assign_op(PExpr e1, PExpr e2, bool constness)
{
 if (!e1) return make_op(ASSIGN,e2->type(),NULL,e2); // see above...only called from there?
 Type t = type_of(e1);
 // *hack 1.2.8 things like 'const char*' fox UC because it can't understand the
 // difference between a pointer which is const, and a pointer to const data.
 if (constness && t.is_const() && ! t.is_pointer())
     return expr_error("Can't assign to a const type");
 // *fix 0.9.6 We didn't check to see if the _RHS_ was an object..
 if (is_object(t) || is_object(e2->type())) {
   if (is_object(t) && t.as_class()->simple_struct()) return make_op(COPY_BLOCK,t,e1,e2);
   else return bin_op(ASSIGN,e1,e2);
 } else {
    e2 = typecast_op(STATIC_CAST,t,e2);
    return make_op(ASSIGN,t,e1,e2);
 }
}

PExpr convert_type_op(Type t, PExpr e, bool try_conversions_from = false); // forward

PExpr cast_to_bool_op(PExpr e1)
{
 Type t = e1->type();
 // it's necessary to strip the reference before is_object() works;
 // I'm sure this is going to cause grief elsewhere!
 t.strip_reference(); 
 PExpr ce; // *fix 0.9.5 We fell over when an object didn't have a conversion....
 if (t.is_object() && (ce = convert_type_op(t_int,e1)) != NULL) return ce;
 else return e1;    
}

PExpr compound_assign_op(int op, PExpr e1, PExpr e2)
{
 if (is_object(e1->type()) || is_object(e2->type()) )
    return bin_op(op,e1,e2);
 else return assign_op(e1,arith_op(assign_equiv_op[op],e1,e2));
}

PEntry add_temporary(Type t, PExpr e, bool always_construct, bool dont_call_dtor)
{
using namespace Parser;
  PEntry pe; 
  state.check_context(0);
  // *fix 0.9.6 Nested temporary contexts require special attention.
  Table *context = &state.context();
  if (context == temp_context()) context = temp_context()->parent_context();
  // *fix 0.9.4 If a temporary is the first object in a nested fn context,
  //  then we need to force this context to be available.
  //temp_context()->reserved_space(t.size());
  temp_context()->set_parent(context);
  state.push_context(temp_context());
  temp_context()->set_no_auto_dtor(dont_call_dtor);  
  pe = state.add_variable(t,"*",e,always_construct ? TEMP : DEFER_TEMP);
  temp_context()->set_no_auto_dtor(false);  
  state.pop_context();
  return pe;
}

// *fix 1.2.8 must guarantee that the temporary is initialized as close
// to the point of use; this expression ensures that ctors for temp objects
// are constructed next to the argument push value.
PExpr construct_temporary_op(Type t, PExpr e, bool dont_call_dtor=false)
{
 //* return entry_op(add_temporary(t,e,true,dont_call_dtor));
  // create a temporary entry, but don't initialize it.
  PExpr ee = entry_op(add_temporary(t,NULL,false,dont_call_dtor));
  PExpr ec = init_op(ee,1,e,NULL,1);
  // this is a comma expression which evaluates the initialization code first;
  // but explicitly use the temporary's type!
  PExpr ea;
  if (ec) ea = append_op(ec,ee,true);
  else ea = ee;
  ea->set_type(ee->type());
  return ea;
}

PExpr force_addr_op(Type tt, PExpr e);  // forward...
 
  // *note* in general this is the _initialization logic_
PExpr return_op(Function *fn, Type rt, PExpr e)
{
   if(rt.is_reference()) return force_addr_op(rt,e);  // force a reference conversion
   else if (rt.is_object()) {
    // basically, force a copy construction!
    PExpr re = entry_op(fn->return_object());
    if (rt.as_class()->has_constructors()) {
       PExpr ctor = construct_op(rt.as_class(),expr_list(e));
       if (ctor->is_nil()) return ctor;
       else return construct_context_op(ctor,re);
    } else return assign_op(re,e,false);
   } else
   return typecast_op(STATIC_CAST,rt,e);
}

// These guys are very similar by the array/pointer equivalence;
// arrays are implemented as a dereference of a pointer addition.
// note again that the result is an implicit reference!
PExpr deref_op(PExpr e1, bool do_overload)
{
 Type t = e1->type();
 if (t.is_pointer()) {
  // try to reduce (* (& x)) to x
    if (e1->op()==ADDR) return e1->arg1();
    t.decr_pointer();
    t.strip_array();    // *fix 0.9.7 damn array flag caused trouble
    t.make_reference();
    return make_op(DEREF,t,e1);
 } else
 if (do_overload) return unary_op(DEREF,e1);
 else return expr_error("Was not a pointer");
}

PExpr array_op(PExpr e1, PExpr e2)
{
 if (e1->type().is_pointer()) {
   return deref_op(make_op(ARRAY,e1->type(),e1,e2));
 } else
 return bin_op(ARRAY,e1,e2);
}

// *fix 0.9.5 Non-reference values can be passed as references via a _reference stub_
// (this copies the value into a temporary static var, and pushes the addr of that)
// *fix 0.9.7 The target type can be of a different size, so we need it explicitly!
PExpr reference_stub_op(Type tt, PExpr e)
{
 tt.strip_qualifiers();
 PExpr temp = entry_op(Parser::state.add_variable(tt,"*",NULL,Static));
 return make_op(REF_STUB,tt,e,temp);
}

PExpr addr_op(PExpr e1, bool do_overload, bool force_reference /* = false */)
{
// tricky one this - not clear how to unambiguously distinguish the ordinary
// old case!
 Type t = e1->type();
 if (t.is_reference() || e1->is_entry()) { // works also for _literal constants_
	 if (e1->op()==DEREF) { // we reduce (& (* x)) to x
       PExpr e = e1->arg1();
// NOTE(4) a reference x is represented by (* x)
// *fix 1.1.2 This typecast is necessary even if we're not an entry!
       if (t.is_reference() /*&& e->is_entry()*/) {
         t.incr_pointer();   // so ensure that we do get a pointer
         e = bcast_op(t,e);  // force a new entry w/ this type
       }  
       return e;
    }
    // *fix 1.2.1 Ensure that function pointer signatures are unique
    if (t.is_function()) t = Type(unique_signature(t.as_signature()));
    t.incr_pointer();
    return make_op(ADDR,t,e1);
 } else 
 if (do_overload) return unary_op(ADDR,e1);
 else return NULL;  /// expr_error("Not an lvalue"); *now caller is responsible*
}

PExpr force_addr_op(Type tt, PExpr e)
{
// This is ONLY called for reference conversions - so tt is always a reference....
 PExpr ex;
 if (e->op()==ADDR || e->type().size() != tt.size()) return reference_stub_op(tt,e);
 ex = addr_op(e,false);
 if (ex==NULL)   // i.e we got a 'Not an lvalue' error message!
    ex = reference_stub_op(tt,e);
 return ex;
}

Type t_const_int; // initialized in init();

PExpr unary_op(int op, PExpr e1)
{
// A special case: -<number>
// *NOTE* 1st Oct 00: let's think about this, because it would be
// a good easier to get numbers in negative right from the start;
// this JUST does integers
 if(op==UMINUS && e1->is_entry() && e1->type()==t_const_int) {
    int *data = (int *)Parser::ExprToPtr(e1);
    *data = - *data;
    return e1;
 } else
 return bin_op(op,e1,NULL);
}

PExpr arith_if_op(PExpr cond, PExpr e1, PExpr e2)
{
 PExpr rest = arith_op(':',e1,e2);
 return make_op(ARITH_IF,rest->type(),cond,rest);
}

PExpr sizeof_op(int type_size)
// *Fix 0.9.2 No longer takes a type argument;  currently
// there's no array size information in Type, so I had
// to ask the entry...
{
 return constant_op(t_int,type_size);
}

string t2s(Type t)
{
 string ts;
 t.as_string(ts);
 return "'" + ts + "'";
}

bool type_has_VMT(Type t)
{ 
 return t.is_class() ? t.as_class()->has_VMT() : false;
}

PExpr typecast_op(int which, Type t, PExpr e)
{
 Type te = type_of(e);
 if (which == DYNAMIC_CAST) { // *add 0.9.5 dynamic cast
   if (! t.is_class() && t != t_void_ptr)  return expr_error("dynamic_cast: must be class or void *");
   if (!type_has_VMT(t) || !type_has_VMT(e->type())) return expr_error("dynamic_cast: class must have virtual methods");
   return make_op(DYNACAST,t,e,NULL);
 } else {
   TypeDistance td = match(t,te);
   if (td == NO_MATCH && (which==STATIC_CAST || which==REINTERPRET_CAST)) {  //*SJD* 23/09/00 TEMP
    // *fix 0.9.8 try user-defined conversions in typecast
	// I'm not sure about the REINTERPRET_CAST; trying to distinguish from STATIC_CAST!
      PExpr et = convert_type_op(t,e,which == REINTERPRET_CAST);
      if (et != NULL) return et;
   }
   return bcast_op(t,e);
 }
}

// *fix 1.2.8 It's necessary to look at the 'no args' case specially.
PExpr function_cast_op(Type t, PExprList pel)
{
 if (t.is_class() && !t.is_ref_or_ptr()) { 
     return construct_temporary_op(t,pel->size() > 0 ? function_op(NULL,pel) : NULL);   
 } else {
  if (pel->size() != 1) return expr_error("typecast needs one argument");
  return typecast_op(STATIC_CAST,t,pel->front());
 }
}

PExpr must_be_lvalue(int op)
{ return expr_error(Operators::name_from_id(op) + ": must be lvalue"); }

PExpr inc_dec_op(int op, PExpr e, bool is_postfix)
{
 Type t = e->type();
 // C++ convention for indicating whether ++ is post or prefix: a dummy parm!
 // (cd easily be constant, but what the hell...
 PExpr extra = is_postfix ? constant_op(t_int,0) : NULL;
 if (t.is_reference()) { // NOTE(3) hack!!
   // *hack 1.2.9a  'const T*' is a pointer to const _data_ (working around UC const limitation)
   if (t.is_const() && ! t.is_pointer()) return must_be_lvalue(op);
   else if (t.is_pointer()) { //*a special case*
    return make_op(op==INCR ? INCR_PTR : DECR_PTR,t,e,extra);
   } else if (t.is_int()){
     // plain ++/-- acting on variables 
     if (e->is_variable()) return make_op(op,t,e,extra); 
     // in the general case, we take the address of the argument & use it
     else {
       PExpr re = addr_op(e,false); 
       if (re == NULL) return must_be_lvalue(op);
       return make_op(op,t,re,extra);      
     }
   } else if (t.is_float()) return not_for_float(op);  //*NOTE* this isn't true!
     //else return expr_error("Que?");
 } 
 //else
  if (t.is_class()) return bin_op(op,e,extra);
 else return must_be_lvalue(op);
}


PExpr equal_op(PExpr e1, PExpr e2)
{
 return relational_op(EQUAL,e1,e2);
}

PExpr relational_op(int op, PExpr e1, PExpr e2)
{
 PExpr e;
 if (e2) e = arith_op(op,e1,e2);
 else e = unary_op(op,e1);
 // *fix 1.1.0 builtin relational ops return bool (but can be arbitrarily overloaded)
 if (e->op() == op) e->set_type(t_bool);
 return e;
}

PEntry ExprToEntry(PExpr e)
{
 return  e->entry();
}

PExpr convert_function_ptr(PFunction pf, PExpr e)
// pf is the function we're passing this fn ptr!
// *add 1.1.4: functions passed to builtin routines can be true function pointers!
{
  if (pf->builtin()) { // we must pass a native routine to such beasts!
    void *pfn;
    if (e->op()==ADDR || e->op()==BCAST) e = e->arg1();
    if (e->is_entry() && e->type().is_function()) {
      PEntry pe = e->entry();
      PFunction fun_ptr = PFunctionEntry(pe->data)->back();
      if (fun_ptr->builtin()) { // extract the addr of wrapped native routine!
        pfn = fun_ptr->fun_block()->native_addr();      
      } else { // build native code stub for this function!
        pfn = Builtin::generate_native_stub(fun_ptr);
      }
      return constant_op(t_void_ptr,(long)pfn);
    } else { 
     // a function ptr expression requires dynamic stub generation
		return function_call("_native_stub",expr_list(e));
    }
  } else return e;
}

void generate_type_list(PExprList args, TypeList& tl)
{
 ExprList::iterator eli;
 for (eli = args->begin(); eli != args->end(); ++eli)
      tl.push_back((*eli)->type());
}

PExpr convert_type_op(Type t, PExpr e, bool try_conversions_from)
{
// *fix 1.1.3 will also try to use ctors to construct a type!
    Function *pf;
  // first try user-defined conversions, if the expression is an object
  // *fix 1.2.2 This was kicking in for pointers to objects as well!
    Type et = e->type();
    if (et.is_class() && ! et.is_pointer()) {	
       pf = et.as_class()->get_conversion_to(t);
       if (pf != NULL) return method_call(pf,e,NULL); 
	} 
	// optionally try to convert the expression using a ctor
	if (try_conversions_from && t.is_class()) {
	    pf = t.as_class()->get_conversion_from(et);
		if (pf != NULL) return construct_temporary_op(t,e);
    } 
	return NULL;  // no conversions possible
}

int min(int a, int b) { return a > b ? b : a; }


bool was_plain(int match)
{ 
 return match == EXACT_MATCH || match == TRIVIAL_MATCH || match == PROMOTE_MATCH || match == STD_MATCH;
}

PExpr this_ref()
{
  PEntry pe = new Entry();
  pe->data = THIS_OFFSET;
  return entry_op(pe);
}

PExpr pass_by_value(Type t,PExpr e)
{
   PExpr ec = construct_op(t.as_class(),expr_list(e)); 
   return make_op(PASS_BY_VALUE,t,ec);
}

PExpr function_op(PExpr e, PExprList args, bool suppress_error)
{
 PExpr er;
 FunctionMatch fm;
 PFunction fn;
 // An empty function expression allows expr lists to be expressions
 if (!e) return new Expr(FUNCTION,t_void,NULL,args);
 if(e->op() == NIL) return e;      // error in function expression
 if (!args) args = new ExprList;  /// empty arg list

 
 //Extract the actual function expression (may be a method call!)
 bool was_method = e->op() == DOT;
 PExpr epf = was_method ? e->arg2() : e;

 // If the expression isn't a function, then operator() may be overloaded...
 if (!was_method && !e->type().is_signature()) {
   Type t = e->type();
   if (t.is_class() && !t.is_pointer()) {
      PEntry fpe = e->type().as_class()->lookup("()");
      if (fpe != NULL) return method_op(fpe,e,args);     
   } 
   return expr_error("Not a function");
 }

 // if the function expr is NOT an entry, it's assumed to evaluate to a function!
 bool was_fn_ptr = epf->type().is_pointer() || !epf->is_entry();
 
 PFunctionEntry pfe;
 if (was_fn_ptr) {
 // to match pointers to functions we make up a fake entry!
     PEntry pef = new Entry;
     pef->name = "";
     pfe = new FunctionEntry(pef); 
     pfe->push_back(new Function(epf->type().as_signature(),pfe));
 } else {
     pfe = PFunctionEntry(epf->entry()->data);
 }

 // extract the signature from the list of actual arguments
 Signature *sig = new Signature();
 ExprList::iterator eli;
 ImportScheme *import;
 for (eli = args->begin(); eli != args->end(); ++eli)
      sig->push_back((*eli)->type());

 if (fm.function_match(*pfe,*sig)) { 
    PExprList pdef;
    fn = fm.matched_function();
    // *add 0.9.5 Enforce const method rule *HACK exclude operator[]
//* if (was_method && !fn->is_const() && e->arg1()->type().is_const()
//*    && fn->name() != "[]") 
//*    return expr_error("cannot call a non-const method with a const object");
    //*NOTE* Be careful - shd be fn->is_complete() or something - I will
    // want to optimize this!!
    if (fn == NULL) 
        return expr_error("fn==NULL");

  // add any default arguments....
    pdef = fn->complete_arg_list(args);

// *add 1.1.0 support for pre-constructing objects when passed by value
	import = fn->import_scheme();
	
    MatchList::iterator mli = fm.matches(pdef);
    int i = 0, n_sig = min(fn->signature()->size(),args->size());  // i.e. not for default args..
    for (eli = args->begin(); i < n_sig; ++eli,++mli,++i) { 
       int match = mli->match();
       Type t = mli->type();
       Type et = (*eli)->type();

	   if (import != NULL && t.is_object()) {
	       if(import->true_pass_by_value(t)) {
		      *eli = pass_by_value(t,*eli);
			  continue;
	       } else if(was_plain(match))
		 // *fix 1.2.0 Final 'true' here means that callee is responsible for disposing of object!
		 *eli = construct_temporary_op(t,*eli,true);
       }

       switch(match) { 
        case PROMOTE_MATCH:
        case STD_MATCH:
          *eli = bcast_op(t,*eli);
          break;
        case REFERENCE_MATCH:
        case REF_STD_MATCH:
        case REF_PROMOTE_MATCH:
          if (et.is_variable()) et.strip_qualifiers();  // *hack 0.9.7 trouble w/ is_object()!
          if (! et.is_object())  // objects are always passed by reference anyhow
            *eli = force_addr_op(t,*eli);  // if necesary, force a reference conversion
          break;
        case CONVERT_TO_MATCH:
          *eli = convert_type_op(t,*eli);
          break;
        case CONVERT_FROM_MATCH:
		  t.strip_qualifiers();
          *eli = construct_temporary_op(t,*eli);
          break;
        case FUN_PTR_MATCH:
          *eli = convert_function_ptr(fn,*eli);
          break;
       }
    }
 } 
 else {
 // *add 1.2.3 Suppressed errors (like when we are looking at operators) are saved for later...
    set_function_error(pfe,fm.error());
    if (suppress_error) return NULL;
    else return function_error();
 }

 Type rt = fn->return_type();
 bool returns_object = rt.is_object();
 bool true_value_return = import != NULL && returns_object && import->true_return_by_value(rt); 
 if (returns_object && ! true_value_return) fn_return_object(rt,args);
 
 // *fix 0.9.6 The DCALL function type _forces_ a function to be directly called, virtual or not
 bool plain_fun = true;
 if (gScopeContext != NULL) {
    plain_fun = false;
    gScopeContext = NULL;
 }

 if (was_method) args->push_back(e->arg1());  // append obj ptr to arg list
 if (!was_fn_ptr) { // plain function or method
   er = new Expr(was_method ? METHOD_CALL : (plain_fun ? FUNCTION : DCALL),rt,fn,args);
 } else {          // pointer to function or method
   er = new Expr(was_method ? EXPR_METHOD : EXPR,rt,epf,args);
 }
 // Dereference any returned reference type! (can't use deref_op()!)
 if (rt.is_reference()) er = make_op(DEREF,rt,er);
 if(was_fn_ptr) { // was temporary!
    delete pfe;  
    delete fn;
 } 

 // finally, if it was a class template method then instantiate it (if not already)
 if (!was_fn_ptr && pfe->get_template() && pfe->get_template()->is_method()) 
   if (!pfe->get_template()->instantiate_method(fn)) return expr_error("instantiation failed");
           
 if (returns_object && true_value_return) return fn_return_object_as_value(rt,er);
 else return er;
}

PExpr expr_not_found_error(char *name, PClass pc)
{
 using Parser::quotes;
 return expr_error(quotes(name) + " is not a member of " + quotes(pc->name()));
}

PExpr selection_op(PExpr e, char *name,bool is_ptr,bool is_member_ptr)
{
 Type t = e->type();
 bool was_pointer = t.is_pointer();

 if (!t.is_class()) return expr_error("Not a class object or pointer");
 PClass pc = t.as_class();
 PEntry pe;
 bool not_found;
 if (!is_member_ptr) {
    pe = pc->lookup(name);                 // in the context of the first operand...
    not_found = !pe || !pc->inherits_from((Class *)pe->context);  // and not any enclosing scope...
 } else {
    pe = Parser::symbol_lookup(name);
    not_found = !pe;
 } 

 if (is_ptr && !was_pointer) { // -> may be overloaded...
    PExpr efn = unary_op(ARROW,e);
    if (efn->op() != ARROW) { // was overloaded!
      Type rtype = efn->type();   
      if (!rtype.is_class()) return expr_error("operator-> should return a class");
      return selection_op(efn,name,true);
    }
  }
  // otherwise, it's just a plain selection member operator
  if (not_found) return expr_not_found_error(name,pc);
  if (!was_pointer && is_ptr) return expr_error("Must be a pointer"); else
  if (was_pointer && !is_ptr) return expr_error("Cannot be an object pointer");
  if (was_pointer) e = deref_op(e,false);
  // crucial to use the _expression's_ type, since this will preserve
  // reference information!
  PExpr em = entry_op(pe);
  return make_op(DOT,em->type(),e,em); 
}
//----------------------------------------------------------
PExpr entry_op(PEntry pe)
{
// NOTE(3) identifiers are given an 'variable reference' type
//  (Note: this replaces a scheme where it was actually a reference type! (Algol 68
//   understood this kinda thing better).  We now have an extra type flag to indicate
//   that variable references are Different)
// NOTE(4) a reference x is represented by (* x)
// arrays are _not_variables!
  Type t = pe->type;
  if (!t.is_reference()) {
    if (pe->name != "" && Parser::array_size(pe) == 1)  t.make_variable();
    return new Expr(IREF,t,pe);
  } else
  return make_op(DEREF,t,new Expr(IREF,t,pe));
}

PExpr constant_op(Type t, unsigned long val)
{
 PEntry pe;
 //using Parser::create_const_entry;
 unsigned long *pi = (unsigned long *)Parser::create_const_entry(t,pe);
 *pi = val;
 return entry_op(pe);
}

// *add 1.2.3 Experimental implementation of __lambda
PExpr lambda_op(PEntry)
{
    PEntry pe = Parser::symbol_lookup("_");
 FunctionEntry* pfe = (FunctionEntry*)pe->data;
 // *NOTE* shd dispose of '_'!!
 Function* fn = pfe->back();
 return entry_op(pe);
}

PExpr make_op(int op,Type type, PExpr e1, PExpr e2/* = NULL */)
{
 return new Expr(op,type,e1,e2);
}

PExpr clone (PExpr e)
// make a deep copy of an expression!
{ 
  if (!e) return NULL;
  int op = e->op(); 
  Type t = e->type();
  PExpr er;
  if (e->is_function() || e->is_expr()) {
    PExprList pel = e->arg_list();
    PExprList pelc = new ExprList(*pel);
    er = new Expr(op,t,
               e->is_function() ? (PExpr)e->function() : clone(e->arg1()),
               pelc);
  } else
  if (e->is_entry()) {
    er = new Expr(op,t,e->entry());
  }
  // for all other operators, can treat the arguments as expressions
  else er = new Expr(op,t,clone(e->arg1()),clone(e->arg2()));
  return er;
}

void dump(ostream& os, PExpr e)
{
 if (!e) return;
 if (e->is_function() || e->is_expr()) {
   os << " (";
   if (e->is_expr()) dump(os,e->arg1());
                else os << '(' << e->type() << ')' << e->function()->name();
   ExprList::iterator eli;
   PExprList pel = e->arg_list();
   if (pel)
    for(eli = pel->begin();  eli != pel->end(); ++eli)
      dump(os,*eli);
   os << ')';
 } else 
 if (e->is_nil()) os << "<nil>"; 
 else
 if (! e->is_entry()) { // i.e. a binary or unary op!
     os << " (";
     if (! e->is_bcast()) os << e->name();
                     else os << '(' << e->type() << ')';
     dump(os,e->arg1());
     dump(os,e->arg2());
     os << ')';
 } else os << ' ' << e->name();
}

void init()
{
 int equiv[] = {MUL_A,STAR,DIV_A,DIVIDE,MOD_A,MODULO,ADD_A,PLUS,MINUS_A,MINUS,
               SHL_A,LSHIFT,SHR_A,RSHIFT,BAND_A,BIN_AND,BOR_A,BIN_OR,XOR_A,BIN_XOR};

 int takes_float[] = {PLUS,MINUS,UPLUS,UMINUS,STAR,DIVIDE,EQUAL,LESS_THAN,
                       NOT_EQUAL,GREATER,LEQ,GEQ,ASSIGN,COMMA,':'}; 
 // *fix 0.9.3 arith. if refused floating-point arguments - added ':' above.
 int i;

 for(i = 0; i < sizeof(equiv)/(2*sizeof(int)); i++)
   assign_equiv_op[equiv[2*i]] = equiv[2*i+1];

 for(i = 0; i < sizeof(takes_float)/sizeof(int); i++)
   float_op[takes_float[i]] = 1;

 t_const_int = t_int;
 t_const_int.make_const();

}

} // namespace Expressions


