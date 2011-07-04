// Expressions.h

#ifndef __EXPRESSIONS_H
#define __EXPRESSIONS_H
#include "types.h"
#include "function.h"
#include <assert.h>

const int NIL = 0, EXPR = -1, IREF = 1, BCAST = 2, ECONST = 3;
// extra operations MUST NOT CONFLICT w/ tparser.h!
// Fortunately, the value of TOKEN is 258, so there's plenty
// of room.
const int INCR_PTR = 10, DECR_PTR = 11, COPY_BLOCK = 12,
          CCONTEXT = 13, INIT_REF = 14, METHOD_CALL = 15,
          EXPR_METHOD = 16, APPEND = 17, 
          DYNACAST = 18, REF_STUB = 19,
          DCALL = 20, PASS_BY_VALUE = 21,
		  DCONTEXT = 22, VCONTEXT = 23, FCONTEXT = 24,
          LAST_OP = 100
          ; 

// forward dcl of Expr, etc in function.h above!
// note the convention that a NULL function is 
// an expr list.
class Expr {
protected:
    int m_op;     
    Type m_type;  // type of this item or subexpression
    PExpr m_arg1; 
    PExpr m_arg2;
public:
    Expr(int op=0,Type type=t_void,void *arg1=NULL,void *arg2=NULL)
     : m_op(op), m_type(type), m_arg1((PExpr)arg1),m_arg2((PExpr)arg2) 
     {}

    int    op()          { return m_op;   }
    void   set_op(int op){ m_op = op;     }
    Type   type()        { return m_type; }
    void   set_type(Type t) { m_type = t; }
    bool   is_expr()     { return m_op == EXPR; }
    bool   is_entry()    { return m_op == IREF; }
    bool   is_nil()      { return m_op == NIL;  } 
	bool   is_const()    { return m_op == ECONST; }
    bool   is_function();
    bool   is_variable();
    bool   is_bcast()    { return m_op == BCAST; } 
    bool   is_expr_list();
	bool   is_brace_list();
    PExpr  arg1()        { return m_arg1; }
    PExpr  arg2()        { return m_arg2; }
    void   arg1(PExpr e) { m_arg1 = e;    }
    void   arg2(PExpr e) { m_arg2 = e;    }
    PEntry    entry() { /*assert(is_entry());*/ return (PEntry)m_arg1; } 
    PExpr     expr()  { /*assert(is_expr());*/  return m_arg1; }
    Function *function() { /*assert(is_function());*/ return (Function *)m_arg1; }
    PExprList arg_list() { return (PExprList) m_arg2; }
    PExprList expr_list() { return arg_list(); }
    string name();
};

namespace Expressions {

void init();
bool is_atom(PExpr e);
void dump(ostream& os, PExpr e);
PExpr clone (PExpr e);
PExpr entry_op(PEntry pe);
PExpr make_op(int op,Type type, PExpr e1, PExpr e2 = NULL);

PEntry ExprToEntry(PExpr e);
Type type_of(PExpr e);
PFunction lookup_function(char *name);
int  call_main(int argc, char **argv);
void  stop_main(bool is_error=true);
PExprList expr_list(PExpr e1=NULL, PExpr e2=NULL);
PExpr function_call(const char* name, PExprList pel);

// functions to build up expression trees
PExpr arith_op(int op, PExpr e1, PExpr e2);
PExpr bin_op(int op, PExpr e1, PExpr e2);
PExpr delete_op(PExpr e1, bool is_arr);
PExpr initialize_op(PEntry pe, PExpr er, PExprList pel, int ctype);
PExpr construct_op(PClass pc, PExprList pel, bool check_abstract = true);
PExpr init_ref_op(PExpr el, PExpr er);
PExpr new_op(Type t, PExpr e, PExprList pel);
PExpr deref_op(PExpr e1, bool do_overload=true);
PExpr addr_op(PExpr e1, bool do_overload=true,bool force_reference=false);
PExpr reference_stub_op(PExpr e);
PExpr unary_op(int op, PExpr e1);
PExpr arith_if_op(PExpr cond, PExpr e1, PExpr e2);
PExpr sizeof_op(int type_size);
PExpr typecast_op(int which, Type t, PExpr e);
PExpr function_cast_op(Type t, PExprList pel);
PExpr cast_to_bool_op(PExpr e1);
PExpr inc_dec_op(int op, PExpr e, bool is_prefix); 
PExpr assign_op(PExpr e1, PExpr e2, bool constness = true);
PExpr compound_assign_op(int op, PExpr e1, PExpr e2);
PExpr return_op(Function *fn, Type rt, PExpr e);
PExpr relational_op(int op, PExpr e1, PExpr e2);
PExpr equal_op(PExpr e1, PExpr e2);
PExpr array_op(PExpr e1, PExpr e2);
PExpr function_op(PExpr e1, PExprList e2, bool suppress_error = false);
PExpr method_call(PFunction fn, PExpr obj, PExprList pel);
void set_first_object(PExpr e1);
Type typeof_op(PExpr e);
PExpr selection_op(PExpr e, char *name,bool is_ptr, bool is_member_ptr=false);
PExpr expr_list_op(PExprList pel, bool is_list);
PExpr append_op(PExpr e1, PExpr e2, bool drop_values=false);
PExpr lambda_op(PEntry pe);
 
/*template <class T>
PExpr constant_op(Type t, T val)
{
 PEntry pe;
 using Parser::create_const_entry;
 T *pi = (T *)create_const_entry(t,pe);
 *pi = val;
 return entry_op(pe);
}
*/
PExpr constant_op(Type t, unsigned long val);



} // namespace Expressions



#endif

