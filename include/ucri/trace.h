/* Functions to make using the UnderC custom trace facility easier.
 * For example, 
 *        add_trace_to_funs("string::*",new XTrace());
 * will start a standard trace on every member function of class string.
 * add_trace_expr() is a particularly convenient way of attaching
 * an expression which will be evaluated whenever a function (or functions)
 * is entered.
 * This can be used to force a function to immediately return a specified
 * value, or to call another function altogether.
 * For example, given a function fred() returning an integer,
 *   add_trace_expr("fred","RET4(xs,0)");
 * is the same as a _temporary_ redefinition of fred() as just { return 10; }
 * Afterwards,
 *   remove_trace("fred");
 * will restore previous behaviour.
*/ 

#ifndef __UCRI_TRACE_H
#define __UCRI_TRACE_H

#include <ucri/utils.h>

namespace UCRI {

void set_fun_trace(XFunction* f, void* ptr)
{
  f->set_trace((XTrace*)ptr);
}

void add_trace_to_funs(const string& fun_pat, XTrace* pt)
{
 uc_ucri_init();
 XFunction::set_tracing(false);
 apply_op_to_funs(fun_pat, set_fun_trace, pt);
 XFunction::set_tracing(true);
}

void remove_trace(const string& fun_pat)
{
 add_trace_to_funs(fun_pat,NULL);
}

int kount = 1;

string tmp_name()
{
 char buff[80];
 sprintf(buff,"__TT%d",kount++);
 return buff;
}

bool add_trace_expr(string fun_pat, string expr)
{
  string cname = tmp_name();
  string cx = "struct " + cname + ": XTrace { ";
  cx += "  void enter(XExecState* xs) { " + expr + "; } };";
  if (uc_exec(cx.c_str())==0) {
    XClass*  pc = uc_global()->lookup_class(cname.c_str());
    XTrace* pt = (XTrace*)pc->create();
    add_trace_to_funs(fun_pat, pt);
    return true;
  } else return false;
}

XInstruction ret0_instr = {8,0,0}; // RET
XInstruction ret4_instr = {9,0,0};  // RETI

#define RET4(val) xs->sp = xs->sp-2; xs->ip = &ret4_instr; *(xs->sp) = val
#define RET0 xs->ip = &ret0_instr
#define CHAIN(fn) xs->fb = &fn; xs->ip = xs->fb->pstart

void dump_fun_args(XExecState* xs)
{
  XFunction* xf = XFunction::from_fb(xs->fb);
  XTList tl;
  XStringList args;
  xf->get_args(&tl,&args);
  string name,s;
  FOR_EACH(name,args) {
     XEntry* pe = xf->lookup_local(name.c_str());
     cout << name << " = ";
     pe->type()->val_as_str(s,pe->data()+xs->bp);
     cout << s << ' ';
  }
  cout << endl;
}

} // namespace UCRI

#endif





