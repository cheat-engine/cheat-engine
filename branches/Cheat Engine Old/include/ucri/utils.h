/* A set of utilities for working with functions using UCRI.
 */

#ifndef __UCRI_UTILS_H
#define __UCRI_UTILS_H

#include <ucri.h>
#include <for_each.h>
#include <algorithm>

namespace UCRI {

XNTable* context_from_pattern(string& pat, XNTable* context = NULL)
{
 int i = pat.find("::");
 if (context==NULL) context=uc_global();
 if (i != string::npos) {
    string class_name = pat.substr(0,i);
    pat = pat.substr(i+2,999);
    context = context->lookup_class(class_name.c_str());
 } 
 return context;
}

XFunction* lookup_fun(string name, int k = 1, XNTable* context = NULL)
{
  context = context_from_pattern(name,context);
  if (! context) return NULL;
  XEntry* xe = context->lookup(name.c_str());
  if (xe->nfun() >= k) return xe->function(k);
  else return NULL;
}

int get_matching_functions(string fun_pat, XFunctions& fl, XNTable* context = NULL)
{
 context = context_from_pattern(fun_pat,context);
 if (! context) return 0;
 context->get_functions(fl,0,fun_pat.c_str());
 return fl.size();
}

int get_matching_classes(const string& clss_pat, XClasses& cl, XNTable* context = NULL)
{
  XEntries xl;
  if (context==NULL) context=uc_global();
  context->get_variables(xl,CLASSES | NAMESPACES, clss_pat.c_str());
  XEntry* xe;
  cl.clear();
  FOR_EACH(xe,xl)
    cl.push_back(xe->type()->as_class());
  return cl.size();
}

int grab_all_functions(XFunctions& fl, XClass* exclude_context=NULL)
{
 XFunctions cl_fl;
 XClass* context;
 XClasses cl;
 get_matching_functions("*",fl);
 get_matching_classes("*",cl);
 FOR_EACH(context,cl) 
  if (context != exclude_context) {
    get_matching_functions("*",cl_fl,context);
    fl.splice(fl.begin(),cl_fl);  // append the methods to the function list
  }
 return fl.size();
}

void dump_functions(XFunctions& fl)
{
  XFunction* f;
  string s;
  FOR_EACH(f,fl) {
    f->as_str(s);
    cout << s << ';' << endl;
  }
}

void dump_entries(XEntries& xl)
{
  XEntry* xe;
  int k = 0;
  FOR_EACH(xe,xl) {
   cout << xe->name();
   if (++k % 5 != 0) cout << '\t';
   else cout << endl;
  }
  cout << endl;
}

void dump_classes(XClasses& xl)
{
  XClass* xc;
  FOR_EACH(xc,xl)
   cout << xc->name() << endl;
}

void dump_function(ostream& out, void* fblk)
{
 XFunction* f = XFunction::from_fb(fblk);
 string s;
 f->as_str(s);
 out << s;
}

XFunctions fns;

void testf(string pat)
{
 get_matching_functions(pat,fns);
 dump_functions(fns);
}

typedef void (*FUN_OP)(XFunction* f, void* ptr);
typedef XFunction* PXFunction;

// note: this is encoded somewhat elaborately because we want to guarantee
// that _only_ fun_op() will be called in the iteration loop.  Otherwise
// one gets interesting results with operations which affect function behaviour
// like tracing, profiling, etc.
int apply_op_to_funs(string fun_pat, FUN_OP fun_op, void* arg)
{
  XFunctions fl;
  XFunction* pf;
  get_matching_functions(fun_pat,fl);
  int n = fl.size();
  XFunction** temp_fns = new PXFunction[n];
  std::copy(fl.begin(),fl.end(),temp_fns);
  for (int i = 0; i < n; i++) 
    fun_op(temp_fns[i],arg);
  delete temp_fns;
  return n;
}

} // namespace UCRI

#endif