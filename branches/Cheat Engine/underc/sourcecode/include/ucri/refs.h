/* Find all references to a function or static variable/constant
 * Basically, look at the pcode of all functions in the system
 * and find any 'direct' references to the address. (Direct addresses are
 * actually 22-bit offsets into the global data segment; this is how
 * we fit them into a 32-bit instruction).  The address of a function is
 * its function block, which contains a reference to the actual pcode.
*/

#ifndef __UCRI_REFS_H
#define __UCRI_REFS_H
#include <ucri/utils.h>

namespace UCRI {

class XPosition {
  XFunction* m_fun;
  XInstruction* m_ip;
public:
  XPosition(XFunction* fun, XInstruction* ip)
  : m_fun(fun), m_ip(ip) { }
  XPosition() { }
  XFunction*    fun() { return m_fun; }
  XInstruction* ip()  { return m_ip; }
  string        file() {
   string fs;
   m_fun->where(fs);
   return fs;
  }
  int           ip_offs() { return m_ip - m_fun->pcode(); }
  int           line()    { return m_fun->ip_to_line(m_ip); }
};
typedef list<XPosition> XPositions;

class References {
  XFunctions g_fl;
  XPositions g_pos_list;
  XClass* g_self;
  XNTable* g_context;
public:  
  void update()
  {
    grab_all_functions(g_fl,g_self);
  }

  References()
  {
    uc_ucri_init();
    g_self = uc_global()->lookup_class("References");
    g_context = uc_global();
    update();
  }

  void set_context(XNTable* context)
  { g_context = context; }
  
  void generate(int target_data)
  {
  // for all functions _excluding_ the methods of this class!
    XFunction* fn;
    FOR_EACH(fn,g_fl) {
      // look at the pcode for any direct addressing of the given offset
      XInstruction* pp = fn->pcode();
      if (pp)
       while (pp->opcode != 0) {
         if (pp->rmode == DIRECT && pp->data == target_data)
           g_pos_list.push_back(XPosition(fn,pp));
         pp++;
       }
     }
   }

   void dump()
   {
     XPosition pos;
     FOR_EACH(pos,g_pos_list) {
      cout << pos.file() << ": " << pos.line() << ' '
           << pos.fun()->name() << ' ' << pos.ip_offs() << endl;
     }
   }

  void go(int data)
  {
    g_pos_list.clear();
    generate(data);
    dump();
  }

  bool set_function(XFunction* pf)
  {
    // find the offset of the function block in the global data segment
    int data = uc_global()->offset(pf->fblock());
    go(data);
    return true;
  }

  bool set_function(string name, int k = 1)
  {
    XFunction* fn = lookup_fun(name,k);
    if (fn) return set_function(fn);
    else return false; // no overloaded function w/ this index...
  }

  bool set_global(string name)
  {
   // we fail if this reference either doesn't exist, or doesn't use
   // direct addressing.
    XEntry* xe = g_context->lookup(name.c_str()); 
    if (! xe || xe->addr_mode() != DIRECT) return false;
    go(xe->data());
    return true;
  }

};  // class References

} // namespace UCRI

#endif
