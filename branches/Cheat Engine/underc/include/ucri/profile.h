/* A simple profiler for UnderC
 * This uses the custom trace facility to monitor every function in the system
*/

#ifndef __UCRI_PROFILE_H
#define __UCRI_PROFILE_H
#include <ucri/utils.h>
#include <algorithm>

unsigned long* g_pic;

// XProfiler is a custom trace class which is attached to every function;
// it updates the called count and the total cycles used by the function.
class XProfiler: public XTrace {
   int m_kount;
   int m_cycles;
   int m_start;
 public:
   int kount()  { return m_kount;  }

   XProfiler() : m_kount(0),m_cycles(0),XTrace(true) { }

   int cycles() { return m_cycles; }

   void enter(XExecState* xs)
   {
     m_kount++;
     m_start = *g_pic;
   }

   void leave(XExecState* xs)
   {
     m_cycles += (*g_pic - m_start);
   }
 };

struct ProfileItem {
  XFunction* fun;
  int count;
  float percent;
};

typedef std::list<ProfileItem> ItemList;

// we want to sort these guys in reverse order, hence this eccentric definition!
bool operator< (ProfileItem& p1, ProfileItem& p2)
{
  return p1.percent > p2.percent;
}

class Profiler {
  XFunctions m_fl;
  ItemList m_item_list;
  XClass* m_self;
public:
  Profiler()
  {
    uc_ucri_init() 	
    m_self = uc_global()->lookup_class("Profiler");
  }

  // to start the profiler, attach a TProfiler tracer to each function
  // and switch on profiling and tracing.
  void start()
  {
    XFunction* fn;
    XFunction::set_tracing(false);
    UCRI::grab_all_functions(m_fl,m_self);
    FOR_EACH(fn,m_fl)
      fn->set_trace(new XProfiler());
    g_pic = ucri_instruction_counter(true);
    XFunction::set_tracing(true);
  }

  // to stop, extract the trace objects and switch off tracing for each function.
  void stop()
  {
    XFunction::set_tracing(false);
    XFunction* fn;
    ProfileItem pitem;
    double total = *g_pic/100.0, frac;
    m_item_list.clear();
    FOR_EACH(fn, m_fl) {
      XProfiler* pi = (XProfiler*)fn->get_trace();
      if (pi) {
        fn->set_trace(NULL);
        frac = pi->cycles()/total;
        if (frac > 0.2) { // only consider contributions greater than 0.2%
          pitem.fun = fn;
          pitem.count = pi->kount();
          pitem.percent = frac;
          m_item_list.push_back(pitem);
        }
        delete pi;
      }
    }
    std::sort(m_item_list.begin(), m_item_list.end());
  }

  void report(int n = 10)
  {
    ProfileItem pitem;
    int k = 0;
    cout << "Function\tPercent\tCount" << endl;
    FOR_EACH(pitem,m_item_list) {
      cout << pitem.fun->name() << '\t' << pitem.percent << '\t' << pitem.count << endl;
      if (++k > n) break;
    }
  }
}; // class Profiler
#endif
