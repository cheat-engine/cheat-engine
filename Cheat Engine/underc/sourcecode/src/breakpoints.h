// breakpoints.h
#ifndef __BREAKPOINTS_H
#define __BREAKPOINTS_H
#include "classlib.h"
#include "common.h"
#include "engine.h"
#include <list>

const int MAX_BREAKPOINTS = 20;

class Breakpoint;
typedef std::list<Breakpoint *> BreakpointList;
 
class Breakpoint {
private:
  int m_ip;
  int m_id;
  int m_line;
  Function *m_pf;
  bool m_paused, m_persistent, m_set;
  PInstruction m_pi;
  Instruction m_saved_instruction;

public:
  typedef BreakpointList::iterator iterator;

  Breakpoint(int id, bool persist, Function *pf, int lineno);
  ~Breakpoint(); 
 
  Instruction saved_instruction() { return m_saved_instruction; }
  int         line()              { return m_line; }
  Function *  function()          { return m_pf;  }
  
  static iterator   find_in_function(Function *pf);
  static void toggle(char *file, int lineno, bool is_persistent, ostream& out);
  static void group(char *file, int *lines, int& sz, bool do_get);
  static iterator find_in_file(const string& file);
  static iterator   end_list();
  static Breakpoint *create(const string& filename, int lineno, bool persist); 
  static Breakpoint *from_id(int id);
  static Breakpoint *exists_at(const string& filename, int lineno);
  static void remove_all();

  bool set_line(int l);
  void set_break();
  void restore_instruction();
  bool execute();
};
#endif
