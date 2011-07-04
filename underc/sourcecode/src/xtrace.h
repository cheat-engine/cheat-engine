// Xtrace.h
// *add 1.2.3 XTrace can be used to perform actions when entering or leaving a function
// (definitions are in engine.cpp)

#ifndef __XTRACE_H
#define __XTRACE_H

/// XExecState is a representation of the UnderC machine state.
/// This class can be used to modify the run-time environment.
struct XExecState {
    FBlock* fb;          ///< currently executing function block
    int*    sp;           ///< stack pointer
    char*   op;          ///< object pointer
    void*   ip;           ///< instruction pointer
    int*    bp;           ///< base pointer (args and locals are relative to this) 
    FBlock* last_fb;   ///< from where we've been 
    void*   last_ip;    ///< called...
    int*    last_bp;    ///< base pointer of calling context...
};

/// XTrace is a customizable tracing class, which by default will log each
/// time a function is entered.  It is possible to set whether enter() and
/// leave() are called, if at all (i.e. tracing can be switched off for a 
/// individual function by set_on_entry(false); set_on_exit(false)).
/// You will usually derive a custom trace class from XTrace and override
/// either enter() or leave() (or both).
class EXPORT XTrace {
private:
    int m_on_entry, m_on_exit;
public:
  // a new trace object
  // \param on_exit  Is XTrace::leave() called when function returns?
    XTrace(bool on_exit = false);
 // set whether XTrace::enter() is called
    void set_on_entry(bool b) { m_on_entry = b;  }
	void set_on_exit(bool b)  { m_on_exit = b;  }
    bool do_leave()         { return m_on_exit; }
	bool do_enter()         { return m_on_entry; }

    // the XTrace interface
    virtual void enter(XExecState* xs);
    virtual void leave(XExecState* xs);
};

#endif
