// hard_except.h
// Maps Win32 structured exceptions (SE) onto C++ exceptions

#ifndef _hard_except_h
#define _hard_except_h
class Exception {
private:
    char *m_what;
    int   m_type;
public:
    enum { UNKNOWN, BAD_PTR, INT_OVERFLOW, FLOAT_OVERFLOW, INT_DIV0, FLOAT_DIV0, ACCESS, RANGE_ERROR};
    Exception(char *msg, int type = UNKNOWN) : m_what(msg), m_type(type) {}
    virtual char *what() { return m_what; }
    int type() { return m_type; }
    static void initialize();
};

class BadPointer: public Exception {
public:
    BadPointer(char *msg) : Exception(msg,BAD_PTR) { }
};

class HardwareException: public Exception {
public:
    HardwareException(char *msg,int type) : Exception(msg,type) { }
};

class RangeError: public Exception {
public:
    RangeError(char *msg) : Exception(msg,RANGE_ERROR) { }
};

class IntOverflow: public HardwareException {
public:
    IntOverflow() : HardwareException("integer overflow",INT_OVERFLOW) {}
};

class FloatOverflow: public HardwareException {
public:
    FloatOverflow() : HardwareException("floating point overflow",FLOAT_OVERFLOW) {}
};

class IntDivByZero: public HardwareException {
public:
    IntDivByZero() : HardwareException("integer division by zero",INT_DIV0) {}
};

class FloatDivByZero: public HardwareException {
public:
    FloatDivByZero() : HardwareException("floating point division by zero",FLOAT_DIV0) {}
};

class AccessViolation: public HardwareException {
public:
    AccessViolation() : HardwareException("access violation",ACCESS) {}
};

#ifndef _WIN32
#include <setjmp.h>
  extern jmp_buf here_in_execute;
  void signal_to_exception(int signum);  
  void throw_range_error(char *msg);
#define CATCH_SIGNALS \
{ int sgn; if ((sgn=setjmp(here_in_execute))!=0) signal_to_exception(sgn);}
#else
# define CATCH_SIGNALS
#endif

#endif
