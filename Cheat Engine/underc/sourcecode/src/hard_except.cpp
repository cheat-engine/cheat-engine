/* hard_except.cpp
 * Mapping between hardware signals/exceptions and C++ exceptions
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
*/
#include "hard_except.h"

#ifndef __GNUC__
// Mapping Win32 SE exceptions onto C++ exceptions
#include <eh.h>
#include <excpt.h>
#include <windows.h>
#include <winnt.h>
#include <float.h>
#include <assert.h>

void trans_func( unsigned int u, _EXCEPTION_POINTERS* pExp )
{
   //*fix 1.2.4 Reset floating point after exceptions - otherwise we may lose further fp exceptions!
   _fpreset();
   if(u == STATUS_ACCESS_VIOLATION) throw AccessViolation(); else
   if(u == STATUS_FLOAT_DIVIDE_BY_ZERO) throw FloatDivByZero(); else
   if(u == STATUS_FLOAT_OVERFLOW) throw FloatOverflow(); else
   if(u == STATUS_INTEGER_DIVIDE_BY_ZERO) throw IntDivByZero(); else
   if(u == STATUS_INTEGER_OVERFLOW) throw IntOverflow();
   else throw Exception("Unknown");
}

// I found it necessary to use this little low-level fiddle
// from Matt Pietrek (_Under the Hood) article from MSJ, 1997)
void UnmaskFPExceptionBits( void )
{
    unsigned short cw;

    __asm   fninit      // Initialize the coprocessor
    __asm   fstcw [cw]
    cw &= 0xFFE0;       // Turn off the most of the exception bits (except the
                        // the precision exception)
    __asm   fldcw [cw]

}

int check_mem();

void Exception::initialize()
{
 // enable FP exceptions
 // *unfix 1.2.1 Use _controlfp() rather than half-assed inline assembler
//  _controlfp(_EM_ZERODIVIDE, _MCW_EM);
// Trouble is I get 'bad number' exceptions as well, which may well be a Win9x thing.
   UnmaskFPExceptionBits();
  _set_se_translator( trans_func );
}

#else
#include <signal.h>
#include <string.h>

static char* mRangeMsg = NULL;

void signal_to_exception(int signl)
{
 switch(signl) {
  case SIGSEGV: throw AccessViolation();
  case SIGILL:  throw RangeError(mRangeMsg);
  case SIGFPE:  throw FloatDivByZero();
  default: throw Exception("unknown");
 } 
}

// *fix 1.2.6 raise() only works _once_.  However, there's no reason why we can't
// call the handler directly!  

void handler(int);
 
void throw_range_error(char* msg)
{
  mRangeMsg = strdup(msg); 
  handler(SIGILL);
}

// Win32 and Linux work differently here. In Win32 you can directly
// throw a C++ exception from a signal handler, in Linux you have
// to make a long jump out of the handler to avoid crashing in flames.
// See hard_except.h and CATCH_SIGNALS in engine.cpp to see how the rest works.
#ifndef _WIN32
 #include <setjmp.h>
 jmp_buf here_in_execute;

 void handler(int signl)
 {
   longjmp(here_in_execute,signl);
 }
#else
 void handler(int signl)
 {
   signal_to_exception(signl);
 }
#endif


void UnmaskFPExceptionBits()
{
//?que? ISSUE: we _do_ need to do this, I think!
}

void Exception::initialize()
{
 UnmaskFPExceptionBits();
 signal(SIGSEGV, handler);
 signal(SIGILL, handler);
 signal(SIGFPE, handler);
}

#endif

class __Init__ {
public:
    __Init__() {
        Exception::initialize();    
    }
};

__Init__ this__Init__;


