// CLASSLIB.H
// This concentrates all the assumptions about the iostreams
// and string libraries being used.  Personally I prefer the classic
// iostreams in console mode, and a hand-rolled string;  the std
// versions are too expensive in compilation time and executable size.
// The alternative libraries are all kept in the same directory to
// prevent confusion about include paths.
#ifndef _CLASSLIB_H
#define _CLASSLIB_H

#ifndef _WCON
# ifndef _FAKE_IOSTREAM
#  include <iostream.h>
#  ifdef __GNUC__
#   include <strstream.h>
#  else
#   include <strstrea.h>
#  endif
#  include <fstream.h>
# else
#  include "iostrm.h"
# endif
# if defined(_CONSOLE) || defined(_USRDLL)
// *add 1.2.4 Redirection of cmsg and cerr is by redefining them to be pointer references
// (see errors.cpp for the implementation)
extern ostream* _cmsg_out;
extern ostream* _cerr_out;
#   define cmsg *_cmsg_out
#   undef cerr
#   define  cerr *_cerr_out
# else
// this is how the DLL redirects output...
#  define cmsg str_cmsg
#  undef cerr
#  define cerr str_cerr
#  undef cout
#  define cout cmsg
   extern ostrstream str_cmsg, str_cerr;
# endif
#else
# include "wcon.h"
#endif

// disable warnings about template names being too long for the debugger..
#pragma warning(disable:4786)
#pragma warning(disable:4800)
#include "mstring.h"

// a useful pattern for using STL containers
#define FORALL(i,ls) for(i = (ls).begin(); i != (ls).end(); ++i)

// *ch 1.2.9 patch
#ifndef _WIN32
// iscsym() and iscsymf() are useful <cctype> extensions found only on Windows
#define iscsym(x)  (isalnum(x) || (x) == '_')
#define iscsymf(x) (isalpha(x) || (x) == '_')
// ditto for itoa()
char *itoa(int, char *,int);
#define __STDCALL
#else
#define __STDCALL __stdcall
#endif

// *fix 1.2.7 The GCC 3.2 <iostream> library is more uptodate, and doesn't
// have nocreate.  However, for earlier versions we do need it explicitly!
#if  __GNUC__ == 3
#define IOS_IN_FLAGS ios::in 
#else
#define IOS_IN_FLAGS ios::nocreate
#endif

#endif

