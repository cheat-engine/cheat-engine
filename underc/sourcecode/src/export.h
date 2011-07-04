// export.h

#ifndef __EXPORT_H
#define __EXPORT_H

#define CEXPORT extern "C"  EXPORT

// *ch 1.2.9 patch
#ifndef _WIN32
# ifndef __BEOS__
#   define __declspec(x)
# endif
# define XAPI
#else
# define XAPI __stdcall
#endif

#ifdef WITHIN_UC
# define EXPORT __declspec(dllexport)
#else
# ifdef __UNDERC__
#   include <string>
    using std::string;
#   define EXPORT
# else
// using this to import into a C++ prg from the DLL
#   define EXPORT __declspec(dllimport)
#   include "mstring.h"
# endif
#endif

#endif
