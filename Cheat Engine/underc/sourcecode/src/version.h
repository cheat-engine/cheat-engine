// version.h

#ifndef __VERSION_H
#define __VERSION_H

#define _UCV_  "1.2.9"
#ifdef _WCON
#define mUCVersion _UCV_ "w"
#define mUCTitle "UnderC for Windows"
#else
# ifdef _WIN32
#define mUCVersion _UCV_ "c" 
# else
#define mUCVersion _UCV_ "L"
# endif
#define mUCTitle "UnderC"
#endif

#endif


