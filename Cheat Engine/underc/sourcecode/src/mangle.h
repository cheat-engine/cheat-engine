// Mangle.h
// creates C++ 'decorated names' for a few common compilers
#ifndef __MANGLE_H
#define __MANGLE_H
namespace Mangle {
  char *microsoft(Function *pf);
  char *GCC2(Function *pf);
  char *GCC3(Function *pf);
}

#endif

