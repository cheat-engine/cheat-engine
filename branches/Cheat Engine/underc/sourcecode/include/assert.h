// assert.h
// UnderC Development Project, Steve Donovan, 2001
#ifndef __ASSERT_H
#define __ASSERT_H
// 1.2.7 Now uses <stdio.h>, __LINE__, and respects NDEBUG
#include <stdio.h>

#define AS_STR(s) #s

#ifndef NDEBUG
  #define assert(expr)  if (!(expr)) \
           __assert(#expr,__FILE__ ,__LINE__)
#else
 #define assert(expr)
#endif

inline void __assert(char *expr, char* file, int line)
{
   fprintf(stderr,"assertion failed %s %d %s\n",file,line,expr);   
   //exit(-1);
}

#endif
