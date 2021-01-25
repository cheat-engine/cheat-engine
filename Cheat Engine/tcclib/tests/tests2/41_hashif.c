#include <stdio.h>

int main()
{
   printf("#include test\n");

#if 1
#if 0
   printf("a\n");
#else
   printf("b\n");
#endif
#else
#if 0
   printf("c\n");
#else
   printf("d\n");
#endif
#endif

#if 0
#if 1
   printf("e\n");
#else
   printf("f\n");
#endif
#else
#if 1
   printf("g\n");
#else
   printf("h\n");
#endif
#endif

#define DEF

#ifdef DEF
#ifdef DEF
   printf("i\n");
#else
   printf("j\n");
#endif
#else
#ifdef DEF
   printf("k\n");
#else
   printf("l\n");
#endif
#endif

#ifndef DEF
#ifndef DEF
   printf("m\n");
#else
   printf("n\n");
#endif
#else
#ifndef DEF
   printf("o\n");
#else
   printf("p\n");
#endif
#endif

#define ONE 1
#define ZERO 0

#if ONE
#if ZERO
   printf("q\n");
#else
   printf("r\n");
#endif
#else
#if ZERO
   printf("s\n");
#else
   printf("t\n");
#endif
#endif

   return 0;
}

/* vim: set expandtab ts=4 sw=3 sts=3 tw=80 :*/
