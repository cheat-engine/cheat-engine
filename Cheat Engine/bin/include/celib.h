#ifndef celib_h
#define celib_h

typedef struct _cecs
{
  volatile int locked;
  volatile int threadif;
  volatile int lockcount; 
} cecs, *Pcecs;

#endif