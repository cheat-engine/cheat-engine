#ifndef celib_h
#define celib_h

typedef struct _cecs
{
  volatile int locked;
  volatile int threadid;
  volatile int lockcount; 
} cecs, *Pcecs;

void csenter(cecs *cs);
void csleave(cecs *cs);

#endif