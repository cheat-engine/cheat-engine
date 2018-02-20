#ifndef MULTICORE_H_
#define MULTICORE_H_

#include "common.h"

extern volatile unsigned int cpucount;
unsigned int initAPcpus(DWORD entrypage);

#endif /*MULTICORE_H_*/
