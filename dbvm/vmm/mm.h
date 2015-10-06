#ifndef MM_H_
#define MM_H_

#include "vmmhelper.h"
#include "common.h"



void InitializeMM(UINT64 BaseVirtualAddress);
UINT64 MapPhysicalMemory(UINT64 address, UINT64 VirtualAddress);
UINT64 MapPhysicalMemoryEx(UINT64 address, UINT64 VirtualAddress, int writable);


void *malloc(unsigned int size);
void free(void* pointer);
unsigned int maxAllocatableMemory(void);
void printMMregions();
UINT64 VirtualToPhysical(UINT64 address);
void SetPageToWriteThrough(UINT64 address);


#endif //MM_H_
