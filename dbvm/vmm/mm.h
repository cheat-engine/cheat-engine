#ifndef MM_H_
#define MM_H_


#include <stddef.h>

#include "vmmhelper.h"
#include "common.h"

typedef struct
{
  //UINT64 PA; //Physical address
  //Virtual address is known by the index in the array
  UINT64 BitMask;
} PageAllocationInfo, *PPageAllocationInfo;

extern QWORD FirstFreeAddress;
extern QWORD extramemory;
extern QWORD extramemorysize;

extern QWORD contiguousmemoryPA;
extern QWORD contiguousmemorysize;

extern unsigned char MAXPHYADDR;
extern QWORD MAXPHYADDRMASK;
extern QWORD MAXPHYADDRMASKPB;


extern PPDPTE_PAE pml4table;





void InitializeMM(UINT64 FirstFreeVirtualAddress);
QWORD getTotalFreeMemory(QWORD *FullPages);

void configureMapping(pcpuinfo cpuinfo);

void* getMappedMemoryBase();

PPTE_PAE mapAddressAtPML4(QWORD address);
void unmapAddressAtPML4(PPTE_PAE base);

void mapPhysicalAddressToVirtualAddress(QWORD PhysicalAddress, QWORD VirtualAddress);
void* mapMemory(void *destination, void *source, int size);  //maps a virtual memory region with the specified virtual memory region

int mmFindMapPositionForSize(pcpuinfo cpuinfo, int size);

void unmapPhysicalMemory(void *virtualaddress, int size);
void* mapPhysicalMemory(QWORD PhysicalAddress, int size);
void* mapPhysicalMemoryAddresses(QWORD *addresses, int count);

void* mapPhysicalMemoryGlobal(QWORD PhysicalAddress, int size);
void unmapPhysicalMemoryGlobal(void *virtualaddress, int size);

void VirtualAddressToIndexes(QWORD address, int *pml4index, int *pagedirptrindex, int *pagedirindex, int *pagetableindex);

#define IndexesToVirtualAddress(pml4index, pagedirptrindex, pagedirindex, pagetableindex) (QWORD)(((QWORD)pml4index<<39) | ((QWORD)pagedirptrindex << 30) | ((QWORD)pagedirindex << 21) | ((QWORD)pagetableindex << 12))


void *malloc(size_t size);
void *malloc2(unsigned int size);
void free(void* pointer);
void free2(void* pointer, unsigned int size);
void *realloc(void *old, size_t size);
void *realloc2(void *oldaddress, unsigned int oldsize, unsigned int newsize);

void *allocateContiguousMemory(int pagecount);

unsigned int maxAllocatableMemory(void);
void printMMregions();
UINT64 VirtualToPhysical(void *address);
void SetPageToWriteThrough(void *address);
PPDE_PAE getPageTableEntryForAddress(void *address);
PPDE_PAE getPageTableEntryForAddressEx(void *address, int allocateIfNotPresent);

void markPageAsNotReadable(void *address);
void markPageAsReadOnly(void *address);
void markPageAsWritable(void *address);

void VirtualAddressToPageEntries(QWORD address, PPDPTE_PAE *pml4entry, PPDPTE_PAE *pagedirpointerentry, PPDE_PAE *pagedirentry, PPTE_PAE *pagetableentry);

void mmAddPhysicalPageListToDBVM(QWORD *pagelist, int count, int inuse);
void mmtest();

typedef void(*MMENUMPAGESCALLBACK)(QWORD VirtualAddress, QWORD PhysicalAddress, int size, PPTE_PAE entry, void *context);

void mmEnumAllPageEntries(MMENUMPAGESCALLBACK callbackfunction, int skipmapped, void *context);
//void wtftest(void); //test routine to figure out why some memory gets paged out

#endif //MM_H_
