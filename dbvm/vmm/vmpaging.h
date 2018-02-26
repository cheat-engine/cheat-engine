#ifndef VMPAGING_H_
#define VMPAGING_H_

#include "vmmhelper.h"


void *nonpagedEmulationPagedir; //nonpaged memory emulation pagedir (pml4)
void *nonpagedEmulationPagedir32BitPAE;
void *nonpagedEmulationPagedir32Bit;

QWORD nonpagedEmulationPagedirPA; //nonpaged memory emulation pagedir (pml4)
QWORD nonpagedEmulationPagedir32BitPAEPA;
QWORD nonpagedEmulationPagedir32BitPA;


void *mapVMmemory(pcpuinfo currentcpuinfo, UINT64 address, int size, int *error, UINT64 *pagefaultaddress);
void *mapVMmemoryEx(pcpuinfo currentcpuinfo, UINT64 address, int size, int *error, UINT64 *pagefaultaddress, int donotunmaponfail );
void unmapVMmemory(void *address, int size);
UINT64 getPhysicalAddressVM(pcpuinfo currentcpuinfo, UINT64 address, int *notpaged);
void optimizeVTLB(pcpuinfo currentcpuinfo, int usermode, int WPbit);
int ReadVMMemory(pcpuinfo currentcpuinfo, UINT64 address, unsigned char *buf,int size);
int handleVMPageException(pcpuinfo currentcpuinfo);
int handleINVLPG(pcpuinfo currentcpuinfo);
void handleFullTLB(pcpuinfo currentcpuinfo);
int emulatePaging(pcpuinfo currentcpuinfo);
//int setupA20maskedPaging(void);
int setupNonPagedPaging(pcpuinfo currentcpuinfo);
int setupRealModePaging(pcpuinfo currentcpuinfo);
int setupNonPagedPaging_invalidstate_c0000(pcpuinfo currentcpuinfo, UINT64 cs_base);
int allocateVirtualTLB(void);


#endif
