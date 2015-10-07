#ifndef VMPAGING_H_
#define VMPAGING_H_

#include "vmmhelper.h"

/* VirtualMachinePagingspace: this contains a pointer to memory that can be used to 
   store the pages for the vm , this includes the pagedirptr and pagedir depending on what
   type of scheme is chosen by the guest */
void *nonpagedEmulationPagedir; //nonpaged memory emulation pagedir


//void *VirtualMachinePagingspace; //virtual TLB 
//void *VirtualMachinePagingspaceFreeSpot; //next entry for virtual TLB

void *mapVMmemory(pcpuinfo currentcpuinfo, UINT64 address, int size,  UINT64 VirtualAddress, int *error, UINT64 *pagefaultaddress);
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
