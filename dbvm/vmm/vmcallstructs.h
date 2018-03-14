/*
 * vmcallstructs.h
 *
 *  Created on: Mar 12, 2018
 *      Author: erich
 */

#ifndef VMM_VMCALLSTRUCTS_H_
#define VMM_VMCALLSTRUCTS_H_


typedef struct
{
  DWORD size;
  DWORD password2;
  DWORD command;
} __attribute__((__packed__))  VMCALL_BASIC, *PVMCALL_BASIC;

typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD sourcePA; //physical address to read
  DWORD bytesToRead; //number of bytes to read
  QWORD destinationVA; //virtual address to write to
  DWORD nopagefault; //if not 0 stop at the first pagefault
} __attribute__((__packed__)) VMCALL_READPHYSICALMEMORY, *PVMCALL_READPHYSICALMEMORY;

typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD destinationPA; //physical address to read
  DWORD bytesToWrite; //number of bytes to read
  QWORD sourceVA; //virtual address to write to
  DWORD nopagefault; //if not 0 stop at the first pagefault
} __attribute__((__packed__)) VMCALL_WRITEPHYSICALMEMORY, *PVMCALL_WRITEPHYSICALMEMORY;



typedef struct
{
  VMCALL_BASIC vmcall;
  QWORD PhysicalAddress;
  int Size;
  int Options; //binary.
               //  Bit 0: 0=Log RIP once. 1=Log RIP multiple times (when different registers)
               //  Bit 1: 0=Only log given Physical Address. 1=Log everything in the page(s) that is/are affected
               //  Bit 2: 0=Do not save FPU/XMM data, 1=Also save FPU/XMM data
               //  Bit 3: 0=Do not save a stack snapshot, 1=Save stack snapshot
  int MaxEntryCount; //DBVM will allocate this buffer
  int UsePMI; //trigger a PMI interrupt when full (so you don't lose info)
  int ID; //ID describing this watcher for this CPU (keep track of this on a per cpu basis if you do more than 1 and don't specify all cpu's)
} __attribute__((__packed__)) VMCALL_WATCH_PARAM, *PVMCALL_WATCH_PARAM;

typedef struct
{
  VMCALL_BASIC vmcall;
  DWORD ID;
} __attribute__((__packed__)) VMCALL_WATCH_DISABLE_PARAM, *PVMCALL_WATCH_DISABLE_PARAM;


typedef struct
{
  VMCALL_BASIC vmcall;
  DWORD ID;
  QWORD results; //virtual address receiving log
  DWORD resultsize;
  DWORD copied; //the number of bytes copied so far (This is a repeating instruction)
} __attribute__((__packed__)) VMCALL_WATCH_RETRIEVELOG_PARAM, *PVMCALL_WATCH_RETRIEVELOG_PARAM;

/*
 *    DWORD ID;
    UINT64 results;
    int resultsize;
    int copied; //the number of bytes copied so far (This is a repeating instruction)
 */


#endif /* VMM_VMCALLSTRUCTS_H_ */
