/*
 * eptstructs.h
 *
 *  Created on: Mar 12, 2018
 *      Author: erich
 */

#ifndef VMM_EPTSTRUCTS_H_
#define VMM_EPTSTRUCTS_H_

#include "common.h"
#include "vmcallstructs.h"

//the EPT fields are pretty much the same as normal paging fields, Read==Present, RW=write etc... BUT the A and D bits access fields used for other things that WILL cause bad effects.
//so do NOT map them as page tables for easy editing

typedef struct _EPT_PML4E
{
  unsigned RA        :  1; // Read Access
  unsigned WA        :  1; // Write Access
  unsigned XA        :  1; // Execute access (supervisor mode in mode-based execute control for EPT)
  unsigned reserved  :  5; //
  unsigned Accessed  :  1; // Accessed. (if EPTP[6]==1)
  unsigned ignored   :  1; //
  unsigned XA_UM     :  1; // Execute access (usermode in mode-based execute control fopr EPT)
  unsigned ignored2  :  1; //
  unsigned long long PFN       : 36; //physical address of EPT pagedirptr
  unsigned reserved2 :  4;  //must be 0
  unsigned reserved3 : 12;

} __attribute__((__packed__)) EPT_PML4E, *PEPT_PML4E;

typedef struct _EPT_PDPTE
{
  unsigned RA        :  1; // 0: Read Access
  unsigned WA        :  1; // 1: Write Access
  unsigned XA        :  1; // 2: Execute access (supervisor mode in mode-based execute control for EPT)
  unsigned MEMTYPE   :  3; // 3-5: BIG=1: EPT memory type (28.2.6)
  unsigned PAT_IGNORE:  1; // 6: BIG=1: Ignore PAT type
  unsigned BIG       :  1; // 7: 1 if it points to a 1 GB physical memory block, 0 if it points to a pagedir
  unsigned Accessed  :  1; // Accessed. (if EPTP[6]==1)
  unsigned Dirty     :  1; //
  unsigned XA_UM     :  1; // Execute access (usermode in mode-based execute control fopr EPT)
  unsigned ignored2  :  1; //
  unsigned long long PFN       : 36; //physical address of EPT pagedir (if big==0) else available physical memory (bit 12-29 must be 0 if big==1)
  unsigned reserved2 :  4;  //must be 0
  unsigned ignored3  : 11;
  unsigned VESupress :  1; //disable #VE exception if EPT-violation #VE is 1 (if big==0)
} __attribute__((__packed__)) EPT_PDPTE, *PEPT_PDPTE;

typedef struct _EPT_PDE
{
  unsigned RA        :  1; // 0: Read Access
  unsigned WA        :  1; // 1: Write Access
  unsigned XA        :  1; // 2: Execute access (supervisor mode in mode-based execute control for EPT)
  unsigned MEMTYPE   :  3; // 3-5: BIG=1: EPT memory type (28.2.6)
  unsigned PAT_IGNORE:  1; // 6: BIG=1: Ignore PAT type
  unsigned BIG       :  1; // 7: 1 if it points to a 1 GB physical memory block, 0 if it points to a pagedir
  unsigned Accessed  :  1; // 8: Accessed. (if EPTP[6]==1)
  unsigned Dirty     :  1; // 9:
  unsigned XA_UM     :  1; // 10: Execute access (usermode in mode-based execute control fopr EPT)
  unsigned ignored2  :  1; // 11:
  unsigned long long PFN       : 36; //physical address of EPT pagetable (if big==0) else available physical memory (bit 12-20 must be 0 if big==1)
  unsigned reserved2 :  4; //must be 0
  unsigned ignored3  : 11;
  unsigned VESupress :  1; //BIG=1: disable #VE exception if EPT-violation #VE is 1
} __attribute__((__packed__)) EPT_PDE, *PEPT_PDE;

typedef struct _EPT_PTE
{
  unsigned RA        :  1; // 0: Read Access
  unsigned WA        :  1; // 1: Write Access
  unsigned XA        :  1; // 2: Execute access (supervisor mode in mode-based execute control for EPT)
  unsigned MEMTYPE   :  3; // 3-5: EPT memory type (28.2.6)
  unsigned PAT_IGNORE:  1; // 6: Ignore PAT type
  unsigned ignored   :  1; // 7:
  unsigned Accessed  :  1; // 8: Accessed. (if EPTP[6]==1)
  unsigned Dirty     :  1; // 9:
  unsigned XA_UM     :  1; // 10: Execute access (usermode in mode-based execute control fopr EPT)
  unsigned ignored2  :  1; // 11:
  unsigned long long PFN       : 36; //physical address of EPT pagetable (if big==0) else available physical memory (bit 12-20 must be 0 if big==1)
  unsigned reserved2 :  4; //must be 0
  unsigned ignored3  : 11;
  unsigned VESupress :  1; //disable #VE exception if EPT-violation #VE is 1
} __attribute__((__packed__)) EPT_PTE, *PEPT_PTE;

typedef struct _EPTP
{
  unsigned MEMTYPE        :  3; // paging structure type
  unsigned PAGEWALKLENGTH :  3; // ept page walk lenght-1
  unsigned ACCESSFLAGS    :  1; // Support Accessed/Dirty flags
  unsigned reserved       :  5; //
  unsigned long long PFN            : 36; //physical address of PML4 table
  unsigned reserved2      : 16; //must be 0
} __attribute__((__packed__)) EPTP, *PEPTP;

typedef union _EPT_VIOLATION_INFO
{
  QWORD ExitQualification;
  struct{
    unsigned R        :  1; // 0: Read Access
    unsigned W        :  1; // 1: Write Access
    unsigned X        :  1; // 2: Execute access (supervisor mode in mode-based execute control for EPT)
    unsigned WasReadable   :  1; //
    unsigned WasWritable   :  1; //
    unsigned WasExecutable :  1; //
    unsigned ANDb10   :  1; // if mode-based execute control else undefined
    unsigned GuestLinearAddressValid: 1;
    unsigned AddressAccess: 1; //if GuestLinearAddressValid then 1 if the access was to a page, 0 if it was in the paging system
    //only defined if GuestLinearAddressValid & AddressAccess == 1 and advanced VM-exit information for EPT violations
    unsigned supervisor     : 1; //1 if supervisor, 0 if usermode
    unsigned pagingreadonly : 1; //1 if paged as read only, 0 if read/write
    unsigned XD             : 1; //1 if execute disable, 0 if executable
    unsigned NMIUnblockedDueToIRET : 1; //
    unsigned reserved1      : 19;
    DWORD reserved2;
  };
} __attribute__((__packed__)) EPT_VIOLATION_INFO, *PEPT_VIOLATION_INFO;

typedef union _NP_VIOLATION_INFO
{
  QWORD ErrorCode;
  struct{
    unsigned P        :  1; // 0: 0 if not present
    unsigned W        :  1; // 1: 1 if it was a write access
    unsigned US       :  1; // 2: 1 if it was a usermode execution
    unsigned RSRVD    :  1; // 3: 1 if a reserved bit was set
    unsigned ID       :  1; // 4: 1 if it was a code fetch
    unsigned reserved1: 27;
    unsigned gfa      :  1; // 32:guest final addr ess
    unsigned gpt      :  1; // 33:guest page table
  };
} __attribute__((__packed__)) NP_VIOLATION_INFO, *PNP_VIOLATION_INFO;



//pagewatch:

typedef struct _fxsave64
{
  WORD FCW;
  WORD FSW;
  BYTE FTW;
  BYTE Reserved;
  WORD FOP;
  UINT64 FPU_IP;
  UINT64 FPU_DP;
  DWORD MXCSR;
  DWORD MXCSR_MASK;
  QWORD FP_MM0;
  QWORD FP_MM0_H;
  QWORD FP_MM1;
  QWORD FP_MM1_H;
  QWORD FP_MM2;
  QWORD FP_MM2_H;
  QWORD FP_MM3;
  QWORD FP_MM3_H;
  QWORD FP_MM4;
  QWORD FP_MM4_H;
  QWORD FP_MM5;
  QWORD FP_MM5_H;
  QWORD FP_MM6;
  QWORD FP_MM6_H;
  QWORD FP_MM7;
  QWORD FP_MM7_H;
  QWORD XMM0;
  QWORD XMM0_H;
  QWORD XMM1;
  QWORD XMM1_H;
  QWORD XMM2;
  QWORD XMM2_H;
  QWORD XMM3;
  QWORD XMM3_H;
  QWORD XMM4;
  QWORD XMM4_H;
  QWORD XMM5;
  QWORD XMM5_H;
  QWORD XMM6;
  QWORD XMM6_H;
  QWORD XMM7;
  QWORD XMM7_H;
  QWORD XMM8;
  QWORD XMM8_H;
  QWORD XMM9;
  QWORD XMM9_H;
  QWORD XMM10;
  QWORD XMM10_H;
  QWORD XMM11;
  QWORD XMM11_H;
  QWORD XMM12;
  QWORD XMM12_H;
  QWORD XMM13;
  QWORD XMM13_H;
  QWORD XMM14;
  QWORD XMM14_H;
  QWORD XMM15;
  QWORD XMM15_H;
  QWORD res1;
  QWORD res1_H;
  QWORD res2;
  QWORD res2_H;
  QWORD res3;
  QWORD res3_H;
  QWORD res4;
  QWORD res4_H;
  QWORD res5;
  QWORD res5_H;
  QWORD res6;
  QWORD res6_H;
} FXSAVE64, *PFXSAVE64;


typedef struct _pageevent_basic
{
  QWORD VirtualAddress;
  QWORD PhysicalAddress;
  QWORD CR3; //in case of kernel or other process
  QWORD FSBASE;
  QWORD GSBASE;
  QWORD GSBASE_KERNEL;
  QWORD FLAGS;
  QWORD RAX;
  QWORD RBX;
  QWORD RCX;
  QWORD RDX;
  QWORD RSI;
  QWORD RDI;
  QWORD R8;
  QWORD R9;
  QWORD R10;
  QWORD R11;
  QWORD R12;
  QWORD R13;
  QWORD R14;
  QWORD R15;
  QWORD RBP;
  QWORD RSP;
  QWORD RIP;
  QWORD DR0;
  QWORD DR1;
  QWORD DR2;
  QWORD DR3;
  QWORD DR6;
  QWORD DR7;
  WORD CS;
  WORD DS;
  WORD ES;
  WORD SS;
  WORD FS;
  WORD GS;
  DWORD Count; //number of times this block has been seen, or heartbeat when used for internal dbvm bp
} PageEventBasic, *PPageEventBasic;

typedef struct _pageevent_extended
{
  PageEventBasic basic;
  FXSAVE64 fpudata;
} PageEventExtended, *PPageEventExtended;

typedef struct _pageevent_basic_withstack
{
  PageEventBasic basic;
  unsigned char stack[4096];
} PageEventBasicWithStack, *PPageEventBasicWithStack;

typedef struct _pageevent_extended_withstack
{
  PageEventBasic basic;
  FXSAVE64 fpudata;
  unsigned char stack[4096];
} PageEventExtendedWithStack, *PPageEventExtendedWithStack;

#define PE_BASIC 0
#define PE_EXTENDED 1
#define PE_BASICSTACK 2
#define PE_EXTENDEDSTACK 3

typedef struct _pageeventlistdescriptor
{
  DWORD ID;
  DWORD maxNumberOfEntries;
  DWORD numberOfEntries;
  DWORD missedEntries; //number of entries missed because the list was full
  DWORD entryType; //0=PageEventBasic, 1=PageEventExtended, 2=PageEventBasicWithStack, 3=PageEventExtendedWithStack
  union
  {
    PageEventBasic basic[0];
    PageEventExtended extended[0];
    PageEventBasicWithStack basics[0];
    PageEventExtendedWithStack extendeds[0];
  } pe;

} PageEventListDescriptor, *PPageEventListDescriptor;

typedef struct
{
  QWORD PhysicalAddress;
  int Size;
  int Type; //0=write, 1=access,2=execute
  DWORD Options;
  QWORD LoopUserMode;
  QWORD LoopKernelMode;
  int Active;
  int CopyInProgress; //if 1 events will be ignored
  PPageEventListDescriptor Log;
} EPTWatchEntry, *PEPTWatchEntry;

typedef struct
{
  QWORD PhysicalAddressExecutable; //the PA of the original page and used for execute
  QWORD PhysicalAddressData; //the PA of the page shown when read/write operations happen
  void *Data;
  void *Executable;
  QWORD CloakMode;
  union
  {
    PEPT_PTE eptentry[0]; //for every cpu hold the ept entry (PTE_PAE entry on AMD)
    PPTE_PAE npentry[0];
  };

  //debug info
  BYTE InvokingCPU;
  BYTE LastWritingCPU;
} CloakedPageData, *PCloakedPageData;


typedef struct
{
  int Active;
  PCloakedPageData cloakdata;
  QWORD PhysicalAddress;
  unsigned char originalbyte;
  CHANGEREGONBPINFO changereginfo;
} ChangeRegBPEntry, *PChangeRegBPEntry;

typedef struct
{
  int triggered; //set to true if it has been started
  QWORD triggeredcr3;
  QWORD triggeredfsbase;
  QWORD triggeredgsbase;
  int count; //number of steps left to log

  int shouldquit;
  int finished;
  QWORD PhysicalAddress;
  unsigned char originalbyte;
  PCloakedPageData cloakdata; //needed to disable it

  //copy from here to the client
  int datatype;
  int numberOfEntries;
  union
  {
    PageEventBasic basic[0];
    PageEventExtended extended[0];
    PageEventBasicWithStack basics[0];
    PageEventExtendedWithStack extendeds[0];
  } pe;

} TraceOnBPEntry, *PTraceOnBPEntry;

typedef struct
{
  int inuse;//1 if this entry contains a thread
  int continueMethod; //0=no, 1=single step, 2=run  (resets to 0 after taking a step.  if 2 then inuse turns false
  int watchid; //the watchid that caused the break.  -1 if it's a single step

  QWORD UserModeLoop; //where to go to after a step
  QWORD KernelModeLoop;

  PageEventExtended state; //contains CR3, FSBASE and GSBASE
} BrokenThreadEntry, *PBrokenThreadEntry;


#endif /* VMM_EPTSTRUCTS_H_ */
