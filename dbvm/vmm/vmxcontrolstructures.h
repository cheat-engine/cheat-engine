/*
 * vmxcontrolstructures.h
 *
 *  Created on: Sep 7, 2009
 *      Author: erich
 */

#ifndef VMXCONTROLSTRUCTURES_H_
#define VMXCONTROLSTRUCTURES_H_

#include "common.h"

//interruptabilitystate
#define BLOCKINGBYSTI (1<<0)
#define BLOCKINGBYSS  (1<<1)
#define BLOCKINGBYSMI (1<<2)
#define BLOCKINGBYNMI (1<<3)

//pin-based vm-execution controls
#define EXTERNAL_INTERRUPT_EXITING    (1<<0)
#define NMI_EXITING                   (1<<3)
#define VIRTUAL_NMIS                  (1<<5)
#define ACTIVATE_VMX_PREEMPTION_TIMER (1<<6)

//processor based vm-execution controls
#define INTERRUPT_WINDOW_EXITING  (1<<2)
#define USE_TSC_OFFSETTING        (1<<3)
#define HLT_EXITING               (1<<7)
#define INVLPG_EXITING            (1<<9)
#define MWAIT_EXITING             (1<<10)
#define RDPMC_EXITING             (1<<11)
#define RDTSC_EXITING             (1<<12)
#define CR8LOAD_EXITING           (1<<19)
#define CR8STORE_EXITING          (1<<20)
#define USE_TPR_SHADOW            (1<<21)
#define MOV_DR_EXITING            (1<<23)
#define UNCONDITIONAL_IO_EXITING  (1<<24)
#define USE_IO_BITMAPS            (1<<25)
#define USE_MSR_BITMAPS           (1<<28)
#define MONITOR_EXITING           (1<<29)
#define PAUSE_EXITING             (1<<30)
#define SECONDARY_EXECUTION_CONTROLS (1<<31)

//secondary processor based vm-execution flags
#define SPBEF_ENABLE_EPT            (1<<1)
#define SPBEF_ENABLE_RDTSCP         (1<<3)
#define SPBEF_ENABLE_UNRESTRICTED   (1<<7)
#define SPBEF_ENABLE_INVPCID        (1<<12)
#define SPBEF_ENABLE_XSAVES         (1<<20)

//vm-exit controls
#define VMEXITC_SAVE_DEBUG_CONTROLS           (1<<2)
#define VMEXITC_HOST_ADDRESS_SPACE_SIZE       (1<<9)
#define VMEXITC_ACKNOWLEDGE_INTERRUPT_ON_EXIT (1<<15)
#define VMEXITC_SAVE_IA32_EFER        (1<<20)
#define VMEXITC_LOAD_IA32_EFER        (1<<21)

//vm-entry controls
#define VMENTRYC_RESTORE_DEBUG_CONTROLS              (1<<2)
#define VMENTRYC_IA32E_MODE_GUEST                    (1<<9)
#define VMENTRYC_ENTRY_TO_SMM                        (1<<10)
#define VMENTRYC_DEACTIVATE_DUALMANAGEMENT_TREATMENT (1<<11)
#define VMENTRYC_LOAD_IA32_EFER                      (1<<15)



//IA32_FEATURE_CONTROL bits
#define FEATURE_CONTROL_LOCK  		(1<<0)
#define FEATURE_CONTROL_VMXON_SMX 	(1<<1)
#define FEATURE_CONTROL_VMXON 		(1<<2)

#define vm_exit_interrupt   0
#define vm_exit_sipi        4
#define vm_exit_taskswitch  9
#define vm_exit_cpuid      10
#define vm_exit_invlpg     14
#define vm_exit_vmcall     18
#define vm_exit_vmptrld    21
#define vm_exit_vmread     23
#define vm_exit_vmresume   24
#define vm_exit_vmwrite    25
#define vm_exit_cr_access  28
#define vm_exit_io_access  30
#define vm_exit_rdmsr      31
#define vm_exit_wrmsr      32
#define vm_exit_ept_violation 48
#define vm_exit_ept_misconfiguration 49
#define vm_exit_vmx_preemptiontimer_reachedzero  52
#define vm_exit_xsetbv    55
#define vm_exit_invalid_guest_state  0x80000021


typedef struct
{
  DWORD RevisionID;
  DWORD VMXAbortIndicator;
  DWORD VMCSData[(4096 - 8) / 4];
}__attribute__((__packed__)) VMCSRegion, *PVMCSRegion;



typedef struct
{
  union {
    ULONG AccessRights;
    struct {
      ULONG Segment_type : 4; //0-3
      ULONG S            : 1; //4
      ULONG DPL          : 2; //5-6
      ULONG P            : 1; //7
      ULONG reserved     : 4; //8-11
      ULONG AVL          : 1; //12
      ULONG L            : 1; //13 //reserved for anything except CS
      ULONG D_B          : 1; //14
      ULONG G            : 1; //15
      ULONG unusable     : 1; //16
      ULONG reserved_2   : 15;
    };
  };
} Access_Rights, *PAccess_Rights;

typedef union
{
  UINT64 pendingDebugExceptions;
  struct {
    unsigned B0                 :1;
    unsigned B1                 :1;
    unsigned B2                 :1;
    unsigned B3                 :1;
    unsigned reserved           :8; //must be 0
    unsigned EnabledBreakpoint  :1; //When set, this bit indicates that at least one Data or I/O breakpoint was met and enabled in DR7
    unsigned reserved2          :1; //must be 0
    unsigned BS                 :1; //indicates that a single step bp would have been triggered
    UINT64   reserved3          :49; //must be 0
  } binary;
}__attribute__((__packed__)) PendingDebugExceptions, *PPendingDebugExceptions;

typedef struct _vmexit_interruption_information
{
  union {
    ULONG interruption_information;
    struct {
      ULONG interruptvector : 8;
      ULONG type            : 3;
      ULONG haserrorcode    : 1;
      ULONG ireterror       : 1;
      ULONG reserved0       : 18;
      ULONG valid           : 1;
    };
  };
} __attribute__((__packed__)) VMExit_interruption_information;

typedef struct _vmexit_idt_vector_information
{
  union {
    ULONG idtvector_info;
    struct {
      ULONG interruptvector : 8;
      ULONG type            : 3;
      ULONG haserrorcode    : 1;
      ULONG undefined       : 1;
      ULONG reserved0       : 18;
      ULONG valid           : 1;
    };
  };
} __attribute__((__packed__)) VMExit_idt_vector_information;


typedef struct _vmentry_interruption_information
{
  union {
    ULONG interruption_information;
    struct {
      ULONG interruptvector : 8;
      ULONG type            : 3;
      ULONG haserrorcode    : 1;
      ULONG reserved0       : 19;
      ULONG valid           : 1;
    };
  };
} __attribute__((__packed__)) VMEntry_interruption_information;

typedef struct _IA32_VMX_BASIC
{
  union{
    unsigned long long IA32_VMX_BASIC;
    struct {
      ULONG rev_id      :32; //0-31
      ULONG VMXON_size  :13; //32-44
      ULONG reserved1   :3;  //45-47
      ULONG addresswidth:1;  //48
      ULONG dual_monitor:1;  //49
      ULONG memtype     :4;  //50-53
      ULONG insoutsinfo :1; //54
      ULONG default1canbe0 : 1;
      ULONG reserved2   :8;  //56-63
    };
  };
} __attribute__((__packed__)) TIA32_VMX_BASIC;

typedef struct _IA32_VMX_MISC
{
  union{
    unsigned long long IA32_VMX_MISC;
    struct {
      ULONG vmx_premption_timer_tsc_relation: 5; //0-4
      ULONG storeslma: 1; //5
      ULONG activity_HLT:1; //6
      ULONG activity_shutdown: 1; //7
      ULONG activity_waitforsipi: 1; //8
      ULONG reserved2           : 7; //9-15
      ULONG maxcr3targets       : 9; //16-24
      ULONG maxmsrsavelist      : 3; //25-27
      ULONG reserved3           : 4; //28-31
      ULONG MSEG_revid          : 32;
    };
  };
} __attribute__((__packed__)) TIA32_VMX_MISC;

typedef struct _IA32_VMX_VPID_EPT_CAP
{
  union{
    unsigned long long IA32_VMX_VPID_EPT_CAP;
    struct {
      ULONG EPT_executeOnlySupport        : 1; //0
      ULONG reserved1                     : 5; //1-5
      ULONG EPT_pagewalklength4           : 1; //6
      ULONG reserved2                     : 1; //7
      ULONG EPT_uncachableSupport         : 1; //8
      ULONG reserved3                     : 5; //9-13
      ULONG EPT_writeBackSupport          : 1; //14
      ULONG reserved4                     : 1; //15
      ULONG EPT_2MBSupport                : 1; //16
      ULONG EPT_1GBSupport                : 1; //17
      ULONG reserved5                     : 2; //18-19
      ULONG EPT_INVEPTSupport             : 1; //20
      ULONG EPT_ADSupport                 : 1; //21
      ULONG EPT_AdvancedExitInfo          : 1; //22
      ULONG reserved6                     : 2; //23-24
      ULONG EPT_INVEPTSingleContext       : 1; //25
      ULONG EPT_INVEPTAllContext          : 1; //26
      ULONG reserved7                     : 5; //27-31
      //VPID
      ULONG VPID_INVVPIDSupport           : 1; //32
      ULONG reserved8                     : 7; //33-39
      ULONG VPID_INVVPIDIndividualAddress : 1; //40
      ULONG VPID_INVVPIDSingleContext     : 1; //41
      ULONG VPID_INVVPIDAllContext        : 1; //42
      ULONG VPID_INVVPIDSingleContextRetainingGlobals : 1; //43
      ULONG reserved9                     : 20; //44-63
    };
  };
} __attribute__((__packed__)) TIA32_VMX_VPID_EPT_CAP, *PIA32_VMX_VPID_EPT_CAP;

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


#endif /* VMXCONTROLSTRUCTURES_H_ */
