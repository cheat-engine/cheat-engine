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

//secondary processor based vm-execution flags
#define SPBEF_ENABLE_RDTSCP         (1<<3)
#define SPBEF_ENABLE_INVPCID        (1<<12)
#define SPBEF_ENABLE_XSAVES         (1<<20)

//vm-exit controls
#define SAVE_DEBUG_CONTROLS           (1<<2)
#define HOST_ADDRESS_SPACE_SIZE       (1<<9)
#define ACKNOWLEDGE_INTERRUPT_ON_EXIT (1<<15)

//vm-entry controls
#define RESTORE_DEBUG_CONTROLS              (1<<2)
#define IA32E_MODE_GUEST                    (1<<9)
#define ENTRY_TO_SMM                        (1<<10)
#define DEACTIVATE_DUALMANAGEMENT_TREATMENT (1<<11)


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
#define vm_exit_cr_access  28
#define vm_exit_io_access  30
#define vm_exit_rdmsr      31
#define vm_exit_wrmsr      32
#define vm_exit_vmx_preemptiontimer_reachedzero  52
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
      ULONG reserved2   :10;  //54-63
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



#endif /* VMXCONTROLSTRUCTURES_H_ */
