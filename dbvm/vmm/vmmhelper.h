#ifndef VMMHELPER_H_
#define VMMHELPER_H_

#include "common.h"
#include "vmreadwrite.h"
#include "vmxcontrolstructures.h"
#include "eptstructs.h"
#include "exports.h"


extern int vmxstartup;
extern int vmxstartup_end;

extern volatile unsigned char *MSRBitmap;

extern unsigned long long vmmstart;
extern volatile unsigned long long nextstack;
extern PPDPTE_PAE pagedirlvl4;

int debugmode,breakpointset;
int globals_have_been_configured;


//global int 1 redirection vars
int int1redirection;
int int1redirection_idtbypass;
UINT64 int1redirection_idtbypass_cs;
UINT64 int1redirection_idtbypass_rip;

int int3redirection;
int int3redirection_idtbypass;
UINT64 int3redirection_idtbypass_cs;
UINT64 int3redirection_idtbypass_rip;

int int14redirection;
int int14redirection_idtbypass;
UINT64 int14redirection_idtbypass_cs;
UINT64 int14redirection_idtbypass_rip;


typedef struct _vmregisters /* DO NOT CHANGE THIS ORDER */
{
  UINT64         r15;
  UINT64         r14;
  UINT64         r13;
  UINT64         r12;
  UINT64         r11;
  UINT64         r10;
  UINT64         r9;
  UINT64         r8;
  UINT64         rbp;
  UINT64         rsi;
  UINT64         rdi;
  UINT64         rdx;
  UINT64         rcx;
  UINT64         rbx;
  UINT64         rax; //not valid in AMD
} VMRegisters;

#ifdef DEBUG
typedef struct _vmstate
{
  VMRegisters registers;
  UINT64 rsp;
  UINT64 rip;
  UINT64 rflags;
  WORD es;
  WORD cs;
  WORD ss;
  WORD ds;
  WORD fs;
  WORD gs;
  WORD ldtr;
  WORD tr;

  UINT64 es_base;
  UINT64 cs_base;
  UINT64 ss_base;
  UINT64 ds_base;
  UINT64 fs_base;
  UINT64 gs_base;
  UINT64 ldtr_base;
  UINT64 tr_base;

  unsigned long long efer;

  DWORD exit_reason;
  DWORD exit_interruptioninfo;
  DWORD exit_interruptionerror;
  DWORD idtvector_information;
  DWORD idtvector_error;

  //exit
  WORD exit_cs;
  WORD exit_ss;
  UINT64 exit_cs_base;
  UINT64 exit_ss_base;
  UINT64 exit_rip;
  UINT64 exit_rsp;
  UINT64 exit_rflags;

} VMState, *PVMState;

VMState vmstates[4];
int vmstates_pos;

#endif

typedef struct _vmxhoststate //structure for easy management of hoststates
{
  WORD ES; //0xc00
  WORD CS; //0xc02
  WORD SS; //0xc04
  WORD DS; //0xc06
  WORD FS; //0xc08
  WORD GS; //0xc0a
  WORD TR; //0xc0c

  QWORD IA32_PAT; //0x2C00
  QWORD IA32_EFER; //0x2C02
  QWORD IA32_PERF_GLOBAL_CTRL;//0x2c04

  DWORD IA32_SYSENTER_CS; //0x4c00

  QWORD CR0; //0x6c00
  QWORD CR3; //0x6c02
  QWORD CR4; //0x6c04

  QWORD FS_BASE; //0x6c06
  QWORD GS_BASE; //0x6c08
  QWORD TR_BASE; //0x6c0a
  QWORD GDTR_BASE; //0x6c0c ( limit will be 0xffff)
  QWORD IDTR_BASE; //0x6c0e

  QWORD IA32_SYSENTER_ESP; //0x6c10
  QWORD IA32_SYSENTER_EIP; //0x6c12

  QWORD RSP; //0x6c14
  QWORD RIP; //0x6c16
} vmxhoststate, *pvmxhoststate;


typedef struct _EXITINTINFO
{
  BYTE Vector;
  BYTE Type: 3;
  BYTE ErrorCodeValid: 1;
  DWORD Zero: 19;
  BYTE Valid: 1;
  DWORD ErrorCode;
}  __attribute__((__packed__)) EXITINTINFO, *PEXITINTINFO;

typedef volatile struct _vmcb
{
	WORD InterceptCR0_15Read;
	WORD InterceptCR0_15Write;
	WORD InterceptDR0_15Read;
	WORD InterceptDR0_15Write;
	DWORD InterceptExceptions;
	union{
	  DWORD InstructionIntercept1;
	  struct {
	    unsigned InterceptINTR       :1;
      unsigned InterceptNMI        :1;
      unsigned InterceptSMI        :1;
      unsigned InterceptINIT       :1;
      unsigned InterceptVINTR      :1;
      unsigned InterceptCR0WritesThatChangeTSorMP        :1;
      unsigned InterceptIDTRRead       :1;
      unsigned InterceptGDTRRead       :1;
      unsigned InterceptLDTRRead       :1;
      unsigned InterceptTRRead         :1;
      unsigned InterceptIDTRWrite       :1;
      unsigned InterceptGDTRWrite       :1;
      unsigned InterceptLDTRWrite       :1;
      unsigned InterceptTRWrite       :1;
      unsigned InterceptRDTSC       :1;
      unsigned InterceptRDPMC       :1;

      unsigned InterceptPUSHF       :1;
      unsigned InterceptPOPF       :1;
      unsigned InterceptCPUID       :1;
      unsigned InterceptRSM       :1;
      unsigned InterceptIRET       :1;
      unsigned InterceptINT       :1;
      unsigned InterceptINVD       :1;
      unsigned InterceptPAUSE       :1;
      unsigned InterceptHLT       :1;
      unsigned InterceptINVLPG       :1;
      unsigned InterceptINVLPGA       :1;
      unsigned IOIO_PROT      :1;
      unsigned MSR_PROT       :1;
      unsigned InterceptTaskSwitches       :1;
      unsigned FERR_FREEZE       :1;
      unsigned InterceptShutdown       :1;

	  };
	};

	union{
	  DWORD InstructionIntercept2;
	  struct {
	    unsigned InterceptVMRUN      :1;
      unsigned InterceptVMMCALL        :1;
      unsigned InterceptVMLOAD        :1;
      unsigned InterceptVMSAVE      :1;
      unsigned InterceptSTGI      :1;
      unsigned InterceptCLGI       :1;
      unsigned InterceptSKINIT       :1;
      unsigned InterceptRDTSCP       :1;
      unsigned InterceptICEBP       :1;
      unsigned InterceptWBINVD         :1;
      unsigned InterceptMONITOR       :1;
      unsigned InterceptMWAIT       :1;
      unsigned InterceptMWAIT_IfArmed      :1;
      unsigned InterceptXSETBV       :1;
	  };
	};

	BYTE reserved1[40];
	WORD  PauseFilterThreshold; //3c = correct
	WORD  PauseFilterCount;
	QWORD IOPM_BASE_PA; //physical base address of IOPM
	QWORD MSRPM_BASE_PA;
	QWORD TSC_OFFSET;
	DWORD GuestASID; //58=correct
	BYTE  TLB_CONTROL;
	BYTE  reserved2[3];
	BYTE  V_TPR;  //60=correct
	unsigned V_IRQ : 1;
	unsigned reserved4: 7;
	unsigned V_INTR_PRIO: 4;
	unsigned V_IGN_TPR: 1;
	unsigned reserved5: 3;
	unsigned V_INTR_MASKING : 1;
	unsigned reserved6: 7;
	BYTE  V_INTR_VECTOR; //64=correct
	unsigned reserved6_1: 24;


	union{
	  QWORD InterruptShadow;
	  struct {
	    unsigned INTERRUPT_SHADOW :1;
	  };
	};

	QWORD EXITCODE; //70 correct
	QWORD EXITINFO1;
	QWORD EXITINFO2;
	QWORD EXITINTINFO;

	union{
	  QWORD Enable_Nested_Paging;  //90 correct
	  struct {
	    unsigned NP_ENABLE :1;
	  };
	};

	BYTE reserved7[16];

	//a8
	union {
	  QWORD EVENTINJ;
	  struct {
	    unsigned inject_Vector : 8;
	    unsigned inject_Type : 3;
	    unsigned inject_EV : 1;
	    unsigned reserved: 19;
	    unsigned inject_Valid: 1;
	    DWORD    inject_ERRORCODE;
	  };
	};
	QWORD N_CR3; //nested paging

	union{
	  QWORD Enable_LBR_Virtualization;
	  struct {
	    unsigned LBR_VIRTUALIZATION_ENABLE: 1;
	  };
	};
	DWORD VMCB_CLEAN_BITS;
	DWORD reserved8;

	QWORD nRIP;  //c8
	BYTE NumberOfBytesFetched;
	BYTE GuestInstructionBytes[15];
	//E0
	BYTE reserved9[800];

	//400:
    //State Save Area
    WORD  es_selector;
    WORD  es_attrib;
    DWORD es_limit;
    QWORD es_base;

    WORD  cs_selector;
    WORD  cs_attrib;
    DWORD cs_limit;
    QWORD cs_base;

    WORD  ss_selector;
    WORD  ss_attrib;
    DWORD ss_limit;
    QWORD ss_base;

    WORD  ds_selector;
    WORD  ds_attrib;
    DWORD ds_limit;
    QWORD ds_base;

    WORD  fs_selector;
    WORD  fs_attrib;
    DWORD fs_limit;
    QWORD fs_base;

    WORD  gs_selector;
    WORD  gs_attrib;
    DWORD gs_limit;
    QWORD gs_base;

    WORD  gdtr_selector;
    WORD  gdtr_attrib;
    DWORD gdtr_limit;
    QWORD gdtr_base;

    WORD  ldtr_selector;
    WORD  ldtr_attrib;
    DWORD ldtr_limit;
    QWORD ldtr_base;

    WORD  idtr_selector;
    WORD  idtr_attrib;
    DWORD idtr_limit;
    QWORD idtr_base;

    WORD  tr_selector;
    WORD  tr_attrib;
    DWORD tr_limit;
    QWORD tr_base;

    BYTE  reserved10[43];
    BYTE  CPL;
    DWORD reserved11;
    QWORD EFER;

    BYTE reserved12[112];
    QWORD CR4;
    QWORD CR3;
    QWORD CR0;
    QWORD DR7;
    QWORD DR6;
    QWORD RFLAGS;
    QWORD RIP; //0x578

    BYTE reserved13[88];
    QWORD RSP; //5d8

    BYTE reserved14[24];  //5e0
    QWORD RAX; //5f8
    QWORD STAR; //600
    QWORD LSTAR; //608
    QWORD CSTAR; //610
    QWORD SFMASK; //618
    QWORD KernelGsBase; //620
    QWORD SYSENTER_CS; //628
    QWORD SYSENTER_ESP; //630
    QWORD SYSENTER_EIP; //638
    QWORD CR2; //640

    BYTE reserved15[32]; //648
    QWORD G_PAT;  //0x668
    QWORD DBGCTL;
    QWORD BR_FROM;
    QWORD BR_TO;
    QWORD LASTEXCPFROM;
    QWORD LASTEXCPTO; //690


} __attribute__((__packed__)) vmcb, *pvmcb;

typedef struct _singlestepreason
{
  int Reason;
      //1=EPT watch event single step (Restore page back to non write/ non  read)
      //2=EPT cloak event (Put the old executable page back and mark as non read/write)
      //3=change reg on bp event (restored the int3 bp (0xcc))


  int ID; //index of the array used for this reason (watchlist, cloaklist, changeregonbplist) (About to become obsolete and replaced by the data pointer)

  void* Data; //pointer to the object for this reason (watchlist, cloaklist, changeregonbplist)
} SingleStepReason, *PSingleStepReason;

typedef volatile struct tcpuinfo
{
  volatile struct tcpuinfo *self; //pointer to itself (must be offset 0)
  volatile struct tcpuinfo *next; //must be offset 0x8
  DWORD guest_error; //must be offset 0x10
  DWORD cpunr; //must be offset 0x14
  QWORD lasttsc; //must be offset 0x18
  QWORD totaltsctaken; //must be offset 0x20
  QWORD lastTSCTouch;
  QWORD lowestTSC;
  DWORD active;
  DWORD apicid;

  DWORD hasIF;
  UINT64 guestCR0;
  UINT64 guestCR3; //realmode emu
  char  isboot;
  char  command;
  char  vmxsetup;
  char  invalidcs;


  int   int1happened; //set if it has just redirected a interrupt, cleared by vmcall query command
  int   int3happened; //'  '  '
  int   int14happened; //'  '  '

  /* the TS elements are used for transition from protected mode to real and back, since that changes the TS temporarily */
  WORD  TSsegment;
  UINT64 TSbase;
  DWORD TSlimit;
  DWORD TSaccessRights;



  unsigned long long sysenter_CS; //what the guest sees
  unsigned long long sysenter_EIP;
  unsigned long long sysenter_ESP;

  int   hidden_sysenter_modification;
  unsigned long long actual_sysenter_CS; //what is actually is
  unsigned long long actual_sysenter_EIP;
  unsigned long long actual_sysenter_ESP;
  unsigned long long efer;


  void *vmcb_host;
  pvmcb vmcb; //AMD's virtual machine control_block. Give the physical address of this to VMRUN
  UINT64 vmcb_PA;

  UINT64 guest_VM_HSAVE_PA; //the current VM_HSAVE_PA according to the guest



  void* vmxon_region;
  void* vmcs_region;
  UINT64 vmcs_regionPA;

  PPTE_PAE mappagetables;

  UINT64 Previous_Interuptability_State; //used for the block/unblock interrupts vmcall
  int    Previous_CLI;



  struct {
    UINT64 IDTBase;
    WORD IDTLimit;
    UINT64 GDTBase;
    WORD GDTLimit;
  } RealMode;

  struct {
    int cr3_change_callback; //=1 when a callback has been registered
    int called_callback;

    int calling_convention;
    WORD callback_cs;
    WORD callback_ss;
    UINT64 callback_rip;
    UINT64 callback_rsp;

    UINT64 rip;

    //saved state before the callback as made
    UINT64 rax;
    UINT64 rbx;
    UINT64 rcx;
    UINT64 rdx;
    UINT64 rsi;
    UINT64 rdi;
    UINT64 rbp;
    UINT64 rsp;
    UINT64 r8;
    UINT64 r9;
    UINT64 r10;
    UINT64 r11;
    UINT64 r12;
    UINT64 r13;
    UINT64 r14;
    UINT64 r15;

    UINT64 rflags;
    UINT64 interruptability_state;

    ULONG cs_selector;
    ULONG cs_limit;
    ULONG cs_base;
    ULONG cs_accessrights;

    ULONG ss_selector;
    ULONG ss_limit;
    ULONG ss_base;
    ULONG ss_accessrights;

    ULONG ds_selector;
    ULONG ds_limit;
    ULONG ds_base;
    ULONG ds_accessrights;

    ULONG es_selector;
    ULONG es_limit;
    ULONG es_base;
    ULONG es_accessrights;

    ULONG fs_selector;
    ULONG fs_limit;
    ULONG fs_base;
    ULONG fs_accessrights;

    ULONG gs_selector;
    ULONG gs_limit;
    ULONG gs_base;
    ULONG gs_accessrights;

    UINT64 newcr3;
    int changedcr3;
  } cr3_callback;

  struct
  {
    int Active; //set to 1 when active
    QWORD CR3; //Holds the CR3 value to watch taskswitch to and from
    QWORD DEBUGCTL; //Holds the DebugCTL value to set when inside the target process
    QWORD DS_AREA; //Holds the DS_AREA to set when
    QWORD OriginalDebugCTL; //When inside the target process this holds the debugctl that was set before entering. Return this on readMSR (and set with writeMSR when inside the process)
    QWORD OriginalDS_AREA; //When inside the target process this holds the DS_AREA that was set before entering. Return this with readMSR ('''')

#ifdef ULTIMAPDEBUG
    QWORD CR3_switchcount;
    QWORD CR3_switchcount2;
    QWORD LastOldCR3;
    QWORD LastNewCR3;
#endif
  } Ultimap;

  struct
  {
      int Active;
      QWORD LastIgnoredPageFault;
  } IgnorePageFaults;


  //Host-only fields:
  //field for exception handling
  struct
  {
      QWORD RIP;  //if set go to this address on return from any interrupt
      QWORD RBP;
      QWORD RSP;
  } OnInterrupt; //obsolete , switch over to OnException

  jmp_buf OnException;
  QWORD LastExceptionRIP;


  unsigned char LastInterrupt;
  unsigned char LastInterruptHasErrorcode;
  WORD LastInterruptErrorcode;

  int NMIOccured; //set to not 0 if an NMI triggered while inside the VMX event handler (this means that before vmresume, raise the NMI interrupt)

  struct //extra data about guest state VMX features (every cpu gets their own)
  {
    int insideVMXRootMode;
    QWORD guest_vmxonaddress;
    QWORD guest_activeVMCS; //the VMCS the guest thinks it is. (usually the same with some modification)
    //saved hoststate (used by handleByGuest)

    int currenterrorcode; //if not 0, return this errorcode on vmread
    vmxhoststate originalhoststate;
    vmxhoststate dbvmhoststate;
    int runningvmx; //1 if the previous call was a vmlaunch/vmresume and no vmexit happened yet

  } vmxdata;

  QWORD EPTPML4;
  criticalSection EPTPML4CS; // since other cpu's can map in pages for other cpu's as well, use a CS
  /*
  PEPT_PTE *eptCloakList; //pointer to the EPT entry of the index related to CloakedPages
  int eptCloakListLength;
  */
  int eptCloak_LastOperationWasWrite;
  QWORD eptCloak_LastWriteOffset;

  PEPT_PTE *eptWatchList; //pointer to the EPT entry of the index related to the WatchList
  int eptWatchListLength;
  int eptUpdated;


  struct
  {
    CloakedPageData *ActiveRegion; //if not null this contains the current page that is executable
    QWORD LastCloakedVirtualBase;
  } NP_Cloak; //AMD cloaking


  struct //single stepping data
  {
    int Method;
    int PreviousTFState;
    QWORD PreviousEFER; //AMD single step
    QWORD PreviousFMASK;
    int LastInstructionWasSyscall;//AMD single step

    SingleStepReason *Reasons;
    int ReasonsPos;
    int ReasonsLength;

  } singleStepping;

  int BPAfterStep;
  int BPCausedByDBVM; //gets read out by ce's driver to see if the bp was because of DBVM or not

#ifdef STATISTICS
  int eventcounter[56];
#endif

  struct {
    UINT64 RFLAGS, CR4;
    WORD CS, SS;
  } SwitchKernel;

  int LastVMCall;
  int insideHandler;

} tcpuinfo, *pcpuinfo; //allocated when the number of cpu's is known

typedef struct
{
  union
  {
    ULONG	Selectorvalue;
	  struct {
      ULONG	RPL      : 2;
      ULONG TI       : 1;
      ULONG	Selector : 29;
		};
	};
} Selector_Field, *PSelector_Field;


typedef struct _regCR0
{
  union{
    unsigned long long CR0;
    struct {
      unsigned PE         :1; //0
      unsigned MP         :1; //1
      unsigned EM         :1; //2
      unsigned TS         :1; //3
      unsigned ET         :1; //4
      unsigned NE         :1; //5
      unsigned reserved1  :10; //6-15
      unsigned WP         :1; //16
      unsigned reserved2  :1; //17
      unsigned AM         :1; //18
      unsigned reserved   :10; //19-28
      unsigned NW         :1; //29
      unsigned CD         :1; //30
      unsigned PG         :1; //31
    };
  };
} __attribute__((__packed__)) regCR0,*PregCR0;

typedef struct _regCR4
{
  union{
    unsigned long long CR4;
    struct {
      unsigned VME        :1;
      unsigned PVI        :1;
      unsigned TSD        :1;
      unsigned DE         :1;
      unsigned PSE        :1;
      unsigned PAE        :1;
      unsigned MCE        :1;
      unsigned PGE        :1;
      unsigned PCE        :1;
      unsigned OSFXSR     :1;
      unsigned OSXMMEXCPT :1;
      unsigned reserved   :2;
      unsigned VMXE       :1;
      UINT64   reserved2  :50;
    };
  };
} __attribute__((__packed__)) regCR4,*PregCR4;

//PAE setting bits(when used without regCR4)
#define CR4_VME         (1<<0)
#define CR4_PVI         (1<<1)
#define CR4_TSD         (1<<2)
#define CR4_DE          (1<<3)
#define CR4_PSE         (1<<4)
#define CR4_PAE         (1<<5)
#define CR4_MCE         (1<<6)
#define CR4_PGE         (1<<7)
#define CR4_PCE         (1<<8)
#define CR4_OSFXSR      (1<<9)
#define CR4_OSXMMEXCPT  (1<<10)
#define CR4_VMXE        (1<<13)
#define CR4_SMXE        (1<<14)
#define CR4_FSGSBASE    (1<<16)
#define CR4_PCIDE       (1<<17)
#define CR4_OSXSAVE     (1<<18)
#define CR4_SMEP		(1<<20)
#define CR4_SMAP		(1<<21)

#define CR0_PE          (1<<0)
#define CR0_NE          (1<<5)
#define CR0_WP          (1<<16)
#define CR0_PG          (1<<31)


typedef struct _regDR6
{
  union{
    unsigned long long DR6;
    struct {
      unsigned B0        :1;
      unsigned B1        :1;
      unsigned B2        :1;
      unsigned B3        :1;
      unsigned Reserved  :9;
      unsigned BD        :1;
      unsigned BS        :1;
      unsigned BT        :1;
      unsigned RTM       :1;
    };
  };
} __attribute__((__packed__)) regDR6,*PregDR6;

typedef struct _regDR7
{
  union{
    unsigned long long DR7;
    struct {
      unsigned L0        :1; //0
      unsigned G0        :1; //1
      unsigned L1        :1; //2
      unsigned G1        :1; //3
      unsigned L2        :1; //4
      unsigned G2        :1; //5
      unsigned L3        :1; //6
      unsigned G3        :1; //7
      unsigned LE        :1; //8
      unsigned GE        :1; //9
      unsigned reserved  :3; //001  //10-11-12
      unsigned GD        :1; //13...
      unsigned reserved2 :2; //00
      unsigned RW0       :2;
      unsigned LEN0      :2;
      unsigned RW1       :2;
      unsigned LEN1      :2;
      unsigned RW2       :2;
      unsigned LEN2      :2;
      unsigned RW3       :2;
      unsigned LEN3      :2;
      unsigned reserved3 :32;
    };
  };
} __attribute__((__packed__)) regDR7,*PregDR7;


typedef struct _pferrorcode /* errorcode returned for pagefaults */
{
  union{
    unsigned long long errorcode;
    struct {
      unsigned P          :1;  /* 0=non present page, 1=page level protection violation */
      unsigned W          :1;  /* 0=read exception, 1=write exception */
      unsigned US         :1;  /* 0=processor was in supervisor mode, 1=processor was in usermode */
      unsigned RSVD       :1;  /* 0=Not caused by reserved bits set, 1=Caused because reserved bits are set */
      unsigned ID         :1;  /* 0=Not caused by an instruction fetch, 1=Caused by an instruction fetch */
      unsigned reserved   :27;
    };
  };
} __attribute__((__packed__)) PFerrorcode,*PPFerrorcode;


UINT64 IA32_VMX_CR0_FIXED0,IA32_VMX_CR0_FIXED1;
UINT64 IA32_VMX_CR4_FIXED0,IA32_VMX_CR4_FIXED1;
TIA32_VMX_BASIC IA32_VMX_BASIC;
UINT64 IA32_VMX_PINBASED_CTLS;
UINT64 IA32_VMX_PROCBASED_CTLS;
UINT64 IA32_VMX_SECONDARY_PROCBASED_CTLS;
UINT64 IA32_VMX_EXIT_CTLS;
UINT64 IA32_VMX_ENTRY_CTLS;
TIA32_VMX_MISC IA32_VMX_MISC;

extern void SaveExtraHostState(UINT64 VMCB_PA);

void CheckGuest(void);
void displayVMmemory(pcpuinfo currentcpuinfo);
void displayPhysicalMemory();
void setupTSS8086(void);

void launchVMX(pcpuinfo currentcpuinfo);
int vmexit(tcpuinfo *cpu, UINT64 *registers, void *fxsave);
int vmexit_amd(pcpuinfo currentcpuinfo, UINT64 *registers, void *fxsave);

void sendvmstate(pcpuinfo currentcpuinfo, VMRegisters *registers);
char *getVMInstructionErrorString(void);

void ShowCurrentInstruction(pcpuinfo currentcpuinfo);
void ShowCurrentInstructions(pcpuinfo currentcpuinfo);
void displayPreviousStates(void);

int isDebugFault(QWORD dr6, QWORD dr7);


int ISREALMODE(pcpuinfo currentcpuinfo);
int IS64BITPAGING(pcpuinfo currentcpuinfo);
int IS64BITCODE(pcpuinfo currentcpuinfo);
int ISPAGING(pcpuinfo currentcpuinfo);

extern volatile DWORD vmmentrycount;
extern volatile DWORD initcs;

int APStartsInSIPI;
extern pcpuinfo getcpuinfo();


typedef BOOL DBVM_PLUGIN_EXIT_PRE(PDBVMExports exports, pcpuinfo currentcpuinfo, void *registers, void *fxsave);
typedef void DBVM_PLUGIN_EXIT_POST(PDBVMExports exports, pcpuinfo currentcpuinfo, void *registers, void *fxsave, int *DBVMResult);

extern DBVM_PLUGIN_EXIT_PRE *dbvm_plugin_exit_pre;
extern DBVM_PLUGIN_EXIT_POST *dbvm_plugin_exit_post;




#endif /*VMMHELPER_H_*/
