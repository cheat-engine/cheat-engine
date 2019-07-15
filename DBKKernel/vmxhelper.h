#ifndef VMXHELPER_H
#define VMXHELPER_H

#pragma warning( disable: 4200)

#define VMCALL_GETVERSION 0
#define VMCALL_CHANGEPASSWORD 1
#define VMCALL_READ_PHYSICAL_MEMORY 3
#define VMCALL_WRITE_PHYSICAL_MEMORY 4
#define VMCALL_REDIRECTINT1 9
#define VMCALL_INT1REDIRECTED 10
#define VMCALL_CHANGESELECTORS 12
#define VMCALL_BLOCK_INTERRUPTS 13
#define VMCALL_RESTORE_INTERRUPTS 14

#define VMCALL_REGISTER_CR3_EDIT_CALLBACK 16
#define VMCALL_RETURN_FROM_CR3_EDIT_CALLBACK 17
#define VMCALL_GETCR0 18
#define VMCALL_GETCR3 19
#define VMCALL_GETCR4 20
#define VMCALL_RAISEPRIVILEGE 21
#define VMCALL_REDIRECTINT14 22
#define VMCALL_INT14REDIRECTED 23
#define VMCALL_REDIRECTINT3 24
#define VMCALL_INT3REDIRECTED 25

//dbvm v6+
#define VMCALL_READMSR 26
#define VMCALL_WRITEMSR 27

#define VMCALL_ULTIMAP 28
#define VMCALL_ULTIMAP_DISABLE 29


//dbvm v7+
#define VMCALL_SWITCH_TO_KERNELMODE 30

#define VMCALL_DISABLE_DATAPAGEFAULTS 31
#define VMCALL_ENABLE_DATAPAGEFAULTS 32
#define VMCALL_GETLASTSKIPPEDPAGEFAULT 33

#define VMCALL_ULTIMAP_PAUSE 34
#define VMCALL_ULTIMAP_RESUME 35

#define VMCALL_ULTIMAP_DEBUGINFO 36


//dbvm v10+
#define VMCALL_WATCH_WRITES 41
#define VMCALL_WATCH_READS 41
#define VMCALL_WATCH_RETRIEVELOG 43
#define VMCALL_WATCH_DELETE 44

#define VMCALL_CLOAK_ACTIVATE 45
#define VMCALL_CLOAK_DEACTIVATE 46
#define VMCALL_CLOAK_READORIGINAL 47
#define VMCALL_CLOAK_WRITEORIGINAL 48

#define VMCALL_CLOAK_CHANGEREGONBP 49

#define VMCALL_ADD_MEMORY 57

#define VMCALL_CAUSEDDEBUGBREAK 63


typedef UINT64 QWORD;

typedef struct _CHANGEREGONBPINFO
{
	struct
	{
		unsigned changeRAX : 1;
		unsigned changeRBX : 1;
		unsigned changeRCX : 1;
		unsigned changeRDX : 1;
		unsigned changeRSI : 1;
		unsigned changeRDI : 1;
		unsigned changeRBP : 1;
		unsigned changeRSP : 1;
		unsigned changeRIP : 1;
		unsigned changeR8 : 1;
		unsigned changeR9 : 1;
		unsigned changeR10 : 1;
		unsigned changeR11 : 1;
		unsigned changeR12 : 1;
		unsigned changeR13 : 1;
		unsigned changeR14 : 1;
		unsigned changeR15 : 1;
		//flags reg:
		unsigned changeCF : 1;
		unsigned changePF : 1;
		unsigned changeAF : 1;
		unsigned changeZF : 1;
		unsigned changeSF : 1;
		unsigned changeOF : 1;
		unsigned newCF : 1;
		unsigned newPF : 1;
		unsigned newAF : 1;
		unsigned newZF : 1;
		unsigned newSF : 1;
		unsigned newOF : 1;
		unsigned reserved : 3;
	} Flags;

	QWORD newRAX;
	QWORD newRBX;
	QWORD newRCX;
	QWORD newRDX;
	QWORD newRSI;
	QWORD newRDI;
	QWORD newRBP;
	QWORD newRSP;
	QWORD newRIP;
	QWORD newR8;
	QWORD newR9;
	QWORD newR10;
	QWORD newR11;
	QWORD newR12;
	QWORD newR13;
	QWORD newR14;
	QWORD newR15;
} CHANGEREGONBPINFO, *PCHANGEREGONBPINFO;

typedef struct _pageevent_basic
{
	QWORD VirtualAddress;
	QWORD PhysicalAddress;
	QWORD CR3; //in case of kernel or other process
	QWORD FSBASE;
	QWORD GSBASE;
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
	WORD CS;
	WORD DS;
	WORD ES;
	WORD SS;
	WORD FS;
	WORD GS;
	DWORD Count;
} PageEventBasic, *PPageEventBasic;

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

typedef struct _pageeventlistdescriptor
{
	DWORD ID;
	DWORD maxSize;
	DWORD numberOfEntries;
	DWORD missedEntries;
	DWORD entryType; //0=PageEventBasic, 1=PageEventExtended, 2=PageEventBasicWithStack, 3=PageEventExtendedWithStack
	union
	{
		PageEventBasic basic[0];
		PageEventExtended extended[0];
		PageEventBasicWithStack basics[0];
		PageEventExtendedWithStack extendeds[0];
	} pe;

} PageEventListDescriptor, *PPageEventListDescriptor;


typedef enum {virt_differentInterrupt=0, virt_emulateInterrupt=1} VMXInterruptRedirectType;

typedef struct
{
	UINT64 Active; //set to 1 when active
	UINT64 CR3; //Holds the CR3 value to watch taskswitch to and from
	UINT64 DEBUGCTL; //Holds the DebugCTL value to set when inside the target process
	UINT64 DS_AREA; //Holds the DS_AREA to set when
	UINT64 OriginalDebugCTL; //When inside the target process this holds the debugctl that was set before entering. Return this on readMSR (and set with writeMSR when inside the process)
	UINT64 OriginalDS_AREA; //When inside the target process this holds the DS_AREA that was set before entering. Return this with readMSR ('''')
	UINT64 CR3_switchcount;
	UINT64 CR3_switchcount2;
	UINT64 LastOldCR3;
	UINT64 LastNewCR3;
} ULTIMAPDEBUGINFO, *PULTIMAPDEBUGINFO;


unsigned int vmcall(void *vmcallinfo, unsigned int level1pass);

unsigned int vmx_getversion();
unsigned int vmx_getRealCR0();
UINT_PTR vmx_getRealCR3();
unsigned int vmx_getRealCR4();

unsigned int vmx_redirect_interrupt1(VMXInterruptRedirectType redirecttype, unsigned int newintvector, unsigned int int1cs, UINT_PTR int1eip);
unsigned int vmx_redirect_interrupt3(VMXInterruptRedirectType redirecttype, unsigned int newintvector, unsigned int int3cs, UINT_PTR int3eip);
unsigned int vmx_redirect_interrupt14(VMXInterruptRedirectType redirecttype, unsigned int newintvector, unsigned int int14cs, UINT_PTR int14eip);

unsigned int vmx_register_cr3_callback(unsigned int cs, unsigned int eip, unsigned int ss, unsigned int esp);
unsigned int vmx_exit_cr3_callback(unsigned int newcr3);

unsigned int vmx_ultimap(UINT_PTR cr3towatch, UINT64 debugctl_value, void *storeaddress);
unsigned int vmx_ultimap_disable();

unsigned int vmx_ultimap_pause();
unsigned int vmx_ultimap_resume();


unsigned int vmx_ultimap_getDebugInfo(PULTIMAPDEBUGINFO debuginfo);

unsigned int vmxusable;
unsigned int vmx_password1;
unsigned int vmx_password2;
unsigned int vmx_version;

UINT_PTR vmx_getLastSkippedPageFault();
unsigned int vmx_enable_dataPageFaults();
unsigned int vmx_disable_dataPageFaults();

unsigned int vmx_add_memory(UINT64 *list, int count);

int vmx_causedCurrentDebugBreak();

void vmx_init_dovmcall(int isIntel);

#endif