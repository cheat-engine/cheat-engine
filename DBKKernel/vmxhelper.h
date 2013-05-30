#ifndef VMXHELPER_H
#define VMXHELPER_H

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

unsigned int vmx_ultimap_getDebugInfo(PULTIMAPDEBUGINFO debuginfo);

unsigned int vmxusable;
unsigned int vmx_password1;
unsigned int vmx_password2;
unsigned int vmx_version;

UINT_PTR vmx_getLastSkippedPageFault();
unsigned int vmx_enable_dataPageFaults();
unsigned int vmx_disable_dataPageFaults();

void vmx_init_dovmcall(int isIntel);

#endif