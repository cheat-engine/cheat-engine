#ifndef VMCALL_H_
#define VMCALL_H_

#include "main.h"

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


//dbvm v7
//not yet implemented (lost since last hd crash)
#define VMCALL_SWITCH_TO_KERNELMODE 30
#define VMCALL_DISABLE_DATAPAGEFAULTS 31
#define VMCALL_ENABLE_DATAPAGEFAULTS 32
#define VMCALL_GETLASTSKIPPEDPAGEFAULT 33

#define VMCALL_ULTIMAP_PAUSE 34
#define VMCALL_ULTIMAP_RESUME 35

#define VMCALL_ULTIMAP_DEBUGINFO 36

#define VMCALL_PSODTEST 37



int handleVMCall(pcpuinfo currentcpuinfo, VMRegisters *vmregisters);

void returnFromCR3Callback(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, unsigned long long newcr3);
QWORD readMSRSafe(pcpuinfo currentcpuinfo, DWORD msr);

int raiseInvalidOpcodeException(pcpuinfo currentcpuinfo); //

#endif /*VMCALL_H_*/
