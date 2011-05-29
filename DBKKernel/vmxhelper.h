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


typedef enum {virt_differentInterrupt=0, virt_emulateInterrupt=1} VMXInterruptRedirectType;

unsigned int vmcall(void *vmcallinfo, unsigned int level1pass);

unsigned int vmx_getversion();
unsigned int vmx_getRealCR0();
unsigned int vmx_getRealCR3();
unsigned int vmx_getRealCR4();

unsigned int vmx_redirect_interrupt1(VMXInterruptRedirectType redirecttype, unsigned int newintvector, unsigned int int1cs, UINT_PTR int1eip);
unsigned int vmx_redirect_interrupt3(VMXInterruptRedirectType redirecttype, unsigned int newintvector, unsigned int int3cs, UINT_PTR int3eip);
unsigned int vmx_redirect_interrupt14(VMXInterruptRedirectType redirecttype, unsigned int newintvector, unsigned int int14cs, UINT_PTR int14eip);

unsigned int vmx_register_cr3_callback(unsigned int cs, unsigned int eip, unsigned int ss, unsigned int esp);
unsigned int vmx_exit_cr3_callback(unsigned int newcr3);

unsigned int vmx_ultimap(UINT_PTR cr3towatch, UINT64 debugctl_value, void *storeaddress);
unsigned int vmx_ultimap_disable();

unsigned int vmxusable;
unsigned int vmx_password1;
unsigned int vmx_password2;
unsigned int vmx_version;

#endif