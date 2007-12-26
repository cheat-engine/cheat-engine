#ifndef VMXHELPER_H
#define VMXHELPER_H

#define VMCALL_GETVERSION		0
#define VMCALL_CHANGEPASSWORD	1
#define VMCALL_REDIRECTINT1		9
#define VMCALL_INT1REDIRECTED	10
#define VMCALL_CHANGESELECTORS	12
#define VMCALL_BLOCK_INTERRUPTS 13
#define VMCALL_RESTORE_INTERRUPTS 14

#define VMCALL_REGISTER_CR3_EDIT_CALLBACK 16
#define VMCALL_RETURN_FROM_CR3_EDIT_CALLBACK 17
#define VMCALL_GETCR0			18
#define VMCALL_GETCR3			19
#define VMCALL_GETCR4			20


unsigned int vmcall(void *vmcallinfo, unsigned int level1pass);

unsigned int vmx_getversion();
unsigned int vmx_getRealCR0();
unsigned int vmx_getRealCR3();
unsigned int vmx_getRealCR4();

unsigned int vmx_redirect_interrupt1(unsigned int redirecttype, unsigned int newintvector, unsigned int int1cs, unsigned int int1eip);

unsigned int vmx_register_cr3_callback(unsigned int cs, unsigned int eip, unsigned int ss, unsigned int esp);
unsigned int vmx_exit_cr3_callback(unsigned int newcr3);

unsigned int vmxusable;
unsigned int vmx_password1;
unsigned int vmx_password2;
unsigned int vmx_version;

#endif