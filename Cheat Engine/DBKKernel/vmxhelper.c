#include "ntifs.h"
#include "vmxhelper.h"

_declspec( naked ) unsigned int vmcall(void *vmcallinfo, unsigned int level1pass)
{
	__asm
	{
		push edx
		mov eax,[esp+8]  //not +4 because of that push, retard
		mov edx,[esp+12]
		__emit 0x0f
		__emit 0x01
	    __emit 0xc1 //vmcall, eax will be edited, or a UD exception will be raised
		pop edx
		ret 8
	}
}

int vmx_hasredirectedint1()
{
	struct
	{
		unsigned int structsize;
		unsigned int level2pass;
		unsigned int command;
	} vmcallinfo;

	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_INT1REDIRECTED;
	return vmcall(&vmcallinfo, vmx_password1);
}

unsigned int vmx_getversion()
/*
This will either raise a unhandled opcode exception, or return the used dbvm version
*/
{
	struct
	{
		unsigned int structsize;
		unsigned int level2pass;
		unsigned int command;
	} vmcallinfo;

	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_GETVERSION;

	return vmcall(&vmcallinfo, vmx_password1);
}

unsigned int vmx_getRealCR0()
{
	struct
	{
		unsigned int structsize;
		unsigned int level2pass;
		unsigned int command;
	} vmcallinfo;

	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_GETCR0;

	return vmcall(&vmcallinfo, vmx_password1);
}

unsigned int vmx_getRealCR3()
{
	struct
	{
		unsigned int structsize;
		unsigned int level2pass;
		unsigned int command;
	} vmcallinfo;

	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_GETCR3;

	return vmcall(&vmcallinfo, vmx_password1);
}

unsigned int vmx_getRealCR4()
{
	struct
	{
		unsigned int structsize;
		unsigned int level2pass;
		unsigned int command;
	} vmcallinfo;

	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_GETCR4;

	return vmcall(&vmcallinfo, vmx_password1);
}

unsigned int vmx_redirect_interrupt1(unsigned int redirecttype, unsigned int newintvector, unsigned int int1cs, unsigned int int1eip)
{
	#pragma pack(1)
	struct
	{
		unsigned int structsize;
		unsigned int level2pass;
		unsigned int command;
		unsigned int redirecttype;
		unsigned int newintvector;
		unsigned long long int1eip;
		unsigned int int1cs;
	} vmcallinfo;
	#pragma pack()

	DbgPrint("vmx_redirect_interrupt1: int1cs=%x int1eip=%x sizeof(vmcallinfo)=%x\n", int1cs, int1eip, sizeof(vmcallinfo));
	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_REDIRECTINT1;
	vmcallinfo.redirecttype=redirecttype;
	vmcallinfo.newintvector=newintvector;
	vmcallinfo.int1eip=int1eip;
	vmcallinfo.int1cs=int1cs;

	return vmcall(&vmcallinfo, vmx_password1);
}

unsigned int vmx_register_cr3_callback(unsigned int cs, unsigned int eip, unsigned int ss, unsigned int esp)
{
	#pragma pack(1)
	struct
	{
		unsigned int structsize;
		unsigned int level2pass;
		unsigned int command;
		unsigned int callbacktype; //32-bit for this driver, so always 0
		unsigned long long callback_eip;
		unsigned int callback_cs;
		unsigned long long callback_esp;
		unsigned int callback_ss;
	} vmcallinfo;
	#pragma pack()

	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_REGISTER_CR3_EDIT_CALLBACK;
	vmcallinfo.callbacktype=0;
	vmcallinfo.callback_eip=eip;
	vmcallinfo.callback_cs=cs;
	vmcallinfo.callback_esp=esp;
	vmcallinfo.callback_ss=ss;

	return vmcall(&vmcallinfo, vmx_password1);
}

unsigned int vmx_exit_cr3_callback(unsigned int newcr3)
{
	#pragma pack(1)
	struct
	{
		unsigned int structsize;
		unsigned int level2pass;
		unsigned int command;
		unsigned long long newcr3;
	} vmcallinfo;
	#pragma pack()

	//DbgPrint("vmx_exit_cr3_callback(%x)\n",newcr3);

	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_RETURN_FROM_CR3_EDIT_CALLBACK;
	vmcallinfo.newcr3=newcr3;

	return vmcall(&vmcallinfo, vmx_password1);
}