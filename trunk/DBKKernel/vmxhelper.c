#pragma warning( disable: 4103)
#include "ntddk.h"
#include <windef.h>
#include "vmxhelper.h"

#ifdef AMD64
extern UINT_PTR dovmcall_intel(void *vmcallinfo, unsigned int level1pass);
extern UINT_PTR dovmcall_amd(void *vmcallinfo, unsigned int level1pass);
//dovmcall is defined in vmxhelpera.asm
#else
_declspec( naked ) UINT_PTR dovmcall_intel(void *vmcallinfo, unsigned int level1pass)
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

_declspec( naked ) UINT_PTR dovmcall_amd(void *vmcallinfo, unsigned int level1pass)
{
	__asm
	{
		push edx
		mov eax,[esp+8]  //not +4 because of that push, retard
		mov edx,[esp+12]
		__emit 0x0f
		__emit 0x01
	    __emit 0xd9 //vmmcall, eax will be edited, or a UD exception will be raised
		pop edx
		ret 8
	}
}
#endif

typedef UINT_PTR (DOVMCALL) (void *vmcallinfo, unsigned int level1pass);
DOVMCALL *dovmcall;


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
	return (int)dovmcall(&vmcallinfo, vmx_password1);
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

	DbgPrint("vmx_getversion()\n");

	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_GETVERSION;

	return (unsigned int)dovmcall(&vmcallinfo, vmx_password1);
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

	return (unsigned int)dovmcall(&vmcallinfo, vmx_password1);
}

UINT_PTR vmx_getRealCR3()
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

	return dovmcall(&vmcallinfo, vmx_password1);
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

	return (unsigned int)dovmcall(&vmcallinfo, vmx_password1);
}

unsigned int vmx_redirect_interrupt1(VMXInterruptRedirectType redirecttype, unsigned int newintvector, unsigned int int1cs, UINT_PTR int1eip)
{
	#pragma pack(1)
	struct
	{
		unsigned int structsize;
		unsigned int level2pass;
		unsigned int command;
		unsigned int redirecttype;
		unsigned int newintvector;
		UINT64 int1eip;
		unsigned int int1cs;
	} vmcallinfo;
	#pragma pack()

	DbgPrint("vmx_redirect_interrupt1: redirecttype=%d int1cs=%x int1eip=%llx sizeof(vmcallinfo)=%x\n", redirecttype, int1cs, int1eip, sizeof(vmcallinfo));
	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_REDIRECTINT1;
	vmcallinfo.redirecttype=redirecttype;
	vmcallinfo.newintvector=newintvector;
	vmcallinfo.int1eip=int1eip;
	vmcallinfo.int1cs=int1cs;

	return (unsigned int)dovmcall(&vmcallinfo, vmx_password1);
}

unsigned int vmx_redirect_interrupt3(VMXInterruptRedirectType redirecttype, unsigned int newintvector, unsigned int int3cs, UINT_PTR int3eip)
{
	#pragma pack(1)
	struct
	{
		unsigned int structsize;
		unsigned int level2pass;
		unsigned int command;
		unsigned int redirecttype;
		unsigned int newintvector;
		unsigned long long int3eip;
		unsigned int int3cs;
	} vmcallinfo;
	#pragma pack()

	DbgPrint("vmx_redirect_interrupt3: int3cs=%x int3eip=%x sizeof(vmcallinfo)=%x\n", int3cs, int3eip, sizeof(vmcallinfo));
	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_REDIRECTINT3;
	vmcallinfo.redirecttype=redirecttype;
	vmcallinfo.newintvector=newintvector;
	vmcallinfo.int3eip=int3eip;
	vmcallinfo.int3cs=int3cs;

	return (unsigned int)dovmcall(&vmcallinfo, vmx_password1);
}


unsigned int vmx_redirect_interrupt14(VMXInterruptRedirectType redirecttype, unsigned int newintvector, unsigned int int14cs, UINT_PTR int14eip)
{
	#pragma pack(1)
	struct
	{
		unsigned int structsize;
		unsigned int level2pass;
		unsigned int command;
		unsigned int redirecttype;
		unsigned int newintvector;
		unsigned long long int14eip;
		unsigned int int14cs;
	} vmcallinfo;
	#pragma pack()

	DbgPrint("vmx_redirect_interrupt14: int14cs=%x int14eip=%x sizeof(vmcallinfo)=%x\n", int14cs, int14eip, sizeof(vmcallinfo));
	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_REDIRECTINT14;
	vmcallinfo.redirecttype=redirecttype;
	vmcallinfo.newintvector=newintvector;
	vmcallinfo.int14eip=int14eip;
	vmcallinfo.int14cs=int14cs;

	return (unsigned int)dovmcall(&vmcallinfo, vmx_password1);
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

	return (unsigned int)dovmcall(&vmcallinfo, vmx_password1);
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

	return (unsigned int)dovmcall(&vmcallinfo, vmx_password1);
}

unsigned int vmx_ultimap_getDebugInfo(PULTIMAPDEBUGINFO debuginfo)
{
	#pragma pack(1)
	struct
	{
		unsigned int structsize;
		unsigned int level2pass;
		unsigned int command;	
		ULTIMAPDEBUGINFO debuginfo;
	} vmcallinfo;
	#pragma pack()

	unsigned int i;

	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_ULTIMAP_DEBUGINFO;

	i=(unsigned int)dovmcall(&vmcallinfo, vmx_password1);
	*debuginfo=vmcallinfo.debuginfo;

	return i;
}

unsigned int vmx_ultimap(UINT_PTR cr3towatch, UINT64 debugctl_value, void *storeaddress)
{
	#pragma pack(1)
	struct
	{
		unsigned int structsize;
		unsigned int level2pass;
		unsigned int command;
		UINT64 cr3;
		UINT64 debugctl;
		UINT64 storeaddress;		
	} vmcallinfo;
	#pragma pack()

	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_ULTIMAP;
	vmcallinfo.cr3=(UINT64)cr3towatch;
	vmcallinfo.debugctl=(UINT64)debugctl_value;
	vmcallinfo.storeaddress=(UINT64)(UINT_PTR)storeaddress;

	DbgPrint("vmx_ultimap(%I64x, %I64x, %I64x)\n", (UINT64)vmcallinfo.cr3, (UINT64)vmcallinfo.debugctl, vmcallinfo.storeaddress);
	

	return (unsigned int)dovmcall(&vmcallinfo, vmx_password1);
}

unsigned int vmx_ultimap_disable()
{
	#pragma pack(1)
	struct
	{
		unsigned int structsize;
		unsigned int level2pass;
		unsigned int command;
	} vmcallinfo;
	#pragma pack()

	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_ULTIMAP_DISABLE;

	return (unsigned int)dovmcall(&vmcallinfo, vmx_password1);
}

unsigned int vmx_disable_dataPageFaults()
{
	#pragma pack(1)
	struct
	{
		unsigned int structsize;
		unsigned int level2pass;
		unsigned int command;
	} vmcallinfo;
	#pragma pack()

	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_DISABLE_DATAPAGEFAULTS;

	return (unsigned int)dovmcall(&vmcallinfo, vmx_password1);
}

unsigned int vmx_enable_dataPageFaults()
{
	#pragma pack(1)
	struct
	{
		unsigned int structsize;
		unsigned int level2pass;
		unsigned int command;
	} vmcallinfo;
	#pragma pack()

	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_ENABLE_DATAPAGEFAULTS;

	return (unsigned int)dovmcall(&vmcallinfo, vmx_password1);
}

UINT_PTR vmx_getLastSkippedPageFault()
{
	#pragma pack(1)
	struct
	{
		unsigned int structsize;
		unsigned int level2pass;
		unsigned int command;
	} vmcallinfo;
	#pragma pack()

	vmcallinfo.structsize=sizeof(vmcallinfo);
	vmcallinfo.level2pass=vmx_password2;
	vmcallinfo.command=VMCALL_GETLASTSKIPPEDPAGEFAULT;

	return (UINT_PTR)dovmcall(&vmcallinfo, vmx_password1);
}

void vmx_init_dovmcall(int isIntel)
{
	if (isIntel)
		(void *)dovmcall=(void *)dovmcall_intel;
	else
		(void *)dovmcall=(void *)dovmcall_amd;

}
