#include "ntifs.h"
#include <windef.h>

#include "DBKFunc.h"
#include "vmxhelper.h"

#include "interruptHook.h"

//this sourcefile only so no need to worry about it being modified by out of context code
struct 
{
	int hooked;
	int dbvmInterruptEmulation; //if used, originalCS and originalEIP are ignored and the current IDT data is used to resume the interrupt, currently only for interrupt 1 and 14
	WORD originalCS;
	ULONG_PTR originalEIP;
} InterruptHook[256]; 


WORD inthook_getOriginalCS(unsigned char intnr)
{
	return InterruptHook[intnr].originalCS;
}

ULONG_PTR inthook_getOriginalEIP(unsigned char intnr)
{
	return InterruptHook[intnr].originalEIP;
}

int inthook_isHooked(unsigned char intnr)
{
	return InterruptHook[intnr].hooked;
}

int inthook_isDBVMHook(unsigned char intnr)
{
	return InterruptHook[intnr].dbvmInterruptEmulation;
}

int inthook_UnhookInterrupt(unsigned char intnr)
{
	if (InterruptHook[intnr].hooked)
	{
		//it's hooked, try to unhook
		if (InterruptHook[intnr].dbvmInterruptEmulation)
		{
			if (intnr==1)
				vmx_redirect_interrupt1(0, 1, 0, 0);
			else
				vmx_redirect_interrupt14(0, 14, 0, 0); 

			return TRUE; //that's all we need
		}

		//still here so not a dbvm hook, unhook the old way and hope nothing has interfered

		{
			INT_VECTOR newVector;
			
			newVector.bAccessFlags=0x8e;
			newVector.bUnused=0;
			/*
			newVector.gatetype=6; //interrupt gate
			newVector.gatesize=1; //32-bit
			newVector.zero=0;
			newVector.DPL=0;
			newVector.P=1;
			*/
			newVector.wHighOffset=(WORD)((DWORD)(InterruptHook[intnr].originalEIP >> 16));
			newVector.wLowOffset=(WORD)InterruptHook[intnr].originalEIP;
			newVector.wSelector=(WORD)InterruptHook[intnr].originalCS;

			{
				IDT idt;	
				GetIDT(&idt);

				__asm{CLI} //no kernelmode taskswitches please
				idt.vector[intnr]=newVector;
				__asm{STI}
			}
		}		
	}

	return TRUE;
}

int inthook_HookInterrupt(unsigned char intnr, int newCS, ULONG_PTR newEIP)
{
	IDT idt;	
	GetIDT(&idt);
	DbgPrint("inthook_HookInterrupt\n");

	if (!InterruptHook[intnr].hooked)
	{
		//new hook, so save the originals
		InterruptHook[intnr].originalCS=idt.vector[intnr].wSelector;
		InterruptHook[intnr].originalEIP=idt.vector[intnr].wLowOffset+(idt.vector[intnr].wHighOffset << 16);
	}

	if (vmxusable && ((intnr==1) || (intnr==14)) )
	{		
		if (intnr==1)
		  vmx_redirect_interrupt1(1, 0, newCS, newEIP);
		else
		  vmx_redirect_interrupt14(1, 0, newCS, newEIP);

		InterruptHook[intnr].dbvmInterruptEmulation=1;
	}
	else
	{
		//old fashioned hook
		INT_VECTOR newVector;

		DbgPrint("sizeof newVector=%d\n",sizeof(INT_VECTOR));
		
		
		newVector.wHighOffset=(WORD)((DWORD)(newEIP >> 16));
		newVector.wLowOffset=(WORD)newEIP;
		newVector.wSelector=(WORD)newCS;
		newVector.bUnused=0;
		newVector.bAccessFlags=0x8e;
		/*
		newVector.gatetype=6; //interrupt gate
		newVector.gatesize=1; //32-bit
		newVector.zero=0;
		newVector.DPL=0;
		newVector.P=1;
		*/

		__asm{CLI} //no kernelmode taskswitches please
		idt.vector[intnr]=newVector;
		__asm{STI}

		InterruptHook[intnr].dbvmInterruptEmulation=0;
	}

	InterruptHook[intnr].hooked=1;
	return TRUE;
}
