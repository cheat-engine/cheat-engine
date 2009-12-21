#pragma warning( disable: 4103)

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
	if (InterruptHook[intnr].hooked)
	{
		//todo: add a check to see if the hook is still present. if not, return false and update the hooked value
		return TRUE;
	} else return FALSE;
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
		DbgPrint("cpu %d : interrupt %d is hooked\n",cpunr(),intnr);
		if (InterruptHook[intnr].dbvmInterruptEmulation)
		{
			if (intnr==1)
				vmx_redirect_interrupt1(virt_differentInterrupt, 1, 0, 0);
			else if (intnr==3)
				vmx_redirect_interrupt3(virt_differentInterrupt, 3, 0, 0); 
			else
				vmx_redirect_interrupt14(virt_differentInterrupt, 14, 0, 0); 

			return TRUE; //that's all we need
		}

#ifndef AMD64
		//still here so not a dbvm hook, unhook the old way and hope nothing has interfered

		{
			INT_VECTOR newVector;
			
			
			//newVector.bUnused=0;
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

				
				newVector.bAccessFlags=idt.vector[intnr].bAccessFlags;

				disableInterrupts();
				idt.vector[intnr]=newVector;
				enableInterrupts();				
			}

			DbgPrint("Restored\n");
		}
#endif
	}

	return TRUE;
}

int inthook_HookInterrupt(unsigned char intnr, int newCS, ULONG_PTR newEIP, PJUMPBACK jumpback)
{
	IDT idt;	
	GetIDT(&idt);
	DbgPrint("inthook_HookInterrupt for cpu %d (vmxusable=%d)\n",cpunr(), vmxusable);

	if (!InterruptHook[intnr].hooked)
	{
		//new hook, so save the originals
		InterruptHook[intnr].originalCS=idt.vector[intnr].wSelector;
		InterruptHook[intnr].originalEIP=idt.vector[intnr].wLowOffset+(idt.vector[intnr].wHighOffset << 16);
#ifdef AMD64
		InterruptHook[intnr].originalEIP+=(UINT64)((UINT64)idt.vector[intnr].TopOffset << 32);
#endif


	}

	if (jumpback)
	{
		jumpback->cs=InterruptHook[intnr].originalCS;
		jumpback->eip=InterruptHook[intnr].originalEIP;
	}

	if (vmxusable && ((intnr==1) || (intnr==3) || (intnr==14)) )
	{	
		
		switch (intnr)
		{
			case 1:
				vmx_redirect_interrupt1(virt_emulateInterrupt, 0, newCS, newEIP);
				break;

			case 3:
				vmx_redirect_interrupt3(virt_emulateInterrupt, 0, newCS, newEIP);
				break;

			case 14:
				vmx_redirect_interrupt14(virt_emulateInterrupt, 0, newCS, newEIP);
				break;
		}

		InterruptHook[intnr].dbvmInterruptEmulation=1;
	}
	else
	{
#ifdef AMD64
		DbgPrint("64-bit: DBVM is not loaded or a non dbvm hookable interrupt is being hooked\n");
		return FALSE;
#else
		//old fashioned hook
		INT_VECTOR newVector;

		DbgPrint("sizeof newVector=%d\n",sizeof(INT_VECTOR));
		
		
		newVector.wHighOffset=(WORD)((DWORD)(newEIP >> 16));
		newVector.wLowOffset=(WORD)newEIP;
		newVector.wSelector=(WORD)newCS;
		newVector.bUnused=0;
		newVector.bAccessFlags=idt.vector[intnr].bAccessFlags; //don't touch accessflag, the default settings are good (e.g: int3,4 and 8 have dpl=3)

		disableInterrupts(); //no kernelmode taskswitches please
		idt.vector[intnr]=newVector;
		enableInterrupts();	

		InterruptHook[intnr].dbvmInterruptEmulation=0;

		DbgPrint("int %d will now go to %x:%x\n",intnr, newCS, newEIP);
#endif
	}

	InterruptHook[intnr].hooked=1;
	return TRUE;
}

