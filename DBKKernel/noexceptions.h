#ifndef NOEXCEPTIONS_H
#define NOEXCEPTIONS_H

/*
Will be responsible for temporarily switching out the IDT of the curent CPU with one that doesn't call windows functions on errors
*/
#include <ntifs.h>
#include <wdm.h>
#include <windef.h>

#include "interruptHook.h"
#include "dbkfunc.h"


typedef struct
{
	KIRQL entryIRQL;
	PINT_VECTOR NoExceptionVectorList; //list pointing to an idt table with hooked ints
	IDT OriginalIDT;	
	IDT ModdedIDT;
} CPUSTATE, *PCPUSTATE;

BOOL NoExceptions_Enter();
int  NoExceptions_CopyMemory(PVOID Destination, PVOID Source, int size);
void NoExceptions_Leave();

void NoExceptions_Cleanup();

void NoException14_ErrorHandler();

#endif
