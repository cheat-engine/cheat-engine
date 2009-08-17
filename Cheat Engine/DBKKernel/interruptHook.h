#ifndef INTERRUPTHOOK_H
#define INTERRUPTHOOK_H

#include <windef.h>

#pragma pack(1) //allignemnt of 1 byte
typedef struct tagINT_VECTOR
{
	WORD	wLowOffset;
	WORD	wSelector;
	BYTE	bUnused;
	BYTE    bAccessFlags;
	
	/*
	unsigned gatetype  : 3; //101=Task, 110=interrupt, 111=trap
	unsigned gatesize  : 1; //1=32bit, 0=16bit
	unsigned zero      : 1;
	unsigned DPL       : 2;
	unsigned P         : 1;
	*/
	WORD	wHighOffset;
} INT_VECTOR, *PINT_VECTOR;
#pragma pack()

#pragma pack(2) //allignemnt of 1 byte
typedef struct tagIDT
{    
    WORD wLimit;
	PINT_VECTOR vector;
} IDT, *PIDT;
#pragma pack()

int inthook_HookInterrupt(unsigned char intnr, int newCS, ULONG_PTR newEIP);
int inthook_UnhookInterrupt(unsigned char intnr);
int inthook_isHooked(unsigned char intnr);
int inthook_isDBVMHook(unsigned char intnr);
ULONG_PTR inthook_getOriginalEIP(unsigned char intnr);
WORD inthook_getOriginalCS(unsigned char intnr);

#endif