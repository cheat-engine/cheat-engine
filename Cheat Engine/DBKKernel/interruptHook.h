#ifndef INTERRUPTHOOK_H
#define INTERRUPTHOOK_H

#include <windef.h>

//assuming the standard interrupt hook routine is used this stackindex will be valid
typedef enum {si_gs=-12, si_fs=-11, si_es=-10, si_ds=-9, si_edi=-8, si_esi=-7, si_stack_ebp=-6, si_stack_esp=-5, si_ebx=-4, si_edx=-3, si_ecx=-2, si_eax=-1, si_ebp=0, si_errorcode=1, si_eip=2, si_cs=3, si_eflags=4, si_esp=5, si_ss=6} stackindex;



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
#ifdef AMD64
	DWORD	TopOffset;
	DWORD	Reserved;
#endif
} INT_VECTOR, *PINT_VECTOR;
#pragma pack()

#pragma pack(2) //allignemnt of 2 byte
typedef struct tagIDT
{    
    WORD wLimit;
	PINT_VECTOR vector;
} IDT, *PIDT;
#pragma pack()


#ifdef AMD64
typedef
#pragma pack(1) //allignemnt of 1 byte
struct
{    
	UINT64 eip;
	UINT64 cs;
} JUMPBACK, *PJUMPBACK;
#pragma pack()
#else
typedef
#pragma pack(1) //allignemnt of 1 byte
struct
{    
	DWORD eip;
	WORD cs;
} JUMPBACK, *PJUMPBACK;
#pragma pack()
#endif


int inthook_HookInterrupt(unsigned char intnr, int newCS, ULONG_PTR newEIP, PJUMPBACK jumpback);
int inthook_UnhookInterrupt(unsigned char intnr);
int inthook_isHooked(unsigned char intnr);
int inthook_isDBVMHook(unsigned char intnr);
ULONG_PTR inthook_getOriginalEIP(unsigned char intnr);
WORD inthook_getOriginalCS(unsigned char intnr);




#endif