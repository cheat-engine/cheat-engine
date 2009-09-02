//note: instead of int3 one could use the GDT and assign the current thread a segment selector set to the copy
//downside is there's only a limited number of available segments

#include "ntifs.h"
#include <windef.h>

#include "dbkfunc.h"
#include "interruptHook.h"
#include "stealthedit.h"
//hooks the pagefault handler


void interrupt3_asmentry( void );
void interrupt14_asmentry( void );

JUMPBACK Int3JumpBackLocation;
JUMPBACK Int14JumpBackLocation;


typedef struct
{
	unsigned P			: 1; //0 when not present, 1 when caused by protection flags
	unsigned WR			: 1; //0 when read, 1 when write
	unsigned US			: 1; //0 when pagefault caused by kernelmode, 1 when pagefault caused by usermode
	unsigned RSVD		: 1; //1 when caused by having reserved bits set in the pagetable
	unsigned ID			: 1; //1 when caused by an instruction fetch. <-- Most important one for stealthedit
} ErrorcodePF, *PErrorcodePF;




typedef struct
{
	DWORD ProcessID;
	DWORD pagebase;
	DWORD relocatedpagebase;
	int size;
} CloakedSection, *PCloakedSection;



struct
{
	PCloakedSection cs; //pointer to array of CloakedSection
	int size; //max size
	int pos; //current position for adding
} CloakedSections;
criticalSection CloakedSections_CS;

void stealthedit_initialize(void)
{
	RtlZeroMemory(&CloakedSections_CS,sizeof(criticalSection));	
	CloakedSections.cs=ExAllocatePoolWithTag(NonPagedPool, 256*sizeof(CloakedSection),0);
	if (CloakedSections.cs==NULL)
	{
		DbgPrint("Alert! Out of non paged memory for CloakedSections!");
		return;
	}

	CloakedSections.size=256;
	CloakedSections.pos=0;
}

int stealthedit_increaseBuffer(void)
{
	PCloakedSection newlist;
	PCloakedSection oldlist;
	int newsize;

	DbgPrint("stealthedit_increaseBuffer()\n");
	
	//create a new list
	newsize=CloakedSections.size*sizeof(CloakedSection)*2;
	newlist=ExAllocatePoolWithTag(NonPagedPool, newsize,0);
	if (newlist==NULL)
	{
		DbgPrint("Out of nonpaged memory\n");
		return FALSE;
	}

	oldlist=CloakedSections.cs;

	//copy the contents of the old list to the new one
	RtlCopyMemory(newlist, oldlist, CloakedSections.size*sizeof(CloakedSection));
	
	
	//assign new list to the structure
	csEnter(&CloakedSections_CS);
	CloakedSections.cs=newlist;
	CloakedSections.size=newsize;
	csLeave(&CloakedSections_CS);

	//free old list
	ExFreePool(CloakedSections.cs);
	return TRUE;
}

int stealthedit_AddCloakedSection(DWORD ProcessID, DWORD pagebase, DWORD relocatedpagebase, int size)
/*
size has to be in pagesizes (4096, 8192, etc...)
relocatedpagebase is the address where the real code starts
*/
{
	int i;

	DbgPrint("stealthedit_AddCloakedSection(%x, %x, %x, %d);\n",ProcessID,pagebase,relocatedpagebase, size);  
	
	//first check if it's already in, if so, adjust size
	for (i=0; i<CloakedSections.pos; i++)
	{
		if (CloakedSections.cs[i].pagebase==pagebase)
		{
			DbgPrint("Already present\n");
			csEnter(&CloakedSections_CS);
			CloakedSections.cs[i].relocatedpagebase=relocatedpagebase;
			CloakedSections.cs[i].size=size;
			csLeave(&CloakedSections_CS);
			return TRUE;
		}
	}

	//still here so a new one

	//make sure there's enough memory
	if (CloakedSections.pos>=CloakedSections.size)
		if (!stealthedit_increaseBuffer())
			return FALSE;
	
	DbgPrint("Registering at position %d\n",CloakedSections.pos);
	if (CloakedSections.cs)
	{
		CloakedSections.cs[CloakedSections.pos].ProcessID=ProcessID;
		CloakedSections.cs[CloakedSections.pos].pagebase=pagebase;
		CloakedSections.cs[CloakedSections.pos].relocatedpagebase=relocatedpagebase;
		CloakedSections.cs[CloakedSections.pos].size=size;
	}
	else
		DbgPrint("Failed to set because the fucking array has't been allocated goddamit!!!\n");
	 
	CloakedSections.pos++;

	return TRUE;

}

int stealthedit_RemoveCloakedSection(DWORD ProcessID, DWORD pagebase)
{
	int i,j;
	for (i=0; i<CloakedSections.pos; i++)
	{
		if ((CloakedSections.cs[i].ProcessID==ProcessID) && (CloakedSections.cs[i].pagebase==pagebase) )
		{
			//found it, now move all other items to the left
			csEnter(&CloakedSections_CS);

			DbgPrint("Removing relocation item %d\n",i);
			for (j=i; j<CloakedSections.pos-1; j++)
				CloakedSections.cs[j]=CloakedSections.cs[j+1];

			CloakedSections.pos--;			

			csLeave(&CloakedSections_CS);
			return TRUE;
		}
	}

	return FALSE;
}

int stealthedit_initStealthEditHooksForCurrentCPU(void)
{
	int result1=TRUE,result2=TRUE;
	DbgPrint("Hooking int14 for this cpu\n");	
	result1=inthook_HookInterrupt(14,0x8, (ULONG_PTR)interrupt14_asmentry, &Int14JumpBackLocation);	
	DbgPrint("result1=%d\n",result1);

	DbgPrint("Hooking int3 for this cpu\n");
	result2=inthook_HookInterrupt(3,0x8, (ULONG_PTR)interrupt3_asmentry, &Int3JumpBackLocation);	
	DbgPrint("result2=%d\n",result2);

	
	return (result1 && result2);
}

int interrupt3_centry(DWORD *stackpointer)
{
	DWORD currentPID=(DWORD)PsGetCurrentProcessId();
	int i;
	int handled=0;
	DWORD eip=stackpointer[si_eip]-1;

	DbgPrint("interrupt 3. PID=%x eip=%x\n", currentPID, eip);
	csEnter(&CloakedSections_CS);
	for (i=0; i<CloakedSections.pos; i++)
	{
		if (CloakedSections.cs[i].ProcessID==currentPID)
		{
			int offset;
			offset=eip-CloakedSections.cs[i].relocatedpagebase;
			DbgPrint("offset=%d\n",offset);

			if ((offset>-4096) && (offset<CloakedSections.cs[i].size+4096))
			{
				DbgPrint("Inside\n");
				DbgPrint("old eip=%x\n",stackpointer[si_eip]);
				stackpointer[si_eip]=CloakedSections.cs[i].pagebase+offset;				
				DbgPrint("new eip=%x\n",stackpointer[si_eip]);
			}

		}
	}

	csLeave(&CloakedSections_CS);

	return handled;
}


_declspec( naked ) void interrupt3_asmentry( void )
//This routine is called upon an interrupt 1, even before windows gets it
{
	__asm{ 
		
		push 0 //int3 doesn't have an errorcode, so push errorcode for the stackindex

		//save stack position
		push ebp
		mov ebp,esp

		//save state
		pushad
		push ds
		push es
		push fs
		push gs

		mov ax,0x23 //0x10 should work too, but even windows itself is using 0x23
		mov ds,ax
		mov es,ax
		mov gs,ax
		mov ax,0x30
		mov fs,ax

		push ebp
		call interrupt3_centry

		cmp eax,1	//set flag

		//restore state
		pop gs
		pop fs
		pop es
		pop ds
		popad

		pop ebp		

		je skip_original_int3
		add esp,4 //undo push errorcode
		jmp far [Int3JumpBackLocation]

skip_original_int3:
		add esp,4 //undo push errorcode
		iretd		
	}
}

int interrupt14_centry(DWORD *stackpointer)
{
	PErrorcodePF errorcode=(PErrorcodePF)&stackpointer[si_errorcode];
	int handled=0;

	if ((errorcode->P) && (errorcode->US) && (errorcode->ID))
	{
		int i;
		DWORD currentPID=(DWORD)PsGetCurrentProcessId();
		DWORD cr2=getCR2();

		//NO EXECUTE PAGEFAULT
		DbgPrint("interrupt14_centry for a NO EXECUTE PF: pid= %x, CR2=%x, errorcode(%x): P=%d WR=%d US=%d RSVD=%d ID=%d\n",currentPID, cr2, stackpointer[si_errorcode], errorcode->P, errorcode->WR, errorcode->US, errorcode->RSVD, errorcode->ID);

		//check if in one of my ranges
		//if so, change eip and don't tell windows
		csEnter(&CloakedSections_CS);
		for (i=0; i<CloakedSections.pos; i++)
		{
			if (CloakedSections.cs[i].ProcessID==currentPID)
			{
								
				if ((cr2>=CloakedSections.cs[i].pagebase) && (cr2<(CloakedSections.cs[i].pagebase+CloakedSections.cs[i].size)))
				{					
					DWORD eip=stackpointer[si_eip];					
					DWORD offset=eip-CloakedSections.cs[i].pagebase; //eip CAN be lower than pagebase on a pageboundary, but as long as DWORD's are used, it's safe enough (and assuming the usermode part has the first bytes of the instruction saved as well on the prologue region)

					DbgPrint("%d: No-execute in relocated region. EIP=%x\n",i,eip);
                    DbgPrint("%d: PID=%d pagebase=%x relocatedpagebase=%x size=%d\n",i,currentPID, CloakedSections.cs[i].pagebase, CloakedSections.cs[i].relocatedpagebase, CloakedSections.cs[i].size);
					stackpointer[si_eip]=CloakedSections.cs[i].relocatedpagebase+offset;
					
					DbgPrint("%d: Changing eip from %x to %x",i, eip,stackpointer[si_eip]);					
					handled=1;
					csLeave(&CloakedSections_CS); //unlock (I might be able to do a try/finally but not sure it'd work in this context)
					return 1;
				}
			}
		}

		csLeave(&CloakedSections_CS);

	}

	return handled;
}


_declspec( naked ) void interrupt14_asmentry( void )
//This routine is called upon an interrupt 1, even before windows gets it
{
	__asm{ 
		//save stack position	
		push ebp
		mov ebp,esp

		//save state
		pushad
		push ds
		push es
		push fs
		push gs

		mov ax,0x23 //0x10 should work too, but even windows itself is using 0x23
		mov ds,ax
		mov es,ax
		mov gs,ax
		mov ax,0x30
		mov fs,ax

		push ebp
		call interrupt14_centry

		cmp eax,1	//set flag
		je skip_original_int14




		//restore state
		pop gs
		pop fs
		pop es
		pop ds
		popad

		pop ebp		

		je skip_original_int14
		jmp far [Int14JumpBackLocation]

skip_original_int14:
		add esp,4 //skip errorcode
		iretd
		jmp skip_original_int14
	}
}