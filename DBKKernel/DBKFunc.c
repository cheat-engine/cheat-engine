#pragma warning( disable: 4103)

#include "ntddk.h"
#include "DBKFunc.h"
/*x
#include "vmxhelper.h"
#include "interruptHook.h"
*/

#ifdef TESTCOMPILE
//small bypass to make test compiles function (this compiler doesn't have CLI/STI )
void _enable() {}
void _disable() {}
#endif


void forEachCpuPassive(PF f, UINT_PTR param)
/*
calls a specific function for each cpu that runs in passive mode
*/
{
	CCHAR cpunr;
	KAFFINITY cpus, original;
	ULONG cpucount;

	
	//KeIpiGenericCall is not present in xp
	
	//count cpus first KeQueryActiveProcessorCount is not present in xp)
	cpucount=0;
	cpus=KeQueryActiveProcessors();
	original=cpus;
	while (cpus)
	{
		if (cpus % 2)
			cpucount++;

		cpus=cpus / 2;		
	}

	cpus=KeQueryActiveProcessors();
	cpunr=0;
	while (cpus)
	{
		if (cpus % 2)
		{
			//bit is set
#if (NTDDI_VERSION >= NTDDI_VISTA)
			KAFFINITY oldaffinity;
#endif
			KAFFINITY newaffinity;


			
			//DbgPrint("Calling passive function for cpunr %d\n", cpunr);
			//set affinity

			newaffinity=(KAFFINITY)(1 << cpunr);

#if (NTDDI_VERSION >= NTDDI_VISTA)
			oldaffinity=KeSetSystemAffinityThreadEx(newaffinity);
#else
			//XP and earlier (this routine is not called often, only when the user asks explicitly
			{
				LARGE_INTEGER delay;
				delay.QuadPart=-50; //short wait just to be sure... (the docs do not say that a switch happens imeadiatly for the no Ex version)
				
				KeSetSystemAffinityThread(newaffinity);
				KeDelayExecutionThread(UserMode, FALSE, &delay);
			}
#endif



			//call function
			f(param);

#if (NTDDI_VERSION >= NTDDI_VISTA)
			KeRevertToUserAffinityThreadEx(oldaffinity);
#endif

		}

		cpus=cpus / 2;
		cpunr++;
	}

#if (NTDDI_VERSION < NTDDI_VISTA)
	KeSetSystemAffinityThread(original);
#endif
}

void forEachCpu(PKDEFERRED_ROUTINE dpcfunction,  PVOID DeferredContext, PVOID  SystemArgument1, PVOID  SystemArgument2)
/*
calls a specified dpcfunction for each cpu on the system
*/
{
	CCHAR cpunr;
	KAFFINITY cpus;
	ULONG cpucount;
	PKDPC dpc;
	int dpcnr;

	
	//KeIpiGenericCall is not present in xp
	
	//count cpus first KeQueryActiveProcessorCount is not present in xp)
	cpucount=0;
	cpus=KeQueryActiveProcessors();
	while (cpus)
	{
		if (cpus % 2)
			cpucount++;

		cpus=cpus / 2;		
	}

	dpc=ExAllocatePool(NonPagedPool, sizeof(KDPC)*cpucount);

	cpus=KeQueryActiveProcessors();
	cpunr=0;
	dpcnr=0;
	while (cpus)
	{
		if (cpus % 2)
		{
			//bit is set
			
			//DbgPrint("Calling dpc routine for cpunr %d\n", cpunr);

			KeInitializeDpc(&dpc[dpcnr], dpcfunction, DeferredContext);
			KeSetTargetProcessorDpc (&dpc[dpcnr], cpunr);
			KeInsertQueueDpc(&dpc[dpcnr], SystemArgument1, SystemArgument2);
			KeFlushQueuedDpcs();
			dpcnr++;
		}

		cpus=cpus / 2;
		cpunr++;
	}


	ExFreePool(dpc);
}


void forEachCpuAsync(PKDEFERRED_ROUTINE dpcfunction, PVOID DeferredContext, PVOID  SystemArgument1, PVOID  SystemArgument2)
/*
calls a specified dpcfunction for each cpu on the system
*/
{
	CCHAR cpunr;
	KAFFINITY cpus;
	ULONG cpucount;
	PKDPC dpc;
	int dpcnr;


	//KeIpiGenericCall is not present in xp

	//count cpus first KeQueryActiveProcessorCount is not present in xp)
	cpucount = 0;
	cpus = KeQueryActiveProcessors();
	while (cpus)
	{
		if (cpus % 2)
			cpucount++;

		cpus = cpus / 2;
	}

	dpc = ExAllocatePool(NonPagedPool, sizeof(KDPC)*cpucount);

	cpus = KeQueryActiveProcessors();
	cpunr = 0;
	dpcnr = 0;
	while (cpus)
	{
		if (cpus % 2)
		{
			//bit is set

			//DbgPrint("Calling dpc routine for cpunr %d\n", cpunr);

			KeInitializeDpc(&dpc[dpcnr], dpcfunction, DeferredContext);
			KeSetTargetProcessorDpc(&dpc[dpcnr], cpunr);
			KeInsertQueueDpc(&dpc[dpcnr], SystemArgument1, SystemArgument2);			
			dpcnr++;
		}

		cpus = cpus / 2;
		cpunr++;
	}

	KeFlushQueuedDpcs();


	ExFreePool(dpc);
}



//own critical section implementation for use when the os is pretty much useless (dbvm tech)
void spinlock(volatile int *lockvar)
{
	DWORD a[4];
	

	while (1)
	{
		while (*(volatile int *)lockvar!=0)		
		{
			__nop();
			__nop();
			__cpuid(a,0); //serialize cpu's		
			__nop();
			__nop();
		}
		//it was 0, let's see if we can set it to 1
		//race who can set it to 1:
		if (_InterlockedExchange((volatile int *)lockvar, 1)==0)
			return; //lock aquired, else continue loop

	}

}

void csEnter(PcriticalSection CS)
{ 
	EFLAGS oldstate=getEflags();
	
	int apicid=cpunr()+1; //+1 so it never returns 0
	
	
	if ((CS->locked) && (CS->cpunr==cpunr())) 
	{
	    //already locked but the locker is this cpu, so allow, just increase lockcount
	    CS->lockcount++;
	    return; 
	} 

	disableInterrupts(); //disable interrupts to prevent taskswitch in same cpu
	
	spinlock(&(CS->locked)); //sets CS->locked to 1
  
	//here so the lock is aquired and locked is 1
	CS->lockcount=1;
	CS->cpunr=cpunr();  
	CS->oldIFstate=oldstate.IF;
}

void csLeave(PcriticalSection CS)
{
	int apicid=cpunr()+1; //+1 so it never returns 0
  
	if ((CS->locked) && (CS->cpunr==cpunr()))
	{
	    CS->lockcount--;
	    if (CS->lockcount==0)
	    {
			//unlock    
			if (CS->oldIFstate)
				enableInterrupts();				

			CS->cpunr=-1; //set to an cpunr
			CS->locked=0;
		} 
	}
	
}


int getCpuCount(void)
{
	KAFFINITY ap=KeQueryActiveProcessors();
	int count=0;
	while (ap>0)
	{
		if (ap % 2)
			count++;

		ap=ap / 2;
	}
	return count;
}

int isPrefix(unsigned char b)
{
	switch (b)
	{
		case 0x26:
		case 0x2e:
		case 0x36:
		case 0x3e:		
		case 0x64:
		case 0x65:
		case 0x66:
		case 0x67:
		case 0xf0: //lock
		case 0xf2: //repne
		case 0xf3: //rep
			return 1;

		default:
			return 0;

	}

}

UINT64 getDR7(void)
{
	return __readdr(7);

}

int cpunr(void)
{	
	DWORD x[4];
	__cpuid(&x[0],1);
	
	return (x[1] >> 24)+1;

}

EFLAGS getEflags(void)
{
	UINT64 x=__getcallerseflags();
	PEFLAGS y = (PEFLAGS)&x;
	return *y;
}

UINT64 readMSR(DWORD msr)
{
	return __readmsr(msr);
}

void setCR0(UINT64 newcr0)
{
	__writecr0(newcr0);
}

UINT64 getCR0(void)
{
	return __readcr0();
}

UINT64 getCR2(void)
{
	return __readcr2();
}



void setCR3(UINT64 newCR3)
{
	__writecr3(newCR3);
}

UINT64 getCR3(void)
{
	return __readcr3();
}



void setCR4(UINT64 newcr4)
{
	__writecr4(newcr4);
}

UINT64 getCR4(void)
{
	return __readcr4();
}

void GetIDT(PIDT pIdt)
{
	__sidt(pIdt);
}

void enableInterrupts(void)
{
#ifdef AMD64
	_enable();
#else
	__asm{sti};
#endif
}

void disableInterrupts(void)
{
#ifdef AMD64
	_disable();
#else
	__asm{cli};
#endif
}

UINT64 getTSC(void)
{
	return __rdtsc();
}

#ifndef AMD64
//function declarations that can be done inline without needing an .asm file
_declspec( naked ) WORD getSS(void)
{
	__asm
	{
		mov ax,ss
		ret
	}
}

_declspec( naked ) WORD getCS(void)
{
	__asm
	{
		mov ax,cs
		ret
	}
}

_declspec( naked ) WORD getDS(void)
{
	__asm
	{
		mov ax,ds
		ret
	}
}

_declspec( naked ) WORD getES(void)
{
	__asm
	{
		mov ax,es
		ret
	}
}

_declspec( naked ) WORD getFS(void)
{
	__asm
	{
		mov ax,fs
		ret
	}
}

_declspec( naked ) WORD getGS(void)
{
	__asm
	{
		mov ax,gs
		ret
	}
}


_declspec( naked ) ULONG getRSP(void) //...
{
	__asm
	{
		mov eax,esp
		add eax,4 //don't add this call
		ret
	}
}

_declspec( naked ) ULONG getRBP(void)
{
	__asm
	{
		mov eax,ebp
		ret
	}
}

_declspec( naked ) ULONG getRAX(void)
{
	__asm
	{
		mov eax,eax
		ret
	}
}
_declspec( naked ) ULONG getRBX(void)
{
	__asm
	{
		mov eax,ebx
		ret
	}
}
_declspec( naked ) ULONG getRCX(void)
{
	__asm
	{
		mov eax,ecx
		ret
	}
}
_declspec( naked ) ULONG getRDX(void)
{
	__asm
	{
		mov eax,edx
		ret
	}
}
_declspec( naked ) ULONG getRSI(void)
{
	__asm
	{
		mov eax,esi
		ret
	}
}
_declspec( naked ) ULONG getRDI(void)
{
	__asm
	{
		mov eax,edi
		ret
	}
}

_declspec( naked ) unsigned short GetTR(void)
{
	__asm{
		STR AX
		ret
	}	
}


void GetGDT(PGDT pGdt)
{
	__asm
    {
		MOV EAX, [pGdt]
	    SGDT [EAX]
    }       
}

_declspec( naked )WORD GetLDT()
{	
	__asm
	{		
		SLDT ax
		ret
	}
}

#endif
