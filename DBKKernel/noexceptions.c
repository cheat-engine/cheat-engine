#include "noexceptions.h"

int MaxCPUCount;


PCPUSTATE cpustate = NULL;

#ifdef AMD64
extern void NoException14(void); //declared in debuggera.asm
extern int ExceptionlessCopy_Internal(PVOID destination, PVOID source, int size);
#else
extern void __cdecl NoException14(void); //declared in debuggera.asm
extern int __cdecl ExceptionlessCopy_Internal(PVOID destination, PVOID source, int size);
#endif

#if (NTDDI_VERSION < NTDDI_VISTA)
int KeQueryActiveProcessorCount(PVOID x)
{
	int cpucount=0;
	KAFFINITY cpus = KeQueryActiveProcessors();
	while (cpus)
	{
		if (cpus % 2)
			cpucount++;

		cpus = cpus / 2;
	}

	return cpucount;
}
#endif


BOOL NoExceptions_Enter()
{
	KIRQL old;
	int i;
	int cpunr;

	//DbgPrint("NoExceptions_Enter");

	__try
	{
		if (cpustate == NULL)
		{
			//initialize the list
			MaxCPUCount = (int)KeQueryActiveProcessorCount(NULL);
			cpustate = ExAllocatePool(NonPagedPool, MaxCPUCount*sizeof(CPUSTATE));

			if (cpustate)
			{
				RtlZeroMemory(cpustate, MaxCPUCount*sizeof(CPUSTATE));
				for (i = 0; i < MaxCPUCount; i++)
				{
					cpustate[i].NoExceptionVectorList = ExAllocatePool(NonPagedPool, 256 * sizeof(INT_VECTOR));
					if (cpustate[i].NoExceptionVectorList)
					{
						RtlZeroMemory(cpustate[i].NoExceptionVectorList, 256 * sizeof(INT_VECTOR));						
					}
					else
					{
						//alloc failed, cleanup and quit
						int j;
						for (j = i - 1; j >= 0; j--)
							ExFreePool(cpustate[j].NoExceptionVectorList);

						cpustate = NULL;
						return FALSE;
					}
				}
			}
			else
				return FALSE;
		}

		//DbgPrint("cpustate setup here");

		KeRaiseIrql(HIGH_LEVEL, &old);
		cpunr = KeGetCurrentProcessorNumber();

		cpustate[cpunr].entryIRQL = old;

		

		

		if (cpustate[cpunr].OriginalIDT.wLimit == 0)
		{
			//initialize this

			UINT_PTR newAddress;
			__sidt(&cpustate[cpunr].OriginalIDT);
			
			RtlCopyMemory(cpustate[cpunr].NoExceptionVectorList, cpustate[cpunr].OriginalIDT.vector, cpustate[cpunr].OriginalIDT.wLimit + 1);				

			//DbgPrint("idt. Limit=%d Vector=%p", (int)cpustate[cpunr].OriginalIDT.wLimit, cpustate[cpunr].OriginalIDT.vector);

			//hook cpustate[cpunr].NoExceptionVectorList[0-15]
			//DbgPrint("")

			newAddress = (UINT_PTR)NoException14;

			cpustate[cpunr].NoExceptionVectorList[14].wHighOffset = (WORD)((DWORD)(newAddress >> 16));
			cpustate[cpunr].NoExceptionVectorList[14].wLowOffset = (WORD)newAddress;

#ifdef AMD64
			cpustate[cpunr].NoExceptionVectorList[14].TopOffset = (newAddress >> 32);
			cpustate[cpunr].NoExceptionVectorList[14].Reserved = 0;
#endif


				/*
				newVector.wHighOffset=(WORD)((DWORD)(newEIP >> 16));
				newVector.wLowOffset=(WORD)newEIP;
				newVector.wSelector=(WORD)newCS;
				newVector.bUnused=0;
				newVector.bAccessFlags=idt.vector[intnr].bAccessFlags; //don't touch accessflag, the default settings are good (e.g: int3,4 and 8 have dpl=3)

				#ifdef AMD64
				newVector.TopOffset=(newEIP >> 32);
				newVector.Reserved=0;
				#endif
				*/


			cpustate[cpunr].ModdedIDT = cpustate[cpunr].OriginalIDT;
			cpustate[cpunr].ModdedIDT.vector = cpustate[cpunr].NoExceptionVectorList;
			

			
		};

#ifdef AMD64
		_disable();
#else
		__asm{
			cli
		}
#endif
		__lidt(&cpustate[cpunr].ModdedIDT);
	

		
		return TRUE;
	}
	__except (1)
	{
		DbgPrint("Exception during NoExceptions_Enter. Figures");
	}

	
	//KeGetCurrentProcessorNumber()
	//hadError = FALSE;

	return FALSE;
}

int NoExceptions_CopyMemory(PVOID Destination, PVOID Source, int size)
{
	BOOL EnteredNoExceptions = FALSE;
	int r;
	
	if (KeGetCurrentIrql() <= DISPATCH_LEVEL)
	{
		//DbgPrint("calling NoExceptions_Enter");
		EnteredNoExceptions = NoExceptions_Enter();
		if (EnteredNoExceptions == FALSE)
			return 0;
	}
	
	r = ExceptionlessCopy_Internal(Destination, Source, size);

	
	
	if (EnteredNoExceptions)
	{
		//DbgPrint("calling NoExceptions_Leave");
		NoExceptions_Leave();
	}


	return r;
}

void NoExceptions_Leave()
{
	
	int cpunr = KeGetCurrentProcessorNumber();

	//restore the IDT
	__lidt(&cpustate[cpunr].OriginalIDT);
#ifdef AMD64	
	_enable();
#else
	__asm{
		sti
	}
#endif
	KeLowerIrql(cpustate[cpunr].entryIRQL);
}

void NoExceptions_Cleanup()
{
	if (cpustate)
	{
		int i;
		for (i = 0; i < MaxCPUCount; i++)
		{
			if (cpustate[i].NoExceptionVectorList)
			{
				ExFreePool(cpustate[i].NoExceptionVectorList);
				cpustate[i].NoExceptionVectorList = NULL;
			}
		}

		ExFreePool(cpustate);
	}
		

}



