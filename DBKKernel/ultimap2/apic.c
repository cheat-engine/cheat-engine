#include <ntifs.h>

#include "apic.h"
#include <ntddk.h>
#include "..\DBKFunc.h"


#define MSR_IA32_APICBASE               0x0000001b
#define MSR_IA32_X2APIC_EOI				0x80b
#define MSR_IA32_X2APIC_LVT_PMI			0x834


volatile PAPIC APIC_BASE;

BOOL x2APICMode = FALSE;


void apic_clearPerfmon()
{
	if (x2APICMode)
	{
		__writemsr(MSR_IA32_X2APIC_LVT_PMI, (__readmsr(MSR_IA32_X2APIC_LVT_PMI) & (UINT64)0xff));
		__writemsr(MSR_IA32_X2APIC_EOI, 0);
	}
	else
	{
		//DbgPrint("Clear perfmon (APIC_BASE at %llx)\n", APIC_BASE);
		//DbgPrint("APIC_BASE->LVT_Performance_Monitor.a at %p\n", &APIC_BASE->LVT_Performance_Monitor.a);
		//DbgPrint("APIC_BASE->EOI.a at %p\n", &APIC_BASE->EOI.a);

		//DbgPrint("APIC_BASE->LVT_Performance_Monitor.a had value %x\n", APIC_BASE->LVT_Performance_Monitor.a);
		//DbgPrint("APIC_BASE->LVT_Performance_Monitor.b had value %x\n", APIC_BASE->LVT_Performance_Monitor.b);
//		DbgPrint("APIC_BASE->LVT_Performance_Monitor.c had value %x\n", APIC_BASE->LVT_Performance_Monitor.c);
	//	DbgPrint("APIC_BASE->LVT_Performance_Monitor.d had value %x\n", APIC_BASE->LVT_Performance_Monitor.d);

		APIC_BASE->LVT_Performance_Monitor.a = APIC_BASE->LVT_Performance_Monitor.a & 0xff;
		APIC_BASE->EOI.a = 0;
	}
}


void setup_APIC_BASE(void)
{
	PHYSICAL_ADDRESS Physical_APIC_BASE;
	UINT64 APIC_BASE_VALUE = readMSR(MSR_IA32_APICBASE);
	DbgPrint("Fetching the APIC base\n");

	Physical_APIC_BASE.QuadPart = APIC_BASE_VALUE & 0xFFFFFFFFFFFFF000ULL;


	DbgPrint("Physical_APIC_BASE=%p\n", Physical_APIC_BASE.QuadPart);

	APIC_BASE = (PAPIC)MmMapIoSpace(Physical_APIC_BASE, sizeof(APIC), MmNonCached);


	DbgPrint("APIC_BASE at %p\n", APIC_BASE);

	x2APICMode = (APIC_BASE_VALUE & (1 << 10))!=0;
	DbgPrint("x2APICMode=%d\n", x2APICMode);
}

void clean_APIC_BASE(void)
{
	if (APIC_BASE)
		MmUnmapIoSpace((PVOID)APIC_BASE, sizeof(APIC));
}
