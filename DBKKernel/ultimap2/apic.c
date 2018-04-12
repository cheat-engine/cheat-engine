#include <ntifs.h>

#include "apic.h"
#include <ntddk.h>
#include "..\DBKFunc.h"


#define MSR_IA32_APICBASE               0x0000001b

volatile PAPIC APIC_BASE;


void apic_clearPerfmon()
{
	APIC_BASE->LVT_Performance_Monitor.a = APIC_BASE->LVT_Performance_Monitor.a & 0xff;
	APIC_BASE->EOI.a = 0;
}


void setup_APIC_BASE(void)
{
	PHYSICAL_ADDRESS Physical_APIC_BASE;
	DbgPrint("Fetching the APIC base\n");

	Physical_APIC_BASE.QuadPart = readMSR(MSR_IA32_APICBASE) & 0xFFFFFFFFFFFFF000ULL;


	DbgPrint("Physical_APIC_BASE=%p\n", Physical_APIC_BASE.QuadPart);

	APIC_BASE = (PAPIC)MmMapIoSpace(Physical_APIC_BASE, sizeof(APIC), MmNonCached);


	DbgPrint("APIC_BASE at %p\n", APIC_BASE);

}

void clean_APIC_BASE(void)
{
	if (APIC_BASE)
		MmUnmapIoSpace((PVOID)APIC_BASE, sizeof(APIC));
}
