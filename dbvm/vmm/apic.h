#ifndef APIC_H_
#define APIC_H_

#include "common.h"
extern unsigned int apic_getBootID(void);
extern void apic_enableSVR(void);
extern void initcpus(QWORD apic_base, DWORD entrypage);
extern void apic_eoi(void);

extern void APbootcode(void);
extern void APbootcodeEnd(void);
extern QWORD APBootVar_CR3;
extern QWORD APBootVar_GDT[24];
extern WORD APBootVar_Size;



#endif /*APIC_H_*/
