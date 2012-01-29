#ifndef APIC_H_
#define APIC_H_

#include "common.h"
extern unsigned int apic_getBootID(void);
extern void apic_enableSVR(void);
extern void initcpus(QWORD apic_base);
extern void apic_eoi(void);


#endif /*APIC_H_*/
