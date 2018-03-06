/*
 * apic.c
 *
 *  Created on: Jun 21, 2009
 *      Author: erich
 */


#include "main.h"
#include "common.h"
#include "apic.h"

DWORD ReadAPICRegister(DWORD reg)
{
    return *((volatile DWORD*) ((IA32_APIC_BASE & 0xfffff000) + (reg * 16)));
}

DWORD WriteAPICRegister(DWORD reg, DWORD value)
{
    return *((volatile DWORD*) ((IA32_APIC_BASE & 0xfffff000) + (reg * 16))) = value;
}

void apic_eoi(void)
{
  *(volatile DWORD *)(IA32_APIC_BASE+0xb0)=0;
}
