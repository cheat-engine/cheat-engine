/*
 * apic.c
 *
 *  Created on: Jun 21, 2009
 *      Author: erich
 */


#include "main.h"
#include "common.h"
#include "apic.h"
#include "msrnames.h"
#include "mm.h"

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


void apic_sendWaitInterrupt(BYTE apicid)
{
  APIC_ICR Command;
  APIC_ICR temp;
  if (sizeof(APIC_ICR)!=8)
  {
    sendstringf("sizeof(APIC_ICR)=%d\n",sizeof(APIC_ICR));
    while (1);
  }

  sendstringf("apic_nmi(%d)\n",apicid);



  Command.command=0;
  Command.vector=0xce; //wait interrupt
  Command.deliverymode=4; //nmi
  Command.destinationmode=0; //physical
  Command.deliverystatus=1; //not being sent, but just setting it anyhow
  Command.level=1;
  Command.triggermode=0;

  Command.destination_shorthand=0; //APICID specific

  sendstringf("sending APIC command to wait to cpu with APICID %d", apicid);

  if (readMSR(IA32_APICBASE_MSR) & (1<<10))
  {
    //x2apic mode
    Command.x2destination=apicid;
    sendstringf(" using x2apic\n");

    sendstringf("Sending NMI\n");
    writeMSR(IA32_X2APIC_ICR_MSR, Command.command);

  }
  else
  {
    //oldmode
    Command.destination=apicid;

    sendstringf(" using legacy apic\n");

    QWORD APIC_PA=readMSR(IA32_APICBASE_MSR) & MAXPHYADDRMASKPB;

    PAPIC apic=mapPhysicalMemory(APIC_PA, 4096);
    if (apic==NULL)
      while (1);

    SetPageToWriteThrough(apic);

    sendstringf("Waiting till ready\n");
    do
    {
      temp.lower=apic->Interrupt_Command_Low32Bit.a;
    } while (temp.deliverystatus==1);

    sendstringf("Sending NMI\n");
    //send the NMI
    asm volatile ("": : :"memory");
    apic->Interrupt_Command_High32Bit.a=Command.higher;
    asm volatile ("": : :"memory");
    apic->Interrupt_Command_Low32Bit.a=Command.lower; //this write sends it
    asm volatile ("": : :"memory");

    sendstringf("Waiting till ready again\n");

    do
    {
      temp.lower=apic->Interrupt_Command_Low32Bit.a;
    } while (temp.deliverystatus==1);

    sendstringf("Done\n");

    unmapPhysicalMemory(apic,4096);
  }
}
