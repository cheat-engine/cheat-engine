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

int getHighestPendingInterrupt()
{
  int result=-1;
  QWORD APIC_PA=readMSR(IA32_APICBASE_MSR) & MAXPHYADDRMASKPB;
  PAPIC apic=mapPhysicalMemory(APIC_PA, 4096);
  SetPageToWriteThrough(apic);

  int i;
  for (i=7; i>=0; i--)
  {
    DWORD v=apic->Interrupt_Request[i].a;
    if (v)
    {
      int j;
      for (j=31; j>=0; j--)
        if (v & (1<<j))
        {
          result=i*32+j;
          break;
        }
      break;
    }

  }


  unmapPhysicalMemory(apic,4096);

  return result;
}

DWORD ReadAPICRegister(DWORD reg)
{
  DWORD result;
  QWORD APIC_PA=readMSR(IA32_APICBASE_MSR) & MAXPHYADDRMASKPB;
  PAPIC apic=mapPhysicalMemory(APIC_PA, 4096);
  SetPageToWriteThrough(apic);

  result=*(volatile DWORD*)((UINT64)apic + (reg * 0x10));

  unmapPhysicalMemory(apic,4096);

  return result;
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
