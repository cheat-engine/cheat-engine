/*
 * test.c
 *
 *  Created on: May 22, 2011
 *      Author: erich
 */

#include "common.h"
#include "main.h"
#include "mm.h"
#include "msrnames.h"
#include "test.h"
#include "apic.h"

int MSR_LASTBRANCH_TOS=0x1C9;
int MSR_LASTBRANCH_0=0x1DB;


int canstoreds=0;

void *DebugStore;

int testBranchPrediction(void)
{

  QWORD LVT_Perfmon_Counter_Register;
  BYTE LVT_Perfmon_Vector;
  displayline("testing branch prediction\n");

  displayline("cpu_stepping=%d\n",cpu_stepping);
  displayline("cpu_model=%d\n",cpu_model);
  displayline("cpu_familyID=%d\n",cpu_familyID);
  displayline("cpu_type=%d\n",cpu_type);
  displayline("cpu_ext_modelID=%d\n",cpu_ext_modelID);
  displayline("cpu_ext_familyID=%d\n\n", cpu_ext_familyID);

  displayline("IA32_APIC_BASE=%6\n", IA32_APIC_BASE);


  LVT_Perfmon_Counter_Register=*(volatile DWORD *)(IA32_APIC_BASE+0x340);
  LVT_Perfmon_Vector=LVT_Perfmon_Counter_Register & 0xff;



  displayline("[IA32_APIC_BASE+0x340]=%x  (perfmon)\n", LVT_Perfmon_Counter_Register);
  displayline("[IA32_APIC_BASE+0x350]=%x  (Lint0)\n", *(DWORD *)(IA32_APIC_BASE+0x350));
  displayline("[IA32_APIC_BASE+0x360]=%x  (Lint1)\n", *(DWORD *)(IA32_APIC_BASE+0x360));

  displayline("perfmon interrupt vector was %d ", LVT_Perfmon_Vector);
  if (LVT_Perfmon_Counter_Register & (1<<16) )
    displayline("and it was masked\n");
  else
    displayline("and it was unmasked\n");


  LVT_Perfmon_Counter_Register=0xfe;
  LVT_Perfmon_Vector=LVT_Perfmon_Counter_Register & 0xff;

  *(DWORD *)(IA32_APIC_BASE+0x340)=LVT_Perfmon_Counter_Register;

  displayline("[IA32_APIC_BASE+0x340]=%x  (perfmon)\n", LVT_Perfmon_Counter_Register);
  displayline("perfmon interrupt vector is now %d ", LVT_Perfmon_Vector);
  if (LVT_Perfmon_Counter_Register & (1<<16) )
    displayline("and it is masked\n");
  else
    displayline("and it is unmasked\n");




  displayline("IA32_DEBUGCTL_MSR=%6\n", readMSR(IA32_DEBUGCTL_MSR));
  {
    QWORD rax,rbx,rcx,rdx;
    rax=1;
    _cpuid(&rax, &rbx, &rcx, &rdx);

    displayline("cpuid 1: rax=%8  rcx=%8   rdx=%8\n", rax, rcx, rdx);

    if (rdx & (1 << 21))
      displayline("This cpu supports storing of branch messages\n");
    else
      displayline("This cpu DOES NOT support storing of branch messages\n");



    canstoreds=rcx & (1<<4);
    if (canstoreds)
    {
      displayline("This cpu supports a cpl qualified ds store\n");
    }
    else
    {
      displayline("This cpu DOES NOT support a cpl qualified ds store\n");
    }


    displayline("Making sure LBR is enabled");
    writeMSR(IA32_DEBUGCTL_MSR, readMSR(IA32_DEBUGCTL_MSR) | (1 << 0));

    //displayline("Setting bts_off_os");
    //writeMSR(IA32_DEBUGCTL_MSR, readMSR(IA32_DEBUGCTL_MSR) | (1 << 9));


    //displayline("setting bts");
   // writeMSR(IA32_DEBUGCTL_MSR, readMSR(IA32_DEBUGCTL_MSR) | (1 << 7));

    //displayline("setting BTF");
 //   writeMSR(IA32_DEBUGCTL_MSR, readMSR(IA32_DEBUGCTL_MSR) | (1 << 1));

    if (cpu_familyID==0x06)
    {
      int maxindex=0;
      int i, count;
      MSR_LASTBRANCH_TOS=0x1c9;
      MSR_LASTBRANCH_0=0x40;

      switch (cpu_model)
      {
        case 0x2a:
        case 0x1a:
        case 0x1e:
        case 0x1f:
        case 0x2e:
        case 0x25:
        case 0x2c:
          maxindex=16;
          break;

        case 0x17:
        case 0x1d:
        case 0x0f:
          maxindex=4;
          break;

        case 0x1c:
          maxindex=8;
          break;
      }

      displayline("testing. turning lbr off\n");
      writeMSR(IA32_DEBUGCTL_MSR, 0);


      displayline("TOS=%d\n", readMSR(MSR_LASTBRANCH_TOS));
      displayline("Max index=%d\n", maxindex);
      displayline("\n");

      i=readMSR(MSR_LASTBRANCH_TOS);
      count=0;
      while (count<maxindex)
      {
        QWORD x;
        x=readMSR(MSR_LASTBRANCH_0+i);

        displayline("MSR_LASTBRANCH_%d=%x", i, x);

        displayline("   (   %x   %x   )", (DWORD)(x & 0xffffffff) ,  (DWORD)(x >> 32)  );

        displayline("\n");

        count++;
        i++;
        i=i % maxindex;
      }




    }


    displayline("Setting up branch recording to memory\n");
    {
      QWORD misc_enable=readMSR(IA32_MISC_ENABLE);

      if (misc_enable & (1 << 11))
      {
        displayline("Branch trace storage unavailable\n");
        return 0;
      }
      else
        displayline("Branch trace storage is available\n");

      displayline("The original IA32_DS_AREA=%x\n", readMSR(IA32_DS_AREA));



      DebugStore=malloc(4096);

      if (DebugStore)
      {
        PDS_AREA_MANAGEMENT64  DebugStore64;
        zeromemory(DebugStore, 4096);

        displayline("sizeof(BTS64)=%d\n", sizeof(BTS64));

        //now configure it
        DebugStore64=(PDS_AREA_MANAGEMENT64)DebugStore;

        DebugStore64->BTS_BufferBaseAddress=(QWORD)DebugStore64+sizeof(DS_AREA_MANAGEMENT64);
        DebugStore64->BTS_BufferBaseAddress+=sizeof(BTS64);

        DebugStore64->BTS_BufferBaseAddress-=DebugStore64->BTS_BufferBaseAddress % sizeof(BTS64);

        DebugStore64->BTS_IndexBaseAddress=DebugStore64->BTS_BufferBaseAddress;
        DebugStore64->BTS_AbsoluteMaxAddress=(QWORD)DebugStore64+4096-sizeof(BTS64);
        DebugStore64->BTS_AbsoluteMaxAddress-=DebugStore64->BTS_AbsoluteMaxAddress % sizeof(BTS64);
        DebugStore64->BTS_AbsoluteMaxAddress++;

        DebugStore64->BTS_InterruptThresholdAddress=(DebugStore64->BTS_AbsoluteMaxAddress-1) - 3*sizeof(BTS64);

        displayline("DebugStore64=%p\n", DebugStore64);
        displayline("DebugStore64->BTS_BufferBaseAddress=%x\n", DebugStore64->BTS_BufferBaseAddress);
        displayline("DebugStore64->BTS_IndexBaseAddress=%x\n", DebugStore64->BTS_IndexBaseAddress);
        displayline("DebugStore64->BTS_AbsoluteMaxAddress=%x\n", DebugStore64->BTS_AbsoluteMaxAddress);
        displayline("DebugStore64->BTS_InterruptThresholdAddress=%x\n", DebugStore64->BTS_InterruptThresholdAddress);


        writeMSR(IA32_DS_AREA, (QWORD)DebugStore);
        displayline("The new IA32_DS_AREA=%x\n", readMSR(IA32_DS_AREA));
      }
      else
        displayline("Someone has fucked up the memory allocator\n");

    }




    writeMSR(IA32_DEBUGCTL_MSR, readMSR(IA32_DEBUGCTL_MSR) | 1 );




    writeMSR(IA32_DEBUGCTL_MSR, readMSR(IA32_DEBUGCTL_MSR) | (1<<7) );
    writeMSR(IA32_DEBUGCTL_MSR, readMSR(IA32_DEBUGCTL_MSR) | (1<<6) ); //start trace

    writeMSR(IA32_DEBUGCTL_MSR, readMSR(IA32_DEBUGCTL_MSR) | (1<<8) ); //interrupt when full

    displayline("IA32_DEBUGCTL_MSR is now %x", readMSR(IA32_DEBUGCTL_MSR));

    testBranch();


  }

  return 0;

}

int handlePerformanceCounterInterrupt(void)
{

    PDS_AREA_MANAGEMENT64 DebugStore64=(PDS_AREA_MANAGEMENT64)DebugStore;
    sendstring("This was int 0xfe\n");
    sendstringf("IA32_DEBUGCTL_MSR is now %x\n", readMSR(IA32_DEBUGCTL_MSR));
    sendstringf("[IA32_APIC_BASE+0x340]=%x  (perfmon)\n", *(QWORD *)(IA32_APIC_BASE+0x340));

    sendstringf("DebugStore64=%p\n", DebugStore64);
    sendstringf("DebugStore64->BTS_BufferBaseAddress=%x\n", DebugStore64->BTS_BufferBaseAddress);
    sendstringf("DebugStore64->BTS_IndexBaseAddress=%x\n", DebugStore64->BTS_IndexBaseAddress);
    sendstringf("DebugStore64->BTS_AbsoluteMaxAddress=%x\n", DebugStore64->BTS_AbsoluteMaxAddress);
    sendstringf("DebugStore64->BTS_InterruptThresholdAddress=%x\n", DebugStore64->BTS_InterruptThresholdAddress);

    *(QWORD *)(IA32_APIC_BASE+0x340)=*(QWORD *)(IA32_APIC_BASE+0x340) & (~(1<<16)) ;
    sendstringf("[IA32_APIC_BASE+0x340]=%x  (perfmon)\n", *(QWORD *)(IA32_APIC_BASE+0x340));

    apic_eoi();
    DebugStore64->BTS_IndexBaseAddress=DebugStore64->BTS_BufferBaseAddress;
  //  writeMSR(IA32_DEBUGCTL_MSR, (1<<7) | (1<<0) | (1<<6) | (1<<8));
   // sendstringf("IA32_DEBUGCTL_MSR is now %x\n", readMSR(IA32_DEBUGCTL_MSR));

    return 1;
}
