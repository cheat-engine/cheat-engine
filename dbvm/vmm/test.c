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
#include "vmcall.h"

int MSR_LASTBRANCH_TOS=0x1C9;
int MSR_LASTBRANCH_0=0x1DB;


int canstoreds=0;

void *DebugStore;

extern int cloakTestFunction(void);
extern void* cloakTestFunctionEnd;
extern void* cloakTestFunctionInstruction;

typedef int(*CLOAKTESTFUNCTION)(void);



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

int getDBVMVersion(void)
{
  int result;
  VMCALL_BASIC param;
  sendstringf("getDBVMVersion");

  param.command=VMCALL_GETVERSION;
  param.password2=Password2;
  param.size=sizeof(param);

  try
  {
    result=_vmcall(&param);
  }
  except
  {
    result=0;
  }
  tryend

  return result;
}

int dbvm_watch_writes(QWORD PhysicalAddress, int *ID)
{
  int result;
  VMCALL_WATCH_PARAM param;
  param.vmcall.command=VMCALL_WATCH_WRITES;
  param.vmcall.password2=Password2;
  param.vmcall.size=sizeof(param);

  param.PhysicalAddress=PhysicalAddress;
  param.Size=4;
  param.Options=0;
  param.MaxEntryCount=16;
  param.ID=*ID;

  try
  {
    result=_vmcall(&param);
    *ID=param.ID;
  }
  except
  {
    result=-1;
  }
  tryend

  return result;
}

int dbvm_watch_reads(QWORD PhysicalAddress, int *ID)
{
  int result;
  VMCALL_WATCH_PARAM param;
  param.vmcall.command=VMCALL_WATCH_READS;
  param.vmcall.password2=Password2;
  param.vmcall.size=sizeof(param);

  param.PhysicalAddress=PhysicalAddress;
  param.Size=4;
  param.Options=0;
  param.MaxEntryCount=16;
  param.ID=*ID;

  try
  {
    result=_vmcall(&param);
    *ID=param.ID;
  }
  except
  {
    result=-1;
  }
  tryend

  return result;
}

int dbvm_watch_executes(QWORD PhysicalAddress, int *ID)
{
  int result;
  VMCALL_WATCH_PARAM param;
  param.vmcall.command=VMCALL_WATCH_EXECUTES;
  param.vmcall.password2=Password2;
  param.vmcall.size=sizeof(param);

  param.PhysicalAddress=PhysicalAddress;
  param.Size=4;
  param.Options=0;
  param.MaxEntryCount=16;
  param.ID=*ID;

  try
  {
    result=_vmcall(&param);
    *ID=param.ID;
  }
  except
  {
    result=-1;
  }
  tryend

  return result;
}

int dbvm_watch_retrievelog(int watchid, void* result, int *size)
{
  int r;
  VMCALL_WATCH_RETRIEVELOG_PARAM param;
  param.vmcall.command=VMCALL_WATCH_RETRIEVELOG;
  param.vmcall.password2=Password2;
  param.vmcall.size=sizeof(param);

  param.ID=watchid;
  param.results=(QWORD)result;
  param.resultsize=*size;
  param.copied=0;


  try
  {
    r=_vmcall(&param);
    *size=param.resultsize;
  }
  except
  {
    r=-1;
  }
  tryend

  return r;
}

int dbvm_watch_delete(int id)
{
  int r;
  VMCALL_WATCH_DISABLE_PARAM param;
  param.vmcall.command=VMCALL_WATCH_DELETE;
  param.vmcall.password2=Password2;
  param.vmcall.size=sizeof(param);

  param.ID=id;

  try
  {
    r=_vmcall(&param);
  }
  except
  {
    r=-1;
  }
  tryend

  return r;
}


int dbvm_cloak_activate(QWORD physicalAddress, int mode)
{
  VMCALL_CLOAK_ACTIVATE_PARAM param;
  param.vmcall.command=VMCALL_CLOAK_ACTIVATE;
  param.vmcall.password2=Password2;
  param.vmcall.size=sizeof(param);

  param.physicalAddress=physicalAddress;
  param.mode=mode; //single step

  int r;
  try
  {
    r=_vmcall(&param);
  }
  except
  {
    r=-1;
  }
  tryend

  return r;
}

int dbvm_cloak_deactivate(QWORD PhysicalAddress)
{
  VMCALL_CLOAK_DEACTIVATE_PARAM param;
  param.vmcall.command=VMCALL_CLOAK_DEACTIVATE;
  param.vmcall.password2=Password2;
  param.vmcall.size=sizeof(param);

  param.physicalAddress=PhysicalAddress;

  int r;
  try
  {
    r=_vmcall(&param);
  }
  except
  {
    r=-1;
  }
  tryend

  return r;

}

int dbvm_cloak_readExecutable(QWORD PhysicalAddress, void *destination)
{
  VMCALL_CLOAK_READ_PARAM param;
  param.vmcall.command=VMCALL_CLOAK_READORIGINAL;
  param.vmcall.password2=Password2;
  param.vmcall.size=sizeof(param);

  param.physicalAddress=PhysicalAddress;
  param.destination=(QWORD)destination;


  int r;
  try
  {
    r=_vmcall(&param);
  }
  except
  {
    r=-1;
  }
  tryend

  return r;
}

int dbvm_cloak_writeExecutable(QWORD PhysicalAddress, void *source)
{
  VMCALL_CLOAK_WRITE_PARAM param;
  param.vmcall.command=VMCALL_CLOAK_WRITEORIGINAL;
  param.vmcall.password2=Password2;
  param.vmcall.size=sizeof(param);

  param.physicalAddress=PhysicalAddress;
  param.source=(QWORD)source;


  int r;
  try
  {
    r=_vmcall(&param);
  }
  except
  {
    r=-1;
  }
  tryend

  return r;
}


void testfunction()
{
  sendstringf("testfunction\n");
}

void dbvm_changeregonbp_test(void)
{

}

void dbvm_cloak_test(void)
{
  int DBVMVersion=getDBVMVersion();
  int watchid=-1;


  if (DBVMVersion)
  {
    int r;
    int i;
    unsigned char *newmem=(unsigned char *)malloc(8192);
    CLOAKTESTFUNCTION newtestfunction=(CLOAKTESTFUNCTION)&newmem[4094];
    QWORD PA=VirtualToPhysical((void*)newtestfunction)+2;
    sendstringf("newtestfunction at %6 (PA : %6)\n", newtestfunction, PA);

    QWORD size=(QWORD)&cloakTestFunctionEnd-(QWORD)cloakTestFunction;
    sendstringf("size=%d bytes\n", size);
    sendstringf("Copying...");
    copymem(newtestfunction,cloakTestFunction, size );

    sendstringf("Done\n");

    sendstringf("Calling original\n");
    r=cloakTestFunction();
    sendstringf("Returned %d\n", r);


    sendstringf("Calling copy\n");
    r=newtestfunction();
    sendstringf("Returned %d\n", r);
    displayline("Unpatched result is %d\n", r);




    sendstring("Patching copy\n");
    unsigned char *funcindex=(unsigned char *)newtestfunction;
    sendstringf("Original:");
    for (i=0; i<5; i++)
    {
      int index=(QWORD)(&cloakTestFunctionInstruction)-((QWORD)(cloakTestFunction))+i;
      sendstringf("%2 ",funcindex[index]);
      funcindex[index]=0x90;
    }
    sendstringf("\n");

    sendstringf("Calling copy after patch\n");
    r=newtestfunction();

    sendstringf("Patched result of cloakTestFunction() is %d\n", r);
    displayline("Patched result without cloak is %d\n", r);

    sendstringf("Undo patch. Now trying with cloak\n");
    copymem(newtestfunction,cloakTestFunction, size );

    int beforecloak=generateCRC((unsigned char*)newtestfunction, size);
    displayline("Before cloak crc was %x\n", beforecloak);

    unsigned char *buf=(unsigned char *)newtestfunction;
    sendstringf("%6:", newtestfunction);
    for (i=0; i<size; i++)
      sendstringf("%2 ",buf[i]);

    sendstringf("\n");

    sendstringf("Activating cloak (mode 1)\n");
    r=dbvm_cloak_activate(PA, 1);
    displayline("Cloak activated\n");
    sendstringf("dbvm_cloak_activate(%6) returned %d\n", PA, r);

    _invlpg((QWORD)newtestfunction);

    buf=(unsigned char *)newtestfunction;
    sendstringf("%6:", newtestfunction);
    for (i=0; i<size; i++)
      sendstringf("%2 ",buf[i]); //when testing, should be all 0xce's

    sendstringf("\n");



    int aftercloak=generateCRC((unsigned char*)newtestfunction, size);
    displayline("After cloak crc is %x\n", aftercloak);



    sendstringf("Calling newtestfunction: (cloaked, unpatched)\n");
    r=newtestfunction();
    sendstringf("After cloak but no patch ,result of cloakTestFunction() is %d\n", r);
    displayline("Cloak active but unpatched, result is %d\n",r);

    unsigned char* executable=malloc(4096);

    sendstringf("Allocated memory for the executable copy at %6\n", executable);

    displayline("1: crc=%x\n", generateCRC((unsigned char*)newtestfunction, size));

    r=dbvm_cloak_readExecutable(PA, executable);
    displayline("2: crc=%x\n", generateCRC((unsigned char*)newtestfunction, size));
    sendstringf("dbvm_cloak_readExecutable returned %d\n",r);

    if (r==0)
    {

      int beforepatch=generateCRC((unsigned char*)newtestfunction, size);
      displayline("3: crc=%x\n", generateCRC((unsigned char*)newtestfunction, size));

      sendstringf("Applying patch to executable memory\n");
      for (i=0; i<5; i++)
        executable[(QWORD)(&cloakTestFunctionInstruction)-((QWORD)(cloakTestFunction))+i-2]=0x90;

      displayline("4: crc=%x\n", generateCRC((unsigned char*)newtestfunction, size));

      r=dbvm_cloak_writeExecutable(PA, executable);
      sendstringf("dbvm_cloak_writeExectuable returned %d\n",r);
      displayline("5: crc=%x\n", generateCRC((unsigned char*)newtestfunction, size));

      int afterpatch=generateCRC((unsigned char*)newtestfunction, size);

      displayline("6: crc=%x\n", generateCRC((unsigned char*)newtestfunction, size));

      if (r==0)
      {
        sendstringf("Calling newtestfunction: (cloaked, patched)\n");
        r=newtestfunction();
        sendstringf("After cloak and patch ,result of cloakTestFunction() is %d\n", r);
        displayline("Cloak active and patched, result is %d\n",r);
        displayline("7: crc=%x\n", generateCRC((unsigned char*)newtestfunction, size));
      }

    }




    r=dbvm_cloak_deactivate(PA);
    displayline("8: crc=%x\n", generateCRC((unsigned char*)newtestfunction, size));
    sendstringf("dbvm_cloak_deactivate(%6) returned %d\n", PA, r);
    sendstringf("\n");




  }
  else
    sendstring("Error, no DBVM loaded\n");


}


void dbvm_watch_execute_test(void)
{
  int DBVMVersion=getDBVMVersion();
  int watchid=-1;


  if (DBVMVersion)
  {
    QWORD PA;
    void *results=malloc(8192);
    zeromemory(results,8192);

    writeMSR(EFER_MSR, readMSR(EFER_MSR) & ~(1<<11)); //this is the guest, disable no execute (test)


    sendstringf("DBVM version is %x\n", DBVMVersion);



    PA=VirtualToPhysical((void *)testfunction);


    int r;


    r=dbvm_watch_executes(PA,&watchid);
    sendstringf("dbvm_watch_read returned %d. ID=%d\n", r,watchid);

    sendstringf("Going to execute testfunction\n");
    testfunction();
    sendstringf("testfunction returned\n");

    sendstringf("It should have recorded by now\n");

    int size=8192;
    int ec=dbvm_watch_retrievelog(watchid, results, &size);

    sendstringf("dbvm_watch_retrievelog returned %d. size=%d\n", ec, size);


    dbvm_watch_delete(watchid);

    free(results);
  }
  else
    sendstring("Error, no DBVM loaded\n");
}


void dbvm_watch_reads_test(void)
{
  int DBVMVersion=getDBVMVersion();
  volatile int *testvar;
  volatile int *testvar2;
  int watchid=-1;


  if (DBVMVersion)
  {
    QWORD PA;
    void *results=malloc(8192);
    zeromemory(results,8192);

    sendstringf("DBVM version is %x\n", DBVMVersion);

    testvar=(int *)malloc(4096);

    testvar2=&testvar[64];
    *testvar=123;
    *testvar2=456;

    PA=VirtualToPhysical((void *)testvar);


    int r;




    sendstringf("testvar has been allocated at %6 which is PA %6\n", testvar, PA);

    r=dbvm_watch_reads(PA,&watchid);
    sendstringf("dbvm_watch_read returned %d. ID=%d\n", r,watchid);


    sendstringf("Reading from same page as testvar but different location (%6)\n", testvar2);
    asm volatile ("": : :"memory");
    sendstringf("testvar2=%d\n", *testvar2);
    asm volatile ("": : :"memory");

    sendstringf("reading testvar\n");
    asm volatile ("": : :"memory");
    sendstringf("testvar=%d\n", *testvar);
    asm volatile ("": : :"memory");

    sendstringf("Writing to the same page as testvar but different location (%6)\n", testvar2);
    asm volatile ("": : :"memory");
    *testvar2=789;
    asm volatile ("": : :"memory");
    sendstringf("testvar2=%d\n", *testvar2);
    asm volatile ("": : :"memory");

    sendstringf("Writing to testvar\n");
    asm volatile ("": : :"memory");
    *testvar=321;
    asm volatile ("": : :"memory");
    sendstringf("testvar=%d\n", *testvar);
    asm volatile ("": : :"memory");

    sendstringf("It should have recorded by now\n");

    int size=8192;
    int ec=dbvm_watch_retrievelog(watchid, results, &size);
    sendstringf("dbvm_watch_retrievelog returned %d. size=%d\n", ec, size);


    dbvm_watch_delete(watchid);

    free(results);
    free((void*)testvar);
  }
  else
    sendstring("Error, no DBVM loaded\n");
}



void dbvm_watch_writes_test(void)
{
  int DBVMVersion=getDBVMVersion();
  volatile int *testvar;
  volatile int *testvar2;
  int watchid=-1;


  if (DBVMVersion)
  {
    QWORD PA;
    void *results=malloc(8192);
    zeromemory(results,8192);

    sendstringf("DBVM version is %x\n", DBVMVersion);

    testvar=(int *)malloc(4096);

    testvar2=&testvar[64];
    *testvar=123;
    *testvar2=456;

    PA=VirtualToPhysical((void *)testvar);


    int r;




    sendstringf("testvar has been allocated at %6 which is PA %6\n", testvar, PA);

    r=dbvm_watch_writes(PA,&watchid);
    sendstringf("dbvm_watch_writes returned %d. ID=%d\n", r,watchid);


    sendstringf("Reading from same page as testvar but different location (%6)\n", testvar2);
    asm volatile ("": : :"memory");
    sendstringf("testvar2=%d\n", *testvar2);
    asm volatile ("": : :"memory");

    sendstringf("reading testvar\n");
    asm volatile ("": : :"memory");
    sendstringf("testvar=%d\n", *testvar);
    asm volatile ("": : :"memory");

    sendstringf("Writing to the same page as testvar but different location (%6)\n", testvar2);
    asm volatile ("": : :"memory");
    *testvar2=789;
    asm volatile ("": : :"memory");
    sendstringf("testvar2=%d\n", *testvar2);
    asm volatile ("": : :"memory");

    sendstringf("Writing to testvar\n");
    asm volatile ("": : :"memory");
    *testvar=321;
    asm volatile ("": : :"memory");
    sendstringf("testvar=%d\n", *testvar);
    asm volatile ("": : :"memory");

    sendstringf("It should have recorded by now\n");

    int size=8192;
    int ec=dbvm_watch_retrievelog(watchid, results, &size);
    sendstringf("dbvm_watch_retrievelog returned %d. size=%d\n", ec, size);


    dbvm_watch_delete(watchid);

    free(results);
    free((void*)testvar);
  }
  else
    sendstring("Error, no DBVM loaded\n");

}
