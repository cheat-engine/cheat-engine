/*
 * vmeventhandler_amd.c
 *
 *  Created on: May 22, 2013
 *      Author: eric
 */

#include "vmeventhandler_amd.h"
#include "vmeventhandler.h"
#include "main.h"
#include "common.h"
#include "vmpaging.h"
#include "keyboard.h"
#include "neward.h"
#include "vmcall.h"
#include "vmmhelper.h"
#include "apic.h"
#include "msrnames.h"
#include "nphandler.h"
#include "epthandler.h"
#include "vmxemu.h"

//GIF_EMUMETHOD : 1=Intercept external interrupts and trigger them back when GIF is back to 1
//                2=Use interrupt masking, and don't set the IF flag, so interrupts do not get sent to the VM (faster)

#define GIF_EMUMETHOD 2


int c=0;

criticalSection debugoutput={.name="debugoutput", .debuglevel=2};


int isPrefix(unsigned char b)
{
  switch(b)
  {
    case 0x26:
    case 0x2e:
    case 0x36:
    case 0x3e:
    case 0x64:
    case 0x65:
    case 0x66:
    case 0x67:
    case 0xf0:
    case 0xf2:
    case 0xf3:
      return TRUE;
  }

  return FALSE;
}

int logeverything=0;



int handleVMEvent_amd(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave)
{
  int i;
  int wasStep;
/*
  c++;
  if (c>50)
    while (1);*/





  wasStep=currentcpuinfo->singleStepping.ReasonsPos>0;

  nosendchar[getAPICID()]=1; //currentcpuinfo->vmcb->EXITCODE==0x400;//everything but npt //!logeverything;

  if (currentcpuinfo->showall)
   nosendchar[getAPICID()]=0;


 // nosendchar[getAPICID()]=((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->IF==1; //don't bother if IF==1

 // if (currentcpuinfo->vmcb_GIF==0)
 //   nosendchar[getAPICID()]=0;


  currentcpuinfo->LastVMCall=currentcpuinfo->vmcb->EXITCODE;




  PEXITINTINFO eii=(PEXITINTINFO)&currentcpuinfo->vmcb->EXITINTINFO;

  if ((eii->Valid) && (currentcpuinfo->vmcb->EXITCODE!=VMEXIT_NPF))
  {
    nosendchar[getAPICID()]=0;
    sendstringf("ExitCode: %x - EXITINTINFO is valid and not a NPF\n", currentcpuinfo->vmcb->EXITCODE);
    sendstringf("EXITINTINFO=%x\n", currentcpuinfo->vmcb->EXITINTINFO);
  }


  sendstringf("%d: %x:%6 - getAPICID()=%d VM_HSAVE_PA_MSR=%6 RFLAGS=%x\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP, getAPICID(),readMSR(VM_HSAVE_PA_MSR), currentcpuinfo->vmcb->RFLAGS);
  sendstringf("IF==%d\n",((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->IF);

  sendstringf("GIF==%d\n", currentcpuinfo->vmcb_GIF);
  sendstringf("V_TPR=%d V_IRQ=%d\n", currentcpuinfo->vmcb->V_TPR, currentcpuinfo->vmcb->V_IRQ);



  sendstringf("currentcpuinfo->cpunr=%d (nr:%d)\n", currentcpuinfo->cpunr, currentcpuinfo->eventcounter[0]);
  currentcpuinfo->eventcounter[0]++;


  sendstringf("currentcpuinfo->vmcb->EXITCODE=%x\n", currentcpuinfo->vmcb->EXITCODE);
  sendstringf("EXITINTINFO=%x\nEXITINFO1=%x\nEXITINFO2=%x\n", currentcpuinfo->vmcb->EXITINTINFO, currentcpuinfo->vmcb->EXITINFO1, currentcpuinfo->vmcb->EXITINFO2);

  sendstringf("currentcpuinfo->vmcb->VMCB_CLEAN_BITS = %8\n", currentcpuinfo->vmcb->VMCB_CLEAN_BITS);

  currentcpuinfo->vmcb->VMCB_CLEAN_BITS=0xffffffff; //nothing cached changed (yet)

  if (currentcpuinfo->eptUpdated==1)
  {
    sendstring("ept was updated\n");
    currentcpuinfo->eptUpdated=0;
    currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(1 << 4);
    ept_invalidate();
  }







  if ((currentcpuinfo->singleStepping.ReasonsPos) && (currentcpuinfo->vmcb->EXITCODE!=VMEXIT_NPF)) //ANYTHING except NPF is counted as a single step (in case of interrupts)
  {
    //logeverything=1; //start the log

    //restore EFER
    currentcpuinfo->vmcb->EFER=currentcpuinfo->singleStepping.PreviousEFER;
    currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(1<< 5); //crx and EFER

    if (currentcpuinfo->singleStepping.LastInstructionWasSyscall)
    {

      if (currentcpuinfo->vmcb->RIP!=currentcpuinfo->vmcb->LSTAR)
      {
        nosendchar[getAPICID()]=0;
        sendstringf("FUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUCK\n");
      }

      sendstringf("%d: exit. singleStepping.LastInstructionWasSyscall:  ExitCode=%x\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->EXITCODE);
      sendstringf("%d: RIP=%6 (rflags=%x) - Last instruction was a syscall.  Changing R11(%x) to hide the TF flag and restore the flag mask\n",currentcpuinfo->cpunr, currentcpuinfo->vmcb->RIP, currentcpuinfo->vmcb->RFLAGS, vmregisters->r11);

      vmregisters->r11 &= ~(QWORD)(1<<8);

      currentcpuinfo->vmcb->SFMASK=currentcpuinfo->singleStepping.PreviousFMASK;
     // currentcpuinfo->vmcb->VMCB_CLEAN_BITS=0;

      sendstringf("%d: r11=%x\n", currentcpuinfo->cpunr, vmregisters->r11);

      //efer is restored, fmask is restored, going to handle the step like always
    }

    if (currentcpuinfo->vmcb->EXITCODE==VMEXIT_EXCP6)
    {
     // nosendchar[getAPICID()]=0;
      sendstringf("%d: UD exception while single stepping at %6.  Could be a syscall\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->RIP);

      int error;
      UINT64 pagefaultaddress;

      sendstringf("%d: csbase=%6\n rip=%6\n",currentcpuinfo->cpunr, currentcpuinfo->vmcb->cs_base, currentcpuinfo->vmcb->RIP);

      //nosendchar[getAPICID()]=0;
      sendstringf("%d: mapping vm memory\n", currentcpuinfo->cpunr);

      int size=15;
      unsigned char *bytes=(unsigned char *)mapVMmemory(currentcpuinfo, currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP, size, &error, &pagefaultaddress);
      if (!bytes)
      {
        size=pagefaultaddress-currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP;
        bytes=mapVMmemory(currentcpuinfo, currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP, size, &error, &pagefaultaddress);
      }

      sendstringf("%d: after mapping vm memory. Bytes=%6\n", currentcpuinfo->cpunr, bytes);
      nosendchar[getAPICID()]=1;

      if (bytes)
      {
        int start=-1;
        //check if it's a syscall (0f 05)
        for (i=0; i<15; i++)
          if (isPrefix(bytes[i])==FALSE)
          {
            start=i;
            break;
          }

        if ((start==-1) || (start>=14))
        {
          sendstringf("Just a bunch of prefixes. WTF?\n");
          currentcpuinfo->vmcb->EVENTINJ=currentcpuinfo->vmcb->EXITINTINFO; //retrigger
          unmapVMmemory(bytes, size);
          return 0;
        }
        else
        {
          //valid instruction
          if ((bytes[start]==0x0f) && (bytes[start+1]==0x05))
          {
            sendstringf("%d: it IS a syscall\n", currentcpuinfo->cpunr);
            currentcpuinfo->singleStepping.LastInstructionWasSyscall=1;
            currentcpuinfo->singleStepping.PreviousFMASK=currentcpuinfo->vmcb->SFMASK;

            currentcpuinfo->vmcb->SFMASK=currentcpuinfo->singleStepping.PreviousFMASK & ~(QWORD)(1<<8);
            sendstringf("%d: old IA32_FMASK was %6\nnew IA32_FMASK is  %6\n", currentcpuinfo->cpunr, currentcpuinfo->singleStepping.PreviousFMASK, currentcpuinfo->vmcb->SFMASK);


            sendstringf("%d: current rflags before syscall is %x\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->RFLAGS);

            sendstringf("%d: Next step should be at %6\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->LSTAR);

            //
           // currentcpuinfo->vmcb->VMCB_CLEAN_BITS=0;


            //efer was already restored, so repeat this instruction
            unmapVMmemory(bytes, size);
            return 0;
          }
        }

        unmapVMmemory(bytes, size);
      }
      else
      {
        nosendchar[getAPICID()]=0;
        sendstringf("Failed reading the memory that was just executed and caused an UD... Guess it's not a syscall... fuuuuu (error=%d pagefaultaddress=%6)\n", error, pagefaultaddress);
        currentcpuinfo->vmcb->EVENTINJ=currentcpuinfo->vmcb->EXITINTINFO; //retrigger
        return 0;
      }
    }

    //nosendchar[getAPICID()]=0;
    //sendstringf("AMD Handler: currentcpuinfo->singleStepping.ReasonsPos=%d Calling handleSingleStep()\n",currentcpuinfo->singleStepping.ReasonsPos);
    if (currentcpuinfo->singleStepping.ReasonsPos>=2)
    {
      nosendchar[getAPICID()]=0;
      sendstringf("Handling multiple single step reasons\n");
    }


    handleSingleStep(currentcpuinfo, vmregisters, fxsave);
    sendstringf("%d: After handleSingleStep()\n", currentcpuinfo->cpunr);

    if (currentcpuinfo->vmcb->EXITCODE==VMEXIT_EXCP1)
    {
      sendstringf("%d: It was an int1 so skip this (cs:rip=%x:%6  rflags=%x)\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP, currentcpuinfo->vmcb->RFLAGS); //Todo: Check if it was a int1 or other BP before the step


      //sendstringf("%6:\n", currentcpuinfo->vmcb->RIP);

      if (currentcpuinfo->singleStepping.PreviousTFState)
      {
        nosendchar[getAPICID()]=0;
        sendstring("Previous TF state was 1. calling ept_handleHardwareBreakpoint");
        ept_handleHardwareBreakpoint(currentcpuinfo, vmregisters, fxsave);
      }

      //no further handling is needed
      return 0;
    }

    sendstringf("%d: Need to do some further handling (rflags=%x)\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->RFLAGS);//eg external interrupts
  }
  else
  {
    if (currentcpuinfo->singleStepping.ReasonsPos)
      sendstringf("AMD Handler: currentcpuinfo->singleStepping.ReasonsPos=%d and EXITCODE IS AN NPF\n",currentcpuinfo->singleStepping.ReasonsPos);
  }

//  if (currentcpuinfo->NP_Cloak.ActiveRegion)
//  {

//  }




  switch (currentcpuinfo->vmcb->EXITCODE)
  {

    case VMEXIT_CR3_WRITE:
    {
      int gpr=currentcpuinfo->vmcb->EXITINFO1 & 0xf;
      int result;
      QWORD value=getRegister(currentcpuinfo, vmregisters, gpr);

      sendstringf("cpu %d: CR3 write operation: %6 (gpr=%d  value=%6)\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->EXITINFO1, gpr, value);

      result=setVM_CR3(currentcpuinfo, vmregisters, value);

      if (CR3ValueLog==NULL) //stop logging
        currentcpuinfo->vmcb->InterceptCR0_15Write&=~(1<<3);

      if (result==0)
        currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;

      return result;
    }

    case VMEXIT_INTR:
    {

      if (currentcpuinfo->vmcb->InterceptINTR==0) return 0; //not handled anymore

      nosendchar[getAPICID()]=1;
      sendstringf("%d: VMEXIT_INTR: EXITINFO1=%6 EXITINFO2=%6 EXITINTINFO=%6 RFLAGS=%x V_IRQ=%d V_TPR=%d\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->EXITINFO1, currentcpuinfo->vmcb->EXITINFO2, currentcpuinfo->vmcb->EXITINTINFO, currentcpuinfo->vmcb->RFLAGS, currentcpuinfo->vmcb->V_IRQ, currentcpuinfo->vmcb->V_TPR);
      sendstringf("my RSP=%6\n", getRSP());



      while(1)
      {
        int interruptnr=-1;

        try
        {
          //sendstringf("Checking for interrupt\n");
          //setCR8(0);
          asm("stgi\nsti");
          asm("nop");
          //no interrupt...
          asm("clgi\ncli");
          //sendstringf("No interrupt\n");
        }
        except
        {
          interruptnr=lastexception&0xff;

          sendstringf("Received interrupt %d  (%x)\n", interruptnr, interruptnr);
          sendstringf("my RSP=%6\n", getRSP());
        }
        tryend

        asm("clgi\ncli");
        asm("nop");
        asm("nop");
        asm("nop");

        setCR8(currentcpuinfo->vmcb->V_TPR);

        if (interruptnr!=-1)
        {
          sendstringf("Adding pending interrupt %d to the list\n",interruptnr);

          if (currentcpuinfo->vmcb_pending[interruptnr >> 4])
          {
            nosendchar[getAPICID()]=0;
            sendstringf("WARNING. A pending external interrupt in this IRQ was already set (%d)\n",currentcpuinfo->vmcb_pending);

            while (1);
          }

          currentcpuinfo->vmcb_pending[interruptnr >> 4]=interruptnr;



        }
        else break;

      } //test while

      sendstringf("done. Resuming\n");

      return 0;

    }

    case VMEXIT_EXCP0:
    case VMEXIT_EXCP2:
    case VMEXIT_EXCP4:
    case VMEXIT_EXCP5:
    case VMEXIT_EXCP6:
    case VMEXIT_EXCP7:
    case VMEXIT_EXCP8:
    case VMEXIT_EXCP9:
    case VMEXIT_EXCP10:
    case VMEXIT_EXCP11:
    case VMEXIT_EXCP12:
    case VMEXIT_EXCP13:
    case VMEXIT_EXCP15:
    case VMEXIT_EXCP16:
    case VMEXIT_EXCP17:
    case VMEXIT_EXCP18:
    case VMEXIT_EXCP19:
    case VMEXIT_EXCP20:
    case VMEXIT_EXCP21:
    case VMEXIT_EXCP22:
    case VMEXIT_EXCP23:
    case VMEXIT_EXCP24:
    case VMEXIT_EXCP25:
    case VMEXIT_EXCP26:
    case VMEXIT_EXCP27:
    case VMEXIT_EXCP28:
    case VMEXIT_EXCP29:
    case VMEXIT_EXCP30:
    case VMEXIT_EXCP31:
    {
      //shouldn't happen (but used as debug to find exceptions)
      nosendchar[getAPICID()]=0;
      sendstringf("One of the other exceptions (%d) (not INTR) got intercepted (wasStep=%d EXITINTINFO=%8)\n", (int)(currentcpuinfo->vmcb->EXITCODE-VMEXIT_EXCP0), wasStep, currentcpuinfo->vmcb->EXITINTINFO);

      sendvmstate(currentcpuinfo, vmregisters);
      ShowCurrentInstructions(currentcpuinfo);


      currentcpuinfo->vmcb->EVENTINJ=0;
      currentcpuinfo->vmcb->inject_Vector=currentcpuinfo->vmcb->EXITCODE-VMEXIT_EXCP0;
      currentcpuinfo->vmcb->inject_Type=3;

      while (1);
      //currentcpuinfo->vmcb->inject_EV
     // currentcpuinfo->vmcb->EXITINTINFO; //retrigger
      return 0;

    }

    case VMEXIT_EXCP1:
    {
      int isFault=0; //on amd it seems it ever ever set RF. isDebugFault(currentcpuinfo->vmcb->DR6, currentcpuinfo->vmcb->DR7);

      //int1 breakpoint
      nosendchar[getAPICID()]=0; //urrentcpuinfo->vmcb->CPL!=3;
      sendstringf("%d: INT1 breakpoint at %6 (rflags=%x)\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->RIP, currentcpuinfo->vmcb->RFLAGS);

      if (ept_handleHardwareBreakpoint(currentcpuinfo, vmregisters, fxsave))
        return 0;


      if (wasStep)
      {
        sendstringf("wasStep=1 so retrigger\n");
        currentcpuinfo->vmcb->EVENTINJ=currentcpuinfo->vmcb->EXITINTINFO; //retrigger
        return 0;
      }
      else
      {
        sendstringf("wasStep=0\n");
      }



      sendstringf("dr0=%x\n", getDR0());
      sendstringf("dr1=%x\n", getDR1());
      sendstringf("dr2=%x\n", getDR2());
      sendstringf("dr3=%x\n", getDR3());


      sendstringf("dr6=%x\n", currentcpuinfo->vmcb->DR6);
      sendstringf("dr7=%x\n", currentcpuinfo->vmcb->DR7);
      sendstringf("rflags=%x\n", currentcpuinfo->vmcb->RFLAGS);

      sendstringf("isFault=%d\n", isFault);


      if (((PregDR7)(&currentcpuinfo->vmcb->DR7))->GD)
      {
        //GD is set, unset it (should already be unset)
        ((PregDR7)(&currentcpuinfo->vmcb->DR7))->GD=0;
        currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(1 << 6); //tell the cpu it got changed
      }



      if ((int1redirection_idtbypass==0) || (ISREALMODE(currentcpuinfo)))
      {
        sendstring("Realmode bp or No idtbypass\n");
        currentcpuinfo->vmcb->inject_Type=3; //exception
        currentcpuinfo->vmcb->inject_Vector=int1redirection;
        currentcpuinfo->vmcb->inject_Valid=1;
        currentcpuinfo->vmcb->inject_EV=0;

        if (isFault) //set the RF flag in rflags
          ((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->RF=1;

        return 0;
      }
      else
      {
        //idt bypass method
        int result;

        sendstringf("before:\n");
        sendvmstate(currentcpuinfo, vmregisters);
        result=emulateExceptionInterrupt(currentcpuinfo, vmregisters, int1redirection_idtbypass_cs, int1redirection_idtbypass_rip, 0, 0, isFault);
        sendstringf("after:\n");
        sendvmstate(currentcpuinfo, vmregisters);

        return result;
      }

      break;
    }


    case VMEXIT_EXCP3:
    {
      nosendchar[getAPICID()]=0;
      sendstringf("Int3 BP\n");
      sendstringf("EXITINTINFO=%x\nEXITINFO1=%x\nEXITINFO2=%x\n", currentcpuinfo->vmcb->EXITINTINFO, currentcpuinfo->vmcb->EXITINFO1, currentcpuinfo->vmcb->EXITINFO2);
      sendstringf("RIP=%6\n", currentcpuinfo->vmcb->RIP);
      sendstringf("nRIP=%6\n", currentcpuinfo->vmcb->nRIP);

      if (handleSoftwareBreakpoint(currentcpuinfo, vmregisters, fxsave))
      {
        //sendstring("VMEXIT_EXCP3: handleSoftwareBreakpoint handled it. Returning 0\n");
        return 0;
      }

      nosendchar[getAPICID()]=0;
      sendstringf("GIF=%d\n", currentcpuinfo->vmcb_GIF);
      sendstringf("%d: Unhandled int3 bp at %6\n",currentcpuinfo->cpunr, isAMD?currentcpuinfo->vmcb->RIP:vmread(vm_guest_rip));

#ifdef DEBUG
      sendvmstate(currentcpuinfo, vmregisters);
      ShowCurrentInstructions(currentcpuinfo);
#endif


      //set RIP to after the instruction
      if (AMD_hasNRIPS)
      {
        currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;
      }
      else
      {
        //scan where the 0xcc is...
        int error;
        UINT64 pagefaultaddress;
        unsigned char *bytes=(unsigned char *)mapVMmemory(currentcpuinfo, currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP, 15, &error, &pagefaultaddress);
        if (!bytes)
            bytes=mapVMmemory(currentcpuinfo, currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP, pagefaultaddress-currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP, &error, &pagefaultaddress);


        int i;
        for (i=0; i<15; i++)
        {
          if (bytes[i]==0xcc)
          {
            currentcpuinfo->vmcb->RIP+=i+1;
            break;
          }
        }

        unmapVMmemory(bytes, 15);
      }
      sendstringf("new RIP=%6\n", currentcpuinfo->vmcb->RIP);



      //and raise the interrupt
      if ((int3redirection_idtbypass==0) || (ISREALMODE(currentcpuinfo)))
      {
        sendstring("Realmode bp or No idtbypass\n");
        currentcpuinfo->vmcb->EVENTINJ=0;
        currentcpuinfo->vmcb->inject_Type=3; //exception
        currentcpuinfo->vmcb->inject_Vector=int3redirection;
        currentcpuinfo->vmcb->inject_Valid=1;
        currentcpuinfo->vmcb->inject_EV=0;
        return 0;
       }
       else
       {
         //idt bypass method
         int result;

         sendstringf("before:\n");
         sendvmstate(currentcpuinfo, vmregisters);
         result=emulateExceptionInterrupt(currentcpuinfo, vmregisters, int3redirection_idtbypass_cs, int3redirection_idtbypass_rip, 0, 0, 0); //it' a trap!
         sendstringf("after:\n");
         sendvmstate(currentcpuinfo, vmregisters);

         return result;
       }



      return 1;
    }

    case VMEXIT_EXCP14:
    {
      DWORD errorcode=currentcpuinfo->vmcb->EXITINFO1;
      QWORD cr2=currentcpuinfo->vmcb->EXITINFO2;


      nosendchar[getAPICID()]=(((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->IF) && (currentcpuinfo->vmcb->V_TPR<=1); //don't bother if IF==1



      sendstringf("INT14 breakpoint\n");
      sendstringf("RFLAGS=%x", currentcpuinfo->vmcb->RFLAGS);
      sendstringf("errorcode=%x\n", errorcode);
      sendstringf("cr2=%x\n", cr2);
      sendstringf("currentcpuinfo->vmcb->CR2=%x\n", currentcpuinfo->vmcb->CR2);

      sendstringf("EXITINTINFO=%x\n", currentcpuinfo->vmcb->EXITINTINFO);

      sendvmstate(currentcpuinfo, vmregisters);
      ShowCurrentInstructions(currentcpuinfo);

      if (((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->IF==0)
      {
        nosendchar[getAPICID()]=0;
        sendstringf("Pagefault with IF 0\n");
        while (1);
      }




      setCR2(cr2); //<---bochs bug? My real hardware does not use this CR2 value but the one in the vmcb. (no matter)
      currentcpuinfo->vmcb->CR2=cr2;
      currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(1 << 9); //cr2 got changed


      ((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->RF=1;



      if ((int14redirection_idtbypass==0) || (ISREALMODE(currentcpuinfo)))
      {
        sendstring("Realmode int14 or No idtbypass\n");

        if (ISREALMODE(currentcpuinfo))
        {
          //todo: setup a realmode paging setup and set cr3 to that and intercept cr3 read/write and cr0 read/write

        }



        currentcpuinfo->vmcb->inject_Type=3; //exception
        currentcpuinfo->vmcb->inject_Vector=int14redirection;
        currentcpuinfo->vmcb->inject_Valid=1;
        currentcpuinfo->vmcb->inject_EV=1;
        currentcpuinfo->vmcb->inject_ERRORCODE=errorcode;

        return 0;
      }
      else
      {
        //idt bypass method
        int result;

        sendstringf("before:\n");
        sendvmstate(currentcpuinfo, vmregisters);
        result=emulateExceptionInterrupt(currentcpuinfo, vmregisters, int14redirection_idtbypass_cs, int14redirection_idtbypass_rip, 1, errorcode, 1);
        sendstringf("after:\n");
        sendvmstate(currentcpuinfo, vmregisters);

        return result;
      }

      break;
    }


    case VMEXIT_SWINT: //software interrupts (INTn)
    {
      int handled=0;
      int intnr;
      int instructionlength=0;

      //nosendchar[getAPICID()]=0; //this seems to work well enough
      //sendstringf("VMEXIT_SWINT: Software interrupt\n");

      if (AMD_hasDecodeAssists)
      {
        sendstringf("Using EXITINFO1 to get the interrupt\n");
        intnr=currentcpuinfo->vmcb->EXITINFO1;

      }
      else
      {
        //get the interrupt nr from the instruction
        //map CS:RIP and read out the instruction
        int error;
        UINT64 pagefaultaddress;

        //sendstringf("DB:2:currentcpuinfo->AvailableVirtualAddress=%6\n", currentcpuinfo->AvailableVirtualAddress);


        unsigned char *bytes=mapVMmemory(currentcpuinfo, currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP, 15, &error, &pagefaultaddress);

        if (!bytes)
          bytes=mapVMmemory(currentcpuinfo, currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP, pagefaultaddress-currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP, &error, &pagefaultaddress);

        int i;
        for (i=0; i<15; i++)
        {
          if (bytes[i]==0xcd)
          {
            intnr=bytes[i+1];
            instructionlength=i+2;
            break;
          }
        }
        sendstringf("\n");

        unmapVMmemory(bytes,15);
      }


      if ((ISREALMODE(currentcpuinfo)/* || ((currentcpuinfo->vmcb->RFLAGS >> 17) & 1) */) && (intnr==0x15))
      {


        //todo: Split this up into a function used by both intel and amd. Right now it's basically just a copy/paste with minor changes

        //realmode of Virtual 8086 mode and the interrupt matches
        //sendstringf("INT 0x15\n");
        //sendstringf("RAX=%6\n", currentcpuinfo->vmcb->RAX);

        if ((currentcpuinfo->vmcb->RAX & 0xff00)==0x8800)
        {
          sendstringf("Handling int 15h, AH=88\n\r");
          currentcpuinfo->vmcb->RAX=(currentcpuinfo->vmcb->RAX & 0xffffffff00000000ULL)+0xfc00;

         // RFLAGS
         // currentcpuinfo->vmcb->RFLAGS

          //clear the carry flag
          ((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->CF=0;

          handled=1;
        }

        if ((currentcpuinfo->vmcb->RAX & 0xffff)==0xe801)
        {


          DWORD between1and16MB=0; //in KB, max 3c00  (between 0x100000 and 0x1000000)
          DWORD above16MB=0;





          sendstringf("Handling int 15h, AH=e801. ARDcount=%d \n\r",ARDcount);


          for (i=0; i<ARDcount; i++)
          {
            sendstringf("i=%d\n",i);
            sendstringf("between1and16MB=%x\n",between1and16MB);
            sendstringf("above16MB=%x\n",above16MB);

            if (fakeARD[i].BaseAddrHigh>0)
              continue;



            if ((fakeARD[i].Type==1) && ((fakeARD[i].BaseAddrLow+fakeARD[i].LengthLow)>0x100000))
            {
              //upper mem, and available
              DWORD start=fakeARD[i].BaseAddrLow;
              DWORD stop=fakeARD[i].BaseAddrLow+fakeARD[i].LengthLow;

              if (start<0x100000)
                start=0x100000;

              if (start<0x1000000)
              {
                DWORD tempstop=stop;
                if (tempstop>0x1000000)
                  tempstop=0x1000000;

                between1and16MB+=tempstop-start;
                start=tempstop;
              }

              if (start>=0x1000000)
                above16MB+=stop-start;


            }
          }

          sendstringf("After for loop\n");
          sendstringf("between1and16MB=%x\n",between1and16MB);
          sendstringf("above16MB=%x\n",above16MB);

          currentcpuinfo->vmcb->RAX=(currentcpuinfo->vmcb->RAX & 0xffffffffffff0000ULL) + (between1and16MB / 1024);
          vmregisters->rbx=(vmregisters->rbx & 0xffffffffffff0000ULL) + (above16MB / (64*1024));
          vmregisters->rcx=(vmregisters->rcx & 0xffffffffffff0000ULL) + (between1and16MB / 1024);
          vmregisters->rdx=(vmregisters->rdx & 0xffffffffffff0000ULL) + (above16MB / (64*1024));

          ((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->CF=0; //clear carry

          handled=1;
        }
      }

      if ((currentcpuinfo->vmcb->RAX & 0xffff)==0xe820)
      {
        int startindex=(ULONG)vmregisters->rbx;

        //return 1;


        sendstringf("Handling int 15h, ax=E820 (maxindex=%d)\n\r",ARDcount-1);
        sendstringf("startindex=%d vmregisters->rcx=%d\n\r",startindex,vmregisters->rcx);


        if (((ULONG)vmregisters->rcx >= 20) && ((ULONG)vmregisters->rdx==0x534D4150) && (startindex<ARDcount))
        {
          //call=ok

          PARD output=(PARD)(currentcpuinfo->vmcb->es_base+(vmregisters->rdi & 0xffff)); //es:di
          int totalentries=(ULONG)vmregisters->rcx/20;
          int o,i;
          currentcpuinfo->vmcb->RAX=(currentcpuinfo->vmcb->RAX & 0xffffffff00000000ULL) + 0x534D4150;

          sendstringf("totalentries=%d\n\r",totalentries);

          i=startindex;
          o=0;
          while ((o<totalentries) && (i<ARDcount) )
          {
            output[o]=fakeARD[i];
            if (output[o].Type==255)
              output[o].Type=2;

            o++;
            i++;
          }

          //set next index, i already contains the value of the next index
          if (i>=ARDcount)
          {
            vmregisters->rbx=(vmregisters->rbx & 0xffffffff00000000ULL) + 0;
          }
          else
          {
            vmregisters->rbx=(vmregisters->rbx & 0xffffffff00000000ULL) + i;
          }

          vmregisters->rcx=(vmregisters->rcx & 0xffffffff00000000ULL) + (o*20);
          ((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->CF=0; //clear carry

          sendstringf("Handled int15h ax=e820. ECX=%8 \n\r",(ULONG)vmregisters->rcx);

          handled=1;

        }
        else
        {
          //return error
          sendstringf("Returning error\n\r");
          ((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->CF=1; //set carry
          handled=1;
        }


      }
      else
      {
       // sendstringf("INT 0x%x .  Not INT 0x15 or not in realmode so handle it normally (inject this interrupt)\n", intnr);
      }

      //adjust RIP
      if (AMD_hasNRIPS)
        currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;
      else
        currentcpuinfo->vmcb->RIP+=instructionlength;

      if (!handled)
      {
        {
         // sendstringf("Injecting interrupt\n");
          currentcpuinfo->vmcb->inject_Type=4; //software int
          currentcpuinfo->vmcb->inject_Vector=intnr;
          currentcpuinfo->vmcb->inject_Valid=1;
          currentcpuinfo->vmcb->inject_EV=0;
        }
      }

      return 0;
    }

    case VMEXIT_RDTSC:
    {
      int r;
      r=handle_rdtsc(currentcpuinfo, vmregisters);
      currentcpuinfo->lastTSCTouch=_rdtsc();
      return r;
    }

    case VMEXIT_RDTSCP:
    {
      int r=handle_rdtsc(currentcpuinfo, vmregisters);
      currentcpuinfo->lastTSCTouch=_rdtsc();

      vmregisters->rcx=readMSR(IA32_TSC_AUX_MSR);
      return r;
    }

    case VMEXIT_MSR:
    {
      int error=0;
      int exceptionnr=0;
      unsigned int msr=vmregisters->rcx & 0xffffffff;

      sendstring("VMEXIT_MSR\n");
      sendstringf("EXITINFO1=%d\n", currentcpuinfo->vmcb->EXITINFO1);
      sendstringf("EXITINFO2=%d\n", currentcpuinfo->vmcb->EXITINFO2);


      if (currentcpuinfo->vmcb->EXITINFO1)
      {

        QWORD newvalue=((vmregisters->rdx & 0xffffffff) << 32) + (currentcpuinfo->vmcb->RAX & 0xffffffff);

        sendstringf("WRITE %6 to msr %x\n", newvalue, vmregisters->rcx);

        switch (msr)
        {
          case IA32_TIME_STAMP_COUNTER:
            sendstringf("write to IA32_TIME_STAMP_COUNTER\n");
            writeMSR(IA32_TIME_STAMP_COUNTER, newvalue);

            currentcpuinfo->lowestTSC=0;
            globalTSC=newvalue;
            currentcpuinfo->lastTSCTouch=newvalue;

            lowestTSC=0;
            break;

          case IA32_TSC_ADJUST:
            sendstringf("write to IA32_TSC_ADJUST\n");
            writeMSR(IA32_TSC_ADJUST, newvalue);
            globalTSC=_rdtsc();
            currentcpuinfo->lasttsc=_rdtsc();
            currentcpuinfo->lastTSCTouch=globalTSC;
            currentcpuinfo->lowestTSC=0;
            lowestTSC=0;

            break;

          case 0xc0000080://efer
            //store the efer the guest wants it to be
            currentcpuinfo->efer=((vmregisters->rdx & 0xffffffff) << 32) + (currentcpuinfo->vmcb->RAX & 0xffffffff);

            //and set the actual efer  (make sure SVME is set)
            currentcpuinfo->vmcb->EFER=currentcpuinfo->efer | (1 << 12);

            sendstringf("Wants to set efer to %x\nActually set efer to %x\n",currentcpuinfo->efer, currentcpuinfo->vmcb->EFER);

            currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(1 << 5); //the efer got changed

            sendstringf("currentcpuinfo->vmcb->VMCB_CLEAN_BITS = %8\n", currentcpuinfo->vmcb->VMCB_CLEAN_BITS);



            break;

          case VM_IGGNE_MSR:
            nosendchar[getAPICID()]=0;
            sendstring("WEEEEEEEEEEEEEEEEEEEE\n");
            while (1);
            break;

          case VM_HSAVE_PA_MSR:
            //nosendchar[getAPICID()]=0;
            sendstringf("wrmsr(VM_HSAVE_PA,%6)\n", newvalue);
            currentcpuinfo->guest_VM_HSAVE_PA=newvalue;

            if (newvalue==0)
            {
              //sendvmstate(currentcpuinfo, vmregisters);
              //ShowCurrentInstructions(currentcpuinfo);
              //currentcpuinfo->showall=1;
            }


            nosendchar[getAPICID()]=1;
            break;

          default:
            nosendchar[getAPICID()]=0;
            sendstringf("Unhandled WRMSR(%8, %6)\n", vmregisters->rcx & 0xffffffff, newvalue);
            try
            {
              writeMSR(vmregisters->rcx, newvalue);
            }
            except
            {
              error=1;
              exceptionnr=lastexception & 0xff;
            }
            tryend

            if (error)
            {
              int er;
              sendstringf("Exception during MSR write (interrupt %d)\n",exceptionnr);
              sendstringf("Calling raiseGeneralProtectionFault\n");
              er=raiseGeneralProtectionFault(0);
              sendstringf("raiseGeneralProtectionFault returned %d", er);
              return er;
            }


            break;




        }
      }
      else
      {
        QWORD value;
        sendstringf("READ %x\n", vmregisters->rcx);
        sendstringf("vmregisters->rdx was %6\n", vmregisters->rdx);
        sendstringf("currentcpuinfo->vmcb->RAX was %6\n", currentcpuinfo->vmcb->RAX);

        switch (vmregisters->rcx & 0xffffffff)
        {
          case IA32_TIME_STAMP_COUNTER:
            return handle_rdtsc(currentcpuinfo, vmregisters); //will be responsible for adjust rip as well


          case 0xc0000080://efer
            //update LMA

            if ((currentcpuinfo->efer >> 12) & 1) //just give it the full EFER if it has enabled svmx as well
              currentcpuinfo->efer=currentcpuinfo->vmcb->EFER;
            else
            {
              currentcpuinfo->efer=currentcpuinfo->vmcb->EFER & ~(1<<12); //everything except this bit
            }

            value=currentcpuinfo->efer;
            break;

          case VM_HSAVE_PA_MSR:
            value=currentcpuinfo->guest_VM_HSAVE_PA;
            break;

          default:
            nosendchar[getAPICID()]=0;
            sendstringf("Unhandled RDMSR(%8)\n", msr);
            try
            {
              value=readMSR(msr);
              sendstringf("value=%6\n", value);
            }
            except
            {
              error=1;
              exceptionnr=lastexception & 0xff;
              sendstringf("error=%d\n", exceptionnr);
            }
            tryend

            if (error)
            {
              int er;
              sendstringf("Exception during MSR read (%8) (interrupt %d)\n",msr, exceptionnr);
              sendstringf("Calling raiseGeneralProtectionFault\n");
              er=raiseGeneralProtectionFault(0);
              sendstringf("raiseGeneralProtectionFault returned %d", er);
              return er;
            }

            break;
        }

        currentcpuinfo->vmcb->RAX=(DWORD)value;
        vmregisters->rdx=(DWORD)(value >> 32);

        sendstringf("vmregisters->rdx is %6\n", vmregisters->rdx);
        sendstringf("currentcpuinfo->vmcb->RAX is %6\n", currentcpuinfo->vmcb->RAX);

        //tell the cpu that ONLY the EFER and RIP got changed and nothing else




      }
      sendstringf("RIP=%6\n",currentcpuinfo->vmcb->RIP);
      sendstringf("nRIP=%6\n", currentcpuinfo->vmcb->nRIP);

      if (AMD_hasNRIPS)
      {
        currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;
      }
      else
      {
        //FFS (I don't think i'm going to support cpu's without this)
        int error;
        UINT64 pagefaultaddress;

        sendstringf("AMD_hasNRIPS == 0 so...\n");

       // sendstringf("DB:1:currentcpuinfo->AvailableVirtualAddress=%6\n", currentcpuinfo->AvailableVirtualAddress);

        unsigned char *bytes=(unsigned char *)mapVMmemory(currentcpuinfo, currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP, 15, &error, &pagefaultaddress);
        if (!bytes)
            bytes=mapVMmemory(currentcpuinfo, currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP, pagefaultaddress-currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP, &error, &pagefaultaddress);

        for (i=0; i<15; i++)
        {
          sendstringf("%x ", bytes[i]);
          if (bytes[i]==0x0f)
          {
            sendstringf("%x ", bytes[i+1]);
            sendstringf("%x ", bytes[i+2]);
            sendstringf("%x ", bytes[i+3]);
            currentcpuinfo->vmcb->RIP+=i+2;
            break;
          }
        }

        unmapVMmemory(bytes,15);

      }



      return 0;


      break;
    }


    case VMEXIT_CLGI:
    {
      nosendchar[getAPICID()]=1;
      sendstringf("%d %x:%6 VMEXIT_CLGI  (CR8=%d TPR=%d IF=%d)\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP, getCR8(), currentcpuinfo->vmcb->V_TPR, ((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->IF);

      currentcpuinfo->vmcb_GIF=0;

      if (AMD_hasNRIPS)
        currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;
      else
        currentcpuinfo->vmcb->RIP+=3;



#if GIF_EMUMETHOD == 1
      currentcpuinfo->vmcb->InterceptINTR=1; //method 1
      currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(CLEAN_I);
#elif GIF_EMUMETHOD == 2
      currentcpuinfo->vmcb->V_INTR_MASKING=1; //method 2
      currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(CLEAN_TPR);
      asm("cli"); //method 2
#else
  #error GIF_EMUMETHOD not set
#endif
      currentcpuinfo->vmcb->VMCB_CLEAN_BITS=0;
      currentcpuinfo->vmcb->TLB_CONTROL=1;

    //  sendstringf("%d %x:%6 VMEXIT_CLGI DONE  (CR8=%d TPR=%d IF=%d)\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP, getCR8(), currentcpuinfo->vmcb->V_TPR, ((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->IF);
      return 0;
    }

    case VMEXIT_STGI:
    {
      nosendchar[getAPICID()]=1;
      sendstringf("%d %x:%6 - VMEXIT_STGI (CR8=%d TPR=%d IF=%d)\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP,  getCR8(), currentcpuinfo->vmcb->V_TPR, ((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->IF);

      //sendvmstate(currentcpuinfo, vmregisters);
      //ShowCurrentInstructions(currentcpuinfo);

#if GIF_EMUMETHOD == 1
      currentcpuinfo->vmcb->InterceptINTR=0; //method 1
#elif GIF_EMUMETHOD == 2
      currentcpuinfo->vmcb->V_INTR_MASKING=0; //method 2
      asm("cli"); //method 2
#else
  #error GIF_EMUMETHOD not set
#endif
      currentcpuinfo->vmcb_GIF=1;

      if (AMD_hasNRIPS)
        currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;
      else
        currentcpuinfo->vmcb->RIP+=3;

      currentcpuinfo->vmcb->VMCB_CLEAN_BITS=0;
      currentcpuinfo->vmcb->TLB_CONTROL=1;

#if GIF_EMUMETHOD == 1
      int i;
      for (i=15; i>0; i--)
      {
        if (currentcpuinfo->vmcb_pending[i])
        {
          sendstringf("Pending interrupt in IRQ region %d (%x). Setting V_IRQ to 1\n",i, currentcpuinfo->vmcb_pending[i]);
          currentcpuinfo->vmcb->V_IRQ=1;
          currentcpuinfo->vmcb->V_INTR_PRIO=i;
          currentcpuinfo->vmcb->V_INTR_VECTOR=currentcpuinfo->vmcb_pending[i];

          currentcpuinfo->vmcb_pending[i]=0;

          currentcpuinfo->vmcb->InterceptVINTR=1;

          break;
        }
      }
#endif

      return 0;
    }

    case VMEXIT_VMLOAD:
    {
      QWORD PA=currentcpuinfo->vmcb->RAX;
      nosendchar[getAPICID()]=1;
      sendstringf("%d: %x:%6 - VMLOAD %6  IF=%d\n",currentcpuinfo->cpunr, currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP, PA, ((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->IF);

      sendstringf("\nbefore vmload:\n");
      sendvmstate(currentcpuinfo, vmregisters);


      pvmcb guestvmcb=(pvmcb)mapPhysicalMemory(PA,4096);
      currentcpuinfo->vmcb->fs_selector=guestvmcb->fs_selector;
      currentcpuinfo->vmcb->fs_attrib=guestvmcb->fs_attrib;
      currentcpuinfo->vmcb->fs_limit=guestvmcb->fs_limit;
      currentcpuinfo->vmcb->fs_base=guestvmcb->fs_base;
      currentcpuinfo->vmcb->gs_selector=guestvmcb->gs_selector;
      currentcpuinfo->vmcb->gs_attrib=guestvmcb->gs_attrib;
      currentcpuinfo->vmcb->gs_limit=guestvmcb->gs_limit;
      currentcpuinfo->vmcb->gs_base=guestvmcb->gs_base;
      currentcpuinfo->vmcb->tr_selector=guestvmcb->tr_selector;
      currentcpuinfo->vmcb->tr_attrib=guestvmcb->tr_attrib;
      currentcpuinfo->vmcb->tr_limit=guestvmcb->tr_limit;
      currentcpuinfo->vmcb->tr_base=guestvmcb->tr_base;
      currentcpuinfo->vmcb->ldtr_selector=guestvmcb->ldtr_selector;
      currentcpuinfo->vmcb->ldtr_attrib=guestvmcb->ldtr_attrib;
      currentcpuinfo->vmcb->ldtr_limit=guestvmcb->ldtr_limit;
      currentcpuinfo->vmcb->ldtr_base=guestvmcb->ldtr_base;
      currentcpuinfo->vmcb->KernelGsBase=guestvmcb->KernelGsBase;
      currentcpuinfo->vmcb->STAR=guestvmcb->STAR;
      currentcpuinfo->vmcb->LSTAR=guestvmcb->LSTAR;
      currentcpuinfo->vmcb->CSTAR=guestvmcb->CSTAR;
      currentcpuinfo->vmcb->SFMASK=guestvmcb->SFMASK;
      currentcpuinfo->vmcb->SYSENTER_CS=guestvmcb->SYSENTER_CS;
      currentcpuinfo->vmcb->SYSENTER_ESP=guestvmcb->SYSENTER_ESP;
      currentcpuinfo->vmcb->SYSENTER_EIP=guestvmcb->SYSENTER_EIP;

      if (AMD_hasNRIPS)
        currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;
      else
        currentcpuinfo->vmcb->RIP+=3;

      currentcpuinfo->vmcb->VMCB_CLEAN_BITS=0;
      currentcpuinfo->vmcb->TLB_CONTROL=1;


      unmapPhysicalMemory((void*)guestvmcb,4096);


      sendstringf("after vmload\n");
      sendvmstate(currentcpuinfo, vmregisters);
      //ShowCurrentInstructions(currentcpuinfo);


     // sendstringf("%d: %x:%6 - VMLOAD_DONE %6  IF=%d\n",currentcpuinfo->cpunr, currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP, PA, ((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->IF);

      return 0;
    }

    case VMEXIT_VMSAVE:
    {
      QWORD PA=currentcpuinfo->vmcb->RAX;
      nosendchar[getAPICID()]=1;
      sendstringf("%d: %x:%6 - VMSAVE %6  IF=%d\n",currentcpuinfo->cpunr, currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP, PA, ((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->IF);


      pvmcb guestvmcb=(pvmcb)mapPhysicalMemory(PA,4096);
      guestvmcb->fs_selector=currentcpuinfo->vmcb->fs_selector;
      guestvmcb->fs_attrib=currentcpuinfo->vmcb->fs_attrib;
      guestvmcb->fs_limit=currentcpuinfo->vmcb->fs_limit;
      guestvmcb->fs_base=currentcpuinfo->vmcb->fs_base;
      guestvmcb->gs_selector=currentcpuinfo->vmcb->gs_selector;
      guestvmcb->gs_attrib=currentcpuinfo->vmcb->gs_attrib;
      guestvmcb->gs_limit=currentcpuinfo->vmcb->gs_limit;
      guestvmcb->gs_base=currentcpuinfo->vmcb->gs_base;
      guestvmcb->tr_selector=currentcpuinfo->vmcb->tr_selector;
      guestvmcb->tr_attrib=currentcpuinfo->vmcb->tr_attrib;
      guestvmcb->tr_limit=currentcpuinfo->vmcb->tr_limit;
      guestvmcb->tr_base=currentcpuinfo->vmcb->tr_base;
      guestvmcb->ldtr_selector=currentcpuinfo->vmcb->ldtr_selector;
      guestvmcb->ldtr_attrib=currentcpuinfo->vmcb->ldtr_attrib;
      guestvmcb->ldtr_limit=currentcpuinfo->vmcb->ldtr_limit;
      guestvmcb->ldtr_base=currentcpuinfo->vmcb->ldtr_base;
      guestvmcb->KernelGsBase=currentcpuinfo->vmcb->KernelGsBase;
      guestvmcb->STAR=currentcpuinfo->vmcb->STAR;
      guestvmcb->LSTAR=currentcpuinfo->vmcb->LSTAR;
      guestvmcb->CSTAR=currentcpuinfo->vmcb->CSTAR;
      guestvmcb->SFMASK=currentcpuinfo->vmcb->SFMASK;
      guestvmcb->SYSENTER_CS=currentcpuinfo->vmcb->SYSENTER_CS;
      guestvmcb->SYSENTER_ESP=currentcpuinfo->vmcb->SYSENTER_ESP;
      guestvmcb->SYSENTER_EIP=currentcpuinfo->vmcb->SYSENTER_EIP;

      //guestvmcb->VMCB_CLEAN_BITS=0;
      unmapPhysicalMemory((void*)guestvmcb,4096);

      if (AMD_hasNRIPS)
        currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;
      else
        currentcpuinfo->vmcb->RIP+=3;


      //sendvmstate(currentcpuinfo, vmregisters);
      //ShowCurrentInstructions(currentcpuinfo);
      currentcpuinfo->vmcb->VMCB_CLEAN_BITS=0;
      currentcpuinfo->vmcb->TLB_CONTROL=1;

     // sendstringf("%d: %x:%6 - VMSAVE_DONE %6  IF=%d\n",currentcpuinfo->cpunr, currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP, PA, ((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->IF);

      return 0;
    }



    case VMEXIT_VMRUN:
    {
      //nosendchar[getAPICID()]=0;
      //sendstringf("%d: VMEXIT_VMRUN\n", currentcpuinfo->cpunr);

      return handleAMDVMRUNInstruction(currentcpuinfo, vmregisters, fxsave);
    }

    case VMEXIT_INVLPGA:
    {
      nosendchar[getAPICID()]=0;
      sendstringf("VMEXIT_INVLPGA (%6) for ASID %x\n", currentcpuinfo->vmcb->RAX, vmregisters->rcx);

      currentcpuinfo->vmcb->TLB_CONTROL=1;
      return 0;

    }

    case VMEXIT_VMMCALL:
    {
      //dbvm callback for amd
      nosendchar[getAPICID()]=0;
     // sendstringf("%d: handleVMCall()", currentcpuinfo->cpunr);
      return handleVMCall(currentcpuinfo, vmregisters);
      break;
    }

    case VMEXIT_CPUID:
    {
      nosendchar[getAPICID()]=0;
      sendstringf("!CPUID! %6->%6\n", currentcpuinfo->vmcb->RIP, currentcpuinfo->vmcb->nRIP);
      currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;

      while (1);
      return 0;
    }

    case VMEXIT_VINTR:
    {
      nosendchar[getAPICID()]=1;
      sendstringf("%d: VMEXIT_VINTR\n", currentcpuinfo->cpunr);

      sendstringf("EXITINFO1=%d\n", currentcpuinfo->vmcb->EXITINFO1);
      sendstringf("EXITINFO2=%d\n", currentcpuinfo->vmcb->EXITINFO2);
      sendstringf("EXITINTINFO=%x\n", currentcpuinfo->vmcb->EXITINTINFO);

      sendstringf("CR8=%d\n", getCR8());
      sendstringf("V_TPR=%d\n", currentcpuinfo->vmcb->V_TPR);
      sendstringf("V_IRQ=%d\n", currentcpuinfo->vmcb->V_IRQ);
      sendstringf("V_INTR_PRIO=%d\n", currentcpuinfo->vmcb->V_INTR_PRIO);
      sendstringf("V_INTR_VECTOR=%d (%x)\n", currentcpuinfo->vmcb->V_INTR_VECTOR, currentcpuinfo->vmcb->V_INTR_VECTOR);

      //sendvmstate(currentcpuinfo, vmregisters);
      //ShowCurrentInstructions(currentcpuinfo);



      //while (1);
      if (currentcpuinfo->vmcb_GIF==1)
      {
        sendstringf("GIF==1 so handling normally\n");


        currentcpuinfo->vmcb->EVENTINJ=0; //init
        currentcpuinfo->vmcb->inject_Vector=currentcpuinfo->vmcb->V_INTR_VECTOR;
        currentcpuinfo->vmcb->inject_Type=0;//external
        currentcpuinfo->vmcb->inject_Valid=1;


        currentcpuinfo->vmcb->V_IRQ=0;

        int i;
        for (i=15; i>0; i--)
        {
          if (currentcpuinfo->vmcb_pending[i])
          {

            sendstringf("Found a new pending interrupt. Marking it for next pickup");
            currentcpuinfo->vmcb->V_IRQ=1;
            currentcpuinfo->vmcb->V_INTR_PRIO=i;
            currentcpuinfo->vmcb->V_INTR_VECTOR=currentcpuinfo->vmcb_pending[i];

            currentcpuinfo->vmcb_pending[i]=0;

            currentcpuinfo->vmcb->INTERRUPT_SHADOW=1;
            break;
          }
        }


        //currentcpuinfo->vmcb->InterceptExceptions=0xFFFFBFFF;

        currentcpuinfo->vmcb->VMCB_CLEAN_BITS=0;
      }
      else
      {
        nosendchar[getAPICID()]=0;
        sendstringf("GIF==0\n"); //shouldn't happen
        while (1);
      }

      return 0;

    }

    case VMEXIT_HLT:
    {


      nosendchar[getAPICID()]=1; //with set to 0 eventually interrupts stop
      PRFLAGS f=(PRFLAGS)&(currentcpuinfo->vmcb->RFLAGS);
      sendstringf("HLT IF=%d TPR=%d  (%d) ", f->IF, currentcpuinfo->vmcb->V_TPR, getCR8() );
      currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;

      int i=-1;
      int counter=0;

      currentcpuinfo->vmcb->GuestASID++;
      if (currentcpuinfo->vmcb->GuestASID>65535)
        currentcpuinfo->vmcb->GuestASID=1;

      currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(CLEAN_ASID);

      while (i==-1)
      {
        i=getHighestPendingInterrupt();
        counter++;
        if (counter%10==0)
          sendstringf("\n...No interrupts after %d checks...",counter);

        if (counter % 1000==0)
        {
          i=-1;
          try
          {
            asm("stgi\nsti");
            asm("nop");
            asm("cli\nclgi");
          }
          except
          {
            i=lastexception&0xff;

          }
          tryend
          asm("cli\nclgi");

          if (i!=-1)
          {
            sendstringf(" and got an interrupt using stgi/sti: %x\n", i);
            currentcpuinfo->vmcb->inject_Type=0; //external
            currentcpuinfo->vmcb->inject_Vector=i;
            currentcpuinfo->vmcb->inject_Valid=1;
            currentcpuinfo->vmcb->inject_EV=0;
            nosendchar[getAPICID()]=0;
            return 0;
          }
          else
          {
            int j;
            sendstringf(" and still no interrupt\n");
            for (j=0; j<8; j++)
            {
              DWORD v=ReadAPICRegister(0x10+j);
              sendstringf("%d-%d=%8\n",j*32,(j+1)*32-1, v);
            }

            sendstringf("2:\n");
            for (j=0; j<8; j++)
            {
              DWORD v=ReadAPICRegister(0x18+j);
              sendstringf("%d-%d=%8\n",j*32,(j+1)*32-1, v);
            }


          }
        }
      }

      sendstringf("Handling %x\n", i);

      nosendchar[getAPICID()]=0;

      return 0;
    }

    case VMEXIT_INIT:
    {

      nosendchar[getAPICID()]=0;
      while (1) sendstringf("PENIS");
      /*
      if (currentcpuinfo->cpunr==0)
      {
        apic_eoi();

        nosendchar[getAPICID()]=0;
        sendstringf("%6: %d(%d): VMEXIT_INIT Skipping (%6 -> %6)\n", currentcpuinfo, currentcpuinfo->cpunr,getAPICID(), currentcpuinfo->vmcb->RIP, currentcpuinfo->vmcb->nRIP);


        if ((currentcpuinfo->vmcb->nRIP==0) || (currentcpuinfo->vmcb->nRIP==currentcpuinfo->vmcb->RIP))
        {
          currentcpuinfo->vmcb->RIP+=2;

        }
        else
          currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;

        currentcpuinfo->vmcb->VMCB_CLEAN_BITS=0;


        return 0;
      }*/

      UINT64 a,b,c,d;
      zeromemory(vmregisters,sizeof(VMRegisters));



      currentcpuinfo->vmcb->CR0=0x10;
      currentcpuinfo->vmcb->CR2=0;
      currentcpuinfo->vmcb->CR3=0;
      currentcpuinfo->vmcb->RFLAGS=2;
      currentcpuinfo->vmcb->EFER=0;
      currentcpuinfo->vmcb->RIP=0xfff0;


      Segment_Attribs attrib;
      attrib.G=0;
      attrib.D_B=0;
      attrib.L=0;
      attrib.P=1;
      attrib.DPL=0;

      //cs:
      attrib.S=1;
      attrib.Segment_type=0b1010;
      currentcpuinfo->vmcb->cs_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->cs_selector=0xf000;
      currentcpuinfo->vmcb->cs_base=0xffff0000;
      currentcpuinfo->vmcb->cs_limit=0xffff;

      //data
      attrib.S=1;
      attrib.Segment_type=0b0010;
      currentcpuinfo->vmcb->ss_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->ss_selector=0;
      currentcpuinfo->vmcb->ss_base=0;
      currentcpuinfo->vmcb->ss_limit=0xffff;

      currentcpuinfo->vmcb->ds_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->ds_selector=0;
      currentcpuinfo->vmcb->ds_base=0;
      currentcpuinfo->vmcb->ds_limit=0xffff;

      currentcpuinfo->vmcb->es_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->es_selector=0;
      currentcpuinfo->vmcb->es_base=0;
      currentcpuinfo->vmcb->es_limit=0xffff;

      currentcpuinfo->vmcb->fs_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->fs_selector=0;
      currentcpuinfo->vmcb->fs_base=0;
      currentcpuinfo->vmcb->fs_limit=0xffff;

      currentcpuinfo->vmcb->gs_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->gs_selector=0;
      currentcpuinfo->vmcb->gs_base=0;
      currentcpuinfo->vmcb->gs_limit=0xffff;

      //ldtr:
      attrib.S=0;
      attrib.Segment_type=0b0010;
      currentcpuinfo->vmcb->ldtr_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->ldtr_selector=0;
      currentcpuinfo->vmcb->ldtr_base=0;
      currentcpuinfo->vmcb->ldtr_limit=0xffff;

      //tr:
      attrib.S=0;
      attrib.Segment_type=0b0011;
      currentcpuinfo->vmcb->tr_attrib=attrib.SegmentAttrib;
      currentcpuinfo->vmcb->tr_selector=0;
      currentcpuinfo->vmcb->tr_base=0;
      currentcpuinfo->vmcb->tr_limit=0xffff;


      currentcpuinfo->vmcb->gdtr_base=0;
      currentcpuinfo->vmcb->gdtr_limit=0xffff;

      currentcpuinfo->vmcb->idtr_base=0;
      currentcpuinfo->vmcb->idtr_limit=0xffff;


      a=1;
      _cpuid(&a,&b,&c,&d);

      currentcpuinfo->vmcb->RAX=0;
      vmregisters->rdx=a;
      vmregisters->rbx=0;
      vmregisters->rcx=0;
      vmregisters->rbp=0;
      currentcpuinfo->vmcb->RSP=0;
      vmregisters->rdi=0;
      vmregisters->rsi=0;
      vmregisters->r8=0;
      vmregisters->r9=0;
      vmregisters->r10=0;
      vmregisters->r11=0;
      vmregisters->r12=0;
      vmregisters->r13=0;
      vmregisters->r14=0;
      vmregisters->r15=0;

      setDR0(0);
      setDR1(0);
      setDR2(0);
      setDR3(0);

      currentcpuinfo->vmcb->DR6=0xffff0ff0;
      currentcpuinfo->vmcb->DR7=0x400;

      currentcpuinfo->vmcb->VMCB_CLEAN_BITS=0;

      return 0;



      break;
    }


    case VMEXIT_SHUTDOWN: //shutdown
    {
      nosendchar[getAPICID()]=0;
      sendvmstate(currentcpuinfo, vmregisters);
      ShowCurrentInstructions(currentcpuinfo);
      displayline("FUUUUCK!\n");
      while(1);

      break;
    }

    case VMEXIT_INVALID:
    {
      nosendchar[getAPICID()]=0;
      sendstring("VMEXIT_INVALID\n");
      sendstringf("EFER=%x\n", currentcpuinfo->vmcb->EFER);

      sendvmstate(currentcpuinfo, vmregisters);

      break;
    }

    case VMEXIT_NPF:
    {
      nosendchar[getAPICID()]=0;
      //sendstringf("%d VMEXIT_NPF\n", currentcpuinfo->cpunr);


      return handleNestedPagingFault(currentcpuinfo,vmregisters, fxsave);
    }
  }

  nosendchar[getAPICID()]=0;
  sendstring("WEEE\n");

  displayline("%d: Unhandled event 0x%x (%x:%6: %6)  nrip=%6\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->EXITCODE, currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP, (QWORD)(currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP), currentcpuinfo->vmcb->nRIP);
  while (1) ;
  //still here
  return 1;
}


