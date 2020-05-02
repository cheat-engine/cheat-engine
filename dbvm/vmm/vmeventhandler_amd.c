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

criticalSection debugoutput;
int handleVMEvent_amd(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave)
{
  int i;



  nosendchar[getAPICID()]=1;

  sendstringf("getAPICID()=%d VM_HSAVE_PA_MSR=%6\n", getAPICID(),readMSR(VM_HSAVE_PA_MSR));


  sendstringf("currentcpuinfo->cpunr=%d (nr:%d)\n", currentcpuinfo->cpunr, currentcpuinfo->eventcounter[0]);



  sendstringf("currentcpuinfo->vmcb->EXITCODE=%x\n", currentcpuinfo->vmcb->EXITCODE);
  sendstringf("EXITINTINFO=%x\nEXITINFO1=%x\nEXITINFO2=%x\n", currentcpuinfo->vmcb->EXITINTINFO, currentcpuinfo->vmcb->EXITINFO1, currentcpuinfo->vmcb->EXITINFO2);

  sendstringf("currentcpuinfo->vmcb->VMCB_CLEAN_BITS = %8\n", currentcpuinfo->vmcb->VMCB_CLEAN_BITS);

  currentcpuinfo->vmcb->VMCB_CLEAN_BITS=0xffffffff; //nothing cached changed (yet)

  if (currentcpuinfo->eptUpdated==1)
  {
    currentcpuinfo->eptUpdated=0;
    currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(1 << 4);
  }

  if (currentcpuinfo->singleStepping.ReasonsPos)
  {
    nosendchar[getAPICID()]=0;
    sendstringf("AMD Handler: currentcpuinfo->singleStepping.ReasonsPos=%d Calling handleSingleStep()\n",currentcpuinfo->singleStepping.ReasonsPos);

    handleSingleStep(currentcpuinfo);
    sendstring("After handleSingleStep()\n");

    if (currentcpuinfo->vmcb->EXITCODE==VMEXIT_EXCP1)
    {
      sendstring("It was an int1 so skip this\n"); //Todo: Check if it was a int1 before the step


      //no further handling is needed
      return 0;
    }

    sendstring("Need to do some further handling\n");//eg external interrupts

  }


  currentcpuinfo->eventcounter[0]++;

  switch (currentcpuinfo->vmcb->EXITCODE)
  {

    case VMEXIT_EXCP1:
    {
      int isFault=0; //on amd it seems it ever ever set RF. isDebugFault(currentcpuinfo->vmcb->DR6, currentcpuinfo->vmcb->DR7);

      //int1 breakpoint

      nosendchar[getAPICID()]=0; //urrentcpuinfo->vmcb->CPL!=3;

      sendstringf("INT1 breakpoint\n");
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

      sendstringf("Int3 BP\n");
      sendstringf("EXITINTINFO=%x\nEXITINFO1=%x\nEXITINFO2=%x\n", currentcpuinfo->vmcb->EXITINTINFO, currentcpuinfo->vmcb->EXITINFO1, currentcpuinfo->vmcb->EXITINFO2);
      sendstringf("RIP=%6\n", currentcpuinfo->vmcb->RIP);
      sendstringf("nRIP=%6\n", currentcpuinfo->vmcb->nRIP);

      ShowCurrentInstructions(currentcpuinfo);

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

      nosendchar[getAPICID()]=0;

      sendstringf("INT14 breakpoint\n");
      sendstringf("RFLAGS=%x", currentcpuinfo->vmcb->RFLAGS);
      sendstringf("errorcode=%x\n", errorcode);
      sendstringf("cr2=%x\n", cr2);
      sendstringf("EXITINTINFO=%x\n", currentcpuinfo->vmcb->EXITINTINFO);


      setCR2(cr2); //<---bochs bug? My real hardware does not use this CR2 value but the one in the vmcb. (no matter)
      currentcpuinfo->vmcb->CR2=cr2;
      currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(1 << 9); //cr2 got changed


      ((PRFLAGS)(&currentcpuinfo->vmcb->RFLAGS))->RF=1;



      if ((int14redirection_idtbypass==0) || (ISREALMODE(currentcpuinfo)))
      {
        sendstring("Realmode bp? or No idtbypass\n");

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

      nosendchar[getAPICID()]=1; //this seems to work well enough
      sendstringf("Software interrupt\n");

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

    case VMEXIT_MSR:
    {

      sendstring("VMEXIT_MSR\n");
      sendstringf("EXITINFO1=%d\n", currentcpuinfo->vmcb->EXITINFO1);
      sendstringf("EXITINFO2=%d\n", currentcpuinfo->vmcb->EXITINFO2);

      if (currentcpuinfo->vmcb->EXITINFO1)
      {
        sendstringf("WRITE %x\n", vmregisters->rcx);

        switch (vmregisters->rcx & 0xffffffff)
        {
          case 0xc0000080://efer
            //store the efer the guest wants it to be
            currentcpuinfo->efer=((vmregisters->rdx & 0xffffffff) << 32) + (currentcpuinfo->vmcb->RAX & 0xffffffff);

            //and set the actual efer  (make sure SVME is set)
            currentcpuinfo->vmcb->EFER=currentcpuinfo->efer | (1 << 12);

            sendstringf("Wants to set efer to %x\nActually set efer to %x\n",currentcpuinfo->efer, currentcpuinfo->vmcb->EFER);

            currentcpuinfo->vmcb->VMCB_CLEAN_BITS&=~(1 << 5); //the efer got changed

            sendstringf("currentcpuinfo->vmcb->VMCB_CLEAN_BITS = %8\n", currentcpuinfo->vmcb->VMCB_CLEAN_BITS);



            break;

          case 0xc0010117:
            currentcpuinfo->guest_VM_HSAVE_PA=((vmregisters->rdx & 0xffffffff) << 32) + (currentcpuinfo->vmcb->RAX & 0xffffffff);
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

          case 0xc0010117:
            value=currentcpuinfo->guest_VM_HSAVE_PA;
            break;
        }

        currentcpuinfo->vmcb->RAX=(DWORD)value;
        vmregisters->rdx=(DWORD)(currentcpuinfo->efer >> 32);

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


    case VMEXIT_VMRUN:
    {

      sendstring("VMEXIT_VMRUN\n");
      raiseInvalidOpcodeException(currentcpuinfo);
      return 0;

      //alternatively, handle the vmrun (everything changed)
      //execute vmexit in the host, and when it exits pass on everything to the guest.
      //if needed, add an extra intercept
      break;
    }

    case VMEXIT_VMMCALL:
    {
      //dbvm callback for amd
      nosendchar[getAPICID()]=0;
      sendstringf("%d: handleVMCall()", currentcpuinfo->cpunr);
      return handleVMCall(currentcpuinfo, vmregisters);
      break;
    }

    case VMEXIT_CPUID:
    {
      sendstringf("!CPUID! %6->%6\n", currentcpuinfo->vmcb->RIP, currentcpuinfo->vmcb->nRIP);
      currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;
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
      sendstring("VMEXIT_NPF\n");


      return handleNestedPagingFault(currentcpuinfo,vmregisters, fxsave);

      break;
    }


  }

  nosendchar[getAPICID()]=0;

  displayline("%d: Unhandled event 0x%x (%x:%6: %6)  nrip=%6\n", currentcpuinfo->cpunr, currentcpuinfo->vmcb->EXITCODE, currentcpuinfo->vmcb->cs_selector, currentcpuinfo->vmcb->RIP, (QWORD)(currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP), currentcpuinfo->vmcb->nRIP);
  while (1) ;
  //still here
  return 1;
}


