/*
 * vmeventhandler_amd.c
 *
 *  Created on: May 22, 2013
 *      Author: eric
 */

#include "vmeventhandler_amd.h"
#include "main.h"
#include "common.h"
#include "vmpaging.h"
#include "keyboard.h"
#include "neward.h"

int handleVMEvent_amd(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{



  nosendchar[getAPICID()]=1;
  sendstringf("currentcpuinfo->vmcb->EXITCODE=%d\n", currentcpuinfo->vmcb->EXITCODE);
  sendstringf("EXITINTINFO=%x\nEXITINFO1=%x\nEXITINFO2=%x\n", currentcpuinfo->vmcb->EXITINTINFO, currentcpuinfo->vmcb->EXITINFO1, currentcpuinfo->vmcb->EXITINFO2);


  switch (currentcpuinfo->vmcb->EXITCODE)
  {


    case 0x75: //software interrupts (INTn)
    {
      int handled=0;
      int intnr;
      int instructionlength=0;
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
        unsigned char *bytes=(unsigned char *)mapVMmemory(currentcpuinfo, currentcpuinfo->vmcb->cs_base+currentcpuinfo->vmcb->RIP, 15, currentcpuinfo->AvailableVirtualAddress, &error, &pagefaultaddress);

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
      }


      if ((ISREALMODE(currentcpuinfo)/* || ((currentcpuinfo->vmcb->RFLAGS >> 17) & 1) */) && (intnr==0x15))
      {

        nosendchar[getAPICID()]=0;
        //todo: Split this up into a function used by both intel and amd. Right now it's basically just a copy/paste with minor changes

        //realmode of Virtual 8086 mode and the interrupt matches
        sendstringf("INT 0x15\n");
        sendstringf("RAX=%6\n", currentcpuinfo->vmcb->RAX);

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
          int i;

          DWORD between1and16MB=0; //in KB, max 3c00  (between 0x100000 and 0x1000000)
          DWORD above16MB=0;




          nosendchar[getAPICID()]=0;
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
        nosendchar[getAPICID()]=0;

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

          vmregisters->rcx=(vmregisters->rcx & 0xffffffff00000000) + (o*20);
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
        sendstringf("INT 0x%x .  Not INT 0x15 or not in realmode so handle it normally (inject this interrupt)\n", intnr);
      }

      //adjust RIP
      if (AMD_hasNRIPS)
        currentcpuinfo->vmcb->RIP=currentcpuinfo->vmcb->nRIP;
      else
        currentcpuinfo->vmcb->RIP+=instructionlength;

      if (!handled)
      {
        {
          sendstringf("Injecting interrupt\n");
          currentcpuinfo->vmcb->inject_Type=4; //software int
          currentcpuinfo->vmcb->inject_Vector=intnr;
          currentcpuinfo->vmcb->inject_Valid=1;
          currentcpuinfo->vmcb->inject_EV=0;
        }
      }

      return 0;
    }


    case 0x7F: //shutdown
    {
      displayline("FUUUUCK!\n");
      while(1);

      break;
    }


  }


  displayline("Unhandled event\n");
  while (1) ;
  //still here
  return 1;
}


