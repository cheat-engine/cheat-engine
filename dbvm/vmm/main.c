/* vmm.c: This is the virtual machine
 * It will be loaded at virtual address 0x00400000 (vmma.asm that is which just jumps short to intiialize paging)
 * On initialisation 0 to 4MB is identity mapped, to the stored memory regions are available to mess with
 */


#include "common.h"
#include "main.h"
#include "mm.h"
#include "neward.h"
#include "apic.h"

#include "multicore.h"
#include "inthandlers.h"
#include "vmmhelper.h"
#include "vmeventhandler.h"

#include "distorm.h"
#include "keyboard.h"

#include "pci.h"
#include "offloados.h"
#include "msrnames.h"
#include "vmxcontrolstructures.h"

#include "test.h"
#include "vmcall.h"
#include "vmpaging.h"
//#include "psod.h" //for pink screen of death support

/*
//#include "logo.c"
*/

#define APIC_ID_OFFSET 0x020
#define APIC_SVR_OFFSET 0x0f0



void menu(void);
void menu2(void);



PINT_VECTOR intvector=NULL;

unsigned char *ffpage=NULL;
PPTE_PAE   ffpagetable=NULL;
PPDE_PAE   ffpagedir=NULL;


PTSS testTSS=NULL;

int cpu_stepping;
int cpu_model;
int cpu_familyID;
int cpu_type;
int cpu_ext_modelID;
int cpu_ext_familyID;



unsigned long long IA32_APIC_BASE=0xfee00000;
unsigned long long APIC_ID=0xfee00020;
unsigned long long APIC_SVR=0xfee000f0;

unsigned int BOOT_ID=0xffffffff;


criticalSection CScpu;


extern unsigned int isAP;


#define SETINT(INTNR) intvector[INTNR].wLowOffset=(WORD)(UINT64)inthandler##INTNR; \
                      intvector[INTNR].wHighOffset=(WORD)((UINT64)inthandler##INTNR >> 16);


#ifdef DEBUG
int autostart=0; //since simnow amd emu doesn't support serial import...
#else
int autostart=1;
#endif //debug

//int isrunning=0;


int cinthandler(unsigned long long *stack, int intnr)
{
  PRFLAGS rflags;
  int errorcode=0;
  UINT64 errorcodeValue;
  int i;
  DWORD thisAPICID;
  int cpunr=0;

  UINT64 originalDR7=getDR7();

  if (intnr==1)
  {
    //disable breakpoints
    setDR7(0ULL);
  }

  thisAPICID=getAPICID();

  sendstringCS.lockcount=0;
  sendstringCS.locked=0;
  sendstringfCS.lockcount=0;
  sendstringfCS.locked=0;

  if (cpuinfo[cpunr].OnInterrupt.RIP==0)
  {
	  //unexpected exception
	  enableserial();
	  nosendchar[thisAPICID]=0; //override a block if there was one, this is important:
  }


 // sendstringf("interrupt fired : %d (%x)\n\r", intnr,intnr);

  //lookup the cpucore
  for (i=0; (unsigned)i<cpucount; i++)
    if (cpuinfo[i].apicid==thisAPICID)
    {
      cpunr=i;
      break;
    }

  sendstringf("cpunr=%d\n\r",cpunr);
  sendstringf("intnr=%d\n\r",intnr);
  sendstringf("rsp=%x\n\r",getRSP());
  sendstringf("cr2=%6\n\r",getCR2());

  if ((stack[17]==80) && (stack[18]==80))
  {
    //not sure...
    if ((stack[16]>=0x00400000) && (stack[16]<0x00800000))
    {
      //in the region of the code of the vmm, so I guess it's no errorcode (and cs and eflags=80)
      errorcode=0;
    }
    else
      errorcode=1;
  }
  else
  if (stack[18]==80)
    errorcode=1;
  else
  if (stack[17]==80)
    errorcode=0;

  if (errorcode)
  {

    errorcodeValue=stack[16];
    sendstringf("Interrupt has errorcode : %x (",errorcodeValue);
    if (errorcodeValue & 1)
    {
      sendstring("EXT ");
    }

    if (errorcodeValue & 2)
    {
      sendstring("IDT ");
    }

    if (errorcodeValue & 4)
    {
      sendstring("TI ");
    }

    sendstringf("%x ",errorcodeValue & 0xFFFFFFF8);

    sendstringf(")\n\r");
  }
  else
  {
    //sendstringf("Interrupt has no errorcode\n\r");
  }

  rflags=(PRFLAGS)&stack[16+2+errorcode];

  sendstringf("Checking if it was an expected interrupt\n\r");

  if (cpuinfo[cpunr].OnInterrupt.RIP)
  {
    sendstringf("Yes, OnInterrupt is set to %x\n\r",cpuinfo[cpunr].OnInterrupt);
    stack[16+errorcode]=(QWORD)cpuinfo[cpunr].OnInterrupt.RIP;
    stack[19+errorcode]=(QWORD)cpuinfo[cpunr].OnInterrupt.RSP;

    rflags->IF=0; //disable the IF flag in the eflags register stored on the stack (when called during checks if an int was pending)


    cpuinfo[cpunr].LastInterrupt=(unsigned char)intnr;
    if (errorcode)
    {
      cpuinfo[cpunr].LastInterruptHasErrorcode=1;
      cpuinfo[cpunr].LastInterruptErrorcode=(WORD)stack[16];
    }
    cpuinfo[cpunr].LastInterruptHasErrorcode=0;

    cpuinfo[cpunr].OnInterrupt.RIP=0; //clear exception handler
    cpuinfo[cpunr].OnInterrupt.RSP=0;

    sendstring("changed eip, and returning now\n\r");


    return errorcode;
  }
  sendstring("not expected\n\r");



  sendstring("Status:\n\r");
  sendstringf("r15=%6\n\r",stack[0]);
  sendstringf("r14=%6\n\r",stack[1]);
  sendstringf("r13=%6\n\r",stack[2]);
  sendstringf("r12=%6\n\r",stack[3]);
  sendstringf("r11=%6\n\r",stack[4]);
  sendstringf("r10=%6\n\r",stack[5]);
  sendstringf("r9=%6\n\r",stack[6]);
  sendstringf("r8=%6\n\r",stack[7]);
  sendstringf("rbp=%6\n\r",stack[8]);
  sendstringf("rsi=%6\n\r",stack[9]);
  sendstringf("rdi=%6\n\r",stack[10]);
  sendstringf("rdx=%6\n\r",stack[11]);
  sendstringf("rcx=%6\n\r",stack[12]);
  sendstringf("rbx=%6\n\r",stack[13]);
  sendstringf("rax=%6\n\r",stack[14]);
  sendstringf("intnr=%6\n\r",stack[15]);
  sendstringf("stack[16]=%6\n\r",stack[16]);
  sendstringf("stack[17]=%6\n\r",stack[17]);
  sendstringf("stack[18]=%6\n\r",stack[18]);
  sendstringf("stack[19]=%6\n\r",stack[19]);
  sendstringf("--------------\n\r");

  sendstringf("DR6=%6\n\r",getDR6());
  if (intnr==14)
  {
    sendstringf("DR2=%6\n\r",getDR2());
  }
  //16=errorcode/eip
  //17=eip/cs
  //18=cs/eflags

	sendstringf("eip=%6\n\r",stack[16+errorcode]);
	sendstringf("cs=%6\n\r",stack[17+errorcode]);




  sendDissectedFlags(rflags);

  sendstringf("Trying to disassemble caller instruction\n\r");

  int found=0;
  unsigned int used=0;
  unsigned int start=0;
  _DecodedInst disassembled[22];

  while (start<30)
  {
    distorm_decode(stack[16+errorcode]-30+start, (unsigned char *)(stack[16+errorcode]-30+start), 120, Decode64Bits, disassembled, 22, &used);

    for (i=0; (unsigned)i<used; i++)
      if (disassembled[i].offset==stack[16+errorcode])
      {
        found=1;
        break;
      }

    if (found)
      break;
    start++;
  }

  for (i=0; (unsigned)i<used; i++)
  {
    if (disassembled[i].offset==stack[16+errorcode])
    {
      sendstringf(">>");
    }

    sendstringf("%x : %s - %s %s\n\r",
                    disassembled[i].offset,
                    disassembled[i].instructionHex.p,
                    disassembled[i].mnemonic.p,
                    disassembled[i].operands.p);
  }



  autostart=0;
  setDR7(originalDR7);


  sendstring("End of interrupt\n\r");


#ifdef DEBUGINTHANDLER
  char key;
  while (1)
  {
    sendstring("----------------------------\n\r");
    sendstring("Interrupt handler debug menu\n\r");
    sendstring("----------------------------\n\r");
    sendstring("1: Exit from interrupt\n\r");
    sendstring("2: Check CRC values\n\r");
    sendstring("3: Get vmstate\n\r");
    sendstring("p: Previous vmstates\n\r");

    key=waitforchar();
    switch (key)
    {
      case '1':
        sendstring("Exiting from interrupt\n\r");
        return errorcode;

      case '2':
        CheckCRCValues();
        break;

      case '3':
        sendvmstate(&cpuinfo[cpunr], NULL);
        break;

      case 'p':
    	displayPreviousStates();
        break;
    }
  }

#endif

	return errorcode;
}


void CheckCRCValues(void)
{
  unsigned int newcrc;
  sendstringf("Original IDT crc = %x\n\r",originalIDTcrc);

  newcrc=generateCRC(NULL,0x400);
  sendstringf("Current  IDT crc = %x\n\r",newcrc);

  if (originalIDTcrc!=newcrc)
  {
    sendstring("!!!!!!!!!!MISMATCH!!!!!!!!!!\n\r");
  }

  sendstringf("Original VMM crc = %x\n\r",originalVMMcrc);
  newcrc=generateCRC((void *)vmxloop,0x2a000);
  sendstringf("Current  VMM crc = %x\n\r",newcrc);
  if (originalVMMcrc!=newcrc)
  {
    sendstring("!!!!!!!!!!MISMATCH!!!!!!!!!!\n\r");
  }


  return;
}

void setints(void)
{
  IDT tidt;
  int i;

  tidt.wLimit=16*256;
  tidt.vector=intvector;

  if (intvector==NULL)
  {
    sendstring("setints was called too early");
    while(1);
  }


  for (i=0; i<256; i++)
  {
    intvector[i].wSelector=80;
    intvector[i].bUnused=0;
    intvector[i].bAccess=0x8e; //10001110
  }

  SETINT(0);
  SETINT(1);
  SETINT(2);
  SETINT(3);
  SETINT(4);
  SETINT(5);
  SETINT(6);
  SETINT(7);
  SETINT(8);
  SETINT(9);
  SETINT(10);
  SETINT(11);
  SETINT(12);
  SETINT(13);
  SETINT(14);
  SETINT(15);
  SETINT(16);
  SETINT(17);
  SETINT(18);
  SETINT(19);
  SETINT(20);
  SETINT(21);
  SETINT(22);
  SETINT(23);
  SETINT(24);
  SETINT(25);
  SETINT(26);
  SETINT(27);
  SETINT(28);
  SETINT(29);
  SETINT(30);
  SETINT(31);
  SETINT(32);
  SETINT(33);
  SETINT(34);
  SETINT(35);
  SETINT(36);
  SETINT(37);
  SETINT(38);
  SETINT(39);
  SETINT(40);
  SETINT(41);
  SETINT(42);
  SETINT(43);
  SETINT(44);
  SETINT(45);
  SETINT(46);
  SETINT(47);
  SETINT(48);
  SETINT(49);
  SETINT(50);
  SETINT(51);
  SETINT(52);
  SETINT(53);
  SETINT(54);
  SETINT(55);
  SETINT(56);
  SETINT(57);
  SETINT(58);
  SETINT(59);
  SETINT(60);
  SETINT(61);
  SETINT(62);
  SETINT(63);
  SETINT(64);
  SETINT(65);
  SETINT(66);
  SETINT(67);
  SETINT(68);
  SETINT(69);
  SETINT(70);
  SETINT(71);
  SETINT(72);
  SETINT(73);
  SETINT(74);
  SETINT(75);
  SETINT(76);
  SETINT(77);
  SETINT(78);
  SETINT(79);
  SETINT(80);
  SETINT(81);
  SETINT(82);
  SETINT(83);
  SETINT(84);
  SETINT(85);
  SETINT(86);
  SETINT(87);
  SETINT(88);
  SETINT(89);
  SETINT(90);
  SETINT(91);
  SETINT(92);
  SETINT(93);
  SETINT(94);
  SETINT(95);
  SETINT(96);
  SETINT(97);
  SETINT(98);
  SETINT(99);
  SETINT(100);
  SETINT(101);
  SETINT(102);
  SETINT(103);
  SETINT(104);
  SETINT(105);
  SETINT(106);
  SETINT(107);
  SETINT(108);
  SETINT(109);
  SETINT(110);
  SETINT(111);
  SETINT(112);
  SETINT(113);
  SETINT(114);
  SETINT(115);
  SETINT(116);
  SETINT(117);
  SETINT(118);
  SETINT(119);
  SETINT(120);
  SETINT(121);
  SETINT(122);
  SETINT(123);
  SETINT(124);
  SETINT(125);
  SETINT(126);
  SETINT(127);
  SETINT(128);
  SETINT(129);
  SETINT(130);
  SETINT(131);
  SETINT(132);
  SETINT(133);
  SETINT(134);
  SETINT(135);
  SETINT(136);
  SETINT(137);
  SETINT(138);
  SETINT(139);
  SETINT(140);
  SETINT(141);
  SETINT(142);
  SETINT(143);
  SETINT(144);
  SETINT(145);
  SETINT(146);
  SETINT(147);
  SETINT(148);
  SETINT(149);
  SETINT(150);
  SETINT(151);
  SETINT(152);
  SETINT(153);
  SETINT(154);
  SETINT(155);
  SETINT(156);
  SETINT(157);
  SETINT(158);
  SETINT(159);
  SETINT(160);
  SETINT(161);
  SETINT(162);
  SETINT(163);
  SETINT(164);
  SETINT(165);
  SETINT(166);
  SETINT(167);
  SETINT(168);
  SETINT(169);
  SETINT(170);
  SETINT(171);
  SETINT(172);
  SETINT(173);
  SETINT(174);
  SETINT(175);
  SETINT(176);
  SETINT(177);
  SETINT(178);
  SETINT(179);
  SETINT(180);
  SETINT(181);
  SETINT(182);
  SETINT(183);
  SETINT(184);
  SETINT(185);
  SETINT(186);
  SETINT(187);
  SETINT(188);
  SETINT(189);
  SETINT(190);
  SETINT(191);
  SETINT(192);
  SETINT(193);
  SETINT(194);
  SETINT(195);
  SETINT(196);
  SETINT(197);
  SETINT(198);
  SETINT(199);
  SETINT(200);
  SETINT(201);
  SETINT(202);
  SETINT(203);
  SETINT(204);
  SETINT(205);
  SETINT(206);
  SETINT(207);
  SETINT(208);
  SETINT(209);
  SETINT(210);
  SETINT(211);
  SETINT(212);
  SETINT(213);
  SETINT(214);
  SETINT(215);
  SETINT(216);
  SETINT(217);
  SETINT(218);
  SETINT(219);
  SETINT(220);
  SETINT(221);
  SETINT(222);
  SETINT(223);
  SETINT(224);
  SETINT(225);
  SETINT(226);
  SETINT(227);
  SETINT(228);
  SETINT(229);
  SETINT(230);
  SETINT(231);
  SETINT(232);
  SETINT(233);
  SETINT(234);
  SETINT(235);
  SETINT(236);
  SETINT(237);
  SETINT(238);
  SETINT(239);
  SETINT(240);
  SETINT(241);
  SETINT(242);
  SETINT(243);
  SETINT(244);
  SETINT(245);
  SETINT(246);
  SETINT(247);
  SETINT(248);
  SETINT(249);
  SETINT(250);
  SETINT(251);
  SETINT(252);
  SETINT(253);
  SETINT(254);
  SETINT(255);

  cLIDT(&tidt);
}


void vmm_entry2_hlt(pcpuinfo currentcpuinfo)
{
  UINT64 a,b,c,d;
  sendstringf("CPU %d : Terminating...\n\r",currentcpuinfo->cpunr);
  while (1)
  {
    currentcpuinfo->active=0;
    a=1;
    _cpuid(&a,&b,&c,&d);  //always serialize after writing to a structure read by a different cpu
    __asm("hlt\n\r");
  }
}


void vmm_entry2(void)
//Entry for application processors
{
  static int initializedCPUCount=1; //assume the first (main) cpu managed to start up

  unsigned int cpunr;


  //sendstringf("Welcome to a extra cpu (cpunr=%d)\n",cpucount);

  csEnter(&CScpu);

  cpunr=initializedCPUCount;
  sendstringf("Setting up cpunr=%d\n",cpunr);


  initializedCPUCount++;

  if (!loadedOS)
    cpucount++; //cpucount is known, don't increase it

  cpuinfo[cpunr].active=1;
  cpuinfo[cpunr].cpunr=cpunr;
  cpuinfo[cpunr].apicid=getAPICID();

  cpuinfo[cpunr].TSbase=0;
  cpuinfo[cpunr].TSlimit=0xffff;
  cpuinfo[cpunr].TSsegment=0;
  cpuinfo[cpunr].TSaccessRights=0x8b;
  cpuinfo[cpunr].virtualTLB_guest_PDPTR_lookup=malloc(4096);
  cpuinfo[cpunr].virtualTLB_guest_PD_lookup=malloc(8*4096); //4 should be enough, but this gives me 8GB (can be changed in the future)

  displayline("New CPU CORE. CPUNR=%d APICID=%d (cpuinfo struct at : %p rsp=%x)\n",cpuinfo[cpunr].cpunr, cpuinfo[cpunr].apicid, &cpuinfo[cpunr], getRSP());

  sendstringf("configured a cpuinfo structure (%d (%8) )\n\r",cpuinfo[cpunr].cpunr, (unsigned long long)&cpuinfo[cpunr] );


  csLeave(&CScpu);

  if (!loadedOS)
  {
    while (cpuinfo[cpunr].command==0)
    {
      resync();
      if (cpuinfo[cpunr].hastoterminate==1)
        vmm_entry2_hlt(&cpuinfo[cpunr]);
    }


    if (cpuinfo[cpunr].hastoterminate==1)
      vmm_entry2_hlt(&cpuinfo[cpunr]);
  }


  csEnter(&CScpu);
  if (!loadedOS)
    sendstringf("%d : Starting VMX (AP cpu so entering wait for sipi vmx mode)\n\r",cpunr);
  displayline("CPU CORE %d: entering VMX mode\n",cpunr);
  csLeave(&CScpu);

  setints();

  startvmx(&(cpuinfo[cpunr]));





  sendstringf("Application cpu returned from startvmx\n\r");

  vmm_entry2_hlt(&cpuinfo[cpunr]);
  while (1);
}





void vmm_entry(void)
{
  int i,k;
  UINT64 a,b,c,d;



  //stack has been properly setup, so lets allow other cpu's to launch as well
  if (!loadedOS)
    *(volatile DWORD *)0x7c00=0; //unlock

  memorycloak=0;
  Password1=0x76543210; //later to be filled in by user, sector on disk, or at compile time
  Password2=0xfedcba98;
  dbvmversion=9; //version 1 was the 32-bit only version, 2 added 32-bit, 3 had a revised int1 redirect option, 4 has major bugfixes, 5=more fixes and some basic device recog, 6=Even more compatibility fixes, rm emu, and new vmcalls, 7=driver loading , 8=amd support, 9 memory usage decrease and some fixes for newer systems
  int1redirection=1; //redirect to int vector 1 , ooh, what a weird redirection....
  int3redirection=3;
  int14redirection=14;

  sendstringf("APICID=%8\n\rrsp=%6\n\rnextstack=%6\n\r",getAPICID(), getRSP(), (UINT64)nextstack);



  if (isAP)
  {
    vmm_entry2();
    sendstringf("vmm_entry2 has PHAILED!!!!");
    while (1);
  }

  isAP=1; //all other entries will be an AP
  //enableserial();



  sendstringf("If you see this that means that the transition from unpaged to paged was a success\n\r");
  sendstringf("loadedOS=%6\n",loadedOS);

  currentdisplayline=7;


  displayline("BOOT CPU CORE initializing\n");

  displayline("CR3=%6\n", getCR3());
  displayline("pagedirptrvirtual=%6\n",(UINT64)pagedirptrvirtual);
  displayline("&pagedirptrvirtual=%6\n",(UINT64)&pagedirptrvirtual);
  displayline("vmmstart=%6 (this is virtual address 00400000)\n",(UINT64)vmmstart);

  //displayline("press any key to continue\n");


  //initialize

  sendstring("Welcome to Dark Byte\'s Virtual Machine Manager\n\r");

  //since the setup sets pagedirptrvirtual as the cr3 virtual address, adjust it
  pagedirlvl4=pagedirptrvirtual;
  sendstringf("pagedirlvl4=%6\n\r",(unsigned long long)pagedirlvl4);

  pagedirptrvirtual=(PPDPTE_PAE)((unsigned long long)pagedirlvl4+0x1000);


  pagedirvirtual=(PPDE2MB_PAE)((unsigned long long)pagedirptrvirtual+0x1000);  //000000000 to 03fffffff
  pagedirvirtual2=(PPDE2MB_PAE)((unsigned long long)pagedirptrvirtual+0x2000); //040000000 to 07fffffff
  pagedirvirtual3=(PPDE2MB_PAE)((unsigned long long)pagedirptrvirtual+0x3000); //080000000 to 0bfffffff
  pagedirvirtual4=(PPDE2MB_PAE)((unsigned long long)pagedirptrvirtual+0x4000); //0c0000000 to 0ffffffff
  pagedirvirtual5=(PPDE2MB_PAE)((unsigned long long)pagedirptrvirtual+0x5000); //100000000 to 13fffffff


  sendstringf("pagedirptrvirtual=%6 (%6)\n",(UINT64)pagedirptrvirtual, VirtualToPhysical((UINT64)pagedirptrvirtual));
  sendstringf("pagedirvirtual=%6 (%6)\n",(UINT64)pagedirvirtual, VirtualToPhysical((UINT64)pagedirvirtual));
  sendstringf("pagedirvirtual2=%6 (%6)\n",(UINT64)pagedirvirtual2, VirtualToPhysical((UINT64)pagedirvirtual2));
  sendstringf("pagedirvirtual3=%6 (%6)\n",(UINT64)pagedirvirtual3, VirtualToPhysical((UINT64)pagedirvirtual3));
  sendstringf("pagedirvirtual4=%6 (%6)\n",(UINT64)pagedirvirtual4, VirtualToPhysical((UINT64)pagedirvirtual4));
  sendstringf("pagedirvirtual5=%6 (%6)\n",(UINT64)pagedirvirtual5, VirtualToPhysical((UINT64)pagedirvirtual5));



  zeromemory(pagedirvirtual2,4096);
  zeromemory(pagedirvirtual3,4096);
  zeromemory(pagedirvirtual4,4096);
  zeromemory(pagedirvirtual5,4096);
  sendstring("Zeroed directory ptr tables 2, 3 and 4\n\r");


  *(UINT64 *)(&pagedirptrvirtual[1])=VirtualToPhysical((UINT64)pagedirvirtual2);
  *(UINT64 *)(&pagedirptrvirtual[2])=VirtualToPhysical((UINT64)pagedirvirtual3);
  *(UINT64 *)(&pagedirptrvirtual[3])=VirtualToPhysical((UINT64)pagedirvirtual4);
  *(UINT64 *)(&pagedirptrvirtual[4])=VirtualToPhysical((UINT64)pagedirvirtual5);



  for (i=1;i<5; i++)
  {
    pagedirptrvirtual[i].P=1;
    pagedirptrvirtual[i].RW=1;
    pagedirptrvirtual[i].US=1;

//    pagedirptrvirtual[i].PFN=pagedirptrvirtual[i-1].PFN+1;
  }

  sendstring("resetting paging:\n");
  setCR3(getCR3());
  sendstring("Still alive\n");

  {
    UINT64 blaaa;
    for (blaaa=0; blaaa<0x00800000; blaaa+=4096)
      _invlpg(blaaa);
  }





  IA32_APIC_BASE=(unsigned long long)readMSR(0x1b);


  displayline("IA32_APIC_BASE=%6\n\r",IA32_APIC_BASE);
  sendstringf("IA32_APIC_BASE=%6\n\r",IA32_APIC_BASE);
  sendstringf("\tLocal APIC base=%6\n\r",IA32_APIC_BASE & 0xfffff000);
  sendstringf("\tAPIC global enable/disable=%d\n\r",(IA32_APIC_BASE >> 11) & 1);
  sendstringf("\tBSP=%d\n\r",(IA32_APIC_BASE >> 8) & 1);

  IA32_APIC_BASE=IA32_APIC_BASE & 0xfffffffffffff000ULL;
  APIC_ID=IA32_APIC_BASE+APIC_ID_OFFSET;
  APIC_SVR=IA32_APIC_BASE+APIC_SVR_OFFSET;


  //identity map the apic memory region

  sendstringf("PML4 ptr = %d \n\r", IA32_APIC_BASE >> 39);
  sendstringf("Directory ptr = %d \n\r",IA32_APIC_BASE >> 30);
  sendstringf("Directory = %d \n\r",(IA32_APIC_BASE >> 21) & 0x1ff);
  sendstringf("Offset = %d \n\r",(IA32_APIC_BASE & 0x1FFFFF ));

  {
   // PPDPTE_PAE acpiPageDirectoryPointerEntry=&pagedirptrvirtual[IA32_APIC_BASE >> 30];
    PPDE2MB_PAE apicPageDirTable=(PPDE2MB_PAE)((UINT64)(pagedirvirtual)+(0x1000*(IA32_APIC_BASE >> 30)));

    PPDE2MB_PAE apicPageDirEntry;

    sendstringf("apicPageDirTable=%6\n\r", (UINT64)apicPageDirTable);

    apicPageDirEntry=&apicPageDirTable[(IA32_APIC_BASE >> 21) & 0x1ff];
    sendstringf("apicPageDirEntry=%6\n\r", (UINT64)apicPageDirEntry);

    *(volatile UINT64 *)apicPageDirEntry=0; //IA32_APIC_BASE;
    apicPageDirEntry->P=1;
    apicPageDirEntry->RW=1;
    apicPageDirEntry->US=0;
    apicPageDirEntry->PS=1;
    apicPageDirEntry->PCD=1;
    apicPageDirEntry->PFN=IA32_APIC_BASE >> 13;
    sendstringf("*apicPageDirEntry=%6 \n\r",*(volatile UINT64 *)apicPageDirEntry);

    _invlpg(IA32_APIC_BASE);
  }


  a=1;
  _cpuid(&a,&b,&c,&d);
  displayline("CPUID.1: %8, %8, %8, %8\n",a,b,c,d);

  cpu_stepping=a & 0xf;
  cpu_model=(a >> 4) & 0xf;
  cpu_familyID=(a >> 8) & 0xf;
  cpu_type=(a >> 12) & 0x3;
  cpu_ext_modelID=(a >> 16) & 0xf;
  cpu_ext_familyID=(a >> 20) & 0xff;


  cpu_model=cpu_model + (cpu_ext_modelID << 4);
  cpu_familyID=cpu_familyID + (cpu_ext_familyID << 4);


  if (1) //((d & (1<<28))>0) //this doesn't work in vmware, so find a different method
  {
    unsigned long long initialcount;
    unsigned int foundcpus;
    sendstring("Multi processor supported\n\r");
    sendstring("Launching application cpu's\n");


    if (!loadedOS)
    {
      sendstringf("Getting apic_BootID");

      BOOT_ID=apic_getBootID();
      sendstringf("BOOT_ID=%8\n\r",BOOT_ID);
      sendstringf("APIC_SVR=%8\n\r",APIC_SVR);

    }


    cpucount=1;
    cpuinfo[0].active=1;
    cpuinfo[0].cpunr=0;
    cpuinfo[0].apicid=getAPICID();
    cpuinfo[0].isboot=1;

    //displaystring("Multi processor supported\n");
    displayline("Launching other cpu cores if present\n");
    sendstring("Starting other cpu's\n\r");


    //displayline("Before wait cpucount=%d\n",cpucount);

    //uncomment me for full release



    if (loadedOS)
    {
      sendstringf("mapping loadedOS (%6) at virtual address 0x00800000...", loadedOS);




      POriginalState original=(POriginalState)MapPhysicalMemory(loadedOS,0x00800000);
      sendstringf("Success. It has been mapped at virtual address %6\n",original);

      sendstring("getting foundcpus from loadedOS\n");
      foundcpus=original->cpucount;
      cpucount=foundcpus*2; //temporary test for mem menagement

      sendstringf("cpucount=%x\n",cpucount);

      if (cpucount==0)
      {
        sendstring("---ERROR: CPUCOUNT=0! Check if loadedOS is valid---\n");
        foundcpus=1;
      }
    }
    else
    {
      foundcpus=initAPcpus();


      sendstringf("foundcpus=%d cpucount=%d. Waiting till cpucount==foundcpus, or timeout\n",foundcpus, cpucount);


      for (i=0; i<3; i++)
      {
        initialcount=_rdtsc();
        while ((_rdtsc()-initialcount) < 2000000000ULL)
        {

          //sendstringf("cpucount=%d foundcpus=%d\n\r", cpucount, foundcpus);

          if (cpucount>=foundcpus)
            break;

          _pause();
        }
        displayline(".");
      }

      if (i>=3)
        displayline("Timeout\n");
      else
        displayline("\n");

    }

    displayline("Wait done. Cpu's found : %d (expected %d)\n",cpucount, foundcpus);

  }




  sendstring("Initializing MM\n\r");


  InitializeMM((UINT64)pagedirptrvirtual+9*4096);



  displayline("MM initialized\n");

  //copy GDT and IDT to VMM memory
  GDT_IDT_BASE=malloc(4096);
  if (GDT_IDT_BASE==NULL)
  {
    sendstring("Memory allocation failed\n");
    while (1) ;
  }

  sendstringf("Allocated GDT_IDT_BASE  %x\n", GDT_IDT_BASE);

  {
    void *GDTbase=(void *)getGDTbase();
    void *IDTbase=(void *)getIDTbase();
    int GDTsize=getGDTsize();
    int IDTsize=getIDTsize();
    void *target;


    sendstringf("part1:getGDTbase=%p, getGDTsize=%d\n",GDTbase,GDTsize );

    copymem(GDT_IDT_BASE,GDTbase,GDTsize);




    sendstringf("part2:getIDTbase=%p, getIDTsize=%d\n",IDTbase,IDTsize);
    target=(void *)(UINT64)GDT_IDT_BASE+0x800;
    sendstringf("target=%p\n",target);

    if (!loadedOS) //don't need the copy of the IDT on a loaded OS (saved somewhere else anyhow)
    {
      copymem(target,IDTbase, IDTsize);
    }
    sendstring("part2=done\n");
  }
  sendstringf("Allocated and copied GDT and IDT to %x\n\r",(UINT64)GDT_IDT_BASE);

  //*(BYTE *)0xdead=40;
  setGDT((UINT64)GDT_IDT_BASE, getGDTsize());
  setIDT((UINT64)GDT_IDT_BASE+0x800, getIDTsize());

  fakeARD=malloc(4096);
  sendstringf("Allocated fakeARD at %6\n",(unsigned long long)fakeARD);
  sendstringf("That is physical address %6\n", VirtualToPhysical((UINT64)fakeARD));


  if (!loadedOS)
  {
    sendstring("Copying ARD from 80000 to ARD location");
    copymem((void *)fakeARD,(void *)0x80000,4096); //that should be enough
  }

  sendstring("Calling initARDcount()\n");
  initARDcount();
  sendstring("Calling sendARD()\n");
  sendARD();

  intvector=malloc(sizeof(INT_VECTOR)*256);
  zeromemory(intvector,sizeof(INT_VECTOR)*256);
  sendstringf("Allocated intvector at %8\n\r",(unsigned long long)intvector);

  setints();

  sendstring("after setints()\n");


  // alloc VirtualMachineTSS_V8086
  VirtualMachineTSS_V8086=malloc(3*4096);
  zeromemory(VirtualMachineTSS_V8086,3*4096);

  RealmodeRing0Stack=malloc(4096); //(not even 1 KB used, but let's waste some mem)


  //configure ffpage

  //create a page filled with 0xff (for faking non present memory access)
  ffpage=malloc(4096);
  for (i=0; i<4096; i++)
    ffpage[i]=0xce;

  sendstringf("Physical address of ffpage=%6\n\r",(UINT64)VirtualToPhysical((UINT64)ffpage));


  //create a pagetable with only 0xff (2MB 0xff)
  ffpagetable=malloc(4096);
  zeromemory(ffpagetable,4096);
  for (i=0; i<4096/8; i++)
  {
    ffpagetable[i].P=1;
    ffpagetable[i].RW=0;
    ffpagetable[i].PFN=(UINT64)VirtualToPhysical((UINT64)ffpage) >> 12;
  }

  sendstringf("Physical address of ffpagetable=%6\n\r",(UINT64)VirtualToPhysical((UINT64)ffpagetable));

  //create a pagedir wjere all entries point to the ffpagetable
  ffpagedir=malloc(4096);
  zeromemory(ffpagedir,4096);
  for (i=0; i<4096/8; i++)
  {
    ffpagedir[i].P=1;
    ffpagedir[i].RW=0;
    ffpagedir[i].PS=0;
    ffpagedir[i].PFN=(ULONG)VirtualToPhysical((UINT64)ffpagetable) >> 12;
  }

  sendstringf("Physical address of ffpagedir=%6\n\r",(UINT64)VirtualToPhysical((UINT64)ffpagedir));

  __asm("mov %cr3,%rax\n  mov %rax,%cr3\n");

  displayline("rsp=%6\n\r",getRSP());
  displayline("emulated virtual memory has been configured\n");
	displayline("rsp=%6\n\r",getRSP());


  displayline("Paging:\n");
  displayline("0x00000000 is at %6\n", (UINT64)VirtualToPhysical((UINT64)0));
  displayline("0x00200000 is at %6\n", (UINT64)VirtualToPhysical((UINT64)0x00200000));
  displayline("0x00400000 is at %6\n", (UINT64)VirtualToPhysical((UINT64)0x00400000));
  displayline("0x00600000 is at %6\n", (UINT64)VirtualToPhysical((UINT64)0x00600000));



  displayline("Calling hascpuid()\n");
	if (hascpuid())
	{
		char *t;
		a=0;
		b=0;
		c=0;
		d=0;
		_cpuid(&a,&b,&c,&d);
		sendstringf("Your comp supports cpuid! (%d , %x %x %x )\n\r",a,b,d,c);

		t=(char *)&b;
		sendstringf("Max basicid=%x\n\r",a);
		sendstringf("%c%c%c%c",t[0],t[1],t[2],t[3]);


		t=(char *)&d;
		sendstringf("%c%c%c%c",t[0],t[1],t[2],t[3]);


		t=(char *)&c;
		sendstringf("%c%c%c%c\n\r",t[0],t[1],t[2],t[3]);



    if ((b==0x68747541) && (d==0x69746e65) && (c==0x444d4163))
    {
      isAMD=1;
      AMD_hasDecodeAssists=0;
      sendstring("This is an AMD system. going to use the AMD virtualization tech\n\r");
    }
    else
      isAMD=0;


    //a=0x80000000; _cpuid(&a,&b,&c,&d);
    //if (!(a & 0x80000000))
    {
      unsigned int j;

     // sendstring("\n\r\n\rBranch string=");
      displayline("Branch string=");

      for (j=0x80000002; j<=0x80000004; j++)
      {
        a=j;
        _cpuid(&a,&b,&c,&d);

        t=(char *)&a;
        for (k=0; k<4; k++)
          if (t[k]<32)
            t[k]=' ';
        //sendstringf("%c%c%c%c",t[0],t[1],t[2],t[3]);
        displayline("%c%c%c%c",t[0],t[1],t[2],t[3]);

        t=(char *)&b;
        for (k=0; k<4; k++)
          if (t[k]<32)
            t[k]=' ';
        //sendstringf("%c%c%c%c",t[0],t[1],t[2],t[3]);
        displayline("%c%c%c%c",t[0],t[1],t[2],t[3]);

        t=(char *)&c;
        for (k=0; k<4; k++)
          if (t[k]<32)
            t[k]=' ';
      //  sendstringf("%c%c%c%c",t[0],t[1],t[2],t[3]);
        displayline("%c%c%c%c",t[0],t[1],t[2],t[3]);

        t=(char *)&d;
        for (k=0; k<4; k++)
          if (t[k]<32)
            t[k]=' ';
     //   sendstringf("%c%c%c%c",t[0],t[1],t[2],t[3]);
        displayline("%c%c%c%c",t[0],t[1],t[2],t[3]);
      }
    //  sendstringf("\n\r");
      displayline(" \n");

    }


	}
	else
  {
		sendstring("Your crappy comp doesn\'t support cpuid\n\r");
    displayline("Your system does not support CPUID\n");
  }







	//_invlpg((UINT64)IA32_APIC_BASE);

	//search for _MP_ (0x5f504d5f) header in memory






  cpuinfo[0].TSbase=0;
  cpuinfo[0].TSlimit=0xffff;
  cpuinfo[0].TSsegment=0;
  cpuinfo[0].TSaccessRights=0x8b;

  cpuinfo[0].virtualTLB_guest_PDPTR_lookup=malloc(4096);
  cpuinfo[0].virtualTLB_guest_PD_lookup=malloc(8*4096); //4 should be enough, but this gives me 8GB (can be changed in the future)



  sendstringf("Setting up idttable and jumptable\n\r");

  jumptable=malloc(4096);
  idttable32=malloc(4096);

  sendstringf("jumptable allocated at %x (%6)\n\r",(UINT64)jumptable, VirtualToPhysical((UINT64)jumptable));
  sendstringf("idttable32 allocated at %x (%6)\n\r",(UINT64)idttable32, VirtualToPhysical((UINT64)idttable32));

  //fill jumptable and IDT
  PINT_VECTOR32 idt32=(PINT_VECTOR32)idttable32;
  UINT64 pa=VirtualToPhysical((UINT64)jumptable);
  UINT64 inthandler32address=(UINT64)VirtualToPhysical((UINT64)&inthandler_32);

  unsigned char *jumptablepc;
  jumptablepc=(unsigned char *)jumptable;
  for (i=0; i<256; i++, pa+=10)
  {
    //push i
    jumptablepc[i*10]=0x68; //push
    *(DWORD *)&jumptablepc[i*10+1]=i;

    //jmp inthandler_32
    jumptablepc[i*10+5]=0xe9;
    *(DWORD *)(&jumptablepc[i*10+6])=inthandler32address-(pa+10); //from (current offset+5(including push 7)) to inthandler_32
    idt32[i].bAccess=0x8e;
    idt32[i].bUnused=0;
    idt32[i].wHighOffset=pa >> 16;
    idt32[i].wLowOffset=pa & 0xffff;
    idt32[i].wSelector=24;

    //sendstringf("idt32[%x]=%x\n\r",i, *(UINT64*)&idt32[i]);
  }

  sendstring("setting up gdt entry at offset 0x64 as virtual8086 task\n\r");
  PGDT_ENTRY currentgdt=(PGDT_ENTRY)getGDTbase();
  sendstringf("currentgdt is %x (limit=%x)\n\r",(UINT64)currentgdt, getGDTsize());
  ULONG length=(ULONG)sizeof(TSS)+32+8192+1;


  currentgdt[8].Limit0_15=length;
  currentgdt[8].Base0_23=(DWORD)VirtualToPhysical((UINT64)VirtualMachineTSS_V8086);
  currentgdt[8].Type=0x9;
  currentgdt[8].System=0;
  currentgdt[8].DPL=3;
  currentgdt[8].P=1;
  currentgdt[8].Limit16_19=length >> 16;
  currentgdt[8].AVL=1;
  currentgdt[8].Reserved=0;
  currentgdt[8].B_D=0;
  currentgdt[8].G=0;
  currentgdt[8].Base24_31=(DWORD)VirtualToPhysical((UINT64)VirtualMachineTSS_V8086) >> 24;

  //setup GDT for realmode jump
  currentgdt[4].Base0_23=0x20000;


  if (!loadedOS)
  {
    sendstringf("Setting up 64-bit TS and TSS\n\r");

    TSS *temp=malloc(4096);
    ownTSS=temp;
    zeromemory(temp,4096);
    currentgdt=(PGDT_ENTRY)(getGDTbase()+96);


    currentgdt->Limit0_15=sizeof(TSS);
    currentgdt->Base0_23=(UINT64)temp;
    currentgdt->Type=0x9;
    currentgdt->System=0;
    currentgdt->DPL=3;
    currentgdt->P=1;
    currentgdt->Limit16_19=sizeof(TSS) >> 16;
    currentgdt->AVL=1;
    currentgdt->Reserved=0;
    currentgdt->B_D=0;
    currentgdt->G=0;
    currentgdt->Base24_31=((UINT64)temp) >> 24;
    loadTaskRegister(96);

    {
      //debugging a nasty bug that is probably the cause of the 75% chance crash
      unsigned char testgdt[0x80]={0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, \
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, \
          0x00, 0x00, 0x00, 0x00, 0x00, 0x9b, 0x20, 0x00, \
          0xff, 0xff, 0x00, 0x00, 0x00, 0x93, 0xcf, 0x00, \
          0xff, 0xff, 0x00, 0x00, 0x00, 0xfb, 0xcf, 0x00, \
          0xff, 0xff, 0x00, 0x00, 0x00, 0xf3, 0xcf, 0x00, \
          0x00, 0x00, 0x00, 0x00, 0x00, 0xfb, 0x20, 0x00, \
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, \
  /*TSS*/ 0x67, 0x00, 0x80, 0x20, 0x3f, 0x8b, 0x00, 0xe8, \
          0x01, 0xf8, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00, \
          0x00, 0x3c, 0x00, 0xe0, 0xfd, 0xf3, 0x40, 0xff, \
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, \
          0xff, 0xff, 0x00, 0x00, 0x00, 0x9b, 0xcf, 0x00, \
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, \
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, \
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

      PGDT_ENTRY tgdt=&testgdt[0x40];

/*
     TSS=
     0x67, 0x00, 0x80, 0x20, 0x3f, 0x8b, 0x00, 0xe8
     0x01, 0xf8, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00

     00000000
     fffff801
     e8008b3f
     20800067

     base:
     15:00=2080
     23:16=3f
     31:24=e8
     63:32=fffff801

     fffff801e83f2080   good

     fffff800e83f2080   bad
 */

      QWORD testbase;

      sendstringf("tgdt->Base0_23=%x\n", tgdt->Base0_23);  //3f2080
      sendstringf("tgdt->Base24_31=%x\n", tgdt->Base24_31); //e8
      sendstringf("*(ULONG *)(&tgdt[1])=%x\n", *(ULONG *)(&tgdt[1])); //fffff801


      testbase=tgdt->Base24_31 << 24;

      sendstringf("Shift test:%x\n", testbase);


      testbase=getSegmentBaseEx(testgdt, 0, 0x40, 1);



      sendstringf("testbase is %6\n", testbase);
      if (testbase!=0xfffff801e83f2080ULL)
        sendstringf("FAIL!\n");

    }

    {
      //another debugtest where an emulated interrupt did not see it was a CPL change
      unsigned char testgdt[0x80]={ \
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, \
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, \
          0x00, 0x00, 0x00, 0x00, 0x00, 0x9b, 0x20, 0x00, \
          0xff, 0xff, 0x00, 0x00, 0x00, 0x93, 0xcf, 0x00, \
          0xff, 0xff, 0x00, 0x00, 0x00, 0xfb, 0xcf, 0x00, \
          0xff, 0xff, 0x00, 0x00, 0x00, 0xf3, 0xcf, 0x00, \
          0x00, 0x00, 0x00, 0x00, 0x00, 0xfb, 0x20, 0x00, \
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, \
  /*TSS*/ 0x67, 0x00, 0xc0, 0x1d, 0xae, 0x8b, 0x00, 0x02, \
          0x80, 0xf8, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00, \
          0x00, 0xfc, 0x00, 0xe0, 0xa1, 0xf3, 0x40, 0xff, \
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, \
          0xff, 0xff, 0x00, 0x00, 0x00, 0x9b, 0xcf, 0x00, \
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, \
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, \
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

      PGDT_ENTRY tgdt=&testgdt[0];
      Segment_Attribs old_ssattribs, new_csattribs;
      old_ssattribs.SegmentAttrib=getSegmentAttrib(tgdt, 0, 0x2b);
      new_csattribs.SegmentAttrib=getSegmentAttrib(tgdt, 0, 0x10);
      sendstring("DPL test:\n");


      sendstringf("old_ssattribs.SegmentAttrib=%x\n", old_ssattribs.SegmentAttrib);
      sendstringf("old_csattribs.SegmentAttrib=%x\n", new_csattribs.SegmentAttrib);
      sendstringf("ss dpl=%d\n", old_ssattribs.DPL);
      sendstringf("cs dpl=%d\n", new_csattribs.DPL);


    }



  }

  displayline("Generating debug information\n\r");
  originalIDTcrc=generateCRC(NULL, 0x400);
  originalVMMcrc=generateCRC((void*)vmxloop,0x2a000);


  displayline("Virtual machine manager loaded\n");




#ifdef DEBUG
  displayline("Entering menu system\n");
#else
  displayline("Skipping menu system and autostarting VM\n");
#endif




  menu2();


  return;
}

#pragma GCC push_options
#pragma GCC optimize ("O0")
int testexception(void)
{

  volatile int result=2;


  bochsbp();

  __asm("nop");
  __asm("nop");
  __asm("nop");

  result=readMSRSafe(&cpuinfo[0], 553);

  //nothing happened
  //result=0;
  displayline("result=%d\n", result);


  return result;
}

void vmcalltest(void)
{
  pcpuinfo currentcpuinfo=&cpuinfo[0];
  int dbvmversion;
  dbvmversion=0;

  currentcpuinfo->LastInterrupt=0;
  currentcpuinfo->OnInterrupt.RIP=(volatile void *)&&InterruptFired; //set interrupt location
  currentcpuinfo->OnInterrupt.RSP=getRSP();



  dbvmversion=vmcalltest_asm();


InterruptFired:
  currentcpuinfo->OnInterrupt.RIP=0;


  if (dbvmversion==0)
    displayline("DBVM is not loaded (Exception %d)\n", currentcpuinfo->LastInterrupt);
  else
    displayline("DBVM version = %x\n", dbvmversion);




}


#pragma GCC pop_options



void reboot(void)
{
  int i;
  for (i=1; i<cpucount; i++) //disable all AP cpu's first (in vmm mode this will put the AP cpu's into wait-for-sipi mode)
  {
    cpuinfo[i].hastoterminate=1;
    while (cpuinfo[i].active) ;
  }

  ULONG gdtaddress=getGDTbase();  //0x40002 contains the address of the GDT table

  sendstring("Copying gdt to low memory\n\r");
  copymem((void *)0x50000,(void *)(UINT64)gdtaddress,0x50); //copy gdt to 0x50000

  sendstring("copying movetoreal to 0x2000\n\r");
  copymem((void *)0x20000,(void *)(UINT64)&movetoreal,(UINT64)&vmxstartup_end-(UINT64)&movetoreal);


  sendstring("Calling quickboot\n\r");
  quickboot();
  sendstring("WTF?\n\r");
}


void menu2(void)
{
  unsigned char key;

  if (isAMD)
    vmcall_instr=vmcall_amd;
  else
    vmcall_instr=vmcall_intel;

  if (!loadedOS)
  {


    if (*(BYTE *)0x7c10==1) //if override by setup then
    {
      *(BYTE *)0x7c0e=*(BYTE *)0x7c0f; //set to bootdrive2
    }
    else
    {
      //find out myself
      if (*(BYTE *)0x7c0e==0)
        *(BYTE *)0x7c0e=0x80;
      else
      if (*(BYTE *)0x7c0e==0x80)
        *(BYTE *)0x7c0e=0x81;
    }
  } else sendstringf("loadedOS=%6\n",loadedOS);


  //*(BYTE *)0x7c0e=0x80;
  while (1)
  {
    clearScreen();
    currentdisplayline=0;

    {
    	vmcb x;
    	int offset;

    	offset=(QWORD)&x.DR6-(QWORD)&x;


        	displayline("DR6=%x\n", offset);


    }

    displayline("Welcome to the DBVM interactive menu\n\n");
    displayline("These are your options:\n");
    displayline("0: Start virtualization\n");
    displayline("1: Keyboard test\n");
    displayline("2: Set disk to startup from (currently %2)\n",*(BYTE *)0x7c0e);
    displayline("3: Disassembler test\n");
    displayline("4: Interrupt test\n");
    displayline("5: Breakpoint test\n");
    displayline("6: Set Redirects with dbvm (only if dbvm is already loaded)\n");
    displayline("7: Pagefault test\n");
    displayline("8: PCI enum test (finds db's serial port)\n");
    displayline("9: test input\n");
    displayline("a: test branch profiling\n");
    displayline("b: boot without vm (test state vm would set)\n");
    displayline("c: boot without vm and lock FEATURE CONTROL\n");
    displayline("v: vm(m)call test (test state vm would set)\n");


    key=0;
    while (!key)
    {
#if (!defined(DEBUG)) || (defined(DISPLAYDEBUG))
      if (!loadedOS)
      {
        if (*(BYTE *)0x7c11==1) //autorun
          key='3';
        else
          key=kbd_getchar();
      }
      else
        key='0';
#else
      //in debugmode
      if (!loadedOS)
      {
    	  //loaded OS is null, get user inputer
    	  key=kbd_getchar();
      } else key='0'; //loadedOS, so start imeadiatly
#endif


      if (key)
      {
        char temps[16];
        displayline("%c\n", key);

        switch (key)
        {
          case '0':
            clearScreen();
            menu();
            break;

          case '1':
            displayline("kdbstatus=%2\n",kbd_getstatus());
            displayline("kdb_getoutputport=%2\n",kdb_getoutputport());
            displayline("kdb_getinputport=%2\n",kdb_getinputport());
            displayline("kdb_getcommandbyte=%2\n",kdb_getcommandbyte());
            break;

          case '2':
            displayline("Set the new drive: ");
            readstringc(temps,2,16);
            temps[15]=0;

            *(BYTE *)0x7c0e=atoi(temps,16,NULL);
            displayline("\nNew drive=%2 \n",*(BYTE *)0x7c0e);
            break;

          case '3': //disassemblerr
            {
              _DecodedInst disassembled[22];
              unsigned int i;
              unsigned int used=0;
              distorm_decode((UINT64)&menu2,(unsigned char*)menu2, 256, Decode64Bits, disassembled, 22, &used);

              if (used)
              {
                for (i=0; i<used; i++)
                {
                  displayline("%x : %s - %s %s\n",
                    disassembled[i].offset,
                    disassembled[i].instructionHex.p,
                    disassembled[i].mnemonic.p,
                    disassembled[i].operands.p);
                }

              }
              else
              {
                displayline("Failure...\n");
              }


            }
            break;

          case '4':
          {
            testexception();
            break;
          }

          case '5':
          {
            UINT64 rflags;

            displayline("Doing an int3 bp\n");
            int3bptest();


            displayline("Setting the GD flag");
            setDR6(0xfffffff0);
            setDR7(getDR7() | (1<<13));
            setDR6(0xffff0ff0);
            //RF


            displayline("Setting the single step flag\n\r");
            rflags=getRFLAGS(); //NO RF
            setRFLAGS(rflags | (1<<8));

            setRFLAGS(rflags & (~(1<<8))); //unset

            displayline("Setting an execute breakpoint\n\r");
            setDR0(getCR0);
            setDR6(0xffff0ff0);
            setDR7(getDR7() | (1<<0));
            displayline("Going to execute it\n");  //NO RF

            getCR0();

            displayline("Setting a RW breakpoint\n\r");
            setDR0(&isAP);
            setDR6(0xffff0ff0);
            setDR7(getDR7() | (3<<18) | (3<<16) | (1<<0));
            displayline("Goint to write to that breakpoint\n"); //NO RF

           // bochsbp();
            isAP++;
            //*(DWORD *)4=0x12345678;
            displayline("done writing\n");

            break;
          }

          case '6':
          {
            displayline("Setting the redirects. #UD Interrupt will fire if dbvm is not loaded (and crash)");
            vmcall_setintredirects();

            break;

          }

          case '7':
          {
            //PSOD("FUUUUUUU");
            int v=*(int *)0xcececece;
            sendstringf("Value = %x\n", v);

            break;
          }

          case '8':
          {
            pciConfigEnumPci();
            break;
          }

          case '9':
          {
            {
              char temps[17];
              int bits;
              UINT64 address;
              int size;
              int err1,err2,err3;

              sendstring("\nAddress:");
              readstring(temps,16,16);
              address=atoi(temps,16,&err2);

              sendstring("\nNumber of bytes:");
              readstring(temps,16,16);
              size=atoi(temps,10,&err3);

              {
                _DecodedInst disassembled[22];
                unsigned int i;
                unsigned int used=0;
                distorm_decode((UINT64)address,(unsigned char*)address, size, Decode64Bits, disassembled, 22, &used);

                if (used)
                {
                  for (i=0; i<used; i++)
                  {
                    displayline("%x : %s - %s %s\n",
                      disassembled[i].offset,
                      disassembled[i].instructionHex.p,
                      disassembled[i].mnemonic.p,
                      disassembled[i].operands.p);
                  }

                }
                else
                {
                  displayline("Failure...\n");
                }
              }

            }

            break;
          }

          case 'a':
          {
            testBranchPrediction();
            break;
          }

          case 'b':
          {
            reboot();
            displayline("WTF?\n");
            break;
          }

          case 'c':
          {
            QWORD IA32_FEATURE_CONTROL;
            IA32_FEATURE_CONTROL=readMSR(IA32_FEATURE_CONTROL_MSR);
            displayline("IA32_FEATURE_CONTROL was %6\n\r",IA32_FEATURE_CONTROL);

            if (IA32_FEATURE_CONTROL & FEATURE_CONTROL_LOCK)
            {
              displayline("IA32_FEATURE_CONTROL is locked (value=%6). (Disabled in bios?)\n\r",IA32_FEATURE_CONTROL);
              if (!(IA32_FEATURE_CONTROL & FEATURE_CONTROL_VMXON ))
              {
                displayline("Bit 2 (VMX) is also disabled. VMX is not possible\n");
                return;
              }
              else
                displayline("VMXON was already enabled in the feature control MSR\n");
            }
            else
            {
              displayline("Not locked yet\n");

              IA32_FEATURE_CONTROL=IA32_FEATURE_CONTROL | FEATURE_CONTROL_VMXON | FEATURE_CONTROL_LOCK;

              displayline("setting IA32_FEATURE_CONTROL to %6\n\r",IA32_FEATURE_CONTROL);

              writeMSR(IA32_FEATURE_CONTROL_MSR,IA32_FEATURE_CONTROL);
              IA32_FEATURE_CONTROL=readMSR(IA32_FEATURE_CONTROL_MSR);
              displayline("IA32_FEATURE_CONTROL is now %6\n\r",IA32_FEATURE_CONTROL);
            }




            displayline("Press a key to boot");
            key=kbd_getchar();
            reboot();
            displayline("WTF?\n");
            break;
          }


          case 'v':
          {
            vmcalltest();
            break;
          }

          default:
            key=0;
            break;
        }



        if (key)
        {
          displayline("Press any key to return to the menu\n");
          while (kbd_getchar()==0) ;
        }
      }
    }

		resync();
	}
}


void menu(void)
{
  displayline("menu\n\r"); //debug to find out why the vm completely freezes when SERIALPORT==0

  sendstring("menu\n\r");

  displayline("After sendstring\n");

  int i,j;

  nosendchar[getAPICID()]=0; //force that it gets send

  while (1)
  {
    char command;
    sendstring("\n\r\n\rWelcome to Dark Byte\'s virtual machine monitor\n\r");
    sendstring("\n\r^^^^^^^^^^^^^^^^^^^^^^^Menu 1^^^^^^^^^^^^^^^^^^\n\r");
    sendstring("Press 0 to run the VM\n\r");
    sendstring("Press 1 to display the fake memory map\n\r");
    sendstring("Press 2 to display the virtual memory of the VMM\n\r");
    sendstring("Press 3 to display the physical memory of this system\n\r");
    sendstring("Press 4 to display the virtual memory of the Virtual Machine\n\r");
    sendstring("Press 5 to raise int 1 by software\n\r");
    sendstring("Press 6 to run some testcode in the 2nd core (assuming there is one)\n\r");
    sendstring("Press 7 to test some crap\n\r");
    sendstring("Press 8 to execute testcode()\n\r");
    sendstring("Press 9 to restart\n\r");
    sendstring("Your command:");

#ifndef DEBUG
    if (autostart || loadedOS)
    {
      autostart=0;
      command='0';
    }
    else
#endif
    {
      displayline("Waiting for serial port command:\n");
      sendstring("waiting for command:");

      if (loadedOS)
      {
        command='0';

      }
      else
      {
#if (defined SERIALPORT) && (SERIALPORT != 0)
        command=waitforchar();
#else
        command='0';
#endif

      }
    }



    displayline("Checking command");


    sendchar(command);

    displayline("After sendchar\n");
    sendstring("\n\r");

    displayline("...");

    switch (command)
    {

      case  '0' : //run virtual machine
      {
        UINT64 a,b,c,d;

        displayline("Starting the virtual machine\n");

        if (!loadedOS)
        {

          if (cpucount>0) //!isAMD for now during tests
          {
            displayline("Sending other CPU cores into VMX wait mode\n");
            sendstring("BootCPU: Sending all AP's the command to start the VMX code\n\r");



            for (i=1; (unsigned)i<cpucount; i++)
            {
              UINT64 rsp;
              displayline("BOOT CORE:Starting Application core %d to go into VMX and waiting for it to actually enter\n",i);
              cpuinfo[i].command=1;

              rsp=0;

              _cpuid(&a,&b,&c,&d); //serializes instructions


              while (cpuinfo[i].vmxsetup==0)
              {
                if (rsp==0)
                  rsp=getRSP();

                _pause();
                _cpuid(&a,&b,&c,&d);

                if (getRSP()!=rsp)
                {
                  displayline("BootCPU: STACK CORRUPTION DETECTED DURING WAIT\n");
                  while (1);
                }
              }



            }

            //wait till the other cpu's are started
            sendstring("BOOT CORE: Other cpu's finished setting up, now start the boot cpu\n\r");
            displayline("BOOT CORE: Wait done, other cores are ready to be handled by boot core vm\n");
          }


          for (i=1; i<32; i++) //tell slow startup cores to run as soon as possible
            cpuinfo[i].command=1;


          displayline("Calling startvmx for main core\n");
        }


        startvmx(&(cpuinfo[0]));
        sendstring("BootCPU: Back from startvmx\n\r");
        break;
      }

      case  '1' : //display fake mem map
        sendARD();
        break;


      case  '2' : //display vmm mem
        {
          char temps[17];
          unsigned long long StartAddress;
          unsigned int nrofbytes;

          sendstring("Startaddress:");
          readstring(temps,16,16);
          sendstring("\n\r");
          sendstringf("temps=%s \n\r",temps);
          StartAddress=atoi(temps,16,NULL);


          sendstring("Number of bytes:");
          readstring(temps,8,8);
          sendstring("\n\r");
          nrofbytes=atoi(temps,10,NULL);

          sendstringf("Going to show the memory region %6 to %6 (physical=%6)\n\r",StartAddress,StartAddress+nrofbytes,VirtualToPhysical((UINT64)StartAddress));

          for (i=0; (unsigned int)i<nrofbytes; i+=16)
          {
            sendstringf("%8 : ",StartAddress+i);
            for (j=i; (j<(i+16)) && ((unsigned)j<nrofbytes); j++)
              sendstringf("%2 ",(unsigned char)*(unsigned char *)(StartAddress+j));

            if ((unsigned)(i+16)>nrofbytes)
            {
              // Get the cursor to the right spot
              int currentcol=11+3*(nrofbytes-i);
              int wantedcol=11+3*16;
              for (j=0; j<(wantedcol-currentcol); j++)
                sendstring(" ");
            }

            for (j=i; j<(i+16) && ((unsigned)j<nrofbytes); j++)
            {
              unsigned char tempc=*(unsigned char *)(StartAddress+j);
              if (tempc<32)
                tempc='.';

              sendstringf("%c",tempc);
            }

            sendstring("\n\r");
          }

          break;
        }



      case  '3' : //display physical memory
        //use one page (mapped at 0x80000000, 4mb) to display the memory
        {
          displayPhysicalMemory();
        }
        break;


      case  '4' : //display vm memory (virtual)
				{
				  allocateVirtualTLB();
				  sendstringf("AvailableVirtualAddress=%6\n", cpuinfo[0].AvailableVirtualAddress);

          displayVMmemory(&cpuinfo[0]);
          sendstringf("AvailableVirtualAddress=%6\n", cpuinfo[0].AvailableVirtualAddress);
				}
        break;


      case  '5' :
        asm("int $5\n");
        break;


      case  '6' : //run 2nd core testapp
        {
          /*
          unsigned int a,b,c,d;
          a=1;
          _cpuid(&a,&b,&c,&d);
          sendstringf("cpuid:1: eax=%8, ebx=%8, ecx=%8, edx=%8, \n\r",a,b,c,d);
          if ((d & (1<<28))>0)
          {
            sendstring("Multi processor supported\n\r");
            sendstringf("logical processor per package: %d \n\r",(d >> 16) & 0xff);

            BOOT_ID=apic_getBootID();
            sendstringf("BOOT_ID=%8\n\r",BOOT_ID);

            sendstringf("APIC_SVR=%8\n\r",APIC_SVR);


            cpucount=1;
            apic_enableSVR();
            while (cpucount==1)
            {
              a=1;
              _cpuid(&a,&b,&c,&d); //serializing instruction
            }

          }
          else
            sendstring("No multi processor support\n");


          */

          break;
        }

      case  '7':
        {
          int error;
          UINT64 pf;
          pcpuinfo currentcpuinfo=&cpuinfo[0];
          currentcpuinfo->AvailableVirtualAddress=(UINT64)(currentcpuinfo->cpunr+16) << 28;

          sendstringf("currentcpuinfo->AvailableVirtualAddress=%6\n", currentcpuinfo->AvailableVirtualAddress);

          allocateVirtualTLB();


          void *address=mapVMmemory(&cpuinfo[0], 0xc0000ULL, 16, currentcpuinfo->AvailableVirtualAddress, &error, &pf);

          sendstringf("address=%6\n", address);
          sendstringf("currentcpuinfo->AvailableVirtualAddress=%6\n", currentcpuinfo->AvailableVirtualAddress);

          if (error==0)
            sendstringf("*address=%2\n", *(char *)address);
          else
            sendstringf("error=%d  (pf=%6)\n", error, pf);





          break;
          //



        }

			case  '8':
				{
          unsigned char *wr;
          unsigned char *temppage1=malloc(4096);
          unsigned char *temppage2=malloc(4096);
          unsigned char *temppage3=malloc(4096);
          unsigned char *temppage4=malloc(4096);
          PPTE_PAE temppagetable=malloc(4096);
          zeromemory(temppagetable,4096);
          zeromemory(temppage1,4096);
          zeromemory(temppage2,4096);
          zeromemory(temppage3,4096);
          zeromemory(temppage4,4096);

          strcpy((char *)temppage1,"1: Unmodified\n\r");
          strcpy((char *)temppage2,"2: Unmodified\n\r");
          strcpy((char *)temppage3,"3: Unmodified\n\r");
          strcpy((char *)temppage4,"4: Unmodified\n\r");

          sendstringf("pagedirvirtual=%8 (%8)\n\r",(UINT64)pagedirvirtual,(UINT64)VirtualToPhysical((UINT64)pagedirvirtual));
          sendstringf("temppagetable=%8 (%8)\n\r",(UINT64)temppagetable,(UINT64)VirtualToPhysical((UINT64)temppagetable));


          sendstringf("address of temppage1=%6\n\r",(UINT64)temppage1);
          sendstringf("address of temppage2=%6\n\r",(UINT64)temppage2);
          sendstringf("address of temppage3=%6\n\r",(UINT64)temppage3);
          sendstringf("address of temppage4=%6\n\r",(UINT64)temppage4);

          //0: supervisor, rw pagetableentry
          temppagetable[0].P=1;
          temppagetable[0].RW=1;
          temppagetable[0].US=0;
          temppagetable[0].PFN=(ULONG)VirtualToPhysical((UINT64)temppage1) >> 12;

          //1: usermode, rw pagetableentry
          temppagetable[1].P=1;
          temppagetable[1].RW=1;
          temppagetable[1].US=1;
          temppagetable[1].PFN=(ULONG)VirtualToPhysical((UINT64)temppage2) >> 12;

          //2: supervisor, ro pagetableentry
          temppagetable[2].P=1;
          temppagetable[2].RW=0;
          temppagetable[2].US=1;
          temppagetable[2].PFN=(ULONG)VirtualToPhysical((UINT64)temppage3) >> 12;

          //3: usermode ro pagetableentry
          temppagetable[3].P=1;
          temppagetable[3].RW=0;
          temppagetable[3].US=0;
          temppagetable[3].PFN=(ULONG)VirtualToPhysical((UINT64)temppage4) >> 12;


          pagedirvirtual[0].P=1;
          pagedirvirtual[0].US=1;
          pagedirvirtual[0].RW=1;
          pagedirvirtual[0].PS=0;
          ((PPDE_PAE)(pagedirvirtual))[0].PFN=(UINT64)VirtualToPhysical((UINT64)temppagetable) >> 12;

          _invlpg(0);
          _invlpg(0x1000);
          _invlpg(0x2000);
          _invlpg(0x3000);

          sendstring("Unedited:\n\r");
          sendstringf("0=%s\n\r",(char *)0);
          sendstringf("1=%s\n\r",(char *)0x1000);
          sendstringf("2=%s\n\r",(char *)0x2000);
          sendstringf("3=%s\n\r",(char *)0x3000);

          sendstring("\n\rEdited:\n\r");



          wr=(unsigned char *)0;
          wr[0]='L';
          wr[1]='O';
          wr[2]='S';
          wr[3]='E';
          wr[4]='R';
          sendstringf("0=%s\n\r",(char *)0);


          wr=(unsigned char *)0x1000;
          wr[0]='I';
          wr[1]='D';
          wr[2]='I';
          wr[3]='O';
          wr[4]='T';
          sendstringf("1=%s\n\r",(char *)0x1000);


          wr=(unsigned char *)0x2000;
          wr[0]='H';
          wr[1]='O';
          wr[2]='M';
          wr[3]='O';
          sendstringf("2=%s\n\r",(char *)0x2000);

          wr=(unsigned char *)0x3000;
          wr[0]='R';
          wr[1]='E';
          wr[2]='T';
          wr[3]='A';
          wr[3]='R';
          wr[3]='D';

          sendstringf("3=%s\n\r",(char *)0x3000);

          //set WP bit
          sendstring("Writing back to original\n\r");
          strcpy((char *)temppage1,"1: Unmodified\n\r");
          strcpy((char *)temppage2,"2: Unmodified\n\r");
          strcpy((char *)temppage3,"3: Unmodified\n\r");
          strcpy((char *)temppage4,"4: Unmodified\n\r");

          sendstringf("Setting wp bit\n\r");
          setCR0(getCR0() | 1<<16);

          sendstring("Invalidating pages\n\r");

          _invlpg(0);
          _invlpg(0x1000);
          _invlpg(0x2000);
          _invlpg(0x3000);

          sendstring("modifying mem\n\r");


          wr=(unsigned char *)0;
          wr[0]='L';
          wr[1]='O';
          wr[2]='S';
          wr[3]='E';
          wr[4]='R';
          sendstringf("0=%s\n\r",(char *)0);


          wr=(unsigned char *)0x1000;
          wr[0]='I';
          wr[1]='D';
          wr[2]='I';
          wr[3]='O';
          wr[4]='T';
          sendstringf("1=%s\n\r",(char *)0x1000);


          wr=(unsigned char *)0x2000;
          wr[0]='H';
          wr[1]='O';
          wr[2]='M';
          wr[3]='O';
          sendstringf("2=%s\n\r",(char *)0x2000);

          wr=(unsigned char *)0x3000;
          wr[0]='R';
          wr[1]='E';
          wr[2]='T';
          wr[3]='A';
          wr[3]='R';
          wr[3]='D';

          sendstringf("3=%s\n\r",(char *)0x3000);


          break;
        }

        case	'9':
        {
          reboot();

				}

				break;

       /*
      case  'i':
        showstate();
        break;

      case  't':
        {
          ULONG xxx=MapPhysicalMemory(0xfb000000, 0xfb000000);
          sendstringf("xxx=%8\n\r",xxx);
          testcode();
          break;
        }

      case  'c' :
      {
        unsigned int crc=generateCRC((unsigned char *)(e), 0x007fffff-(e));
        unsigned int idtcrc=generateCRC((unsigned char *)0, 0x400);
        unsigned int vmmcrc=generateCRC((unsigned char *)0x00400000, getGDTbase() - 0x00400000);
        sendstringf("stack crc=%8\n\r",crc);
        sendstringf("idt crc=%8\n\r",idtcrc);
        sendstringf("vmm crc=%8\n\r",vmmcrc);
        break;
      }
      */

      default :
        sendstring("That is not a valid option\n\r");
        break;

    }


  }

}


void startvmx(pcpuinfo currentcpuinfo)
{
#ifdef DEBUG
  UINT64 entryrsp=getRSP();
#endif

  UINT64 a,b,c,d;



  displayline("cpu %d: startvmx:\n",currentcpuinfo->cpunr);
#ifdef DEBUG
  sendstringf("currentcpuinfo=%6  (cpunr=%d)\n\r",(UINT64)currentcpuinfo, currentcpuinfo->cpunr);
  sendstringf("ESP=%6\n\r",entryrsp);
  sendstringf("APICID=%d\n\r",getAPICID());
#endif




  // setup the vmx environment and run it
  if (hascpuid())
  {

    unsigned char ext_fam_id;
    unsigned char ext_model_id;
    unsigned char proc_type;
    unsigned char family_id;
    unsigned char model;
    unsigned char stepping_id;

    a=1;
    _cpuid(&a,&b,&d,&c);


    stepping_id= a & 0xf; //4
    model=(a >> 4) & 0xf; //4
    family_id=(a >> 8) & 0xf; //4
    proc_type=(a >> 12) & 0x3; //2
    //2
    ext_model_id=(a >> 16 ) & 0xf; //4
    ext_fam_id=(a >> 20 ) & 0xff;

    sendstringf("Version Information=%x : \n\r",a);
    sendstringf("\tstepping_id=%d\n\r",stepping_id);
    sendstringf("\tmodel=%d\n\r",model);
    sendstringf("\tfamily_id=%d\n\r",family_id);
    sendstringf("\tproc_type=%d\n\r",proc_type);
    sendstringf("\text_model_id=%d\n\r",ext_model_id);
    sendstringf("\text_fam_id=%d\n\r",ext_fam_id);



    sendstringf("Brand Index/CLFLUSH/Maxnrcores/Init APIC=%x :\n",b);
    {
      unsigned char brand_index;
      unsigned char CLFLUSH_line_size;
      unsigned char max_logical_cpu;
      unsigned char initial_APIC;

      brand_index=b & 0xff; //4
      CLFLUSH_line_size=(b >> 8) & 0xff;
      max_logical_cpu=(b >> 16) & 0xff;
      initial_APIC=(b >> 24) & 0xff;

      sendstringf("\tBrand Index=%d\n\r",brand_index);
      sendstringf("\tCLFLUSH line size=%d\n\r",CLFLUSH_line_size);
      sendstringf("\tMaximum logical cpu\'s=%d\n\r",max_logical_cpu);
      sendstringf("\tinitial APIC=%d\n\r",initial_APIC);
    }

   // PSOD("Line 2149: PSOD Test");

    if (isAMD)
    {
      sendstring("AMD virtualization handling\n\r");



      UINT64 a=0x80000001;
      UINT64 b,c,d;

      _cpuid(&a,&b,&c,&d);

      if (c & (1<<2)) //SVM bit in cpuid
      {
    	  sendstring("SVM supported\n");

    	  a=0x8000000a;
    	  _cpuid(&a,&b,&c,&d);

    	  displayline("cpuid: 0x8000000a:\n");
    	  displayline("EAX=%8\n", a);
    	  displayline("EBX=%8\n", b);
    	  displayline("ECX=%8\n", c);
    	  displayline("EDX=%8\n", d);


    	  AMD_hasDecodeAssists=(d & (1<<7))>0;
    	  AMD_hasNRIPS=(d & (1<<3))>0;
    	  sendstringf("AMD_hasDecodeAssists=%d\n", AMD_hasDecodeAssists);

    	  UINT64 VM_CR=readMSR(0xc0010114); //VM_CR MSR
    	  sendstringf("VM_CR=%6\n", VM_CR);

    	  if ((VM_CR & (1<<4))==0)
    	  {
    		  UINT64 efer;
    		  sendstring("SVM is available\n");



    		  currentcpuinfo->vmcb=malloc(4096);
    		  //SetPageToWriteThrough((UINT64)currentcpuinfo->vmcb); //test. I doubt it's needed since no cpu touches another's vmcb

    		  sendstringf("Have set vmcb at %x to WriteThrough caching protection\n", currentcpuinfo->vmcb);

    		  zeromemory(currentcpuinfo->vmcb, 4096);

    		  currentcpuinfo->vmcb_PA=(UINT64)VirtualToPhysical((UINT64)currentcpuinfo->vmcb);

          sendstring("Setting SVME bit in EFER\n");
          efer=readMSR(EFER_MSR);

          sendstringf("EFER was %6\n", efer);
          efer=efer | (1 << 12);
          sendstringf("EFER will become %6\n", efer);


          writeMSR(EFER_MSR, efer);




          UINT64 VM_HSAVE_PA_MSR=readMSR(0xc0010117); //VM_HSAVE_PA MSR
          sendstringf("VM_HSAVE_PA_MSR was %6\n", VM_HSAVE_PA_MSR);

          currentcpuinfo->vmcb_host=malloc(4096);
        //  bochsbp();
          writeMSR(0xc0010117, (UINT64)VirtualToPhysical((UINT64)currentcpuinfo->vmcb_host));



    		  setupVMX(currentcpuinfo);


          /*if (!isAP)
            clearScreen();*/




          launchVMX(currentcpuinfo);

          sendstring("launchVMX returned\n");
          while (1)
          {

          }


    	  }
    	  else
    	  {
    		  sendstring("SVM has been disabled\n");
    	  }


      }
      else
      {
    	  sendstring("This cpu does not support SVM\n");
    	  sendstringf("cpuid: 0x80000001:\n");
    	  sendstringf("EAX=%8\n", a);
    	  sendstringf("EBX=%8\n", b);
    	  sendstringf("ECX=%8\n", c);
    	  sendstringf("EDX=%8\n", d);

      }



    }
    else
    {

      if ((d >> 5) & 1)
      {

        volatile UINT64 IA32_FEATURE_CONTROL;


        displayline("%d:System check successful. INTEL-VT is supported\n", currentcpuinfo->cpunr);
        sendstring("!!!!!!!!!!!!!!This system supports VMX!!!!!!!!!!!!!!\n\r");

        displayline("Going to call IA32_FEATURE_CONTROL=readMSR(0x3a)\n");
        IA32_FEATURE_CONTROL=readMSR(IA32_FEATURE_CONTROL_MSR);
        displayline("IA32_FEATURE_CONTROL=%6\n\r",IA32_FEATURE_CONTROL);


        if (IA32_FEATURE_CONTROL & FEATURE_CONTROL_LOCK)
        {
          displayline("IA32_FEATURE_CONTROL is locked (value=%6). (Disabled in bios?)\n\r",IA32_FEATURE_CONTROL);
          if (!(IA32_FEATURE_CONTROL & FEATURE_CONTROL_VMXON ))
          {
            displayline("Bit 2 (VMX) is also disabled. VMX is not possible. Reboot!\n");
            return;
          }
          else
            sendstring("VMXON was already enabled in the feature control MSR\n");
        }
        else
        {
          sendstring("Not locked yet\n");

          IA32_FEATURE_CONTROL=IA32_FEATURE_CONTROL | FEATURE_CONTROL_VMXON | FEATURE_CONTROL_LOCK;

          displayline("setting IA32_FEATURE_CONTROL to %6\n\r",IA32_FEATURE_CONTROL);

          writeMSR(IA32_FEATURE_CONTROL_MSR,IA32_FEATURE_CONTROL);
          IA32_FEATURE_CONTROL=readMSR(IA32_FEATURE_CONTROL_MSR);
          displayline("IA32_FEATURE_CONTROL is now %6\n\r",IA32_FEATURE_CONTROL);
        }


        sendstring("Gathering VMX info\n\r");
  			//gather info
        IA32_VMX_BASIC.IA32_VMX_BASIC=readMSR(0x480);

        IA32_VMX_CR0_FIXED0=readMSR(IA32_VMX_CR0_FIXED0_MSR);
        IA32_VMX_CR0_FIXED1=readMSR(IA32_VMX_CR0_FIXED1_MSR);
        IA32_VMX_CR4_FIXED0=readMSR(IA32_VMX_CR4_FIXED0_MSR);
        IA32_VMX_CR4_FIXED1=readMSR(IA32_VMX_CR4_FIXED1_MSR);


        sendstring("Setting CR4\n\r");


  			setCR4(getCR4() | CR4_VMXE);

        if (currentcpuinfo->vmxon_region==NULL)
          currentcpuinfo->vmxon_region=malloc(4096);

        sendstringf("Allocated vmxon_region at %6 (%6)\n\r",(UINT64)currentcpuinfo->vmxon_region,(UINT64)VirtualToPhysical((UINT64)currentcpuinfo->vmxon_region));

        if (currentcpuinfo->vmxon_region==NULL)
        {
          sendstringf(">>>>>>>>>>>>>>>>>>>>vmxon allocation has failed<<<<<<<<<<<<<<<<<<<<<<<<<<\n");
          while (1);
        }

        zeromemory(currentcpuinfo->vmxon_region,4096);
  			*(ULONG *)currentcpuinfo->vmxon_region=IA32_VMX_BASIC.rev_id;

        if (currentcpuinfo->vmcs_region==NULL)
          currentcpuinfo->vmcs_region=malloc(4096);

        sendstringf("Allocated vmcs_region at %6 (%6)\n\r",currentcpuinfo->vmcs_region,VirtualToPhysical((UINT64)currentcpuinfo->vmcs_region));

        if (currentcpuinfo->vmcs_region==NULL)
        {
          sendstringf(">>>>>>>>>>>>>>>>>>>>vmcs_region allocation has failed<<<<<<<<<<<<<<<<<<<<<<<<<<\n");
          while (1);
        }


        zeromemory(currentcpuinfo->vmcs_region,4096);
        *(ULONG *)currentcpuinfo->vmcs_region=IA32_VMX_BASIC.rev_id;

        displayline("revision id=%d\n\r",IA32_VMX_BASIC.rev_id);

        displayline("IA32_FEATURE_CONTROL=%6\n\r",IA32_FEATURE_CONTROL);
        displayline("IA32_VMX_CR0_FIXED0=%6 IA32_VMX_CR0_FIXED1=%6\n\r",IA32_VMX_CR0_FIXED0,IA32_VMX_CR0_FIXED1);
        displayline("IA32_VMX_CR4_FIXED0=%6 IA32_VMX_CR4_FIXED1=%6\n\r",IA32_VMX_CR4_FIXED0,IA32_VMX_CR4_FIXED1);


        displayline("CR0=%6  (Should be %6)\n\r",(UINT64)getCR0(),((UINT64)getCR0() | (UINT64)IA32_VMX_CR0_FIXED0) & (UINT64)IA32_VMX_CR0_FIXED1);
        displayline("CR4=%6  (Should be %6)\n\r",(UINT64)getCR4(),((UINT64)getCR4() | (UINT64)IA32_VMX_CR4_FIXED0) & (UINT64)IA32_VMX_CR4_FIXED1);


        displayline("vmxon_region=%6\n\r",VirtualToPhysical((UINT64)currentcpuinfo->vmxon_region));

        displayline("%d:Checks successfull. Going to call vmxon\n",currentcpuinfo->cpunr);

  		if (vmxon(VirtualToPhysical((UINT64)currentcpuinfo->vmxon_region))==0)
  		{
  		  sendstring("vmxon success\n\r");
          displayline("%d: vmxon success\n",currentcpuinfo->cpunr);

          displayline("%d: calling vmclear\n",currentcpuinfo->cpunr);

          if (vmclear(VirtualToPhysical((UINT64)currentcpuinfo->vmcs_region))==0)
          {
            displayline("%d: calling vmptrld\n",currentcpuinfo->cpunr);

            if (vmptrld(VirtualToPhysical((UINT64)currentcpuinfo->vmcs_region))==0)
            {

              displayline("%d: vmptrld successful. Calling setupVMX\n", currentcpuinfo->cpunr);

              sendstringf("%d: Calling setupVMX with currentcpuinfo %6\n\r",currentcpuinfo->cpunr ,(UINT64)currentcpuinfo);
              setupVMX(currentcpuinfo);

              displayline("%d: Virtual Machine configuration successful. Launching...\n",currentcpuinfo->cpunr);


              /*if (currentcpuinfo->cpunr==0)
                while (1);*/

              if (!isAP)
                clearScreen();



              launchVMX(currentcpuinfo);

              displayline("Exit from launchVMX, if you see this, something horrible has happened\n");
              sendstring("Exit from launchVMX\n\r");

#ifdef DISPLAYDEBUG
              while (1);
#endif

            }
            else
            {
              sendstring("vmptrld failed\n\r");

              displayline("vmptrld failure:");
              displayline(getVMInstructionErrorString());
              displayline(" \n");


            }
          }
          else
          {
            sendstring("vmclear failed\n\r");


            displayline("vmclear failure:");
            displayline(getVMInstructionErrorString());
            displayline(" \n");

          }
          sendstringf("Calling vmxoff() (rsp=%x)\n\r", getRSP());

  				vmxoff();
  				sendstring("still here so vmxoff succeeded\n\r");

  			}
        else
        {
          displayline("vmxon failure\n");
          sendstring("vmxon failed\n\r");
        }



      }
      else
      {
        displayline("Fatal error: Your system does not support intel-VT!!!!\n");
        displayline("Remove the disk, reboot, and go look for a better cpu\n");
        sendstring("!!!!!!!!!!!!!!Your system is crap, it does NOT support VMX!!!!!!!!!!!!!!\n\r");
        sendstringf("cpuid(1):\n");
        sendstringf("EAX=%8\n", a);
        sendstringf("EBX=%8\n", b);
        sendstringf("ECX=%8\n", c);
        sendstringf("EDX=%8\n", d);

      }
    }


  }
  else
  {
    sendstring("This fucking retarded system doesn'\t even have cpuid. Should I fry it ?\n\r");
    displayline("Your system doesn\'t even support CPUID, therefore it probably won\'t support Virtualization either\n");
  }

#ifdef DEBUG
  sendstringf("End of startvmx (entryrsp=%6, returnrsp=%6)\n\r",entryrsp,getRSP());
#endif

  if (currentcpuinfo->cpunr==0)
    displayline("bye...\n");

#ifdef DISPLAYDEBUG
  while (1);
#endif

}
