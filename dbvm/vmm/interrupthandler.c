/*
 * interrupthandler.c
 *
 *  Created on: May 10, 2020
 *      Author: eric
 */

#include "interrupthandler.h"
#include "distorm.h"
#include "main.h"
#include "inthandlers.h"
#include "displaydebug.h"
#include "msrnames.h"

PINT_VECTOR intvector=NULL;

#ifdef DEBUGINTHANDLER
criticalSection cinthandlerMenuCS={.name="cinthandlerMenuCS", .debuglevel=2};
#endif

int IntHandlerDebug=0;
int ClearDR6OnInterrupt=0;


#define SETINT(INTNR) intvector[INTNR].wLowOffset=(WORD)(UINT64)inthandler##INTNR; \
                      intvector[INTNR].wHighOffset=(WORD)((UINT64)inthandler##INTNR >> 16);


void setints(void)
{
  IDT tidt;
  int i;

  tidt.wLimit=16*256;
  tidt.vector=intvector;

  if (intvector==NULL)
  {
    sendstring("setints was called too early");
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while(1);
  }


  zeromemory(intvector, 256*sizeof(INT_VECTOR));
  for (i=0; i<256; i++)
  {
    intvector[i].wSelector=80;
    intvector[i].bUnused=0;
    intvector[i].bAccess=0x8e; //10001110
  }

  intvector[8].bUnused=1; //double fault
  intvector[11].bUnused=1; //segment fault
  intvector[12].bUnused=1; //stack fault

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



int cinthandler(unsigned long long *stack, int intnr) //todo: move to it's own sourcefile
{
  PRFLAGS rflags;
  int errorcode=0;
  UINT64 errorcodeValue;
  int i;
  DWORD thisAPICID;
  int cpunr=0;

  thisAPICID=getAPICID();
  nosendchar[getAPICID()]=0;

  enableserial();

  emergencyOutputOnly=0;

  sendstring("\n------------------------------------------\n");
  sendstringf("|             EXCEPTION %d               |\n", intnr);
  sendstring("------------------------------------------\n");

  ddDrawRectangle(DDHorizontalResolution-100,0,100,100,_rdtsc());

  if (readMSR(IA32_FS_BASE_MSR)==0)
  {

#ifdef DEBUG
  sendstringCS.ignorelock=1;
  sendstringfCS.ignorelock=1;
#endif


    sendstringf("Invalid FS base during exception %d  CR2=%6!!\n",intnr, getCR2());


    if (intnr==13)
    {
      UINT64 RIP=stack[17];
      errorcodeValue=stack[16];

      sendstringf("RIP=%6\n", RIP);
      sendstringf("ErrorCode=%x\n", errorcodeValue);

    }

    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1) outportb(0x80,0xc5);
  }

  pcpuinfo cpuinfo=getcpuinfo();
  cpunr=cpuinfo->cpunr;

  //debug, remove:
  //if PIC_StillEnabled
  //outportb(0x20,0x20);
  //outportb(0xa0,0x20);


  //apic_eoi();
  //^^^


  UINT64 originalDR7=getDR7();

  if (intnr==1)
  {
    //disable breakpoints
    setDR7(0ULL);
  }



#ifdef CHECKAPICID
  if (thisAPICID!=cpuinfo->apicid)
  {
    sendstringCS.ignorelock=1;
    sendstringfCS.ignorelock=1;
    sendstringf("Interrupt %d. Invalid cpuinfo", intnr);
    while(1);
  }
#endif





 // sendstringf("interrupt fired : %d (%x)\n\r", intnr,intnr);

  sendstringf("cpunr=%d (apicid=%d)\n\r",cpunr, thisAPICID);
  sendstringf("intnr=%d\n\r",intnr);
  sendstringf("rsp=%6\n\r",getRSP());
  sendstringf("cr2=%6\n\r",getCR2());
  errorcode=0;


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

  sendstringf("rip=%x\n\r",stack[16+errorcode]);
  sendstringf("rflags=%x\n\r",stack[16+2+errorcode]);


  rflags=(PRFLAGS)&stack[16+2+errorcode];


  if ((intnr==2) && (rflags->IF==0))
  {
    cpuinfo->NMIOccured=1;
    NMIcount++;

    cpuinfo->NMIOccured=2;
    /*

    //set up NMI window exiting

    if (vmx_enableNMIWindowExiting()==0) //todo: test this code. I think it enters an invalid state
    {
      sendstringf("NMI handling: failed to set PBEF_NMI_WINDOW_EXITING.  Raising NMI like a retard\n");
      cpuinfo->NMIOccured=2;
    }
    */
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);

    return 0;
  }

  sendstringf("Checking if it was an expected interrupt\n\r");
  cpuinfo->LastExceptionRIP=stack[16+errorcode];
  cpuinfo->LastInterrupt=(unsigned char)intnr;

  if (cpuinfo->OnException[0].RIP)
  {
    nosendchar[thisAPICID]=0;
    sendstringf("OnException is set. Passing it to longjmp\n");  //no need to set rflags back, the original state contains that info

    longjmp(cpuinfo->OnException, 0x100 | intnr);

    sendstringf("longjmp just went through...\n");
    ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);
    while (1) outportb(0x80,0xc6);
  }

  if (cpuinfo->OnInterrupt.RIP)
  {
    QWORD oldrip=stack[16+errorcode];
    sendstringf("Yes, OnInterrupt is set to %x\n\r",cpuinfo->OnInterrupt);

    stack[8+errorcode]=(QWORD)(cpuinfo->OnInterrupt.RBP);
    stack[16+errorcode]=(QWORD)(cpuinfo->OnInterrupt.RIP);
    stack[19+errorcode]=(QWORD)(cpuinfo->OnInterrupt.RSP);

    rflags->IF=0; //disable the IF flag in the eflags register stored on the stack (when called during checks if an int was pending)


    cpuinfo->LastInterrupt=(unsigned char)intnr;
    if (errorcode)
    {
      cpuinfo->LastInterruptHasErrorcode=1;
      cpuinfo->LastInterruptErrorcode=(WORD)stack[16];
    }
    cpuinfo->LastInterruptHasErrorcode=0;

    cpuinfo->OnInterrupt.RIP=0; //clear exception handler
    cpuinfo->OnInterrupt.RBP=0;
    cpuinfo->OnInterrupt.RSP=0;

    sendstringf("changed rip(was %6 is now %6)\n\r", oldrip, stack[16+errorcode]);
    sendstringf("rflags upon return is %x\n\r", stack[16+2+errorcode]);

    sendstring("returning now\n");

    return errorcode;
  }
  sendstring("not expected\n\r");

  ddDrawRectangle(0,DDVerticalResolution-100,100,100,0xff0000);



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


  if ((intnr==1) && (ClearDR6OnInterrupt))
    setDR6(0xffff0ff0);


#ifdef DEBUGINTHANDLER

  csEnter(&cinthandlerMenuCS);
 // inthandleroverride=1;
  IntHandlerDebug=1;


  unsigned char key;
  while (1)
  {
    sendstring("----------------------------\n\r");
    sendstring("Interrupt handler debug menu\n\r");
    sendstring("----------------------------\n\r");
    sendstring("1: Exit from interrupt\n\r");
    sendstring("2: Check CRC values\n\r");
    sendstring("3: Get vmstate\n\r");
    sendstring("4: Main menu\n\r");

    sendstring("p: Previous vmstates\n\r");



    key=waitforchar();
    if (key==0xff) //serial port borked
      key='1';

    switch (key)
    {
      case '1':
        sendstring("Exiting from interrupt\n\r");
        IntHandlerDebug=0;
        csLeave(&cinthandlerMenuCS);
        return errorcode;

      case '2':
        CheckCRCValues();
        break;

      case '3':
        sendvmstate(cpuinfo, NULL);
        break;

      case '4':
        menu();
        break;

      case 'p':
      displayPreviousStates();
        break;
    }
  }



#else
  return errorcode;
#endif
}
