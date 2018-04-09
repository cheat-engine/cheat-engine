#include "vmevent_invalidstate.h"
#include "realmodeemu.h"
#include "pmodeemu.h"
#include "vmreadwrite.h"
#include "mm.h"

void fixInterruptabilityState(void)
{
  DWORD is=vmread(vm_guest_interruptability_state);
  RFLAGS guestrflags;
  VMEntry_interruption_information entryinterrupt;
  guestrflags.value=vmread(vm_guest_rflags);


  is=is & 0x1f; //remove reserved bits

  if (guestrflags.IF==0) //block by sti may not be enabled when IF is 0
    is=is & 0x1e; //disable block by sti



  if ((is & 3)==3)
  {
    //both block by STI and block by mov ss are enabled
    is=is & 0x1e; //disable block by sti
  }

  entryinterrupt.interruption_information=vmread(vm_entry_interruptioninfo);
  if (entryinterrupt.valid)
  {
    if (entryinterrupt.type==0) //external interrupt entry must have the both sti and ss to 0
      is = is & 0x1c;

    if (entryinterrupt.type==2) //nmi
      is = is & 0x1d; //disable blick by ss

  }


  vmwrite(vm_guest_interruptability_state, is);
}

int handleInvalidEntryState(pcpuinfo currentcpuinfo,VMRegisters *vmregisters)
{

  sendstring("Handling invalid entry state\n\r");
  //fix interruptability state (common bug when emulating and fixing it won't cause a problem)

  fixInterruptabilityState();



  if (ISREALMODE(currentcpuinfo))
  {
    int result;
    sendstring("Inside realmode. Trying to emulate instructions\n\r");
    result=emulateRealMode(currentcpuinfo, vmregisters)==0;

    sendstringf("emulateRealMode(...) returned %d\n\r",result);

    if (result==0)
    {
      sendstring("emulation was handled properly\n");
      return 0; //handled at least one instruction
    }
    else
      sendstring("emulation was a total failure. Not one instruction got emulated. Trying to fix the state");




    //emulateRealMode failed
    if (ISREALMODE(currentcpuinfo)) //still realmode ? (most likely, but possible it isn't anymore once enough has been emulated)
    {
      Access_Rights reg_rmaccessrights,reg_traccessrights;
      RFLAGS guestrflags;
      DWORD gdtbase, idtbase;
      guestrflags.value=0;

      gdtbase=VirtualToPhysical((void *)getGDTbase());
      idtbase=VirtualToPhysical(idttable32);

      sendstring("Still in realmode, enabling VMx86 mode if not already in it and fixing possible other bugs\n\r");
      //set taskregister to realmode tr
      //set GDT and IDT to my own (so a 32-bit interrupt is possible)
      vmwrite(vm_guest_gdtr_base, gdtbase);
      vmwrite(vm_guest_gdt_limit, getGDTsize());
      vmwrite(vm_guest_idtr_base, idtbase);
      vmwrite(vm_guest_idt_limit, 256*8);
      setupTSS8086();
      vmwrite(vm_guest_tr_base,(UINT64)VirtualToPhysical(VirtualMachineTSS_V8086)); //tr base
      vmwrite(vm_guest_tr_limit,(ULONG)sizeof(TSS)+32+8192+1); //tr limit
      vmwrite(vm_guest_tr,64); //the tss o

#ifdef DEBUG
      UINT64 idtbase2, gdtbase2;
      gdtbase2=vmread(vm_guest_gdtr_base);
      idtbase2=vmread(vm_guest_idtr_base);

      sendstringf("Set vm_guest_gdtr_base to %6 while I wanted to set it to %6\n\r",gdtbase2, gdtbase);
      sendstringf("Set vm_guest_idtr_base to %6 while I wanted to set it to %6\n\r",idtbase2, idtbase);
#endif


      reg_rmaccessrights.AccessRights=0;
      reg_rmaccessrights.Segment_type=3;
      reg_rmaccessrights.S=1;
      reg_rmaccessrights.DPL=3;
      reg_rmaccessrights.P=1;
      reg_rmaccessrights.G=0;
      reg_rmaccessrights.D_B=0;

      reg_traccessrights.AccessRights=0;
      reg_traccessrights.Segment_type=11; //11=32-bit 3=16-bit
      reg_traccessrights.S=0;
      reg_traccessrights.DPL=0;
      reg_traccessrights.P=1;
      reg_traccessrights.G=0;
      reg_traccessrights.D_B=1;
      vmwrite(vm_guest_es_access_rights,(ULONG)reg_rmaccessrights.AccessRights); //es access rights
      vmwrite(vm_guest_cs_access_rights,(ULONG)reg_rmaccessrights.AccessRights); //cs access rights
      vmwrite(vm_guest_ss_access_rights,(ULONG)reg_rmaccessrights.AccessRights); //ss access rights
      vmwrite(vm_guest_ds_access_rights,(ULONG)reg_rmaccessrights.AccessRights); //ds access rights
      vmwrite(vm_guest_fs_access_rights,(ULONG)reg_rmaccessrights.AccessRights); //fs access rights
      vmwrite(vm_guest_gs_access_rights,(ULONG)reg_rmaccessrights.AccessRights); //gs access rights
      vmwrite(vm_guest_ldtr_access_rights,(ULONG)(1<<16)); //ldtr access rights (bit 16 is unusable bit
      vmwrite(vm_guest_tr_access_rights,(ULONG)reg_traccessrights.AccessRights); //tr access rights

      vmwrite(vm_guest_es,(ULONG)vmread(vm_guest_es_base) >> 4); //es selector
      vmwrite(vm_guest_cs,(ULONG)vmread(vm_guest_cs_base) >> 4); //cs selector
      vmwrite(vm_guest_ss,(ULONG)vmread(vm_guest_ss_base) >> 4); //ss selector
      vmwrite(vm_guest_ds,(ULONG)vmread(vm_guest_ds_base) >> 4); //ds selector
      vmwrite(vm_guest_fs,(ULONG)vmread(vm_guest_fs_base) >> 4); //fs selector
      vmwrite(vm_guest_gs,(ULONG)vmread(vm_guest_gs_base) >> 4); //gs selector
      vmwrite(vm_guest_ldtr,(ULONG)0); //ldtr selector
      vmwrite(vm_guest_tr,(ULONG)0); //tr selector

      vmwrite(vm_guest_es_limit,(ULONG)0xffff); //es limit
      vmwrite(vm_guest_cs_limit,(ULONG)0xffff); //cs limit
      vmwrite(vm_guest_ss_limit,(ULONG)0xffff); //ss limit
      vmwrite(vm_guest_ds_limit,(ULONG)0xffff); //ds limit
      vmwrite(vm_guest_fs_limit,(ULONG)0xffff); //fs limit
      vmwrite(vm_guest_gs_limit,(ULONG)0xffff); //gs limit


      //make sure VM flag is set appropriatly for vm mode
      guestrflags.value=vmread(vm_guest_rflags);
      guestrflags.IOPL=3;
      guestrflags.VM=1;
//      pguesteflags->v
      //currentcpuinfo->hasIF=pguesteflags->IF;
      vmwrite(vm_guest_rflags,guestrflags.value);

      return 0;
    }
  }
  else
  {
    WORD segment,segment2;
    Access_Rights tempAR;
    int handled=0;



    sendstring("Not in realmode and in an invalid entry state\n");
    sendstring("Emulating instruction\n");
    handled=emulateProtectedMode(currentcpuinfo, vmregisters);

    if (handled)
      return 0; //no error


    sendstring("Emulation failed, trying to force state to be valid\n");
    //try to figure out what could be the problem
    //segments must be ACCESSED (damn bochs)
    tempAR.AccessRights=vmread(vm_guest_cs_access_rights);
    if ((tempAR.Segment_type & 1)==0)
    {
      sendstringf("Code segment was usable but not accessed (Let me guess, it's Bochs?)\n");
      tempAR.Segment_type=tempAR.Segment_type | 1;
      vmwrite(vm_guest_cs_access_rights, tempAR.AccessRights);
      handled=1;
    }

    tempAR.AccessRights=vmread(vm_guest_ds_access_rights);
    if ((tempAR.unusable==0) && ((tempAR.Segment_type & 1)==0))
    {
      sendstringf("DS segment was usable but not accessed\n");
      tempAR.Segment_type=tempAR.Segment_type | 1;
      vmwrite(vm_guest_ds_access_rights, tempAR.AccessRights);
      handled=1;
    }

    tempAR.AccessRights=vmread(vm_guest_es_access_rights);
    if ((tempAR.unusable==0) && ((tempAR.Segment_type & 1)==0))
    {
      sendstringf("ES segment was usable but not accessed\n");
      tempAR.Segment_type=tempAR.Segment_type | 1;
      vmwrite(vm_guest_es_access_rights, tempAR.AccessRights);
      handled=1;
    }

    tempAR.AccessRights=vmread(vm_guest_fs_access_rights);
    if ((tempAR.unusable==0) && ((tempAR.Segment_type & 1)==0))
    {
      sendstringf("FS segment was usable but not accessed\n");
      tempAR.Segment_type=tempAR.Segment_type | 1;
      vmwrite(vm_guest_fs_access_rights, tempAR.AccessRights);
      handled=1;
    }

    tempAR.AccessRights=vmread(vm_guest_gs_access_rights);
    if ((tempAR.unusable==0) && ((tempAR.Segment_type & 1)==0))
    {
      sendstringf("GS segment was usable but not accessed\n");
      tempAR.Segment_type=tempAR.Segment_type | 1;
      vmwrite(vm_guest_gs_access_rights, tempAR.AccessRights);
      handled=1;
    }

    /*
    //check if the current cs.rpl = cs.dpl
    tempAR.AccessRights=vmread(vm_guest_cs_access_rights);
    segment=vmread(vm_guest_cs);
    if ( (segment & 3) != tempAR.DPL )
    {
      sendstringf("CS: %x is an invalid value for a dpl of %d\n",segment,tempAR.DPL);
      segment=(segment & 0xfffc) | tempAR.DPL;
      vmwrite(vm_guest_cs,segment);
      handled=1;
    }

    tempAR.AccessRights=vmread(vm_guest_ds_access_rights);
    segment=vmread(vm_guest_ds);
    if ( (segment & 3) != tempAR.DPL )
    {
      sendstringf("DS: %x is an invalid value for a dpl of %d\n",segment,tempAR.DPL);
      segment=(segment & 0xfffc) | tempAR.DPL;
      vmwrite(vm_guest_ds,segment);
      handled=1;
    }

    tempAR.AccessRights=vmread(vm_guest_es_access_rights);
    segment=vmread(vm_guest_es);
    if ( (segment & 3) != tempAR.DPL )
    {
      sendstringf("ES: %x is an invalid value for a dpl of %d\n",segment,tempAR.DPL);
      segment=(segment & 0xfffc) | tempAR.DPL;
      vmwrite(vm_guest_es,segment);
      handled=1;
    }

    tempAR.AccessRights=vmread(vm_guest_fs_access_rights);
    segment=vmread(vm_guest_fs);
    if ( (segment & 3) != tempAR.DPL )
    {
      sendstringf("FS: %x is an invalid value for a dpl of %d\n",segment,tempAR.DPL);
      segment=(segment & 0xfffc) | tempAR.DPL;
      vmwrite(vm_guest_fs,segment);
      handled=1;
    }

    tempAR.AccessRights=vmread(vm_guest_gs_access_rights);
    segment=vmread(vm_guest_gs);
    if ( (segment & 3) != tempAR.DPL )
    {
      sendstringf("GS: %x is an invalid value for a dpl of %d\n",segment,tempAR.DPL);
      segment=(segment & 0xfffc) | tempAR.DPL;
      vmwrite(vm_guest_gs,segment);
      handled=1;
    }
    */

    //check if SS.rpl == CS.rpl
    segment=vmread(vm_guest_ss);
    segment2=vmread(vm_guest_cs);

    if ((segment & 3) != (segment2 & 3))
    {
      sendstringf("SS(%x).rpl != CS(%x).rpl\n",segment,segment2);
      segment=(segment & 0xfffc) | (segment2 & 3);
      sendstringf("Setting SS to %x\n",segment);
      vmwrite(vm_guest_ss, segment);

      //also set ss.dpl so ss.rpl
      tempAR.AccessRights=vmread(vm_guest_ss_access_rights);
      tempAR.DPL=(segment & 3);
      vmwrite(vm_guest_ss_access_rights, tempAR.AccessRights);
      handled=1;
    }

    //check if CR0 and CR4 have bits set or unset that are not allowed
    UINT64 oldcr0,newcr0;
    UINT64 oldcr4,newcr4;

    oldcr0=vmread(vm_guest_cr0);
    oldcr4=vmread(vm_guest_cr4);

    newcr0=(oldcr0 | (UINT64)IA32_VMX_CR0_FIXED0) & (UINT64)IA32_VMX_CR0_FIXED1;

    if (newcr0!=oldcr0)
    {
      sendstringf("old CR0 was %6 new CR0 is %6\n",oldcr0,newcr0);

      vmwrite(vm_guest_cr0, newcr0);
      handled=1;
    }

    newcr4=(oldcr4 | (UINT64)IA32_VMX_CR4_FIXED0) & (UINT64)IA32_VMX_CR4_FIXED1;
    if (newcr4!=oldcr4)
    {
      sendstringf("old CR4 was %6 new CR4 is %6\n",oldcr4,newcr4);
      vmwrite(vm_guest_cr4, newcr4);
      handled=1;
    }

    //setCR4(((UINT64)getCR4() | (UINT64)IA32_VMX_CR4_FIXED0) & (UINT64)IA32_VMX_CR4_FIXED1);




    if (handled==1)
    {
      //set RF flag
      UINT64 rflags=vmread(vm_guest_rflags);
      PRFLAGS prflags=(PRFLAGS)&rflags;
      prflags->RF=1;
      vmwrite(vm_guest_rflags,rflags);
    }
    //make sure



    return (handled==0);

  }
  return 0;
}

