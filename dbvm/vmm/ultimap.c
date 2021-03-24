/*
 * ultimap.c
 *
 *  Created on: May 28, 2011
 *      Author: erich
 */

#include "common.h"
#include "vmreadwrite.h"
#include "ultimap.h"
#include "msrnames.h"
#include "vmmhelper.h"
#include "main.h"
#include "vmxsetup.h"

#ifdef ULTIMAPDEBUG



void ultimap_debugoutput(pcpuinfo currentcpuinfo, PULTIMAPDEBUGINFO UltimapDebugInfo)
{

//	int error;
	//UINT64 pagefaultaddress;

	//if (!error)
	{
		UltimapDebugInfo->Active=currentcpuinfo->Ultimap.Active;
		UltimapDebugInfo->CR3=currentcpuinfo->Ultimap.CR3;
		UltimapDebugInfo->DEBUGCTL=currentcpuinfo->Ultimap.DEBUGCTL;
		UltimapDebugInfo->DS_AREA=currentcpuinfo->Ultimap.DS_AREA;
		UltimapDebugInfo->OriginalDebugCTL=currentcpuinfo->Ultimap.OriginalDebugCTL;
		UltimapDebugInfo->OriginalDS_AREA=currentcpuinfo->Ultimap.OriginalDS_AREA;
		UltimapDebugInfo->CR3_switchcount=currentcpuinfo->Ultimap.CR3_switchcount;
		UltimapDebugInfo->CR3_switchcount2=currentcpuinfo->Ultimap.CR3_switchcount2;
		UltimapDebugInfo->LastOldCR3=currentcpuinfo->Ultimap.LastOldCR3;
		UltimapDebugInfo->LastNewCR3=currentcpuinfo->Ultimap.LastNewCR3;
		UltimapDebugInfo->CpuNr=currentcpuinfo->cpunr;
	}

}
#endif

void ultimap_pause(pcpuinfo currentcpuinfo)
{
  ultimap_disable(currentcpuinfo);
}

void ultimap_resume(pcpuinfo currentcpuinfo)
{
  currentcpuinfo->Ultimap.Active=1;
}

void ultimap_disable(pcpuinfo currentcpuinfo)
{
  if (currentcpuinfo->Ultimap.Active)
  {
    vmwrite(vm_guest_IA32_DEBUGCTL, currentcpuinfo->Ultimap.OriginalDebugCTL);
    writeMSR(IA32_DS_AREA, currentcpuinfo->Ultimap.OriginalDS_AREA);

    currentcpuinfo->Ultimap.Active=0;
  }

  if (canToggleCR3Exit)
    vmwrite(vm_execution_controls_cpu, vmread(vm_execution_controls_cpu) | PPBEF_CR3LOAD_EXITING | PPBEF_CR3STORE_EXITING);
}

void ultimap_setup(pcpuinfo currentcpuinfo, QWORD CR3, QWORD DEBUGCTL, QWORD DS_AREA)
{
  if (canToggleCR3Exit)
    vmwrite(vm_execution_controls_cpu, vmread(vm_execution_controls_cpu) | PPBEF_CR3LOAD_EXITING | PPBEF_CR3STORE_EXITING);

  currentcpuinfo->Ultimap.CR3=CR3;
  currentcpuinfo->Ultimap.DEBUGCTL=DEBUGCTL;
  currentcpuinfo->Ultimap.DS_AREA=DS_AREA;
  currentcpuinfo->Ultimap.Active=1;
  currentcpuinfo->Ultimap.OriginalDebugCTL=vmread(vm_guest_IA32_DEBUGCTL);
  currentcpuinfo->Ultimap.OriginalDS_AREA=readMSR(IA32_DS_AREA);
}

void ultimap_handleCR3Change(pcpuinfo currentcpuinfo, QWORD oldcr3, QWORD newcr3)
/*
 * Called when cr3 changes and ultimap is active
 */
{


	currentcpuinfo->Ultimap.CR3_switchcount++;

    if (oldcr3 != newcr3)
    {
      if (currentcpuinfo->Ultimap.CR3==newcr3) //if the new cr3 is the process to watch
      {
    	currentcpuinfo->Ultimap.CR3_switchcount2++;

    	currentcpuinfo->Ultimap.LastOldCR3=oldcr3;
    	currentcpuinfo->Ultimap.LastNewCR3=newcr3;



        //set the MSR values
        currentcpuinfo->Ultimap.OriginalDebugCTL=vmread(vm_guest_IA32_DEBUGCTL);
        currentcpuinfo->Ultimap.OriginalDS_AREA=readMSR(IA32_DS_AREA);

        vmwrite(vm_guest_IA32_DEBUGCTL, currentcpuinfo->Ultimap.DEBUGCTL);
        writeMSR(IA32_DS_AREA, currentcpuinfo->Ultimap.DS_AREA);


        //and register a vm-exit event on MSR read/write for DEBUGCTL and DS_AREA
        MSRBitmap[IA32_DS_AREA/8]|=1 << (IA32_DS_AREA % 8);
        MSRBitmap[1024+IA32_DS_AREA/8]|=1 << (IA32_DS_AREA % 8);

        MSRBitmap[IA32_DEBUGCTL_MSR/8]|=1 << (IA32_DEBUGCTL_MSR % 8);
        MSRBitmap[1024+IA32_DEBUGCTL_MSR/8]|=1 << (IA32_DEBUGCTL_MSR % 8);


      }
      else
      if (currentcpuinfo->Ultimap.CR3==currentcpuinfo->guestCR3) //if the old cr3 is the process to watch and is switched out to a different one
      {
        //unset the MSR values
        vmwrite(vm_guest_IA32_DEBUGCTL, currentcpuinfo->Ultimap.OriginalDebugCTL);
        writeMSR(IA32_DS_AREA, currentcpuinfo->Ultimap.OriginalDS_AREA);

        //and unregister the vm-exit event on MSR read/write for DEBUGCTL and DS_AREA
        MSRBitmap[IA32_DS_AREA/8]&=~(1 << (IA32_DS_AREA % 8));
        MSRBitmap[1024+IA32_DS_AREA/8]&=~(1 << (IA32_DS_AREA % 8));

        MSRBitmap[IA32_DEBUGCTL_MSR/8]&=~(1 << (IA32_DEBUGCTL_MSR % 8));
        MSRBitmap[1024+IA32_DEBUGCTL_MSR/8]&=~(1 << (IA32_DEBUGCTL_MSR % 8));
      }
    }

}

void ultimap_handleDB(pcpuinfo currentcpuinfo)
/*
 * Called when an int1 happens
 */
{
  currentcpuinfo->Ultimap.OriginalDebugCTL&=~1; //disable the LBR flag on bp

  if (currentcpuinfo->guestCR3 != currentcpuinfo->Ultimap.CR3)
    vmwrite(vm_guest_IA32_DEBUGCTL, vmread(vm_guest_IA32_DEBUGCTL) & ~1);
  else
    vmwrite(vm_guest_IA32_DEBUGCTL, currentcpuinfo->Ultimap.DEBUGCTL); //just write it again
}

void ultimap_handleMSRWrite(pcpuinfo currentcpuinfo, DWORD msr, QWORD value)
{

  switch (msr)
  {
    case IA32_DEBUGCTL_MSR:
    {
      currentcpuinfo->Ultimap.OriginalDebugCTL=value;

      if ((currentcpuinfo->Ultimap.Active) && (currentcpuinfo->Ultimap.CR3==currentcpuinfo->guestCR3))
        vmwrite(vm_guest_IA32_DEBUGCTL, currentcpuinfo->Ultimap.DEBUGCTL);
      else
        vmwrite(vm_guest_IA32_DEBUGCTL, value);

      break;
    }

    case IA32_DS_AREA:
    {
      currentcpuinfo->Ultimap.OriginalDS_AREA=value;

      if ((currentcpuinfo->Ultimap.Active) && (currentcpuinfo->Ultimap.CR3==currentcpuinfo->guestCR3))
        writeMSR(IA32_DS_AREA, currentcpuinfo->Ultimap.DS_AREA);
      else
        writeMSR(IA32_DS_AREA, value);

      break;
    }
  }


}

QWORD ultimap_handleMSRRead(pcpuinfo currentcpuinfo, DWORD msr)
{
  switch (msr)
  {
    case IA32_DEBUGCTL_MSR:
    {
      if ((currentcpuinfo->Ultimap.Active) && (currentcpuinfo->Ultimap.CR3==currentcpuinfo->guestCR3))
        return currentcpuinfo->Ultimap.OriginalDebugCTL;
      else
        return vmread(vm_guest_IA32_DEBUGCTL);
    }

    case IA32_DS_AREA:
    {
      if ((currentcpuinfo->Ultimap.Active) && (currentcpuinfo->Ultimap.CR3==currentcpuinfo->guestCR3))
        return currentcpuinfo->Ultimap.OriginalDS_AREA;
      else
        return readMSR(IA32_DS_AREA);
    }
  }

  return 0;
}
