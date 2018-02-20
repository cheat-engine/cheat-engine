/*
 * pmodeemu.c
 *
 *  Created on: Jul 13, 2009
 *      Author: erich
 */
#include "common.h"
#include "main.h"
#include "realmodeemu.h"
#include "vmmhelper.h"
#include "vmpaging.h"
#include "vmreadwrite.h"
#include "distorm.h"

#include "vmeventhandler.h"

int emulateProtectedMode(pcpuinfo currentcpuinfo, VMRegisters *vmregisters)
{
  int result=0; //not emulated yet

  if (currentcpuinfo==NULL)
    return 0;

  if (vmregisters==NULL)
    return 0;

  //address=(vmread(vm_guest_cs_base))+vmread(vm_guest_rip);

  //to emulate:
  //20262: xchg bx,bx

  //ea ea4c 2000 - JMP FAR 0x20:0x4cea

  return result;
}
