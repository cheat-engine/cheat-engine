/*
 * vmxemu.h
 *
 *  Created on: Jan 15, 2018
 *      Author: erich
 */

#ifndef VMM_VMXEMU_H_
#define VMM_VMXEMU_H_

#include "vmmhelper.h"

typedef struct _instruction_info_vm
{
  union{
    unsigned long instructioninfo;
    struct {
      unsigned scaling    :2; //0-1 scaling
      unsigned reserved1  :1; //2
      unsigned reg1       :4; //3-6  //invalid for INVEPT, INVPCID, and INVVPID
      unsigned addressSize:3; //7-9
      unsigned usesreg    :1; //10   //0 for INVEPT, INVPCID, and INVVPID
      unsigned undefined  :4; //11-14
      unsigned segmentReg :3; //15-17
      unsigned indexReg   :4; //18-21
      unsigned indexRegInvalid :1; //22
      unsigned baseReg    :4; //23-26
      unsigned baseRegInvalid :1; //27
      unsigned reg2       :4; //28-31
    };
  };
} __attribute__((__packed__)) instruction_info_vm,*pinstruction_info_vm;



int handleIntelVMXInstruction(pcpuinfo currentcpuinfo, VMRegisters *vmregisters);
int handleByGuest(pcpuinfo currentcpuinfo, VMRegisters *vmregisters);

extern int emulatevmx;

#endif /* VMM_VMXEMU_H_ */
