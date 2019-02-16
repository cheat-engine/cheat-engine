/*
 * exports.h
 *
 *  Created on: Apr 9, 2018
 *      Author: root
 */

#ifndef VMM_EXPORTS_H_
#define VMM_EXPORTS_H_


#define DBVM_EXPORTLISTVERSION 1;
typedef struct
{
  int version;
  void *getCPUCount;
  void *getcpunr;
  void *sendstringf;
  void *zeromemory;
  void *memcpy;
  void *memcmp;
  void *csEnter;
  void *csLeave;
  void *malloc;
  void *free;
  void *mapVMMemory;
  void *mapPhysicalMemory;
  void *unmapMemory;
  void *VirtualToPhysical;
  void *setMSRReadExit;
  void *removeMSRReadExit;
  void *setMSRWriteExit;
  void *removeMSRWriteExit;


} DBVMExports, *PDBVMExports;

extern PDBVMExports exportlist; //defined in vmma.asm

void InitExports();

#endif /* VMM_EXPORTS_H_ */
