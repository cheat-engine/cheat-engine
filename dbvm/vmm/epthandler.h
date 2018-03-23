/*
 * epthandler.h
 *
 *  Created on: Feb 2, 2018
 *      Author: erich
 */



#ifndef VMM_EPTHANDLER_H_
#define VMM_EPTHANDLER_H_

#include "vmmhelper.h"
#include "eptstructs.h"


#define MTC_UC  0
#define MTC_WC  1
#define MTC_WT  3
#define MTC_WP  5
#define MTC_WB  6


void initMemTypeRanges();

VMSTATUS handleEPTViolation(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, PFXSAVE64 fxsave);
VMSTATUS handleEPTMisconfig(pcpuinfo currentcpuinfo, VMRegisters *vmregisters);
int ept_handleWatchEventAfterStep(pcpuinfo currentcpuinfo, int ID);
int ept_handleCloakEventAfterStep(pcpuinfo currentcpuinfo, int ID);
int ept_handleSoftwareBreakpointAfterStep(pcpuinfo currentcpuinfo,  int ID);

int ept_watch_activate(QWORD PhysicalAddress, int Size, int Type, DWORD Options, int MaxEntryCount, int *outID);
int ept_watch_deactivate(int ID);
VMSTATUS ept_watch_retrievelog(int ID, QWORD results, DWORD *resultSize, DWORD *offset, QWORD *errorcode);
//int ept_activateWatch(pcpuinfo currentcpuinfo, int ID);



int ept_cloak_activate(QWORD physicalAddress);
int ept_cloak_deactivate(QWORD physicalAddress);
int ept_cloak_readOriginal(pcpuinfo currentcpuinfo, VMRegisters *registers, QWORD physicalAddress, QWORD destination);
int ept_cloak_writeOriginal(pcpuinfo currentcpuinfo, VMRegisters *registers, QWORD physicalAddress, QWORD source);
int ept_cloak_changeregonbp(QWORD physicalAddress, PCHANGEREGONBPINFO changereginfo);
int ept_cloak_removechangeregonbp(QWORD physicalAddress);
int ept_handleSoftwareBreakpoint(pcpuinfo currentcpuinfo);

#endif /* VMM_EPTHANDLER_H_ */
