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

#define EPTO_MULTIPLERIP    (1<<0) //log the same RIP multiple times (if different registers)
#define EPTO_LOG_ALL        (1<<1) //log every access in the page
#define EPTO_SAVE_XSAVE     (1<<2) //logs contain the xsave state
#define EPTO_SAVE_STACK     (1<<3) //logs contain a 4kb stack snapshot
#define EPTO_PMI_WHENFULL   (1<<4) //triggers a PMI when full
#define EPTO_GROW_WHENFULL  (1<<5) //grows the buffer when full
#define EPTO_INTERRUPT      (1<<6) //raise interrupt on match instead of log

#define EPTW_WRITE 0
#define EPTW_READWRITE 1
#define EPTW_EXECUTE 2


void initMemTypeRanges();

VMSTATUS handleEPTViolation(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, PFXSAVE64 fxsave);
VMSTATUS handleEPTMisconfig(pcpuinfo currentcpuinfo, VMRegisters *vmregisters);
int ept_handleWatchEventAfterStep(pcpuinfo currentcpuinfo, int ID);
int ept_handleCloakEventAfterStep(pcpuinfo currentcpuinfo, PCloakedPageData cloakdata);
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
BOOL ept_handleSoftwareBreakpoint(pcpuinfo currentcpuinfo, VMRegisters *vmregisters);

void ept_reset();
void ept_invalidate();
void vpid_invalidate();

#endif /* VMM_EPTHANDLER_H_ */
