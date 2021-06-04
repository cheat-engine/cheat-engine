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
#include "list.h"
#include "maps.h"

typedef struct
{
  QWORD physicalAddress;
  QWORD initialID;
  QWORD actualID;
  QWORD rip;
  QWORD data;
  QWORD cacheIssue;
  QWORD skipped; //0=taken

} EPTWatchLogData;


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
#define EPTO_DBVMBP         (1<<7) /*similar to EPTO_INTERRUPT but push RIP onto the stack and change RIP to a designated
                                     loop location(possibly cloaked) specified by the watch initiator and log this thread
                                     as currently broken (the thread will have no communicating with pipes or events. It's
                                     just a thread in a loop)

                                     the loop will have to trigger DBVM exits for DBVM to resume it when the user wishes,
                                     so just an ebfe won't do.

                                     just a single cc (int3) should work.  It will trigger DBVM and might cause some
                                     performance issues, so needs to be evaluated. Otherwise a longer loop might be preferred

                                     Use a change reg on bp if you wish to let the usermode program deal with this
                                   */





#define EPTW_WRITE 0
#define EPTW_READWRITE 1
#define EPTW_EXECUTE 2

extern PAddressList CloakedPagesList;
extern PMapInfo CloakedPagesMap;

extern EPTWatchLogData lastSeenEPTWatch;
extern EPTWatchLogData lastSeenEPTWatchVerySure;


void initMemTypeRanges();

BOOL ept_handleWatchEvent(pcpuinfo currentcpuinfo, VMRegisters *registers, PFXSAVE64 fxsave, QWORD PhysicalAddress);
BOOL ept_handleCloakEvent(pcpuinfo currentcpuinfo, QWORD Address, QWORD AddressVA);

VMSTATUS handleEPTViolation(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, PFXSAVE64 fxsave);
VMSTATUS handleEPTMisconfig(pcpuinfo currentcpuinfo, VMRegisters *vmregisters);
int ept_handleWatchEventAfterStep(pcpuinfo currentcpuinfo, int ID);
int ept_handleCloakEventAfterStep(pcpuinfo currentcpuinfo, PCloakedPageData cloakdata);
int ept_handleSoftwareBreakpointAfterStep(pcpuinfo currentcpuinfo,  int ID);
int ept_handleStepAndBreak(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave, int brokenthreadid);

int ept_watch_activate(QWORD PhysicalAddress, int Size, int Type, DWORD Options, int MaxEntryCount, int *outID, QWORD OptionalField1, QWORD OptionalField2);
int ept_watch_deactivate(int ID);


VMSTATUS ept_watch_retrievelog(int ID, QWORD results, DWORD *resultSize, DWORD *offset, QWORD *errorcode);
//int ept_activateWatch(pcpuinfo currentcpuinfo, int ID);


int ept_getBrokenThreadListCount(void);
int ept_getBrokenThreadEntryShort(int id, int *WatchID, int *status, QWORD *CR3, QWORD *FSBASE, QWORD *GSBASE, QWORD *GSBASE_KERNEL, DWORD *CS, QWORD *RIP, QWORD *heartbeat);
int ept_getBrokenThreadEntryFull(int id, int *watchid, int *status, PPageEventExtended entry);
int ept_setBrokenThreadEntryFull(int id, PPageEventExtended entry);
int ept_resumeBrokenThread(int id, int continueMethod);


int ept_cloak_activate(QWORD physicalAddress, int mode);
int ept_cloak_deactivate(QWORD physicalAddress);
int ept_cloak_readOriginal(pcpuinfo currentcpuinfo, VMRegisters *registers, QWORD physicalAddress, QWORD destination);
int ept_cloak_writeOriginal(pcpuinfo currentcpuinfo, VMRegisters *registers, QWORD physicalAddress, QWORD source);
int ept_cloak_changeregonbp(QWORD physicalAddress, PCHANGEREGONBPINFO changereginfo);
int ept_cloak_removechangeregonbp(QWORD physicalAddress);
int ept_cloak_traceonbp(QWORD physicalAddress, DWORD flags, DWORD count);
int ept_cloak_traceonbp_remove(int forcequit);
int ept_cloak_traceonbp_getstatus(DWORD *count, DWORD *maxcount);
VMSTATUS ept_traceonbp_retrievelog(QWORD results, DWORD *resultSize, DWORD *offset, QWORD *errorcode);
int ept_cloak_traceonbp_stoptrace();

BOOL ept_handleSoftwareBreakpoint(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave);
BOOL ept_handleHardwareBreakpoint(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave);

void ept_reset();
void ept_invalidate();
void vpid_invalidate();

void ept_hideDBVMPhysicalAddresses(pcpuinfo currentcpuinfo);
void ept_hideDBVMPhysicalAddressesAllCPUs();

#endif /* VMM_EPTHANDLER_H_ */
