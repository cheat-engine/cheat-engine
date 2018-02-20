/*
 * epthandler.h
 *
 *  Created on: Feb 2, 2018
 *      Author: erich
 */

#include "vmmhelper.h"

#ifndef VMM_EPTHANDLER_H_
#define VMM_EPTHANDLER_H_

typedef union _EPT_VIOLATION_INFO
{
  QWORD ExitQualification;
  struct{
    unsigned R        :  1; // 0: Read Access
    unsigned W        :  1; // 1: Write Access
    unsigned X        :  1; // 2: Execute access (supervisor mode in mode-based execute control for EPT)
    unsigned ANDb0    :  1; //
    unsigned ANDb1    :  1; //
    unsigned ANDb2    :  1; //
    unsigned ANDb10   :  1; // if mode-based execute control else undefined
    unsigned GuestLinearAddressValid: 1;
    unsigned AddressAccess: 1; //if GuestLinearAddressValid then 1 if the access was to a page, 0 if it was in the paging system
    //only defined if GuestLinearAddressValid & AddressAccess == 1 and advanced VM-exit information for EPT violations
    unsigned supervisor     : 1; //1 if supervisor, 0 if usermode
    unsigned pagingreadonly : 1; //1 if paged as read only, 0 if read/write
    unsigned XD             : 1; //1 if execute disable, 0 if executable
    unsigned NMIUnblockedDueToIRET : 1; //
    unsigned reserved1      : 19;
    DWORD reserved2;
  };
} __attribute__((__packed__)) EPT_VIOLATION_INFO, *PEPT_VIOLATION_INFO;


#define MTC_UC  0
#define MTC_WC  1
#define MTC_WT  3
#define MTC_WP  5
#define MTC_WB  6


void initMemTypeRanges();

int handleEPTViolation(pcpuinfo currentcpuinfo, VMRegisters *vmregisters);
int handleEPTMisconfig(pcpuinfo currentcpuinfo, VMRegisters *vmregisters);


#endif /* VMM_EPTHANDLER_H_ */
