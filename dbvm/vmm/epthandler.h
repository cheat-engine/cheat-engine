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

int handleEPTViolation(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, PFXSAVE64 fxsave);
int handleEPTMisconfig(pcpuinfo currentcpuinfo, VMRegisters *vmregisters);
int ept_handleWatchEventAfterStep(pcpuinfo currentcpuinfo, int ID);

int ept_activateWatch(pcpuinfo currentcpuinfo, int ID);
int ept_disableWatch(pcpuinfo currentcpuinfo, int ID);
int getFreeWatchID(pcpuinfo currentcpuinfo);

#endif /* VMM_EPTHANDLER_H_ */
