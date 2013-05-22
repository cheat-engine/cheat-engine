/*
 * ultimap.h
 *
 *  Created on: May 28, 2011
 *      Author: erich
 */

#ifndef ULTIMAP_H_
#define ULTIMAP_H_

#include "vmmhelper.h"
#include "common.h"

void ultimap_disable(pcpuinfo currentcpuinfo);
void ultimap_setup(pcpuinfo currentcpuinfo, QWORD CR3, QWORD DEBUGCTL, QWORD DS_AREA);
void ultimap_handleCR3Change(pcpuinfo currentcpuinfo, QWORD oldcr3, QWORD newcr3);
void ultimap_handleDB(pcpuinfo currentcpuinfo);
void ultimap_handleMSRWrite(pcpuinfo currentcpuinfo, DWORD msr, QWORD value);
QWORD ultimap_handleMSRRead(pcpuinfo currentcpuinfo, DWORD msr);

void ultimap_pause(pcpuinfo currentcpuinfo);
void ultimap_resume(pcpuinfo currentcpuinfo);
#ifdef ULTIMAPDEBUG

typedef struct
{
  QWORD Active; //set to 1 when active
  QWORD CR3; //Holds the CR3 value to watch taskswitch to and from
  QWORD DEBUGCTL; //Holds the DebugCTL value to set when inside the target process
  QWORD DS_AREA; //Holds the DS_AREA to set when
  QWORD OriginalDebugCTL; //When inside the target process this holds the debugctl that was set before entering. Return this on readMSR (and set with writeMSR when inside the process)
  QWORD OriginalDS_AREA; //When inside the target process this holds the DS_AREA that was set before entering. Return this with readMSR ('''')
  QWORD CR3_switchcount;
  QWORD CR3_switchcount2;
  QWORD LastOldCR3;
  QWORD LastNewCR3;
  QWORD CpuNr;
} *PULTIMAPDEBUGINFO;

void ultimap_debugoutput(pcpuinfo currentcpuinfo, PULTIMAPDEBUGINFO UltimapDebugInfo);
#endif /* ULTIMAPDEBUG */

#endif /* ULTIMAP_H_ */
