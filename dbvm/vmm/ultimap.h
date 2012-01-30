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

void ultimap_pause(pcpuinfo currentcpuinfo);
void ultimap_resume(pcpuinfo currentcpuinfo);

#endif /* ULTIMAP_H_ */
