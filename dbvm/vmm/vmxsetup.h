/*
 * vmxsetup.h
 *
 *  Created on: Jan 26, 2018
 *      Author: eric heijnen
 */

#ifndef VMM_VMXSETUP_H_
#define VMM_VMXSETUP_H_

#include "common.h"
#include "vmmhelper.h"

typedef struct _RMIDT
{
  WORD offset;
  WORD segment;
} RMIDT, *PRMIDT;

typedef union _MTRRCAP
{
  QWORD Value;
  struct
  {
    unsigned VCNT     : 8; //variable range registers count
    unsigned FIX      : 1; //fixed range register support
    unsigned reserved : 1;
    unsigned WC       : 1; //Write combining support
    unsigned SMRR     : 1; //SMRR register support
  };
} MTRRCAP, *PMTRRCAP;  //e.g d0a: vcnt=10 FIX=1 WC=1 SMRR=1

typedef union _MTRRDEF
{
  QWORD Value;
  struct
  {
    unsigned TYPE     : 8; //variable range registers count
    unsigned reserved : 2;
    unsigned FE       : 1; //Fixed MTTR's enable/disable
    unsigned E        : 1; //MTTR enable/disable
  };
} MTRRDEF, *PMTRRDEF;

int hasMTRRsupport;
MTRRCAP MTRRCapabilities;
MTRRDEF MTRRDefType;

int has_EPT_1GBsupport;
int has_EPT_2MBSupport;
int has_EPT_ExecuteOnlySupport;
int has_EPT_INVEPTSingleContext;
int has_EPT_INVEPTAllContext;

int hasUnrestrictedSupport;
int hasVPIDSupport;
int canToggleCR3Exit;

int has_VPID_INVVPIDIndividualAddress;
int has_VPID_INVVPIDSingleContext;
int has_VPID_INVVPIDAllContext;
int has_VPID_INVVPIDSingleContextRetainingGlobals;

//AMD
int has_NP_1GBsupport;
int has_NP_2MBsupport;



int vmx_enableProcBasedFeature(DWORD PBF);
int vmx_disableProcBasedFeature(DWORD PBF);

int vmx_enableNMIWindowExiting(void);
int vmx_disableNMIWindowExiting(void);

int vmx_enableSingleStepMode(void);
int vmx_disableSingleStepMode(void);

int vmx_addSingleSteppingReason(pcpuinfo currentcpuinfo, int reason, int ID);
int vmx_addSingleSteppingReasonEx(pcpuinfo currentcpuinfo, int reason, void *data);

void vmx_setMSRReadExit(DWORD msrValue);
void vmx_removeMSRReadExit(DWORD msrValue);
void vmx_setMSRWriteExit(DWORD msrValue);
void vmx_removeMSRWriteExit(DWORD msrValue);

void vmx_enableTSCHook(pcpuinfo currentcpuinfo);
void vmx_disableTSCHook(pcpuinfo currentcpuinfo);


void setupVMX(pcpuinfo currentcpuinfo);

void setup8086WaitForSIPI(pcpuinfo currentcpuinfo, int setupvmcontrols);

QWORD realmode_inthook_calladdressPA;
int realmode_inthook_calladdressJumpSize;

extern int TSCHooked;

#endif /* VMM_VMXSETUP_H_ */
