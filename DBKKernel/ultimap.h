#ifndef ULTIMAP_H
#define ULTIMAP_H

#include <ntifs.h>
#include <windef.h>

typedef UINT64 QWORD;

#pragma pack(1)
/*
typedef struct
{
  DWORD BTS_BufferBaseAddress;
  DWORD BTS_IndexBaseAddress;
  DWORD BTS_AbsoluteMaxAddress;
  DWORD BTS_InterruptThresholdAddress;
  DWORD PEBS_BufferBaseAddress;
  DWORD PEBS_IndexBaseAddress;
  DWORD PEBS_AbsoluteMaxAddress;
  DWORD PEBS_InterruptThresholdAddress;
  QWORD PEBS_CounterReset;
  QWORD Reserved;
  DWORD Reserved2;
} DS_AREA_MANAGEMENT32,*PDS_AREA_MANAGEMENT32;
*/

typedef struct
{

  QWORD BTS_BufferBaseAddress;
  QWORD BTS_IndexBaseAddress;
  QWORD BTS_AbsoluteMaxAddress;
  QWORD BTS_InterruptThresholdAddress;
  QWORD PEBS_BufferBaseAddress;
  QWORD PEBS_IndexBaseAddress;
  QWORD PEBS_AbsoluteMaxAddress;
  QWORD PEBS_InterruptThresholdAddress;
  QWORD PEBS_CounterReset;
  QWORD Reserved;
  QWORD Reserved2;
} DS_AREA_MANAGEMENT64,*PDS_AREA_MANAGEMENT64;

typedef struct
{

  QWORD LastBranchFrom;
  QWORD LastBranchTo;
  QWORD Extra;
} BTS,*PBTS;


//#ifdef AMD64	
typedef DS_AREA_MANAGEMENT64 DS_AREA_MANAGEMENT;
typedef PDS_AREA_MANAGEMENT64 PDS_AREA_MANAGEMENT;
//#else
//typedef DS_AREA_MANAGEMENT32 DS_AREA_MANAGEMENT;	
//typedef PDS_AREA_MANAGEMENT32 PDS_AREA_MANAGEMENT;	
//#endif

typedef struct
{
	UINT64 DataReadyEvent;
	UINT64 DataHandledEvent;
} ULTIMAPEVENT, *PULTIMAPEVENT;

typedef struct
{
	UINT64 Address;
	UINT64 Size;
	UINT64 Block;
	UINT64 CpuID;
	UINT64 KernelAddress;
	UINT64 Mdl; //go ahead, scream
} ULTIMAPDATAEVENT, *PULTIMAPDATAEVENT;

NTSTATUS ultimap_continue(PULTIMAPDATAEVENT data);
NTSTATUS ultimap_waitForData(ULONG timeout, PULTIMAPDATAEVENT data);
NTSTATUS ultimap(UINT64 cr3, UINT64 dbgctl_msr, int DS_AREA_SIZE, BOOL savetofile, WCHAR *filename, int handlerCount);
void ultimap_disable(void);
void ultimap_flushBuffers(void);
void setup_APIC_BASE(void);
void clean_APIC_BASE(void);

void apic_clearPerfmon();

PDS_AREA_MANAGEMENT DS_AREA[256]; //used to store the addresses. (reading the msr that holds the DS_AREA is impossible with dbvm active)
int DS_AREA_SIZE;

//Not sure if it's already defined somewhere in a header, but I need this
typedef struct {
  DWORD a;
  DWORD b;
  DWORD c;
  DWORD d;
} UINT128;

typedef volatile struct {
	UINT128 Reserved1;
	UINT128 Reserved2;
	UINT128 LocalAPIC_ID;
	UINT128 LocalAPIC_Version;
	UINT128 Reserved3;
	UINT128 Reserved4;
	UINT128 Reserved5;
	UINT128 Reserved6;
	UINT128 Task_Priority;
	UINT128 Arbritation_Priority;
	UINT128 Processor_Priority;
	UINT128 EOI;
	UINT128 Reserved7;
	UINT128 Logical_Destination;
	UINT128 Destination_Format;
	UINT128 Spurious_Interrupt_Vector;
	UINT128 In_Service[8];
	UINT128 Trigger_Mode[8];
	UINT128 Interrupt_Request[8];
	UINT128 Error_Status;
	UINT128 Reserved8[7];
	UINT128 Interrupt_Command_Low32Bit;
	UINT128 Interrupt_Command_High32Bit;
	UINT128 LVT_Timer;
	UINT128 LVT_Thermal_Sensor;
	UINT128 LVT_Performance_Monitor;
	UINT128 LVT_LINT0;
	UINT128 LVT_LINT1;
	UINT128 LVT_Error;
	UINT128 Initial_Count;
	UINT128 Current_Count;
	UINT128 Reserved9[4];
	UINT128 Divide_Configuration;
	UINT128 Reserved10;
} APIC, *PAPIC;

extern volatile PAPIC APIC_BASE;

#endif