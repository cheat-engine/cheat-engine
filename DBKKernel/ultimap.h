#ifndef ULTIMAP_H
#define ULTIMAP_H

#include <ntifs.h>
#include <windef.h>

typedef UINT64 QWORD;

#pragma pack(push)
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
NTSTATUS ultimap(UINT64 cr3, UINT64 dbgctl_msr, int _DS_AREA_SIZE, BOOL savetofile, WCHAR *filename, int handlerCount);
void ultimap_pause();
void ultimap_resume();

void ultimap_disable(void);
void ultimap_flushBuffers(void);


PDS_AREA_MANAGEMENT DS_AREA[256]; //used to store the addresses. (reading the msr that holds the DS_AREA is impossible with dbvm active)
int DS_AREA_SIZE;


#pragma pack(pop)

#endif