/*
 * test.h
 *
 *  Created on: May 22, 2011
 *      Author: erich
 */

#ifndef TEST_H_
#define TEST_H_

#include "common.h"

int testBranchPrediction(void);

extern void testBranch(void);

extern void *DebugStore;

extern int handlePerformanceCounterInterrupt(void);


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
} __attribute__((__packed__)) DS_AREA_MANAGEMENT32,*PDS_AREA_MANAGEMENT32;

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
} __attribute__((__packed__)) DS_AREA_MANAGEMENT64,*PDS_AREA_MANAGEMENT64;

typedef struct
{

  DWORD LastBranchFrom;
  DWORD LastBranchTo;
  DWORD Extra;
} __attribute__((__packed__)) BTS32,*PBTS32;

typedef struct
{

  QWORD LastBranchFrom;
  QWORD LastBranchTo;
  QWORD Extra;
} __attribute__((__packed__)) BTS64,*PBTS64;


//DBVM vmcall stuff

int getDBVMVersion(void);
void dbvm_watch_writes_test(void);
void dbvm_watch_reads_test(void);
void dbvm_watch_execute_test(void);
void dbvm_cloak_test(void);
void dbvm_changeregonbp_test(void);




#endif /* TEST_H_ */
