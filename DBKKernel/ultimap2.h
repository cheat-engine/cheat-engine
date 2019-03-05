#ifndef ULTIMAP2_H
#define ULTIMAP2_H

#include <ntifs.h>

//MSR's
#define IA32_PERF_GLOBAL_STATUS		0x38e  
#define IA32_PERF_GLOBAL_OVF_CTRL	0x390

#define IA32_RTIT_CTL				0x570
#define IA32_RTIT_STATUS			0x571
#define IA32_RTIT_CR3_MATCH			0x572

#define IA32_RTIT_OUTPUT_BASE		0x560
#define IA32_RTIT_OUTPUT_MASK_PTRS  0x561

#define IA32_RTIT_ADDR0_A			0x580
#define IA32_RTIT_ADDR0_B			0x581
#define IA32_RTIT_ADDR1_A			0x582
#define IA32_RTIT_ADDR1_B			0x583
#define IA32_RTIT_ADDR2_A			0x584
#define IA32_RTIT_ADDR2_B			0x585
#define IA32_RTIT_ADDR3_A			0x586
#define IA32_RTIT_ADDR3_B			0x587

typedef struct
{
	UINT64 StartAddress;
	UINT64 EndAddress;
	UINT64 IsStopAddress;
} URANGE, *PURANGE;

#pragma pack(push)
#pragma pack(1)
typedef union {
		struct{
			ULONG TraceEn : 1;   //0
			ULONG Reserved_0 : 1; //1
			ULONG OS : 1; //2
			ULONG USER : 1; //3
			ULONG Reserved_1 : 2; //4,5
			ULONG FabricEn : 1; //6
			ULONG CR3Filter : 1; //7
			ULONG ToPA : 1; //8
			ULONG MTCEn : 1; //9
			ULONG TSCEn : 1; //10
			ULONG DisRETC : 1; //11
			ULONG Reserved_2 : 1; //12
			ULONG BranchEn : 1; //13
			ULONG MTCFreq : 4; //14,15,16,17
			ULONG Reserved_3 : 1; //18
			ULONG CycThresh : 4; //19,20,21,22
			ULONG Reserved_4 : 1; //32
			ULONG PSBFreq : 4; //24,25,26,27
			ULONG Reserved_5 : 4; //28,29,30,31

			ULONG ADDR0_CFG : 4;
			ULONG ADDR1_CFG : 4;
			ULONG ADDR2_CFG : 4;
			ULONG ADDR3_CFG : 4;
			ULONG Reserved_6 : 16;
		} Bits;
		UINT64 Value;
	}  RTIT_CTL, *PRTIT_CTL;

 
typedef union {
	struct{
		UINT64 FilterEn : 1;
		UINT64 ContextEn : 1;
		UINT64 TriggerEn : 1;
		UINT64 Reserved_1 : 1;
		UINT64 Error : 1;
		UINT64 Stopped : 1;
		UINT64 Reserved_2 : 58;
	} Bits;
	UINT64 Value;
}  RTIT_STATUS, *PRTIT_STATUS;

typedef union {
	struct{
		UINT64 END : 1;
		UINT64 Reserved_0 : 1;
		UINT64 INT : 1;
		UINT64 Reserved_1 : 1;
		UINT64 STOP : 1;
		UINT64 Reserved_2 : 1;
		UINT64 Size : 4;
		UINT64 Reserved_3 : 2;
		UINT64 PhysicalFrameNr : 52;
	} Bits;
	UINT64 Value;
}  ToPA_ENTRY, *PToPA_ENTRY;

typedef struct{
	UINT64 PhysicalAddress;
	int index;
} ToPA_LOOKUP, *PToPA_LOOKUP;

typedef struct
{
	UINT64 Address;
	UINT64 Size;
	UINT64 CpuID;	
} ULTIMAP2DATAEVENT, *PULTIMAP2DATAEVENT;

#pragma pack(pop)

void SetupUltimap2(UINT32 PID, UINT32 BufferSize, WCHAR *Path, int rangeCount, PURANGE Ranges, int NoPMI, int UserMode, int KernelMode);
void DisableUltimap2(void);

NTSTATUS ultimap2_waitForData(ULONG timeout, PULTIMAP2DATAEVENT data);
NTSTATUS ultimap2_continue(int cpunr);
NTSTATUS ultimap2_flushBuffers();

NTSTATUS ultimap2_pause();
NTSTATUS ultimap2_resume();
void ultimap2_LockFile(int cpunr);
void ultimap2_ReleaseFile(int cpunr);
UINT64 ultimap2_GetTraceFileSize();
void ultimap2_ResetTraceFileSize();

void UnregisterUltimapPMI();

typedef NTSTATUS(*PSSUSPENDPROCESS)(PEPROCESS p);

extern PSSUSPENDPROCESS PsSuspendProcess;
extern PSSUSPENDPROCESS PsResumeProcess;

#endif