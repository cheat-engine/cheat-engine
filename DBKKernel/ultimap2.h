#ifndef ULTIMAP2_H
#define ULTIMAP2_H

//MSR's
#define IA32_PERF_GLOBAL_STATUS		0x38e  
#define IA32_PERF_GLOBAL_OVF_CTRL	0x390

#define IA32_RTIT_CTL				0x570
#define IA32_RTIT_STATUS			0x571
#define IA32_RTIT_CR3_MATCH			0x572

#define IA32_RTIT_OUTPUT_BASE		0x560
#define IA32_RTIT_OUTPUT_MASK_PTRS  0x561

#pragma pack(1)
typedef union {
		struct{
			ULONG TraceEn : 1;
			ULONG Reserved_0 : 1;
			ULONG OS : 1;
			ULONG USER : 1;
			ULONG Reserved_1 : 3;
			ULONG CR3Filter : 1;
			ULONG ToPA : 1;
			ULONG Reserved_2 : 1;
			ULONG TSCEn : 1;
			ULONG DisRETC : 1;
			ULONG Reserved_3 : 1;
			ULONG BranchEn : 1;
			ULONG Reserved_4 : 18;
			ULONG Reserved_5 : 32;
		} Bits;
		UINT64 Value;
	}  RTIT_CTL, *PRTIT_CTL;

 
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

void SetupUltimap2(UINT32 PID, UINT32 BufferSize, WCHAR *Path);
void DisableUltimap2(void);

NTSTATUS ultimap2_waitForData(ULONG timeout, PULTIMAP2DATAEVENT data);
NTSTATUS ultimap2_continue(int cpunr);
NTSTATUS ultimap2_flushBuffers();

NTSTATUS ultimap2_pause();
NTSTATUS ultimap2_resume();

#endif