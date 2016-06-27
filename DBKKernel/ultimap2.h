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

typedef union {
		struct{
			ULONG TraceEn : 1;
			ULONG OS : 1;
			ULONG USER : 1;
			ULONG Reserved : 4;
			ULONG CR3Filter : 1;
			ULONG Accessed : 1;
			ULONG ToPA : 1;
			ULONG Reserved_2 : 1;
			ULONG TSCEn : 1;
			ULONG DisRETC : 1;
			ULONG Reserved_3 : 1;
			ULONG BranchEn : 1;
			ULONG Reserved_4 : 17;
			ULONG Reserved_5 : 32;
		} Bits;
		UINT64 Value;
	}  RTIT_CTL, *PRTIT_CTL;


typedef union {
	struct{
		ULONG END : 1;
		ULONG INT : 1;
		ULONG Reserved : 1;
		ULONG STOP : 1;		
		ULONG Reserved2 : 8;
		UINT64 PhysicalFrameNr : 52;
	} Bits;
	UINT64 Value;
}  ToPA_ENTRY, *PToPA_ENTRY;

typedef struct{
	UINT64 PhysicalAddress;
	int index;
} ToPA_LOOKUP, *PToPA_LOOKUP;

void SetupUltimap2(PEPROCESS target, UINT64 cr3, UINT32 BufferSize);

#endif