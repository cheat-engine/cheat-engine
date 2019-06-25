#ifndef DBKFUNC_H
#define DBKFUNC_H

#pragma warning( disable: 4214 )

#include <ntifs.h>
#include <ntstrsafe.h>

#include <windef.h>

#include "interruptHook.h"

#ifdef RELEASE
#define DbgPrint(...)
#endif

int _fltused;


typedef VOID F(UINT_PTR param);
typedef F *PF;

typedef VOID PREDPC_CALLBACK(CCHAR cpunr, PKDEFERRED_ROUTINE Dpc, PVOID DeferredContext, PVOID *SystemArgument1, PVOID *SystemArgument2);

typedef PREDPC_CALLBACK *PPREDPC_CALLBACK;


typedef struct _criticalSection
{
  LONG locked;
  int cpunr; //unique id for a cpu
  int lockcount;
  int oldIFstate;
} criticalSection, *PcriticalSection;


//ntosp.h

typedef
_Function_class_(KNORMAL_ROUTINE)
_IRQL_requires_max_(PASSIVE_LEVEL)
_IRQL_requires_min_(PASSIVE_LEVEL)
_IRQL_requires_(PASSIVE_LEVEL)
_IRQL_requires_same_
VOID
KNORMAL_ROUTINE(
	_In_opt_ PVOID NormalContext,
	_In_opt_ PVOID SystemArgument1,
	_In_opt_ PVOID SystemArgument2
);
typedef KNORMAL_ROUTINE *PKNORMAL_ROUTINE;

typedef
_Function_class_(KKERNEL_ROUTINE)
_IRQL_requires_max_(APC_LEVEL)
_IRQL_requires_min_(APC_LEVEL)
_IRQL_requires_(APC_LEVEL)
_IRQL_requires_same_
VOID
KKERNEL_ROUTINE(
	_In_ struct _KAPC *Apc,
	_Inout_ PKNORMAL_ROUTINE *NormalRoutine,
	_Inout_ PVOID *NormalContext,
	_Inout_ PVOID *SystemArgument1,
	_Inout_ PVOID *SystemArgument2
);
typedef KKERNEL_ROUTINE *PKKERNEL_ROUTINE;


typedef
_Function_class_(KRUNDOWN_ROUTINE)
_IRQL_requires_max_(PASSIVE_LEVEL)
_IRQL_requires_min_(PASSIVE_LEVEL)
_IRQL_requires_(PASSIVE_LEVEL)
_IRQL_requires_same_
VOID
KRUNDOWN_ROUTINE(
	_In_ struct _KAPC *Apc
);
typedef KRUNDOWN_ROUTINE *PKRUNDOWN_ROUTINE;


typedef
_IRQL_requires_same_
_Function_class_(KENUM_ROUTINE)
VOID
KENUM_ROUTINE(
	_In_reads_(_Inexpressible_(Length)) PVOID Data,
	_In_ ULONG Length,
	_In_ PVOID Context
);

typedef KENUM_ROUTINE *PKENUM_ROUTINE;

typedef enum _KAPC_ENVIRONMENT {
	OriginalApcEnvironment,
	AttachedApcEnvironment,
	CurrentApcEnvironment,
	InsertApcEnvironment
} KAPC_ENVIRONMENT;



NTKERNELAPI
_IRQL_requires_max_(DISPATCH_LEVEL)
_IRQL_requires_min_(PASSIVE_LEVEL)
_IRQL_requires_same_
VOID
KeEnumerateQueueApc(
	_Inout_ PKTHREAD Thread,
	_In_ PKENUM_ROUTINE CallbackRoutine,
	_In_ PVOID Context,
	_In_opt_ KPROCESSOR_MODE *ApcMode
);


NTKERNELAPI
_IRQL_requires_same_
_When_(Environment != OriginalApcEnvironment, __drv_reportError("Caution: "
	"Using an APC environment other than the original environment can lead to "
	"a system bugcheck if the target thread is attached to a process with APCs "
	"disabled. APC environments should be used with care."))
	VOID
	KeInitializeApc(
		_Out_ PRKAPC Apc,
		_In_ PRKTHREAD Thread,
		_In_ KAPC_ENVIRONMENT Environment,
		_In_ PKKERNEL_ROUTINE KernelRoutine,
		_In_opt_ PKRUNDOWN_ROUTINE RundownRoutine,
		_In_opt_ PKNORMAL_ROUTINE NormalRoutine,
		_In_opt_ KPROCESSOR_MODE ProcessorMode,
		_In_opt_ PVOID NormalContext
	);

NTKERNELAPI
_Must_inspect_result_
_IRQL_requires_max_(DISPATCH_LEVEL)
_IRQL_requires_min_(PASSIVE_LEVEL)
_IRQL_requires_same_
BOOLEAN
KeInsertQueueApc(
	_Inout_ PRKAPC Apc,
	_In_opt_ PVOID SystemArgument1,
	_In_opt_ PVOID SystemArgument2,
	_In_ KPRIORITY Increment
);



NTSYSAPI
NTSTATUS
NTAPI
ZwOpenThread(
	OUT PHANDLE             ThreadHandle,
	IN ACCESS_MASK          DesiredAccess,
	IN POBJECT_ATTRIBUTES   ObjectAttributes,
	IN PCLIENT_ID           ClientId
);



struct PTEStruct
{
	unsigned P         :  1; // present (1 = present)
	unsigned RW        :  1; // read/write
	unsigned US        :  1; // user/supervisor
	unsigned PWT       :  1; // page-level write-through
	unsigned PCD       :  1; // page-level cache disabled
	unsigned A         :  1; // accessed
	unsigned Reserved  :  1; // dirty
	unsigned PS        :  1; // page size (0 = 4-KB page)
	unsigned G         :  1; // global page
	unsigned A1		   :  1; // available 1 aka copy-on-write
	unsigned A2		   :  1; // available 2/ is 1 when paged to disk
	unsigned A3		   :  1; // available 3
	unsigned PFN       : 20; // page-frame number
};

//typedef struct PTEStruct *PPDPTE;
//typedef struct PTEStruct *PPDE;
//typedef struct PTEStruct *PPTE;

struct PTEStruct64
{
	unsigned long long P : 1; // present (1 = present)
	unsigned long long RW : 1; // read/write
	unsigned long long US : 1; // user/supervisor
	unsigned long long PWT : 1; // page-level write-through
	unsigned long long PCD : 1; // page-level cache disabled
	unsigned long long A : 1; // accessed
	unsigned long long Reserved : 1; // dirty
	unsigned long long PS : 1; // page size (0 = 4-KB page)
	unsigned long long G : 1; // global page
	unsigned long long A1 : 1; // available 1 aka copy-on-write
	unsigned long long A2 : 1; // available 2/ is 1 when paged to disk
	unsigned long long A3 : 1; // available 3
	unsigned long long PFN : 52; // page-frame number
};

//typedef struct PTEStruct64 *PPDPTE_PAE;
//typedef struct PTEStruct64 *PPDE_PAE;
//typedef struct PTEStruct64 *PPTE_PAE;


typedef struct tagDebugregs
{
	ULONG DR0;
	ULONG DR1;
	ULONG DR2;
	ULONG DR3;
	ULONG DR5;
	ULONG DR6;
	ULONG DR7;
} Debugregs;



typedef struct 
{
	unsigned CF			:1; // 0
	unsigned reserved1	:1; // 1
	unsigned PF			:1; // 2
	unsigned reserved2	:1; // 3
	unsigned AF			:1; // 4
	unsigned reserved3	:1; // 5
	unsigned ZF			:1; // 6
	unsigned SF			:1; // 7
	unsigned TF			:1; // 8
	unsigned IF			:1; // 9
	unsigned DF			:1; // 10
	unsigned OF			:1; // 11
	unsigned IOPL		:2; // 12+13
	unsigned NT			:1; // 14
	unsigned reserved4	:1; // 15
	unsigned RF			:1; // 16
	unsigned VM			:1; // 17
	unsigned AC			:1; // 18
	unsigned VIF		:1; // 19
	unsigned VIP		:1; // 20
	unsigned ID			:1; // 21
	unsigned reserved5	:10; // 22-31
#ifdef AMD64
	unsigned reserved6	:8;
	unsigned reserved7	:8;
	unsigned reserved8	:8;
	unsigned reserved9	:8;
#endif
} EFLAGS,*PEFLAGS;

typedef struct tagDebugReg7
{
	unsigned L0			:1; //			0
	unsigned G0			:1; //			1
	unsigned L1			:1; //			2
	unsigned G1			:1; //			3
	unsigned L2			:1; //			4
	unsigned G2			:1; //			5
	unsigned L3			:1; //			6
	unsigned G3			:1; //			7
	unsigned GL			:1; //			8
	unsigned GE			:1; //			9
	unsigned undefined1	:3; // 001		10
	unsigned GD			:1; //			11
	unsigned undefined2	:2; // 00 
	unsigned RW0		:2;
	unsigned LEN0		:2;
	unsigned RW1		:2;
	unsigned LEN1		:2;
	unsigned RW2		:2;
	unsigned LEN2		:2;
	unsigned RW3		:2;
	unsigned LEN3		:2;
#ifdef AMD64
	unsigned undefined3	:8;
	unsigned undefined4	:8;
	unsigned undefined5	:8;
	unsigned undefined6	:8;
#endif
} DebugReg7;

typedef struct DebugReg6
{
	unsigned B0			:1;
	unsigned B1			:1;
	unsigned B2			:1;
	unsigned B3			:1;
	unsigned undefined1	:9; // 011111111
	unsigned BD			:1;
	unsigned BS			:1;
	unsigned BT			:1;
	unsigned undefined2	:16; // 1111111111111111
#ifdef AMD64
	unsigned undefined3	:8;
	unsigned undefined4	:8;
	unsigned undefined5	:8;
	unsigned undefined6	:8;
#endif
} DebugReg6;




#pragma pack(2) //allignment of 2 bytes
typedef struct tagGDT
{    
    WORD wLimit;
	PVOID vector;
} GDT, *PGDT;
#pragma pack()

//UCHAR BufferSize;

void GetIDT(PIDT pIdt);

#ifdef AMD64
extern void _fxsave(volatile void *);
extern void GetGDT(PGDT pGdt);
extern WORD GetLDT();
extern WORD GetTR(void);
#else
void GetGDT(PGDT pGdt);
WORD GetLDT();
WORD GetTR(void);
#endif



UINT64 readMSR(DWORD msr);
UINT64 getDR7(void);
void setCR0(UINT64 newCR0);
UINT64 getCR0(void);
UINT64 getCR2(void);
void setCR3(UINT64 newCR3);
UINT64 getCR3(void);
UINT64 getCR4(void);
void setCR4(UINT64 newcr4);
UINT64 getTSC(void);

#ifdef AMD64

extern WORD getCS(void);
extern WORD getSS(void);
extern WORD getDS(void);
extern WORD getES(void);
extern WORD getFS(void);
extern WORD getGS(void);
extern UINT64 getRSP(void);
extern UINT64 getRBP(void);
extern UINT64 getRAX(void);
extern UINT64 getRBX(void);
extern UINT64 getRCX(void);
extern UINT64 getRDX(void);
extern UINT64 getRSI(void);
extern UINT64 getRDI(void);
#else
WORD getCS(void);
WORD getSS(void);
WORD getDS(void);
WORD getES(void);
WORD getFS(void);
WORD getGS(void);
ULONG getRSP(void);
ULONG getRBP(void);
ULONG getRAX(void);
ULONG getRBX(void);
ULONG getRCX(void);
ULONG getRDX(void);
ULONG getRSI(void);
ULONG getRDI(void);
#endif

extern UINT64 getR8(void);
extern UINT64 getR9(void);
extern UINT64 getR10(void);
extern UINT64 getR11(void);
extern UINT64 getR12(void);
extern UINT64 getR13(void);
extern UINT64 getR14(void);
extern UINT64 getR15(void);

extern UINT64 getAccessRights(UINT64 segment);
extern UINT64 getSegmentLimit(UINT64 segment);


int getCpuCount(void);

BOOL loadedbydbvm;
int PTESize;
UINT_PTR PAGE_SIZE_LARGE;
UINT_PTR MAX_PDE_POS;
UINT_PTR MAX_PTE_POS;

int cpu_stepping;
int cpu_model;
int cpu_familyID;
int cpu_type;
int cpu_ext_modelID;
int cpu_ext_familyID;

int KernelCodeStepping;
int KernelWritesIgnoreWP;

int isPrefix(unsigned char b);
EFLAGS getEflags(void);
int cpunr(void);
void disableInterrupts(void);
void enableInterrupts(void);


void csEnter(PcriticalSection CS);
void csLeave(PcriticalSection CS);

void forOneCpu(CCHAR cpunr, PKDEFERRED_ROUTINE dpcfunction, PVOID DeferredContext, PVOID  SystemArgument1, PVOID  SystemArgument2, OPTIONAL PPREDPC_CALLBACK preDPCCallback);
void forEachCpu(PKDEFERRED_ROUTINE dpcfunction, PVOID DeferredContext, PVOID  SystemArgument1, PVOID  SystemArgument2, OPTIONAL PPREDPC_CALLBACK preDPCCallback);
void forEachCpuAsync(PKDEFERRED_ROUTINE dpcfunction, PVOID DeferredContext, PVOID  SystemArgument1, PVOID  SystemArgument2, OPTIONAL PPREDPC_CALLBACK preDPCCallback);
void forEachCpuPassive(PF f, UINT_PTR param);

#endif;