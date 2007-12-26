#include "ntifs.h"
#include <windef.h>

int _fltused;

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
Debugregs DebuggedProcessDR; //the debugregs registers as seen by the program itself
Debugregs DebuggerDR; //the debugregs owned by the debugger

typedef struct tagEFLAGS
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
} EFLAGS,*PEFLAGS;

typedef struct tagDebugReg7
{
	unsigned L0			:1; // 
	unsigned G0			:1; // 
	unsigned L1			:1; // 
	unsigned G1			:1; // 
	unsigned L2			:1; // 
	unsigned G2			:1; // 
	unsigned L3			:1; // 
	unsigned G3			:1; // 
	unsigned GL			:1; // 
	unsigned GE			:1; // 
	unsigned undefined1	:3; // 001 
	unsigned GD			:1; // 
	unsigned undefined2	:2; // 00 
	unsigned RW0		:2;
	unsigned LEN0		:2;
	unsigned RW1		:2;
	unsigned LEN1		:2;
	unsigned RW2		:2;
	unsigned LEN2		:2;
	unsigned RW3		:2;
	unsigned LEN3		:2;
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
} DebugReg6;

#pragma pack(1) //allignemnt of 1 byte
typedef struct tagINT_VECTOR
{
	WORD	wLowOffset;
	WORD	wSelector;
	BYTE	bAccess;
	BYTE	wUnused;
	WORD	wHighOffset;
} INT_VECTOR, *PINT_VECTOR;
#pragma pack()


#pragma pack(2) //allignment of 2 bytes
typedef struct tagGDT
{    
    WORD wLimit;
	PKGDTENTRY vector;
} GDT, *PGDT;

typedef struct tagIDT
{    
    WORD wLimit;
	PINT_VECTOR vector;
} IDT, *PIDT;
#pragma pack()

typedef struct tagDebugEvent
{
DWORD EAX,EBX,ECX,EDX,ESI,EDI,EBP,ESP,EIP;

} DebugEvent,*PDebugEvent;
DebugEvent DebugEvents[50];

typedef struct tagChangeReg
{
DWORD BreakAddress;
DWORD newEAX,newEBX,newECX,newEDX,newESI,newEDI,newEBP,newESP,newEIP;
BOOLEAN newCF,newPF,newAF,newZF,newSF,newOF;

BOOLEAN changeEAX,changeEBX,changeECX,changeEDX,changeESI,changeEDI,changeEBP,changeESP,changeEIP;
BOOLEAN changeCF,changePF,changeAF,changeZF,changeSF,changeOF;
BOOLEAN Active;

} ChangeReg,*PChangeReg;
ChangeReg ChangeRegs[4]; //max of 4
BOOLEAN ChangeRegistersOnBP;

UCHAR BufferSize;

INT_VECTOR	OriginalInt1;
INT_VECTOR	OriginalInt3;

UINT_PTR IDTAddresses[32]; //max 32 cpu's

//note: Make this a struct and even an array if needed. (Need to figure out dynamic mem)
PEPROCESS  	DebuggedProcessPEPROCESS;
DWORD		DebuggedProcessID;
DWORD		DebuggedAddress;
DWORD		DebuggedAddressLength;
BYTE		DebuggedAddressRWE;

void GetIDT(PIDT pIdt);
void GetGDT(PGDT pGdt);
void GetLDT(PGDT pLdt);
unsigned short GetTR(void);


unsigned __int64 readMSR(ULONG msr);
ULONG getCR3(void);
ULONG getCR4(void);
void  setCR4(ULONG cr4reg);
unsigned long long getTSC(void);

BOOLEAN ChangeRegOnBP(DWORD ProcessID, int DebugRegNR, PChangeReg CR);
BOOLEAN DebugProcess(DWORD ProcessID, DWORD Address, BYTE Length, BYTE RWE);
void StopDebugging(void);
void StopChangeRegOnBP(int DebugRegNR);

BOOLEAN HookInt1(void);
BOOLEAN HookInt3(void);
int globaldebug;
int PTESize;
UINT_PTR PAGE_SIZE_LARGE;
UINT_PTR MAX_PDE_POS;

BOOLEAN UsesAlternateMethod;
void int1apihook(void);
void OriginalInt1handler(void);