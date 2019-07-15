#ifndef DEBUGGER_H
#define DEBUGGER_H

#include <ntifs.h>
#include <windef.h>

#pragma pack(4)
typedef struct {
	UINT64 threadid;  //
	UINT64 causedbydbvm;
	UINT64 rflags;
	UINT64 rax;//
	UINT64 rbx;
	UINT64 rcx;//
	UINT64 rdx;
	UINT64 rsi;//
	UINT64 rdi;
	UINT64 rbp;//
	UINT64 rsp;
	UINT64 rip;//
	UINT64 r8;
	UINT64 r9;//
	UINT64 r10;
	UINT64 r11;//
	UINT64 r12;
	UINT64 r13;//
	UINT64 r14;
	UINT64 r15;//
	UINT64 cs;
	UINT64 ds;//
	UINT64 es;
	UINT64 fs;//
	UINT64 gs;
	UINT64 ss;//
	UINT64 dr0;
	UINT64 dr1;//
	UINT64 dr2;
	UINT64 dr3;//
	UINT64 dr6;
	UINT64 dr7;//
	BYTE fxstate[512];

	UINT64 LBR_Count;
	UINT64 LBR[16];
} DebugStackState, *PDebugStackState;
#pragma pack()

//stack index

typedef enum {bt_OnInstruction=0,bt_OnWrites=1, bt_OnIOAccess=2, bt_OnReadsAndWrites=3} BreakType;
typedef enum {bl_1byte=0, bl_2byte=1, bl_8byte=2/*Only when in 64-bit*/, bl_4byte=3} BreakLength;

void debugger_initialize(void);
int debugger_initHookForCurrentCPU(void);
int debugger_setGlobalDebugState(BOOL state);
void debugger_setStoreLBR(BOOL state);

int debugger_startDebugging(DWORD debuggedProcessID);
int debugger_setGDBreakpoint(int breakpointnr, ULONG_PTR Address, BreakType bt, BreakLength bl);
int debugger_unsetGDBreakpoint(int breakpointnr);
void debugger_touchDebugRegister(UINT_PTR param);
int debugger_stopDebugging(void);
NTSTATUS debugger_waitForDebugEvent(ULONG timeout);
NTSTATUS debugger_continueDebugEvent(BOOL handled);
UINT_PTR *debugger_getLastStackPointer(void);
NTSTATUS debugger_getDebuggerState(PDebugStackState state);
NTSTATUS debugger_setDebuggerState(PDebugStackState state);

void GetDebuggerInfo(void);
VOID debugger_initHookForCurrentCPU_DPC(IN struct _KDPC *Dpc, IN PVOID  DeferredContext, IN PVOID  SystemArgument1, IN PVOID  SystemArgument2);

#endif