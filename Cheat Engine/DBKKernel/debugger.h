#ifndef DEBUGGER_H
#define DEBUGGER_H

#include <ntifs.h>
#include <windef.h>

typedef struct {
	ULONG threadid;
	ULONG eflags;
	ULONG eax;
	ULONG ebx;
	ULONG ecx;
	ULONG edx;
	ULONG esi;
	ULONG edi;
	ULONG ebp;
	ULONG esp;
	ULONG eip;
	ULONG cs;
	ULONG ds;
	ULONG es;
	ULONG fs;
	ULONG gs;
	ULONG ss;
	ULONG dr0;
	ULONG dr1;
	ULONG dr2;
	ULONG dr3;
	ULONG dr6;
	ULONG dr7;
} DebugStackState, *PDebugStackState;

//stack index

typedef enum {bt_OnInstruction=0,bt_OnWrites=1, bt_OnIOAccess=2, bt_OnReadsAndWrites=3} BreakType;
typedef enum {bl_1byte=0, bl_2byte=1, bl_8byte=2/*Only when in 64-bit*/, bl_4byte=3} BreakLength;

void debugger_initialize(void);
int debugger_initHookForCurrentCPU(void);
int debugger_setGlobalDebugState(BOOL state);

int debugger_startDebugging(DWORD debuggedProcessID);
int debugger_setGDBreakpoint(int breakpointnr, ULONG_PTR Address, BreakType bt, BreakLength bl);
int debugger_unsetGDBreakpoint(int breakpointnr);
void debugger_touchDebugRegister(void);
int debugger_stopDebugging(void);
NTSTATUS debugger_waitForDebugEvent(ULONG timeout);
NTSTATUS debugger_continueDebugEvent(BOOL handled);
DWORD *debugger_getLastStackPointer(void);
NTSTATUS debugger_getDebuggerState(PDebugStackState state);
NTSTATUS debugger_setDebuggerState(PDebugStackState state);

#endif