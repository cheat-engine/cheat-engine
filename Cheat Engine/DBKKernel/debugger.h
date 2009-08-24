#ifndef DEBUGGER_H
#define DEBUGGER_H

#include <ntifs.h>
#include <windef.h>

typedef struct {
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
typedef enum {si_ds=-12, si_es=-11, si_fs=-10, si_gs=-9, si_edi=-8, si_esi=-7, si_stack_ebp=-6, si_stack_esp=-5, si_ebx=-4, si_edx=-3, si_ecx=-2, si_eax=-1, si_ebp=0, si_eip=1, si_cs=2, si_eflags=3, si_esp=4, si_ss=5} stackindex;


typedef enum {bt_OnInstruction=0,bt_OnWrites=1, bt_OnIOAccess=2, bt_OnReadsAndWrites=3} BreakType;
typedef enum {bl_1byte=0, bl_2byte=1, bl_8byte=2/*Only when in 64-bit*/, bl_4byte=3} BreakLength;

void debugger_initialize(void);
int debugger_initHookForCurrentCPU(void);
int debugger_setGlobalDebugState(BOOL state);

int debugger_startDebugging(DWORD debuggedProcessID);
int debugger_setGDBreakpoint(int breakpointnr, ULONG_PTR Address, BreakType bt, BreakLength bl);
int debugger_unsetGDBreakpoint(int breakpointnr);
int debugger_stopDebugging(void);
NTSTATUS debugger_waitForDebugEvent(ULONG timeout);
NTSTATUS debugger_continueDebugEvent(BOOL handled);
DWORD *debugger_getLastStackPointer(void);
NTSTATUS debugger_getDebuggerState(PDebugStackState state);
NTSTATUS debugger_setDebuggerState(PDebugStackState state);

#endif