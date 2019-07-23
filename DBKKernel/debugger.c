/*
debugger.c:
This unit will handle all debugging related code, from hooking, to handling interrupts

todo: this whole thing can be moved to a few simple lines in dbvm...
*/
#pragma warning( disable: 4100 4103 4189)
#include <ntifs.h>
#include <windef.h>

#include "DBKFunc.h"
#include "interruptHook.h"

#include "debugger.h"
#include "vmxhelper.h"

#ifdef AMD64 
extern void interrupt1_asmentry( void ); //declared in debuggera.asm
#else
void interrupt1_asmentry( void );
#endif



volatile struct
{
	BOOL		isDebugging;		//TRUE if a process is currently being debugged
	BOOL		stoppingTheDebugger;
	DWORD		debuggedProcessID;	//The processID that is currently debugger
	struct {
		BOOL		active;
		UINT_PTR	address;		//Up to 4 addresses to break on
		BreakType	breakType;		//What type of breakpoint for each seperate address
		BreakLength breakLength;	//How many bytes does this breakpoint look at
	} breakpoint[4];

	//...
	BOOL globalDebug;			//If set all threads of every process will raise an interrupt on taskswitch

	//while debugging:
	UINT_PTR *LastStackPointer;
	UINT_PTR *LastRealDebugRegisters;
	HANDLE LastThreadID;
	BOOL CausedByDBVM;
	BOOL handledlastevent;
	
	BOOL storeLBR;
	int storeLBR_max;
	UINT_PTR *LastLBRStack;

	volatile struct {		
		UINT_PTR DR0;
		UINT_PTR DR1;
		UINT_PTR DR2;
		UINT_PTR DR3;
		UINT_PTR DR6;
		UINT_PTR DR7;
		UINT_PTR reserved;
		volatile int inEpilogue; //if set the global debug bit does no faking
	} FakedDebugRegisterState[256];

	char b[1];

	volatile BYTE DECLSPEC_ALIGN(16) fxstate[512];

	BOOL isSteppingTillClear; //when set the user has entered single stepping mode. This is a one thread only thing, so when it's active and another single step happens, discard it

} DebuggerState;


KEVENT debugger_event_WaitForContinue; //event for kernelmode. Waits till it's set by usermode (usermode function: DBK_Continue_Debug_Event sets it)
KEVENT debugger_event_CanBreak; //event for kernelmode. Waits till a break has been handled so a new one can enter
KEVENT debugger_event_WaitForDebugEvent; //event for usermode. Waits till it's set by a debugged event

DebugReg7 debugger_dr7_getValue(void);
void debugger_dr7_setValue(DebugReg7 value);
DebugReg6 debugger_dr6_getValue(void);

JUMPBACK Int1JumpBackLocation;




void debugger_dr7_setGD(int state)
{

	DebugReg7 _dr7=debugger_dr7_getValue();
	_dr7.GD=state; //usually 1
	debugger_dr7_setValue(_dr7);

	
}

void debugger_dr0_setValue(UINT_PTR value)
{
	__writedr(0,value);
}

UINT_PTR debugger_dr0_getValue(void)
{
	return __readdr(0);
}

void debugger_dr1_setValue(UINT_PTR value)
{
	__writedr(1,value);
}

UINT_PTR debugger_dr1_getValue(void)
{
	return __readdr(1);
}

void debugger_dr2_setValue(UINT_PTR value)
{
	__writedr(2,value);
}

UINT_PTR debugger_dr2_getValue(void)
{
	return __readdr(2);
}

void debugger_dr3_setValue(UINT_PTR value)
{
	__writedr(3,value);
}

UINT_PTR debugger_dr3_getValue(void)
{
	return __readdr(3);
}

void debugger_dr6_setValue(UINT_PTR value)
{
	__writedr(6,value);
}

void debugger_dr7_setValue(DebugReg7 value)
{
	UINT_PTR temp=*(UINT_PTR *)&value;		
	__writedr(7,temp);
}

void debugger_dr7_setValueDword(UINT_PTR value)
{
	__writedr(7,value);	
}

UINT_PTR debugger_dr7_getValueDword(void) //I wonder why I couldn't just typecast the DebugReg7 to a dword...
{
	return __readdr(7);
}


DebugReg7 debugger_dr7_getValue(void)
{
	UINT_PTR temp=debugger_dr7_getValueDword();
	return *(DebugReg7 *)&temp;
}

UINT_PTR debugger_dr6_getValueDword(void)
{
	return __readdr(6);
}

DebugReg6 debugger_dr6_getValue(void)
{
	UINT_PTR temp=debugger_dr6_getValueDword();
	return *(DebugReg6 *)&temp;
}



void debugger_touchDebugRegister(UINT_PTR param)
{
	DbgPrint("Touching debug register. inepilogue=\n", DebuggerState.FakedDebugRegisterState[cpunr()].inEpilogue);

	
	debugger_dr0_setValue(debugger_dr0_getValue());
	
}

void debugger_initialize(void)
{
	DbgPrint("Initializing debugger events\n");

	KeInitializeEvent(&debugger_event_WaitForContinue, SynchronizationEvent, FALSE);	
	KeInitializeEvent(&debugger_event_CanBreak, SynchronizationEvent, TRUE); //true so the first can enter
	KeInitializeEvent(&debugger_event_WaitForDebugEvent, SynchronizationEvent, FALSE);

	DbgPrint("DebuggerState.fxstate=%p\n",DebuggerState.fxstate);

}

void debugger_setInitialFakeState(void)
{	
	DbgPrint("setInitialFakeState for cpu %d\n",cpunr());
	DebuggerState.FakedDebugRegisterState[cpunr()].DR0=debugger_dr0_getValue();
	DebuggerState.FakedDebugRegisterState[cpunr()].DR1=debugger_dr1_getValue();
	DebuggerState.FakedDebugRegisterState[cpunr()].DR2=debugger_dr2_getValue();
	DebuggerState.FakedDebugRegisterState[cpunr()].DR3=debugger_dr3_getValue();
	DebuggerState.FakedDebugRegisterState[cpunr()].DR6=debugger_dr6_getValueDword();
	DebuggerState.FakedDebugRegisterState[cpunr()].DR7=debugger_dr7_getValueDword();
}

VOID debugger_initHookForCurrentCPU_DPC(IN struct _KDPC *Dpc, IN PVOID  DeferredContext, IN PVOID  SystemArgument1, IN PVOID  SystemArgument2)
{
	debugger_initHookForCurrentCPU();
}

int debugger_removeHookForCurrentCPU(UINT_PTR params)
{
	//DbgPrint("Unhooking int1 for this cpu\n");
    return inthook_UnhookInterrupt(1);	
}

int debugger_initHookForCurrentCPU(void)
/*
Must be called for each cpu
*/
{
	int result=TRUE;
	DbgPrint("Hooking int1 for cpu %d\n", cpunr());
	
	result=inthook_HookInterrupt(1,getCS() & 0xfff8, (ULONG_PTR)interrupt1_asmentry, &Int1JumpBackLocation);	

#ifdef AMD64
	if (result)
	{
		DbgPrint("hooked int1. Int1JumpBackLocation=%x:%llx\n", Int1JumpBackLocation.cs, Int1JumpBackLocation.eip);
	}
#endif

	if (DebuggerState.globalDebug)
	{
		//set the fake state
		//debugger_setInitialFakeState();
		DbgPrint("Setting GD bit for cpu %d\n",cpunr());

		debugger_dr7_setGD(1); //enable the GD flag		
	}

	if (DebuggerState.storeLBR)
	{		
		DbgPrint("Enabling LBR logging. IA32_DEBUGCTL was %x\n", __readmsr(0x1d9));
		__writemsr(0x1d9, __readmsr(0x1d9) | 1);
		DbgPrint("Enabling LBR logging. IA32_DEBUGCTL is  %x\n", __readmsr(0x1d9));
	}
		
	return result;
}

void debugger_setStoreLBR(BOOL state)
{
	if (state)
		DbgPrint("Setting storeLBR to true\n");
	else
		DbgPrint("Setting storeLBR to false\n");

	DebuggerState.storeLBR=state; //it's not THAT crucial to disable/enable it

	DebuggerState.storeLBR_max=0;

	switch (cpu_model)
    {
        case 0x2a:
        case 0x1a:
        case 0x1e:
        case 0x1f:
        case 0x2e:
        case 0x25:
        case 0x2c:
          DebuggerState.storeLBR_max=16;
          break;

        case 0x17:
        case 0x1d:
        case 0x0f:
          DebuggerState.storeLBR_max=4;
          break;

        case 0x1c:
          DebuggerState.storeLBR_max=8;
          break;
    }

	DbgPrint("Because your cpu_model=%d I think that your storeLBR_max=%d\n", cpu_model, DebuggerState.storeLBR_max);

	
}


int debugger_setGlobalDebugState(BOOL state)
//call this BEFORE debugging, if already debugging, the user must call this for each cpu
{
	DbgPrint("debugger_setGlobalDebugState(%d)\n",state);
	if (state)
	  DebuggerState.globalDebug=state; //on enable set this first

	if (inthook_isHooked(1))
	{
		int oldEpilogueState=DebuggerState.FakedDebugRegisterState[cpunr()].inEpilogue;

		DbgPrint("Int 1 is hooked,%ssetting GD\n",(state ? "":"un"));
		DbgPrint("oldEpilogueState=%d\n",oldEpilogueState);
		//debugger_setInitialFakeState();

		DebuggerState.FakedDebugRegisterState[cpunr()].inEpilogue=TRUE;
		DebuggerState.globalDebug=state;
		debugger_dr7_setGD(state);
		
		DebuggerState.FakedDebugRegisterState[cpunr()].inEpilogue=oldEpilogueState;
		

		DebuggerState.FakedDebugRegisterState[cpunr()].DR7=0x400;
		debugger_dr7_setValueDword(0x400);		

	}

	return TRUE;
}



int debugger_startDebugging(DWORD debuggedProcessID)
/*
Call this AFTER the interrupts are hooked
*/
{
	DbgPrint("debugger_startDebugging. Processid=%x\n",debuggedProcessID);
	Int1JumpBackLocation.eip=inthook_getOriginalEIP(1);
	Int1JumpBackLocation.cs=inthook_getOriginalCS(1);

#ifdef AMD64
	DbgPrint("Int1 jump back = %x:%llx\n", Int1JumpBackLocation.cs, Int1JumpBackLocation.eip);
#endif

	DebuggerState.isDebugging=TRUE;
	DebuggerState.debuggedProcessID=debuggedProcessID;

	return TRUE;
}

int debugger_stopDebugging(void)
{	
	int i;

	DbgPrint("Stopping the debugger if it is running\n");

	DebuggerState.stoppingTheDebugger=TRUE;	

	if (DebuggerState.globalDebug)
	{
		//touch the global debug for each debug processor
		DbgPrint("Touching the debug registers\n");
        forEachCpuPassive(debugger_touchDebugRegister, 0);
	}

	

    DebuggerState.globalDebug=FALSE; //stop when possible, saves speed
	DebuggerState.isDebugging=FALSE;	

	for (i=0; i<4; i++)
		DebuggerState.breakpoint[i].active=FALSE;

	//unhook all processors

	forEachCpuPassive(debugger_removeHookForCurrentCPU, 0);


	return TRUE;
}

int debugger_unsetGDBreakpoint(int breakpointnr)
{
	int result=DebuggerState.breakpoint[breakpointnr].active;
	DebuggerState.breakpoint[breakpointnr].active=FALSE;
	return result; //returns true if it was active
}

int debugger_setGDBreakpoint(int breakpointnr, ULONG_PTR Address, BreakType bt, BreakLength bl)
/*
Will register a specific breakpoint. If global debug is used it'll set this debug register accordingly
*/
{
	DbgPrint("debugger_setGDBreakpoint(%d, %x, %d, %d)\n", breakpointnr, Address, bt, bl);
	DebuggerState.breakpoint[breakpointnr].active=TRUE;
	DebuggerState.breakpoint[breakpointnr].address=Address;
	DebuggerState.breakpoint[breakpointnr].breakType=bt;
	DebuggerState.breakpoint[breakpointnr].breakLength=bl;
	return TRUE;
}

NTSTATUS debugger_waitForDebugEvent(ULONG timeout)
{
	NTSTATUS r;
	LARGE_INTEGER wait;

	//DbgPrint("debugger_waitForDebugEvent with timeout of %d\n",timeout);

	//-10000000LL=1 second
	//-10000LL should be 1 millisecond
	//-10000LL
	wait.QuadPart=-10000LL * timeout;

	if (timeout==0xffffffff) //infinite wait
	  r=KeWaitForSingleObject(&debugger_event_WaitForDebugEvent, UserRequest, KernelMode, TRUE, NULL);
	else
	  r=KeWaitForSingleObject(&debugger_event_WaitForDebugEvent, UserRequest, KernelMode, TRUE, &wait);

	if (r==STATUS_SUCCESS)
		return r;
	else
		return STATUS_UNSUCCESSFUL;	
}

NTSTATUS debugger_continueDebugEvent(BOOL handled)
/*
Only call this by one thread only, and only when there's actually a debug eevnt in progress
*/
{
	DbgPrint("debugger_continueDebugEvent\n");
	
	DebuggerState.handledlastevent=handled;
	KeSetEvent(&debugger_event_WaitForContinue, 0,FALSE);

	return STATUS_SUCCESS;
}

UINT_PTR *debugger_getLastStackPointer(void)
{
	
	return DebuggerState.LastStackPointer;
}


NTSTATUS debugger_getDebuggerState(PDebugStackState state)
{
	DbgPrint("debugger_getDebuggerState\n");
	state->threadid=(UINT64)DebuggerState.LastThreadID;
	state->causedbydbvm = (UINT64)DebuggerState.CausedByDBVM;
	if (DebuggerState.LastStackPointer)
	{
		state->rflags=(UINT_PTR)DebuggerState.LastStackPointer[si_eflags];
		state->rax=DebuggerState.LastStackPointer[si_eax];
		state->rbx=DebuggerState.LastStackPointer[si_ebx];
		state->rcx=DebuggerState.LastStackPointer[si_ecx];
		state->rdx=DebuggerState.LastStackPointer[si_edx];
		state->rsi=DebuggerState.LastStackPointer[si_esi];
		state->rdi=DebuggerState.LastStackPointer[si_edi];
		state->rbp=DebuggerState.LastStackPointer[si_ebp];

		

	#ifdef AMD64
		//fill in the extra registers
		state->r8=DebuggerState.LastStackPointer[si_r8];
		state->r9=DebuggerState.LastStackPointer[si_r9];
		state->r10=DebuggerState.LastStackPointer[si_r10];
		state->r11=DebuggerState.LastStackPointer[si_r11];
		state->r12=DebuggerState.LastStackPointer[si_r12];
		state->r13=DebuggerState.LastStackPointer[si_r13];
		state->r14=DebuggerState.LastStackPointer[si_r14];
		state->r15=DebuggerState.LastStackPointer[si_r15];	
	#endif

		
		memcpy(state->fxstate, (void *)DebuggerState.fxstate,512);


		//generally speaking, NOTHING should touch the esp register, but i'll provide it anyhow
		if ((DebuggerState.LastStackPointer[si_cs] & 3) == 3) //if usermode code segment
		{
			//priv level change, so the stack info was pushed as well
			state->rsp=DebuggerState.LastStackPointer[si_esp]; 
			state->ss=DebuggerState.LastStackPointer[si_ss];

		}
		else
		{
			//kernelmode stack, yeah, it's really useless here since changing it here only means certain doom, but hey...
			state->rsp=(UINT_PTR)(DebuggerState.LastStackPointer)-4;
			state->ss=getSS();; //unchangeble by the user
		}

		
		state->rip=DebuggerState.LastStackPointer[si_eip];
		state->cs=DebuggerState.LastStackPointer[si_cs];
		state->ds=DebuggerState.LastStackPointer[si_ds];
		state->es=DebuggerState.LastStackPointer[si_es];
	#ifdef AMD64
		state->fs=0;
		state->gs=0;
	#else
		state->fs=DebuggerState.LastStackPointer[si_fs];
		state->gs=DebuggerState.LastStackPointer[si_gs];
	#endif

		state->dr0=DebuggerState.LastRealDebugRegisters[0];
		state->dr1=DebuggerState.LastRealDebugRegisters[1];
		state->dr2=DebuggerState.LastRealDebugRegisters[2];
		state->dr3=DebuggerState.LastRealDebugRegisters[3];
		state->dr6=DebuggerState.LastRealDebugRegisters[4];
		state->dr7=DebuggerState.LastRealDebugRegisters[5];

		if (DebuggerState.storeLBR)
		{
			DbgPrint("Copying the LBR stack to usermode\n");
			DbgPrint("storeLBR_max=%d\n", DebuggerState.storeLBR_max);

		
			for (state->LBR_Count=0; state->LBR_Count<DebuggerState.storeLBR_max; state->LBR_Count++ )
			{
				DbgPrint("DebuggerState.LastLBRStack[%d]=%x\n", state->LBR_Count, DebuggerState.LastLBRStack[state->LBR_Count]);
				state->LBR[state->LBR_Count]=DebuggerState.LastLBRStack[state->LBR_Count];
				if (state->LBR[state->LBR_Count]==0) //no need to copy once a 0 has been reached
					break;				
			}
		}
		else
			state->LBR_Count=0;


		return STATUS_SUCCESS;
	}
	else
	{
		DbgPrint("debugger_getDebuggerState was called while DebuggerState.LastStackPointer was still NULL");
		return STATUS_UNSUCCESSFUL;
	}
}

NTSTATUS debugger_setDebuggerState(PDebugStackState state)
{
	if (DebuggerState.LastStackPointer)
	{
		DebuggerState.LastStackPointer[si_eflags]=(UINT_PTR)state->rflags;

		DbgPrint("have set eflags to %x\n",DebuggerState.LastStackPointer[si_eflags]);


		DebuggerState.LastStackPointer[si_eax]=(UINT_PTR)state->rax;
		DebuggerState.LastStackPointer[si_ebx]=(UINT_PTR)state->rbx;
		DebuggerState.LastStackPointer[si_ecx]=(UINT_PTR)state->rcx;
		DebuggerState.LastStackPointer[si_edx]=(UINT_PTR)state->rdx;
		
		DebuggerState.LastStackPointer[si_esi]=(UINT_PTR)state->rsi;
		DebuggerState.LastStackPointer[si_edi]=(UINT_PTR)state->rdi;
		
		DebuggerState.LastStackPointer[si_ebp]=(UINT_PTR)state->rbp;

		//generally speaking, NOTHING should touch the esp register, but i'll provide it anyhow
		if ((DebuggerState.LastStackPointer[si_cs] & 3) == 3) //if usermode code segment
		{
			//priv level change, so the stack info was pushed as well
			DebuggerState.LastStackPointer[si_esp]=(UINT_PTR)state->rsp;
			//don't mess with ss
		}
		else
		{
			//no change in kernelmode allowed		
		}

		
		DebuggerState.LastStackPointer[si_eip]=(UINT_PTR)state->rip;
		DebuggerState.LastStackPointer[si_cs]=(UINT_PTR)state->cs;
		DebuggerState.LastStackPointer[si_ds]=(UINT_PTR)state->ds;
		DebuggerState.LastStackPointer[si_es]=(UINT_PTR)state->es;
	#ifndef AMD64
		DebuggerState.LastStackPointer[si_fs]=(UINT_PTR)state->fs;
		DebuggerState.LastStackPointer[si_gs]=(UINT_PTR)state->gs;
	#else //don't touch fs or gs in 64-bit
		DebuggerState.LastStackPointer[si_r8]=(UINT_PTR)state->r8;
		DebuggerState.LastStackPointer[si_r9]=(UINT_PTR)state->r9;
		DebuggerState.LastStackPointer[si_r10]=(UINT_PTR)state->r10;
		DebuggerState.LastStackPointer[si_r11]=(UINT_PTR)state->r11;
		DebuggerState.LastStackPointer[si_r12]=(UINT_PTR)state->r12;
		DebuggerState.LastStackPointer[si_r13]=(UINT_PTR)state->r13;
		DebuggerState.LastStackPointer[si_r14]=(UINT_PTR)state->r14;
		DebuggerState.LastStackPointer[si_r15]=(UINT_PTR)state->r15;
	#endif

		if (!DebuggerState.globalDebug)
		{
			//no idea why someone would want to use this method, but it's in (for NON globaldebug only)

			//updating this array too just so the user can see it got executed. (it eases their state of mind...)
			DebuggerState.LastRealDebugRegisters[0]=(UINT_PTR)state->dr0; 
			DebuggerState.LastRealDebugRegisters[1]=(UINT_PTR)state->dr1;
			DebuggerState.LastRealDebugRegisters[2]=(UINT_PTR)state->dr2;
			DebuggerState.LastRealDebugRegisters[3]=(UINT_PTR)state->dr3;
			DebuggerState.LastRealDebugRegisters[4]=(UINT_PTR)state->dr6;
			DebuggerState.LastRealDebugRegisters[5]=(UINT_PTR)state->dr7;

			//no setting of the DebugRegs here

		}
	}
	else
	{
		DbgPrint("debugger_setDebuggerState was called while DebuggerState.LastStackPointer was still NULL");
		return STATUS_UNSUCCESSFUL;
	}

	return STATUS_SUCCESS;
}

int breakpointHandler_kernel(UINT_PTR *stackpointer, UINT_PTR *currentdebugregs, UINT_PTR *LBR_Stack, int causedbyDBVM)
//Notice: This routine is called when interrupts are enabled and the GD bit has been set if globaL DEBUGGING HAS BEEN USED
//Interrupts are enabled and should be at passive level, so taskswitching is possible
{
	NTSTATUS r=STATUS_UNSUCCESSFUL;
	int handled=0; //0 means let the OS handle it
	LARGE_INTEGER timeout;
	timeout.QuadPart=-100000;
	
	
	
	//DbgPrint("breakpointHandler for kernel breakpoints\n");

#ifdef AMD64
	DbgPrint("cs=%x ss=%x ds=%x es=%x fs=%x gs=%x\n",getCS(), getSS(), getDS(), getES(), getFS(), getGS());

	DbgPrint("fsbase=%llx gsbase=%llx gskernel=%llx\n", readMSR(0xc0000100), readMSR(0xc0000101), readMSR(0xc0000102));

	DbgPrint("rbp=%llx\n", getRBP());

	DbgPrint("gs:188=%llx\n", __readgsqword(0x188));
	DbgPrint("causedbyDBVM=%d\n", causedbyDBVM);
#endif
	
	if (KeGetCurrentIrql()==0)
	{
		//crititical section here
		if ((stackpointer[si_cs] & 3)==0)
		{
			DbgPrint("Going to wait in a kernelmode routine\n");
		}

	
		//block other threads from breaking until this one has been handled
		while (r != STATUS_SUCCESS)
		{
			r=KeWaitForSingleObject(&debugger_event_CanBreak,Executive, KernelMode, FALSE, NULL);
			//check r and handle specific events

			DbgPrint("Woke up. r=%x\n",r);
				
		}

		if ((stackpointer[si_cs] & 3)==0)
		{
			DbgPrint("Woke up in a kernelmode routine\n");
		}
		

		//We're here, let's notify the usermode debugger of our situation
		//first store the stackpointer so it can be manipulated externally
		DebuggerState.LastStackPointer=stackpointer;
		DebuggerState.LastRealDebugRegisters=currentdebugregs;		
		DebuggerState.LastLBRStack=LBR_Stack;
		DebuggerState.LastThreadID=PsGetCurrentThreadId();
		DebuggerState.CausedByDBVM = causedbyDBVM;
		

#ifdef AMD64
		_fxsave(DebuggerState.fxstate);
#else
		__asm
		{
			fxsave [DebuggerState.fxstate]
		}
#endif
 


		//notify usermode app that this thread has halted due to a debug event
		
		KeSetEvent(&debugger_event_WaitForDebugEvent,0,FALSE);


		//wait for event from usermode that debgu event has been handled
		//KeWaitForSingleObject();
		//continue with state

		while (1)
		{


			//LARGE_INTEGER wt;
			NTSTATUS s=STATUS_UNSUCCESSFUL;
			
			//wt.QuadPart=-10000000LL; 
			//s=KeDelayExecutionThread(KernelMode, FALSE, &wt);

			DbgPrint("Waiting...\n");


			while (s != STATUS_SUCCESS)
			{
				s=KeWaitForSingleObject(&debugger_event_WaitForContinue, Executive, KernelMode, FALSE, NULL);
				DbgPrint("KeWaitForSingleObject=%x\n",s);		
			}

			

			if (s==STATUS_SUCCESS)
			{
				if (DebuggerState.handledlastevent)
				{
					//DbgPrint("handledlastevent=TRUE");
					handled=1;
				}
				else
					handled=0;

				break;
			}
				
		}


		DebuggerState.LastStackPointer=NULL; //NULL the stackpointer so routines know it should not be called

		//i'm done, let other threads catch it
		KeSetEvent(&debugger_event_CanBreak, 0, FALSE);

		DbgPrint("Returning after a wait. handled=%d and eflags=%x\n",handled, stackpointer[si_eflags]);
		if ((stackpointer[si_cs] & 3)==0) //check rpl of cs
		{
			DbgPrint("and in kernelmode\n");
		}

		return handled;
	}
	else
	{
		DbgPrint("Breakpoint wasn't at passive level. Screw this, i'm not going to break here\n");
		
		return 1;
	}

}

int interrupt1_handler(UINT_PTR *stackpointer, UINT_PTR *currentdebugregs)
{
	HANDLE CurrentProcessID=PsGetCurrentProcessId();	
	UINT_PTR originaldr6=currentdebugregs[4];
	DebugReg6 _dr6=*(DebugReg6 *)&currentdebugregs[4];

	UINT_PTR LBR_Stack[16]; //max 16
//	DebugReg7 _dr7=*(DebugReg7 *)&currentdebugregs[5];

	int causedbyDBVM = vmxusable && vmx_causedCurrentDebugBreak();

	if (cpu_familyID==0x6)
	{
		if (DebuggerState.storeLBR)
		{
			//fetch the lbr stack
			int MSR_LASTBRANCH_TOS=0x1c9;
			int MSR_LASTBRANCH_0=0x40;

			int i;
			int count;

			

			i=(int)__readmsr(MSR_LASTBRANCH_TOS);
			count=0;
			while (count<DebuggerState.storeLBR_max)
			{
				UINT64 x;
				x=__readmsr(MSR_LASTBRANCH_0+i);
				LBR_Stack[count]=(UINT_PTR)x;
				__writemsr(MSR_LASTBRANCH_0+i,0); //it has been read out, so can be erased now

				count++;
				i++;
				i=i % DebuggerState.storeLBR_max;				
			}
		}
	}
	

	DbgPrint("interrupt1_handler. DR6=%x (%x)\n", originaldr6, debugger_dr6_getValueDword());
	
	//check if this break should be handled or not
	
	if (DebuggerState.globalDebug)
	{
		//DbgPrint("DebuggerState.globalDebug=TRUE\n");
		//global debugging is being used
		if (_dr6.BD)
		{
			//The debug registers are being accessed, emulate it with DebuggerState.FakedDebugRegisterState[cpunr()].DRx
			int instructionPointer;
#ifdef AMD64
			int prefixpointer;
#endif
			int currentcpunr=cpunr();
			int debugregister;
			int generalpurposeregister;
			unsigned char *instruction=(unsigned char *)stackpointer[si_eip];

			//unset this flag in DR6
			_dr6.BD=0;
			debugger_dr6_setValue(*(UINT_PTR *)&_dr6);

			if (DebuggerState.FakedDebugRegisterState[cpunr()].inEpilogue)
			{					
				((EFLAGS *)&stackpointer[si_eflags])->RF=1; //repeat this instruction and don't break
				return 2;
			}

		
			//DbgPrint("handler: Setting fake dr6 to %x\n",*(UINT_PTR *)&_dr6);
			
			DebuggerState.FakedDebugRegisterState[cpunr()].DR6=*(UINT_PTR *)&_dr6;

			for (instructionPointer=0; instruction[instructionPointer] != 0x0f; instructionPointer++) ; //find the start of the instruction, skipping prefixes etc...
			
			//we now have the start of the instruction.
			//Find out which instruction it is, and which register is used
			debugregister=(instruction[instructionPointer+2] >> 3) & 7;	
			generalpurposeregister=instruction[instructionPointer+2] & 7;

#ifdef AMD64
			for (prefixpointer=0; prefixpointer<instructionPointer; prefixpointer++)
			{
				//check for a REX.B prefix  (0x40  + 0x1 : 0x41)
				if ((instruction[prefixpointer] & 0x41) == 0x41)
				{
					//rex.b prefix is used, r8 to r15 are being accessed
					generalpurposeregister+=8;
				}
			}

#endif

			//DbgPrint("debugregister=%d, generalpurposeregister=%d\n",debugregister,generalpurposeregister); 

			if (instruction[instructionPointer+1]==0x21)
			{
				UINT_PTR drvalue=0;
				//DbgPrint("read opperation\n");
				//21=read
				switch (debugregister)
				{
					case 0: 
						
						drvalue=DebuggerState.FakedDebugRegisterState[cpunr()].DR0;
						//DbgPrint("Reading DR0 (returning %x real %x)\n", drvalue, currentdebugregs[0]); 
						break;

					case 1: 
						drvalue=DebuggerState.FakedDebugRegisterState[cpunr()].DR1;
						break;

					case 2: 
						drvalue=DebuggerState.FakedDebugRegisterState[cpunr()].DR2;
						break;

					case 3: 
						drvalue=DebuggerState.FakedDebugRegisterState[cpunr()].DR3;
						break;

					case 4: 
					case 6:
						drvalue=DebuggerState.FakedDebugRegisterState[cpunr()].DR6;
						//DbgPrint("reading dr6 value:%x\n",drvalue);
						break;

					case 5: 
					case 7:
						drvalue=DebuggerState.FakedDebugRegisterState[cpunr()].DR7;						
						break;

					default:
						DbgPrint("Invalid debugregister\n");
						drvalue = 0;
						break;
				}

				switch (generalpurposeregister)
				{
					case 0:
						stackpointer[si_eax]=drvalue;
						break;

					case 1:
						stackpointer[si_ecx]=drvalue;
						break;

					case 2:
						stackpointer[si_edx]=drvalue;
						break;

					case 3:
						stackpointer[si_ebx]=drvalue;
						break;

					case 4:				
						if ((stackpointer[si_cs] & 3) == 3)  //usermode dr access ?
							stackpointer[si_esp]=drvalue;
						else
							stackpointer[si_stack_esp]=drvalue;

						break;

					case 5:
						stackpointer[si_ebp]=drvalue;
						break;

					case 6:
						stackpointer[si_esi]=drvalue;
						break;

					case 7:
						stackpointer[si_edi]=drvalue;
						break;

#ifdef AMD64
					case 8:
						stackpointer[si_r8]=drvalue;
						break;

					case 9:
						stackpointer[si_r9]=drvalue;
						break;

					case 10:
						stackpointer[si_r10]=drvalue;
						break;

					case 11:
						stackpointer[si_r11]=drvalue;
						break;

					case 12:
						stackpointer[si_r12]=drvalue;
						break;

					case 13:
						stackpointer[si_r13]=drvalue;
						break;

					case 14:
						stackpointer[si_r14]=drvalue;
						break;

					case 15:
						stackpointer[si_r15]=drvalue;
						break;


#endif
				}

			}
			else 
			if (instruction[instructionPointer+1]==0x23)
			{
				//23=write
				UINT_PTR gpvalue=0;
				//DbgPrint("Write operation\n");
				switch (generalpurposeregister)
				{
					case 0:
						gpvalue=stackpointer[si_eax];
						break;

					case 1:
						gpvalue=stackpointer[si_ecx];
						break;

					case 2:
						gpvalue=stackpointer[si_edx];
						break;

					case 3:
						gpvalue=stackpointer[si_ebx];
						break;

					case 4:
						if ((stackpointer[si_cs] & 3) == 3)
							gpvalue=stackpointer[si_esp];

						break;

					case 5:
						gpvalue=stackpointer[si_ebp];
						break;

					case 6:
						gpvalue=stackpointer[si_esi];
						break;

					case 7:
						gpvalue=stackpointer[si_edi];
						break;
#ifdef AMD64
					case 8:
						gpvalue=stackpointer[si_r8];
						break;

					case 9:
						gpvalue=stackpointer[si_r9];
						break;

					case 10:
						gpvalue=stackpointer[si_r10];
						break;

					case 11:
						gpvalue=stackpointer[si_r11];
						break;

					case 12:
						gpvalue=stackpointer[si_r12];
						break;

					case 13:
						gpvalue=stackpointer[si_r13];
						break;

					case 14:
						gpvalue=stackpointer[si_r14];
						break;

					case 15:
						gpvalue=stackpointer[si_r15];
						break;

					default:
						DbgPrint("Invalid register value\n");
						break;
#endif
				}

				//gpvalue now contains the value to set the debug register
				switch (debugregister)
				{
					case 0: 	
						//DbgPrint("Writing DR0. Original value=%x new value=%x\n", currentdebugregs[0], gpvalue);
						debugger_dr0_setValue(gpvalue);
						DebuggerState.FakedDebugRegisterState[cpunr()].DR0=debugger_dr0_getValue();
						break;

					case 1: 						
						debugger_dr1_setValue(gpvalue);
						DebuggerState.FakedDebugRegisterState[cpunr()].DR1=debugger_dr1_getValue();
						break;

					case 2: 						
						debugger_dr2_setValue(gpvalue);
						DebuggerState.FakedDebugRegisterState[cpunr()].DR2=debugger_dr2_getValue();
						break;

					case 3: 						
						debugger_dr3_setValue(gpvalue);
						DebuggerState.FakedDebugRegisterState[cpunr()].DR3=debugger_dr3_getValue();
						break;

					case 4: 
					case 6:						
						//DbgPrint("Setting dr6 to %x (was %x)\n", gpvalue, DebuggerState.FakedDebugRegisterState[cpunr()].DR6);
						debugger_dr6_setValue(gpvalue);						
						DebuggerState.FakedDebugRegisterState[cpunr()].DR6=debugger_dr6_getValueDword();
						break;

					case 5: 
					case 7:
						//make sure it doesn't set the GD flag here
						
						if (DebuggerState.FakedDebugRegisterState[cpunr()].inEpilogue)
						{
						//	DbgPrint("Was in epilogue\n");
						}

						//check for invalid bits and raise a GPF if invalid


						gpvalue=(gpvalue | 0x400) & (~(1<<13)); //unset the GD value

						//gpvalue=0xf0401;
						debugger_dr7_setValueDword(gpvalue);

						DebuggerState.FakedDebugRegisterState[cpunr()].DR7=debugger_dr7_getValueDword();
						
						break;
				}



			}
			else 
			{
				//DbgPrint("Some unknown instruction accessed the debug registers?\n");
				//if (CurrentProcessID==(HANDLE)(UINT_PTR)DebuggerState.debuggedProcessID)
				//	DbgPrint("Happened inside the target process\n");

				//DbgPrint("interrupt1_handler dr6=%x (original=%x) dr7=%d\n",_dr6, originaldr6, _dr7);
				//DbgPrint("eip=%x\n",stackpointer[si_eip]);
			}

			//adjust eip to after this instruction
			stackpointer[si_eip]+=instructionPointer+3; //0f xx /r

			return 1; //don't tell windows about it
		}
	}

	if (DebuggerState.isSteppingTillClear) //this doesn't really work because when the state comes back to interruptable the system has a critical section lock on the GUI, so yeah... I really need a DBVM display driver for this
	{
	
		if ((((PEFLAGS)&stackpointer[si_eflags])->IF == 0) || (KeGetCurrentIrql() != PASSIVE_LEVEL))
		{
			((PEFLAGS)&stackpointer[si_eflags])->TF = 1;
			((PEFLAGS)&stackpointer[si_eflags])->RF = 1;
			debugger_dr6_setValue(0xffff0ff0);
			return 1;
		}

		DebuggerState.isSteppingTillClear = FALSE;	
	}

	
	if (DebuggerState.isDebugging)
	{
		DbgPrint("DebuggerState.isDebugging\n");
		//check if this should break
		if (CurrentProcessID==(HANDLE)(UINT_PTR)DebuggerState.debuggedProcessID)
		{	
			UINT_PTR originaldebugregs[6];
			UINT64 oldDR7=getDR7();


			if ((((PEFLAGS)&stackpointer[si_eflags])->IF==0) || (KeGetCurrentIrql() != PASSIVE_LEVEL))
			{
				//There's no way to display the state to the usermode part of CE
				DbgPrint("int1 at unstoppable location");
				if (!KernelCodeStepping)
				{
					((PEFLAGS)&stackpointer[si_eflags])->TF = 0; //just give up stepping
					DbgPrint("Quitting this");
				}
				else
				{
					DbgPrint("Stepping until valid\n");
					((PEFLAGS)&stackpointer[si_eflags])->TF = 1; //keep going until a valid state
					DebuggerState.isSteppingTillClear = TRUE; //Just in case a taskswitch happens right after enabling passive level with interrupts
				}

				((PEFLAGS)&stackpointer[si_eflags])->RF=1;
				debugger_dr6_setValue(0xffff0ff0);
				return 1;
			}

			DebuggerState.isSteppingTillClear = FALSE;
	


			//DbgPrint("CurrentProcessID==(HANDLE)(UINT_PTR)DebuggerState.debuggedProcessID\n");

			if (DebuggerState.globalDebug)
			{
				originaldebugregs[0]=DebuggerState.FakedDebugRegisterState[cpunr()].DR0;
				originaldebugregs[1]=DebuggerState.FakedDebugRegisterState[cpunr()].DR1;
				originaldebugregs[2]=DebuggerState.FakedDebugRegisterState[cpunr()].DR2;
				originaldebugregs[3]=DebuggerState.FakedDebugRegisterState[cpunr()].DR3;
				originaldebugregs[4]=DebuggerState.FakedDebugRegisterState[cpunr()].DR6;
				originaldebugregs[5]=DebuggerState.FakedDebugRegisterState[cpunr()].DR7;
			}

			//DbgPrint("BP in target process\n");
			
			//no extra checks if it's caused by the debugger or not. That is now done in the usermode part
			//if (*(PEFLAGS)(&stackpointer[si_eflags]).IF)	
/*
			if (((PEFLAGS)&stackpointer[si_eflags])->IF==0)
			{
				//DbgPrint("Breakpoint while interrupts are disabled: %x\n",stackpointer[si_eip]);
				//An breakpoint happened while IF was 0. Step through the code untill IF is 1
				((PEFLAGS)&stackpointer[si_eflags])->RF=1;
				((PEFLAGS)&stackpointer[si_eflags])->TF=1; //keep going until IF=1
				DbgPrint("IF==0\n");
				return 1; //don't handle it but also don't tell windows
			}*/

			//set the real debug registers to what it is according to the guest (so taskswitches take over these values) .... shouldn't be needed as global debug is on which fakes that read...

			

			if (DebuggerState.globalDebug)
			{
				//enable the GD flag for taskswitches that will occur as soon as interrupts are enabled
				//this also means: DO NOT EDIT THE DEBUG REGISTERS IN GLOBAL DEBUG MODE at this point. Only in the epilogue

				if (!DebuggerState.stoppingTheDebugger) //This is set when the driver is unloading. So do NOT set it back then
					debugger_dr7_setGD(DebuggerState.globalDebug); 
			}	
			else
			{
				//unset ALL debug registers before enabling taskswitching. Just re-enable it when back when interrupts are disabled again
				debugger_dr7_setValueDword(0x400);
				debugger_dr0_setValue(0);
				debugger_dr1_setValue(0);
				debugger_dr2_setValue(0);
				debugger_dr3_setValue(0);
				debugger_dr6_setValue(0xffff0ff0);
			}



			//start the windows taskswitching mode

			enableInterrupts();
			{
				int rs=1;
				//DbgPrint("calling breakpointHandler_kernel\n");
				
				rs=breakpointHandler_kernel(stackpointer, currentdebugregs, LBR_Stack, causedbyDBVM);
				//DbgPrint("After handler\n");

				//DbgPrint("rs=%d\n",rs);


				disableInterrupts();


				//we might be on a different CPU now
				if (DebuggerState.globalDebug)
				{
					DebuggerState.FakedDebugRegisterState[cpunr()].DR0=originaldebugregs[0];
					DebuggerState.FakedDebugRegisterState[cpunr()].DR1=originaldebugregs[1];
					DebuggerState.FakedDebugRegisterState[cpunr()].DR2=originaldebugregs[2];
					DebuggerState.FakedDebugRegisterState[cpunr()].DR3=originaldebugregs[3];
					DebuggerState.FakedDebugRegisterState[cpunr()].DR6=originaldebugregs[4];
					DebuggerState.FakedDebugRegisterState[cpunr()].DR7=originaldebugregs[5];
				}
				else
				{
					
					/*if (getDR7() != oldDR7)
					{
						DbgPrint("Something changed DR7. old=%llx new=%llx\n",oldDR7, getDR7());
					}*/

					
					//set the debugregisters to what they where set to before taskswitching was enable
					//with global debug this is done elsewhere
					debugger_dr0_setValue(currentdebugregs[0]);
					debugger_dr1_setValue(currentdebugregs[1]);
					debugger_dr2_setValue(currentdebugregs[2]);
					debugger_dr3_setValue(currentdebugregs[3]);
					debugger_dr6_setValue(currentdebugregs[4]);

					if ((currentdebugregs[5] >> 13) & 1)
					{
						DbgPrint("WTF? GD is 1 in currentdebugregs[5]: %llx\n", currentdebugregs[5]);
					}
					else
						debugger_dr7_setValue(*(DebugReg7 *)&currentdebugregs[5]);	
						
				}
				
				return rs;
			}
		}
		else 
		{
			DbgPrint("Not the debugged process (%x != %x)\n",CurrentProcessID,DebuggerState.debuggedProcessID );
			//check if this break is due to a breakpoint ce has set. (during global debug threadsurfing))
			//do that by checking if the breakpoint condition exists in the FAKE dr7 registers
			//if so, let windows handle it, if not, it is caused by ce, which then means, skip (so execute normally)

			if (DebuggerState.globalDebug)
			{			
				DebugReg6 dr6=debugger_dr6_getValue();
				DebugReg7 dr7=*(DebugReg7 *)&DebuggerState.FakedDebugRegisterState[cpunr()].DR7;

				//real dr6		//fake dr7
				if ((dr6.B0) && (!(dr7.L0 || dr7.G0))) { /*DbgPrint("setting RF because of B0\n");*/ ((PEFLAGS)&stackpointer[si_eflags])->RF=1; return 1; } //break caused by DR0 and not expected by the current process, ignore this bp and continue
				if ((dr6.B1) && (!(dr7.L1 || dr7.G1))) { /*DbgPrint("setting RF because of B1\n");*/ ((PEFLAGS)&stackpointer[si_eflags])->RF=1; return 1; } //		...		DR1		...
				if ((dr6.B2) && (!(dr7.L2 || dr7.G2))) { /*DbgPrint("setting RF because of B2\n");*/ ((PEFLAGS)&stackpointer[si_eflags])->RF=1; return 1; }  //		...		DR2		...
				if ((dr6.B3) && (!(dr7.L3 || dr7.G3))) { /*DbgPrint("setting RF because of B3\n");*/ ((PEFLAGS)&stackpointer[si_eflags])->RF=1; return 1; }  //		...		DR3		...
			}

			if (causedbyDBVM)
				return 1; //correct PA, bad PID, ignore BP

			if (DebuggerState.isSteppingTillClear) //shouldn't happen often
			{
				DbgPrint("That thing that shouldn\'t happen often happened\n");
				((PEFLAGS)&stackpointer[si_eflags])->TF = 0;

				DebuggerState.isSteppingTillClear = 0;
				return 1; //ignore
			}

			DbgPrint("Returning unhandled. DR6=%x", debugger_dr6_getValueDword());
			
			return 0; //still here, so let windows handle it

		}
	}
	else
		return 0; //Let windows handle it

	//get the current processid
	//is it being debugged
	//if yes, check if the breakpoint is something done by me
	//if no, exit
	
}

int interrupt1_centry(UINT_PTR *stackpointer) //code segment 8 has a 32-bit stackpointer
{
	UINT_PTR before;//,after;
	UINT_PTR currentdebugregs[6]; //used for determining if the current bp is caused by the debugger ot not
	int handled=0; //if 0 at return, the interupt will be passed down to the operating system

	//DbgPrint("interrupt1_centry cpunr=%d esp=%x\n",cpunr(), getRSP());

	//bsod crashfix, but also disables kernelmode stepping




	before=getRSP();

	//Fetch current debug registers
	currentdebugregs[0]=debugger_dr0_getValue();
	currentdebugregs[1]=debugger_dr1_getValue();
	currentdebugregs[2]=debugger_dr2_getValue();
	currentdebugregs[3]=debugger_dr3_getValue();
	currentdebugregs[4]=debugger_dr6_getValueDword();
	currentdebugregs[5]=debugger_dr7_getValueDword();


	handled=interrupt1_handler(stackpointer, currentdebugregs);

	//epilogue:
	//At the end when returning:
	
	

	//
	//--------------------------------------------------------------------------
	//--------------EPILOGUE (AFTER HAVING HANDLED THE BREAKPOINT)--------------
	//--------------------------------------------------------------------------
	//
	
	
	disableInterrupts(); //just making sure..	


	DebuggerState.FakedDebugRegisterState[cpunr()].inEpilogue=1;
	debugger_dr7_setGD(0); //make sure the GD bit is disabled (int1 within int1, oooh the fun..., and yes, THIS itself will cause one too)
	DebuggerState.FakedDebugRegisterState[cpunr()].inEpilogue=1; //just be sure...


	if (inthook_isDBVMHook(1))
	{
		//update the int1 return address, could have been changed
		IDT idt;	
		GetIDT(&idt);

		//DbgPrint("This was a dbvm hook. Changing if the interrupt return address is still valid\n");

		Int1JumpBackLocation.cs=idt.vector[1].wSelector;
		Int1JumpBackLocation.eip=idt.vector[1].wLowOffset+(idt.vector[1].wHighOffset << 16);
#ifdef AMD64
		Int1JumpBackLocation.eip+=((UINT64)idt.vector[1].TopOffset << 32);		
#endif
	}
	

	if (DebuggerState.globalDebug) //DR's are only accesses when there are DR's(no idea how it handles breakpoints in a different process...), so set them in each thread even those that don't belong original: && (PsGetCurrentProcessId()==(HANDLE)DebuggerState.debuggedProcessID))
	{
		//set the breakpoint in this thread. 		
        DebugReg6 dr6=debugger_dr6_getValue();
		//DebugReg7 dr7=debugger_dr7_getValue();

		DebugReg6 _dr6=*(DebugReg6 *)&DebuggerState.FakedDebugRegisterState[cpunr()].DR6;
        DebugReg7 _dr7=*(DebugReg7 *)&DebuggerState.FakedDebugRegisterState[cpunr()].DR7;
		int debugregister=0, breakpoint=0;
		


        //first clear the DR6 bits caused by the debugger

		if (!handled)
		{
			//it's going to get sent to windows
			if (dr6.BD && _dr7.GD) _dr6.BD=1; //should already have been done, but what the heck...
			if (dr6.B0 && (_dr7.L0 || _dr7.G0)) _dr6.B0=1;
			if (dr6.B1 && (_dr7.L1 || _dr7.G1)) _dr6.B1=1;
			if (dr6.B2 && (_dr7.L2 || _dr7.G2)) _dr6.B2=1;
			if (dr6.B3 && (_dr7.L3 || _dr7.G3)) _dr6.B3=1;

			_dr6.BS=dr6.BS;
			_dr6.BT=dr6.BT;
			//DbgPrint("epilogue: Setting fake dr6 to %x (fake=%x)\n",*(DWORD *)&dr6, *(DWORD *)&_dr6);
		}
		

		debugger_dr6_setValue(0xffff0ff0);

		
		//set the debug registers of active breakpoints. Doesn't have to be in the specified order. Just find an unused debug registers
		//check DebuggerState.FakedDebugRegisterState[cpunumber].DR7 for unused breakpoints

		//set state to what the guest thinks it is
		debugger_dr0_setValue(DebuggerState.FakedDebugRegisterState[cpunr()].DR0);
		debugger_dr1_setValue(DebuggerState.FakedDebugRegisterState[cpunr()].DR1);
		debugger_dr2_setValue(DebuggerState.FakedDebugRegisterState[cpunr()].DR2);
		debugger_dr3_setValue(DebuggerState.FakedDebugRegisterState[cpunr()].DR3);
		debugger_dr6_setValue(DebuggerState.FakedDebugRegisterState[cpunr()].DR6);

		
		
		for (breakpoint=0; breakpoint<4; breakpoint++)
		{
			
			if (DebuggerState.breakpoint[breakpoint].active)
			{
				
				int foundone=0;
			//	DbgPrint("Want to set breakpoint %d\n",breakpoint);
			
				

				//find a usable debugregister
				while ((debugregister<4) && (foundone==0))				
				{
				
					if (DebuggerState.FakedDebugRegisterState[cpunr()].inEpilogue==0)
					{
						DebuggerState.FakedDebugRegisterState[cpunr()].inEpilogue=1;
					}
					

					//check if this debugregister is usable
					if (((DebuggerState.FakedDebugRegisterState[cpunr()].DR7 >> (debugregister*2)) & 3)==0)  //DR7.Gx and DR7.Lx are 0
					{
					  //  DbgPrint("debugregister %d is free to be used\n",debugregister);
						foundone=1;
						
						//set address
						switch (debugregister)
						{
							case 0:	
								debugger_dr0_setValue(DebuggerState.breakpoint[breakpoint].address);
								_dr7.L0=1;
								_dr7.LEN0=DebuggerState.breakpoint[breakpoint].breakLength;
								_dr7.RW0=DebuggerState.breakpoint[breakpoint].breakType;
								break;

							case 1:
								debugger_dr1_setValue(DebuggerState.breakpoint[breakpoint].address);
								_dr7.L1=1;
								_dr7.LEN1=DebuggerState.breakpoint[breakpoint].breakLength;
								_dr7.RW1=DebuggerState.breakpoint[breakpoint].breakType;
								break;

							case 2:
								debugger_dr2_setValue(DebuggerState.breakpoint[breakpoint].address);
								_dr7.L2=1;
								_dr7.LEN2=DebuggerState.breakpoint[breakpoint].breakLength;
								_dr7.RW2=DebuggerState.breakpoint[breakpoint].breakType;
								break;

							case 3:
								debugger_dr3_setValue(DebuggerState.breakpoint[breakpoint].address);
								_dr7.L3=1;
								_dr7.LEN3=DebuggerState.breakpoint[breakpoint].breakLength;
								_dr7.RW3=DebuggerState.breakpoint[breakpoint].breakType;
								break;
						}
						

					}

					debugregister++;

				}
				
				
			}
			
			
		}
		

		debugger_dr7_setValue(_dr7);

		//DbgPrint("after:\n");

		//DbgPrint("after fake DR0=%x real DR0=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR0, debugger_dr0_getValue());
		//DbgPrint("after fake DR1=%x real DR1=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR1, debugger_dr1_getValue());
		//DbgPrint("after fake DR2=%x real DR2=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR2, debugger_dr2_getValue());
		//DbgPrint("after fake DR3=%x real DR3=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR3, debugger_dr3_getValue());
		//DbgPrint("after fake DR6=%x real DR6=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR6, debugger_dr6_getValueDword());
		//DbgPrint("after fake DR7=%x real DR7=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR7, debugger_dr7_getValueDword());

	}
	else
	{
		//not global debug, just clear all flags and be done with it
		if (handled)
			debugger_dr6_setValue(0xffff0ff0);
	
	}

	disableInterrupts();


	if (handled == 2)
	{
		//DbgPrint("handled==2\n");		
		handled = 1; //epilogue = 1 Dr handler
	}
	else
	{		
		//not handled by the epilogue set DR0, so the actual epilogue
		//DbgPrint("handled==1\n");
		
		if (DebuggerState.globalDebug)
		{
			DebuggerState.FakedDebugRegisterState[cpunr()].inEpilogue=0;

			if (!DebuggerState.stoppingTheDebugger)
				debugger_dr7_setGD(DebuggerState.globalDebug); //set it back to 1, if not unloading
		}
	}
	//after=getRSP();

	//DbgPrint("before=%llx after=%llx\n",before,after);

	//DbgPrint("end of interrupt1_centry. eflags=%x", stackpointer[si_eflags]);

	//if branch tracing set lbr back on (get's disabled on debug interrupts)	
	if (DebuggerState.storeLBR)
		__writemsr(0x1d9, __readmsr(0x1d9) | 1);
		



	return handled;
}

#ifndef AMD64
_declspec( naked ) void interrupt1_asmentry( void )
//This routine is called upon an interrupt 1, even before windows gets it
{
	__asm{
		//change the start of the stack so that instructions like setthreadcontext do not affect the stack it when it's frozen and waiting for input
		//meaning the setting of debug registers will have to be done with the changestate call

		//sub esp,4096
		//push [esp+4096+0+16] //optional ss
		//push [esp+4096+4+12] //optional esp
		//push [esp+4096+8+8] //eflags
		//push [esp+4096+12+4] //cs
		//push [esp+4096+16+0] //eip

		cld //reset the direction flag
		
		
		
		



		//save stack position
		push 0 //push an errorcode on the stack so the stackindex can stay the same
		push ebp
		mov ebp,esp

		//save state
		pushad
		xor eax,eax
		mov ax,ds
		push eax

		mov ax,es
		push eax

		mov ax,fs
		push eax

		mov ax,gs
		push eax

		//save fpu state
		//save sse state
		
		mov ax,0x23 //0x10 should work too, but even windows itself is using 0x23
		mov ds,ax
		mov es,ax
		mov gs,ax
		mov ax,0x30
		mov fs,ax

		
		

		push ebp
		call interrupt1_centry

		cmp eax,1	//set flag

		//restore state
		pop gs
		pop fs
		pop es
		pop ds
		popad

		pop ebp		

		je skip_original_int1
		
		add esp,4 //undo errorcode push (add effects eflags, so set it at both locations)

		jmp far [Int1JumpBackLocation]

skip_original_int1:
		add esp,4 //undo errorcode push
		iretd
	}
}
#endif
