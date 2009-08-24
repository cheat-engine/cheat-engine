/*
debugger.c:
This unit will handle all debugging related code, from hooking, to handling interrupts
*/
#include "ntifs.h"
#include <windef.h>

#include "DBKFunc.h"
#include "interruptHook.h"

#include "debugger.h"

//setup debugging
  //setup which process is being debugged
  //setup which address(es) is/are affected
  //hook int 1  

//handle breakpoint

void interrupt1_asmentry( void );

#pragma pack(1)
struct
{	
	ULONG_PTR eip;
	WORD cs;
} Int1JumpBackLocation;
#pragma pack()



struct
{
	BOOL		isDebugging;		//TRUE if a process is currently being debugged
	DWORD		debuggedProcessID;	//The processID that is currently debugger
	struct {
		BOOL		active;
		ULONG_PTR	address;		//Up to 4 addresses to break on
		BreakType	breakType;		//What type of breakpoint for each seperate address
		BreakLength breakLength;	//How many bytes does this breakpoint look at
	} breakpoint[4];

	//...
	BOOL globalDebug;			//If set all threads of every process will raise an interrupt on taskswitch

	//while debugging:
	DWORD *LastStackPointer;
	DWORD *LastRealDebugRegisters;
	BOOL handledlastevent;

	struct {
		
		ULONG DR0;
		ULONG DR1;
		ULONG DR2;
		ULONG DR3;
		ULONG DR6;
		ULONG DR7;
		ULONG reserved;
		int inEpilogue; //if set the global debug bit does no faking
	} FakedDebugRegisterState[256];

} DebuggerState;



KEVENT debugger_event_WaitForContinue; //event for kernelmode. Waits till it's set by usermode (usermode function: DBK_Continue_Debug_Event sets it)
KEVENT debugger_event_WaitForDebugEvent; //event for usermode. Waits till it's set by a debugged event

KEVENT debugger_event_CanBreak; //event for kernelmode. Waits till a break has been handled so a new one can enter

DebugReg7 debugger_dr7_getValue(void);
void debugger_dr7_setValue(DebugReg7 value);
DebugReg6 debugger_dr6_getValue(void);


void debugger_dr7_setGD(int state)
{
	DebugReg7 _dr7=debugger_dr7_getValue();
	_dr7.GD=state; //usually 1
	debugger_dr7_setValue(_dr7);
}

void debugger_dr0_setValue(DWORD value)
{
	__asm
	{
		mov eax,value
		mov dr0,eax
	}
}

_declspec( naked ) DWORD debugger_dr0_getValue(void)
{
	__asm
	{
		mov eax,dr0	
		ret
	}
}

void debugger_dr1_setValue(DWORD value)
{
	__asm
	{
		mov eax,value
		mov dr1,eax
	}
}

_declspec( naked ) DWORD debugger_dr1_getValue(void)
{
	__asm
	{
		mov eax,dr1	
		ret
	}
}

void debugger_dr2_setValue(DWORD value)
{
	__asm
	{
		mov eax,value
		mov dr2,eax
	}
}

_declspec( naked ) DWORD debugger_dr2_getValue(void)
{
	__asm
	{
		mov eax,dr2	
		ret
	}
}

void debugger_dr3_setValue(DWORD value)
{
	__asm
	{
		mov eax,value
		mov dr3,eax
	}
}

_declspec( naked ) DWORD debugger_dr3_getValue(void)
{
	__asm
	{
		mov eax,dr3		
		ret
	}
}

void debugger_dr6_setValue(DWORD value)
{
	__asm
	{
		mov eax,value
		mov dr6,eax
	}
}

void debugger_dr7_setValue(DebugReg7 value)
{
	__asm
	{
		mov eax,value
		mov dr7,eax
	}
}


_declspec( naked ) DebugReg7 debugger_dr7_getValue(void)
{
	__asm{
		mov eax,dr7
		ret
	}
}

_declspec( naked ) DWORD debugger_dr7_getValueDword(void) //I wonder why I couldn't just typecast the DebugReg7 to a dword...
{
	__asm{
		mov eax,dr7
		ret
	}
}

_declspec( naked ) DebugReg6 debugger_dr6_getValue(void)
{
	__asm{
		mov eax,dr6
		ret
	}
}

_declspec( naked ) DWORD debugger_dr6_getValueDword(void)
{
	__asm{
		mov eax,dr6
		ret
	}
}

void debugger_initialize(void)
{
	DbgPrint("Initializing debugger events\n");
	KeInitializeEvent(&debugger_event_WaitForContinue, SynchronizationEvent, FALSE);
	KeInitializeEvent(&debugger_event_WaitForDebugEvent, SynchronizationEvent, FALSE);
	KeInitializeEvent(&debugger_event_CanBreak, SynchronizationEvent, TRUE); //true so the first can enter
}

void debugger_setInitialFakeState(void)
{	
	DebuggerState.FakedDebugRegisterState[cpunr()].DR0=debugger_dr0_getValue();
	DebuggerState.FakedDebugRegisterState[cpunr()].DR1=debugger_dr1_getValue();
	DebuggerState.FakedDebugRegisterState[cpunr()].DR2=debugger_dr2_getValue();
	DebuggerState.FakedDebugRegisterState[cpunr()].DR3=debugger_dr3_getValue();
	DebuggerState.FakedDebugRegisterState[cpunr()].DR6=debugger_dr6_getValueDword();
	DebuggerState.FakedDebugRegisterState[cpunr()].DR7=debugger_dr7_getValueDword();
}

int debugger_initHookForCurrentCPU(void)
/*
Must be called for each cpu
*/
{
	int result;
	DbgPrint("Hooking int1 for this cpu\n");
	if (!inthook_isHooked(1))
	{
		result=inthook_HookInterrupt(1,0x8, (ULONG_PTR)interrupt1_asmentry);

		if (DebuggerState.globalDebug)
		{
			//set the fake state
			//debugger_setInitialFakeState();
			debugger_dr7_setGD(1); //enable the GD flag
		}
	}
		
	return result;
}

int debugger_setGlobalDebugState(BOOL state)
//call this BEFORE debugging, if already debugging, the user must call this for each cpu
{
	DbgPrint("debugger_setGlobalDebugState(%d)\n",state);
	DebuggerState.globalDebug=state; //to other editors: just make sure this is set BEFORE calling debugger_dr7_setGD

	if (inthook_isHooked(1))
	{
		DbgPrint("Int 1 is hooked, setting GD");
		//debugger_setInitialFakeState();
		debugger_dr7_setGD(1);
	}

	return TRUE;
}

int debugger_removeHookForCurrentCPU(void)
{
	DbgPrint("Dehooking int1 for this cpu\n");

	if (DebuggerState.globalDebug)
		return FALSE;
	else
		return inthook_UnhookInterrupt(1);	
}

int debugger_startDebugging(DWORD debuggedProcessID)
/*
Call this AFTER the interrupts are hooked
*/
{
	DbgPrint("debugger_startDebugging\n");
	Int1JumpBackLocation.eip=inthook_getOriginalEIP(1);
	Int1JumpBackLocation.cs=inthook_getOriginalCS(1);

	DbgPrint("Int1 jump back = %x:%x\n", Int1JumpBackLocation.cs, Int1JumpBackLocation.eip);

	DebuggerState.isDebugging=TRUE;
	DebuggerState.debuggedProcessID=debuggedProcessID;

	return TRUE;
}

int debugger_stopDebugging(void)
{	
	//don't unhook if globaldebug has been used	
	int i;

	DbgPrint("Stopping the debugger if it is running\n");

	DebuggerState.isDebugging=FALSE;
	//DebuggerState.globalDebug=FALSE; //stop when possible, saves speed

	for (i=0; i<4; i++)
		DebuggerState.breakpoint[i].active=FALSE;

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
	DebuggerState.breakpoint[breakpointnr].active=TRUE;
	DebuggerState.breakpoint[breakpointnr].address=Address;
	DebuggerState.breakpoint[breakpointnr].breakType=bt;
	DebuggerState.breakpoint[breakpointnr].breakLength=bl;

	//debugger_dr0_setValue(debugger_dr0_getValue()); //kickstart the dr setting

	return TRUE;
}

NTSTATUS debugger_waitForDebugEvent(ULONG timeout)
{
	NTSTATUS r;
	LARGE_INTEGER wait;

	DbgPrint("debugger_waitForDebugEvent with timeout of %d\n",timeout);

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

DWORD *debugger_getLastStackPointer(void)
{
	return DebuggerState.LastStackPointer;
}


NTSTATUS debugger_getDebuggerState(PDebugStackState state)
{
	state->eflags=DebuggerState.LastStackPointer[si_eflags];
	state->eax=DebuggerState.LastStackPointer[si_eax];
	state->ebx=DebuggerState.LastStackPointer[si_ebx];
	state->ecx=DebuggerState.LastStackPointer[si_ecx];
	state->edx=DebuggerState.LastStackPointer[si_edx];
	state->esi=DebuggerState.LastStackPointer[si_esi];
	state->edi=DebuggerState.LastStackPointer[si_edi];
	state->ebp=DebuggerState.LastStackPointer[si_ebp];

	//generally speaking, NOTHING should touch the esp register, but i'll provide it anyhow
	if ((DebuggerState.LastStackPointer[si_cs] & 3) == 3) //if usermode code segment
	{
		//priv level change, so the stack info was pushed as well
		state->esp=DebuggerState.LastStackPointer[si_esp]; 
		state->ss=DebuggerState.LastStackPointer[si_ss];

	}
	else
	{
		WORD temp;
		//kernelmode stack, yeah, it's really useless here since changing it here only means certain doom, but hey...
		state->esp=(DWORD)DebuggerState.LastStackPointer-4;
		__asm
		{
			mov ax,ss
			mov temp,ax		
		}
		state->ss=temp; //unchangable by the user
	}

	
	state->eip=DebuggerState.LastStackPointer[si_eip];
	state->cs=DebuggerState.LastStackPointer[si_cs];
	state->ds=DebuggerState.LastStackPointer[si_ds];
	state->fs=DebuggerState.LastStackPointer[si_fs];
	state->gs=DebuggerState.LastStackPointer[si_gs];

	state->dr0=DebuggerState.LastRealDebugRegisters[0];
	state->dr1=DebuggerState.LastRealDebugRegisters[1];
	state->dr2=DebuggerState.LastRealDebugRegisters[2];
	state->dr3=DebuggerState.LastRealDebugRegisters[3];
	state->dr6=DebuggerState.LastRealDebugRegisters[4];
	state->dr7=DebuggerState.LastRealDebugRegisters[5];


	return STATUS_SUCCESS;
}

NTSTATUS debugger_setDebuggerState(PDebugStackState state)
{
	DebuggerState.LastStackPointer[si_eflags]=state->eflags;
	DebuggerState.LastStackPointer[si_eax]=state->eax;
	DebuggerState.LastStackPointer[si_ebx]=state->ebx;
	DebuggerState.LastStackPointer[si_ecx]=state->ecx;
	DebuggerState.LastStackPointer[si_edx]=state->edx;
	
	DebuggerState.LastStackPointer[si_esi]=state->esi;
	DebuggerState.LastStackPointer[si_edi]=state->edi;
	
	DebuggerState.LastStackPointer[si_ebp]=state->ebp;

	//generally speaking, NOTHING should touch the esp register, but i'll provide it anyhow
	if ((DebuggerState.LastStackPointer[si_cs] & 3) == 3) //if usermode code segment
	{
		//priv level change, so the stack info was pushed as well
		DebuggerState.LastStackPointer[si_esp]=state->esp;
		//don't mess with ss
	}
	else
	{
		//no change in kernelmode allowed		
	}

	
	DebuggerState.LastStackPointer[si_eip]=state->eip;
	DebuggerState.LastStackPointer[si_cs]=state->cs;
	DebuggerState.LastStackPointer[si_ds]=state->ds;
	DebuggerState.LastStackPointer[si_fs]=state->fs;
	DebuggerState.LastStackPointer[si_gs]=state->gs;

	if (!DebuggerState.globalDebug)
	{
		//no idea why someone would want to use this method, but it's in (for NON globaldebug only)

		//updating this array too just so the user can see it got executed. (it eases their state of mind...)
		DebuggerState.LastRealDebugRegisters[0]=state->dr0; 
		DebuggerState.LastRealDebugRegisters[1]=state->dr1;
		DebuggerState.LastRealDebugRegisters[2]=state->dr2;
		DebuggerState.LastRealDebugRegisters[3]=state->dr3;
		DebuggerState.LastRealDebugRegisters[4]=state->dr6;
		DebuggerState.LastRealDebugRegisters[5]=state->dr7;

		//no setting of the DebugRegs here

	}


	return STATUS_SUCCESS;
}

int breakpointHandler_kernel(DWORD *stackpointer, DWORD *currentdebugregs)
//Notice: This routine is called when interrupts are enabled and the GD bit has been set if globaL DEBUGGING HAS BEEN USED
{
	int handled=0; //0 means let the OS handle it
	
	

	DbgPrint("breakpointHandler for kernel breakpoints\n");

	if (KeGetCurrentIrql()==0)
	{
		//crititical section here
		NTSTATUS r;
		r=STATUS_UNSUCCESSFUL;
		while (r != STATUS_SUCCESS)
		{
			r=KeWaitForSingleObject(&debugger_event_CanBreak,UserRequest, KernelMode, TRUE, NULL);
			//check r and handle specific events
				
		}
		

		//We're here, let's notify the usermode debugger of our situation
		//first store the stackpointer so it can be manipulated externally
		DebuggerState.LastStackPointer=stackpointer;
		DebuggerState.LastRealDebugRegisters=currentdebugregs;


		//notify usermore app that this thread has halted due to a debug event
		KeSetEvent(&debugger_event_WaitForDebugEvent,0,FALSE);


		//wait for event from usermode that debgu event has been handled
		//KeWaitForSingleObject();
		//continue with state

		while (1)
		{


			LARGE_INTEGER wt;
			NTSTATUS s;
			
			wt.QuadPart=-10000000LL; 
			//s=KeDelayExecutionThread(KernelMode, FALSE, &wt);

			DbgPrint("Waiting...\n");
			s=KeWaitForSingleObject(&debugger_event_WaitForContinue, UserRequest, KernelMode, TRUE, NULL);
			DbgPrint("KeWaitForSingleObject=%x\n",s);		
			if (s==STATUS_SUCCESS)
			{
				if (DebuggerState.handledlastevent)
				{
					DbgPrint("handledlastevent=TRUE");
					handled=1;
				}
				else
					handled=0;

				break;
			}
				
		}

		//i'm done, let other threads catch it
		KeSetEvent(&debugger_event_CanBreak, 0, FALSE);

		return handled;
	}
	else
	{
		DbgPrint("Breakpoint wasn't at passive level. Screw this, i'm not going to break here\n");
		
		return 1;
	}

}

int interrupt1_handler(DWORD *stackpointer, DWORD *currentdebugregs)
{

	HANDLE CurrentProcessID=PsGetCurrentProcessId();	
	DebugReg6 _dr6=*(DebugReg6 *)&currentdebugregs[4];
	DebugReg7 _dr7=*(DebugReg7 *)&currentdebugregs[5];

	//DbgPrint("interrupt1_handler dr6=%x dr7=%d\n",_dr6,_dr7);

	


	//check if this break should be handled or not
	
	if (DebuggerState.globalDebug)
	{
		//DbgPrint("DebuggerState.globalDebug=TRUE\n");
		//global debugging is being used
		if (_dr6.BD)
		{
			//The debug registers are being accessed, emulate it with DebuggerState.FakedDebugRegisterState[cpunr()].DRx
			int instructionPointer;
			int currentcpunr=cpunr();
			int debugregister;
			int generalpurposeregister;
			unsigned char *instruction=(unsigned char *)stackpointer[si_eip];

			//unset this flag in DR6
			_dr6.BD=0;
			debugger_dr6_setValue(*(DWORD *)&_dr6);

			if (DebuggerState.FakedDebugRegisterState[cpunr()].inEpilogue)
			{					
				((EFLAGS *)&stackpointer[si_eflags])->RF=1; //repeat this instruction and don't break
				return 2;
			}

		
			
			DebuggerState.FakedDebugRegisterState[cpunr()].DR6=*(DWORD *)&_dr6;

			for (instructionPointer=0; instruction[instructionPointer] != 0x0f; instructionPointer++) ; //find the start of the instruction, skipping prefixes etc...
			
			//we now have the start of the instruction.
			//Find out which instruction it is, and which register is used
			debugregister=(instruction[instructionPointer+2] >> 3) & 7;	
			generalpurposeregister=instruction[instructionPointer+2] & 7;

			//DbgPrint("debugregister=%d, generalpurposeregister=%d\n",debugregister,generalpurposeregister); 

			if (instruction[instructionPointer+1]==0x21)
			{
				DWORD drvalue;
				//DbgPrint("read opperation\n");
				//21=read
				switch (debugregister)
				{
					case 0: 
						drvalue=DebuggerState.FakedDebugRegisterState[cpunr()].DR0;
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
						break;

					case 5: 
					case 7:
						drvalue=DebuggerState.FakedDebugRegisterState[cpunr()].DR7;						
						break;

				}

				if (drvalue==0x455c00)
					DbgPrint("Somehow I told the guest the hidden breakpoint value\n");

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
						if ((stackpointer[si_cs] & 3) == 3)
							stackpointer[si_esp]=drvalue;

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
				}

			}
			else 
			if (instruction[instructionPointer+1]==0x23)
			{
				//23=write
				DWORD gpvalue;
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
				}

				if (gpvalue==0x455c00)
					DbgPrint("somehow the guest knew the hidden breakpoint value\n");

				//gpvalue now contains the value to set the debug register
				switch (debugregister)
				{
					case 0: 
						
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
						debugger_dr6_setValue(gpvalue);
						DebuggerState.FakedDebugRegisterState[cpunr()].DR6=debugger_dr6_getValueDword();
						break;

					case 5: 
					case 7:
						//make sure it doesn't set the GD flag nowgpvalue
						gpvalue=gpvalue | 0x400;
						((DebugReg7 *)&gpvalue)->GD=0;
						debugger_dr7_setValue(*((DebugReg7 *)&gpvalue));
						DebuggerState.FakedDebugRegisterState[cpunr()].DR7=debugger_dr7_getValueDword();
						
						
						
						break;
				}



			}
			else 
			{
				DbgPrint("Some unknown instruction accessed the debug registers?\n");
				if (CurrentProcessID==(HANDLE)DebuggerState.debuggedProcessID)
					DbgPrint("Happened inside the target process\n");

				DbgPrint("interrupt1_handler dr6=%x dr7=%d\n",_dr6,_dr7);
				DbgPrint("eip=%x\n",stackpointer[si_eip]);



			}

			//adjust eip to after this instruction
			stackpointer[si_eip]+=instructionPointer+3; //0f xx /r

			return 1; //don't tell windows about it
		}
	}

	
	if (DebuggerState.isDebugging)
	{
		//check if this should break
		if (CurrentProcessID==(HANDLE)DebuggerState.debuggedProcessID)
		{
			DbgPrint("BP in target process\n");
			
			//no extra checks if it's caused by the debugger or not. That is now done in the usermode part

			if (DebuggerState.globalDebug)
			{
				//enable the GD flag for taskswitches that will occur as soon as interrupts are enabled
				//this also means: DO NOT EDIT THE DEBUG REGISTERS IN GLOBAL DEBUG MODE at this point. Only in the epilogue
				debugger_dr7_setGD(DebuggerState.globalDebug); 
			}			
			__asm
			{
				sti //start interrupt handling
			}
			return breakpointHandler_kernel(stackpointer, currentdebugregs);				
		}
		else 
		{
			DebugReg6 dr6=debugger_dr6_getValue();
			DebugReg7 dr7=*(DebugReg7 *)&DebuggerState.FakedDebugRegisterState[cpunr()].DR7;

			//DbgPrint("Not the debugged process (%x != %x)\n",CurrentProcessID,DebuggerState.debuggedProcessID );
			//check if this break is due to a breakpoint ce has set.
			//do that by checking if the breakpoint condition exists in the FAKE dr7 registers
			//if so, let windows handle it, if not, it is caused by ce, which then means, skip

			

			if ((dr6.B0) && (!(dr7.L0 || dr7.G0))) return 1;  //break caused by DR0 and not expected by the current process
			if ((dr6.B1) && (!(dr7.L1 || dr7.G1))) return 1;  //		...		DR1		...
			if ((dr6.B2) && (!(dr7.L2 || dr7.G2))) return 1;  //		...		DR2		...
			if ((dr6.B3) && (!(dr7.L3 || dr7.G3))) return 1;  //		...		DR3		...

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

int interrupt1_centry(DWORD *stackpointer) //code segment 8 has a 32-bit stackpointer
{
	DWORD currentdebugregs[6]; //used for determining if the current bp is caused by the debugger ot not
	int handled=0; //if 0 at return, the interupt will be passed down to the opperating system

	int debugme=0;

	//DbgPrint("interrupt1_centry: %x\n",PsGetCurrentProcessId());
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
	__asm{
		cli
	}	

	DebuggerState.FakedDebugRegisterState[cpunr()].inEpilogue=1;
	debugger_dr7_setGD(0); //make sure the GD bit is disabled (int1 within int1, oooh the fun...)
	DebuggerState.FakedDebugRegisterState[cpunr()].inEpilogue=1;

	if (inthook_isDBVMHook(1))
	{
		//update the int1 return address, could have been changed
		IDT idt;	
		GetIDT(&idt);

		Int1JumpBackLocation.cs=idt.vector[1].wSelector;
		Int1JumpBackLocation.eip=idt.vector[1].wLowOffset+(idt.vector[1].wHighOffset << 16);
	}

	
	//DbgPrint("interrupt1_centry returning %d\n",handled);

	/* rf should be set by the usermode debugger part. reason: gd bit, there handled = 1 but rf must not be 1 because eip is changed
	if (handled) //set the resume flag so it executes at least one instruction
		((PEFLAGS)&DebuggerState.LastStackPointer[si_eflags])->RF=1;
		*/

	if (KeGetCurrentIrql() != PASSIVE_LEVEL)
	{
		DbgPrint("Not at PASSIVE LEVEL\n");

	}
	
	if ((DebuggerState.globalDebug)) //vista: DR's are only accesses when there are DR's(no idea how it handles breakpoints in a different process...), so set them in each thread even those that don't belong original: && (PsGetCurrentProcessId()==(HANDLE)DebuggerState.debuggedProcessID))
	{		
		//set the breakpoint in this thread. 
		int debugregister=0, breakpoint=0;
		int currentcpunr=cpunr();
		DebugReg7 _dr7=*(DebugReg7 *)&DebuggerState.FakedDebugRegisterState[currentcpunr].DR7;

		/*DbgPrint("Target process:\n");
		DbgPrint("real dr0=%x\n",debugger_dr0_getValue());
		DbgPrint("real dr1=%x\n",debugger_dr1_getValue());
		DbgPrint("real dr2=%x\n",debugger_dr2_getValue());
		DbgPrint("real dr3=%x\n",debugger_dr3_getValue());
		DbgPrint("real dr6=%x\n",debugger_dr6_getValueDword());
		DbgPrint("real dr7=%x\n",debugger_dr7_getValueDword());		

		DbgPrint("fake dr0=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR0);
		DbgPrint("fake dr1=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR1);
		DbgPrint("fake dr2=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR2);
		DbgPrint("fake dr3=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR3);
		DbgPrint("fake dr6=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR6);
		DbgPrint("fake dr7=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR7);	*/	
		//set the debug registers of active breakpoints. Doesn't have to be in the specified order. Just find an unused debug registers
		//check DebuggerState.FakedDebugRegisterState[cpunr()].DR7 for unused breakpoints

		//first set the breakpoitns to what they should be according to the guest

		
		debugger_dr0_setValue(DebuggerState.FakedDebugRegisterState[currentcpunr].DR0);
		debugger_dr1_setValue(DebuggerState.FakedDebugRegisterState[currentcpunr].DR1);
		debugger_dr2_setValue(DebuggerState.FakedDebugRegisterState[currentcpunr].DR2);
		debugger_dr3_setValue(DebuggerState.FakedDebugRegisterState[currentcpunr].DR3);
		debugger_dr6_setValue(DebuggerState.FakedDebugRegisterState[currentcpunr].DR6);

		if ((*(DebugReg7 *)&DebuggerState.FakedDebugRegisterState[currentcpunr].DR7).GD)
		{
			DbgPrint("DR7.GD IS SET IN THE GUEST STATE\n");
		}
		//debugger_dr7_setValue(*(DebugReg7 *)&DebuggerState.FakedDebugRegisterState[currentcpunr].DR7);

//		DbgPrint("PID/TID:%x/%x\n", PsGetCurrentProcessId(), PsGetCurrentThreadId());
//		DbgPrint("before fake DR0=%x real DR0=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR0, debugger_dr0_getValue());
//		DbgPrint("before fake DR1=%x real DR1=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR1, debugger_dr1_getValue());
//		DbgPrint("before fake DR2=%x real DR2=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR2, debugger_dr2_getValue());
//		DbgPrint("before fake DR3=%x real DR3=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR3, debugger_dr3_getValue());
//		DbgPrint("before fake DR6=%x real DR6=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR6, debugger_dr6_getValueDword());
//		DbgPrint("before fake DR7=%x real DR7=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR7, debugger_dr7_getValueDword());

		if (!DebuggerState.FakedDebugRegisterState[currentcpunr].inEpilogue)
		{
			
			DbgPrint("Before: It wasn't in the epilogue. eflags=%x\n");
			DebuggerState.FakedDebugRegisterState[currentcpunr].inEpilogue=1;
		}
		
		for (breakpoint=0; breakpoint<4; breakpoint++)
		{
			if (DebuggerState.breakpoint[breakpoint].active)
			{
				int foundone=0;
			
				//find a usable debugregister
				while ((debugregister<4) && (foundone==0))				
				{
					if (!DebuggerState.FakedDebugRegisterState[currentcpunr].inEpilogue)
					{
						DbgPrint("During: It wasn't in the epilogue\n");
						DebuggerState.FakedDebugRegisterState[currentcpunr].inEpilogue=1;
					}

					//check if this debugregister is usable
					if (((DebuggerState.FakedDebugRegisterState[currentcpunr].DR7 >> (debugregister*2)) & 3)==0)  //DR7.Gx and DR7.Lx are 0
					{
					    DbgPrint("debugregister %d is free to be used\n",debugregister);
						foundone=1;
						debugme = 1;
						DbgPrint("Before\n");
						

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
						DbgPrint("After\n");

					}

					debugregister++;

				}
				if (foundone==0)
					DbgPrint("Failure setting breakpoint %d\n",breakpoint);
					
			}
			
		}

		if (debugme) 
		{
			DWORD x=*(DWORD *)&_dr7;
			
			//DbgPrint("Setting DR7 to %x\n",x);
		}

		debugger_dr7_setValue(_dr7);

		DbgPrint("after:\n");

		DbgPrint("after fake DR0=%x real DR0=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR0, debugger_dr0_getValue());
		DbgPrint("after fake DR1=%x real DR1=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR1, debugger_dr1_getValue());
		DbgPrint("after fake DR2=%x real DR2=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR2, debugger_dr2_getValue());
		DbgPrint("after fake DR3=%x real DR3=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR3, debugger_dr3_getValue());
		DbgPrint("after fake DR6=%x real DR6=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR6, debugger_dr6_getValueDword());
		DbgPrint("after fake DR7=%x real DR7=%x\n",DebuggerState.FakedDebugRegisterState[currentcpunr].DR7, debugger_dr7_getValueDword());

	}

	
	
/*	if (handled)
	{
		//clear DR6
		debugger_dr6_setValue(0xffff0ff0);
	}	
	*/

	if (handled == 2)
	{
		handled = 1; //epilogue = 1 Dr handler
	}
	else
	{
		//not handled by the epilogue set DR0, so the actual epilogue
		DebuggerState.FakedDebugRegisterState[cpunr()].inEpilogue=0;
		debugger_dr7_setGD(DebuggerState.globalDebug);
	}

	return handled;
}

_declspec( naked ) void interrupt1_asmentry( void )
//This routine is called upon an interrupt 1, even before windows gets it
{
	__asm{ 
		//save stack position	
		push ebp
		mov ebp,esp

		//save state
		pushad
		push ds
		push es
		push fs
		push gs

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
		jmp far [Int1JumpBackLocation]

skip_original_int1:
		iretd
	}
}