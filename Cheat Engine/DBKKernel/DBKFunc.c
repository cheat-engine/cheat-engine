//#include "C:\WINDDK\3790\inc\ifs\w2k\ntifs.h"
#include "DBKFunc.h"
#include "vmxhelper.h"

#ifndef AMD64
void interrupt1( void );
void interrupt3( void );
void interruptD1( void );
#endif

ULONG Int1Address;
ULONG Int3Address;
ULONG IntD1Address;
INT_VECTOR	NewInt1;
INT_VECTOR  NewIntD1;


unsigned __int64 readMSR(ULONG msr)
{
	ULONG a,b;
	__asm
	{
		push ecx
		push eax
		push edx
		mov ecx,msr
		rdmsr

		mov [a],eax
		mov [b],edx

		pop edx
		pop eax
		pop ecx
	}

	return ((INT64)b<<32) + a;
}

ULONG getCR3(void)
{
	ULONG cr3reg;
	__asm
	{
		mov eax,CR3
		mov cr3reg,eax
	}

	return cr3reg;

}

ULONG getCR4(void)
{
#ifndef AMD64
	ULONG cr4reg=0;
    __try
	{
		__asm
		{
			__emit 0x0f  //-|
			__emit 0x20  //-|-mov eax,cr4
			__emit 0xe0  //-|
			mov cr4reg,eax
		}
	}
	__except(1)
	{
		DbgPrint("Error getting CR4\n");
	}
	return cr4reg;
#else
	return 0;
#endif

}

void setCR4(ULONG cr4reg)
{
	__asm
	{
		mov eax,cr4reg
		__emit 0x0f  //-|
		__emit 0x22  //-|-mov cr4,eax
		__emit 0xe0  //-|			
	}
	return;
}

unsigned long long getTSC(void)
{
	ULONG a,b;
	unsigned long long result;
	__asm
	{
		push eax
		push edx
		rdtsc
		mov [a],eax
		mov [b],edx
		
		pop edx
		pop eax
	}

	result=a+((unsigned long long)(b) << 32);
	return result;
}

void StopDebugging(void)
{
	DebuggedProcessID=0;
	ChangeRegs[0].Active=FALSE;
	ChangeRegs[1].Active=FALSE;
	ChangeRegs[2].Active=FALSE;
	ChangeRegs[3].Active=FALSE;
	ChangeRegistersOnBP=FALSE;

	DbgPrint("Stopped the debugger\n");
	//the int hooks can stay, they don't hurt anyhow
}

void StopChangeRegOnBP(int DebugRegNR)
{
	int i;


	DbgPrint("DBKKERNEL:StopChangeRegOnBP %d\n",DebugRegNR);

	ChangeRegs[DebugRegNR].Active=FALSE;
	ChangeRegs[DebugRegNR].BreakAddress=0;

	for(i=0; i<4; i++)
	{
		if (ChangeRegs[DebugRegNR].Active)
		{
			ChangeRegistersOnBP=TRUE;
			DbgPrint("Still one debugreg active, so keep changing others\n" );
			return;
		}
	}

	ChangeRegistersOnBP=FALSE;

}

BOOLEAN ChangeRegOnBP(DWORD ProcessID, int DebugRegNR, PChangeReg CR)
{
	//This gets called just before the usermode app sets the debugreg
	//CR->Active ignored in this case, becomes true
	DbgPrint("Going to enable the debugger to set a change reg breakpoint on ProcessID %d(%x) for debugreg %d",ProcessID,ProcessID,DebugRegNR);
	CR->Active=TRUE;
	ChangeRegs[DebugRegNR]=*CR;
	ChangeRegs[DebugRegNR].Active=TRUE;
	DebuggedProcessID=ProcessID;	
	PsLookupProcessByProcessId((PVOID)ProcessID,&DebuggedProcessPEPROCESS);

	ChangeRegistersOnBP=TRUE;

	return TRUE; //always
}

BOOLEAN DebugProcess(DWORD ProcessID, DWORD Address, BYTE Length, BYTE RWE)
{
	DbgPrint("Going to debug a process for 1 process+debug address\nProcessID=%d (%x)\n",ProcessID,ProcessID);

	//EventsMissed=0;

	DebuggedProcessID=ProcessID;
	PsLookupProcessByProcessId((PVOID)ProcessID,&DebuggedProcessPEPROCESS);
	DebuggedAddress=Address;
	DebuggedAddressLength=Length;
	DebuggedAddressRWE=RWE;

	DbgPrint("DebuggedAddress=%x\n",DebuggedAddress);

	/*
	setting the debug registers in the context will be done from usermode (unless I figure out how to call 
	NtSetContextThread which doesn't seem to be in ntos so requires user32.sys which is fucking buggy to use
	but, the threadid's can be retrieved using a toolhelpsnapshot, (or a notify routine for created threads)
	and can be opened using OpenThread (win2k+ but I dont give a shit about that...)
	*/

	DbgPrint("Still here so I gues it works!\n");
	return TRUE;
}

unsigned short GetTR(void)
{
	unsigned short result;
#ifndef AMD64	
	__asm
	{
		STR AX
		mov result,ax
	}	
#endif;
	return result;
}

void GetGDT(PGDT pGdt)
{
#ifndef AMD64	
	__asm
	{
		MOV EAX, [pGdt]
		SGDT [EAX]
	}	
#endif;
}

void GetLDT(PGDT pLdt)
{
#ifndef AMD64	
	__asm
	{
		MOV EAX, [pLdt]
		SLDT [EAX]
	}	
#endif;
}

void GetIDT(PIDT pIdt)
{
	//I should do this for each logical processor
#ifndef AMD64	
	__asm
	{
		MOV EAX, [pIdt]
		SIDT [EAX]
	}	
#endif;
}

void SetIDT(PIDT pIdt)
{
	__asm
	{
		MOV EAX, [pIdt]
		LIDT [EAX]
	}
}

BOOLEAN HookInt1(void)
{
#ifndef AMD64
	IDT idt;

	//DbgPrint("Going to hook int1\n");
	GetIDT(&idt);

	__try
	{
		if (OriginalInt1.wHighOffset==0)
		{
			DebugReg7 DR_7;

			//DbgPrint("New hook, so storing the original Int1Handler\n");
            OriginalInt1=idt.vector[1];
			NewInt1=idt.vector[1];
			//NewIntD1=idt.vector[0xd1];

			Int1Address=idt.vector[1].wLowOffset+(idt.vector[1].wHighOffset << 16); //save the original address of the int3 handler
			NewInt1.wLowOffset=(WORD)&interrupt1;
			NewInt1.wHighOffset=(WORD)((DWORD)&interrupt1 >> 16);

			if (globaldebug)
			{
				//set the global debug bit and start tasksurfing
				__asm
				{
					mov eax,dr7
					mov DR_7,eax
				}
	
				DR_7.GD=1;
				__asm
				{
					mov eax,DR_7
					mov dr7,eax
				}
			}
			
		}

		//now overwrite the vector so it points to my handler
		//DbgPrint("Changing the vector to point to my handler\n");

		if (vmxusable)
		{
			DbgPrint("using VMX idt1 bypass\n");

			__try
			{
				vmx_redirect_interrupt1(1, 0, NewInt1.wSelector, (DWORD)&interrupt1); //idt bypass hook
			}
			__except(1)
			{
				DbgPrint("VMX idt1 bypass failed\n");
				return FALSE;
			}

		}
		else
		{
			__asm
			{
				PUSHFD //no idea why, I doubt it's usefull, but let's use it too....
				CLI
			}
			idt.vector[1]=NewInt1;
			__asm
			{
				STI
				POPFD
			}
		}


		return TRUE;
	}
	__except(1)
	{
		DbgPrint("Exception while hooking int1\n");
		return FALSE;
	}
#else
	return FALSE;
#endif
}


BOOLEAN HookInt3(void)
{
#ifndef AMD64
	IDT idt;
	//DbgPrint("Going to hook int3\n");
	GetIDT(&idt);

	__try
	{
		OriginalInt3=idt.vector[3];
		Int3Address=idt.vector[3].wLowOffset+(idt.vector[3].wHighOffset << 16); //save the original address of the int3 handler

		//now overwrite the vector so it points to my handler
		__asm
		{
			PUSHFD //no idea why, I doubt it's usefull, but let's use it too....
			CLI
		}

		idt.vector[3].wLowOffset=(WORD)&interrupt3;
		idt.vector[3].wHighOffset=(WORD)((DWORD)&interrupt3 >> 16);
		__asm
		{
			STI
			POPFD
		}
		//DbgPrint("Hooking int3 was successful\n");

	
		return TRUE;
	}
	__except(1)
	{
		DbgPrint("Exception while hooking int3\n");
		return FALSE;
	}
#else
	return FALSE;
#endif
}



int cpunr(void)
{
	unsigned int b;

	__asm
	{
		push eax
		push ebx
		push ecx
		push edx
		mov eax,1
		cpuid

		mov [b],ebx

		pop edx
		pop ecx
		pop ebx
		pop eax
	}

	return (b >> 24)+1;
}


struct {
	ULONG DR0;
	ULONG DR1;
	ULONG DR2;
	ULONG DR3;
	ULONG DR6;
	ULONG DR7;
} CPUDebugRegisterState[256]; //might optimize this in the future by allocating only the needed part... for now, fuck memory


ULONG r1,r2;
ULONG __stdcall GeneralHandler(IN ULONG iInt,IN PULONG Stacklocation )
{
#ifndef AMD64
/*	//check the current priviledge level
	DbgPrint("Welcome to my int handler. (I need to find out how this instruction gets me in ring0)\n");
	DbgPrint("Processid=%d (%x)\n",PsGetCurrentProcessId(),PsGetCurrentProcessId());
*/
	ULONG result=0;	//by default do handle the interrupt by the os
	ULONG DR_0,DR_1,DR_2,DR_3,ef;
	DebugReg6 DR_6;
	DebugReg7 DR_7;

	/*
	DbgPrint("Int1: CPUnr=%d",cpunr());	
	*/

	__asm{
        mov eax,dr0
		mov DR_0,eax
        mov eax,dr1
		mov DR_1,eax
        mov eax,dr2
		mov DR_2,eax
        mov eax,dr3
		mov DR_3,eax
        mov eax,dr6
		mov DR_6,eax
        mov eax,dr7
		mov DR_7,eax
		pushfd
		pop eax
		mov ef,eax
	};
	/*
	DbgPrint("Hello from int1\n");
	DbgPrint("eax=%x\n",Stacklocation[-1]);
	DbgPrint("ebx=%x\n",Stacklocation[-4]);
	DbgPrint("ecx=%x\n",Stacklocation[-2]);
	DbgPrint("edx=%x\n",Stacklocation[-3]);
	DbgPrint("esi=%x\n",Stacklocation[-7]);
	DbgPrint("edi=%x\n",Stacklocation[-8]);			
	DbgPrint("ebp=%x\n",Stacklocation[-6]);
	DbgPrint("ss=%x\n",Stacklocation[4]);
	DbgPrint("esp=%x\n",Stacklocation[3]);
	DbgPrint("eflags=%x\n",Stacklocation[2]);
	DbgPrint("cs=%x\n",Stacklocation[1]); //it was a break			
	DbgPrint("eip=%x\n",Stacklocation[0]); //it was a break			

	DbgPrint("DR0=%x\n",DR_0);
	DbgPrint("DR1=%x\n",DR_1);
	DbgPrint("DR2=%x\n",DR_2);
	DbgPrint("DR3=%x\n",DR_3);
	DbgPrint("DR6=%x\n",DR_6);
	DbgPrint("DR7=%x\n",DR_7);


	DbgPrint("Int1 DR6=%x DR7=%x int_eflags=%x\n",DR_6,DR_7,ef);*/
	
	
	if (globaldebug)
	{
		DR_7.GD=1; //re-set GD bit , but don't set it in the DR7 register yet


		if (DR_6.BD)
		{
			char *instruction=(char *)(Stacklocation[0]);
			int ip;
			int rm,reg;
			int id=cpunr();
			PEFLAGS flags;
			//gd bit is already 0 now
		
			flags=(PEFLAGS)(&Stacklocation[2]);
			//flags->RF=1;
			
			DR_6.BD=0;
			__asm
			{
				mov eax,DR_6
				mov dr6,eax
			}
				
				


			//check which instruction it is, read/write, has prefixes etc...
			//which registers used, and store that / retrieve it

			//0f 21 = read
			//0f 23 = write

			for (ip=0; instruction[ip]!=0x0f; ip++) ;

			reg=(instruction[ip+2] >> 3) & 7; //affected debug register
			rm=instruction[ip+2] & 7; //used general purpose register
		
	
			//get current cpu id to find out what drx should be or where to write the current state to

			if (instruction[ip+1] == 0x21)
			{
				
				//emulate read: rm,reg
				//return what the process has set it to
				switch (reg)
				{
					case 0:
						Stacklocation[-1-rm]=CPUDebugRegisterState[id].DR0; //-1-rm because it's a inverse and starts at -1
						break;

					case 1:
						Stacklocation[-1-rm]=CPUDebugRegisterState[id].DR1; 
						break;

					case 2:
						Stacklocation[-1-rm]=CPUDebugRegisterState[id].DR2; 
						break;

					case 3:
						Stacklocation[-1-rm]=CPUDebugRegisterState[id].DR3; 
						break;

					case 4:
						Stacklocation[-1-rm]=CPUDebugRegisterState[id].DR6; 
						break;

					case 5:
						Stacklocation[-1-rm]=CPUDebugRegisterState[id].DR7; 
						break;

					case 6:
						Stacklocation[-1-rm]=CPUDebugRegisterState[id].DR6; 
						break;

					case 7:
						Stacklocation[-1-rm]=CPUDebugRegisterState[id].DR7; 
						break;
				}
				
				

			}				
			else if (instruction[ip+1] == 0x23)
			{
				ULONG debugregvalue=Stacklocation[-1-rm];
				
				//emulate write: reg,rm
				switch (reg)
				{
					
					case 0:
						CPUDebugRegisterState[id].DR0=debugregvalue; //-1-rm because it's a inverse and starts at -1
						//and set the real register if allowed
						__asm
						{
							mov eax,debugregvalue
							mov dr0,eax
						}
						break;

					case 1:
						CPUDebugRegisterState[id].DR1=debugregvalue; 
						__asm
						{
							mov eax,debugregvalue
							mov dr1,eax
						}
						break;

					case 2:
						CPUDebugRegisterState[id].DR2=debugregvalue; 
						__asm
						{
							mov eax,debugregvalue
							mov dr2,eax
						}
						break;

					case 3:
						CPUDebugRegisterState[id].DR3=debugregvalue; 
						__asm
						{
							mov eax,debugregvalue
							mov dr3,eax
						}
						break;

					case 4:
						CPUDebugRegisterState[id].DR6=debugregvalue;
						__asm
						{
							mov eax,debugregvalue
							mov dr6,eax
						}
						break;

					case 5:
						CPUDebugRegisterState[id].DR7=debugregvalue;
						
						break;

					case 6:
						CPUDebugRegisterState[id].DR6=debugregvalue;
						__asm
						{
							mov eax,debugregvalue
							mov dr6,eax
						}
						break;

					case 7:
						CPUDebugRegisterState[id].DR7=debugregvalue;
						break;
						
				}
				
			}
			else
				DbgPrint("Something crappy has just happened\n");

			//if a global breakpoint is set check which debug breakpoint can be used and configure it
			//DebuggedAddress and DebuggedAddressLength

			*(ULONG *)&DR_7=CPUDebugRegisterState[id].DR7;
			DR_7.GD=1; //I could do a or operation in assembler, but i'm lazy

			if (DebuggedProcessID!=0)
			{
				__asm
				{
					mov eax,DebuggedAddress
					mov dr0,eax
				}
				DR_7.G0=1;
				DR_7.L0=1;
				DR_7.LEN0=3;
				DR_7.RW0=3;
			}

			__asm
			{
				mov eax,DR_7
				mov dr7,eax //don't touch debug regs now
			};

			
			Stacklocation[0]+=ip+3;
			return 1;
		}		
	}

	//DbgPrint("DebuggedProcessID==%d\n", DebuggedProcessID);
	if (DebuggedProcessID==0)
		return 0; //no debugging going on

	
	//DbgPrint("Int1: PsGetCurrentProcess()=%x DebuggedProcessPEPROCESS=%x\n", (ULONG)PsGetCurrentProcess(), (ULONG)DebuggedProcessPEPROCESS);
	

	
	if ((ULONG)PsGetCurrentProcess()==(ULONG)DebuggedProcessPEPROCESS)
	{
		//this is inside the memory of my debugged process
		//DbgPrint("(ULONG)PsGetCurrentProcess()==(ULONG)DebuggedProcessPEPROCESS\n");

		//DbgPrint("iInt=%d\n",iInt);
		if (iInt==1)
		{
			
			if ((ULONG)PsGetCurrentProcessId()!=DebuggedProcessID)
			{
				//different processid, probably caused by a keattachprocess
				//DbgPrint("Access by other process :%d\n",(ULONG)PsGetCurrentProcessId());

			}
		
			/*
			DbgPrint("Hello from int1\n");
			DbgPrint("eax=%x\n",Stacklocation[-1]);
			DbgPrint("ebx=%x\n",Stacklocation[-4]);
			DbgPrint("ecx=%x\n",Stacklocation[-2]);
			DbgPrint("edx=%x\n",Stacklocation[-3]);
			DbgPrint("esi=%x\n",Stacklocation[-7]);
			DbgPrint("edi=%x\n",Stacklocation[-8]);			
			DbgPrint("ebp=%x\n",Stacklocation[-6]);
			DbgPrint("esp=%x\n",Stacklocation[3]);
			DbgPrint("eip=%x\n",Stacklocation[0]); //it was a break			

			DbgPrint("DR0=%x\n",DR_0);
			DbgPrint("DR1=%x\n",DR_1);
			DbgPrint("DR2=%x\n",DR_2);
			DbgPrint("DR3=%x\n",DR_3);
			DbgPrint("DR6=%x\n",DR_6);
			DbgPrint("DR7=%x\n",DR_7);


		    DbgPrint("DR_7.L3=%d\nDR_6.B3=%d\nDR_3=%x\n",DR_7.L3,DR_6.B3,DR_3);
			*/
			DbgPrint("Stacklocation=%p\n",Stacklocation);
			DbgPrint("-3=%x\n",Stacklocation[-3]);
			DbgPrint("-2=%x\n",Stacklocation[-2]);
			DbgPrint("-1=%x\n",Stacklocation[-1]);
			DbgPrint("0=%x\n",Stacklocation[0]);
			DbgPrint("1=%x\n",Stacklocation[1]);
			DbgPrint("2=%x\n",Stacklocation[2]);
			DbgPrint("3=%x\n",Stacklocation[3]);
			DbgPrint("4=%x\n",Stacklocation[4]);
			DbgPrint("5=%x\n",Stacklocation[5]);
			DbgPrint("6=%x\n",Stacklocation[6]);
			DbgPrint("7=%x\n",Stacklocation[7]);
			DbgPrint("8=%x\n",Stacklocation[8]);
			DbgPrint("9=%x\n",Stacklocation[9]);
			DbgPrint("10=%x\n",Stacklocation[10]);
			DbgPrint("11=%x\n",Stacklocation[11]);
			DbgPrint("12=%x\n",Stacklocation[12]);


			if (ChangeRegistersOnBP)
			{
				int i;
				//see if the breakpoint occurred on one of the 
				for (i=0;i<4;i++)
				{
					if (ChangeRegs[i].Active)
					{						
                        if (
							((DR_7.L0) && (DR_6.B0) && (DR_0==ChangeRegs[i].BreakAddress) ) ||
							((DR_7.L1) && (DR_6.B1) && (DR_1==ChangeRegs[i].BreakAddress) ) ||
							((DR_7.L2) && (DR_6.B2) && (DR_2==ChangeRegs[i].BreakAddress) ) ||
							((DR_7.L3) && (DR_6.B3) && (DR_3==ChangeRegs[i].BreakAddress) )
							)                            				
						{
							//if (ChangeRegs[i].changeAF)
							PEFLAGS x;
							x=(PEFLAGS)(&Stacklocation[2]);



							DbgPrint("This is one caused by me. Changing the registers\n");
							DbgPrint("2=%x\n",Stacklocation[2]);

							DbgPrint("eax=%x\n",Stacklocation[-1]);
							DbgPrint("ebx=%x\n",Stacklocation[-4]);
							DbgPrint("EIP=%x\n",Stacklocation[0]);
							DbgPrint("PEFLAGS=%p\nEFLAGS=%x\n",x,*x);
							DbgPrint("------\n");
							DbgPrint("DR_7.L0=%d\n",DR_7.L0);
							DbgPrint("DR_7.L1=%d\n",DR_7.L1);
							DbgPrint("DR_7.L2=%d\n",DR_7.L2);
							DbgPrint("DR_7.L3=%d\n",DR_7.L3);
							DbgPrint("DR_6.B0=%d\n",DR_6.B0);
							DbgPrint("DR_6.B1=%d\n",DR_6.B1);
							DbgPrint("DR_6.B2=%d\n",DR_6.B2);
							DbgPrint("DR_6.B3=%d\n",DR_6.B3);
							DbgPrint("BP number = %d",i);
														
							

							if (ChangeRegs[i].changeAF) { DbgPrint("Changing AF from %d to %d",x->AF,ChangeRegs[i].newAF); x->AF=ChangeRegs[i].newAF; }
							if (ChangeRegs[i].changeCF) { DbgPrint("Changing CF from %d to %d",x->CF,ChangeRegs[i].newCF); x->CF=ChangeRegs[i].newCF; }
							if (ChangeRegs[i].changeOF) { DbgPrint("Changing OF from %d to %d",x->OF,ChangeRegs[i].newOF); x->OF=ChangeRegs[i].newOF; }
							if (ChangeRegs[i].changePF) { DbgPrint("Changing PF from %d to %d",x->PF,ChangeRegs[i].newPF); x->PF=ChangeRegs[i].newPF; }
							if (ChangeRegs[i].changeSF) { DbgPrint("Changing SF from %d to %d",x->SF,ChangeRegs[i].newSF); x->SF=ChangeRegs[i].newSF; }
							if (ChangeRegs[i].changeZF) { DbgPrint("Changing ZF from %d to %d",x->ZF,ChangeRegs[i].newZF); x->ZF=ChangeRegs[i].newZF; }

							if (ChangeRegs[i].changeEAX) { DbgPrint("Changing EAX from %x to %x",Stacklocation[-1],ChangeRegs[i].newEAX); Stacklocation[-1]=ChangeRegs[i].newEAX; }
							if (ChangeRegs[i].changeEBX) { DbgPrint("Changing EBX from %x to %x",Stacklocation[-4],ChangeRegs[i].newEBX); Stacklocation[-4]=ChangeRegs[i].newEBX; }
							if (ChangeRegs[i].changeECX) { DbgPrint("Changing ECX from %x to %x",Stacklocation[-2],ChangeRegs[i].newECX); Stacklocation[-2]=ChangeRegs[i].newECX; }
							if (ChangeRegs[i].changeEDX) { DbgPrint("Changing EDX from %x to %x",Stacklocation[-3],ChangeRegs[i].newEDX); Stacklocation[-3]=ChangeRegs[i].newEDX; }
							if (ChangeRegs[i].changeESI) { DbgPrint("Changing ESI from %x to %x",Stacklocation[-7],ChangeRegs[i].newESI); Stacklocation[-7]=ChangeRegs[i].newESI; }
							if (ChangeRegs[i].changeEDI) { DbgPrint("Changing EDI from %x to %x",Stacklocation[-8],ChangeRegs[i].newEDI); Stacklocation[-8]=ChangeRegs[i].newEDI; }			
							if (ChangeRegs[i].changeEBP) { DbgPrint("Changing EBP from %x to %x",Stacklocation[-6],ChangeRegs[i].newEBP); Stacklocation[-6]=ChangeRegs[i].newEBP; }
							if (ChangeRegs[i].changeESP) { DbgPrint("Changing ESP from %x to %x",Stacklocation[3],ChangeRegs[i].newESP); Stacklocation[3]=ChangeRegs[i].newESP; }
							if (ChangeRegs[i].changeEIP) { DbgPrint("Changing EIP from %x to %x",Stacklocation[0],ChangeRegs[i].newEIP); Stacklocation[0]=ChangeRegs[i].newEIP; }

							
							DbgPrint("Setting the resume flag and continue\n");
							x->RF=1; //resume flag
							//x->TF=1;

						
							//reset bp
							switch (i)
							{
							case 0: 
								DR_6.B0=0;
								break;

							case 1:
								DR_6.B1=0;
								break;

							case 2:
								DR_6.B2=0;
								break;

							case 3:
								DR_6.B3=0;                                
							}

							
							__asm
							{
								mov eax,DR_6
								mov dr6,eax									
							}

							if (globaldebug)
							{
								__asm
								{
									mov eax,DR_7
									mov dr7,eax //don't touch debug regs after this
								}
							}

							return 1;
						}
					}
				}
				
			}

			

			if (
				((DR_7.L0) && (DR_6.B0) && (DR_0>=DebuggedAddress) && (DR_0<=DebuggedAddress+DebuggedAddressLength)) ||
				((DR_7.L1) && (DR_6.B1) && (DR_1>=DebuggedAddress) && (DR_1<=DebuggedAddress+DebuggedAddressLength)) ||
				((DR_7.L2) && (DR_6.B2) && (DR_2>=DebuggedAddress) && (DR_2<=DebuggedAddress+DebuggedAddressLength)) ||
				((DR_7.L3) && (DR_6.B3) && (DR_3>=DebuggedAddress) && (DR_3<=DebuggedAddress+DebuggedAddressLength))
				)
				
			{
				WORD this_ss;
				__asm
				{
					mov ax,ss
					mov [this_ss],ax
				}


				DbgPrint("this ss=%x\n",this_ss);


				DbgPrint("I'm going to handle this! The OS won't know what happened!\n");
				result=1;

				DR_6.B0=0;
				DR_6.B1=0;
				DR_6.B2=0;
				DR_6.B3=0;                                
	
				__asm
				{
					mov eax,DR_6
					mov dr6,eax							
				}

				if (BufferSize<50)
				{
					int spot;
					spot=BufferSize;					
					BufferSize++;
					DebugEvents[spot].EAX=Stacklocation[-1];
					DebugEvents[spot].EBX=Stacklocation[-4];
					DebugEvents[spot].ECX=Stacklocation[-2];
					DebugEvents[spot].EDX=Stacklocation[-3];
					DebugEvents[spot].ESI=Stacklocation[-7];
					DebugEvents[spot].EDI=Stacklocation[-8];
					DebugEvents[spot].EBP=Stacklocation[-6];
					DebugEvents[spot].ESP=Stacklocation[3];
					DebugEvents[spot].EIP=Stacklocation[0];					
				}



			}
			else
			{
                DbgPrint("It was a debug event, but not one that I expected. I'll just let it go through\n");
			}

			//reset the GD flag of DR7 on exit (GD is a detection of the DebugRegs being modified

            if (DR_6.BS)
			{
				//single step 
				//if the previous instruction was a debugreg access then save the current debugregs values to ownprocessdebugregs (in case is was a write access)
				//then put my own debugregs back
			}

			if (DR_6.BD)
			{
				//the debugregs got accesses
				//save the current debugregs
				//set the ownprocessdebugregs back to the debugregs
                //do a single step (set the step flag in eflags	
				

				//set the debugregs back to what the program put them to
				DbgPrint("The debugregs got accessed\n");


			}

			if (globaldebug)
			{
				__asm
				{
					mov eax,DR_7
					mov dr7,eax //don't touch debug regs after this
				}
			}
			

		}
	}
	else
	{
		if (!globaldebug)
			return 0; //handle original

		DbgPrint("Int1 in other process\n");
		//not the process we want it to be, check if it is caused by us or not:
		if (
			((DR_7.L0) && (DR_6.B0) && (DR_0>=DebuggedAddress) && (DR_0<=DebuggedAddress+DebuggedAddressLength)) ||
			((DR_7.L1) && (DR_6.B1) && (DR_1>=DebuggedAddress) && (DR_1<=DebuggedAddress+DebuggedAddressLength)) ||
			((DR_7.L2) && (DR_6.B2) && (DR_2>=DebuggedAddress) && (DR_2<=DebuggedAddress+DebuggedAddressLength)) ||
			((DR_7.L3) && (DR_6.B3) && (DR_3>=DebuggedAddress) && (DR_3<=DebuggedAddress+DebuggedAddressLength))
			)
		{
			DbgPrint("Caused by driver, ignore int1\n");

			DR_6.B0=0;
			DR_6.B1=0;
			DR_6.B2=0;
			DR_6.B3=0;                                

			__asm
			{
				mov eax,DR_6
				mov dr6,eax							
				mov eax,DR_7
				mov dr7,eax //don't touch debug regs after this
			}


			return 1; //don't handle the original handler
		}
	}

	if (globaldebug)
	{
		__asm
		{
			mov eax,DR_7
			mov dr7,eax 
		}
	}
	return result;
#else
	return 0;
#endif
}

#ifndef AMD64
_declspec( naked ) void interrupt1( void )
{

	__asm{ 

		//cmp [DebuggedProcessID],0 //there's currently no debugging gong on so quit
		//je Original
		
		PUSHAD	//32		
		push ds //4
		push es //4
		push gs //4
		push fs //4

		mov ax,0x23
		mov ds,ax
		mov es,ax
		mov gs,ax
		mov ax,0x30
		mov fs,ax

		mov eax,esp
		add eax,48
		push eax //the location of the original stack
		PUSH 1 //int 1 identifier
		CALL GeneralHandler //call my regular int handler
		cmp eax,1 //if 1 then do no handle the original handler
		je Exit
		pop fs
		pop gs
		pop es
		pop ds
		POPAD
//Original:
	    JMP [Int1Address]

Exit:
		pop fs	
		pop gs
		pop es
		pop ds
		POPAD
		
		IRETD
	};

} 

_declspec( naked ) void interrupt3( void )
{
	__asm{ 
		//iretd //return

		cmp [DebuggedProcessID],0 //there's currently no debugging gong on so quit
		je Original

		PUSHAD	//32		
		push ds //4
		push es //4
		push gs //4
		push fs //4

		mov ax,0x23
		mov ds,ax
		mov es,ax
		mov gs,ax
		mov ax,0x30
		mov fs,ax

		mov eax,esp
		add eax,48
		push eax //the location of the original stack
		PUSH 3 //int 3 identifier
	    CALL GeneralHandler //call my regular int handler
		cmp eax,1 //if 1 then do no handle the original handler
		je Exit
		pop fs
		pop gs
		pop es
		pop ds
		POPAD
Original:
	    JMP [Int3Address]

Exit:
		pop fs	
		pop gs
		pop es
		pop ds
		POPAD		
		IRETD
	};

}

_declspec( naked ) void interruptD1( void )
{
	__asm{ 
		//iretd //return

		cmp [DebuggedProcessID],0 //there's currently no debugging gong on so quit
		je Original

		PUSHAD	//32		
		push ds //4
		push es //4
		push gs //4
		push fs //4

		mov ax,0x23
		mov ds,ax
		mov es,ax
		mov gs,ax
		mov ax,0x30
		mov fs,ax

		mov eax,esp
		add eax,48
		push eax //the location of the original stack
		PUSH 0xD1 //int d1 identifier
	    CALL GeneralHandler //call my regular int handler
		cmp eax,1 //if 1 then do no handle the original handler
		je Exit
		pop fs
		pop gs
		pop es
		pop ds
		POPAD
Original:
	    JMP [IntD1Address]

Exit:
		pop fs	
		pop gs
		pop es
		pop ds
		POPAD		
		IRETD
	};

}


//int1 hook section
//int1 gets rewritten with a jmp to int1apihook declared down here
//OriginalInt1handler gets the auto assembled code to do the original bytes followed by a jmp to the code after the jmp in the original int1 code
_declspec( naked ) void OriginalInt1handler(void)
{
	__asm
	{
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
	}
}  //200 bytes should be enough for the original code+jmp back

_declspec( naked ) void int1apihook(void)
{
    //if 1 do not handle, else do handle
	__asm
	{
	

		cmp [DebuggedProcessID],0 //there's currently no debugging gong on so quit
		je Original

		PUSHAD	//32		
		push ds //4
		push es //4
		push gs //4
		push fs //4

		mov ax,0x23
		mov ds,ax
		mov es,ax
		mov gs,ax
		mov ax,0x30
		mov fs,ax

		mov eax,esp
		add eax,48
		push eax //the location of the original stack
		PUSH 0x1 //int 1 identifier
	    CALL GeneralHandler //call my regular int handler
		cmp eax,1 //if 1 then do no handle the original handler
		;je Exit
		jmp Exit

		pop fs
		pop gs
		pop es
		pop ds
		POPAD
Original:
		//all back to the original state, so lets continue with the original call
	    JMP OriginalInt1handler

Exit:
		//don't execute the original code and just exit. Restore all registers and return to the caller
		pop fs	
		pop gs
		pop es
		pop ds
		POPAD		
		IRETD
	};

}

#endif
