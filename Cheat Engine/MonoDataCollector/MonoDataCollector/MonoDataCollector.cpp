#include "stdafx.h"
#include "pipeserver.h"


HANDLE DataCollectorThread;
HANDLE SuicideThread;
HINSTANCE g_hInstance;

typedef enum _THREADINFOCLASS {
    ThreadBasicInformation,
    ThreadTimes,
    ThreadPriority,
    ThreadBasePriority,
    ThreadAffinityMask,
    ThreadImpersonationToken,
    ThreadDescriptorTableEntry,
    ThreadEnableAlignmentFaultFixup,
    ThreadEventPair_Reusable,
    ThreadQuerySetWin32StartAddress,
    ThreadZeroTlsCell,
    ThreadPerformanceCount,
    ThreadAmILastThread,
    ThreadIdealProcessor,
    ThreadPriorityBoost,
    ThreadSetTlsArrayAddress,   // Obsolete
    ThreadIsIoPending,
    ThreadHideFromDebugger,
    ThreadBreakOnTermination,
    ThreadSwitchLegacyState,
    ThreadIsTerminated,
    ThreadLastSystemCall,
    ThreadIoPriority,
    ThreadCycleTime,
    ThreadPagePriority,
    ThreadActualBasePriority,
    ThreadTebInformation,
    ThreadCSwitchMon,          // Obsolete
    ThreadCSwitchPmu,
    ThreadWow64Context,
    ThreadGroupInformation,
    ThreadUmsInformation,      // UMS
    ThreadCounterProfiling,
    ThreadIdealProcessorEx,
    MaxThreadInfoClass
} THREADINFOCLASS;

typedef int (NTAPI *ZWSETINFORMATIONTHREAD)(
    __in HANDLE ThreadHandle,
    __in THREADINFOCLASS ThreadInformationClass,
    __in_bcount(ThreadInformationLength) PVOID ThreadInformation,
    __in ULONG ThreadInformationLength
    );


DWORD WINAPI DataCollectorEntry(LPVOID lpThreadParameter)
{
	CPipeServer *pw;

#ifdef NDEBUG
	ZWSETINFORMATIONTHREAD ZwSetInformationThread=(ZWSETINFORMATIONTHREAD)GetProcAddress(GetModuleHandleA("ntdll.dll"), "ZwSetInformationThread");
	if (ZwSetInformationThread)
	{
		int r=ZwSetInformationThread(GetCurrentThread(), ThreadHideFromDebugger, NULL, 0);
		if (r!=0)
		{
			//OutputDebugStringA("No debug safety");
		}
	}
#endif


	pw=new CPipeServer();
	pw->Start();

	DataCollectorThread=0;
	delete pw;	

	if (SuicideThread)
		TerminateThread(SuicideThread, 0);
	
	Sleep(1000);

	FreeLibraryAndExitThread(g_hInstance, 0);
	return 0;
}

DWORD WINAPI SuicideCheck(LPVOID lpThreadParameter)
{

	Sleep(5000);

	//todo: Figure out a way to detect how to close.
	//if (shouldKillMyself())
	
	{

	//pipe:=CreateFile(pchar('\\.\pipe\'+pipename), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);  
		WCHAR datapipename[256];
		swprintf(datapipename, 256,L"\\\\.\\pipe\\cemonodc_pid%d", GetCurrentProcessId());

		HANDLE pipe;
		pipe=CreateFileW(datapipename, GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, 0);  
		if ((pipe) && (pipe!=INVALID_HANDLE_VALUE))
		{
			unsigned char command=MONOCMD_TERMINATE;
			DWORD bw;
			WriteFile(pipe, &command, 1, &bw, NULL); 
			return 0;
		}
		else
		{
			TerminateThread(DataCollectorThread, -1);
			FreeLibraryAndExitThread(g_hInstance, -1);
		}		
	}
}