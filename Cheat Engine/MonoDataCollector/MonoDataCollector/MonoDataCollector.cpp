#ifdef _WINDOWS
#include "stdafx.h"
#else
#include "macport.h"
#endif

#include "PipeServer.h"



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

#ifdef _WINDOWS
typedef int (NTAPI *ZWSETINFORMATIONTHREAD)(
    __in HANDLE ThreadHandle,
    __in THREADINFOCLASS ThreadInformationClass,
    __in_bcount(ThreadInformationLength) PVOID ThreadInformation,
    __in ULONG ThreadInformationLength
    );
#endif



DWORD WINAPI DataCollectorEntry(LPVOID lpThreadParameter)
{
	CPipeServer *pw;
    
#ifdef _WINDOWS
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
#endif


	pw=new CPipeServer();
    
   
    
	pw->Start();

	DataCollectorThread=0;
	delete pw;	

	if (SuicideThread)
		TerminateThread(SuicideThread, 0);
	
	Sleep(1000);

#ifdef _WINDOWS
	FreeLibraryAndExitThread(g_hInstance, 0);
#endif
	return 0;
}

#ifndef _WINDOWS
#include <syslog.h>
void MacPortEntryPoint(void *param)
{
    
    pthread_setname_np("MonoDataCollector Thread");
    //called by a thread
    //openlog((char*)"Cheat Engine MDC", 0, LOG_USER);
    //setlogmask(LOG_UPTO(LOG_DEBUG));
    //syslog(LOG_NOTICE, (char*)"CELOGX:FUUUCK");
    
    //OutputDebugString((char*)"MonoDataCollector for Mac loaded");
   
    DataCollectorEntry(param);
    
}
#endif

#ifndef _WINDOWS
#endif


DWORD WINAPI SuicideCheck(LPVOID lpThreadParameter)
{

	Sleep(5000);
    
#ifdef _WINDOWS

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
#endif
    return 0;
}
