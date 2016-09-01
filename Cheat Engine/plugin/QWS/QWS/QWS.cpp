#include "stdafx.h"


using namespace std;
map<ULONG_PTR, PSAPI_WORKING_SET_BLOCK> LastMap;
ULONGLONG LastMapTime = 0;

ExportedFunctions g_ef;
int g_pluginid, callbackid;
PVOID OriginalVQE = NULL;

DWORD BlockProtectionToPageProtection(int p)
{
#ifdef IWANTITALL
	return PAGE_EXECUTE_READWRITE;
#endif


	switch (p)
	{
		
	   case 0: return PAGE_NOACCESS;
	   case	1: return PAGE_READONLY;
	   case	2: return PAGE_EXECUTE;
	   case	3: return PAGE_EXECUTE_READ;
	   case	4: return PAGE_READWRITE;
	   case	5: return PAGE_WRITECOPY;
	   case	6: return PAGE_EXECUTE_READWRITE;
	   case 7: return PAGE_EXECUTE_WRITECOPY;
	   case 8: return PAGE_NOACCESS;
	   case 9: return PAGE_READONLY | PAGE_NOCACHE;
	   case 10: return PAGE_EXECUTE | PAGE_NOCACHE;
	   case 11: return PAGE_EXECUTE_READ | PAGE_NOCACHE;
	   case 12: return PAGE_READWRITE | PAGE_NOCACHE;
	   case 13: return PAGE_WRITECOPY | PAGE_NOCACHE;
	   case 14: return PAGE_EXECUTE_READWRITE | PAGE_NOCACHE;
	   case 15: return PAGE_EXECUTE_WRITECOPY | PAGE_NOCACHE;
	   case 16: return PAGE_NOACCESS;
	   case 17: return PAGE_READONLY | PAGE_GUARD;
	   case 18: return PAGE_EXECUTE | PAGE_GUARD;
	   case 19: return PAGE_EXECUTE_READ | PAGE_GUARD;
	   case 20: return PAGE_READWRITE | PAGE_GUARD;
	   case 21: return PAGE_WRITECOPY | PAGE_GUARD;
	   case 22: return PAGE_EXECUTE_READWRITE | PAGE_GUARD;
	   case 23: return PAGE_EXECUTE_WRITECOPY | PAGE_GUARD;

	   case 24: return PAGE_NOACCESS;
	   case 25: return PAGE_READONLY | PAGE_GUARD | PAGE_NOCACHE;
	   case 26: return PAGE_EXECUTE | PAGE_GUARD | PAGE_NOCACHE;
	   case 27: return PAGE_EXECUTE_READ | PAGE_GUARD | PAGE_NOCACHE;
	   case 28: return PAGE_READWRITE | PAGE_GUARD | PAGE_NOCACHE;
	   case 29: return PAGE_WRITECOPY | PAGE_GUARD | PAGE_NOCACHE;
	   case 30: return PAGE_EXECUTE_READWRITE | PAGE_GUARD | PAGE_NOCACHE;
	   case 31: return PAGE_EXECUTE_WRITECOPY | PAGE_GUARD | PAGE_NOCACHE;	   
	}

	return PAGE_NOACCESS;

}


SIZE_T WINAPI NewVirtualQueryEx(_In_ HANDLE hProcess, _In_opt_ LPCVOID lpAddress, PMEMORY_BASIC_INFORMATION lpBuffer, SIZE_T dwLength)
{
	PSAPI_WORKING_SET_INFORMATION *info = NULL;
	int size = 4096*4;
	BOOL toosmall = TRUE;

	if (dwLength != sizeof(MEMORY_BASIC_INFORMATION))
		return 0;

	if (GetTickCount64() > LastMapTime + 2000)
	{
		do
		{

			info = (PSAPI_WORKING_SET_INFORMATION *)calloc(1, size);

			if (info == NULL)
				return 0;

			//info->VirtualAttributes.

			if (!QueryWorkingSet(hProcess, info, size))
			{
				toosmall = (GetLastError() == ERROR_BAD_LENGTH);
				free(info);

				if (toosmall)
					size *= 4; //NumberOfEntries does not function in win7	
			}
			else
				toosmall = FALSE;

		} while (toosmall);

		LastMap.clear();
		if (info)
		{
			for (ULONG_PTR i = 0; i < info->NumberOfEntries; i++)
				LastMap[info->WorkingSetInfo[i].VirtualPage] = info->WorkingSetInfo[i];

			free(info);
			info = NULL;
		}

		LastMapTime = GetTickCount64();
	}

	//scan LastMap
	map<ULONG_PTR, PSAPI_WORKING_SET_BLOCK>::iterator i = LastMap.lower_bound((ULONG_PTR)lpAddress >> 12);

	if (i != LastMap.end())
	{
		UINT_PTR lastPage = i->first;
		int currentprotection;

		if (i->first > (ULONG_PTR)lpAddress >> 12)
		{
			//unreadable from lpAddress to i->first
			lpBuffer->AllocationBase = 0;
			lpBuffer->AllocationProtect = 0;
			lpBuffer->BaseAddress = (PVOID)((ULONG_PTR)lpAddress & (~0xfff));
			lpBuffer->Protect = 0;
			lpBuffer->RegionSize = (i->first << 12) - ((ULONG_PTR)lpAddress & (~0xfff));
			lpBuffer->State = MEM_FREE;
			lpBuffer->Type = 0;				
			return sizeof(MEMORY_BASIC_INFORMATION);
		}

		//still here
		lpBuffer->AllocationBase = (PVOID)((ULONG_PTR)lpAddress & (~0xfff));
		lpBuffer->AllocationProtect = PAGE_EXECUTE_READWRITE;
		lpBuffer->BaseAddress = (PVOID)((ULONG_PTR)lpAddress & (~0xfff));
		lpBuffer->Protect = BlockProtectionToPageProtection(i->second.Protection);
		lpBuffer->RegionSize = 4096;
		lpBuffer->State = MEM_COMMIT;
		lpBuffer->Type = 0;

		currentprotection = i->second.Protection;

		i++;

		while (i != LastMap.end())
		{
			ULONG_PTR Page = i->first;
			PSAPI_WORKING_SET_BLOCK b = i->second;

			if ((Page!=lastPage+1) || (i->second.Protection!=currentprotection)) //different block or protection
				return sizeof(MEMORY_BASIC_INFORMATION);

			lpBuffer->RegionSize += 4096;
			lastPage = Page;
			i++;
		}

		return sizeof(MEMORY_BASIC_INFORMATION);

	}
	return 0;
}

void __stdcall PointersChanged(int reserved)
{
	
	//OutputDebugStringA("Changed");
	
	if (*(PVOID *)(g_ef.VirtualQueryEx) != (PVOID)NewVirtualQueryEx)
	{
		OriginalVQE = *(PVOID *)(g_ef.VirtualQueryEx);
		*(PVOID *)(g_ef.VirtualQueryEx) = (PVOID)NewVirtualQueryEx;
	}

	
}

BOOL __stdcall CEPlugin_GetVersion(PPluginVersion pv, int sizeofpluginversion)
{
	pv->version = CESDK_VERSION;
	pv->pluginname = "QueryWorkingSet instead of VirtualQueryEx";
	return TRUE;
}

BOOL __stdcall CEPlugin_InitializePlugin(PExportedFunctions ef, int pluginid)
{

	POINTERREASSIGNMENTPLUGIN_INIT init;

	g_ef = *ef;
	g_pluginid = pluginid;

	init.callbackroutine = PointersChanged;
	callbackid=ef->RegisterFunction(pluginid, ptFunctionPointerchange, &init);

	PointersChanged(0);

	return TRUE;
}


BOOL __stdcall CEPlugin_DisablePlugin(void)
{
	//clean up memory you might have allocated

	g_ef.UnregisterFunction(g_pluginid, callbackid);

	if (OriginalVQE)
		*(PVOID *)(g_ef.VirtualQueryEx) = OriginalVQE;

	return TRUE;
}
