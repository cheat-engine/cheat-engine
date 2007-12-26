// example-c.cpp : Defines the entry point for the DLL application.
//

//#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers
// Windows Header Files:
#include <windows.h>
#include "example-c.h"

int selfid;
int memorybrowserpluginid=-1; //initialize it to -1 to indicate failure (used by the DisablePlugin routine)

BOOL memorybrowserplugin(ULONG *disassembleraddress, ULONG *selected_disassembler_address, ULONG *hexviewaddress)
{
	Exported.ShowMessage("A Plugin function got executed");
	*disassembleraddress=*hexviewaddress; //make the disassembleraddress and hexviewaddress the same
	return TRUE;
}


BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
		MessageBox(0,"This plugin dll got loaded","C Plugin Example",MB_OK);
		break;

	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
	case DLL_PROCESS_DETACH:
		break;
	}
	
    return TRUE;
}

BOOL __stdcall GetVersion(struct PluginVersion *pv , int sizeofpluginversion)
{
	pv->version=1;
	pv->pluginname="C Example"; //exact strings like this are pointers to the string in the dll, so workable
	return TRUE;
}

BOOL __stdcall InitializePlugin(struct ExportedFunctions *ef , int pluginid)
{
	struct PLUGINTYPE1_INIT init;

	selfid=pluginid;
	Exported=*ef; //Exported is defined in the .h
	if (Exported.sizeofExportedFunctions!=sizeof(Exported))
		return FALSE;

	init.name="Sample plugin";
	init.callbackroutine=memorybrowserplugin;

	memorybrowserpluginid=Exported.RegisterFunction(pluginid,1,&init);
	if ( memorybrowserpluginid == -1 )
	{
		Exported.ShowMessage("Failure to register a plugin function");
		return FALSE;
	}
	
	
	Exported.ShowMessage("The \"Example C\" plugin got enabled");
	return TRUE;
}


BOOL __stdcall DisablePlugin(void)
{
	if (memorybrowserpluginid!=-1)
	{
		if ( Exported.UnregisterFunction(selfid,memorybrowserpluginid) == FALSE )
			Exported.ShowMessage("Failure to unregister a plugin function"); //nothing to be done about this. the plugin is being set on stand by...
	}

	return TRUE;
}

