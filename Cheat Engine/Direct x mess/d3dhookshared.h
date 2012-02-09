#ifndef D3DHOOKSHARED_H
#define D3DHOOKSHARED_H

#include <windows.h>


#pragma pack(1)
typedef struct 
{
	char CheatEngineDir[256];
	UINT64 dxgi_present;
	UINT64 d3d9_present;
	UINT64 d3d9_reset;

	UINT64 d3d9_drawprimitive;
	UINT64 d3d9_drawindexedprimitive;
	UINT64 d3d9_drawprimitiveup;
	UINT64 d3d9_drawindexedprimitiveup;
	UINT64 d3d9_drawrectpatch;
	UINT64 d3d9_drawtripatch;

	UINT64 d3d10_drawindexed;
	UINT64 d3d10_draw;
	UINT64 d3d10_drawindexedinstanced;
	UINT64 d3d10_drawinstanced;
	UINT64 d3d10_drawauto;	

	UINT64 d3d11_drawindexed;
	UINT64 d3d11_draw;
	UINT64 d3d11_drawindexedinstanced;
	UINT64 d3d11_drawinstanced;
	UINT64 d3d11_drawauto;	

	UINT64 dxgi_newpresent; //From the dll: set this to the function address in the dll
	UINT64 d3d9_newpresent;                              
	UINT64 d3d9_newreset;

	UINT64 d3d9_newdrawprimitive;
	UINT64 d3d9_newdrawindexedprimitive;
	UINT64 d3d9_newdrawprimitiveup;
	UINT64 d3d9_newdrawindexedprimitiveup;
	UINT64 d3d9_newdrawrectpatch;
	UINT64 d3d9_newdrawtripatch;

	UINT64 d3d10_newdrawindexed;
	UINT64 d3d10_newdraw;
	UINT64 d3d10_newdrawindexedinstanced;
	UINT64 d3d10_newdrawinstanced;
	UINT64 d3d10_newdrawauto;	

	UINT64 d3d11_newdrawindexed;
	UINT64 d3d11_newdraw;
	UINT64 d3d11_newdrawindexedinstanced;
	UINT64 d3d11_newdrawinstanced;
	UINT64 d3d11_newdrawauto;	

	UINT64 dxgi_originalpresent; //From the dll: set this to the variable that should get the address of the original function with no hook
	UINT64 d3d9_originalpresent;   
	UINT64 d3d9_originalreset;

	UINT64 d3d9_originaldrawprimitive;
	UINT64 d3d9_originaldrawindexedprimitive;
	UINT64 d3d9_originaldrawprimitiveup;
	UINT64 d3d9_originaldrawindexedprimitiveup;
	UINT64 d3d9_originaldrawrectpatch;
	UINT64 d3d9_originaldrawtripatch;

	UINT64 d3d10_originaldrawindexed;
	UINT64 d3d10_originaldraw;
	UINT64 d3d10_originaldrawindexedinstanced;
	UINT64 d3d10_originaldrawinstanced;
	UINT64 d3d10_originaldrawauto;	

	UINT64 d3d11_originaldrawindexed;
	UINT64 d3d11_originaldraw;
	UINT64 d3d11_originaldrawindexedinstanced;
	UINT64 d3d11_originaldrawinstanced;
	UINT64 d3d11_originaldrawauto;	

	int wireframe;
	int disabledzbuffer;

	int hookwnd;
	int clipmouseinwindow;
	int clickedoverlay;
	int clickedx;
	int clickedy;

	struct
	{
		int hasconsole;
		int consolevisible;		
		DWORD consolekey;

		int overlayid;

		struct
		{
			UINT uMsg;
			UINT64 wParam;
			UINT64 lParam;
			DWORD character;
		} lastmessage;
		
		

	} console;

	DWORD lastHwnd;
	int MouseOverlayId; //set to -1 if no mouse overlayd ID is set, otherwise it contains an overlay ID to be used and rendered at the current mouse coords
	

	int OverLayHasUpdate;
	int overlaycount;


	struct _bitmap {
		int valid;
		int updatedpos;
		int updatedresource;

		int width;
		int height;
		int x;
		int y;
		float alphaBlend;
		int resourcesize;
		int resourceoffset;
	} resources[100];	
	//etc...etc...etc...
} *PD3DHookShared;
#pragma pack()


#endif