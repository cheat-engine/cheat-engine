#ifndef D3DHOOKSHARED_H
#define D3DHOOKSHARED_H

#include <windows.h>


#pragma pack(1)

/*
CE Rendering:
First it checks for updated textures. (See 1.1)
Then it renders based on the render command list. (See 1.2)


1.1: How ce updates a texture:
                 CE                                  RENDERER
Sets texturelistHasUpdate to 0
Sets hasBeenUpdated to 0
allocate a new block of memory
copy the imagedata to there
Finally update AddressOfTexture
set hasBeenUpdated to 1
And set texturelistHasUpdate to 1                     
														The renderer sees that texturelistHasUpdate is 1
														It goes through the list of textures looking for hasBeenUpdated=1
It then waits for the HasHandledTextureUpdate			If found, it destroys the texture associated with the specific spot
<waiting>			         							Then creates a new texture based on the data of AddressofTexture
														Set hasBeenUpdated to 0
														continues looking for other updated textures
														Then sets texturelistHasUpdate to 0
														And calls setEvent for HasHandledTextureUpdate
<ce wakes up and does stuff> 

*/

typedef struct
{
	//texture coordinates for each character
	float offset; //offset where this character starts
	float charwidth; //width in pixel of this character	
} CHARINFO, *PCHARINFO;


typedef struct
{
	float charheight; //height in pixels of each character
	float fullwidth; //width in pixels of the full fontmap
	CHARINFO charinfo[96];
} FONTMAP, *PFONTMAP;

typedef volatile struct
{
	UINT64 AddressOfTexture;
	UINT64 AddressOfFontmap;
	int size;
	int hasBeenUpdated;	
} *PTextureEntry;




//The rendercommand is used in an array inside PD3DHookShared. It can change often
enum RenderCommandEnum { rcEndOfCommandlist=0,  //Stop going through the list
						 rcIgnored=1,  //Ignore (Being updated)
						 rcDrawSprite=2,		//Render the sprite at the given position
						 rcDrawFont=3			//Render some text at the given coordinates. The string is located at  "addressoftext"						 
   					};

typedef volatile struct
{
	int Command;
	float x;
	float y;
	float alphablend;

	float centerX; //
	float centerY;
	float rotation;

	union
	{
		struct
		{
			int width;
			int height;				
			int textureid;
		} sprite;

		struct
		{
			UINT64 addressoftext;
			int fontid;
		} font;
	};

} D3DRenderCommand, *PD3DRenderCommand;

typedef volatile struct 
{
	char CheatEngineDir[256];
	char SnapShotDir[256];
	UINT64 dxgi_present;
	UINT64 dxgi_resizebuffers;
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
	UINT64 dxgi_newresizebuffers;
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
	UINT64 dxgi_originalresizebuffers;
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
		int cursorid;

		struct
		{
			UINT uMsg;
			UINT64 wParam;
			UINT64 lParam;
			DWORD character;
		} lastmessage;
		
		

	} console;

	DWORD lastHwnd;
 
	int texturelistHasUpdate; //If 1 this means that CE might be waiting for the HasHandledTextureUpdate event (if it is a high priority update)
	int textureCount;	
	UINT64 texturelist; //offset into texturelist based on the start of the shared object (once setup, this does not change)
	UINT64 TextureLock; //

	UINT64 CommandlistLock;
	int UseCommandlistLock; //set to 1 if you wish to make use of the command list locking feature (might slow down due to acuiring/releasing of the event)
	int hasOnKey;


    DWORD snapshotKey;
	DWORD smallSnapshotKey;

	UINT64 snapshotDone; //Event to signal that a snapshot is done
	int snapshotImageFormat;
	int snapshotcount;
	int progressiveSnapshot; //set to 1 if you do not wish the snapshot to clear the screen before each draw. (This makes it easier to see how a scene was build up)
	int alsoClearDepthBuffer; //set to 1 if you also want the depth buffer to be cleared before each draw
	int savePNGSeperateAsWell;
	int canDoSnapshot; //set to 0 after making a snapshot. CE will set it to 1 when ready
	DWORD initialized; //set to 0xdbcedbce
	
	D3DRenderCommand RenderCommands[100000];

/*

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
		int hasTransparency; //set to 1 if 255,255,255=100% transpart
		float alphaBlend;
		int resourcesize;
		int resourceoffset;
	} resources[100];	*/
	//etc...etc...etc...
} *PD3DHookShared;
#pragma pack()


#endif