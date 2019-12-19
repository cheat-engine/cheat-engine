// DXHookBase.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include "Dxhookbase.h"

#ifdef DEBUG
#undef DEBUG

#include "DX11Hook.h"
#include "DX10Hook.h"
#include "DX9Hook.h"
#endif

//Explenation about the DEBUG define.
//If DEBUG is used the hook assumes that this hook is compiled in with a dx program that calls the functions manually
//If DEBUG is not used it will assume it was loaded from dll injection

PD3DHookShared shared;



HANDLE hasClickEvent;
HANDLE handledClickEvent;

HANDLE hasKeyboardEvent;
HANDLE handledKeyboardEvent;



typedef HRESULT     (__stdcall *D3D9_RESET_ORIGINAL)(IDirect3DDevice9 *Device, D3DPRESENT_PARAMETERS *pPresentationParameters);
typedef HRESULT     (__stdcall *D3D9_PRESENT_ORIGINAL)(IDirect3DDevice9 *Device, RECT* pSourceRect,CONST RECT* pDestRect,HWND hDestWindowOverride,CONST RGNDATA* pDirtyRegion);
typedef HRESULT		(__stdcall *D3D9_DRAWPRIMITIVE_ORIGINAL)(IDirect3DDevice9 *Device, D3DPRIMITIVETYPE PrimitiveType,UINT StartVertex,UINT PrimitiveCount);
typedef HRESULT		(__stdcall *D3D9_DRAWINDEXEDPRIMITIVE_ORIGINAL)(IDirect3DDevice9 *Device, D3DPRIMITIVETYPE PrimitiveType,INT BaseVertexIndex,UINT MinVertexIndex,UINT NumVertices,UINT startIndex,UINT primCount);
typedef HRESULT		(__stdcall *D3D9_DRAWPRIMITIVEUP_ORIGINAL)(IDirect3DDevice9 *Device, D3DPRIMITIVETYPE PrimitiveType,UINT PrimitiveCount,CONST void* pVertexStreamZeroData,UINT VertexStreamZeroStride);
typedef HRESULT		(__stdcall *D3D9_DRAWINDEXEDPRIMITIVEUP_ORIGINAL)(IDirect3DDevice9 *Device, D3DPRIMITIVETYPE PrimitiveType,UINT MinVertexIndex,UINT NumVertices,UINT PrimitiveCount,CONST void* pIndexData,D3DFORMAT IndexDataFormat,CONST void* pVertexStreamZeroData,UINT VertexStreamZeroStride);
typedef HRESULT		(__stdcall *D3D9_DRAWRECTPATCH_ORIGINAL)(IDirect3DDevice9 *device, UINT Handle,CONST float* pNumSegs,CONST D3DRECTPATCH_INFO* pRectPatchInfo);
typedef HRESULT		(__stdcall *D3D9_DRAWTRIPATCH_ORIGINAL)(IDirect3DDevice9 *device, UINT Handle,CONST float* pNumSegs,CONST D3DTRIPATCH_INFO* pTriPatchInfo);

typedef HRESULT		(__stdcall *D3D10_DRAWINDEXED_ORIGINAL)(ID3D10Device *device, UINT IndexCount, UINT StartIndexLocation, INT BaseVertexLocation);
typedef HRESULT		(__stdcall *D3D10_DRAW_ORIGINAL)(ID3D10Device *device, UINT VertexCount, UINT StartVertexLocation);
typedef HRESULT		(__stdcall *D3D10_DRAWINDEXEDINSTANCED_ORIGINAL)(ID3D10Device *device, UINT IndexCountPerInstance, UINT InstanceCount, UINT StartIndexLocation, INT BaseVertexLocation, UINT StartInstanceLocation);
typedef HRESULT		(__stdcall *D3D10_DRAWINSTANCED_ORIGINAL)(ID3D10Device *device, UINT VertexCountPerInstance, UINT InstanceCount, UINT StartVertexLocation, UINT StartInstanceLocation);
typedef HRESULT		(__stdcall *D3D10_DRAWAUTO_ORIGINAL)(ID3D10Device *device);

typedef HRESULT		(__stdcall *D3D11_DRAWINDEXED_ORIGINAL)(ID3D11DeviceContext *dc, UINT IndexCount, UINT StartIndexLocation, INT BaseVertexLocation);
typedef HRESULT		(__stdcall *D3D11_DRAW_ORIGINAL)(ID3D11DeviceContext *dc, UINT VertexCount, UINT StartVertexLocation);
typedef HRESULT		(__stdcall *D3D11_DRAWINDEXEDINSTANCED_ORIGINAL)(ID3D11DeviceContext *dc, UINT IndexCountPerInstance, UINT InstanceCount, UINT StartIndexLocation, INT BaseVertexLocation, UINT StartInstanceLocation);
typedef HRESULT		(__stdcall *D3D11_DRAWINSTANCED_ORIGINAL)(ID3D11DeviceContext *dc, UINT VertexCountPerInstance, UINT InstanceCount, UINT StartVertexLocation, UINT StartInstanceLocation);
typedef HRESULT		(__stdcall *D3D11_DRAWAUTO_ORIGINAL)(ID3D11DeviceContext *dc);



typedef HRESULT     (__stdcall *DXGI_PRESENT_ORIGINAL)(IDXGISwapChain *x, UINT SyncInterval, UINT Flags);
typedef HRESULT     (__stdcall *DXGI_RESIZEBUFFERS_ORIGINAL)(IDXGISwapChain *x, UINT BufferCount, UINT Width, UINT Height, DXGI_FORMAT NewFormat, UINT SwapChainFlags);

typedef void        (__stdcall *D3D10PlusHookPresentAPICall)(IDXGISwapChain *swapchain, void *device, PD3DHookShared shared);
typedef void        (__stdcall *D3D9HookPresentAPICall)(IDirect3DDevice9 *device, PD3DHookShared shared);
typedef HRESULT     (__stdcall *D3D9HookResetAPICall)(D3D9_RESET_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRESENT_PARAMETERS *pPresentationParameters);


typedef HRESULT		(__stdcall *D3D9HookDrawPrimitiveAPICall)(D3D9_DRAWPRIMITIVE_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRIMITIVETYPE PrimitiveType,UINT StartVertex,UINT PrimitiveCount);
typedef HRESULT 	(__stdcall *D3D9HookDrawIndexedPrimitiveAPICall)(D3D9_DRAWINDEXEDPRIMITIVE_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRIMITIVETYPE PrimitiveType,INT BaseVertexIndex,UINT MinVertexIndex,UINT NumVertices,UINT startIndex,UINT primCount);
typedef HRESULT 	(__stdcall *D3D9HookDrawPrimitiveUPAPICall)(D3D9_DRAWPRIMITIVEUP_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRIMITIVETYPE PrimitiveType,UINT PrimitiveCount,CONST void* pVertexStreamZeroData,UINT VertexStreamZeroStride);
typedef HRESULT 	(__stdcall *D3D9HookDrawIndexedPrimitiveUPAPICall)(D3D9_DRAWINDEXEDPRIMITIVEUP_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRIMITIVETYPE PrimitiveType,UINT MinVertexIndex,UINT NumVertices,UINT PrimitiveCount,CONST void* pIndexData,D3DFORMAT IndexDataFormat,CONST void* pVertexStreamZeroData,UINT VertexStreamZeroStride);
typedef HRESULT 	(__stdcall *D3D9HookDrawRectPatchAPICall)(D3D9_DRAWRECTPATCH_ORIGINAL originalfunction, IDirect3DDevice9 *device, UINT Handle,CONST float* pNumSegs,CONST D3DRECTPATCH_INFO* pRectPatchInfo);
typedef HRESULT 	(__stdcall *D3D9HookDrawTriPatchAPICall)(D3D9_DRAWTRIPATCH_ORIGINAL originalfunction, IDirect3DDevice9 *device, UINT Handle,CONST float* pNumSegs,CONST D3DTRIPATCH_INFO* pTriPatchInfo);


typedef HRESULT		(__stdcall *D3D10HookDrawIndexedAPICall)(D3D10_DRAWINDEXED_ORIGINAL originalfunction, ID3D10Device *device, UINT IndexCount, UINT StartIndexLocation, INT BaseVertexLocation);
typedef HRESULT		(__stdcall *D3D10HookDrawAPICall)(D3D10_DRAW_ORIGINAL originalfunction, ID3D10Device *device, UINT VertexCount, UINT StartVertexLocation);
typedef HRESULT		(__stdcall *D3D10HookDrawIndexedInstancedAPICall)(D3D10_DRAWINDEXEDINSTANCED_ORIGINAL originalfunction, ID3D10Device *device, UINT IndexCountPerInstance, UINT InstanceCount, UINT StartIndexLocation, INT BaseVertexLocation, UINT StartInstanceLocation);
typedef HRESULT		(__stdcall *D3D10HookDrawInstancedAPICall)(D3D10_DRAWINSTANCED_ORIGINAL originalfunction, ID3D10Device *device, UINT VertexCountPerInstance, UINT InstanceCount, UINT StartVertexLocation, UINT StartInstanceLocation);
typedef HRESULT		(__stdcall *D3D10HookDrawAutoAPICall)(D3D10_DRAWAUTO_ORIGINAL originalfunction, ID3D10Device *device);

typedef HRESULT		(__stdcall *D3D11HookDrawIndexedAPICall)(D3D11_DRAWINDEXED_ORIGINAL originalfunction, ID3D11DeviceContext *dc, UINT IndexCount, UINT StartIndexLocation, INT BaseVertexLocation);
typedef HRESULT		(__stdcall *D3D11HookDrawAPICall)(D3D11_DRAW_ORIGINAL originalfunction, ID3D11DeviceContext *dc, UINT VertexCount, UINT StartVertexLocation);
typedef HRESULT		(__stdcall *D3D11HookDrawIndexedInstancedAPICall)(D3D11_DRAWINDEXEDINSTANCED_ORIGINAL originalfunction, ID3D11DeviceContext *dc, UINT IndexCountPerInstance, UINT InstanceCount, UINT StartIndexLocation, INT BaseVertexLocation, UINT StartInstanceLocation);
typedef HRESULT		(__stdcall *D3D11HookDrawInstancedAPICall)(D3D11_DRAWINSTANCED_ORIGINAL originalfunction, ID3D11DeviceContext *dc, UINT VertexCountPerInstance, UINT InstanceCount, UINT StartVertexLocation, UINT StartInstanceLocation);
typedef HRESULT		(__stdcall *D3D11HookDrawAutoAPICall)(D3D11_DRAWAUTO_ORIGINAL originalfunction, ID3D11DeviceContext *dc);




typedef IDirect3D9* (__stdcall *DIRECT3DCREATE9)(UINT SDKVersion); 
typedef HRESULT     (__stdcall *D3D10CREATEDEVICEANDSWAPCHAIN)(IDXGIAdapter *pAdapter, D3D10_DRIVER_TYPE DriverType, HMODULE Software, UINT Flags, UINT SDKVersion, DXGI_SWAP_CHAIN_DESC *pSwapChainDesc, IDXGISwapChain **ppSwapChain, ID3D10Device **ppDevice);
typedef HRESULT     (__stdcall *D3D11CREATEDEVICEANDSWAPCHAIN)(IDXGIAdapter *pAdapter, D3D_DRIVER_TYPE DriverType, HMODULE Software, UINT Flags, CONST D3D_FEATURE_LEVEL* pFeatureLevels, UINT FeatureLevels, UINT SDKVersion, CONST DXGI_SWAP_CHAIN_DESC* pSwapChainDesc, IDXGISwapChain** ppSwapChain, ID3D11Device** ppDevice, D3D_FEATURE_LEVEL* pFeatureLevel, ID3D11DeviceContext** ppImmediateContext );

  
    




D3D10CREATEDEVICEANDSWAPCHAIN d3d10create=NULL;
D3D11CREATEDEVICEANDSWAPCHAIN d3d11create=NULL;
DIRECT3DCREATE9 D3DCreate9=NULL;

DXGI_PRESENT_ORIGINAL DXGI_Present_Original=NULL;
DXGI_RESIZEBUFFERS_ORIGINAL DXGI_ResizeBuffers_Original=NULL;
D3D10PlusHookPresentAPICall D3D11Hook_SwapChain_Present=NULL;
D3D10PlusHookPresentAPICall D3D11Hook_SwapChain_ResizeBuffers=NULL;
D3D10PlusHookPresentAPICall D3D10Hook_SwapChain_Present=NULL;
D3D10PlusHookPresentAPICall D3D10Hook_SwapChain_ResizeBuffers=NULL;
D3D10PlusHookPresentAPICall D3D10_1Hook_SwapChain_Present=NULL;
D3D10PlusHookPresentAPICall D3D10_1Hook_SwapChain_ResizeBuffers=NULL;

D3D9HookPresentAPICall D3D9Hook_Present;
D3D9HookResetAPICall D3D9Hook_Reset;
D3D9HookDrawPrimitiveAPICall D3D9Hook_DrawPrimitive;
D3D9HookDrawIndexedPrimitiveAPICall D3D9Hook_DrawIndexedPrimitive;
D3D9HookDrawPrimitiveUPAPICall D3D9Hook_DrawPrimitiveUP;
D3D9HookDrawIndexedPrimitiveUPAPICall D3D9Hook_DrawIndexedPrimitiveUP;
D3D9HookDrawRectPatchAPICall D3D9Hook_DrawRectPatch;
D3D9HookDrawTriPatchAPICall D3D9Hook_DrawTriPatch;

D3D10HookDrawIndexedAPICall D3D10Hook_DrawIndexed;
D3D10HookDrawAPICall D3D10Hook_Draw;
D3D10HookDrawIndexedInstancedAPICall D3D10Hook_DrawIndexedInstanced;
D3D10HookDrawInstancedAPICall D3D10Hook_DrawInstanced;
D3D10HookDrawAutoAPICall D3D10Hook_DrawAuto;

D3D11HookDrawIndexedAPICall D3D11Hook_DrawIndexed;
D3D11HookDrawAPICall D3D11Hook_Draw;
D3D11HookDrawIndexedInstancedAPICall D3D11Hook_DrawIndexedInstanced;
D3D11HookDrawInstancedAPICall D3D11Hook_DrawInstanced;
D3D11HookDrawAutoAPICall D3D11Hook_DrawAuto;

D3D9_RESET_ORIGINAL D3D9_Reset_Original=NULL;
D3D9_PRESENT_ORIGINAL D3D9_Present_Original=NULL;
D3D9_DRAWPRIMITIVE_ORIGINAL D3D9_DrawPrimitive_Original=NULL;
D3D9_DRAWINDEXEDPRIMITIVE_ORIGINAL D3D9_DrawIndexedPrimitive_Original=NULL;
D3D9_DRAWPRIMITIVEUP_ORIGINAL D3D9_DrawPrimitiveUP_Original=NULL;
D3D9_DRAWINDEXEDPRIMITIVEUP_ORIGINAL D3D9_DrawIndexedPrimitiveUP_Original=NULL;
D3D9_DRAWRECTPATCH_ORIGINAL D3D9_DrawRectPatch_Original=NULL;
D3D9_DRAWTRIPATCH_ORIGINAL D3D9_DrawTriPatch_Original=NULL;


D3D10_DRAWINDEXED_ORIGINAL D3D10_DrawIndexed_Original=NULL;
D3D10_DRAW_ORIGINAL D3D10_Draw_Original=NULL;
D3D10_DRAWINDEXEDINSTANCED_ORIGINAL D3D10_DrawIndexedInstanced_Original=NULL;
D3D10_DRAWINSTANCED_ORIGINAL D3D10_DrawInstanced_Original=NULL;
D3D10_DRAWAUTO_ORIGINAL D3D10_DrawAuto_Original=NULL;

D3D11_DRAWINDEXED_ORIGINAL D3D11_DrawIndexed_Original=NULL;
D3D11_DRAW_ORIGINAL D3D11_Draw_Original=NULL;
D3D11_DRAWINDEXEDINSTANCED_ORIGINAL D3D11_DrawIndexedInstanced_Original=NULL;
D3D11_DRAWINSTANCED_ORIGINAL D3D11_DrawInstanced_Original=NULL;
D3D11_DRAWAUTO_ORIGINAL D3D11_DrawAuto_Original=NULL;





//this function is exported and only called by CE when inside CE
void GetAddresses(void)
{
	//create window and create a d3ddevice for dx9, dx10 and dx11H
	OutputDebugStringA("GetAddresses()");

	HRESULT hr=S_OK;
	HWND x=0;
    WNDCLASSEXW wcex;

	ZeroMemory(&wcex, sizeof(wcex));

	wcex.cbSize = sizeof( WNDCLASSEX );
	wcex.style = CS_HREDRAW | CS_VREDRAW;
	wcex.lpfnWndProc = DefWindowProc;//WndProc;
	wcex.cbClsExtra = 0;
	wcex.cbWndExtra = 0;
	
	wcex.hInstance = GetModuleHandle(0);
	wcex.hCursor = LoadCursor( NULL, IDC_ARROW );
	wcex.hbrBackground = ( HBRUSH )( COLOR_WINDOW + 1 );
	wcex.lpszMenuName = NULL;
	wcex.lpszClassName = L"BLA";
	if( !RegisterClassExW( &wcex ) )
		OutputDebugStringA("Failure\n");


	// Create window   
	RECT rc = { 0, 0, 640, 480 };
	AdjustWindowRect( &rc, WS_OVERLAPPEDWINDOW, FALSE );
	x = CreateWindowW( L"BLA", L"BLA",
						   WS_OVERLAPPEDWINDOW,
						   CW_USEDEFAULT, CW_USEDEFAULT, rc.right - rc.left, rc.bottom - rc.top, NULL, NULL, GetModuleHandle(0),
						   NULL );

	if (x)
	{

		DXGI_SWAP_CHAIN_DESC sd;
		
		ZeroMemory( &sd, sizeof( sd ) );
		sd.BufferCount = 1;
		sd.BufferDesc.Width = 640;
		sd.BufferDesc.Height = 480;
		sd.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
		sd.BufferDesc.RefreshRate.Numerator = 60;
		sd.BufferDesc.RefreshRate.Denominator = 1;
		sd.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
		sd.OutputWindow = x;
		sd.SampleDesc.Count = 1;
		sd.SampleDesc.Quality = 0;
		sd.Windowed = TRUE;

		IDXGISwapChain *pSwapChain;
		ID3D10Device *pd3dDevice;
		ID3D11Device *pd3dDevice11;
		ID3D11DeviceContext *pd3dDevice11Context;
		D3D_FEATURE_LEVEL fl;


		HMODULE d3d11dll=LoadLibraryA("D3D11.dll");

		if (d3d11dll)
		{
			d3d11create=(D3D11CREATEDEVICEANDSWAPCHAIN)GetProcAddress(d3d11dll, "D3D11CreateDeviceAndSwapChain");
			hr=d3d11create( NULL, D3D_DRIVER_TYPE_HARDWARE, NULL, 0, NULL, 0, D3D11_SDK_VERSION, &sd, &pSwapChain, &pd3dDevice11, &fl, &pd3dDevice11Context);

			if (SUCCEEDED(hr))
			{	
				//get the present function of the swapchain
				uintptr_t *a=(uintptr_t *)*(uintptr_t *)pSwapChain;

	
				a=(uintptr_t *)*(uintptr_t *)pd3dDevice11Context;
				shared->d3d11_drawindexed=a[12]; //DrawIndexed();  //v
				shared->d3d11_draw=a[13]; //Draw();  //v
				shared->d3d11_drawindexedinstanced=a[20];
				shared->d3d11_drawinstanced=a[21];
				shared->d3d11_drawauto=a[38];   //v
				

				//now cleanup
				pSwapChain->Release();
				pd3dDevice11Context->Release();
				pd3dDevice11->Release();
			}		
		}
		else
			OutputDebugStringA("D3D11.dll not loaded");


		HMODULE d3d10dll=LoadLibraryA("D3D10.dll");

		if (d3d10dll)
		{
			d3d10create=(D3D10CREATEDEVICEANDSWAPCHAIN)GetProcAddress(d3d10dll, "D3D10CreateDeviceAndSwapChain");
			hr=d3d10create( NULL, D3D10_DRIVER_TYPE_HARDWARE, NULL, 0, D3D10_SDK_VERSION, &sd, &pSwapChain, &pd3dDevice );

			if (SUCCEEDED(hr))
			{	
				//get the present function of the swapchain
				uintptr_t *a=(uintptr_t *)*(uintptr_t *)pSwapChain;

				shared->dxgi_present=a[8]; //8th element is Present()		
				shared->dxgi_resizebuffers=a[13]; 
			
							

				a=(uintptr_t *)*(uintptr_t *)pd3dDevice;
				shared->d3d10_drawindexed=a[8]; //DrawIndexed();  //v
				shared->d3d10_draw=a[9]; //Draw();  //v
				shared->d3d10_drawindexedinstanced=a[14];
				shared->d3d10_drawinstanced=a[15];
				shared->d3d10_drawauto=a[28];   //v
	


				//now cleanup
				pSwapChain->Release();	
				pd3dDevice->Release();
			}		
		}
		else
			OutputDebugStringA("D3D10 dll not loaded");

		//now the same for d3d9 to get the present function of d3d9device
		HMODULE d3d9dll=LoadLibraryA("D3D9.dll");
		

		if (d3d9dll)
		{
			D3DCreate9=(DIRECT3DCREATE9)GetProcAddress(d3d9dll, "Direct3DCreate9");		
			if (D3DCreate9)
			{
				IDirect3D9 *d3d9=D3DCreate9(D3D_SDK_VERSION);
				IDirect3DDevice9 *d3d9device;		

				if (d3d9)
				{
					D3DPRESENT_PARAMETERS d3dpp; 
					ZeroMemory( &d3dpp, sizeof(d3dpp) );
					d3dpp.Windowed   = TRUE;
					d3dpp.SwapEffect = D3DSWAPEFFECT_COPY;

					hr=d3d9->CreateDevice(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, x, D3DCREATE_SOFTWARE_VERTEXPROCESSING, &d3dpp, &d3d9device);
					if (SUCCEEDED(hr))
					{
						if (d3d9device)
						{
							//get present address
							uintptr_t *a=(uintptr_t *)*(uintptr_t *)d3d9device;

							shared->d3d9_present=a[17]; //17th element is Present()	
							shared->d3d9_reset=a[16]; //16th element is reset

							shared->d3d9_drawprimitive=a[81];
							shared->d3d9_drawindexedprimitive=a[82];
							shared->d3d9_drawprimitiveup=a[83];
							shared->d3d9_drawindexedprimitiveup=a[84];
							shared->d3d9_drawrectpatch=a[115];
							shared->d3d9_drawtripatch=a[116];												

							//d3d9device->Present(NULL,NULL,0,NULL);
							d3d9device->Release();

							OutputDebugStringA("Success: Fetched the Direct3D9 method addresses");
						}
						else
							OutputDebugStringA("d3d9device is NULL");

					}
					else
						OutputDebugStringA("FAILED d3d9->CreateDevice(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, x, D3DCREATE_SOFTWARE_VERTEXPROCESSING, &d3dpp, &d3d9device);");

					d3d9->Release();
				}
				else
					OutputDebugStringA("D3DCreate9 failed");
			}
			else
				OutputDebugStringA("Direct3DCreate9 not found");


		}
		else
		  OutputDebugStringA("D3D9 dll not loaded");

		DestroyWindow(x);	
	}

}

#include <map>
using namespace std;

typedef LRESULT (CALLBACK *WNDPROCHOOK)(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

map<HWND, LONG_PTR> originalwndprocs;




//windowhook
int overlaydown=-1;
RECT oldoverlayrect;

int waslockedin=0;
int appdoestranslate=0;

#ifdef _DEBUG
int called=0;
#endif


LRESULT CALLBACK windowhook(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	int i;

	
	POINTS p;
	RECT r;
	RECT cr;	
	LONG_PTR o=originalwndprocs[hwnd];
	BOOL hasTextureLock=FALSE;
	BOOL hasLock=FALSE;

#ifdef _DEBUG
	if (called==0)
	{
		OutputDebugStringA("windowhook got called");
		called=1;
	}
#endif




	switch(uMsg)	
	{

		case WM_CHAR:
			{
				appdoestranslate=1;
				break;
			}

	
		case WM_KEYDOWN:
			if ((shared->console.hasconsole) || (shared->hasOnKey))
			{
				if ((shared->console.hasconsole) && ((!shared->console.consolevisible) && (wParam==shared->console.consolekey)))
				{
				
					//tell ce to create a console background sprite, a cursor, a fontmap and a textcontainer

					//make the console entries visible

					shared->console.lastmessage.uMsg=0xffffffff; //special identifier to show the console stuff
					SetEvent(hasKeyboardEvent);
					WaitForSingleObject(handledKeyboardEvent, 2000); //wait for ce finish this (so the next frame rendered has the console)

					shared->console.consolevisible=1;
				
				}
				else
				{
					if (wParam==shared->console.consolekey)
					{
						/*
						shared->resources[shared->console.overlayid].valid=0;
						shared->resources[shared->console.cursorid].valid=0;
						shared->OverLayHasUpdate=1;						
						*/
						shared->console.lastmessage.uMsg=0xfffffffe; //special identifier to hide the console stuff

						SetEvent(hasKeyboardEvent);
						WaitForSingleObject(handledKeyboardEvent, 2000); 
						shared->console.consolevisible=0;
						
					}
					else
					if (hasKeyboardEvent)
					{
						BYTE keyboardstate[256];
						
						shared->console.lastmessage.uMsg=uMsg;
						shared->console.lastmessage.wParam=wParam;
						shared->console.lastmessage.lParam=lParam;

						GetKeyboardState(keyboardstate);
						shared->console.lastmessage.character=0;						
						ToAscii(wParam, (lParam >> 16) & 0xff, keyboardstate, (LPWORD)&shared->console.lastmessage.character,0);

						
						SetEvent(hasKeyboardEvent);
						WaitForSingleObject(handledKeyboardEvent, 10000);
						
						if (shared->console.lastmessage.uMsg==0)
							return DefWindowProcA(hwnd, uMsg, wParam, lParam); //no handling, else handle it by the original handler 
					}
					else OutputDebugStringA("Keyboard event handler events are not present");
				}				
			}
		
			
			break;
		
			

		case WM_LBUTTONDOWN:			
			
			if (shared->clipmouseinwindow)
			{				
				//lock the mouse inside the current client region
				POINT pt;


				GetClientRect(hwnd, &r);			

				//convert the client coordinates to screen coordinates
				pt.x=r.left;
				pt.y=r.top;
				ClientToScreen(hwnd, &pt);
				r.left=pt.x;
				r.top=pt.y;

				pt.x=r.right;
				pt.y=r.bottom;
				ClientToScreen(hwnd, &pt);
				r.right=pt.x;
				r.bottom=pt.y;


				ClipCursor(&r);
				waslockedin=1;
			}
			else
			{
				if (waslockedin)
				{//release the lock
					ClipCursor(NULL);
					waslockedin=0;
				}
			}

			p=MAKEPOINTS(lParam);


			//check if an sprite is pressed down
			overlaydown=-1;

			if (shared->UseCommandlistLock)
				hasLock=WaitForSingleObject((HANDLE)shared->CommandlistLock, INFINITE)==WAIT_OBJECT_0;

			//find the end of the list
			i=0;
			while (shared->RenderCommands[i].Command)
				i++;

			for (i=i-1; i>=0; i--)
			{
				int x,y,width, height;
				x=(int)shared->RenderCommands[i].x;
				y=(int)shared->RenderCommands[i].y;

				if ((x==-2) || (y==-2))
					continue; //mouse objects are not clickable

				if ((x==-1) || (y==-1)) //the client rect is required
					GetClientRect(hwnd, &cr);

				//find the width and height (and adjust x and y accordingly)
				switch (shared->RenderCommands[i].Command)
				{
					case rcDrawSprite:
					{
						width=shared->RenderCommands[i].sprite.width;
						height=shared->RenderCommands[i].sprite.height;

						if (x==-1)
							x=((cr.right-cr.left) / 2)-(width / 2);

						if (y==-1)
							y=((cr.bottom-cr.top) / 2)-(height / 2);

						
						break;
					}

					case rcDrawFont:
					{
						PTextureEntry tea=(PTextureEntry)((uintptr_t)shared+shared->texturelist);
						if (hasTextureLock==FALSE)					
							hasTextureLock=(WaitForSingleObject((HANDLE)(shared->TextureLock), INFINITE)==WAIT_OBJECT_0);						

						WORD *cefontmap=(WORD *)(tea[shared->RenderCommands[i].font.fontid].AddressOfFontmap);

						if (cefontmap)
						{

						
								
							height=cefontmap[0];				

							//if the clicked y position falls between y and y+height then count the full size of this string to determine the width, else skip it

							if (y==-1)					
								y=((cr.bottom-cr.top) / 2)-(height / 2);						

							if ((p.y>=y) && (p.y<y+height))
							{
								//clicked inside the range, find out the width
								unsigned int j;
								char *text=(char *)(shared->RenderCommands[i].font.addressoftext);
								width=0;
								for (j=0; j<strlen(text); j++)							
								{
									if ((text[j]>=32) && (text[j]<=127))
										width+=cefontmap[(text[j]-32)+1];
								}

								if (x==-1)
									x=((cr.right-cr.left) / 2)-(width / 2);
								
							}
							else						
								continue; //not inside the range
						}
						

						break;
					}
				}

				//x,y width and height are now filled in, check if it was clicked
				r.left=x;
				r.right=x+width;
				r.top=y;
				r.bottom=y+height;

				POINT p2;

				p2.x=p.x;
				p2.y=p.y;
			
				if (PtInRect(&r, p2))
				{
					overlaydown=i; //this object was clicked
					oldoverlayrect=r;
					break;
				}
			}

			if (hasTextureLock)
				SetEvent((HANDLE)(shared->TextureLock));

			if (hasLock)
				SetEvent((HANDLE)(HANDLE)shared->CommandlistLock);


			
			break;
		
		case WM_LBUTTONUP:
			
			//check if the same overlay is released
			//known bug: It doesn't handle zorder rearangement while the mouse is down

			p=MAKEPOINTS(lParam);
			POINT p2;

			p2.x=p.x;
			p2.y=p.y;

			if (overlaydown != -1)
			{
				//check if it is still in the same rect
				if (PtInRect(&oldoverlayrect, p2))
				{
					//still focused				
					if (WaitForSingleObject(handledClickEvent, 5000)==WAIT_OBJECT_0) //wait for a previous click to get handled
					{
						shared->clickedoverlay=overlaydown;

						shared->clickedx=p.x-oldoverlayrect.left;
						shared->clickedy=p.y-oldoverlayrect.top;
						
						SetEvent(hasClickEvent);
					}
					

					break;
				}
			}
			
			break;

		//case WM_MOUSEMOVE:
			//break;
		
	}

	

	
	return CallWindowProcW((WNDPROC)o, hwnd, uMsg, wParam, lParam); //call the original window message handler
}



void hookIfNeeded(void)
{
	HWND h=(HWND)shared->lastHwnd;
	if (originalwndprocs[h]==NULL)
	{
		LONG_PTR o;
		o=GetWindowLongPtrW(h, GWLP_WNDPROC);


		originalwndprocs[h]=o;

		//now change it to point to the hook handler
		SetWindowLongPtrW(h, GWLP_WNDPROC, (LONG_PTR)windowhook);  

	}


}


void InitializeD3D9Api()
{
	char dllpath[MAX_PATH];				
	strcpy_s(dllpath, MAX_PATH, (char *)shared->CheatEngineDir);
#ifdef _AMD64_
	strcat_s(dllpath, MAX_PATH, "CED3D9Hook64.dll");
#else
	strcat_s(dllpath, MAX_PATH, "CED3D9Hook.dll");
#endif

	OutputDebugStringA("InitializeD3D9Api()");
	OutputDebugStringA("dllpath = :");
	OutputDebugStringA(dllpath);


	HMODULE hdll=LoadLibraryA((char *)dllpath);
	if (hdll)
	{
		D3D9Hook_Reset=(D3D9HookResetAPICall)GetProcAddress(hdll, "D3D9Hook_Reset_imp");
		D3D9Hook_Present=(D3D9HookPresentAPICall)GetProcAddress(hdll, "D3D9Hook_Present_imp");

		D3D9Hook_DrawPrimitive=(D3D9HookDrawPrimitiveAPICall)GetProcAddress(hdll, "D3D9Hook_DrawPrimitive_imp");
		D3D9Hook_DrawIndexedPrimitive=(D3D9HookDrawIndexedPrimitiveAPICall)GetProcAddress(hdll, "D3D9Hook_DrawIndexedPrimitive_imp");
		D3D9Hook_DrawPrimitiveUP=(D3D9HookDrawPrimitiveUPAPICall)GetProcAddress(hdll, "D3D9Hook_DrawPrimitiveUP_imp");
		D3D9Hook_DrawIndexedPrimitiveUP=(D3D9HookDrawIndexedPrimitiveUPAPICall)GetProcAddress(hdll, "D3D9Hook_DrawIndexedPrimitiveUP_imp");
		D3D9Hook_DrawRectPatch=(D3D9HookDrawRectPatchAPICall)GetProcAddress(hdll, "D3D9Hook_DrawRectPatch_imp");
		D3D9Hook_DrawTriPatch=(D3D9HookDrawTriPatchAPICall)GetProcAddress(hdll, "D3D9Hook_DrawTriPatch_imp");

		if (D3D9Hook_Reset && D3D9Hook_Present && D3D9Hook_DrawPrimitive && D3D9Hook_DrawIndexedPrimitive && D3D9Hook_DrawPrimitiveUP && D3D9Hook_DrawIndexedPrimitiveUP && D3D9Hook_DrawRectPatch && D3D9Hook_DrawTriPatch)
			OutputDebugStringA("D3D9Hook: SUCCESSFULLY LOADED");		
		else
			OutputDebugStringA("D3D9Hook: Not all exports found");

	}
	else
	{
		int err=GetLastError();		
		char msg[255];
		sprintf_s(msg, 255, "LoadLibraryA for the D3D9 hook failed: %d", err);
		OutputDebugStringA(msg);
	}
}

HRESULT	__stdcall D3D9_DrawPrimitive_new(IDirect3DDevice9 *Device, D3DPRIMITIVETYPE PrimitiveType,UINT StartVertex,UINT PrimitiveCount)
{
	if (shared)
	{
			if (D3D9Hook_DrawPrimitive==NULL)		
				InitializeD3D9Api();

			if (D3D9Hook_DrawPrimitive)
				return D3D9Hook_DrawPrimitive(D3D9_DrawPrimitive_Original, Device, PrimitiveType, StartVertex, PrimitiveCount);
	}
	return D3D9_DrawPrimitive_Original(Device, PrimitiveType, StartVertex, PrimitiveCount);
}

HRESULT	__stdcall D3D9_DrawIndexedPrimitive_new(IDirect3DDevice9 *Device, D3DPRIMITIVETYPE PrimitiveType,INT BaseVertexIndex,UINT MinVertexIndex,UINT NumVertices,UINT startIndex,UINT primCount)
{
	if (shared)
	{
			if (D3D9Hook_DrawIndexedPrimitive==NULL)		
				InitializeD3D9Api();

			if (D3D9Hook_DrawIndexedPrimitive)
				return D3D9Hook_DrawIndexedPrimitive(D3D9_DrawIndexedPrimitive_Original, Device, PrimitiveType, BaseVertexIndex, MinVertexIndex, NumVertices, startIndex, primCount);
	}
	return D3D9_DrawIndexedPrimitive_Original(Device, PrimitiveType, BaseVertexIndex, MinVertexIndex, NumVertices, startIndex, primCount);
}

HRESULT	__stdcall D3D9_DrawPrimitiveUP_new(IDirect3DDevice9 *Device, D3DPRIMITIVETYPE PrimitiveType,UINT PrimitiveCount,CONST void* pVertexStreamZeroData,UINT VertexStreamZeroStride)
{
	if (shared)
	{
			if (D3D9Hook_DrawPrimitiveUP==NULL)		
				InitializeD3D9Api();

			if (D3D9Hook_DrawPrimitiveUP)
				return D3D9Hook_DrawPrimitiveUP(D3D9_DrawPrimitiveUP_Original, Device, PrimitiveType, PrimitiveCount, pVertexStreamZeroData, VertexStreamZeroStride);
	}
	return D3D9_DrawPrimitiveUP_Original(Device, PrimitiveType, PrimitiveCount, pVertexStreamZeroData, VertexStreamZeroStride);
}

HRESULT	__stdcall D3D9_DrawIndexedPrimitiveUP_new(IDirect3DDevice9 *Device, D3DPRIMITIVETYPE PrimitiveType,UINT MinVertexIndex,UINT NumVertices,UINT PrimitiveCount,CONST void* pIndexData,D3DFORMAT IndexDataFormat,CONST void* pVertexStreamZeroData,UINT VertexStreamZeroStride)
{
	if (shared)
	{
			if (D3D9Hook_DrawIndexedPrimitiveUP==NULL)		
				InitializeD3D9Api();

			if (D3D9Hook_DrawIndexedPrimitiveUP)
				return D3D9Hook_DrawIndexedPrimitiveUP(D3D9_DrawIndexedPrimitiveUP_Original, Device, PrimitiveType, MinVertexIndex, NumVertices, PrimitiveCount, pIndexData, IndexDataFormat, pVertexStreamZeroData, VertexStreamZeroStride);
	}
	return D3D9_DrawIndexedPrimitiveUP_Original(Device, PrimitiveType, MinVertexIndex, NumVertices, PrimitiveCount, pIndexData, IndexDataFormat, pVertexStreamZeroData, VertexStreamZeroStride);
}


HRESULT	__stdcall D3D9_DrawRectPatch_new(IDirect3DDevice9 *device, UINT Handle,CONST float* pNumSegs,CONST D3DRECTPATCH_INFO* pRectPatchInfo)
{
	if (shared)
	{
			if (D3D9Hook_DrawRectPatch==NULL)		
				InitializeD3D9Api();

			if (D3D9Hook_DrawRectPatch)
				return D3D9Hook_DrawRectPatch(D3D9_DrawRectPatch_Original, device, Handle, pNumSegs, pRectPatchInfo);
	}
	return D3D9_DrawRectPatch_Original(device, Handle, pNumSegs, pRectPatchInfo);
}

HRESULT	__stdcall D3D9_DrawTriPatch_new(IDirect3DDevice9 *device, UINT Handle,CONST float* pNumSegs,CONST D3DTRIPATCH_INFO* pTriPatchInfo)
{
	if (shared)
	{
			if (D3D9Hook_DrawTriPatch==NULL)		
				InitializeD3D9Api();

			if (D3D9Hook_DrawTriPatch)
				return D3D9Hook_DrawTriPatch(D3D9_DrawTriPatch_Original, device, Handle, pNumSegs, pTriPatchInfo);
	}
	return D3D9_DrawTriPatch_Original(device, Handle, pNumSegs, pTriPatchInfo);
}


HRESULT __stdcall D3D9_Reset_new(IDirect3DDevice9 *Device, D3DPRESENT_PARAMETERS *pPresentationParameters)
{
	if (shared)
	{
			if (D3D9Hook_Reset==NULL)		
				InitializeD3D9Api();

			if (D3D9Hook_Reset)
				return D3D9Hook_Reset(D3D9_Reset_Original, Device, pPresentationParameters);
	}
	return D3D9_Reset_Original(Device, pPresentationParameters);
}

HRESULT __stdcall D3D9_Present_new(IDirect3DDevice9 *Device, RECT* pSourceRect,CONST RECT* pDestRect,HWND hDestWindowOverride,CONST RGNDATA* pDirtyRegion)
{
	if (shared)
	{
		if (D3D9Hook_Present==NULL)
			InitializeD3D9Api();

		if (D3D9Hook_Present)
			D3D9Hook_Present(Device, shared);

		if ((shared->hookwnd) && (shared->lastHwnd))
			hookIfNeeded();
	}
	return D3D9_Present_Original(Device, pSourceRect, pDestRect, hDestWindowOverride, pDirtyRegion);
}



void InitializeD3D10Api()
{
	char dllpath[MAX_PATH];				
	strcpy_s(dllpath, MAX_PATH, (char *)shared->CheatEngineDir);
#ifdef _AMD64_
	strcat_s(dllpath, MAX_PATH, "CED3D10Hook64.dll");
#else
	strcat_s(dllpath, MAX_PATH, "CED3D10Hook.dll");
#endif

	HMODULE hdll=LoadLibraryA((char *)dllpath);


	D3D10Hook_DrawIndexed=(D3D10HookDrawIndexedAPICall)GetProcAddress(hdll, "D3D10Hook_DrawIndexed_imp");
	D3D10Hook_Draw=(D3D10HookDrawAPICall)GetProcAddress(hdll, "D3D10Hook_Draw_imp");
	D3D10Hook_DrawIndexedInstanced=(D3D10HookDrawIndexedInstancedAPICall)GetProcAddress(hdll, "D3D10Hook_DrawIndexedInstanced_imp");
	D3D10Hook_DrawInstanced=(D3D10HookDrawInstancedAPICall)GetProcAddress(hdll, "D3D10Hook_DrawInstanced_imp");
	D3D10Hook_DrawAuto=(D3D10HookDrawAutoAPICall)GetProcAddress(hdll, "D3D10Hook_DrawAuto_imp");

}

HRESULT	__stdcall D3D10_DrawIndexed_new(ID3D10Device *device, UINT IndexCount, UINT StartIndexLocation, INT BaseVertexLocation)
{
	if (shared)
	{
			if (D3D10Hook_DrawIndexed==NULL)		
				InitializeD3D10Api();

			if (D3D10Hook_DrawIndexed)
				return D3D10Hook_DrawIndexed(D3D10_DrawIndexed_Original, device, IndexCount, StartIndexLocation, BaseVertexLocation);
	}
	return D3D10_DrawIndexed_Original(device, IndexCount, StartIndexLocation, BaseVertexLocation);
}

HRESULT	__stdcall D3D10_Draw_new(ID3D10Device *device, UINT VertexCount, UINT StartVertexLocation)
{
	if (shared)
	{
			if (D3D10Hook_Draw==NULL)		
				InitializeD3D10Api();

			if (D3D10Hook_Draw)
				return D3D10Hook_Draw(D3D10_Draw_Original, device, VertexCount, StartVertexLocation);			
	}
	return D3D10_Draw_Original(device, VertexCount, StartVertexLocation);
}

HRESULT	__stdcall D3D10_DrawIndexedInstanced_new(ID3D10Device *device, UINT IndexCountPerInstance, UINT InstanceCount, UINT StartIndexLocation, INT BaseVertexLocation, UINT StartInstanceLocation)
{
	if (shared)
	{
			if (D3D10Hook_DrawIndexedInstanced==NULL)		
				InitializeD3D10Api();

			if (D3D10Hook_DrawIndexedInstanced)
				return D3D10Hook_DrawIndexedInstanced(D3D10_DrawIndexedInstanced_Original, device, IndexCountPerInstance, InstanceCount, StartIndexLocation, BaseVertexLocation, StartInstanceLocation);			
	}
	return D3D10_DrawIndexedInstanced_Original(device, IndexCountPerInstance, InstanceCount, StartIndexLocation, BaseVertexLocation, StartInstanceLocation);			
}

HRESULT	__stdcall D3D10_DrawInstanced_new(ID3D10Device *device, UINT VertexCountPerInstance, UINT InstanceCount, UINT StartVertexLocation, UINT StartInstanceLocation)
{
	if (shared)
	{
			if (D3D10Hook_DrawInstanced==NULL)		
				InitializeD3D10Api();

			if (D3D10Hook_DrawInstanced)
				return D3D10Hook_DrawInstanced(D3D10_DrawInstanced_Original, device, VertexCountPerInstance, InstanceCount, StartVertexLocation, StartInstanceLocation);
	}
	return D3D10_DrawInstanced_Original(device, VertexCountPerInstance, InstanceCount, StartVertexLocation, StartInstanceLocation);
}

HRESULT	__stdcall D3D10_DrawAuto_new(ID3D10Device *device)
{
	if (shared)
	{
			if (D3D10Hook_DrawAuto==NULL)		
				InitializeD3D10Api();

			if (D3D10Hook_DrawAuto)
				return D3D10Hook_DrawAuto(D3D10_DrawAuto_Original, device);
	}
	return D3D10_DrawAuto_Original(device);
}

void InitializeD3D11Api()
{
	char dllpath[MAX_PATH];				
	strcpy_s(dllpath, MAX_PATH, (char *)shared->CheatEngineDir);
#ifdef _AMD64_
	strcat_s(dllpath, MAX_PATH, "CED3D11Hook64.dll");
#else
	strcat_s(dllpath, MAX_PATH, "CED3D11Hook.dll");
#endif

	HMODULE hdll=LoadLibraryA((char *)dllpath);


	D3D11Hook_DrawIndexed=(D3D11HookDrawIndexedAPICall)GetProcAddress(hdll, "D3D11Hook_DrawIndexed_imp");
	D3D11Hook_Draw=(D3D11HookDrawAPICall)GetProcAddress(hdll, "D3D11Hook_Draw_imp");
	D3D11Hook_DrawIndexedInstanced=(D3D11HookDrawIndexedInstancedAPICall)GetProcAddress(hdll, "D3D11Hook_DrawIndexedInstanced_imp");
	D3D11Hook_DrawInstanced=(D3D11HookDrawInstancedAPICall)GetProcAddress(hdll, "D3D11Hook_DrawInstanced_imp");
	D3D11Hook_DrawAuto=(D3D11HookDrawAutoAPICall)GetProcAddress(hdll, "D3D11Hook_DrawAuto_imp");

}

HRESULT	__stdcall D3D11_DrawIndexed_new(ID3D11DeviceContext *dc, UINT IndexCount, UINT StartIndexLocation, INT BaseVertexLocation)
{
	if (shared)
	{
			if (D3D11Hook_DrawIndexed==NULL)		
				InitializeD3D11Api();

			if (D3D11Hook_DrawIndexed)
				return D3D11Hook_DrawIndexed(D3D11_DrawIndexed_Original, dc, IndexCount, StartIndexLocation, BaseVertexLocation);
	}
	return D3D11_DrawIndexed_Original(dc, IndexCount, StartIndexLocation, BaseVertexLocation);
}

HRESULT	__stdcall D3D11_Draw_new(ID3D11DeviceContext *dc, UINT VertexCount, UINT StartVertexLocation)
{
	if (shared)
	{
			if (D3D11Hook_Draw==NULL)		
				InitializeD3D11Api();

			if (D3D11Hook_Draw)
				return D3D11Hook_Draw(D3D11_Draw_Original, dc, VertexCount, StartVertexLocation);			
	}
	return D3D11_Draw_Original(dc, VertexCount, StartVertexLocation);
}

HRESULT	__stdcall D3D11_DrawIndexedInstanced_new(ID3D11DeviceContext *dc, UINT IndexCountPerInstance, UINT InstanceCount, UINT StartIndexLocation, INT BaseVertexLocation, UINT StartInstanceLocation)
{
	if (shared)
	{
			if (D3D11Hook_DrawIndexedInstanced==NULL)		
				InitializeD3D11Api();

			if (D3D11Hook_DrawIndexedInstanced)
				return D3D11Hook_DrawIndexedInstanced(D3D11_DrawIndexedInstanced_Original, dc, IndexCountPerInstance, InstanceCount, StartIndexLocation, BaseVertexLocation, StartInstanceLocation);			
	}
	return D3D11_DrawIndexedInstanced_Original(dc, IndexCountPerInstance, InstanceCount, StartIndexLocation, BaseVertexLocation, StartInstanceLocation);			
}

HRESULT	__stdcall D3D11_DrawInstanced_new(ID3D11DeviceContext *dc, UINT VertexCountPerInstance, UINT InstanceCount, UINT StartVertexLocation, UINT StartInstanceLocation)
{
	if (shared)
	{
			if (D3D11Hook_DrawInstanced==NULL)		
				InitializeD3D11Api();

			if (D3D11Hook_DrawInstanced)
				return D3D11Hook_DrawInstanced(D3D11_DrawInstanced_Original, dc, VertexCountPerInstance, InstanceCount, StartVertexLocation, StartInstanceLocation);
	}
	return D3D11_DrawInstanced_Original(dc, VertexCountPerInstance, InstanceCount, StartVertexLocation, StartInstanceLocation);
}

HRESULT	__stdcall D3D11_DrawAuto_new(ID3D11DeviceContext *dc)
{
	if (shared)
	{
			if (D3D11Hook_DrawAuto==NULL)		
				InitializeD3D11Api();

			if (D3D11Hook_DrawAuto)
				return D3D11Hook_DrawAuto(D3D11_DrawAuto_Original, dc);
	}
	return D3D11_DrawAuto_Original(dc);
}

int GetDXVersionAndInitDLL(IDXGISwapChain *x, void *device)
{

	ID3D10Device *d10=NULL;
	ID3D10Device1 *d101=NULL;
	ID3D11Device *d11=NULL;

	DWORD fl;


	int version=0;


	//find what kind of device this is	
	if (SUCCEEDED(x->GetDevice(__uuidof(ID3D11Device), (void**)&d11)))
	{
		//D3D11
		fl=(DWORD)d11->GetFeatureLevel();	
		version=11;
		
		d11->Release();
	}
	else
	if (SUCCEEDED(x->GetDevice(__uuidof(ID3D10Device1), (void**)&d101)))
	{
		fl=(DWORD)d101->GetFeatureLevel();
		version=101;
		d101->Release();		
	}
	else
	if (SUCCEEDED(x->GetDevice(__uuidof(ID3D10Device), (void**)&d10)))
	{		
		version=10;		
		fl=D3D_FEATURE_LEVEL_10_0;
		d10->Release();
	}

	switch (fl)
	{
		case D3D_FEATURE_LEVEL_10_0:
			x->GetDevice(__uuidof(ID3D10Device), (void **)device);
			version=10;
			break;

		case D3D_FEATURE_LEVEL_10_1:
			x->GetDevice(__uuidof(ID3D10Device1), (void **)device);
			version=101;
			break;

		case D3D_FEATURE_LEVEL_11_0:
		case D3D_FEATURE_LEVEL_11_1:
			x->GetDevice(__uuidof(ID3D11Device), (void **)device);
			version=11;			
			break;		
	}


	switch (version)
	{
		case 11:
			//make sure the D3DHook11.dll is loaded and pass on this device ,swapchain and shared buffer
			if ((D3D11Hook_SwapChain_Present==NULL) || (D3D11Hook_SwapChain_ResizeBuffers==NULL))
			{
				//load the dll and use getprocaddress
				char dllpath[MAX_PATH];
				strcpy_s(dllpath, MAX_PATH, (char *)shared->CheatEngineDir);
				strcat_s(dllpath, MAX_PATH, "CED3D11Hook.dll");

				HMODULE hdll=LoadLibraryA(dllpath);
				D3D11Hook_SwapChain_Present=(D3D10PlusHookPresentAPICall)GetProcAddress(hdll, "D3D11Hook_SwapChain_Present_imp");
				D3D11Hook_SwapChain_ResizeBuffers=(D3D10PlusHookPresentAPICall)GetProcAddress(hdll, "D3D11Hook_SwapChain_ResizeBuffers_imp");
			}		

			break;

		case 101:
			//make sure the D3DHook10.1.dll is loaded and pass on this device ,swapchain and shared buffer
			if ((D3D10_1Hook_SwapChain_Present==NULL) || (D3D10_1Hook_SwapChain_ResizeBuffers==NULL))
			{
				char dllpath[MAX_PATH];
				strcpy_s(dllpath, MAX_PATH, (char *)shared->CheatEngineDir);
				strcat_s(dllpath, MAX_PATH, "CED3D10Hook.dll");

				HMODULE hdll=LoadLibraryA(dllpath);
				D3D10_1Hook_SwapChain_Present=(D3D10PlusHookPresentAPICall)GetProcAddress(hdll, "D3D10Hook_SwapChain_Present_imp");
				D3D10_1Hook_SwapChain_ResizeBuffers=(D3D10PlusHookPresentAPICall)GetProcAddress(hdll, "D3D10Hook_SwapChain_ResizeBuffers_imp");
			}

			break;

		case 10:
			if ((D3D10Hook_SwapChain_Present==NULL) || (D3D10Hook_SwapChain_ResizeBuffers==NULL))
			{
				char dllpath[MAX_PATH];
				strcpy_s(dllpath, MAX_PATH, (char *)shared->CheatEngineDir);
				strcat_s(dllpath, MAX_PATH, "CED3D10Hook.dll");

				HMODULE hdll=LoadLibraryA(dllpath);
				D3D10Hook_SwapChain_Present=(D3D10PlusHookPresentAPICall)GetProcAddress(hdll, "D3D10Hook_SwapChain_Present_imp");
				D3D10Hook_SwapChain_ResizeBuffers=(D3D10PlusHookPresentAPICall)GetProcAddress(hdll, "D3D10Hook_SwapChain_ResizeBuffers_imp");

			
			}
			break;

		
	}

	return version;
}


HRESULT __stdcall IDXGISwapChain_ResizeBuffers_new(IDXGISwapChain *x, UINT BufferCount, UINT Width, UINT Height, DXGI_FORMAT NewFormat, UINT SwapChainFlags)
{

	if (shared)
	{
		IUnknown *dev;
		int version=GetDXVersionAndInitDLL(x, &dev);

		switch (version)
		{			
			case 10:
				if (D3D10Hook_SwapChain_ResizeBuffers)
					D3D10Hook_SwapChain_ResizeBuffers(x, dev, shared);
				break;

			case 101:
				if (D3D10_1Hook_SwapChain_ResizeBuffers)
					D3D10_1Hook_SwapChain_ResizeBuffers(x, dev, shared);
				break;

			case 11:
				if (D3D11Hook_SwapChain_ResizeBuffers)
					D3D11Hook_SwapChain_ResizeBuffers(x, dev, shared);
				break;
		}

		if (version)
			dev->Release();
	}



	return DXGI_ResizeBuffers_Original(x, BufferCount, Width, Height, NewFormat, SwapChainFlags);
}

HRESULT __stdcall IDXGISwapChain_Present_new(IDXGISwapChain *x, UINT SyncInterval, UINT Flags)
{

	if (shared)	
	{
		IUnknown *dev;

		int version=GetDXVersionAndInitDLL(x, &dev);
		switch (version)
		{
			case 10:
				if (D3D10Hook_SwapChain_Present)
					D3D10Hook_SwapChain_Present(x, dev, shared);
				break;

			case 101:
				if (D3D10_1Hook_SwapChain_Present)
					D3D10_1Hook_SwapChain_Present(x, dev, shared);
				break;

			case 11:
				if (D3D11Hook_SwapChain_Present)
					D3D11Hook_SwapChain_Present(x, dev, shared);
				break;
		}

		if (version)
			dev->Release();

		if ((shared->hookwnd) && (shared->lastHwnd))
			hookIfNeeded();


	}
	

	//call original present
	return DXGI_Present_Original(x, SyncInterval, Flags);
}



DWORD WINAPI InitializeD3DHookDll(PVOID params)
{
	//called when the dll is injected
	//open the map
	uintptr_t present=0,d3d9present=0, d3d9reset=0;
	HANDLE fmhandle;

	char sharename[50];
	char eventname[50];
	char hasclickeventname[50];
	char handledclickeventname[50];

	char haskeyboardeventname[50]; 
	char handledkeyboardeventname[50];

	

//#ifdef DEBUG	
//	sprintf_s(sharename, 100,"CED3D_DEBUG2");	
	//sprintf_s(eventname, 100,"CED3D_DEBUG2_READY");	
//#else
	sprintf_s(sharename, 50,"CED3D_%d", GetCurrentProcessId());
	sprintf_s(eventname, 50,"%s_READY", sharename);	

	sprintf_s(hasclickeventname, 50,"%s_HASCLICK", sharename);
	sprintf_s(handledclickeventname, 50,"%s_HANDLEDCLICK", sharename);

	sprintf_s(haskeyboardeventname, 50,"%s_HASKEYBOARD", sharename);
	sprintf_s(handledkeyboardeventname, 50,"%s_HANDLEDKEYBOARD", sharename);
//#endif
 
	fmhandle=OpenFileMappingA(FILE_MAP_EXECUTE | FILE_MAP_READ | FILE_MAP_WRITE, FALSE, sharename);
  
    shared=(PD3DHookShared)MapViewOfFile(fmhandle,FILE_MAP_EXECUTE | FILE_MAP_READ | FILE_MAP_WRITE, 0,0,0 );   


	if (shared)
	{

		
		//tell ce the address to hook
		GetAddresses();

		
	

		//tell ce the address where the hook should point to
		shared->dxgi_newpresent=(uintptr_t)IDXGISwapChain_Present_new;
		shared->dxgi_newresizebuffers=(uintptr_t)IDXGISwapChain_ResizeBuffers_new;
		shared->d3d9_newpresent=(uintptr_t)D3D9_Present_new;
		shared->d3d9_newreset=(uintptr_t)D3D9_Reset_new;

		
		shared->d3d9_newdrawprimitive=(uintptr_t)D3D9_DrawPrimitive_new;
		shared->d3d9_newdrawindexedprimitive=(uintptr_t)D3D9_DrawIndexedPrimitive_new;
		shared->d3d9_newdrawprimitiveup=(uintptr_t)D3D9_DrawPrimitiveUP_new;
		shared->d3d9_newdrawindexedprimitiveup=(uintptr_t)D3D9_DrawIndexedPrimitiveUP_new;
		shared->d3d9_newdrawrectpatch=(uintptr_t)D3D9_DrawRectPatch_new;
		shared->d3d9_newdrawtripatch=(uintptr_t)D3D9_DrawTriPatch_new;	

		
		shared->d3d10_newdrawindexed=(uintptr_t)D3D10_DrawIndexed_new;
		shared->d3d10_newdraw=(uintptr_t)D3D10_Draw_new;
		shared->d3d10_newdrawindexedinstanced=(uintptr_t)D3D10_DrawIndexedInstanced_new;
		shared->d3d10_newdrawinstanced=(uintptr_t)D3D10_DrawInstanced_new;
		shared->d3d10_newdrawauto=(uintptr_t)D3D10_DrawAuto_new;
		
		shared->d3d11_newdrawindexed=(uintptr_t)D3D11_DrawIndexed_new;
		shared->d3d11_newdraw=(uintptr_t)D3D11_Draw_new;
		shared->d3d11_newdrawindexedinstanced=(uintptr_t)D3D11_DrawIndexedInstanced_new;
		shared->d3d11_newdrawinstanced=(uintptr_t)D3D11_DrawInstanced_new;
		shared->d3d11_newdrawauto=(uintptr_t)D3D11_DrawAuto_new;
		


		//tell ce where it should write a pointer to the unhooked version of the hooked functions
		shared->dxgi_originalpresent=(uintptr_t)&DXGI_Present_Original;
		shared->dxgi_originalresizebuffers=(uintptr_t)&DXGI_ResizeBuffers_Original;
		shared->d3d9_originalpresent=(uintptr_t)&D3D9_Present_Original;
		shared->d3d9_originalreset=(uintptr_t)&D3D9_Reset_Original;

		shared->d3d9_originaldrawprimitive=(uintptr_t)&D3D9_DrawPrimitive_Original;
		shared->d3d9_originaldrawindexedprimitive=(uintptr_t)&D3D9_DrawIndexedPrimitive_Original;
		shared->d3d9_originaldrawprimitiveup=(uintptr_t)&D3D9_DrawPrimitiveUP_Original;
		shared->d3d9_originaldrawindexedprimitiveup=(uintptr_t)&D3D9_DrawIndexedPrimitiveUP_Original;
		shared->d3d9_originaldrawrectpatch=(uintptr_t)&D3D9_DrawRectPatch_Original;
		shared->d3d9_originaldrawtripatch=(uintptr_t)&D3D9_DrawTriPatch_Original;

		shared->d3d10_originaldrawindexed=(uintptr_t)&D3D10_DrawIndexed_Original;
		shared->d3d10_originaldraw=(uintptr_t)&D3D10_Draw_Original;
		shared->d3d10_originaldrawindexedinstanced=(uintptr_t)&D3D10_DrawIndexedInstanced_Original;
		shared->d3d10_originaldrawinstanced=(uintptr_t)&D3D10_DrawInstanced_Original;
		shared->d3d10_originaldrawauto=(uintptr_t)&D3D10_DrawAuto_Original;

		shared->d3d11_originaldrawindexed=(uintptr_t)&D3D11_DrawIndexed_Original;
		shared->d3d11_originaldraw=(uintptr_t)&D3D11_Draw_Original;
		shared->d3d11_originaldrawindexedinstanced=(uintptr_t)&D3D11_DrawIndexedInstanced_Original;
		shared->d3d11_originaldrawinstanced=(uintptr_t)&D3D11_DrawInstanced_Original;
		shared->d3d11_originaldrawauto=(uintptr_t)&D3D11_DrawAuto_Original;


	}

	HANDLE eventhandle=OpenEventA(EVENT_MODIFY_STATE, FALSE, eventname);
	if (eventhandle)
	{
		SetEvent(eventhandle);
		CloseHandle(eventhandle);
	}

	if (shared->hookwnd)
	{
		hasClickEvent=OpenEventA(EVENT_MODIFY_STATE | SYNCHRONIZE , FALSE, hasclickeventname);
		handledClickEvent=OpenEventA(EVENT_MODIFY_STATE| SYNCHRONIZE, FALSE, handledclickeventname);

		hasKeyboardEvent=OpenEventA(EVENT_MODIFY_STATE | SYNCHRONIZE , FALSE, haskeyboardeventname);
		handledKeyboardEvent=OpenEventA(EVENT_MODIFY_STATE| SYNCHRONIZE, FALSE, handledkeyboardeventname);
	}





#ifdef DEBUG
	D3D9_Present_Original=(D3D9_PRESENT_ORIGINAL)d3d9present;
	DXGI_Present_Original=(DXGI_PRESENT_ORIGINAL)present;	
#endif
	return 0;
}


