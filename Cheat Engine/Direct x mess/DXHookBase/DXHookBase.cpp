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

typedef HRESULT     (__stdcall *D3D9_RESET_ORIGINAL)(IDirect3DDevice9 *Device, D3DPRESENT_PARAMETERS *pPresentationParameters);
typedef HRESULT     (__stdcall *D3D9_PRESENT_ORIGINAL)(IDirect3DDevice9 *Device, RECT* pSourceRect,CONST RECT* pDestRect,HWND hDestWindowOverride,CONST RGNDATA* pDirtyRegion);
typedef HRESULT     (__stdcall *DXGI_PRESENT_ORIGINAL)(IDXGISwapChain *x, UINT SyncInterval, UINT Flags);
typedef void        (__stdcall *D3D10PlusHookPresentAPICall)(IDXGISwapChain *swapchain, void *device, PD3DHookShared shared);
typedef void        (__stdcall *D3D9HookPresentAPICall)(IDirect3DDevice9 *device, PD3DHookShared shared);
typedef HRESULT     (__stdcall *D3D9HookResetAPICall)(D3D9_RESET_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRESENT_PARAMETERS *pPresentationParameters, PD3DHookShared shared);

typedef IDirect3D9* (__stdcall *DIRECT3DCREATE9)(UINT SDKVersion); 
typedef HRESULT     (__stdcall *D3D10CREATEDEVICEANDSWAPCHAIN)(IDXGIAdapter *pAdapter, D3D10_DRIVER_TYPE DriverType, HMODULE Software, UINT Flags, UINT SDKVersion, DXGI_SWAP_CHAIN_DESC *pSwapChainDesc, IDXGISwapChain **ppSwapChain, ID3D10Device **ppDevice);




D3D10CREATEDEVICEANDSWAPCHAIN d3d10create=NULL;
DIRECT3DCREATE9 D3DCreate9=NULL;
D3D9_RESET_ORIGINAL D3D9_Reset_Original=NULL;
D3D9_PRESENT_ORIGINAL D3D9_Present_Original=NULL;
DXGI_PRESENT_ORIGINAL DXGI_Present_Original=NULL;
D3D10PlusHookPresentAPICall D3D11Hook_SwapChain_Present=NULL;
D3D10PlusHookPresentAPICall D3D10Hook_SwapChain_Present=NULL;
D3D10PlusHookPresentAPICall D3D10_1Hook_SwapChain_Present=NULL;
D3D9HookPresentAPICall D3D9Hook_Present;
D3D9HookResetAPICall D3D9Hook_Reset;


//this function is exported and only called by CE when inside CE
void GetAddresses(uintptr_t *presentaddress, uintptr_t *d3d9presentaddress, uintptr_t *d3d9resetaddress)
{
	//create window and create a d3ddevice for dx9, dx10 and dx11H

	HRESULT hr=S_OK;
	HWND x=0;
    WNDCLASSEXW wcex;

	*presentaddress=0;
	*d3d9presentaddress=0;




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
	
		HMODULE d3d10dll=LoadLibraryA("D3D10.dll");

		if (d3d10dll)
		{
			d3d10create=(D3D10CREATEDEVICEANDSWAPCHAIN)GetProcAddress(d3d10dll, "D3D10CreateDeviceAndSwapChain");
			hr=d3d10create( NULL, D3D10_DRIVER_TYPE_HARDWARE, NULL, 0, D3D10_SDK_VERSION, &sd, &pSwapChain, &pd3dDevice );

			if (SUCCEEDED(hr))
			{	
				//get the present function of the swapchain
				uintptr_t *a=(uintptr_t *)*(uintptr_t *)pSwapChain;
				*presentaddress=a[8]; //8th element is Present()			
			
				//now cleanup
				pSwapChain->Release();				
				pd3dDevice->Release();
			}		
		}

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

							*d3d9resetaddress=a[16]; //16th element is reset
							*d3d9presentaddress=a[17]; //17th element is Present()										
							

							//d3d9device->Present(NULL,NULL,0,NULL);
							d3d9device->Release();
						}
					}

					d3d9->Release();
				}
			}


		}

		DestroyWindow(x);	
	}

}

HRESULT __stdcall D3D9_Reset_new(IDirect3DDevice9 *Device, D3DPRESENT_PARAMETERS *pPresentationParameters)
{
	if (shared)
	{
			if (D3D9Hook_Reset==NULL)
			{
				char dllpath[MAX_PATH];				
				strcpy_s(dllpath, MAX_PATH, shared->CheatEngineDir);
#ifdef AMD64
				strcat_s(dllpath, MAX_PATH, "CED3D9Hook64.dll");
#else
				strcat_s(dllpath, MAX_PATH, "CED3D9Hook.dll");
#endif

				HMODULE hdll=LoadLibraryA((char *)dllpath);
				D3D9Hook_Reset=(D3D9HookResetAPICall)GetProcAddress(hdll, "D3D9Hook_Reset_imp");
			}

			if (D3D9Hook_Reset)
				return D3D9Hook_Reset(D3D9_Reset_Original, Device, pPresentationParameters, shared);
			else
				return D3D9_Reset_Original(Device, pPresentationParameters);
	}
	else
		return D3D9_Reset_Original(Device, pPresentationParameters);


}

HRESULT __stdcall D3D9_Present_new(IDirect3DDevice9 *Device, RECT* pSourceRect,CONST RECT* pDestRect,HWND hDestWindowOverride,CONST RGNDATA* pDirtyRegion)
{
	if (shared)
	{
			if (D3D9Hook_Present==NULL)
			{

				char dllpath[MAX_PATH];				
				strcpy_s(dllpath, MAX_PATH, shared->CheatEngineDir);
#ifdef AMD64
				strcat_s(dllpath, MAX_PATH, "CED3D9Hook64.dll");
#else
				strcat_s(dllpath, MAX_PATH, "CED3D9Hook.dll");
#endif

				HMODULE hdll=LoadLibraryA((char *)dllpath);
				D3D9Hook_Present=(D3D9HookPresentAPICall)GetProcAddress(hdll, "D3D9Hook_Present_imp");
			}

			if (D3D9Hook_Present)
				D3D9Hook_Present(Device, shared);

	}
	return D3D9_Present_Original(Device, pSourceRect, pDestRect, hDestWindowOverride, pDirtyRegion);
}



HRESULT __stdcall IDXGISwapChain_Present_new(IDXGISwapChain *x, UINT SyncInterval, UINT Flags)
{

	if (shared)	
	{
		ID3D11Device *dev11;
		ID3D10Device1 *dev10_1;
		ID3D10Device *dev10;

		

		//find what kind of device this is				
		if (SUCCEEDED(x->GetDevice(__uuidof(ID3D11Device), (void**)&dev11)))
		{
			//D3D11

			//make sure the D3DHook11.dll is loaded and pass on this device ,swapchain and shared buffer
			if (D3D11Hook_SwapChain_Present==NULL)
			{
				//load the dll and use getprocaddress
#ifdef DEBUG
				D3D11Hook_SwapChain_Present=(D3D10PlusHookPresentAPICall)D3D11Hook_SwapChain_Present_imp;
#else
				char dllpath[MAX_PATH];
				strcpy_s(dllpath, MAX_PATH, shared->CheatEngineDir);
				strcat_s(dllpath, MAX_PATH, "CED3D11Hook.dll");

				HMODULE hdll=LoadLibraryA(dllpath);
				D3D11Hook_SwapChain_Present=(D3D10PlusHookPresentAPICall)GetProcAddress(hdll, "D3D11Hook_SwapChain_Present_imp");
#endif
			}


			if (D3D11Hook_SwapChain_Present)
				D3D11Hook_SwapChain_Present(x, dev11, shared);

			dev11->Release();
		}
		else
		if (SUCCEEDED(x->GetDevice(__uuidof(ID3D10Device), (void**)&dev10)))
		{
			//D3D11

			//make sure the D3DHook10.dll is loaded and pass on this device ,swapchain and shared buffer
			if (D3D10Hook_SwapChain_Present==NULL)
			{
#ifdef DEBUG
			    D3D10Hook_SwapChain_Present=(D3D10PlusHookPresentAPICall)D3D10Hook_SwapChain_Present_imp;
#else
				char dllpath[MAX_PATH];
				strcpy_s(dllpath, MAX_PATH, shared->CheatEngineDir);
				strcat_s(dllpath, MAX_PATH, "CED3D10Hook.dll");

				HMODULE hdll=LoadLibraryA(dllpath);
				D3D10Hook_SwapChain_Present=(D3D10PlusHookPresentAPICall)GetProcAddress(hdll, "D3D10Hook_SwapChain_Present_imp");
#endif
		
			}

			if (D3D10Hook_SwapChain_Present)
				D3D10Hook_SwapChain_Present(x, dev10, shared);

			dev10->Release();
		}
		else
		if (SUCCEEDED(x->GetDevice(__uuidof(ID3D10Device1), (void**)&dev10_1)))
		{
			//D3D10.1

			//make sure the D3DHook10.1.dll is loaded and pass on this device ,swapchain and shared buffer
			if (D3D10_1Hook_SwapChain_Present==NULL)
			{
#ifdef DEBUG
				//D3D10_1Hook_SwapChain_Present=(D3D10PlusHookPresentAPICall)D3D10_1Hook_SwapChain_Present_imp;
				D3D10_1Hook_SwapChain_Present=(D3D10PlusHookPresentAPICall)D3D10Hook_SwapChain_Present_imp; //since 10_1 inherits from 10 this should just work
#else
				char dllpath[MAX_PATH];
				strcpy_s(dllpath, MAX_PATH, shared->CheatEngineDir);
				strcat_s(dllpath, MAX_PATH, "CED3D10Hook.dll");

				HMODULE hdll=LoadLibraryA(dllpath);
				D3D10_1Hook_SwapChain_Present=(D3D10PlusHookPresentAPICall)GetProcAddress(hdll, "D3D10Hook_SwapChain_Present_imp");
#endif
			}

			if (D3D10_1Hook_SwapChain_Present)
				D3D10_1Hook_SwapChain_Present(x, dev10_1, shared);

			dev10_1->Release();
		}		

	}
	
	//call original present
	return DXGI_Present_Original(x, SyncInterval, Flags);
	//return x->Present(SyncInterval, Flags);	
}



DWORD WINAPI InitializeD3DHookDll(PVOID params)
{
	//called when the dll is injected
	//open the map
	uintptr_t present=0,d3d9present=0, d3d9reset=0;
	HANDLE fmhandle;

	char sharename[100];
	char eventname[100];
	

//#ifdef DEBUG	
//	sprintf_s(sharename, 100,"CED3D_DEBUG2");	
	//sprintf_s(eventname, 100,"CED3D_DEBUG2_READY");	
//#else
	sprintf_s(sharename, 100,"CED3D_%d", GetCurrentProcessId());
	sprintf_s(eventname, 100,"%s_READY", sharename);	
//#endif
 
	fmhandle=OpenFileMappingA(FILE_MAP_EXECUTE | FILE_MAP_READ | FILE_MAP_WRITE, FALSE, sharename);
  
    shared=(PD3DHookShared)MapViewOfFile(fmhandle,FILE_MAP_EXECUTE | FILE_MAP_READ | FILE_MAP_WRITE, 0,0,0 );   


	if (shared)
	{

		

		GetAddresses(&present,&d3d9present, &d3d9reset);

		//tell ce the address to hook
		shared->dxgi_present=present;
		shared->d3d9_present=d3d9present;
		shared->d3d9_reset=d3d9reset;

		//tell ce the address where the hook should point to
		shared->dxgi_newpresent=(uintptr_t)IDXGISwapChain_Present_new;
		shared->d3d9_newpresent=(uintptr_t)D3D9_Present_new;
		shared->d3d9_newreset=(uintptr_t)D3D9_Reset_new;

		//tell ce where it should write a pointer to the unhooked version of the hooked functions
		shared->dxgi_originalpresent=(uintptr_t)&DXGI_Present_Original;
		shared->d3d9_originalpresent=(uintptr_t)&D3D9_Present_Original;
		shared->d3d9_originalreset=(uintptr_t)&D3D9_Reset_Original;
	}

	HANDLE eventhandle=OpenEventA(EVENT_MODIFY_STATE, FALSE, eventname);
	if (eventhandle)
	{
		SetEvent(eventhandle);
		CloseHandle(eventhandle);
	}

#ifdef DEBUG
	D3D9_Present_Original=(D3D9_PRESENT_ORIGINAL)d3d9present;
	DXGI_Present_Original=(DXGI_PRESENT_ORIGINAL)present;	
#endif
	return 0;
}


