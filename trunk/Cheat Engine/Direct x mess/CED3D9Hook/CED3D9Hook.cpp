// CED3D9Hook.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"


using namespace std;
map<IDirect3DDevice9 *, DXMessD3D9Handler *> D3D9devices;

void DXMessD3D9Handler::BeforeReset()
{
	if (sprite)
		sprite->OnLostDevice();

	//sprite=NULL;
}

void DXMessD3D9Handler::AfterReset()
{
	if (sprite)
		sprite->OnResetDevice();
}


HRESULT DXMessD3D9Handler::setupOverlayTexture()
{
	int i;
	HRESULT hr=S_OK;

	if (shared->overlaycount==0)
		return S_OK;

	if (shared->overlaycount > OverlayCount)
	{	
		//update the textures if needed{
		int newcount=shared->overlaycount;
	

		if (overlays==NULL) //initial alloc
		{
			
			overlays=(OverlayData9 *)malloc(sizeof(OverlayData9)* newcount);			
		}
		else
		{
			//realloc
			overlays=(OverlayData9 *)realloc(overlays, sizeof(OverlayData9)* newcount);			
		}


		//initialize the new entries to NULL
		for (i=OverlayCount; i<shared->overlaycount; i++)
		{
			overlays[i].pOverlayTex=NULL;
			overlays[i].y=overlays[i].x=-1;
				
			overlays[i].pOverlayVB=NULL;			
		}	

		OverlayCount=newcount;
	}

	for (i=0; i<OverlayCount; i++)
	{
		if (shared->resources[i].valid)
		{
			if ((shared->resources[i].updatedresource) || (overlays[i].pOverlayTex==NULL))
			{
				if (overlays[i].pOverlayTex)
				{
					
					if (overlays[i].pOverlayTex->Release()==0)
						overlays[i].pOverlayTex=NULL; //should always happen
				}				

				hr=D3DXCreateTextureFromFileInMemoryEx(dev, (void *)(uintptr_t(shared)+shared->resources[i].resourceoffset), shared->resources[i].resourcesize, D3DX_DEFAULT, D3DX_DEFAULT, 1,0,D3DFMT_A8R8G8B8, D3DPOOL_MANAGED,D3DX_DEFAULT,D3DX_DEFAULT, 0xFFFFFFFF, NULL, NULL, &overlays[i].pOverlayTex);
				if( FAILED( hr ) )
				{
					OutputDebugStringA("Failure creating a texture");
					return hr;
				}

			}

			if ((shared->resources[i].updatedpos) || ((overlays[i].x==-1) && (overlays[i].y==-1)))
			{
				overlays[i].x=shared->resources[i].x;
				overlays[i].y=shared->resources[i].y;
			}
		}
	}

	shared->OverLayHasUpdate=0;
	return hr;	
}

void DXMessD3D9Handler::RenderOverlay()
{	
	int i;
	HRESULT hr;

	if (sprite)
	{
		if (shared->OverLayHasUpdate)
			setupOverlayTexture();

		hr=dev->BeginScene();

		if (SUCCEEDED(hr))
		{
			
			sprite->OnLostDevice();
			sprite->OnResetDevice();
			hr=sprite->Begin(D3DXSPRITE_ALPHABLEND);

		
			if (SUCCEEDED(hr))
			{

				for (i=0; i<OverlayCount; i++)	
					hr=sprite->Draw(overlays[0].pOverlayTex, NULL, NULL, &D3DXVECTOR3(overlays[0].x,overlays[0].y,0), D3DCOLOR_ARGB(255,255,255,255));
		
				hr=sprite->Flush();
				
				hr=sprite->End();

			}
		}
		
		dev->EndScene();
	}
	
}

DXMessD3D9Handler::DXMessD3D9Handler(IDirect3DDevice9 *dev, PD3DHookShared shared)
{
	HRESULT hr;
	sprite=NULL;

	dev->AddRef();

	this->dev=dev;
	this->shared=shared;

	overlays=NULL;
	OverlayCount=0;

	hr=D3DXCreateSprite(dev, &sprite); //
	if( FAILED( hr ) )
		return;

	hr=setupOverlayTexture();
	if( FAILED( hr ) )
		return;


}

DXMessD3D9Handler::~DXMessD3D9Handler()
{
	D3D9devices[dev]=NULL;
}
	
void __stdcall D3D9Hook_Present_imp(IDirect3DDevice9 *device, PD3DHookShared shared)
{

	DXMessD3D9Handler *currenthandler=D3D9devices[device];
	if (currenthandler==NULL)
	{
		currenthandler=new DXMessD3D9Handler(device, shared);//create a new devicehandler
		D3D9devices[device]=currenthandler;
	}
	currenthandler->RenderOverlay();	
}

HRESULT __stdcall D3D9Hook_Reset_imp(D3D9_RESET_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRESENT_PARAMETERS *pPresentationParameters, PD3DHookShared shared)
{
	HRESULT hr;
	DXMessD3D9Handler *currenthandler=D3D9devices[device];
	if (currenthandler)
	{
		currenthandler->BeforeReset();
		hr=originalfunction(device, pPresentationParameters);
		if (SUCCEEDED(hr))
			currenthandler->AfterReset();		
	}

	return hr;
}