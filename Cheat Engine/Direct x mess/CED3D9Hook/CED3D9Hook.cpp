// CED3D9Hook.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"


using namespace std;
map<IDirect3DDevice9 *, DXMessD3D9Handler *> D3D9devices;

PD3DHookShared shared=NULL; //set at first present
int insidehook;

void DXMessD3D9Handler::BeforeReset()
{
	

	if (sprite)
		sprite->OnLostDevice();

}

void DXMessD3D9Handler::AfterReset()
{
	if (sprite)
		sprite->OnResetDevice();

	shared->lastHwnd=0; //reset that
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
			overlays[i].actualWidth=0;
			overlays[i].actualHeight=0;		
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

				D3DSURFACE_DESC d;

				overlays[i].pOverlayTex->GetLevelDesc(0, &d);
				overlays[i].actualWidth=d.Width;
				overlays[i].actualHeight=d.Height;



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

		if (shared->lastHwnd==0)
		{
			D3DDEVICE_CREATION_PARAMETERS cp;
			dev->GetCreationParameters(&cp);
			shared->lastHwnd=(DWORD)cp.hFocusWindow;			
		}

		if ((shared->MouseOverlayId>=0) && (OverlayCount>=shared->MouseOverlayId) && (shared->resources[shared->MouseOverlayId].valid))
		{
			//update the mouse position each frame for as long as the mouse is valid
			
			

			POINT p;	

			p.x=0;
			p.y=0;

			GetCursorPos(&p);

			ScreenToClient((HWND)shared->lastHwnd, &p);			
			
			overlays[shared->MouseOverlayId].x=p.x;
			overlays[shared->MouseOverlayId].y=p.y;
		}



		hr=dev->BeginScene();

		if (SUCCEEDED(hr))
		{
			
			sprite->OnLostDevice();
			sprite->OnResetDevice();
			hr=sprite->Begin(D3DXSPRITE_ALPHABLEND);

		
			if (SUCCEEDED(hr))
			{

				for (i=0; i<OverlayCount; i++)
					if (shared->resources[i].valid)
					{
						D3DXVECTOR3 scale,position;
						
						D3DXMATRIX m;

						scale.x=(float)shared->resources[i].width / (float)overlays[i].actualWidth;
						scale.y=(float)shared->resources[i].height / (float)overlays[i].actualHeight;
						scale.z=1.0f;

						if ((overlays[i].x==-1) && (overlays[i].y==-1))
						{
							//center of screen
							D3DVIEWPORT9 vp;
							dev->GetViewport(&vp);
							
							position.x=((float)vp.Width / 2.0f) - ((float)shared->resources[i].width / 2.0f);
							position.y=((float)vp.Height / 2.0f) - ((float)shared->resources[i].height / 2.0f);
						}
						else
						{
							position.x=(float)overlays[i].x;
							position.y=(float)overlays[i].y;							
						}	
						position.z=0.0f;
		

						D3DXMatrixTransformation(&m, NULL, NULL, &scale, NULL, NULL, &position);
						
						sprite->SetTransform(&m);

						

						hr=sprite->Draw(overlays[i].pOverlayTex, NULL, NULL, NULL, D3DCOLOR_ARGB((int)(shared->resources[i].alphaBlend*255),255,255,255));
					}
		
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
	
void __stdcall D3D9Hook_Present_imp(IDirect3DDevice9 *device, PD3DHookShared s)
{

	
	DXMessD3D9Handler *currenthandler=D3D9devices[device];
	if (currenthandler==NULL)
	{
		currenthandler=new DXMessD3D9Handler(device, s);//create a new devicehandler
		D3D9devices[device]=currenthandler;
		shared=s;
	}
	insidehook=1;
	currenthandler->RenderOverlay();	
	insidehook=0;
}

HRESULT __stdcall D3D9Hook_Reset_imp(D3D9_RESET_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRESENT_PARAMETERS *pPresentationParameters)
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

HRESULT __stdcall D3D9Hook_DrawPrimitive_imp(D3D9_DRAWPRIMITIVE_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRIMITIVETYPE PrimitiveType,UINT StartVertex,UINT PrimitiveCount)
{
	
	if ((shared) && ((shared->wireframe) || (shared->disabledzbuffer) ) && (insidehook==0) )
	{
		//setup for wireframe	

		DWORD oldfillmode;
		DWORD oldzenable;
		HRESULT hr,hr2;
		
		hr=device->GetRenderState(D3DRS_FILLMODE, &oldfillmode);
		hr2=device->GetRenderState(D3DRS_ZENABLE, &oldzenable);


		if (SUCCEEDED(hr) && SUCCEEDED(hr2))
		{			
			device->SetRenderState(D3DRS_FILLMODE,D3DFILL_WIREFRAME);
			hr=originalfunction(device, PrimitiveType, StartVertex, PrimitiveCount);
			device->SetRenderState(D3DRS_FILLMODE,oldfillmode);
			device->SetRenderState(D3DRS_ZENABLE,oldzenable);
			return hr;
		}
	}
	return originalfunction(device, PrimitiveType, StartVertex, PrimitiveCount);	
}

HRESULT __stdcall D3D9Hook_DrawIndexedPrimitive_imp(D3D9_DRAWINDEXEDPRIMITIVE_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRIMITIVETYPE PrimitiveType,INT BaseVertexIndex,UINT MinVertexIndex,UINT NumVertices,UINT startIndex,UINT primCount)
{	
	if ((shared) && ((shared->wireframe) || (shared->disabledzbuffer) ) && (insidehook==0) )
	{
		//setup for wireframe and/or zbuffer	

		DWORD oldfillmode;
		DWORD oldzenable;
		HRESULT hr,hr2;
		
		hr=device->GetRenderState(D3DRS_FILLMODE, &oldfillmode);
		hr2=device->GetRenderState(D3DRS_ZENABLE, &oldzenable);


		if (SUCCEEDED(hr) && SUCCEEDED(hr2))
		{			
			if (shared->wireframe)
				device->SetRenderState(D3DRS_FILLMODE,D3DFILL_WIREFRAME);

			if (shared->disabledzbuffer)
				device->SetRenderState(D3DRS_ZENABLE,FALSE);

			hr=originalfunction(device, PrimitiveType, BaseVertexIndex, MinVertexIndex, NumVertices, startIndex, primCount);
			device->SetRenderState(D3DRS_FILLMODE,oldfillmode);
			device->SetRenderState(D3DRS_ZENABLE,oldzenable);
			return hr;
		}
	}
	return originalfunction(device, PrimitiveType, BaseVertexIndex, MinVertexIndex, NumVertices, startIndex, primCount);
}

HRESULT __stdcall D3D9Hook_DrawPrimitiveUP_imp(D3D9_DRAWPRIMITIVEUP_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRIMITIVETYPE PrimitiveType,UINT PrimitiveCount,CONST void* pVertexStreamZeroData,UINT VertexStreamZeroStride)
{	
	if ((shared) && ((shared->wireframe) || (shared->disabledzbuffer) ) && (insidehook==0) )
	{
		//setup for wireframe and/or zbuffer	

		DWORD oldfillmode;
		DWORD oldzenable;
		HRESULT hr,hr2;
		
		hr=device->GetRenderState(D3DRS_FILLMODE, &oldfillmode);
		hr2=device->GetRenderState(D3DRS_ZENABLE, &oldzenable);


		if (SUCCEEDED(hr) && SUCCEEDED(hr2))
		{			
			if (shared->wireframe)
				device->SetRenderState(D3DRS_FILLMODE,D3DFILL_WIREFRAME);

			if (shared->disabledzbuffer)
				device->SetRenderState(D3DRS_ZENABLE,FALSE);			
	
			hr=originalfunction(device, PrimitiveType, PrimitiveCount, pVertexStreamZeroData, VertexStreamZeroStride);
			device->SetRenderState(D3DRS_FILLMODE,oldfillmode);
			device->SetRenderState(D3DRS_ZENABLE,oldzenable);
			return hr;
		}
	}

	return originalfunction(device, PrimitiveType, PrimitiveCount, pVertexStreamZeroData, VertexStreamZeroStride);
}

HRESULT __stdcall D3D9Hook_DrawIndexedPrimitiveUP_imp(D3D9_DRAWINDEXEDPRIMITIVEUP_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRIMITIVETYPE PrimitiveType,UINT MinVertexIndex,UINT NumVertices,UINT PrimitiveCount,CONST void* pIndexData,D3DFORMAT IndexDataFormat,CONST void* pVertexStreamZeroData,UINT VertexStreamZeroStride)
{	
	if ((shared) && ((shared->wireframe) || (shared->disabledzbuffer) ) && (insidehook==0) )
	{
		//setup for wireframe and/or zbuffer	

		DWORD oldfillmode;
		DWORD oldzenable;
		HRESULT hr,hr2;
		
		hr=device->GetRenderState(D3DRS_FILLMODE, &oldfillmode);
		hr2=device->GetRenderState(D3DRS_ZENABLE, &oldzenable);


		if (SUCCEEDED(hr) && SUCCEEDED(hr2))
		{			
			if (shared->wireframe)
				device->SetRenderState(D3DRS_FILLMODE,D3DFILL_WIREFRAME);

			if (shared->disabledzbuffer)
				device->SetRenderState(D3DRS_ZENABLE,FALSE);

		
			hr=originalfunction(device, PrimitiveType, MinVertexIndex, NumVertices, PrimitiveCount, pIndexData, IndexDataFormat, pVertexStreamZeroData, VertexStreamZeroStride);
			device->SetRenderState(D3DRS_FILLMODE,oldfillmode);
			device->SetRenderState(D3DRS_ZENABLE,oldzenable);
			return hr;
		}
	}
	return originalfunction(device, PrimitiveType, MinVertexIndex, NumVertices, PrimitiveCount, pIndexData, IndexDataFormat, pVertexStreamZeroData, VertexStreamZeroStride);
}

HRESULT __stdcall D3D9Hook_DrawRectPatch_imp(D3D9_DRAWRECTPATCH_ORIGINAL originalfunction, IDirect3DDevice9 *device, UINT Handle,CONST float* pNumSegs,CONST D3DRECTPATCH_INFO* pRectPatchInfo)
{	
	
	if ((shared) && ((shared->wireframe) || (shared->disabledzbuffer) ) && (insidehook==0) )
	{
		//setup for wireframe and/or zbuffer	

		DWORD oldfillmode;
		DWORD oldzenable;
		HRESULT hr,hr2;
		
		hr=device->GetRenderState(D3DRS_FILLMODE, &oldfillmode);
		hr2=device->GetRenderState(D3DRS_ZENABLE, &oldzenable);


		if (SUCCEEDED(hr) && SUCCEEDED(hr2))
		{			
			if (shared->wireframe)
				device->SetRenderState(D3DRS_FILLMODE,D3DFILL_WIREFRAME);

			if (shared->disabledzbuffer)
				device->SetRenderState(D3DRS_ZENABLE,FALSE);

			hr=originalfunction(device, Handle, pNumSegs, pRectPatchInfo);
			device->SetRenderState(D3DRS_FILLMODE,oldfillmode);
			device->SetRenderState(D3DRS_ZENABLE,oldzenable);
			return hr;
		}
	}
	return originalfunction(device, Handle, pNumSegs, pRectPatchInfo);
}

HRESULT __stdcall D3D9Hook_DrawTriPatch_imp(D3D9_DRAWTRIPATCH_ORIGINAL originalfunction, IDirect3DDevice9 *device, UINT Handle,CONST float* pNumSegs,CONST D3DTRIPATCH_INFO* pTriPatchInfo)
{	
	if ((shared) && ((shared->wireframe) || (shared->disabledzbuffer) ) && (insidehook==0) )
	{
		//setup for wireframe and/or zbuffer	

		DWORD oldfillmode;
		DWORD oldzenable;
		HRESULT hr,hr2;
		
		hr=device->GetRenderState(D3DRS_FILLMODE, &oldfillmode);
		hr2=device->GetRenderState(D3DRS_ZENABLE, &oldzenable);


		if (SUCCEEDED(hr) && SUCCEEDED(hr2))
		{			
			if (shared->wireframe)
				device->SetRenderState(D3DRS_FILLMODE,D3DFILL_WIREFRAME);

			if (shared->disabledzbuffer)
				device->SetRenderState(D3DRS_ZENABLE,FALSE);


			hr=originalfunction(device, Handle, pNumSegs, pTriPatchInfo);
			device->SetRenderState(D3DRS_FILLMODE,oldfillmode);
			device->SetRenderState(D3DRS_ZENABLE,oldzenable);
			return hr;
		}
	}
	return originalfunction(device, Handle, pNumSegs, pTriPatchInfo);
}


	
    //STDMETHOD(DrawTriPatch)(THIS_ UINT Handle,CONST float* pNumSegs,CONST D3DTRIPATCH_INFO* pTriPatchInfo) PURE;
