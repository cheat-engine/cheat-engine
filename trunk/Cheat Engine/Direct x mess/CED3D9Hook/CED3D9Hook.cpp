// CED3D9Hook.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"


using namespace std;
map<IDirect3DDevice9 *, DXMessD3D9Handler *> D3D9devices;

PD3DHookShared shared=NULL; //set at first present
int insidehook;

void DXMessD3D9Handler::DrawString(D3DXVECTOR3 position, PTextureData9 pFontTexture, char *s, int strlen)
/*
Render the text using the sprite object (select a texture region and draw that specific character) .Coordinates are in normal x,y coordinates
Pre: Sprite must be active
*/
{
	int i;
	D3DXVECTOR3 currentpos = position;
	float fGlyphSizeY = pFontTexture->DefinedFontMap->charheight;
	position.z=0;

	D3DXVECTOR3 scale;
	D3DXMATRIX m;


	//calculate the original size of the texture

	scale.x=1.0f;
	scale.y=1.0f;
	scale.z=1.0f;


	for (i=0; i<strlen; i++)
	{
		if( s[i] == '\n' ) //next line
		{
			currentpos.x=position.x;
			currentpos.y+=fGlyphSizeY;

			continue;
		}

		float offset=pFontTexture->DefinedFontMap->charinfo[s[i]-32].offset;
		float width=pFontTexture->DefinedFontMap->charinfo[s[i]-32].charwidth;
		RECT charactertexture;

		charactertexture.left=(LONG)offset;
		charactertexture.top=0;
		charactertexture.bottom=(LONG)fGlyphSizeY;
		charactertexture.right=(LONG)(offset+width);	

		D3DXMatrixTransformation(&m, NULL, NULL, &scale, NULL, NULL, &currentpos);						
		sprite->SetTransform(&m);					

		sprite->Draw(pFontTexture->pTexture, &charactertexture, NULL, NULL, D3DCOLOR_ARGB(255,255,255,255));		

		currentpos.x+=width;



	}	
}

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


BOOL DXMessD3D9Handler::UpdateTextures()
{
	int i;
	int newTextureCount;
	HRESULT hr;

	WaitForSingleObject((HANDLE)(shared->TextureLock), INFINITE);

	if (shared->textureCount)
	{
		newTextureCount=shared->textureCount;
		

		if (shared->textureCount > TextureCount)
		{				
			//update the textures if needed
	

			if (textures==NULL) //initial alloc
				textures=(TextureData9 *)malloc(sizeof(TextureData9)* shared->textureCount);			
			else //realloc
				textures=(TextureData9 *)realloc(textures, sizeof(TextureData9)* shared->textureCount);			


			//initialize the new entries to NULL
			for (i=TextureCount; i<shared->textureCount; i++)
			{
				textures[i].pTexture=NULL;				
				textures[i].DefinedFontMap=NULL;
			}	

			TextureCount=shared->textureCount;
			
		}

		for (i=0; i<TextureCount; i++)
		{
			
			if (tea[i].AddressOfTexture)
			{
				
				
				if ((tea[i].hasBeenUpdated) || (textures[i].pTexture==NULL))
				{
					if (textures[i].pTexture)
					{
						//already has a texture, so an update. Free the old one	first
						textures[i].pTexture->Release();
						textures[i].pTexture=NULL; //should always happen
					}

					if (textures[i].DefinedFontMap)
					{
						//already has a fontmap. Free the old one
						free(textures[i].DefinedFontMap);
						textures[i].DefinedFontMap=NULL;
					}

					D3DXIMAGE_INFO imageinfo;
					ZeroMemory(&imageinfo, sizeof(imageinfo));
					

					hr=D3DXCreateTextureFromFileInMemoryEx(dev, (void *)(tea[i].AddressOfTexture), tea[i].size, D3DX_DEFAULT, D3DX_DEFAULT, 1,0,D3DFMT_A8R8G8B8, D3DPOOL_MANAGED,D3DX_FILTER_NONE,D3DX_DEFAULT, 0, &imageinfo, NULL, &textures[i].pTexture);
					if( FAILED( hr ) )
					{
						OutputDebugStringA("Failure creating a texture");
						return hr;
					}

					D3DSURFACE_DESC d;
					textures[i].pTexture->GetLevelDesc(0, &d);

					textures[i].width=(float)imageinfo.Width;
					textures[i].height=(float)imageinfo.Height;				
					

					if (tea[i].AddressOfFontmap)
					{
						int j;
						float currentOffset=0;

						textures[i].DefinedFontMap=(PFONTMAP)malloc(sizeof(FONTMAP));
						//now parse the fontmap provided by ce and fill in the gaps						
						
						
						WORD *cefontmap=(WORD *)(tea[i].AddressOfFontmap);											
						textures[i].DefinedFontMap->charheight=(float)cefontmap[0];

						for (j=0; j<96; j++)
						{
							textures[i].DefinedFontMap->charinfo[j].offset=currentOffset;
							textures[i].DefinedFontMap->charinfo[j].charwidth=(float)cefontmap[j+1];

							currentOffset+=cefontmap[j+1];
						}						

						textures[i].DefinedFontMap->fullwidth=currentOffset;
					}

					tea[i].hasBeenUpdated=0;

				}			
			}
			else
			{
				//It's NULL
				if (textures[i].pTexture)
				{
					textures[i].pTexture->Release();
					textures[i].pTexture=NULL;
				}

				if (textures[i].DefinedFontMap)
				{				
					free(textures[i].DefinedFontMap);
					textures[i].DefinedFontMap=NULL;
				}
			}
			
		}
	}
	if (shared->texturelistHasUpdate)
		InterlockedExchange((volatile LONG *)&shared->texturelistHasUpdate,0);		
	
	
	SetEvent((HANDLE)(shared->TextureLock));

	return TRUE;	
}

void DXMessD3D9Handler::RenderOverlay()
{	
	int i;
	HRESULT hr;
	D3DVIEWPORT9 vp;
	POINT clientMousepos;
	vp.Width=0;
	clientMousepos.x=-1;
	clientMousepos.y=-1;


	if (sprite)
	{
		if (shared->lastHwnd==0)
		{
			D3DDEVICE_CREATION_PARAMETERS cp;
			dev->GetCreationParameters(&cp);
			shared->lastHwnd=(DWORD)cp.hFocusWindow;			
		}

		if (shared->texturelistHasUpdate)
			UpdateTextures();
		

		hr=dev->BeginScene();

		if (SUCCEEDED(hr))
		{
			
			
			hr=sprite->Begin(D3DXSPRITE_ALPHABLEND);
			if (!SUCCEEDED(hr))
			{
				sprite->OnLostDevice();
				sprite->OnResetDevice();
				hr=sprite->Begin(D3DXSPRITE_ALPHABLEND);
			}

			
		
			if (SUCCEEDED(hr))
			{
				BOOL hasLock=FALSE;
				i=0;

				if (shared->UseCommandlistLock)
					hasLock=WaitForSingleObject((HANDLE)shared->CommandlistLock, INFINITE)==WAIT_OBJECT_0;
				

				while (shared->RenderCommands[i].Command)
				{
					switch (shared->RenderCommands[i].Command)
					{
						case rcDrawSprite:
						{
							D3DXVECTOR3 scale,position;
							D3DXMATRIX m;
							int tid=shared->RenderCommands[i].sprite.textureid;

							if ((tid<TextureCount) && (textures[tid].pTexture))
							{

								//render a sprite

								//set the dimensions
								
								scale.x=(float)shared->RenderCommands[i].sprite.width / (float)textures[tid].width;
								scale.y=(float)shared->RenderCommands[i].sprite.height / (float)textures[tid].height;
								scale.z=1.0f;
								

								//set the position
								
								//set x
								if (shared->RenderCommands[i].x==-2)
								{
									//mouse position
									if (clientMousepos.x==-1)
									{
										//get the mouse position
										GetCursorPos(&clientMousepos);
										ScreenToClient((HWND)shared->lastHwnd, &clientMousepos);	
									}

									//set x to the x position of the mouse (center is origin)
									position.x=(float)clientMousepos.x-((float)shared->RenderCommands[i].sprite.width / 2.0f);  //make the center of the texture the position of the mouse (to add crosshairs, and normal mousecursors just have to keep that in mind so only render in the bottom left quadrant
								}
								else
								if (shared->RenderCommands[i].x==-1) //center
								{
									if (!vp.Width)									
										dev->GetViewport(&vp);

									position.x=((float)vp.Width / 2.0f) - ((float)shared->RenderCommands[i].sprite.width / 2.0f);
								}
								else
									position.x=shared->RenderCommands[i].x;



								//set y
								if (shared->RenderCommands[i].y==-2)
								{
									//set to the position of the mouse (center is origin)
								
									if (clientMousepos.x==-1)
									{
										//get the mouse position
										GetCursorPos(&clientMousepos);
										ScreenToClient((HWND)shared->lastHwnd, &clientMousepos);										
									}									
									position.y=(float)clientMousepos.y-((float)shared->RenderCommands[i].sprite.height / 2.0f);		
								}
								else
								if (shared->RenderCommands[i].y==-2)
								{
									if (!vp.Width)									
										dev->GetViewport(&vp);

									position.y=((float)vp.Height / 2.0f) - ((float)shared->RenderCommands[i].sprite.height / 2.0f);
								}
								else
									position.y=(float)shared->RenderCommands[i].y;	
									
								position.z=0.0f;

								RECT texturepos;

								texturepos.left=0;
								texturepos.top=0;
								texturepos.bottom=(LONG)textures[tid].height;  //shared->RenderCommands[i].sprite.height;
								texturepos.right=(LONG)textures[tid].width; //shared->RenderCommands[i].sprite.width;


								D3DXMatrixTransformation(&m, NULL, NULL, &scale, NULL, NULL, &position);						
								sprite->SetTransform(&m);					

								hr=sprite->Draw(textures[tid].pTexture, &texturepos, NULL, NULL, D3DCOLOR_ARGB((int)(shared->RenderCommands[i].alphablend*255),255,255,255));						
							}
							break;
						}

						case rcDrawFont:
						{
							D3DXVECTOR3 position;
							PTextureData9 td;
							char *s;

							D3DVIEWPORT9 vp;
							dev->GetViewport(&vp);
						


							if (!hasLock)
								hasLock=WaitForSingleObject((HANDLE)shared->CommandlistLock, INFINITE)==WAIT_OBJECT_0; //fonts demand a lock  (stringpointer)

							position.x=(float)shared->RenderCommands[i].x;
							position.y=(float)shared->RenderCommands[i].y;	
							position.z=0;

							td=&textures[shared->RenderCommands[i].font.fontid];
							s=(char *)shared->RenderCommands[i].font.addressoftext;	

							if (position.x==-1) 
							{
								//horizontal center
								//calculate the width
								float width=0;
								int slen=strlen(s);

								for (i=0; i<slen; i++)
								{
									width+=td->DefinedFontMap->charinfo[32-i].charwidth;
								}
								position.x=((float)vp.Width / 2.0f) - ((float)width / 2.0f);
								
							}

							if (position.y==-1)
							{						
								//vertical center						
								position.y=((float)vp.Height / 2.0f) - ((float)td->DefinedFontMap->charheight / 2.0f);
							}

							//now draw the string (nyi)
							DrawString(position, &textures[shared->RenderCommands[i].font.fontid], s,strlen(s));



							break;
						}

					}

					i++;
				}

				if (hasLock) //release the lock if it was obtained
					SetEvent((HANDLE)shared->CommandlistLock);
		
				hr=sprite->Flush();			
				hr=sprite->End();
			}
			
			
			dev->EndScene();
		}
		
		
		
	}
	
}


/*

typedef ULONG (__stdcall *RELEASE)(IUnknown *self);
typedef ULONG (__stdcall *ADDREF)(IUnknown *self);


int careAboutRefcount=1;
ADDREF origAddRef;
RELEASE origRelease;


ULONG __stdcall d3d9devAddRef(IUnknown *self)
{
	int i=0;
	if (insidehook==0)
	{
		i=origAddRef(self);
	}
		
	return i;
}

ULONG __stdcall d3d9devRelease(IUnknown *self)
{
	int i;
	if (insidehook)
		return 1;

	origAddRef(self);
	i=origRelease(self);

	if (i==1)
	{
		//free the DXMess handler
		MessageBoxA(0,"Going to free the handler","FREE Device", MB_OK);		
	}

	i=origRelease(self);		
	return 0; //i;
}
*/

DXMessD3D9Handler::DXMessD3D9Handler(IDirect3DDevice9 *dev, PD3DHookShared shared)
{
	HRESULT hr;
	sprite=NULL;

	
	//Failed experiment. For some reason a[1] and a[2] get restored to their original address (which is the main reason why I use the deep hook method)
	//If it is important, I can add Release and AddRef to the hooklist at a later date
	/*
	if (careAboutRefcount)
	{
		DWORD old;
		//hook AddRef and Release for this object
		uintptr_t *a=(uintptr_t *)*(uintptr_t *)dev;
		VirtualProtect(dev, 8, PAGE_EXECUTE_READWRITE, &old);
		
		

		origAddRef=(ADDREF)a[1];
		origRelease=(RELEASE)a[2];
		a[1]=(uintptr_t)&d3d9devAddRef;
		a[2]=(uintptr_t)&d3d9devRelease;
		VirtualProtect(dev, 8, old, &old);
	}
	*/


	dev->AddRef();

	this->dev=dev;
	this->shared=shared;

	textures=NULL;
	TextureCount=0;

	hr=D3DXCreateSprite(dev, &sprite); //
	if( FAILED( hr ) )
		return;

	tea=(PTextureEntry)((uintptr_t)shared+shared->texturelist);

	UpdateTextures();
}

DXMessD3D9Handler::~DXMessD3D9Handler()
{
	D3D9devices[dev]=NULL;
}
	
void __stdcall D3D9Hook_Present_imp(IDirect3DDevice9 *device, PD3DHookShared s)
{	
	insidehook=1;
	DXMessD3D9Handler *currenthandler=D3D9devices[device];
	if (currenthandler==NULL)
	{
		currenthandler=new DXMessD3D9Handler(device, s);//create a new devicehandler
		D3D9devices[device]=currenthandler;
		shared=s;
	}
	
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
