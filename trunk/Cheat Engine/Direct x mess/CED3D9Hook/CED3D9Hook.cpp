// CED3D9Hook.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"


using namespace std;
map<IDirect3DDevice9 *, DXMessD3D9Handler *> D3D9devices;

PD3DHookShared shared=NULL; //set at first present
int insidehook;

BOOL makeSnapshot=FALSE; //duplicate variable to be used as a hint that one of the devices is making a snapshot. (in case of multiple devices)



void DXMessD3D9Handler::PrepareForSnapshot() //just clears the screen
{
	if (!shared->progressiveSnapshot) //if this is true no erasing will be done
	{
		if (!shared->alsoClearDepthBuffer)
			dev->Clear(0, NULL, D3DCLEAR_TARGET, 0xFFFF00FF, 0.0f, 0);
		else
			dev->Clear(0, NULL, D3DCLEAR_TARGET, 0xFFFF00FF, 1.0f, 0);
	}
}

void DXMessD3D9Handler::TakeSnapshot(char *functionname)
{
	IDirect3DDevice9 *d=dev;
	IDirect3DSurface9 *backbuffer;
	dev->EndScene();


	if (SUCCEEDED(d->GetRenderTarget(0, &backbuffer)))	
	{
		BOOL savethis=FALSE;
		D3DSURFACE_DESC desc;


		if (smallSnapshot)
		{
			if (SUCCEEDED(backbuffer->GetDesc( &desc)))
			{	
				IDirect3DSurface9 *backbuffer2=NULL;
				if (SUCCEEDED(d->CreateOffscreenPlainSurface(desc.Width, desc.Height, desc.Format, D3DPOOL_SYSTEMMEM, &backbuffer2, NULL)))
				{
					if (SUCCEEDED(d->GetRenderTargetData(backbuffer, backbuffer2)))
					{
						D3DLOCKED_RECT r;
						if (SUCCEEDED(backbuffer2->LockRect(&r, NULL, D3DLOCK_READONLY)))
						{
							DWORD *color;
							
							int xpos, ypos;
							
							xpos=(int)floor(desc.Width * smallSnapshotPointRelative.x);
							ypos=(int)floor(desc.Height * smallSnapshotPointRelative.y);

							
						    color=(DWORD *)((UINT_PTR)r.pBits+r.Pitch*ypos+xpos*4);
							if ((*color & 0xffffff)!=0xff00ff)	//pixels got changed
								savethis=TRUE;
						}

						backbuffer2->UnlockRect();
					}
					backbuffer2->Release();
				}


				
			}
		}
		else
			savethis=TRUE;
			

		

		if (savethis)
		{
			char s[MAX_PATH];
			UINT64 stackbase=0;
			HANDLE h;
			int x;
			DWORD bw;
			LPD3DXBUFFER dest=NULL;
		

#ifdef _AMD64_
			stackbase=(UINT64)&functionname;
#else
			
			__asm
			{
				mov dword ptr [stackbase], ebp   //sure, it's a bit too far, but close enough
			}			
#endif
			snapshotCounter++;

			
			sprintf_s(s, MAX_PATH, "%ssnapshot%d.ce3dsnapshot", shared->SnapShotDir, snapshotCounter);

			h=CreateFileA(s, GENERIC_WRITE,  0, NULL, CREATE_ALWAYS, 0, NULL);
			x=9; //dx9
			WriteFile(h, &x, sizeof(x), &bw, NULL); 

			if (shared->savePNGSeperateAsWell)
			{			
				switch (shared->snapshotImageFormat)
				{
					case 0: 
						strcat_s(s,MAX_PATH, ".BMP");
						break;

					case 1:
						strcat_s(s,MAX_PATH, ".JPG");
						break;

					case 3:
						strcat_s(s,MAX_PATH, ".PNG");
						break;

					default:
						strcat_s(s,MAX_PATH, ".WTF");
						break;
				}
				
				D3DXSaveSurfaceToFileA(s, (D3DXIMAGE_FILEFORMAT)shared->snapshotImageFormat, backbuffer, NULL, NULL);
			}


			if (SUCCEEDED(D3DXSaveSurfaceToFileInMemory(&dest, (D3DXIMAGE_FILEFORMAT)shared->snapshotImageFormat, backbuffer, NULL, NULL)))
			{
				x=shared->snapshotImageFormat;
				WriteFile(h, &x, sizeof(x), &bw, NULL);
				x=dest->GetBufferSize();
				WriteFile(h, &x, sizeof(x), &bw, NULL); 													
				WriteFile(h, dest->GetBufferPointer(), x, &bw, NULL); 
			}
			else
			{
				x=0;
				WriteFile(h, &x, sizeof(x), &bw, NULL);
				WriteFile(h, &x, sizeof(x), &bw, NULL); 
			}

			dest->Release();

			WriteFile(h, &stackbase, sizeof(stackbase), &bw, NULL);
			
			MEMORY_BASIC_INFORMATION mbi;
			int stacksize;

			if (VirtualQuery((void *)stackbase, &mbi, sizeof(mbi))==sizeof(mbi))
			{								
				stacksize=min(mbi.RegionSize- ((uintptr_t)stackbase-(uintptr_t)mbi.BaseAddress), 8192);
				WriteFile(h, &stacksize, sizeof(stacksize), &bw, NULL);
				WriteFile(h, (void *)stackbase, stacksize, &bw, NULL); 
			}
			else
			{
				stacksize=0;
				stackbase=0;
				WriteFile(h, &stacksize, sizeof(stacksize), &bw, NULL);								
			}

			//no constant buffer	
			{

				//d->GetVertexDeclaration
				/* perhaps something could be done with this, but no idea right now
				IDirect3DVertexShader9 *vs;
				
				if (SUCCEEDED(d->GetVertexShader(&vs)))
				{
					vs->QueryInterface(
				
					vs->Release();
				}*/

				x=0;	
				WriteFile(h, &x, sizeof(x), &bw, NULL);	
			}

			if (functionname)
			{
				int i=strlen(functionname);
				WriteFile(h, &i, sizeof(i), &bw, NULL);	
				WriteFile(h, functionname, i, &bw, NULL);	
			}


			CloseHandle(h);
			

		}
		

		
		backbuffer->Release();


		
	}

	dev->BeginScene();
	
	
}



void DXMessD3D9Handler::DrawString(D3DXVECTOR3 position, PTextureData9 pFontTexture, char *s, int strlen, float alphablend)
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
		else
		if ((s[i] < 32) || (s[i] > 127))
			continue;//invalid char

		float offset=pFontTexture->DefinedFontMap->charinfo[s[i]-32].offset;
		float width=pFontTexture->DefinedFontMap->charinfo[s[i]-32].charwidth;
		RECT charactertexture;

		charactertexture.left=(LONG)offset;
		charactertexture.top=0;
		charactertexture.bottom=(LONG)fGlyphSizeY;
		charactertexture.right=(LONG)(offset+width);	

		D3DXMatrixTransformation(&m, NULL, NULL, &scale, NULL, NULL, &currentpos);						
		sprite->SetTransform(&m);					
		

		
		sprite->Draw(pFontTexture->pTexture, &charactertexture, NULL, NULL, D3DCOLOR_ARGB((int)floor(alphablend*255),255,255,255));		

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

			if (textures==NULL)
			{
				OutputDebugStringA("Failure allocating memory for texturedata");
				SetEvent((HANDLE)(shared->TextureLock)); //unlock
				return FALSE;
			}

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
					

					hr=D3DXCreateTextureFromFileInMemoryEx(dev, (void *)(tea[i].AddressOfTexture), tea[i].size, D3DX_DEFAULT, D3DX_DEFAULT, 1,0,D3DFMT_UNKNOWN, D3DPOOL_MANAGED,D3DX_FILTER_NONE,D3DX_DEFAULT, 0, &imageinfo, NULL, &textures[i].pTexture);
					if( FAILED( hr ) )
					{						
						char s[255];

						//OutputDebugStringA("Fail 1");
						hr=D3DXCreateTextureFromFileInMemoryEx(dev, (void *)(tea[i].AddressOfTexture), tea[i].size, D3DX_DEFAULT, D3DX_DEFAULT, 1, 0, D3DFMT_UNKNOWN, D3DPOOL_MANAGED, D3DX_FILTER_NONE, D3DX_DEFAULT, 0,  &imageinfo, NULL, &textures[i].pTexture);

						if (FAILED(hr))
						{
							//OutputDebugStringA("Fail 2");


							hr=D3DXCreateTextureFromFileInMemoryEx(dev, (void *)(tea[i].AddressOfTexture), tea[i].size, D3DX_DEFAULT, D3DX_DEFAULT, 1, 0, D3DFMT_UNKNOWN, D3DPOOL_DEFAULT, D3DX_FILTER_NONE, D3DX_DEFAULT, 0, &imageinfo, NULL, &textures[i].pTexture);
							if (FAILED(hr))
							{
								//OutputDebugStringA("Fail 3");

								
								hr=D3DXCreateTextureFromFileInMemory(dev, (void *)(tea[i].AddressOfTexture), tea[i].size, &textures[i].pTexture);
								if (FAILED(hr))
								{
									//OutputDebugStringA("Fail 4");
									textures[i].pTexture=NULL;
									sprintf_s(s,254,"Failure creating a texture. (%x) AOT=%p size=%d", hr, (void *)(tea[i].AddressOfTexture), tea[i].size);

									if (hr==D3DERR_NOTAVAILABLE)
										sprintf_s(s,254,"%s - D3DERR_NOTAVAILABLE",s);
									
									if (hr==D3DERR_OUTOFVIDEOMEMORY)
										sprintf_s(s,254,"%s - D3DERR_OUTOFVIDEOMEMORY",s);

									if (hr==D3DERR_INVALIDCALL)
										sprintf_s(s,254,"%s - D3DERR_INVALIDCALL",s);

									if (hr==D3DXERR_INVALIDDATA)
										sprintf_s(s,254,"%s - D3DXERR_INVALIDDATA",s);

									if (hr==E_OUTOFMEMORY)
										sprintf_s(s,254,"%s - D3DERR_NOTAVAILABLE",s);

									OutputDebugStringA(s);
								
	
									SetEvent((HANDLE)(shared->TextureLock));

									return hr;
								}
								else
								{
									//just get the width and height from the texture
									if (textures[i].pTexture)
									{
										D3DSURFACE_DESC d;
										textures[i].pTexture->GetLevelDesc(0, &d);

										imageinfo.Width=d.Width;
										imageinfo.Height=d.Height;
										
									}

								}
							}
						}
					}

					if (textures[i].pTexture) //should be 100% true here
					{
						D3DSURFACE_DESC d;
						textures[i].pTexture->GetLevelDesc(0, &d);

						textures[i].width=(float)imageinfo.Width;
						textures[i].height=(float)imageinfo.Height;				
						

						if (tea[i].AddressOfFontmap)
						{
							int j;
							float currentOffset=0;

							textures[i].DefinedFontMap=(PFONTMAP)malloc(sizeof(FONTMAP));

							if (textures[i].DefinedFontMap==NULL)
							{
								OutputDebugStringA("Failure allocating memory for fontmap");
								SetEvent((HANDLE)(shared->TextureLock)); //unlock
								return FALSE;
							}

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

float tmp=0.01f;

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
							D3DXVECTOR2 scale,position, rotation;							
							D3DXMATRIX m;
							int tid=shared->RenderCommands[i].sprite.textureid;

							if ((tid<TextureCount) && (textures[tid].pTexture))
							{

								//render a sprite

								//set the dimensions
								if (shared->RenderCommands[i].sprite.width==-1)
								{
									if (!vp.Width)									
										dev->GetViewport(&vp);

									scale.x=(float)vp.Width / (float)textures[tid].width;
								}
								else
									scale.x=(float)shared->RenderCommands[i].sprite.width / (float)textures[tid].width;

								scale.y=(float)shared->RenderCommands[i].sprite.height / (float)textures[tid].height;
						
								

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
								if (shared->RenderCommands[i].y==-1)
								{
									if (!vp.Width)									
										dev->GetViewport(&vp);

									position.y=((float)vp.Height / 2.0f) - ((float)shared->RenderCommands[i].sprite.height / 2.0f);
								}
								else
									position.y=(float)shared->RenderCommands[i].y;	
									
		

								
								//truncate to a pixel exact position
								position.x=(float)(int)position.x;
								position.y=(float)(int)position.y;
							

								RECT texturepos;

								texturepos.left=0;
								texturepos.top=0;
								texturepos.bottom=(LONG)textures[tid].height;  //shared->RenderCommands[i].sprite.height;
								texturepos.right=(LONG)textures[tid].width; //shared->RenderCommands[i].sprite.width;

								rotation.x=shared->RenderCommands[i].centerX;
								rotation.y=shared->RenderCommands[i].centerY;

								position.x-=shared->RenderCommands[i].centerX;
								position.y-=shared->RenderCommands[i].centerY;							
				
			
								D3DXMatrixTransformation2D(&m, NULL, NULL, &scale, &rotation, shared->RenderCommands[i].rotation, &position);						
								sprite->SetTransform(&m);	

								hr=sprite->Draw(textures[tid].pTexture, &texturepos, NULL, NULL, D3DCOLOR_ARGB((int)(shared->RenderCommands[i].alphablend*255),255,255,255));
							}
							break;
						}

						case rcDrawFont:
						{							
							int tid=shared->RenderCommands[i].font.fontid;							

							if ((tid<TextureCount) && (textures[tid].pTexture))
							{
								char *s;
								D3DXVECTOR3 position;
								PTextureData9 td;

								if (!vp.Width)									
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
									float width=0, maxwidth=0;
									int slen=strlen(s);
									int j;

									for (j=0; j<slen; j++)
									{
										if ((s[j]>=32) && (s[j]<(96+32)))
											width+=td->DefinedFontMap->charinfo[s[j]-32].charwidth;
										else
										if (s[j]=='\n')
											width=0;
										
										if (width>maxwidth)
											maxwidth=width;
									}
									position.x=((float)vp.Width / 2.0f) - ((float)maxwidth / 2.0f);
									
								}

								if (position.y==-1)
								{						
									//vertical center						
									position.y=((float)vp.Height / 2.0f) - ((float)td->DefinedFontMap->charheight / 2.0f);
								}

								position.x=(float)(int)position.x;
								position.y=(float)(int)position.y;

								//now draw the string
								//truncate to a pixel exact position

								DrawString(position, &textures[tid], s,strlen(s), shared->RenderCommands[i].alphablend);

							}


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

	snapshotCounter=0;
	lastSnapshot=0;
	makeSnapshot=FALSE;
	smallSnapshot=FALSE;

	
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

	tea=(PTextureEntry)((uintptr_t)shared+shared->texturelist);
	UpdateTextures();

	hr=D3DXCreateSprite(dev, &sprite); //
	if( FAILED( hr ) )
		OutputDebugStringA("D3DXCreateSprite FAILED");		

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

	//check if a snapshot key is down
	if (currenthandler->makeSnapshot) //possible threading issue if more than one render device
	{
		//notify CE that all render operations have completed
		shared->snapshotcount=currenthandler->snapshotCounter;
		shared->canDoSnapshot=0;
		SetEvent((HANDLE)shared->snapshotDone);		
		currenthandler->makeSnapshot=FALSE;
	}

	
	if (shared->canDoSnapshot)
	{
		
		if ((shared->snapshotKey) && (GetTickCount()>currenthandler->lastSnapshot+250))
		{
			SHORT ks=GetAsyncKeyState(shared->snapshotKey);
			currenthandler->makeSnapshot=((ks & 1) || (ks & (1 << 15)));
			currenthandler->smallSnapshot=FALSE;

			if (currenthandler->makeSnapshot)
				currenthandler->lastSnapshot=GetTickCount();
			
		}

		if ((currenthandler->makeSnapshot==FALSE) && (shared->smallSnapshotKey) && (GetTickCount()>currenthandler->lastSnapshot+250) && (shared->lastHwnd))
		{
			SHORT ks=GetAsyncKeyState(shared->smallSnapshotKey);
			currenthandler->makeSnapshot=((ks & 1) || (ks & (1 << 15)));

			if (currenthandler->makeSnapshot)
			{
				POINT p;
				currenthandler->smallSnapshot=TRUE;
				currenthandler->lastSnapshot=GetTickCount();

				//get the pixel the mouse is hovering over
				GetCursorPos(&p);
				ScreenToClient((HWND)shared->lastHwnd, &p);
				currenthandler->smallSnapshotPoint=p;

				GetClientRect((HWND)shared->lastHwnd, &currenthandler->smallSnapshotClientRect);


				//get the relative position (0.00 - 1.00) this position is in for the clientrect
				currenthandler->smallSnapshotPointRelative.x=(float)currenthandler->smallSnapshotPoint.x/(float)(currenthandler->smallSnapshotClientRect.right-currenthandler->smallSnapshotClientRect.left);
				currenthandler->smallSnapshotPointRelative.y=(float)currenthandler->smallSnapshotPoint.y/(float)(currenthandler->smallSnapshotClientRect.bottom-currenthandler->smallSnapshotClientRect.top);
			}
			
		}


		

		if (currenthandler->makeSnapshot)
		{

			makeSnapshot=TRUE; //once true, always true

			
			//clear the render target with a specific color
			currenthandler->dev->Clear(0, NULL, D3DCLEAR_TARGET, 0xFFFF00FF, 1.0f, 0);
			
			currenthandler->snapshotCounter=0;
			

		}
	}
	
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
	
	if (((shared) && ((shared->wireframe) || (shared->disabledzbuffer) || (makeSnapshot) ) && (insidehook==0)))
	{
		//setup for wireframe	

		DWORD oldfillmode;
		DWORD oldzenable;
		HRESULT hr,hr2;
		
		hr=device->GetRenderState(D3DRS_FILLMODE, &oldfillmode);
		hr2=device->GetRenderState(D3DRS_ZENABLE, &oldzenable);


		DXMessD3D9Handler *currentDevice=D3D9devices[device];

		if ((currentDevice) && (currentDevice->makeSnapshot))
			currentDevice->PrepareForSnapshot();

		if (SUCCEEDED(hr) && SUCCEEDED(hr2))
		{			
			device->SetRenderState(D3DRS_FILLMODE,D3DFILL_WIREFRAME);
			hr=originalfunction(device, PrimitiveType, StartVertex, PrimitiveCount);
			device->SetRenderState(D3DRS_FILLMODE,oldfillmode);
			device->SetRenderState(D3DRS_ZENABLE,oldzenable);
			
		}
		else
			hr=originalfunction(device, PrimitiveType, StartVertex, PrimitiveCount);


		if ((currentDevice) && (currentDevice->makeSnapshot))
			currentDevice->TakeSnapshot("D3D9Hook_DrawPrimitive_imp");

		return hr;

	}
	return originalfunction(device, PrimitiveType, StartVertex, PrimitiveCount);	
}

HRESULT __stdcall D3D9Hook_DrawIndexedPrimitive_imp(D3D9_DRAWINDEXEDPRIMITIVE_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRIMITIVETYPE PrimitiveType,INT BaseVertexIndex,UINT MinVertexIndex,UINT NumVertices,UINT startIndex,UINT primCount)
{	

	if (((shared) && ((shared->wireframe) || (shared->disabledzbuffer) || (makeSnapshot) ) && (insidehook==0)))
	{
		//setup for wireframe and/or zbuffer	

		DWORD oldfillmode;
		DWORD oldzenable;
		HRESULT hr,hr2;
		
		hr=device->GetRenderState(D3DRS_FILLMODE, &oldfillmode);
		hr2=device->GetRenderState(D3DRS_ZENABLE, &oldzenable);

		DXMessD3D9Handler *currentDevice=D3D9devices[device];		


		if ((currentDevice) && (currentDevice->makeSnapshot))
			currentDevice->PrepareForSnapshot();

		if (SUCCEEDED(hr) && SUCCEEDED(hr2))
		{			
			if (shared->wireframe)
				device->SetRenderState(D3DRS_FILLMODE,D3DFILL_WIREFRAME);

			if (shared->disabledzbuffer)
				device->SetRenderState(D3DRS_ZENABLE,FALSE);




			hr=originalfunction(device, PrimitiveType, BaseVertexIndex, MinVertexIndex, NumVertices, startIndex, primCount);
			device->SetRenderState(D3DRS_FILLMODE,oldfillmode);
			device->SetRenderState(D3DRS_ZENABLE,oldzenable);
			
		}
		else 
			hr=originalfunction(device, PrimitiveType, BaseVertexIndex, MinVertexIndex, NumVertices, startIndex, primCount);

		if ((currentDevice) && (currentDevice->makeSnapshot))
			currentDevice->TakeSnapshot("D3D9Hook_DrawIndexedPrimitive_imp");

		return hr;

	}
	return originalfunction(device, PrimitiveType, BaseVertexIndex, MinVertexIndex, NumVertices, startIndex, primCount);
}

HRESULT __stdcall D3D9Hook_DrawPrimitiveUP_imp(D3D9_DRAWPRIMITIVEUP_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRIMITIVETYPE PrimitiveType,UINT PrimitiveCount,CONST void* pVertexStreamZeroData,UINT VertexStreamZeroStride)
{	
	if (((shared) && ((shared->wireframe) || (shared->disabledzbuffer) || (makeSnapshot) ) && (insidehook==0)))
	{
		//setup for wireframe and/or zbuffer	

		DWORD oldfillmode;
		DWORD oldzenable;
		HRESULT hr,hr2;
		
		hr=device->GetRenderState(D3DRS_FILLMODE, &oldfillmode);
		hr2=device->GetRenderState(D3DRS_ZENABLE, &oldzenable);


		DXMessD3D9Handler *currentDevice=D3D9devices[device];
		if ((currentDevice) && (currentDevice->makeSnapshot))
			currentDevice->PrepareForSnapshot();

		if (SUCCEEDED(hr) && SUCCEEDED(hr2))
		{			
			if (shared->wireframe)
				device->SetRenderState(D3DRS_FILLMODE,D3DFILL_WIREFRAME);

			if (shared->disabledzbuffer)
				device->SetRenderState(D3DRS_ZENABLE,FALSE);	


			
	
			hr=originalfunction(device, PrimitiveType, PrimitiveCount, pVertexStreamZeroData, VertexStreamZeroStride);



			device->SetRenderState(D3DRS_FILLMODE,oldfillmode);
			device->SetRenderState(D3DRS_ZENABLE,oldzenable);
			
		}
		else
			hr=originalfunction(device, PrimitiveType, PrimitiveCount, pVertexStreamZeroData, VertexStreamZeroStride);

		if ((currentDevice) && (currentDevice->makeSnapshot))
			currentDevice->TakeSnapshot("D3D9Hook_DrawPrimitiveUP_imp");

		return hr;
	}

	return originalfunction(device, PrimitiveType, PrimitiveCount, pVertexStreamZeroData, VertexStreamZeroStride);
}

HRESULT __stdcall D3D9Hook_DrawIndexedPrimitiveUP_imp(D3D9_DRAWINDEXEDPRIMITIVEUP_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRIMITIVETYPE PrimitiveType,UINT MinVertexIndex,UINT NumVertices,UINT PrimitiveCount,CONST void* pIndexData,D3DFORMAT IndexDataFormat,CONST void* pVertexStreamZeroData,UINT VertexStreamZeroStride)
{	
	if (((shared) && ((shared->wireframe) || (shared->disabledzbuffer) || (makeSnapshot) ) && (insidehook==0)))
	{
		//setup for wireframe and/or zbuffer	

		DWORD oldfillmode;
		DWORD oldzenable;
		HRESULT hr,hr2;
		
		hr=device->GetRenderState(D3DRS_FILLMODE, &oldfillmode);
		hr2=device->GetRenderState(D3DRS_ZENABLE, &oldzenable);


		DXMessD3D9Handler *currentDevice=D3D9devices[device];
		if ((currentDevice) && (currentDevice->makeSnapshot))
			currentDevice->PrepareForSnapshot();

		if (SUCCEEDED(hr) && SUCCEEDED(hr2))
		{			
			if (shared->wireframe)
				device->SetRenderState(D3DRS_FILLMODE,D3DFILL_WIREFRAME);

			if (shared->disabledzbuffer)
				device->SetRenderState(D3DRS_ZENABLE,FALSE);

		
			hr=originalfunction(device, PrimitiveType, MinVertexIndex, NumVertices, PrimitiveCount, pIndexData, IndexDataFormat, pVertexStreamZeroData, VertexStreamZeroStride);
			device->SetRenderState(D3DRS_FILLMODE,oldfillmode);
			device->SetRenderState(D3DRS_ZENABLE,oldzenable);
			
		}
		else
			hr=originalfunction(device, PrimitiveType, MinVertexIndex, NumVertices, PrimitiveCount, pIndexData, IndexDataFormat, pVertexStreamZeroData, VertexStreamZeroStride);


		if ((currentDevice) && (currentDevice->makeSnapshot))
			currentDevice->TakeSnapshot("D3D9Hook_DrawIndexedPrimitiveUP_imp");

		return hr;
	}
	return originalfunction(device, PrimitiveType, MinVertexIndex, NumVertices, PrimitiveCount, pIndexData, IndexDataFormat, pVertexStreamZeroData, VertexStreamZeroStride);
}

HRESULT __stdcall D3D9Hook_DrawRectPatch_imp(D3D9_DRAWRECTPATCH_ORIGINAL originalfunction, IDirect3DDevice9 *device, UINT Handle,CONST float* pNumSegs,CONST D3DRECTPATCH_INFO* pRectPatchInfo)
{	
	
	if (((shared) && ((shared->wireframe) || (shared->disabledzbuffer) || (makeSnapshot) ) && (insidehook==0)))
	{
		//setup for wireframe and/or zbuffer	

		DWORD oldfillmode;
		DWORD oldzenable;
		HRESULT hr,hr2;
		
		hr=device->GetRenderState(D3DRS_FILLMODE, &oldfillmode);
		hr2=device->GetRenderState(D3DRS_ZENABLE, &oldzenable);

		DXMessD3D9Handler *currentDevice=D3D9devices[device];
		if ((currentDevice) && (currentDevice->makeSnapshot))
			currentDevice->PrepareForSnapshot();


		if (SUCCEEDED(hr) && SUCCEEDED(hr2))
		{			
			if (shared->wireframe)
				device->SetRenderState(D3DRS_FILLMODE,D3DFILL_WIREFRAME);

			if (shared->disabledzbuffer)
				device->SetRenderState(D3DRS_ZENABLE,FALSE);

			hr=originalfunction(device, Handle, pNumSegs, pRectPatchInfo);
			device->SetRenderState(D3DRS_FILLMODE,oldfillmode);
			device->SetRenderState(D3DRS_ZENABLE,oldzenable);
			
		}
		else
			hr=originalfunction(device, Handle, pNumSegs, pRectPatchInfo);

		if ((currentDevice) && (currentDevice->makeSnapshot))
			currentDevice->TakeSnapshot("D3D9Hook_DrawRectPatch_imp");

		return hr;
	}
	return originalfunction(device, Handle, pNumSegs, pRectPatchInfo);
}

HRESULT __stdcall D3D9Hook_DrawTriPatch_imp(D3D9_DRAWTRIPATCH_ORIGINAL originalfunction, IDirect3DDevice9 *device, UINT Handle,CONST float* pNumSegs,CONST D3DTRIPATCH_INFO* pTriPatchInfo)
{	
	if (((shared) && ((shared->wireframe) || (shared->disabledzbuffer) || (makeSnapshot) ) && (insidehook==0)))
	{
		//setup for wireframe and/or zbuffer	

		DWORD oldfillmode;
		DWORD oldzenable;
		HRESULT hr,hr2;
		
		hr=device->GetRenderState(D3DRS_FILLMODE, &oldfillmode);
		hr2=device->GetRenderState(D3DRS_ZENABLE, &oldzenable);

		DXMessD3D9Handler *currentDevice=D3D9devices[device];
		if ((currentDevice) && (currentDevice->makeSnapshot))
			currentDevice->PrepareForSnapshot();

		if (SUCCEEDED(hr) && SUCCEEDED(hr2))
		{			
			if (shared->wireframe)
				device->SetRenderState(D3DRS_FILLMODE,D3DFILL_WIREFRAME);

			if (shared->disabledzbuffer)
				device->SetRenderState(D3DRS_ZENABLE,FALSE);


			hr=originalfunction(device, Handle, pNumSegs, pTriPatchInfo);
			device->SetRenderState(D3DRS_FILLMODE,oldfillmode);
			device->SetRenderState(D3DRS_ZENABLE,oldzenable);
	
		}
		else
			hr=originalfunction(device, Handle, pNumSegs, pTriPatchInfo);

		if ((currentDevice) && (currentDevice->makeSnapshot))
			currentDevice->TakeSnapshot("D3D9Hook_DrawTriPatch_imp");

		return hr;

	}
	return originalfunction(device, Handle, pNumSegs, pTriPatchInfo);
}


	
    //STDMETHOD(DrawTriPatch)(THIS_ UINT Handle,CONST float* pNumSegs,CONST D3DTRIPATCH_INFO* pTriPatchInfo) PURE;
