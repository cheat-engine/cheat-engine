// CED3D11Hook.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"



using namespace std;
map<ID3D11Device *, DXMessD3D11Handler *> D3D11devices;

DXMessD3D11Handler *lastdevice=NULL;
int insidehook=0;
PD3DHookShared shared;
BOOL makeSnapshot;

//definitions
struct SpriteVertex{
    XMFLOAT3 Pos;
    XMFLOAT2 Tex;
};

struct ConstantBuffer
{	
	XMMATRIX rotation;
	XMFLOAT2 originpoint;
	XMFLOAT2 translation;	
	XMFLOAT2 scaling;
	FLOAT transparency;		
	FLOAT garbage; //16 byte alignment crap
};

BOOL test=TRUE;
ID3D11DeviceContext *DXMessD3D11Handler::PrepareForSnapshot(ID3D11DeviceContext *dc) //just clears the screen
{
	ID3D11DeviceContext *drawdc=dc;
	EnterCriticalSection(&cs);

	if ((test) && (dc->GetType()!=D3D11_DEVICE_CONTEXT_IMMEDIATE))
	{
		//If necesary create my own immeadiate context

		if (RenderContext==NULL)
			dev->GetImmediateContext(&RenderContext);

		//get all the data from all the stages (this will be SLOOOOW)

		/*

		drawdc=RenderContext;




		
	

		//get IA
		{
			ID3D11Buffer *IndexBuffer;
			ID3D11InputLayout *InputLayout;
			D3D11_PRIMITIVE_TOPOLOGY Topology;
			DXGI_FORMAT format;
			UINT offset;
			ID3D11Buffer *vertexBuffers[D3D11_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT];
			UINT strides[D3D11_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT];
			UINT offsets[D3D11_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT];


			dc->IAGetIndexBuffer(&IndexBuffer, &format, &offset);
			dc->IAGetInputLayout(&InputLayout);
			dc->IAGetPrimitiveTopology(&Topology);
			dc->IAGetVertexBuffers(0, D3D11_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT, vertexBuffers, strides, offsets);  

			drawdc->IASetIndexBuffer(IndexBuffer, format, offset);
			drawdc->IASetInputLayout(InputLayout);
			drawdc->IASetPrimitiveTopology(Topology);
			drawdc->IASetVertexBuffers(0, D3D11_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT, vertexBuffers, strides, offsets);

		
			if (IndexBuffer)
				IndexBuffer->Release();

			if (InputLayout)
				InputLayout->Release();

			for (i=0; i<D3D11_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT; i++)
				if (vertexBuffers[i])
					vertexBuffers[i]->Release();
				
		}			

	
		//get VS
		{
			ID3D11Buffer *ConstantBuffers[D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT];
			ID3D11SamplerState *Samplers[D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT];
			ID3D11VertexShader *VertexShader=NULL;
			ID3D11ClassInstance **ClassInstances=NULL;
			ID3D11ClassInstance *Bla[10];
			ID3D11ShaderResourceView *ShaderResourceViews[D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT];
			
			UINT NumClassInstances=0;

			dc->VSGetConstantBuffers(0, D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT, ConstantBuffers); 
			dc->VSGetSamplers(0, D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT, Samplers);
			NumClassInstances=10;
			dc->VSGetShader(&VertexShader, Bla, &NumClassInstances);
			

			if (NumClassInstances)
			{
				if (VertexShader)
					VertexShader->Release();

				ClassInstances=(ID3D11ClassInstance **)malloc(NumClassInstances*sizeof(ID3D11ClassInstance *));
			
				dc->VSGetShader(&VertexShader, ClassInstances, &NumClassInstances);					
			}

			dc->VSGetShaderResources(0,D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT, ShaderResourceViews);

			drawdc->VSSetShader(VertexShader, ClassInstances, NumClassInstances);
			drawdc->VSSetConstantBuffers(0,D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT, ConstantBuffers);
			drawdc->VSSetSamplers(0, D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT, Samplers);
			
			drawdc->VSSetShaderResources(0,D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT, ShaderResourceViews);
			

		
			for (i=0; i<D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT; i++)
				if (ConstantBuffers[i])
					ConstantBuffers[i]->Release();

			for (i=0; i<D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT; i++)
				if (Samplers[i])
					Samplers[i]->Release();
			
			if (VertexShader)  //for some reason the game tends to crash if I release the vertex shader here. Seems like refcount is 0 (I thought VSSetShader would increase it with 1)
				VertexShader->Release();

			if (ClassInstances)
			{
			
				for (i=0; i<NumClassInstances; i++)
					if (ClassInstances[i])
						ClassInstances[i]->Release();
						
				
				free(ClassInstances);
			}

		
			for (i=0; i<D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT; i++)
			{
				if (ShaderResourceViews[i])
					ShaderResourceViews[i]->Release();
			}
		

			
		}


		//get HS (Hull shader)
		{
			ID3D11Buffer *ConstantBuffers[D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT];
			ID3D11SamplerState *Samplers[D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT];
			ID3D11HullShader *HullShader=NULL;
			ID3D11ClassInstance **ClassInstances=NULL;
			ID3D11ShaderResourceView *ShaderResourceViews[D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT];
			
			UINT NumClassInstances=0;

			dc->HSGetConstantBuffers(0, D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT, ConstantBuffers); 
			dc->HSGetSamplers(0, D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT, Samplers);
			dc->HSGetShader(&HullShader, NULL, &NumClassInstances);
			

			if (NumClassInstances)
			{
				if (HullShader)
					HullShader->Release();

				ClassInstances=(ID3D11ClassInstance **)malloc(NumClassInstances*sizeof(ID3D11ClassInstance *));
			
				dc->HSGetShader(&HullShader, ClassInstances, &NumClassInstances);					
			}

			dc->HSGetShaderResources(0,D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT, ShaderResourceViews);

			drawdc->HSSetShader(HullShader, ClassInstances, NumClassInstances);
			drawdc->HSSetConstantBuffers(0,D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT, ConstantBuffers);
			drawdc->HSSetSamplers(0, D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT, Samplers);
			
			drawdc->HSSetShaderResources(0,D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT, ShaderResourceViews);
	
			if (ClassInstances)
			{	
				free(ClassInstances);
			}

		

		}

		//get DS (Domain shader)
		{
			ID3D11Buffer *ConstantBuffers[D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT];
			ID3D11SamplerState *Samplers[D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT];
			ID3D11DomainShader *DomainShader=NULL;
			ID3D11ClassInstance **ClassInstances=NULL;
			ID3D11ShaderResourceView *ShaderResourceViews[D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT];
			
			UINT NumClassInstances=0;

			dc->DSGetConstantBuffers(0, D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT, ConstantBuffers); 
			dc->DSGetSamplers(0, D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT, Samplers);
			dc->DSGetShader(&DomainShader, NULL, &NumClassInstances);


			if (NumClassInstances)
			{
				if (DomainShader)
					DomainShader->Release();

				ClassInstances=(ID3D11ClassInstance **)malloc(NumClassInstances*sizeof(ID3D11ClassInstance *));
			
				dc->DSGetShader(&DomainShader, ClassInstances, &NumClassInstances);					
			}

			dc->DSGetShaderResources(0,D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT, ShaderResourceViews);

			drawdc->DSSetShader(DomainShader, ClassInstances, NumClassInstances);
			drawdc->DSSetConstantBuffers(0,D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT, ConstantBuffers);
			drawdc->DSSetSamplers(0, D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT, Samplers);
			
			drawdc->DSSetShaderResources(0,D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT, ShaderResourceViews);
	
			if (ClassInstances)
			{	
				free(ClassInstances);
			}

		}



	
		//get GS
		{
			ID3D11Buffer *ConstantBuffers[D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT];
			ID3D11SamplerState *Samplers[D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT];
			UINT NumClassInstances=0;
			ID3D11GeometryShader *GeometryShader=NULL;
			ID3D11ClassInstance **ClassInstances=NULL;
			ID3D11ShaderResourceView *ShaderResourceViews[D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT];


			dc->GSGetConstantBuffers(0, D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT, ConstantBuffers);
			dc->GSGetSamplers(0, D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT, Samplers);
			dc->GSGetShader(&GeometryShader, NULL, &NumClassInstances);



			if (NumClassInstances)
			{
				if (GeometryShader)
					GeometryShader->Release();

				ClassInstances=(ID3D11ClassInstance **)malloc(NumClassInstances*sizeof(ID3D11ClassInstance *));
			
				dc->GSGetShader(&GeometryShader, ClassInstances, &NumClassInstances);					
			}

			
			dc->GSGetShaderResources(0,D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT, ShaderResourceViews);

			drawdc->GSSetShader(GeometryShader, ClassInstances, NumClassInstances);
			drawdc->GSSetConstantBuffers(0,D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT, ConstantBuffers);
			drawdc->GSSetSamplers(0, D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT, Samplers);
			
			drawdc->GSSetShaderResources(0,D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT, ShaderResourceViews);
			
			for (i=0; i<D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT; i++)
				if (ConstantBuffers[i])
					ConstantBuffers[i]->Release();

			for (i=0; i<D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT; i++)
				if (Samplers[i])
					Samplers[i]->Release();

			if (GeometryShader)
				GeometryShader->Release();

				
			if (ClassInstances)
			{
			
				for (i=0; i<NumClassInstances; i++)
					if (ClassInstances[i])
						ClassInstances[i]->Release();
					

				free(ClassInstances);
			}

	
			for (i=0; i<D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT; i++)
			{
				if (ShaderResourceViews[i])
					ShaderResourceViews[i]->Release();
			}
		
			



		}

		//get SO
		{
			ID3D11Buffer *SOTarget[D3D11_SO_BUFFER_SLOT_COUNT];
			UINT offsets[D3D11_SO_BUFFER_SLOT_COUNT];
			for (i=0; i<D3D11_SO_BUFFER_SLOT_COUNT; i++)
				offsets[i]=-1;

			dc->SOGetTargets(D3D11_SO_BUFFER_SLOT_COUNT, SOTarget);
			drawdc->SOSetTargets(D3D11_SO_BUFFER_SLOT_COUNT, SOTarget, offsets);

	
		
			for (i=0; i<D3D11_SO_BUFFER_SLOT_COUNT; i++)
				if (SOTarget[i])
					SOTarget[i]->Release();
				
					

		}

		//get RS
		{
			D3D11_RECT rects[D3D11_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE];
			UINT rectcount=D3D11_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE;
			D3D11_VIEWPORT viewports[D3D11_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE];
			UINT viewportcount= D3D11_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE;
			

			ID3D11RasterizerState *RasterizerState;

			dc->RSGetScissorRects(&rectcount, NULL);
			dc->RSGetScissorRects(&rectcount, rects);
			dc->RSGetState(&RasterizerState);
			dc->RSGetViewports(&viewportcount, NULL);
			dc->RSGetViewports(&viewportcount,  viewports);			
			

			drawdc->RSSetScissorRects(rectcount, rects);
			drawdc->RSSetState(RasterizerState);
			drawdc->RSSetViewports(viewportcount, viewports);				

			if (RasterizerState)
				RasterizerState->Release();
		}



		//get PS
		{
			ID3D11Buffer *ConstantBuffers[D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT];
			ID3D11SamplerState *Samplers[D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT];
			UINT NumClassInstances=0;
			ID3D11PixelShader *PixelShader=NULL;
			ID3D11ClassInstance **ClassInstances=NULL;
			ID3D11ShaderResourceView *ShaderResourceViews[D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT];


			dc->PSGetConstantBuffers(0, D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT, ConstantBuffers);
			dc->PSGetSamplers(0, D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT, Samplers);
			dc->PSGetShader(&PixelShader, NULL, &NumClassInstances);
	
			if (NumClassInstances)
			{
				if (PixelShader)
					PixelShader->Release();

				ClassInstances=(ID3D11ClassInstance **)malloc(NumClassInstances*sizeof(ID3D11ClassInstance *));
			
				dc->PSGetShader(&PixelShader, ClassInstances, &NumClassInstances);					
			}
			

			
			dc->PSGetShaderResources(0,D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT, ShaderResourceViews);


			drawdc->PSSetShader(NULL, NULL, 0);
			drawdc->PSSetShader(PixelShader, ClassInstances, NumClassInstances);
			drawdc->PSSetConstantBuffers(0,D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT, ConstantBuffers);
			drawdc->PSSetSamplers(0, D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT, Samplers);				
			drawdc->PSSetShaderResources(0,D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT, ShaderResourceViews);
			

	
			for (i=0; i<D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT; i++)
				if (ConstantBuffers[i])
					ConstantBuffers[i]->Release();

			for (i=0; i<D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT; i++)
				if (Samplers[i])
					Samplers[i]->Release();

			if (PixelShader)
				PixelShader->Release();
			

			if (ClassInstances)
			{
			
				for (i=0; i<NumClassInstances; i++)
					if (ClassInstances[i])
						ClassInstances[i]->Release();
					

				free(ClassInstances);
			}
		
			for (i=0; i<D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT; i++)
			{
				if (ShaderResourceViews[i])
					ShaderResourceViews[i]->Release();
			}
			



		}

		//get OM
		{
			ID3D11BlendState* BlendState;
			ID3D11DepthStencilState *DepthStencilState;
			ID3D11RenderTargetView *RenderTargets[D3D11_SIMULTANEOUS_RENDER_TARGET_COUNT];
			ID3D11DepthStencilView *DepthStencilView;
			FLOAT BlendFactor[4];
			UINT SampleMask;
			UINT StencilRef;
			dc->OMGetBlendState( &BlendState, BlendFactor, &SampleMask);
			dc->OMGetDepthStencilState(&DepthStencilState, &StencilRef);
			dc->OMGetRenderTargets(D3D11_SIMULTANEOUS_RENDER_TARGET_COUNT, RenderTargets, &DepthStencilView);
		
			//drawdc->OMGetRenderTargetsAndUnorderedAccessViews

			drawdc->OMSetBlendState(BlendState, BlendFactor, SampleMask);
			drawdc->OMSetDepthStencilState(DepthStencilState, StencilRef);
			drawdc->OMSetRenderTargets(D3D11_SIMULTANEOUS_RENDER_TARGET_COUNT, RenderTargets, DepthStencilView);
		
			for (i=0; i<D3D11_SIMULTANEOUS_RENDER_TARGET_COUNT; i++)
				if (RenderTargets[i])
					RenderTargets[i]->Release();

			
			if (DepthStencilView)
				DepthStencilView->Release();
			

	
			if (BlendState)
				BlendState->Release();
				

		
			if (DepthStencilState)
				DepthStencilState->Release();
				


		

		}

		*/
		
		//OutputDebugStringA("Defered");
	}

	if (makeSnapshot)
	{

		if (!shared->progressiveSnapshot) //if this is true no erasing will be done
		{
			

			ID3D11RenderTargetView *currentrt=NULL;
			ID3D11DepthStencilView* currentds=NULL;
			FLOAT f[]={1.0f,0.0f,1.0f,1.0f};

			//dc->OMSetRenderTargets(1, &ExtraRenderTarget, NULL);

			
			
			if (!shared->alsoClearDepthBuffer)
				drawdc->OMGetRenderTargets(1, &currentrt, NULL);
			else
				drawdc->OMGetRenderTargets(1, &currentrt, &currentds);

			if (currentrt)
			{				
				drawdc->ClearRenderTargetView(currentrt, f);		
				currentrt->Release();
			}

			if (currentds)
			{
				drawdc->ClearDepthStencilView(currentds, D3D11_CLEAR_DEPTH, 1.0, 0);
				currentds->Release();
			}
			
		}
	}

	return drawdc;
}

void DXMessD3D11Handler::TakeSnapshot(ID3D11DeviceContext *dc, char* functionname)
{
	ID3D11DeviceContext *drawdc=dc;
	
	ID3D11RenderTargetView *currentrt=NULL ;



	


	if (makeSnapshot)
	{		
		dc->OMGetRenderTargets(1, &currentrt, NULL);
		if (dc->GetType()!=D3D11_DEVICE_CONTEXT_IMMEDIATE)
		{
			ID3D11CommandList *cl;
			if (SUCCEEDED(dc->FinishCommandList(TRUE, &cl)))
			{
				
				dev->GetImmediateContext(&dc);
				dc->ExecuteCommandList(cl, TRUE);
			}
			else
				dev->GetImmediateContext(&dc);

			
		}

		







		
		if (currentrt)
		{
			ID3D11Texture2D *backbuffer=NULL;
			
			D3D11_TEXTURE2D_DESC texDesc;
			D3D11_RENDER_TARGET_VIEW_DESC backbufferdesc;
			ID3D11Resource *r=NULL;

			currentrt->GetDesc(&backbufferdesc);


			texDesc.Format=backbufferdesc.Format;
			


			currentrt->GetResource(&r);
			if (r)
			{
				BOOL isMultiSampled=FALSE;
				if (SUCCEEDED(r->QueryInterface(__uuidof(ID3D11Texture2D), (void **)&backbuffer)))
				{
					backbuffer->GetDesc(&texDesc);

					isMultiSampled=texDesc.SampleDesc.Count>1;
					
					texDesc.SampleDesc.Count=1;
					texDesc.SampleDesc.Quality=0;

					if (smallSnapshot && (isMultiSampled==FALSE))
					{
						
						texDesc.BindFlags = 0; //D3D11_BIND_UNORDERED_ACCESS;//0;
						texDesc.CPUAccessFlags = D3D11_CPU_ACCESS_READ;					
						texDesc.Usage = D3D11_USAGE_STAGING;
					}
					else
					{
						texDesc.CPUAccessFlags = 0;
						texDesc.Usage = D3D11_USAGE_DEFAULT;
					}

					//figure out a good format				
					
					ID3D11Texture2D *texture;					
					if (SUCCEEDED(dev->CreateTexture2D(&texDesc, 0, &texture)))
					{
			
						BOOL savethis=FALSE;

					
						if (isMultiSampled)
						{							
							dc->ResolveSubresource(texture, 0, r, 0, texDesc.Format);

							if (smallSnapshot)
							{
								ID3D11Texture2D *texture2; //temp storage for the new texture
								
								texDesc.BindFlags = 0; 
								texDesc.CPUAccessFlags = D3D11_CPU_ACCESS_READ;					
								texDesc.Usage = D3D11_USAGE_STAGING;

								if (SUCCEEDED(dev->CreateTexture2D(&texDesc, 0, &texture2)))
								{
									dc->CopyResource(texture2, texture);
									texture->Release(); //release the old one
									texture=texture2;
								}

							}
						}
						else												
							dc->CopyResource(texture, r);
						
						


						

						if (smallSnapshot)
						{
							D3D11_MAPPED_SUBRESOURCE mappedtexture;	


							if (SUCCEEDED(dc->Map(texture, 0, D3D11_MAP_READ, 0, &mappedtexture)))
							{	
								
								PVOID color;
								
								int xpos, ypos;
								int pixelsize=4;
								
								xpos=(int)floor(texDesc.Width * smallSnapshotPointRelative.x);
								ypos=(int)floor(texDesc.Height * smallSnapshotPointRelative.y);

								//check if the pixel at xpos,ypos is not 0xffff00ff		

								//use texDesc.Format to figure out where the pixel is located (size) and what format is equivalent to 1,0,1
								

								if ((texDesc.Format>=DXGI_FORMAT_R32G32B32A32_TYPELESS) && (texDesc.Format<=DXGI_FORMAT_R32G32B32_SINT))
									pixelsize=16;
								else
								if ((texDesc.Format>=DXGI_FORMAT_R16G16B16A16_TYPELESS) && (texDesc.Format<=DXGI_FORMAT_X32_TYPELESS_G8X24_UINT ))
									pixelsize=8;
								else
								if ((texDesc.Format>=DXGI_FORMAT_R10G10B10A2_TYPELESS ) && (texDesc.Format<=DXGI_FORMAT_X24_TYPELESS_G8_UINT ))
									pixelsize=4;								
								else
									pixelsize=-1; //too small, compressed or unknown. Either way, don't use it


								if (pixelsize!=-1)
								{
									color=(PVOID)((UINT_PTR)mappedtexture.pData+mappedtexture.RowPitch*ypos+xpos*pixelsize);
									switch (pixelsize)
									{
										case 16:
										{
											if (texDesc.Format==DXGI_FORMAT_R32G32B32A32_FLOAT)
											{
												typedef struct
												{
													FLOAT c1;
													FLOAT c2;
													FLOAT c3;
													FLOAT c4;
												} *PC128;

												PC128 c=(PC128)color;
												
												savethis=((c->c1!=1.0f) || (c->c2!=0.0f) || (c->c3!=1.0f)); 
											}
											else
											{
												typedef struct
												{
													DWORD c1;
													DWORD c2;
													DWORD c3;
													DWORD c4;
												} *PC128;

												PC128 c=(PC128)color;
												
												savethis=((c->c1!=0xffffffff) || (c->c2!=0x00000000) || (c->c3!=0xffffffff)); 
										
											}
											break;
										}

										case 8:
										{
											if (texDesc.Format==DXGI_FORMAT_R16G16B16A16_FLOAT)
											{
												typedef struct
												{
													WORD c1;
													WORD c2;
													WORD c3;
													WORD c4;
												} *PC64;

												PC64 c=(PC64)color;
												
												savethis=((c->c1!=0x3c00) || (c->c2!=0x0000) || (c->c3!=0x3c00));	

											}
											else
											{
												typedef struct
												{
													WORD c1;
													WORD c2;
													WORD c3;
													WORD c4;
												} *PC64;

												PC64 c=(PC64)color;
												
												savethis=((c->c1!=0xffff) || (c->c2!=0x0000) || (c->c3!=0xffff));										
											}
											break;
										}

										case 4:
										{
											typedef struct
											{
												BYTE c1;
												BYTE c2;
												BYTE c3;
												BYTE c4;
											} *PC32;

											PC32 c=(PC32)color;
											
											savethis=((c->c1!=0xff) || (c->c2!=0x00) || (c->c3!=0xff));										
											break;
										}

										default:
											savethis=FALSE;


									}
								}

								dc->Unmap(texture, 0);											
							}								

						}
						else
							savethis=TRUE;

						if (savethis)				
						{
							char s[MAX_PATH];
							HANDLE h;
							DWORD bw;
							int x;
							UINT64 stackbase=0;

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

							x=11; //dx11
							WriteFile(h, &x, sizeof(x), &bw, NULL); 

							ID3D10Blob *dest=NULL;
				
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
								D3DX11SaveTextureToFileA(dc, texture, (D3DX11_IMAGE_FILE_FORMAT)shared->snapshotImageFormat, s);
							}

							if (SUCCEEDED(D3DX11SaveTextureToMemory(dc, texture, (D3DX11_IMAGE_FILE_FORMAT)shared->snapshotImageFormat, &dest, 0))) //weird. PNG has some information loss on certain things like text
							{
								x=shared->snapshotImageFormat;
								WriteFile(h, &x, sizeof(x), &bw, NULL);
								
								x=dest->GetBufferSize();
								WriteFile(h, &x, sizeof(x), &bw, NULL); 													
								WriteFile(h, dest->GetBufferPointer(), x, &bw, NULL); 

								dest->Release();
							}
							else
							{
								x=0;
								WriteFile(h, &x, sizeof(x), &bw, NULL);
								WriteFile(h, &x, sizeof(x), &bw, NULL); 
							}


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

							ID3D11Buffer* c=NULL;	

							//save the VS Constant buffers

							
							drawdc->VSGetConstantBuffers(0, 1, &c);

							int nocb=1; //if getting the constantbuffers fail this notifies that it has a length of 0

							
							if (c)
							{
								ID3D11Buffer* c2=NULL;
								D3D11_BUFFER_DESC desc;

								c->GetDesc(&desc);
								desc.CPUAccessFlags=D3D11_CPU_ACCESS_READ;
								desc.BindFlags=0;
								desc.Usage=D3D11_USAGE_STAGING;

								dev->CreateBuffer(&desc, NULL, &c2);

								if (c2)
								{
									D3D11_MAPPED_SUBRESOURCE buf;
									
									dc->CopyResource(c2, c);

									if (SUCCEEDED(dc->Map(c2, 0, D3D11_MAP_READ, 0, &buf)))
									{				
										int i=desc.ByteWidth;
										WriteFile(h, &i, sizeof(i), &bw, NULL);

										
										WriteFile(h, buf.pData, desc.ByteWidth, &bw, NULL);

										nocb=0;


										dc->Unmap(c2,0);
									}
									

									c2->Release();
								}
								c->Release();								

							}

							if (nocb)
							{
								int i=0;
								WriteFile(h, &i, sizeof(i), &bw, NULL);								
							}

							if (functionname)
							{
								int i=strlen(functionname);
								WriteFile(h, &i, sizeof(i), &bw, NULL);	
								WriteFile(h, functionname, i, &bw, NULL);	
							}


							//perhaps also save the vertex buffers?

							CloseHandle(h);
							
						}


						texture->Release();
					}
					backbuffer->Release();

				}

				r->Release();
			}

			currentrt->Release();

		}
		
	
	}

	LeaveCriticalSection(&cs);
	
}

void DXMessD3D11Handler::SetupFontVertexBuffer(int count)
{
	if (currentMaxCharacterCount<count)
	{
		HRESULT hr;
		D3D11_BUFFER_DESC bd2d;


		count+=4; //let's add a bit of overhead
		if (pFontVB)
		{
			pFontVB->Release();
			pFontVB=NULL;
		}

		ZeroMemory( &bd2d, sizeof(bd2d) );
		bd2d.Usage = D3D11_USAGE_DYNAMIC;
		bd2d.ByteWidth = sizeof( SpriteVertex ) * 6 * count; //size of a vertex*verticel per char*charcount
		bd2d.BindFlags = D3D11_BIND_VERTEX_BUFFER;
		bd2d.CPUAccessFlags = D3D11_CPU_ACCESS_WRITE;
	                           
		hr = dev->CreateBuffer( &bd2d, NULL, &pFontVB );

		if (SUCCEEDED(hr))
		{
			currentMaxCharacterCount=count;
		}
	}
}

void DXMessD3D11Handler::DrawString(D3D11_VIEWPORT vp, PTextureData11 pFontTexture, char *s, int strlen)
{
	if (pFontTexture)
	{
		ID3D11DeviceContext *dc;
		dev->GetImmediateContext(&dc);
		SetupFontVertexBuffer(strlen);

		if (currentMaxCharacterCount<strlen) return; //error

		//The following code is mainly ripped from DXUTgui.cpp and modified to support dynamic sized characters
		
		
		float fGlyphSizeY = (pFontTexture->DefinedFontMap->charheight*2) / vp.Height;


		float fRectLeft = 0;
		float fRectTop = 1.0f;

		fRectLeft = fRectLeft * 2.0f - 1.0f;
		fRectTop = fRectTop * 2.0f - 1.0f;

		float fOriginalLeft = fRectLeft;  //0*2.0f-1.0f=-1.0f
		float fTexTop = 0.0f;
		float fTexBottom = 1.0f;


		//fill the FontVB with the sizes and texture coordinates

		D3D11_MAPPED_SUBRESOURCE sr;

		if (SUCCEEDED(dc->Map(pFontVB,0, D3D11_MAP_WRITE_DISCARD, 0, &sr)))
		{
			int i,j;
			SpriteVertex *vertices=(SpriteVertex *)sr.pData;

			for (j=0, i=0; i<strlen; i++)
			{
				if( s[i] == '\n' )
				{
					fRectLeft = fOriginalLeft;
					fRectTop -= fGlyphSizeY;

					continue;
				}
				if ((s[i] < 32) || (s[i] > 127))
					continue;//invalid char


				float fGlyphSizeX = (pFontTexture->DefinedFontMap->charinfo[s[i]-32].charwidth*2) / vp.Width;

				float fCharTexSizeX = pFontTexture->DefinedFontMap->charinfo[s[i]-32].charwidth / (pFontTexture->DefinedFontMap->fullwidth);  //0.010526315f;

				float fRectRight = fRectLeft + fGlyphSizeX;
				float fRectBottom = fRectTop - fGlyphSizeY;
				float fTexLeft = pFontTexture->DefinedFontMap->charinfo[s[i]-32].offset / pFontTexture->DefinedFontMap->fullwidth;				
				float fTexRight = fTexLeft + fCharTexSizeX;
				


				vertices[j*6+0].Pos=XMFLOAT3( fRectLeft, fRectTop, 1.0f );
				vertices[j*6+0].Tex=XMFLOAT2( fTexLeft, fTexTop );
				vertices[j*6+1].Pos=XMFLOAT3( fRectRight, fRectTop, 1.0f );
				vertices[j*6+1].Tex=XMFLOAT2( fTexRight, fTexTop );
				vertices[j*6+2].Pos=XMFLOAT3( fRectLeft, fRectBottom, 1.0f );
				vertices[j*6+2].Tex=XMFLOAT2( fTexLeft, fTexBottom );

				vertices[j*6+3].Pos=XMFLOAT3( fRectRight, fRectTop, 1.0f);
				vertices[j*6+3].Tex=XMFLOAT2( fTexRight, fTexTop  );
				vertices[j*6+4].Pos=XMFLOAT3( fRectRight, fRectBottom,  1.0f);
				vertices[j*6+4].Tex=XMFLOAT2( fTexRight, fTexBottom  );
				vertices[j*6+5].Pos=XMFLOAT3( fRectLeft, fRectBottom,  1.0f);
				vertices[j*6+5].Tex=XMFLOAT2( fTexLeft, fTexBottom );
			
				j++;
				fRectLeft += fGlyphSizeX;
			}




			dc->Unmap(pFontVB, 0);
		}

		

		//then render
		UINT stride = sizeof( SpriteVertex );
		UINT offset = 0;

		dc->IASetVertexBuffers( 0, 1, &pFontVB, &stride, &offset );
		dc->PSSetShaderResources( 0, 1, &pFontTexture->pTexture );

		dc->Draw(6*strlen,0);

		dc->Release();
	}
}



BOOL DXMessD3D11Handler::UpdateTextures()
{
	//call this each time the resolution changes (when the buffer changes)
	HRESULT hr;
	ID3D11Resource *test;
	ID3D11Texture2D *texturex;
	DXGI_SWAP_CHAIN_DESC desc;
	int i;

	int newTextureCount;


	WaitForSingleObject((HANDLE)(shared->TextureLock), INFINITE);
	
	if (shared->textureCount)
	{
		ZeroMemory(&desc, sizeof(desc));
		hr=swapchain->GetDesc(&desc);
		if (FAILED(hr))
			return hr;

		newTextureCount=shared->textureCount;

		if (shared->textureCount > TextureCount)
		{				
			//update the textures if needed
			if (textures==NULL) //initial alloc
				textures=(TextureData11 *)malloc(sizeof(TextureData11)* shared->textureCount);			
			else //realloc
				textures=(TextureData11 *)realloc(textures, sizeof(TextureData11)* shared->textureCount);		

			//initialize the new entries to NULL
			for (i=TextureCount; i<shared->textureCount; i++)
			{
				textures[i].pTexture=NULL;	
				textures[i].DefinedFontMap=NULL;
			}
			
		}
		

		for (i=0; i<newTextureCount; i++)
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

					hr=D3DX11CreateTextureFromMemory(dev, (void *)(tea[i].AddressOfTexture), tea[i].size, NULL, NULL, &test, NULL);
					if( FAILED( hr ) )
					{
						OutputDebugStringA("Failure creating a texture");
						return hr;
					}

					
					hr=test->QueryInterface(__uuidof(ID3D11Texture2D), (void **)(&texturex));	

					
					hr=dev->CreateShaderResourceView(test, NULL, &textures[i].pTexture);
					if( FAILED( hr ) )
						return hr;
				

					test->Release();
					texturex->Release();

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
				//It's NULL (cleanup)
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

		TextureCount=newTextureCount;
		

		
	}

	if (shared->texturelistHasUpdate)
		InterlockedExchange((volatile LONG *)&shared->texturelistHasUpdate,0);		

	SetEvent((HANDLE)(shared->TextureLock));

	return TRUE;	

}

DXMessD3D11Handler::~DXMessD3D11Handler()
{


	if (textures)
	{
		int i;
		for (i=0; i<TextureCount; i++)
		{
			if (textures[i].pTexture)
				textures[i].pTexture->Release();
		}
		free(textures);	
	}

	if (pFontVB)
		pFontVB->Release();

	if (pSpriteVB)
		pSpriteVB->Release();

	if (pPixelShaderNormal)
		pPixelShaderNormal->Release();

	if (pVertexShader)
		pVertexShader->Release();

	if (pVertexLayout)
		pVertexLayout->Release();

	if (pSamplerLinear)
		pSamplerLinear->Release();

	if (pSpriteRasterizer)
		pSpriteRasterizer->Release();

	if (pTransparency)
		pTransparency->Release();

	if (pDepthStencil)
		pDepthStencil->Release();

	if (pRenderTargetView)
		pRenderTargetView->Release();

	if (pDepthStencilView)
		pDepthStencilView->Release();

	if (pConstantBuffer)
		pConstantBuffer->Release();

	if (pWireframeRasterizer)
		pWireframeRasterizer->Release();


	if (pDisabledDepthStencilState)
		pDisabledDepthStencilState->Release();

	/*if (dc)
		dc->Release();*/

	if (dev)
	  dev->Release();

	if (swapchain)
	  swapchain->Release();

	

}

DXMessD3D11Handler::DXMessD3D11Handler(ID3D11Device *dev, IDXGISwapChain *sc, PD3DHookShared shared)
{
	HRESULT hr;
	InitializeCriticalSection(&cs); //initialize the critical section used for snapshots
			
	RenderContext=NULL;

	snapshotCounter=0;
	lastSnapshot=0;
	makeSnapshot=FALSE;
	smallSnapshot=FALSE;

    pPixelShaderNormal=NULL;
	pVertexShader=NULL;
	pVertexLayout=NULL;

	pSamplerLinear=NULL;
	pSpriteRasterizer=NULL;
	pTransparency=NULL;
	pDepthStencil=NULL;
	pRenderTargetView=NULL;
	pDepthStencilView=NULL;
	pConstantBuffer=NULL;	

	pWireframeRasterizer=NULL;
	pDisabledDepthStencilState=NULL;

	pSpriteVB=NULL;
	pFontVB=NULL;
		

	TextureCount=0;
	textures=NULL;

	tea=NULL;



	Valid=FALSE;

	this->shared=shared;
	this->dev=dev;
	this->swapchain=sc;

	dev->AddRef();
	sc->AddRef();

//	dc=NULL;
//	dev->GetImmediateContext(&dc); //increases the reference count

	D3D11_BUFFER_DESC bd2d;
	D3D11_SUBRESOURCE_DATA InitData2d;

	

	
	//create the shaders
    ID3DBlob* pBlob = NULL;
	ID3DBlob* pErrorBlob = NULL;

	char filename[MAX_PATH];
	sprintf_s(filename,MAX_PATH, "%s%s", shared->CheatEngineDir,"overlay.fx");

	//load the "normal" pixel shader
	pBlob = NULL;
	pErrorBlob = NULL;
	hr=D3DX11CompileFromFileA( filename, NULL, NULL, "PSNormal", "ps_4_0", D3DCOMPILE_ENABLE_STRICTNESS, 0, NULL, &pBlob, &pErrorBlob, NULL );

	if (pErrorBlob) 
		pErrorBlob->Release();

	if( FAILED( hr ) )
	{
		OutputDebugStringA("pixelshader compilation failed\n");
		return;
	}

    hr = dev->CreatePixelShader( pBlob->GetBufferPointer(), pBlob->GetBufferSize(), NULL, &pPixelShaderNormal );
	pBlob->Release();
	if( FAILED( hr ) )
	{
		OutputDebugStringA("CreatePixelShader failed\n");
		return;
	}
	



	pBlob = NULL;
	pErrorBlob = NULL;

	hr=D3DX11CompileFromFileA( filename, NULL, NULL, "VS", "vs_4_0", D3DCOMPILE_ENABLE_STRICTNESS, 0, NULL, &pBlob, &pErrorBlob, NULL );

	if (pErrorBlob) 
		pErrorBlob->Release();

	if( FAILED( hr ) )
	{
		OutputDebugStringA("vertexshader compilation failed\n");
		return;
	}

    hr = dev->CreateVertexShader( pBlob->GetBufferPointer(), pBlob->GetBufferSize(), NULL, &pVertexShader );
	
	if( FAILED( hr ) )
	{
		pBlob->Release();
		OutputDebugStringA("CreateVertexShader failed\n");
		return;
	}
	

    D3D11_INPUT_ELEMENT_DESC layout[] =
    {
        { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0, D3D11_INPUT_PER_VERTEX_DATA, 0 },
        { "TEXCOORD", 0, DXGI_FORMAT_R32G32_FLOAT, 0, 12, D3D11_INPUT_PER_VERTEX_DATA, 0 },
    };
    UINT numElements = ARRAYSIZE( layout );

    // Create the input layout
    hr = dev->CreateInputLayout( layout, numElements, pBlob->GetBufferPointer(),
                                          pBlob->GetBufferSize(), &pVertexLayout );

	pBlob->Release();

	//create rectangular vertex buffer for sprites
	SpriteVertex spriteVertices[] ={ 
		{XMFLOAT3( 1.0f,  1.0f, 1.0f), XMFLOAT2( 1.0f, 0.0f ) },
		{XMFLOAT3( 1.0f, -1.0f, 1.0f), XMFLOAT2( 1.0f, 1.0f )},		
		{XMFLOAT3(-1.0f, -1.0f, 1.0f), XMFLOAT2( 0.0f, 1.0f )},
		
		{XMFLOAT3(-1.0f, -1.0f, 1.0f), XMFLOAT2( 0.0f, 1.0f )},
		{XMFLOAT3(-1.0f,  1.0f, 1.0f), XMFLOAT2( 0.0f, 0.0f )},
		{XMFLOAT3( 1.0f,  1.0f, 1.0f), XMFLOAT2( 1.0f, 0.0f )},		
	};

	ZeroMemory( &bd2d, sizeof(bd2d) );
	bd2d.Usage = D3D11_USAGE_IMMUTABLE;
	bd2d.ByteWidth = sizeof( SpriteVertex ) * 6;
	bd2d.BindFlags = D3D11_BIND_VERTEX_BUFFER;
	//bd2d.CPUAccessFlags = D3D11_CPU_ACCESS_WRITE;

	ZeroMemory( &InitData2d, sizeof(InitData2d) );
	InitData2d.pSysMem = spriteVertices;
	hr = dev->CreateBuffer( &bd2d, &InitData2d, &pSpriteVB );
	if( FAILED( hr ) )
	{
		OutputDebugStringA("Vertexbuffer creation failed\n");
		return;
	}


    D3D11_SAMPLER_DESC sampDesc;
    ZeroMemory( &sampDesc, sizeof(sampDesc) );
    sampDesc.Filter = D3D11_FILTER_MIN_MAG_MIP_LINEAR;
    sampDesc.AddressU = D3D11_TEXTURE_ADDRESS_WRAP;
    sampDesc.AddressV = D3D11_TEXTURE_ADDRESS_WRAP;
    sampDesc.AddressW = D3D11_TEXTURE_ADDRESS_WRAP;
    sampDesc.ComparisonFunc = D3D11_COMPARISON_NEVER;
    sampDesc.MinLOD = 0;
    sampDesc.MaxLOD = D3D11_FLOAT32_MAX;
    hr = dev->CreateSamplerState( &sampDesc, &pSamplerLinear );
    if( FAILED( hr ) )
        return;


	//rasterizer
	D3D11_RASTERIZER_DESC rasterizerdesc;
	ZeroMemory(&rasterizerdesc, sizeof(rasterizerdesc));
	
	rasterizerdesc.FillMode = D3D11_FILL_SOLID;
	rasterizerdesc.CullMode = D3D11_CULL_NONE;
	rasterizerdesc.FrontCounterClockwise = false;
	rasterizerdesc.DepthBias = 0;
	rasterizerdesc.DepthBiasClamp = 0.0f;
	rasterizerdesc.DepthClipEnable = false;
	rasterizerdesc.AntialiasedLineEnable = false;
	
	
	rasterizerdesc.MultisampleEnable = false;
	rasterizerdesc.ScissorEnable = false;
	rasterizerdesc.SlopeScaledDepthBias = 0.0f;


	hr=dev->CreateRasterizerState(&rasterizerdesc, &pSpriteRasterizer);
	if( FAILED( hr ) )
        return;

	rasterizerdesc.FillMode = D3D11_FILL_WIREFRAME;
	hr=dev->CreateRasterizerState(&rasterizerdesc, &pWireframeRasterizer);
	if( FAILED( hr ) )
        pWireframeRasterizer=NULL; //no biggie


	D3D11_DEPTH_STENCIL_DESC dsDesc;
	ZeroMemory(&dsDesc, sizeof(dsDesc)); //everything 0, including DepthEnable
	hr= dev->CreateDepthStencilState(&dsDesc, &pDisabledDepthStencilState);
	if( FAILED( hr ) )
		pDisabledDepthStencilState=NULL;


	D3D11_BLEND_DESC blend;
	
	D3D11_RENDER_TARGET_BLEND_DESC rtbd;
	ZeroMemory( &rtbd, sizeof(rtbd) );
	ZeroMemory( &blend, sizeof(blend) );


	rtbd.BlendEnable			 = true;
	rtbd.SrcBlend				 = D3D11_BLEND_SRC_ALPHA;	
	rtbd.DestBlend				 = D3D11_BLEND_INV_SRC_ALPHA;	
	rtbd.BlendOp				 = D3D11_BLEND_OP_ADD;
	rtbd.SrcBlendAlpha			 = D3D11_BLEND_SRC_ALPHA;
	rtbd.DestBlendAlpha			 = D3D11_BLEND_INV_SRC_ALPHA;
	rtbd.BlendOpAlpha			 = D3D11_BLEND_OP_ADD;
	rtbd.RenderTargetWriteMask	 = D3D11_COLOR_WRITE_ENABLE_ALL;

	blend.AlphaToCoverageEnable=false;
	blend.IndependentBlendEnable=false; //true;
	blend.RenderTarget[0]=rtbd;


	hr=dev->CreateBlendState(&blend, &pTransparency);
	if( FAILED( hr ) )
        return;




	//create a rendertarget
    ID3D11Texture2D* pBackBuffer = NULL;
	hr = sc->GetBuffer( 0, __uuidof( ID3D11Texture2D ), ( LPVOID* )&pBackBuffer );
    if( FAILED( hr ) )
        return;

    hr = dev->CreateRenderTargetView( pBackBuffer, NULL, &pRenderTargetView );
    pBackBuffer->Release();

	DXGI_SWAP_CHAIN_DESC scd;
	if (SUCCEEDED(sc->GetDesc(&scd)))
	{
		D3D11_TEXTURE2D_DESC texdesc;
		texdesc.ArraySize=1;
		texdesc.BindFlags=D3D11_BIND_RENDER_TARGET | D3D11_BIND_SHADER_RESOURCE;
		texdesc.CPUAccessFlags=0;
		texdesc.Format=DXGI_FORMAT_R8G8B8A8_UNORM;
		texdesc.Height=scd.BufferDesc.Height;
		texdesc.Width=scd.BufferDesc.Width;
		texdesc.MipLevels=1;
		texdesc.MiscFlags=0;
		texdesc.SampleDesc.Count=1;
		texdesc.SampleDesc.Quality=0;
		texdesc.Usage=D3D11_USAGE_DEFAULT;
		
		if (SUCCEEDED(dev->CreateTexture2D(&texdesc, NULL, &ExtraRenderTargetTexture)))
		{
			ExtraRenderTarget=NULL;
			dev->CreateRenderTargetView(ExtraRenderTargetTexture, NULL,  &ExtraRenderTarget);

		}		
		
	}



    if( FAILED( hr ) )
        return;


	DXGI_SWAP_CHAIN_DESC scdesc;
	ZeroMemory(&scdesc,sizeof(scdesc));
	sc->GetDesc(&scdesc);

	
	// Create depth stencil texture
    D3D11_TEXTURE2D_DESC descDepth;
	ZeroMemory( &descDepth, sizeof(descDepth) );
    descDepth.Width = scdesc.BufferDesc.Width;
    descDepth.Height = scdesc.BufferDesc.Height;
    descDepth.MipLevels = 1;
    descDepth.ArraySize = 1;
    descDepth.Format = DXGI_FORMAT_D24_UNORM_S8_UINT;
    descDepth.SampleDesc.Count = 1;
    descDepth.SampleDesc.Quality = 0;
    descDepth.Usage = D3D11_USAGE_DEFAULT;
    descDepth.BindFlags = D3D11_BIND_DEPTH_STENCIL;
    descDepth.CPUAccessFlags = 0;
    descDepth.MiscFlags = 0;
    hr = dev->CreateTexture2D( &descDepth, NULL, &pDepthStencil );
    if( FAILED( hr ) )
        return;

    // Create the depth stencil view
    D3D11_DEPTH_STENCIL_VIEW_DESC descDSV;
	ZeroMemory( &descDSV, sizeof(descDSV) );
    descDSV.Format = descDepth.Format;
    descDSV.ViewDimension = D3D11_DSV_DIMENSION_TEXTURE2D;
    descDSV.Texture2D.MipSlice = 0;
    hr = dev->CreateDepthStencilView( pDepthStencil, &descDSV, &pDepthStencilView );
    if( FAILED( hr ) )
        return;

	//now load the textures
	tea=(PTextureEntry)((uintptr_t)shared+shared->texturelist);
	UpdateTextures();

	// Create the constant buffer
	D3D11_BUFFER_DESC bd;
	ZeroMemory(&bd, sizeof(bd));
 
	bd.Usage = D3D11_USAGE_DEFAULT;
	bd.ByteWidth = sizeof(ConstantBuffer);
	bd.BindFlags = D3D11_BIND_CONSTANT_BUFFER;
	bd.CPUAccessFlags = 0;
    hr = dev->CreateBuffer( &bd, NULL, &pConstantBuffer );
    if( FAILED( hr ) )
        return;


	//create a vertexbuffer to hold the characters
	currentMaxCharacterCount=0;
	SetupFontVertexBuffer(32); //init to 32 chars



	Valid=TRUE;
}


void DXMessD3D11Handler::RenderOverlay()
{
	int i;
	if (Valid)
	{	
		//render the overlay
		BOOL hasLock=FALSE;
		POINT clientMousepos;


		ID3D11DeviceContext *dc;

		dev->GetImmediateContext(&dc);

		clientMousepos.x=-1;
		clientMousepos.y=-1;

		//check if the overlay has an update
		//if so, first update the texture

		DXGI_SWAP_CHAIN_DESC desc;
		swapchain->GetDesc(&desc);
		shared->lastHwnd=(DWORD)desc.OutputWindow;
	
		if (shared->texturelistHasUpdate)
			UpdateTextures();


		UINT stride = sizeof( SpriteVertex );
		UINT offset = 0;
		float blendFactor[] = {0.1f, 1.0f, 1.0f, 0.1f};

		ID3D11VertexShader *oldvs=NULL;
		ID3D11ClassInstance **oldvsinstances=NULL;
		UINT vci_count=0;

		ID3D11PixelShader *oldps=NULL;
		ID3D11ClassInstance **oldpsinstances=NULL;
		UINT pci_count=0;

		ID3D11GeometryShader *oldgs=NULL;
		ID3D11ClassInstance **oldgsinstances=NULL;
		UINT gci_count=0;

		ID3D11SamplerState *oldPSSampler=NULL;
		ID3D11ShaderResourceView *oldPSShaderResource=NULL;
		ID3D11BlendState *oldBlendState=NULL;
		float oldblendFactor[4];
		UINT oldblendsamplemask;

		ID3D11DepthStencilState *oldDepthStencilState=NULL;
		UINT oldstencilref;

		D3D11_PRIMITIVE_TOPOLOGY oldPrimitiveTopology;

		ID3D11InputLayout *oldInputLayout=NULL;
		ID3D11Buffer *oldIndexBuffer=NULL;
		DXGI_FORMAT oldIndexBufferFormat;
		UINT oldIndexBufferOffset;

		ID3D11Buffer *oldVertexBuffer=NULL;
		UINT oldVertexBufferStrides;
		UINT oldVertexBufferOffset;

		ID3D11RasterizerState *oldRastersizerState=NULL;

		ID3D11RenderTargetView *oldRenderTarget;
		ID3D11DepthStencilView *oldDepthStencilView=NULL;


		ID3D11Buffer *oldConstantBuffersVS=NULL;
		ID3D11Buffer *oldConstantBuffersPS=NULL;

		//save state		

		dc->VSGetConstantBuffers(0,1, &oldConstantBuffersVS);
		dc->VSGetShader( &oldvs, NULL, &vci_count);
		if (vci_count)
		{
			if (oldvs)
				oldvs->Release();

			oldvsinstances=new ID3D11ClassInstance*[vci_count];
			dc->VSGetShader( &oldvs, oldvsinstances, &vci_count);
		}

		dc->GSGetShader( &oldgs, NULL, &gci_count);
		if (gci_count)
		{
			if (oldgs)
				oldgs->Release();

			oldgsinstances=new ID3D11ClassInstance*[gci_count];
			dc->GSGetShader( &oldgs, oldgsinstances, &gci_count);
		}
		
		dc->PSGetConstantBuffers(0,1, &oldConstantBuffersPS);
		dc->PSGetShader( &oldps, NULL, &pci_count);
		if (pci_count)
		{
			if (oldps)
				oldps->Release();
			oldpsinstances=new ID3D11ClassInstance*[pci_count];
			dc->PSGetShader( &oldps, oldpsinstances, &pci_count);
		}

		dc->PSGetSamplers(0,1, &oldPSSampler);
		dc->PSGetShaderResources(0,1, &oldPSShaderResource);
		
		

		dc->OMGetRenderTargets(1, &oldRenderTarget, &oldDepthStencilView);
		dc->OMGetBlendState( &oldBlendState, oldblendFactor, &oldblendsamplemask);
		dc->OMGetDepthStencilState( &oldDepthStencilState, &oldstencilref);
		

		dc->IAGetPrimitiveTopology(&oldPrimitiveTopology);
		dc->IAGetInputLayout(&oldInputLayout);
		dc->IAGetIndexBuffer( &oldIndexBuffer, &oldIndexBufferFormat, &oldIndexBufferOffset);
		dc->IAGetVertexBuffers(0,1,&oldVertexBuffer, &oldVertexBufferStrides, &oldVertexBufferOffset); //I just need to save 1

		dc->RSGetState(&oldRastersizerState);

		UINT oldviewports=0;
		D3D11_VIEWPORT viewports[D3D11_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE];
		dc->RSGetViewports(&oldviewports, NULL);
		dc->RSGetViewports(&oldviewports, viewports);


		//change state

		dc->GSSetShader(NULL,NULL,0);
	    dc->VSSetShader( pVertexShader, NULL, 0 );
		
		dc->PSSetSamplers( 0, 1, &pSamplerLinear );
		
		D3D11_VIEWPORT vp;
		vp.Width = (float)desc.BufferDesc.Width;
		vp.Height = (float)desc.BufferDesc.Height;
		vp.MinDepth = 0.0f;
		vp.MaxDepth = 1.0f;
		vp.TopLeftX = 0;
		vp.TopLeftY = 0;
		dc->RSSetViewports( 1, &vp );
		 

		
		dc->OMSetRenderTargetsAndUnorderedAccessViews(0,NULL,NULL,0,0,NULL,NULL);
		dc->OMSetRenderTargets(1, &pRenderTargetView, pDepthStencilView);		
		dc->ClearDepthStencilView( pDepthStencilView, D3D11_CLEAR_DEPTH, 1.0f, 0 );

		dc->OMSetBlendState(pTransparency, blendFactor, 0xffffffff);
		dc->OMSetDepthStencilState(pDisabledDepthStencilState,0);
		

		

		dc->IASetPrimitiveTopology( D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST );
		dc->IASetInputLayout( pVertexLayout );
		dc->IASetIndexBuffer( NULL, DXGI_FORMAT_R16_UINT, 0 );
		

		dc->RSSetState(pSpriteRasterizer);

		if (shared->UseCommandlistLock)
			hasLock=WaitForSingleObject((HANDLE)shared->CommandlistLock, INFINITE)==WAIT_OBJECT_0;
		



	
		i=0;
		while (shared->RenderCommands[i].Command)
		{
			switch (shared->RenderCommands[i].Command)
			{
				case rcDrawSprite:
				{
					if (shared->RenderCommands[i].sprite.textureid<TextureCount)
					{
					
						XMFLOAT3 position;				

						dc->IASetVertexBuffers( 0, 1, &pSpriteVB, &stride, &offset );
						dc->PSSetShader( pPixelShaderNormal, NULL, 0);

						dc->PSSetSamplers( 0, 1, &pSamplerLinear );						
						dc->PSSetShaderResources( 0, 1, &textures[shared->RenderCommands[i].sprite.textureid].pTexture );

						if (shared->RenderCommands[i].x==-1) //center	
						{
							position.x=((float)vp.Width / 2.0f) - ((float)shared->RenderCommands[i].sprite.width / 2.0f);						
						}
						else
						if (shared->RenderCommands[i].x==-2) //mouse
						{
							if (clientMousepos.x==-1)
							{
								//get the mouse position
								GetCursorPos(&clientMousepos);
								ScreenToClient((HWND)shared->lastHwnd, &clientMousepos);	
							}

							position.x=(float)clientMousepos.x-((float)shared->RenderCommands[i].sprite.width / 2.0f); 
						}
						else
							position.x=(float)shared->RenderCommands[i].x;


						if (shared->RenderCommands[i].y==-1) //center
						{
							position.y=((float)vp.Height / 2.0f) - ((float)shared->RenderCommands[i].sprite.height / 2.0f);
						}
						else
						if (shared->RenderCommands[i].y==-2) //mouse
						{
							if (clientMousepos.y==-1)
							{
								//get the mouse position
								GetCursorPos(&clientMousepos);
								ScreenToClient((HWND)shared->lastHwnd, &clientMousepos);	
							}
							position.y=(float)clientMousepos.y-((float)shared->RenderCommands[i].sprite.height / 2.0f);	

						}
						else
							position.y=(float)shared->RenderCommands[i].y;

						//truncate the position to an exact pixel
						position.x=(float)(int)position.x;
						position.y=(float)(int)position.y;
		
			
						ConstantBuffer cb;					
						cb.transparency=shared->RenderCommands[i].alphablend;

						if (shared->RenderCommands[i].sprite.width==-1)
							cb.scaling.x=(float)vp.Width/(float)vp.Width;
						else
							cb.scaling.x=(float)shared->RenderCommands[i].sprite.width/(float)vp.Width;
						cb.scaling.y=(float)shared->RenderCommands[i].sprite.height/(float)vp.Height;

				

						cb.originpoint.x=-1.0f+((float)shared->RenderCommands[i].centerX * 2)/(float)shared->RenderCommands[i].sprite.width;
						cb.originpoint.y=-1.0f+((float)shared->RenderCommands[i].centerY * 2)/(float)shared->RenderCommands[i].sprite.height;

						cb.rotation=XMMatrixRotationZ(shared->RenderCommands[i].rotation);


						
	
						cb.translation.x=-1.0f+((float)(((float)position.x-(float)shared->RenderCommands[i].centerX) * 2)/(float)vp.Width);
						cb.translation.y=-1.0f+((float)(((float)position.y-(float)shared->RenderCommands[i].centerY) * 2)/(float)vp.Height);

												
						
						dc->UpdateSubresource( pConstantBuffer, 0, NULL, &cb, 0, 0 );

						dc->VSSetConstantBuffers(0,1, &pConstantBuffer);
						dc->PSSetConstantBuffers(0,1, &pConstantBuffer);


						
						
						dc->Draw(6,0);
					
					}
				
					break;
				}

				case rcDrawFont:
				{
					int tid=shared->RenderCommands[i].font.fontid;						

					if ((tid<TextureCount) && (textures[tid].pTexture))
					{
						XMFLOAT3 position;	
						PTextureData11 td;
						char *s;

						if (!hasLock)
							hasLock=WaitForSingleObject((HANDLE)shared->CommandlistLock, INFINITE)==WAIT_OBJECT_0; //fonts demand a lock  (stringpointer)

						position.x=(float)shared->RenderCommands[i].x;
						position.y=(float)shared->RenderCommands[i].y;		

						td=&textures[tid];
						s=(char *)shared->RenderCommands[i].font.addressoftext;

						if (position.x==-1) 
						{
							//horizontal center
							//calculate the width
							float maxwidth=0, width=0;
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
						//truncate the position to an exact pixel
						position.x=(float)(int)position.x;
						position.y=(float)(int)position.y;

					
						dc->PSSetShader( pPixelShaderNormal, NULL, 0);
						dc->PSSetSamplers( 0, 1, &pSamplerLinear );						
						

						ConstantBuffer cb;					
						cb.transparency=shared->RenderCommands[i].alphablend;						
						cb.scaling.x=1.0f;
						cb.scaling.y=1.0f;//if you wish a bigger font, use a bigger font, don't scale (ugly)
						
						cb.originpoint.x=-1.0f;
						cb.originpoint.y=-1.0f;						

						cb.rotation=XMMatrixRotationZ(shared->RenderCommands[i].rotation);

						cb.translation.x=-1.0f+((float)((float)position.x * 2)/(float)vp.Width);
						cb.translation.y=-1.0f+((float)((float)position.y * 2)/(float)vp.Height);


						dc->UpdateSubresource( pConstantBuffer, 0, NULL, &cb, 0, 0 );

						dc->VSSetConstantBuffers(0,1, &pConstantBuffer);
						dc->PSSetConstantBuffers(0,1, &pConstantBuffer);

						DrawString(vp, &textures[tid], s,strlen(s));
					}
					
					break;
				}
			}

			i++;				
		}

		if (hasLock) //release the lock if it was obtained
			SetEvent((HANDLE)shared->CommandlistLock);

		//restore
		dc->GSSetShader(oldgs, oldgsinstances, gci_count);
		if (oldgs)
			oldgs->Release();

		if (oldgsinstances)
		{
			for (UINT j=0; j<gci_count; j++)
				oldgsinstances[j]->Release();
			delete [] oldgsinstances;
		}

		dc->VSSetShader(oldvs, oldvsinstances, vci_count);
		if (oldvs)
			oldvs->Release();

		if (oldvsinstances)
		{
			for (UINT j=0; j<gci_count; j++)
				oldvsinstances[j]->Release();
			delete [] oldvsinstances;
		}

		dc->PSSetShader(oldps, oldpsinstances, pci_count);
		if (oldps)
			oldps->Release();

		if (oldpsinstances)
		{
			for (UINT j=0; j<pci_count; j++)
				oldpsinstances[j]->Release();
			delete [] oldpsinstances;
		}

		dc->PSSetSamplers(0, 1, &oldPSSampler);
		if (oldPSSampler)
			oldPSSampler->Release();


		dc->PSSetShaderResources(0,1, &oldPSShaderResource);
		if (oldPSShaderResource)
			oldPSShaderResource->Release();

		dc->VSSetConstantBuffers(0,1, &oldConstantBuffersVS);
		if (oldConstantBuffersVS)
			oldConstantBuffersVS->Release();		

		dc->PSSetConstantBuffers(0,1, &oldConstantBuffersPS);
		if (oldConstantBuffersPS)
			oldConstantBuffersPS->Release();


		dc->OMSetRenderTargets(1, &oldRenderTarget, oldDepthStencilView);
		if (oldRenderTarget)
			oldRenderTarget->Release();

		if (oldDepthStencilView)
			oldDepthStencilView->Release();

		dc->OMSetBlendState(oldBlendState, oldblendFactor, oldblendsamplemask);
		if (oldBlendState)
			oldBlendState->Release();


		dc->OMSetDepthStencilState(oldDepthStencilState, oldstencilref);
		if (oldDepthStencilState)
			oldDepthStencilState->Release();

		dc->IASetPrimitiveTopology(oldPrimitiveTopology);
		dc->IASetInputLayout(oldInputLayout);
		if (oldInputLayout)
			oldInputLayout->Release();

		dc->IASetIndexBuffer(oldIndexBuffer, oldIndexBufferFormat, oldIndexBufferOffset);
		if (oldIndexBuffer)
			oldIndexBuffer->Release();
		
		dc->IASetVertexBuffers(0,1,&oldVertexBuffer, &oldVertexBufferStrides, &oldVertexBufferOffset);
		if (oldVertexBuffer)
			oldVertexBuffer->Release();


		dc->RSSetState(oldRastersizerState);
		if (oldRastersizerState)
			oldRastersizerState->Release();

		dc->RSSetViewports(oldviewports, viewports);

		

		//check if the overlay texture needs to be updated, if so, getdc, update, continue
	}

}

void __stdcall D3D11Hook_SwapChain_ResizeBuffers_imp(IDXGISwapChain *swapchain, ID3D11Device *device, PD3DHookShared s)
{
	DXMessD3D11Handler *currentDevice=D3D11devices[device];
	if (currentDevice)
	{
		D3D11devices[device]=NULL;
		lastdevice=NULL;

		//currentDevice->dc->ClearState();
		delete(currentDevice);
	}
}


void __stdcall D3D11Hook_SwapChain_Present_imp(IDXGISwapChain *swapchain, ID3D11Device *device, PD3DHookShared s)
{
	//look up the controller class for this device

	DXMessD3D11Handler *currenthandler=D3D11devices[device];
	


	if (currenthandler==NULL)
	{
		//OutputDebugStringA("New\n");
		//D3D11devices[dev]=1;

		currenthandler=new DXMessD3D11Handler(device, swapchain, s);//create a new devicehandler

		//add to the map
		D3D11devices[device]=currenthandler;
		shared=s;

	}

	insidehook=1;
	D3D11devices[device]->RenderOverlay();			
	insidehook=0;

	lastdevice=currenthandler;

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
			ID3D11DeviceContext *dc;
			ID3D11RenderTargetView *rt=NULL;
			device->GetImmediateContext(&dc);
			makeSnapshot=TRUE; //once true, always true

			
			dc->OMGetRenderTargets(1, &rt, NULL);


			//currenthandler->dc->OMSetRenderTargets(1, &currenthandler->ExtraRenderTarget, NULL);



			//clear the render target with a specific color
			if (rt)
			{
				FLOAT f[]={1.0f,0.0f,1.0f,1.0f};
				dc->ClearRenderTargetView(currenthandler->ExtraRenderTarget, f);	
				
				rt->Release();
			}

			currenthandler->snapshotCounter=0;
			

		}
	}

}


HRESULT __stdcall D3D11Hook_DrawIndexed_imp(D3D11_DRAWINDEXED_ORIGINAL originalfunction, ID3D11DeviceContext *dc, UINT IndexCount, UINT StartIndexLocation, INT BaseVertexLocation)	
{	
	if (((shared) && ((shared->wireframe) || (shared->disabledzbuffer) || (makeSnapshot) ) && (insidehook==0)))
	{
		//setup for wireframe and/or zbuffer
		HRESULT hr;
		ID3D11Device *device=NULL;
		dc->GetDevice(&device);		
		
		DXMessD3D11Handler *currentDevice=D3D11devices[device];		

		if (currentDevice==NULL) //this can happen in some situations when there is a layer inbetween
			currentDevice=lastdevice;			
	

		if (currentDevice)
		{
			ID3D11DeviceContext *drawdc;
			ID3D11DepthStencilState *oldDepthStencilState;
			ID3D11RasterizerState *oldRasterizerState;
			UINT stencilref=0;


			dc->OMGetDepthStencilState(&oldDepthStencilState,&stencilref);
			dc->RSGetState(&oldRasterizerState);

			if (shared->wireframe)
				dc->RSSetState(currentDevice->pWireframeRasterizer);

			if (shared->disabledzbuffer)
				dc->OMSetDepthStencilState(currentDevice->pDisabledDepthStencilState, 0);

			if (currentDevice->makeSnapshot)
				drawdc=currentDevice->PrepareForSnapshot(dc);
			else
				drawdc=dc;

			hr=originalfunction(drawdc, IndexCount, StartIndexLocation, BaseVertexLocation);
						
			if (currentDevice->makeSnapshot)
				currentDevice->TakeSnapshot(drawdc, "D3D11Hook_DrawIndexed_imp");
			
			dc->RSSetState(oldRasterizerState);
			dc->OMSetDepthStencilState(oldDepthStencilState, stencilref);

			if (oldRasterizerState)
				oldRasterizerState->Release();

			if (oldDepthStencilState)
				oldDepthStencilState->Release();



			return hr;
			
		}
		

	}
	return originalfunction(dc, IndexCount, StartIndexLocation, BaseVertexLocation);
}


HRESULT __stdcall D3D11Hook_Draw_imp(D3D11_DRAW_ORIGINAL originalfunction, ID3D11DeviceContext *dc, UINT VertexCount, UINT StartVertexLocation)
{	
	if (((shared) && ((shared->wireframe) || (shared->disabledzbuffer) || (makeSnapshot) ) && (insidehook==0)))
	{
		//setup for wireframe and/or zbuffer
		HRESULT hr;
		ID3D11Device *device=NULL;
		dc->GetDevice(&device);		
		
		DXMessD3D11Handler *currentDevice=D3D11devices[device];		

		device->Release();



		if (currentDevice==NULL) //this can happen in some situations when there is a layer inbetween
			currentDevice=lastdevice;			


		if (currentDevice)
		{
			ID3D11DeviceContext *drawdc;
			ID3D11DepthStencilState *oldDepthStencilState;
			ID3D11RasterizerState *oldRasterizerState;
			UINT stencilref=0;


			dc->OMGetDepthStencilState(&oldDepthStencilState,&stencilref);
			dc->RSGetState(&oldRasterizerState);

			if (shared->wireframe)
				dc->RSSetState(currentDevice->pWireframeRasterizer);

			if (shared->disabledzbuffer)
				dc->OMSetDepthStencilState(currentDevice->pDisabledDepthStencilState, 0);;


			if (currentDevice->makeSnapshot)
				drawdc=currentDevice->PrepareForSnapshot(dc);
			else
				drawdc=dc;

			hr=originalfunction(drawdc, VertexCount, StartVertexLocation);

			if (currentDevice->makeSnapshot)
				currentDevice->TakeSnapshot(drawdc, "D3D11Hook_Draw_imp");
			
			dc->RSSetState(oldRasterizerState);
			dc->OMSetDepthStencilState(oldDepthStencilState, stencilref);

			if (oldRasterizerState)
				oldRasterizerState->Release();

			if (oldDepthStencilState)
				oldDepthStencilState->Release();


			return hr;
		}		
	}
	return originalfunction(dc, VertexCount, StartVertexLocation);
}

HRESULT __stdcall D3D11Hook_DrawIndexedInstanced_imp(D3D11_DRAWINDEXEDINSTANCED_ORIGINAL originalfunction, ID3D11DeviceContext *dc, UINT IndexCountPerInstance, UINT InstanceCount, UINT StartIndexLocation, INT BaseVertexLocation, UINT StartInstanceLocation)
{	
	if (((shared) && ((shared->wireframe) || (shared->disabledzbuffer) || (makeSnapshot) ) && (insidehook==0)))
	{
		//setup for wireframe and/or zbuffer
		HRESULT hr;
		ID3D11DeviceContext *drawdc;
		ID3D11Device *device=NULL;
		dc->GetDevice(&device);		
		
		DXMessD3D11Handler *currentDevice=D3D11devices[device];		
		device->Release();

		if (currentDevice==NULL) //this can happen in some situations when there is a layer inbetween
			currentDevice=lastdevice;	

		if (currentDevice)
		{
			ID3D11DepthStencilState *oldDepthStencilState;
			ID3D11RasterizerState *oldRasterizerState;
			UINT stencilref=0;


			dc->OMGetDepthStencilState(&oldDepthStencilState,&stencilref);
			dc->RSGetState(&oldRasterizerState);

			if (shared->wireframe)
				dc->RSSetState(currentDevice->pWireframeRasterizer);

			if (shared->disabledzbuffer)
				dc->OMSetDepthStencilState(currentDevice->pDisabledDepthStencilState, 0);;


			if (currentDevice->makeSnapshot)
				drawdc=currentDevice->PrepareForSnapshot(dc);
			else
				drawdc=dc;

			hr=originalfunction(drawdc, IndexCountPerInstance, InstanceCount, StartIndexLocation, BaseVertexLocation, StartInstanceLocation);

			if (currentDevice->makeSnapshot)
				currentDevice->TakeSnapshot(drawdc, "D3D11Hook_DrawIndexedInstanced_imp");
			
			dc->RSSetState(oldRasterizerState);
			dc->OMSetDepthStencilState(oldDepthStencilState, stencilref);


			if (oldRasterizerState)
				oldRasterizerState->Release();

			if (oldDepthStencilState)
				oldDepthStencilState->Release();


			return hr;
		}		
	}
	return originalfunction(dc, IndexCountPerInstance, InstanceCount, StartIndexLocation, BaseVertexLocation, StartInstanceLocation);
}

HRESULT __stdcall D3D11Hook_DrawInstanced_imp(D3D11_DRAWINSTANCED_ORIGINAL originalfunction, ID3D11DeviceContext *dc, UINT VertexCountPerInstance, UINT InstanceCount, UINT StartVertexLocation, UINT StartInstanceLocation)
{	
	if (((shared) && ((shared->wireframe) || (shared->disabledzbuffer) || (makeSnapshot) ) && (insidehook==0)))
	{
		//setup for wireframe and/or zbuffer
		HRESULT hr;
		ID3D11DeviceContext *drawdc;
		ID3D11Device *device=NULL;
		dc->GetDevice(&device);		
		
		DXMessD3D11Handler *currentDevice=D3D11devices[device];		
		device->Release();

		if (currentDevice==NULL) //this can happen in some situations when there is a layer inbetween
			currentDevice=lastdevice;	

		if (currentDevice)
		{
			ID3D11DepthStencilState *oldDepthStencilState;
			ID3D11RasterizerState *oldRasterizerState;
			UINT stencilref=0;

			dc->OMGetDepthStencilState(&oldDepthStencilState, &stencilref);
			dc->RSGetState(&oldRasterizerState);

			if (shared->wireframe)
				dc->RSSetState(currentDevice->pWireframeRasterizer);

			if (shared->disabledzbuffer)
				dc->OMSetDepthStencilState(currentDevice->pDisabledDepthStencilState, 0);;


			if (currentDevice->makeSnapshot)
				drawdc=currentDevice->PrepareForSnapshot(dc);
			else
				drawdc=dc;

			hr=originalfunction(drawdc, VertexCountPerInstance, InstanceCount, StartVertexLocation, StartInstanceLocation);

			if (currentDevice->makeSnapshot)
				currentDevice->TakeSnapshot(drawdc, "D3D11Hook_DrawInstanced_imp");
			
			dc->RSSetState(oldRasterizerState);
			dc->OMSetDepthStencilState(oldDepthStencilState, stencilref);

			if (oldRasterizerState)
				oldRasterizerState->Release();

			if (oldDepthStencilState)
				oldDepthStencilState->Release();



			return hr;
		}		
	}
	return originalfunction(dc, VertexCountPerInstance, InstanceCount, StartVertexLocation, StartInstanceLocation);
}

HRESULT __stdcall D3D11Hook_DrawAuto_imp(D3D11_DRAWAUTO_ORIGINAL originalfunction, ID3D11DeviceContext *dc)
{	
	if (((shared) && ((shared->wireframe) || (shared->disabledzbuffer) || (makeSnapshot) ) && (insidehook==0)))
	{
		//setup for wireframe and/or zbuffer
		HRESULT hr;
		ID3D11DeviceContext *drawdc;
		ID3D11Device *device=NULL;
		dc->GetDevice(&device);		
		
		DXMessD3D11Handler *currentDevice=D3D11devices[device];		
		device->Release();

		if (currentDevice==NULL) //this can happen in some situations when there is a layer inbetween
			currentDevice=lastdevice;	

		if (currentDevice)
		{
			ID3D11DepthStencilState *oldDepthStencilState;
			ID3D11RasterizerState *oldRasterizerState;
			UINT stencilref=0;

			dc->OMGetDepthStencilState(&oldDepthStencilState, &stencilref);
			dc->RSGetState(&oldRasterizerState);

			if (shared->wireframe)
				dc->RSSetState(currentDevice->pWireframeRasterizer);

			if (shared->disabledzbuffer)
				dc->OMSetDepthStencilState(currentDevice->pDisabledDepthStencilState, 0);;

			if (currentDevice->makeSnapshot)
				drawdc=currentDevice->PrepareForSnapshot(dc);
			else
				drawdc=dc;

			hr=originalfunction(drawdc);

			if (currentDevice->makeSnapshot)
				currentDevice->TakeSnapshot(drawdc, "D3D11Hook_DrawAuto_imp");


			
			dc->RSSetState(oldRasterizerState);
			dc->OMSetDepthStencilState(oldDepthStencilState, stencilref);

			if (oldRasterizerState)
				oldRasterizerState->Release();

			if (oldDepthStencilState)
				oldDepthStencilState->Release();



			return hr;
		}		
	}

	return originalfunction(dc);
}
