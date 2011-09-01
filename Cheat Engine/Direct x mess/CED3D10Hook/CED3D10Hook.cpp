// CED3D10Hook.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"

using namespace std;
map<ID3D10Device *, DXMessD3D10Handler *> D3D10devices;

//definitions
struct OverlayVertex{
    XMFLOAT3 Pos;
    XMFLOAT2 Tex;
};



HRESULT DXMessD3D10Handler::setupOverlayTexture()
//hmm, I could update it to a x,y,width,height, winhandle method now...
{
	//call this each time the resolution changes (when the buffer changes)
	HRESULT hr;
	ID3D10Resource *test;
	ID3D10Texture2D *texturex;
	D3D10_TEXTURE2D_DESC tdesc;
	DXGI_SWAP_CHAIN_DESC desc;
	int i;

	if (shared->overlaycount==0)
		return S_OK;

	ZeroMemory(&desc, sizeof(desc));
	hr=swapchain->GetDesc(&desc);
	if (FAILED(hr))
		return hr;


	

	
	if (shared->overlaycount > OverlayCount)
	{
		int newcount=shared->overlaycount;
	

		if (overlays==NULL) //initial alloc
		{
			
			overlays=(OverlayData10 *)malloc(sizeof(OverlayData10)* newcount);			
		}
		else
		{
			//realloc
			overlays=(OverlayData10 *)realloc(overlays, sizeof(OverlayData10)* newcount);			
		}

		

		//initialize the new entries to NULL
		for (i=OverlayCount; i<shared->overlaycount; i++)
		{
			overlays[i].pOverlayTex=NULL;
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
				//(Re)create the texture
				if (overlays[i].pOverlayTex)
				{
					if (overlays[i].pOverlayTex->Release()==0)
						overlays[i].pOverlayTex=NULL; //should always happen
				}

				hr=D3DX10CreateTextureFromMemory(dev, (void *)(uintptr_t(shared)+shared->resources[i].resourceoffset), shared->resources[i].resourcesize, NULL, NULL, &test, NULL);
				if( FAILED( hr ) )
				{
					OutputDebugStringA("Failure creating a texture");
					return hr;
				}

				//not sure if this is needed or if I can just pass the texturex as resource for resourceview
				hr=test->QueryInterface(__uuidof(ID3D10Texture2D), (void **)(&texturex));			

				hr=dev->CreateShaderResourceView(test, NULL, &overlays[i].pOverlayTex);
				if( FAILED( hr ) )
					return hr;

				test->Release();
				texturex->Release();

				shared->resources[i].updatedresource=0;

			}

			if ((shared->resources[i].updatedpos) || (overlays[i].pOverlayVB==NULL))
			{
				if (overlays[i].pOverlayVB)
				{
					if (overlays[i].pOverlayVB->Release()==0)
						overlays[i].pOverlayVB=NULL;
				}

				//create the vertexbuffer or edit. (I think recreating is easier....)
				float left=shared->resources[i].x;
				float top=shared->resources[i].y;
				float right=shared->resources[i].x+shared->resources[i].width;
				float bottom=shared->resources[i].y+shared->resources[i].height;

				//convert to -1 / +1 regions

				left=(left / desc.BufferDesc.Width ) * 2 -1;
				top=-((top / desc.BufferDesc.Height ) * 2 -1);

				right=(right / desc.BufferDesc.Width) * 2-1;
				bottom=-((bottom / desc.BufferDesc.Height) * 2-1);

				OverlayVertex overlayVertices[] ={ //x,y,z, x,y
					{XMFLOAT3(left, top, 0.0f), XMFLOAT2( 0.0f, 0.0f ) },
					{XMFLOAT3(left, bottom, 0.0f), XMFLOAT2( 0.0f, 1.0f )},		
					{XMFLOAT3(right, bottom, 0.0f), XMFLOAT2( 1.0f, 1.0f )},
					
					{XMFLOAT3(right, bottom, 0.0f), XMFLOAT2( 1.0f, 1.0f )},
					{XMFLOAT3(right, top, 0.0f), XMFLOAT2( 1.0f, 0.0f )},
					{XMFLOAT3(left, top, 0.0f), XMFLOAT2( 0.0f, 0.0f )},		
				};

				D3D10_BUFFER_DESC bd2d;
				ZeroMemory( &bd2d, sizeof(bd2d) );
				bd2d.Usage = D3D10_USAGE_DYNAMIC;
				bd2d.ByteWidth = sizeof( OverlayVertex ) * 6;
				bd2d.BindFlags = D3D10_BIND_VERTEX_BUFFER;
				bd2d.CPUAccessFlags = D3D10_CPU_ACCESS_WRITE;
			   
				D3D10_SUBRESOURCE_DATA InitData2d;
				ZeroMemory( &InitData2d, sizeof(InitData2d) );
				InitData2d.pSysMem = overlayVertices;
				hr = dev->CreateBuffer( &bd2d, &InitData2d, &overlays[i].pOverlayVB );
				if( FAILED( hr ) )
				{
					OutputDebugStringA("Vertexbuffer creation failed\n");
					return hr;
				}

				shared->resources[i].updatedpos=0;

			}
		}
	}


	shared->OverLayHasUpdate=0;
	return hr;

}

DXMessD3D10Handler::~DXMessD3D10Handler()
{
	dev->Release();	
}

DXMessD3D10Handler::DXMessD3D10Handler(ID3D10Device *dev, IDXGISwapChain *sc, PD3DHookShared shared)
{
	HRESULT hr;
	int i;


	//hr=D3DX10CreateSprite(dev, 0, &sprite);

	pOverlayIB=NULL;


	pPixelShader=NULL;
	pVertexShader=NULL;
	pVertexLayout=NULL;

	pSamplerLinear=NULL;
	pOverlayRasterizer=NULL;
	pTransparency=NULL;

	OverlayCount=0;
	overlays=NULL;



	Valid=FALSE;

	this->shared=shared;
	this->dev=dev;
	this->swapchain=sc;

	dev->AddRef();
	sc->AddRef();

	D3D10_BUFFER_DESC bd2d;
	D3D10_SUBRESOURCE_DATA InitData2d;

	
    WORD overlayIndices[] =
    {
        0,1,2,
		3,4,5,
    };

	ZeroMemory( &bd2d, sizeof(bd2d) );

    bd2d.Usage = D3D10_USAGE_DEFAULT;
    bd2d.ByteWidth = sizeof( WORD ) * 6;
    bd2d.BindFlags = D3D10_BIND_INDEX_BUFFER;
    bd2d.CPUAccessFlags = 0;
    InitData2d.pSysMem = overlayIndices;
    hr = dev->CreateBuffer( &bd2d, &InitData2d, &pOverlayIB );

    if( FAILED( hr ) )
	{
		OutputDebugStringA("Indexbuffer creation failed\n");
		return;
	}

	

    ID3DBlob* pBlob = NULL;
	ID3DBlob* pErrorBlob = NULL;

	hr=D3DX10CompileFromFileA( "c:\\overlay.fx", NULL, NULL, "PS", "ps_4_0", D3DCOMPILE_ENABLE_STRICTNESS, 0, NULL, &pBlob, &pErrorBlob, NULL );

	if (pErrorBlob) 
		pErrorBlob->Release();

	if( FAILED( hr ) )
	{
		OutputDebugStringA("pixelshader compilation failed\n");
		return;
	}

    hr = dev->CreatePixelShader( pBlob->GetBufferPointer(), pBlob->GetBufferSize(), &pPixelShader );
	pBlob->Release();
	if( FAILED( hr ) )
	{
		OutputDebugStringA("CreatePixelShader failed\n");
		return;
	}

	pBlob = NULL;
	pErrorBlob = NULL;

	hr=D3DX10CompileFromFileA( "c:\\overlay.fx", NULL, NULL, "VS", "vs_4_0", D3DCOMPILE_ENABLE_STRICTNESS, 0, NULL, &pBlob, &pErrorBlob, NULL );

	if (pErrorBlob) 
		pErrorBlob->Release();

	if( FAILED( hr ) )
	{
		OutputDebugStringA("vertexshader compilation failed\n");
		return;
	}

    hr = dev->CreateVertexShader( pBlob->GetBufferPointer(), pBlob->GetBufferSize(), &pVertexShader );
	
	if( FAILED( hr ) )
	{
		pBlob->Release();
		OutputDebugStringA("CreatePixelShader failed\n");
		return;
	}
	

    D3D10_INPUT_ELEMENT_DESC layout[] =
    {
        { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0, D3D10_INPUT_PER_VERTEX_DATA, 0 },
        { "TEXCOORD", 0, DXGI_FORMAT_R32G32_FLOAT, 0, 12, D3D10_INPUT_PER_VERTEX_DATA, 0 },
    };
    UINT numElements = ARRAYSIZE( layout );

    // Create the input layout
    hr = dev->CreateInputLayout( layout, numElements, pBlob->GetBufferPointer(),
                                          pBlob->GetBufferSize(), &pVertexLayout );

	pBlob->Release();



    D3D10_SAMPLER_DESC sampDesc;
    ZeroMemory( &sampDesc, sizeof(sampDesc) );
    sampDesc.Filter = D3D10_FILTER_MIN_MAG_MIP_LINEAR;
    sampDesc.AddressU = D3D10_TEXTURE_ADDRESS_WRAP;
    sampDesc.AddressV = D3D10_TEXTURE_ADDRESS_WRAP;
    sampDesc.AddressW = D3D10_TEXTURE_ADDRESS_WRAP;
    sampDesc.ComparisonFunc = D3D10_COMPARISON_NEVER;
    sampDesc.MinLOD = 0;
    sampDesc.MaxLOD = D3D10_FLOAT32_MAX;
    hr = dev->CreateSamplerState( &sampDesc, &pSamplerLinear );
    if( FAILED( hr ) )
        return;


	//rasterizer
	D3D10_RASTERIZER_DESC rasterizerdesc;
	ZeroMemory(&rasterizerdesc, sizeof(rasterizerdesc));
	
	rasterizerdesc.FillMode = D3D10_FILL_SOLID;
	rasterizerdesc.CullMode = D3D10_CULL_NONE;
	rasterizerdesc.FrontCounterClockwise = false;
	rasterizerdesc.DepthBias = 0;
	rasterizerdesc.DepthBiasClamp = 0.0f;
	rasterizerdesc.DepthClipEnable = true;
	rasterizerdesc.AntialiasedLineEnable = false;
	
	
	rasterizerdesc.MultisampleEnable = false;
	rasterizerdesc.ScissorEnable = false;
	rasterizerdesc.SlopeScaledDepthBias = 0.0f;


	hr=dev->CreateRasterizerState(&rasterizerdesc, &pOverlayRasterizer);
	if( FAILED( hr ) )
        return;


	D3D10_BLEND_DESC blend;	
	ZeroMemory( &blend, sizeof(blend) );

	blend.BlendEnable[0]		 = true;
	blend.SrcBlend				 = D3D10_BLEND_ONE;
	blend.DestBlend				 = D3D10_BLEND_INV_SRC_ALPHA;
	blend.BlendOp				 = D3D10_BLEND_OP_ADD;
	blend.SrcBlendAlpha			 = D3D10_BLEND_ONE;
	blend.DestBlendAlpha		 = D3D10_BLEND_ZERO;
	blend.BlendOpAlpha			 = D3D10_BLEND_OP_ADD;

	blend.AlphaToCoverageEnable=true;

	for (i=0; i<8; i++)
		blend.RenderTargetWriteMask[i]=D3D10_COLOR_WRITE_ENABLE_ALL;

	pTransparency=NULL;
	hr=dev->CreateBlendState(&blend, &pTransparency);
	if( FAILED( hr ) )
        return;



	//now create the texture of the overlay
	hr=setupOverlayTexture();

	if( FAILED( hr ) )
		return;

	


	Valid=TRUE;
}


void DXMessD3D10Handler::RenderOverlay()
{
	int i;
	HRESULT hr;
	if (Valid)
	{
		

		//render the overlay

		//check if the overlay has an update
		//if so, first update the texture


		if (shared->OverLayHasUpdate)
			setupOverlayTexture();


		UINT stride = sizeof( OverlayVertex );
		UINT offset = 0;
		float blendFactor[] = {1.0f, 1.0f, 1.0f, 1.0f};

		ID3D10VertexShader *oldvs=NULL;
		//ID3D10ClassInstance *oldvsinstances=NULL;
		//UINT vci_count=0;

		ID3D10PixelShader *oldps=NULL;
		//ID3D10ClassInstance *oldpsinstances=NULL;
		//UINT pci_count=0;

		ID3D10SamplerState *oldPSSampler=NULL;
		ID3D10ShaderResourceView *oldPSShaderResource=NULL;
		ID3D10BlendState *oldBlendState=NULL;
		float oldblendFactor[4];
		UINT oldblendsamplemask;

		ID3D10DepthStencilState *oldDepthStencilState=NULL;
		UINT oldstencilref;

		D3D10_PRIMITIVE_TOPOLOGY oldPrimitiveTopology;

		ID3D10InputLayout *oldInputLayout=NULL;
		ID3D10Buffer *oldIndexBuffer=NULL;
		DXGI_FORMAT oldIndexBufferFormat;
		UINT oldIndexBufferOffset;

		ID3D10Buffer *oldVertexBuffer=NULL;
		UINT oldVertexBufferStrides;
		UINT oldVertexBufferOffset;

		ID3D10RasterizerState *oldRastersizerState=NULL;

		//save state

		
		dev->VSGetShader( &oldvs);
		dev->PSGetShader( &oldps);
		dev->PSGetSamplers(0,1, &oldPSSampler);
		dev->PSGetShaderResources(0,1, &oldPSShaderResource);

		dev->OMGetBlendState( &oldBlendState, oldblendFactor, &oldblendsamplemask);
		dev->OMGetDepthStencilState( &oldDepthStencilState, &oldstencilref);

		dev->IAGetPrimitiveTopology(&oldPrimitiveTopology);
		dev->IAGetInputLayout(&oldInputLayout);
		dev->IAGetIndexBuffer( &oldIndexBuffer, &oldIndexBufferFormat, &oldIndexBufferOffset);
		dev->IAGetVertexBuffers(0,1,&oldVertexBuffer, &oldVertexBufferStrides, &oldVertexBufferOffset);

		dev->RSGetState(&oldRastersizerState);

		//change state

	    dev->VSSetShader(pVertexShader);
		dev->PSSetShader(pPixelShader);
		dev->PSSetSamplers( 0, 1, &pSamplerLinear );
		

		dev->OMSetBlendState(pTransparency, blendFactor, 0xffffffff);
		dev->OMSetDepthStencilState(NULL,0);

		dev->IASetPrimitiveTopology( D3D10_PRIMITIVE_TOPOLOGY_TRIANGLELIST );
		dev->IASetInputLayout( pVertexLayout );
		dev->IASetIndexBuffer( pOverlayIB, DXGI_FORMAT_R16_UINT, 0 );
		

		dev->RSSetState(pOverlayRasterizer);

		
		for (i=0; i<OverlayCount; i++)
		{
			if (shared->resources[i].valid)
			{			
				//set the vertexbuffer and texture and render
				dev->IASetVertexBuffers( 0, 1, &overlays[i].pOverlayVB, &stride, &offset );
				dev->PSSetShaderResources( 0, 1, &overlays[i].pOverlayTex );	
	
				//render
				dev->DrawIndexed( 6, 0,0);

				//dev->VSSetShader(NULL);

				D3DXMATRIX m,m2;
				DXGI_SWAP_CHAIN_DESC scdesc;
				
				swapchain->GetDesc(&scdesc);			
				D3DXMatrixOrthoOffCenterLH(&m2, 0, (float)scdesc.BufferDesc.Width, (float)scdesc.BufferDesc.Height, 0, 0, 1);
				//sprite->SetProjectionTransform(&m2);

				//sprite->Begin(D3DX10_SPRITE_SAVE_STATE);

				D3DX10_SPRITE s;

				
				ZeroMemory(&s,sizeof(s));				
				D3DXMatrixTransformation2D(&s.matWorld, &D3DXVECTOR2(0, 0), 0, &D3DXVECTOR2(shared->resources[i].width, shared->resources[i].height), &D3DXVECTOR2(0, 0), 0, &D3DXVECTOR2(shared->resources[i].width / 2.0f+shared->resources[i].x, shared->resources[i].height / 2.0f+shared->resources[i].y));


			//	

				


				s.TexCoord=D3DXVECTOR2( 0, 0 );
				s.TexSize=D3DXVECTOR2( 0.5f, 0.5f );
				s.ColorModulate=0xffffffff;
				s.pTexture=overlays[i].pOverlayTex;
				s.TextureIndex=0;

				
				dev->RSSetState(pOverlayRasterizer);

				//sprite->DrawSpritesImmediate(&s, 1, 0,0);
				
				//sprite->End();

			}
		}

		//restore
		dev->VSSetShader(oldvs);
		dev->PSSetShader(oldps);
		dev->PSSetSamplers(0, 1, &oldPSSampler);
		dev->PSSetShaderResources(0,1, &oldPSShaderResource);

		dev->OMSetBlendState(oldBlendState, oldblendFactor, oldblendsamplemask);
		dev->OMSetDepthStencilState(oldDepthStencilState, oldstencilref);

		dev->IASetPrimitiveTopology(oldPrimitiveTopology);
		dev->IASetInputLayout(oldInputLayout);
		dev->IASetIndexBuffer(oldIndexBuffer, oldIndexBufferFormat, oldIndexBufferOffset);
		dev->IASetVertexBuffers(0,1,&oldVertexBuffer, &oldVertexBufferStrides, &oldVertexBufferOffset);

		dev->RSSetState(oldRastersizerState);
	}

}


void __stdcall D3D10Hook_SwapChain_Present_imp(IDXGISwapChain *swapchain, ID3D10Device *device, PD3DHookShared shared)
{
	//look up the controller class for this device
	if (D3D10devices[device]==NULL)
	{
		DXMessD3D10Handler *dc=new DXMessD3D10Handler(device, swapchain, shared);//create a new devicehandler
		D3D10devices[device]=dc;
	}
	D3D10devices[device]->RenderOverlay();	

}