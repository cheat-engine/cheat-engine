// CED3D10Hook.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"

using namespace std;
map<ID3D10Device *, DXMessD3D10Handler *> D3D10devices;

PD3DHookShared shared=NULL;
int insidehook=0;

DXMessD3D10Handler *lastdevice;


//definitions
struct OverlayVertex{
    XMFLOAT3 Pos;
    XMFLOAT2 Tex;
};

struct ConstantBuffer
{	
	XMFLOAT2 translation;	
	FLOAT transparency;	
	FLOAT garbage; //16 byte alignment crap
};

void DXMessD3D10Handler::UpdatePosForOverlay(int i, DXGI_SWAP_CHAIN_DESC *desc)
//pre: i must be valid
{

	if ((shared->resources[i].x==-1) && (shared->resources[i].y==-1))
	{
		//center of screen
		float newx,newy;
		newx=((float)desc->BufferDesc.Width / 2.0f) - ((float)shared->resources[i].width / 2.0f);
		newy=((float)desc->BufferDesc.Height / 2.0f) - ((float)shared->resources[i].height / 2.0f);

		overlays[i].x=(float)((float)newx / (float)desc->BufferDesc.Width) *2.0f;
		overlays[i].y=-(float)((float)newy / (float)desc->BufferDesc.Height) *2.0f;


	}
	else
	{
		overlays[i].x=(float)((float)shared->resources[i].x / (float)desc->BufferDesc.Width) *2.0f;
		overlays[i].y=-(float)((float)shared->resources[i].y / (float)desc->BufferDesc.Height) *2.0f;
	}
	shared->resources[i].updatedpos=0;
}

HRESULT DXMessD3D10Handler::setupOverlayTexture()
//hmm, I could update it to a x,y,width,height, winhandle method now...
{
	//call this each time the resolution changes (when the buffer changes)
	HRESULT hr;
	ID3D10Resource *test;
	ID3D10Texture2D *texturex;
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

			if ((shared->resources[i].updatedresource) || (overlays[i].pOverlayTex==NULL) || (overlays[i].pOverlayVB==NULL))
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

				//create the vertex buffer with the proper width/height
				float right, bottom;

				right=-1.0f+((float)shared->resources[i].width / (float)desc.BufferDesc.Width)*2.0f;
				bottom=1.0f-((float)shared->resources[i].height / (float)desc.BufferDesc.Height)*2.0f;

				OverlayVertex overlayVertices[] ={ //x,y,z, x,y
					{XMFLOAT3(-1.0f, 1.0f, 0.0f), XMFLOAT2( 0.0f, 0.0f ) },
					{XMFLOAT3(-1.0f, bottom, 0.0f), XMFLOAT2( 0.0f, 1.0f )},		
					{XMFLOAT3(right, bottom, 0.0f), XMFLOAT2( 1.0f, 1.0f )},
					
					{XMFLOAT3(right, bottom, 0.0f), XMFLOAT2( 1.0f, 1.0f )},
					{XMFLOAT3(right, 1.0f, 0.0f), XMFLOAT2( 1.0f, 0.0f )},
					{XMFLOAT3(-1.0f, 1.0f, 0.0f), XMFLOAT2( 0.0f, 0.0f )},		
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

				shared->resources[i].updatedresource=0;

			}

			if ((shared->resources[i].updatedpos) || (overlays[i].pOverlayVB==NULL))
				UpdatePosForOverlay(i, &desc);

		}
	}


	shared->OverLayHasUpdate=0;
	return hr;

}

DXMessD3D10Handler::~DXMessD3D10Handler()
{
	int i;
	OutputDebugStringA("DXMessD3D10Handler destructor");

	i=0;


	if (pOverlayIB)
		pOverlayIB->Release();

	if (overlays)
		free(overlays);	

	if (pPixelShader)
		pPixelShader->Release();

	if (pVertexShader)
		pVertexShader->Release();

	if (pVertexLayout)
		pVertexLayout->Release();

	if (pSamplerLinear)
		pSamplerLinear->Release();

	if (pOverlayRasterizer)
		pOverlayRasterizer->Release();

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

	if (dev)
	  dev->Release();

	if (swapchain)
	  swapchain->Release();

	
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
	pDepthStencil=NULL;
	pRenderTargetView=NULL;
	pDepthStencilView=NULL;
	pConstantBuffer=NULL;

	pWireframeRasterizer=NULL;
	pDisabledDepthStencilState=NULL;

	OverlayCount=0;
	overlays=NULL;



	Valid=FALSE;

	


	this->shared=shared;

	this->dev=NULL;
	this->swapchain=NULL;


	


	this->dev=dev;
	this->dev->AddRef();
	this->swapchain=sc;	
	this->swapchain->AddRef();

	
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

	char shaderfile[MAX_PATH];
	sprintf_s(shaderfile,MAX_PATH, "%s%s", shared->CheatEngineDir,"overlay.fx");



	hr=D3DX10CompileFromFileA( shaderfile, NULL, NULL, "PS", "ps_4_0", D3DCOMPILE_ENABLE_STRICTNESS, 0, NULL, &pBlob, &pErrorBlob, NULL );

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

	hr=D3DX10CompileFromFileA( shaderfile, NULL, NULL, "VS", "vs_4_0", D3DCOMPILE_ENABLE_STRICTNESS, 0, NULL, &pBlob, &pErrorBlob, NULL );

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
	rasterizerdesc.DepthClipEnable = false;
	rasterizerdesc.AntialiasedLineEnable = false;
	
	
	rasterizerdesc.MultisampleEnable = false;
	rasterizerdesc.ScissorEnable = false;
	rasterizerdesc.SlopeScaledDepthBias = 0.0f;


	hr=dev->CreateRasterizerState(&rasterizerdesc, &pOverlayRasterizer);
	if( FAILED( hr ) )
        return;

	rasterizerdesc.FillMode = D3D10_FILL_WIREFRAME;
	hr=dev->CreateRasterizerState(&rasterizerdesc, &pWireframeRasterizer);
	if( FAILED( hr ) )
        pWireframeRasterizer=NULL; //no biggie

	D3D10_DEPTH_STENCIL_DESC dsDesc;
	ZeroMemory(&dsDesc, sizeof(dsDesc)); //everything 0, including DepthEnable
	hr= dev->CreateDepthStencilState(&dsDesc, &pDisabledDepthStencilState);
	if( FAILED( hr ) )
		pDisabledDepthStencilState=NULL;


	D3D10_BLEND_DESC blend;	
	ZeroMemory( &blend, sizeof(blend) );

	
	
	blend.BlendEnable[0]		 = true;
	blend.SrcBlend				 = D3D10_BLEND_SRC_ALPHA;	
	blend.DestBlend				 = D3D10_BLEND_INV_SRC_ALPHA;	
	blend.BlendOp				 = D3D10_BLEND_OP_ADD;
	blend.SrcBlendAlpha			 = D3D10_BLEND_ZERO;
	blend.DestBlendAlpha		 = D3D10_BLEND_ZERO;
	blend.BlendOpAlpha			 = D3D10_BLEND_OP_ADD;
	blend.RenderTargetWriteMask[0]	 = D3D10_COLOR_WRITE_ENABLE_ALL;


	blend.AlphaToCoverageEnable=false;
	

	for (i=0; i<8; i++)
		blend.RenderTargetWriteMask[i]=D3D10_COLOR_WRITE_ENABLE_ALL;
	
	


	pTransparency=NULL;
	hr=dev->CreateBlendState(&blend, &pTransparency);
	if( FAILED( hr ) )
        return;


	//create a rendertarget
    ID3D10Texture2D* pBackBuffer = NULL;
	hr = sc->GetBuffer( 0, __uuidof( ID3D10Texture2D ), ( LPVOID* )&pBackBuffer );
    if( FAILED( hr ) )
        return;

    hr = dev->CreateRenderTargetView( pBackBuffer, NULL, &pRenderTargetView );
    pBackBuffer->Release();

    if( FAILED( hr ) )
        return;


	DXGI_SWAP_CHAIN_DESC scdesc;
	ZeroMemory(&scdesc,sizeof(scdesc));
	sc->GetDesc(&scdesc);

	
	// Create depth stencil texture
    D3D10_TEXTURE2D_DESC descDepth;
	ZeroMemory( &descDepth, sizeof(descDepth) );
    descDepth.Width = scdesc.BufferDesc.Width;
    descDepth.Height = scdesc.BufferDesc.Height;
    descDepth.MipLevels = 1;
    descDepth.ArraySize = 1;
    descDepth.Format = DXGI_FORMAT_D24_UNORM_S8_UINT;
    descDepth.SampleDesc.Count = 1;
    descDepth.SampleDesc.Quality = 0;
    descDepth.Usage = D3D10_USAGE_DEFAULT;
    descDepth.BindFlags = D3D10_BIND_DEPTH_STENCIL;
    descDepth.CPUAccessFlags = 0;
    descDepth.MiscFlags = 0;
    hr = dev->CreateTexture2D( &descDepth, NULL, &pDepthStencil );
    if( FAILED( hr ) )
        return;

    // Create the depth stencil view
    D3D10_DEPTH_STENCIL_VIEW_DESC descDSV;
	ZeroMemory( &descDSV, sizeof(descDSV) );
    descDSV.Format = descDepth.Format;
    descDSV.ViewDimension = D3D10_DSV_DIMENSION_TEXTURE2D;
    descDSV.Texture2D.MipSlice = 0;
    hr = dev->CreateDepthStencilView( pDepthStencil, &descDSV, &pDepthStencilView );
    if( FAILED( hr ) )
        return;


	//now create the texture of the overlay
	hr=setupOverlayTexture();

	if( FAILED( hr ) )
		return;

	
	// Create the constant buffer
	D3D10_BUFFER_DESC bd;
	ZeroMemory(&bd, sizeof(bd));
 
	bd.Usage = D3D10_USAGE_DEFAULT;
	bd.ByteWidth = sizeof(ConstantBuffer);
	bd.BindFlags = D3D10_BIND_CONSTANT_BUFFER;
	bd.CPUAccessFlags = 0;
    hr = dev->CreateBuffer( &bd, NULL, &pConstantBuffer );
    if( FAILED( hr ) )
        return;



	Valid=TRUE;
}


void DXMessD3D10Handler::RenderOverlay()
{





	int i;
	if (Valid)
	{

		//render the overlay

		//check if the overlay has an update
		//if so, first update the texture

		DXGI_SWAP_CHAIN_DESC desc;
		swapchain->GetDesc(&desc);

			

		shared->lastHwnd=(DWORD)desc.OutputWindow;


		if ((shared->MouseOverlayId>=0) && (OverlayCount>=shared->MouseOverlayId) && (shared->resources[shared->MouseOverlayId].valid))
		{
			//update the mouse position each frame for as long as the mouse is valid
			POINT p;

			p.x=0;
			p.y=0;

			GetCursorPos(&p);

			ScreenToClient(desc.OutputWindow, &p);				
			
			shared->resources[shared->MouseOverlayId].x=p.x;
			shared->resources[shared->MouseOverlayId].y=p.y;			

			UpdatePosForOverlay(shared->MouseOverlayId, &desc);	
		}

		

		if (shared->OverLayHasUpdate)
			setupOverlayTexture();

		

		UINT stride = sizeof( OverlayVertex );
		UINT offset = 0;
		float blendFactor[] = {0.0f, 0.0f, 0.0f, 0.0f};

		ID3D10VertexShader *oldvs=NULL;
		ID3D10PixelShader *oldps=NULL;
		ID3D10GeometryShader *oldgs=NULL;

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
		ID3D10RenderTargetView *oldRenderTarget;
		ID3D10DepthStencilView *oldDepthStencilView=NULL;

		ID3D10Buffer *oldConstantBuffersVS=NULL;
		ID3D10Buffer *oldConstantBuffersPS=NULL;

		D3D10_VIEWPORT vp;
		UINT oldviewports=0;
		D3D10_VIEWPORT viewports[D3D10_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE];

		//save state
		
		dev->VSGetShader( &oldvs);
		

		dev->VSGetConstantBuffers(0,1, &oldConstantBuffersVS);
		

		dev->GSGetShader(&oldgs);

		dev->PSGetShader( &oldps);
		dev->PSGetSamplers(0,1, &oldPSSampler);
		dev->PSGetShaderResources(0,1, &oldPSShaderResource);
		dev->PSGetConstantBuffers(0,1, &oldConstantBuffersPS);

		
		

		dev->OMGetRenderTargets(1, &oldRenderTarget, &oldDepthStencilView);
		dev->OMGetBlendState( &oldBlendState, oldblendFactor, &oldblendsamplemask);
		dev->OMGetDepthStencilState( &oldDepthStencilState, &oldstencilref);
		
	

		
		dev->IAGetPrimitiveTopology(&oldPrimitiveTopology);
		dev->IAGetInputLayout(&oldInputLayout);
		dev->IAGetIndexBuffer( &oldIndexBuffer, &oldIndexBufferFormat, &oldIndexBufferOffset);
		dev->IAGetVertexBuffers(0,1,&oldVertexBuffer, &oldVertexBufferStrides, &oldVertexBufferOffset);

		

		dev->RSGetState(&oldRastersizerState);

		
		
		dev->RSGetViewports(&oldviewports, NULL);
		dev->RSGetViewports(&oldviewports, viewports);

		

	//	if (0)
		{

		//change state
		dev->GSSetShader(NULL); //not used
	    dev->VSSetShader(pVertexShader);
		dev->PSSetShader(pPixelShader);
		dev->PSSetSamplers( 0, 1, &pSamplerLinear );



		

		
		vp.Width = desc.BufferDesc.Width;
		vp.Height = desc.BufferDesc.Height;
		vp.MinDepth = 0.0f;
		vp.MaxDepth = 1.0f;
		vp.TopLeftX = 0;
		vp.TopLeftY = 0;
		dev->RSSetViewports( 1, &vp );
		

		dev->OMSetRenderTargets(1, &pRenderTargetView, NULL); //pDepthStencilView);		
		dev->ClearDepthStencilView( pDepthStencilView, D3D10_CLEAR_DEPTH, 1.0f, 0 );

		dev->OMSetBlendState(pTransparency, blendFactor, 0xffffffff);
		dev->OMSetDepthStencilState(pDisabledDepthStencilState, 0);;

		

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
	
				ConstantBuffer cb;
				UpdatePosForOverlay(i, &desc);
				cb.transparency=shared->resources[i].alphaBlend;

				cb.translation.x=overlays[i].x;
				cb.translation.y=overlays[i].y;

				dev->UpdateSubresource( pConstantBuffer, 0, NULL, &cb, 0, 0 );

				dev->VSSetConstantBuffers(0,1, &pConstantBuffer);
				dev->PSSetConstantBuffers(0,1, &pConstantBuffer);


				

				//render
				dev->DrawIndexed( 6, 0,0);

				/*

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
				
				//sprite->End();*/

			}
		}

		}

		//restore
		dev->GSSetShader(oldgs);
		if (oldgs)
			oldgs->Release();

		dev->VSSetShader(oldvs);
		if (oldvs)
			oldvs->Release();

		dev->PSSetShader(oldps);
		if (oldps)
			oldps->Release();

		dev->PSSetSamplers(0, 1, &oldPSSampler);
		if (oldPSSampler)
			oldPSSampler->Release();
			
		dev->PSSetShaderResources(0,1, &oldPSShaderResource);
		if (oldPSShaderResource)
			oldPSShaderResource->Release();

		dev->VSSetConstantBuffers(0,1, &oldConstantBuffersVS);
		if (oldConstantBuffersVS)
			oldConstantBuffersVS->Release();


		dev->PSSetConstantBuffers(0,1, &oldConstantBuffersPS);
		if (oldConstantBuffersPS)
			oldConstantBuffersPS->Release();

		dev->OMSetRenderTargets(1, &oldRenderTarget, oldDepthStencilView);
		if (oldRenderTarget)
			oldRenderTarget->Release();

		if (oldDepthStencilView)
			oldDepthStencilView->Release();

		dev->OMSetBlendState(oldBlendState, oldblendFactor, oldblendsamplemask);
		if (oldBlendState)
			oldBlendState->Release();

		dev->OMSetDepthStencilState(oldDepthStencilState, oldstencilref);
		if (oldDepthStencilState)
			oldDepthStencilState->Release();

		
		dev->IASetPrimitiveTopology(oldPrimitiveTopology);
		dev->IASetInputLayout(oldInputLayout);
		if (oldInputLayout)
			oldInputLayout->Release();

		dev->IASetIndexBuffer(oldIndexBuffer, oldIndexBufferFormat, oldIndexBufferOffset);
		if (oldIndexBuffer)
			oldIndexBuffer->Release();

		dev->IASetVertexBuffers(0,1,&oldVertexBuffer, &oldVertexBufferStrides, &oldVertexBufferOffset);
		if (oldVertexBuffer)
			oldVertexBuffer->Release();

		dev->RSSetState(oldRastersizerState);
		if (oldRastersizerState)
			oldRastersizerState->Release();

		dev->RSSetViewports(oldviewports, viewports);
	}

}

void __stdcall D3D10Hook_SwapChain_ResizeBuffers_imp(IDXGISwapChain *swapchain, ID3D10Device *device, PD3DHookShared s)
{
	//release all buffers
	DXMessD3D10Handler *currentDevice=D3D10devices[device];
	if (currentDevice)
	{
		D3D10devices[device]=NULL;

		device->ClearState();
		delete(currentDevice);
	}

	

}

void __stdcall D3D10Hook_SwapChain_Present_imp(IDXGISwapChain *swapchain, ID3D10Device *device, PD3DHookShared s)
{
	//look up the controller class for this device
	DXMessD3D10Handler *currentDevice=D3D10devices[device];

		

	
	if (currentDevice==NULL)
	{
		currentDevice=new DXMessD3D10Handler(device, swapchain, s);//create a new devicehandler
		D3D10devices[device]=currentDevice;
		shared=s;
	}
	insidehook=1; //tell the draw hooks not to mess with the following draw operations

	lastdevice=currentDevice;

	currentDevice->RenderOverlay();	
	insidehook=0;

}



HRESULT __stdcall D3D10Hook_DrawIndexed_imp(D3D10_DRAWINDEXED_ORIGINAL originalfunction, ID3D10Device *device, UINT IndexCount, UINT StartIndexLocation, INT BaseVertexLocation)	
{	
	if ((shared) && ((shared->wireframe) || (shared->disabledzbuffer) ) && (insidehook==0) )
	{
		//setup for wireframe and/or zbuffer
		HRESULT hr;
		DXMessD3D10Handler *currentDevice=D3D10devices[device];

		if (currentDevice==NULL) //this can happen in some situations when there is a layer inbetween
			currentDevice=lastdevice;			
	

		if (currentDevice)
		{
			ID3D10DepthStencilState *oldDepthStencilState;
			ID3D10RasterizerState *oldRasterizerState;

			currentDevice->dev->OMGetDepthStencilState(&oldDepthStencilState,0);
			currentDevice->dev->RSGetState(&oldRasterizerState);

			if (shared->wireframe)
				currentDevice->dev->RSSetState(currentDevice->pWireframeRasterizer);

			if (shared->disabledzbuffer)
				currentDevice->dev->OMSetDepthStencilState(currentDevice->pDisabledDepthStencilState, 0);;


			hr=originalfunction(device, IndexCount, StartIndexLocation, BaseVertexLocation);
			
			currentDevice->dev->RSSetState(oldRasterizerState);
			currentDevice->dev->OMSetDepthStencilState(oldDepthStencilState, 0);

			return hr;
		}		
	}
	return originalfunction(device, IndexCount, StartIndexLocation, BaseVertexLocation);
}


HRESULT __stdcall D3D10Hook_Draw_imp(D3D10_DRAW_ORIGINAL originalfunction, ID3D10Device *device, UINT VertexCount, UINT StartVertexLocation)
{	
	if ((shared) && ((shared->wireframe) || (shared->disabledzbuffer) ) && (insidehook==0) )
	{
		//setup for wireframe and/or zbuffer
		HRESULT hr;
		DXMessD3D10Handler *currentDevice=D3D10devices[device];

		if (currentDevice==NULL) //this can happen in some situations when there is a layer inbetween
			currentDevice=lastdevice;			


		if (currentDevice)
		{
			ID3D10DepthStencilState *oldDepthStencilState;
			ID3D10RasterizerState *oldRasterizerState;

			currentDevice->dev->OMGetDepthStencilState(&oldDepthStencilState,0);
			currentDevice->dev->RSGetState(&oldRasterizerState);

			if (shared->wireframe)
				currentDevice->dev->RSSetState(currentDevice->pWireframeRasterizer);

			if (shared->disabledzbuffer)
				currentDevice->dev->OMSetDepthStencilState(currentDevice->pDisabledDepthStencilState, 0);;


			hr=originalfunction(device, VertexCount, StartVertexLocation);
			
			currentDevice->dev->RSSetState(oldRasterizerState);
			currentDevice->dev->OMSetDepthStencilState(oldDepthStencilState, 0);

			return hr;
		}		
	}
	return originalfunction(device, VertexCount, StartVertexLocation);
}

HRESULT __stdcall D3D10Hook_DrawIndexedInstanced_imp(D3D10_DRAWINDEXEDINSTANCED_ORIGINAL originalfunction, ID3D10Device *device, UINT IndexCountPerInstance, UINT InstanceCount, UINT StartIndexLocation, INT BaseVertexLocation, UINT StartInstanceLocation)
{	
	if ((shared) && ((shared->wireframe) || (shared->disabledzbuffer) ) && (insidehook==0) )
	{
		//setup for wireframe and/or zbuffer
		HRESULT hr;
		DXMessD3D10Handler *currentDevice=D3D10devices[device];

		if (currentDevice==NULL) //this can happen in some situations when there is a layer inbetween
			currentDevice=lastdevice;	

		if (currentDevice)
		{
			ID3D10DepthStencilState *oldDepthStencilState;
			ID3D10RasterizerState *oldRasterizerState;

			currentDevice->dev->OMGetDepthStencilState(&oldDepthStencilState,0);
			currentDevice->dev->RSGetState(&oldRasterizerState);

			if (shared->wireframe)
				currentDevice->dev->RSSetState(currentDevice->pWireframeRasterizer);

			if (shared->disabledzbuffer)
				currentDevice->dev->OMSetDepthStencilState(currentDevice->pDisabledDepthStencilState, 0);;


			hr=originalfunction(device, IndexCountPerInstance, InstanceCount, StartIndexLocation, BaseVertexLocation, StartInstanceLocation);
			
			currentDevice->dev->RSSetState(oldRasterizerState);
			currentDevice->dev->OMSetDepthStencilState(oldDepthStencilState, 0);

			return hr;
		}		
	}
	return originalfunction(device, IndexCountPerInstance, InstanceCount, StartIndexLocation, BaseVertexLocation, StartInstanceLocation);
}

HRESULT __stdcall D3D10Hook_DrawInstanced_imp(D3D10_DRAWINSTANCED_ORIGINAL originalfunction, ID3D10Device *device, UINT VertexCountPerInstance, UINT InstanceCount, UINT StartVertexLocation, UINT StartInstanceLocation)
{	
	if ((shared) && ((shared->wireframe) || (shared->disabledzbuffer) ) && (insidehook==0) )
	{
		//setup for wireframe and/or zbuffer
		HRESULT hr;
		DXMessD3D10Handler *currentDevice=D3D10devices[device];

		if (currentDevice==NULL) //this can happen in some situations when there is a layer inbetween
			currentDevice=lastdevice;	

		if (currentDevice)
		{
			ID3D10DepthStencilState *oldDepthStencilState;
			ID3D10RasterizerState *oldRasterizerState;

			currentDevice->dev->OMGetDepthStencilState(&oldDepthStencilState,0);
			currentDevice->dev->RSGetState(&oldRasterizerState);

			if (shared->wireframe)
				currentDevice->dev->RSSetState(currentDevice->pWireframeRasterizer);

			if (shared->disabledzbuffer)
				currentDevice->dev->OMSetDepthStencilState(currentDevice->pDisabledDepthStencilState, 0);;


			hr=originalfunction(device, VertexCountPerInstance, InstanceCount, StartVertexLocation, StartInstanceLocation);
			
			currentDevice->dev->RSSetState(oldRasterizerState);
			currentDevice->dev->OMSetDepthStencilState(oldDepthStencilState, 0);

			return hr;
		}		
	}
	return originalfunction(device, VertexCountPerInstance, InstanceCount, StartVertexLocation, StartInstanceLocation);
}

HRESULT __stdcall D3D10Hook_DrawAuto_imp(D3D10_DRAWAUTO_ORIGINAL originalfunction, ID3D10Device *device)
{	
	if ((shared) && ((shared->wireframe) || (shared->disabledzbuffer) ) && (insidehook==0) )
	{
		//setup for wireframe and/or zbuffer
		HRESULT hr;
		DXMessD3D10Handler *currentDevice=D3D10devices[device];

		if (currentDevice==NULL) //this can happen in some situations when there is a layer inbetween
			currentDevice=lastdevice;	

		if (currentDevice)
		{
			ID3D10DepthStencilState *oldDepthStencilState;
			ID3D10RasterizerState *oldRasterizerState;

			currentDevice->dev->OMGetDepthStencilState(&oldDepthStencilState,0);
			currentDevice->dev->RSGetState(&oldRasterizerState);

			if (shared->wireframe)
				currentDevice->dev->RSSetState(currentDevice->pWireframeRasterizer);

			if (shared->disabledzbuffer)
				currentDevice->dev->OMSetDepthStencilState(currentDevice->pDisabledDepthStencilState, 0);;


			hr=originalfunction(device);
			
			currentDevice->dev->RSSetState(oldRasterizerState);
			currentDevice->dev->OMSetDepthStencilState(oldDepthStencilState, 0);

			return hr;
		}		
	}
	HRESULT br;
	br=originalfunction(device);
	return br;
}
