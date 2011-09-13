// CED3D11Hook.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"

using namespace std;
map<ID3D11Device *, DXMessD3D11Handler *> D3D11devices;

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

void DXMessD3D11Handler::UpdatePosForOverlay(int i, DXGI_SWAP_CHAIN_DESC *desc)
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

HRESULT DXMessD3D11Handler::setupOverlayTexture()
//hmm, I could update it to a x,y,width,height, winhandle method now...
{
	//call this each time the resolution changes (when the buffer changes)
	HRESULT hr;
	ID3D11Resource *test;
	ID3D11Texture2D *texturex;
	D3D11_TEXTURE2D_DESC tdesc;
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
			
			overlays=(OverlayData *)malloc(sizeof(OverlayData)* newcount);			
		}
		else
		{
			//realloc
			overlays=(OverlayData *)realloc(overlays, sizeof(OverlayData)* newcount);			
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

				hr=D3DX11CreateTextureFromMemory(dev, (void *)(uintptr_t(shared)+shared->resources[i].resourceoffset), shared->resources[i].resourcesize, NULL, NULL, &test, NULL);
				if( FAILED( hr ) )
				{
					OutputDebugStringA("Failure creating a texture");
					return hr;
				}

				//not sure if this is needed or if I can just pass the texturex as resource for resourceview
				hr=test->QueryInterface(__uuidof(ID3D11Texture2D), (void **)(&texturex));			

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

				D3D11_BUFFER_DESC bd2d;
				ZeroMemory( &bd2d, sizeof(bd2d) );
				bd2d.Usage = D3D11_USAGE_DYNAMIC;
				bd2d.ByteWidth = sizeof( OverlayVertex ) * 6;
				bd2d.BindFlags = D3D11_BIND_VERTEX_BUFFER;
				bd2d.CPUAccessFlags = D3D11_CPU_ACCESS_WRITE;
			   
				D3D11_SUBRESOURCE_DATA InitData2d;
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

			if (shared->resources[i].updatedpos)
				UpdatePosForOverlay(i, &desc);

		}
	}


	shared->OverLayHasUpdate=0;
	return hr;

}

DXMessD3D11Handler::~DXMessD3D11Handler()
{
	dev->Release();	
}

DXMessD3D11Handler::DXMessD3D11Handler(ID3D11Device *dev, IDXGISwapChain *sc, PD3DHookShared shared)
{
	HRESULT hr;

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

	D3D11_BUFFER_DESC bd2d;
	D3D11_SUBRESOURCE_DATA InitData2d;

	
    WORD overlayIndices[] =
    {
        0,1,2,
		3,4,5,
    };

	ZeroMemory( &bd2d, sizeof(bd2d) );

    bd2d.Usage = D3D11_USAGE_DEFAULT;
    bd2d.ByteWidth = sizeof( WORD ) * 6;
    bd2d.BindFlags = D3D11_BIND_INDEX_BUFFER;
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

	hr=D3DX11CompileFromFileA( shaderfile, NULL, NULL, "PS", "ps_4_0", D3DCOMPILE_ENABLE_STRICTNESS, 0, NULL, &pBlob, &pErrorBlob, NULL );

	if (pErrorBlob) 
		pErrorBlob->Release();

	if( FAILED( hr ) )
	{
		OutputDebugStringA("pixelshader compilation failed\n");
		return;
	}

    hr = dev->CreatePixelShader( pBlob->GetBufferPointer(), pBlob->GetBufferSize(), NULL, &pPixelShader );
	pBlob->Release();
	if( FAILED( hr ) )
	{
		OutputDebugStringA("CreatePixelShader failed\n");
		return;
	}

	pBlob = NULL;
	pErrorBlob = NULL;

	hr=D3DX11CompileFromFileA( shaderfile, NULL, NULL, "VS", "vs_4_0", D3DCOMPILE_ENABLE_STRICTNESS, 0, NULL, &pBlob, &pErrorBlob, NULL );

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
	rasterizerdesc.DepthClipEnable = true;
	rasterizerdesc.AntialiasedLineEnable = false;
	
	
	rasterizerdesc.MultisampleEnable = false;
	rasterizerdesc.ScissorEnable = false;
	rasterizerdesc.SlopeScaledDepthBias = 0.0f;


	hr=dev->CreateRasterizerState(&rasterizerdesc, &pOverlayRasterizer);
	if( FAILED( hr ) )
        return;


	D3D11_BLEND_DESC blend;
	
	D3D11_RENDER_TARGET_BLEND_DESC rtbd;
	ZeroMemory( &rtbd, sizeof(rtbd) );
	ZeroMemory( &blend, sizeof(blend) );

	rtbd.BlendEnable			 = true;
	rtbd.SrcBlend				 = D3D11_BLEND_SRC_ALPHA;	
	rtbd.DestBlend				 = D3D11_BLEND_INV_SRC_ALPHA;	
	rtbd.BlendOp				 = D3D11_BLEND_OP_ADD;
	rtbd.SrcBlendAlpha			 = D3D11_BLEND_ZERO;
	rtbd.DestBlendAlpha			 = D3D11_BLEND_ZERO;
	rtbd.BlendOpAlpha			 = D3D11_BLEND_OP_ADD;
	rtbd.RenderTargetWriteMask	 = D3D11_COLOR_WRITE_ENABLE_ALL;

	blend.AlphaToCoverageEnable=false;
	blend.IndependentBlendEnable=true; //true;
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



	//now create the texture of the overlay
	hr=setupOverlayTexture();

	if( FAILED( hr ) )
		return;

	

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




	Valid=TRUE;
}


void DXMessD3D11Handler::RenderOverlay()
{
	int i;
	HRESULT hr;
	if (Valid)
	{
		

		//render the overlay
		ID3D11DeviceContext *dc;
		dev->GetImmediateContext(&dc); //increases the reference count


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
		float blendFactor[] = {1.0f, 1.0f, 1.0f, 1.0f};

		ID3D11VertexShader *oldvs=NULL;
		ID3D11ClassInstance *oldvsinstances=NULL;
		UINT vci_count=0;

		ID3D11PixelShader *oldps=NULL;
		ID3D11ClassInstance *oldpsinstances=NULL;
		UINT pci_count=0;

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
		dc->VSGetShader( &oldvs, &oldvsinstances, &vci_count);
		
		dc->PSGetConstantBuffers(0,1, &oldConstantBuffersPS);
		dc->PSGetShader( &oldps, &oldpsinstances, &pci_count);
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

	    dc->VSSetShader( pVertexShader, NULL, 0 );
		dc->PSSetShader( pPixelShader, NULL, 0 );
		dc->PSSetSamplers( 0, 1, &pSamplerLinear );
		
		D3D11_VIEWPORT vp;
		vp.Width = desc.BufferDesc.Width;
		vp.Height = desc.BufferDesc.Height;
		vp.MinDepth = 0.0f;
		vp.MaxDepth = 1.0f;
		vp.TopLeftX = 0;
		vp.TopLeftY = 0;
		dc->RSSetViewports( 1, &vp );
		 

		
		dc->OMSetRenderTargetsAndUnorderedAccessViews(0,NULL,NULL,0,0,NULL,NULL);
		dc->OMSetRenderTargets(1, &pRenderTargetView, pDepthStencilView);		
		dc->ClearDepthStencilView( pDepthStencilView, D3D11_CLEAR_DEPTH, 1.0f, 0 );

		dc->OMSetBlendState(pTransparency, blendFactor, 0xffffffff);
		dc->OMSetDepthStencilState(NULL,0);
		

		

		dc->IASetPrimitiveTopology( D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST );
		dc->IASetInputLayout( pVertexLayout );
		dc->IASetIndexBuffer( pOverlayIB, DXGI_FORMAT_R16_UINT, 0 );
		

		dc->RSSetState(pOverlayRasterizer);

		
		for (i=0; i<OverlayCount; i++)
		{
			if (shared->resources[i].valid)
			{			
				//set the vertexbuffer and texture and render
				dc->IASetVertexBuffers( 0, 1, &overlays[i].pOverlayVB, &stride, &offset );
				dc->PSSetShaderResources( 0, 1, &overlays[i].pOverlayTex );	
	
				ConstantBuffer cb;
				UpdatePosForOverlay(i, &desc);
				cb.transparency=shared->resources[i].alphaBlend;
				cb.translation.x=overlays[i].x;
				cb.translation.y=overlays[i].y;

				dc->UpdateSubresource( pConstantBuffer, 0, NULL, &cb, 0, 0 );

				dc->VSSetConstantBuffers(0,1, &pConstantBuffer);
				dc->PSSetConstantBuffers(0,1, &pConstantBuffer);
				

				
				dc->DrawIndexed( 6, 0,0);
			}
		}

		//restore
		dc->VSSetShader(oldvs, (ID3D11ClassInstance *const *)oldvsinstances, vci_count);
		dc->PSSetShader(oldps, (ID3D11ClassInstance *const *)oldpsinstances, pci_count);
		dc->PSSetSamplers(0, 1, &oldPSSampler);
		dc->PSSetShaderResources(0,1, &oldPSShaderResource);

		dc->VSSetConstantBuffers(0,1, &oldConstantBuffersVS);
		dc->PSSetConstantBuffers(0,1, &oldConstantBuffersPS);


		dc->OMSetRenderTargets(1, &oldRenderTarget, oldDepthStencilView);
		dc->OMSetBlendState(oldBlendState, oldblendFactor, oldblendsamplemask);
		dc->OMSetDepthStencilState(oldDepthStencilState, oldstencilref);

		dc->IASetPrimitiveTopology(oldPrimitiveTopology);
		dc->IASetInputLayout(oldInputLayout);
		dc->IASetIndexBuffer(oldIndexBuffer, oldIndexBufferFormat, oldIndexBufferOffset);
		dc->IASetVertexBuffers(0,1,&oldVertexBuffer, &oldVertexBufferStrides, &oldVertexBufferOffset);

		dc->RSSetState(oldRastersizerState);
		dc->RSSetViewports(oldviewports, viewports);

		dc->Release(); //lower the referencecount

		//check if the overlay texture needs to be updated, if so, getdc, update, continue
	}

}



void __stdcall D3D11Hook_SwapChain_Present_imp(IDXGISwapChain *swapchain, ID3D11Device *device, PD3DHookShared shared)
{
	//look up the controller class for this device
	if (D3D11devices[device]==NULL)
	{
		//OutputDebugStringA("New\n");
		//D3D11devices[dev]=1;

		DXMessD3D11Handler *dc=new DXMessD3D11Handler(device, swapchain, shared);//create a new devicehandler

		//add to the map
		D3D11devices[device]=dc;

	}
	D3D11devices[device]->RenderOverlay();			

}