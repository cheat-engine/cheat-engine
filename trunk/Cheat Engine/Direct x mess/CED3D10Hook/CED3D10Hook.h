#ifndef DX10HOOK_H
#define DX10HOOK_H

#include "stdafx.h"


typedef struct
{
	ID3D10Buffer *pOverlayVB;
	ID3D10ShaderResourceView *pOverlayTex;
	float x,y;
} OverlayData10, *POverlayData10;

class DXMessD3D10Handler
{
private:
	volatile PD3DHookShared shared;

	
	IDXGISwapChain *swapchain;

	
	ID3D10Buffer *pOverlayIB;

	int OverlayCount;
	OverlayData10 *overlays;



	ID3D10PixelShader *pPixelShader, *pPixelShaderNormal;
	ID3D10VertexShader *pVertexShader;
	ID3D10InputLayout *pVertexLayout;

	ID3D10SamplerState *pSamplerLinear;
	ID3D10RasterizerState *pOverlayRasterizer;
	
	ID3D10BlendState *pTransparency;

	ID3D10Texture2D *pDepthStencil;
	


	ID3D10RenderTargetView *pRenderTargetView;
	ID3D10DepthStencilView *pDepthStencilView;
	ID3D10Buffer *pConstantBuffer;

	//ID3DX10Sprite *sprite; //in case the pixelshader stuff fails (I can't seem to set it to a lower PS version than 4...)


	BOOL Valid;
	HRESULT setupOverlayTexture();
	void UpdatePosForOverlay(int i, DXGI_SWAP_CHAIN_DESC *desc);
public:
	ID3D10Device *dev;
	ID3D10RasterizerState *pWireframeRasterizer;
	ID3D10DepthStencilState *pDisabledDepthStencilState;

	DXMessD3D10Handler(ID3D10Device *dev, IDXGISwapChain *sc, PD3DHookShared s);
	~DXMessD3D10Handler();
	void RenderOverlay();
	
};

typedef HRESULT		(__stdcall *D3D10_DRAWINDEXED_ORIGINAL)(ID3D10Device *device, UINT IndexCount, UINT StartIndexLocation, INT BaseVertexLocation);
typedef HRESULT		(__stdcall *D3D10_DRAW_ORIGINAL)(ID3D10Device *device, UINT VertexCount, UINT StartVertexLocation);
typedef HRESULT		(__stdcall *D3D10_DRAWINDEXEDINSTANCED_ORIGINAL)(ID3D10Device *device, UINT IndexCountPerInstance, UINT InstanceCount, UINT StartIndexLocation, INT BaseVertexLocation, UINT StartInstanceLocation);
typedef HRESULT		(__stdcall *D3D10_DRAWINSTANCED_ORIGINAL)(ID3D10Device *device, UINT VertexCountPerInstance, UINT InstanceCount, UINT StartVertexLocation, UINT StartInstanceLocation);
typedef HRESULT		(__stdcall *D3D10_DRAWAUTO_ORIGINAL)(ID3D10Device *device);

HRESULT __stdcall D3D10Hook_DrawIndexed_imp(D3D10_DRAWINDEXED_ORIGINAL originalfunction, ID3D10Device *device, UINT IndexCount, UINT StartIndexLocation, INT BaseVertexLocation)	;
HRESULT __stdcall D3D10Hook_Draw_imp(D3D10_DRAW_ORIGINAL originalfunction, ID3D10Device *device, UINT VertexCount, UINT StartVertexLocation);
HRESULT __stdcall D3D10Hook_DrawIndexedInstanced_imp(D3D10_DRAWINDEXEDINSTANCED_ORIGINAL originalfunction, ID3D10Device *device, UINT IndexCountPerInstance, UINT InstanceCount, UINT StartIndexLocation, INT BaseVertexLocation, UINT StartInstanceLocation);
HRESULT __stdcall D3D10Hook_DrawInstanced_imp(D3D10_DRAWINSTANCED_ORIGINAL originalfunction, ID3D10Device *device, UINT VertexCountPerInstance, UINT InstanceCount, UINT StartVertexLocation, UINT StartInstanceLocation);
HRESULT __stdcall D3D10Hook_DrawAuto_imp(D3D10_DRAWAUTO_ORIGINAL originalfunction, ID3D10Device *device);
            

void __stdcall D3D10Hook_SwapChain_Present_imp(IDXGISwapChain *swapchain, ID3D10Device *device, PD3DHookShared shared);
void __stdcall D3D10Hook_SwapChain_ResizeBuffers_imp(IDXGISwapChain *swapchain, ID3D10Device *device, PD3DHookShared shared);

#endif