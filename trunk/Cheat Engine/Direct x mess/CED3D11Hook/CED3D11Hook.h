#ifndef DX11HOOK_H
#define DX11HOOK_H

#include "stdafx.h"





typedef struct
{
	ID3D11Buffer *pOverlayVB;
	ID3D11ShaderResourceView *pOverlayTex;
	float x,y;
} OverlayData, *POverlayData;

class DXMessD3D11Handler
{
private:
	PD3DHookShared shared;

	ID3D11Device *dev;
	IDXGISwapChain *swapchain;

	
	ID3D11Buffer *pOverlayIB;

	int OverlayCount;
	OverlayData *overlays;



	ID3D11PixelShader *pPixelShader;
	ID3D11VertexShader *pVertexShader;
	ID3D11InputLayout *pVertexLayout;

	ID3D11SamplerState *pSamplerLinear;
	ID3D11RasterizerState *pOverlayRasterizer;
	ID3D11BlendState *pTransparency;

	ID3D11Texture2D *pDepthStencil;
	ID3D11RenderTargetView *pRenderTargetView;
	ID3D11DepthStencilView *pDepthStencilView;
	ID3D11Buffer *pConstantBuffer;



	BOOL Valid;
	HRESULT setupOverlayTexture();
	void UpdatePosForOverlay(int i, DXGI_SWAP_CHAIN_DESC *desc);
public:
	DXMessD3D11Handler(ID3D11Device *dev, IDXGISwapChain *sc, PD3DHookShared shared);
	~DXMessD3D11Handler();
	void RenderOverlay();

};



void __stdcall D3D11Hook_SwapChain_Present_imp(IDXGISwapChain *swapchain, ID3D11Device *device, PD3DHookShared shared);

#endif