#ifndef DX10HOOK_H
#define DX10HOOK_H

#include "stdafx.h"


typedef struct
{
	ID3D10Buffer *pOverlayVB;
	ID3D10ShaderResourceView *pOverlayTex;
} OverlayData10, *POverlayData10;

class DXMessD3D10Handler
{
private:
	PD3DHookShared shared;

	ID3D10Device *dev;
	IDXGISwapChain *swapchain;

	
	ID3D10Buffer *pOverlayIB;

	int OverlayCount;
	OverlayData10 *overlays;



	ID3D10PixelShader *pPixelShader;
	ID3D10VertexShader *pVertexShader;
	ID3D10InputLayout *pVertexLayout;

	ID3D10SamplerState *pSamplerLinear;
	ID3D10RasterizerState *pOverlayRasterizer;
	ID3D10BlendState *pTransparency;

	ID3D10Texture2D *pDepthStencil;
	ID3D10RenderTargetView *pRenderTargetView;
	ID3D10DepthStencilView *pDepthStencilView;

	//ID3DX10Sprite *sprite; //in case the pixelshader stuff fails (I can't seem to set it to a lower PS version than 4...)


	BOOL Valid;
	HRESULT setupOverlayTexture();
	HRESULT UpdateVBPosForOverlay(int i, DXGI_SWAP_CHAIN_DESC *desc);
public:
	DXMessD3D10Handler(ID3D10Device *dev, IDXGISwapChain *sc, PD3DHookShared shared);
	~DXMessD3D10Handler();
	void RenderOverlay();

};


void __stdcall D3D10Hook_SwapChain_Present_imp(IDXGISwapChain *swapchain, ID3D10Device *device, PD3DHookShared shared);

#endif