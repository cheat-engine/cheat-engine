#ifndef DX9HOOK_H
#define DX9HOOK_H

#include "stdafx.h"

typedef struct
{
	IDirect3DTexture9 *pOverlayTex;
	IDirect3DVertexBuffer9 *pOverlayVB; //currently unused in peference of the sprite
	int x,y;
	int actualHeight, actualWidth;
} OverlayData9, *POverlayData9;


class DXMessD3D9Handler
{
private:
	PD3DHookShared shared;

	IDirect3DDevice9 *dev;

	OverlayData9 *overlays;
	int OverlayCount;
	
	/*IDXGISwapChain *swapchain;

	
	ID3D9Buffer *pOverlayIB;


	



	ID3D9PixelShader *pPixelShader;
	ID3D9VertexShader *pVertexShader;
	ID3D9InputLayout *pVertexLayout;

	ID3D9SamplerState *pSamplerLinear;
	ID3D9RasterizerState *pOverlayRasterizer;
	ID3D9BlendState *pTransparency;
	*/


	//ID3DX9Sprite *sprite; //in case the pixelshader stuff fails (I can't seem to set it to a lower PS version than 4...)
	ID3DXSprite *sprite; 


	BOOL Valid;
	HRESULT setupOverlayTexture();
public:
	DXMessD3D9Handler(IDirect3DDevice9 *dev, PD3DHookShared shared);
	~DXMessD3D9Handler();
	void RenderOverlay();
	void BeforeReset();
	void AfterReset();
};

typedef HRESULT     (__stdcall *D3D9_RESET_ORIGINAL)(IDirect3DDevice9 *Device, D3DPRESENT_PARAMETERS *pPresentationParameters);

void __stdcall D3D9Hook_Present_imp(IDirect3DDevice9 *device, PD3DHookShared shared);
HRESULT __stdcall D3D9Hook_Reset_imp(D3D9_RESET_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRESENT_PARAMETERS *pPresentationParameters, PD3DHookShared shared);

#endif