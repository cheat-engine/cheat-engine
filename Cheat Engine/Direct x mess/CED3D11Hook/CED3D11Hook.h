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
	ID3D11Device *dev;
	ID3D11DeviceContext *dc;
	ID3D11RasterizerState *pWireframeRasterizer;
	ID3D11DepthStencilState *pDisabledDepthStencilState;

	DXMessD3D11Handler(ID3D11Device *dev, IDXGISwapChain *sc, PD3DHookShared s);
	~DXMessD3D11Handler();
	void RenderOverlay();

};


typedef HRESULT		(__stdcall *D3D11_DRAWINDEXED_ORIGINAL)(ID3D11DeviceContext *dc, UINT IndexCount, UINT StartIndexLocation, INT BaseVertexLocation);
typedef HRESULT		(__stdcall *D3D11_DRAW_ORIGINAL)(ID3D11DeviceContext *dc, UINT VertexCount, UINT StartVertexLocation);
typedef HRESULT		(__stdcall *D3D11_DRAWINDEXEDINSTANCED_ORIGINAL)(ID3D11DeviceContext *dc, UINT IndexCountPerInstance, UINT InstanceCount, UINT StartIndexLocation, INT BaseVertexLocation, UINT StartInstanceLocation);
typedef HRESULT		(__stdcall *D3D11_DRAWINSTANCED_ORIGINAL)(ID3D11DeviceContext *dc, UINT VertexCountPerInstance, UINT InstanceCount, UINT StartVertexLocation, UINT StartInstanceLocation);
typedef HRESULT		(__stdcall *D3D11_DRAWAUTO_ORIGINAL)(ID3D11DeviceContext *dc);

HRESULT __stdcall D3D11Hook_DrawIndexed_imp(D3D11_DRAWINDEXED_ORIGINAL originalfunction, ID3D11DeviceContext *dc, UINT IndexCount, UINT StartIndexLocation, INT BaseVertexLocation)	;
HRESULT __stdcall D3D11Hook_Draw_imp(D3D11_DRAW_ORIGINAL originalfunction, ID3D11DeviceContext *dc, UINT VertexCount, UINT StartVertexLocation);
HRESULT __stdcall D3D11Hook_DrawIndexedInstanced_imp(D3D11_DRAWINDEXEDINSTANCED_ORIGINAL originalfunction, ID3D11DeviceContext *dc, UINT IndexCountPerInstance, UINT InstanceCount, UINT StartIndexLocation, INT BaseVertexLocation, UINT StartInstanceLocation);
HRESULT __stdcall D3D11Hook_DrawInstanced_imp(D3D11_DRAWINSTANCED_ORIGINAL originalfunction, ID3D11DeviceContext *dc, UINT VertexCountPerInstance, UINT InstanceCount, UINT StartVertexLocation, UINT StartInstanceLocation);
HRESULT __stdcall D3D11Hook_DrawAuto_imp(D3D11_DRAWAUTO_ORIGINAL originalfunction, ID3D11DeviceContext *dc);
            

void __stdcall D3D11Hook_SwapChain_Present_imp(IDXGISwapChain *swapchain, ID3D11Device *device, PD3DHookShared shared);
void __stdcall D3D11Hook_SwapChain_ResizeBuffers_imp(IDXGISwapChain *swapchain, ID3D11Device *device, PD3DHookShared shared);

#endif