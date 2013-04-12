#ifndef DX11HOOK_H
#define DX11HOOK_H

#include "stdafx.h"

typedef struct
{	
	ID3D11ShaderResourceView *pTexture;
	PFONTMAP DefinedFontMap; //Optional pointer to a fontmaparray if it's a font texture
} TextureData11, *PTextureData11;

class DXMessD3D11Handler
{
private:
	volatile PD3DHookShared shared;

	CRITICAL_SECTION cs;

	IDXGISwapChain *swapchain;	
	ID3D11Buffer *pSpriteVB;

	int currentMaxCharacterCount; //holds the number of vertices in pSpriteVB divided by 6
	ID3D11Buffer *pFontVB;
	

	int TextureCount;
	TextureData11 *textures;

	PTextureEntry tea; //texture entry area



	ID3D11PixelShader *pPixelShaderNormal;
	ID3D11VertexShader *pVertexShader;
	ID3D11InputLayout *pVertexLayout;

	ID3D11SamplerState *pSamplerLinear;
	ID3D11RasterizerState *pSpriteRasterizer;
	ID3D11BlendState *pTransparency;

	ID3D11Texture2D *pDepthStencil;
	ID3D11RenderTargetView *pRenderTargetView;
	ID3D11DepthStencilView *pDepthStencilView;
	ID3D11Buffer *pConstantBuffer;

	ID3D11DeviceContext *RenderContext;




	BOOL Valid;
	BOOL UpdateTextures();

	void SetupFontVertexBuffer(int count);
	void DrawString(D3D11_VIEWPORT vp, PTextureData11 pFontTexture, char *s, int strlen);

public:	
	int snapshotCounter;
	DWORD lastSnapshot; //tickcount when the last snapshot was made (so there's at least 250 ms between snapshots)	
	BOOL makeSnapshot;
	BOOL smallSnapshot;
	POINT smallSnapshotPoint;
	POINTF smallSnapshotPointRelative;
	RECT smallSnapshotClientRect; //in case the render target has a different size than the clientrect (e.g in games that fake a high fps by actually lowering the resolution without the user seeing the res change)

	ID3D11Device *dev;
	//ID3D11DeviceContext *dc;
	ID3D11RasterizerState *pWireframeRasterizer;
	ID3D11DepthStencilState *pDisabledDepthStencilState;

	ID3D11Texture2D *ExtraRenderTargetTexture;
	ID3D11RenderTargetView *ExtraRenderTarget;

	DXMessD3D11Handler(ID3D11Device *dev, IDXGISwapChain *sc, PD3DHookShared s);
	~DXMessD3D11Handler();
	void RenderOverlay();
	void TakeSnapshot(ID3D11DeviceContext *dc, char *functionname);
    ID3D11DeviceContext *PrepareForSnapshot(ID3D11DeviceContext *dc) ;

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