#ifndef DX10HOOK_H
#define DX10HOOK_H

#include "stdafx.h"


typedef struct
{	
	ID3D10ShaderResourceView *pTexture;
	PFONTMAP DefinedFontMap; //Optional pointer to a fontmaparray if it's a font texture
} TextureData10, *PTextureData10;

class DXMessD3D10Handler
{
private:
	volatile PD3DHookShared shared;	
	
	ID3D10Buffer *pSpriteVB;

	int currentMaxCharacterCount; //holds the number of vertices in pSpriteVB divided by 6
	ID3D10Buffer *pFontVB;
	

	int TextureCount;
	TextureData10 *textures;

	PTextureEntry tea; //texture entry area



	ID3D10PixelShader *pPixelShaderNormal;
	ID3D10VertexShader *pVertexShader;
	ID3D10InputLayout *pVertexLayout;

	ID3D10SamplerState *pSamplerLinear;
	ID3D10RasterizerState *pSpriteRasterizer;	
	ID3D10BlendState *pTransparency;

	ID3D10Texture2D *pDepthStencil;
	
	ID3D10DepthStencilView *pDepthStencilView;
	ID3D10Buffer *pConstantBuffer;

	BOOL Valid;
	BOOL UpdateTextures();

	void SetupFontVertexBuffer(int count);
	void DrawString(D3D10_VIEWPORT vp, PTextureData10 pFontTexture, char *s, int strlen);

	
	
public:
	int snapshotCounter;
	DWORD lastSnapshot; //tickcount when the last snapshot was made (so there's at least 250 ms between snapshots)	
	BOOL makeSnapshot;
	BOOL smallSnapshot;
	POINT smallSnapshotPoint;
	POINTF smallSnapshotPointRelative;
	RECT smallSnapshotClientRect; //in case the render target has a different size than the clientrect (e.g in games that fake a high fps by actually lowering the resolution without the user seeing the res change)
	ID3D10RenderTargetView *pRenderTargetView;
	ID3D10Device *dev;
	IDXGISwapChain *swapchain;
	ID3D10RasterizerState *pWireframeRasterizer;
	ID3D10DepthStencilState *pDisabledDepthStencilState;

	DXMessD3D10Handler(ID3D10Device *dev, IDXGISwapChain *sc, PD3DHookShared s);
	~DXMessD3D10Handler();
	void RenderOverlay();
	void TakeSnapshot(char *functionname);
	void PrepareForSnapshot();
	
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