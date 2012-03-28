#ifndef DX9HOOK_H
#define DX9HOOK_H

#include "stdafx.h"

typedef struct
{
	//texture coordinates for each character
	float offset; //offset where this character starts
	float charwidth; //width in pixel of this character	
} CHARINFO, *PCHARINFO;

typedef struct
{
	float charheight; //height in pixels of each character
	float fullwidth; //width in pixels of the full fontmap
	CHARINFO charinfo[96];
} FONTMAP, *PFONTMAP;

typedef struct
{
	IDirect3DTexture9 *pTexture;
	int actualHeight, actualWidth;
	PFONTMAP DefinedFontMap; //Optional pointer to a fontmaparray if it's a font texture
} TextureData9, *PTextureData9;


class DXMessD3D9Handler
{
private:
	PD3DHookShared shared;

	IDirect3DDevice9 *dev;

	TextureData9 *textures;
	int TextureCount;
	
	ID3DXSprite *sprite; 

	PTextureEntry tea; //texture entry area

	BOOL Valid;
	BOOL UpdateTextures();
	void DrawString(D3DXVECTOR3 position, PTextureData9 pFontTexture, char *s, int strlen);
public:
	DXMessD3D9Handler(IDirect3DDevice9 *dev, PD3DHookShared shared);
	~DXMessD3D9Handler();
	void RenderOverlay();
	void BeforeReset();
	void AfterReset();
};

typedef HRESULT     (__stdcall *D3D9_RESET_ORIGINAL)(IDirect3DDevice9 *Device, D3DPRESENT_PARAMETERS *pPresentationParameters);
typedef HRESULT		(__stdcall *D3D9_DRAWPRIMITIVE_ORIGINAL)(IDirect3DDevice9 *Device, D3DPRIMITIVETYPE PrimitiveType,UINT StartVertex,UINT PrimitiveCount);
typedef HRESULT		(__stdcall *D3D9_DRAWINDEXEDPRIMITIVE_ORIGINAL)(IDirect3DDevice9 *Device, D3DPRIMITIVETYPE PrimitiveType,INT BaseVertexIndex,UINT MinVertexIndex,UINT NumVertices,UINT startIndex,UINT primCount);
typedef HRESULT		(__stdcall *D3D9_DRAWPRIMITIVEUP_ORIGINAL)(IDirect3DDevice9 *Device, D3DPRIMITIVETYPE PrimitiveType,UINT PrimitiveCount,CONST void* pVertexStreamZeroData,UINT VertexStreamZeroStride);
typedef HRESULT		(__stdcall *D3D9_DRAWINDEXEDPRIMITIVEUP_ORIGINAL)(IDirect3DDevice9 *Device, D3DPRIMITIVETYPE PrimitiveType,UINT MinVertexIndex,UINT NumVertices,UINT PrimitiveCount,CONST void* pIndexData,D3DFORMAT IndexDataFormat,CONST void* pVertexStreamZeroData,UINT VertexStreamZeroStride);

typedef HRESULT		(__stdcall *D3D9_DRAWRECTPATCH_ORIGINAL)(IDirect3DDevice9 *device, UINT Handle,CONST float* pNumSegs,CONST D3DRECTPATCH_INFO* pRectPatchInfo);
typedef HRESULT		(__stdcall *D3D9_DRAWTRIPATCH_ORIGINAL)(IDirect3DDevice9 *device, UINT Handle,CONST float* pNumSegs,CONST D3DTRIPATCH_INFO* pTriPatchInfo);

void __stdcall D3D9Hook_Present_imp(IDirect3DDevice9 *device, PD3DHookShared s);
HRESULT __stdcall D3D9Hook_Reset_imp(D3D9_RESET_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRESENT_PARAMETERS *pPresentationParameters);


HRESULT __stdcall D3D9Hook_DrawPrimitive_imp(D3D9_DRAWPRIMITIVE_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRIMITIVETYPE PrimitiveType,UINT StartVertex,UINT PrimitiveCount);
HRESULT __stdcall D3D9Hook_DrawIndexedPrimitive_imp(D3D9_DRAWINDEXEDPRIMITIVE_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRIMITIVETYPE PrimitiveType,INT BaseVertexIndex,UINT MinVertexIndex,UINT NumVertices,UINT startIndex,UINT primCount);
HRESULT __stdcall D3D9Hook_DrawPrimitiveUP_imp(D3D9_DRAWPRIMITIVEUP_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRIMITIVETYPE PrimitiveType,UINT PrimitiveCount,CONST void* pVertexStreamZeroData,UINT VertexStreamZeroStride);
HRESULT __stdcall D3D9Hook_DrawIndexedPrimitiveUP_imp(D3D9_DRAWINDEXEDPRIMITIVEUP_ORIGINAL originalfunction, IDirect3DDevice9 *device, D3DPRIMITIVETYPE PrimitiveType,UINT MinVertexIndex,UINT NumVertices,UINT PrimitiveCount,CONST void* pIndexData,D3DFORMAT IndexDataFormat,CONST void* pVertexStreamZeroData,UINT VertexStreamZeroStride);
HRESULT __stdcall D3D9Hook_DrawRectPatch_imp(D3D9_DRAWRECTPATCH_ORIGINAL originalfunction, IDirect3DDevice9 *device, UINT Handle,CONST float* pNumSegs,CONST D3DRECTPATCH_INFO* pRectPatchInfo);
HRESULT __stdcall D3D9Hook_DrawTriPatch_imp(D3D9_DRAWTRIPATCH_ORIGINAL originalfunction, IDirect3DDevice9 *device, UINT Handle,CONST float* pNumSegs,CONST D3DTRIPATCH_INFO* pTriPatchInfo);

#endif