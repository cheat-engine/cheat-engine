#ifndef DX9HOOK_H
#define DX9HOOK_H

#include "stdafx.h"

#include <ocidl.h>




typedef struct
{
	IDirect3DTexture9 *pTexture;
	float width,height;
	PFONTMAP DefinedFontMap; //Optional pointer to a fontmaparray if it's a font texture
} TextureData9, *PTextureData9;


class DXMessD3D9Handler
{
private:
	PD3DHookShared shared;

	

	TextureData9 *textures;
	int TextureCount;
	
	ID3DXSprite *sprite; 

	PTextureEntry tea; //texture entry area

	BOOL Valid;
	BOOL UpdateTextures();
	void DrawString(D3DXVECTOR3 position, PTextureData9 pFontTexture, char *s, int strlen, float alphablend);
public:
	
	int snapshotCounter;
	DWORD lastSnapshot; //tickcount when the last snapshot was made (so there's at least 250 ms between snapshots)	
	BOOL makeSnapshot;
	BOOL smallSnapshot;
	POINT smallSnapshotPoint;
	POINTF smallSnapshotPointRelative;
	RECT smallSnapshotClientRect; //in case the render target has a different size than the clientrect (e.g in games that fake a high fps by actually lowering the resolution without the user seeing the res change)

	IDirect3DDevice9 *dev;

	void TakeSnapshot(char *functionname);
	void PrepareForSnapshot();
	void RenderOverlay();
	void BeforeReset();
	void AfterReset();

	DXMessD3D9Handler(IDirect3DDevice9 *dev, PD3DHookShared shared);
	~DXMessD3D9Handler();


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