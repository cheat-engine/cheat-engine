(*)
 [------------------------------------------------------------------------------
 [  DirectXGraphics 8[.1] Delphi Adaptation (c) by Tim Baumgarten
 [------------------------------------------------------------------------------
 [  Files    : d3d8types.h
 [             d3d8caps.h
 [             d3d8.h
 [  Modified : 10-Nov-2001
 [  E-Mail   : Ampaze@gmx.net
 [  Download : http://www.crazyentertainment.net
 [------------------------------------------------------------------------------
(*)

(*)
 [------------------------------------------------------------------------------
 [ History :
 [----------
 [ 10-Nov-2001 (Tim Baumgarten) : Added DX 8.1, and still testing,
 [                                Define DX8 to make it work with dx8 runtime.
 [------------------------------------------------------------------------------
 [ 05-Nov-2001 (Tim Baumgarten) : Changed the EnumTypes for D6, so you might
 [                                have to typecast to LongWord in some cases.
 [------------------------------------------------------------------------------
 [ 28-Jul-2001 (Tim Baumgarten) : Changed "var pDestinationSurface : IDirect3DSurface8" to
 [                                "const pDestinationSurface : IDirect3DSurface8" in
 [                                IDirect3DDevice8.CopyRects
 [------------------------------------------------------------------------------
 [ 14-Mar-2001 (Tim Baumgarten) : Changed CreateVertexShader as pFunction can
 [                                be nil.
 [------------------------------------------------------------------------------
 [ 28-Jan-2001 (Tim Baumgarten) : Added TD3DMultiSampleType = TD3DMultiSample_Type;
 [------------------------------------------------------------------------------
 [ 23-Dec-2000 (Tim Baumgarten) : Changed all types that are declared as UInt
 [                                in C to be Cardinal in Delphi
 [------------------------------------------------------------------------------
 [ 23-Dec-2000 (Tim Baumgarten) : Changed all types that are declared as DWord
 [                                in C to be LongWord in Delphi
 [------------------------------------------------------------------------------
 [ 14-Dec-2000 (Tim Baumgarten) : Changed some parameters of IDirect3DDevice8.DrawRectPatch
 [                                and IDirect3DDevice8.DrawTriPatch to Pointers.
 [------------------------------------------------------------------------------
 [ 14-Dec-2000 (Tim Baumgarten) : Added versions without underlines of some structures
 [------------------------------------------------------------------------------
 [ 14-Dec-2000 (Tim Baumgarten) : Added "Pointer to Structure" (PStructure = ^TStructure)
 [                                to all structures.
 [------------------------------------------------------------------------------
 [ 26-Nov-2000 (Tim Baumgarten) : Returncodes are now typecasted with HResult
 [------------------------------------------------------------------------------
(*)

unit DirectXGraphics;

{$MINENUMSIZE 4}
{$ALIGN ON}

//Remove dot to make all enums to be const's
{$DEFINE NOENUMS}

//Remove dot to revert to dx8
{.$DEFINE DX8}

//Remove dot to enable static linking, your app will halt with a error message
//when the d3d8.dll cannot be found.
{$DEFINE STATIC_LINKING}

{$IFDEF VER140}
  {$DEFINE DELPHI6}
{$ENDIF}
{$IFDEF DELPHI6}
  {$DEFINE DELPHI6_AND_HIGHER}
  {$DEFINE D6UP}
{$ENDIF}

{$IFNDEF DELPHI6_AND_HIGHER}
  {$DEFINE NOENUMS}
{$ENDIF}
interface

uses
  Windows;

{$IFNDEF STATIC_LINKING}
var D3D8DLL : HMODULE = 0;
{$ENDIF}

const D3D8DLLName = 'd3d8.dll';

(*==========================================================================;
 *
 *  Copyright (C) 1995-2000 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       d3d8types.h
 *  Content:    Direct3D capabilities include file
 *
 ***************************************************************************)

const DIRECT3D_VERSION = $0800;

const iTrue = DWord(True);
      iFalse = DWord(False);

type TD3DColor = type LongWord;

  // maps unsigned 8 bits/channel to D3DCOLOR
  function D3DCOLOR_ARGB(a, r, g, b : Cardinal) : TD3DColor; // ((D3DCOLOR)((((a)&= $ff)<<24)|(((r)&= $ff)<<16)|(((g)&= $ff)<<8)|((b)&= $ff)))
  function D3DCOLOR_RGBA(r, g, b, a : Cardinal) : TD3DColor; // D3DCOLOR_ARGB(a;r;g;b)
  function D3DCOLOR_XRGB(r, g, b : Cardinal) : TD3DColor; //   D3DCOLOR_ARGB(= $ff;r;g;b)

// maps floating point channels (0.f to 1.f range) to D3DCOLOR
  function D3DCOLOR_COLORVALUE(r, g, b, a : Single) : TD3DColor; // D3DCOLOR_RGBA((DWORD)((r)*255.f);(DWORD)((g)*255.f);(DWORD)((b)*255.f);(DWORD)((a)*255.f))

type
  PD3DVector = ^TD3DVector;
  TD3DVector = packed record
    x : Single;
    y : Single;
    z : Single;
  end;

  PD3DColorValue = ^TD3DColorValue;
  TD3DColorValue = packed record
    r : Single;
    g : Single;
    b : Single;
    a : Single;
  end;

  PD3DRect = ^TD3DRect;
  TD3DRect = packed record
    x1 : LongInt;
    y1 : LongInt;
    x2 : LongInt;
    y2 : LongInt;
  end;

  PD3DMatrix = ^TD3DMatrix;
  TD3DMatrix = packed record
    case Integer of
      0 : (_11, _12, _13, _14 : Single;
           _21, _22, _23, _24 : Single;
           _31, _32, _33, _34 : Single;
           _41, _42, _43, _44 : Single);
      1 : (m : array [0..3, 0..3] of Single);
  end;

  PD3DViewport8 = ^TD3DViewport8;
  TD3DViewport8 = packed record
    X      : LongWord;
    Y      : LongWord;  (* Viewport Top left *)
    Width  : LongWord;
    Height : LongWord;  (* Viewport Dimensions *)
    MinZ   : Single;    (* Min/max of clip Volume *)
    MaxZ   : Single
  end;


(* Values for clip fields.*)

// Max number of user clipping planes; supported in D3D.
const
  D3DMAXUSERCLIPPLANES = 32;

// These bits could be ORed together to use with D3DRS_CLIPPLANEENABLE
  D3DCLIPPLANE0 = 1;   // (1 << 0)
  D3DCLIPPLANE1 = 2;   // (1 << 1)
  D3DCLIPPLANE2 = 4;   // (1 << 2)
  D3DCLIPPLANE3 = 8;   // (1 << 3)
  D3DCLIPPLANE4 = 16;  // (1 << 4)
  D3DCLIPPLANE5 = 32;  // (1 << 5)

// The following bits are used in the ClipUnion and ClipIntersection
// members of the D3DCLIPSTATUS8
  D3DCS_LEFT        = $00000001;
  D3DCS_RIGHT       = $00000002;
  D3DCS_TOP         = $00000004;
  D3DCS_BOTTOM      = $00000008;
  D3DCS_FRONT       = $00000010;
  D3DCS_BACK        = $00000020;
  D3DCS_PLANE0      = $00000040;
  D3DCS_PLANE1      = $00000080;
  D3DCS_PLANE2      = $00000100;
  D3DCS_PLANE3      = $00000200;
  D3DCS_PLANE4      = $00000400;
  D3DCS_PLANE5      = $00000800;

  D3DCS_ALL = (D3DCS_LEFT or D3DCS_RIGHT or D3DCS_TOP or
              D3DCS_BOTTOM or D3DCS_FRONT or D3DCS_BACK or
              D3DCS_PLANE0 or D3DCS_PLANE1 or D3DCS_PLANE2 or
              D3DCS_PLANE3 or D3DCS_PLANE4 or D3DCS_PLANE5);


type
  PD3DClipStatus8 = ^TD3DClipStatus8;
  TD3DClipStatus8 = packed record
    ClipUnion        : LongWord;
    ClipIntersection : LongWord;
  end;

  PD3DMaterial8 = ^TD3DMaterial8;
  TD3DMaterial8 = packed record
    Diffuse  : TD3DColorValue;  (* Diffuse color RGBA *)
    Ambient  : TD3DColorValue;  (* Ambient color RGB *)
    Specular : TD3DColorValue;  (* Specular 'shininess' *)
    Emissive : TD3DColorValue;  (* Emissive color RGB *)
    Power    : Single;          (* Sharpness if specular highlight *)
  end;

//{$IFDEF D6UP}({$ELSE}LongWord;{$ENDIF}



type TD3DLightType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DLIGHT_POINT          = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DLIGHT_SPOT           = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DLIGHT_DIRECTIONAL    = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DLIGHT_FORCE_DWORD    = $7fffffff{$IFNDEF NOENUMS}){$ENDIF}; (* force 32-bit size enum *)

type
  PD3DLight8 = ^TD3DLight8;
  TD3DLight8 = packed record
    _Type        : TD3DLightType;   (* Type of light source *) //D3DLIGHTTYPE
    Diffuse      : TD3DColorValue;  (* Diffuse color of light *)
    Specular     : TD3DColorValue;  (* Specular color of light *)
    Ambient      : TD3DColorValue;  (* Ambient color of light *)
    Position     : TD3DVector;      (* Position in world space *)
    Direction    : TD3DVector;      (* Direction in world space *)
    Range        : Single;          (* Cutoff range *)
    Falloff      : Single;          (* Falloff *)
    Attenuation0 : Single;          (* Constant attenuation *)
    Attenuation1 : Single;          (* Linear attenuation *)
    Attenuation2 : Single;          (* Quadratic attenuation *)
    Theta        : Single;          (* Inner angle of spotlight cone *)
    Phi          : Single;          (* Outer angle of spotlight cone *)
  end;

(* Options for clearing *)
const
  D3DCLEAR_TARGET  = $00000001;  (* Clear target surface *)
  D3DCLEAR_ZBUFFER = $00000002;  (* Clear target z buffer *)
  D3DCLEAR_STENCIL = $00000004;  (* Clear stencil planes *)

(* The following defines the rendering states *)

type TD3DShadeMode = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DSHADE_FLAT               = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DSHADE_GOURAUD            = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DSHADE_PHONG              = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DSHADE_FORCE_DWORD        = $7fffffff{$IFNDEF NOENUMS}){$ENDIF}; (* force 32-bit size enum *)

type TD3DFillMode = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DFILL_POINT               = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFILL_WIREFRAME           = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFILL_SOLID               = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFILL_FORCE_DWORD         = $7fffffff{$IFNDEF NOENUMS}){$ENDIF}; (* force 32-bit size enum *)

type
  PD3DLinePattern = ^TD3DLinePattern;
  TD3DLinePattern = packed record
    wRepeatFactor : Word;
    wLinePattern  : Word;
  end;

type TD3DBlend = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DBLEND_ZERO               = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLEND_ONE                = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLEND_SRCCOLOR           = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLEND_INVSRCCOLOR        = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLEND_SRCALPHA           = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLEND_INVSRCALPHA        = 6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLEND_DESTALPHA          = 7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLEND_INVDESTALPHA       = 8{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLEND_DESTCOLOR          = 9{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLEND_INVDESTCOLOR       = 10{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLEND_SRCALPHASAT        = 11{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLEND_BOTHSRCALPHA       = 12{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLEND_BOTHINVSRCALPHA    = 13{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLEND_FORCE_DWORD        = $7fffffff{$IFNDEF NOENUMS}){$ENDIF}; (* force 32-bit size enum *)

type TD3DBLendOp = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DBLENDOP_ADD              = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLENDOP_SUBTRACT         = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLENDOP_REVSUBTRACT      = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLENDOP_MIN              = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLENDOP_MAX              = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBLENDOP_FORCE_DWORD      = $7fffffff{$IFNDEF NOENUMS}){$ENDIF}; (* force 32-bit size enum *)

type TD3DTextureAddress = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DTADDRESS_WRAP            = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DTADDRESS_MIRROR          = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DTADDRESS_CLAMP           = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DTADDRESS_BORDER          = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DTADDRESS_MIRRORONCE      = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DTADDRESS_FORCE_DWORD     = $7fffffff{$IFNDEF NOENUMS}){$ENDIF}; (* force 32-bit size enum *)

type TD3DCull = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DCULL_NONE                = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DCULL_CW                  = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DCULL_CCW                 = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DCULL_FORCE_DWORD         = $7fffffff{$IFNDEF NOENUMS}){$ENDIF}; (* force 32-bit size enum *)

type TD3DCmpFunc = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DCMP_NEVER                = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DCMP_LESS                 = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DCMP_EQUAL                = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DCMP_LESSEQUAL            = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DCMP_GREATER              = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DCMP_NOTEQUAL             = 6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DCMP_GREATEREQUAL         = 7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DCMP_ALWAYS               = 8{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DCMP_FORCE_DWORD          = $7fffffff{$IFNDEF NOENUMS}){$ENDIF}; (* force 32-bit size enum *)

type TD3DStencilOp = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DSTENCILOP_KEEP           = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DSTENCILOP_ZERO           = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DSTENCILOP_REPLACE        = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DSTENCILOP_INCRSAT        = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DSTENCILOP_DECRSAT        = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DSTENCILOP_INVERT         = 6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DSTENCILOP_INCR           = 7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DSTENCILOP_DECR           = 8{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DSTENCILOP_FORCE_DWORD    = $7fffffff{$IFNDEF NOENUMS}){$ENDIF}; (* force 32-bit size enum *)

type TD3DFogMode = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DFOG_NONE                 = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFOG_EXP                  = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFOG_EXP2                 = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFOG_LINEAR               = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFOG_FORCE_DWORD          = $7fffffff{$IFNDEF NOENUMS}){$ENDIF}; (* force 32-bit size enum *)

type TD3DZBufferType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DZB_FALSE                 = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DZB_TRUE                  = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Z buffering
       D3DZB_USEW                  = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // W buffering
       D3DZB_FORCE_DWORD           = $7fffffff{$IFNDEF NOENUMS}){$ENDIF}; (* force 32-bit size enum *)

// Primitives supported by draw-primitive API
type TD3DPrimitiveType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DPT_POINTLIST             = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DPT_LINELIST              = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DPT_LINESTRIP             = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DPT_TRIANGLELIST          = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DPT_TRIANGLESTRIP         = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DPT_TRIANGLEFAN           = 6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DPT_FORCE_DWORD           = $7fffffff{$IFNDEF NOENUMS}){$ENDIF}; (* force 32-bit size enum *)

type TD3DTransformStateType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DTS_VIEW          = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DTS_PROJECTION    = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DTS_TEXTURE0      = 16{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DTS_TEXTURE1      = 17{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DTS_TEXTURE2      = 18{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DTS_TEXTURE3      = 19{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DTS_TEXTURE4      = 20{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DTS_TEXTURE5      = 21{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DTS_TEXTURE6      = 22{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DTS_TEXTURE7      = 23{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DTS_WORLD         = 0 + 256{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // D3DTS_WORLDMATRIX(0)
       D3DTS_WORLD1        = 1 + 256{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // D3DTS_WORLDMATRIX(1)
       D3DTS_WORLD2        = 2 + 256{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // D3DTS_WORLDMATRIX(2)
       D3DTS_WORLD3        = 3 + 256{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // D3DTS_WORLDMATRIX(3)

       D3DTS_FORCE_DWORD   = $7fffffff{$IFNDEF NOENUMS}){$ENDIF}; (* force 32-bit size enum *)

function D3DTS_WORLDMATRIX(index : LongWord) : TD3DTransformStateType; // (D3DTRANSFORMSTATETYPE)(index + 256)

type TD3DRenderStateType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DRS_ZENABLE                   = 7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    (* D3DZBUFFERTYPE (or TRUE/FALSE for legacy) *)
       D3DRS_FILLMODE                  = 8{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    (* D3DFILL_MODE        *)
       D3DRS_SHADEMODE                 = 9{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    (* D3DSHADEMODE *)
       D3DRS_LINEPATTERN               = 10{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DLINEPATTERN *)
       D3DRS_ZWRITEENABLE              = 14{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* TRUE to enable z writes *)
       D3DRS_ALPHATESTENABLE           = 15{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* TRUE to enable alpha tests *)
       D3DRS_LASTPIXEL                 = 16{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* TRUE for last-pixel on lines *)
       D3DRS_SRCBLEND                  = 19{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DBLEND *)
       D3DRS_DESTBLEND                 = 20{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DBLEND *)
       D3DRS_CULLMODE                  = 22{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DCULL *)
       D3DRS_ZFUNC                     = 23{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DCMPFUNC *)
       D3DRS_ALPHAREF                  = 24{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DFIXED *)
       D3DRS_ALPHAFUNC                 = 25{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DCMPFUNC *)
       D3DRS_DITHERENABLE              = 26{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* TRUE to enable dithering *)
       D3DRS_ALPHABLENDENABLE          = 27{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* TRUE to enable alpha blending *)
       D3DRS_FOGENABLE                 = 28{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* TRUE to enable fog blending *)
       D3DRS_SPECULARENABLE            = 29{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* TRUE to enable specular *)
       D3DRS_ZVISIBLE                  = 30{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* TRUE to enable z checking *)
       D3DRS_FOGCOLOR                  = 34{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DCOLOR *)
       D3DRS_FOGTABLEMODE              = 35{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DFOGMODE *)
       D3DRS_FOGSTART                  = 36{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* Fog start (for both vertex and pixel fog) *)
       D3DRS_FOGEND                    = 37{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* Fog end      *)
       D3DRS_FOGDENSITY                = 38{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* Fog density  *)
       D3DRS_EDGEANTIALIAS             = 40{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* TRUE to enable edge antialiasing *)
       D3DRS_ZBIAS                     = 47{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* LONG Z bias *)
       D3DRS_RANGEFOGENABLE            = 48{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* Enables range-based fog *)
       D3DRS_STENCILENABLE             = 52{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* BOOL enable/disable stenciling *)
       D3DRS_STENCILFAIL               = 53{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DSTENCILOP to do if stencil test fails *)
       D3DRS_STENCILZFAIL              = 54{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DSTENCILOP to do if stencil test passes and Z test fails *)
       D3DRS_STENCILPASS               = 55{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DSTENCILOP to do if both stencil and Z tests pass *)
       D3DRS_STENCILFUNC               = 56{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DCMPFUNC fn.  Stencil Test passes if ((ref & mask) stencilfn (stencil & mask)) is true *)
       D3DRS_STENCILREF                = 57{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* Reference value used in stencil test *)
       D3DRS_STENCILMASK               = 58{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* Mask value used in stencil test *)
       D3DRS_STENCILWRITEMASK          = 59{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* Write mask applied to values written to stencil buffer *)
       D3DRS_TEXTUREFACTOR             = 60{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DCOLOR used for multi-texture blend *)
       D3DRS_WRAP0                     = 128{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* wrap for 1st texture coord. set *)
       D3DRS_WRAP1                     = 129{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* wrap for 2nd texture coord. set *)
       D3DRS_WRAP2                     = 130{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* wrap for 3rd texture coord. set *)
       D3DRS_WRAP3                     = 131{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* wrap for 4th texture coord. set *)
       D3DRS_WRAP4                     = 132{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* wrap for 5th texture coord. set *)
       D3DRS_WRAP5                     = 133{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* wrap for 6th texture coord. set *)
       D3DRS_WRAP6                     = 134{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* wrap for 7th texture coord. set *)
       D3DRS_WRAP7                     = 135{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* wrap for 8th texture coord. set *)
       D3DRS_CLIPPING                  = 136{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRS_LIGHTING                  = 137{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRS_AMBIENT                   = 139{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRS_FOGVERTEXMODE             = 140{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRS_COLORVERTEX               = 141{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRS_LOCALVIEWER               = 142{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRS_NORMALIZENORMALS          = 143{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRS_DIFFUSEMATERIALSOURCE     = 145{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRS_SPECULARMATERIALSOURCE    = 146{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRS_AMBIENTMATERIALSOURCE     = 147{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRS_EMISSIVEMATERIALSOURCE    = 148{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRS_VERTEXBLEND               = 151{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRS_CLIPPLANEENABLE           = 152{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRS_SOFTWAREVERTEXPROCESSING  = 153{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRS_POINTSIZE                 = 154{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* float point size *)
       D3DRS_POINTSIZE_MIN             = 155{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* float point size min threshold *)
       D3DRS_POINTSPRITEENABLE         = 156{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* BOOL point texture coord control *)
       D3DRS_POINTSCALEENABLE          = 157{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* BOOL point size scale enable *)
       D3DRS_POINTSCALE_A              = 158{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* float point attenuation A value *)
       D3DRS_POINTSCALE_B              = 159{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* float point attenuation B value *)
       D3DRS_POINTSCALE_C              = 160{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* float point attenuation C value *)
       D3DRS_MULTISAMPLEANTIALIAS      = 161{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // BOOL - set to do FSAA with multisample buffer *)
       D3DRS_MULTISAMPLEMASK           = 162{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // DWORD - per-sample enable/disable
       D3DRS_PATCHEDGESTYLE            = 163{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // Sets whether patch edges will use float style tessellation
       D3DRS_PATCHSEGMENTS             = 164{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // Number of segments per edge when drawing patches
       D3DRS_DEBUGMONITORTOKEN         = 165{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // DEBUG ONLY - token to debug monitor
       D3DRS_POINTSIZE_MAX             = 166{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* float point size max threshold *)
       D3DRS_INDEXEDVERTEXBLENDENABLE  = 167{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRS_COLORWRITEENABLE          = 168{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // per-channel write enable
       D3DRS_TWEENFACTOR               = 170{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // float tween factor
       D3DRS_BLENDOP                   = 171{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // D3DBLENDOP setting
{$IFNDEF DX8}
       D3DRS_POSITIONORDER             = 172{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // NPatch position interpolation order. D3DORDER_LINEAR or D3DORDER_CUBIC (default)
       D3DRS_NORMALORDER               = 173{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // NPatch normal interpolation order. D3DORDER_LINEAR (default) or D3DORDER_QUADRATIC
{$ENDIF}
       D3DRS_FORCE_DWORD               = $7fffffff{$IFNDEF NOENUMS}){$ENDIF}; (* force 32-bit size enum *)

// Values for material source
type TD3DMaterialColorSource = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DMCS_MATERIAL         = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // Color from material is used
       D3DMCS_COLOR1           = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // Diffuse vertex color is used
       D3DMCS_COLOR2           = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // Specular vertex color is used
       D3DMCS_FORCE_DWORD      = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};   // force 32-bit size enum


const
// Bias to apply to the texture coordinate set to apply a wrap to.
  D3DRENDERSTATE_WRAPBIAS  = 128;

(* Flags to construct the WRAP render states *)
  D3DWRAP_U = $00000001;
  D3DWRAP_V = $00000002;
  D3DWRAP_W = $00000004;

(* Flags to construct the WRAP render states for 1D thru 4D texture coordinates *)
  D3DWRAPCOORD_0  = $00000001;    // same as D3DWRAP_U
  D3DWRAPCOORD_1  = $00000002;    // same as D3DWRAP_V
  D3DWRAPCOORD_2  = $00000004;    // same as D3DWRAP_W
  D3DWRAPCOORD_3  = $00000008;

(* Flags to construct D3DRS_COLORWRITEENABLE *)
  D3DCOLORWRITEENABLE_RED   = 1;  // (1L<<0)
  D3DCOLORWRITEENABLE_GREEN = 2;  // (1L<<1)
  D3DCOLORWRITEENABLE_BLUE  = 4;  // (1L<<2)
  D3DCOLORWRITEENABLE_ALPHA = 8;  // (1L<<3)

(*
 * State enumerants for per-stage texture processing.
 *)
type TD3DTextureStageStateType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DTSS_COLOROP               =  1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTEXTUREOP - per-stage blending controls for color channels *)
       D3DTSS_COLORARG1             =  2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTA_* (texture arg) *)
       D3DTSS_COLORARG2             =  3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTA_* (texture arg) *)
       D3DTSS_ALPHAOP               =  4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTEXTUREOP - per-stage blending controls for alpha channel *)
       D3DTSS_ALPHAARG1             =  5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTA_* (texture arg) *)
       D3DTSS_ALPHAARG2             =  6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTA_* (texture arg) *)
       D3DTSS_BUMPENVMAT00          =  7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* float (bump mapping matrix) *)
       D3DTSS_BUMPENVMAT01          =  8{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* float (bump mapping matrix) *)
       D3DTSS_BUMPENVMAT10          =  9{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* float (bump mapping matrix) *)
       D3DTSS_BUMPENVMAT11          = 10{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* float (bump mapping matrix) *)
       D3DTSS_TEXCOORDINDEX         = 11{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* identifies which set of texture coordinates index this texture *)
       D3DTSS_ADDRESSU              = 13{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTEXTUREADDRESS for U coordinate *)
       D3DTSS_ADDRESSV              = 14{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTEXTUREADDRESS for V coordinate *)
       D3DTSS_BORDERCOLOR           = 15{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DCOLOR *)
       D3DTSS_MAGFILTER             = 16{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTEXTUREFILTER filter to use for magnification *)
       D3DTSS_MINFILTER             = 17{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTEXTUREFILTER filter to use for minification *)
       D3DTSS_MIPFILTER             = 18{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTEXTUREFILTER filter to use between mipmaps during minification *)
       D3DTSS_MIPMAPLODBIAS         = 19{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* float Mipmap LOD bias *)
       D3DTSS_MAXMIPLEVEL           = 20{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* DWORD 0..(n-1) LOD index of largest map to use (0 == largest) *)
       D3DTSS_MAXANISOTROPY         = 21{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* DWORD maximum anisotropy *)
       D3DTSS_BUMPENVLSCALE         = 22{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* float scale for bump map luminance *)
       D3DTSS_BUMPENVLOFFSET        = 23{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* float offset for bump map luminance *)
       D3DTSS_TEXTURETRANSFORMFLAGS = 24{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTEXTURETRANSFORMFLAGS controls texture transform *)
       D3DTSS_ADDRESSW              = 25{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTEXTUREADDRESS for W coordinate *)
       D3DTSS_COLORARG0             = 26{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTA_* third arg for triadic ops *)
       D3DTSS_ALPHAARG0             = 27{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTA_* third arg for triadic ops *)
       D3DTSS_RESULTARG             = 28{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTA_* arg for result (CURRENT or TEMP) *)
       D3DTSS_FORCE_DWORD           = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};     (* force 32-bit size enum *)

const
// Values; used with D3DTSS_TEXCOORDINDEX; to specify that the vertex data(position
// and normal in the camera space) should be taken as texture coordinates
// Low 16 bits are used to specify texture coordinate index; to take the WRAP mode from
  D3DTSS_TCI_PASSTHRU                    = $00000000;
  D3DTSS_TCI_CAMERASPACENORMAL           = $00010000;
  D3DTSS_TCI_CAMERASPACEPOSITION         = $00020000;
  D3DTSS_TCI_CAMERASPACEREFLECTIONVECTOR = $00030000;

(* Enumerations for COLOROP and ALPHAOP texture blending operations set in
 * texture processing stage controls in D3DTSS. *)

type TD3DTextureOp = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}

       // Control
       D3DTOP_DISABLE                   = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   // disables stage
       D3DTOP_SELECTARG1                = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   // the default
       D3DTOP_SELECTARG2                = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       // Modulate
       D3DTOP_MODULATE                  = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   // multiply args together
       D3DTOP_MODULATE2X                = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   // multiply and  1 bit
       D3DTOP_MODULATE4X                = 6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   // multiply and  2 bits

       // Add
       D3DTOP_ADD                       = 7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   // add arguments together
       D3DTOP_ADDSIGNED                 = 8{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   // add with -0.5 bias
       D3DTOP_ADDSIGNED2X               = 9{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   // as above but left  1 bit
       D3DTOP_SUBTRACT                  = 10{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // Arg1 - Arg2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} with no saturation
       D3DTOP_ADDSMOOTH                 = 11{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // add 2 args{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} subtract product
                                              // Arg1 + Arg2 - Arg1*Arg2
                                              // = Arg1 + (1-Arg1)*Arg2

       // Linear alpha blend: Arg1*(Alpha) + Arg2*(1-Alpha)
       D3DTOP_BLENDDIFFUSEALPHA         = 12{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // iterated alpha
       D3DTOP_BLENDTEXTUREALPHA         = 13{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // texture alpha
       D3DTOP_BLENDFACTORALPHA          = 14{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // alpha from D3DRS_TEXTUREFACTOR

       // Linear alpha blend with pre-multiplied arg1 input: Arg1 + Arg2*(1-Alpha)
       D3DTOP_BLENDTEXTUREALPHAPM       = 15{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // texture alpha
       D3DTOP_BLENDCURRENTALPHA         = 16{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // by alpha of current color

       // Specular mapping
       D3DTOP_PREMODULATE               = 17{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // modulate with next texture before use
       D3DTOP_MODULATEALPHA_ADDCOLOR    = 18{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // Arg1.RGB + Arg1.A*Arg2.RGB
                                              // COLOROP only
       D3DTOP_MODULATECOLOR_ADDALPHA    = 19{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // Arg1.RGB*Arg2.RGB + Arg1.A
                                              // COLOROP only
       D3DTOP_MODULATEINVALPHA_ADDCOLOR = 20{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // (1-Arg1.A)*Arg2.RGB + Arg1.RGB
                                              // COLOROP only
       D3DTOP_MODULATEINVCOLOR_ADDALPHA = 21{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // (1-Arg1.RGB)*Arg2.RGB + Arg1.A
                                              // COLOROP only

       // Bump mapping
       D3DTOP_BUMPENVMAP                = 22{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // per pixel env map perturbation
       D3DTOP_BUMPENVMAPLUMINANCE       = 23{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // with luminance channel

       // This can do either diffuse or specular bump mapping with correct input.
       // Performs the function (Arg1.R*Arg2.R + Arg1.G*Arg2.G + Arg1.B*Arg2.B)
       // where each component has been scaled and offset to make it signed.
       // The result is replicated into all four (including alpha) channels.
       // This is a valid COLOROP only.
       D3DTOP_DOTPRODUCT3               = 24{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       // Triadic ops
       D3DTOP_MULTIPLYADD               = 25{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // Arg0 + Arg1*Arg2
       D3DTOP_LERP                      = 26{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // (Arg0)*Arg1 + (1-Arg0)*Arg2

       D3DTOP_FORCE_DWORD               = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};

const
(* Values for COLORARG0;1;2; ALPHAARG0;1;2; and RESULTARG texture blending
 * operations set in texture processing stage controls in D3DRENDERSTATE. *)
  D3DTA_SELECTMASK        = $0000000f;  // mask for arg selector
  D3DTA_DIFFUSE           = $00000000;  // select diffuse color (read only)
  D3DTA_CURRENT           = $00000001;  // select stage destination register (read/write)
  D3DTA_TEXTURE           = $00000002;  // select texture color (read only)
  D3DTA_TFACTOR           = $00000003;  // select D3DRS_TEXTUREFACTOR (read only)
  D3DTA_SPECULAR          = $00000004;  // select specular color (read only)
  D3DTA_TEMP              = $00000005;  // select temporary register color (read/write)
  D3DTA_COMPLEMENT        = $00000010;  // take 1.0 - x (read modifier)
  D3DTA_ALPHAREPLICATE    = $00000020;  // replicate alpha to color components (read modifier)


// Values for D3DTSS_***FILTER texture stage states

type TD3DTextureFilterType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DTEXF_NONE            = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // filtering disabled (valid for mip filter only)
       D3DTEXF_POINT           = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // nearest
       D3DTEXF_LINEAR          = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // linear interpolation
       D3DTEXF_ANISOTROPIC     = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // anisotropic
       D3DTEXF_FLATCUBIC       = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // cubic
       D3DTEXF_GAUSSIANCUBIC   = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // different cubic kernel
       D3DTEXF_FORCE_DWORD     = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};   // force 32-bit size enum

const

(* Bits for Flags in ProcessVertices call *)
  D3DPV_DONOTCOPYDATA  = 1 shl 0;

//-------------------------------------------------------------------

// Flexible vertex format bits
//
  D3DFVF_RESERVED0        = $001;
  D3DFVF_POSITION_MASK    = $00E;
  D3DFVF_XYZ              = $002;
  D3DFVF_XYZRHW           = $004;
  D3DFVF_XYZB1            = $006;
  D3DFVF_XYZB2            = $008;
  D3DFVF_XYZB3            = $00a;
  D3DFVF_XYZB4            = $00c;
  D3DFVF_XYZB5            = $00e;

  D3DFVF_NORMAL           = $010;
  D3DFVF_PSIZE            = $020;
  D3DFVF_DIFFUSE          = $040;
  D3DFVF_SPECULAR         = $080;

  D3DFVF_TEXCOUNT_MASK    = $f00;
  D3DFVF_TEXCOUNT_SHIFT   = 8;
  D3DFVF_TEX0             = $000;
  D3DFVF_TEX1             = $100;
  D3DFVF_TEX2             = $200;
  D3DFVF_TEX3             = $300;
  D3DFVF_TEX4             = $400;
  D3DFVF_TEX5             = $500;
  D3DFVF_TEX6             = $600;
  D3DFVF_TEX7             = $700;
  D3DFVF_TEX8             = $800;

  D3DFVF_LASTBETA_UBYTE4  = $1000;

  D3DFVF_RESERVED2        = $E000;  // 4 reserved bits

//---------------------------------------------------------------------
// Vertex Shaders
//

(*

Vertex Shader Declaration

The declaration portion of a vertex shader defines the static external
interface of the shader.  The information in the declaration includes:

- Assignments of vertex shader input registers to data streams.  These
assignments bind a specific vertex register to a single component within a
vertex stream.  A vertex stream element is identified by a byte offset
within the stream and a type.  The type specifies the arithmetic data type
plus the dimensionality (1; 2; 3; or 4 values).  Stream data which is
less than 4 values are always expanded out to 4 values with zero or more
0.F values and one 1.F value.

- Assignment of vertex shader input registers to implicit data from the
primitive tessellator.  This controls the loading of vertex data which is
not loaded from a stream; but rather is generated during primitive
tessellation prior to the vertex shader.

- Loading data into the constant memory at the time a shader is set as the
current shader.  Each token specifies values for one or more contiguous 4
DWORD constant registers.  This allows the shader to update an arbitrary
subset of the constant memory; overwriting the device state (which
contains the current values of the constant memory).  Note that these
values can be subsequently overwritten (between DrawPrimitive calls)
during the time a shader is bound to a device via the
SetVertexShaderConstant method.


Declaration arrays are single-dimensional arrays of DWORDs composed of
multiple tokens each of which is one or more DWORDs.  The single-DWORD
token value = $FFFFFFFF is a special token used to indicate the end of the
declaration array.  The single DWORD token value = $00000000 is a NOP token
with is ignored during the declaration parsing.  Note that = $00000000 is a
valid value for DWORDs following the first DWORD for multiple word tokens.

[31:29] TokenType
    = $0 - NOP (requires all DWORD bits to be zero)
    = $1 - stream selector
    = $2 - stream data definition (map to vertex input memory)
    = $3 - vertex input memory from tessellator
    = $4 - constant memory from shader
    = $5 - extension
    = $6 - reserved
    = $7 - end-of-array (requires all DWORD bits to be 1)

NOP Token (single DWORD token)
    [31:29] = $0
    [28:00] = $0

Stream Selector (single DWORD token)
    [31:29] = $1
    [28]    indicates whether this is a tessellator stream
    [27:04] = $0
    [03:00] stream selector (0..15)

Stream Data Definition (single DWORD token)
    Vertex Input Register Load
      [31:29] = $2
      [28]    = $0
      [27:20] = $0
      [19:16] type (dimensionality and data type)
      [15:04] = $0
      [03:00] vertex register address (0..15)
    Data Skip (no register load)
      [31:29] = $2
      [28]    = $1
      [27:20] = $0
      [19:16] count of DWORDS to skip over (0..15)
      [15:00] = $0
    Vertex Input Memory from Tessellator Data (single DWORD token)
      [31:29] = $3
      [28]    indicates whether data is normals or u/v
      [27:24] = $0
      [23:20] vertex register address (0..15)
      [19:16] type (dimensionality)
      [15:04] = $0
      [03:00] vertex register address (0..15)

Constant Memory from Shader (multiple DWORD token)
    [31:29] = $4
    [28:25] count of 4*DWORD constants to load (0..15)
    [24:07] = $0
    [06:00] constant memory address (0..95)

Extension Token (single or multiple DWORD token)
    [31:29] = $5
    [28:24] count of additional DWORDs in token (0..31)
    [23:00] extension-specific information

End-of-array token (single DWORD token)
    [31:29] = $7
    [28:00] = $1fffffff

The stream selector token must be immediately followed by a contiguous set of stream data definition tokens.  This token sequence fully defines that stream; including the set of elements within the stream; the order in which the elements appear; the type of each element; and the vertex register into which to load an element.
Streams are allowed to include data which is not loaded into a vertex register; thus allowing data which is not used for this shader to exist in the vertex stream.  This skipped data is defined only by a count of DWORDs to skip over; since the type information is irrelevant.
The token sequence:
Stream Select: stream=0
Stream Data Definition (Load): type=FLOAT3; register=3
Stream Data Definition (Load): type=FLOAT3; register=4
Stream Data Definition (Skip): count=2
Stream Data Definition (Load): type=FLOAT2; register=7

defines stream zero to consist of 4 elements; 3 of which are loaded into registers and the fourth skipped over.  Register 3 is loaded with the first three DWORDs in each vertex interpreted as FLOAT data.  Register 4 is loaded with the 4th; 5th; and 6th DWORDs interpreted as FLOAT data.  The next two DWORDs (7th and 8th) are skipped over and not loaded into any vertex input register.   Register 7 is loaded with the 9th and 10th DWORDS interpreted as FLOAT data.
Placing of tokens other than NOPs between the Stream Selector and Stream Data Definition tokens is disallowed.

*)
type TD3DVSDTokenType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DVSD_TOKEN_NOP         = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // NOP or extension
       D3DVSD_TOKEN_STREAM      = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // stream selector
       D3DVSD_TOKEN_STREAMDATA  = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // stream data definition (map to vertex input memory)
       D3DVSD_TOKEN_TESSELLATOR = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // vertex input memory from tessellator
       D3DVSD_TOKEN_CONSTMEM    = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // constant memory from shader
       D3DVSD_TOKEN_EXT         = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // extension
       D3DVSD_TOKEN_END         = 7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // end-of-array (requires all DWORD bits to be 1)

       D3DVSD_TOKENTYPESHIFT    = 29{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DVSD_TOKENTYPEMASK     = 7 shl D3DVSD_TOKENTYPESHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DVSD_STREAMNUMBERSHIFT = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DVSD_STREAMNUMBERMASK  = $F shl D3DVSD_STREAMNUMBERSHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DVSD_DATALOADTYPESHIFT = 28{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DVSD_DATALOADTYPEMASK  = $1 shl D3DVSD_DATALOADTYPESHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DVSD_DATATYPESHIFT     = 16{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DVSD_DATATYPEMASK      = $F shl D3DVSD_DATATYPESHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DVSD_SKIPCOUNTSHIFT    = 16{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DVSD_SKIPCOUNTMASK     = $F shl D3DVSD_SKIPCOUNTSHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DVSD_VERTEXREGSHIFT    = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DVSD_VERTEXREGMASK     = $1F shl D3DVSD_VERTEXREGSHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DVSD_VERTEXREGINSHIFT  = 20{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DVSD_VERTEXREGINMASK   = $F shl D3DVSD_VERTEXREGINSHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DVSD_CONSTCOUNTSHIFT   = 25{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DVSD_CONSTCOUNTMASK    = $F shl D3DVSD_CONSTCOUNTSHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DVSD_CONSTADDRESSSHIFT = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DVSD_CONSTADDRESSMASK  = $7F shl D3DVSD_CONSTADDRESSSHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DVSD_CONSTRSSHIFT      = 16{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DVSD_CONSTRSMASK       = $1FFF shl D3DVSD_CONSTRSSHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DVSD_EXTCOUNTSHIFT     = 24{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DVSD_EXTCOUNTMASK      = $1F shl D3DVSD_EXTCOUNTSHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DVSD_EXTINFOSHIFT      = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DVSD_EXTINFOMASK       = $FFFFFF shl D3DVSD_EXTINFOSHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       // Set tessellator stream
       D3DVSD_STREAMTESSSHIFT   = 28{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DVSD_STREAMTESSMASK    = 1 shl D3DVSD_STREAMTESSSHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DVSD_STREAM_TESS       = ((D3DVSD_TOKEN_STREAM shl D3DVSD_TOKENTYPESHIFT) and D3DVSD_TOKENTYPEMASK) or D3DVSD_STREAMTESSMASK{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DVSD_FORCE_DWORD       = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};   // force 32-bit size enum

  function D3DVSD_MAKETOKENTYPE(tokenType : TD3DVSDTokenType) : TD3DVSDTokenType;

// macros for generation of CreateVertexShader Declaration token array

// Set current stream
// _StreamNumber [0..(MaxStreams-1)] stream to get data from
//
  function D3DVSD_STREAM(_StreamNumber : LongWord) : TD3DVSDTokenType;

type TD3DVSD_TokenType = TD3DVSDTokenType;

// bind single vertex register to vertex element from vertex stream
//
// _VertexRegister [0..15] address of the vertex register
// _Type [D3DVSDT_*] dimensionality and arithmetic data type

  function D3DVSD_REG( _VertexRegister, _Type : LongWord) : TD3DVSDTokenType;

// Skip _DWORDCount DWORDs in vertex
//
  function D3DVSD_SKIP(_DWORDCount : LongWord) : TD3DVSDTokenType;

// load data into vertex shader constant memory
//
// _ConstantAddress [0..95] - address of constant array to begin filling data
// _Count [0..15] - number of constant vectors to load (4 DWORDs each)
// followed by 4*_Count DWORDS of data
//
  function D3DVSD_CONST( _ConstantAddress, _Count : LongWord) : TD3DVSDTokenType;

// enable tessellator generated normals
//
// _VertexRegisterIn  [0..15] address of vertex register whose input stream
//                            will be used in normal computation
// _VertexRegisterOut [0..15] address of vertex register to output the normal to
//
  function D3DVSD_TESSNORMAL( _VertexRegisterIn, _VertexRegisterOut : LongWord) : TD3DVSDTokenType;

// enable tessellator generated surface parameters
//
// _VertexRegister [0..15] address of vertex register to output parameters
//
  function D3DVSD_TESSUV( _VertexRegister : LongWord) : TD3DVSDTokenType;

// Generates END token
//
const
  D3DVSD_END = $FFFFFFFF;

// Generates NOP token
  D3DVSD_NOP = $00000000;

// bit declarations for _Type fields
  D3DVSDT_FLOAT1      = $00;    // 1D float expanded to (value; 0.; 0.; 1.)
  D3DVSDT_FLOAT2      = $01;    // 2D float expanded to (value; value; 0.; 1.)
  D3DVSDT_FLOAT3      = $02;    // 3D float expanded to (value; value; value; 1.)
  D3DVSDT_FLOAT4      = $03;    // 4D float
  D3DVSDT_D3DCOLOR    = $04;    // 4D packed unsigned bytes mapped to 0. to 1. range
                                    // Input is in D3DCOLOR format (ARGB) expanded to (R; G; B; A)
  D3DVSDT_UBYTE4      = $05;    // 4D unsigned byte
  D3DVSDT_SHORT2      = $06;    // 2D signed short expanded to (value; value; 0.; 1.)
  D3DVSDT_SHORT4      = $07;    // 4D signed short

// assignments of vertex input registers for fixed function vertex shader
//
  D3DVSDE_POSITION      = 0;
  D3DVSDE_BLENDWEIGHT   = 1;
  D3DVSDE_BLENDINDICES  = 2;
  D3DVSDE_NORMAL        = 3;
  D3DVSDE_PSIZE         = 4;
  D3DVSDE_DIFFUSE       = 5;
  D3DVSDE_SPECULAR      = 6;
  D3DVSDE_TEXCOORD0     = 7;
  D3DVSDE_TEXCOORD1     = 8;
  D3DVSDE_TEXCOORD2     = 9;
  D3DVSDE_TEXCOORD3     = 10;
  D3DVSDE_TEXCOORD4     = 11;
  D3DVSDE_TEXCOORD5     = 12;
  D3DVSDE_TEXCOORD6     = 13;
  D3DVSDE_TEXCOORD7     = 14;
  D3DVSDE_POSITION2     = 15;
  D3DVSDE_NORMAL2       = 16;

// Maximum supported number of texture coordinate sets
  D3DDP_MAXTEXCOORD = 8;


//
// Instruction Token Bit Definitions
//
  D3DSI_OPCODE_MASK       = $0000FFFF;

type
  TD3DShaderInstructionOpcodeType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
  D3DSIO_NOP          = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}      // PS/VS
  D3DSIO_MOV          = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}      // PS/VS
  D3DSIO_ADD          = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}      // PS/VS
  D3DSIO_SUB          = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}      // PS
  D3DSIO_MAD          = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}      // PS/VS
  D3DSIO_MUL          = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}      // PS/VS
  D3DSIO_RCP          = 6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}      // VS
  D3DSIO_RSQ          = 7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}      // VS
  D3DSIO_DP3          = 8{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}      // PS/VS
  D3DSIO_DP4          = 9{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}      // PS/VS
  D3DSIO_MIN          = 10{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // VS
  D3DSIO_MAX          = 11{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // VS
  D3DSIO_SLT          = 12{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // VS
  D3DSIO_SGE          = 13{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // VS
  D3DSIO_EXP          = 14{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // VS
  D3DSIO_LOG          = 15{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // VS
  D3DSIO_LIT          = 16{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // VS
  D3DSIO_DST          = 17{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // VS
  D3DSIO_LRP          = 18{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_FRC          = 19{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // VS
  D3DSIO_M4x4         = 20{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // VS
  D3DSIO_M4x3         = 21{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // VS
  D3DSIO_M3x4         = 22{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // VS
  D3DSIO_M3x3         = 23{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // VS
  D3DSIO_M3x2         = 24{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // VS

  D3DSIO_TEXCOORD     = 64{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEXKILL      = 65{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEX          = 66{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEXBEM       = 67{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEXBEML      = 68{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEXREG2AR    = 69{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEXREG2GB    = 70{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEXM3x2PAD   = 71{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEXM3x2TEX   = 72{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEXM3x3PAD   = 73{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEXM3x3TEX   = 74{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEXM3x3DIFF  = 75{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEXM3x3SPEC  = 76{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEXM3x3VSPEC = 77{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_EXPP         = 78{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // VS
  D3DSIO_LOGP         = 79{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // VS
  D3DSIO_CND          = 80{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_DEF          = 81{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
{$IFNDEF DX8}
  D3DSIO_TEXREG2RGB   = 82{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEXDP3TEX    = 83{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEXM3x2DEPTH = 84{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEXDP3       = 85{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEXM3x3      = 86{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_TEXDEPTH     = 87{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_CMP          = 88{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS
  D3DSIO_BEM          = 89{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     // PS

  D3DSIO_PHASE        = $FFFD{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
{$ENDIF}
  D3DSIO_COMMENT      = $FFFE{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
  D3DSIO_END          = $FFFF{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

  D3DSIO_FORCE_DWORD  = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};     // force 32-bit size enum

type TD3DShader_Instruction_Opcode_Type = TD3DShaderInstructionOpcodeType;

// Co-Issue Instruction Modifier - if set then this instruction is to be
// issued in parallel with the previous instruction(s) for which this bit
// is not set.
const
  D3DSI_COISSUE           = $40000000;

//
// Parameter Token Bit Definitions
//

{$IFDEF DX8}
  D3DSP_REGNUM_MASK       = $00000FFF;
{$ELSE}
  D3DSP_REGNUM_MASK       = $00001FFF;
{$ENDIF}
// destination parameter write mask
  D3DSP_WRITEMASK_0       = $00010000;  // Component 0 (X;Red)
  D3DSP_WRITEMASK_1       = $00020000;  // Component 1 (Y;Green)
  D3DSP_WRITEMASK_2       = $00040000;  // Component 2 (Z;Blue)
  D3DSP_WRITEMASK_3       = $00080000;  // Component 3 (W;Alpha)
  D3DSP_WRITEMASK_ALL     = $000F0000;  // All Components

// destination parameter modifiers
  D3DSP_DSTMOD_SHIFT      = 20;
  D3DSP_DSTMOD_MASK       = $00F00000;

type TD3DShaderParamDSTModType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
  D3DSPDM_NONE        = 0 shl D3DSP_DSTMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // nop
  D3DSPDM_SATURATE    = 1 shl D3DSP_DSTMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // clamp to 0. to 1. range

// destination parameter
  D3DSP_DSTSHIFT_SHIFT    = 24{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
  D3DSP_DSTSHIFT_MASK     = $0F000000{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

// destination/source parameter register type
  D3DSP_REGTYPE_SHIFT     = 28{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
  D3DSP_REGTYPE_MASK      = $70000000{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

  D3DSPDM_FORCE_DWORD = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};                       // force 32-bit size enum

type TD3DShader_Param_DSTMod_Type = TD3DShaderParamDSTModType;

type TD3DShaderParamRegisterType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
  D3DSPR_TEMP        = 0 shl LongWord(D3DSP_REGTYPE_SHIFT){$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Temporary Register File
  D3DSPR_INPUT       = 1 shl LongWord(D3DSP_REGTYPE_SHIFT){$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Input Register File
  D3DSPR_CONST       = 2 shl LongWord(D3DSP_REGTYPE_SHIFT){$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Constant Register File
  D3DSPR_ADDR        = 3 shl LongWord(D3DSP_REGTYPE_SHIFT){$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Address Register (VS)
  D3DSPR_TEXTURE     = 3 shl LongWord(D3DSP_REGTYPE_SHIFT){$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Texture Register File (PS)
  D3DSPR_RASTOUT     = 4 shl LongWord(D3DSP_REGTYPE_SHIFT){$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Rasterizer Register File
  D3DSPR_ATTROUT     = 5 shl LongWord(D3DSP_REGTYPE_SHIFT){$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Attribute Output Register File
  D3DSPR_TEXCRDOUT   = 6 shl LongWord(D3DSP_REGTYPE_SHIFT){$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Texture Coordinate Output Register File
  D3DSPR_FORCE_DWORD = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};                                  // force 32-bit size enum

type TD3DShader_Param_Register_Type = TD3DShaderParamRegisterType;

// Register offsets in the Rasterizer Register File

type TD3DVSRastOutOffsets = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
  D3DSRO_POSITION    = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
  D3DSRO_FOG         = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
  D3DSRO_POINT_SIZE  = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
  D3DSRO_FORCE_DWORD = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};  // force 32-bit size enum

type TD3DVS_RastOut_Offsets = TD3DVSRastOutOffsets;

// Source operand addressing modes
const
  D3DVS_ADDRESSMODE_SHIFT = 13;
  D3DVS_ADDRESSMODE_MASK  = 1 shl D3DVS_ADDRESSMODE_SHIFT;

type TD3DVSAddressModeType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
  D3DVS_ADDRMODE_ABSOLUTE    = 0 shl D3DVS_ADDRESSMODE_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
  D3DVS_ADDRMODE_RELATIVE    = 1 shl D3DVS_ADDRESSMODE_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // Relative to register A0
  D3DVS_ADDRMODE_FORCE_DWORD = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};                            // force 32-bit size enum

type TD3DVS_AddressMode_Type = TD3DVSAddressModeType;

const  
// Source operand swizzle definitions
//
  D3DVS_SWIZZLE_SHIFT     = 16;
  D3DVS_SWIZZLE_MASK      = $00FF0000;

// The following bits define where to take component X from:

  D3DVS_X_X = 0 shl D3DVS_SWIZZLE_SHIFT;
  D3DVS_X_Y = 1 shl D3DVS_SWIZZLE_SHIFT;
  D3DVS_X_Z = 2 shl D3DVS_SWIZZLE_SHIFT;
  D3DVS_X_W = 3 shl D3DVS_SWIZZLE_SHIFT;

// The following bits define where to take component Y from:

  D3DVS_Y_X = 0 shl (D3DVS_SWIZZLE_SHIFT + 2);
  D3DVS_Y_Y = 1 shl (D3DVS_SWIZZLE_SHIFT + 2);
  D3DVS_Y_Z = 2 shl (D3DVS_SWIZZLE_SHIFT + 2);
  D3DVS_Y_W = 3 shl (D3DVS_SWIZZLE_SHIFT + 2);

// The following bits define where to take component Z from:

  D3DVS_Z_X = 0 shl (D3DVS_SWIZZLE_SHIFT + 4);
  D3DVS_Z_Y = 1 shl (D3DVS_SWIZZLE_SHIFT + 4);
  D3DVS_Z_Z = 2 shl (D3DVS_SWIZZLE_SHIFT + 4);
  D3DVS_Z_W = 3 shl (D3DVS_SWIZZLE_SHIFT + 4);

// The following bits define where to take component W from:

  D3DVS_W_X = 0 shl (D3DVS_SWIZZLE_SHIFT + 6);
  D3DVS_W_Y = 1 shl (D3DVS_SWIZZLE_SHIFT + 6);
  D3DVS_W_Z = 2 shl (D3DVS_SWIZZLE_SHIFT + 6);
  D3DVS_W_W = 3 shl (D3DVS_SWIZZLE_SHIFT + 6);

// Value when there is no swizzle (X is taken from X; Y is taken from Y;
// Z is taken from Z; W is taken from W
//
  D3DVS_NOSWIZZLE = D3DVS_X_X or D3DVS_Y_Y or D3DVS_Z_Z or D3DVS_W_W;

// source parameter swizzle
  D3DSP_SWIZZLE_SHIFT = 16;
  D3DSP_SWIZZLE_MASK  = $00FF0000;

  D3DSP_NOSWIZZLE = (0 shl (D3DSP_SWIZZLE_SHIFT + 0)) or
                    (1 shl (D3DSP_SWIZZLE_SHIFT + 2)) or
                    (2 shl (D3DSP_SWIZZLE_SHIFT + 4)) or
                    (3 shl (D3DSP_SWIZZLE_SHIFT + 6));

// pixel-shader swizzle ops
{$IFNDEF DX8}
  D3DSP_REPLICATERED = (0 shl (D3DSP_SWIZZLE_SHIFT + 0)) or
                       (0 shl (D3DSP_SWIZZLE_SHIFT + 2)) or
                       (0 shl (D3DSP_SWIZZLE_SHIFT + 4)) or
                       (0 shl (D3DSP_SWIZZLE_SHIFT + 6));

  D3DSP_REPLICATEGREEN = (1 shl (D3DSP_SWIZZLE_SHIFT + 0)) or
                         (1 shl (D3DSP_SWIZZLE_SHIFT + 2)) or
                         (1 shl (D3DSP_SWIZZLE_SHIFT + 4)) or
                         (1 shl (D3DSP_SWIZZLE_SHIFT + 6));

  D3DSP_REPLICATEBLUE = (2 shl (D3DSP_SWIZZLE_SHIFT + 0)) or
                        (2 shl (D3DSP_SWIZZLE_SHIFT + 2)) or
                        (2 shl (D3DSP_SWIZZLE_SHIFT + 4)) or
                        (2 shl (D3DSP_SWIZZLE_SHIFT + 6));
{$ENDIF}
  D3DSP_REPLICATEALPHA = (3 shl (D3DSP_SWIZZLE_SHIFT + 0)) or
                         (3 shl (D3DSP_SWIZZLE_SHIFT + 2)) or
                         (3 shl (D3DSP_SWIZZLE_SHIFT + 4)) or
                         (3 shl (D3DSP_SWIZZLE_SHIFT + 6));

// source parameter modifiers
  D3DSP_SRCMOD_SHIFT      = 24;
  D3DSP_SRCMOD_MASK       = $0F000000;

type TD3DShaderParamSRCModType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DSPSM_NONE        =  0 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // nop
       D3DSPSM_NEG         =  1 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // negate
       D3DSPSM_BIAS        =  2 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // bias
       D3DSPSM_BIASNEG     =  3 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // bias and negate
       D3DSPSM_SIGN        =  4 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // sign
       D3DSPSM_SIGNNEG     =  5 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // sign and negate
       D3DSPSM_COMP        =  6 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // complement
{$IFNDEF DX8}
       D3DSPSM_X2          =  7 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // *2
       D3DSPSM_X2NEG       =  8 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // *2 and negate
       D3DSPSM_DZ          =  9 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // divide through by z component
       D3DSPSM_DW          = 10 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // divide through by w component
{$ENDIF}
       D3DSPSM_FORCE_DWORD = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};                        // force 32-bit size enum

type TD3DShader_Param_SRCMod_Type = TD3DShaderParamSRCModType;
// pixel shader version token
  function D3DPS_VERSION(_Major, _Minor : LongWord) : LongWord;

// vertex shader version token
  function D3DVS_VERSION(_Major, _Minor : LongWord) : LongWord;

// extract major/minor from version cap
  function D3DSHADER_VERSION_MAJOR(_Version : LongWord) : LongWord;
  function D3DSHADER_VERSION_MINOR(_Version : LongWord) : LongWord;


// destination/source parameter register type
const
  D3DSI_COMMENTSIZE_SHIFT = 16;
  D3DSI_COMMENTSIZE_MASK  = $7FFF0000;

  function  D3DSHADER_COMMENT(_DWordSize : LongWord) : LongWord;

// pixel/vertex shader end token
const
  D3DPS_END  = $0000FFFF;
  D3DVS_END  = $0000FFFF;

//---------------------------------------------------------------------

// High order surfaces
//
type TD3DBasisType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DBASIS_BEZIER      = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBASIS_BSPLINE     = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBASIS_INTERPOLATE = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBASIS_FORCE_DWORD = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};

type TD3DOrderType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DORDER_LINEAR      = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
{$IFNDEF DX8}
       D3DORDER_QUADRATIC   = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
{$ENDIF}
       D3DORDER_CUBIC       = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DORDER_QUINTIC     = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DORDER_FORCE_DWORD = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};

type TD3DPatchEdgeStyle = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DPATCHEDGE_DISCRETE    = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DPATCHEDGE_CONTINUOUS  = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DPATCHEDGE_FORCE_DWORD = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};

type TD3DStateBlockType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DSBT_ALL           = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // capture all state
       D3DSBT_PIXELSTATE    = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // capture pixel state
       D3DSBT_VERTEXSTATE   = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // capture vertex state
       D3DSBT_FORCE_DWORD   = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};


// The D3DVERTEXBLENDFLAGS type is used with D3DRS_VERTEXBLEND state.
//
type TD3DVertexBlendFlags = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DVBF_DISABLE  = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}         // Disable vertex blending
       D3DVBF_1WEIGHTS = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}         // 2 matrix blending
       D3DVBF_2WEIGHTS = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}         // 3 matrix blending
       D3DVBF_3WEIGHTS = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}         // 4 matrix blending
       D3DVBF_TWEENING = 255{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}       // blending using D3DRS_TWEENFACTOR
       D3DVBF_0WEIGHTS = 256{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}       // one matrix is used with weight 1.0
       D3DVBF_FORCE_DWORD = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};     // force 32-bit size enum

type TD3DTextureTransformFlags = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DTTFF_DISABLE         = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // texture coordinates are passed directly
       D3DTTFF_COUNT1          = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // rasterizer should expect 1-D texture coords
       D3DTTFF_COUNT2          = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // rasterizer should expect 2-D texture coords
       D3DTTFF_COUNT3          = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // rasterizer should expect 3-D texture coords
       D3DTTFF_COUNT4          = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // rasterizer should expect 4-D texture coords
       D3DTTFF_PROJECTED       = 256{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // texcoords to be divided by COUNTth element
       D3DTTFF_FORCE_DWORD     = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};

// Macros to set texture coordinate format bits in the FVF id

const
  D3DFVF_TEXTUREFORMAT2 = 0;         // Two floating point values
  D3DFVF_TEXTUREFORMAT1 = 3;         // One floating point value
  D3DFVF_TEXTUREFORMAT3 = 1;         // Three floating point values
  D3DFVF_TEXTUREFORMAT4 = 2;         // Four floating point values

  function D3DFVF_TEXCOORDSIZE3(CoordIndex : LongWord) : LongWord;
  function D3DFVF_TEXCOORDSIZE2(CoordIndex : LongWord) : LongWord;
  function D3DFVF_TEXCOORDSIZE4(CoordIndex : LongWord) : LongWord;
  function D3DFVF_TEXCOORDSIZE1(CoordIndex : LongWord) : LongWord;

//---------------------------------------------------------------------

(* Direct3D8 Device types *)

type TD3DDevType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
  D3DDEVTYPE_HAL         = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
  D3DDEVTYPE_REF         = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
  D3DDEVTYPE_SW          = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

  D3DDEVTYPE_FORCE_DWORD = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};

(* Multi-Sample buffer types *)
type TD3DMultiSampleType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DMULTISAMPLE_NONE            =  0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DMULTISAMPLE_2_SAMPLES       =  2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DMULTISAMPLE_3_SAMPLES       =  3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DMULTISAMPLE_4_SAMPLES       =  4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DMULTISAMPLE_5_SAMPLES       =  5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DMULTISAMPLE_6_SAMPLES       =  6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DMULTISAMPLE_7_SAMPLES       =  7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DMULTISAMPLE_8_SAMPLES       =  8{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DMULTISAMPLE_9_SAMPLES       =  9{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DMULTISAMPLE_10_SAMPLES      = 10{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DMULTISAMPLE_11_SAMPLES      = 11{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DMULTISAMPLE_12_SAMPLES      = 12{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DMULTISAMPLE_13_SAMPLES      = 13{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DMULTISAMPLE_14_SAMPLES      = 14{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DMULTISAMPLE_15_SAMPLES      = 15{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DMULTISAMPLE_16_SAMPLES      = 16{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DMULTISAMPLE_FORCE_DWORD     = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};

type TD3DMultiSample_Type = TD3DMultiSampleType;
(* Formats
 * Most of these names have the following convention:
 *      A = Alpha
 *      R = Red
 *      G = Green
 *      B = Blue
 *      X = Unused Bits
 *      P = Palette
 *      L = Luminance
 *      U = dU coordinate for BumpMap
 *      V = dV coordinate for BumpMap
 *      S = Stencil
 *      D = Depth (e.g. Z or W buffer)
 *
 *      Further; the order of the pieces are from MSB first; hence
 *      D3DFMT_A8L8 indicates that the high byte of this two byte
 *      format is alpha.
 *
 *      D16 indicates:
 *           - An integer 16-bit value.
 *           - An app-lockable surface.
 *
 *      All Depth/Stencil formats except D3DFMT_D16_LOCKABLE indicate:
 *          - no particular bit ordering per pixel; and
 *          - are not app lockable; and
 *          - the driver is allowed to consume more than the indicated
 *            number of bits per Depth channel (but not Stencil channel).
 *)

  function MAKEFOURCC(ch0, ch1, ch2, ch3 : Char) : LongWord;

type PD3DFormat = ^TD3DFormat;
     TD3DFormat = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}

       D3DFMT_UNKNOWN              = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DFMT_R8G8B8               = 20{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_A8R8G8B8             = 21{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_X8R8G8B8             = 22{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_R5G6B5               = 23{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_X1R5G5B5             = 24{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_A1R5G5B5             = 25{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_A4R4G4B4             = 26{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_R3G3B2               = 27{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_A8                   = 28{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_A8R3G3B2             = 29{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_X4R4G4B4             = 30{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
{$IFNDEF DX8}
       D3DFMT_A2B10G10R10          = 31{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_G16R16               = 34{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
{$ENDIF}
       D3DFMT_A8P8                 = 40{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_P8                   = 41{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DFMT_L8                   = 50{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_A8L8                 = 51{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_A4L4                 = 52{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DFMT_V8U8                 = 60{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_L6V5U5               = 61{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_X8L8V8U8             = 62{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_Q8W8V8U8             = 63{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_V16U16               = 64{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_W11V11U10            = 65{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
{$IFNDEF DX8}
       D3DFMT_A2W10V10U10          = 67{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
{$ENDIF}
       D3DFMT_UYVY = Byte('U') or (Byte('Y') shl 8) or (Byte('V') shl 16) or (Byte('Y') shl 24 ){$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_YUY2 = Byte('Y') or (Byte('U') shl 8) or (Byte('Y') shl 16) or (Byte('2') shl 24 ){$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_DXT1 = Byte('D') or (Byte('X') shl 8) or (Byte('T') shl 16) or (Byte('1') shl 24 ){$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_DXT2 = Byte('D') or (Byte('X') shl 8) or (Byte('T') shl 16) or (Byte('2') shl 24 ){$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_DXT3 = Byte('D') or (Byte('X') shl 8) or (Byte('T') shl 16) or (Byte('3') shl 24 ){$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_DXT4 = Byte('D') or (Byte('X') shl 8) or (Byte('T') shl 16) or (Byte('4') shl 24 ){$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_DXT5 = Byte('D') or (Byte('X') shl 8) or (Byte('T') shl 16) or (Byte('5') shl 24 ){$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DFMT_D16_LOCKABLE         = 70{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_D32                  = 71{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_D15S1                = 73{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_D24S8                = 75{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_D16                  = 80{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_D24X8                = 77{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_D24X4S4              = 79{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}


       D3DFMT_VERTEXDATA           = 100{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_INDEX16              = 101{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DFMT_INDEX32              = 102{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DFMT_FORCE_DWORD          = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};

(* Display Modes *)
type
  PD3DDisplayMode = ^TD3DDisplayMode;
  TD3DDisplayMode = packed record
    Width       : Cardinal;
    Height      : Cardinal;
    RefreshRate : Cardinal;
    Format      : TD3DFormat;
  end;

(* Creation Parameters *)
  PD3DDevice_Creation_Parameters = ^TD3DDevice_Creation_Parameters;
  TD3DDevice_Creation_Parameters = packed record
    AdapterOrdinal : Cardinal;
    DeviceType     : TD3DDevType;
    hFocusWindow   : HWND;
    BehaviorFlags  : LongWord;
  end;

  PD3DDeviceCreationParameters = ^TD3DDeviceCreationParameters;
  TD3DDeviceCreationParameters = TD3DDevice_Creation_Parameters; 

(* SwapEffects *)
type TD3DSwapEffect = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DSWAPEFFECT_DISCARD           = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DSWAPEFFECT_FLIP              = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DSWAPEFFECT_COPY              = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DSWAPEFFECT_COPY_VSYNC        = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DSWAPEFFECT_FORCE_DWORD       = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};

(* Pool types *)
type TD3DPool  = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DPOOL_DEFAULT                 = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DPOOL_MANAGED                 = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DPOOL_SYSTEMMEM               = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
{$IFNDEF DX8}
       D3DPOOL_SCRATCH                 = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
{$ENDIF}

       D3DPOOL_FORCE_DWORD             = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};

const
(* RefreshRate pre-defines *)
  D3DPRESENT_RATE_DEFAULT         = $00000000;
  D3DPRESENT_RATE_UNLIMITED       = $7fffffff;


(* Resize Optional Parameters *)
type
  PD3DPresent_Parameters = ^TD3DPresent_Parameters;
  TD3DPresent_Parameters = packed record
    BackBufferWidth                 : Cardinal;
    BackBufferHeight                : Cardinal;
    BackBufferFormat                : TD3DFormat;
    BackBufferCount                 : Cardinal;

    MultiSampleType                 : TD3DMultiSample_Type;

    SwapEffect                      : TD3DSwapEffect;
    hDeviceWindow                   : HWND;
    Windowed                        : BOOL;
    EnableAutoDepthStencil          : BOOL;
    AutoDepthStencilFormat          : TD3DFormat;
    Flags                           : LongWord;

    (* Following elements must be zero for Windowed mode *)
    FullScreen_RefreshRateInHz      : Cardinal;
    FullScreen_PresentationInterval : Cardinal;
  end;

  PD3DPresentParameters = ^TD3DPresentParameters;
  TD3DPresentParameters = TD3DPresent_Parameters;

// Values for D3DPRESENT_PARAMETERS.Flags
const
  D3DPRESENTFLAG_LOCKABLE_BACKBUFFER  = $00000001;


(* Gamma Ramp: Same as DX7 *)
type
  PD3DGammaRamp = ^TD3DGammaRamp;
  TD3DGammaRamp = packed record
    red   : array [0..255] of Word;
    green : array [0..255] of Word;
    blue  : array [0..255] of Word;
  end;

(* Back buffer types *)
type TD3DBackBufferType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DBACKBUFFER_TYPE_MONO         = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBACKBUFFER_TYPE_LEFT         = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DBACKBUFFER_TYPE_RIGHT        = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DBACKBUFFER_TYPE_FORCE_DWORD  = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};

type TD3DBackBuffer_Type = TD3DBackBufferType;

(* Types *)
type TD3DResourceType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DRTYPE_SURFACE                = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRTYPE_VOLUME                 = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRTYPE_TEXTURE                = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRTYPE_VOLUMETEXTURE          = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRTYPE_CUBETEXTURE            = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRTYPE_VERTEXBUFFER           = 6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DRTYPE_INDEXBUFFER            = 7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DRTYPE_FORCE_DWORD            = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};

const
(* Usages *)
  D3DUSAGE_RENDERTARGET       = $00000001;
  D3DUSAGE_DEPTHSTENCIL       = $00000002;

(* Usages for Vertex/Index buffers *)
  D3DUSAGE_WRITEONLY          = $00000008;
  D3DUSAGE_SOFTWAREPROCESSING = $00000010;
  D3DUSAGE_DONOTCLIP          = $00000020;
  D3DUSAGE_POINTS             = $00000040;
  D3DUSAGE_RTPATCHES          = $00000080;
  D3DUSAGE_NPATCHES           = $00000100;
  D3DUSAGE_DYNAMIC            = $00000200;

(* CubeMap Face identifiers *)
type TD3DCubeMapFaces = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DCUBEMAP_FACE_POSITIVE_X     = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DCUBEMAP_FACE_NEGATIVE_X     = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DCUBEMAP_FACE_POSITIVE_Y     = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DCUBEMAP_FACE_NEGATIVE_Y     = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DCUBEMAP_FACE_POSITIVE_Z     = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
       D3DCUBEMAP_FACE_NEGATIVE_Z     = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

       D3DCUBEMAP_FACE_FORCE_DWORD    = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};

type TD3DCubeMap_Faces = TD3DCubeMapFaces;

const
(* Lock flags *)

  D3DLOCK_READONLY         = $00000010;
  D3DLOCK_DISCARD          = $00002000;
  D3DLOCK_NOOVERWRITE      = $00001000;
  D3DLOCK_NOSYSLOCK        = $00000800;

  D3DLOCK_NO_DIRTY_UPDATE  = $00008000;

(* Vertex Buffer Description *)
type
  PD3DVertexBuffer_Desc = ^TD3DVertexBuffer_Desc;
  TD3DVertexBuffer_Desc = packed record
    Format : TD3DFormat;
    _Type  : TD3DResourceType;
    Usage  : LongWord;
    Pool   : TD3DPool;
    Size   : Cardinal;
    FVF    : LongWord;
  end;

  PD3DVertexBufferDesc = ^TD3DVertexBufferDesc;
  TD3DVertexBufferDesc = TD3DVertexBuffer_Desc;

(* Index Buffer Description *)
  PTD3DIndexBuffer_Desc = ^TD3DIndexBuffer_Desc;
  TD3DIndexBuffer_Desc = packed record
    Format : TD3DFormat;
    _Type  : TD3DResourceType;
    Usage  : LongWord;
    Pool   : TD3DPool;
    Size   : Cardinal;
  end;

  PTD3DIndexBufferDesc = ^TD3DIndexBufferDesc;
  TD3DIndexBufferDesc = TD3DIndexBuffer_Desc;


(* Surface Description *)
  PD3DSurface_Desc = ^TD3DSurface_Desc;
  TD3DSurface_Desc = packed record
    Format          : TD3DFormat;
    _Type           : TD3DResourceType;
    Usage           : LongWord;
    Pool            : TD3DPool;
    Size            : Cardinal;
    MultiSampleType : TD3DMultiSample_Type;
    Width           : Cardinal;
    Height          : Cardinal;
  end;

  PD3DSurfaceDesc = ^TD3DSurfaceDesc;
  TD3DSurfaceDesc = TD3DSurface_Desc;


  PD3DVolume_Desc = ^TD3DVolume_Desc;
  TD3DVolume_Desc = packed record
    Format : TD3DFormat;
    _Type  : TD3DResourceType;
    Usage  : LongWord;
    Pool   : TD3DPool;
    Size   : Cardinal;
    Width  : Cardinal;
    Height : Cardinal;
    Depth  : Cardinal;
  end;

  PD3DVolumeDesc = ^TD3DVolumeDesc;
  TD3DVolumeDesc = TD3DVolume_Desc;


(* Structure for LockRect *)
  PD3DLocked_Rect = ^TD3DLocked_Rect;
  TD3DLocked_Rect = packed record
    Pitch : Integer;
    pBits : Pointer;//void*
  end;

  PD3DLockedRect = ^TD3DLockedRect;
  TD3DLockedRect = TD3DLocked_Rect;


(* Structures for LockBox *)
  PD3DBox = ^TD3DBox;
  TD3DBox = packed record
    Left   : Cardinal;
    Top    : Cardinal;
    Right  : Cardinal;
    Bottom : Cardinal;
    Front  : Cardinal;
    Back   : Cardinal;
  end;

  PD3DLocked_Box = ^TD3DLocked_Box;
  TD3DLocked_Box = packed record
    RowPitch   : Integer;
    SlicePitch : Integer;
    pBits      : Pointer;
  end;

  PD3DLockedBox = ^TD3DLockedBox;
  TD3DLockedBox = TD3DLocked_Box;


(* Structures for LockRange *)
  PD3DRange = ^TD3DRange;
  TD3DRange = packed record
    Offset : Cardinal;
    Size   : Cardinal;
  end;

(* Structures for high order primitives *)
  PD3DRectPatch_Info = ^TD3DRectPatch_Info;
  TD3DRectPatch_Info = packed record
    StartVertexOffsetWidth  : Cardinal;
    StartVertexOffsetHeight : Cardinal;
    Width                   : Cardinal;
    Height                  : Cardinal;
    Stride                  : Cardinal;
    Basis                   : TD3DBasisType;
    Order                   : TD3DOrderType;
  end;

  PD3DRectPatchInfo = ^TD3DRectPatchInfo;
  TD3DRectPatchInfo = TD3DRectPatch_Info;

  
  PD3DTriPatch_Info = ^TD3DTriPatch_Info;
  TD3DTriPatch_Info = packed record
    StartVertexOffset : Cardinal;
    NumVertices       : Cardinal;
    Basis             : TD3DBasisType;
    Order             : TD3DOrderType;
  end;

  PD3DTriPatchInfo = ^TD3DTriPatchInfo;
  TD3DTriPatchInfo = TD3DTriPatch_Info;


(* Adapter Identifier *)
const
  MAX_DEVICE_IDENTIFIER_STRING = 512;

type
  PD3DAdapter_Identifier8 = ^TD3DAdapter_Identifier8; 
  TD3DAdapter_Identifier8 = packed record
    Driver      : array [0..MAX_DEVICE_IDENTIFIER_STRING - 1] of Char;
    Description : array [0..MAX_DEVICE_IDENTIFIER_STRING - 1] of Char;

    DriverVersionLowPart : LongWord;     (* Defined for 16 bit driver components *)
    DriverVersionHighPart : LongWord;

    VendorId : LongWord;
    DeviceId : LongWord;
    SubSysId : LongWord;
    Revision : LongWord;

    DeviceIdentifier : TGUID;

    WHQLLevel : LongWord;

  end;

  PD3DAdapterIdentifier8 = ^TD3DAdapterIdentifier8;
  TD3DAdapterIdentifier8 = TD3DAdapter_Identifier8;


(* Raster Status structure returned by GetRasterStatus *)
  PD3DRaster_Status = ^TD3DRaster_Status;
  TD3DRaster_Status = packed record
    InVBlank : Bool;
    ScanLine : Cardinal;
  end;

  PD3DRasterStatus = ^TD3DRasterStatus;
  TD3DRasterStatus = TD3DRaster_Status;

(* Debug monitor tokens (DEBUG only)

   Note that if D3DRS_DEBUGMONITORTOKEN is set; the call is treated as
   passing a token to the debug monitor.  For example; if; after passing
   D3DDMT_ENABLE/DISABLE to D3DRS_DEBUGMONITORTOKEN other token values
   are passed in; the enabled/disabled state of the debug
   monitor will still persist.

   The debug monitor defaults to enabled.

   Calling GetRenderState on D3DRS_DEBUGMONITORTOKEN is not of any use.
*)
type TD3DDebugMonitorTokens = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
       D3DDMT_ENABLE          = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // enable debug monitor
       D3DDMT_DISABLE         = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // disable debug monitor
       D3DDMT_FORCE_DWORD     = $7fffffff{$IFNDEF NOENUMS}){$ENDIF};

{$IFNDEF DX8}
// GetInfo IDs
const
  D3DDEVINFOID_RESOURCEMANAGER = 5;    (* Used with D3DDEVINFO_RESOURCEMANAGER *)
  D3DDEVINFOID_VERTEXSTATS     = 6;    (* Used with D3DDEVINFO_D3DVERTEXSTATS *)

type
  PD3DResourceStats = ^TD3DResourceStats;
  TD3DResourceStats = packed record
// Data collected since last Present()
    bThrashing            : LongBool;  (* indicates if thrashing *)
    ApproxBytesDownloaded : LongWord;  (* Approximate number of bytes downloaded by resource manager *)
    NumEvicts             : LongWord;  (* number of objects evicted *)
    NumVidCreates         : LongWord;  (* number of objects created in video memory *)
    LastPri               : LongWord;  (* priority of last object evicted *)
    NumUsed               : LongWord;  (* number of objects set to the device *)
    NumUsedInVidMem       : LongWord;  (* number of objects set to the device, which are already in video memory *)
// Persistent data
    WorkingSet            : LongWord;  (* number of objects in video memory *)
    WorkingSetBytes       : LongWord;  (* number of bytes in video memory *)
    TotalManaged          : LongWord;  (* total number of managed objects *)
    TotalBytes            : LongWord;  (* total number of bytes of managed objects *)
  end;

const D3DRTYPECOUNT = LongWord(D3DRTYPE_INDEXBUFFER) + 1;

type
  PD3DDevInfo_ResourceManager = ^TD3DDevInfo_ResourceManager;
  TD3DDevInfo_ResourceManager = packed record
    stats : array [0..D3DRTYPECOUNT - 1] of TD3DResourceStats;
  end;

  PD3DDevInfoResourceManager = ^TD3DDevInfoResourceManager;
  TD3DDevInfoResourceManager = TD3DDevInfo_ResourceManager;


type
  PD3DDevInfo_D3DVertexStats = ^TD3DDevInfo_D3DVertexStats;
  TD3DDevInfo_D3DVertexStats = packed record
    NumRenderedTriangles      : LongWord;  (* total number of triangles that are not clipped in this frame *)
    NumExtraClippingTriangles : LongWord;  (* Number of new triangles generated by clipping *)
  end;
  PD3DDevInfoD3DVertexStats = ^TD3DDevInfoD3DVertexStats;
  TD3DDevInfoD3DVertexStats = TD3DDevInfo_D3DVertexStats;

{$ENDIF}

(*==========================================================================;
 *
 *  Copyright (C) 1995-2000 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       d3d8caps.h
 *  Content:    Direct3D capabilities include file
 *
 ***************************************************************************)

type
  PD3DCaps8 = ^TD3DCaps8;
  TD3DCaps8 = packed record
    (* Device Info *)
    DeviceType                : TD3DDevType;
    AdapterOrdinal            : LongWord;

    (* Caps from DX7 Draw *)
    Caps                      : LongWord;
    Caps2                     : LongWord;
    Caps3                     : LongWord;
    PresentationIntervals     : LongWord;

    (* Cursor Caps *)
    CursorCaps                : LongWord;

    (* 3D Device Caps *)
    DevCaps                   : LongWord;

    PrimitiveMiscCaps         : LongWord;
    RasterCaps                : LongWord;
    ZCmpCaps                  : LongWord;
    SrcBlendCaps              : LongWord;
    DestBlendCaps             : LongWord;
    AlphaCmpCaps              : LongWord;
    ShadeCaps                 : LongWord;
    TextureCaps               : LongWord;
    TextureFilterCaps         : LongWord;  // D3DPTFILTERCAPS for IDirect3DTexture8's
    CubeTextureFilterCaps     : LongWord;  // D3DPTFILTERCAPS for IDirect3DCubeTexture8's
    VolumeTextureFilterCaps   : LongWord;  // D3DPTFILTERCAPS for IDirect3DVolumeTexture8's
    TextureAddressCaps        : LongWord;  // D3DPTADDRESSCAPS for IDirect3DTexture8's
    VolumeTextureAddressCaps  : LongWord;  // D3DPTADDRESSCAPS for IDirect3DVolumeTexture8's

    LineCaps                  : LongWord;  // D3DLINECAPS

    MaxTextureWidth           : LongWord;
    MaxTextureHeight          : LongWord;
    MaxVolumeExtent           : LongWord;

    MaxTextureRepeat          : LongWord;
    MaxTextureAspectRatio     : LongWord;
    MaxAnisotropy             : LongWord;
    MaxVertexW                : Single;

    GuardBandLeft             : Single;
    GuardBandTop              : Single;
    GuardBandRight            : Single;
    GuardBandBottom           : Single;

    ExtentsAdjust             : Single;
    StencilCaps               : LongWord;

    FVFCaps                   : LongWord;
    TextureOpCaps             : LongWord;
    MaxTextureBlendStages     : LongWord;
    MaxSimultaneousTextures   : LongWord;

    VertexProcessingCaps      : LongWord;
    MaxActiveLights           : LongWord;
    MaxUserClipPlanes         : LongWord;
    MaxVertexBlendMatrices    : LongWord;
    MaxVertexBlendMatrixIndex : LongWord;

    MaxPointSize              : Single;

    MaxPrimitiveCount         : LongWord;  // max number of primitives per DrawPrimitive call
    MaxVertexIndex            : LongWord;
    MaxStreams                : LongWord;
    MaxStreamStride           : LongWord;  // max stride for SetStreamSource

    VertexShaderVersion       : LongWord;
    MaxVertexShaderConst      : LongWord;  // number of vertex shader constant registers

    PixelShaderVersion        : LongWord;
    MaxPixelShaderValue       : Single;    // max value of pixel shader arithmetic component
  end;

//
// BIT DEFINES FOR D3DCAPS8 DWORD MEMBERS
//

//
// Caps
//
const
  D3DCAPS_READ_SCANLINE = $00020000;

//
// Caps2
//
  D3DCAPS2_NO2DDURING3DSCENE      = $00000002;
  D3DCAPS2_FULLSCREENGAMMA        = $00020000;
  D3DCAPS2_CANRENDERWINDOWED      = $00080000;
  D3DCAPS2_CANCALIBRATEGAMMA      = $00100000;
  D3DCAPS2_RESERVED               = $02000000;
{$IFNDEF DX8}
  D3DCAPS2_CANMANAGERESOURCE      = $10000000;
  D3DCAPS2_DYNAMICTEXTURES        = $20000000;
{$ENDIF}

//
// Caps3
//
  D3DCAPS3_RESERVED               = $8000001f;

{$IFNDEF DX8}
// Indicates that the device can respect the ALPHABLENDENABLE render state
// when fullscreen while using the FLIP or DISCARD swap effect.
// COPY and COPYVSYNC swap effects work whether or not this flag is set.
  D3DCAPS3_ALPHA_FULLSCREEN_FLIP_OR_DISCARD = $00000020;
{$ENDIF}



//
// PresentationIntervals
//
  D3DPRESENT_INTERVAL_DEFAULT     = $00000000;
  D3DPRESENT_INTERVAL_ONE         = $00000001;
  D3DPRESENT_INTERVAL_TWO         = $00000002;
  D3DPRESENT_INTERVAL_THREE       = $00000004;
  D3DPRESENT_INTERVAL_FOUR        = $00000008;
  D3DPRESENT_INTERVAL_IMMEDIATE   = $80000000;

//
// CursorCaps
//
// Driver supports HW color cursor in at least hi-res modes(height >=400)
  D3DCURSORCAPS_COLOR             = $00000001;
// Driver supports HW cursor also in low-res modes(height < 400)
  D3DCURSORCAPS_LOWRES            = $00000002;

//
// DevCaps
//                                   
  D3DDEVCAPS_EXECUTESYSTEMMEMORY     = $00000010; (* Device can use execute buffers from system memory *)
  D3DDEVCAPS_EXECUTEVIDEOMEMORY      = $00000020; (* Device can use execute buffers from video memory *)
  D3DDEVCAPS_TLVERTEXSYSTEMMEMORY    = $00000040; (* Device can use TL buffers from system memory *)
  D3DDEVCAPS_TLVERTEXVIDEOMEMORY     = $00000080; (* Device can use TL buffers from video memory *)
  D3DDEVCAPS_TEXTURESYSTEMMEMORY     = $00000100; (* Device can texture from system memory *)
  D3DDEVCAPS_TEXTUREVIDEOMEMORY      = $00000200; (* Device can texture from device memory *)
  D3DDEVCAPS_DRAWPRIMTLVERTEX        = $00000400; (* Device can draw TLVERTEX primitives *)
  D3DDEVCAPS_CANRENDERAFTERFLIP      = $00000800; (* Device can render without waiting for flip to complete *)
  D3DDEVCAPS_TEXTURENONLOCALVIDMEM   = $00001000; (* Device can texture from nonlocal video memory *)
  D3DDEVCAPS_DRAWPRIMITIVES2         = $00002000; (* Device can support DrawPrimitives2 *)
  D3DDEVCAPS_SEPARATETEXTUREMEMORIES = $00004000; (* Device is texturing from separate memory pools *)
  D3DDEVCAPS_DRAWPRIMITIVES2EX       = $00008000; (* Device can support Extended DrawPrimitives2 i.e. DX7 compliant driver*)
  D3DDEVCAPS_HWTRANSFORMANDLIGHT     = $00010000; (* Device can support transformation and lighting in hardware and DRAWPRIMITIVES2EX must be also *)
  D3DDEVCAPS_CANBLTSYSTONONLOCAL     = $00020000; (* Device supports a Tex Blt from system memory to non-local vidmem *)
  D3DDEVCAPS_HWRASTERIZATION         = $00080000; (* Device has HW acceleration for rasterization *)
  D3DDEVCAPS_PUREDEVICE              = $00100000; (* Device supports D3DCREATE_PUREDEVICE *)
  D3DDEVCAPS_QUINTICRTPATCHES        = $00200000; (* Device supports quintic Beziers and BSplines *)
  D3DDEVCAPS_RTPATCHES               = $00400000; (* Device supports Rect and Tri patches *)
  D3DDEVCAPS_RTPATCHHANDLEZERO       = $00800000; (* Indicates that RT Patches may be drawn efficiently using handle 0 *)
  D3DDEVCAPS_NPATCHES                = $01000000; (* Device supports N-Patches *)

//
// PrimitiveMiscCaps
//                                   
  D3DPMISCCAPS_MASKZ                 = $00000002;
  D3DPMISCCAPS_LINEPATTERNREP        = $00000004;
  D3DPMISCCAPS_CULLNONE              = $00000010;
  D3DPMISCCAPS_CULLCW                = $00000020;
  D3DPMISCCAPS_CULLCCW               = $00000040;
  D3DPMISCCAPS_COLORWRITEENABLE      = $00000080;
  D3DPMISCCAPS_CLIPPLANESCALEDPOINTS = $00000100; (* Device correctly clips scaled points to clip planes *)
  D3DPMISCCAPS_CLIPTLVERTS           = $00000200; (* device will clip post-transformed vertex primitives *)
  D3DPMISCCAPS_TSSARGTEMP            = $00000400; (* device supports D3DTA_TEMP for temporary register *)
  D3DPMISCCAPS_BLENDOP               = $00000800; (* device supports D3DRS_BLENDOP *)
{$IFNDEF DX8}
  D3DPMISCCAPS_NULLREFERENCE         = $00001000; (* Reference Device that doesnt render *)
{$ENDIF}

//
// LineCaps
//
  D3DLINECAPS_TEXTURE             = $00000001;
  D3DLINECAPS_ZTEST               = $00000002;
  D3DLINECAPS_BLEND               = $00000004;
  D3DLINECAPS_ALPHACMP            = $00000008;
  D3DLINECAPS_FOG                 = $00000010;

//
// RasterCaps
//                                      
  D3DPRASTERCAPS_DITHER                 = $00000001;
  D3DPRASTERCAPS_PAT                    = $00000008;
  D3DPRASTERCAPS_ZTEST                  = $00000010;
  D3DPRASTERCAPS_FOGVERTEX              = $00000080;
  D3DPRASTERCAPS_FOGTABLE               = $00000100;
  D3DPRASTERCAPS_ANTIALIASEDGES         = $00001000;
  D3DPRASTERCAPS_MIPMAPLODBIAS          = $00002000;
  D3DPRASTERCAPS_ZBIAS                  = $00004000;
  D3DPRASTERCAPS_ZBUFFERLESSHSR         = $00008000;
  D3DPRASTERCAPS_FOGRANGE               = $00010000;
  D3DPRASTERCAPS_ANISOTROPY             = $00020000;
  D3DPRASTERCAPS_WBUFFER                = $00040000;
  D3DPRASTERCAPS_WFOG                   = $00100000;
  D3DPRASTERCAPS_ZFOG                   = $00200000;
  D3DPRASTERCAPS_COLORPERSPECTIVE       = $00400000; (* Device iterates colors perspective correct *)
  D3DPRASTERCAPS_STRETCHBLTMULTISAMPLE  = $00800000;

//
// ZCmpCaps, AlphaCmpCaps
//
  D3DPCMPCAPS_NEVER               = $00000001;
  D3DPCMPCAPS_LESS                = $00000002;
  D3DPCMPCAPS_EQUAL               = $00000004;
  D3DPCMPCAPS_LESSEQUAL           = $00000008;
  D3DPCMPCAPS_GREATER             = $00000010;
  D3DPCMPCAPS_NOTEQUAL            = $00000020;
  D3DPCMPCAPS_GREATEREQUAL        = $00000040;
  D3DPCMPCAPS_ALWAYS              = $00000080;

//
// SourceBlendCaps, DestBlendCaps
//
  D3DPBLENDCAPS_ZERO              = $00000001;
  D3DPBLENDCAPS_ONE               = $00000002;
  D3DPBLENDCAPS_SRCCOLOR          = $00000004;
  D3DPBLENDCAPS_INVSRCCOLOR       = $00000008;
  D3DPBLENDCAPS_SRCALPHA          = $00000010;
  D3DPBLENDCAPS_INVSRCALPHA       = $00000020;
  D3DPBLENDCAPS_DESTALPHA         = $00000040;
  D3DPBLENDCAPS_INVDESTALPHA      = $00000080;
  D3DPBLENDCAPS_DESTCOLOR         = $00000100;
  D3DPBLENDCAPS_INVDESTCOLOR      = $00000200;
  D3DPBLENDCAPS_SRCALPHASAT       = $00000400;
  D3DPBLENDCAPS_BOTHSRCALPHA      = $00000800;
  D3DPBLENDCAPS_BOTHINVSRCALPHA   = $00001000;

//
// ShadeCaps
//
  D3DPSHADECAPS_COLORGOURAUDRGB       = $00000008;
  D3DPSHADECAPS_SPECULARGOURAUDRGB    = $00000200;
  D3DPSHADECAPS_ALPHAGOURAUDBLEND     = $00004000;
  D3DPSHADECAPS_FOGGOURAUD            = $00080000;

//
// TextureCaps
//                                         
  D3DPTEXTURECAPS_PERSPECTIVE              = $00000001; (* Perspective-correct texturing is supported *)
  D3DPTEXTURECAPS_POW2                     = $00000002; (* Power-of-2 texture dimensions are required - applies to non-Cube/Volume textures only. *)
  D3DPTEXTURECAPS_ALPHA                    = $00000004; (* Alpha in texture pixels is supported *)
  D3DPTEXTURECAPS_SQUAREONLY               = $00000020; (* Only square textures are supported *)
  D3DPTEXTURECAPS_TEXREPEATNOTSCALEDBYSIZE = $00000040; (* Texture indices are not scaled by the texture size prior to interpolation *)
  D3DPTEXTURECAPS_ALPHAPALETTE             = $00000080; (* Device can draw alpha from texture palettes *)
// Device can use non-POW2 textures if:
//  1) D3DTEXTURE_ADDRESS is set to CLAMP for this texture's stage
//  2) D3DRS_WRAP(N) is zero for this texture's coordinates
//  3) mip mapping is not enabled (use magnification filter only)
  D3DPTEXTURECAPS_NONPOW2CONDITIONAL       = $00000100;
  D3DPTEXTURECAPS_PROJECTED                = $00000400; (* Device can do D3DTTFF_PROJECTED *)
  D3DPTEXTURECAPS_CUBEMAP                  = $00000800; (* Device can do cubemap textures *)
  D3DPTEXTURECAPS_VOLUMEMAP                = $00002000; (* Device can do volume textures *)
  D3DPTEXTURECAPS_MIPMAP                   = $00004000; (* Device can do mipmapped textures *)
  D3DPTEXTURECAPS_MIPVOLUMEMAP             = $00008000; (* Device can do mipmapped volume textures *)
  D3DPTEXTURECAPS_MIPCUBEMAP               = $00010000; (* Device can do mipmapped cube maps *)
  D3DPTEXTURECAPS_CUBEMAP_POW2             = $00020000; (* Device requires that cubemaps be power-of-2 dimension *)
  D3DPTEXTURECAPS_VOLUMEMAP_POW2           = $00040000; (* Device requires that volume maps be power-of-2 dimension *)

//
// TextureFilterCaps
//
  D3DPTFILTERCAPS_MINFPOINT           = $00000100; (* Min Filter *)
  D3DPTFILTERCAPS_MINFLINEAR          = $00000200;
  D3DPTFILTERCAPS_MINFANISOTROPIC     = $00000400;
  D3DPTFILTERCAPS_MIPFPOINT           = $00010000; (* Mip Filter *)
  D3DPTFILTERCAPS_MIPFLINEAR          = $00020000;
  D3DPTFILTERCAPS_MAGFPOINT           = $01000000; (* Mag Filter *)
  D3DPTFILTERCAPS_MAGFLINEAR          = $02000000;
  D3DPTFILTERCAPS_MAGFANISOTROPIC     = $04000000;
  D3DPTFILTERCAPS_MAGFAFLATCUBIC      = $08000000;
  D3DPTFILTERCAPS_MAGFGAUSSIANCUBIC   = $10000000;

//
// TextureAddressCaps
//
  D3DPTADDRESSCAPS_WRAP           = $00000001;
  D3DPTADDRESSCAPS_MIRROR         = $00000002;
  D3DPTADDRESSCAPS_CLAMP          = $00000004;
  D3DPTADDRESSCAPS_BORDER         = $00000008;
  D3DPTADDRESSCAPS_INDEPENDENTUV  = $00000010;
  D3DPTADDRESSCAPS_MIRRORONCE     = $00000020;

//
// StencilCaps
//
  D3DSTENCILCAPS_KEEP             = $00000001;
  D3DSTENCILCAPS_ZERO             = $00000002;
  D3DSTENCILCAPS_REPLACE          = $00000004;
  D3DSTENCILCAPS_INCRSAT          = $00000008;
  D3DSTENCILCAPS_DECRSAT          = $00000010;
  D3DSTENCILCAPS_INVERT           = $00000020;
  D3DSTENCILCAPS_INCR             = $00000040;
  D3DSTENCILCAPS_DECR             = $00000080;

//
// TextureOpCaps
//
  D3DTEXOPCAPS_DISABLE                    = $00000001;
  D3DTEXOPCAPS_SELECTARG1                 = $00000002;
  D3DTEXOPCAPS_SELECTARG2                 = $00000004;
  D3DTEXOPCAPS_MODULATE                   = $00000008;
  D3DTEXOPCAPS_MODULATE2X                 = $00000010;
  D3DTEXOPCAPS_MODULATE4X                 = $00000020;
  D3DTEXOPCAPS_ADD                        = $00000040;
  D3DTEXOPCAPS_ADDSIGNED                  = $00000080;
  D3DTEXOPCAPS_ADDSIGNED2X                = $00000100;
  D3DTEXOPCAPS_SUBTRACT                   = $00000200;
  D3DTEXOPCAPS_ADDSMOOTH                  = $00000400;
  D3DTEXOPCAPS_BLENDDIFFUSEALPHA          = $00000800;
  D3DTEXOPCAPS_BLENDTEXTUREALPHA          = $00001000;
  D3DTEXOPCAPS_BLENDFACTORALPHA           = $00002000;
  D3DTEXOPCAPS_BLENDTEXTUREALPHAPM        = $00004000;
  D3DTEXOPCAPS_BLENDCURRENTALPHA          = $00008000;
  D3DTEXOPCAPS_PREMODULATE                = $00010000;
  D3DTEXOPCAPS_MODULATEALPHA_ADDCOLOR     = $00020000;
  D3DTEXOPCAPS_MODULATECOLOR_ADDALPHA     = $00040000;
  D3DTEXOPCAPS_MODULATEINVALPHA_ADDCOLOR  = $00080000;
  D3DTEXOPCAPS_MODULATEINVCOLOR_ADDALPHA  = $00100000;
  D3DTEXOPCAPS_BUMPENVMAP                 = $00200000;
  D3DTEXOPCAPS_BUMPENVMAPLUMINANCE        = $00400000;
  D3DTEXOPCAPS_DOTPRODUCT3                = $00800000;
  D3DTEXOPCAPS_MULTIPLYADD                = $01000000;
  D3DTEXOPCAPS_LERP                       = $02000000;

//
// FVFCaps
//
  D3DFVFCAPS_TEXCOORDCOUNTMASK    = $0000ffff; (* mask for texture coordinate count field *)
  D3DFVFCAPS_DONOTSTRIPELEMENTS   = $00080000; (* Device prefers that vertex elements not be stripped *)
  D3DFVFCAPS_PSIZE                = $00100000; (* Device can receive point size *)

//
// VertexProcessingCaps
//
  D3DVTXPCAPS_TEXGEN              = $00000001; (* device can do texgen *)
  D3DVTXPCAPS_MATERIALSOURCE7     = $00000002; (* device can do DX7-level colormaterialsource ops *)
  D3DVTXPCAPS_DIRECTIONALLIGHTS   = $00000008; (* device can do directional lights *)
  D3DVTXPCAPS_POSITIONALLIGHTS    = $00000010; (* device can do positional lights (includes point and spot) *)
  D3DVTXPCAPS_LOCALVIEWER         = $00000020; (* device can do local viewer *)
  D3DVTXPCAPS_TWEENING            = $00000040; (* device can do vertex tweening *)
  D3DVTXPCAPS_NO_VSDT_UBYTE4      = $00000080; (* device does not support D3DVSDT_UBYTE4 *)



(*==========================================================================;
 *
 *
 *  File:   d3d8.h
 *  Content:    Direct3D include file
 *
 ****************************************************************************)

(* This identifier is passed to Direct3DCreate8 in order to ensure that an
 * application was built against the correct header files. This number is
 * incremented whenever a header (or other) change would require applications
 * to be rebuilt. If the version doesn't match, Direct3DCreate8 will fail.
 * (The number itself has no meaning.)*)

const
{$IFDEF DX8}
  D3D_SDK_VERSION = 120;
{$ELSE}
  D3D_SDK_VERSION = 220;
{$ENDIF}

type
  HMonitor = THandle;

const
  IID_IDirect3D8              : TGUID = '{1DD9E8DA-1C77-4d40-B0CF-98FEFDFF9512}';
  IID_IDirect3DDevice8        : TGUID = '{7385E5DF-8FE8-41D5-86B6-D7B48547B6CF}';
  IID_IDirect3DResource8      : TGUID = '{1B36BB7B-09B7-410a-B445-7D1430D7B33F}';
  IID_IDirect3DBaseTexture8   : TGUID = '{B4211CFA-51B9-4a9f-AB78-DB99B2BB678E}';
  IID_IDirect3DTexture8       : TGUID = '{E4CDD575-2866-4f01-B12E-7EECE1EC9358}';
  IID_IDirect3DCubeTexture8   : TGUID = '{3EE5B968-2ACA-4c34-8BB5-7E0C3D19B750}';
  IID_IDirect3DVolumeTexture8 : TGUID = '{4B8AAAFA-140F-42ba-9131-597EAFAA2EAD}';
  IID_IDirect3DVertexBuffer8  : TGUID = '{8AEEEAC7-05F9-44d4-B591-000B0DF1CB95}';
  IID_IDirect3DIndexBuffer8   : TGUID = '{0E689C9A-053D-44a0-9D92-DB0E3D750F86}';
  IID_IDirect3DSurface8       : TGUID = '{B96EEBCA-B326-4ea5-882F-2FF5BAE021DD}';
  IID_IDirect3DVolume8        : TGUID = '{BD7349F5-14F1-42e4-9C79-972380DB40C0}';
  IID_IDirect3DSwapChain8     : TGUID = '{928C088B-76B9-4C6B-A536-A590853876CD}';

(*
 * Direct3D interfaces
 *)

type
  IDirect3D8              = interface;
  IDirect3DDevice8        = interface;

  IDirect3DResource8      = interface;
  IDirect3DBaseTexture8   = interface;
  IDirect3DTexture8       = interface;
  IDirect3DVolumeTexture8 = interface;
  IDirect3DCubeTexture8   = interface;

  IDirect3DVertexBuffer8  = interface;
  IDirect3DIndexBuffer8   = interface;

  IDirect3DSurface8       = interface;
  IDirect3DVolume8        = interface;

  IDirect3DSwapChain8     = interface;


  IDirect3D8 = interface (IUnknown)
    ['{1DD9E8DA-1C77-4d40-B0CF-98FEFDFF9512}']
    (*** IDirect3D8 methods ***)
    function RegisterSoftwareDevice(pInitializeFunction : Pointer) : HResult; stdcall;
    function GetAdapterCount : Cardinal; stdcall;
    function GetAdapterIdentifier(const Adapter : Cardinal; const Flags : LongWord; out pIdentifier : TD3DAdapter_Identifier8) : HResult; stdcall;
    function GetAdapterModeCount(Adapter : Cardinal) : Cardinal; stdcall;
    function EnumAdapterModes(const Adapter, Mode : Cardinal; var pMode : TD3DDisplayMode) : HResult; stdcall;
    function GetAdapterDisplayMode(const Adapter : Cardinal; var pMode : TD3DDisplayMode) : HResult; stdcall;
    function CheckDeviceType(const Adapter : Cardinal; const CheckType : TD3DDevType; const DisplayFormat, BackBufferFormat : TD3DFormat; const Windowed : BOOL) : HResult; stdcall;
    function CheckDeviceFormat(const Adapter : Cardinal; const DeviceType : TD3DDevType; const AdapterFormat : TD3DFormat; const Usage : LongWord; const RType : TD3DResourceType; const CheckFormat : TD3DFormat) : HResult; stdcall;
    function CheckDeviceMultiSampleType(const Adapter : Cardinal; const DeviceType : TD3DDevType; const SurfaceFormat : TD3DFormat; const Windowed : BOOL; const MultiSampleType : TD3DMultiSample_Type) : HResult; stdcall;
    function CheckDepthStencilMatch(const Adapter : Cardinal; const DeviceType : TD3DDevType; const AdapterFormat, RenderTargetFormat, DepthStencilFormat : TD3DFormat) : HResult; stdcall;
    function GetDeviceCaps(const Adapter : Cardinal; const DeviceType : TD3DDevType; out pCaps : TD3DCaps8) : HResult; stdcall;
    function GetAdapterMonitor(const Adapter : Cardinal) : HMONITOR; stdcall;
    function CreateDevice(const Adapter : Cardinal; const DeviceType : TD3DDevType; hFocusWindow : HWND; BehaviorFlags : LongWord; var pPresentationParameters : TD3DPresent_Parameters; out ppReturnedDeviceInterface : IDirect3DDevice8) : HResult; stdcall;
  end;

  IDirect3DDevice8 = interface (IUnknown)
    ['{7385E5DF-8FE8-41D5-86B6-D7B48547B6CF}']
     (*** IDirect3DDevice8 methods ***)
    function TestCooperativeLevel : HResult; stdcall;
    function GetAvailableTextureMem : Cardinal; stdcall;
    function ResourceManagerDiscardBytes(const Bytes : LongWord) : HResult; stdcall;
    function GetDirect3D(out ppD3D8 : IDirect3D8) : HResult; stdcall;
    function GetDeviceCaps(out pCaps : TD3DCaps8) : HResult; stdcall;
    function GetDisplayMode(out pMode : TD3DDisplayMode) : HResult; stdcall;
    function GetCreationParameters(out pParameters : TD3DDevice_Creation_Parameters) : HResult; stdcall;
    function SetCursorProperties(const XHotSpot, YHotSpot : Cardinal; const pCursorBitmap : IDirect3DSurface8) : HResult; stdcall;
    procedure SetCursorPosition(const XScreenSpace, YScreenSpace : Cardinal; const Flags : LongWord); stdcall;
    function ShowCursor(const bShow : BOOL) : BOOL; stdcall;
    function CreateAdditionalSwapChain(var pPresentationParameters : TD3DPresent_Parameters; out pSwapChain : IDirect3DSwapChain8) : HResult; stdcall;
    function Reset(var pPresentationParameters : TD3DPresent_Parameters) : HResult; stdcall;
    function Present(pSourceRect, pDestRect : PRect; const hDestWindowOverride : HWND; pDirtyRegion : PRgnData) : HResult; stdcall;
    function GetBackBuffer(const BackBuffer : Cardinal; const _Type : TD3DBackBuffer_Type; out ppBackBuffer : IDirect3DSurface8) : HResult; stdcall;
    function GetRasterStatus(out pRasterStatus : TD3DRaster_Status) : HResult; stdcall;
    procedure SetGammaRamp(Flags : LongWord; var pRamp : TD3DGammaRamp); stdcall;
    procedure GetGammaRamp(out pRamp : TD3DGammaRamp); stdcall;
    function CreateTexture(const Width, Height, Levels : Cardinal; const Usage : LongWord; const Format : TD3DFormat; const Pool : TD3DPool; out ppTexture : IDirect3DTexture8) : HResult; stdcall;
    function CreateVolumeTexture(const Width, Height, Depth, Levels : Cardinal; const Usage : LongWord; const Format : TD3DFormat; const Pool : TD3DPool; out ppVolumeTexture : IDirect3DVolumeTexture8) : HResult; stdcall;
    function CreateCubeTexture(const EdgeLength, Levels : Cardinal; const Usage : LongWord; const Format : TD3DFormat; const Pool : TD3DPool; out ppCubeTexture : IDirect3DCubeTexture8) : HResult; stdcall;
    function CreateVertexBuffer(const Length : Cardinal; const Usage, FVF : LongWord; const Pool : TD3DPool; out ppVertexBuffer : IDirect3DVertexBuffer8) : HResult; stdcall;
    function CreateIndexBuffer(const Length : Cardinal; Usage : LongWord; const Format : TD3DFormat; Pool : TD3DPool; out ppIndexBuffer : IDirect3DIndexBuffer8) : HResult; stdcall;
    function CreateRenderTarget(const Width, Height : Cardinal; const Format : TD3DFormat; const MultiSample : TD3DMultiSample_Type; const Lockable : BOOL; out ppSurface : IDirect3DSurface8) : HResult; stdcall;
    function CreateDepthStencilSurface(const Width, Height : Cardinal; const Format : TD3DFormat; const MultiSample : TD3DMultiSample_Type; out ppSurface : IDirect3DSurface8) : HResult; stdcall;
    function CreateImageSurface(const Width, Height : Cardinal; const Format : TD3DFormat; out ppSurface : IDirect3DSurface8) : HResult; stdcall;
    function CopyRects(const pSourceSurface : IDirect3DSurface8; pSourceRectsArray : PRect; const cRects : Cardinal; const pDestinationSurface : IDirect3DSurface8; pDestPointsArray : PPoint) : HResult; stdcall;
    function UpdateTexture(pSourceTexture, pDestinationTexture : IDirect3DBaseTexture8) : HResult; stdcall;
    function GetFrontBuffer(pDestSurface : IDirect3DSurface8) : HResult; stdcall;
    function SetRenderTarget(pRenderTarget, pNewZStencil : IDirect3DSurface8) : HResult; stdcall;
    function GetRenderTarget(out ppRenderTarget : IDirect3DSurface8) : HResult; stdcall;
    function GetDepthStencilSurface(out ppZStencilSurface : IDirect3DSurface8) : HResult; stdcall;
    function BeginScene : HResult; stdcall;
    function EndScene : HResult; stdcall;
    function Clear(const Count : LongWord; pRects : PD3DRect; const Flags : LongWord; const Color : TD3DColor; const Z : Single; const Stencil : LongWord) : HResult; stdcall;
    function SetTransform(const State : TD3DTransformStateType; const pMatrix : TD3DMatrix) : HResult; stdcall;
    function GetTransform(const State : TD3DTransformStateType; out pMatrix : TD3DMatrix) : HResult; stdcall;
    function MultiplyTransform(const State : TD3DTransformStateType; const pMatrix : TD3DMatrix) : HResult; stdcall;
    function SetViewport(var pViewport : TD3DViewport8) : HResult; stdcall;
    function GetViewport(out pViewport : TD3DViewport8) : HResult; stdcall;
    function SetMaterial(var pMaterial : TD3DMaterial8) : HResult; stdcall;
    function GetMaterial(out pMaterial : TD3DMaterial8) : HResult; stdcall;
    function SetLight(const Index : LongWord; var pLight : TD3DLight8) : HResult; stdcall;
    function GetLight(const Index : LongWord; out pLight : TD3DLight8) : HResult; stdcall;
    function LightEnable(const Index : LongWord; const Enable : BOOL) : HResult; stdcall;
    function GetLightEnable(const Index : LongWord; out pEnable : BOOL) : HResult; stdcall;
    function SetClipPlane(const Index : LongWord; pPlane : PSingle) : HResult; stdcall;
    function GetClipPlane(const Index : LongWord; out pPlane : Single) : HResult; stdcall;
    function SetRenderState(const State : TD3DRenderStateType; const Value : LongWord) : HResult; stdcall;
    function GetRenderState(const State : TD3DRenderStateType; out pValue : LongWord) : HResult; stdcall;
    function BeginStateBlock : HResult; stdcall;
    function EndStateBlock(out pToken : LongWord) : HResult; stdcall;
    function ApplyStateBlock(const Token : LongWord) : HResult; stdcall;
    function CaptureStateBlock(const Token : LongWord) : HResult; stdcall;
    function DeleteStateBlock(const Token : LongWord) : HResult; stdcall;
    function CreateStateBlock(const _Type : TD3DStateBlockType; out Token : LongWord) : HResult; stdcall;
    function SetClipStatus(var pClipStatus : TD3DClipStatus8) : HResult; stdcall;
    function GetClipStatus(out pClipStatus : TD3DClipStatus8) : HResult; stdcall;
    function GetTexture(const Stage : LongWord; out ppTexture : IDirect3DBaseTexture8) : HResult; stdcall;
    function SetTexture(const Stage : LongWord; pTexture : IDirect3DBaseTexture8) : HResult; stdcall;
    function GetTextureStageState(const Stage : LongWord; const _Type : TD3DTextureStageStateType; out pValue : LongWord) : HResult; stdcall;
    function SetTextureStageState(const Stage : LongWord; const _Type : TD3DTextureStageStateType; const Value : LongWord) : HResult; stdcall;
    function ValidateDevice(out pNumPasses : LongWord) : HResult; stdcall;
    function GetInfo(const DevInfoID : LongWord; pDevInfoStruct : Pointer; const DevInfoStructSize : LongWord) : HResult; stdcall;
    function SetPaletteEntries(const PaletteNumber : Cardinal; var pEntries : TPaletteEntry) : HResult; stdcall;
    function GetPaletteEntries(const PaletteNumber : Cardinal; out pEntries : TPaletteEntry) : HResult; stdcall;
    function SetCurrentTexturePalette(const PaletteNumber : Cardinal) : HResult; stdcall;
    function GetCurrentTexturePalette(out PaletteNumber : Cardinal) : HResult; stdcall;
    function DrawPrimitive(const PrimitiveType : TD3DPrimitiveType; const StartVertex, PrimitiveCount : Cardinal) : HResult; stdcall;
    function DrawIndexedPrimitive(const _Type : TD3DPrimitiveType; const minIndex, NumVertices, startIndex, primCount : Cardinal) : HResult; stdcall;
    function DrawPrimitiveUP(const PrimitiveType : TD3DPrimitiveType; const PrimitiveCount : Cardinal; pVertexStreamZeroData : Pointer; const VertexStreamZeroStride : Cardinal) : HResult; stdcall;
    function DrawIndexedPrimitiveUP(const PrimitiveType : TD3DPrimitiveType; const MinVertexIndex, NumVertexIndices, PrimitiveCount : Cardinal; pIndexData : Pointer; IndexDataFormat : TD3DFormat; pVertexStreamZeroData : Pointer; const VertexStreamZeroStride : Cardinal) : HResult; stdcall;
    function ProcessVertices(const SrcStartIndex, DestIndex, VertexCount : Cardinal; pDestBuffer : IDirect3DVertexBuffer8; const Flags : LongWord) : HResult; stdcall;
//    function CreateVertexShader(var pDeclaration, pFunction : LongWord; var pHandle : LongWord; const Usage : LongWord) : HResult; stdcall;

    function CreateVertexShader(var pDeclaration : LongWord; pFunction : PLongWord; var pHandle : LongWord; const Usage : LongWord) : HResult; stdcall;
//  function CreateVertexShader(pDeclaration, pFunction : PLongWord; var pHandle : LongWord; const Usage : LongWord) : HResult; stdcall;


    function SetVertexShader(const Handle : LongWord) : HResult; stdcall;
    function GetVertexShader(out pHandle : LongWord) : HResult; stdcall;
    function DeleteVertexShader(const Handle : LongWord) : HResult; stdcall;
    function SetVertexShaderConstant(const _Register : LongWord; {const} pConstantData : Pointer; const ConstantCount : LongWord) : HResult; stdcall;
    function GetVertexShaderConstant(const _Register : LongWord; {out} pConstantData : Pointer; const ConstantCount : LongWord) : HResult; stdcall;
    function GetVertexShaderDeclaration(const Handle : LongWord; {const} pData : Pointer; var pSizeOfData : LongWord) : HResult; stdcall;
    function GetVertexShaderFunction(const Handle : LongWord; {out} pData : Pointer; var pSizeOfData : LongWord) : HResult; stdcall;
    function SetStreamSource(const StreamNumber : Cardinal; const pStreamData : IDirect3DVertexBuffer8; const Stride : Cardinal) : HResult; stdcall;
    function GetStreamSource(const StreamNumber : Cardinal; out ppStreamData : IDirect3DVertexBuffer8; var pStride : Cardinal) : HResult; stdcall;
    function SetIndices(const pIndexData : IDirect3DIndexBuffer8; const BaseVertexIndex : Cardinal) : HResult; stdcall;
    function GetIndices(out ppIndexData : IDirect3DIndexBuffer8; out pBaseVertexIndex : Cardinal) : HResult; stdcall;
    function CreatePixelShader(var pFunction : LongWord; out pHandle : LongWord) : HResult; stdcall;
    function SetPixelShader(const Handle : LongWord) : HResult; stdcall;
    function GetPixelShader(out Handle : LongWord) : HResult; stdcall;
    function DeletePixelShader(const Handle : LongWord) : HResult; stdcall;
    function SetPixelShaderConstant(const _Register : LongWord; {const} pConstantData : Pointer; const ConstantCount : LongWord) : HResult; stdcall;
    function GetPixelShaderConstant(const _Register : LongWord; {out} pConstantData : Pointer; const ConstantCount : LongWord) : HResult; stdcall;
    function GetPixelShaderFunction(const Handle : LongWord; pData : Pointer; var pSizeOfData : LongWord) : HResult; stdcall;
    function DrawRectPatch(const Handle : Cardinal; pNumSegs : PSingle; pRectPatchInfo : PD3DRectPatch_Info) : HResult; stdcall;
    function DrawTriPatch(const Handle : Cardinal; pNumSegs : PSingle; pTriPatchInfo : PD3DTriPatch_Info) : HResult; stdcall;
    function DeletePatch(const Handle : Cardinal) : HResult; stdcall;
  end;

  IDirect3DSwapChain8 = interface (IUnknown)
    ['{928C088B-76B9-4C6B-A536-A590853876CD}']
    (*** IDirect3DSwapChain8 methods ***)
    function Present(pSourceRect, pDestRect : PRect; const hDestWindowOverride : HWND; pDirtyRegion : PRgnData) : HResult; stdcall;
    function GetBackBuffer(const BackBuffer : Cardinal; const _Type : TD3DBackBuffer_Type; out ppBackBuffer : IDirect3DSurface8) : HResult; stdcall;
  end;


  IDirect3DResource8 = interface (IUnknown)
    ['{1B36BB7B-09B7-410a-B445-7D1430D7B33F}']
    (*** IDirect3DResource8 methods ***)
    function GetDevice(out ppDevice : IDirect3DDevice8) : HResult; stdcall;
    function SetPrivateData(const refguid : TGUID; {const} pData : Pointer; const SizeOfData, Flags : LongWord) : HResult; stdcall;
    function GetPrivateData(const refguid : TGUID; {out} pData : Pointer; out pSizeOfData : LongWord) : HResult; stdcall;
    function FreePrivateData(const refguid : TGUID) : HResult; stdcall;
    function SetPriority(const PriorityNew : LongWord) : LongWord; stdcall;
    function GetPriority : LongWord; stdcall;
    procedure PreLoad stdcall;
    function GetType : TD3DResourceType; stdcall;
  end;

  IDirect3DBaseTexture8 = interface (IDirect3DResource8)
    ['{B4211CFA-51B9-4a9f-AB78-DB99B2BB678E}']
    (*** IDirect3DBaseTexture8 methods ***)
    function SetLOD(const LODNew : LongWord) : LongWord; stdcall;
    function GetLOD : LongWord; stdcall;
    function GetLevelCount : LongWord; stdcall;
  end;

  IDirect3DTexture8 = interface (IDirect3DBaseTexture8)
    ['{E4CDD575-2866-4f01-B12E-7EECE1EC9358}']
    (*** IDirect3DTexture8 methods ***)
    function GetLevelDesc(const Level : Cardinal; out pDesc : TD3DSurface_Desc) : HResult; stdcall;
    function GetSurfaceLevel(const Level : Cardinal; out ppSurfaceLevel : IDirect3DSurface8) : HResult; stdcall;
    function LockRect(const Level : Cardinal; out pLockedRect : TD3DLocked_Rect; pRect : PRect; const Flags : LongWord) : HResult; stdcall;
    function UnlockRect(const Level : Cardinal) : HResult; stdcall;
    function AddDirtyRect(pDirtyRect : PRect) : HResult; stdcall;
  end;

  IDirect3DVolumeTexture8 = interface (IDirect3DBaseTexture8)
    ['{E4CDD575-2866-4f01-B12E-7EECE1EC9358}']
    (*** IDirect3DVolumeTexture8 methods ***)
    function GetLevelDesc(const Level : Cardinal; out pDesc : TD3DVolume_Desc) : HResult; stdcall;
    function GetVolumeLevel(const Level : Cardinal; out ppVolumeLevel : IDirect3DVolume8) : HResult; stdcall;
    function LockBox(const Level : Cardinal; out pLockedVolume : TD3DLocked_Box; pBox : PD3DBox; const Flags : LongWord) : HResult; stdcall;
    function UnlockBox(const Level : Cardinal) : HResult; stdcall;
    function AddDirtyBox(pDirtyBox : PD3DBox) : HResult; stdcall;
  end;

  IDirect3DCubeTexture8 = interface (IDirect3DBaseTexture8)
    ['{3EE5B968-2ACA-4c34-8BB5-7E0C3D19B750}']
    (*** IDirect3DCubeTexture8 methods ***)
    function GetLevelDesc(const Level : Cardinal; out pDesc : TD3DSurface_Desc) : HResult; stdcall;
    function GetCubeMapSurface(const FaceType : TD3DCubeMap_Faces; const Level : Cardinal; out ppCubeMapSurface : IDirect3DSurface8) : HResult; stdcall;
    function LockRect(const FaceType : TD3DCubeMap_Faces; const Level : Cardinal; out pLockedRect : TD3DLocked_Rect; pRect : PRect; const Flags : LongWord) : HResult; stdcall;
    function UnlockRect(const FaceType : TD3DCubeMap_Faces; const Level : Cardinal) : HResult; stdcall;
    function AddDirtyRect(const FaceType : TD3DCubeMap_Faces; pDirtyRect : PRect) : HResult; stdcall;
  end;

  IDirect3DVertexBuffer8 = interface (IDirect3DResource8)
    ['{8AEEEAC7-05F9-44d4-B591-000B0DF1CB95}']
    (*** IDirect3DVertexBuffer8 methods ***)
    function Lock(const OffsetToLock, SizeToLock : Cardinal; var ppbData : PByte; const Flags : LongWord) : HResult; stdcall;
    function Unlock : HResult; stdcall;
    function GetDesc(out pDesc : TD3DVertexBuffer_Desc) : HResult; stdcall;
  end;

  IDirect3DIndexBuffer8 = interface (IDirect3DResource8)
    ['{0E689C9A-053D-44a0-9D92-DB0E3D750F86}']
    (*** IDirect3DIndexBuffer8 methods ***)
    function Lock(const OffsetToLock, SizeToLock : LongWord; var ppbData : PByte; const Flags : LongWord) : HResult; stdcall;
    function Unlock : HResult; stdcall;
    function GetDesc(out pDesc : TD3DIndexBuffer_Desc) : HResult; stdcall;
  end;



  IDirect3DSurface8 = interface (IUnknown)
    ['{B96EEBCA-B326-4ea5-882F-2FF5BAE021DD}']
    (*** IDirect3DSurface8 methods ***)
    function GetDevice(out ppDevice : IDirect3DDevice8) : HResult; stdcall;
    function SetPrivateData(const refguid : TGUID; {const} pData : Pointer; const SizeOfData, Flags : LongWord) : HResult; stdcall;
    function GetPrivateData(const refguid : TGUID; {out} pData : Pointer; out pSizeOfData : LongWord) : HResult; stdcall;
    function FreePrivateData(const refguid : TGUID) : HResult; stdcall;
    function GetContainer(const riid : TGUID; var ppContainer : Pointer) : HResult; stdcall;
    function GetDesc(out pDesc : TD3DSurface_Desc) : HResult; stdcall;
    function LockRect(out pLockedRect : TD3DLocked_Rect; pRect : PRect; const Flags : LongWord) : HResult; stdcall;
    function UnlockRect : HResult; stdcall;
  end;



  IDirect3DVolume8 = interface (IUnknown)
    ['{BD7349F5-14F1-42e4-9C79-972380DB40C0}']
    (*** IDirect3DVolume8 methods ***)
    function GetDevice(out ppDevice : IDirect3DDevice8) : HResult; stdcall;
    function SetPrivateData(const refguid : TGUID; {const} pData : Pointer; const SizeOfData, Flags : LongWord) : HResult; stdcall;
    function GetPrivateData(const refguid : TGUID; {out} pData : Pointer; out pSizeOfData : LongWord) : HResult; stdcall;
    function FreePrivateData(const refguid : TGUID) : HResult; stdcall;
    function GetContainer(const riid : TGUID; var ppContainer : Pointer) : HResult; stdcall;
    function GetDesc(out pDesc : TD3DVolume_Desc) : HResult; stdcall;
    function LockBox(out pLockedVolume : TD3DLocked_Box; pBox : PD3DBox; const Flags : LongWord) : HResult; stdcall;
    function UnlockBox : HResult; stdcall;
  end;


(****************************************************************************
 * Flags for SetPrivateData method on all D3D8 interfaces
 *
 * The passed pointer is an IUnknown ptr. The SizeOfData argument to SetPrivateData
 * must be set to sizeof( IUnknown* ). Direct3D will call AddRef through this
 * pointer and Release when the private data is destroyed. The data will be
 * destroyed when another SetPrivateData with the same GUID is set, when
 * FreePrivateData is called, or when the D3D8 object is freed.
 ****************************************************************************)
const
  D3DSPD_IUNKNOWN                         = $00000001;

(****************************************************************************
 *
 * Parameter for IDirect3D8 Enum and GetCaps8 functions to get the info for
 * the current mode only.
 *
 ****************************************************************************)

  D3DCURRENT_DISPLAY_MODE                 = $00EFFFFF;

(****************************************************************************
 *
 * Flags for IDirect3D8::CreateDevice's BehaviorFlags
 *
 ****************************************************************************)

  D3DCREATE_FPU_PRESERVE                  = $00000002;
  D3DCREATE_MULTITHREADED                 = $00000004;

  D3DCREATE_PUREDEVICE                    = $00000010;
  D3DCREATE_SOFTWARE_VERTEXPROCESSING     = $00000020;
  D3DCREATE_HARDWARE_VERTEXPROCESSING     = $00000040;
  D3DCREATE_MIXED_VERTEXPROCESSING        = $00000080;
{$IFNDEF DX8}
  D3DCREATE_DISABLE_DRIVER_MANAGEMENT     = $00000100;
{$ENDIF}

(****************************************************************************
 *
 * Parameter for IDirect3D8::CreateDevice's iAdapter
 *
 ****************************************************************************)

  D3DADAPTER_DEFAULT                      = 0;

(****************************************************************************
 *
 * Flags for IDirect3D8::EnumAdapters
 *
 ****************************************************************************)

  D3DENUM_NO_WHQL_LEVEL                   = $00000002;

(****************************************************************************
 *
 * Maximum number of back-buffers supported in DX8
 *
 ****************************************************************************)

  D3DPRESENT_BACK_BUFFERS_MAX             = 3;

(****************************************************************************
 *
 * Flags for IDirect3DDevice8::SetGammaRamp
 *
 ****************************************************************************)

  D3DSGR_NO_CALIBRATION                  = $00000000;
  D3DSGR_CALIBRATE                       = $00000001;

(****************************************************************************
 *
 * Flags for IDirect3DDevice8::SetCursorPosition
 *
 ****************************************************************************)

  D3DCURSOR_IMMEDIATE_UPDATE             = $00000001;

(****************************************************************************
 *
 * Flags for DrawPrimitive/DrawIndexedPrimitive
 *   Also valid for Begin/BeginIndexed
 *   Also valid for VertexBuffer::CreateVertexBuffer
 ****************************************************************************)


(*
 *  DirectDraw error codes
 *)

const
  _FACD3D = $876;
  MAKE_D3DHRESULT = (1 shl 31) or (_FACD3D shl 16);

(*
 * Direct3D Errors
 *)
  D3D_OK                                  = S_OK;

  D3DERR_WRONGTEXTUREFORMAT               = HResult(MAKE_D3DHRESULT + 2072);
  D3DERR_UNSUPPORTEDCOLOROPERATION        = HResult(MAKE_D3DHRESULT + 2073);
  D3DERR_UNSUPPORTEDCOLORARG              = HResult(MAKE_D3DHRESULT + 2074);
  D3DERR_UNSUPPORTEDALPHAOPERATION        = HResult(MAKE_D3DHRESULT + 2075);
  D3DERR_UNSUPPORTEDALPHAARG              = HResult(MAKE_D3DHRESULT + 2076);
  D3DERR_TOOMANYOPERATIONS                = HResult(MAKE_D3DHRESULT + 2077);
  D3DERR_CONFLICTINGTEXTUREFILTER         = HResult(MAKE_D3DHRESULT + 2078);
  D3DERR_UNSUPPORTEDFACTORVALUE           = HResult(MAKE_D3DHRESULT + 2079);
  D3DERR_CONFLICTINGRENDERSTATE           = HResult(MAKE_D3DHRESULT + 2081);
  D3DERR_UNSUPPORTEDTEXTUREFILTER         = HResult(MAKE_D3DHRESULT + 2082);
  D3DERR_CONFLICTINGTEXTUREPALETTE        = HResult(MAKE_D3DHRESULT + 2086);
  D3DERR_DRIVERINTERNALERROR              = HResult(MAKE_D3DHRESULT + 2087);

  D3DERR_NOTFOUND                         = HResult(MAKE_D3DHRESULT + 2150);
  D3DERR_MOREDATA                         = HResult(MAKE_D3DHRESULT + 2151);
  D3DERR_DEVICELOST                       = HResult(MAKE_D3DHRESULT + 2152);
  D3DERR_DEVICENOTRESET                   = HResult(MAKE_D3DHRESULT + 2153);
  D3DERR_NOTAVAILABLE                     = HResult(MAKE_D3DHRESULT + 2154);
  D3DERR_OUTOFVIDEOMEMORY                 = HResult(MAKE_D3DHRESULT + 380);
  D3DERR_INVALIDDEVICE                    = HResult(MAKE_D3DHRESULT + 2155);
  D3DERR_INVALIDCALL                      = HResult(MAKE_D3DHRESULT + 2156);
  D3DERR_DRIVERINVALIDCALL                = HResult(MAKE_D3DHRESULT + 2157);


(*
 * DLL Function for creating a Direct3D8 object. This object supports
 * enumeration and allows the creation of Direct3DDevice8 objects.
 * Pass the value of the constant D3D_SDK_VERSION to this function, so
 * that the run-time can validate that your application was compiled
 * against the right headers.
 *)

function Direct3DCreate8(SDKVersion : Cardinal) : IDirect3D8;

{$IFNDEF STATIC_LINKING}
var _Direct3DCreate8 : function (SDKVersion : Cardinal) : Pointer; stdcall;
{$ENDIF}

function DXGErrorString(ErrorValue : HResult) : string;

implementation

uses DXCommon;

function D3DCOLOR_ARGB(a, r, g, b : Cardinal) : TD3DColor;
begin
  Result := (a shl 24) or (r shl 16) or (g shl 8) or b;
end;

function D3DCOLOR_RGBA(r, g, b, a : Cardinal) : TD3DColor;
begin
  Result := D3DCOLOR_ARGB(a, r, g, b);
end;

function D3DCOLOR_XRGB(r, g, b : Cardinal) : TD3DColor;
begin
  Result := D3DCOLOR_ARGB($ff, r, g, b);
end;

function D3DCOLOR_COLORVALUE(r, g, b, a : Single) : TD3DColor;
begin
  Result := D3DCOLOR_RGBA(Byte(Round(r * 255)), Byte(Round(g * 255)), Byte(Round(b * 255)), Byte(Round(a * 255)))
end;

function D3DTS_WORLDMATRIX(index : LongWord) : TD3DTransformStateType;
begin
  Result := TD3DTransformStateType(index + 256);
end;

function D3DVSD_MAKETOKENTYPE(tokenType : TD3DVSDTokenType) : TD3DVSDTokenType;
begin
  Result := TD3DVSDTokenType((LongWord(tokenType) shl LongWord(D3DVSD_TOKENTYPESHIFT)) and LongWord(D3DVSD_TOKENTYPEMASK));
end;

function D3DVSD_STREAM(_StreamNumber : LongWord) : TD3DVSDTokenType;
begin
  Result := TD3DVSDTokenType(LongWord(D3DVSD_MAKETOKENTYPE(D3DVSD_TOKEN_STREAM)) or _StreamNumber);
end;

function D3DVSD_REG( _VertexRegister, _Type : LongWord) : TD3DVSDTokenType;
begin
  Result := TD3DVSDTokenType(LongWord(D3DVSD_MAKETOKENTYPE(D3DVSD_TOKEN_STREAMDATA)) or ((_Type shl LongWord(D3DVSD_DATATYPESHIFT)) or _VertexRegister))
end;

function D3DVSD_SKIP( _DWORDCount : LongWord) : TD3DVSDTokenType;
begin
  Result := TD3DVSDTokenType(LongWord(D3DVSD_MAKETOKENTYPE(D3DVSD_TOKEN_STREAMDATA)) or $10000000 or (_DWORDCount shl LongWord(D3DVSD_SKIPCOUNTSHIFT)))
end;

function D3DVSD_CONST( _ConstantAddress, _Count : LongWord)  : TD3DVSDTokenType;
begin
  Result := TD3DVSDTokenType(LongWord(D3DVSD_MAKETOKENTYPE(D3DVSD_TOKEN_CONSTMEM)) or (_Count shl LongWord(D3DVSD_CONSTCOUNTSHIFT)) or _ConstantAddress)
end;

function D3DVSD_TESSNORMAL( _VertexRegisterIn, _VertexRegisterOut : LongWord) : TD3DVSDTokenType;
begin
  Result := TD3DVSDTokenType(LongWord(D3DVSD_MAKETOKENTYPE(D3DVSD_TOKEN_TESSELLATOR)) or
            (_VertexRegisterIn shl LongWord(D3DVSD_VERTEXREGINSHIFT)) or
            ($02 shl LongWord(D3DVSD_DATATYPESHIFT)) or _VertexRegisterOut);
end;

function D3DVSD_TESSUV( _VertexRegister : LongWord) : TD3DVSDTokenType;
begin
  Result := TD3DVSDTokenType(LongWord(D3DVSD_MAKETOKENTYPE(D3DVSD_TOKEN_TESSELLATOR)) or $10000000 or
            ($01 shl LongWord(D3DVSD_DATATYPESHIFT)) or _VertexRegister);
end;

function D3DPS_VERSION(_Major, _Minor : LongWord) : LongWord;
begin
  Result := $FFFF0000 or (_Major shl 8 ) or _Minor;
end;

function D3DVS_VERSION(_Major, _Minor : LongWord) : LongWord;
begin
  Result := $FFFE0000 or (_Major shl 8 ) or _Minor;
end;

function D3DSHADER_VERSION_MAJOR(_Version : LongWord) : LongWord;
begin
  Result := (_Version shr 8 ) and $FF;
end;

function D3DSHADER_VERSION_MINOR(_Version : LongWord) : LongWord;
begin
  Result := (_Version shr 0) and $FF;
end;

function  D3DSHADER_COMMENT(_DWordSize : LongWord)  : LongWord;
begin
  Result := ((_DWordSize shl D3DSI_COMMENTSIZE_SHIFT) and D3DSI_COMMENTSIZE_MASK) or LongWord(D3DSIO_COMMENT);
end;

function D3DFVF_TEXCOORDSIZE3(CoordIndex : LongWord) : LongWord;
begin
  Result := D3DFVF_TEXTUREFORMAT3 shl (CoordIndex * 2 + 16)
end;

function D3DFVF_TEXCOORDSIZE2(CoordIndex : LongWord) : LongWord;
begin
  Result := D3DFVF_TEXTUREFORMAT2;
end;

function D3DFVF_TEXCOORDSIZE4(CoordIndex : LongWord) : LongWord;
begin
  Result := D3DFVF_TEXTUREFORMAT4 shl (CoordIndex * 2 + 16)
end;

function D3DFVF_TEXCOORDSIZE1(CoordIndex : LongWord) : LongWord;
begin
  Result := D3DFVF_TEXTUREFORMAT1 shl (CoordIndex * 2 + 16)
end;

function MAKEFOURCC(ch0, ch1, ch2, ch3 : Char) : LongWord;
begin
  Result := Byte(ch0) or (Byte(ch1) shl 8) or (Byte(ch2) shl 16) or (Byte(ch3) shl 24 );
end;

function DXGErrorString(ErrorValue : HResult) : string;
begin
//You should better use D3DXErrorString
  case ErrorValue of

    HResult(D3D_OK)                           : Result := 'No error occurred.';
    HResult(D3DERR_CONFLICTINGRENDERSTATE)    : Result := 'The currently set render states cannot be used together.';
    HResult(D3DERR_CONFLICTINGTEXTUREFILTER)  : Result := 'The current texture filters cannot be used together.';
    HResult(D3DERR_CONFLICTINGTEXTUREPALETTE) : Result := 'The current textures cannot be used simultaneously. This generally occurs when a multitexture device requires that all palletized textures simultaneously enabled also share the same palette.';
    HResult(D3DERR_DEVICELOST)                : Result := 'The device is lost and cannot be restored at the current time, so rendering is not possible.';
    HResult(D3DERR_DEVICENOTRESET)            : Result := 'The device cannot be reset.';
    HResult(D3DERR_DRIVERINTERNALERROR)       : Result := 'Internal driver error.';
    HResult(D3DERR_INVALIDCALL)               : Result := 'The method call is invalid. For example, a method''s parameter may have an invalid value.';
    HResult(D3DERR_INVALIDDEVICE)             : Result := 'The requested device type is not valid.';
    HResult(D3DERR_MOREDATA)                  : Result := 'There is more data available than the specified buffer size can hold.';
    HResult(D3DERR_NOTAVAILABLE)              : Result := 'This device does not support the queried technique.';
    HResult(D3DERR_NOTFOUND)                  : Result := 'The requested item was not found.';
    HResult(D3DERR_OUTOFVIDEOMEMORY)          : Result := 'Direct3D does not have enough display memory to perform the operation.';
    HResult(D3DERR_TOOMANYOPERATIONS)         : Result := 'The application is requesting more texture-filtering operations than the device';
    HResult(D3DERR_UNSUPPORTEDALPHAARG)       : Result := 'The device does not support a specified texture-blending argument for the alpha channel.';
    HResult(D3DERR_UNSUPPORTEDALPHAOPERATION) : Result := 'The device does not support a specified texture-blending operation for the alpha channel.';
    HResult(D3DERR_UNSUPPORTEDCOLORARG)       : Result := 'The device does not support a specified texture-blending argument for color values.';
    HResult(D3DERR_UNSUPPORTEDCOLOROPERATION) : Result := 'The device does not support a specified texture-blending operation for color values.';
    HResult(D3DERR_UNSUPPORTEDFACTORVALUE)    : Result := 'The device does not support the specified texture factor value.';
    HResult(D3DERR_UNSUPPORTEDTEXTUREFILTER)  : Result := 'The device does not support the specified texture filter.';
    HResult(D3DERR_WRONGTEXTUREFORMAT)        : Result := 'The pixel format of the texture surface is not valid.';
    HResult(E_FAIL)                           : Result := 'An undetermined error occurred inside the Direct3D subsystem.';
    HResult(E_INVALIDARG)                     : Result := 'An invalid parameter was passed to the returning function';
//    HResult(E_INVALIDCALL)                    : Result := 'The method call is invalid. For example, a method''s parameter may have an invalid value.';
    HResult(E_OUTOFMEMORY)                    : Result := 'Direct3D could not allocate sufficient memory to complete the call.';

    else Result := 'Unknown Error';
  end;
end;

{$IFDEF STATIC_LINKING}
function _Direct3DCreate8(SDKVersion : Cardinal) : Pointer; stdcall; external D3D8DLLName name 'Direct3DCreate8';
{$ENDIF}

function Direct3DCreate8(SDKVersion : Cardinal) : IDirect3D8;
begin
{$IFNDEF STATIC_LINKING}
  if Assigned(_Direct3DCreate8) then
  begin
{$ENDIF}
    Result := IDirect3D8(_Direct3DCreate8(SDKVersion));
    if Result <> nil then Result._Release;
{$IFNDEF STATIC_LINKING}
  end else Result := nil;
{$ENDIF}
end;

{$IFNDEF STATIC_LINKING}
initialization
begin
  if not IsNTandDelphiRunning then
  begin
    D3D8DLL := LoadLibrary(D3D8DLLName);
    if D3D8DLL <> 0 then
      _Direct3DCreate8 := GetProcAddress(D3D8DLL, 'Direct3DCreate8')
    else _Direct3DCreate8 := nil;
  end;
end;

finalization
begin
  FreeLibrary(D3D8DLL);
end;
{$ENDIF}

end.
