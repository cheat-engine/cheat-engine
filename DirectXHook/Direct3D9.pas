(*)
 [------------------------------------------------------------------------------
 [                _     _     _   _     _         _ ___   _ ___
 [              _| |___| |___| |_|_|  _| |_ _   _| |_  |_| | . |
 [             | . | -_| | . |   | |_| . |_'_|_| . |_  | . |_  |
 [             |___|___|_|  _|_|_|_|_|___|_,_|_|___|___|___|___|
 [                       |_|
 [
 [------------------------------------------------------------------------------
 [  Direct3D 9 Delphi Adaptation (c) by Tim Baumgarten
 [------------------------------------------------------------------------------
 [  Files    : d3d9.h
 [             d3d9types.h
 [             d3d9caps.h
 [  Modified : 10-Jul-2003
 [  E-Mail   : ampaze at gmx dot net
 [  Download : http://www.crazyentertainment.net
 [------------------------------------------------------------------------------
(*)

(*)
 [------------------------------------------------------------------------------
 [ History :
 [----------
 [ 10-Jul-2003 (Tim Baumgarten) : Changed some Enum Stuff that was broken 
 [ 23-Jan-2003 (Tim Baumgarten) : out out out, anyone need outs ?
 [ 12-Jan-2003 (Tim Baumgarten) : Improved some parts.
 [ 20-Dec-2002 (Tim Baumgarten) : first public version
 [                                most likely to contains bugs, please report
 [------------------------------------------------------------------------------
(*)

unit Direct3D9;

{$INCLUDE jedi.inc}

{$MINENUMSIZE 4}
{$ALIGN ON}

//Remove dot to make all enums to be const's
{$DEFINE NOENUMS}

//Remove dot to enable static linking
{$DEFINE STATIC_LINKING}

{$IFDEF DELPHI7_UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFNDEF DELPHI6_UP}
  {$DEFINE NOENUMS}
{$ENDIF}


interface

uses
  Windows;

{$IFNDEF STATIC_LINKING}
var
  d3d9dll : HMODULE = 0;
{$ENDIF}

const
  d3d9dllname = 'd3d9.dll';

type
  PLongWord = ^LongWord;

(*)
 *******************************************************************************
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File    : d3d9types.h
 *  Content : Direct3D capabilities include file
 *
 *******************************************************************************
(*)

const
  DIRECT3D_VERSION = $0900;

type
  // D3DCOLOR is equivalent to D3DFMT_A8R8G8B8
  TD3DColor = LongWord;

  // maps unsigned 8 bits/channel to D3DCOLOR
  function D3DCOLOR_ARGB(a, r, g, b : Cardinal) : TD3DColor;
  function D3DCOLOR_RGBA(r, g, b, a : Cardinal) : TD3DColor;
  function D3DCOLOR_XRGB(r, g, b : Cardinal) : TD3DColor;

  function D3DCOLOR_XYUV(y, u, v  : Cardinal) : TD3DColor;
  function D3DCOLOR_AYUV(a, y, u, v : Cardinal) : TD3DColor;

  // maps floating point channels (0.f to 1.f range) to D3DCOLOR
  function D3DCOLOR_COLORVALUE(r, g, b, a : Single) : TD3DColor;

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
      0 : (_00, _01, _02, _03,
           _10, _11, _12, _13,
           _20, _21, _22, _23,
           _30, _31, _32, _33 : Single);
      1 : (m : array [0..3, 0..3] of Single);
  end;

  PD3DViewport9 = ^TD3DViewport9;
  TD3DViewport9 = packed record
    X      : LongWord;
    Y      : LongWord;  (* Viewport Top left *)
    Width  : LongWord;
    Height : LongWord;  (* Viewport Dimensions *)
    MinZ   : Single;    (* Min/max of clip Volume *)
    MaxZ   : Single
  end;

(* Values for clip fields.*)

const
  // Max number of user clipping planes; supported in D3D.
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
  D3DCS_LEFT    = $00000001;
  D3DCS_RIGHT   = $00000002;
  D3DCS_TOP     = $00000004;
  D3DCS_BOTTOM  = $00000008;
  D3DCS_FRONT   = $00000010;
  D3DCS_BACK    = $00000020;
  D3DCS_PLANE0  = $00000040;
  D3DCS_PLANE1  = $00000080;
  D3DCS_PLANE2  = $00000100;
  D3DCS_PLANE3  = $00000200;
  D3DCS_PLANE4  = $00000400;
  D3DCS_PLANE5  = $00000800;

  D3DCS_ALL = (D3DCS_LEFT or D3DCS_RIGHT or D3DCS_TOP or
               D3DCS_BOTTOM or D3DCS_FRONT or D3DCS_BACK or
               D3DCS_PLANE0 or D3DCS_PLANE1 or D3DCS_PLANE2 or
               D3DCS_PLANE3 or D3DCS_PLANE4 or D3DCS_PLANE5);

type
  PD3DClipStatus9 = ^TD3DClipStatus9;
  TD3DClipStatus9 = packed record
    ClipUnion        : LongWord;
    ClipIntersection : LongWord;
  end;

  PD3DMaterial9 = ^TD3DMaterial9;
  TD3DMaterial9 = packed record
    Diffuse  : TD3DColorValue;  (* Diffuse color RGBA *)
    Ambient  : TD3DColorValue;  (* Ambient color RGB *)
    Specular : TD3DColorValue;  (* Specular 'shininess' *)
    Emissive : TD3DColorValue;  (* Emissive color RGB *)
    Power    : Single;          (* Sharpness if specular highlight *)
  end;

type
  TD3DLightType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DLIGHT_POINT       = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DLIGHT_SPOT        = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DLIGHT_DIRECTIONAL = 3{$IFNDEF NOENUMS}){$ENDIF};

type
  PD3DLight9 = ^TD3DLight9;
  TD3DLight9 = packed record
    _Type        : TD3DLightType;   (* Type of light source *)
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

type
  TD3DShadeMode = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DSHADE_FLAT               = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSHADE_GOURAUD            = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSHADE_PHONG              = 3{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DFillMode = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DFILL_POINT               = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFILL_WIREFRAME           = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFILL_SOLID               = 3{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DBlend = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
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
    D3DBLEND_BLENDFACTOR        = 14{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} (* Only supported if D3DPBLENDCAPS_BLENDFACTOR is on *)
    D3DBLEND_INVBLENDFACTOR     = 15{$IFNDEF NOENUMS}){$ENDIF};        (* Only supported if D3DPBLENDCAPS_BLENDFACTOR is on *)

type
  TD3DBLendOp = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DBLENDOP_ADD              = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DBLENDOP_SUBTRACT         = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DBLENDOP_REVSUBTRACT      = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DBLENDOP_MIN              = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DBLENDOP_MAX              = 5{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DTextureAddress = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DTADDRESS_WRAP            = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DTADDRESS_MIRROR          = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DTADDRESS_CLAMP           = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DTADDRESS_BORDER          = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DTADDRESS_MIRRORONCE      = 5{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DCull = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DCULL_NONE                = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DCULL_CW                  = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DCULL_CCW                 = 3{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DCmpFunc = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DCMP_NEVER                = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DCMP_LESS                 = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DCMP_EQUAL                = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DCMP_LESSEQUAL            = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DCMP_GREATER              = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DCMP_NOTEQUAL             = 6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DCMP_GREATEREQUAL         = 7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DCMP_ALWAYS               = 8{$IFNDEF NOENUMS}){$ENDIF};

type TD3DStencilOp = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DSTENCILOP_KEEP           = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSTENCILOP_ZERO           = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSTENCILOP_REPLACE        = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSTENCILOP_INCRSAT        = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSTENCILOP_DECRSAT        = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSTENCILOP_INVERT         = 6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSTENCILOP_INCR           = 7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSTENCILOP_DECR           = 8{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DFogMode = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DFOG_NONE                 = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFOG_EXP                  = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFOG_EXP2                 = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFOG_LINEAR               = 3{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DZBufferType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DZB_FALSE                 = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DZB_TRUE                  = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Z buffering
    D3DZB_USEW                  = 2{$IFNDEF NOENUMS}){$ENDIF};        // W buffering

// Primitives supported by draw-primitive API
type
  TD3DPrimitiveType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DPT_POINTLIST             = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DPT_LINELIST              = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DPT_LINESTRIP             = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DPT_TRIANGLELIST          = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DPT_TRIANGLESTRIP         = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DPT_TRIANGLEFAN           = 6{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DTransformStateType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DTS_VIEW          = 02{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DTS_PROJECTION    = 03{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DTS_TEXTURE0      = 16{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DTS_TEXTURE1      = 17{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DTS_TEXTURE2      = 18{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DTS_TEXTURE3      = 19{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DTS_TEXTURE4      = 20{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DTS_TEXTURE5      = 21{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DTS_TEXTURE6      = 22{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DTS_TEXTURE7      = 23{$IFNDEF NOENUMS}){$ENDIF};

function D3DTS_WORLDMATRIX(Index : LongWord) : TD3DTransformStateType; // (D3DTRANSFORMSTATETYPE)(index + 256)

const
  D3DTS_WORLD         = 0 + 256; // D3DTS_WORLDMATRIX(0)
  D3DTS_WORLD1        = 1 + 256; // D3DTS_WORLDMATRIX(1)
  D3DTS_WORLD2        = 2 + 256; // D3DTS_WORLDMATRIX(2)
  D3DTS_WORLD3        = 3 + 256; // D3DTS_WORLDMATRIX(3)

type
  TD3DRenderStateType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DRS_ZENABLE                    = 7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    (* D3DZBUFFERTYPE (or TRUE/FALSE for legacy) *)
    D3DRS_FILLMODE                   = 8{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    (* D3DFILL_MODE        *)
    D3DRS_SHADEMODE                  = 9{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    (* D3DSHADEMODE *)
    D3DRS_ZWRITEENABLE               = 14{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* TRUE to enable z writes *)
    D3DRS_ALPHATESTENABLE            = 15{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* TRUE to enable alpha tests *)
    D3DRS_LASTPIXEL                  = 16{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* TRUE for last-pixel on lines *)
    D3DRS_SRCBLEND                   = 19{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DBLEND *)
    D3DRS_DESTBLEND                  = 20{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DBLEND *)
    D3DRS_CULLMODE                   = 22{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DCULL *)
    D3DRS_ZFUNC                      = 23{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DCMPFUNC *)
    D3DRS_ALPHAREF                   = 24{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DFIXED *)
    D3DRS_ALPHAFUNC                  = 25{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DCMPFUNC *)
    D3DRS_DITHERENABLE               = 26{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* TRUE to enable dithering *)
    D3DRS_ALPHABLENDENABLE           = 27{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* TRUE to enable alpha blending *)
    D3DRS_FOGENABLE                  = 28{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* TRUE to enable fog blending *)
    D3DRS_SPECULARENABLE             = 29{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* TRUE to enable specular *)
    D3DRS_FOGCOLOR                   = 34{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DCOLOR *)
    D3DRS_FOGTABLEMODE               = 35{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DFOGMODE *)
    D3DRS_FOGSTART                   = 36{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* Fog start (for both vertex and pixel fog) *)
    D3DRS_FOGEND                     = 37{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* Fog end      *)
    D3DRS_FOGDENSITY                 = 38{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* Fog density  *)
    D3DRS_RANGEFOGENABLE             = 48{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* Enables range-based fog *)
    D3DRS_STENCILENABLE              = 52{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* BOOL enable/disable stenciling *)
    D3DRS_STENCILFAIL                = 53{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DSTENCILOP to do if stencil test fails *)
    D3DRS_STENCILZFAIL               = 54{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DSTENCILOP to do if stencil test passes and Z test fails *)
    D3DRS_STENCILPASS                = 55{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DSTENCILOP to do if both stencil and Z tests pass *)
    D3DRS_STENCILFUNC                = 56{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DCMPFUNC fn.  Stencil Test passes if ((ref & mask) stencilfn (stencil & mask)) is true *)
    D3DRS_STENCILREF                 = 57{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* Reference value used in stencil test *)
    D3DRS_STENCILMASK                = 58{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* Mask value used in stencil test *)
    D3DRS_STENCILWRITEMASK           = 59{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* Write mask applied to values written to stencil buffer *)
    D3DRS_TEXTUREFACTOR              = 60{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   (* D3DCOLOR used for multi-texture blend *)
    D3DRS_WRAP0                      = 128{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* wrap for 1st texture coord. set *)
    D3DRS_WRAP1                      = 129{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* wrap for 2nd texture coord. set *)
    D3DRS_WRAP2                      = 130{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* wrap for 3rd texture coord. set *)
    D3DRS_WRAP3                      = 131{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* wrap for 4th texture coord. set *)
    D3DRS_WRAP4                      = 132{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* wrap for 5th texture coord. set *)
    D3DRS_WRAP5                      = 133{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* wrap for 6th texture coord. set *)
    D3DRS_WRAP6                      = 134{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* wrap for 7th texture coord. set *)
    D3DRS_WRAP7                      = 135{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* wrap for 8th texture coord. set *)
    D3DRS_CLIPPING                   = 136{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_LIGHTING                   = 137{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_AMBIENT                    = 139{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_FOGVERTEXMODE              = 140{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_COLORVERTEX                = 141{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_LOCALVIEWER                = 142{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_NORMALIZENORMALS           = 143{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_DIFFUSEMATERIALSOURCE      = 145{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_SPECULARMATERIALSOURCE     = 146{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_AMBIENTMATERIALSOURCE      = 147{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_EMISSIVEMATERIALSOURCE     = 148{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_VERTEXBLEND                = 151{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_CLIPPLANEENABLE            = 152{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_POINTSIZE                  = 154{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* float point size *)
    D3DRS_POINTSIZE_MIN              = 155{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* float point size min threshold *)
    D3DRS_POINTSPRITEENABLE          = 156{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* BOOL point texture coord control *)
    D3DRS_POINTSCALEENABLE           = 157{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* BOOL point size scale enable *)
    D3DRS_POINTSCALE_A               = 158{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* float point attenuation A value *)
    D3DRS_POINTSCALE_B               = 159{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* float point attenuation B value *)
    D3DRS_POINTSCALE_C               = 160{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* float point attenuation C value *)
    D3DRS_MULTISAMPLEANTIALIAS       = 161{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // BOOL - set to do FSAA with multisample buffer *)
    D3DRS_MULTISAMPLEMASK            = 162{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // DWORD - per-sample enable/disable
    D3DRS_PATCHEDGESTYLE             = 163{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // Sets whether patch edges will use float style tessellation
    D3DRS_DEBUGMONITORTOKEN          = 165{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // DEBUG ONLY - token to debug monitor
    D3DRS_POINTSIZE_MAX              = 166{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* float point size max threshold *)
    D3DRS_INDEXEDVERTEXBLENDENABLE   = 167{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_COLORWRITEENABLE           = 168{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // per-channel write enable
    D3DRS_TWEENFACTOR                = 170{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // float tween factor
    D3DRS_BLENDOP                    = 171{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // D3DBLENDOP setting
    D3DRS_POSITIONDEGREE             = 172{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // NPatch position interpolation degree. D3DDEGREE_LINEAR or D3DDEGREE_CUBIC (default)
    D3DRS_NORMALDEGREE               = 173{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // NPatch normal interpolation degree. D3DDEGREE_LINEAR (default) or D3DDEGREE_QUADRATIC
    D3DRS_SCISSORTESTENABLE          = 174{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_SLOPESCALEDEPTHBIAS        = 175{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_ANTIALIASEDLINEENABLE      = 176{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_MINTESSELLATIONLEVEL       = 178{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_MAXTESSELLATIONLEVEL       = 179{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_ADAPTIVETESS_X             = 180{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_ADAPTIVETESS_Y             = 181{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_ADAPTIVETESS_Z             = 182{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_ADAPTIVETESS_W             = 183{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_ENABLEADAPTIVETESSELLATION = 184{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_TWOSIDEDSTENCILMODE        = 185{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* BOOL enable/disable 2 sided stenciling *)
    D3DRS_CCW_STENCILFAIL            = 186{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* D3DSTENCILOP to do if ccw stencil test fails *)
    D3DRS_CCW_STENCILZFAIL           = 187{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* D3DSTENCILOP to do if ccw stencil test passes and Z test fails *)
    D3DRS_CCW_STENCILPASS            = 188{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* D3DSTENCILOP to do if both ccw stencil and Z tests pass *)
    D3DRS_CCW_STENCILFUNC            = 189{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* D3DCMPFUNC fn.  ccw Stencil Test passes if ((ref & mask) stencilfn (stencil & mask)) is true *)
    D3DRS_COLORWRITEENABLE1          = 190{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* Additional ColorWriteEnables for the devices that support D3DPMISCCAPS_INDEPENDENTWRITEMASKS *)
    D3DRS_COLORWRITEENABLE2          = 191{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* Additional ColorWriteEnables for the devices that support D3DPMISCCAPS_INDEPENDENTWRITEMASKS *)
    D3DRS_COLORWRITEENABLE3          = 192{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* Additional ColorWriteEnables for the devices that support D3DPMISCCAPS_INDEPENDENTWRITEMASKS *)
    D3DRS_BLENDFACTOR                = 193{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* D3DCOLOR used for a constant blend factor during alpha blending for devices that support D3DPBLENDCAPS_BLENDFACTOR *)
    D3DRS_SRGBWRITEENABLE            = 194{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* Enable rendertarget writes to be DE-linearized to SRGB (for formats that expose D3DUSAGE_QUERY_SRGBWRITE) *)
    D3DRS_DEPTHBIAS                  = 195{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_WRAP8                      = 198{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* Additional wrap states for vs_3_0+ attributes with D3DDECLUSAGE_TEXCOORD *)
    D3DRS_WRAP9                      = 199{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_WRAP10                     = 200{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_WRAP11                     = 201{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_WRAP12                     = 202{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_WRAP13                     = 203{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_WRAP14                     = 204{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_WRAP15                     = 205{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRS_SEPARATEALPHABLENDENABLE   = 206{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* TRUE to enable a separate blending function for the alpha channel *)
    D3DRS_SRCBLENDALPHA              = 207{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* SRC blend factor for the alpha channel when D3DRS_SEPARATEDESTALPHAENABLE is TRUE *)
    D3DRS_DESTBLENDALPHA             = 208{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* DST blend factor for the alpha channel when D3DRS_SEPARATEDESTALPHAENABLE is TRUE *)
    D3DRS_BLENDOPALPHA               = 209{$IFNDEF NOENUMS}){$ENDIF};          (* Blending operation for the alpha channel when D3DRS_SEPARATEDESTALPHAENABLE is TRUE *)

const
  // Maximum number of simultaneous render targets D3D supports
  D3D_MAX_SIMULTANEOUS_RENDERTARGETS = 4;

// Values for material source
type
  TD3DMaterialColorSource = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DMCS_MATERIAL         = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // Color from material is used
    D3DMCS_COLOR1           = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // Diffuse vertex color is used
    D3DMCS_COLOR2           = 2{$IFNDEF NOENUMS}){$ENDIF};           // Specular vertex color is used

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

(*)
 * State enumerants for per-stage processing of fixed function pixel processing
 * Two of these affect fixed function vertex processing as well: TEXTURETRANSFORMFLAGS and TEXCOORDINDEX.
(*)

type
  TD3DTextureStageStateType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
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
    D3DTSS_BUMPENVLSCALE         = 22{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* float scale for bump map luminance *)
    D3DTSS_BUMPENVLOFFSET        = 23{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* float offset for bump map luminance *)
    D3DTSS_TEXTURETRANSFORMFLAGS = 24{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTEXTURETRANSFORMFLAGS controls texture transform *)
    D3DTSS_COLORARG0             = 26{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTA_* third arg for triadic ops *)
    D3DTSS_ALPHAARG0             = 27{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTA_* third arg for triadic ops *)
    D3DTSS_RESULTARG             = 28{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}     (* D3DTA_* arg for result (CURRENT or TEMP) *)
    D3DTSS_CONSTANT              = 32{$IFNDEF NOENUMS}){$ENDIF};            (* Per-stage constant D3DTA_CONSTANT *)

(*)
 * State enumerants for per-sampler texture processing.
(*)

type
  TD3DSamplerStateType  = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}  
    D3DSAMP_ADDRESSU       = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* D3DTEXTUREADDRESS for U coordinate *)
    D3DSAMP_ADDRESSV       = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* D3DTEXTUREADDRESS for V coordinate *)
    D3DSAMP_ADDRESSW       = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* D3DTEXTUREADDRESS for W coordinate *)
    D3DSAMP_BORDERCOLOR    = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* D3DCOLOR *)
    D3DSAMP_MAGFILTER      = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* D3DTEXTUREFILTER filter to use for magnification *)
    D3DSAMP_MINFILTER      = 6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* D3DTEXTUREFILTER filter to use for minification *)
    D3DSAMP_MIPFILTER      = 7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* D3DTEXTUREFILTER filter to use between mipmaps during minification *)
    D3DSAMP_MIPMAPLODBIAS  = 8{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* float Mipmap LOD bias *)
    D3DSAMP_MAXMIPLEVEL    = 9{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  (* DWORD 0..(n-1) LOD index of largest map to use (0 == largest) *)
    D3DSAMP_MAXANISOTROPY  = 10{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} (* DWORD maximum anisotropy *)
    D3DSAMP_SRGBTEXTURE    = 11{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} (* Default = 0 (which means Gamma 1.0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
                                                                     no correction required.) else correct for
                                                                     Gamma = 2.2 *)
    D3DSAMP_ELEMENTINDEX   = 12{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} (* When multi-element texture is assigned to sampler{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} this
                                                                     indicates which element index to use.  Default = 0.  *)
    D3DSAMP_DMAPOFFSET     = 13{$IFNDEF NOENUMS}){$ENDIF};        (* Offset in vertices in the pre-sampled displacement map.
                                                                     Only valid for D3DDMAPSAMPLER sampler  *)
                                                                     
(* Special sampler which is used in the tesselator *)
const
  D3DDMAPSAMPLER = 256;

// Samplers used in vertex shaders
  D3DVERTEXTEXTURESAMPLER0 = (D3DDMAPSAMPLER + 1);
  D3DVERTEXTEXTURESAMPLER1 = (D3DDMAPSAMPLER + 2);
  D3DVERTEXTEXTURESAMPLER2 = (D3DDMAPSAMPLER + 3);
  D3DVERTEXTEXTURESAMPLER3 = (D3DDMAPSAMPLER + 4);

// Values, used with D3DTSS_TEXCOORDINDEX, to specify that the vertex data(position
// and normal in the camera space) should be taken as texture coordinates
// Low 16 bits are used to specify texture coordinate index, to take the WRAP mode from

  D3DTSS_TCI_PASSTHRU                    = $00000000;
  D3DTSS_TCI_CAMERASPACENORMAL           = $00010000;
  D3DTSS_TCI_CAMERASPACEPOSITION         = $00020000;
  D3DTSS_TCI_CAMERASPACEREFLECTIONVECTOR = $00030000;
  D3DTSS_TCI_SPHEREMAP                   = $00040000;

(*)
 * Enumerations for COLOROP and ALPHAOP texture blending operations set in
 * texture processing stage controls in D3DTSS.
(*)



type
  TD3DTextureOp = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
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
    D3DTOP_LERP                      = 26{$IFNDEF NOENUMS}){$ENDIF};         // (Arg0)*Arg1 + (1-Arg0)*Arg2

const
(*)
 * Values for COLORARG0;1;2; ALPHAARG0;1;2; and RESULTARG texture blending
 * operations set in texture processing stage controls in D3DRENDERSTATE.
(*)

  D3DTA_SELECTMASK        = $0000000f;  // mask for arg selector
  D3DTA_DIFFUSE           = $00000000;  // select diffuse color (read only)
  D3DTA_CURRENT           = $00000001;  // select stage destination register (read/write)
  D3DTA_TEXTURE           = $00000002;  // select texture color (read only)
  D3DTA_TFACTOR           = $00000003;  // select D3DRS_TEXTUREFACTOR (read only)
  D3DTA_SPECULAR          = $00000004;  // select specular color (read only)
  D3DTA_TEMP              = $00000005;  // select temporary register color (read/write)
  D3DTA_CONSTANT          = $00000006;  // select texture stage constant
  D3DTA_COMPLEMENT        = $00000010;  // take 1.0 - x (read modifier)
  D3DTA_ALPHAREPLICATE    = $00000020;  // replicate alpha to color components (read modifier)

// Values for D3DTSS_***FILTER texture stage states

type
  TD3DTextureFilterType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DTEXF_NONE            = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // filtering disabled (valid for mip filter only)
    D3DTEXF_POINT           = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // nearest
    D3DTEXF_LINEAR          = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // linear interpolation
    D3DTEXF_ANISOTROPIC     = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // anisotropic
    D3DTEXF_PYRAMIDALQUAD   = 6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // 4-sample tent
    D3DTEXF_GAUSSIANQUAD    = 7{$IFNDEF NOENUMS}){$ENDIF};           // 4-sample gaussian

const
(* Bits for Flags in ProcessVertices call *)
  D3DPV_DONOTCOPYDATA  = 1 shl 0;

//-------------------------------------------------------------------

// Flexible vertex format bits
//
  D3DFVF_RESERVED0         = $001;
  D3DFVF_POSITION_MASK     = $400E;
  D3DFVF_XYZ               = $002;
  D3DFVF_XYZRHW            = $004;
  D3DFVF_XYZB1             = $006;
  D3DFVF_XYZB2             = $008;
  D3DFVF_XYZB3             = $00a;
  D3DFVF_XYZB4             = $00c;
  D3DFVF_XYZB5             = $00e;
  D3DFVF_XYZW              = $4002;

  D3DFVF_NORMAL            = $010;
  D3DFVF_PSIZE             = $020;
  D3DFVF_DIFFUSE           = $040;
  D3DFVF_SPECULAR          = $080;

  D3DFVF_TEXCOUNT_MASK     = $f00;
  D3DFVF_TEXCOUNT_SHIFT    = 8;
  D3DFVF_TEX0              = $000;
  D3DFVF_TEX1              = $100;
  D3DFVF_TEX2              = $200;
  D3DFVF_TEX3              = $300;
  D3DFVF_TEX4              = $400;
  D3DFVF_TEX5              = $500;
  D3DFVF_TEX6              = $600;
  D3DFVF_TEX7              = $700;
  D3DFVF_TEX8              = $800;

  D3DFVF_LASTBETA_UBYTE4   = $1000;
  D3DFVF_LASTBETA_D3DCOLOR = $8000;

  D3DFVF_RESERVED2         = $6000;  // 2 reserved bits

//---------------------------------------------------------------------
// Vertex Shaders
//

// Vertex shader declaration

// Vertex element semantics
//
{$MINENUMSIZE 1}
type
  TD3DDeclUsage = {$IFNDEF NOENUMS}({$ELSE}Byte;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DDECLUSAGE_POSITION     = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDECLUSAGE_BLENDWEIGHT  = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDECLUSAGE_BLENDINDICES = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDECLUSAGE_NORMAL       = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDECLUSAGE_PSIZE        = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDECLUSAGE_TEXCOORD     = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDECLUSAGE_TANGENT      = 6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDECLUSAGE_BINORMAL     = 7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDECLUSAGE_TESSFACTOR   = 8{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDECLUSAGE_POSITIONT    = 9{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDECLUSAGE_COLOR        = 10{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDECLUSAGE_FOG          = 11{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDECLUSAGE_DEPTH        = 12{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDECLUSAGE_SAMPLE       = 13{$IFNDEF NOENUMS}){$ENDIF};

{$MINENUMSIZE 4}
const
  MAXD3DDECLUSAGE         = D3DDECLUSAGE_SAMPLE;
  MAXD3DDECLUSAGEINDEX    = 15;
  MAXD3DDECLLENGTH        = 64; // does not include "end" marker vertex element

{$MINENUMSIZE 1}
type
  TD3DDeclMethod = {$IFNDEF NOENUMS}({$ELSE}Byte;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DDECLMETHOD_DEFAULT          = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDECLMETHOD_PARTIALU         = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDECLMETHOD_PARTIALV         = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDECLMETHOD_CROSSUV          = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Normal
    D3DDECLMETHOD_UV               = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDECLMETHOD_LOOKUP           = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Lookup a displacement map
    D3DDECLMETHOD_LOOKUPPRESAMPLED = 6{$IFNDEF NOENUMS}){$ENDIF};        // Lookup a pre-sampled displacement map

{$MINENUMSIZE 4}
const
  MAXD3DDECLMETHOD = D3DDECLMETHOD_LOOKUPPRESAMPLED;

// Declarations for _Type fields
//
{$MINENUMSIZE 1}
type
  TD3DDeclType = {$IFNDEF NOENUMS}({$ELSE}Byte;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DDECLTYPE_FLOAT1    =  0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // 1D float expanded to (value{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} 0.{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} 0.{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} 1.)
    D3DDECLTYPE_FLOAT2    =  1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // 2D float expanded to (value{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} value{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} 0.{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} 1.)
    D3DDECLTYPE_FLOAT3    =  2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // 3D float expanded to (value{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} value{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} value{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} 1.)
    D3DDECLTYPE_FLOAT4    =  3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // 4D float
    D3DDECLTYPE_D3DCOLOR  =  4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // 4D packed unsigned bytes mapped to 0. to 1. range
                                                                  // Input is in D3DCOLOR format (ARGB) expanded to (R{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} G{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} B{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} A)
    D3DDECLTYPE_UBYTE4    =  5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // 4D unsigned byte
    D3DDECLTYPE_SHORT2    =  6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // 2D signed short expanded to (value{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} value{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} 0.{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} 1.)
    D3DDECLTYPE_SHORT4    =  7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // 4D signed short

    // The following types are valid only with vertex shaders >= 2.0

    D3DDECLTYPE_UBYTE4N   =  8{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // Each of 4 bytes is normalized by dividing to 255.0
    D3DDECLTYPE_SHORT2N   =  9{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // 2D signed short normalized (v[0]/32767.0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}v[1]/32767.0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}1)
    D3DDECLTYPE_SHORT4N   = 10{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // 4D signed short normalized (v[0]/32767.0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}v[1]/32767.0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}v[2]/32767.0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}v[3]/32767.0)
    D3DDECLTYPE_USHORT2N  = 11{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // 2D unsigned short normalized (v[0]/65535.0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}v[1]/65535.0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}1)
    D3DDECLTYPE_USHORT4N  = 12{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // 4D unsigned short normalized (v[0]/65535.0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}v[1]/65535.0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}v[2]/65535.0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}v[3]/65535.0)
    D3DDECLTYPE_UDEC3     = 13{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // 3D unsigned 10 10 10 format expanded to (value{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} value{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} value{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} 1)
    D3DDECLTYPE_DEC3N     = 14{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // 3D signed 10 10 10 format normalized and expanded to (v[0]/511.0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} v[1]/511.0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} v[2]/511.0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} 1)
    D3DDECLTYPE_FLOAT16_2 = 15{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // Two 16-bit floating point values{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} expanded to (value{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} value{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} 1)
    D3DDECLTYPE_FLOAT16_4 = 16{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // Four 16-bit floating point values
    D3DDECLTYPE_UNUSED    = 17{$IFNDEF NOENUMS}){$ENDIF};         // When the type field in a decl is unused.

{$MINENUMSIZE 4}
const
  MAXD3DDECLTYPE = D3DDECLTYPE_UNUSED;

type
  PD3DVertexElement9 = ^TD3DVertexElement9;
  TD3DVertexElement9 = packed record
    Stream     : Word;  // Stream index
    Offset     : Word;  // Offset in the stream in bytes
    _Type      : TD3DDeclType;    // Data type
    Method     : TD3DDeclMethod;  // Processing method
    Usage      : TD3DDeclUsage;   // Semantics
    UsageIndex : Byte;  // Semantic index
  end;

// This is used to initialize the last vertex element in a vertex declaration
// array
//
const
  D3DDECL_END : TD3DVertexElement9 = (Stream : $FF; Offset : 0; _Type : D3DDECLTYPE_UNUSED; Method : D3DDECLMETHOD_DEFAULT; Usage : D3DDECLUSAGE_POSITION; UsageIndex : 0);

// Maximum supported number of texture coordinate sets
const
  D3DDP_MAXTEXCOORD = 8;

//---------------------------------------------------------------------
//
// The internal format of Pixel Shader (PS) & Vertex Shader (VS)
// Instruction Tokens is defined in the Direct3D Device Driver Kit
//
//---------------------------------------------------------------------

//
// Instruction Token Bit Definitions
//
const
  D3DSI_OPCODE_MASK       = $0000FFFF;

  D3DSI_INSTLENGTH_MASK   = $0F000000;
  D3DSI_INSTLENGTH_SHIFT  = 24;

type
  TD3DShaderInstructionOpcodeType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DSIO_NOP          = 00{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_MOV          = 01{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_ADD          = 02{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_SUB          = 03{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_MAD          = 04{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_MUL          = 05{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_RCP          = 06{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_RSQ          = 07{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_DP3          = 08{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_DP4          = 09{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_MIN          = 10{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_MAX          = 11{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_SLT          = 12{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_SGE          = 13{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_EXP          = 14{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_LOG          = 15{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_LIT          = 16{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_DST          = 17{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_LRP          = 18{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_FRC          = 19{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_M4x4         = 20{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_M4x3         = 21{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_M3x4         = 22{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_M3x3         = 23{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_M3x2         = 24{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_CALL         = 25{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_CALLNZ       = 26{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_LOOP         = 27{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_RET          = 28{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_ENDLOOP      = 29{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_LABEL        = 30{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_DCL          = 31{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_POW          = 32{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_CRS          = 33{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_SGN          = 34{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_ABS          = 35{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_NRM          = 36{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_SINCOS       = 37{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_REP          = 38{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_ENDREP       = 39{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_IF           = 40{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_IFC          = 41{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_ELSE         = 42{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_ENDIF        = 43{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_BREAK        = 44{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_BREAKC       = 45{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_MOVA         = 46{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_DEFB         = 47{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_DEFI         = 48{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

    D3DSIO_TEXCOORD     = 64{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXKILL      = 65{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEX          = 66{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXBEM       = 67{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXBEML      = 68{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXREG2AR    = 69{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXREG2GB    = 70{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXM3x2PAD   = 71{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXM3x2TEX   = 72{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXM3x3PAD   = 73{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXM3x3TEX   = 74{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_RESERVED0    = 75{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXM3x3SPEC  = 76{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXM3x3VSPEC = 77{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_EXPP         = 78{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_LOGP         = 79{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_CND          = 80{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_DEF          = 81{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXREG2RGB   = 82{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXDP3TEX    = 83{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXM3x2DEPTH = 84{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXDP3       = 85{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXM3x3      = 86{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXDEPTH     = 87{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_CMP          = 88{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_BEM          = 89{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_DP2ADD       = 90{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_DSX          = 91{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_DSY          = 92{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXLDD       = 93{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_SETP         = 94{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_TEXLDL       = 95{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_BREAKP       = 96{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

    D3DSIO_PHASE        = $FFFD{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_COMMENT      = $FFFE{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSIO_END          = $FFFF{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DShader_Instruction_Opcode_Type = TD3DShaderInstructionOpcodeType;

//---------------------------------------------------------------------
// Use these constants with D3DSIO_SINCOS macro as SRC2, SRC3
//
const
  D3DSINCOSCONST1 : array[0..3] of Single = (-1.5500992e-006, -2.1701389e-005,  0.0026041667, 0.00026041668);
  D3DSINCOSCONST2 : array[0..3] of Single = (-0.020833334, -0.12500000, 1.0, 0.50000000);


//---------------------------------------------------------------------
// Co-Issue Instruction Modifier - if set then this instruction is to be
// issued in parallel with the previous instruction(s) for which this bit
// is not set.
//
const
  D3DSI_COISSUE = $40000000;

//---------------------------------------------------------------------
// Opcode specific controls

const
  D3DSP_OPCODESPECIFICCONTROL_MASK  = $00ff0000;
  D3DSP_OPCODESPECIFICCONTROL_SHIFT = 16;

// ps_2_0 texld controls
   D3DSI_TEXLD_PROJECT  = ($01 shl D3DSP_OPCODESPECIFICCONTROL_SHIFT);
   D3DSI_TEXLD_BIAS     = ($02 shl D3DSP_OPCODESPECIFICCONTROL_SHIFT);

{$MINENUMSIZE 1}
type
  TD3DShaderComparison = {$IFNDEF NOENUMS}({$ELSE}Byte;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}                              // < = >
    D3DSPC_RESERVED0 = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // 0 0 0
    D3DSPC_GT        = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // 0 0 1
    D3DSPC_EQ        = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // 0 1 0
    D3DSPC_GE        = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // 0 1 1
    D3DSPC_LT        = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // 1 0 0
    D3DSPC_NE        = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // 1 0 1
    D3DSPC_LE        = 6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // 1 1 0
    D3DSPC_RESERVED1 = 7{$IFNDEF NOENUMS}){$ENDIF};        // 1 1 1
{$MINENUMSIZE 4}
type
 TD3DShader_Comparison = TD3DShaderComparison;

const
// Comparison is part of instruction opcode token:
  D3DSHADER_COMPARISON_SHIFT = D3DSP_OPCODESPECIFICCONTROL_SHIFT;
  D3DSHADER_COMPARISON_MASK  = ($7 shl D3DSHADER_COMPARISON_SHIFT);

//---------------------------------------------------------------------
// Predication flags on instruction token
  D3DSHADER_INSTRUCTION_PREDICATED = ($1 shl 28);

//---------------------------------------------------------------------
// DCL Info Token Controls

const
// For dcl info tokens requiring a semantic (usage + index)
  D3DSP_DCL_USAGE_SHIFT = 0;
  D3DSP_DCL_USAGE_MASK  = $0000000F;

  D3DSP_DCL_USAGEINDEX_SHIFT = 16;
  D3DSP_DCL_USAGEINDEX_MASK  = $000F0000;

const
// DCL pixel shader sampler info token.
  D3DSP_TEXTURETYPE_SHIFT = 27;
  D3DSP_TEXTURETYPE_MASK  = $78000000;

type
  TD3DSamplerTextureType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DSTT_UNKNOWN     = 0 shl D3DSP_TEXTURETYPE_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // uninitialized value
    D3DSTT_2D          = 2 shl D3DSP_TEXTURETYPE_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // dcl_2d s# (for declaring a 2-D texture)
    D3DSTT_CUBE        = 3 shl D3DSP_TEXTURETYPE_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // dcl_cube s# (for declaring a cube texture)
    D3DSTT_VOLUME      = 4 shl D3DSP_TEXTURETYPE_SHIFT{$IFNDEF NOENUMS}){$ENDIF};        // dcl_volume s# (for declaring a volume texture)

type
  TD3DSampler_Texture_Type = TD3DSamplerTextureType;

//
// Parameter Token Bit Definitions
//
const
  D3DSP_REGNUM_MASK       = $000007FF;

// destination parameter write mask
  D3DSP_WRITEMASK_0       = $00010000;  // Component 0 (X;Red)
  D3DSP_WRITEMASK_1       = $00020000;  // Component 1 (Y;Green)
  D3DSP_WRITEMASK_2       = $00040000;  // Component 2 (Z;Blue)
  D3DSP_WRITEMASK_3       = $00080000;  // Component 3 (W;Alpha)
  D3DSP_WRITEMASK_ALL     = $000F0000;  // All Components

// destination parameter modifiers
  D3DSP_DSTMOD_SHIFT      = 20;
  D3DSP_DSTMOD_MASK       = $00F00000;

// Bit masks for destination parameter modifiers
  D3DSPDM_NONE             = (0 shl D3DSP_DSTMOD_SHIFT); // nop
  D3DSPDM_SATURATE         = (1 shl D3DSP_DSTMOD_SHIFT); // clamp to 0. to 1. range
  D3DSPDM_PARTIALPRECISION = (2 shl D3DSP_DSTMOD_SHIFT); // Partial precision hint
  D3DSPDM_MSAMPCENTROID    = (4 shl D3DSP_DSTMOD_SHIFT); // Relevant to multisampling only:
                                                         //      When the pixel center is not covered, sample
                                                         //      attribute or compute gradients/LOD
                                                         //      using multisample "centroid" location.
                                                         //      "Centroid" is some location within the covered
                                                         //      region of the pixel.

// destination parameter
  D3DSP_DSTSHIFT_SHIFT    = 24;
  D3DSP_DSTSHIFT_MASK     = $0F000000;

// destination/source parameter register type
  D3DSP_REGTYPE_SHIFT     = 28;
  D3DSP_REGTYPE_SHIFT2    = 8;
  D3DSP_REGTYPE_MASK      = $70000000;
  D3DSP_REGTYPE_MASK2     = $00001800;

type
  TD3DShaderParamRegisterType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DSPR_TEMP        = 00{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Temporary Register File
    D3DSPR_INPUT       = 01{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Input Register File
    D3DSPR_CONST       = 02{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Constant Register File
    D3DSPR_ADDR        = 03{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Address Register (VS)
    D3DSPR_TEXTURE     = 03{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Texture Register File (PS)
    D3DSPR_RASTOUT     = 04{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Rasterizer Register File
    D3DSPR_ATTROUT     = 05{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Attribute Output Register File
    D3DSPR_TEXCRDOUT   = 06{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Texture Coordinate Output Register File
    D3DSPR_OUTPUT      = 06{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Output register file for VS3.0+
    D3DSPR_CONSTINT    = 07{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Constant Integer Vector Register File
    D3DSPR_COLOROUT    = 08{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Color Output Register File
    D3DSPR_DEPTHOUT    = 09{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Depth Output Register File
    D3DSPR_SAMPLER     = 10{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Sampler State Register File
    D3DSPR_CONST2      = 11{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Constant Register File  2048 - 4095
    D3DSPR_CONST3      = 12{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Constant Register File  4096 - 6143
    D3DSPR_CONST4      = 13{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Constant Register File  6144 - 8191
    D3DSPR_CONSTBOOL   = 14{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Constant Boolean register file
    D3DSPR_LOOP        = 15{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Loop counter register file
    D3DSPR_TEMPFLOAT16 = 16{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // 16-bit float temp register file
    D3DSPR_MISCTYPE    = 17{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Miscellaneous (single) registers.
    D3DSPR_LABEL       = 18{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Label
    D3DSPR_PREDICATE   = 19{$IFNDEF NOENUMS}){$ENDIF};        // Predicate register

type
  TD3DShader_Param_Register_Type = TD3DShaderParamRegisterType;

// The miscellaneous register file (D3DSPR_MISCTYPES)
// contains register types for which there is only ever one
// register (i.e. the register # is not needed).
// Rather than use up additional register types for such
// registers, they are defined
// as particular offsets into the misc. register file:
{$MINENUMSIZE 1}
type
  TD3DShaderMisctypeOffsets = {$IFNDEF NOENUMS}({$ELSE}Byte;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DSMO_POSITION = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Input position x,y,z,rhw (PS)
    D3DSMO_FACE     = 1{$IFNDEF NOENUMS}){$ENDIF};        // Floating point primitive area (PS)
{$MINENUMSIZE 4}
type
  TD3DShader_Misctype_Offsets = TD3DShaderMisctypeOffsets;

// Register offsets in the Rasterizer Register File

type
  TD3DVSRastOutOffsets = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DSRO_POSITION    = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSRO_FOG         = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSRO_POINT_SIZE  = 2{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DVS_RastOut_Offsets = TD3DVSRastOutOffsets;

// Source operand addressing modes
const
  D3DSHADER_ADDRESSMODE_SHIFT = 13;
  D3DSHADER_ADDRESSMODE_MASK  = 1 shl D3DSHADER_ADDRESSMODE_SHIFT;

type
  TD3DSHADERAddressModeType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DSHADER_ADDRMODE_ABSOLUTE    = 0 shl D3DSHADER_ADDRESSMODE_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSHADER_ADDRMODE_RELATIVE    = 1 shl D3DSHADER_ADDRESSMODE_SHIFT{$IFNDEF NOENUMS}){$ENDIF};       // Relative to register A0

type
  TD3DSHADER_AddressMode_Type = TD3DSHADERAddressModeType;

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
    D3DSPSM_X2          =  7 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // *2
    D3DSPSM_X2NEG       =  8 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // *2 and negate
    D3DSPSM_DZ          =  9 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // divide through by z component
    D3DSPSM_DW          = 10 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // divide through by w component
    D3DSPSM_ABS         = 11 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // abs()
    D3DSPSM_ABSNEG      = 12 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // -abs()
    D3DSPSM_NOT         = 13 shl D3DSP_SRCMOD_SHIFT{$IFNDEF NOENUMS}){$ENDIF};        // for predicate register: "!p0"

type
  TD3DShader_Param_SRCMod_Type = TD3DShaderParamSRCModType;

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
type
  TD3DBasisType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DBASIS_BEZIER      = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DBASIS_BSPLINE     = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DBASIS_INTERPOLATE = 2{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DDegreeType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
   D3DDEGREE_LINEAR      = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
   D3DDEGREE_QUADRATIC   = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
   D3DDEGREE_CUBIC       = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
   D3DDEGREE_QUINTIC     = 5{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DPatchEdgeStyle = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
   D3DPATCHEDGE_DISCRETE    = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
   D3DPATCHEDGE_CONTINUOUS  = 1{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DStateBlockType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DSBT_ALL           = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // capture all state
    D3DSBT_PIXELSTATE    = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // capture pixel state
    D3DSBT_VERTEXSTATE   = 3{$IFNDEF NOENUMS}){$ENDIF};        // capture vertex state

type
  TD3DVertexBlendFlags = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DVBF_DISABLE     = 000{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Disable vertex blending
    D3DVBF_1WEIGHTS    = 001{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // 2 matrix blending
    D3DVBF_2WEIGHTS    = 002{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // 3 matrix blending
    D3DVBF_3WEIGHTS    = 003{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // 4 matrix blending
    D3DVBF_TWEENING    = 255{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // blending using D3DRS_TWEENFACTOR
    D3DVBF_0WEIGHTS    = 256{$IFNDEF NOENUMS}){$ENDIF};        // one matrix is used with weight 1.0

type
  TD3DTextureTransformFlags = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DTTFF_DISABLE     = 000{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // texture coordinates are passed directly
    D3DTTFF_COUNT1      = 001{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // rasterizer should expect 1-D texture coords
    D3DTTFF_COUNT2      = 002{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // rasterizer should expect 2-D texture coords
    D3DTTFF_COUNT3      = 003{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // rasterizer should expect 3-D texture coords
    D3DTTFF_COUNT4      = 004{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // rasterizer should expect 4-D texture coords
    D3DTTFF_PROJECTED   = 256{$IFNDEF NOENUMS}){$ENDIF};           // texcoords to be divided by COUNTth element

// Macros to set texture coordinate format bits in the FVF id
const
  D3DFVF_TEXTUREFORMAT2 = 0; // Two floating point values
  D3DFVF_TEXTUREFORMAT1 = 3; // One floating point value
  D3DFVF_TEXTUREFORMAT3 = 1; // Three floating point values
  D3DFVF_TEXTUREFORMAT4 = 2; // Four floating point values

  function D3DFVF_TEXCOORDSIZE3(CoordIndex : LongWord) : LongWord;
  function D3DFVF_TEXCOORDSIZE2(CoordIndex : LongWord) : LongWord;
  function D3DFVF_TEXCOORDSIZE4(CoordIndex : LongWord) : LongWord;
  function D3DFVF_TEXCOORDSIZE1(CoordIndex : LongWord) : LongWord;

//---------------------------------------------------------------------

(* Direct3D9 Device types *)  


type
  TD3DDevType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DDEVTYPE_HAL         = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDEVTYPE_REF         = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DDEVTYPE_SW          = 3{$IFNDEF NOENUMS}){$ENDIF};

(* Multi-Sample buffer types *)
type
  TD3DMultiSampleType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DMULTISAMPLE_NONE        =  0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DMULTISAMPLE_NONMASKABLE =  1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DMULTISAMPLE_2_SAMPLES   =  2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DMULTISAMPLE_3_SAMPLES   =  3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DMULTISAMPLE_4_SAMPLES   =  4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DMULTISAMPLE_5_SAMPLES   =  5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DMULTISAMPLE_6_SAMPLES   =  6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DMULTISAMPLE_7_SAMPLES   =  7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DMULTISAMPLE_8_SAMPLES   =  8{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DMULTISAMPLE_9_SAMPLES   =  9{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DMULTISAMPLE_10_SAMPLES  = 10{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DMULTISAMPLE_11_SAMPLES  = 11{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DMULTISAMPLE_12_SAMPLES  = 12{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DMULTISAMPLE_13_SAMPLES  = 13{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DMULTISAMPLE_14_SAMPLES  = 14{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DMULTISAMPLE_15_SAMPLES  = 15{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DMULTISAMPLE_16_SAMPLES  = 16{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DMultiSample_Type = TD3DMultiSampleType;    
(*)
 * Formats
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
 *      C = Computed from other channels (typically on certain read operations)
 *
 *      Further, the order of the pieces are from MSB first; hence
 *      D3DFMT_A8L8 indicates that the high byte of this two byte
 *      format is alpha.
 *
 *      D16 indicates:
 *           - An integer 16-bit value.
 *           - An app-lockable surface.
 *
 *      All Depth/Stencil formats except D3DFMT_D16_LOCKABLE indicate:
 *          - no particular bit ordering per pixel, and
 *          - are not app lockable, and
 *          - the driver is allowed to consume more than the indicated
 *            number of bits per Depth channel (but not Stencil channel).
(*)

  function MAKEFOURCC(ch0, ch1, ch2, ch3 : Char) : LongWord;

type
  PD3DFormat = ^TD3DFormat;
  TD3DFormat = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DFMT_UNKNOWN              =  0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

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
    D3DFMT_A2B10G10R10          = 31{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_A8B8G8R8             = 32{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_X8B8G8R8             = 33{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_G16R16               = 34{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_A2R10G10B10          = 35{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_A16B16G16R16         = 36{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

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
    D3DFMT_A2W10V10U10          = 67{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

    D3DFMT_UYVY                 = Byte('U') or (Byte('Y') shl 8) or (Byte('V') shl 16) or (Byte('Y') shl 24 ){$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_R8G8_B8G8            = Byte('R') or (Byte('G') shl 8) or (Byte('B') shl 16) or (Byte('G') shl 24 ){$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_YUY2                 = Byte('Y') or (Byte('U') shl 8) or (Byte('Y') shl 16) or (Byte('2') shl 24 ){$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_G8R8_G8B8            = Byte('G') or (Byte('R') shl 8) or (Byte('G') shl 16) or (Byte('B') shl 24 ){$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_DXT1                 = Byte('D') or (Byte('X') shl 8) or (Byte('T') shl 16) or (Byte('1') shl 24 ){$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_DXT2                 = Byte('D') or (Byte('X') shl 8) or (Byte('T') shl 16) or (Byte('2') shl 24 ){$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_DXT3                 = Byte('D') or (Byte('X') shl 8) or (Byte('T') shl 16) or (Byte('3') shl 24 ){$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_DXT4                 = Byte('D') or (Byte('X') shl 8) or (Byte('T') shl 16) or (Byte('4') shl 24 ){$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_DXT5                 = Byte('D') or (Byte('X') shl 8) or (Byte('T') shl 16) or (Byte('5') shl 24 ){$IFNDEF NOENUMS},{$ELSE};{$ENDIF}


    D3DFMT_D16_LOCKABLE         = 70{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_D32                  = 71{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_D15S1                = 73{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_D24S8                = 75{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_D24X8                = 77{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_D24X4S4              = 79{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_D16                  = 80{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

    D3DFMT_D32F_LOCKABLE        = 82{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_D24FS8               = 83{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}


    D3DFMT_L16                  = 81{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

    D3DFMT_VERTEXDATA           =100{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_INDEX16              =101{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_INDEX32              =102{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

    D3DFMT_Q16W16V16U16         =110{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

    D3DFMT_MULTI2_ARGB8         = Byte('M') or (Byte('E') shl 8) or (Byte('T') shl 16) or (Byte('1') shl 24 ){$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

    // Floating point surface formats

    // s10e5 formats (16-bits per channel)
    D3DFMT_R16F                 = 111{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_G16R16F              = 112{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_A16B16G16R16F        = 113{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

    // IEEE s23e8 formats (32-bits per channel)
    D3DFMT_R32F                 = 114{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_G32R32F              = 115{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DFMT_A32B32G32R32F        = 116{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}

    D3DFMT_CxV8U8               = 117{$IFNDEF NOENUMS}){$ENDIF};

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
type
  TD3DSwapEffect = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DSWAPEFFECT_DISCARD           = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSWAPEFFECT_FLIP              = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DSWAPEFFECT_COPY              = 3{$IFNDEF NOENUMS}){$ENDIF};

(* Pool types *)
type
  TD3DPool  = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DPOOL_DEFAULT                 = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DPOOL_MANAGED                 = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DPOOL_SYSTEMMEM               = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DPOOL_SCRATCH                 = 3{$IFNDEF NOENUMS}){$ENDIF};

const    
(* RefreshRate pre-defines *)
  D3DPRESENT_RATE_DEFAULT         = $00000000;

(* Resize Optional Parameters *)
type
  PD3DPresent_Parameters = ^TD3DPresent_Parameters;
  TD3DPresent_Parameters = packed record
    BackBufferWidth                 : Cardinal;
    BackBufferHeight                : Cardinal;
    BackBufferFormat                : TD3DFormat;
    BackBufferCount                 : Cardinal;

    MultiSampleType                 : TD3DMultiSampleType;
    MultiSampleQuality              : LongWord;    

    SwapEffect                      : TD3DSwapEffect;
    hDeviceWindow                   : HWND;
    Windowed                        : Bool;
    EnableAutoDepthStencil          : Bool;
    AutoDepthStencilFormat          : TD3DFormat;
    Flags                           : LongWord;

    (* FullScreen_RefreshRateInHz must be zero for Windowed mode *)
    FullScreen_RefreshRateInHz      : Cardinal;
    PresentationInterval            : Cardinal;
  end;

  PD3DPresentParameters = ^TD3DPresentParameters;
  TD3DPresentParameters = TD3DPresent_Parameters;

// Values for D3DPRESENT_PARAMETERS.Flags
const
  D3DPRESENTFLAG_LOCKABLE_BACKBUFFER  = $00000001;
  D3DPRESENTFLAG_DISCARD_DEPTHSTENCIL = $00000002;
  D3DPRESENTFLAG_DEVICECLIP           = $00000004;
  D3DPRESENTFLAG_VIDEO                = $00000010;

(* Gamma Ramp: Same as DX7 *)
type
  PD3DGammaRamp = ^TD3DGammaRamp;
  TD3DGammaRamp = packed record
    red   : array [0..255] of Word;
    green : array [0..255] of Word;
    blue  : array [0..255] of Word;
  end;

(* Back buffer types *)
type
  TD3DBackBufferType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DBACKBUFFER_TYPE_MONO         = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DBACKBUFFER_TYPE_LEFT         = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DBACKBUFFER_TYPE_RIGHT        = 2{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DBackBuffer_Type = TD3DBackBufferType;


(* Types *)
type
  TD3DResourceType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DRTYPE_SURFACE                = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRTYPE_VOLUME                 = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRTYPE_TEXTURE                = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRTYPE_VOLUMETEXTURE          = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRTYPE_CUBETEXTURE            = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRTYPE_VERTEXBUFFER           = 6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DRTYPE_INDEXBUFFER            = 7{$IFNDEF NOENUMS}){$ENDIF};

const
(* Usages *)
  D3DUSAGE_RENDERTARGET       = $00000001;
  D3DUSAGE_DEPTHSTENCIL       = $00000002;
  D3DUSAGE_DYNAMIC            = $00000200;    

// When passed to CheckDeviceFormat, D3DUSAGE_AUTOGENMIPMAP may return
// D3DOK_NOAUTOGEN if the device doesn't support autogeneration for that format.
// D3DOK_NOAUTOGEN is a success code, not a failure code... the SUCCEEDED and FAILED macros
// will return true and false respectively for this code.
  D3DUSAGE_AUTOGENMIPMAP      = $00000400;
  D3DUSAGE_DMAP               = $00004000;

// The following usages are valid only for querying CheckDeviceFormat
  D3DUSAGE_QUERY_LEGACYBUMPMAP            = $00008000;
  D3DUSAGE_QUERY_SRGBREAD                 = $00010000;
  D3DUSAGE_QUERY_FILTER                   = $00020000;
  D3DUSAGE_QUERY_SRGBWRITE                = $00040000;
  D3DUSAGE_QUERY_POSTPIXELSHADER_BLENDING = $00080000;
  D3DUSAGE_QUERY_VERTEXTEXTURE            = $00100000;

(* Usages for Vertex/Index buffers *)
  D3DUSAGE_WRITEONLY          = $00000008;
  D3DUSAGE_SOFTWAREPROCESSING = $00000010;
  D3DUSAGE_DONOTCLIP          = $00000020;
  D3DUSAGE_POINTS             = $00000040;
  D3DUSAGE_RTPATCHES          = $00000080;
  D3DUSAGE_NPATCHES           = $00000100;


(* CubeMap Face identifiers *)
type
  TD3DCubeMapFaces = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DCUBEMAP_FACE_POSITIVE_X     = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DCUBEMAP_FACE_NEGATIVE_X     = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DCUBEMAP_FACE_POSITIVE_Y     = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DCUBEMAP_FACE_NEGATIVE_Y     = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DCUBEMAP_FACE_POSITIVE_Z     = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DCUBEMAP_FACE_NEGATIVE_Z     = 5{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DCubeMap_Faces = TD3DCubeMapFaces;

const
(* Lock flags *)

  D3DLOCK_READONLY         = $00000010;
  D3DLOCK_DISCARD          = $00002000;
  D3DLOCK_NOOVERWRITE      = $00001000;
  D3DLOCK_NOSYSLOCK        = $00000800;

  D3DLOCK_DONOTWAIT        = $00004000;  

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
    Format             : TD3DFormat;
    _Type              : TD3DResourceType;
    Usage              : LongWord;
    Pool               : TD3DPool;
    MultiSampleType    : TD3DMultiSampleType;
    MultiSampleQuality : LongWord;
    Width              : Cardinal;
    Height             : Cardinal;
  end;

  PD3DSurfaceDesc = ^TD3DSurfaceDesc;
  TD3DSurfaceDesc = TD3DSurface_Desc;

  PD3DVolume_Desc = ^TD3DVolume_Desc;
  TD3DVolume_Desc = packed record
    Format : TD3DFormat;
    _Type  : TD3DResourceType;
    Usage  : LongWord;
    Pool   : TD3DPool;
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
    Bits  : Pointer;
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
    Bits       : Pointer;
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
    Degree                  : TD3DDegreeType;
  end;

  PD3DRectPatchInfo = ^TD3DRectPatchInfo;
  TD3DRectPatchInfo = TD3DRectPatch_Info;

  PD3DTriPatch_Info = ^TD3DTriPatch_Info;
  TD3DTriPatch_Info = packed record
    StartVertexOffset : Cardinal;
    NumVertices       : Cardinal;
    Basis             : TD3DBasisType;
    Degree            : TD3DDegreeType;
  end;

  PD3DTriPatchInfo = ^TD3DTriPatchInfo;
  TD3DTriPatchInfo = TD3DTriPatch_Info;

(* Adapter Identifier *)
const
  MAX_DEVICE_IDENTIFIER_STRING = 512;

type
  PD3DAdapter_Identifier9 = ^TD3DAdapter_Identifier9;
  TD3DAdapter_Identifier9 = packed record
    Driver      : array [0..MAX_DEVICE_IDENTIFIER_STRING - 1] of Char;
    Description : array [0..MAX_DEVICE_IDENTIFIER_STRING - 1] of Char;
    DeviceName  : array [0..31]  of Char; (* Device name for GDI (ex. \\.\DISPLAY1) *)

    DriverVersionLowPart : LongWord;     (* Defined for 16 bit driver components *)
    DriverVersionHighPart : LongWord;

    VendorId : LongWord;
    DeviceId : LongWord;
    SubSysId : LongWord;
    Revision : LongWord;

    DeviceIdentifier : TGUID;

    WHQLLevel : LongWord;

  end;

  PD3DAdapterIdentifier9 = ^TD3DAdapterIdentifier9;
  TD3DAdapterIdentifier9 = TD3DAdapter_Identifier9;

(* Raster Status structure returned by GetRasterStatus *)
  PD3DRaster_Status = ^TD3DRaster_Status;
  TD3DRaster_Status = packed record
    InVBlank : Bool;
    ScanLine : Cardinal;
  end;

  PD3DRasterStatus = ^TD3DRasterStatus;
  TD3DRasterStatus = TD3DRaster_Status;

(*)
 * Debug monitor tokens (DEBUG only)
 *
 * Note that if D3DRS_DEBUGMONITORTOKEN is set, the call is treated as
 * passing a token to the debug monitor.  For example, if, after passing
 * D3DDMT_ENABLE/DISABLE to D3DRS_DEBUGMONITORTOKEN other token values
 * are passed in, the enabled/disabled state of the debug
 * monitor will still persist.
 *
 * The debug monitor defaults to enabled.
 *
 * Calling GetRenderState on D3DRS_DEBUGMONITORTOKEN is not of any use.
(*)

type
  TD3DDebugMonitorTokens = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DDMT_ENABLE          = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}    // enable debug monitor
    D3DDMT_DISABLE         = 1{$IFNDEF NOENUMS}){$ENDIF};          // disable debug monitor

// Async feedback

{$MINENUMSIZE 1}
type
  TD3DQueryType = {$IFNDEF NOENUMS}({$ELSE}Byte;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DQUERYTYPE_VCACHE          = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} (* D3DISSUE_END *)
    D3DQUERYTYPE_RESOURCEMANAGER = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} (* D3DISSUE_END *)
    D3DQUERYTYPE_VERTEXSTATS     = 6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} (* D3DISSUE_END *)
    D3DQUERYTYPE_EVENT           = 8{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} (* D3DISSUE_END *)
    D3DQUERYTYPE_OCCLUSION       = 9{$IFNDEF NOENUMS}){$ENDIF};        (* D3DISSUE_BEGIN, D3DISSUE_END *)

{$MINENUMSIZE 4}
// Flags field for Issue
const
  D3DISSUE_END   = (1 shl 0); // Tells the runtime to issue the end of a query, changing it's state to "non-signaled".
  D3DISSUE_BEGIN = (1 shl 1); // Tells the runtime to issue the beginng of a query.

// Flags field for GetData
  D3DGETDATA_FLUSH = (1 shl 0); // Tells the runtime to flush if the query is outstanding.

type
  PD3DResourceStats = ^TD3DResourceStats;
  TD3DResourceStats = packed record
// Data collected since last Present()
    bThrashing            : Bool;  (* indicates if thrashing *)
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

const
  D3DRTYPECOUNT = LongWord(D3DRTYPE_INDEXBUFFER) + 1;

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

type
  PD3DDevInfoVCache = ^TD3DDevInfoVCache;
  TD3DDevInfoVCache = packed record
    Pattern     : LongWord; (* bit pattern, return value must be FOUR_CC(‘C’, ‘A’, ‘C’, ‘H’) *)
    OptMethod   : LongWord; (* optimization method 0 means longest strips, 1 means vertex cache based *)
    CacheSize   : LongWord; (* cache size to optimize for  (only required if type is 1) *)
    MagicNumber : LongWord; (* used to determine when to restart strips (only required if type is 1)*)
  end;
  
  PD3DDevInfo_VCache = ^TD3DDevInfo_VCache;
  TD3DDevInfo_VCache = TD3DDevInfoVCache;

(*)
 *******************************************************************************
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File    : d3d9caps.h
 *  Content : Direct3D capabilities include file
 *
 *******************************************************************************
(*)

type
  TD3DVShaderCaps20 = packed record
    Caps                    : LongWord;
    DynamicFlowControlDepth : Integer;
    NumTemps                : Integer;
    StaticFlowControlDepth  : Integer;
  end;

  TD3DVShaderCaps2_0 = TD3DVShaderCaps20;

const
  D3DVS20CAPS_PREDICATI1ON             = (1 shl 0);

  D3DVS20_MAX_DYNAMICFLOWCONTROLDEPTH  = 24;
  D3DVS20_MIN_DYNAMICFLOWCONTROLDEPTH  = 0;
  D3DVS20_MAX_NUMTEMPS                 = 32;
  D3DVS20_MIN_NUMTEMPS                 = 12;
  D3DVS20_MAX_STATICFLOWCONTROLDEPTH   = 4;
  D3DVS20_MIN_STATICFLOWCONTROLDEPTH   = 1;

type
  TD3DPShaderCaps20 = packed record
    Caps                    : LongWord;
    DynamicFlowControlDepth : Integer;
    NumTemps                : Integer;
    StaticFlowControlDepth  : Integer;
    NumInstructionSlots     : Integer;
  end;

  TD3DPShaderCaps2_0 = TD3DPShaderCaps20;

const
  D3DPS20CAPS_ARBITRARYSWIZZLE        = (1 shl 0);
  D3DPS20CAPS_GRADIENTINSTRUCTIONS    = (1 shl 1);
  D3DPS20CAPS_PREDICATION             = (1 shl 2);
  D3DPS20CAPS_NODEPENDENTREADLIMIT    = (1 shl 3);
  D3DPS20CAPS_NOTEXINSTRUCTIONLIMIT   = (1 shl 4);

  D3DPS20_MAX_DYNAMICFLOWCONTROLDEPTH  = 24;
  D3DPS20_MIN_DYNAMICFLOWCONTROLDEPTH  = 0;
  D3DPS20_MAX_NUMTEMPS                 = 32;
  D3DPS20_MIN_NUMTEMPS                 = 12;
  D3DPS20_MAX_STATICFLOWCONTROLDEPTH   = 4;
  D3DPS20_MIN_STATICFLOWCONTROLDEPTH   = 0;
  D3DPS20_MAX_NUMINSTRUCTIONSLOTS      = 512;
  D3DPS20_MIN_NUMINSTRUCTIONSLOTS      = 96;

  D3DMIN30SHADERINSTRUCTIONS = 512;
  D3DMAX30SHADERINSTRUCTIONS = 32768;

type
  PD3DCaps9 = ^TD3DCaps9;
  TD3DCaps9 = packed record
    (* Device Info *)
    DeviceType                : TD3DDevType;
    AdapterOrdinal            : Cardinal;

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
    TextureFilterCaps         : LongWord;  // D3DPTFILTERCAPS for IDirect3DTexture9's
    CubeTextureFilterCaps     : LongWord;  // D3DPTFILTERCAPS for IDirect3DCubeTexture9's
    VolumeTextureFilterCaps   : LongWord;  // D3DPTFILTERCAPS for IDirect3DVolumeTexture9's
    TextureAddressCaps        : LongWord;  // D3DPTADDRESSCAPS for IDirect3DTexture9's
    VolumeTextureAddressCaps  : LongWord;  // D3DPTADDRESSCAPS for IDirect3DVolumeTexture9's

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

    // Here are the DX9 specific ones 
    DevCaps2                          : LongWord;

    MaxNpatchTessellationLevel        : Single;
    Reserved5                         : LongWord;

    MasterAdapterOrdinal              : Cardinal;  // ordinal of master adaptor for adapter group
    AdapterOrdinalInGroup             : Cardinal;  // ordinal inside the adapter group
    NumberOfAdaptersInGroup           : Cardinal;  // number of adapters in this adapter group (only if master)
    DeclTypes                         : LongWord;  // Data types, supported in vertex declarations
    NumSimultaneousRTs                : LongWord;  // Will be at least 1
    StretchRectFilterCaps             : LongWord;  // Filter caps supported by StretchRect
    VS20Caps                          : TD3DVShaderCaps20;
    PS20Caps                          : TD3DPShaderCaps20;
    VertexTextureFilterCaps           : LongWord;  // D3DPTFILTERCAPS for IDirect3DTexture9's for texture, used in vertex shaders
    MaxVShaderInstructionsExecuted    : LongWord;  // maximum number of vertex shader instructions that can be executed
    MaxPShaderInstructionsExecuted    : LongWord;  // maximum number of pixel shader instructions that can be executed
    MaxVertexShader30InstructionSlots : LongWord;
    MaxPixelShader30InstructionSlots  : LongWord;

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
  D3DCAPS2_FULLSCREENGAMMA        = $00020000;
  D3DCAPS2_CANCALIBRATEGAMMA      = $00100000;
  D3DCAPS2_RESERVED               = $02000000;
  D3DCAPS2_CANMANAGERESOURCE      = $10000000;
  D3DCAPS2_DYNAMICTEXTURES        = $20000000;
  D3DCAPS2_CANAUTOGENMIPMAP       = $40000000;

//
// Caps3
//
  D3DCAPS3_RESERVED                         = $8000001f;

// Indicates that the device can respect the ALPHABLENDENABLE render state
// when fullscreen while using the FLIP or DISCARD swap effect.
// COPY and COPYVSYNC swap effects work whether or not this flag is set.
  D3DCAPS3_ALPHA_FULLSCREEN_FLIP_OR_DISCARD = $00000020;

// Indicates that the device can perform a gamma correction from
// a windowed back buffer containing linear content to the sRGB desktop.
  D3DCAPS3_LINEAR_TO_SRGB_PRESENTATION      = $00000080;

  D3DCAPS3_COPY_TO_VIDMEM                   = $00000100; (* Device can acclerate copies from sysmem to local vidmem *)
  D3DCAPS3_COPY_TO_SYSTEMMEM                = $00000200; (* Device can acclerate copies from local vidmem to sysmem *)


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
  D3DDEVCAPS_EXECUTESYSTEMMEMORY          = $00000010; (* Device can use execute buffers from system memory *)
  D3DDEVCAPS_EXECUTEVIDEOMEMORY           = $00000020; (* Device can use execute buffers from video memory *)
  D3DDEVCAPS_TLVERTEXSYSTEMMEMORY         = $00000040; (* Device can use TL buffers from system memory *)
  D3DDEVCAPS_TLVERTEXVIDEOMEMORY          = $00000080; (* Device can use TL buffers from video memory *)
  D3DDEVCAPS_TEXTURESYSTEMMEMORY          = $00000100; (* Device can texture from system memory *)
  D3DDEVCAPS_TEXTUREVIDEOMEMORY           = $00000200; (* Device can texture from device memory *)
  D3DDEVCAPS_DRAWPRIMTLVERTEX             = $00000400; (* Device can draw TLVERTEX primitives *)
  D3DDEVCAPS_CANRENDERAFTERFLIP           = $00000800; (* Device can render without waiting for flip to complete *)
  D3DDEVCAPS_TEXTURENONLOCALVIDMEM        = $00001000; (* Device can texture from nonlocal video memory *)
  D3DDEVCAPS_DRAWPRIMITIVES2              = $00002000; (* Device can support DrawPrimitives2 *)
  D3DDEVCAPS_SEPARATETEXTUREMEMORIES      = $00004000; (* Device is texturing from separate memory pools *)
  D3DDEVCAPS_DRAWPRIMITIVES2EX            = $00008000; (* Device can support Extended DrawPrimitives2 i.e. DX7 compliant driver*)
  D3DDEVCAPS_HWTRANSFORMANDLIGHT          = $00010000; (* Device can support transformation and lighting in hardware and DRAWPRIMITIVES2EX must be also *)
  D3DDEVCAPS_CANBLTSYSTONONLOCAL          = $00020000; (* Device supports a Tex Blt from system memory to non-local vidmem *)
  D3DDEVCAPS_HWRASTERIZATION              = $00080000; (* Device has HW acceleration for rasterization *)
  D3DDEVCAPS_PUREDEVICE                   = $00100000; (* Device supports D3DCREATE_PUREDEVICE *)
  D3DDEVCAPS_QUINTICRTPATCHES             = $00200000; (* Device supports quintic Beziers and BSplines *)
  D3DDEVCAPS_RTPATCHES                    = $00400000; (* Device supports Rect and Tri patches *)
  D3DDEVCAPS_RTPATCHHANDLEZERO            = $00800000; (* Indicates that RT Patches may be drawn efficiently using handle 0 *)
  D3DDEVCAPS_NPATCHES                     = $01000000; (* Device supports N-Patches *)

//
// PrimitiveMiscCaps
//
  D3DPMISCCAPS_MASKZ                      = $00000002;
  D3DPMISCCAPS_CULLNONE                   = $00000010;
  D3DPMISCCAPS_CULLCW                     = $00000020;
  D3DPMISCCAPS_CULLCCW                    = $00000040;
  D3DPMISCCAPS_COLORWRITEENABLE           = $00000080;
  D3DPMISCCAPS_CLIPPLANESCALEDPOINTS      = $00000100; (* Device correctly clips scaled points to clip planes *)
  D3DPMISCCAPS_CLIPTLVERTS                = $00000200; (* device will clip post-transformed vertex primitives *)
  D3DPMISCCAPS_TSSARGTEMP                 = $00000400; (* device supports D3DTA_TEMP for temporary register *)
  D3DPMISCCAPS_BLENDOP                    = $00000800; (* device supports D3DRS_BLENDOP *)
  D3DPMISCCAPS_NULLREFERENCE              = $00001000; (* Reference Device that doesnt render *)
  D3DPMISCCAPS_INDEPENDENTWRITEMASKS      = $00004000; (* Device supports independent write masks for MET or MRT *)
  D3DPMISCCAPS_PERSTAGECONSTANT           = $00008000; (* Device supports per-stage constants *)
  D3DPMISCCAPS_FOGANDSPECULARALPHA        = $00010000; (* Device supports separate fog and specular alpha (many devices
                                                          use the specular alpha channel to store fog factor) *)
  D3DPMISCCAPS_SEPARATEALPHABLEND         = $00020000; (* Device supports separate blend settings for the alpha channel *)
  D3DPMISCCAPS_MRTINDEPENDENTBITDEPTHS    = $00040000; (* Device supports different bit depths for MRT *)
  D3DPMISCCAPS_MRTPOSTPIXELSHADERBLENDING = $00080000; (* Device supports post-pixel shader operations for MRT *)
  D3DPMISCCAPS_FOGVERTEXCLAMPED           = $00100000; (* Device clamps fog blend factor per vertex *)

//
// LineCaps
//
  D3DLINECAPS_TEXTURE             = $00000001;
  D3DLINECAPS_ZTEST               = $00000002;
  D3DLINECAPS_BLEND               = $00000004;
  D3DLINECAPS_ALPHACMP            = $00000008;
  D3DLINECAPS_FOG                 = $00000010;
  D3DLINECAPS_ANTIALIAS           = $00000020;
//

//
// RasterCaps
//
  D3DPRASTERCAPS_DITHER                 = $00000001;
  D3DPRASTERCAPS_ZTEST                  = $00000010;
  D3DPRASTERCAPS_FOGVERTEX              = $00000080;
  D3DPRASTERCAPS_FOGTABLE               = $00000100;
  D3DPRASTERCAPS_MIPMAPLODBIAS          = $00002000;
  D3DPRASTERCAPS_ZBUFFERLESSHSR         = $00008000;
  D3DPRASTERCAPS_FOGRANGE               = $00010000;
  D3DPRASTERCAPS_ANISOTROPY             = $00020000;
  D3DPRASTERCAPS_WBUFFER                = $00040000;
  D3DPRASTERCAPS_WFOG                   = $00100000;
  D3DPRASTERCAPS_ZFOG                   = $00200000;
  D3DPRASTERCAPS_COLORPERSPECTIVE       = $00400000; (* Device iterates colors perspective correct *)
  D3DPRASTERCAPS_SCISSORTEST            = $01000000;
  D3DPRASTERCAPS_SLOPESCALEDEPTHBIAS    = $02000000;
  D3DPRASTERCAPS_DEPTHBIAS              = $04000000;
  D3DPRASTERCAPS_MULTISAMPLE_TOGGLE     = $08000000;

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
  D3DPBLENDCAPS_BLENDFACTOR       = $00002000; (* Supports both D3DBLEND_BLENDFACTOR and D3DBLEND_INVBLENDFACTOR *)  

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
  D3DPTEXTURECAPS_NOPROJECTEDBUMPENV       = $00200000; (* Device does not support projected bump env lookup operation
                                                           in programmable and fixed function pixel shaders *)

//
// TextureFilterCaps, StretchRectFilterCaps
//
  D3DPTFILTERCAPS_MINFPOINT           = $00000100; (* Min Filter *)
  D3DPTFILTERCAPS_MINFLINEAR          = $00000200;
  D3DPTFILTERCAPS_MINFANISOTROPIC     = $00000400;
  D3DPTFILTERCAPS_MINFPYRAMIDALQUAD   = $00000800;
  D3DPTFILTERCAPS_MINFGAUSSIANQUAD    = $00001000;
  D3DPTFILTERCAPS_MIPFPOINT           = $00010000; (* Mip Filter *)
  D3DPTFILTERCAPS_MIPFLINEAR          = $00020000;
  D3DPTFILTERCAPS_MAGFPOINT           = $01000000; (* Mag Filter *)
  D3DPTFILTERCAPS_MAGFLINEAR          = $02000000;
  D3DPTFILTERCAPS_MAGFANISOTROPIC     = $04000000;
  D3DPTFILTERCAPS_MAGFPYRAMIDALQUAD   = $08000000;
  D3DPTFILTERCAPS_MAGFGAUSSIANQUAD    = $10000000;

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
  D3DSTENCILCAPS_TWOSIDED         = $00000100;

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
  D3DVTXPCAPS_TEXGEN                     = $00000001; (* device can do texgen *)
  D3DVTXPCAPS_MATERIALSOURCE7            = $00000002; (* device can do DX7-level colormaterialsource ops *)
  D3DVTXPCAPS_DIRECTIONALLIGHTS          = $00000008; (* device can do directional lights *)
  D3DVTXPCAPS_POSITIONALLIGHTS           = $00000010; (* device can do positional lights (includes point and spot) *)
  D3DVTXPCAPS_LOCALVIEWER                = $00000020; (* device can do local viewer *)
  D3DVTXPCAPS_TWEENING                   = $00000040; (* device can do vertex tweening *)
  D3DVTXPCAPS_TEXGEN_SPHEREMAP           = $00000100; (* device supports D3DTSS_TCI_SPHEREMAP *)
  D3DVTXPCAPS_NO_TEXGEN_NONLOCALVIEWER   = $00000200; (* device does not support TexGen in non-local
                                                            viewer mode *)

//
// DevCaps2
//
  D3DDEVCAPS2_STREAMOFFSET                        = $00000001; (* Device supports offsets in streams. Must be set by DX9 drivers *)
  D3DDEVCAPS2_DMAPNPATCH                          = $00000002; (* Device supports displacement maps for N-Patches*)
  D3DDEVCAPS2_ADAPTIVETESSRTPATCH                 = $00000004; (* Device supports adaptive tesselation of RT-patches*)
  D3DDEVCAPS2_ADAPTIVETESSNPATCH                  = $00000008; (* Device supports adaptive tesselation of N-patches*)
  D3DDEVCAPS2_CAN_STRETCHRECT_FROM_TEXTURES       = $00000010; (* Device supports StretchRect calls with a texture as the source*)
  D3DDEVCAPS2_PRESAMPLEDDMAPNPATCH                = $00000020; (* Device supports presampled displacement maps for N-Patches *)
  D3DDEVCAPS2_VERTEXELEMENTSCANSHARESTREAMOFFSET  = $00000040; (* Vertex elements in a vertex declaration can share the same stream offset *)

//
// DeclTypes
//
  D3DDTCAPS_UBYTE4     = $00000001;
  D3DDTCAPS_UBYTE4N    = $00000002;
  D3DDTCAPS_SHORT2N    = $00000004;
  D3DDTCAPS_SHORT4N    = $00000008;
  D3DDTCAPS_USHORT2N   = $00000010;
  D3DDTCAPS_USHORT4N   = $00000020;
  D3DDTCAPS_UDEC3      = $00000040;
  D3DDTCAPS_DEC3N      = $00000080;
  D3DDTCAPS_FLOAT16_2  = $00000100;
  D3DDTCAPS_FLOAT16_4  = $00000200;



(*)
 *******************************************************************************
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File    : d3d9.h
 *  Content : Direct3D include file
 *
 *******************************************************************************
(*)

// Start AmpazeInventions (tm)
type
  P4SingleArray  = ^T4SingleArray;
  T4SingleArray = array[0..3] of Single;

  PD3DVertexElement9Array = ^TD3DVertexElement9Array;
  TD3DVertexElement9Array = array[0..0] of TD3DVertexElement9;

// End AmpazeInventions (tm)

(*)
 * This identifier is passed to Direct3DCreate9 in order to ensure that an
 * application was built against the correct header files. This number is
 * incremented whenever a header (or other) change would require applications
 * to be rebuilt. If the version doesn't match, Direct3DCreate9 will fail.
 * (The number itself has no meaning.)
(*)

const
  D3D_SDK_VERSION = 31;

type
  HMonitor = THandle;

const
  IID_IDirect3D9                  : TGUID = '{81BDCBCA-64D4-426D-AE8D-AD0147F4275C}';
  IID_IDirect3DDevice9            : TGUID = '{D0223B96-BF7A-43FD-92BD-A43B0D82B9EB}';
  IID_IDirect3DResource9          : TGUID = '{05EEC05D-8F7D-4362-B999-D1BAF357C704}';
  IID_IDirect3DBaseTexture9       : TGUID = '{580CA87E-1D3C-4D54-991D-B7D3E3C298CE}';
  IID_IDirect3DTexture9           : TGUID = '{85C31227-3DE5-4F00-9B3A-F11AC38C18B5}';
  IID_IDirect3DCubeTexture9       : TGUID = '{FFF32F81-D953-473A-9223-93D652ABA93F}';
  IID_IDirect3DVolumeTexture9     : TGUID = '{2518526C-E789-4111-A7B9-47EF328D13E6}';
  IID_IDirect3DVertexBuffer9      : TGUID = '{B64BB1B5-FD70-4DF6-BF91-19D0A12455E3}';
  IID_IDirect3DIndexBuffer9       : TGUID = '{7C9DD65E-D3F7-4529-ACEE-785830ACDE35}';
  IID_IDirect3DSurface9           : TGUID = '{0CFBAF3A-9FF6-429A-99B3-A2796AF8B89B}';
  IID_IDirect3DVolume9            : TGUID = '{24F416E6-1F67-4AA7-B88E-D33F6F3128A1}';
  IID_IDirect3DSwapChain9         : TGUID = '{794950F2-ADFC-458A-905E-10A10B0B503B}';
  IID_IDirect3DVertexDeclaration9 : TGUID = '{DD13C59C-36FA-4098-A8FB-C7ED39DC8546}';
  IID_IDirect3DVertexShader9      : TGUID = '{EFC5557E-6265-4613-8A94-43857889EB36}';
  IID_IDirect3DPixelShader9       : TGUID = '{6D3BDBDC-5B02-4415-B852-CE5E8BCCB289}';
  IID_IDirect3DStateBlock9        : TGUID = '{B07C4FE5-310D-4BA8-A23C-4F0F206F218B}';
  IID_IDirect3DQuery9             : TGUID = '{D9771460-A695-4F26-BBD3-27B840B541CC}';

type
  IDirect3D9                  = interface;
  IDirect3DDevice9            = interface;
  IDirect3DStateBlock9        = interface;
  IDirect3DVertexDeclaration9 = interface;
  IDirect3DVertexShader9      = interface;
  IDirect3DPixelShader9       = interface;
  IDirect3DResource9          = interface;
  IDirect3DBaseTexture9       = interface;
  IDirect3DTexture9           = interface;
  IDirect3DVolumeTexture9     = interface;
  IDirect3DCubeTexture9       = interface;
  IDirect3DVertexBuffer9      = interface;
  IDirect3DIndexBuffer9       = interface;
  IDirect3DSurface9           = interface;
  IDirect3DVolume9            = interface;
  IDirect3DSwapChain9         = interface;
  IDirect3DQuery9             = interface;


(*)
 * Direct3D interfaces
(*)

  IDirect3D9 = interface (IUnknown)
    ['{81BDCBCA-64D4-426D-AE8D-AD0147F4275C}']
    (*** IDirect3D9 methods ***)
    function RegisterSoftwareDevice(InitializeFunction : Pointer) : HResult; stdcall;
    function GetAdapterCount : Cardinal; stdcall;
    function GetAdapterIdentifier(const Adapter : Cardinal; const Flags : LongWord; out Identifier : TD3DAdapterIdentifier9) : HResult; stdcall;
    function GetAdapterModeCount(Adapter : Cardinal; Format : TD3DFormat) : Cardinal; stdcall;
    function EnumAdapterModes(const Adapter : Cardinal; Format : TD3DFormat; Mode : Cardinal; ArrayOfModes : PD3DDisplayMode) : HResult; stdcall;
    function GetAdapterDisplayMode(const Adapter : Cardinal; out Mode : TD3DDisplayMode) : HResult; stdcall;
    function CheckDeviceType(const Adapter : Cardinal; const CheckType : TD3DDevType; const DisplayFormat, BackBufferFormat : TD3DFormat; const Windowed : Bool) : HResult; stdcall;
    function CheckDeviceFormat(const Adapter : Cardinal; const DeviceType : TD3DDevType; const AdapterFormat : TD3DFormat; const Usage : LongWord; const RType : TD3DResourceType; const CheckFormat : TD3DFormat) : HResult; stdcall;
    function CheckDeviceMultiSampleType(const Adapter : Cardinal; const DeviceType : TD3DDevType; const SurfaceFormat : TD3DFormat; const Windowed : Bool; const MultiSampleType : TD3DMultiSampleType; QualityLevels : PLongWord) : HResult; stdcall;
    function CheckDepthStencilMatch(const Adapter : Cardinal; const DeviceType : TD3DDevType; const AdapterFormat, RenderTargetFormat, DepthStencilFormat : TD3DFormat) : HResult; stdcall;
    function CheckDeviceFormatConversion(const Adapter : Cardinal; const DeviceType : TD3DDevType; const SourceFormat, TargetFormat : TD3DFormat) : HResult; stdcall;
    function GetDeviceCaps(const Adapter : Cardinal; const DeviceType : TD3DDevType; out Caps : TD3DCaps9) : HResult; stdcall;
    function GetAdapterMonitor(const Adapter : Cardinal) : HMONITOR; stdcall;
    function CreateDevice(const Adapter : Cardinal; const DeviceType : TD3DDevType; FocusWindow : HWND; BehaviorFlags : LongWord; var PresentationParameters : TD3DPresentParameters; out ReturnedDeviceInterface : IDirect3DDevice9) : HResult; stdcall;
  end;

  IDirect3DDevice9 = interface (IUnknown)
    ['{D0223B96-BF7A-43FD-92BD-A43B0D82B9EB}']
    (*** IDirect3DDevice9 methods ***)
    function TestCooperativeLevel : HResult; stdcall;
    function GetAvailableTextureMem : Cardinal; stdcall;
    function EvictManagedResources : HResult; stdcall;
    function GetDirect3D(out D3D9 : IDirect3D9) : HResult; stdcall;
    function GetDeviceCaps(out Caps : TD3DCaps9) : HResult; stdcall;
    function GetDisplayMode(const SwapChain : Cardinal; out Mode : TD3DDisplayMode) : HResult; stdcall;
    function GetCreationParameters(out Parameters : TD3DDeviceCreationParameters) : HResult; stdcall;
    function SetCursorProperties(const XHotSpot, YHotSpot : Cardinal; CursorBitmap : IDirect3DSurface9) : HResult; stdcall;
    procedure SetCursorPosition(const X, Y : Integer; const Flags : LongWord); stdcall;
    function ShowCursor(const Show : Bool) : Bool; stdcall;
    function CreateAdditionalSwapChain(var PresentationParameters : TD3DPresentParameters; out SwapChain : IDirect3DSwapChain9) : HResult; stdcall;
    function GetSwapChain(const iSwapChain : Cardinal; out SwapChain : IDirect3DSwapChain9) : HResult; stdcall;
    function GetNumberOfSwapChains : Cardinal; stdcall;
    function Reset(var PresentationParameters : TD3DPresentParameters) : HResult; stdcall;
    function Present(const SourceRect, DestRect : PRect; const DestWindowOverride : HWND; DirtyRegion : PRgnData) : HResult; stdcall;
    function GetBackBuffer(const SwapChain, iBackBuffer : Cardinal; const _Type : TD3DBackBufferType; out BackBuffer : IDirect3DSurface9) : HResult; stdcall;
    function GetRasterStatus(const iSwapChain : Cardinal; out RasterStatus : TD3DRasterStatus) : HResult; stdcall;
    function SetDialogBoxMode(EnableDialogs : Bool) : HResult; stdcall;
    procedure SetGammaRamp(const iSwapChain : Cardinal; const Flags : LongWord; const Ramp : TD3DGammaRamp); stdcall;
    procedure GetGammaRamp(const iSwapChain : Cardinal; out Ramp : TD3DGammaRamp); stdcall;
    function CreateTexture(const Width, Height, Levels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; out Texture : IDirect3DTexture9; SharedHandle : PHandle) : HResult; stdcall;
    function CreateVolumeTexture(const Width, Height, Depth, Levels : Cardinal; Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; out VolumeTexture : IDirect3DVolumeTexture9; SharedHandle : PHandle) : HResult; stdcall;
    function CreateCubeTexture(const EdgeLength, Levels : Cardinal; Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; out CubeTexture : IDirect3DCubeTexture9; SharedHandle : PHandle) : HResult; stdcall;
    function CreateVertexBuffer(const Length : Cardinal; const Usage, FVF : LongWord; Pool : TD3DPool; out VertexBuffer : IDirect3DVertexBuffer9; SharedHandle : PHandle) : HResult; stdcall;
    function CreateIndexBuffer(const Length : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; out IndexBuffer : IDirect3DIndexBuffer9; SharedHandle : PHandle) : HResult; stdcall;
    function CreateRenderTarget(const Width, Height : Cardinal; Format : TD3DFormat; MultiSample : TD3DMultiSampleType; const MultisampleQuality : LongWord; Lockable : Bool; out Surface : IDirect3DSurface9; SharedHandle : PHandle) : HResult; stdcall;
    function CreateDepthStencilSurface(const Width, Height : Cardinal; Format : TD3DFormat; MultiSample : TD3DMultiSampleType; const MultisampleQuality : LongWord; Discard : Bool; out Surface : IDirect3DSurface9; SharedHandle : PHandle) : HResult; stdcall;
    function UpdateSurface(SourceSurface : IDirect3DSurface9; SourceRect : PRect; DestinationSurface : IDirect3DSurface9; DestPoint : PPoint) : HResult; stdcall;
    function UpdateTexture(SourceTexture : IDirect3DBaseTexture9; DestinationTexture : IDirect3DBaseTexture9) : HResult; stdcall;
    function GetRenderTargetData(RenderTarget : IDirect3DSurface9; DestSurface : IDirect3DSurface9) : HResult; stdcall;
    function GetFrontBufferData(SwapChain : Cardinal; DestSurface : IDirect3DSurface9) : HResult; stdcall;
    function StretchRect(SourceSurface : IDirect3DSurface9; SourceRect : PRect; DestSurface : IDirect3DSurface9; DestRect : PRect; Filter : TD3DTextureFilterType) : HResult; stdcall;
    function ColorFill(Surface : IDirect3DSurface9; Rect : PRect; Color : TD3DColor) : HResult; stdcall;
    function CreateOffscreenPlainSurface(const Width, Height : Cardinal; Format : TD3DFormat; Pool : TD3DPool; out Surface : IDirect3DSurface9; SharedHandle : PHandle) : HResult; stdcall;
    function SetRenderTarget(const RenderTargetIndex : LongWord; RenderTarget : IDirect3DSurface9) : HResult; stdcall;
    function GetRenderTarget(const RenderTargetIndex : LongWord; out RenderTarget : IDirect3DSurface9) : HResult; stdcall;
    function SetDepthStencilSurface(NewZStencil : IDirect3DSurface9) : HResult; stdcall;
    function GetDepthStencilSurface(out ZStencilSurface : IDirect3DSurface9) : HResult; stdcall;
    function BeginScene : HResult; stdcall;
    function EndScene : HResult; stdcall;
    function Clear(const Count : LongWord; Rects : PD3DRect; const Flags : LongWord; Color : TD3DColor; const Z : Single; const Stencil : LongWord) : HResult; stdcall;
    function SetTransform(State : TD3DTransformStateType; const Matrix : TD3DMatrix) : HResult; stdcall;
    function GetTransform(State : TD3DTransformStateType; out Matrix : TD3DMatrix) : HResult; stdcall;
    function MultiplyTransform(State : TD3DTransformStateType; const Matrix : TD3DMatrix) : HResult; stdcall;
    function SetViewport(const Viewport : TD3DViewport9) : HResult; stdcall;
    function GetViewport(out Viewport : TD3DViewport9) : HResult; stdcall;
    function SetMaterial(const Material : TD3DMaterial9) : HResult; stdcall;
    function GetMaterial(out Material : TD3DMaterial9) : HResult; stdcall;
    function SetLight(const Index : LongWord; const Light : TD3DLight9) : HResult; stdcall;
    function GetLight(const Index : LongWord; out Light : TD3DLight9) : HResult; stdcall;
    function LightEnable(const Index : LongWord; Enable : Bool) : HResult; stdcall;
    function GetLightEnable(const Index : LongWord; out Enable : Bool) : HResult; stdcall;
    function SetClipPlane(const Index : LongWord; Plane : P4SingleArray) : HResult; stdcall;
    function GetClipPlane(const Index : LongWord; Plane : P4SingleArray) : HResult; stdcall;
    function SetRenderState(State : TD3DRenderStateType; const Value : LongWord) : HResult; stdcall;
    function GetRenderState(State : TD3DRenderStateType; out Value : LongWord) : HResult; stdcall;
    function CreateStateBlock(_Type : TD3DStateBlockType; out SB : IDirect3DStateBlock9) : HResult; stdcall;
    function BeginStateBlock : HResult; stdcall;
    function EndStateBlock(var SB : IDirect3DStateBlock9) : HResult; stdcall;
    function SetClipStatus(const ClipStatus : TD3DClipStatus9) : HResult; stdcall;
    function GetClipStatus(out ClipStatus : TD3DClipStatus9) : HResult; stdcall;
    function GetTexture(const Stage : LongWord; out Texture : IDirect3DBaseTexture9) : HResult; stdcall;
    function SetTexture(const Stage : LongWord; Texture : IDirect3DBaseTexture9) : HResult; stdcall;
    function GetTextureStageState(const Stage : LongWord; _Type : TD3DTextureStageStateType; out Value : LongWord) : HResult; stdcall;
    function SetTextureStageState(const Stage : LongWord; _Type : TD3DTextureStageStateType; const Value : LongWord) : HResult; stdcall;
    function GetSamplerState(const Sampler : LongWord; _Type : TD3DSamplerStateType; out Value : LongWord) : HResult; stdcall;
    function SetSamplerState(const Sampler : LongWord; _Type : TD3DSamplerStateType; const Value : LongWord) : HResult; stdcall;
    function ValidateDevice(out NumPasses : LongWord) : HResult; stdcall;
    function SetPaletteEntries(const PaletteNumber : Cardinal; Entries : PPaletteEntry) : HResult; stdcall;
    function GetPaletteEntries(const PaletteNumber : Cardinal; Entries : PPaletteEntry) : HResult; stdcall;
    function SetCurrentTexturePalette(const PaletteNumber : Cardinal) : HResult; stdcall;
    function GetCurrentTexturePalette(out PaletteNumber : Cardinal) : HResult; stdcall;
    function SetScissorRect(const Rect : TRect) : HResult; stdcall;
    function GetScissorRect(out Rect : TRect) : HResult; stdcall;
    function SetSoftwareVertexProcessing(Software : Bool) : HResult; stdcall;
    function GetSoftwareVertexProcessing : Bool; stdcall;
    function SetNPatchMode(nSegments : Single) : HResult; stdcall;
    function GetNPatchMode : Single; stdcall;
    function DrawPrimitive(PrimitiveType : TD3DPrimitiveType; const StartVertex, PrimitiveCount : Cardinal) : HResult; stdcall;
    function DrawIndexedPrimitive(_Type : TD3DPrimitiveType; const BaseVertexIndex : Integer; const MinVertexIndex, NumVertices, StartIndex, PrimCount : Cardinal) : HResult; stdcall;
    function DrawPrimitiveUP(PrimitiveType : TD3DPrimitiveType; const PrimitiveCount : Cardinal; VertexStreamZeroData : Pointer; const VertexStreamZeroStride : Cardinal) : HResult; stdcall;
    function DrawIndexedPrimitiveUP(PrimitiveType : TD3DPrimitiveType; const MinVertexIndex, NumVertices, PrimitiveCount : Cardinal; IndexData : Pointer; IndexDataFormat : TD3DFormat; VertexStreamZeroData : Pointer; const VertexStreamZeroStride : Cardinal) : HResult; stdcall;
    function ProcessVertices(const SrcStartIndex, DestIndex, VertexCount : Cardinal; DestBuffer : IDirect3DVertexBuffer9; VertexDecl : IDirect3DVertexDeclaration9; const Flags : LongWord) : HResult; stdcall;
    function CreateVertexDeclaration(VertexElements : TD3DVertexElement9; out Decl : IDirect3DVertexDeclaration9) : HResult; stdcall;
    function SetVertexDeclaration(Decl : IDirect3DVertexDeclaration9) : HResult; stdcall;
    function GetVertexDeclaration(out Decl : IDirect3DVertexDeclaration9) : HResult; stdcall;
    function SetFVF(const FVF : LongWord) : HResult; stdcall;
    function GetFVF(out FVF : LongWord) : HResult; stdcall;
    function CreateVertexShader(_Function : PLongWord; out Shader : IDirect3DVertexShader9) : HResult; stdcall;
    function SetVertexShader(Shader : IDirect3DVertexShader9) : HResult; stdcall;
    function GetVertexShader(out Shader : IDirect3DVertexShader9) : HResult; stdcall;
    function SetVertexShaderConstantF(const StartRegister : Cardinal; ConstantData : PSingle; const Vector4fCount : Cardinal) : HResult; stdcall;
    function GetVertexShaderConstantF(const StartRegister : Cardinal; ConstantData : PSingle; const Vector4fCount : Cardinal) : HResult; stdcall;
    function SetVertexShaderConstantI(const StartRegister : Cardinal; ConstantData : PInteger; const  Vector4iCount : Cardinal) : HResult; stdcall;
    function GetVertexShaderConstantI(const StartRegister : Cardinal; ConstantData : PInteger; const Vector4iCount : Cardinal) : HResult; stdcall;
    function SetVertexShaderConstantB(const StartRegister : Cardinal; ConstantData : PBool; const BoolCount : Cardinal) : HResult; stdcall;
    function GetVertexShaderConstantB(const StartRegister : Cardinal; ConstantData : PBool; const BoolCount : Cardinal) : HResult; stdcall;
    function SetStreamSource(const StreamNumber : Cardinal; StreamData : IDirect3DVertexBuffer9; const OffsetInBytes, Stride : Cardinal) : HResult; stdcall;
    function GetStreamSource(const StreamNumber : Cardinal; out StreamData : IDirect3DVertexBuffer9; out OffsetInBytes, Stride : Cardinal) : HResult; stdcall;
    function SetStreamSourceFreq(const StreamNumber, Divider : Cardinal) : HResult; stdcall;
    function GetStreamSourceFreq(const StreamNumber : Cardinal; out Divider : Cardinal) : HResult; stdcall;
    function SetIndices(IndexData : IDirect3DIndexBuffer9) : HResult; stdcall;
    function GetIndices(out IndexData : IDirect3DIndexBuffer9) : HResult; stdcall;
    function CreatePixelShader(_Function : PLongWord; out Shader : IDirect3DPixelShader9) : HResult; stdcall;
    function SetPixelShader(Shader : IDirect3DPixelShader9) : HResult; stdcall;
    function GetPixelShader(out Shader : IDirect3DPixelShader9) : HResult; stdcall;
    function SetPixelShaderConstantF(const StartRegister : Cardinal; ConstantData : PSingle; const Vector4fCount : Cardinal) : HResult; stdcall;
    function GetPixelShaderConstantF(const StartRegister : Cardinal; ConstantData : PSingle; const Vector4fCount : Cardinal) : HResult; stdcall;
    function SetPixelShaderConstantI(const StartRegister : Cardinal; ConstantData : PInteger; const Vector4iCount : Cardinal) : HResult; stdcall;
    function GetPixelShaderConstantI(const StartRegister : Cardinal; ConstantData : PInteger; const Vector4iCount : Cardinal) : HResult; stdcall;
    function SetPixelShaderConstantB(const StartRegister : Cardinal; ConstantData : PBool; const BoolCount : Cardinal) : HResult; stdcall;
    function GetPixelShaderConstantB(const StartRegister : Cardinal; ConstantData : PBool; const BoolCount : Cardinal) : HResult; stdcall;
    function DrawRectPatch(const Handle : Cardinal; NumSegs : P4SingleArray; const RectPatchInfo : TD3DRectPatchInfo) : HResult; stdcall;
    function DrawTriPatch(const Handle : Cardinal; NumSegs : P4SingleArray; const TriPatchInfo : TD3DRectPatchInfo) : HResult; stdcall;
    function DeletePatch(const Handle : Cardinal) : HResult; stdcall;
    function CreateQuery(_Type : TD3DQueryType; out Query : IDirect3DQuery9) : HResult; stdcall;
  end;


  IDirect3DStateBlock9 = interface (IUnknown)
    ['{B07C4FE5-310D-4BA8-A23C-4F0F206F218B}']
    (*** IDirect3DStateBlock9 methods ***)
    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;
    function Capture : HResult; stdcall;
    function Apply : HResult; stdcall;
  end;


  IDirect3DSwapChain9 = interface (IUnknown)
    ['{794950F2-ADFC-458A-905E-10A10B0B503B}']
    (*** IDirect3DSwapChain9 methods ***)
    function Present(SourceRect, DestRect : PRect; DestWindowOverride : HWND; DirtyRegion : PRgnData; const Flags : LongWord) : HResult; stdcall;
    function GetFrontBufferData(DestSurface : IDirect3DSurface9) : HResult; stdcall;
    function GetBackBuffer(const iBackBuffer : Cardinal; _Type : TD3DBackBufferType; out BackBuffer : IDirect3DSurface9) : HResult; stdcall;
    function GetRasterStatus(out RasterStatus : TD3DRasterStatus) : HResult; stdcall;
    function GetDisplayMode(out Mode : TD3DDisplayMode) : HResult; stdcall;
    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;
    function GetPresentParameters(out PresentationParameters : TD3DPresentParameters) : HResult; stdcall;
  end;

  IDirect3DResource9 = interface (IUnknown)
    ['{794950F2-ADFC-458A-905E-10A10B0B503B}']
    (*** IDirect3DResource9 methods ***)
    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;
    function SetPrivateData(const refguid : TGUID; Data : Pointer; const SizeOfData, Flags : LongWord) : HResult; stdcall;
    function GetPrivateData(const refguid : TGUID; Data : Pointer; out SizeOfData : LongWord) : HResult; stdcall;
    function FreePrivateData(const refguid : TGUID) : HResult; stdcall;
    function SetPriority(const PriorityNew : LongWord) : LongWord; stdcall;
    function GetPriority : LongWord; stdcall;
    procedure PreLoad stdcall;
    function GetType : TD3DResourceType; stdcall;
  end;

  IDirect3DVertexDeclaration9 = interface (IUnknown)
    ['{DD13C59C-36FA-4098-A8FB-C7ED39DC8546}']
    (*** IDirect3DVertexDeclaration9 methods ***)
    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;
    function GetDeclaration(Decl : PD3DVertexElement9Array; var NumElements : Cardinal) : HResult; stdcall;
  end;


  IDirect3DVertexShader9 = interface (IUnknown)
    ['{EFC5557E-6265-4613-8A94-43857889EB36}']
    (*** IDirect3DVertexShader9 methods ***)
    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;
    function GetFunction(Data : Pointer; var SizeOfData : Cardinal) : HResult; stdcall;
  end;

  IDirect3DPixelShader9 = interface (IUnknown)
    ['{6D3BDBDC-5B02-4415-B852-CE5E8BCCB289}']
    (*** IDirect3DPixelShader9 methods ***)
    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;
    function GetFunction(Data : Pointer; var SizeOfData : Cardinal) : HResult; stdcall;
  end;

  IDirect3DBaseTexture9 = interface (IDirect3DResource9)
    ['{580CA87E-1D3C-4D54-991D-B7D3E3C298CE}']
    (*** IDirect3DBaseTexture9 methods ***)
    function SetLOD(const LODNew : LongWord) : LongWord; stdcall;
    function GetLOD : LongWord; stdcall;
    function GetLevelCount : LongWord; stdcall;
    function SetAutoGenFilterType(FilterType : TD3DTextureFilterType) : HResult; stdcall;
    function GetAutoGenFilterType : TD3DTextureFilterType; stdcall;
    procedure GenerateMipSubLevels stdcall;
  end;

  PIDirect3DTexture9 = ^IDirect3DTexture9;
  IDirect3DTexture9 = interface (IDirect3DBaseTexture9)
    ['{85C31227-3DE5-4F00-9B3A-F11AC38C18B5}']
    (*** IDirect3DTexture9 methods ***)
    function GetLevelDesc(const Level : Cardinal; out Desc : TD3DSurfaceDesc) : HResult; stdcall;
    function GetSurfaceLevel(const Level : Cardinal; out SurfaceLevel : IDirect3DSurface9) : HResult; stdcall;
    function LockRect(const Level : Cardinal; out LockedRect : TD3DLockedRect; Rect : PRect; Flags : LongWord) : HResult; stdcall;
    function UnlockRect(const Level : Cardinal) : HResult; stdcall;
    function AddDirtyRect(DirtyRect : PRect) : HResult; stdcall;
  end;

  PIDirect3DVolumeTexture9 = ^IDirect3DVolumeTexture9;
  IDirect3DVolumeTexture9 = interface (IDirect3DBaseTexture9)
    ['{2518526C-E789-4111-A7B9-47EF328D13E6}']
    (*** IDirect3DVolumeTexture9 methods ***)
    function GetLevelDesc(const Level : Cardinal; out Desc : TD3DVolumeDesc) : HResult; stdcall;
    function GetVolumeLevel(const Level : Cardinal; out VolumeLevel : IDirect3DVolume9) : HResult; stdcall;
    function LockBox(const Level : Cardinal; out LockedVolume : TD3DLockedBox; Box : PD3DBox; const Flags : LongWord) : HResult; stdcall;
    function UnlockBox(const Level : Cardinal) : HResult; stdcall;
    function AddDirtyBox(DirtyBox : PD3DBox) : HResult; stdcall;
  end;

  PIDirect3DCubeTexture9 = ^IDirect3DCubeTexture9;
  IDirect3DCubeTexture9 = interface (IDirect3DBaseTexture9)
    ['{FFF32F81-D953-473A-9223-93D652ABA93F}']
    (*** IDirect3DCubeTexture9 methods ***)
    function GetLevelDesc(const Level : Cardinal; out Desc : TD3DSurfaceDesc) : HResult; stdcall;
    function GetCubeMapSurface(FaceType : TD3DCubeMapFaces; const Level : Cardinal; out CubeMapSurface : IDirect3DSurface9) : HResult; stdcall;
    function LockRect(FaceType : TD3DCubeMapFaces; const Level : Cardinal; out LockedRect : TD3DLockedRect; Rect : PRect; const Flags : LongWord) : HResult; stdcall;
    function UnlockRect(FaceType : TD3DCubeMapFaces; const Level : Cardinal) : HResult; stdcall;
    function AddDirtyRect(FaceType : TD3DCubeMapFaces; DirtyRect : PRect) : HResult; stdcall;
  end;

  IDirect3DVertexBuffer9 = interface (IDirect3DResource9)
    ['{B64BB1B5-FD70-4DF6-BF91-19D0A12455E3}']
    (*** IDirect3DVertexBuffer9 methods ***)
    function Lock(const OffsetToLock, SizeToLock : Cardinal; out Data : Pointer; const Flags : LongWord) : HResult; stdcall;
    function Unlock : HResult; stdcall;
    function GetDesc(out Desc : TD3DVertexBufferDesc) : HResult; stdcall;
  end;                      

  IDirect3DIndexBuffer9 = interface (IDirect3DResource9)
    ['{7C9DD65E-D3F7-4529-ACEE-785830ACDE35}']
    (*** IDirect3DIndexBuffer9 methods ***)
    function Lock(const OffsetToLock, SizeToLock : Cardinal; out Data : Pointer; const Flags : LongWord) : HResult; stdcall;
    function Unlock : HResult; stdcall;
    function GetDesc(out Desc : TD3DIndexBufferDesc) : HResult; stdcall;
  end;

  IDirect3DSurface9 = interface (IDirect3DResource9)
    ['{B64BB1B5-FD70-4DF6-BF91-19D0A12455E3}']
    (*** IDirect3DSurface9 methods ***)
    function GetContainer(riid : TGUID; out Container : Pointer) : HResult; stdcall;
    function GetDesc(out Desc : TD3DSurfaceDesc) : HResult; stdcall;
    function LockRect(out LockedRect : TD3DLockedRect; Rect : PRect; const Flags : LongWord) : HResult; stdcall;
    function UnlockRect : HResult; stdcall;
    function GetDC(out hdc : HDC) : HResult; stdcall;
    function ReleaseDC(hdc : HDC) : HResult; stdcall;
  end;


  IDirect3DVolume9 = interface (IUnknown)
    ['{24F416E6-1F67-4AA7-B88E-D33F6F3128A1}']
    (*** IDirect3DVolume9 methods ***)
    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;
    function SetPrivateData(const refguid : TGUID; Data : Pointer; const SizeOfData, Flags : LongWord) : HResult; stdcall;
    function GetPrivateData(const refguid : TGUID; Data : Pointer; out SizeOfData : LongWord) : HResult; stdcall;
    function FreePrivateData(const refguid : TGUID) : HResult; stdcall;
    function GetContainer(riid : TGUID; out Container : Pointer) : HResult; stdcall;
    function GetDesc(out Desc : TD3DVolumeDesc) : HResult; stdcall;
    function LockBox(out LockedVolume : TD3DLockedBox; Box : PD3DBox; const Flags : LongWord) : HResult; stdcall;
    function UnlockBox : HResult; stdcall;
  end;

  IDirect3DQuery9 = interface (IUnknown)
    ['{D9771460-A695-4F26-BBD3-27B840B541CC}']
    (*** IDirect3DQuery9 methods ***)
    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;
    function GetType : TD3DQueryType; stdcall;
    function GetDataSize : LongWord; stdcall;
    function Issue(const IssueFlags : LongWord) : HResult; stdcall;
    function GetData(Data : Pointer; const Size, GetDataFlags : LongWord) : HResult; stdcall;
  end;

(*)
 *******************************************************************************
 * Flags for SetPrivateData method on all D3D9 interfaces
 *
 * The passed pointer is an IUnknown ptr. The SizeOfData argument to SetPrivateData
 * must be set to sizeof( IUnknown* ). Direct3D will call AddRef through this
 * pointer and Release when the private data is destroyed. The data will be
 * destroyed when another SetPrivateData with the same GUID is set, when
 * FreePrivateData is called, or when the D3D9 object is freed.
 *******************************************************************************
(*)
const
  D3DSPD_IUNKNOWN                         = $00000001;

(*
 *******************************************************************************
 *
 * Flags for IDirect3D9::CreateDevice's BehaviorFlags
 *
 *******************************************************************************
(*)

  D3DCREATE_FPU_PRESERVE                  = $00000002;
  D3DCREATE_MULTITHREADED                 = $00000004;

  D3DCREATE_PUREDEVICE                    = $00000010;
  D3DCREATE_SOFTWARE_VERTEXPROCESSING     = $00000020;
  D3DCREATE_HARDWARE_VERTEXPROCESSING     = $00000040;
  D3DCREATE_MIXED_VERTEXPROCESSING        = $00000080;

  D3DCREATE_DISABLE_DRIVER_MANAGEMENT     = $00000100;
  D3DCREATE_ADAPTERGROUP_DEVICE           = $00000200;


(*)
 *******************************************************************************
 *
 * Parameter for IDirect3D9::CreateDevice's iAdapter
 *
 *******************************************************************************
(*)

  D3DADAPTER_DEFAULT                     = 0;

(*)
 *******************************************************************************
 *
 * Flags for IDirect3D9::EnumAdapters
 *
 *******************************************************************************
(*)

  D3DENUM_WHQL_LEVEL                      = $00000002;

(*)
 *******************************************************************************
 *
 * Maximum number of back-buffers supported in DX8
 *
 *******************************************************************************
(*)

  D3DPRESENT_BACK_BUFFERS_MAX             = 3;

(*)
 *******************************************************************************
 *
 * Flags for IDirect3DDevice9::SetGammaRamp
 *
 *******************************************************************************
(*)

  D3DSGR_NO_CALIBRATION                  = $00000000;
  D3DSGR_CALIBRATE                       = $00000001;

(*)
 *******************************************************************************
 *
 * Flags for IDirect3DDevice9::SetCursorPosition
 *
 *******************************************************************************
(*)

  D3DCURSOR_IMMEDIATE_UPDATE             = $00000001;

(*)
 *******************************************************************************
 *
 * Flags for IDirect3DSwapChain9::Present
 *
 *******************************************************************************
(*)

  D3DPRESENT_DONOTWAIT                   = $00000001;
  D3DPRESENT_LINEAR_CONTENT              = $00000002;

(*)
 *******************************************************************************
 *
 * Flags for DrawPrimitive/DrawIndexedPrimitive
 *   Also valid for Begin/BeginIndexed
 *   Also valid for VertexBuffer::CreateVertexBuffer
 *******************************************************************************
(*)

(*)
 *  DirectDraw error codes
(*)

const
  _FACD3D = $876;
  MAKE_D3DHRESULT = (1 shl 31) or (_FACD3D shl 16);

  MAKE_D3DSTATUS = (0 shl 31) or (_FACD3D shl 16); // ?? MAKE_D3DSTATUS( code )  MAKE_HRESULT( 0, _FACD3D, code )

(*)
 * Direct3D Errors
(*)
  D3D_OK                                  = S_OK;

  D3DERR_WRONGTEXTUREFORMAT               = HResult(MAKE_D3DHResult + 2072);
  D3DERR_UNSUPPORTEDCOLOROPERATION        = HResult(MAKE_D3DHResult + 2073);
  D3DERR_UNSUPPORTEDCOLORARG              = HResult(MAKE_D3DHResult + 2074);
  D3DERR_UNSUPPORTEDALPHAOPERATION        = HResult(MAKE_D3DHResult + 2075);
  D3DERR_UNSUPPORTEDALPHAARG              = HResult(MAKE_D3DHResult + 2076);
  D3DERR_TOOMANYOPERATIONS                = HResult(MAKE_D3DHResult + 2077);
  D3DERR_CONFLICTINGTEXTUREFILTER         = HResult(MAKE_D3DHResult + 2078);
  D3DERR_UNSUPPORTEDFACTORVALUE           = HResult(MAKE_D3DHResult + 2079);
  D3DERR_CONFLICTINGRENDERSTATE           = HResult(MAKE_D3DHResult + 2081);
  D3DERR_UNSUPPORTEDTEXTUREFILTER         = HResult(MAKE_D3DHResult + 2082);
  D3DERR_CONFLICTINGTEXTUREPALETTE        = HResult(MAKE_D3DHResult + 2086);
  D3DERR_DRIVERINTERNALERROR              = HResult(MAKE_D3DHResult + 2087);

  D3DERR_NOTFOUND                         = HResult(MAKE_D3DHResult + 2150);
  D3DERR_MOREDATA                         = HResult(MAKE_D3DHResult + 2151);
  D3DERR_DEVICELOST                       = HResult(MAKE_D3DHResult + 2152);
  D3DERR_DEVICENOTRESET                   = HResult(MAKE_D3DHResult + 2153);
  D3DERR_NOTAVAILABLE                     = HResult(MAKE_D3DHResult + 2154);
  D3DERR_OUTOFVIDEOMEMORY                 = HResult(MAKE_D3DHResult + 380);
  D3DERR_INVALIDDEVICE                    = HResult(MAKE_D3DHResult + 2155);
  D3DERR_INVALIDCALL                      = HResult(MAKE_D3DHResult + 2156);
  D3DERR_DRIVERINVALIDCALL                = HResult(MAKE_D3DHResult + 2157);
  D3DERR_WASSTILLDRAWING                  = HResult(MAKE_D3DHResult + 540);
  D3DOK_NOAUTOGEN                         = HResult(MAKE_D3DSTATUS + 2159);

(*)
 * DLL Function for creating a Direct3D9 object. This object supports
 * enumeration and allows the creation of Direct3DDevice9 objects.
 * Pass the value of the constant D3D_SDK_VERSION to this function, so
 * that the run-time can validate that your application was compiled
 * against the right headers.
(*)

function Direct3DCreate9(SDKVersion : Cardinal) : IDirect3D9;

{$IFNDEF STATIC_LINKING}
var _Direct3DCreate9 : function (SDKVersion : Cardinal) : Pointer; stdcall;
{$ENDIF}

implementation

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

function D3DCOLOR_XYUV(y,u,v : Cardinal) : TD3DColor;
begin
  Result := D3DCOLOR_ARGB($FF, y, u, v);
end;

function D3DCOLOR_AYUV(a, y, u, v : Cardinal) : TD3DColor;
begin
  Result := D3DCOLOR_ARGB(a, y, u, v);
end;

function D3DCOLOR_COLORVALUE(r, g, b, a : Single) : TD3DColor;
begin
  Result := D3DCOLOR_RGBA(Byte(Round(r * 255)), Byte(Round(g * 255)), Byte(Round(b * 255)), Byte(Round(a * 255)))
end;

function D3DTS_WORLDMATRIX(Index : LongWord) : TD3DTransformStateType;
begin
  Result := TD3DTransformStateType(Index + 256);
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

function D3DSHADER_COMMENT(_DWordSize : LongWord)  : LongWord;
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
  Result := Byte(ch0) or (Byte(ch1) shl 8) or (Byte(ch2) shl 16) or (Byte(ch3) shl 24);
end;

{$IFDEF STATIC_LINKING}
function _Direct3DCreate9(SDKVersion : Cardinal) : Pointer; stdcall; external d3d9dllname name 'Direct3DCreate9';
{$ENDIF}

function Direct3DCreate9(SDKVersion : Cardinal) : IDirect3D9;
begin
{$IFNDEF STATIC_LINKING}
  if Assigned(_Direct3DCreate9) then
  begin
{$ENDIF}
    Result := IDirect3D9(_Direct3DCreate9(SDKVersion));
    if Result <> nil then Result._Release;
{$IFNDEF STATIC_LINKING}
  end else Result := nil;
{$ENDIF}
end;

{$IFNDEF STATIC_LINKING}
initialization
begin
  d3d9dll := LoadLibrary(d3d9dllname);
  if d3d9dll <> 0 then
    _Direct3DCreate9 := GetProcAddress(d3d9dll, 'Direct3DCreate9')
  else _Direct3DCreate9 := nil;
end;

finalization
begin
  FreeLibrary(d3d9dll);
end;
{$ENDIF}


end.
