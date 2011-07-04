(*)
 [------------------------------------------------------------------------------
 [               _     _     _   _     _         _ ___   _     ___
 [             _| |___| |___| |_|_|  _| |_ _   _| |_  |_| |_ _| . |
 [            | . | -_| | . |   | |_| . |_'_|_| . |_  | . |_'_|_  |
 [            |___|___|_|  _|_|_|_|_|___|_,_|_|___|___|___|_,_|___|
 [                      |_|
 [
 [------------------------------------------------------------------------------
 [  d3dx9 Delphi Adaptation (c) by Tim Baumgarten
 [  lib to dll conversion by Christopher McGinnis
 [------------------------------------------------------------------------------
 [  Files    : d3dx9.h
 [             d3dx9math.h
 [             d3dx9core.h
 [             d3dx9mesh.h
 [             d3dx9tex.h
 [             d3dx9shader.h
 [             d3dx9effect.h
 [             d3dx9shape.h
 [             d3dx9anim.h
 [  Modified : 02-Oct-2003
 [  E-Mail   : Ampaze at gmx dot net
 [  Download : http://www.crazyentertainment.net
 [------------------------------------------------------------------------------
(*)

(*)
 [------------------------------------------------------------------------------
 [ History :
 [----------
 [ 02-Oct-2003 (Tim Baumgarten) : Changes to fit the D3DX9 Summer Update
 [ 10-Jul-2003 (Tim Baumgarten) : Changed some Enum Stuff that was broken
 [ 02-Jun-2003 (Tim Baumgarten) : you can change d3dx9dllname to 'd3dx9d.dll'
 [                                for debuging and the "inline functions" will
 [                                still use 'd3dx9.dll'
 [ 02-Jun-2003 (Christopher McGinnis) : new dll build from d3dx9a
 [ 07-Apr-2003 (Tim Baumgarten) : Gave overloads to D3DXLoadMeshFromX{A|W}
 [ 07-Apr-2003 (Christopher McGinnis) : fixed TD3DXEffectInstance
 [ 03-Apr-2003 (Tim Baumgarten) : again new dll,
 [                                all "inline" functions now called from dll
 [ 01-Apr-2003 (Tim Baumgarten) : new dll,
 [                                some "inline" functions now called from dll
 [ 19-Mar-2003 (Tim Baumgarten) : bugfixed
 [                                - D3DXPlaneIntersectLine
 [                                - ID3DXAllocateHierarchy.CreateMeshContainer
 [ 16-Feb-2003 (Tim Baumgarten) : bugfixes
 [ 31-Jan-2003 (Tim Baumgarten) : bugfixed D3DXCreateLine
 [ 19-Jan-2003 (Tim Baumgarten) : first public version
 [                                most likely to contains bugs, please report
 [------------------------------------------------------------------------------
(*)

unit d3dx9;

{$INCLUDE jedi.inc}

{$MINENUMSIZE 4}
{$ALIGN ON}

//Remove dot to make all enums to be const's
{$DEFINE NOENUMS}

{$IFDEF DELPHI7_UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFNDEF DELPHI6_UP}
  {$DEFINE NOENUMS}
{$ENDIF}

interface

uses windows, Direct3D9, sysutils, dxfile9, activex;

type
  PCardinal = ^Cardinal;

const
  d3dx9dllname = 'd3dx9.dll';
  d3dx9inldllname = 'd3dx9.dll';

(*)
 *******************************************************************************
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File    : d3dx9.h
 *  Content : D3DX utility library
 *
 *******************************************************************************
(*)

const
  D3DX_DEFAULT         = Cardinal(-1);
  D3DX_DEFAULT_NONPOW2 = Cardinal(-2);
  D3DX_DEFAULT_FLOAT   : Single = 3.402823466e+38;  // max single value

(*)
 *******************************************************************************
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File    : d3dx9math.h
 *  Content : D3DX math types and functions
 *
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 *
 * General purpose utilities
 *
 *******************************************************************************
(*)
const
  D3DX_PI    : Single = 3.141592654;
  D3DX_1BYPI : Single = 0.318309886;

function D3DXToRadian(const Degree : Single) : Single;
function D3DXToDegree(const Radian : Single) : Single;

(*)
 *******************************************************************************
 *
 * 16 bit floating point numbers
 *
 *******************************************************************************
(*)

const
  D3DX_16F_DIG        = 3;                       // # of decimal digits of precision
  D3DX_16F_EPSILON    : Single = 4.8875809e-4;   // smallest such that 1.0 + epsilon != 1.0
  D3DX_16F_MANT_DIG   = 11;                      // # of bits in mantissa
  D3DX_16F_MAX        = 6.550400e+004;           // max value
  D3DX_16F_MAX_10_EXP = 4;                       // max decimal exponent
  D3DX_16F_MAX_EXP    = 15;                      // max binary exponent
  D3DX_16F_MIN        : Single = 6.1035156e-5;   // min positive value
  D3DX_16F_MIN_10_EXP = (-4);                    // min decimal exponent
  D3DX_16F_MIN_EXP    = (-12);                   // min binary exponent
  D3DX_16F_RADIX      = 2;                       // exponent radix
  D3DX_16F_ROUNDS     = 1;                       // addition rounding: near

type
  PD3DXFloat16 = ^TD3DXFloat16;
  TD3DXFloat16 = packed record
    Value : Word;
  end;

(*)
 *******************************************************************************
 *
 * Vectors
 *
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 * 2D Vector
 *******************************************************************************
(*)

type
  PD3DXVector2 = ^TD3DXVector2;
  TD3DXVector2 = packed record
    x, y : Single;
  end;

const
  D3DXVector2Zero : TD3DXVector2 = (x : 0; y : 0);

function D3DXVector2(const x, y : Single) : TD3DXVector2;
function D3DXVector2Equal(const v1, v2 : TD3DXVector2) : Boolean;

(*)
 *******************************************************************************
 * 2D Vector (16 Bit)
 *******************************************************************************
(*)

type
  PD3DXVector2_16F = ^TD3DXVector2_16F;
  TD3DXVector2_16F = packed record
    x, y : TD3DXFloat16
  end;

(*)
 *******************************************************************************
 * 3D Vector
 *******************************************************************************
(*)

type
  PD3DXVector3 = ^TD3DXVector3;
  TD3DXVector3 = TD3DVector;

const
  D3DXVector3Zero : TD3DXVector3 = (x : 0; y : 0; z : 0);
  
function D3DXVector3(const x, y, z : Single) : TD3DXVector3;
function D3DXVector3Equal(const v1, v2 : TD3DXVector3) : Boolean;

(*)
 *******************************************************************************
 * 3D Vector (16 Bit)
 *******************************************************************************
(*)

type
  PD3DXVector3_16F = ^TD3DXVector3_16F;
  TD3DXVector3_16F = packed record
    x, y, z : TD3DXFloat16
  end;

(*)
 *******************************************************************************
 * 4D Vector
 *******************************************************************************
(*)

type
  PD3DXVector4 = ^TD3DXVector4;
  TD3DXVector4 = packed record
    x, y, z, w : Single;
  end;

const
  D3DXVector4Zero : TD3DXVector4 = (x : 0; y : 0; z : 0; w : 0);

function D3DXVector4(const x, y, z, w : Single) : TD3DXVector4;
function D3DXVector4Equal(const v1, v2 : TD3DXVector4) : Boolean;

(*)
 *******************************************************************************
 * 4D Vector (16 Bit)
 *******************************************************************************
(*)
type
  PD3DXVector4_16F = ^TD3DXVector4_16F;
  TD3DXVector4_16F = packed record
    x, y, z, w : TD3DXFloat16
  end;

(*)
 *******************************************************************************
 *
 * Matrices
 *
 *******************************************************************************
(*)

type
  PPD3DXMatrix = ^PD3DXMatrix;
  PD3DXMatrix = ^TD3DXMatrix;
  TD3DXMatrix = TD3DMatrix;

const
  D3DXMatrixIdentity : TD3DXMatrix = (_00 : 1; _01 : 0; _02 : 0; _03 : 0;
                                      _10 : 0; _11 : 1; _12 : 0; _13 : 0;
                                      _20 : 0; _21 : 0; _22 : 1; _23 : 0;
                                      _30 : 0; _31 : 0; _32 : 0; _33 : 1);

function D3DXMatrix(const m00, m01, m02, m03,
                          m10, m11, m12, m13,
                          m20, m21, m22, m23,
                          m30, m31, m32, m33 : Single) : TD3DXMatrix;
function D3DXMatrixAdd(const m1, m2 : TD3DXMatrix) : TD3DXMatrix;
function D3DXMatrixSubtract(const m1, m2 : TD3DXMatrix) : TD3DXMatrix;
function D3DXMatrixScale(const m : TD3DXMatrix; const ScaleBy : Single) : TD3DXMatrix;
function D3DXMatrixEqual(const m1, m2 : TD3DXMatrix) : Boolean;

(*)
 *******************************************************************************
 *
 * Quaternions
 *
 *******************************************************************************
(*)

type
  PD3DXQuaternion = ^TD3DXQuaternion;
  TD3DXQuaternion = packed record
    x, y, z, w : Single;
  end;

const
  D3DXQuaternionIdentity : TD3DXQuaternion = (x : 0; y : 0; z : 0; w : 1);

function D3DXQuaternion(const x, y, z, w : Single) : TD3DXQuaternion;
function D3DXQuaternionAdd(const q1, q2 : TD3DXQuaternion) : TD3DXQuaternion;
function D3DXQuaternionSubtract(const q1, q2 : TD3DXQuaternion) : TD3DXQuaternion;
function D3DXQuaternionEqual(const q1, q2 : TD3DXQuaternion) : Boolean;
function D3DXQuaternionScale(const q : TD3DXQuaternion; const ScaleBy : Single) : TD3DXQuaternion;

(*)
 *******************************************************************************
 *
 * Planes
 *
 *******************************************************************************
(*)

type
  PD3DXPlane = ^TD3DXPlane;
  TD3DXPlane = packed record
    a, b, c, d : Single;
  end;

const
  D3DXPlaneZero : TD3DXPlane = (a : 0; b : 0; c : 0; d : 0);
function D3DXPlane(const a, b, c, d : Single) : TD3DXPlane;
function D3DXPlaneEqual(const p1, p2 : TD3DXPlane) : Boolean;

(*)
 *******************************************************************************
 *
 * Colors
 *
 *******************************************************************************
(*)

type
  PD3DXColor = ^TD3DXColor;
  TD3DXColor = packed record
    r, g, b, a : Single;
  end;

function D3DXColor(const r, g, b, a : Single) : TD3DXColor;
function D3DXColorToLongWord(const c : TD3DXColor): LongWord;
function D3DXColorFromLongWord(const c : LongWord) : TD3DXColor;
function D3DXColorEqual(const c1, c2 : TD3DXColor) : Boolean;

(*)
 *******************************************************************************
 *
 * D3DX math functions:
 *
 * NOTE:
 *  * All these functions can take the same object as in and out parameters.
 *
 *  * Out parameters are typically also returned as return values, so that
 *    the output of one function may be used as a parameter to another.
 *
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 * Float16
 *******************************************************************************
(*)

// non-inline

// Converts an array 32-bit floats to 16-bit floats
function D3DXFloat32To16Array(OutArray : PD3DXFloat16; InArray : PSingle; const n : Cardinal) : PD3DXFloat16 stdcall; external d3dx9dllname;

// Converts an array 16-bit floats to 32-bit floats
function D3DXFloat16To32Array(OutArray : PSingle; InArray : PD3DXFloat16; const n : Cardinal) : PSingle; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 * 2D Vector
 *******************************************************************************
(*)

// inline, Delphi uses dll

function D3DXVec2Length(const V : TD3DXVector2) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Length';
function D3DXVec2Length(V : PD3DXVector2) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Length';

function D3DXVec2LengthSq(const V : TD3DXVector2) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2LengthSq';
function D3DXVec2LengthSq(V : PD3DXVector2) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2LengthSq';

function D3DXVec2Dot(V1, V2 : PD3DXVector2) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Dot';
function D3DXVec2Dot(const V1 : TD3DXVector2; V2 : PD3DXVector2) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Dot';
function D3DXVec2Dot(V1 : PD3DXVector2; const V2 : TD3DXVector2) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Dot';
function D3DXVec2Dot(const V1, V2 : TD3DXVector2) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Dot';

// Z component of ((x1,y1,0) cross (x2,y2,0))
function D3DXVec2CCW(V1, V2 : PD3DXVector2) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2CCW';
function D3DXVec2CCW(const V1 : TD3DXVector2; V2 : PD3DXVector2) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2CCW';
function D3DXVec2CCW(V1 : PD3DXVector2; const V2 : TD3DXVector2) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2CCW';
function D3DXVec2CCW(const V1, V2 : TD3DXVector2) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2CCW';

function D3DXVec2Add(Out, V1, V2 : PD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Add';
function D3DXVec2Add(out Out : TD3DXVector2; V1, V2 : PD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Add';
function D3DXVec2Add(Out : PD3DXVector2; const V1 : TD3DXVector2; V2 : PD3DXVector2) : pD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Add';
function D3DXVec2Add(out Out : TD3DXVector2; const V1 : TD3DXVector2; V2 : PD3DXVector2) : pD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Add';
function D3DXVec2Add(Out, V1 : PD3DXVector2; const V2 : TD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Add';
function D3DXVec2Add(out Out : TD3DXVector2; V1 : PD3DXVector2; const V2 : TD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Add';
function D3DXVec2Add(Out : PD3DXVector2; const V1, V2 : TD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Add';
function D3DXVec2Add(out Out : TD3DXVector2; const V1, V2 : TD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Add';

function D3DXVec2Subtract(Out, V1, V2 : PD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Subtract';
function D3DXVec2Subtract(out Out : TD3DXVector2; V1, V2 : PD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Subtract';
function D3DXVec2Subtract(Out : PD3DXVector2; const V1 : TD3DXVector2; V2 : PD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Subtract';
function D3DXVec2Subtract(out Out : TD3DXVector2; const V1 : TD3DXVector2; V2 : PD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Subtract';
function D3DXVec2Subtract(Out, V1 : PD3DXVector2; const V2 : TD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Subtract';
function D3DXVec2Subtract(out Out : TD3DXVector2; V1 : PD3DXVector2; const V2 : TD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Subtract';
function D3DXVec2Subtract(Out : PD3DXVector2; const V1, V2 : TD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Subtract';
function D3DXVec2Subtract(out Out : TD3DXVector2; const V1, V2 : TD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Subtract';

// Minimize each component.  x = min(x1, x2), y = min(y1, y2)
function D3DXVec2Minimize(Out, V1, V2 : PD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Minimize';
function D3DXVec2Minimize(out Out : TD3DXVector2; V1, V2 : PD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Minimize';
function D3DXVec2Minimize(Out : PD3DXVector2; const V1 : TD3DXVector2; V2 : PD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Minimize';
function D3DXVec2Minimize(out Out : TD3DXVector2; const V1 : TD3DXVector2; V2 : PD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Minimize';
function D3DXVec2Minimize(Out, V1 : PD3DXVector2; const V2 : TD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Minimize';
function D3DXVec2Minimize(out Out : TD3DXVector2; V1 : PD3DXVector2; const V2 : TD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Minimize';
function D3DXVec2Minimize(Out : PD3DXVector2; const V1, V2 : TD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Minimize';
function D3DXVec2Minimize(out Out : TD3DXVector2; const V1, V2 : TD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Minimize';

// Maximize each component.  x = max(x1, x2), y = max(y1, y2)
function D3DXVec2Maximize(Out, V1, V2 : PD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Maximize';
function D3DXVec2Maximize(out Out : TD3DXVector2; V1, V2 : PD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Maximize';
function D3DXVec2Maximize(Out : PD3DXVector2; const V1 : TD3DXVector2; V2 : PD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Maximize';
function D3DXVec2Maximize(out Out : TD3DXVector2; const V1 : TD3DXVector2; V2 : PD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Maximize';
function D3DXVec2Maximize(Out, V1 : PD3DXVector2; const V2 : TD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Maximize';
function D3DXVec2Maximize(out Out : TD3DXVector2; V1 : PD3DXVector2; const V2 : TD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Maximize';
function D3DXVec2Maximize(Out : PD3DXVector2; const V1, V2 : TD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Maximize';
function D3DXVec2Maximize(out Out : TD3DXVector2; const V1, V2 : TD3DXVector2) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Maximize';

function D3DXVec2Scale(Out, V : PD3DXVector2; const ScaleBy : Single) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Scale';
function D3DXVec2Scale(out Out : TD3DXVector2; V : PD3DXVector2; const ScaleBy : Single) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Scale';
function D3DXVec2Scale(Out : PD3DXVector2; const V : TD3DXVector2; const ScaleBy : Single) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Scale';
function D3DXVec2Scale(out Out : TD3DXVector2; const V : TD3DXVector2; const ScaleBy : Single) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Scale';

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec2Lerp(Out, V1, V2 : PD3DXVector2; const ScaleBy : Single) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Lerp';
function D3DXVec2Lerp(out Out : TD3DXVector2; V1, V2 : PD3DXVector2; const ScaleBy : Single) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Lerp';
function D3DXVec2Lerp(Out : PD3DXVector2; const V1 : TD3DXVector2; V2 : PD3DXVector2; const ScaleBy : Single) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Lerp';
function D3DXVec2Lerp(out Out : TD3DXVector2; const V1 : TD3DXVector2; V2 : PD3DXVector2; const ScaleBy : Single) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Lerp';
function D3DXVec2Lerp(Out, V1 : PD3DXVector2; const V2 : TD3DXVector2; const ScaleBy : Single) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Lerp';
function D3DXVec2Lerp(out Out : TD3DXVector2; V1 : PD3DXVector2; const V2 : TD3DXVector2; const ScaleBy : Single) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Lerp';
function D3DXVec2Lerp(Out : PD3DXVector2; const V1, V2 : TD3DXVector2; const ScaleBy : Single) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Lerp';
function D3DXVec2Lerp(out Out : TD3DXVector2; const V1, V2 : TD3DXVector2; const ScaleBy : Single) : PD3DXVector2; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec2Lerp';

// non-inline

function D3DXVec2Normalize(Out, V : PD3DXVector2) : PD3DXVector2 stdcall; overload; external d3dx9dllname;
function D3DXVec2Normalize(out Out : TD3DXVector2; V : PD3DXVector2) : PD3DXVector2 stdcall; overload; external d3dx9dllname;
function D3DXVec2Normalize(Out : PD3DXVector2; const V : TD3DXVector2) : PD3DXVector2 stdcall; overload; external d3dx9dllname;
function D3DXVec2Normalize(out Out : TD3DXVector2; const V : TD3DXVector2) : PD3DXVector2 stdcall; overload; external d3dx9dllname;

// Hermite interpolation between position V1, tangent T1 (when s == 0)
// and position V2, tangent T2 (when s == 1).

function D3DXVec2Hermite(Out, V1, T1, V2, T2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(out Out : TD3DXVector2; V1, T1, V2, T2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(Out : PD3DXVector2; const V1 : TD3DXVector2; T1, V2, T2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(out Out : TD3DXVector2; const V1 : TD3DXVector2; T1, V2, T2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(Out, V1 : PD3DXVector2; const T1 : TD3DXVector2; V2, T2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(out Out : TD3DXVector2; V1 : PD3DXVector2; const T1 : TD3DXVector2; V2, T2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(Out : PD3DXVector2; const V1, T1 : TD3DXVector2; V2, T2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(out Out : TD3DXVector2; const V1, T1 : TD3DXVector2; V2, T2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(Out, V1, T1 : PD3DXVector2; const V2 : TD3DXVector2; T2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(out Out : TD3DXVector2; V1, T1 : PD3DXVector2; const V2 : TD3DXVector2; T2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(Out : PD3DXVector2; const V1 : TD3DXVector2; T1 : PD3DXVector2; const V2 : TD3DXVector2; T2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(out Out : TD3DXVector2; const V1 : TD3DXVector2; T1 : PD3DXVector2; const V2 : TD3DXVector2; T2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(Out, V1 : PD3DXVector2; const T1, V2 : TD3DXVector2; T2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(out Out : TD3DXVector2; V1 : PD3DXVector2; const T1, V2 : TD3DXVector2; T2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(Out : PD3DXVector2; const V1, T1, V2 : TD3DXVector2; T2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(out Out : TD3DXVector2; const V1, T1, V2 : TD3DXVector2; T2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(Out, V1, T1, V2 : PD3DXVector2; const T2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(out Out : TD3DXVector2; V1, T1, V2 : PD3DXVector2; const T2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(Out : PD3DXVector2; const V1 : TD3DXVector2; T1, V2 : PD3DXVector2; const T2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(out Out : TD3DXVector2; const V1 : TD3DXVector2; T1, V2 : PD3DXVector2; const T2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(Out, V1 : PD3DXVector2; const T1 : TD3DXVector2; V2 : PD3DXVector2; const T2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(out Out : TD3DXVector2; V1 : PD3DXVector2; const T1 : TD3DXVector2; V2 : PD3DXVector2; const T2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(Out : PD3DXVector2; const V1, T1 : TD3DXVector2; V2 : PD3DXVector2; const T2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(out Out : TD3DXVector2; const V1, T1 : TD3DXVector2; V2 : PD3DXVector2; const T2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(Out, V1, T1 : PD3DXVector2; const V2, T2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(out Out : TD3DXVector2; V1, T1 : PD3DXVector2; const V2, T2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(Out : PD3DXVector2; const V1 : TD3DXVector2; T1 : PD3DXVector2; const V2, T2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(out Out : TD3DXVector2; const V1 : TD3DXVector2; T1 : PD3DXVector2; const V2, T2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(Out, V1 : PD3DXVector2; const T1, V2, T2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(out Out : TD3DXVector2; V1 : PD3DXVector2; const T1, V2, T2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(Out : PD3DXVector2; const V1, T1, V2, T2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2Hermite(out Out : TD3DXVector2; const V1, T1, V2, T2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;


// CatmullRom interpolation between V1 (when s == 0) and V2 (when s == 1)
function D3DXVec2CatmullRom(Out, V0, V1, V2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2CatmullRom(out Out : TD3DXVector2; V0, V1, V2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2CatmullRom(Out : PD3DXVector2; const V0 : TD3DXVector2; V1, V2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2CatmullRom(out Out : TD3DXVector2; const V0 : TD3DXVector2; V1, V2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2CatmullRom(Out, V0 : PD3DXVector2; const V1 : TD3DXVector2; V2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2CatmullRom(out Out : TD3DXVector2; V0 : PD3DXVector2; const V1 : TD3DXVector2; V2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2CatmullRom(Out : PD3DXVector2; const V0, V1 : TD3DXVector2; V2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2CatmullRom(out Out : TD3DXVector2; const V0, V1 : TD3DXVector2; V2 : PD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2CatmullRom(Out, V0, V1 : PD3DXVector2; const V2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2CatmullRom(out Out : TD3DXVector2; V0, V1 : PD3DXVector2; const V2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2CatmullRom(Out : PD3DXVector2; const V0 : TD3DXVector2; V1 : PD3DXVector2; const V2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2CatmullRom(out Out : TD3DXVector2; const V0 : TD3DXVector2; V1 : PD3DXVector2; const V2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2CatmullRom(Out, V0 : PD3DXVector2; const V1, V2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2CatmullRom(out Out : TD3DXVector2; V0 : PD3DXVector2; const V1, V2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2CatmullRom(Out : PD3DXVector2; const V0, V1, V2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2CatmullRom(out Out : TD3DXVector2; const V0, V1, V2 : TD3DXVector2; s : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;


// Barycentric coordinates.  V1 + f(V2-V1) + g(V3-V1)
function D3DXVec2BaryCentric(Out, V1, V2, V3 : PD3DXVector2; f, g : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2BaryCentric(out Out : TD3DXVector2; V1, V2, V3 : PD3DXVector2; f, g : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2BaryCentric(Out : PD3DXVector2; const V1 : TD3DXVector2; V2, V3 : PD3DXVector2; f, g : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2BaryCentric(out Out : TD3DXVector2; const V1 : TD3DXVector2; V2, V3 : PD3DXVector2; f, g : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2BaryCentric(Out, V1 : PD3DXVector2; const V2 : TD3DXVector2; V3 : PD3DXVector2; f, g : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2BaryCentric(out Out : TD3DXVector2; V1 : PD3DXVector2; const V2 : TD3DXVector2; V3 : PD3DXVector2; f, g : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2BaryCentric(Out : PD3DXVector2; const V1, V2 : TD3DXVector2; V3 : PD3DXVector2; f, g : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2BaryCentric(out Out : TD3DXVector2; const V1, V2 : TD3DXVector2; V3 : PD3DXVector2; f, g : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2BaryCentric(Out, V1, V2 : PD3DXVector2; const V3 : TD3DXVector2; f, g : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2BaryCentric(out Out : TD3DXVector2; V1, V2 : PD3DXVector2; const V3 : TD3DXVector2; f, g : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2BaryCentric(Out : PD3DXVector2; const V1 : TD3DXVector2; V2 : PD3DXVector2; const V3 : TD3DXVector2; f, g : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2BaryCentric(out Out : TD3DXVector2; const V1 : TD3DXVector2; V2 : PD3DXVector2; const V3 : TD3DXVector2; f, g : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2BaryCentric(Out, V1 : PD3DXVector2; const V2, V3 : TD3DXVector2; f, g : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2BaryCentric(out Out : TD3DXVector2; V1 : PD3DXVector2; const V2, V3 : TD3DXVector2; f, g : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2BaryCentric(Out : PD3DXVector2; const V1, V2, V3 : TD3DXVector2; f, g : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2BaryCentric(out Out : TD3DXVector2; const V1, V2, V3 : TD3DXVector2; f, g : Single) : PD3DXVector2; stdcall; overload; external d3dx9dllname;

// Transform (x, y, 0, 1) by matrix.
function D3DXVec2Transform(Out : PD3DXVector4; v : PD3DXVector2; m : PD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec2Transform(out Out : TD3DXVector4; v : PD3DXVector2; m : PD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec2Transform(Out : PD3DXVector4; const v : TD3DXVector2; m : PD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec2Transform(out Out : TD3DXVector4; const v : TD3DXVector2; m : PD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec2Transform(Out : PD3DXVector4; v : PD3DXVector2; const m : TD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec2Transform(out Out : TD3DXVector4; v : PD3DXVector2; const m : TD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec2Transform(Out : PD3DXVector4; const v : TD3DXVector2; const m : TD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec2Transform(out Out : TD3DXVector4; const v : TD3DXVector2; const m : TD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;

// Transform (x, y, 0, 1) by matrix, project result back into w=1.
function D3DXVec2TransformCoord(Out, V : PD3DXVector2; M : PD3DXMatrix) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2TransformCoord(out Out : TD3DXVector2; V : PD3DXVector2; M : PD3DXMatrix) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2TransformCoord(Out : PD3DXVector2; const V : TD3DXVector2; M : PD3DXMatrix) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2TransformCoord(out Out : TD3DXVector2; const V : TD3DXVector2; M : PD3DXMatrix) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2TransformCoord(Out, V : PD3DXVector2; const M : TD3DXMatrix) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2TransformCoord(out Out : TD3DXVector2; V : PD3DXVector2; const M : TD3DXMatrix) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2TransformCoord(Out : PD3DXVector2; const V : TD3DXVector2; const M : TD3DXMatrix) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2TransformCoord(out Out : TD3DXVector2; const V : TD3DXVector2; const M : TD3DXMatrix) : PD3DXVector2; stdcall; overload; external d3dx9dllname;

// Transform (x, y, 0, 0) by matrix.
function D3DXVec2TransformNormal(Out, V : PD3DXVector2; M : PD3DXMatrix) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2TransformNormal(out Out : TD3DXVector2; V : PD3DXVector2; M : PD3DXMatrix) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2TransformNormal(Out : PD3DXVector2; const V : TD3DXVector2; M : PD3DXMatrix) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2TransformNormal(out Out : TD3DXVector2; const V : TD3DXVector2; M : PD3DXMatrix) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2TransformNormal(Out, V : PD3DXVector2; const M : TD3DXMatrix) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2TransformNormal(out Out : TD3DXVector2; V : PD3DXVector2; const M : TD3DXMatrix) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2TransformNormal(Out : PD3DXVector2; const V : TD3DXVector2; const M : TD3DXMatrix) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2TransformNormal(out Out : TD3DXVector2; const V : TD3DXVector2; const M : TD3DXMatrix) : PD3DXVector2; stdcall; overload; external d3dx9dllname;

// Transform Array (x, y, 0, 1) by matrix.
function D3DXVec2TransformArray(Out : PD3DXVector4; OutStride : Cardinal; V : PD3DXVector2; VStride : Cardinal; M : PD3DXMatrix; n : Cardinal) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec2TransformArray(Out : PD3DXVector4; OutStride : Cardinal; V : PD3DXVector2; VStride : Cardinal; const M : TD3DXMatrix; n : Cardinal) : PD3DXVector4; stdcall; overload; external d3dx9dllname;

// Transform Array (x, y, 0, 1) by matrix, project result back into w=1.
function D3DXVec2TransformCoordArray(Out : PD3DXVector2; OutStride : Cardinal; V : PD3DXVector2; VStride : Cardinal; M : PD3DXMatrix; n : Cardinal) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2TransformCoordArray(Out : PD3DXVector2; OutStride : Cardinal; V : PD3DXVector2; VStride : Cardinal; const M : TD3DXMatrix; n : Cardinal) : PD3DXVector2; stdcall; overload; external d3dx9dllname;

// Transform Array (x, y, 0, 0) by matrix.
function D3DXVec2TransformNormalArray(Out : PD3DXVector2; OutStride : Cardinal; V : PD3DXVector2; VStride : Cardinal; M : PD3DXMatrix; n : Cardinal) : PD3DXVector2; stdcall; overload; external d3dx9dllname;
function D3DXVec2TransformNormalArray(Out : PD3DXVector2; OutStride : Cardinal; V : PD3DXVector2; VStride : Cardinal; const M : TD3DXMatrix; n : Cardinal) : PD3DXVector2; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * 3D Vector
 *******************************************************************************
(*)

// inline, Delphi uses dll

function D3DXVec3Length(V : PD3DXVector3) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Length';
function D3DXVec3Length(const V : TD3DXVector3) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Length';

function D3DXVec3LengthSq(V : PD3DXVector3) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3LengthSq';
function D3DXVec3LengthSq(const V : TD3DXVector3) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3LengthSq';

function D3DXVec3Dot(V1, V2 : PD3DXVector3) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Dot';
function D3DXVec3Dot(const V1 : TD3DXVector3; V2 : PD3DXVector3) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Dot';
function D3DXVec3Dot(V1 : PD3DXVector3; const V2 : TD3DXVector3) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Dot';
function D3DXVec3Dot(const V1, V2 : TD3DXVector3) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Dot';

function D3DXVec3Cross(Out, V1, V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Cross';
function D3DXVec3Cross(out Out : TD3DXVector3; V1, V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Cross';
function D3DXVec3Cross(Out : PD3DXVector3; const V1 : TD3DXVector3; V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Cross';
function D3DXVec3Cross(out Out : TD3DXVector3; const V1 : TD3DXVector3; V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Cross';
function D3DXVec3Cross(Out, V1 : PD3DXVector3; const V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Cross';
function D3DXVec3Cross(out Out : TD3DXVector3; V1 : PD3DXVector3; const V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Cross';
function D3DXVec3Cross(Out : PD3DXVector3; const V1, V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Cross';
function D3DXVec3Cross(out Out : TD3DXVector3; const V1, V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Cross';

function D3DXVec3Add(Out, V1, V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Add';
function D3DXVec3Add(out Out : TD3DXVector3; V1, V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Add';
function D3DXVec3Add(Out : PD3DXVector3; const V1 : TD3DXVector3; V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Add';
function D3DXVec3Add(out Out : TD3DXVector3; const V1 : TD3DXVector3; V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Add';
function D3DXVec3Add(Out, V1 : PD3DXVector3; const V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Add';
function D3DXVec3Add(out Out : TD3DXVector3; V1 : PD3DXVector3; const V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Add';
function D3DXVec3Add(Out : PD3DXVector3; const V1, V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Add';
function D3DXVec3Add(out Out : TD3DXVector3; const V1, V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Add';

function D3DXVec3Subtract(Out, V1, V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Subtract';
function D3DXVec3Subtract(out Out : TD3DXVector3; V1, V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Subtract';
function D3DXVec3Subtract(Out : PD3DXVector3; const V1 : TD3DXVector3; V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Subtract';
function D3DXVec3Subtract(out Out : TD3DXVector3; const V1 : TD3DXVector3; V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Subtract';
function D3DXVec3Subtract(Out, V1 : PD3DXVector3; const V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Subtract';
function D3DXVec3Subtract(out Out : TD3DXVector3; V1 : PD3DXVector3; const V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Subtract';
function D3DXVec3Subtract(Out : PD3DXVector3; const V1, V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Subtract';
function D3DXVec3Subtract(out Out : TD3DXVector3; const V1, V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Subtract';

// Minimize each component.  x = min(x1, x2), y = min(y1, y2), ...
function D3DXVec3Minimize(Out, V1, V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Minimize';
function D3DXVec3Minimize(out Out : TD3DXVector3; V1, V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Minimize';
function D3DXVec3Minimize(Out : PD3DXVector3; const V1 : TD3DXVector3; V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Minimize';
function D3DXVec3Minimize(out Out : TD3DXVector3; const V1 : TD3DXVector3; V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Minimize';
function D3DXVec3Minimize(Out, V1 : PD3DXVector3; const V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Minimize';
function D3DXVec3Minimize(out Out : TD3DXVector3; V1 : PD3DXVector3; const V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Minimize';
function D3DXVec3Minimize(Out : PD3DXVector3; const V1, V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Minimize';
function D3DXVec3Minimize(out Out : TD3DXVector3; const V1, V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Minimize';

// Maximize each component.  x = max(x1, x2), y = max(y1, y2), ...
function D3DXVec3Maximize(Out, V1, V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Maximize';
function D3DXVec3Maximize(out Out : TD3DXVector3; V1, V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Maximize';
function D3DXVec3Maximize(Out : PD3DXVector3; const V1 : TD3DXVector3; V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Maximize';
function D3DXVec3Maximize(out Out : TD3DXVector3; const V1 : TD3DXVector3; V2 : PD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Maximize';
function D3DXVec3Maximize(Out, V1 : PD3DXVector3; const V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Maximize';
function D3DXVec3Maximize(out Out : TD3DXVector3; V1 : PD3DXVector3; const V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Maximize';
function D3DXVec3Maximize(Out : PD3DXVector3; const V1, V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Maximize';
function D3DXVec3Maximize(out Out : TD3DXVector3; const V1, V2 : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Maximize';

function D3DXVec3Scale(Out, V : PD3DXVector3; const s : Single) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Scale';
function D3DXVec3Scale(out Out : TD3DXVector3; V : PD3DXVector3; const s : Single) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Scale';
function D3DXVec3Scale(Out : PD3DXVector3; const V : TD3DXVector3; const s : Single) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Scale';
function D3DXVec3Scale(out Out : TD3DXVector3; const V : TD3DXVector3; const s : Single) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Scale';

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec3Lerp(Out, V1, V2 : PD3DXVector3; const s : Single) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Lerp';
function D3DXVec3Lerp(out Out : TD3DXVector3; V1, V2 : PD3DXVector3; const s : Single) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Lerp';
function D3DXVec3Lerp(Out : PD3DXVector3; const V1 : TD3DXVector3; V2 : PD3DXVector3; const s : Single) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Lerp';
function D3DXVec3Lerp(out Out : TD3DXVector3; const V1 : TD3DXVector3; V2 : PD3DXVector3; const s : Single) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Lerp';
function D3DXVec3Lerp(Out, V1 : PD3DXVector3; const V2 : TD3DXVector3; const s : Single) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Lerp';
function D3DXVec3Lerp(out Out : TD3DXVector3; V1 : PD3DXVector3; const V2 : TD3DXVector3; const s : Single) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Lerp';
function D3DXVec3Lerp(Out : PD3DXVector3; const V1, V2 : TD3DXVector3; const s : Single) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Lerp';
function D3DXVec3Lerp(out Out : TD3DXVector3; const V1, V2 : TD3DXVector3; const s : Single) : PD3DXVector3; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec3Lerp';

// non-inline
function D3DXVec3Normalize(out Out : TD3DXVector3; const V : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Normalize(Out : PD3DXVector3; const V : TD3DXVector3) : PD3DXVector3; stdcall; overload; external d3dx9dllname;

// Hermite interpolation between position V1, tangent T1 (when s == 0)
// and position V2, tangent T2 (when s == 1).
function D3DXVec3Hermite(Out, V1, T1, V2, T2 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(out Out : TD3DXVector3; V1, T1, V2, T2 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(Out : PD3DXVector3; const V1 : TD3DXVector3; T1, V2, T2 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(out Out : TD3DXVector3; const V1 : TD3DXVector3; T1, V2, T2 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(Out, V1 : PD3DXVector3; const T1 : TD3DXVector3; V2, T2 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(out Out : TD3DXVector3; V1 : PD3DXVector3; const T1 : TD3DXVector3; V2, T2 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(Out : PD3DXVector3; const V1, T1 : TD3DXVector3; V2, T2 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(out Out : TD3DXVector3; const V1, T1 : TD3DXVector3; V2, T2 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(Out, V1, T1 : PD3DXVector3; const V2 : TD3DXVector3; T2 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(out Out : TD3DXVector3; V1, T1 : PD3DXVector3; const V2 : TD3DXVector3; T2 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(Out : PD3DXVector3; const V1 : TD3DXVector3; T1 : PD3DXVector3; const V2 : TD3DXVector3; T2 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(out Out : TD3DXVector3; const V1 : TD3DXVector3; T1 : PD3DXVector3; const V2 : TD3DXVector3; T2 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(Out, V1 : PD3DXVector3; const T1, V2 : TD3DXVector3; T2 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(out Out : TD3DXVector3; V1 : PD3DXVector3; const T1, V2 : TD3DXVector3; T2 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(Out : PD3DXVector3; const V1, T1, V2 : TD3DXVector3; T2 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(out Out : TD3DXVector3; const V1, T1, V2 : TD3DXVector3; T2 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(Out, V1, T1, V2 : PD3DXVector3; const T2 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(out Out : TD3DXVector3; V1, T1, V2 : PD3DXVector3; const T2 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(Out : PD3DXVector3; const V1 : TD3DXVector3; T1, V2 : PD3DXVector3; const T2 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(out Out : TD3DXVector3; const V1 : TD3DXVector3; T1, V2 : PD3DXVector3; const T2 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(Out, V1 : PD3DXVector3; const T1 : TD3DXVector3; V2 : PD3DXVector3; const T2 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(out Out : TD3DXVector3; V1 : PD3DXVector3; const T1 : TD3DXVector3; V2 : PD3DXVector3; const T2 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(Out : PD3DXVector3; const V1, T1 : TD3DXVector3; V2 : PD3DXVector3; const T2 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(out Out : TD3DXVector3; const V1, T1 : TD3DXVector3; V2 : PD3DXVector3; const T2 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(Out, V1, T1 : PD3DXVector3; const V2, T2 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(out Out : TD3DXVector3; V1, T1 : PD3DXVector3; const V2, T2 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(Out : PD3DXVector3; const V1 : TD3DXVector3; T1 : PD3DXVector3; const V2, T2 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(out Out : TD3DXVector3; const V1 : TD3DXVector3; T1 : PD3DXVector3; const V2, T2 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(Out, V1 : PD3DXVector3; const T1, V2, T2 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(out Out : TD3DXVector3; V1 : PD3DXVector3; const T1, V2, T2 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(Out : PD3DXVector3; const V1, T1, V2, T2 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Hermite(out Out : TD3DXVector3; const V1, T1, V2, T2 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;

// CatmullRom interpolation between V1 (when s == 0) and V2 (when s == 1)
function D3DXVec3CatmullRom(Out, V0, V1, V2, V3 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(out Out : TD3DXVector3; V0, V1, V2, V3 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(Out : PD3DXVector3; const V0 : TD3DXVector3; V1, V2, V3 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(out Out : TD3DXVector3; const V0 : TD3DXVector3; V1, V2, V3 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(Out, V0 : PD3DXVector3; const V1 : TD3DXVector3; V2, V3 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(out Out : TD3DXVector3; V0 : PD3DXVector3; const V1 : TD3DXVector3; V2, V3 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(Out : PD3DXVector3; const V0, V1 : TD3DXVector3; V2, V3 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(out Out : TD3DXVector3; const V0, V1 : TD3DXVector3; V2, V3 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(Out, V0, V1 : PD3DXVector3; const V2 : TD3DXVector3; V3 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(out Out : TD3DXVector3; V0, V1 : PD3DXVector3; const V2 : TD3DXVector3; V3 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(Out : PD3DXVector3; const V0 : TD3DXVector3; V1 : PD3DXVector3; const V2 : TD3DXVector3; V3 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(out Out : TD3DXVector3; const V0 : TD3DXVector3; V1 : PD3DXVector3; const V2 : TD3DXVector3; V3 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(Out, V0 : PD3DXVector3; const V1, V2 : TD3DXVector3; V3 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(out Out : TD3DXVector3; V0 : PD3DXVector3; const V1, V2 : TD3DXVector3; V3 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(Out : PD3DXVector3; const V0, V1, V2 : TD3DXVector3; V3 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(out Out : TD3DXVector3; const V0, V1, V2 : TD3DXVector3; V3 : PD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(Out, V0, V1, V2 : PD3DXVector3; const V3 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(out Out : TD3DXVector3; V0, V1, V2 : PD3DXVector3; const V3 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(Out : PD3DXVector3; const V0 : TD3DXVector3; V1, V2 : PD3DXVector3; const V3 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(out Out : TD3DXVector3; const V0 : TD3DXVector3; V1, V2 : PD3DXVector3; const V3 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(Out, V0 : PD3DXVector3; const V1 : TD3DXVector3; V2 : PD3DXVector3; const V3 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(out Out : TD3DXVector3; V0 : PD3DXVector3; const V1 : TD3DXVector3; V2 : PD3DXVector3; const V3 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(Out : PD3DXVector3; const V0, V1 : TD3DXVector3; V2 : PD3DXVector3; const V3 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(out Out : TD3DXVector3; const V0, V1 : TD3DXVector3; V2 : PD3DXVector3; const V3 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(Out, V0, V1 : PD3DXVector3; const V2, V3 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(out Out : TD3DXVector3; V0, V1 : PD3DXVector3; const V2, V3 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(Out : PD3DXVector3; const V0 : TD3DXVector3; V1 : PD3DXVector3; const V2, V3 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(out Out : TD3DXVector3; const V0 : TD3DXVector3; V1 : PD3DXVector3; const V2, V3 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(Out, V0 : PD3DXVector3; const V1, V2, V3 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(out Out : TD3DXVector3; V0 : PD3DXVector3; const V1, V2, V3 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(Out : PD3DXVector3; const V0, V1, V2, V3 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3CatmullRom(out Out : TD3DXVector3; const V0, V1, V2, V3 : TD3DXVector3; s : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;

// Barycentric coordinates.  V1 + f(V2-V1) + g(V3-V1)
function D3DXVec3BaryCentric(Out, V1, V2, V3 : PD3DXVector3; f, g : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3BaryCentric(out Out : TD3DXVector3; V1, V2, V3 : PD3DXVector3; f, g : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3BaryCentric(Out : PD3DXVector3; const V1 : TD3DXVector3; V2, V3 : PD3DXVector3; f, g : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3BaryCentric(out Out : TD3DXVector3; const V1 : TD3DXVector3; V2, V3 : PD3DXVector3; f, g : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3BaryCentric(Out, V1 : PD3DXVector3; const V2 : TD3DXVector3; V3 : PD3DXVector3; f, g : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3BaryCentric(out Out : TD3DXVector3; V1 : PD3DXVector3; const V2 : TD3DXVector3; V3 : PD3DXVector3; f, g : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3BaryCentric(Out : PD3DXVector3; const V1, V2 : TD3DXVector3; V3 : PD3DXVector3; f, g : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3BaryCentric(out Out : TD3DXVector3; const V1, V2 : TD3DXVector3; V3 : PD3DXVector3; f, g : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3BaryCentric(Out, V1, V2 : PD3DXVector3; const V3 : TD3DXVector3; f, g : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3BaryCentric(out Out : TD3DXVector3; V1, V2 : PD3DXVector3; const V3 : TD3DXVector3; f, g : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3BaryCentric(Out : PD3DXVector3; const V1 : TD3DXVector3; V2 : PD3DXVector3; const V3 : TD3DXVector3; f, g : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3BaryCentric(out Out : TD3DXVector3; const V1 : TD3DXVector3; V2 : PD3DXVector3; const V3 : TD3DXVector3; f, g : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3BaryCentric(Out, V1 : PD3DXVector3; const V2, V3 : TD3DXVector3; f, g : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3BaryCentric(out Out : TD3DXVector3; V1 : PD3DXVector3; const V2, V3 : TD3DXVector3; f, g : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3BaryCentric(Out : PD3DXVector3; const V1, V2, V3 : TD3DXVector3; f, g : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3BaryCentric(out Out : TD3DXVector3; const V1, V2, V3 : TD3DXVector3; f, g : Single) : PD3DXVector3; stdcall; overload; external d3dx9dllname;

// Transform (x, y, z, 1) by matrix.
function D3DXVec3Transform(Out : PD3DXVector4; V : PD3DXVector3; M : PD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec3Transform(out Out : TD3DXVector4; V : PD3DXVector3; M : PD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec3Transform(Out : PD3DXVector4; const V : TD3DXVector3; M : PD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec3Transform(out Out : TD3DXVector4; const V : TD3DXVector3; M : PD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec3Transform(Out : PD3DXVector4; V : PD3DXVector3; const M : TD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec3Transform(out Out : TD3DXVector4; V : PD3DXVector3; const M : TD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec3Transform(Out : PD3DXVector4; const V : TD3DXVector3; const M : TD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec3Transform(out Out : TD3DXVector4; const V : TD3DXVector3; const M : TD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;


// Transform (x, y, z, 1) by matrix, project result back into w=1.
function D3DXVec3TransformCoord(Out, V : PD3DXVector3; M : PD3DXMatrix) : PD3DXVector3; stdcall; overload; overload; external d3dx9dllname;
function D3DXVec3TransformCoord(out Out : TD3DXVector3; V : PD3DXVector3; M : PD3DXMatrix) : PD3DXVector3; stdcall; overload; overload; external d3dx9dllname;
function D3DXVec3TransformCoord(Out : PD3DXVector3; const V : TD3DXVector3; M : PD3DXMatrix) : PD3DXVector3; stdcall; overload; overload; external d3dx9dllname;
function D3DXVec3TransformCoord(out Out : TD3DXVector3; const V : TD3DXVector3; M : PD3DXMatrix) : PD3DXVector3; stdcall; overload; overload; external d3dx9dllname;
function D3DXVec3TransformCoord(Out, V : PD3DXVector3; const M : TD3DXMatrix) : PD3DXVector3; stdcall; overload; overload; external d3dx9dllname;
function D3DXVec3TransformCoord(out Out : TD3DXVector3; V : PD3DXVector3; const M : TD3DXMatrix) : PD3DXVector3; stdcall; overload; overload; external d3dx9dllname;
function D3DXVec3TransformCoord(Out : PD3DXVector3; const V : TD3DXVector3; const M : TD3DXMatrix) : PD3DXVector3; stdcall; overload; overload; external d3dx9dllname;
function D3DXVec3TransformCoord(out Out : TD3DXVector3; const V : TD3DXVector3; const M : TD3DXMatrix) : PD3DXVector3; stdcall; overload; overload; external d3dx9dllname;

// Transform (x, y, z, 0) by matrix.  If you transforming a normal by a
// non-affine matrix, the matrix you pass to this function should be the
// transpose of the inverse of the matrix you would use to transform a coord.
function D3DXVec3TransformNormal(Out, V : PD3DXVector3; M : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3TransformNormal(out Out : TD3DXVector3; V : PD3DXVector3; M : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3TransformNormal(Out : PD3DXVector3; const V : TD3DXVector3; M : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3TransformNormal(out Out : TD3DXVector3; const V : TD3DXVector3; M : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3TransformNormal(Out, V : PD3DXVector3; const M : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3TransformNormal(out Out : TD3DXVector3; V : PD3DXVector3; const M : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3TransformNormal(Out : PD3DXVector3; const V : TD3DXVector3; const M : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3TransformNormal(out Out : TD3DXVector3; const V : TD3DXVector3; const M : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;


// Transform Array (x, y, z, 1) by matrix.
function D3DXVec3TransformArray(Out : PD3DXVector4; OutStride : Cardinal; V : PD3DXVector3; VStride : Cardinal; M : PD3DXMatrix; n : Cardinal) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec3TransformArray(Out : PD3DXVector4; OutStride : Cardinal; V : PD3DXVector3; VStride : Cardinal; const M : TD3DXMatrix; n : Cardinal) : PD3DXVector4; stdcall; overload; external d3dx9dllname;

// Transform Array (x, y, z, 1) by matrix, project result back into w=1.
function D3DXVec3TransformCoordArray(Out : PD3DXVector3; OutStride : Cardinal; V : PD3DXVector3; VStride : Cardinal; M : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3TransformCoordArray(Out : PD3DXVector3; OutStride : Cardinal; V : PD3DXVector3; VStride : Cardinal; const M : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;

// Transform (x, y, z, 0) by matrix.  If you transforming a normal by a
// non-affine matrix, the matrix you pass to this function should be the
// transpose of the inverse of the matrix you would use to transform a coord.
function D3DXVec3TransformNormalArray(Out : PD3DXVector3; OutStride : Cardinal; V : PD3DXVector3; VStride : Cardinal; M : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3TransformNormalArray(Out : PD3DXVector3; OutStride : Cardinal; V : PD3DXVector3; VStride : Cardinal; const M : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;

// Project vector from object space into screen space
function D3DXVec3Project(Out, V : PD3DXVector3; Viewport : PD3DViewPort9; Projection, View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; V : PD3DXVector3; Viewport : PD3DViewPort9; Projection, View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out : PD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; Projection, View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; Projection, View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out, V : PD3DXVector3; const Viewport : TD3DViewPort9; Projection, View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; V : PD3DXVector3; const Viewport : TD3DViewPort9; Projection, View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out : PD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; Projection, View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; Projection, View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out, V : PD3DXVector3; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; V : PD3DXVector3; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out : PD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out, V : PD3DXVector3; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; V : PD3DXVector3; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out : PD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out, V : PD3DXVector3; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; V : PD3DXVector3; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out : PD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out, V : PD3DXVector3; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; V : PD3DXVector3; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out : PD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out, V : PD3DXVector3; Viewport : PD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; V : PD3DXVector3; Viewport : PD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out : PD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out, V : PD3DXVector3; const Viewport : TD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; V : PD3DXVector3; const Viewport : TD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out : PD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out, V : PD3DXVector3; Viewport : PD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; V : PD3DXVector3; Viewport : PD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out : PD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out, V : PD3DXVector3; const Viewport : TD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; V : PD3DXVector3; const Viewport : TD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out : PD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out, V : PD3DXVector3; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; V : PD3DXVector3; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out : PD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out, V : PD3DXVector3; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; V : PD3DXVector3; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out : PD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out, V : PD3DXVector3; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; V : PD3DXVector3; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out : PD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out, V : PD3DXVector3; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; V : PD3DXVector3; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out : PD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out, V : PD3DXVector3; Viewport : PD3DViewPort9; const Projection, View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; V : PD3DXVector3; Viewport : PD3DViewPort9; const Projection, View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out : PD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; const Projection, View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; const Projection, View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out, V : PD3DXVector3; const Viewport : TD3DViewPort9; const Projection, View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; V : PD3DXVector3; const Viewport : TD3DViewPort9; const Projection, View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(Out : PD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; const Projection, View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Project(out Out : TD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; const Projection, View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;

// Project vector from screen space into object space
function D3DXVec3Unproject(Out, V : PD3DXVector3; Viewport : PD3DViewPort9; Projection, View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; V : PD3DXVector3; Viewport : PD3DViewPort9; Projection, View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out : PD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; Projection, View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; Projection, View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out, V : PD3DXVector3; const Viewport : TD3DViewPort9; Projection, View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; V : PD3DXVector3; const Viewport : TD3DViewPort9; Projection, View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out : PD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; Projection, View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; Projection, View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out, V : PD3DXVector3; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; V : PD3DXVector3; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out : PD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out, V : PD3DXVector3; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; V : PD3DXVector3; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out : PD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out, V : PD3DXVector3; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; V : PD3DXVector3; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out : PD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out, V : PD3DXVector3; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; V : PD3DXVector3; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out : PD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out, V : PD3DXVector3; Viewport : PD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; V : PD3DXVector3; Viewport : PD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out : PD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out, V : PD3DXVector3; const Viewport : TD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; V : PD3DXVector3; const Viewport : TD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out : PD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out, V : PD3DXVector3; Viewport : PD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; V : PD3DXVector3; Viewport : PD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out : PD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out, V : PD3DXVector3; const Viewport : TD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; V : PD3DXVector3; const Viewport : TD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out : PD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out, V : PD3DXVector3; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; V : PD3DXVector3; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out : PD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out, V : PD3DXVector3; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; V : PD3DXVector3; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out : PD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out, V : PD3DXVector3; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; V : PD3DXVector3; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out : PD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out, V : PD3DXVector3; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; V : PD3DXVector3; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out : PD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out, V : PD3DXVector3; Viewport : PD3DViewPort9; const Projection, View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; V : PD3DXVector3; Viewport : PD3DViewPort9; const Projection, View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out : PD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; const Projection, View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; const V : TD3DXVector3; Viewport : PD3DViewPort9; const Projection, View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out, V : PD3DXVector3; const Viewport : TD3DViewPort9; const Projection, View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; V : PD3DXVector3; const Viewport : TD3DViewPort9; const Projection, View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(Out : PD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; const Projection, View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3Unproject(out Out : TD3DXVector3; const V : TD3DXVector3; const Viewport : TD3DViewPort9; const Projection, View, World : TD3DXMatrix) : PD3DXVector3; stdcall; overload; external d3dx9dllname;

// Project vector Array from object space into screen space
function D3DXVec3ProjectArray(Out, V : PD3DXVector3; VStride : Cardinal; Viewport : PD3DViewPort9; Projection, View, World : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3ProjectArray(Out, V : PD3DXVector3; VStride : Cardinal; const Viewport : TD3DViewPort9; Projection, View, World : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3ProjectArray(Out, V : PD3DXVector3; VStride : Cardinal; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3ProjectArray(Out, V : PD3DXVector3; VStride : Cardinal; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3ProjectArray(Out, V : PD3DXVector3; VStride : Cardinal; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3ProjectArray(Out, V : PD3DXVector3; VStride : Cardinal; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3ProjectArray(Out, V : PD3DXVector3; VStride : Cardinal; Viewport : PD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3ProjectArray(Out, V : PD3DXVector3; VStride : Cardinal; const Viewport : TD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3ProjectArray(Out, V : PD3DXVector3; VStride : Cardinal; Viewport : PD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3ProjectArray(Out, V : PD3DXVector3; VStride : Cardinal; const Viewport : TD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3ProjectArray(Out, V : PD3DXVector3; VStride : Cardinal; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3ProjectArray(Out, V : PD3DXVector3; VStride : Cardinal; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3ProjectArray(Out, V : PD3DXVector3; VStride : Cardinal; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3ProjectArray(Out, V : PD3DXVector3; VStride : Cardinal; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3ProjectArray(Out, V : PD3DXVector3; VStride : Cardinal; Viewport : PD3DViewPort9; const Projection, View, World : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3ProjectArray(Out, V : PD3DXVector3; VStride : Cardinal; const Viewport : TD3DViewPort9; const Projection, View, World : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;

// Project vector Array from screen space into object space
function D3DXVec3UnprojectArray(Out, V : PD3DXVector3; VStride : Cardinal; Viewport : PD3DViewPort9; Projection, View, World : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3UnprojectArray(Out, V : PD3DXVector3; VStride : Cardinal; const Viewport : TD3DViewPort9; Projection, View, World : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3UnprojectArray(Out, V : PD3DXVector3; VStride : Cardinal; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3UnprojectArray(Out, V : PD3DXVector3; VStride : Cardinal; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View, World : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3UnprojectArray(Out, V : PD3DXVector3; VStride : Cardinal; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3UnprojectArray(Out, V : PD3DXVector3; VStride : Cardinal; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View : TD3DXMatrix; World : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3UnprojectArray(Out, V : PD3DXVector3; VStride : Cardinal; Viewport : PD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3UnprojectArray(Out, V : PD3DXVector3; VStride : Cardinal; const Viewport : TD3DViewPort9; const Projection, View : TD3DXMatrix; World : PD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3UnprojectArray(Out, V : PD3DXVector3; VStride : Cardinal; Viewport : PD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3UnprojectArray(Out, V : PD3DXVector3; VStride : Cardinal; const Viewport : TD3DViewPort9; Projection, View : PD3DXMatrix; const World : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3UnprojectArray(Out, V : PD3DXVector3; VStride : Cardinal; Viewport : PD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3UnprojectArray(Out, V : PD3DXVector3; VStride : Cardinal; const Viewport : TD3DViewPort9; const Projection : TD3DXMatrix; View : PD3DXMatrix; const World : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3UnprojectArray(Out, V : PD3DXVector3; VStride : Cardinal; Viewport : PD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3UnprojectArray(Out, V : PD3DXVector3; VStride : Cardinal; const Viewport : TD3DViewPort9; Projection : PD3DXMatrix; const View, World : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3UnprojectArray(Out, V : PD3DXVector3; VStride : Cardinal; Viewport : PD3DViewPort9; const Projection, View, World : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
function D3DXVec3UnprojectArray(Out, V : PD3DXVector3; VStride : Cardinal; const Viewport : TD3DViewPort9; const Projection, View, World : TD3DXMatrix; n : Cardinal) : PD3DXVector3; stdcall; overload; external d3dx9dllname;
      
(*)
 *******************************************************************************
 * 4D Vector
 *******************************************************************************
(*)

// inline, Delphi uses dll

function D3DXVec4Length(V : PD3DXVector4) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Length';
function D3DXVec4Length(const V : TD3DXVector4) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Length';

function D3DXVec4LengthSq(V : PD3DXVector4) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4LengthSq';
function D3DXVec4LengthSq(const V : TD3DXVector4) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4LengthSq';

function D3DXVec4Dot(V1, V2 : PD3DXVector4) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Dot';
function D3DXVec4Dot(const V1 : TD3DXVector4; V2 : PD3DXVector4) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Dot';
function D3DXVec4Dot(V1 : PD3DXVector4; const V2 : TD3DXVector4) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Dot';
function D3DXVec4Dot(const V1, V2 : TD3DXVector4) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Dot';

function D3DXVec4Add(Out, V1, V2 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname name 'D3DXVec4Add';
function D3DXVec4Add(out Out : TD3DXVector4; V1, V2 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Add';
function D3DXVec4Add(Out : PD3DXVector4; const V1 : TD3DXVector4; V2 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Add';
function D3DXVec4Add(out Out : TD3DXVector4; const V1 : TD3DXVector4; V2 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Add';
function D3DXVec4Add(Out, V1 : PD3DXVector4; const V2 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Add';
function D3DXVec4Add(out Out : TD3DXVector4; V1 : PD3DXVector4; const V2 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Add';
function D3DXVec4Add(Out : PD3DXVector4; const V1, V2 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Add';
function D3DXVec4Add(out Out : TD3DXVector4; const V1, V2 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Add';

function D3DXVec4Subtract(Out, V1, V2 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Subtract';
function D3DXVec4Subtract(out Out : TD3DXVector4; V1, V2 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Subtract';
function D3DXVec4Subtract(Out : PD3DXVector4; const V1 : TD3DXVector4; V2 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Subtract';
function D3DXVec4Subtract(out Out : TD3DXVector4; const V1 : TD3DXVector4; V2 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Subtract';
function D3DXVec4Subtract(Out, V1 : PD3DXVector4; const V2 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Subtract';
function D3DXVec4Subtract(out Out : TD3DXVector4; V1 : PD3DXVector4; const V2 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Subtract';
function D3DXVec4Subtract(Out : PD3DXVector4; const V1, V2 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Subtract';
function D3DXVec4Subtract(out Out : TD3DXVector4; const V1, V2 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Subtract';

// Minimize each component.  x = min(x1, x2), y = min(y1, y2), ...
function D3DXVec4Minimize(Out, V1, V2 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Minimize';
function D3DXVec4Minimize(out Out : TD3DXVector4; V1, V2 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Minimize';
function D3DXVec4Minimize(Out : PD3DXVector4; const V1 : TD3DXVector4; V2 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Minimize';
function D3DXVec4Minimize(out Out : TD3DXVector4; const V1 : TD3DXVector4; V2 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Minimize';
function D3DXVec4Minimize(Out, V1 : PD3DXVector4; const V2 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Minimize';
function D3DXVec4Minimize(out Out : TD3DXVector4; V1 : PD3DXVector4; const V2 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Minimize';
function D3DXVec4Minimize(Out : PD3DXVector4; const V1, V2 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Minimize';
function D3DXVec4Minimize(out Out : TD3DXVector4; const V1, V2 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Minimize';

// Maximize each component.  x = max(x1, x2), y = max(y1, y2), ...
function D3DXVec4Maximize(Out, V1, V2 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Maximize';
function D3DXVec4Maximize(out Out : TD3DXVector4; V1, V2 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Maximize';
function D3DXVec4Maximize(Out : PD3DXVector4; const V1 : TD3DXVector4; V2 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Maximize';
function D3DXVec4Maximize(out Out : TD3DXVector4; const V1 : TD3DXVector4; V2 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Maximize';
function D3DXVec4Maximize(Out, V1 : PD3DXVector4; const V2 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Maximize';
function D3DXVec4Maximize(out Out : TD3DXVector4; V1 : PD3DXVector4; const V2 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Maximize';
function D3DXVec4Maximize(Out : PD3DXVector4; const V1, V2 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Maximize';
function D3DXVec4Maximize(out Out : TD3DXVector4; const V1, V2 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Maximize';

function D3DXVec4Scale(Out, V : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Scale';
function D3DXVec4Scale(out Out : TD3DXVector4; V : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Scale';
function D3DXVec4Scale(Out : PD3DXVector4; const V : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Scale';
function D3DXVec4Scale(out Out : TD3DXVector4; const V : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Scale';

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec4Lerp(Out, V1, V2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Lerp';
function D3DXVec4Lerp(out Out : TD3DXVector4; V1, V2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Lerp';
function D3DXVec4Lerp(Out : PD3DXVector4; const V1 : TD3DXVector4; V2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Lerp';
function D3DXVec4Lerp(out Out : TD3DXVector4; const V1 : TD3DXVector4; V2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Lerp';
function D3DXVec4Lerp(Out, V1 : PD3DXVector4; const V2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Lerp';
function D3DXVec4Lerp(out Out : TD3DXVector4; V1 : PD3DXVector4; const V2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Lerp';
function D3DXVec4Lerp(Out : PD3DXVector4; const V1, V2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Lerp';
function D3DXVec4Lerp(out Out : TD3DXVector4; const V1, V2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9inldllname name 'INLD3DXVec4Lerp';

// non-inline

// Cross-product in 4 dimensions.
function D3DXVec4Cross(Out, V1, V2, V3 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Cross(out Out : TD3DXVector4; V1, V2, V3 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Cross(Out : PD3DXVector4; const V1 : TD3DXVector4; V2, V3 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Cross(out Out : TD3DXVector4; const V1 : TD3DXVector4; V2, V3 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Cross(Out, V1 : PD3DXVector4; const V2 : TD3DXVector4; V3 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Cross(out Out : TD3DXVector4; V1 : PD3DXVector4; const V2 : TD3DXVector4; V3 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Cross(Out : PD3DXVector4; const V1, V2 : TD3DXVector4; V3 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Cross(out Out : TD3DXVector4; const V1, V2 : TD3DXVector4; V3 : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Cross(Out, V1, V2 : PD3DXVector4; const V3 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Cross(out Out : TD3DXVector4; V1, V2 : PD3DXVector4; const V3 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Cross(Out : PD3DXVector4; const V1 : TD3DXVector4; V2 : PD3DXVector4; const V3 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Cross(out Out : TD3DXVector4; const V1 : TD3DXVector4; V2 : PD3DXVector4; const V3 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Cross(Out, V1 : PD3DXVector4; const V2, V3 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Cross(out Out : TD3DXVector4; V1 : PD3DXVector4; const V2, V3 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Cross(Out : PD3DXVector4; const V1, V2, V3 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Cross(out Out : TD3DXVector4; const V1, V2, V3 : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;

function D3DXVec4Normalize(Out, V : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Normalize(out Out : TD3DXVector4; V : PD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Normalize(Out : PD3DXVector4; const V : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Normalize(out Out : TD3DXVector4; const V : TD3DXVector4) : PD3DXVector4; stdcall; overload; external d3dx9dllname;

// Hermite interpolation between position V1, tangent T1 (when s == 0)
// and position V2, tangent T2 (when s == 1).
function D3DXVec4Hermite(Out, V1, T1, V2, T2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(out Out : TD3DXVector4; V1, T1, V2, T2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(Out : PD3DXVector4; const V1 : TD3DXVector4; T1, V2, T2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(out Out : TD3DXVector4; const V1 : TD3DXVector4; T1, V2, T2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(Out, V1 : PD3DXVector4; const T1 : TD3DXVector4; V2, T2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(out Out : TD3DXVector4; V1 : PD3DXVector4; const T1 : TD3DXVector4; V2, T2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(Out : PD3DXVector4; const V1, T1 : TD3DXVector4; V2, T2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(out Out : TD3DXVector4; const V1, T1 : TD3DXVector4; V2, T2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(Out, V1, T1 : PD3DXVector4; const V2 : TD3DXVector4; T2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(out Out : TD3DXVector4; V1, T1 : PD3DXVector4; const V2 : TD3DXVector4; T2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(Out : PD3DXVector4; const V1 : TD3DXVector4; T1 : PD3DXVector4; const V2 : TD3DXVector4; T2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(out Out : TD3DXVector4; const V1 : TD3DXVector4; T1 : PD3DXVector4; const V2 : TD3DXVector4; T2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(Out, V1 : PD3DXVector4; const T1, V2 : TD3DXVector4; T2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(out Out : TD3DXVector4; V1 : PD3DXVector4; const T1, V2 : TD3DXVector4; T2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(Out : PD3DXVector4; const V1, T1, V2 : TD3DXVector4; T2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(out Out : TD3DXVector4; const V1, T1, V2 : TD3DXVector4; T2 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(Out, V1, T1, V2 : PD3DXVector4; const T2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(out Out : TD3DXVector4; V1, T1, V2 : PD3DXVector4; const T2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(Out : PD3DXVector4; const V1 : TD3DXVector4; T1, V2 : PD3DXVector4; const T2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(out Out : TD3DXVector4; const V1 : TD3DXVector4; T1, V2 : PD3DXVector4; const T2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(Out, V1 : PD3DXVector4; const T1 : TD3DXVector4; V2 : PD3DXVector4; const T2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(out Out : TD3DXVector4; V1 : PD3DXVector4; const T1 : TD3DXVector4; V2 : PD3DXVector4; const T2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(Out : PD3DXVector4; const V1, T1 : TD3DXVector4; V2 : PD3DXVector4; const T2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(out Out : TD3DXVector4; const V1, T1 : TD3DXVector4; V2 : PD3DXVector4; const T2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(Out, V1, T1 : PD3DXVector4; const V2, T2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(out Out : TD3DXVector4; V1, T1 : PD3DXVector4; const V2, T2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(Out : PD3DXVector4; const V1 : TD3DXVector4; T1 : PD3DXVector4; const V2, T2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(out Out : TD3DXVector4; const V1 : TD3DXVector4; T1 : PD3DXVector4; const V2, T2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(Out, V1 : PD3DXVector4; const T1, V2, T2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(out Out : TD3DXVector4; V1 : PD3DXVector4; const T1, V2, T2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(Out : PD3DXVector4; const V1, T1, V2, T2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Hermite(out Out : TD3DXVector4; const V1, T1, V2, T2 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;

// CatmullRom interpolation between V1 (when s == 0) and V2 (when s == 1)
function D3DXVec4CatmullRom(Out, V0, V1, V2, V3 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(out Out : TD3DXVector4; V0, V1, V2, V3 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(Out : PD3DXVector4; const V0 : TD3DXVector4; V1, V2, V3 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(out Out : TD3DXVector4; const V0 : TD3DXVector4; V1, V2, V3 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(Out, V0 : PD3DXVector4; const V1 : TD3DXVector4; V2, V3 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(out Out : TD3DXVector4; V0 : PD3DXVector4; const V1 : TD3DXVector4; V2, V3 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(Out : PD3DXVector4; const V0, V1 : TD3DXVector4; V2, V3 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(out Out : TD3DXVector4; const V0, V1 : TD3DXVector4; V2, V3 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(Out, V0, V1 : PD3DXVector4; const V2 : TD3DXVector4; V3 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(out Out : TD3DXVector4; V0, V1 : PD3DXVector4; const V2 : TD3DXVector4; V3 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(Out : PD3DXVector4; const V0 : TD3DXVector4; V1 : PD3DXVector4; const V2 : TD3DXVector4; V3 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(out Out : TD3DXVector4; const V0 : TD3DXVector4; V1 : PD3DXVector4; const V2 : TD3DXVector4; V3 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(Out, V0 : PD3DXVector4; const V1, V2 : TD3DXVector4; V3 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(out Out : TD3DXVector4; V0 : PD3DXVector4; const V1, V2 : TD3DXVector4; V3 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(Out : PD3DXVector4; const V0, V1, V2 : TD3DXVector4; V3 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(out Out : TD3DXVector4; const V0, V1, V2 : TD3DXVector4; V3 : PD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(Out, V0, V1, V2 : PD3DXVector4; const V3 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(out Out : TD3DXVector4; V0, V1, V2 : PD3DXVector4; const V3 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(Out : PD3DXVector4; const V0 : TD3DXVector4; V1, V2 : PD3DXVector4; const V3 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(out Out : TD3DXVector4; const V0 : TD3DXVector4; V1, V2 : PD3DXVector4; const V3 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(Out, V0 : PD3DXVector4; const V1 : TD3DXVector4; V2 : PD3DXVector4; const V3 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(out Out : TD3DXVector4; V0 : PD3DXVector4; const V1 : TD3DXVector4; V2 : PD3DXVector4; const V3 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(Out : PD3DXVector4; const V0, V1 : TD3DXVector4; V2 : PD3DXVector4; const V3 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(out Out : TD3DXVector4; const V0, V1 : TD3DXVector4; V2 : PD3DXVector4; const V3 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(Out, V0, V1 : PD3DXVector4; const V2, V3 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(out Out : TD3DXVector4; V0, V1 : PD3DXVector4; const V2, V3 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(Out : PD3DXVector4; const V0 : TD3DXVector4; V1 : PD3DXVector4; const V2, V3 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(out Out : TD3DXVector4; const V0 : TD3DXVector4; V1 : PD3DXVector4; const V2, V3 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(Out, V0 : PD3DXVector4; const V1, V2, V3 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(out Out : TD3DXVector4; V0 : PD3DXVector4; const V1, V2, V3 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(Out : PD3DXVector4; const V0, V1, V2, V3 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4CatmullRom(out Out : TD3DXVector4; const V0, V1, V2, V3 : TD3DXVector4; s : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;

// Barycentric coordinates.  V1 + f(V2-V1) + g(V3-V1)
function D3DXVec4BaryCentric(Out, V1, V2, V3 : PD3DXVector4; f, g : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4BaryCentric(out Out : TD3DXVector4; V1, V2, V3 : PD3DXVector4; f, g : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4BaryCentric(Out : PD3DXVector4; const V1 : TD3DXVector4; V2, V3 : PD3DXVector4; f, g : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4BaryCentric(out Out : TD3DXVector4; const V1 : TD3DXVector4; V2, V3 : PD3DXVector4; f, g : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4BaryCentric(Out, V1 : PD3DXVector4; const V2 : TD3DXVector4; V3 : PD3DXVector4; f, g : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4BaryCentric(out Out : TD3DXVector4; V1 : PD3DXVector4; const V2 : TD3DXVector4; V3 : PD3DXVector4; f, g : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4BaryCentric(Out : PD3DXVector4; const V1, V2 : TD3DXVector4; V3 : PD3DXVector4; f, g : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4BaryCentric(out Out : TD3DXVector4; const V1, V2 : TD3DXVector4; V3 : PD3DXVector4; f, g : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4BaryCentric(Out, V1, V2 : PD3DXVector4; const V3 : TD3DXVector4; f, g : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4BaryCentric(out Out : TD3DXVector4; V1, V2 : PD3DXVector4; const V3 : TD3DXVector4; f, g : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4BaryCentric(Out : PD3DXVector4; const V1 : TD3DXVector4; V2 : PD3DXVector4; const V3 : TD3DXVector4; f, g : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4BaryCentric(out Out : TD3DXVector4; const V1 : TD3DXVector4; V2 : PD3DXVector4; const V3 : TD3DXVector4; f, g : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4BaryCentric(Out, V1 : PD3DXVector4; const V2, V3 : TD3DXVector4; f, g : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4BaryCentric(out Out : TD3DXVector4; V1 : PD3DXVector4; const V2, V3 : TD3DXVector4; f, g : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4BaryCentric(Out : PD3DXVector4; const V1, V2, V3 : TD3DXVector4; f, g : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4BaryCentric(out Out : TD3DXVector4; const V1, V2, V3 : TD3DXVector4; f, g : Single) : PD3DXVector4; stdcall; overload; external d3dx9dllname;

// Transform vector by matrix.
function D3DXVec4Transform(Out, V : PD3DXVector4; M : PD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Transform(out Out : TD3DXVector4; V : PD3DXVector4; M : PD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Transform(Out : PD3DXVector4; const V : TD3DXVector4; M : PD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Transform(out Out : TD3DXVector4; const V : TD3DXVector4; M : PD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Transform(Out, V : PD3DXVector4; const M : TD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Transform(out Out : TD3DXVector4; V : PD3DXVector4; const M : TD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Transform(Out : PD3DXVector4; const V : TD3DXVector4; const M : TD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4Transform(out Out : TD3DXVector4; const V : TD3DXVector4; const M : TD3DXMatrix) : PD3DXVector4; stdcall; overload; external d3dx9dllname;

// Transform vector array by matrix.
function D3DXVec4TransformArray(Out : PD3DXVector4; OutStride : Cardinal; V : PD3DXVector4; VStride : Cardinal; M : PD3DXMatrix; n : Cardinal) : PD3DXVector4; stdcall; overload; external d3dx9dllname;
function D3DXVec4TransformArray(Out : PD3DXVector4; OutStride : Cardinal; V : PD3DXVector4; VStride : Cardinal; const M : TD3DXMatrix; n : Cardinal) : PD3DXVector4; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * 4D Matrix
 *******************************************************************************
(*)

// inline, Delphi uses dll

function D3DXMatrixIsIdentity(M : PD3DXMatrix) : Bool; stdcall; overload; external d3dx9inldllname name 'INLD3DXMatrixIsIdentity';
function D3DXMatrixIsIdentity(const M : TD3DXMatrix) : Bool; stdcall; overload; external d3dx9inldllname name 'INLD3DXMatrixIsIdentity';

// non-inline

function D3DXMatrixDeterminant(const M : TD3DXMatrix) : Single; stdcall; overload; external d3dx9dllname;
function D3DXMatrixDeterminant(M : PD3DXMatrix) : Single; stdcall; overload; external d3dx9dllname;

function D3DXMatrixTranspose(Out, M : PD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTranspose(out Out : TD3DXMatrix; M : PD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTranspose(Out : PD3DXMatrix; const M : TD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTranspose(out Out : TD3DXMatrix; const M : TD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Matrix multiplication.  The result represents the transformation M2
// followed by the transformation M1.  (Out = M1 * M2)
function D3DXMatrixMultiply(Out, M1, M2 : PD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixMultiply(out Out : TD3DXMatrix; M1, M2 : PD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixMultiply(Out : PD3DXMatrix; const M1 : TD3DXMatrix; M2 : PD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixMultiply(out Out : TD3DXMatrix; const M1 : TD3DXMatrix; M2 : PD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixMultiply(Out, M1 : PD3DXMatrix; const M2 : TD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixMultiply(out Out : TD3DXMatrix; M1 : PD3DXMatrix; const M2 : TD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixMultiply(Out : PD3DXMatrix; const M1, M2 : TD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixMultiply(out Out : TD3DXMatrix; const M1, M2 : TD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Matrix multiplication, followed by a transpose. (Out = T(M1 * M2))
function D3DXMatrixMultiplyTranspose(Out, M1, M2 : PD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixMultiplyTranspose(out Out : TD3DXMatrix; M1, M2 : PD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixMultiplyTranspose(Out : PD3DXMatrix; var M1 : TD3DXMatrix; M2 : PD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixMultiplyTranspose(out Out : TD3DXMatrix; var M1 : TD3DXMatrix; M2 : PD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixMultiplyTranspose(Out, M1 : PD3DXMatrix; var M2 : TD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixMultiplyTranspose(out Out : TD3DXMatrix; M1 : PD3DXMatrix; var M2 : TD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixMultiplyTranspose(Out : PD3DXMatrix; var M1, M2 : TD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixMultiplyTranspose(out Out : TD3DXMatrix; var M1, M2 : TD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
    
// Calculate inverse of matrix.  Inversion my fail, in which case NULL will
// be returned.  The determinant of pM is also returned it pfDeterminant
// is non-NULL.
function D3DXMatrixInverse(Out : PD3DXMatrix; Determinant : PSingle; M : PD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixInverse(out Out : TD3DXMatrix; Determinant : PSingle; M : PD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixInverse(Out : PD3DXMatrix; var Determinant : Single; M : PD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixInverse(out Out : TD3DXMatrix; var Determinant : Single; M : PD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixInverse(Out : PD3DXMatrix; Determinant : PSingle; const M : TD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixInverse(out Out : TD3DXMatrix; Determinant : PSingle; const M : TD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixInverse(Out : PD3DXMatrix; var Determinant : Single; const M : TD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixInverse(out Out : TD3DXMatrix; var Determinant : Single; const M : TD3DXMatrix) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build a matrix which scales by (sx, sy, sz)
function D3DXMatrixScaling(out Out : TD3DXMatrix; sx, sy, sz : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixScaling(Out : PD3DXMatrix; sx, sy, sz : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixScaling(out Out : TD3DXMatrix; sxyz : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixScaling(Out : PD3DXMatrix; sxyz : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build a matrix which translates by (x, y, z)
function D3DXMatrixTranslation(Out : PD3DXMatrix; x, y, z : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTranslation(out Out : TD3DXMatrix; x, y, z : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTranslation(Out : PD3DXMatrix; xyz : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTranslation(out Out : TD3DXMatrix; xyz : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build a matrix which rotates around the X axis
function D3DXMatrixRotationX(Out : PD3DXMatrix; Angle : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixRotationX(out Out : TD3DXMatrix; Angle : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build a matrix which rotates around the Y axis
function D3DXMatrixRotationY(Out : PD3DXMatrix; Angle : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixRotationY(out Out : TD3DXMatrix; Angle : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;


// Build a matrix which rotates around the Z axis
function D3DXMatrixRotationZ(Out : PD3DXMatrix; Angle : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixRotationZ(out Out : TD3DXMatrix; Angle : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build a matrix which rotates around an arbitrary axis
function D3DXMatrixRotationAxis(Out : PD3DXMatrix; V : PD3DXVector3; Angle : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixRotationAxis(out Out : TD3DXMatrix; V : PD3DXVector3; Angle : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixRotationAxis(Out : PD3DXMatrix; const V : TD3DXVector3; Angle : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixRotationAxis(out Out : TD3DXMatrix; const V : TD3DXVector3; Angle : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build a matrix from a quaternion
function D3DXMatrixRotationQuaternion(Out : PD3DXMatrix; Q : PD3DXQuaternion) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixRotationQuaternion(out Out : TD3DXMatrix; Q : PD3DXQuaternion) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixRotationQuaternion(Out : PD3DXMatrix; const Q : TD3DXQuaternion) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixRotationQuaternion(out Out : TD3DXMatrix; const Q : TD3DXQuaternion) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Yaw around the Y axis, a pitch around the X axis,
// and a roll around the Z axis.
function D3DXMatrixRotationYawPitchRoll(Out : PD3DXMatrix; Yaw, Pitch, Roll : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixRotationYawPitchRoll(out Out : TD3DXMatrix; Yaw, Pitch, Roll : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixRotationYawPitchRoll(Out : PD3DXMatrix; YawPitchRoll : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixRotationYawPitchRoll(out Out : TD3DXMatrix; YawPitchRoll : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build transformation matrix.  NULL arguments are treated as identity.
// Mout = Msc-1 * Msr-1 * Ms * Msr * Msc * Mrc-1 * Mr * Mrc * Mt
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling, RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling : TD3DXVector3; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; Scaling : PD3DXVector3; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; ScalingRotation : PD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector3; const ScalingRotation : TD3DXQuaternion; const Scaling, RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build 2D transformation matrix in XY plane.  NULL arguments are treated as identity.
// Mout = Msc-1 * Msr-1 * Ms * Msr * Msc * Mrc-1 * Mr * Mrc * Mt
function D3DXMatrixTransformation2D(Out : PD3DXMatrix; ScalingCenter : PD3DXVector2; ScalingRotation : Single; Scaling, RotationCenter : PD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector2; ScalingRotation : Single; Scaling, RotationCenter : PD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector2; ScalingRotation : Single; Scaling, RotationCenter : PD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector2; ScalingRotation : Single; Scaling, RotationCenter : PD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(Out : PD3DXMatrix; ScalingCenter : PD3DXVector2; ScalingRotation : Single; const Scaling : TD3DXVector2; RotationCenter : PD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector2; ScalingRotation : Single; const Scaling : TD3DXVector2; RotationCenter : PD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector2; ScalingRotation : Single; const Scaling : TD3DXVector2; RotationCenter : PD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector2; ScalingRotation : Single; const Scaling : TD3DXVector2; RotationCenter : PD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(Out : PD3DXMatrix; ScalingCenter : PD3DXVector2; ScalingRotation : Single; Scaling : PD3DXVector2; const RotationCenter : TD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector2; ScalingRotation : Single; Scaling : PD3DXVector2; const RotationCenter : TD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector2; ScalingRotation : Single; Scaling : PD3DXVector2; const RotationCenter : TD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector2; ScalingRotation : Single; Scaling : PD3DXVector2; const RotationCenter : TD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(Out : PD3DXMatrix; ScalingCenter : PD3DXVector2; ScalingRotation : Single; const Scaling, RotationCenter : TD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector2; ScalingRotation : Single; const Scaling, RotationCenter : TD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector2; ScalingRotation : Single; const Scaling, RotationCenter : TD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector2; ScalingRotation : Single; const Scaling, RotationCenter : TD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(Out : PD3DXMatrix; ScalingCenter : PD3DXVector2; ScalingRotation : Single; Scaling, RotationCenter : PD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector2; ScalingRotation : Single; Scaling, RotationCenter : PD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector2; ScalingRotation : Single; Scaling, RotationCenter : PD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector2; ScalingRotation : Single; Scaling, RotationCenter : PD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(Out : PD3DXMatrix; ScalingCenter : PD3DXVector2; ScalingRotation : Single; const Scaling : TD3DXVector2; RotationCenter : PD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector2; ScalingRotation : Single; const Scaling : TD3DXVector2; RotationCenter : PD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector2; ScalingRotation : Single; const Scaling : TD3DXVector2; RotationCenter : PD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector2; ScalingRotation : Single; const Scaling : TD3DXVector2; RotationCenter : PD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(Out : PD3DXMatrix; ScalingCenter : PD3DXVector2; ScalingRotation : Single; Scaling : PD3DXVector2; const RotationCenter : TD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector2; ScalingRotation : Single; Scaling : PD3DXVector2; const RotationCenter : TD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector2; ScalingRotation : Single; Scaling : PD3DXVector2; const RotationCenter : TD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector2; ScalingRotation : Single; Scaling : PD3DXVector2; const RotationCenter : TD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(Out : PD3DXMatrix; ScalingCenter : PD3DXVector2; ScalingRotation : Single; const Scaling, RotationCenter : TD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(out Out : TD3DXMatrix; ScalingCenter : PD3DXVector2; ScalingRotation : Single; const Scaling, RotationCenter : TD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(Out : PD3DXMatrix; const ScalingCenter : TD3DXVector2; ScalingRotation : Single; const Scaling, RotationCenter : TD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixTransformation2D(out Out : TD3DXMatrix; const ScalingCenter : TD3DXVector2; ScalingRotation : Single; const Scaling, RotationCenter : TD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;


// Build affine transformation matrix.  NULL arguments are treated as identity.
// Mout = Ms * Mrc-1 * Mr * Mrc * Mt
function D3DXMatrixAffineTransformation(Out : PD3DXMatrix; Scaling : Single; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation(out Out : TD3DXMatrix; Scaling : Single; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation(Out : PD3DXMatrix; Scaling : Single; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation(out Out : TD3DXMatrix; Scaling : Single; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation(Out : PD3DXMatrix; Scaling : Single; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation(out Out : TD3DXMatrix; Scaling : Single; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation(Out : PD3DXMatrix; Scaling : Single; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation(out Out : TD3DXMatrix; Scaling : Single; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; Translation : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation(Out : PD3DXMatrix; Scaling : Single; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation(out Out : TD3DXMatrix; Scaling : Single; RotationCenter : PD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation(Out : PD3DXMatrix; Scaling : Single; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation(out Out : TD3DXMatrix; Scaling : Single; const RotationCenter : TD3DXVector3; Rotation : PD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation(Out : PD3DXMatrix; Scaling : Single; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation(out Out : TD3DXMatrix; Scaling : Single; RotationCenter : PD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation(Out : PD3DXMatrix; Scaling : Single; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation(out Out : TD3DXMatrix; Scaling : Single; const RotationCenter : TD3DXVector3; const Rotation : TD3DXQuaternion; const Translation : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;


// Build 2D affine transformation matrix in XY plane.  NULL arguments are treated as identity.
// Mout = Ms * Mrc-1 * Mr * Mrc * Mt
function D3DXMatrixAffineTransformation2D(Out : PD3DXMatrix; Scaling : Single; RotationCenter : PD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation2D(out Out : TD3DXMatrix; Scaling : Single; RotationCenter : PD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation2D(Out : PD3DXMatrix; Scaling : Single; const RotationCenter : TD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation2D(out Out : TD3DXMatrix; Scaling : Single; const RotationCenter : TD3DXVector2; Rotation : Single; Translation : PD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation2D(Out : PD3DXMatrix; Scaling : Single; RotationCenter : PD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation2D(out Out : TD3DXMatrix; Scaling : Single; RotationCenter : PD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation2D(Out : PD3DXMatrix; Scaling : Single; const RotationCenter : TD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixAffineTransformation2D(out Out : TD3DXMatrix; Scaling : Single; const RotationCenter : TD3DXVector2; Rotation : Single; const Translation : TD3DXVector2) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build a lookat matrix. (right-handed)
function D3DXMatrixLookAtRH(Out : PD3DXMatrix; Eye, At, Up : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtRH(out Out : TD3DXMatrix; Eye, At, Up : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtRH(Out : PD3DXMatrix; const Eye : TD3DXVector3; At, Up : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtRH(out Out : TD3DXMatrix; const Eye : TD3DXVector3; At, Up : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtRH(Out : PD3DXMatrix; Eye : PD3DXVector3; const At : TD3DXVector3; Up : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtRH(out Out : TD3DXMatrix; Eye : PD3DXVector3; const At : TD3DXVector3; Up : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtRH(Out : PD3DXMatrix; const Eye, At : TD3DXVector3; Up : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtRH(out Out : TD3DXMatrix; const Eye, At : TD3DXVector3; Up : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtRH(Out : PD3DXMatrix; Eye, At : PD3DXVector3; const Up : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtRH(out Out : TD3DXMatrix; Eye, At : PD3DXVector3; const Up : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtRH(Out : PD3DXMatrix; const Eye : TD3DXVector3; At : PD3DXVector3; const Up : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtRH(out Out : TD3DXMatrix; const Eye : TD3DXVector3; At : PD3DXVector3; const Up : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtRH(Out : PD3DXMatrix; Eye : PD3DXVector3; const At, Up : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtRH(out Out : TD3DXMatrix; Eye : PD3DXVector3; const At, Up : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtRH(Out : PD3DXMatrix; const Eye, At, Up : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtRH(out Out : TD3DXMatrix; const Eye, At, Up : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

function D3DXMatrixLookAtLH(Out : PD3DXMatrix; Eye, At, Up : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtLH(out Out : TD3DXMatrix; Eye, At, Up : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtLH(Out : PD3DXMatrix; const Eye : TD3DXVector3; At, Up : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtLH(out Out : TD3DXMatrix; const Eye : TD3DXVector3; At, Up : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtLH(Out : PD3DXMatrix; Eye : PD3DXVector3; const At : TD3DXVector3; Up : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtLH(out Out : TD3DXMatrix; Eye : PD3DXVector3; const At : TD3DXVector3; Up : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtLH(Out : PD3DXMatrix; const Eye, At : TD3DXVector3; Up : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtLH(out Out : TD3DXMatrix; const Eye, At : TD3DXVector3; Up : PD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtLH(Out : PD3DXMatrix; Eye, At : PD3DXVector3; const Up : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtLH(out Out : TD3DXMatrix; Eye, At : PD3DXVector3; const Up : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtLH(Out : PD3DXMatrix; const Eye : TD3DXVector3; At : PD3DXVector3; const Up : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtLH(out Out : TD3DXMatrix; const Eye : TD3DXVector3; At : PD3DXVector3; const Up : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtLH(Out : PD3DXMatrix; Eye : PD3DXVector3; const At, Up : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtLH(out Out : TD3DXMatrix; Eye : PD3DXVector3; const At, Up : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtLH(Out : PD3DXMatrix; const Eye, At, Up : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixLookAtLH(out Out : TD3DXMatrix; const Eye, At, Up : TD3DXVector3) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build a perspective projection matrix. (right-handed)
function D3DXMatrixPerspectiveRH(Out : PD3DXMatrix; w, h, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixPerspectiveRH(out Out : TD3DXMatrix; w, h, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build a perspective projection matrix. (left-handed)
function D3DXMatrixPerspectiveLH(Out : PD3DXMatrix; w, h, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixPerspectiveLH(out Out : TD3DXMatrix; w, h, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build a perspective projection matrix. (right-handed)
function D3DXMatrixPerspectiveFovRH(Out : PD3DXMatrix; fovy, Aspect, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixPerspectiveFovRH(out Out : TD3DXMatrix; fovy, Aspect, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build a perspective projection matrix. (left-handed)
function D3DXMatrixPerspectiveFovLH(Out : PD3DXMatrix; fovy, Aspect, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixPerspectiveFovLH(out Out : TD3DXMatrix; fovy, Aspect, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build a perspective projection matrix. (right-handed)
function D3DXMatrixPerspectiveOffCenterRH(Out : PD3DXMatrix; l, r, b, t, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixPerspectiveOffCenterRH(out Out : TD3DXMatrix; l, r, b, t, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build a perspective projection matrix. (left-handed)
function D3DXMatrixPerspectiveOffCenterLH(Out : PD3DXMatrix; l, r, b, t, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixPerspectiveOffCenterLH(out Out : TD3DXMatrix; l, r, b, t, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;      

// Build an ortho projection matrix. (right-handed)
function D3DXMatrixOrthoRH(Out : PD3DXMatrix; w, h, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixOrthoRH(out Out : TD3DXMatrix; w, h, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build an ortho projection matrix. (left-handed)
function D3DXMatrixOrthoLH(Out : PD3DXMatrix; w, h, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixOrthoLH(out Out : TD3DXMatrix; w, h, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build an ortho projection matrix. (right-handed)
function D3DXMatrixOrthoOffCenterRH(Out : PD3DXMatrix; l, r, b, t, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixOrthoOffCenterRH(out Out : TD3DXMatrix; l, r, b, t, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build an ortho projection matrix. (left-handed)
function D3DXMatrixOrthoOffCenterLH(Out : PD3DXMatrix; l, r, b, t, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixOrthoOffCenterLH(out Out : TD3DXMatrix; l, r, b, t, zn, zf : Single) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

// Build a matrix which flattens geometry into a plane, as if casting
// a shadow from a light.
function D3DXMatrixShadow(Out : PD3DXMatrix; Light : PD3DXVector4; Plane : PD3DXPlane) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixShadow(out Out : TD3DXMatrix; Light : PD3DXVector4; Plane : PD3DXPlane) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixShadow(Out : PD3DXMatrix; const Light : TD3DXVector4; Plane : PD3DXPlane) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixShadow(out Out : TD3DXMatrix; const Light : TD3DXVector4; Plane : PD3DXPlane) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixShadow(Out : PD3DXMatrix; Light : PD3DXVector4; const Plane : TD3DXPlane) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixShadow(out Out : TD3DXMatrix; Light : PD3DXVector4; const Plane : TD3DXPlane) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixShadow(Out : PD3DXMatrix; const Light : TD3DXVector4; const Plane : TD3DXPlane) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixShadow(out Out : TD3DXMatrix; const Light : TD3DXVector4; const Plane : TD3DXPlane) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
      
// Build a matrix which reflects the coordinate system about a plane
function D3DXMatrixReflect(Out : PD3DXMatrix; Plane : PD3DXPlane) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixReflect(out Out : TD3DXMatrix; Plane : PD3DXPlane) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixReflect(Out : PD3DXMatrix; const Plane : TD3DXPlane) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;
function D3DXMatrixReflect(out Out : TD3DXMatrix; const Plane : TD3DXPlane) : PD3DXMatrix; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * Quaternion
 *******************************************************************************
(*)

// inline, Delphi uses dll

function D3DXQuaternionLength(Q : PD3DXQuaternion) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXQuaternionLength';
function D3DXQuaternionLength(const Q : TD3DXQuaternion) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXQuaternionLength';

// Length squared, or "norm"
function D3DXQuaternionLengthSq(Q : PD3DXQuaternion) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXQuaternionLengthSq';
function D3DXQuaternionLengthSq(const Q : TD3DXQuaternion) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXQuaternionLengthSq';

function D3DXQuaternionDot(Q1, Q2 : PD3DXQuaternion) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXQuaternionDot';
function D3DXQuaternionDot(const Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXQuaternionDot';
function D3DXQuaternionDot(Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXQuaternionDot';
function D3DXQuaternionDot(const Q1, Q2 : TD3DXQuaternion) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXQuaternionDot';

function D3DXQuaternionIsIdentity(Q : PD3DXQuaternion) : Bool; stdcall; overload; external d3dx9inldllname name 'INLD3DXQuaternionIsIdentity';
function D3DXQuaternionIsIdentity(const Q : TD3DXQuaternion) : Bool; stdcall; overload; external d3dx9inldllname name 'INLD3DXQuaternionIsIdentity';

// (-x, -y, -z, w)
function D3DXQuaternionConjugate(Out, Q : PD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9inldllname name 'INLD3DXQuaternionConjugate';
function D3DXQuaternionConjugate(out Out : TD3DXQuaternion; Q : PD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9inldllname name 'INLD3DXQuaternionConjugate';
function D3DXQuaternionConjugate(Out : PD3DXQuaternion; const Q : TD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9inldllname name 'INLD3DXQuaternionConjugate';
function D3DXQuaternionConjugate(out Out : TD3DXQuaternion; const Q : TD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9inldllname name 'INLD3DXQuaternionConjugate';

// non-inline

// Compute a quaternin's axis and angle of rotation. Expects unit quaternions.
procedure D3DXQuaternionToAxisAngle(Q : PD3DXQuaternion; Axis : PD3DXVector3; Angle : PSingle); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionToAxisAngle(const Q : TD3DXQuaternion; Axis : PD3DXVector3; Angle : PSingle); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionToAxisAngle(Q : PD3DXQuaternion; out Axis : TD3DXVector3; Angle : PSingle); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionToAxisAngle(const Q : TD3DXQuaternion; out Axis : TD3DXVector3; Angle : PSingle); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionToAxisAngle(Q : PD3DXQuaternion; Axis : PD3DXVector3; out Angle : Single); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionToAxisAngle(const Q : TD3DXQuaternion; Axis : PD3DXVector3; out Angle : Single); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionToAxisAngle(Q : PD3DXQuaternion; out Axis : TD3DXVector3; out Angle : Single); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionToAxisAngle(const Q : TD3DXQuaternion; out Axis : TD3DXVector3; out Angle : Single); stdcall; overload; external d3dx9dllname;

// Build a quaternion from a rotation matrix.
function D3DXQuaternionRotationMatrix(Out : PD3DXQuaternion; M : PD3DXMatrix) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionRotationMatrix(out Out : TD3DXQuaternion; M : PD3DXMatrix) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionRotationMatrix(Out : PD3DXQuaternion; const M : TD3DXMatrix) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionRotationMatrix(out Out : TD3DXQuaternion; const M : TD3DXMatrix) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;

// Rotation about arbitrary axis.
function D3DXQuaternionRotationAxis(Out : PD3DXQuaternion; V : PD3DXVector3; Angle : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionRotationAxis(out Out : TD3DXQuaternion; V : PD3DXVector3; Angle : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionRotationAxis(Out : PD3DXQuaternion; const V : TD3DXVector3; Angle : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionRotationAxis(out Out : TD3DXQuaternion; const V : TD3DXVector3; Angle : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;

// Yaw around the Y axis, a pitch around the X axis,
// and a roll around the Z axis.
function D3DXQuaternionRotationYawPitchRoll(out Out : TD3DXQuaternion; Yaw, Pitch, Roll : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionRotationYawPitchRoll(Out : PD3DXQuaternion; Yaw, Pitch, Roll : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionRotationYawPitchRoll(out Out : TD3DXQuaternion; YawPitchRoll : TD3DXVector3) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionRotationYawPitchRoll(Out : PD3DXQuaternion; YawPitchRoll : TD3DXVector3) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;

// Quaternion multiplication.  The result represents the rotation Q2
// followed by the rotation Q1.  (Out = Q2 * Q1)
function D3DXQuaternionMultiply(Out, Q1, Q2 : PD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionMultiply(out Out : TD3DXQuaternion; Q1, Q2 : PD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionMultiply(Out : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionMultiply(out Out : TD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionMultiply(Out, Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionMultiply(out Out : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionMultiply(Out : PD3DXQuaternion; const Q1, Q2 : TD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionMultiply(out Out : TD3DXQuaternion; const Q1, Q2 : TD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;

function D3DXQuaternionNormalize(Out, Q : PD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionNormalize(out Out : TD3DXQuaternion; Q : PD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionNormalize(Out : PD3DXQuaternion; const Q : TD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionNormalize(out Out : TD3DXQuaternion; const Q : TD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;

// Conjugate and re-norm
function D3DXQuaternionInverse(Out, Q : PD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionInverse(out Out : TD3DXQuaternion; Q : PD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionInverse(Out : PD3DXQuaternion; const Q : TD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionInverse(out Out : TD3DXQuaternion; const Q : TD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;

// Expects unit quaternions.
// if q = (cos(theta), sin(theta) * v); ln(q) = (0, theta * v)
function D3DXQuaternionLn(Out, Q : PD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionLn(out Out : TD3DXQuaternion; Q : PD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionLn(Out : PD3DXQuaternion; const Q : TD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionLn(out Out : TD3DXQuaternion; const Q : TD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;

// Expects pure quaternions. (w == 0)  w is ignored in calculation.
// if q = (0, theta * v); exp(q) = (cos(theta), sin(theta) * v)
function D3DXQuaternionExp(Out, Q : PD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionExp(out Out : TD3DXQuaternion; Q : PD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionExp(Out : PD3DXQuaternion; const Q : TD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionExp(out Out : TD3DXQuaternion; const Q : TD3DXQuaternion) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
    
// Spherical linear interpolation between Q1 (t == 0) and Q2 (t == 1).
// Expects unit quaternions.
function D3DXQuaternionSlerp(Out, Q1, Q2 : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSlerp(out Out : TD3DXQuaternion; Q1, Q2 : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSlerp(Out : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSlerp(out Out : TD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSlerp(Out, Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSlerp(out Out : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSlerp(Out : PD3DXQuaternion; const Q1, Q2 : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSlerp(out Out : TD3DXQuaternion; const Q1, Q2 : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;

// Spherical quadrangle interpolation.
// Slerp(Slerp(Q1, C, t), Slerp(A, B, t), 2t(1-t))
function D3DXQuaternionSquad(Out, Q1, pA, pB, C : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(out Out : TD3DXQuaternion; Q1, pA, pB, C : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(Out : PD3DXQuaternion; const Q1 : TD3DXQuaternion; pA, pB, C : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(out Out : TD3DXQuaternion; const Q1 : TD3DXQuaternion; pA, pB, C : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(Out, Q1 : PD3DXQuaternion; const pA : TD3DXQuaternion; pB, C : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(out Out : TD3DXQuaternion; Q1 : PD3DXQuaternion; const pA : TD3DXQuaternion; pB, C : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(Out : PD3DXQuaternion; const Q1, pA : TD3DXQuaternion; pB, C : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(out Out : TD3DXQuaternion; const Q1, pA : TD3DXQuaternion; pB, C : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(Out, Q1, pA : PD3DXQuaternion; const pB : TD3DXQuaternion; C : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(out Out : TD3DXQuaternion; Q1, pA : PD3DXQuaternion; const pB : TD3DXQuaternion; C : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(Out : PD3DXQuaternion; const Q1 : TD3DXQuaternion; pA : PD3DXQuaternion; const pB : TD3DXQuaternion; C : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(out Out : TD3DXQuaternion; const Q1 : TD3DXQuaternion; pA : PD3DXQuaternion; const pB : TD3DXQuaternion; C : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(Out, Q1 : PD3DXQuaternion; const pA, pB : TD3DXQuaternion; C : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(out Out : TD3DXQuaternion; Q1 : PD3DXQuaternion; const pA, pB : TD3DXQuaternion; C : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(Out : PD3DXQuaternion; const Q1, pA, pB : TD3DXQuaternion; C : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(out Out : TD3DXQuaternion; const Q1, pA, pB : TD3DXQuaternion; C : PD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(Out, Q1, pA, pB : PD3DXQuaternion; const C : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(out Out : TD3DXQuaternion; Q1, pA, pB : PD3DXQuaternion; const C : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(Out : PD3DXQuaternion; const Q1 : TD3DXQuaternion; pA, pB : PD3DXQuaternion; const C : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(out Out : TD3DXQuaternion; const Q1 : TD3DXQuaternion; pA, pB : PD3DXQuaternion; const C : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(Out, Q1 : PD3DXQuaternion; const pA : TD3DXQuaternion; pB : PD3DXQuaternion; const C : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(out Out : TD3DXQuaternion; Q1 : PD3DXQuaternion; const pA : TD3DXQuaternion; pB : PD3DXQuaternion; const C : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(Out : PD3DXQuaternion; const Q1, pA : TD3DXQuaternion; pB : PD3DXQuaternion; const C : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(out Out : TD3DXQuaternion; const Q1, pA : TD3DXQuaternion; pB : PD3DXQuaternion; const C : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(Out, Q1, pA : PD3DXQuaternion; const pB, C : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(out Out : TD3DXQuaternion; Q1, pA : PD3DXQuaternion; const pB, C : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(Out : PD3DXQuaternion; const Q1 : TD3DXQuaternion; pA : PD3DXQuaternion; const pB, C : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(out Out : TD3DXQuaternion; const Q1 : TD3DXQuaternion; pA : PD3DXQuaternion; const pB, C : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(Out, Q1 : PD3DXQuaternion; const pA, pB, C : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(out Out : TD3DXQuaternion; Q1 : PD3DXQuaternion; const pA, pB, C : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(Out : PD3DXQuaternion; const Q1, pA, pB, C : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionSquad(out Out : TD3DXQuaternion; const Q1, pA, pB, C : TD3DXQuaternion; t : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;

// Setup control points for spherical quadrangle interpolation
// from Q1 to Q2.  The control points are chosen in such a way 
// to ensure the continuity of tangents with adjacent segments.
procedure D3DXQuaternionSquadSetup(AOut, BOut, COut, Q0, Q1, Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut, COut, Q0, Q1, Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut : TD3DXQuaternion; COut, Q0, Q1, Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut : TD3DXQuaternion; COut, Q0, Q1, Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; Q0, Q1, Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; Q0, Q1, Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut, COut : TD3DXQuaternion; Q0, Q1, Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut, COut : TD3DXQuaternion; Q0, Q1, Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut, COut : PD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1, Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut, COut : PD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1, Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut : TD3DXQuaternion; COut : PD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1, Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut : TD3DXQuaternion; COut : PD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1, Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1, Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1, Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut, COut : TD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1, Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut, COut : TD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1, Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut, COut, Q0 : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut, COut, Q0 : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut : TD3DXQuaternion; COut, Q0 : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut : TD3DXQuaternion; COut, Q0 : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; Q0 : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; Q0 : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut, COut : TD3DXQuaternion; Q0 : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut, COut : TD3DXQuaternion; Q0 : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut, COut : PD3DXQuaternion; const Q0, Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut, COut : PD3DXQuaternion; const Q0, Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut : TD3DXQuaternion; COut : PD3DXQuaternion; const Q0, Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut : TD3DXQuaternion; COut : PD3DXQuaternion; const Q0, Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; const Q0, Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; const Q0, Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut, COut : TD3DXQuaternion; const Q0, Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut, COut : TD3DXQuaternion; const Q0, Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut, COut, Q0, Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut, COut, Q0, Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut : TD3DXQuaternion; COut, Q0, Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut : TD3DXQuaternion; COut, Q0, Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; Q0, Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; Q0, Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut, COut : TD3DXQuaternion; Q0, Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut, COut : TD3DXQuaternion; Q0, Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut, COut : PD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut, COut : PD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut : TD3DXQuaternion; COut : PD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut : TD3DXQuaternion; COut : PD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut, COut : TD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut, COut : TD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut, COut, Q0 : PD3DXQuaternion; const Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut, COut, Q0 : PD3DXQuaternion; const Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut : TD3DXQuaternion; COut, Q0 : PD3DXQuaternion; const Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut : TD3DXQuaternion; COut, Q0 : PD3DXQuaternion; const Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; Q0 : PD3DXQuaternion; const Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; Q0 : PD3DXQuaternion; const Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut, COut : TD3DXQuaternion; Q0 : PD3DXQuaternion; const Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut, COut : TD3DXQuaternion; Q0 : PD3DXQuaternion; const Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut, COut : PD3DXQuaternion; const Q0, Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut, COut : PD3DXQuaternion; const Q0, Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut : TD3DXQuaternion; COut : PD3DXQuaternion; const Q0, Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut : TD3DXQuaternion; COut : PD3DXQuaternion; const Q0, Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; const Q0, Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; const Q0, Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut, COut : TD3DXQuaternion; const Q0, Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut, COut : TD3DXQuaternion; const Q0, Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut, COut, Q0, Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut, COut, Q0, Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut : TD3DXQuaternion; COut, Q0, Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut : TD3DXQuaternion; COut, Q0, Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; Q0, Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; Q0, Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut, COut : TD3DXQuaternion; Q0, Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut, COut : TD3DXQuaternion; Q0, Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut, COut : PD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut, COut : PD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut : TD3DXQuaternion; COut : PD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut : TD3DXQuaternion; COut : PD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut, COut : TD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut, COut : TD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut, COut, Q0 : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut, COut, Q0 : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut : TD3DXQuaternion; COut, Q0 : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut : TD3DXQuaternion; COut, Q0 : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; Q0 : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; Q0 : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut, COut : TD3DXQuaternion; Q0 : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut, COut : TD3DXQuaternion; Q0 : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut, COut : PD3DXQuaternion; const Q0, Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut, COut : PD3DXQuaternion; const Q0, Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut : TD3DXQuaternion; COut : PD3DXQuaternion; const Q0, Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut : TD3DXQuaternion; COut : PD3DXQuaternion; const Q0, Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; const Q0, Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; const Q0, Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut, COut : TD3DXQuaternion; const Q0, Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut, COut : TD3DXQuaternion; const Q0, Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut, COut, Q0, Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut, COut, Q0, Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut : TD3DXQuaternion; COut, Q0, Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut : TD3DXQuaternion; COut, Q0, Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; Q0, Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; Q0, Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut, COut : TD3DXQuaternion; Q0, Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut, COut : TD3DXQuaternion; Q0, Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut, COut : PD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut, COut : PD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut : TD3DXQuaternion; COut : PD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut : TD3DXQuaternion; COut : PD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut, COut : TD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut, COut : TD3DXQuaternion; const Q0 : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut, COut, Q0 : PD3DXQuaternion; const Q1, Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut, COut, Q0 : PD3DXQuaternion; const Q1, Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut : TD3DXQuaternion; COut, Q0 : PD3DXQuaternion; const Q1, Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut : TD3DXQuaternion; COut, Q0 : PD3DXQuaternion; const Q1, Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; Q0 : PD3DXQuaternion; const Q1, Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; Q0 : PD3DXQuaternion; const Q1, Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut, COut : TD3DXQuaternion; Q0 : PD3DXQuaternion; const Q1, Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut, COut : TD3DXQuaternion; Q0 : PD3DXQuaternion; const Q1, Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut, COut : PD3DXQuaternion; const Q0, Q1, Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut, COut : PD3DXQuaternion; const Q0, Q1, Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut : TD3DXQuaternion; COut : PD3DXQuaternion; const Q0, Q1, Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut : TD3DXQuaternion; COut : PD3DXQuaternion; const Q0, Q1, Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut, BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; const Q0, Q1, Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut : TD3DXQuaternion; BOut : PD3DXQuaternion; out COut : TD3DXQuaternion; const Q0, Q1, Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(AOut : PD3DXQuaternion; out BOut, COut : TD3DXQuaternion; const Q0, Q1, Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;
procedure D3DXQuaternionSquadSetup(out AOut, BOut, COut : TD3DXQuaternion; const Q0, Q1, Q2, Q3 : TD3DXQuaternion); stdcall; overload; external d3dx9dllname;


// Barycentric interpolation.
// Slerp(Slerp(Q1, Q2, f+g), Slerp(Q1, Q3, f+g), g/(f+g))
function D3DXQuaternionBaryCentric(Out, Q1, Q2, Q3 : PD3DXQuaternion; f, g : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionBaryCentric(out Out : TD3DXQuaternion; Q1, Q2, Q3 : PD3DXQuaternion; f, g : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionBaryCentric(Out : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion; f, g : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionBaryCentric(out Out : TD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2, Q3 : PD3DXQuaternion; f, g : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionBaryCentric(Out, Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion; f, g : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionBaryCentric(out Out : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion; f, g : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionBaryCentric(Out : PD3DXQuaternion; const Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion; f, g : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionBaryCentric(out Out : TD3DXQuaternion; const Q1, Q2 : TD3DXQuaternion; Q3 : PD3DXQuaternion; f, g : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionBaryCentric(Out, Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion; f, g : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionBaryCentric(out Out : TD3DXQuaternion; Q1, Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion; f, g : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionBaryCentric(Out : PD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion; f, g : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionBaryCentric(out Out : TD3DXQuaternion; const Q1 : TD3DXQuaternion; Q2 : PD3DXQuaternion; const Q3 : TD3DXQuaternion; f, g : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionBaryCentric(Out, Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion; f, g : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionBaryCentric(out Out : TD3DXQuaternion; Q1 : PD3DXQuaternion; const Q2, Q3 : TD3DXQuaternion; f, g : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionBaryCentric(Out : PD3DXQuaternion; const Q1, Q2, Q3 : TD3DXQuaternion; f, g : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;
function D3DXQuaternionBaryCentric(out Out : TD3DXQuaternion; const Q1, Q2, Q3 : TD3DXQuaternion; f, g : Single) : PD3DXQuaternion; stdcall; overload; external d3dx9dllname;


(*)
 *******************************************************************************
 * Plane
 *******************************************************************************
(*)

// inline, Delphi uses dll

// ax + by + cz + dw
function D3DXPlaneDot(P : PD3DXPlane; V : PD3DXVector4) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXPlaneDot';
function D3DXPlaneDot(const P : TD3DXPlane; V : PD3DXVector4) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXPlaneDot';
function D3DXPlaneDot(P : PD3DXPlane; const V : TD3DXVector4) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXPlaneDot';
function D3DXPlaneDot(const P : TD3DXPlane; const V : TD3DXVector4) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXPlaneDot';

// ax + by + cz + d
function D3DXPlaneDotCoord(P : PD3DXPlane; V : PD3DXVector3) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXPlaneDotCoord';
function D3DXPlaneDotCoord(const P : TD3DXPlane; V : PD3DXVector3) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXPlaneDotCoord';
function D3DXPlaneDotCoord(P : PD3DXPlane; const V : TD3DXVector3) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXPlaneDotCoord';
function D3DXPlaneDotCoord(const P : TD3DXPlane; const V : TD3DXVector3) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXPlaneDotCoord';

// ax + by + cz
function D3DXPlaneDotNormal(P : PD3DXPlane; V : PD3DXVector3) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXPlaneDotNormal';
function D3DXPlaneDotNormal(const P : TD3DXPlane; V : PD3DXVector3) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXPlaneDotNormal';
function D3DXPlaneDotNormal(P : PD3DXPlane; const V : TD3DXVector3) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXPlaneDotNormal';
function D3DXPlaneDotNormal(const P : TD3DXPlane; const V : TD3DXVector3) : Single; stdcall; overload; external d3dx9inldllname name 'INLD3DXPlaneDotNormal';

function D3DXPlaneScale(Out, Plane : PD3DXPlane; Scale : Single) : PD3DXPlane; stdcall; overload; external d3dx9dllname name 'INLD3DXPlaneScale';
function D3DXPlaneScale(out Out : TD3DXPlane; Plane : PD3DXPlane; Scale : Single) : PD3DXPlane; stdcall; overload; external d3dx9dllname name 'INLD3DXPlaneScale';
function D3DXPlaneScale(Out : PD3DXPlane; const Plane : TD3DXPlane; Scale : Single) : PD3DXPlane; stdcall; overload; external d3dx9dllname name 'INLD3DXPlaneScale';
function D3DXPlaneScale(out Out : TD3DXPlane; const Plane : TD3DXPlane; Scale : Single) : PD3DXPlane; stdcall; overload; external d3dx9dllname name 'INLD3DXPlaneScale';

// non-inline

// Normalize plane (so that |a,b,c| == 1)
function D3DXPlaneNormalize(Out, P : PD3DXPlane) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneNormalize(out Out : TD3DXPlane; P : PD3DXPlane) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneNormalize(Out : PD3DXPlane; const P : TD3DXPlane) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneNormalize(out Out : TD3DXPlane; const P : TD3DXPlane) : PD3DXPlane; stdcall; overload; external d3dx9dllname;

// Find the intersection between a plane and a line.  If the line is
// parallel to the plane, NULL is returned.
function D3DXPlaneIntersectLine(Out : PD3DXVector3; P : PD3DXPlane; V1, V2 : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneIntersectLine(out Out : TD3DXVector3; P : PD3DXPlane; V1, V2 : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneIntersectLine(Out : PD3DXVector3; const P : TD3DXPlane; V1, V2 : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneIntersectLine(out Out : TD3DXVector3; const P : TD3DXPlane; V1, V2 : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneIntersectLine(Out : PD3DXVector3; P : PD3DXPlane; const V1 : TD3DXVector3; V2 : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneIntersectLine(out Out : TD3DXVector3; P : PD3DXPlane; const V1 : TD3DXVector3; V2 : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneIntersectLine(Out : PD3DXVector3; const P : TD3DXPlane; const V1 : TD3DXVector3; V2 : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneIntersectLine(out Out : TD3DXVector3; const P : TD3DXPlane; const V1 : TD3DXVector3; V2 : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneIntersectLine(Out : PD3DXVector3; P : PD3DXPlane; V1 : PD3DXVector3; const V2 : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneIntersectLine(out Out : TD3DXVector3; P : PD3DXPlane; V1 : PD3DXVector3; const V2 : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneIntersectLine(Out : PD3DXVector3; const P : TD3DXPlane; V1 : PD3DXVector3; const V2 : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneIntersectLine(out Out : TD3DXVector3; const P : TD3DXPlane; V1 : PD3DXVector3; const V2 : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneIntersectLine(Out : PD3DXVector3; P : PD3DXPlane; const V1, V2 : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneIntersectLine(out Out : TD3DXVector3; P : PD3DXPlane; const V1, V2 : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneIntersectLine(Out : PD3DXVector3; const P : TD3DXPlane; const V1, V2 : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneIntersectLine(out Out : TD3DXVector3; const P : TD3DXPlane; const V1, V2 : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;

// Construct a plane from a point and a normal
function D3DXPlaneFromPointNormal(Out : PD3DXPlane; Point, Normal : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPointNormal(out Out : TD3DXPlane; Point, Normal : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPointNormal(Out : PD3DXPlane; const Point : TD3DXVector3; Normal : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPointNormal(out Out : TD3DXPlane; const Point : TD3DXVector3; Normal : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPointNormal(Out : PD3DXPlane; Point : PD3DXVector3; const Normal : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPointNormal(out Out : TD3DXPlane; Point : PD3DXVector3; const Normal : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPointNormal(Out : PD3DXPlane; const Point, Normal : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPointNormal(out Out : TD3DXPlane; const Point, Normal : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;

// Construct a plane from 3 points
function D3DXPlaneFromPoints(Out : PD3DXPlane; V1, V2, V3 : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPoints(out Out : TD3DXPlane; V1, V2, V3 : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPoints(Out : PD3DXPlane; const V1 : TD3DXVector3; V2, V3 : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPoints(out Out : TD3DXPlane; const V1 : TD3DXVector3; V2, V3 : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPoints(Out : PD3DXPlane; V1 : PD3DXVector3; const V2 : TD3DXVector3; V3 : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPoints(out Out : TD3DXPlane; V1 : PD3DXVector3; const V2 : TD3DXVector3; V3 : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPoints(Out : PD3DXPlane; const V1, V2 : TD3DXVector3; V3 : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPoints(out Out : TD3DXPlane; const V1, V2 : TD3DXVector3; V3 : PD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPoints(Out : PD3DXPlane; V1, V2 : PD3DXVector3; const V3 : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPoints(out Out : TD3DXPlane; V1, V2 : PD3DXVector3; const V3 : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPoints(Out : PD3DXPlane; const V1 : TD3DXVector3; V2 : PD3DXVector3; const V3 : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPoints(out Out : TD3DXPlane; const V1 : TD3DXVector3; V2 : PD3DXVector3; const V3 : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPoints(Out : PD3DXPlane; V1 : PD3DXVector3; const V2, V3 : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPoints(out Out : TD3DXPlane; V1 : PD3DXVector3; const V2, V3 : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPoints(Out : PD3DXPlane; const V1, V2, V3 : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneFromPoints(out Out : TD3DXPlane; const V1, V2, V3 : TD3DXVector3) : PD3DXPlane; stdcall; overload; external d3dx9dllname;

// Transform a plane by a matrix.  The vector (a,b,c) must be normal.
// M should be the inverse transpose of the transformation desired.
function D3DXPlaneTransform(Out, P : PD3DXPlane; M : PD3DXMatrix) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneTransform(out Out : TD3DXPlane; P : PD3DXPlane; M : PD3DXMatrix) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneTransform(Out : PD3DXPlane; const P : TD3DXPlane; M : PD3DXMatrix) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneTransform(out Out : TD3DXPlane; const P : TD3DXPlane; M : PD3DXMatrix) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneTransform(Out, P : PD3DXPlane; const M : TD3DXMatrix) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneTransform(out Out : TD3DXPlane; P : PD3DXPlane; const M : TD3DXMatrix) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneTransform(Out : PD3DXPlane; const P : TD3DXPlane; const M : TD3DXMatrix) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneTransform(out Out : TD3DXPlane; const P : TD3DXPlane; const M : TD3DXMatrix) : PD3DXPlane; stdcall; overload; external d3dx9dllname;

// Transform an array of planes by a matrix.  The vectors (a,b,c) must be normal.
// M should be the inverse transpose of the transformation desired.
function D3DXPlaneTransformArray(Out : PD3DXPlane; OutStride : Cardinal; P : PD3DXPlane; PStride : Cardinal; M : PD3DXMatrix; n : Cardinal) : PD3DXPlane; stdcall; overload; external d3dx9dllname;
function D3DXPlaneTransformArray(Out : PD3DXPlane; OutStride : Cardinal; P : PD3DXPlane; PStride : Cardinal; const M : TD3DXMatrix; n : Cardinal) : PD3DXPlane; stdcall; overload; external d3dx9dllname;


(*)
 *******************************************************************************
 * Color
 *******************************************************************************
(*)

// inline, Delphi uses dll

// (1-r, 1-g, 1-b, a)
function D3DXColorNegative(Out, C : PD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorNegative';
function D3DXColorNegative(out Out : TD3DXColor; C : PD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorNegative';
function D3DXColorNegative(Out : PD3DXColor; const C : TD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorNegative';
function D3DXColorNegative(out Out : TD3DXColor; const C : TD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorNegative';

function D3DXColorAdd(Out, C1, C2 : PD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorAdd';
function D3DXColorAdd(out Out : TD3DXColor; C1, C2 : PD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorAdd';
function D3DXColorAdd(Out : PD3DXColor; const C1 : TD3DXColor; C2 : PD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorAdd';
function D3DXColorAdd(out Out : TD3DXColor; const C1 : TD3DXColor; C2 : PD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorAdd';
function D3DXColorAdd(Out, C1 : PD3DXColor; const C2 : TD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorAdd';
function D3DXColorAdd(out Out : TD3DXColor; C1 : PD3DXColor; const C2 : TD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorAdd';
function D3DXColorAdd(Out : PD3DXColor; const C1, C2 : TD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorAdd';
function D3DXColorAdd(out Out : TD3DXColor; const C1, C2 : TD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorAdd';

function D3DXColorSubtract(Out, C1, C2 : PD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorSubtract';
function D3DXColorSubtract(out Out : TD3DXColor; C1, C2 : PD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorSubtract';
function D3DXColorSubtract(Out : PD3DXColor; const C1 : TD3DXColor; C2 : PD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorSubtract';
function D3DXColorSubtract(out Out : TD3DXColor; const C1 : TD3DXColor; C2 : PD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorSubtract';
function D3DXColorSubtract(Out, C1 : PD3DXColor; const C2 : TD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorSubtract';
function D3DXColorSubtract(out Out : TD3DXColor; C1 : PD3DXColor; const C2 : TD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorSubtract';
function D3DXColorSubtract(Out : PD3DXColor; const C1, C2 : TD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorSubtract';
function D3DXColorSubtract(out Out : TD3DXColor; const C1, C2 : TD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorSubtract';

function D3DXColorScale(Out, C : PD3DXColor; const s : Single) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorScale';
function D3DXColorScale(out Out : TD3DXColor; C : PD3DXColor; const s : Single) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorScale';
function D3DXColorScale(Out : PD3DXColor; const C : TD3DXColor; const s : Single) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorScale';
function D3DXColorScale(out Out : TD3DXColor; const C : TD3DXColor; const s : Single) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorScale';

// (r1*r2, g1*g2, b1*b2, a1*a2)
function D3DXColorModulate(Out, C1, C2 : PD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorModulate';
function D3DXColorModulate(out Out : TD3DXColor; C1, C2 : PD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorModulate';
function D3DXColorModulate(Out : PD3DXColor; const C1 : TD3DXColor; C2 : PD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorModulate';
function D3DXColorModulate(out Out : TD3DXColor; const C1 : TD3DXColor; C2 : PD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorModulate';
function D3DXColorModulate(Out, C1 : PD3DXColor; const C2 : TD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorModulate';
function D3DXColorModulate(out Out : TD3DXColor; C1 : PD3DXColor; const C2 : TD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorModulate';
function D3DXColorModulate(Out : PD3DXColor; const C1, C2 : TD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorModulate';
function D3DXColorModulate(out Out : TD3DXColor; const C1, C2 : TD3DXColor) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorModulate';

// Linear interpolation of r,g,b, and a. C1 + s(C2-C1)
function D3DXColorLerp(Out, C1, C2 : PD3DXColor; const s : Single) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorLerp';
function D3DXColorLerp(out Out : TD3DXColor; C1, C2 : PD3DXColor; const s : Single) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorLerp';
function D3DXColorLerp(Out : PD3DXColor; const C1 : TD3DXColor; C2 : PD3DXColor; const s : Single) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorLerp';
function D3DXColorLerp(out Out : TD3DXColor; const C1 : TD3DXColor; C2 : PD3DXColor; const s : Single) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorLerp';
function D3DXColorLerp(Out, C1 : PD3DXColor; const C2 : TD3DXColor; const s : Single) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorLerp';
function D3DXColorLerp(out Out : TD3DXColor; C1 : PD3DXColor; const C2 : TD3DXColor; const s : Single) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorLerp';
function D3DXColorLerp(Out : PD3DXColor; const C1, C2 : TD3DXColor; const s : Single) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorLerp';
function D3DXColorLerp(out Out : TD3DXColor; const C1, C2 : TD3DXColor; const s : Single) : PD3DXColor; stdcall; overload; external d3dx9inldllname name 'INLD3DXColorLerp';

// non-inline

// Interpolate r,g,b between desaturated color and color.
// DesaturatedColor + s(Color - DesaturatedColor)
function D3DXColorAdjustSaturation(Out, C : PD3DXColor; s : Single) : PD3DXColor; stdcall; overload; external d3dx9dllname;
function D3DXColorAdjustSaturation(out Out : TD3DXColor; C : PD3DXColor; s : Single) : PD3DXColor; stdcall; overload; external d3dx9dllname;
function D3DXColorAdjustSaturation(Out : PD3DXColor; const C : TD3DXColor; s : Single) : PD3DXColor; stdcall; overload; external d3dx9dllname;
function D3DXColorAdjustSaturation(out Out : TD3DXColor; const C : TD3DXColor; s : Single) : PD3DXColor; stdcall; overload; external d3dx9dllname;

// Interpolate r,g,b between 50% grey and color.  Grey + s(Color - Grey)
function D3DXColorAdjustContrast(Out, C : PD3DXColor; Contrast : Single) : PD3DXColor; stdcall; overload; external d3dx9dllname;
function D3DXColorAdjustContrast(out Out : TD3DXColor; C : PD3DXColor; Contrast : Single) : PD3DXColor; stdcall; overload; external d3dx9dllname;
function D3DXColorAdjustContrast(Out : PD3DXColor; const C : TD3DXColor; Contrast : Single) : PD3DXColor; stdcall; overload; external d3dx9dllname;
function D3DXColorAdjustContrast(out Out : TD3DXColor; const C : TD3DXColor; Contrast : Single) : PD3DXColor; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * Misc
 *******************************************************************************
(*)

// Calculate Fresnel term given the cosine of theta (likely obtained by
// taking the dot of two normals), and the refraction index of the material.
function D3DXFresnelTerm(CosTheta, RefractionIndex : Single) : Single; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *    Matrix Stack
 *******************************************************************************
(*)

const
  IID_ID3DXMatrixStack : TGUID = '{E3357330-CC5E-11D2-A434-00A0C90629A8}';

type
  PID3DXMatrixStack = ^ID3DXMatrixStack;
  ID3DXMatrixStack = interface (IUnknown)
    ['{E3357330-CC5E-11D2-A434-00A0C90629A8}']
    (*** ID3DXMatrixStack methods ***)
    // Pops the top of the stack, returns the current top
    // *after* popping the top.
    function Pop : HResult; stdcall;

    // Pushes the stack by one, duplicating the current matrix.
    function Push : HResult; stdcall;

    // Loads identity in the current matrix.
    function LoadIdentity : HResult; stdcall;

    // Loads the given matrix into the current matrix
    function LoadMatrix(const M : TD3DXMatrix) : HResult; stdcall;

    // Right-Multiplies the given matrix to the current matrix.
    // (transformation is about the current world origin)
    function MultMatrix(const M : TD3DXMatrix) : HResult; stdcall;

    // Left-Multiplies the given matrix to the current matrix
    // (transformation is about the local origin of the object)
    function MultMatrixLocal(const M : TD3DXMatrix) : HResult; stdcall;

    // Right multiply the current matrix with the computed rotation
    // matrix, counterclockwise about the given axis with the given angle.
    // (rotation is about the current world origin)
    function RotateAxis(const V : TD3DXVector3; const Angle : Single) : HResult; stdcall;

    // Left multiply the current matrix with the computed rotation
    // matrix, counterclockwise about the given axis with the given angle.
    // (rotation is about the local origin of the object)
    function RotateAxisLocal(const V : TD3DXVector3; const Angle : Single) : HResult; stdcall;

    // Right multiply the current matrix with the computed rotation
    // matrix. All angles are counterclockwise. (rotation is about the
    // current world origin)

    // The rotation is composed of a yaw around the Y axis, a pitch around
    // the X axis, and a roll around the Z axis.
    function RotateYawPitchRoll(const Yaw, Pitch, Roll : Single) : HResult; stdcall;

    // Left multiply the current matrix with the computed rotation
    // matrix. All angles are counterclockwise. (rotation is about the
    // local origin of the object)

    // The rotation is composed of a yaw around the Y axis, a pitch around
    // the X axis, and a roll around the Z axis.
    function RotateYawPitchRollLocal(const Yaw, Pitch, Roll : Single) : HResult; stdcall;

    // Right multiply the current matrix with the computed scale
    // matrix. (transformation is about the current world origin)
    function Scale(const x, y, z : Single) : HResult; stdcall;

    // Left multiply the current matrix with the computed scale
    // matrix. (transformation is about the local origin of the object)
    function ScaleLocal(const x, y, z : Single) : HResult; stdcall;

    // Right multiply the current matrix with the computed translation
    // matrix. (transformation is about the current world origin)
    function Translate(const x, y, z : Single) : HResult; stdcall;

    // Left multiply the current matrix with the computed translation
    // matrix. (transformation is about the local origin of the object)
    function TranslateLocal(const x, y, z : Single) : HResult; stdcall;

    // Obtain the current matrix at the top of the stack
    function GetTop : PD3DXMatrix; stdcall;
  end;

function D3DXCreateMatrixStack(const Flags : LongWord; out Stack : ID3DXMatrixStack) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateMatrixStack(const Flags : LongWord; Stack : PID3DXMatrixStack) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  Spherical Harmonic Runtime Routines
 *
 * NOTE:
 *  * Most of these functions can take the same object as in and out parameters.
 *    The exceptions are the rotation functions.
 *
 *  * Out parameters are typically also returned as return values, so that
 *    the output of one function may be used as a parameter to another.
 *
 *******************************************************************************
(*)

// non-inline

(*)
 *******************************************************************************
 *
 *  Basic Spherical Harmonic math routines
 *
 *******************************************************************************
(*)

const
  D3DXSH_MINORDER = 2;
  D3DXSH_MAXORDER = 6;

(*)
 *******************************************************************************
 *
 *  D3DXSHEvalDirection:
 *  --------------------
 *  Evaluates the Spherical Harmonic basis functions
 *
 *  Parameters:
 *   pOut
 *      Output SH coefficients - basis function Ylm is stored at l*l + m+l
 *      This is the pointer that is returned.
 *   Order
 *      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
 *   pDir
 *      Direction to evaluate in - assumed to be normalized
 *
 *******************************************************************************
(*)

function D3DXSHEvalDirection(Out : PSingle; Order : Cardinal; Dir : PD3DXVector3) : PSingle; stdcall; overload; external d3dx9dllname;
function D3DXSHEvalDirection(out Out : Single; Order : Cardinal; Dir : PD3DXVector3) : PSingle; stdcall; overload; external d3dx9dllname;
function D3DXSHEvalDirection(Out : PSingle; Order : Cardinal; const Dir : TD3DXVector3) : PSingle; stdcall; overload; external d3dx9dllname;
function D3DXSHEvalDirection(out Out : Single; Order : Cardinal; const Dir : TD3DXVector3) : PSingle; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHRotate:
 *  --------------------
 *  Rotates SH vector by a rotation matrix
 *
 *  Parameters:
 *   pOut
 *      Output SH coefficients - basis function Ylm is stored at l*l + m+l
 *      This is the pointer that is returned (should not alias with pIn.)
 *   Order
 *      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
 *   pMatrix
 *      Matrix used for rotation - rotation sub matrix should be orthogonal
 *      and have a unit determinant.
 *   pIn
 *      Input SH coeffs (rotated), incorect results if this is also output.
 *
 *******************************************************************************
(*)

function D3DXSHRotate(Out : PSingle; Order : Cardinal; Matrix : PD3DXMatrix; _In : PSingle) : PSingle; stdcall; overload; external d3dx9dllname;
function D3DXSHRotate(out Out : Single; Order : Cardinal; Matrix : PD3DXMatrix; _In : PSingle) : PSingle; stdcall; overload; external d3dx9dllname;
function D3DXSHRotate(Out : PSingle; Order : Cardinal; const Matrix : TD3DXMatrix; _In : PSingle) : PSingle; stdcall; overload; external d3dx9dllname;
function D3DXSHRotate(out Out : Single; Order : Cardinal; const Matrix : TD3DXMatrix; _In : PSingle) : PSingle; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHRotateZ:
 *  --------------------
 *  Rotates the SH vector in the Z axis by an angle
 *
 *  Parameters:
 *   pOut
 *      Output SH coefficients - basis function Ylm is stored at l*l + m+l
 *      This is the pointer that is returned (should not alias with pIn.)
 *   Order
 *      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
 *   Angle
 *      Angle in radians to rotate around the Z axis.
 *   pIn
 *      Input SH coeffs (rotated), incorect results if this is also output.
 *
 *******************************************************************************
(*)

function D3DXSHRotateZ(Out : PSingle; Order : Cardinal; Angle : Single; _In : PSingle) : PSingle; stdcall; overload; external d3dx9dllname;
function D3DXSHRotateZ(out Out : Single; Order : Cardinal; Angle : Single; _In : PSingle) : PSingle; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHAdd:
 *  --------------------
 *  Adds two SH vectors, pOut[i] = pA[i] + pB[i];
 *
 *  Parameters:
 *   pOut
 *      Output SH coefficients - basis function Ylm is stored at l*l + m+l
 *      This is the pointer that is returned.
 *   Order
 *      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
 *   pA
 *      Input SH coeffs.
 *   pB
 *      Input SH coeffs (second vector.)
 *
 *******************************************************************************
(*)

function D3DXSHAdd(Out : PSingle; Order : Cardinal; A, B : PSingle) : PSingle; stdcall; overload; external d3dx9dllname;
function D3DXSHAdd(out Out : Single; Order : Cardinal; A, B : PSingle) : PSingle; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHScale:
 *  --------------------
 *  Adds two SH vectors, pOut[i] = pA[i]*Scale;
 *
 *  Parameters:
 *   pOut
 *      Output SH coefficients - basis function Ylm is stored at l*l + m+l
 *      This is the pointer that is returned.
 *   Order
 *      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
 *   pIn
 *      Input SH coeffs.
 *   Scale
 *      Scale factor.
 *
 *******************************************************************************
(*)

function D3DXSHScale(Out : PSingle; Order : Cardinal; _In : PSingle; Scale : Single) : PSingle; stdcall; overload; external d3dx9dllname;
function D3DXSHScale(out Out : Single; Order : Cardinal; _In : PSingle; Scale : Single) : PSingle; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHDot:
 *  --------------------
 *  Computes the dot product of two SH vectors
 *
 *  Parameters:
 *   Order
 *      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
 *   pA
 *      Input SH coeffs.
 *   pB
 *      Second set of input SH coeffs.
 *
 *******************************************************************************
(*)

function D3DXSHDot(Order : Cardinal; A, B : PSingle) : Single; stdcall; external d3dx9dllname;    

(*)
 *******************************************************************************
 *
 *  Basic Spherical Harmonic lighting routines
 *
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 *
 *  D3DXSHEvalDirectionalLight:
 *  --------------------
 *  Evaluates a directional light and returns spectral SH data.  The output
 *  vector is computed so that if the intensity of R/G/B is unit the resulting
 *  exit radiance of a point directly under the light on a diffuse object with
 *  an albedo of 1 would be 1.0.  This will compute 3 spectral samples, pROut
 *  has to be specified, while pGout and pBout are optional.
 *
 *  Parameters:
 *   Order
 *      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
 *   pDir
 *      Direction light is coming from (assumed to be normalized.)
 *   RIntensity
 *      Red intensity of light.
 *   GIntensity
 *      Green intensity of light.
 *   BIntensity
 *      Blue intensity of light.
 *   pROut
 *      Output SH vector for Red.
 *   pGOut
 *      Output SH vector for Green (optional.)
 *   pBOut
 *      Output SH vector for Blue (optional.)
 *
 *******************************************************************************
(*)

function D3DXSHEvalDirectionalLight(Order : Cardinal; var Dir : TD3DXVector3; RIntensity, GIntensity, BIntensity : Single; out ROut, GOut, BOut : Single) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSHEvalDirectionalLight(Order : Cardinal; Dir : PD3DXVector3; RIntensity, GIntensity, BIntensity : Single; out ROut, GOut, BOut : Single) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHEvalSphericalLight:
 *  --------------------
 *  Evaluates a spherical light and returns spectral SH data.  There is no
 *  normalization of the intensity of the light like there is for directional
 *  lights, care has to be taken when specifiying the intensities.  This will
 *  compute 3 spectral samples, pROut has to be specified, while pGout and
 *  pBout are optional.
 *
 *  Parameters:
 *   Order
 *      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
 *   pPos
 *      Position of light - reciever is assumed to be at the origin.
 *   Radius
 *      Radius of the spherical light source.
 *   RIntensity
 *      Red intensity of light.
 *   GIntensity
 *      Green intensity of light.
 *   BIntensity
 *      Blue intensity of light.
 *   pROut
 *      Output SH vector for Red.
 *   pGOut
 *      Output SH vector for Green (optional.)
 *   pBOut
 *      Output SH vector for Blue (optional.)
 *
 *******************************************************************************
(*)

function D3DXSHEvalSphericalLight(Order : Cardinal; var Dir : TD3DXVector3; Radius, RIntensity, GIntensity, BIntensity : Single; out ROut, GOut, BOut : Single) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSHEvalSphericalLight(Order : Cardinal; Dir : PD3DXVector3; Radius, RIntensity, GIntensity, BIntensity : Single; out ROut, GOut, BOut : Single) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHEvalConeLight:
 *  --------------------
 *  Evaluates a light that is a cone of constant intensity and returns spectral
 *  SH data.  The output vector is computed so that if the intensity of R/G/B is
 *  unit the resulting exit radiance of a point directly under the light oriented
 *  in the cone direction on a diffuse object with an albedo of 1 would be 1.0.
 *  This will compute 3 spectral samples, pROut has to be specified, while pGout
 *  and pBout are optional.
 *
 *  Parameters:
 *   Order
 *      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
 *   pDir
 *      Direction light is coming from (assumed to be normalized.)
 *   Radius
 *      Radius of cone in radians.
 *   RIntensity
 *      Red intensity of light.
 *   GIntensity
 *      Green intensity of light.
 *   BIntensity
 *      Blue intensity of light.
 *   pROut
 *      Output SH vector for Red.
 *   pGOut
 *      Output SH vector for Green (optional.)
 *   pBOut
 *      Output SH vector for Blue (optional.)
 *
 *******************************************************************************
(*)

function D3DXSHEvalConeLight(Order : Cardinal; Dir : PD3DXVector3; Radius, RIntensity, GIntensity, BIntensity : Single; out ROut, GOut, BOut : Single) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSHEvalConeLight(Order : Cardinal; var Dir : TD3DXVector3; Radius, RIntensity, GIntensity, BIntensity : Single; out ROut, GOut, BOut : Single) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHEvalHemisphereLight:
 *  --------------------
 *  Evaluates a light that is a linear interpolant between two colors over the
 *  sphere.  The interpolant is linear along the axis of the two points, not
 *  over the surface of the sphere (ie: if the axis was (0,0,1) it is linear in
 *  Z, not in the azimuthal angle.)  The resulting spherical lighting function
 *  is normalized so that a point on a perfectly diffuse surface with no
 *  shadowing and a normal pointed in the direction pDir would result in exit
 *  radiance with a value of 1 if the top color was white and the bottom color
 *  was black.  This is a very simple model where Top represents the intensity
 *  of the "sky" and Bottom represents the intensity of the "ground".
 *
 *  Parameters:
 *   Order
 *      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
 *   pDir
 *      Axis of the hemisphere.
 *   Top
 *      Color of the upper hemisphere.
 *   Bottom
 *      Color of the lower hemisphere.
 *   pROut
 *      Output SH vector for Red.
 *   pGOut
 *      Output SH vector for Green
 *   pBOut
 *      Output SH vector for Blue
 *
 *******************************************************************************
(*)

function D3DXSHEvalHemisphereLight(Order : Cardinal; Dir : PD3DXVector3; Top, Bottom : TD3DXColor; out ROut, GOut, BOut : Single) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSHEvalHemisphereLight(Order : Cardinal; var Dir : TD3DXVector3; Top, Bottom : TD3DXColor; out ROut, GOut, BOut : Single) : HResult; stdcall; overload; external d3dx9dllname;      

(*)
 *******************************************************************************
 *
 *  Basic Spherical Harmonic projection routines
 *
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 *
 *  D3DXSHProjectCubeMap:
 *  --------------------
 *  Projects a function represented on a cube map into spherical harmonics.
 *
 *  Parameters:
 *   Order
 *      Order of the SH evaluation, generates Order^2 coefs, degree is Order-1
 *   pCubeMap
 *      CubeMap that is going to be projected into spherical harmonics
 *   pROut
 *      Output SH vector for Red.
 *   pGOut
 *      Output SH vector for Green
 *   pBOut
 *      Output SH vector for Blue
 *
 *******************************************************************************
(*)

function D3DXSHProjectCubeMap(Order : Cardinal; CubeMap : IDirect3DCubeTexture9; out ROut, GOut, BOut : Single) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File    : d3dx9core.h
 *  Content : D3DX core types and functions
 *
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 * D3DX_SDK_VERSION:
 * -----------------
 * This identifier is passed to D3DXCheckVersion in order to ensure that an
 * application was built against the correct header files and lib files.
 * This number is incremented whenever a header (or other) change would
 * require applications to be rebuilt. If the version doesn't match,
 * D3DXCreateVersion will return FALSE. (The number itself has no meaning.)
 *******************************************************************************
(*)

const
  D3DX_VERSION     = $0901;
  D3DX_SDK_VERSION = 21;

function D3DXGetDriverLevel(Device : IDirect3DDevice9) : Cardinal; stdcall; external d3dx9inldllname;

function D3DXCheckVersion(const D3DSdkVersion, D3DXSdkVersion : Cardinal) : Bool; stdcall; external d3dx9dllname;

const
  IID_ID3DXBuffer          : TGUID = '{932E6A7E-C68E-45dd-A7BF-53D19C86DB1F}';
  IID_ID3DXFont            : TGUID = '{4AAE6B4D-D15F-4909-B09F-8D6AA34AC06B}';
  IID_ID3DXSprite          : TGUID = '{B07EC84A-8D35-4e86-A9A0-8DFF21D71075}';
  IID_ID3DXRenderToSurface : TGUID = '{0D014791-8863-4c2c-A1C0-02F3E0C0B653}';
  IID_ID3DXRenderToEnvMap  : TGUID = '{4E42C623-9451-44b7-8C86-ABCCDE5D52C8}';
  IID_ID3DXLine            : TGUID = '{72CE4D70-CC40-4143-A896-32E50AD2EF35}';

(*)
 *******************************************************************************
 * ID3DXBuffer:
 * ------------
 * The buffer object is used by D3DX to return arbitrary size data.
 *
 * GetBufferPointer -
 *    Returns a pointer to the beginning of the buffer.
 *
 * GetBufferSize -
 *    Returns the size of the buffer, in bytes.
 *******************************************************************************
(*)

type
  PID3DXBuffer = ^ID3DXBuffer;
  ID3DXBuffer = interface (IUnknown)
    ['{932E6A7E-C68E-45dd-A7BF-53D19C86DB1F}']
    (*** ID3DXBuffer methods ***)
    function GetBufferPointer : Pointer; stdcall;
    function GetBufferSize : LongWord; stdcall;
  end;

(*)
 *******************************************************************************
 * D3DXSPRITE flags:
 * -----------------
 * D3DXSPRITE_DONOTSAVESTATE
 *   Specifies device state is not to be saved and restored in Begin/End.
 * D3DXSPRITE_DONOTMODIFY_RENDERSTATE
 *   Specifies device render state is not to be changed in Begin.  The device
 *   is assumed to be in a valid state to draw vertices containing POSITION0,
 *   TEXCOORD0, and COLOR0 data.
 * D3DXSPRITE_OBJECTSPACE
 *   The WORLD, VIEW, and PROJECTION transforms are NOT modified.  The
 *   transforms currently set to the device are used to transform the sprites
 *   when the batch is drawn (at Flush or End).  If this is not specified,
 *   WORLD, VIEW, and PROJECTION transforms are modified so that sprites are
 *   drawn in screenspace coordinates.
 * D3DXSPRITE_BILLBOARD
 *   Rotates each sprite about its center so that it is facing the viewer.
 * D3DXSPRITE_ALPHABLEND
 *   Enables ALPHABLEND(SRCALPHA, INVSRCALPHA) and ALPHATEST(alpha > 0).
 *   ID3DXFont expects this to be set when drawing text.
 * D3DXSPRITE_SORT_TEXTURE
 *   Sprites are sorted by texture prior to drawing.  This is recommended when
 *   drawing non-overlapping sprites of uniform depth.  For example, drawing
 *   screen-aligned text with ID3DXFont.
 * D3DXSPRITE_SORT_DEPTH_FRONTTOBACK
 *   Sprites are sorted by depth front-to-back prior to drawing.  This is
 *   recommended when drawing opaque sprites of varying depths.
 * D3DXSPRITE_SORT_DEPTH_BACKTOFRONT
 *   Sprites are sorted by depth back-to-front prior to drawing.  This is
 *   recommended when drawing transparent sprites of varying depths.
 *******************************************************************************
(*)

const
  D3DXSPRITE_DONOTSAVESTATE               = (1 shl 0);
  D3DXSPRITE_DONOTMODIFY_RENDERSTATE      = (1 shl 1);
  D3DXSPRITE_OBJECTSPACE                  = (1 shl 2);
  D3DXSPRITE_BILLBOARD                    = (1 shl 3);
  D3DXSPRITE_ALPHABLEND                   = (1 shl 4);
  D3DXSPRITE_SORT_TEXTURE                 = (1 shl 5);
  D3DXSPRITE_SORT_DEPTH_FRONTTOBACK       = (1 shl 6);
  D3DXSPRITE_SORT_DEPTH_BACKTOFRONT       = (1 shl 7);


(*)
 *******************************************************************************
 * ID3DXSprite:
 * ------------
 * This object intends to provide an easy way to drawing sprites using D3D.
 *
 * Begin -
 *    Prepares device for drawing sprites.
 *
 * Draw -
 *    Draws a sprite.  Before transformation, the sprite is the size of
 *    SrcRect, with its top-left corner specified by Position.  The color
 *    and alpha channels are modulated by Color.
 *
 * Flush -
 *    Forces all batched sprites to submitted to the device.
 *
 * End -
 *    Restores device state to how it was when Begin was called.
 *
 * OnLostDevice, OnResetDevice -
 *    Call OnLostDevice() on this object before calling Reset() on the
 *    device, so that this object can release any stateblocks and video
 *    memory resources.  After Reset(), the call OnResetDevice().
 *******************************************************************************
(*)

type
  PID3DXSprite = ^ID3DXSprite;
  ID3DXSprite = interface (IUnknown)
    ['{D4715B38-6C44-472a-9024-6E2B0321CAC6}']
    (*** ID3DXSprite methods ***)
    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;
    function GetTransform(out Transform : TD3DXMatrix) : HResult; stdcall;
    function SetTransform(const Transform : TD3DXMatrix) : HResult; stdcall;

    function SetWorldViewRH(const World, View : TD3DXMatrix) : HResult; stdcall;
    function SetWorldViewLH(const World, View : TD3DXMatrix) : HResult; stdcall;

    function _Begin(Flags : LongWord) : HResult; stdcall;
    function Draw(const Texture : IDirect3DTexture9; const SrcRect : PRect; Center, Position : PD3DXVector3; Color : TD3DColor) : HResult; stdcall;
    function Flush : HResult; stdcall;
    function _End : HResult; stdcall;
    
    function OnLostDevice : HResult; stdcall;
    function OnResetDevice : HResult; stdcall;
  end;

function D3DXCreateSprite(Device : IDirect3DDevice9; out Sprite : ID3DXSprite) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateSprite(Device : IDirect3DDevice9; Sprite : PID3DXSprite) : HResult; stdcall; overload; external d3dx9dllname;
  

(*)
 *******************************************************************************
 * ID3DXFont:
 * ----------
 * Font objects contain the textures and resources needed to render a specific
 * font on a specific device.
 *
 * GetGlyphData -
 *    Returns glyph cache data, for a given glyph.
 *
 * PreloadCharacters/PreloadGlyphs/PreloadText -
 *    Preloads glyphs into the glyph cache textures.
 *
 * DrawText -
 *    Draws formatted text on a D3D device.  Some parameters are
 *    surprisingly similar to those of GDI's DrawText function.  See GDI
 *    documentation for a detailed description of these parameters.
 *    If pSprite is NULL, an internal sprite object will be used.
 *
 * OnLostDevice, OnResetDevice -
 *    Call OnLostDevice() on this object before calling Reset() on the
 *    device, so that this object can release any stateblocks and video
 *    memory resources.  After Reset(), the call OnResetDevice().
 *
 *******************************************************************************
(*)

type
  PD3DXFontDescA = ^TD3DXFontDescA;
  TD3DXFontDescA = packed record
    Height          : Cardinal;
    Width           : Cardinal;
    Weight          : Cardinal;
    MipLevels       : Cardinal;
    Italic          : Bool;
    CharSet         : Byte;
    OutputPrecision : Byte;
    Quality         : Byte;
    PitchAndFamily  : Byte;
    FaceName        : array[0..LF_FACESIZE-1] of Char;
  end;

  PD3DXFont_DescA = ^TD3DXFont_DescA;
  TD3DXFont_DescA = TD3DXFontDescA;
  
type
  PD3DXFontDescW = ^TD3DXFontDescW;
  TD3DXFontDescW = packed record
    Height          : Cardinal;
    Width           : Cardinal;
    Weight          : Cardinal;
    MipLevels       : Cardinal;
    Italic          : Bool;
    CharSet         : Byte;
    OutputPrecision : Byte;
    Quality         : Byte;
    PitchAndFamily  : Byte;
    FaceName        : array[0..LF_FACESIZE-1] of WideChar;
  end;

  PD3DXFont_DescW = ^TD3DXFont_DescW;
  TD3DXFont_DescW = TD3DXFontDescW;

type
{$IFDEF UNICODE}
  TD3DXFontDesc = TD3DXFontDescW;
{$ELSE}
  TD3DXFontDesc = TD3DXFontDescA;
{$ENDIF}

type
  PID3DXFont = ^ID3DXFont;
  ID3DXFont = interface (IUnknown)
    ['{0B8D1536-9EEC-49b0-A5AD-93CF63AFB7C6}']
    (*** ID3DXFont methods ***)

    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;

    function GetDescA(out Desc : TD3DXFontDescA) : HResult; stdcall;
    function GetDescW(out Desc : TD3DXFontDescW) : HResult; stdcall;

    function GetDC : HDC; stdcall;
    function GetGlyphData(Glyph : Cardinal; out Texture : IDirect3DTexture9; out BlackBox : TRect; out CellInc : TPoint) : HResult; stdcall;

    function PreloadCharacters(First, Last : Cardinal) : HResult; stdcall;
    function PreloadGlyphs(First, Last : Cardinal) : HResult; stdcall;
    function PreloadTextA(_String : PAnsiChar; Count : Integer) : HResult; stdcall;
    function PreloadTextW(_String : PWideChar; Count : Integer) : HResult; stdcall;

    function DrawTextA(Sprite : ID3DXSprite; _String : PAnsiChar; Count : Integer; const Rect : TRect; Format : LongWord; Color : TD3DColor) : Integer; stdcall;
    function DrawTextW(Sprite : ID3DXSprite; _String : PWideChar; Count : Integer; const Rect : TRect; Format : LongWord; Color : TD3DColor) : Integer; stdcall;

    function OnLostDevice : HResult; stdcall;
    function OnResetDevice : HResult; stdcall;

(*
#ifdef __cplusplus
#ifdef UNICODE
    HRESULT GetDesc(D3DXFONT_DESCW *pDesc) { return GetDescW(pDesc); }
    HRESULT PreloadText(LPCWSTR pString, INT Count) { return PreloadTextW(pString, Count); }
#else
    HRESULT GetDesc(D3DXFONT_DESCA *pDesc) { return GetDescA(pDesc); }
    HRESULT PreloadText(LPCSTR pString, INT Count) { return PreloadTextA(pString, Count); }
*)
  end;

function D3DXCreateFontA(Device : IDirect3DDevice9; Height : Cardinal; Width : Cardinal; Weight : Cardinal; MipLevels : Cardinal; Italic : Bool; CharSet : LongWord; OutputPrecision : LongWord; Quality : LongWord; PitchAndFamily : LongWord; FaceName : PAnsiChar; out Font : ID3DXFont) : HResult; stdcall; external d3dx9dllname;
function D3DXCreateFontW(Device : IDirect3DDevice9; Height : Cardinal; Width : Cardinal; Weight : Cardinal; MipLevels : Cardinal; Italic : Bool; CharSet : LongWord; OutputPrecision : LongWord; Quality : LongWord; PitchAndFamily : LongWord; FaceName : PWideChar; out Font : ID3DXFont) : HResult; stdcall; external d3dx9dllname;

function D3DXCreateFont(Device : IDirect3DDevice9; Height : Cardinal; Width : Cardinal; Weight : Cardinal; MipLevels : Cardinal; Italic : Bool; CharSet : LongWord; OutputPrecision : LongWord; Quality : LongWord; PitchAndFamily : LongWord; FaceName : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; out Font : ID3DXFont) : HResult; stdcall; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateFontW';{$ELSE}'D3DXCreateFontA';{$ENDIF}

function D3DXCreateFontIndirectA(Device : IDirect3DDevice9; const Desc : TD3DXFontDescA; out Font : ID3DXFont) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateFontIndirectA(Device : IDirect3DDevice9; Desc : PD3DXFontDescA; out Font : ID3DXFont) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateFontIndirectW(Device : IDirect3DDevice9; const Desc : TD3DXFontDescW; out Font : ID3DXFont) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateFontIndirectW(Device : IDirect3DDevice9; Desc : PD3DXFontDescW; out Font : ID3DXFont) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateFontIndirect(Device : IDirect3DDevice9; const Desc : {$IFDEF UNICODE}TD3DXFontDescW{$ELSE}TD3DXFontDescA{$ENDIF}; out Font : ID3DXFont) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateFontIndirectW';{$ELSE}'D3DXCreateFontIndirectA';{$ENDIF}
function D3DXCreateFontIndirect(Device : IDirect3DDevice9; Desc : {$IFDEF UNICODE}PD3DXFontDescW{$ELSE}PD3DXFontDescA{$ENDIF}; out Font : ID3DXFont) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateFontIndirectW';{$ELSE}'D3DXCreateFontIndirectA';{$ENDIF}

(*)
 *******************************************************************************
 * ID3DXRenderToSurface:
 * ---------------------
 * This object abstracts rendering to surfaces.  These surfaces do not
 * necessarily need to be render targets.  If they are not, a compatible
 * render target is used, and the result copied into surface at end scene.
 *
 * BeginScene, EndScene -
 *    Call BeginScene() and EndScene() at the beginning and ending of your
 *    scene.  These calls will setup and restore render targets, viewports,
 *    etc..
 *
 * OnLostDevice, OnResetDevice -
 *    Call OnLostDevice() on this object before calling Reset() on the
 *    device, so that this object can release any stateblocks and video
 *    memory resources.  After Reset(), the call OnResetDevice().
 *******************************************************************************
(*)

type
  TD3DXRTSDesc = packed record
    Width              : Cardinal;
    Height             : Cardinal;
    Format             : TD3DFormat;
    DepthStencil       : BOOL;
    DepthStencilFormat : TD3DFormat;
  end;

  TD3DXRTS_Desc = TD3DXRTSDesc;

type
  PID3DXRenderToSurface = ^ID3DXRenderToSurface;
  ID3DXRenderToSurface = interface (IUnknown)
    ['{0D014791-8863-4c2c-A1C0-02F3E0C0B653}']
    (*** ID3DXRenderToSurface methods ***)
    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;
    function GetDesc(out Desc : TD3DXRTSDesc) : HResult; stdcall;

    function BeginScene(Surface : IDirect3DSurface9; const Viewport : TD3DViewport9) : HResult; stdcall;
    function EndScene : HResult; stdcall;

    function OnLostDevice : HResult; stdcall;
    function OnResetDevice : HResult; stdcall;
  end;

function D3DXCreateRenderToSurface(const Device : IDirect3DDevice9; const Width, Height : Cardinal; Format : TD3DFormat; DepthStencil : BOOL; DepthStencilFormat : TD3DFormat; out RenderToSurface : ID3DXRenderToSurface) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 * ID3DXRenderToEnvMap:
 * --------------------
 * This object abstracts rendering to environment maps.  These surfaces
 * do not necessarily need to be render targets.  If they are not, a
 * compatible render target is used, and the result copied into the
 * environment map at end scene.
 *
 * BeginCube, BeginSphere, BeginHemisphere, BeginParabolic -
 *    This function initiates the rendering of the environment map.  As
 *    parameters, you pass the textures in which will get filled in with
 *    the resulting environment map.
 *
 * Face -
 *    Call this function to initiate the drawing of each face.  For each
 *    environment map, you will call this six times.. once for each face
 *    in D3DCUBEMAP_FACES.
 *
 * End -
 *    This will restore all render targets, and if needed compose all the
 *    rendered faces into the environment map surfaces.
 *
 * OnLostDevice, OnResetDevice -
 *    Call OnLostDevice() on this object before calling Reset() on the
 *    device, so that this object can release any stateblocks and video
 *    memory resources.  After Reset(), the call OnResetDevice().
 *******************************************************************************
(*)

type
  TD3DXRTEDesc = packed record
    Size               : Cardinal;
    MipLevels          : Cardinal;
    Format             : TD3DFormat;
    DepthStencil       : Bool;
    DepthStencilFormat : TD3DFormat;
  end;
  TD3DXRTE_Desc = TD3DXRTEDesc;

type
  PID3DXRenderToEnvMap = ^ID3DXRenderToEnvMap;
  ID3DXRenderToEnvMap = interface (IUnknown)
    ['{4E42C623-9451-44b7-8C86-ABCCDE5D52C8}']
    (*** ID3DXRenderToEnvMap methods ***)

    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;
    function GetDesc(out Desc : TD3DXRTEDesc) : HResult; stdcall;

    function BeginCube(CubeTex : IDirect3DCubeTexture9) : HResult; stdcall;

    function BeginSphere(Tex : IDirect3DTexture9) : HResult; stdcall;

    function BeginHemisphere(TexZPos, TexZNeg : IDirect3DTexture9) : HResult; stdcall;

    function BeginParabolic(TexZPos, TexZNeg : IDirect3DTexture9) : HResult; stdcall;

    function Face(Face : TD3DCubeMapFaces; const MipFilter : LongWord) : HResult; stdcall;
    function _End(MipFilter : LongWord) : HResult; stdcall;

    function OnLostDevice : HResult; stdcall;
    function OnResetDevice : HResult; stdcall;
  end;

function D3DXCreateRenderToEnvMap(Device : IDirect3DDevice9; const Size, MipLevels : Cardinal; Format : TD3DFormat; DepthStencil : Bool; DepthStencilFormat : TD3DFormat; out RenderToEnvMap : ID3DXRenderToEnvMap) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 * ID3DXLine:
 * ------------
 * This object intends to provide an easy way to draw lines using D3D.
 *
 * Begin -
 *    Prepares device for drawing lines
 *
 * Draw -
 *    Draws a line strip in screen-space.
 *    Input is in the form of a array defining points on the line strip. of D3DXVECTOR2
 *
 * DrawTransform -
 *    Draws a line in screen-space with a specified input transformation matrix.
 *
 * End -
 *     Restores device state to how it was when Begin was called.
 *
 * SetPattern -
 *     Applies a stipple pattern to the line.  Input is one 32-bit
 *     DWORD which describes the stipple pattern. 1 is opaque, 0 is
 *     transparent.
 *
 * SetPatternScale -
 *     Stretches the stipple pattern in the u direction.  Input is one
 *     floating-point value.  0.0f is no scaling, whereas 1.0f doubles
 *     the length of the stipple pattern.
 *
 * SetWidth -
 *     Specifies the thickness of the line in the v direction.  Input is
 *     one floating-point value.
 *
 * SetAntialias -
 *     Toggles line antialiasing.  Input is a BOOL.
 *     TRUE  = Antialiasing on.
 *     FALSE = Antialiasing off.
 *
 * SetGLLines -
 *     Toggles non-antialiased OpenGL line emulation.  Input is a BOOL.
 *     TRUE  = OpenGL line emulation on.
 *     FALSE = OpenGL line emulation off.
 *
 * OpenGL line:     Regular line:
 *   *\                *\
 *   | \              /  \
 *   |  \            *\   \
 *   *\  \             \   \
 *     \  \             \   \
 *      \  *             \   *
 *       \ |              \ /
 *        \|               *
 *         *
 *
 * OnLostDevice, OnResetDevice -
 *    Call OnLostDevice() on this object before calling Reset() on the
 *    device, so that this object can release any stateblocks and video
 *    memory resources.  After Reset(), the call OnResetDevice().
 *******************************************************************************
(*)

type
  PID3DXLine = ^ID3DXLine;
  ID3DXLine = interface (IUnknown)
    ['{4E42C623-9451-44b7-8C86-ABCCDE5D52C8}']
    (*** ID3DXLine methods ***)
    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;

    function _Begin : HResult; stdcall;

    function Draw(VertexList : PD3DXVector2; VertexListCount : LongWord; Color : TD3DColor) : HResult; stdcall;

    function DrawTransform(VertexList : PD3DXVector3; VertexListCount : LongWord; const Transform : TD3DXMatrix; Color : TD3DColor) : HResult; stdcall;

    function SetPattern(const Pattern : LongWord) : HResult; stdcall;
    function GetPattern : LongWord; stdcall;

    function SetPatternScale(constPatternScale : Single) : HResult; stdcall;
    function GetPatternScale : Single; stdcall;

    function SetWidth(constWidth : Single) : HResult; stdcall;
    function GetWidth : Single; stdcall;

    function SetAntialias(const Antialias : Bool) : HResult; stdcall;
    function GetAntialias : Bool; stdcall;

    function SetGLLines(const GLLines : Bool) : HResult; stdcall;
    function GetGLLines : Bool; stdcall;

    function _End : HResult; stdcall;

    function OnLostDevice : HResult; stdcall;
    function OnResetDevice : HResult; stdcall;
 end;

function D3DXCreateLine(Device : IDirect3DDevice9; out Line : ID3DXLine) : HResult; stdcall; external d3dx9dllname;


(*)
 *******************************************************************************
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       d3dx9mesh.h
 *  Content:    D3DX mesh types and functions
 *
 *******************************************************************************
(*)
const
  IID_ID3DXBaseMesh  : TGUID = '{4F5621A3-7F25-46dc-8239-820B823795CB}';
  IID_ID3DXMesh      : TGUID = '{29E3EB8D-4DD6-4524-B1A2-1EF0581E778D}';
  IID_ID3DXPMesh     : TGUID = '{1DA4801F-A26E-4623-BD54-71FF57F4BD02}';
  IID_ID3DXSPMesh    : TGUID = '{1C4E77C5-8391-4951-A019-D4C5A9539EEC}';
  IID_ID3DXSkinInfo  : TGUID = '{0E7DBBF3-421A-4dd8-B738-A5DAC3A48767}';
  IID_ID3DXPatchMesh : TGUID = '{0AD3E8BC-290D-4dc7-91AB-73A82755B13E}';

{$MINENUMSIZE 1}
type
  //patch mesh can be quads or tris
  TD3DXPatchMeshType = {$IFNDEF NOENUMS}({$ELSE}Byte;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DXPATCHMESH_RECT        = $001{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPATCHMESH_TRI         = $002{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPATCHMESH_NPATCH      = $003{$IFNDEF NOENUMS}){$ENDIF};

{$MINENUMSIZE 4}

// Mesh options - lower 3 bytes only, upper byte used by _D3DXMESHOPT option flags
type
  TD3DXMesh = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DXMESH_32BIT                  = $00001{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // If set, then use 32 bit indices, if not set use 16 bit indices.
    D3DXMESH_DONOTCLIP              = $00002{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Use D3DUSAGE_DONOTCLIP for VB & IB.
    D3DXMESH_POINTS                 = $00004{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Use D3DUSAGE_POINTS for VB & IB.
    D3DXMESH_RTPATCHES              = $00008{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Use D3DUSAGE_RTPATCHES for VB & IB.
    D3DXMESH_NPATCHES               = $04000{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Use D3DUSAGE_NPATCHES for VB & IB.
    D3DXMESH_VB_SYSTEMMEM           = $00010{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Use D3DPOOL_SYSTEMMEM for VB. Overrides D3DXMESH_MANAGEDVERTEXBUFFER
    D3DXMESH_VB_MANAGED             = $00020{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Use D3DPOOL_MANAGED for VB.
    D3DXMESH_VB_WRITEONLY           = $00040{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Use D3DUSAGE_WRITEONLY for VB.
    D3DXMESH_VB_DYNAMIC             = $00080{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Use D3DUSAGE_DYNAMIC for VB.
    D3DXMESH_VB_SOFTWAREPROCESSING  = $08000{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Use D3DUSAGE_SOFTWAREPROCESSING for VB.
    D3DXMESH_IB_SYSTEMMEM           = $00100{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Use D3DPOOL_SYSTEMMEM for IB. Overrides D3DXMESH_MANAGEDINDEXBUFFER
    D3DXMESH_IB_MANAGED             = $00200{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Use D3DPOOL_MANAGED for IB.
    D3DXMESH_IB_WRITEONLY           = $00400{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Use D3DUSAGE_WRITEONLY for IB.
    D3DXMESH_IB_DYNAMIC             = $00800{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Use D3DUSAGE_DYNAMIC for IB.
    D3DXMESH_IB_SOFTWAREPROCESSING  = $10000{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Use D3DUSAGE_SOFTWAREPROCESSING for IB.

    D3DXMESH_VB_SHARE               = $01000{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Valid for Clone* calls only, forces cloned mesh/pmesh to share vertex buffer

    D3DXMESH_USEHWONLY              = $02000{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // Valid for ID3DXSkinMesh::ConvertToBlendedMesh

    // Helper options
    D3DXMESH_SYSTEMMEM              = $00110{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // D3DXMESH_VB_SYSTEMMEM | D3DXMESH_IB_SYSTEMMEM
    D3DXMESH_MANAGED                = $00220{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // D3DXMESH_VB_MANAGED | D3DXMESH_IB_MANAGED
    D3DXMESH_WRITEONLY              = $00440{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // D3DXMESH_VB_WRITEONLY | D3DXMESH_IB_WRITEONLY
    D3DXMESH_DYNAMIC                = $00880{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // D3DXMESH_VB_DYNAMIC | D3DXMESH_IB_DYNAMIC
    D3DXMESH_SOFTWAREPROCESSING     = $18000{$IFNDEF NOENUMS}){$ENDIF};        // D3DXMESH_VB_SOFTWAREPROCESSING | D3DXMESH_IB_SOFTWAREPROCESSING

type    
  //patch mesh options
  TD3DXPatchMesh = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DXPATCHMESH_DEFAULT = 000{$IFNDEF NOENUMS}){$ENDIF};

// option field values for specifying min value in D3DXGeneratePMesh and D3DXSimplifyMesh
{$MINENUMSIZE 1}
type
  TD3DXMeshSimp = {$IFNDEF NOENUMS}({$ELSE}Byte;{$ENDIF}
  {$IFDEF NOENUMS}const{$ENDIF}
    D3DXMESHSIMP_VERTEX   = $1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXMESHSIMP_FACE     = $2{$IFNDEF NOENUMS}){$ENDIF};

type
  TMAXFVFDECLSize = {$IFNDEF NOENUMS}({$ELSE}Byte;{$ENDIF}
  {$IFDEF NOENUMS}const{$ENDIF}
    MAX_FVF_DECL_SIZE = MAXD3DDECLLENGTH + 1{$IFNDEF NOENUMS}){$ENDIF}; // +1 for END
{$MINENUMSIZE 4}
type
  PD3DXAttributeRange = ^TD3DXAttributeRange;
  TD3DXAttributeRange = packed record
    AttribId    : LongWord;
    FaceStart   : LongWord;
    FaceCount   : LongWord;
    VertexStart : LongWord;
    VertexCount : LongWord;
  end;

type
  PD3DXMaterial = ^TD3DXMaterial;
  TD3DXMaterial = packed record
    MatD3D          : TD3DMaterial9;
    TextureFilename : PAnsiChar;
  end;

type
  TD3DXEffectDefaultType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
  {$IFDEF NOENUMS}const{$ENDIF}
    D3DXEDT_STRING = $1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // pValue points to a null terminated ASCII string
    D3DXEDT_FLOATS = $2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // pValue points to an array of floats - number of floats is NumBytes / sizeof(float)
    D3DXEDT_DWORD  = $3{$IFNDEF NOENUMS}){$ENDIF};        // pValue points to a DWORD

type
  PD3DXEffectDefault = ^TD3DXEffectDefault;
  TD3DXEffectDefault = packed record
    ParamName : PAnsiChar;
    _Type     : TD3DXEffectDefaultType ;  // type of the data pointed to by pValue
    NumBytes  : LongWord;                 // size in bytes of the data pointed to by pValue
    Value     : Pointer;                  // data for the default of the effect
  end;

type
  PD3DXEffectInstance = ^TD3DXEffectInstance;
  TD3DXEffectInstance = packed record
    EffectFilename : PAnsiChar;
    NumDefaults    : LongWord;
    Defaults       : PD3DXEffectDefault;
  end;

type
  PD3DXAttributeWeights = ^TD3DXAttributeWeights;
  TD3DXAttributeWeights = packed record
    Position : Single;
    Boundary : Single;
    Normal   : Single;
    Diffuse  : Single;
    Specular : Single;
    TexCoord : array[0..7] of Single;
    Tangent  : Single;
    Binormal : Single;
  end;

{$MINENUMSIZE 1}
type
  TD3DXWeldEpsilonsFlags = {$IFNDEF NOENUMS}({$ELSE}Byte;{$ENDIF}
  {$IFDEF NOENUMS}const{$ENDIF}

    D3DXWELDEPSILONS_WELDALL             = $1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   // weld all vertices marked by adjacency as being overlapping
    D3DXWELDEPSILONS_WELDPARTIALMATCHES  = $2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   // if a given vertex component is within epsilon, modify partial matched
                                                                                  // vertices so that both components identical AND if all components "equal"
                                                                                  // remove one of the vertices
    D3DXWELDEPSILONS_DONOTREMOVEVERTICES = $4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}   // instructs weld to only allow modifications to vertices and not removal
                                                                                  // ONLY valid if D3DXWELDEPSILONS_WELDPARTIALMATCHES is set
                                                                                  // useful to modify vertices to be equal, but not allow vertices to be removed

    D3DXWELDEPSILONS_DONOTSPLIT          = $8{$IFNDEF NOENUMS}){$ENDIF};          // instructs weld to specify the D3DXMESHOPT_DONOTSPLIT flag when doing an Optimize(ATTR_SORT)
                                                                                  // if this flag is not set, all vertices that are in separate attribute groups
                                                                                  // will remain split and not welded.  Setting this flag can slow down software vertex processing

{$MINENUMSIZE 4}
type
  PD3DXWeldEpsilons = ^TD3DXWeldEpsilons;
  TD3DXWeldEpsilons = packed record
    Position      : Single;   // NOTE: This does NOT replace the epsilon in GenerateAdjacency
                              // in general, it should be the same value or greater than the one passed to GeneratedAdjacency
    BlendWeights : Single;
    Normal       : Single;
    PSize        : Single;
    Specular     : Single;
    Diffuse      : Single;
    TexCoord     : array[0..7] of Single;
    Tangent      : Single;
    Binormal     : Single;
    TessFactor   : Single;
  end;


type
  PFVFDeclaration = ^TFVFDeclaration;
  TFVFDeclaration = array [0..LongWord(MAX_FVF_DECL_SIZE) - 1] of TD3DVertexElement9;

  ID3DXMesh = interface;

  ID3DXBaseMesh = interface (IUnknown)
    ['{2A835771-BF4D-43f4-8E14-82A809F17D8A}']
    {*** ID3DXBaseMesh methods ***}
    function DrawSubset(const AttribId : LongWord) : HResult; stdcall;
    function GetNumFaces : LongWord; stdcall;
    function GetNumVertices : LongWord; stdcall;
    function GetFVF : LongWord; stdcall;
    function GetDeclaration(Declaration : TFVFDeclaration) : HResult; stdcall;
    function GetNumBytesPerVertex : LongWord; stdcall;
    function GetOptions : LongWord; stdcall;
    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;
    function CloneMeshFVF(const Options, FVF : LongWord; D3DDevice : IDirect3DDevice9; out CloneMesh : ID3DXMesh) : HResult; stdcall;
    function CloneMesh(const Options : LongWord; Declaration : PD3DVertexElement9; D3DDevice : IDirect3DDevice9; out CloneMesh : ID3DXMesh) : HResult; stdcall;
    function GetVertexBuffer(out VB : IDirect3DVertexBuffer9) : HResult; stdcall;
    function GetIndexBuffer(out IB : IDirect3DIndexBuffer9) : HResult; stdcall;
    function LockVertexBuffer(const Flags : LongWord; out Data : Pointer) : HResult; stdcall;
    function UnlockVertexBuffer : HResult; stdcall;
    function LockIndexBuffer(const Flags : LongWord; out Data : Pointer) : HResult; stdcall;
    function UnlockIndexBuffer : HResult; stdcall;
    function GetAttributeTable(AttribTable : PD3DXAttributeRange; var AttribTableSize : LongWord) : HResult; stdcall;

    function ConvertPointRepsToAdjacency(PRep : PLongWord; out Adjacency : LongWord) : HResult; stdcall;
    function ConvertAdjacencyToPointReps(Adjacency : PLongWord; out PRep : LongWord) : HResult; stdcall;
    function GenerateAdjacency(const Epsilon : Single; Adjacency : PLongWord) : HResult; stdcall;

    function UpdateSemantics(Declaration : TFVFDeclaration) : HResult; stdcall;
  end;

  PID3DXMesh = ^ID3DXMesh;
  ID3DXMesh = interface (ID3DXBaseMesh)
    ['{CCAE5C3B-4DD1-4d0f-997E-4684CA64557F}']
    {*** ID3DXMesh methods ***}
    function LockAttributeBuffer(const Flags : LongWord; out Data : PLongWord) : HResult; stdcall;
    function UnlockAttributeBuffer : HResult; stdcall;
    function Optimize(const Flags : LongWord; AdjacencyIn, AdjacencyOut, FaceRemap : PLongWord; out VertexRemap : ID3DXBuffer; out OptMesh : ID3DXMesh) : HResult; stdcall;
    function OptimizeInplace(const Flags : LongWord; AdjacencyIn, AdjacencyOut, FaceRemap : PLongWord; VertexRemap : PID3DXBuffer) : HResult; stdcall;
    function SetAttributeTable(AttribTable : PD3DXAttributeRange; const AttribTableSize : LongWord) : HResult; stdcall;
  end;

  ID3DXPMesh = interface (ID3DXBaseMesh)
    ['{19FBE386-C282-4659-97BD-CB869B084A6C}']
    {*** ID3DXPMesh methods ***}
    function ClonePMeshFVF(const Options, FVF : LongWord; D3D : IDirect3DDevice9; out CloneMesh : ID3DXPMesh) : HResult; stdcall;
    function ClonePMesh(const Options : LongWord; Declaration : PD3DVertexElement9; D3D : IDirect3DDevice9; out CloneMesh : ID3DXPMesh) : HResult; stdcall;
    function SetNumFaces(const Faces : LongWord) : HResult; stdcall;
    function SetNumVertices(const Vertices : LongWord) : HResult; stdcall;
    function GetMaxFaces : LongWord; stdcall;
    function GetMinFaces : LongWord; stdcall;
    function GetMaxVertices : LongWord; stdcall;
    function GetMinVertices : LongWord; stdcall;
    function Save(const Stream : IStream; const Materials : TD3DXMaterial; EffectInstances : PD3DXEffectInstance; const NumMaterials : LongWord) : HResult; stdcall;

    function Optimize(const Flags : LongWord; AdjacencyOut, FaceRemap : PLongWord; out VertexRemap : ID3DXBuffer; out OptMesh : ID3DXMesh) : HResult; stdcall;

    function OptimizeBaseLOD(const Flags : LongWord; FaceRemap : PLongWord) : HResult; stdcall;
    function TrimByFaces(const NewFacesMin, NewFacesMax : LongWord; rgiFaceRemap, rgiVertRemap : PLongWord) : HResult; stdcall;
    function TrimByVertices(const NewVerticesMin, NewVerticesMax : LongWord; rgiFaceRemap, rgiVertRemap : PLongWord) : HResult; stdcall;

    function GetAdjacency(out Adjacency : LongWord) : HResult; stdcall;

    //  Used to generate the immediate "ancestor" for each vertex when it is removed by a vsplit.  Allows generation of geomorphs
    //     Vertex buffer must be equal to or greater than the maximum number of vertices in the pmesh
    function GenerateVertexHistory(VertexHistory : PLongWord) : HResult; stdcall;
  end;

  ID3DXSPMesh = interface (IUnknown)
    ['{4E3CA05C-D4FF-4d11-8A02-16459E08F6F4}']
    {*** ID3DXSPMesh methods ***}
    function GetNumFaces : LongWord; stdcall;
    function GetNumVertices : LongWord; stdcall;
    function GetFVF : LongWord; stdcall;
    function GetDeclaration(Declaration : TFVFDeclaration) : HResult; stdcall;
    function GetOptions : LongWord; stdcall;
    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;
    function CloneMeshFVF(const Options, FVF : LongWord; const D3D : IDirect3DDevice9; AdjacencyOut, VertexRemapOut : PLongWord; out CloneMesh : ID3DXMesh) : HResult; stdcall;
    function CloneMesh(const Options : LongWord; Declaration : PD3DVertexElement9; const D3DDevice : IDirect3DDevice9; AdjacencyOut, VertexRemapOut : PLongWord; out CloneMesh : ID3DXMesh) : HResult; stdcall;

    function ClonePMeshFVF(const Options, FVF : LongWord; const D3D : IDirect3DDevice9; VertexRemapOut : PLongWord; ErrorsByFace : Single; out CloneMesh : ID3DXMesh) : HResult; stdcall;

    function ClonePMesh(const Options : LongWord; var Declaration : LongWord; const D3DDevice : IDirect3DDevice9; VertexRemapOut : PLongWord; ErrorsByFace : Single; out CloneMesh : ID3DXMesh) : HResult; stdcall;

    function ReduceFaces(const Faces : LongWord) : HResult; stdcall;
    function ReduceVertices(const Vertices : LongWord) : HResult; stdcall;
    function GetMaxFaces : LongWord; stdcall;
    function GetMaxVertices : LongWord; stdcall;
    function GetVertexAttributeWeights(out VertexAttributeWeights : TD3DXAttributeWeights) : HResult; stdcall;
    function GetVertexWeights(VertexWeights : PSingle) : HResult; stdcall;
  end;  

const
  UNUSED16 = $ffff;
  UNUSED32 = $ffffffff;

// ID3DXMesh::Optimize options - upper byte only, lower 3 bytes used from _D3DXMESH option flags
type TD3DXMeshOpt = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
  {$IFDEF NOENUMS}const{$ENDIF}
    D3DXMESHOPT_COMPACT           = $01000000{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXMESHOPT_ATTRSORT          = $02000000{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXMESHOPT_VERTEXCACHE       = $04000000{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXMESHOPT_STRIPREORDER      = $08000000{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXMESHOPT_IGNOREVERTS       = $10000000{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // optimize faces only, don't touch vertices
    D3DXMESHOPT_DONOTSPLIT        = $20000000{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // do not split vertices shared between attribute groups when attribute sorting
    D3DXMESHOPT_DEVICEINDEPENDENT = $00400000{$IFNDEF NOENUMS}){$ENDIF};         // Only affects VCache.  uses a static known good cache size for all cards

    // D3DXMESHOPT_SHAREVB has been removed, please use D3DXMESH_VB_SHARE instead

// Subset of the mesh that has the same attribute and bone combination.
// This subset can be rendered in a single draw call
type
  PD3DXBoneCombination = ^TD3DXBoneCombination;
  TD3DXBoneCombination = packed record
    AttribId    : LongWord;
    FaceStart   : LongWord;
    FaceCount   : LongWord;
    VertexStart : LongWord;
    VertexCount : LongWord;
    BoneId      : PLongWord;
  end;

// The following types of patch combinations are supported:
// Patch type   Basis       Degree
// Rect         Bezier      2,3,5
// Rect         B-Spline    2,3,5
// Rect         Catmull-Rom 3
// Tri          Bezier      2,3,5
// N-Patch      N/A         3

type
  PD3DXPatchInfo = ^TD3DXPatchInfo;
  TD3DXPatchInfo = packed record
    PatchType : TD3DXPatchMeshType;
    Degree    : TD3DDegreeType;
    Basis     : TD3DBasisType;
  end;

type
  ID3DXPatchMesh = interface;
  ID3DXPatchMesh = interface (IUnknown)
    ['{0AD3E8BC-290D-4dc7-91AB-73A82755B13E}']
    {*** ID3DXPatchMesh methods ***}
    // Return creation parameters
    function GetNumPatches : LongWord; stdcall;
    function GetNumVertices : LongWord; stdcall;
    function GetDeclaration(Declaration : PD3DVertexElement9) : HResult; stdcall;
    function GetControlVerticesPerPatch : LongWord; stdcall;
    function GetOptions : LongWord; stdcall;
    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;
    function GetPatchInfo(out PatchInfo : TD3DXPatchInfo) : HResult; stdcall;

    // Control mesh access
    function GetVertexBuffer(out VB : IDirect3DVertexBuffer9) : HResult; stdcall;
    function GetIndexBuffer(out IB : IDirect3DIndexBuffer9) : HResult; stdcall;
    function LockVertexBuffer(const Flags : LongWord; out Data : Pointer) : HResult; stdcall;
    function UnlockVertexBuffer : HResult; stdcall;
    function LockIndexBuffer(const Flags : LongWord; out Data : Pointer) : HResult; stdcall;
    function UnlockIndexBuffer : HResult; stdcall;
    function LockAttributeBuffer(const Flags : LongWord; out Data : PLongWord) : HResult; stdcall;
    function UnlockAttributeBuffer : HResult; stdcall;

    // This function returns the size of the tessellated mesh given a tessellation level.
    // This assumes uniform tessellation. For adaptive tessellation the Adaptive parameter must
    // be set to TRUE and TessellationLevel should be the max tessellation.
    // This will result in the max mesh size necessary for adaptive tessellation.
    function GetTessSize(const TessLevel : Single; const Adapative : LongWord; out NumTriangles, NumVertices : LongWord) : HResult; stdcall;

    //GenerateAdjacency determines which patches are adjacent with provided tolerance
    //this information is used internally to optimize tessellation
    function GenerateAdjacency(const Tolerance : Single) : HResult; stdcall;

    //CloneMesh Creates a new patchmesh with the specified decl, and converts the vertex buffer
    //to the new decl. Entries in the new decl which are new are set to 0. If the current mesh
    //has adjacency, the new mesh will also have adjacency
    function CloneMesh(const Options : LongWord; Decl : PD3DVertexElement9Array; out Mesh : ID3DXPatchMesh) : HResult; stdcall;
    
    // Optimizes the patchmesh for efficient tessellation. This function is designed
    // to perform one time optimization for patch meshes that need to be tessellated
    // repeatedly by calling the Tessellate() method. The optimization performed is
    // independent of the actual tessellation level used.
    // Currently Flags is unused.
    // If vertices are changed, Optimize must be called again
    function Optimize(const Flags : LongWord) : HResult; stdcall;

    //gets and sets displacement parameters
    //displacement maps can only be 2D textures MIP-MAPPING is ignored for non adapative tessellation
    function SetDisplaceParam(Texture : IDirect3DBaseTexture9; MinFilter, MagFilter, MipFilter : TD3DTextureFilterType; Wrap : TD3DTextureAddress; const LODBias : LongWord) : HResult; stdcall;

    function GetDisplaceParam(out Texture : IDirect3DBaseTexture9; out MinFilter, MagFilter, MipFilter : TD3DTextureFilterType; out Wrap : TD3DTextureAddress; out LODBias : LongWord)  : HResult; stdcall;

    // Performs the uniform tessellation based on the tessellation level.
    // This function will perform more efficiently if the patch mesh has been optimized using the Optimize() call.
    function Tessellate(const TessLevel : Single; Mesh : ID3DXMesh) : HResult; stdcall;

    // Performs adaptive tessellation based on the Z based adaptive tessellation criterion.
    // pTrans specifies a 4D vector that is dotted with the vertices to get the per vertex
    // adaptive tessellation amount. Each edge is tessellated to the average of the criterion
    // at the 2 vertices it connects.
    // MaxTessLevel specifies the upper limit for adaptive tesselation.
    // This function will perform more efficiently if the patch mesh has been optimized using the Optimize() call.
    function TessellateAdaptive(var Trans : TD3DXVector4; const MaxTessLevel, MinTessLevel : LongWord; Mesh : ID3DXMesh) : HResult; stdcall;
  end;

type
  ID3DXSkinInfo = interface;
  ID3DXSkinInfo = interface (IUnknown)
    ['{0E7DBBF3-421A-4dd8-B738-A5DAC3A48767}']
    {*** ID3DXSkinInfo methods ***}

    // Specify the which vertices do each bones influence and by how much
    function SetBoneInfluence(const Bone, NumInfluences : LongWord; Vertices : PLongWord; Weights : PSingle) : HResult; stdcall;
    function GetNumBoneInfluences(const Bone : LongWord) : LongWord; stdcall;
    function GetBoneInfluence(const Bone : LongWord; Vertices : PLongWord; Weights : PSingle) : HResult; stdcall;
    function GetMaxVertexInfluences(MaxVertexInfluences : PLongWord) : HResult; stdcall;
    function GetNumBones : LongWord; stdcall;

    // This gets the max face influences based on a triangle mesh with the specified index buffer
    function GetMaxFaceInfluences(IB : IDirect3DIndexBuffer9; const NumFaces : LongWord; MaxFaceInfluences : PLongWord) : HResult; stdcall;

    // Set min bone influence. Bone influences that are smaller than this are ignored
    function SetMinBoneInfluence(const MinInfl : Single) : HResult; stdcall;
    // Get min bone influence.
    function GetMinBoneInfluence : Single; stdcall;

    // Bone names are returned by D3DXLoadSkinMeshFromXof. They are not used by any other method of this object
    function SetBoneName(const Bone : LongWord; Name : PChar) : HResult; stdcall; // Name is copied to an internal string buffer
    function GetBoneName(const Bone : LongWord) : PChar; stdcall; // A pointer to an internal string buffer is returned. Do not free this.

    // Bone offset matrices are returned by D3DXLoadSkinMeshFromXof. They are not used by any other method of this object
    function SetBoneOffsetMatrix(const Bone : LongWord; const BoneTransform : TD3DXMatrix) : HResult; stdcall; // BoneTransform is copied to an internal buffer
    function GetBoneOffsetMatrix(const Bone : LongWord) : PD3DXMatrix; stdcall; // A pointer to an internal matrix is returned. Do not free this.

    // Clone a skin info object
    function Clone(out SkinInfo : ID3DXSkinInfo) : HResult; stdcall;

    // Update bone influence information to match vertices after they are reordered. This should be called
    // if the target vertex buffer has been reordered externally.
    function Remap(const NumVertices : LongWord; VertexRemap : PLongWord) : HResult; stdcall;

    // These methods enable the modification of the vertex layout of the vertices that will be skinned
    function SetFVF(const FVF : LongWord) : HResult; stdcall;
    function SetDeclaration(Declaration : PD3DVertexElement9) : HResult; stdcall;
    function GetFVF : LongWord; stdcall;
    function GetDeclaration(out Declaration : TFVFDeclaration) : HResult; stdcall;

    // Apply SW skinning based on current pose matrices to the target vertices.
    function UpdateSkinnedMesh(const BoneTransforms, BoneInvTransposeTransforms : TD3DXMatrix; VerticesSrc, VerticesDst : Pointer) : HResult; stdcall;

    // Takes a mesh and returns a new mesh with per vertex blend weights and a bone combination
    // table that describes which bones affect which subsets of the mesh
    function ConvertToBlendedMesh(Mesh : ID3DXMesh; const Options : LongWord; AdjacencyIn, AdjacencyOut, FaceRemap : PLongWord; VertexRemap : PID3DXBuffer; out MaxFaceInfl, NumBoneCombinations : LongWord; out BoneCombinationTable : ID3DXBuffer; out BlendedMesh : ID3DXMesh) : HResult; stdcall;

    // Takes a mesh and returns a new mesh with per vertex blend weights and indices
    // and a bone combination table that describes which bones palettes affect which subsets of the mesh
    function ConvertToIndexedBlendedMesh(Mesh : ID3DXMesh; const Options, PaletteSize : LongWord; AdjacencyIn, AdjacencyOut, FaceRemap : PLongWord; VertexRemap : PID3DXBuffer; out MaxVertexInfl, NumBoneCombinations : LongWord; out BoneCombinationTable : ID3DXBuffer; out IndexedBlendedMesh : ID3DXMESH) : HResult; stdcall;
  end;

  PID3DXSkinInfo = ^ID3DXSkinInfo;

function D3DXCreateMesh(const NumFaces, NumVertices, Options : LongWord; Declaration : PD3DVertexElement9; D3D : IDirect3DDevice9; out Mesh : ID3DXMesh) : HResult; stdcall; external d3dx9dllname;

function D3DXCreateMeshFVF(const NumFaces, NumVertices, Options, FVF : LongWord; D3D : IDirect3DDevice9; out Mesh : ID3DXMesh) : HResult; stdcall; external d3dx9dllname;

function D3DXCreateSPMesh(Mesh : ID3DXMesh; Adjacency : PLongWord; VertexAttributeWeights : PD3DXAttributeWeights; VertexWeights : PSingle; out SMesh : ID3DXSPMesh) : HResult; stdcall; external d3dx9dllname;

// clean a mesh up for simplification, try to make manifold
function D3DXCleanMesh(MeshIn : ID3DXMesh; AdjacencyIn : PLongWord; out MeshOut : ID3DXMesh; AdjacencyOut : PLongWord; out ErrorsAndWarnings : ID3DXBuffer) : HResult; stdcall; external d3dx9dllname;

function D3DXValidMesh(MeshIn : ID3DXMesh; Adjacency : PLongWord; out ErrorsAndWarnings : ID3DXBuffer) : HResult; stdcall; external d3dx9dllname;

function D3DXGeneratePMesh(Mesh : ID3DXMesh; Adjacency : PLongWord; VertexAttributeWeights : PD3DXAttributeWeights; VertexWeights : PSingle; const MinValue, Options : LongWord; out PMesh : ID3DXPMesh) : HResult; stdcall; external d3dx9dllname;

function D3DXSimplifyMesh(Mesh : ID3DXMesh; Adjacency : PLongWord; VertexAttributeWeights : PD3DXAttributeWeights; VertexWeights : PSingle; const MinValue, Options : LongWord; out SimplifiedMesh : ID3DXMesh) : HResult; stdcall; external d3dx9dllname;

function D3DXComputeBoundingSphere(FirstPosition : PD3DXVector3; const NumVertices, Stride : LongWord; Center : PD3DXVector3; out Radius : Single) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXComputeBoundingSphere(const FirstPosition : TD3DXVector3; const NumVertices, Stride : LongWord; Center : PD3DXVector3; out Radius : Single) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXComputeBoundingSphere(FirstPosition : PD3DXVector3; const NumVertices, Stride : LongWord; out Center : TD3DXVector3; out Radius : Single) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXComputeBoundingSphere(const FirstPosition : TD3DXVector3; const NumVertices, Stride : LongWord; out Center : TD3DXVector3; out Radius : Single) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXComputeBoundingBox(FirstPosition : PD3DXVector3; const NumVertices, Stride : LongWord; Min, Max : PD3DXVector3) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXComputeBoundingBox(const FirstPosition : TD3DXVector3; const NumVertices, Stride : LongWord; Min, Max : PD3DXVector3) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXComputeBoundingBox(FirstPosition : PD3DXVector3; const NumVertices, Stride : LongWord; out Min : TD3DXVector3; Max : PD3DXVector3) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXComputeBoundingBox(const FirstPosition : TD3DXVector3; const NumVertices, Stride : LongWord; out Min : TD3DXVector3; Max : PD3DXVector3) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXComputeBoundingBox(FirstPosition : PD3DXVector3; const NumVertices, Stride : LongWord; Min : PD3DXVector3; out Max : TD3DXVector3) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXComputeBoundingBox(const FirstPosition : TD3DXVector3; const NumVertices, Stride : LongWord; Min : PD3DXVector3; out Max : TD3DXVector3) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXComputeBoundingBox(FirstPosition : PD3DXVector3; const NumVertices, Stride : LongWord; out Min, Max : TD3DXVector3) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXComputeBoundingBox(const FirstPosition : TD3DXVector3; const NumVertices, Stride : LongWord; out Min, Max : TD3DXVector3) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXComputeNormals(Mesh : ID3DXBaseMesh; Adjacency : PLongWord) : HResult; stdcall; external d3dx9dllname;

function D3DXCreateBuffer(const NumBytes : LongWord; out Buffer : ID3DXBuffer) : HResult; stdcall; external d3dx9dllname;

function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials, EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials, EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials, EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials, EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials, EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials, EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials, EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials, EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials, EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials, EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials, EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials, EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials, EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials, EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials, EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXA(Filename : PAnsiChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials, EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials, EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials, EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials, EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials, EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials, EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials, EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials, EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials, EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials, EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials, EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials, EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials, EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials, EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials, EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials, EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;
function D3DXLoadMeshFromXW(Filename : PWideChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials, EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname;

function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials, EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials, EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials, EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials, EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials, EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials, EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials, EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials, EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; Mesh : PID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials, EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials, EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials, EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials, EffectInstances : ID3DXBuffer; NumMaterials : PLongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials, EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials, EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials : ID3DXBuffer; EffectInstances : PID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency, Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency : ID3DXBuffer; Materials : PID3DXBuffer; out EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; Adjacency : PID3DXBuffer; out Materials, EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}
function D3DXLoadMeshFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials, EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall;  overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshFromXW';{$ELSE}'D3DXLoadMeshFromXA';{$ENDIF}

function D3DXLoadMeshFromXInMemory(Memory : Pointer; const SizeOfMemory, Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials, EffectInstances  : ID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall; external d3dx9dllname;

function D3DXLoadMeshFromXResource(Module : HMODULE; Name, _Type : PChar; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials, EffectInstances  : ID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall; external d3dx9dllname;

function D3DXSaveMeshToXA(Filename : PAnsiChar; Mesh : ID3DXMesh; Adjacency : PLongWord; Materials : PD3DXMaterial; EffectInstances : PD3DXEffectInstance; const NumMaterials, Format : LongWord) : HResult; stdcall; external d3dx9dllname;
function D3DXSaveMeshToXW(Filename : PWideChar; Mesh : ID3DXMesh; Adjacency : PLongWord; Materials : PD3DXMaterial; EffectInstances : PD3DXEffectInstance; const NumMaterials, Format : LongWord) : HResult; stdcall; external d3dx9dllname;
function D3DXSaveMeshToX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Mesh : ID3DXMesh; Adjacency : PLongWord; Materials : PD3DXMaterial; EffectInstances : PD3DXEffectInstance; const NumMaterials, Format : LongWord) : HResult; stdcall; external d3dx9dllname name {$IFDEF UNICODE}'D3DXSaveMeshToXW';{$ELSE}'D3DXSaveMeshToXA';{$ENDIF}

function D3DXCreatePMeshFromStream(Stream : IStream; Options : LongWord; D3DDevice : IDirect3DDevice9; out Materials, EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; out PMesh : ID3DXPMesh) : HResult; stdcall; external d3dx9dllname;

// Creates a skin info object based on the number of vertices, number of bones, and a declaration describing the vertex layout of the target vertices
// The bone names and initial bone transforms are not filled in the skin info object by this method.
function D3DXCreateSkinInfo(const NumVertices : LongWord; Declaration : PD3DVertexElement9; const NumBones : LongWord; out SkinInfo : ID3DXSkinInfo) : HResult; stdcall; external d3dx9dllname;

// Creates a skin info object based on the number of vertices, number of bones, and a FVF describing the vertex layout of the target vertices
// The bone names and initial bone transforms are not filled in the skin info object by this method.
function D3DXCreateSkinInfoFVF(const NumVertices, FVF, NumBones : LongWord; out SkinInfo : ID3DXSkinInfo) : HResult; stdcall; external d3dx9dllname;

function D3DXLoadMeshFromXof(XofObjMesh : IDirectXFileData; const Options : LongWord; D3DDevice : IDirect3DDevice9; out Adjacency, Materials, EffectInstances : ID3DXBuffer; var NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadMeshFromXof(XofObjMesh : IDirectXFileData; const Options : LongWord; D3DDevice : IDirect3DDevice9; Adjacency, Materials, EffectInstances : PID3DXBuffer; var NumMaterials : LongWord; out Mesh : ID3DXMesh) : HResult; stdcall; overload; external d3dx9dllname;

// This similar to D3DXLoadMeshFromXof, except also returns skinning info if present in the file
// If skinning info is not present, ppSkinInfo will be NULL
function D3DXLoadSkinMeshFromXof(XofObjMesh : IDirectXFileData; const Options : LongWord; D3D : IDirect3DDevice9; out Adjacency, Materials, EffectInstances : ID3DXBuffer; var MatOut : LongWord; SkinInfo : PID3DXSkinInfo; out Mesh : ID3DXMesh) : HResult; stdcall; external d3dx9dllname;

// The inverse of D3DXConvertTo{Indexed}BlendedMesh() functions. It figures out the skinning info from
// the mesh and the bone combination table and populates a skin info object with that data. The bone
// names and initial bone transforms are not filled in the skin info object by this method. This works
// with either a non-indexed or indexed blended mesh. It examines the FVF or declarator of the mesh to
// determine what type it is.
function D3DXCreateSkinInfoFromBlendedMesh(Mesh : ID3DXBaseMesh; const NumBoneCombinations : LongWord; BoneCombinationTable : PD3DXBoneCombination; out SkinInfo : ID3DXSkinInfo) : HResult; stdcall; external d3dx9dllname;

function D3DXTessellateNPatches(MeshIn : ID3DXMesh; AdjacencyIn : PLongWord; NumSegs : Single; QuadraticInterpNormals : Bool; out MeshOut : ID3DXMesh; out AdjacencyOut : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXTessellateNPatches(MeshIn : ID3DXMesh; AdjacencyIn : PLongWord; NumSegs : Single; QuadraticInterpNormals : Bool; out MeshOut : ID3DXMesh; AdjacencyOut : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

//generates implied outputdecl from input decl
//the decl generated from this should be used to generate the output decl for
//the tessellator subroutines.

function D3DXGenerateOutputDecl(Output, Input : PD3DVertexElement9Array) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGenerateOutputDecl(Output, Input : PD3DVertexElement9) : HResult; stdcall; overload; external d3dx9dllname;

//loads patches from an XFileData
//since an X file can have up to 6 different patch meshes in it,
//returns them in an array - pNumPatches will contain the number of
//meshes in the actual file.
function D3DXLoadPatchMeshFromXof(XofObjMesh : IDirectXFileData; const Options : LongWord; Device : IDirect3DDevice9; out Materials, EffectInstances : ID3DXBuffer; out NumMaterials : LongWord; out Mesh : ID3DXPatchMesh) : HResult; stdcall; external d3dx9dllname;

//computes the size a single rect patch.
function D3DXRectPatchSize(var NumSegs : Single; out Triangles, Vertices : LongWord) : HResult; stdcall; external d3dx9dllname;

//computes the size of a single triangle patch
function D3DXTriPatchSize(var NumSegs : Single; out Triangles, Vertices : LongWord) : HResult; stdcall; external d3dx9dllname;

//tessellates a patch into a created mesh
//similar to D3D RT patch
function D3DXTessellateRectPatch(VB : IDirect3DVertexBuffer9; var NumSegs : Single; const InDecl : TD3DVertexElement9; const RectPatchInfo : TD3DRectPatchInfo; Mesh : ID3DXMesh) : HResult; stdcall; external d3dx9dllname;

function D3DXTessellateTriPatch(VB : IDirect3DVertexBuffer9; var NumSegs : Single; const InDecl : TD3DVertexElement9; const RectPatchInfo : TD3DTriPatchInfo; Mesh : ID3DXMesh) : HResult; stdcall; external d3dx9dllname;

//creates an NPatch PatchMesh from a D3DXMESH
function D3DXCreateNPatchMesh(MeshSysMem : ID3DXMesh; out PatchMesh : ID3DXPatchMesh) : HResult; stdcall; external d3dx9dllname;

//creates a patch mesh
function D3DXCreatePatchMesh(const Info : TD3DXPatchInfo; const NumPatches, NumVertices, Options : LongWord; Decl : PD3DVertexElement9; Device : IDirect3DDevice9; out PatchMesh : ID3DXPatchMesh) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreatePatchMesh(const Info : TD3DXPatchInfo; const NumPatches, NumVertices, Options : LongWord; Decl : PD3DVertexElement9Array; Device : IDirect3DDevice9; out PatchMesh : ID3DXPatchMesh) : HResult; stdcall; overload; external d3dx9dllname;

//returns the number of degenerates in a patch mesh -
//text output put in string.
function D3DXValidPatchMesh(Mesh : ID3DXPatchMesh; DegenerateVertices, DegeneratePatches : PLongWord; out ErrorsAndWarnings : ID3DXBuffer) : HResult; stdcall; external d3dx9dllname;

function D3DXGetFVFVertexSize(const FVF : LongWord) : Cardinal; stdcall; external d3dx9dllname;

function D3DXGetDeclVertexSize(const Decl : TD3DVertexElement9; Stream : LongWord) : Cardinal; stdcall; external d3dx9dllname;

function D3DXGetDeclLength(const Decl : TD3DVertexElement9) : Cardinal; stdcall; external d3dx9dllname;

function D3DXDeclaratorFromFVF(const FVF : LongWord; var Declarator : TFVFDeclaration) : HResult; stdcall;overload; external d3dx9dllname;
function D3DXDeclaratorFromFVF(const FVF : LongWord; Declarator : PFVFDeclaration) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXFVFFromDeclarator(Declarator : PD3DVertexElement9; out FVF : LongWord) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXFVFFromDeclarator(Declarator : PD3DVertexElement9Array; out FVF : LongWord) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXWeldVertices(Mesh : ID3DXMesh; const Flags : LongWord; const Epsilons : TD3DXWeldEpsilons; AdjacencyIn, AdjacencyOut, FaceRemap : PLongWord; out VertexRemap : ID3DXBuffer) : HResult; stdcall; external d3dx9dllname;

type
  PD3DXIntersectInfo = ^TD3DXIntersectInfo;
  TD3DXIntersectInfo = packed record
    FaceIndex : LongWord;
    U         : Single;
    V         : Single;
    Dist      : Single;
  end;

function D3DXIntersect(Mesh : ID3DXBaseMesh; const RayPos, RayDir : TD3DXVector3; out Hit : Bool; out FaceIndex : LongWord; out U, V, Dist : Single; out AllHits : ID3DXBuffer; out CountOfHits : LongWord) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXIntersect(Mesh : ID3DXBaseMesh; RayPos, RayDir : PD3DXVector3; out Hit : Bool; out FaceIndex : LongWord; out U, V, Dist : Single; out AllHits : ID3DXBuffer; out CountOfHits : LongWord) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXIntersectSubset(Mesh : ID3DXBaseMesh; const AttribId : LongWord; const RayPos, RayDir : TD3DXVector3; out Hit : Bool; out FaceIndex : LongWord; out U, V, Dist : Single; out AllHits : ID3DXBuffer; out CountOfHits : LongWord) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXIntersectSubset(Mesh : ID3DXBaseMesh; const AttribId : LongWord; RayPos, RayDir : PD3DXVector3; out Hit : Bool; out FaceIndex : LongWord; out U, V, Dist : Single; out AllHits : ID3DXBuffer; out CountOfHits : LongWord) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXSplitMesh(MeshIn : ID3DXMesh; AdjacencyIn : PLongWord; const MaxSize, Options : LongWord; out MeshesOut : LongWord; MeshArrayOut, AdjacencyArrayOut, FaceRemapArrayOut, VertRemapArrayOut : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSplitMesh(MeshIn : ID3DXMesh; AdjacencyIn : PLongWord; const MaxSize, Options : LongWord; out MeshesOut : LongWord; MeshArrayOut : PID3DXBuffer; out AdjacencyArrayOut, FaceRemapArrayOut, VertRemapArrayOut : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXIntersectTri(const TriVertPos0, TriVertPos1, TriVertPos2, RayPos, RayDir : TD3DXVector3; out U, V, Dist : Single) : Bool; stdcall; overload; external d3dx9dllname;
function D3DXIntersectTri(TriVertPos0, TriVertPos1, TriVertPos2, RayPos, RayDir : PD3DXVector3; out U, V, Dist : Single) : Bool; stdcall; overload; external d3dx9dllname;

function D3DXSphereBoundProbe(const Center : TD3DXVector3; Radius : Single; const RayPosition, RayDirection : TD3DXVector3) : Bool; stdcall; overload; external d3dx9dllname;
function D3DXSphereBoundProbe(const Center : TD3DXVector3; Radius : Single; RayPosition, RayDirection : PD3DXVector3) : Bool; stdcall; overload; external d3dx9dllname;
function D3DXSphereBoundProbe(Center : PD3DXVector3; Radius : Single; const RayPosition, RayDirection : TD3DXVector3) : Bool; stdcall; overload; external d3dx9dllname;
function D3DXSphereBoundProbe(Center : PD3DXVector3; Radius : Single; RayPosition, RayDirection : PD3DXVector3) : Bool; stdcall; overload; external d3dx9dllname;

function D3DXBoxBoundProbe(const Min, Max, RayPosition, RayDirection : TD3DXVector3) : Bool; stdcall; overload; external d3dx9dllname;
function D3DXBoxBoundProbe(Min, Max, RayPosition, RayDirection : PD3DXVector3) : Bool; stdcall; overload; external d3dx9dllname;

//D3DXComputeTangent
//
//Computes the Tangent vectors for the TexStage texture coordinates
//and places the results in the TANGENT[TangentIndex] specified in the meshes' DECL
//puts the binorm in BINORM[BinormIndex] also specified in the decl.
//
//If neither the binorm or the tangnet are in the meshes declaration,
//the function will fail.
//
//If a tangent or Binorm field is in the Decl, but the user does not
//wish D3DXComputeTangent to replace them, then D3DX_DEFAULT specified
//in the TangentIndex or BinormIndex will cause it to ignore the specified
//semantic.
//
//Wrap should be specified if the texture coordinates wrap.

function D3DXComputeTangent(Mesh : ID3DXMesh; const TexStage, TangentIndex, BinormIndex, Wrap : LongWord; Adjacency : PLongWord) : HResult; stdcall; external d3dx9dllname;

function D3DXConvertMeshSubsetToSingleStrip(MeshIn : ID3DXBaseMesh; AttribId, IBOptions : LongWord; out IndexBuffer : IDirect3DIndexBuffer9; out NumIndices : LongWord) : HResult; stdcall; external d3dx9dllname;

function D3DXConvertMeshSubsetToStrips(MeshIn : ID3DXBaseMesh; AttribId, IBOptions : LongWord; out IndexBuffer : IDirect3DIndexBuffer9; out NumIndices : LongWord; out StripLengths : ID3DXBuffer; out NumStrips : LongWord) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXOptimizeFaces:
 *  --------------------
 *  Generate a face remapping for a triangle list that more effectively utilizes
 *    vertex caches.  This optimization is identical to the one provided
 *    by ID3DXMesh::Optimize with the hardware independent option enabled.
 *
 *  Parameters:
 *   pbIndices
 *      Triangle list indices to use for generating a vertex ordering
 *   NumFaces
 *      Number of faces in the triangle list
 *   NumVertices
 *      Number of vertices referenced by the triangle list
 *   b32BitIndices
 *      TRUE if indices are 32 bit, FALSE if indices are 16 bit
 *   pFaceRemap
 *      Destination buffer to store face ordering
 *      The number stored for a given element is where in the new ordering
 *        the face will have come from.  See ID3DXMesh::Optimize for more info.
 *
 *******************************************************************************
(*)

function D3DXOptimizeFaces(Indices : Pointer; Faces, Vertices : Cardinal; _32BitIndices : Bool; FaceRemap : PLongWord) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXOptimizeVertices:
 *  --------------------
 *  Generate a vertex remapping to optimize for in order use of vertices for 
 *    a given set of indices.  This is commonly used after applying the face
 *    remap generated by D3DXOptimizeFaces
 *
 *  Parameters:
 *   pbIndices
 *      Triangle list indices to use for generating a vertex ordering
 *   NumFaces
 *      Number of faces in the triangle list
 *   NumVertices
 *      Number of vertices referenced by the triangle list
 *   b32BitIndices
 *      TRUE if indices are 32 bit, FALSE if indices are 16 bit
 *   pVertexRemap
 *      Destination buffer to store vertex ordering
 *      The number stored for a given element is where in the new ordering
 *        the vertex will have come from.  See ID3DXMesh::Optimize for more info.
 *
 *******************************************************************************
(*)

function D3DXOptimizeVertices(Indices : Pointer; Faces, Vertices : Cardinal; _32BitIndices : Bool; VertexRemap : PLongWord) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  Data structures for Spherical Harmonic Precomputation
 *
 *
 *******************************************************************************
(*)


type
  TD3DXSHCompressQualityType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DXSHCQUAL_FASTLOWQUALITY  = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXSHCQUAL_SLOWHIGHQUALITY = 2{$IFNDEF NOENUMS}){$ENDIF};


// for all properties that are colors the red channel is used
// if the simulator is run with bSpectral FALSE

type
  PD3DXSHMaterial = ^TD3DXSHMaterial;
  TD3DXSHMaterial = packed record
    Diffuse                   : TD3DColorValue;  // Diffuse albedo of the surface.  (Ignored if object is a Mirror)
    Mirror                    : Bool;            // Must be set to FALSE.  bMirror == TRUE not currently supported
    SubSurf                   : Bool;            // true if the object does subsurface scattering - can't do this and be a mirror

    // subsurface scattering parameters
    RelativeIndexOfRefraction : Single;
    Absorption                : TD3DColorValue;
    ReducedScattering         : TD3DColorValue;
  end;

type
  TD3DXSHPRTBufferDesc = packed record
    NumSamples  : Cardinal;  // number of texels or vertices sampled
    Order       : Cardinal;  // order of spherical harmonics used
    NumChannels : Cardinal;
    Width       : Cardinal;
    Height      : Cardinal;
 end;

  TD3DXSHPRTBuffer_Desc = TD3DXSHPRTBufferDesc;

type
  TD3DXSHPRTCompBufferDesc = packed record
    SampleSize  : Cardinal;
    NumSamples  : Cardinal;    // number of texels or vertices sampled
    NumClusters : Cardinal;
    NumPCA      : Cardinal;
    Order       : Cardinal;    // order of spherical harmonics used
    NumChannels : Cardinal;
  end;

  TD3DXSHPRTCompBuffer_Desc = TD3DXSHPRTCompBufferDesc;

// allocated in D3DXSHPRTCompSplitMeshSC
// vertices are duplicated into multiple super clusters but
// only have a valid status in one super cluster (fill in the rest)

type
  TD3DXSHPRTSPlitMeshVertData = packed record
    VertRemap   : Cardinal;  // vertex in original mesh this corresponds to
    SubCluster  : Cardinal;  // cluster index relative to super cluster
    cVertStatus : Char;      // 1 if vertex has valid data, 0 if it is "fill"
  end;

// used in D3DXSHPRTCompSplitMeshSC
// information for each super cluster that maps into face/vert arrays

type
  TD3DXSHPRTSplitMeshClusterData = packed record
    VertStart     : Cardinal;  // initial index into remapped vertex array
    VertLength    : Cardinal;  // number of vertices in this super cluster

    FaceStart     : Cardinal;  // initial index into face array
    FaceLength    : Cardinal;  // number of faces in this super cluster

    ClusterStart  : Cardinal;  // initial index into cluster array
    ClusterLength : Cardinal;  // number of clusters in this super cluster
  end;

// call back function for simulator
// return S_OK to keep running the simulator - anything else represents
// failure and the simulator will abort.

type
  TD3DXSHPRTSimCB = function(PercentDone : Single) : HResult; stdcall;


(*)
 *******************************************************************************
 *
 *  Spherical Harmonic Precomputation Routines
 *
 * NOTE:
 *  * These functions are intended for offline use, extraction routines can
 *    be used at load time, but the simulators and compressor should only be
 *    used when authoring content.
 *
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 *
 *  D3DXSHPRTSimulation:
 *  --------------------
 *  Runs the PRT simulation on a set of input meshes using a corresponding 
 *  set of materials.  This function can take a long time to run and should 
 *  only done offline.
 *
 *  Parameters:
 *   Order
 *      Order of SH to use, generates Order^2 coeffs (Degree is Order - 1)
 *   NumMeshes
 *      Number of meshes in the scene, where each mesh has a unique material
 *   ppScene
 *      Meshes that represent the scene
 *   ppMaterials
 *      Materials for each corresponding mesh
 *   NumRays
 *      Number of rays to shoot at each vertex
 *   NumBounces
 *      Number of bounces simulated - if this is not zero inter-reflections 
 *      are computed
 *   EnableSubSurf
 *      Indicates whether or not Subsurface Scattering is to be used
 *   LengthScale
 *      Scale used for subsurface scattering (1 would be a 1mm cube)
 *   EnableSpectral
 *      TRUE enables color bleeding by computing RGB transfer vectors
 *      FALSE just uses the red channel for material properties
 *   ppSimulationResults
 *      Buffer that is allocated and filled in by the simulator
 *   pProgressCallback
 *      Optional function pointer that is called periodically
 *      Must return S_OK or simulator exits
 *
 *******************************************************************************
(*)

function D3DXSHPRTSimulation(Order, NumMeshes : Cardinal; const Scene : ID3DXMesh; var Materials : PD3DXSHMaterial; NumRays, NumBounces : Cardinal; EnableSubSurf : Bool; LengthScale : Single; EnableSpectral : Bool; SimulationResults : ID3DXBuffer; ProgressCallback : TD3DXSHPRTSimCB) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHPRTExtractChannel:
 *  ------------------------
 *  Pulls the data for a given channel out of pSimulationResults
 *
 *  Parameters:
 *   Order
 *      Order of spherical harmonic coefficients to extract
 *   pTransferCoefficients
 *      Array of Order^2 floats into which transfer coefficients for the 
 *      specified channel are written
 *   Channel
 *      Specifies the channel to extract (0/1/2 for R/G/B)
 *   pSimulationResults
 *      Buffer obtained from D3DXSHPRTSimulation
 *
 *******************************************************************************
(*)

function D3DXSHPRTExtractChannel(Order : Cardinal; TransferCoefficients : PSingle; Channel : Cardinal; SimulationResults : ID3DXBuffer) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHPRTGetRawDataPointer:
 *  ---------------------------
 *  Given a buffer that has been simulated, makes ppData reference the
 *  raw data in the buffer.  This pointer is invalid after the buffer
 *  has been released.  The data is formatted as follows:
 *      FLOAT fRawData[NumSamples][NumChannels][Order*Order]
 *  where NumSamples is the number of texels/vertices in the scene.
 *
 *  Parameters:
 *   pSimulationResults
 *      Buffer obtained from the simulator that contains transfer vectors
 *   ppRawData
 *      Returns a pointer to the raw data inside the buffer
 *
 *******************************************************************************
(*)

function D3DXSHPRTGetRawDataPointer(SimulationResults : ID3DXBuffer; out RawData : PSingle) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHPRTExtractDesc:
 *  ---------------------
 *  Given the result of a PRT simulation, this function extracts its
 *  description.
 *
 *  Parameters:
 *   pSimulationResults
 *      Buffer obtained from the simulator
 *   pDesc
 *      Structure to be filled in with information from the buffer
 *      Width/Height non-zero only if the textured simulator was used
 *
 *******************************************************************************
(*)

function D3DXSHPRTExtractDesc(SimulationResults : ID3DXBuffer; out Desc : TD3DXSHPRTBufferDesc) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHPRTSimulationTex:
 *  -----------------------
 *  Runs the PRT simulation on an input mesh in texture space.  
 *  Returns a buffer that contains PRT results for every texel.
 *  This function can take a long time to run and should only done offline.
 *
 *  Parameters:
 *   Order
 *      Order of SH to use, generates Order^2 coeffs per channel per texel
 *   pScene
 *      Mesh that represents the scene
 *   pMaterial
 *      Material which specifies the albedo for the scene (if no albedo 
 *      texture was specified), and the subsurface scattering properties
 *   NumRays
 *      Number of rays to shoot at each texel
 *   NumBounces
 *      Number of bounces simulated - if this is not zero inter-reflections
 *      are computed
 *   EnableSubSurf
 *      Indicates whether or not subsurface scattering is to be used
 *   LengthScale
 *      Scale used for subsurface scattering (1 would be a 1mm cube)
 *   EnableSpectral
 *      TRUE enables color bleeding by computing RGB transfer vectors
 *      FALSE just uses the red channel for material properties
 *   ppSimulationResults
 *      Buffer that is allocated and filled in by the simulator
 *   Width
 *      Number of texels to sample across horizontally
 *   Height
 *      Number of texels to sample across vertically
 *   pAlbedoTexture
 *      Albedo (diffuse reflectance) of surface (can be NULL)
 *   pProgressCallback
 *      Optional function pointer that is called periodically  
 *      (must return S_OK or simulator exits)
 *
 *******************************************************************************
(*)

function D3DXSHPRTSimulationTex(Order : Cardinal; Scene : ID3DXMesh; const Material : TD3DXSHMaterial; NumRays, NumBounces : Cardinal; EnableSubSurf : Bool; LengthScale : Single; EnableSpectral : Bool; out SimulationResults : ID3DXBuffer; Width, Height : Cardinal; AlbedoTexture : IDirect3DTexture9; ProgressCallback : TD3DXSHPRTSimCB) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHPRTExtractTexture:
 *  ------------------------
 *  Pulls the data for a given channel out of pSimulationResults.  
 *  
 *  Parameters:
 *   Channel
 *      Channel to be extracted.
 *   StartCoefficient
 *      Initial coefficient to extract 
 *   NumCoefficients
 *      Number of coefficients to extract
 *   pSimulationResults
 *      Buffer obtained from D3DXSHPRTSimulationTex
 *   pTexture
 *      Texture where data will be stored - must match dimensions specified
 *      when simulator was run and be a signed or float format
 *
 *  Example:
 *      For an order 4 simulation, there are 16 coefficients, which can be 
 *      stored into four 4-channel textures by calling D3DXSPHRTExtractTexture 
 *      4 times with NumCoefficients set to 4, and StartCoefficient set to 
 *      0, 4, 8, and 12 in succession.
 *
 *******************************************************************************
(*)

function D3DXSHPRTExtractTexture(Channel, StartCoefficent, NumCoefficients : Cardinal; SimulationResults : ID3DXBuffer; Texture : IDirect3DTexture9) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHPRTExtractToMesh:
 *  -----------------------
 *  Pulls transfer coefficients from the buffer containing the simulation 
 *  results and attaches them to the input mesh.
 *  Can only be called on single channel buffers (use D3DXSHPRTExtractChannel 
 *  otherwise).
 *
 *  Parameters:
 *   Order
 *      Order of SH to use, generates Order^2 coeffs, degree is Order-1
 *   pScene
 *      Single mesh that data is going to be packed into
 *   pSimulationResults
 *      Buffer obtained from D3DXSHPRTSimulation
 *   Usage
 *      D3DDECLUSAGE where coefficients are to be stored
 *   UsageIndexStart
 *      Starting index for coefficients to be stored
 *
 *******************************************************************************
(*)

function D3DXSHPRTExtractToMesh(Order : Cardinal; Scene : ID3DXMesh; SimulationResults : ID3DXBuffer; Usage : TD3DDeclUsage; UsageIndexStart : Cardinal) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHPRTCompress:
 *  ------------------
 *  This function compresses a PRT buffer, generating a new compressed
 *  buffer.
 *
 *  Parameters:
 *   Order
 *      Order of SH to compress, generates Order^2 coeffs, degree is Order-1
 *   pSimulationResults
 *      Buffer obtained from the simulator that contains transfer vectors.
 *   Quality
 *      Type of compression to use 
 *   NumClusters
 *      Number of clusters to use for compression
 *   NumPCA
 *      Number of PCA vectors to use in each cluster
 *   ppCompressedResults
 *      Returns the compressed data
 *
 *******************************************************************************
(*)

function D3DXSHPRTCompress(Order : Cardinal; SimulationResults : ID3DXBuffer; Quality : TD3DXSHCompressQualityType; NumClusters, NumPCA : Cardinal; out CompressedResults : ID3DXBuffer) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHPRTCompExtractToMesh:
 *  ---------------------------
 *  Pulls PCA coefficients from compressed buffer and attaches them to the 
 *  mesh.
 *
 *  Parameters:
 *   NumPCA
 *      Number of PCA coefficients to extract
 *   pScene
 *      Single mesh that data is going to be packed into
 *   pCompressedResults
 *      Buffer obtained from D3DXSHPRTCompress
 *   Usage
 *      D3DDECLUSAGE where coefficients are to be stored
 *   UsageIndexStart
 *      Starting index for coefficients to be stored
 *
 *******************************************************************************
(*)

function D3DXSHPRTCompExtractToMesh(NumPCA : Cardinal; Scene : ID3DXMesh; CompressedResults : ID3DXBuffer; Usage : TD3DDeclUsage; UsageIndexStart : Cardinal) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHPRTCompExtractDesc:
 *  -------------------------
 *  Given a compressed buffer, extracts a description of the data.
 *
 *  Parameters:
 *   pCompressedResults
 *      Buffer obtained D3DXSHPRTCompress
 *   pDesc
 *      Structure to be filled in with information from the buffer
 *
 *******************************************************************************
(*)

function D3DXSHPRTCompExtractDesc(CompressedResults : ID3DXBuffer; out Desc : TD3DXSHPRTCompBufferDesc) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHPRTCompNormalizeData:
 *  ---------------------------
 *  Given a compressed buffer, rescales all of the PCA projection coefficients
 *  so that they are within [-1, 1].  The PCA vectors are scaled so that 
 *  reconstruction is still correct.  This maximizes precision when packing
 *  into textures.
 *
 *  Parameters:
 *   pCompressedResults
 *      Buffer obtained from D3DXSHPRTCompress
 *
 *******************************************************************************
(*)

function D3DXSHPRTCompNormalizeData(CompressedResults : ID3DXBuffer) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHPRTCompExtractBasis:
 *  --------------------------
 *  Extracts the mean + PCA basis vectors for a given cluster from a
 *  compressed buffer.  The data is laid out in memory:
 *      FLOAT fData[NumSamples][NumChannels][Order*Order]
 *  Where NumSamples/NumChannels/Order are from the D3DXSHPRTCOMPBUFFER_DESC
 *  that can be extracted from pBuffer.
 *
 *  Parameters:
 *   Cluster
 *      Cluster whose basis is going to be extracted
 *   pCompressedResults
 *      Buffer obtained from D3DXSHPRTCompress
 *   pClusterBasis
 *      Array of floats into which cluster basis is written
 *
 *******************************************************************************
(*)

function D3DXSHPRTCompExtractBasis(Cluster : Cardinal; CompressedResults : ID3DXBuffer; ClusterBasis : PSingle) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHPRTCompExtractClusterIDs:
 *  -------------------------------
 *  Extracts the per sample cluster ID from a compressed data set.
 *
 *  Parameters:
 *   pCompressedResults
 *      Buffer obtained from D3DXSHPRTCompress
 *   pClusterIDs
 *      Pointer where D3DXSHPRTCOMPBUFFER_DESC::NumSamples IDs are written 
 *
 *******************************************************************************
(*)

function D3DXSHPRTCompExtractClusterIDs(CompressedResults : ID3DXBuffer; ClusterIDs : PCardinal) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHPRTCompExtractPCA:
 *  ------------------------
 *  Extracts the per-sample PCA coefficients from a compressed buffer.
 *
 *  Parameters:
 *   StartPCA
 *      Starting PCA projection coefficient to extract
 *   NumExtract
 *      Number of PCA projection coefficients to extract
 *   pCompressedResults
 *      Buffer obtained from D3DXSHPRTCompress
 *   pPCACoefficients
 *      Pointer where NumSamples * NumPCA PCA coefficients are written 
 *
 *******************************************************************************
(*)

function D3DXSHPRTCompExtractPCA(StartPCA, NumExtract : Cardinal; CompressedResults : ID3DXBuffer; PCACoefficients : PSingle) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHPRTCompExtractTexture:
 *  ----------------------------
 *  Extracts the per sample PCA coefficients from a compressed data set.  They
 *  are extracted into a texture that has already been allocated.
 *
 *  Parameters:
 *   StartPCA
 *      Starting PCA projection coefficient to extract
 *   NumExtract
 *      Number of PCA projection coefficients to extract
 *   pCompressedResults
 *      Buffer obtained from D3DXSHPRTCompress
 *   pTexture
 *      Texture where data is stored - must match dimensions of simulator
 *      and be a signed or floating point format
 *
 *******************************************************************************
(*)

function D3DXSHPRTCompExtractTexture(StartPCA, NumExtract : Cardinal; CompressedResults : ID3DXBuffer; Texture : IDirect3DTexture9) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHPRTCompSuperCluster:
 *  --------------------------
 *  Used with compressed results of D3DXSHPRTSimulation.
 *  Generates "super clusters" - groups of clusters that can be drawn in
 *  the same draw call.  A greedy algorithm that minimizes overdraw is used
 *  to group the clusters.
 *
 *  Parameters:
 *   pClusterIDs
 *      NumVerts cluster ID's (extracted from a compressed buffer)
 *   pScene
 *      Mesh that represents composite scene passed to the simulator
 *   MaxNumClusters
 *      Maximum number of clusters allocated per super cluster
 *   NumClusters
 *      Number of clusters computed in the simulator
 *   pSuperClusterIDs
 *      Array of length NumClusters, contains index of super cluster
 *      that corresponding cluster was assigned to
 *   pNumSuperClusters
 *      Returns the number of super clusters allocated
 *      
 *******************************************************************************
(*)

function D3DXSHPRTCompSuperCluster(ClusterIDs : PCardinal; Scene : ID3DXMesh; MaxNumClusters, NumClusters : Cardinal; SuperClusterIDs : PCardinal; out NumSuperClusters : Cardinal) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  D3DXSHPRTCompSplitMeshSC:
 *  -------------------------
 *  Used with compressed results of the vertex version of the PRT simulator.
 *  After D3DXSHRTCompSuperCluster has been called this function can be used
 *  to split the mesh into a group of faces/vertices per super cluster.
 *  Each super cluster contains all of the faces that contain any vertex
 *  classified in one of its clusters.  All of the vertices connected to this
 *  set of faces are also included with the returned array ppVertStatus 
 *  indicating whether or not the vertex belongs to the supercluster.
 *
 *  Parameters:
 *   pClusterIDs
 *      NumVerts cluster ID's (extracted from a compressed buffer)
 *   NumVertices
 *      Number of vertices in original mesh
 *   NumClusters
 *      Number of clusters (input parameter to compression)
 *   pSuperClusterIDs
 *      Array of size NumClusters that will contain super cluster ID's (from
 *      D3DXSHCompSuerCluster)
 *   NumSuperClusters
 *      Number of superclusters allocated in D3DXSHCompSuerCluster
 *   pInputIB
 *      Raw index buffer for mesh - format depends on bInputIBIs32Bit
 *   InputIBIs32Bit
 *      Indicates whether the input index buffer is 32-bit (otherwise 16-bit
 *      is assumed)
 *   NumFaces
 *      Number of faces in the original mesh (pInputIB is 3 times this length)
 *   ppIBData
 *      Raw index buffer that will contain the resulting split faces.  Format
 *      determined by bIBIs32Bit.  Allocated by function
 *   pIBDataLength
 *      Length of ppIBData, assigned in function
 *   OutputIBIs32Bit
 *      Indicates whether the output index buffer is to be 32-bit (otherwise 
 *      16-bit is assumed)
 *   ppFaceRemap
 *      Mapping of each face in ppIBData to original faces.  Length is
 *      *pIBDataLength/3.  Allocated in function
 *   ppVertData
 *      New vertex data structure.  Size of pVertDataLength
 *   pVertDataLength
 *      Number of new vertices in split mesh.  Assigned in function
 *   pSCClusterList
 *      Array of length NumClusters which pSCData indexes into (Cluster* fields)
 *      for each SC, contains clusters sorted by super cluster
 *   pSCData
 *      Structure per super cluster - contains indices into ppIBData,
 *      pSCClusterList and ppVertData
 *
 *******************************************************************************
(*)

function D3DXSHPRTCompSplitMeshSC(ClusterIDs : PCardinal; NumVertices, NumClusters : Cardinal; SuperClusterIDs : PCardinal; NumSuperClusters : Cardinal; InputIB : Pointer; InputIBIs32Bit : Bool; NumFaces : Cardinal; var IBData : Pointer; var IBDataLength : Cardinal; OutputIBIs32Bit : Bool; var FaceRemap : PCardinal; var VertData : TD3DXSHPRTSPlitMeshVertData; var VertDataLength : Cardinal; SCClusterList : PCardinal; var SCData : TD3DXSHPRTSplitMeshClusterData) : HResult; stdcall; external d3dx9dllname;

(*)
 ******************************************************************************
 *
 *  Definitions of .X file templates used by mesh load/save functions
 *    that are not RM standard
 *
 ******************************************************************************
(*)

const
  DXFILEOBJ_XSkinMeshHeader          : TGUID = '{3CF169CE-FF7C-44AB-93C0-F78F62D172E2}';
  DXFILEOBJ_VertexDuplicationIndices : TGUID = '{B8D65549-D7C9-4995-89CF-53A9A8B031E3}';
  DXFILEOBJ_FaceAdjacency            : TGUID = '{A64C844A-E282-4756-8B80-250CDE04398C}';
  DXFILEOBJ_SkinWeights              : TGUID = '{6F0D123B-BAD2-4167-A0D0-80224F25FABB}';
  DXFILEOBJ_Patch                    : TGUID = '{A3EB5D44-FC22-429D-9AFB-3221CB9719A6}';
  DXFILEOBJ_PatchMesh                : TGUID = '{D02C95CC-EDBA-4305-9B5D-1820D7704BBF}';
  DXFILEOBJ_PatchMesh9               : TGUID = '{B9EC94E1-B9A6-4251-BA18-94893F02C0EA}';
  DXFILEOBJ_PMInfo                   : TGUID = '{B6C3E656-EC8B-4B92-9B62-681659522947}';
  DXFILEOBJ_PMAttributeRange         : TGUID = '{917E0427-C61E-4A14-9C64-AFE65F9E9844}';
  DXFILEOBJ_PMVSplitRecord           : TGUID = '{574CCC14-F0B3-4333-822D-93E8A8A08E4C}';
  DXFILEOBJ_FVFData                  : TGUID = '{B6E70A0E-8EF9-4E83-94AD-ECC8B0C04897}';
  DXFILEOBJ_VertexElement            : TGUID = '{F752461C-1E23-48F6-B9F8-8350850F336F}';
  DXFILEOBJ_DeclData                 : TGUID = '{BF22E553-292C-4781-9FEA-62BD554BDD93}';
  DXFILEOBJ_EffectFloats             : TGUID = '{F1CFE2B3-0DE3-4E28-AFA1-155A750A282D}';
  DXFILEOBJ_EffectString             : TGUID = '{D55B097E-BDB6-4C52-B03D-6051C89D0E42}';
  DXFILEOBJ_EffectDWord              : TGUID = '{622C0ED0-956E-4DA9-908A-2AF94F3CE716}';
  DXFILEOBJ_EffectParamFloats        : TGUID = '{3014B9A0-62F5-478C-9B86-E4AC9F4E418B}';
  DXFILEOBJ_EffectParamString        : TGUID = '{1DBC4C88-94C1-46EE-9076-2C28818C9481}';
  DXFILEOBJ_EffectParamDWord         : TGUID = '{E13963BC-AE51-4C5D-B00F-CFA3A9D97CE5}';
  DXFILEOBJ_EffectInstance           : TGUID = '{E331F7E4-0559-4CC2-8E99-1CEC1657928F}';
  DXFILEOBJ_AnimTicksPerSecond       : TGUID = '{9E415A43-7BA6-4A73-8743-B73D47E88476}';

const
  XSKINEXP_TEMPLATES = 'xof 0303txt 0032' +
                       'template XSkinMeshHeader ' +
                       '{ ' +
                       '    <3CF169CE-FF7C-44ab-93C0-F78F62D172E2> ' +
                       '    WORD nMaxSkinWeightsPerVertex; ' +
                       '    WORD nMaxSkinWeightsPerFace; ' +
                       '    WORD nBones; ' +
                       '} ' +
                       'template VertexDuplicationIndices ' +
                       '{ ' +
                       '    <B8D65549-D7C9-4995-89CF-53A9A8B031E3> ' +
                       '    DWORD nIndices; ' +
                       '    DWORD nOriginalVertices; ' +
                       '    array DWORD indices[nIndices]; ' +
                       '} ' +
                       'template FaceAdjacency ' +
                       '{ ' +
                       '    <A64C844A-E282-4756-8B80-250CDE04398C> ' +
                       '    DWORD nIndices; ' +
                       '    array DWORD indices[nIndices]; ' +
                       '} ' +
                       'template SkinWeights ' +
                       '{ ' +
                       '    <6F0D123B-BAD2-4167-A0D0-80224F25FABB> ' +
                       '    STRING transformNodeName; ' +
                       '    DWORD nWeights; ' +
                       '    array DWORD vertexIndices[nWeights]; ' +
                       '    array float weights[nWeights]; ' +
                       '    Matrix4x4 matrixOffset; ' +
                       '} ' +
                       'template Patch ' +
                       '{ ' +
                       '    <A3EB5D44-FC22-429D-9AFB-3221CB9719A6> ' +
                       '    DWORD nControlIndices; ' +
                       '    array DWORD controlIndices[nControlIndices]; ' +
                       '} ' +
                       'template PatchMesh ' +
                       '{ ' +
                       '    <D02C95CC-EDBA-4305-9B5D-1820D7704BBF> ' +
                       '    DWORD nVertices; ' +
                       '    array Vector vertices[nVertices]; ' +
                       '    DWORD nPatches; ' +
                       '    array Patch patches[nPatches]; ' +
                       '    [ ... ] ' +
                       '} ' +
                       'template PatchMesh9 ' +
                       '{ ' +
                       '    <B9EC94E1-B9A6-4251-BA18-94893F02C0EA> ' +
                       '    DWORD Type; ' +
                       '    DWORD Degree; ' +
                       '    DWORD Basis; ' +
                       '    DWORD nVertices; ' +
                       '    array Vector vertices[nVertices]; ' +
                       '    DWORD nPatches; ' +
                       '    array Patch patches[nPatches]; ' +
                       '    [ ... ] ' +
                       '} ' +
                       'template EffectFloats ' +
                       '{ ' +
                       '    <F1CFE2B3-0DE3-4e28-AFA1-155A750A282D> ' +
                       '    DWORD nFloats; ' +
                       '    array float Floats[nFloats]; ' +
                       '} ' +
                       'template EffectString ' +
                       '{ ' +
                       '    <D55B097E-BDB6-4c52-B03D-6051C89D0E42> ' +
                       '    STRING Value; ' +
                       '} ' +
                       'template EffectDWord ' +
                       '{ ' +
                       '    <622C0ED0-956E-4da9-908A-2AF94F3CE716> ' +
                       '    DWORD Value; ' +
                       '} ' +
                       'template EffectParamFloats ' +
                       '{ ' +
                       '    <3014B9A0-62F5-478c-9B86-E4AC9F4E418B> ' +
                       '    STRING ParamName; ' +
                       '    DWORD nFloats; ' +
                       '    array float Floats[nFloats]; ' +
                       '} ' +
                       'template EffectParamString ' +
                       '{ ' +
                       '    <1DBC4C88-94C1-46ee-9076-2C28818C9481> ' +
                       '    STRING ParamName; ' +
                       '    STRING Value; ' +
                       '} ' +
                       'template EffectParamDWord ' +
                       '{ ' +
                       '    <E13963BC-AE51-4c5d-B00F-CFA3A9D97CE5> ' +
                       '    STRING ParamName; ' +
                       '    DWORD Value; ' +
                       '} ' +
                       'template EffectInstance ' +
                       '{ ' +
                       '    <E331F7E4-0559-4cc2-8E99-1CEC1657928F> ' +
                       '    STRING EffectFilename; ' +
                       '    [ ... ] ' +
                       '} ' +
                       'template AnimTicksPerSecond ' +
                       '{ ' +
                       '    <9E415A43-7BA6-4a73-8743-B73D47E88476> ' +
                       '    DWORD AnimTicksPerSecond; ' +
                       '} ';
   
const
  XEXTENSIONS_TEMPLATES = 'xof 0303txt 0032' +
                          'template FVFData ' +
                          '{ ' +
                          '    <B6E70A0E-8EF9-4e83-94AD-ECC8B0C04897> ' +
                          '    DWORD dwFVF; ' +
                          '    DWORD nDWords; ' +
                          '    array DWORD data[nDWords]; ' +
                          '} ' +
                          'template VertexElement ' +
                          '{ ' +
                          '    <F752461C-1E23-48f6-B9F8-8350850F336F> ' +
                          '    DWORD Type; ' +
                          '    DWORD Method; ' +
                          '    DWORD Usage; ' +
                          '    DWORD UsageIndex; ' +
                          '} ' +
                          'template DeclData ' +
                          '{ ' +
                          '    <BF22E553-292C-4781-9FEA-62BD554BDD93> ' +
                          '    DWORD nElements; ' +
                          '    array VertexElement Elements[nElements]; ' +
                          '    DWORD nDWords; ' +
                          '    array DWORD data[nDWords]; ' +
                          '} ' +
                          'template PMAttributeRange ' +
                          '{ ' +
                          '    <917E0427-C61E-4a14-9C64-AFE65F9E9844> ' +
                          '    DWORD iFaceOffset; ' +
                          '    DWORD nFacesMin; ' +
                          '    DWORD nFacesMax; ' +
                          '    DWORD iVertexOffset; ' +
                          '    DWORD nVerticesMin; ' +
                          '    DWORD nVerticesMax; ' +
                          '} ' +
                          'template PMVSplitRecord ' +
                          '{ ' +
                          '    <574CCC14-F0B3-4333-822D-93E8A8A08E4C> ' +
                          '    DWORD iFaceCLW; ' +
                          '    DWORD iVlrOffset; ' +
                          '    DWORD iCode; ' +
                          '} ' +
                          'template PMInfo ' +
                          '{ ' +
                          '    <B6C3E656-EC8B-4b92-9B62-681659522947> ' +
                          '    DWORD nAttributes; ' +
                          '    array PMAttributeRange attributeRanges[nAttributes]; ' +
                          '    DWORD nMaxValence; ' +
                          '    DWORD nMinLogicalVertices; ' +
                          '    DWORD nMaxLogicalVertices; ' +
                          '    DWORD nVSplits; ' +
                          '    array PMVSplitRecord splitRecords[nVSplits]; ' +
                          '    DWORD nAttributeMispredicts; ' +
                          '    array DWORD attributeMispredicts[nAttributeMispredicts]; ' +
                          '} ';

(*)
 *******************************************************************************
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File    : d3dx9tex.h
 *  Content : D3DX texturing APIs
 *
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 * D3DX_FILTER flags:
 * ------------------
 *
 * A valid filter must contain one of these values:
 *
 *  D3DX_FILTER_NONE
 *      No scaling or filtering will take place.  Pixels outside the bounds
 *      of the source image are assumed to be transparent black.
 *  D3DX_FILTER_POINT
 *      Each destination pixel is computed by sampling the nearest pixel
 *      from the source image.
 *  D3DX_FILTER_LINEAR
 *      Each destination pixel is computed by linearly interpolating between
 *      the nearest pixels in the source image.  This filter works best 
 *      when the scale on each axis is less than 2.
 *  D3DX_FILTER_TRIANGLE
 *      Every pixel in the source image contributes equally to the
 *      destination image.  This is the slowest of all the filters.
 *  D3DX_FILTER_BOX
 *      Each pixel is computed by averaging a 2x2(x2) box pixels from 
 *      the source image. Only works when the dimensions of the 
 *      destination are half those of the source. (as with mip maps)
 *
 * And can be OR'd with any of these optional flags:
 *
 *  D3DX_FILTER_MIRROR_U
 *      Indicates that pixels off the edge of the texture on the U-axis
 *      should be mirrored, not wraped.
 *  D3DX_FILTER_MIRROR_V
 *      Indicates that pixels off the edge of the texture on the V-axis
 *      should be mirrored, not wraped.
 *  D3DX_FILTER_MIRROR_W
 *      Indicates that pixels off the edge of the texture on the W-axis
 *      should be mirrored, not wraped.
 *  D3DX_FILTER_MIRROR
 *      Same as specifying D3DX_FILTER_MIRROR_U | D3DX_FILTER_MIRROR_V |
 *      D3DX_FILTER_MIRROR_V
 *  D3DX_FILTER_DITHER
 *      Dithers the resulting image using a 4x4 order dither pattern.
 *  D3DX_FILTER_SRGB_IN
 *      Denotes that the input data is in sRGB (gamma 2.2) colorspace.
 *  D3DX_FILTER_SRGB_OUT
 *      Denotes that the output data is in sRGB (gamma 2.2) colorspace.
 *  D3DX_FILTER_SRGB
 *      Same as specifying D3DX_FILTER_SRGB_IN | D3DX_FILTER_SRGB_OUT
 *
 *******************************************************************************
(*)

const
  D3DX_FILTER_NONE     = (1 shl 0);
  D3DX_FILTER_POINT    = (2 shl 0);
  D3DX_FILTER_LINEAR   = (3 shl 0);
  D3DX_FILTER_TRIANGLE = (4 shl 0);
  D3DX_FILTER_BOX      = (5 shl 0);
                               
  D3DX_FILTER_MIRROR_U         = (1 shl 16);
  D3DX_FILTER_MIRROR_V         = (2 shl 16);
  D3DX_FILTER_MIRROR_W         = (4 shl 16);
  D3DX_FILTER_MIRROR           = (7 shl 16);
  D3DX_FILTER_DITHER           = (1 shl 19);
  D3DX_FILTER_DITHER_DIFFUSION = (2 shl 19);
  D3DX_FILTER_SRGB_IN          = (1 shl 21);
  D3DX_FILTER_SRGB_OUT         = (2 shl 21);
  D3DX_FILTER_SRGB             = (3 shl 21);

(*)
 *******************************************************************************
 * D3DX_NORMALMAP flags:
 * ---------------------
 * These flags are used to control how D3DXComputeNormalMap generates normal
 * maps.  Any number of these flags may be OR'd together in any combination.
 *
 *  D3DX_NORMALMAP_MIRROR_U
 *      Indicates that pixels off the edge of the texture on the U-axis
 *      should be mirrored, not wraped.
 *  D3DX_NORMALMAP_MIRROR_V
 *      Indicates that pixels off the edge of the texture on the V-axis
 *      should be mirrored, not wraped.
 *  D3DX_NORMALMAP_MIRROR
 *      Same as specifying D3DX_NORMALMAP_MIRROR_U | D3DX_NORMALMAP_MIRROR_V
 *  D3DX_NORMALMAP_INVERTSIGN
 *      Inverts the direction of each normal
 *  D3DX_NORMALMAP_COMPUTE_OCCLUSION
 *      Compute the per pixel Occlusion term and encodes it into the alpha.
 *      An Alpha of 1 means that the pixel is not obscured in anyway, and
 *      an alpha of 0 would mean that the pixel is completly obscured.
 *
 *******************************************************************************
(*)

const
  D3DX_NORMALMAP_MIRROR_U          = ( 1 shl 16);
  D3DX_NORMALMAP_MIRROR_V          = ( 2 shl 16);
  D3DX_NORMALMAP_MIRROR            = ( 3 shl 16);
  D3DX_NORMALMAP_INVERTSIGN        = ( 8 shl 16);
  D3DX_NORMALMAP_COMPUTE_OCCLUSION = (16 shl 16);


(*)
 *******************************************************************************
 * D3DX_CHANNEL flags:
 * -------------------
 * These flags are used by functions which operate on or more channels
 * in a texture.
 *
 * D3DX_CHANNEL_RED
 *     Indicates the red channel should be used
 * D3DX_CHANNEL_BLUE
 *     Indicates the blue channel should be used
 * D3DX_CHANNEL_GREEN
 *     Indicates the green channel should be used
 * D3DX_CHANNEL_ALPHA
 *     Indicates the alpha channel should be used
 * D3DX_CHANNEL_LUMINANCE
 *     Indicates the luminaces of the red green and blue channels should be
 *     used.
 *******************************************************************************
(*)

const
  D3DX_CHANNEL_RED       = (1 shl 0);
  D3DX_CHANNEL_BLUE      = (1 shl 1);
  D3DX_CHANNEL_GREEN     = (1 shl 2);
  D3DX_CHANNEL_ALPHA     = (1 shl 3);
  D3DX_CHANNEL_LUMINANCE = (1 shl 4);


//----------------------------------------------------------------------------
// D3DXIMAGE_FILEFORMAT:
// ---------------------
// This enum is used to describe supported image file formats.
//
//----------------------------------------------------------------------------

type
  PD3DXImageFileformat = ^TD3DXImageFileformat;
  TD3DXImageFileformat = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DXIFF_BMP         = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXIFF_JPG         = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXIFF_TGA         = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXIFF_PNG         = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXIFF_DDS         = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXIFF_PPM         = 5{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXIFF_DIB         = 6{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXIFF_HDR         = 7{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  //high dynamic range formats
    D3DXIFF_PFM         = 8{$IFNDEF NOENUMS}){$ENDIF};

type
  PD3DXImage_Fileformat = ^TD3DXImage_Fileformat;
  TD3DXImage_Fileformat = TD3DXImageFileformat;


(*)
 *******************************************************************************
 * LPD3DXFILL2D and LPD3DXFILL3D:
 * ------------------------------
 * Function types used by the texture fill functions.
 *
 * Parameters:
 *  pOut
 *      Pointer to a vector which the function uses to return its result.
 *      X,Y,Z,W will be mapped to R,G,B,A respectivly.
 *  pTexCoord
 *      Pointer to a vector containing the coordinates of the texel currently
 *      being evaluated.  Textures and VolumeTexture texcoord components
 *      range from 0 to 1. CubeTexture texcoord component range from -1 to 1.
 *  pTexelSize
 *      Pointer to a vector containing the dimensions of the current texel.
 *  pData
 *      Pointer to user data.
 *******************************************************************************
(*)

type
  TD3DXFill2D = procedure(out Out : TD3DXVector4; const TexCoord, TexelSize : TD3DXVector2; Data : Pointer);

  TD3DXFill3D = procedure(out Out : TD3DXVector4; const TexCoord, TexelSize : TD3DXVector3; Data : Pointer);

(*)
 *******************************************************************************
 * D3DXIMAGE_INFO:
 * ---------------
 * This structure is used to return a rough description of what the
 * the original contents of an image file looked like.
 *
 *  Width
 *      Width of original image in pixels
 *  Height
 *      Height of original image in pixels
 *  Depth
 *      Depth of original image in pixels
 *  MipLevels
 *      Number of mip levels in original image
 *  Format
 *      D3D format which most closely describes the data in original image
 *  ResourceType
 *      D3DRESOURCETYPE representing the type of texture stored in the file.
 *      D3DRTYPE_TEXTURE, D3DRTYPE_VOLUMETEXTURE, or D3DRTYPE_CUBETEXTURE.
 *  ImageFileFormat
 *      D3DXIMAGE_FILEFORMAT representing the format of the image file.
 *******************************************************************************
(*)

type
  PD3DXImageInfo = ^TD3DXImageInfo;
  TD3DXImageInfo = packed record
    Width           : Cardinal;
    Height          : Cardinal;
    Depth           : Cardinal;
    MipLevels       : Cardinal;
    Format          : TD3DFormat;
    ResourceType    : TD3DResourceType;
    ImageFileFormat : TD3DXImageFileFormat;
  end;
  PD3DXImage_Info = ^TD3DXImage_Info;
  TD3DXImage_Info = TD3DXImageInfo;

(*)
 *******************************************************************************
 * Image File APIs
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 * GetImageInfoFromFile/Resource:
 * ------------------------------
 * Fills in a D3DXIMAGE_INFO struct with information about an image file.
 *
 * Parameters:
 *  pSrcFile
 *      File name of the source image.
 *  pSrcModule
 *      Module where resource is located, or NULL for module associated
 *      with image the os used to create the current process.
 *  pSrcResource
 *      Resource name
 *  pSrcData
 *      Pointer to file in memory.
 *  SrcDataSize
 *      Size in bytes of file in memory.
 *  pSrcInfo
 *      Pointer to a D3DXIMAGE_INFO structure to be filled in with the
 *      description of the data in the source image file.
 *******************************************************************************
(*)

function D3DXGetImageInfoFromFileA(SrcFile : PAnsiChar; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGetImageInfoFromFileA(SrcFile : PAnsiChar; out SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXGetImageInfoFromFileW(SrcFile : PWideChar; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGetImageInfoFromFileW(SrcFile : PWideChar; out SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXGetImageInfoFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGetImageInfoFromFileW';{$ELSE}'D3DXGetImageInfoFromFileA';{$ENDIF}
function D3DXGetImageInfoFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; out SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGetImageInfoFromFileW';{$ELSE}'D3DXGetImageInfoFromFileA';{$ENDIF}

function D3DXGetImageInfoFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGetImageInfoFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; out SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXGetImageInfoFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGetImageInfoFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; out SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXGetImageInfoFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGetImageInfoFromResourceW';{$ELSE}'D3DXGetImageInfoFromResourceA';{$ENDIF}
function D3DXGetImageInfoFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; out SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGetImageInfoFromResourceW';{$ELSE}'D3DXGetImageInfoFromResourceA';{$ENDIF}

function D3DXGetImageInfoFromFileInMemory(SrcData : Pointer; const SrcDataSize : Cardinal; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGetImageInfoFromFileInMemory(SrcData : Pointer; const SrcDataSize : Cardinal; out SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * Load/Save Surface APIs
 *******************************************************************************
(*)

// Start AmpazeInventions (tm)

type
  PPaletteEntryArray = ^TPaletteEntryArray;
  TPaletteEntryArray = array [Byte] of TPaletteEntry;

// End AmpazeInventions (tm)

(*)
 *******************************************************************************
 * D3DXLoadSurfaceFromFile/Resource:
 * ---------------------------------
 * Load surface from a file or resource
 *
 * Parameters:
 *  pDestSurface
 *      Destination surface, which will receive the image.
 *  pDestPalette
 *      Destination palette of 256 colors, or NULL
 *  pDestRect
 *      Destination rectangle, or NULL for entire surface
 *  pSrcFile
 *      File name of the source image.
 *  pSrcModule
 *      Module where resource is located, or NULL for module associated
 *      with image the os used to create the current process.
 *  pSrcResource
 *      Resource name
 *  pSrcData
 *      Pointer to file in memory.
 *  SrcDataSize
 *      Size in bytes of file in memory.
 *  pSrcRect
 *      Source rectangle, or NULL for entire image
 *  Filter
 *      D3DX_FILTER flags controlling how the image is filtered.
 *      Or D3DX_DEFAULT for D3DX_FILTER_TRIANGLE.
 *  ColorKey
 *      Color to replace with transparent black, or 0 to disable colorkey.
 *      This is always a 32-bit ARGB color, independent of the source image
 *      format.  Alpha is significant, and should usually be set to FF for
 *      opaque colorkeys.  (ex. Opaque black == 0xff000000)
 *  pSrcInfo
 *      Pointer to a D3DXIMAGE_INFO structure to be filled in with the
 *      description of the data in the source image file, or NULL.
 *******************************************************************************
(*)

function D3DXLoadSurfaceFromFileA(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcFile : PAnsiChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileA(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcFile : PAnsiChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileA(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcFile : PAnsiChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileA(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcFile : PAnsiChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileA(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcFile : PAnsiChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileA(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcFile : PAnsiChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileA(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcFile : PAnsiChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileA(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcFile : PAnsiChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileA(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcFile : PAnsiChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileA(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcFile : PAnsiChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileA(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcFile : PAnsiChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileA(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcFile : PAnsiChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileA(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcFile : PAnsiChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileA(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcFile : PAnsiChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileA(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcFile : PAnsiChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileA(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcFile : PAnsiChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXLoadSurfaceFromFileW(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcFile : PWideChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileW(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcFile : PWideChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileW(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcFile : PWideChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileW(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcFile : PWideChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileW(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcFile : PWideChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileW(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcFile : PWideChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileW(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcFile : PWideChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileW(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcFile : PWideChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileW(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcFile : PWideChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileW(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcFile : PWideChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileW(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcFile : PWideChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileW(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcFile : PWideChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileW(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcFile : PWideChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileW(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcFile : PWideChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileW(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcFile : PWideChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileW(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcFile : PWideChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXLoadSurfaceFromFile(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromFileW';{$ELSE}'D3DXLoadSurfaceFromFileA';{$ENDIF}
function D3DXLoadSurfaceFromFile(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromFileW';{$ELSE}'D3DXLoadSurfaceFromFileA';{$ENDIF}
function D3DXLoadSurfaceFromFile(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromFileW';{$ELSE}'D3DXLoadSurfaceFromFileA';{$ENDIF}
function D3DXLoadSurfaceFromFile(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromFileW';{$ELSE}'D3DXLoadSurfaceFromFileA';{$ENDIF}
function D3DXLoadSurfaceFromFile(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromFileW';{$ELSE}'D3DXLoadSurfaceFromFileA';{$ENDIF}
function D3DXLoadSurfaceFromFile(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromFileW';{$ELSE}'D3DXLoadSurfaceFromFileA';{$ENDIF}
function D3DXLoadSurfaceFromFile(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromFileW';{$ELSE}'D3DXLoadSurfaceFromFileA';{$ENDIF}
function D3DXLoadSurfaceFromFile(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromFileW';{$ELSE}'D3DXLoadSurfaceFromFileA';{$ENDIF}
function D3DXLoadSurfaceFromFile(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromFileW';{$ELSE}'D3DXLoadSurfaceFromFileA';{$ENDIF}
function D3DXLoadSurfaceFromFile(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromFileW';{$ELSE}'D3DXLoadSurfaceFromFileA';{$ENDIF}
function D3DXLoadSurfaceFromFile(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromFileW';{$ELSE}'D3DXLoadSurfaceFromFileA';{$ENDIF}
function D3DXLoadSurfaceFromFile(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromFileW';{$ELSE}'D3DXLoadSurfaceFromFileA';{$ENDIF}
function D3DXLoadSurfaceFromFile(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromFileW';{$ELSE}'D3DXLoadSurfaceFromFileA';{$ENDIF}
function D3DXLoadSurfaceFromFile(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromFileW';{$ELSE}'D3DXLoadSurfaceFromFileA';{$ENDIF}
function D3DXLoadSurfaceFromFile(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromFileW';{$ELSE}'D3DXLoadSurfaceFromFileA';{$ENDIF}
function D3DXLoadSurfaceFromFile(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromFileW';{$ELSE}'D3DXLoadSurfaceFromFileA';{$ENDIF}

function D3DXLoadSurfaceFromResourceA(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : PAnsiChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceA(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : PAnsiChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceA(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : PAnsiChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceA(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : PAnsiChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceA(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : PAnsiChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceA(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : PAnsiChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceA(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : PAnsiChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceA(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : PAnsiChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceA(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : PAnsiChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceA(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : PAnsiChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceA(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : PAnsiChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceA(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : PAnsiChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceA(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : PAnsiChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceA(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : PAnsiChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceA(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : PAnsiChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceA(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : PAnsiChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXLoadSurfaceFromResourceW(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : PWideChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceW(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : PWideChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceW(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : PWideChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceW(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : PWideChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceW(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : PWideChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceW(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : PWideChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceW(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : PWideChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceW(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : PWideChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceW(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : PWideChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceW(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : PWideChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceW(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : PWideChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceW(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : PWideChar; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceW(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : PWideChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceW(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : PWideChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceW(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : PWideChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromResourceW(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : PWideChar; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXLoadSurfaceFromResource(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromResourceW';{$ELSE}'D3DXLoadSurfaceFromResourceA';{$ENDIF}
function D3DXLoadSurfaceFromResource(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromResourceW';{$ELSE}'D3DXLoadSurfaceFromResourceA';{$ENDIF}
function D3DXLoadSurfaceFromResource(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromResourceW';{$ELSE}'D3DXLoadSurfaceFromResourceA';{$ENDIF}
function D3DXLoadSurfaceFromResource(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromResourceW';{$ELSE}'D3DXLoadSurfaceFromResourceA';{$ENDIF}
function D3DXLoadSurfaceFromResource(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromResourceW';{$ELSE}'D3DXLoadSurfaceFromResourceA';{$ENDIF}
function D3DXLoadSurfaceFromResource(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromResourceW';{$ELSE}'D3DXLoadSurfaceFromResourceA';{$ENDIF}
function D3DXLoadSurfaceFromResource(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromResourceW';{$ELSE}'D3DXLoadSurfaceFromResourceA';{$ENDIF}
function D3DXLoadSurfaceFromResource(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromResourceW';{$ELSE}'D3DXLoadSurfaceFromResourceA';{$ENDIF}
function D3DXLoadSurfaceFromResource(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromResourceW';{$ELSE}'D3DXLoadSurfaceFromResourceA';{$ENDIF}
function D3DXLoadSurfaceFromResource(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromResourceW';{$ELSE}'D3DXLoadSurfaceFromResourceA';{$ENDIF}
function D3DXLoadSurfaceFromResource(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromResourceW';{$ELSE}'D3DXLoadSurfaceFromResourceA';{$ENDIF}
function D3DXLoadSurfaceFromResource(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromResourceW';{$ELSE}'D3DXLoadSurfaceFromResourceA';{$ENDIF}
function D3DXLoadSurfaceFromResource(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromResourceW';{$ELSE}'D3DXLoadSurfaceFromResourceA';{$ENDIF}
function D3DXLoadSurfaceFromResource(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; const SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromResourceW';{$ELSE}'D3DXLoadSurfaceFromResourceA';{$ENDIF}
function D3DXLoadSurfaceFromResource(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromResourceW';{$ELSE}'D3DXLoadSurfaceFromResourceA';{$ENDIF}
function D3DXLoadSurfaceFromResource(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; const SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadSurfaceFromResourceW';{$ELSE}'D3DXLoadSurfaceFromResourceA';{$ENDIF}

function D3DXLoadSurfaceFromFileInMemory(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcData : Pointer; const SrcDataSize : Cardinal; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileInMemory(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcData : Pointer; const SrcDataSize : Cardinal; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileInMemory(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcData : Pointer; const SrcDataSize : Cardinal; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileInMemory(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcData : Pointer; const SrcDataSize : Cardinal; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileInMemory(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileInMemory(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileInMemory(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileInMemory(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileInMemory(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcData : Pointer; const SrcDataSize : Cardinal; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileInMemory(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcData : Pointer; const SrcDataSize : Cardinal; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileInMemory(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcData : Pointer; const SrcDataSize : Cardinal; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileInMemory(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcData : Pointer; const SrcDataSize : Cardinal; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileInMemory(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileInMemory(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileInMemory(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromFileInMemory(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXLoadSurfaceFromSurface:
 * ---------------------------
 * Load surface from another surface (with color conversion)
 *
 * Parameters:
 *  pDestSurface
 *      Destination surface, which will receive the image.
 *  pDestPalette
 *      Destination palette of 256 colors, or NULL
 *  pDestRect
 *      Destination rectangle, or NULL for entire surface
 *  pSrcSurface
 *      Source surface
 *  pSrcPalette
 *      Source palette of 256 colors, or NULL
 *  pSrcRect
 *      Source rectangle, or NULL for entire surface
 *  Filter
 *      D3DX_FILTER flags controlling how the image is filtered.
 *      Or D3DX_DEFAULT for D3DX_FILTER_TRIANGLE.
 *  ColorKey
 *      Color to replace with transparent black, or 0 to disable colorkey.
 *      This is always a 32-bit ARGB color, independent of the source image
 *      format.  Alpha is significant, and should usually be set to FF for
 *      opaque colorkeys.  (ex. Opaque black == 0xff000000)
 *******************************************************************************
(*)

function D3DXLoadSurfaceFromSurface(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromSurface(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromSurface(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromSurface(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromSurface(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromSurface(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromSurface(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromSurface(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromSurface(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromSurface(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromSurface(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromSurface(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromSurface(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromSurface(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromSurface(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromSurface(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXLoadSurfaceFromMemory:
 * --------------------------
 * Load surface from memory.
 *
 * Parameters:
 *  pDestSurface
 *      Destination surface, which will receive the image.
 *  pDestPalette
 *      Destination palette of 256 colors, or NULL
 *  pDestRect
 *      Destination rectangle, or NULL for entire surface
 *  pSrcMemory
 *      Pointer to the top-left corner of the source image in memory
 *  SrcFormat
 *      Pixel format of the source image.
 *  SrcPitch
 *      Pitch of source image, in bytes.  For DXT formats, this number
 *      should represent the width of one row of cells, in bytes.
 *  pSrcPalette
 *      Source palette of 256 colors, or NULL
 *  pSrcRect
 *      Source rectangle.
 *  Filter
 *      D3DX_FILTER flags controlling how the image is filtered.
 *      Or D3DX_DEFAULT for D3DX_FILTER_TRIANGLE.
 *  ColorKey
 *      Color to replace with transparent black, or 0 to disable colorkey.
 *      This is always a 32-bit ARGB color, independent of the source image
 *      format.  Alpha is significant, and should usually be set to FF for
 *      opaque colorkeys.  (ex. Opaque black == 0xff000000)
 *******************************************************************************
(*)

function D3DXLoadSurfaceFromMemory(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcMemory : Pointer; SrcFormat : TD3DFormat; const SrcPitch : Cardinal; SrcPalette : PPaletteEntryArray; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromMemory(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcMemory : Pointer; SrcFormat : TD3DFormat; const SrcPitch : Cardinal; SrcPalette : PPaletteEntryArray; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromMemory(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcMemory : Pointer; SrcFormat : TD3DFormat; const SrcPitch : Cardinal; SrcPalette : PPaletteEntryArray; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromMemory(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcMemory : Pointer; SrcFormat : TD3DFormat; const SrcPitch : Cardinal; SrcPalette : PPaletteEntryArray; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromMemory(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcMemory : Pointer; SrcFormat : TD3DFormat; const SrcPitch : Cardinal; const SrcPalette : TPaletteEntryArray; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromMemory(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcMemory : Pointer; SrcFormat : TD3DFormat; const SrcPitch : Cardinal; const SrcPalette : TPaletteEntryArray; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromMemory(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcMemory : Pointer; SrcFormat : TD3DFormat; const SrcPitch : Cardinal; const SrcPalette : TPaletteEntryArray; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromMemory(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcMemory : Pointer; SrcFormat : TD3DFormat; const SrcPitch : Cardinal; const SrcPalette : TPaletteEntryArray; SrcRect : PRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromMemory(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcMemory : Pointer; SrcFormat : TD3DFormat; const SrcPitch : Cardinal; SrcPalette : PPaletteEntryArray; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromMemory(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcMemory : Pointer; SrcFormat : TD3DFormat; const SrcPitch : Cardinal; SrcPalette : PPaletteEntryArray; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromMemory(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcMemory : Pointer; SrcFormat : TD3DFormat; const SrcPitch : Cardinal; SrcPalette : PPaletteEntryArray; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromMemory(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcMemory : Pointer; SrcFormat : TD3DFormat; const SrcPitch : Cardinal; SrcPalette : PPaletteEntryArray; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromMemory(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; DestRect : PRect; SrcMemory : Pointer; SrcFormat : TD3DFormat; const SrcPitch : Cardinal; const SrcPalette : TPaletteEntryArray; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromMemory(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; DestRect : PRect; SrcMemory : Pointer; SrcFormat : TD3DFormat; const SrcPitch : Cardinal; const SrcPalette : TPaletteEntryArray; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromMemory(DestSurface : IDirect3DSurface9; DestPalette : PPaletteEntryArray; const DestRect : TRect; SrcMemory : Pointer; SrcFormat : TD3DFormat; const SrcPitch : Cardinal; const SrcPalette : TPaletteEntryArray; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadSurfaceFromMemory(DestSurface : IDirect3DSurface9; const DestPalette : TPaletteEntryArray; const DestRect : TRect; SrcMemory : Pointer; SrcFormat : TD3DFormat; const SrcPitch : Cardinal; const SrcPalette : TPaletteEntryArray; const SrcRect : TRect; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXSaveSurfaceToFile:
 * ----------------------
 * Save a surface to a image file.
 *
 * Parameters:
 *  pDestFile
 *      File name of the destination file
 *  DestFormat
 *      D3DXIMAGE_FILEFORMAT specifying file format to use when saving.
 *  pSrcSurface
 *      Source surface, containing the image to be saved
 *  pSrcPalette
 *      Source palette of 256 colors, or NULL
 *  pSrcRect
 *      Source rectangle, or NULL for the entire image
 *******************************************************************************
(*)

function D3DXSaveSurfaceToFileA(DestFile : PAnsiChar; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; SrcRect : PRect) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveSurfaceToFileA(DestFile : PAnsiChar; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; SrcRect : PRect) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveSurfaceToFileA(DestFile : PAnsiChar; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; const SrcRect : TRect) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveSurfaceToFileA(DestFile : PAnsiChar; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; const SrcRect : TRect) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXSaveSurfaceToFileW(DestFile : PWideChar; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; SrcRect : PRect) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveSurfaceToFileW(DestFile : PWideChar; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; SrcRect : PRect) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveSurfaceToFileW(DestFile : PWideChar; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; const SrcRect : TRect) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveSurfaceToFileW(DestFile : PWideChar; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; const SrcRect : TRect) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXSaveSurfaceToFile(DestFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; SrcRect : PRect) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXSaveSurfaceToFileW';{$ELSE}'D3DXSaveSurfaceToFileA';{$ENDIF}
function D3DXSaveSurfaceToFile(DestFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; SrcRect : PRect) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXSaveSurfaceToFileW';{$ELSE}'D3DXSaveSurfaceToFileA';{$ENDIF}
function D3DXSaveSurfaceToFile(DestFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; const SrcRect : TRect) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXSaveSurfaceToFileW';{$ELSE}'D3DXSaveSurfaceToFileA';{$ENDIF}
function D3DXSaveSurfaceToFile(DestFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; const SrcRect : TRect) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXSaveSurfaceToFileW';{$ELSE}'D3DXSaveSurfaceToFileA';{$ENDIF}

(*)
 *******************************************************************************
 * D3DXSaveSurfaceToFileInMemory:
 * ----------------------
 * Save a surface to a image file.
 *
 * Parameters:
 *  ppDestBuf
 *      address of pointer to d3dxbuffer for returning data bits
 *  DestFormat
 *      D3DXIMAGE_FILEFORMAT specifying file format to use when saving.
 *  pSrcSurface
 *      Source surface, containing the image to be saved
 *  pSrcPalette
 *      Source palette of 256 colors, or NULL
 *  pSrcRect
 *      Source rectangle, or NULL for the entire image
 *
 *******************************************************************************
(*)

function D3DXSaveSurfaceToFileInMemory(DestBuf : PID3DXBuffer; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; SrcRect : PRect) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveSurfaceToFileInMemory(DestBuf : PID3DXBuffer; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; SrcRect : PRect) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveSurfaceToFileInMemory(DestBuf : PID3DXBuffer; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; const SrcRect : TRect) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveSurfaceToFileInMemory(DestBuf : PID3DXBuffer; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; const SrcRect : TRect) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveSurfaceToFileInMemory(out DestBuf : ID3DXBuffer; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; SrcRect : PRect) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveSurfaceToFileInMemory(out DestBuf : ID3DXBuffer; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; SrcRect : PRect) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveSurfaceToFileInMemory(out DestBuf : ID3DXBuffer; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; SrcPalette : PPaletteEntryArray; const SrcRect : TRect) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveSurfaceToFileInMemory(out DestBuf : ID3DXBuffer; DestFormat : TD3DXImageFileFormat; SrcSurface : IDirect3DSurface9; const SrcPalette : TPaletteEntryArray; const SrcRect : TRect) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * Load/Save Volume APIs
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 * D3DXLoadVolumeFromFile/Resource:
 * --------------------------------
 * Load volume from a file or resource
 *
 * Parameters:
 *  pDestVolume
 *      Destination volume, which will receive the image.
 *  pDestPalette
 *      Destination palette of 256 colors, or NULL
 *  pDestBox
 *      Destination box, or NULL for entire volume
 *  pSrcFile
 *      File name of the source image.
 *  pSrcModule
 *      Module where resource is located, or NULL for module associated
 *      with image the os used to create the current process.
 *  pSrcResource
 *      Resource name
 *  pSrcData
 *      Pointer to file in memory.
 *  SrcDataSize
 *      Size in bytes of file in memory.
 *  pSrcBox
 *      Source box, or NULL for entire image
 *  Filter
 *      D3DX_FILTER flags controlling how the image is filtered.
 *      Or D3DX_DEFAULT for D3DX_FILTER_TRIANGLE.
 *  ColorKey
 *      Color to replace with transparent black, or 0 to disable colorkey.
 *      This is always a 32-bit ARGB color, independent of the source image
 *      format.  Alpha is significant, and should usually be set to FF for
 *      opaque colorkeys.  (ex. Opaque black == 0xff000000)
 *  pSrcInfo
 *      Pointer to a D3DXIMAGE_INFO structure to be filled in with the
 *      description of the data in the source image file, or NULL.
 *******************************************************************************
(*)

function D3DXLoadVolumeFromFileA(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcFile : PAnsiChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileA(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcFile : PAnsiChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileA(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcFile : PAnsiChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileA(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcFile : PAnsiChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileA(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcFile : PAnsiChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileA(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcFile : PAnsiChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileA(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcFile : PAnsiChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileA(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcFile : PAnsiChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileA(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcFile : PAnsiChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileA(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcFile : PAnsiChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileA(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcFile : PAnsiChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileA(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcFile : PAnsiChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileA(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcFile : PAnsiChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileA(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcFile : PAnsiChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileA(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcFile : PAnsiChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileA(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcFile : PAnsiChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXLoadVolumeFromFileW(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcFile : PWideChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileW(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcFile : PWideChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileW(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcFile : PWideChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileW(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcFile : PWideChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileW(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcFile : PWideChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileW(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcFile : PWideChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileW(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcFile : PWideChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileW(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcFile : PWideChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileW(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcFile : PWideChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileW(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcFile : PWideChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileW(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcFile : PWideChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileW(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcFile : PWideChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileW(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcFile : PWideChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileW(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcFile : PWideChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileW(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcFile : PWideChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileW(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcFile : PWideChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXLoadVolumeFromFile(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromFileW';{$ELSE}'D3DXLoadVolumeFromFileA';{$ENDIF}
function D3DXLoadVolumeFromFile(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromFileW';{$ELSE}'D3DXLoadVolumeFromFileA';{$ENDIF}
function D3DXLoadVolumeFromFile(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromFileW';{$ELSE}'D3DXLoadVolumeFromFileA';{$ENDIF}
function D3DXLoadVolumeFromFile(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromFileW';{$ELSE}'D3DXLoadVolumeFromFileA';{$ENDIF}
function D3DXLoadVolumeFromFile(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromFileW';{$ELSE}'D3DXLoadVolumeFromFileA';{$ENDIF}
function D3DXLoadVolumeFromFile(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromFileW';{$ELSE}'D3DXLoadVolumeFromFileA';{$ENDIF}
function D3DXLoadVolumeFromFile(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromFileW';{$ELSE}'D3DXLoadVolumeFromFileA';{$ENDIF}
function D3DXLoadVolumeFromFile(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromFileW';{$ELSE}'D3DXLoadVolumeFromFileA';{$ENDIF}
function D3DXLoadVolumeFromFile(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromFileW';{$ELSE}'D3DXLoadVolumeFromFileA';{$ENDIF}
function D3DXLoadVolumeFromFile(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromFileW';{$ELSE}'D3DXLoadVolumeFromFileA';{$ENDIF}
function D3DXLoadVolumeFromFile(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromFileW';{$ELSE}'D3DXLoadVolumeFromFileA';{$ENDIF}
function D3DXLoadVolumeFromFile(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromFileW';{$ELSE}'D3DXLoadVolumeFromFileA';{$ENDIF}
function D3DXLoadVolumeFromFile(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromFileW';{$ELSE}'D3DXLoadVolumeFromFileA';{$ENDIF}
function D3DXLoadVolumeFromFile(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromFileW';{$ELSE}'D3DXLoadVolumeFromFileA';{$ENDIF}
function D3DXLoadVolumeFromFile(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromFileW';{$ELSE}'D3DXLoadVolumeFromFileA';{$ENDIF}
function D3DXLoadVolumeFromFile(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromFileW';{$ELSE}'D3DXLoadVolumeFromFileA';{$ENDIF}

function D3DXLoadVolumeFromResourceA(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : PAnsiChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceA(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : PAnsiChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceA(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : PAnsiChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceA(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : PAnsiChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceA(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : PAnsiChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceA(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : PAnsiChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceA(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : PAnsiChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceA(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : PAnsiChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceA(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : PAnsiChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceA(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : PAnsiChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceA(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : PAnsiChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceA(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : PAnsiChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceA(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : PAnsiChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceA(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : PAnsiChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceA(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : PAnsiChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceA(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : PAnsiChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXLoadVolumeFromResourceW(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : PWideChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceW(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : PWideChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceW(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : PWideChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceW(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : PWideChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceW(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : PWideChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceW(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : PWideChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceW(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : PWideChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceW(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : PWideChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceW(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : PWideChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceW(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : PWideChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceW(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : PWideChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceW(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : PWideChar; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceW(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : PWideChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceW(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : PWideChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceW(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : PWideChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromResourceW(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : PWideChar; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXLoadVolumeFromResource(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromResourceW';{$ELSE}'D3DXLoadVolumeFromResourceA';{$ENDIF}
function D3DXLoadVolumeFromResource(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromResourceW';{$ELSE}'D3DXLoadVolumeFromResourceA';{$ENDIF}
function D3DXLoadVolumeFromResource(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromResourceW';{$ELSE}'D3DXLoadVolumeFromResourceA';{$ENDIF}
function D3DXLoadVolumeFromResource(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromResourceW';{$ELSE}'D3DXLoadVolumeFromResourceA';{$ENDIF}
function D3DXLoadVolumeFromResource(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromResourceW';{$ELSE}'D3DXLoadVolumeFromResourceA';{$ENDIF}
function D3DXLoadVolumeFromResource(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromResourceW';{$ELSE}'D3DXLoadVolumeFromResourceA';{$ENDIF}
function D3DXLoadVolumeFromResource(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromResourceW';{$ELSE}'D3DXLoadVolumeFromResourceA';{$ENDIF}
function D3DXLoadVolumeFromResource(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromResourceW';{$ELSE}'D3DXLoadVolumeFromResourceA';{$ENDIF}
function D3DXLoadVolumeFromResource(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromResourceW';{$ELSE}'D3DXLoadVolumeFromResourceA';{$ENDIF}
function D3DXLoadVolumeFromResource(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromResourceW';{$ELSE}'D3DXLoadVolumeFromResourceA';{$ENDIF}
function D3DXLoadVolumeFromResource(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromResourceW';{$ELSE}'D3DXLoadVolumeFromResourceA';{$ENDIF}
function D3DXLoadVolumeFromResource(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromResourceW';{$ELSE}'D3DXLoadVolumeFromResourceA';{$ENDIF}
function D3DXLoadVolumeFromResource(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromResourceW';{$ELSE}'D3DXLoadVolumeFromResourceA';{$ENDIF}
function D3DXLoadVolumeFromResource(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromResourceW';{$ELSE}'D3DXLoadVolumeFromResourceA';{$ENDIF}
function D3DXLoadVolumeFromResource(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromResourceW';{$ELSE}'D3DXLoadVolumeFromResourceA';{$ENDIF}
function D3DXLoadVolumeFromResource(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadVolumeFromResourceW';{$ELSE}'D3DXLoadVolumeFromResourceA';{$ENDIF}

function D3DXLoadVolumeFromFileInMemory(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileInMemory(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileInMemory(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileInMemory(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileInMemory(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileInMemory(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileInMemory(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileInMemory(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileInMemory(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileInMemory(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileInMemory(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileInMemory(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileInMemory(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileInMemory(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileInMemory(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromFileInMemory(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXLoadVolumeFromVolume:
 * -------------------------
 * Load volume from another volume (with color conversion)
 *
 * Parameters:
 *  pDestVolume
 *      Destination volume, which will receive the image.
 *  pDestPalette
 *      Destination palette of 256 colors, or NULL
 *  pDestBox
 *      Destination box, or NULL for entire volume
 *  pSrcVolume
 *      Source volume
 *  pSrcPalette
 *      Source palette of 256 colors, or NULL
 *  pSrcBox
 *      Source box, or NULL for entire volume
 *  Filter
 *      D3DX_FILTER flags controlling how the image is filtered.
 *      Or D3DX_DEFAULT for D3DX_FILTER_TRIANGLE.
 *  ColorKey
 *      Color to replace with transparent black, or 0 to disable colorkey.
 *      This is always a 32-bit ARGB color, independent of the source image
 *      format.  Alpha is significant, and should usually be set to FF for
 *      opaque colorkeys.  (ex. Opaque black == 0xff000000)
 *******************************************************************************
(*)

function D3DXLoadVolumeFromVolume(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromVolume(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromVolume(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromVolume(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromVolume(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromVolume(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromVolume(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromVolume(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcData : Pointer; const SrcDataSize : Cardinal; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXLoadVolumeFromMemory:
 * -------------------------
 * Load volume from memory.
 *
 * Parameters:
 *  pDestVolume
 *      Destination volume, which will receive the image.
 *  pDestPalette
 *      Destination palette of 256 colors, or NULL
 *  pDestBox
 *      Destination box, or NULL for entire volume
 *  pSrcMemory
 *      Pointer to the top-left corner of the source volume in memory
 *  SrcFormat
 *      Pixel format of the source volume.
 *  SrcRowPitch
 *      Pitch of source image, in bytes.  For DXT formats, this number
 *      should represent the size of one row of cells, in bytes.
 *  SrcSlicePitch
 *      Pitch of source image, in bytes.  For DXT formats, this number
 *      should represent the size of one slice of cells, in bytes.
 *  pSrcPalette
 *      Source palette of 256 colors, or NULL
 *  pSrcBox
 *      Source box.
 *  Filter
 *      D3DX_FILTER flags controlling how the image is filtered.
 *      Or D3DX_DEFAULT for D3DX_FILTER_TRIANGLE.
 *  ColorKey
 *      Color to replace with transparent black, or 0 to disable colorkey.
 *      This is always a 32-bit ARGB color, independent of the source image
 *      format.  Alpha is significant, and should usually be set to FF for
 *      opaque colorkeys.  (ex. Opaque black == 0xff000000)
 *******************************************************************************
(*)

function D3DXLoadVolumeFromMemory(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcMemory : Pointer; const SrcFormat : TD3DFormat; const SrcRowPitch, SrcSlicePitch : Cardinal; SrcPalette : PPaletteEntryArray; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromMemory(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcMemory : Pointer; const SrcFormat : TD3DFormat; const SrcRowPitch, SrcSlicePitch : Cardinal; SrcPalette : PPaletteEntryArray; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromMemory(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcMemory : Pointer; const SrcFormat : TD3DFormat; const SrcRowPitch, SrcSlicePitch : Cardinal; SrcPalette : PPaletteEntryArray; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromMemory(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcMemory : Pointer; const SrcFormat : TD3DFormat; const SrcRowPitch, SrcSlicePitch : Cardinal; SrcPalette : PPaletteEntryArray; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromMemory(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcMemory : Pointer; const SrcFormat : TD3DFormat; const SrcRowPitch, SrcSlicePitch : Cardinal; const SrcPalette : TPaletteEntryArray; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromMemory(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcMemory : Pointer; const SrcFormat : TD3DFormat; const SrcRowPitch, SrcSlicePitch : Cardinal; const SrcPalette : TPaletteEntryArray; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromMemory(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcMemory : Pointer; const SrcFormat : TD3DFormat; const SrcRowPitch, SrcSlicePitch : Cardinal; const SrcPalette : TPaletteEntryArray; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromMemory(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcMemory : Pointer; const SrcFormat : TD3DFormat; const SrcRowPitch, SrcSlicePitch : Cardinal; const SrcPalette : TPaletteEntryArray; SrcBox : PD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromMemory(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcMemory : Pointer; const SrcFormat : TD3DFormat; const SrcRowPitch, SrcSlicePitch : Cardinal; SrcPalette : PPaletteEntryArray; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromMemory(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcMemory : Pointer; const SrcFormat : TD3DFormat; const SrcRowPitch, SrcSlicePitch : Cardinal; SrcPalette : PPaletteEntryArray; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromMemory(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcMemory : Pointer; const SrcFormat : TD3DFormat; const SrcRowPitch, SrcSlicePitch : Cardinal; SrcPalette : PPaletteEntryArray; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromMemory(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcMemory : Pointer; const SrcFormat : TD3DFormat; const SrcRowPitch, SrcSlicePitch : Cardinal; SrcPalette : PPaletteEntryArray; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromMemory(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; DestBox : PD3DBox; SrcMemory : Pointer; const SrcFormat : TD3DFormat; const SrcRowPitch, SrcSlicePitch : Cardinal; const SrcPalette : TPaletteEntryArray; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromMemory(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; DestBox : PD3DBox; SrcMemory : Pointer; const SrcFormat : TD3DFormat; const SrcRowPitch, SrcSlicePitch : Cardinal; const SrcPalette : TPaletteEntryArray; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromMemory(DestVolume : IDirect3DVolume9; DestPalette : PPaletteEntryArray; const DestBox : TD3DBox; SrcMemory : Pointer; const SrcFormat : TD3DFormat; const SrcRowPitch, SrcSlicePitch : Cardinal; const SrcPalette : TPaletteEntryArray; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXLoadVolumeFromMemory(DestVolume : IDirect3DVolume9; const DestPalette : TPaletteEntryArray; const DestBox : TD3DBox; SrcMemory : Pointer; const SrcFormat : TD3DFormat; const SrcRowPitch, SrcSlicePitch : Cardinal; const SrcPalette : TPaletteEntryArray; const SrcBox : TD3DBox; const Filter : LongWord; const ColorKey : TD3DColor) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXSaveVolumeToFile:
 * ---------------------
 * Save a volume to a image file.
 *
 * Parameters:
 *  pDestFile
 *      File name of the destination file
 *  DestFormat
 *      D3DXIMAGE_FILEFORMAT specifying file format to use when saving.
 *  pSrcVolume
 *      Source volume, containing the image to be saved
 *  pSrcPalette
 *      Source palette of 256 colors, or NULL
 *  pSrcBox
 *      Source box, or NULL for the entire volume
 *******************************************************************************
(*)

function D3DXSaveVolumeToFileA(DestFile : PAnsiChar; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; SrcPalette : PPaletteEntryArray; SrcBox : PD3DBox) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveVolumeToFileA(DestFile : PAnsiChar; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; const SrcPalette : TPaletteEntryArray; SrcBox : PD3DBox) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveVolumeToFileA(DestFile : PAnsiChar; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; SrcPalette : PPaletteEntryArray; const SrcBox : TD3DBox) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveVolumeToFileA(DestFile : PAnsiChar; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; const SrcPalette : TPaletteEntryArray; const SrcBox : TD3DBox) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXSaveVolumeToFileW(DestFile : PWideChar; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; SrcPalette : PPaletteEntryArray; SrcBox : PD3DBox) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveVolumeToFileW(DestFile : PWideChar; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; const SrcPalette : TPaletteEntryArray; SrcBox : PD3DBox) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveVolumeToFileW(DestFile : PWideChar; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; SrcPalette : PPaletteEntryArray; const SrcBox : TD3DBox) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveVolumeToFileW(DestFile : PWideChar; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; const SrcPalette : TPaletteEntryArray; const SrcBox : TD3DBox) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXSaveVolumeToFile(DestFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; SrcPalette : PPaletteEntryArray; SrcBox : PD3DBox) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXSaveVolumeToFileW';{$ELSE}'D3DXSaveVolumeToFileA';{$ENDIF}
function D3DXSaveVolumeToFile(DestFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; const SrcPalette : TPaletteEntryArray; SrcBox : PD3DBox) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXSaveVolumeToFileW';{$ELSE}'D3DXSaveVolumeToFileA';{$ENDIF}
function D3DXSaveVolumeToFile(DestFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; SrcPalette : PPaletteEntryArray; const SrcBox : TD3DBox) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXSaveVolumeToFileW';{$ELSE}'D3DXSaveVolumeToFileA';{$ENDIF}
function D3DXSaveVolumeToFile(DestFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; const SrcPalette : TPaletteEntryArray; const SrcBox : TD3DBox) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXSaveVolumeToFileW';{$ELSE}'D3DXSaveVolumeToFileA';{$ENDIF}

(*)
 *******************************************************************************
 * D3DXSaveVolumeToFileInMemory:
 * ---------------------
 * Save a volume to a image file.
 *
 * Parameters:
 *  pDestFile
 *      File name of the destination file
 *  DestFormat
 *      D3DXIMAGE_FILEFORMAT specifying file format to use when saving.
 *  pSrcVolume
 *      Source volume, containing the image to be saved
 *  pSrcPalette
 *      Source palette of 256 colors, or NULL
 *  pSrcBox
 *      Source box, or NULL for the entire volume
 *
 *******************************************************************************
(*)

function D3DXSaveVolumeToFileInMemory(DestBuf : PID3DXBuffer; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; SrcPalette : PPaletteEntryArray; SrcBox : PD3DBox) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveVolumeToFileInMemory(DestBuf : PID3DXBuffer; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; const SrcPalette : TPaletteEntryArray; SrcBox : PD3DBox) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveVolumeToFileInMemory(DestBuf : PID3DXBuffer; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; SrcPalette : PPaletteEntryArray; const SrcBox : TD3DBox) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveVolumeToFileInMemory(DestBuf : PID3DXBuffer; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; const SrcPalette : TPaletteEntryArray; const SrcBox : TD3DBox) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveVolumeToFileInMemory(out DestBuf : ID3DXBuffer; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; SrcPalette : PPaletteEntryArray; SrcBox : PD3DBox) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveVolumeToFileInMemory(out DestBuf : ID3DXBuffer; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; const SrcPalette : TPaletteEntryArray; SrcBox : PD3DBox) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveVolumeToFileInMemory(out DestBuf : ID3DXBuffer; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; SrcPalette : PPaletteEntryArray; const SrcBox : TD3DBox) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveVolumeToFileInMemory(out DestBuf : ID3DXBuffer; DestFormat : TD3DXImageFileFormat; SrcVolume : IDirect3DVolume9; const SrcPalette : TPaletteEntryArray; const SrcBox : TD3DBox) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * Create/Save Texture APIs
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 * D3DXCheckTextureRequirements:
 * -----------------------------
 * Checks texture creation parameters.  If parameters are invalid, this
 * function returns corrected parameters.
 *
 * Parameters:
 *
 *  pDevice
 *      The D3D device to be used
 *  pWidth, pHeight, pDepth, pSize
 *      Desired size in pixels, or NULL.  Returns corrected size.
 *  pNumMipLevels
 *      Number of desired mipmap levels, or NULL.  Returns corrected number.
 *  Usage
 *      Texture usage flags
 *  pFormat
 *      Desired pixel format, or NULL.  Returns corrected format.
 *  Pool
 *      Memory pool to be used to create texture
 *******************************************************************************
(*)

function D3DXCheckTextureRequirements(Device : IDirect3DDevice9; Width, Height, NumMipLevels : PCardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckTextureRequirements(Device : IDirect3DDevice9; var Width : Cardinal; Height, NumMipLevels : PCardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckTextureRequirements(Device : IDirect3DDevice9; Width : PCardinal; var Height : Cardinal; NumMipLevels : PCardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckTextureRequirements(Device : IDirect3DDevice9; var Width, Height : Cardinal; NumMipLevels : PCardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckTextureRequirements(Device : IDirect3DDevice9; Width, Height : PCardinal; var NumMipLevels : Cardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckTextureRequirements(Device : IDirect3DDevice9; var Width : Cardinal; Height : PCardinal; var NumMipLevels : Cardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckTextureRequirements(Device : IDirect3DDevice9; Width : PCardinal; var Height, NumMipLevels : Cardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckTextureRequirements(Device : IDirect3DDevice9; var Width, Height, NumMipLevels : Cardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckTextureRequirements(Device : IDirect3DDevice9; Width, Height, NumMipLevels : PCardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckTextureRequirements(Device : IDirect3DDevice9; var Width : Cardinal; Height, NumMipLevels : PCardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckTextureRequirements(Device : IDirect3DDevice9; Width : PCardinal; var Height : Cardinal; NumMipLevels : PCardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckTextureRequirements(Device : IDirect3DDevice9; var Width, Height : Cardinal; NumMipLevels : PCardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckTextureRequirements(Device : IDirect3DDevice9; Width, Height : PCardinal; var NumMipLevels : Cardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckTextureRequirements(Device : IDirect3DDevice9; var Width : Cardinal; Height : PCardinal; var NumMipLevels : Cardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckTextureRequirements(Device : IDirect3DDevice9; Width : PCardinal; var Height, NumMipLevels : Cardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckTextureRequirements(Device : IDirect3DDevice9; var Width, Height, NumMipLevels : Cardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;


function D3DXCheckCubeTextureRequirements(Device : IDirect3DDevice9; Size, NumMipLevels : PCardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckCubeTextureRequirements(Device : IDirect3DDevice9; var Size : Cardinal; NumMipLevels : PCardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckCubeTextureRequirements(Device : IDirect3DDevice9; Size : PCardinal; var NumMipLevels : Cardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckCubeTextureRequirements(Device : IDirect3DDevice9; var Size, NumMipLevels : Cardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckCubeTextureRequirements(Device : IDirect3DDevice9; Size, NumMipLevels : PCardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckCubeTextureRequirements(Device : IDirect3DDevice9; var Size : Cardinal; NumMipLevels : PCardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckCubeTextureRequirements(Device : IDirect3DDevice9; Size : PCardinal; var NumMipLevels : Cardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckCubeTextureRequirements(Device : IDirect3DDevice9; var Size, NumMipLevels : Cardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; Width, Height, Depth, NumMipLevels : PCardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; var Width : Cardinal; Height, Depth, NumMipLevels : PCardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; Width : PCardinal; var Height : Cardinal; Depth, NumMipLevels : PCardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; var Width, Height : Cardinal; Depth, NumMipLevels : PCardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; Width, Height : PCardinal; var Depth : Cardinal; NumMipLevels : PCardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; var Width : Cardinal; Height : PCardinal; var Depth : Cardinal; NumMipLevels : PCardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; Width : PCardinal; var Height, Depth : Cardinal; NumMipLevels : PCardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; var Width, Height, Depth : Cardinal; NumMipLevels : PCardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; Width, Height, Depth : PCardinal; var NumMipLevels : Cardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; var Width : Cardinal; Height, Depth : PCardinal; var NumMipLevels : Cardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; Width : PCardinal; var Height : Cardinal; Depth : PCardinal; var NumMipLevels : Cardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; var Width, Height : Cardinal; Depth : PCardinal; var NumMipLevels : Cardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; Width, Height : PCardinal; var Depth, NumMipLevels : Cardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; var Width : Cardinal; Height : PCardinal; var Depth, NumMipLevels : Cardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; Width : PCardinal; var Height, Depth, NumMipLevels : Cardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; var Width, Height, Depth, NumMipLevels : Cardinal; const Usage : LongWord; Format : PD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; Width, Height, Depth, NumMipLevels : PCardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; var Width : Cardinal; Height, Depth, NumMipLevels : PCardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; Width : PCardinal; var Height : Cardinal; Depth, NumMipLevels : PCardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; var Width, Height : Cardinal; Depth, NumMipLevels : PCardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; Width, Height : PCardinal; var Depth : Cardinal; NumMipLevels : PCardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; var Width : Cardinal; Height : PCardinal; var Depth : Cardinal; NumMipLevels : PCardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; Width : PCardinal; var Height, Depth : Cardinal; NumMipLevels : PCardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; var Width, Height, Depth : Cardinal; NumMipLevels : PCardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; Width, Height, Depth : PCardinal; var NumMipLevels : Cardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; var Width : Cardinal; Height, Depth : PCardinal; var NumMipLevels : Cardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; Width : PCardinal; var Height : Cardinal; Depth : PCardinal; var NumMipLevels : Cardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; var Width, Height : Cardinal; Depth : PCardinal; var NumMipLevels : Cardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; Width, Height : PCardinal; var Depth, NumMipLevels : Cardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; var Width : Cardinal; Height : PCardinal; var Depth, NumMipLevels : Cardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; Width : PCardinal; var Height, Depth, NumMipLevels : Cardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCheckVolumeTextureRequirements(Device : IDirect3DDevice9; var Width, Height, Depth, NumMipLevels : Cardinal; const Usage : LongWord; var Format : TD3DFormat; Pool : TD3DPool) : HResult; stdcall; overload; external d3dx9dllname;


(*)
 *******************************************************************************
 * D3DXCreateTexture:
 * ------------------
 * Create an empty texture
 *
 * Parameters:
 *
 *  pDevice
 *      The D3D device with which the texture is going to be used.
 *  Width, Height, Depth, Size
 *      size in pixels. these must be non-zero
 *  MipLevels
 *      number of mip levels desired. if zero or D3DX_DEFAULT, a complete
 *      mipmap chain will be created.
 *  Usage
 *      Texture usage flags
 *  Format
 *      Pixel format.
 *  Pool
 *      Memory pool to be used to create texture
 *  ppTexture, ppCubeTexture, ppVolumeTexture
 *      The texture object that will be created
 *******************************************************************************
(*)

function D3DXCreateTexture(Device : IDirect3DDevice9; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTexture(Device : IDirect3DDevice9; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateCubeTexture(Device : IDirect3DDevice9; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTexture(Device : IDirect3DDevice9; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateVolumeTexture(Device : IDirect3DDevice9; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTexture(Device : IDirect3DDevice9; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXCreateTextureFromFile/Resource:
 * -----------------------------------
 * Create a texture object from a file or resource.
 *
 * Parameters:
 *
 *  pDevice
 *      The D3D device with which the texture is going to be used.
 *  pSrcFile
 *      File name.
 *  hSrcModule
 *      Module handle. if NULL, current module will be used.
 *  pSrcResource
 *      Resource name in module
 *  pvSrcData
 *      Pointer to file in memory.
 *  SrcDataSize
 *      Size in bytes of file in memory.
 *  Width, Height, Depth, Size
 *      Size in pixels.  If zero or D3DX_DEFAULT, the size will be taken from
 *      the file and rounded up to a power of two.  If D3DX_DEFAULT_NONPOW2,
 *      the size will be not be rounded, if the device supports NONPOW2 textures.
 *  MipLevels
 *      Number of mip levels.  If zero or D3DX_DEFAULT, a complete mipmap
 *      chain will be created.
 *  Usage
 *      Texture usage flags
 *  Format
 *      Desired pixel format.  If D3DFMT_UNKNOWN, the format will be
 *      taken from the file.
 *  Pool
 *      Memory pool to be used to create texture
 *  Filter
 *      D3DX_FILTER flags controlling how the image is filtered.
 *      Or D3DX_DEFAULT for D3DX_FILTER_TRIANGLE.
 *  MipFilter
 *      D3DX_FILTER flags controlling how each miplevel is filtered.
 *      Or D3DX_DEFAULT for D3DX_FILTER_BOX,
 *  ColorKey
 *      Color to replace with transparent black, or 0 to disable colorkey.
 *      This is always a 32-bit ARGB color, independent of the source image
 *      format.  Alpha is significant, and should usually be set to FF for
 *      opaque colorkeys.  (ex. Opaque black == 0xff000000)
 *  pSrcInfo
 *      Pointer to a D3DXIMAGE_INFO structure to be filled in with the
 *      description of the data in the source image file, or NULL.
 *  pPalette
 *      256 color palette to be filled in, or NULL
 *  ppTexture, ppCubeTexture, ppVolumeTexture
 *      The texture object that will be created
 *******************************************************************************
(*)

// FromFile

function D3DXCreateTextureFromFileA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateTextureFromFileW(Device : IDirect3DDevice9; SrcFile : PWideChar; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileW(Device : IDirect3DDevice9; SrcFile : PWideChar; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateTextureFromFile(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromFileW';{$ELSE}'D3DXCreateTextureFromFileA';{$ENDIF}
function D3DXCreateTextureFromFile(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromFileW';{$ELSE}'D3DXCreateTextureFromFileA';{$ENDIF}

function D3DXCreateCubeTextureFromFileA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateCubeTextureFromFileW(Device : IDirect3DDevice9; SrcFile : PWideChar; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileW(Device : IDirect3DDevice9; SrcFile : PWideChar; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateCubeTextureFromFile(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromFileW';{$ELSE}'D3DXCreateCubeTextureFromFileA';{$ENDIF}
function D3DXCreateCubeTextureFromFile(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromFileW';{$ELSE}'D3DXCreateCubeTextureFromFileA';{$ENDIF}

function D3DXCreateVolumeTextureFromFileA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateVolumeTextureFromFileW(Device : IDirect3DDevice9; SrcFile : PWideChar; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileW(Device : IDirect3DDevice9; SrcFile : PWideChar; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateVolumeTextureFromFile(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromFileW';{$ELSE}'D3DXCreateVolumeTextureFromFileA';{$ENDIF}
function D3DXCreateVolumeTextureFromFile(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromFileW';{$ELSE}'D3DXCreateVolumeTextureFromFileA';{$ENDIF}

// FromResource

function D3DXCreateTextureFromResourceA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromResourceA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateTextureFromResourceW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromResourceW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateTextureFromResource(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromResourceW';{$ELSE}'D3DXCreateTextureFromResourceA';{$ENDIF}
function D3DXCreateTextureFromResource(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromResourceW';{$ELSE}'D3DXCreateTextureFromResourceA';{$ENDIF}

function D3DXCreateCubeTextureFromResourceA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromResourceA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateCubeTextureFromResourceW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromResourceW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateCubeTextureFromResource(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromResourceW';{$ELSE}'D3DXCreateCubeTextureFromResourceA';{$ENDIF}
function D3DXCreateCubeTextureFromResource(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromResourceW';{$ELSE}'D3DXCreateCubeTextureFromResourceA';{$ENDIF}

function D3DXCreateVolumeTextureFromResourceA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromResourceA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateVolumeTextureFromResourceW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromResourceW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateVolumeTextureFromResource(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromResourceW';{$ELSE}'D3DXCreateVolumeTextureFromResourceA';{$ENDIF}
function D3DXCreateVolumeTextureFromResource(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromResourceW';{$ELSE}'D3DXCreateVolumeTextureFromResourceA';{$ENDIF}

// FromFileEx

function D3DXCreateTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromFileExW';{$ELSE}'D3DXCreateTextureFromFileExA';{$ENDIF}
function D3DXCreateTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromFileExW';{$ELSE}'D3DXCreateTextureFromFileExA';{$ENDIF}
function D3DXCreateTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromFileExW';{$ELSE}'D3DXCreateTextureFromFileExA';{$ENDIF}
function D3DXCreateTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromFileExW';{$ELSE}'D3DXCreateTextureFromFileExA';{$ENDIF}
function D3DXCreateTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromFileExW';{$ELSE}'D3DXCreateTextureFromFileExA';{$ENDIF}
function D3DXCreateTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromFileExW';{$ELSE}'D3DXCreateTextureFromFileExA';{$ENDIF}
function D3DXCreateTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromFileExW';{$ELSE}'D3DXCreateTextureFromFileExA';{$ENDIF}
function D3DXCreateTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromFileExW';{$ELSE}'D3DXCreateTextureFromFileExA';{$ENDIF}

function D3DXCreateCubeTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateCubeTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateCubeTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromFileExW';{$ELSE}'D3DXCreateCubeTextureFromFileExA';{$ENDIF}
function D3DXCreateCubeTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromFileExW';{$ELSE}'D3DXCreateCubeTextureFromFileExA';{$ENDIF}
function D3DXCreateCubeTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromFileExW';{$ELSE}'D3DXCreateCubeTextureFromFileExA';{$ENDIF}
function D3DXCreateCubeTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromFileExW';{$ELSE}'D3DXCreateCubeTextureFromFileExA';{$ENDIF}
function D3DXCreateCubeTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromFileExW';{$ELSE}'D3DXCreateCubeTextureFromFileExA';{$ENDIF}
function D3DXCreateCubeTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromFileExW';{$ELSE}'D3DXCreateCubeTextureFromFileExA';{$ENDIF}
function D3DXCreateCubeTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromFileExW';{$ELSE}'D3DXCreateCubeTextureFromFileExA';{$ENDIF}
function D3DXCreateCubeTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromFileExW';{$ELSE}'D3DXCreateCubeTextureFromFileExA';{$ENDIF}

function D3DXCreateVolumeTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileExA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateVolumeTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileExW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateVolumeTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromFileExW';{$ELSE}'D3DXCreateVolumeTextureFromFileExA';{$ENDIF}
function D3DXCreateVolumeTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromFileExW';{$ELSE}'D3DXCreateVolumeTextureFromFileExA';{$ENDIF}
function D3DXCreateVolumeTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromFileExW';{$ELSE}'D3DXCreateVolumeTextureFromFileExA';{$ENDIF}
function D3DXCreateVolumeTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromFileExW';{$ELSE}'D3DXCreateVolumeTextureFromFileExA';{$ENDIF}
function D3DXCreateVolumeTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromFileExW';{$ELSE}'D3DXCreateVolumeTextureFromFileExA';{$ENDIF}
function D3DXCreateVolumeTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromFileExW';{$ELSE}'D3DXCreateVolumeTextureFromFileExA';{$ENDIF}
function D3DXCreateVolumeTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromFileExW';{$ELSE}'D3DXCreateVolumeTextureFromFileExA';{$ENDIF}
function D3DXCreateVolumeTextureFromFileEx(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromFileExW';{$ELSE}'D3DXCreateVolumeTextureFromFileExA';{$ENDIF}

// FromResourceEx

function D3DXCreateTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromResourceExW';{$ELSE}'D3DXCreateTextureFromResourceExA';{$ENDIF}
function D3DXCreateTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromResourceExW';{$ELSE}'D3DXCreateTextureFromResourceExA';{$ENDIF}
function D3DXCreateTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromResourceExW';{$ELSE}'D3DXCreateTextureFromResourceExA';{$ENDIF}
function D3DXCreateTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromResourceExW';{$ELSE}'D3DXCreateTextureFromResourceExA';{$ENDIF}
function D3DXCreateTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromResourceExW';{$ELSE}'D3DXCreateTextureFromResourceExA';{$ENDIF}
function D3DXCreateTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromResourceExW';{$ELSE}'D3DXCreateTextureFromResourceExA';{$ENDIF}
function D3DXCreateTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromResourceExW';{$ELSE}'D3DXCreateTextureFromResourceExA';{$ENDIF}
function D3DXCreateTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextureFromResourceExW';{$ELSE}'D3DXCreateTextureFromResourceExA';{$ENDIF}

function D3DXCreateCubeTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateCubeTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateCubeTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromResourceExW';{$ELSE}'D3DXCreateCubeTextureFromResourceExA';{$ENDIF}
function D3DXCreateCubeTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromResourceExW';{$ELSE}'D3DXCreateCubeTextureFromResourceExA';{$ENDIF}
function D3DXCreateCubeTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromResourceExW';{$ELSE}'D3DXCreateCubeTextureFromResourceExA';{$ENDIF}
function D3DXCreateCubeTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromResourceExW';{$ELSE}'D3DXCreateCubeTextureFromResourceExA';{$ENDIF}
function D3DXCreateCubeTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromResourceExW';{$ELSE}'D3DXCreateCubeTextureFromResourceExA';{$ENDIF}
function D3DXCreateCubeTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromResourceExW';{$ELSE}'D3DXCreateCubeTextureFromResourceExA';{$ENDIF}
function D3DXCreateCubeTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromResourceExW';{$ELSE}'D3DXCreateCubeTextureFromResourceExA';{$ENDIF}
function D3DXCreateCubeTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateCubeTextureFromResourceExW';{$ELSE}'D3DXCreateCubeTextureFromResourceExA';{$ENDIF}

function D3DXCreateVolumeTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromResourceExA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateVolumeTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromResourceExW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateVolumeTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromResourceExW';{$ELSE}'D3DXCreateVolumeTextureFromResourceExA';{$ENDIF}
function D3DXCreateVolumeTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromResourceExW';{$ELSE}'D3DXCreateVolumeTextureFromResourceExA';{$ENDIF}
function D3DXCreateVolumeTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromResourceExW';{$ELSE}'D3DXCreateVolumeTextureFromResourceExA';{$ENDIF}
function D3DXCreateVolumeTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromResourceExW';{$ELSE}'D3DXCreateVolumeTextureFromResourceExA';{$ENDIF}
function D3DXCreateVolumeTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromResourceExW';{$ELSE}'D3DXCreateVolumeTextureFromResourceExA';{$ENDIF}
function D3DXCreateVolumeTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromResourceExW';{$ELSE}'D3DXCreateVolumeTextureFromResourceExA';{$ENDIF}
function D3DXCreateVolumeTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromResourceExW';{$ELSE}'D3DXCreateVolumeTextureFromResourceExA';{$ENDIF}
function D3DXCreateVolumeTextureFromResourceEx(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateVolumeTextureFromResourceExW';{$ELSE}'D3DXCreateVolumeTextureFromResourceExA';{$ENDIF}

// FromFileInMemory

function D3DXCreateTextureFromFileInMemory(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileInMemory(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateCubeTextureFromFileInMemory(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileInMemory(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateVolumeTextureFromFileInMemory(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileInMemory(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

// FromFileInMemoryEx

function D3DXCreateTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; Texture : PIDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Width, Height, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out Texture : IDirect3DTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateCubeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; CubeTexture : PIDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Size, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out CubeTexture : IDirect3DCubeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateCubeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCubeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize : Cardinal; const Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; const Filter, MipFilter : LongWord; const ColorKey : TD3DColor; var SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateVolumeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize, Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; Filter, MipFilter : LongWord; ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize, Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; Filter, MipFilter : LongWord; ColorKey : TD3DColor; out SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize, Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; Filter, MipFilter : LongWord; ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize, Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; Filter, MipFilter : LongWord; ColorKey : TD3DColor; out SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; VolumeTexture : PIDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize, Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; Filter, MipFilter : LongWord; ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; Palette : PPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize, Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; Filter, MipFilter : LongWord; ColorKey : TD3DColor; out SrcInfo : TD3DXImageInfo; Palette : PPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize, Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; Filter, MipFilter : LongWord; ColorKey : TD3DColor; SrcInfo : PD3DXImageInfo; out Palette : TPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateVolumeTextureFromFileInMemoryEx(Device : IDirect3DDevice9; SrcData : Pointer; const SrcDataSize, Width, Height, Depth, MipLevels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; Filter, MipFilter : LongWord; ColorKey : TD3DColor; out SrcInfo : TD3DXImageInfo; out Palette : TPaletteEntryArray; out VolumeTexture : IDirect3DVolumeTexture9) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXSaveTextureToFile:
 * ----------------------
 * Save a texture to a file.
 *
 * Parameters:
 *  pDestFile
 *      File name of the destination file
 *  DestFormat
 *      D3DXIMAGE_FILEFORMAT specifying file format to use when saving.
 *  pSrcTexture
 *      Source texture, containing the image to be saved
 *  pSrcPalette
 *      Source palette of 256 colors, or NULL
 *******************************************************************************
(*)

function D3DXSaveTextureToFileA(DestFile : PAnsiChar; DestFormat : TD3DXImageFileformat; SrcTexture : IDirect3DBaseTexture9; const SrcPalette : TPaletteEntryArray) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveTextureToFileA(DestFile : PAnsiChar; DestFormat : TD3DXImageFileformat; SrcTexture : IDirect3DBaseTexture9; SrcPalette : PPaletteEntryArray) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXSaveTextureToFileW(DestFile : PWideChar; DestFormat : TD3DXImageFileformat; SrcTexture : IDirect3DBaseTexture9; const SrcPalette : TPaletteEntryArray) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveTextureToFileW(DestFile : PWideChar; DestFormat : TD3DXImageFileformat; SrcTexture : IDirect3DBaseTexture9; SrcPalette : PPaletteEntryArray) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXSaveTextureToFile(DestFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; DestFormat : TD3DXImageFileformat; SrcTexture : IDirect3DBaseTexture9; const SrcPalette : TPaletteEntryArray) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXSaveTextureToFileW';{$ELSE}'D3DXSaveTextureToFileA';{$ENDIF}
function D3DXSaveTextureToFile(DestFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; DestFormat : TD3DXImageFileformat; SrcTexture : IDirect3DBaseTexture9; SrcPalette : PPaletteEntryArray) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXSaveTextureToFileW';{$ELSE}'D3DXSaveTextureToFileA';{$ENDIF}

//----------------------------------------------------------------------------
// D3DXSaveTextureToFileInMemory:
// ----------------------
// Save a texture to a file.
//
// Parameters:
//  ppDestBuf
//      address of a d3dxbuffer pointer to return the image data
//  DestFormat
//      D3DXIMAGE_FILEFORMAT specifying file format to use when saving.
//  pSrcTexture
//      Source texture, containing the image to be saved
//  pSrcPalette
//      Source palette of 256 colors, or NULL
//
//----------------------------------------------------------------------------


function D3DXSaveTextureToFileInMemory(DestBuf : PID3DXBuffer; DestFormat : TD3DXImageFileformat; SrcTexture : IDirect3DBaseTexture9; const SrcPalette : TPaletteEntryArray) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveTextureToFileInMemory(DestBuf : PID3DXBuffer; DestFormat : TD3DXImageFileformat; SrcTexture : IDirect3DBaseTexture9; SrcPalette : PPaletteEntryArray) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXSaveTextureToFileInMemory(out DestBuf : ID3DXBuffer; DestFormat : TD3DXImageFileformat; SrcTexture : IDirect3DBaseTexture9; const SrcPalette : TPaletteEntryArray) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXSaveTextureToFileInMemory(out DestBuf : ID3DXBuffer; DestFormat : TD3DXImageFileformat; SrcTexture : IDirect3DBaseTexture9; SrcPalette : PPaletteEntryArray) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * Misc Texture APIs
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 * D3DXFilterTexture:
 * ------------------
 * Filters mipmaps levels of a texture.
 *
 * Parameters:
 *  pBaseTexture
 *      The texture object to be filtered
 *  pPalette
 *      256 color palette to be used, or NULL for non-palettized formats
 *  SrcLevel
 *      The level whose image is used to generate the subsequent levels.
 *  Filter
 *      D3DX_FILTER flags controlling how each miplevel is filtered.
 *      Or D3DX_DEFAULT for D3DX_FILTER_BOX,
 *******************************************************************************
(*)

function D3DXFilterTexture(BaseTexture : IDirect3DBaseTexture9; out Palette : TPaletteEntryArray; const SrcLevel : Cardinal; const Filter : LongWord) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXFilterTexture(BaseTexture : IDirect3DBaseTexture9; Palette : PPaletteEntryArray; const SrcLevel : Cardinal; const Filter : LongWord) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXFilterCubeTexture(BaseTexture : IDirect3DBaseTexture9; out Palette : TPaletteEntryArray; const SrcLevel : Cardinal; const Filter : LongWord) : HResult; stdcall; overload; external d3dx9dllname name 'D3DXFilterTexture';
function D3DXFilterCubeTexture(BaseTexture : IDirect3DBaseTexture9; Palette : PPaletteEntryArray; const SrcLevel : Cardinal; const Filter : LongWord) : HResult; stdcall; overload; external d3dx9dllname name 'D3DXFilterTexture';

function D3DXFilterVolumeTexture(BaseTexture : IDirect3DBaseTexture9; out Palette : TPaletteEntryArray; const SrcLevel : Cardinal; const Filter : LongWord) : HResult; stdcall; overload; external d3dx9dllname name 'D3DXFilterTexture';
function D3DXFilterVolumeTexture(BaseTexture : IDirect3DBaseTexture9; Palette : PPaletteEntryArray; const SrcLevel : Cardinal; const Filter : LongWord) : HResult; stdcall; overload; external d3dx9dllname name 'D3DXFilterTexture';

(*)
 *******************************************************************************
 * D3DXFillTexture:
 * ----------------
 * Uses a user provided function to fill each texel of each mip level of a
 * given texture.
 *
 * Paramters:
 *  pTexture, pCubeTexture, pVolumeTexture
 *      Pointer to the texture to be filled.
 *  pFunction
 *      Pointer to user provided evalutor function which will be used to
 *      compute the value of each texel.
 *  pData
 *      Pointer to an arbitrary block of user defined data.  This pointer
 *      will be passed to the function provided in pFunction
 *******************************************************************************
(*)

function D3DXFillTexture(Texture : IDirect3DTexture9; _Function : TD3DXFill2D; Data : Pointer) : HResult; stdcall; external d3dx9dllname;

function D3DXFillCubeTexture(CubeTexture : IDirect3DCubeTexture9; _Function : TD3DXFill3D; Data : Pointer) : HResult; stdcall; external d3dx9dllname;

function D3DXFillVolumeTexture(VolumeTexture : IDirect3DVolumeTexture9; _Function : TD3DXFill3D; Data : Pointer) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXFillTextureTX:
 * ----------------
 * Uses a TX Shader target to function to fill each texel of each mip level of a
 * given texture. The TX Shader target should be a compiled function taking 2
 * 2 paramters and returning a float4 color.
 *
 * Paramters:
 *  pTexture, pCubeTexture, pVolumeTexture
 *      Pointer to the texture to be filled.
 *  pFunction:
 *      Pointer to the compiled function returned by D3DX
 *  pConstants
 *      Constants used by program. Should be filled by user by parsing constant
 *      Table information
 *  Constants
 *      Number of Constants
 *******************************************************************************
(*)

function D3DXFillTextureTX(Texture : IDirect3DTexture9; _Function : PLongWord; const Constants : TD3DXVector4; const ConstantCount : Cardinal) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXFillTextureTX(Texture : IDirect3DTexture9; _Function : PLongWord; Constants : PD3DXVector4; const ConstantCount : Cardinal) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXFillCubeTextureTX(CubeTexture : IDirect3DCubeTexture9; _Function : PLongWord; const Constants : TD3DXVector4; const ConstantCount : Cardinal) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXFillCubeTextureTX(CubeTexture : IDirect3DCubeTexture9; _Function : PLongWord; Constants : PD3DXVector4; const ConstantCount : Cardinal) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXFillVolumeTextureTX(VolumeTexture : IDirect3DVolumeTexture9; _Function : PLongWord; const Constants : TD3DXVector4; const ConstantCount : Cardinal) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXFillVolumeTextureTX(VolumeTexture : IDirect3DVolumeTexture9; _Function : PLongWord; Constants : PD3DXVector4; const ConstantCount : Cardinal) : HResult; stdcall; overload; external d3dx9dllname;


(*)
 *******************************************************************************
 * D3DXComputeNormalMap:
 * ---------------------
 * Converts a height map into a normal map.  The (x,y,z) components of each
 * normal are mapped to the (r,g,b) channels of the output texture.
 *
 * Parameters
 *  pTexture
 *      Pointer to the destination texture
 *  pSrcTexture
 *      Pointer to the source heightmap texture
 *  pSrcPalette
 *      Source palette of 256 colors, or NULL
 *  Flags
 *      D3DX_NORMALMAP flags
 *  Channel
 *      D3DX_CHANNEL specifying source of height information
 *  Amplitude
 *      The constant value which the height information is multiplied by.
 *******************************************************************************
(*)

function D3DXComputeNormalMap(Texture, SrcTexture : IDirect3DTexture9; const SrcPalette : TPaletteEntryArray; const Flags, Channel : LongWord; const Amplitude : Single) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXComputeNormalMap(Texture, SrcTexture : IDirect3DTexture9; SrcPalette : PPaletteEntryArray; const Flags, Channel : LongWord; const Amplitude : Single) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  Copyright (c) Microsoft Corporation.  All rights reserved.
 *
 *  File    : d3dx9shader.h
 *  Content : D3DX Shader APIs
 *
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 * D3DXTX_VERSION:
 * --------------
 * Version token used to create a procedural texture filler in effects
 * Used by D3DXFill[]TX functions
 *******************************************************************************
(*)

function D3DXTX_VERSION(_Major, _Minor : Byte) : LongWord;

(*)
 *******************************************************************************
 * D3DXSHADER flags:
 * -----------------
 * D3DXSHADER_DEBUG
 *   Insert debug file/line/type/symbol information.
 *
 * D3DXSHADER_SKIPVALIDATION
 *   Do not validate the generated code against known capabilities and
 *   constraints.  This option is only recommended when compiling shaders
 *   you KNOW will work.  (ie. have compiled before without this option.)
 *   Shaders are always validated by D3D before they are set to the device.
 *
 * D3DXSHADER_SKIPOPTIMIZATION (valid for D3DXCompileShader calls only)
 *   Instructs the compiler to skip optimization steps during code generation.
 *   Unless you are trying to isolate a problem in your code, and suspect the
 *   compiler, using this option is not recommended.
 *
 * D3DXSHADER_PACKMATRIX_ROWMAJOR
 *   Unless explicitly specified, matrices will be packed in row-major order
 *   on input and output from the shader.
 *
 * D3DXSHADER_PACKMATRIX_COLUMNMAJOR
 *   Unless explicitly specified, matrices will be packed in column-major
 *   order on input and output from the shader.  This is generally more
 *   efficient, since it allows vector-matrix multiplication to be performed
 *   using a series of dot-products.
 * D3DXSHADER_PARTIAL_PRECISION
 *   Force all computations in resulting shader to occur at partial precision.
 *   This may result in faster evaluation of shaders on some hardware.
 *
 * D3DXSHADER_FORCE_VS_SOFTWARE_NOOPT
 *   Force compiler to compile against the next highest available software
 *   target for vertex shaders.  This flag also turns optimizations off,
 *   and debugging on.
 *
 * D3DXSHADER_FORCE_PS_SOFTWARE_NOOPT
 *   Force compiler to compile against the next highest available software
 *   target for pixel shaders.  This flag also turns optimizations off,
 *   and debugging on.
 *******************************************************************************
(*)

const
  D3DXSHADER_DEBUG                   = (1 shl 0);
  D3DXSHADER_SKIPVALIDATION          = (1 shl 1);
  D3DXSHADER_SKIPOPTIMIZATION        = (1 shl 2);
  D3DXSHADER_PACKMATRIX_ROWMAJOR     = (1 shl 3);
  D3DXSHADER_PACKMATRIX_COLUMNMAJOR  = (1 shl 4);
  D3DXSHADER_PARTIALPRECISION        = (1 shl 5);
  D3DXSHADER_FORCE_VS_SOFTWARE_NOOPT = (1 shl 6);
  D3DXSHADER_FORCE_PS_SOFTWARE_NOOPT = (1 shl 7);

(*)
 *******************************************************************************
 * D3DXHANDLE:
 * -----------
 * Handle values used to efficiently reference shader and effect parameters.
 * Strings can be used as handles.  However, handles are not always strings.
 *******************************************************************************
(*)

type
  PD3DXHandle = ^TD3DXHandle;
  TD3DXHandle = PAnsiChar;

(*)
 *******************************************************************************
 * D3DXMACRO:
 * ----------
 * Preprocessor macro definition.  The application pass in a NULL-terminated
 * array of this structure to various D3DX APIs.  This enables the application
 * to #define tokens at runtime, before the file is parsed.
 *******************************************************************************
(*)

type
  PD3DXMacro = ^TD3DXMacro;
  TD3DXMacro = packed record
    Name       : PAnsiChar;
    Definition : PAnsiChar;
  end;

(*)
 *******************************************************************************
 * D3DXSEMANTIC:
 *******************************************************************************
(*)

type
  PD3DXSemantic = ^TD3DXSemantic;
  TD3DXSemantic = packed record
    Usage      : Cardinal;
    UsageIndex : Cardinal;
  end;

(*)
 *******************************************************************************
 * D3DXFRAGMENT_DESC:
 *******************************************************************************
(*)

type
  PD3DXFragmentDesc = ^TD3DXFragmentDesc;
  TD3DXFragmentDesc = packed record
    Name   : PAnsiChar;
    Target : LongWord;
  end;
  PD3DXFragment_Desc = ^TD3DXFragment_Desc;
  TD3DXFragment_Desc = TD3DXFragmentDesc;

(*)
 *******************************************************************************
 * D3DXREGISTER_SET:
 *******************************************************************************
(*)

type
  PD3DXRegisterSet = ^TD3DXRegisterSet;
  TD3DXRegisterSet = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DXRS_BOOL    = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXRS_INT4    = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXRS_FLOAT4  = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXRS_SAMPLER = 3{$IFNDEF NOENUMS}){$ENDIF};

type
  PD3DXRegister_Set = ^TD3DXRegister_Set;
  TD3DXRegister_Set = TD3DXRegisterSet;

(*)
 *******************************************************************************
 * D3DXPARAMETER_CLASS:
 *******************************************************************************
(*)

type
  TD3DXParameterClass = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DXPC_SCALAR         = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPC_VECTOR         = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPC_MATRIX_ROWS    = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPC_MATRIX_COLUMNS = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPC_OBJECT         = 4{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPC_STRUCT         = 5{$IFNDEF NOENUMS}){$ENDIF};

type
  PD3DXParameter_Class = ^TD3DXParameter_Class;
  TD3DXParameter_Class = TD3DXParameterClass;

(*)
 *******************************************************************************
 * D3DXPARAMETER_TYPE:
 *******************************************************************************
(*)

type
  PD3DXParameterType = ^TD3DXParameterType;
  TD3DXParameterType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DXPT_VOID           = 00{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_BOOL           = 01{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_INT            = 02{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_FLOAT          = 03{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_STRING         = 04{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_TEXTURE        = 05{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_TEXTURE1D      = 06{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_TEXTURE2D      = 07{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_TEXTURE3D      = 08{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_TEXTURECUBE    = 09{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_SAMPLER        = 10{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_SAMPLER1D      = 11{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_SAMPLER2D      = 12{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_SAMPLER3D      = 13{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_SAMPLERCUBE    = 14{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_PIXELSHADER    = 15{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_VERTEXSHADER   = 16{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_PIXELFRAGMENT  = 17{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPT_VERTEXFRAGMENT = 18{$IFNDEF NOENUMS}){$ENDIF};
type
  PD3DXParameter_Type = ^TD3DXParameter_Type;
  TD3DXParameter_Type = TD3DXParameterType;

(*)
 *******************************************************************************
 * D3DXCONSTANTTABLE_DESC:
 *******************************************************************************
(*)

type
  PD3DXConstantTableDesc = ^TD3DXConstantTableDesc;
  TD3DXConstantTableDesc = packed record
    Creator   : PAnsiChar; // Creator string
    Version   : LongWord;    // Shader version
    Constants : Cardinal;    // Number of constants
  end;

  PD3DXConstantTable_Desc = ^TD3DXConstantTable_Desc;
  TD3DXConstantTable_Desc = TD3DXConstantTableDesc;

(*)
 *******************************************************************************
 * D3DXCONSTANT_DESC:
 *******************************************************************************
(*)

type
  PD3DXConstantDesc = ^TD3DXConstantDesc;
  TD3DXConstantDesc = packed record
    Name          : PAnsiChar;            // Constant name

    RegisterSet   : TD3DXRegisterSet;     // Register set
    RegisterIndex : Cardinal;             // Register index
    RegisterCount : Cardinal;             // Number of registers occupied

    _Class        : TD3DXParameterClass;  // Class
    _Type         : TD3DXParameterType;   // Component type

    Rows          : Cardinal;             // Number of rows
    Columns       : Cardinal;             // Number of columns
    Elements      : Cardinal;             // Number of array elements
    StructMembers : Cardinal;             // Number of structure member sub-parameters

    Bytes         : Cardinal;             // Data size, in bytes
    DefaultValue  : Pointer;              // Pointer to default value
  end;
  PD3DXConstant_Desc = ^TD3DXConstant_Desc;
  TD3DXConstant_Desc = TD3DXConstantDesc;


(*)
 *******************************************************************************
 * ID3DXConstantTable:
 *******************************************************************************
(*)

const
  IID_ID3DXConstantTable  : TGUID = '{9DCA3190-38B9-4FC3-92E3-39C6DDFB358B}';
  IID_ID3DXFragmentLinker : TGUID = '{D59D3777-C973-4A3C-B4B0-2A62CD3D8B40}';

type
  PID3DXConstantTable = ^ID3DXConstantTable;
  ID3DXConstantTable = interface (ID3DXBuffer)
    ['{9DCA3190-38B9-4FC3-92E3-39C6DDFB358B}']
    // Descs
    function GetDesc(out Desc : TD3DXConstantTableDesc): HResult; stdcall;
    function GetConstantDesc(Constant : TD3DXHandle; ConstantDesc : PD3DXConstantDesc; var Count : Cardinal) : HResult; stdcall;

    // Handle operations
    function GetConstant(Constant : TD3DXHandle; Index : Cardinal) : TD3DXHandle; stdcall;
    function GetConstantByName(Constant : TD3DXHandle; Name : PAnsiChar) : TD3DXHandle; stdcall;
    function GetConstantElement(Constant : TD3DXHandle; Index : Cardinal) : TD3DXHandle; stdcall;

    // Set Constants
    function SetDefaults(Device : IDirect3DDevice9) : HResult; stdcall;
    function SetValue(Device : IDirect3DDevice9; Constant : TD3DXHandle; Data : Pointer; const Bytes : Cardinal) : HResult; stdcall;
    function SetBool(Device : IDirect3DDevice9; Constant : TD3DXHandle; b : Bool) : HResult; stdcall;
    function SetBoolArray(Device : IDirect3DDevice9; Constant : TD3DXHandle; b : PBool; const Count : Cardinal) : HResult; stdcall;
    function SetInt(Device : IDirect3DDevice9; Constant : TD3DXHandle; n : Integer) : HResult; stdcall;
    function SetIntArray(Device : IDirect3DDevice9; Constant : TD3DXHandle; n : PInteger; const Count : Cardinal) : HResult; stdcall;
    function SetFloat(Device : IDirect3DDevice9; Constant : TD3DXHandle; f : Single) : HResult; stdcall;
    function SetFloatArray(Device : IDirect3DDevice9; Constant : TD3DXHandle; f : PSingle; const Count : Cardinal) : HResult; stdcall;
    function SetVector(Device : IDirect3DDevice9; Constant : TD3DXHandle; const Vector : TD3DXVector4) : HResult; stdcall;
    function SetVectorArray(Device : IDirect3DDevice9; Constant : TD3DXHandle; Vector : PD3DXVector4; const Count : Cardinal) : HResult; stdcall;
    function SetMatrix(Device : IDirect3DDevice9; Constant : TD3DXHandle; const Matrix : TD3DXMatrix) : HResult; stdcall;
    function SetMatrixArray(Device : IDirect3DDevice9; Constant : TD3DXHandle; Matrix : PD3DXMatrix; const Count : Cardinal) : HResult; stdcall;
    function SetMatrixPointerArray(Device : IDirect3DDevice9; Constant : TD3DXHandle; Matrix : PPD3DXMatrix; const Count : Cardinal) : HResult; stdcall;
    function SetMatrixTranspose(Device : IDirect3DDevice9; Constant : TD3DXHandle; const Matrix : TD3DXMatrix) : HResult; stdcall;
    function SetMatrixTransposeArray(Device : IDirect3DDevice9; Constant : TD3DXHandle; Matrix : PD3DXMatrix; const Count : Cardinal) : HResult; stdcall;
    function SetMatrixTransposePointerArray(Device : IDirect3DDevice9; Constant : TD3DXHandle; Matrix : PPD3DXMatrix; const Count : Cardinal) : HResult; stdcall;
  end;

(*)
 *******************************************************************************
 * ID3DXFragmentLinker
 *******************************************************************************
(*)

type
  PID3DXFragmentLinker = ^ID3DXFragmentLinker;
  ID3DXFragmentLinker = interface (IUnknown)
    ['{D59D3777-C973-4a3c-B4B0-2A62CD3D8B40}']
    (*** ID3DXFragmentLinker methods ***)
    // fragment access and information retrieval functions
    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;
    function GetNumberOfFragments : Cardinal; stdcall;

    function GetFragmentHandleByIndex(Index : Cardinal) : TD3DXHandle; stdcall;
    function GetFragmentHandleByName(Name : PAnsiChar) : TD3DXHandle; stdcall;
    function GetFragmentDesc(Name : TD3DXHandle; out FragDesc : TD3DXFragmentDesc) : HResult; stdcall;

    // add the fragments in the buffer to the linker
    function AddFragments(Fragments : PLongWord) : HResult; stdcall;

    // Create a buffer containing the fragments.  Suitable for saving to disk
    function GetAllFragments(out Buffer : ID3DXBuffer) : HResult; stdcall;
    function GetFragment(Name : TD3DXHandle; out Buffer : ID3DXBuffer) : HResult; stdcall;

    function LinkShader(Profile : PAnsiChar; Flags : LongWord; FragmentHandles : PD3DXHandle; Fragments : Cardinal; out Buffer, ErrorMsgs : ID3DXBuffer) : HResult; stdcall;
    function LinkVertexShader(Profile : PAnsiChar; Flags : LongWord; FragmentHandles : PD3DXHandle; Fragments : Cardinal; out VShader : IDirect3DVertexShader9; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall;
    function LinkPixelShader(Profile : PAnsiChar; Flags : LongWord; FragmentHandles : PD3DXHandle; Fragments : Cardinal; out PShader : IDirect3DPixelShader9; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall;

    function ClearCache : HResult; stdcall;
  end;

(*)
 *******************************************************************************
 * D3DXINCLUDE_TYPE:
 *******************************************************************************
(*)

type
  PD3DXIncludeType = ^TD3DXIncludeType;
  TD3DXIncludeType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DXINC_LOCAL  = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXINC_SYSTEM = 1{$IFNDEF NOENUMS}){$ENDIF};
type
  PD3DXInclude_Type = ^TD3DXInclude_Type;
  TD3DXInclude_Type = TD3DXIncludeType;
(*)
 *******************************************************************************
 * ID3DXInclude:
 * *************
 * This interface is intended to be implemented by the application, and can
 * be used by various D3DX APIs.  This enables application*specific handling
 * of #include directives in source files.
 *
 * Open()
 *    Opens an include file.  If successful, it should fill in ppData and
 *    pBytes.  The data pointer returned must remain valid until Close is
 *    subsequently called.
 * Close()
 *    Closes an include file.  If Open was successful, Close is guaranteed
 *    to be called before the API using this interface returns.
 *******************************************************************************
(*)

type
  PID3DXInclude = ^ID3DXInclude;
  ID3DXInclude = interface (IUnknown) // AMP : Not sure if this will work.
    function Open(IncludeType : TD3DXIncludeType; FileName :  PAnsiChar; ParentData : Pointer; out Data : Pointer; out Bytes : Cardinal) : HResult; stdcall;
    function Close(Data : Pointer) : HResult; stdcall;
  end;


(*)
 *******************************************************************************
 * APIs
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 * D3DXAssembleShader:
 * -------------------
 * Assembles a shader.
 *
 * Parameters:
 *  pSrcFile
 *      Source file name
 *  hSrcModule
 *      Module handle. if NULL, current module will be used
 *  pSrcResource
 *      Resource name in module
 *  pSrcData
 *      Pointer to source code
 *  SrcDataLen
 *      Size of source code, in bytes
 *  pDefines
 *      Optional NULL-terminated array of preprocessor macro definitions.
 *  pInclude
 *      Optional interface pointer to use for handling #include directives.
 *      If this parameter is NULL, #includes will be honored when assembling
 *      from file, and will error when assembling from resource or memory.
 *  Flags
 *      See D3DXSHADER_xxx flags
 *  ppShader
 *      Returns a buffer containing the created shader.  This buffer contains
 *      the assembled shader code, as well as any embedded debug info.
 *  ppErrorMsgs
 *      Returns a buffer containing a listing of errors and warnings that were
 *      encountered during assembly.  If you are running in a debugger,
 *      these are the same messages you will see in your debug output.
 *******************************************************************************
(*)

function D3DXAssembleShaderFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXAssembleShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXAssembleShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderFromFileW';{$ELSE}'D3DXAssembleShaderFromFileA';{$ENDIF}
function D3DXAssembleShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderFromFileW';{$ELSE}'D3DXAssembleShaderFromFileA';{$ENDIF}
function D3DXAssembleShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderFromFileW';{$ELSE}'D3DXAssembleShaderFromFileA';{$ENDIF}
function D3DXAssembleShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderFromFileW';{$ELSE}'D3DXAssembleShaderFromFileA';{$ENDIF}
function D3DXAssembleShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderFromFileW';{$ELSE}'D3DXAssembleShaderFromFileA';{$ENDIF}
function D3DXAssembleShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderFromFileW';{$ELSE}'D3DXAssembleShaderFromFileA';{$ENDIF}
function D3DXAssembleShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderFromFileW';{$ELSE}'D3DXAssembleShaderFromFileA';{$ENDIF}
function D3DXAssembleShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderFromFileW';{$ELSE}'D3DXAssembleShaderFromFileA';{$ENDIF}

function D3DXAssembleShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXAssembleShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXAssembleShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderFromResourceW';{$ELSE}'D3DXAssembleShaderFromResourceA';{$ENDIF}
function D3DXAssembleShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderFromResourceW';{$ELSE}'D3DXAssembleShaderFromResourceA';{$ENDIF}
function D3DXAssembleShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderFromResourceW';{$ELSE}'D3DXAssembleShaderFromResourceA';{$ENDIF}
function D3DXAssembleShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderFromResourceW';{$ELSE}'D3DXAssembleShaderFromResourceA';{$ENDIF}
function D3DXAssembleShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderFromResourceW';{$ELSE}'D3DXAssembleShaderFromResourceA';{$ENDIF}
function D3DXAssembleShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderFromResourceW';{$ELSE}'D3DXAssembleShaderFromResourceA';{$ENDIF}
function D3DXAssembleShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderFromResourceW';{$ELSE}'D3DXAssembleShaderFromResourceA';{$ENDIF}
function D3DXAssembleShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderFromResourceW';{$ELSE}'D3DXAssembleShaderFromResourceA';{$ENDIF}

// AMP : Doc says A and W Versions exist, header source says they don't exist

function D3DXAssembleShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

(*
function D3DXAssembleShaderA(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderA(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderA(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderA(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderA(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderA(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderA(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderA(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXAssembleShaderW(SrcData : PWideChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderW(SrcData : PWideChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderW(SrcData : PWideChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderW(SrcData : PWideChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderW(SrcData : PWideChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderW(SrcData : PWideChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderW(SrcData : PWideChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXAssembleShaderW(SrcData : PWideChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXAssembleShader(SrcData : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderW';{$ELSE}'D3DXAssembleShaderA';{$ENDIF}
function D3DXAssembleShader(SrcData : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderW';{$ELSE}'D3DXAssembleShaderA';{$ENDIF}
function D3DXAssembleShader(SrcData : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderW';{$ELSE}'D3DXAssembleShaderA';{$ENDIF}
function D3DXAssembleShader(SrcData : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderW';{$ELSE}'D3DXAssembleShaderA';{$ENDIF}
function D3DXAssembleShader(SrcData : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderW';{$ELSE}'D3DXAssembleShaderA';{$ENDIF}
function D3DXAssembleShader(SrcData : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderW';{$ELSE}'D3DXAssembleShaderA';{$ENDIF}
function D3DXAssembleShader(SrcData : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderW';{$ELSE}'D3DXAssembleShaderA';{$ENDIF}
function D3DXAssembleShader(SrcData : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXAssembleShaderW';{$ELSE}'D3DXAssembleShaderA';{$ENDIF}
*)

(*)
 *******************************************************************************
 * D3DXCompileShader:
 * ------------------
 * Compiles a shader.
 *
 * Parameters:
 *  pSrcFile
 *      Source file name.
 *  hSrcModule
 *      Module handle. if NULL, current module will be used.
 *  pSrcResource
 *      Resource name in module.
 *  pSrcData
 *      Pointer to source code.
 *  SrcDataLen
 *      Size of source code, in bytes.
 *  pDefines
 *      Optional NULL-terminated array of preprocessor macro definitions.
 *  pInclude
 *      Optional interface pointer to use for handling #include directives.
 *      If this parameter is NULL, #includes will be honored when compiling
 *      from file, and will error when compiling from resource or memory.
 *  pFunctionName
 *      Name of the entrypoint function where execution should begin.
 *  pProfile
 *      Instruction set to be used when generating code.  Currently supported
 *      profiles are "vs_1_1", "vs_2_0", "vs_2_a", "vs_2_sw", "ps_1_1", 
 *      "ps_1_2", "ps_1_3", "ps_1_4", "ps_2_0", "ps_2_a", "ps_2_sw", "tx_1_0"
 *  Flags
 *      See D3DXSHADER_xxx flags.
 *  ppShader
 *      Returns a buffer containing the created shader.  This buffer contains
 *      the compiled shader code, as well as any embedded debug and symbol
 *      table info.  (See D3DXGetShaderConstantTable)
 *  ppErrorMsgs
 *      Returns a buffer containing a listing of errors and warnings that were
 *      encountered during the compile.  If you are running in a debugger,
 *      these are the same messages you will see in your debug output.
 *  ppConstantTable
 *      Returns a ID3DXConstantTable object which can be used to set
 *      shader constants to the device.  Alternatively, an application can
 *      parse the D3DXSHADER_CONSTANTTABLE block embedded as a comment within
 *      the shader.
 *******************************************************************************
(*)

function D3DXCompileShaderFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;

// AMP : Doc says all PAnsiChar get PWideChar, header source says they don't

function D3DXCompileShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}

(*
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
function D3DXCompileShaderFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromFileW';{$ELSE}'D3DXCompileShaderFromFileA';{$ENDIF}
*)

function D3DXCompileShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;

// AMP : Doc says all PAnsiChar get PWideChar, header source says they don't

function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}

(*
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShaderFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : PWideChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
function D3DXCompileShaderFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Target : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCompileShaderFromResourceW';{$ELSE}'D3DXCompileShaderFromResourceA';{$ENDIF}
*)

function D3DXCompileShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCompileShader(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; FunctionName, Profile : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXGetPixelShaderProfile/D3DXGetVertexShaderProfile:
 * -----------------------------------------------------
 * Returns the name of the HLSL profile best suited to a given device.
 *
 * Parameters:
 *  pDevice
 *      Pointer to the device in question
 *******************************************************************************
(*)

function D3DXGetPixelShaderProfile(Device : IDirect3DDevice9) : PAnsiChar; stdcall; external d3dx9dllname;

function D3DXGetVertexShaderProfile(Device : IDirect3DDevice9) : PAnsiChar; stdcall; external d3dx9dllname;


(*)
 *******************************************************************************
 * D3DXFindShaderComment:
 * ----------------------
 * Searches through a shader for a particular comment, denoted by a FourCC in
 * the first DWORD of the comment.  If the comment is not found, and no other
 * error has occurred, S_FALSE is returned.
 *
 * Parameters:
 *  pFunction
 *      Pointer to the function DWORD stream
 *  FourCC
 *      FourCC used to identify the desired comment block.
 *  ppData
 *      Returns a pointer to the comment data (not including comment token
 *      and FourCC).  Can be NULL.
 *  pSizeInBytes
 *      Returns the size of the comment data in bytes.  Can be NULL.
 *******************************************************************************
(*)

function D3DXFindShaderComment(_Function : PLongWord; FourCC : LongWord; Data : PPointer; SizeInBytes : PCardinal) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXFindShaderComment(_Function : PLongWord; FourCC : LongWord; out Data : Pointer; SizeInBytes : PCardinal) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXFindShaderComment(_Function : PLongWord; FourCC : LongWord; Data : PPointer; out SizeInBytes : Cardinal) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXFindShaderComment(_Function : PLongWord; FourCC : LongWord; out Data : Pointer; out SizeInBytes : Cardinal) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXGetShaderVersion:
 * ---------------------
 * Returns the shader version of a given shader.  Returns zero if the shader 
 * function is NULL.
 *
 * Parameters:
 *  pFunction
 *      Pointer to the function DWORD stream
 *******************************************************************************
(*)

function D3DXGetShaderVersion(_Function : PLongWord) : LongWord; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXGetShaderSemantics:
 * -----------------------
 * Gets semantics for all input elements referenced inside a given shader.
 *
 * Parameters:
 *  pFunction
 *      Pointer to the function DWORD stream
 *  pSemantics
 *      Pointer to an array of D3DXSEMANTIC structures.  The function will
 *      fill this array with the semantics for each input element referenced
 *      inside the shader.  This array is assumed to contain at least
 *      MAXD3DDECLLENGTH elements.
 *  pCount
 *      Returns the number of elements referenced by the shader
 *******************************************************************************
(*)

function D3DXGetShaderInputSemantics(_Function : PLongWord; Semantics : PD3DXSemantic; out Count : Cardinal) : HResult; stdcall; external d3dx9dllname;

function D3DXGetShaderOutputSemantics(_Function : PLongWord; Semantics : PD3DXSemantic; out Count : Cardinal) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXGetShaderSamplers:
 * ----------------------
 * Gets semantics for all input elements referenced inside a given shader.
 *
 * pFunction
 *      Pointer to the function DWORD stream
 * pSamplers
 *      Pointer to an array of LPCSTRs.  The function will fill this array
 *      with pointers to the sampler names contained within pFunction, for
 *      each sampler referenced inside the shader.  This array is assumed to
 *      contain at least 16 elements.
 * pCount
 *      Returns the number of samplers referenced by the shader
 *******************************************************************************
(*)

function D3DXGetShaderSamplers(_Function : PLongWord; Samplers : PPAnsiChar; out Count : Cardinal) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXGetShaderConstantTable:
 * ---------------------------
 * Gets shader constant table embedded inside shader.  A constant table is
 * generated by D3DXAssembleShader and D3DXCompileShader, and is embedded in
 * the body of the shader.
 *
 * Parameters:
 *  pFunction
 *      Pointer to the function DWORD stream
 *  ppConstantTable
 *      Returns a ID3DXConstantTable object which can be used to set
 *      shader constants to the device.  Alternatively, an application can
 *      parse the D3DXSHADER_CONSTANTTABLE block embedded as a comment within
 *      the shader.
 *******************************************************************************
(*)

function D3DXGetShaderConstantTable(_Function : PLongWord; out ConstantTable : ID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGetShaderConstantTable(_Function : PLongWord; ConstantTable : PID3DXConstantTable) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXGatherFragments:
 * -------------------
 * Assembles shader fragments into a buffer to be passed to a fragment linker.
 *   will generate shader fragments for all fragments in the file
 *
 * Parameters:
 *  pSrcFile
 *      Source file name
 *  hSrcModule
 *      Module handle. if NULL, current module will be used
 *  pSrcResource
 *      Resource name in module
 *  pSrcData
 *      Pointer to source code
 *  SrcDataLen
 *      Size of source code, in bytes
 *  pDefines
 *      Optional NULL-terminated array of preprocessor macro definitions.
 *  pInclude
 *      Optional interface pointer to use for handling #include directives.
 *      If this parameter is NULL, #includes will be honored when assembling
 *      from file, and will error when assembling from resource or memory.
 *  Flags
 *      See D3DXSHADER_xxx flags
 *  ppShader
 *      Returns a buffer containing the created shader fragments.  This buffer contains
 *      the assembled shader code, as well as any embedded debug info.
 *  ppErrorMsgs
 *      Returns a buffer containing a listing of errors and warnings that were
 *      encountered during assembly.  If you are running in a debugger,
 *      these are the same messages you will see in your debug output.
 *******************************************************************************
(*)

function D3DXGatherFragmentsFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXGatherFragmentsFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXGatherFragmentsFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsFromFileW';{$ELSE}'D3DXGatherFragmentsFromFileA';{$ENDIF}
function D3DXGatherFragmentsFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsFromFileW';{$ELSE}'D3DXGatherFragmentsFromFileA';{$ENDIF}
function D3DXGatherFragmentsFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsFromFileW';{$ELSE}'D3DXGatherFragmentsFromFileA';{$ENDIF}
function D3DXGatherFragmentsFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsFromFileW';{$ELSE}'D3DXGatherFragmentsFromFileA';{$ENDIF}
function D3DXGatherFragmentsFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsFromFileW';{$ELSE}'D3DXGatherFragmentsFromFileA';{$ENDIF}
function D3DXGatherFragmentsFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsFromFileW';{$ELSE}'D3DXGatherFragmentsFromFileA';{$ENDIF}
function D3DXGatherFragmentsFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsFromFileW';{$ELSE}'D3DXGatherFragmentsFromFileA';{$ENDIF}
function D3DXGatherFragmentsFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsFromFileW';{$ELSE}'D3DXGatherFragmentsFromFileA';{$ENDIF}

function D3DXGatherFragmentsFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXGatherFragmentsFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXGatherFragmentsFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsFromResourceW';{$ELSE}'D3DXGatherFragmentsFromResourceA';{$ENDIF}
function D3DXGatherFragmentsFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsFromResourceW';{$ELSE}'D3DXGatherFragmentsFromResourceA';{$ENDIF}
function D3DXGatherFragmentsFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsFromResourceW';{$ELSE}'D3DXGatherFragmentsFromResourceA';{$ENDIF}
function D3DXGatherFragmentsFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsFromResourceW';{$ELSE}'D3DXGatherFragmentsFromResourceA';{$ENDIF}
function D3DXGatherFragmentsFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsFromResourceW';{$ELSE}'D3DXGatherFragmentsFromResourceA';{$ENDIF}
function D3DXGatherFragmentsFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsFromResourceW';{$ELSE}'D3DXGatherFragmentsFromResourceA';{$ENDIF}
function D3DXGatherFragmentsFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsFromResourceW';{$ELSE}'D3DXGatherFragmentsFromResourceA';{$ENDIF}
function D3DXGatherFragmentsFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsFromResourceW';{$ELSE}'D3DXGatherFragmentsFromResourceA';{$ENDIF}


// AMP : Doc says A and W Versions exist, header source says they don't exist

function D3DXGatherFragments(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragments(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragments(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragments(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragments(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragments(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragments(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragments(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

(*
function D3DXGatherFragmentsA(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsA(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsA(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsA(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsA(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsA(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsA(SrcData : PAnsiChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsA(SrcData : PAnsiChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXGatherFragmentsW(SrcData : PWideChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsW(SrcData : PWideChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsW(SrcData : PWideChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsW(SrcData : PWideChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsW(SrcData : PWideChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsW(SrcData : PWideChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsW(SrcData : PWideChar; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXGatherFragmentsW(SrcData : PWideChar; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXGatherFragments(SrcData : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsW';{$ELSE}'D3DXGatherFragmentsA';{$ENDIF}
function D3DXGatherFragments(SrcData : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader, ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsW';{$ELSE}'D3DXGatherFragmentsA';{$ENDIF}
function D3DXGatherFragments(SrcData : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsW';{$ELSE}'D3DXGatherFragmentsA';{$ENDIF}
function D3DXGatherFragments(SrcData : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader : ID3DXBuffer; ErrorMsgs : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsW';{$ELSE}'D3DXGatherFragmentsA';{$ENDIF}
function D3DXGatherFragments(SrcData : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsW';{$ELSE}'D3DXGatherFragmentsA';{$ENDIF}
function D3DXGatherFragments(SrcData : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Shader : PID3DXBuffer; out ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsW';{$ELSE}'D3DXGatherFragmentsA';{$ENDIF}
function D3DXGatherFragments(SrcData : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsW';{$ELSE}'D3DXGatherFragmentsA';{$ENDIF}
function D3DXGatherFragments(SrcData : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXGatherFragmentsW';{$ELSE}'D3DXGatherFragmentsA';{$ENDIF}
*)

(*)
 *******************************************************************************
 * D3DXCreateFragmentLinker:
 * -------------------------
 * Creates a fragment linker with a given cache size.  The interface returned
 * can be used to link together shader fragments.  (both HLSL & ASM fragements)
 *
 * Parameters:
 *  pDevice
 *      Pointer of the device on which to create the shaders
 *  ShaderCacheSize
 *      Size of the shader cache
 *  ppFragmentLinker
 *      pointer to a memory location to put the created interface pointer
 *******************************************************************************
(*)

function D3DXCreateFragmentLinker(Device : IDirect3DDevice9; ShaderCacheSize : Cardinal; out FragmentLinker : ID3DXFragmentLinker) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 * Shader comment block layouts
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 * D3DXSHADER_CONSTANTTABLE:
 * -------------------------
 * Shader constant information; included as an CTAB comment block inside
 * shaders.  All offsets are BYTE offsets from start of CONSTANTTABLE struct.
 * Entries in the table are sorted by Name in ascending order.
 *******************************************************************************
(*)

type
  PD3DXShaderConstantTable = ^TD3DXShaderConstantTable;
  TD3DXShaderConstantTable = packed record
    Size         : LongWord;  // sizeof(D3DXSHADER_CONSTANTTABLE)
    Creator      : LongWord;  // LPCSTR offset
    Version      : LongWord;  // shader version
    Constants    : LongWord;  // number of constants
    ConstantInfo : LongWord;  // D3DXSHADER_CONSTANTINFO[Constants] offset
    Flags        : LongWord;  // flags shader was compiled with
    Target       : LongWord;  // LPCSTR offset

  end;
  PD3DXShader_ConstantTable = ^TD3DXShader_ConstantTable;
  TD3DXShader_ConstantTable = TD3DXShaderConstantTable;

type
  PD3DXShaderConstantInfo = ^TD3DXShaderConstantInfo;
  TD3DXShaderConstantInfo = packed record
    Name          : LongWord;  // LPCSTR offset
    RegisterSet   : Word;      // D3DXREGISTER_SET
    RegisterIndex : Word;      // register number
    RegisterCount : Word;      // number of registers
    Reserved      : Word;      // reserved
    TypeInfo      : LongWord;  // D3DXSHADER_TYPEINFO offset
    DefaultValue  : LongWord;  // offset of default value
  end;
  PD3DXShader_ConstantInfo = ^TD3DXShader_ConstantInfo;
  TD3DXShader_ConstantInfo = TD3DXShaderConstantInfo;

type
  PD3DXShaderTypeInfo = ^TD3DXShaderTypeInfo;
  TD3DXShaderTypeInfo = packed record
    _Class           : Word;      // D3DXPARAMETER_CLASS
    _Type            : Word;      // D3DXPARAMETER_TYPE
    Rows             : Word;      // number of rows (matrices)
    Columns          : Word;      // number of columns (vectors and matrices)
    Elements         : Word;      // array dimension
    StructMembers    : Word;      // number of struct members
    StructMemberInfo : LongWord;  // D3DXSHADER_STRUCTMEMBERINFO[Members] offset
  end;
  PD3DXShader_TypeInfo = ^TD3DXShader_TypeInfo;
  TD3DXShader_TypeInfo = TD3DXShaderTypeInfo;

type
  PD3DXShaderStructMemberInfo = ^TD3DXShaderStructMemberInfo;
  TD3DXShaderStructMemberInfo = packed record
    Name     : LongWord;  // LPCSTR offset
    TypeInfo : LongWord;  // D3DXSHADER_TYPEINFO offset
  end;
  PD3DXShader_StructMemberInfo = ^TD3DXShader_StructMemberInfo; 
  TD3DXShader_StructMemberInfo = TD3DXShaderStructMemberInfo; 


(*)
 *******************************************************************************
 *
 *  Copyright (c) Microsoft Corporation.  All rights reserved.
 *
 *  File    : d3dx9effect.h
 *  Content : D3DX effect types and Shaders
 *
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 * D3DXFX_DONOTSAVESTATE
 *   This flag is used as a parameter to ID3DXEffect::Begin().  When this flag
 *   is specified, device state is not saved and restored in Begin/End.
 * D3DXFX_DONOTSAVESHADERSTATE
 *   This flag is used as a parameter to ID3DXEffect::Begin().  When this flag
 *   is specified, shader device state is not saved and restored in Begin/End.
 *   This includes pixel/vertex shaders and shader constants
 *******************************************************************************
(*)
const
  D3DXFX_DONOTSAVESTATE       = (1 shl 0);
  D3DXFX_DONOTSAVESHADERSTATE = (1 shl 1);

(*)
 *******************************************************************************
 * D3DX_PARAMETER_SHARED
 *   Indicates that the value of a parameter will be shared with all effects
 *   which share the same namespace.  Changing the value in one effect will
 *   change it in all.
 *
 * D3DX_PARAMETER_LITERAL
 *   Indicates that the value of this parameter can be treated as literal.
 *   Literal parameters can be marked when the effect is compiled, and their
 *   cannot be changed after the effect is compiled.  Shared parameters cannot
 *   be literal.
 *******************************************************************************
(*)

  D3DX_PARAMETER_SHARED     = (1 shl 0);
  D3DX_PARAMETER_LITERAL    = (1 shl 1);
  D3DX_PARAMETER_ANNOTATION = (1 shl 2);

(*)
 *******************************************************************************
 * D3DXEFFECT_DESC:
 *******************************************************************************
(*)

type
  TD3DXEffectDesc = packed record
    Creator    : PAnsiChar;  // Creator string
    Parameters : Cardinal;   // Number of parameters
    Techniques : Cardinal;   // Number of techniques
    Functions  : Cardinal;   // Number of function entrypoints
  end;
  TD3DXEffect_Desc = TD3DXEffectDesc;


(*)
 *******************************************************************************
 * D3DXPARAMETER_DESC:
 *******************************************************************************
(*)

type
  TD3DXParameterDesc = packed record
    Name          : PAnsiChar;            // Parameter name
    Semantic      : PAnsiChar;            // Parameter semantic
    _Class        : TD3DXParameterClass;  // Class
    _Type         : TD3DXParameterType;   // Component type
    Rows          : Cardinal;             // Number of rows
    Columns       : Cardinal;             // Number of columns
    Elements      : Cardinal;             // Number of array elements
    Annotations   : Cardinal;             // Number of annotations
    StructMembers : Cardinal;             // Number of structure member sub-parameters
    Flags         : LongWord;             // D3DX_PARAMETER_* flags
    Bytes         : Cardinal;             // Parameter size, in bytes
  end;
  TD3DXParameter_Desc = TD3DXParameterDesc;

(*)
 *******************************************************************************
 * D3DXTECHNIQUE_DESC:
 *******************************************************************************
(*)

type
  TD3DXTechniqueDesc = packed record
    Name        : PAnsiChar;  // Technique name
    Passes      : Cardinal;   // Number of passes
    Annotations : Cardinal;   // Number of annotations
  end;
  TD3DXTechnique_Desc = TD3DXTechniqueDesc;

(*)
 *******************************************************************************
 * D3DXPASS_DESC:
 *******************************************************************************
(*)

type
  TD3DXPassDesc = packed record
    Name                 : PAnsiChar;  // Pass name
    Annotations          : Cardinal;   // Number of annotations
    VertexShaderFunction : PLongWord;  // Vertex shader function
    PixelShaderFunction  : PLongWord;  // Pixel shader function
  end;
  TD3DXPass_Desc = TD3DXPassDesc;

(*)
 *******************************************************************************
 * D3DXFUNCTION_DESC:
 *******************************************************************************
(*)

type
  TD3DXFunctionDesc = packed record
    Name        : PAnsiChar;  // Function name
    Annotations : Cardinal;   // Number of annotations
  end;
  TD3DXFunction_Desc = TD3DXFunctionDesc;

(*)
 *******************************************************************************
 * ID3DXEffectPool
 *******************************************************************************
(*)

const
  IID_ID3DXEffectPool : TGUID = '{53CA7768-C0D0-4664-8E79-D156E4F5B7E0}';

type
  PID3DXEffectPool = ^ID3DXEffectPool;
  ID3DXEffectPool = interface (IUnknown)
    ['{53CA7768-C0D0-4664-8E79-D156E4F5B7E0}']
    (*** ID3DXEffectPool methods ***)
    // No public methods
  end;

(*)
 *******************************************************************************
 * ID3DXBaseEffect
 *******************************************************************************
(*)

const
  IID_ID3DXBaseEffect : TGUID = '{804EF574-CCC1-4bf6-B06A-B1404ABDEADE}';

type
  PID3DXBaseEffect = ^ID3DXBaseEffect;
  ID3DXBaseEffect = interface (IUnknown)
    ['{804EF574-CCC1-4bf6-B06A-B1404ABDEADE}']
    (*** ID3DXBaseEffect methods ***)
    // Descs
    function GetDesc(out Desc : TD3DXEffectDesc) : HResult; stdcall;
    function GetParameterDesc(Parameter : TD3DXHandle; out Desc : TD3DXEffectDesc) : HResult; stdcall;
    function GetTechniqueDesc(Technique : TD3DXHandle; out Desc : TD3DXEffectDesc) : HResult; stdcall;
    function GetPassDesc(Pass : TD3DXHandle; out Desc : TD3DXEffectDesc) : HResult; stdcall;
    function GetFunctionDesc(Shader : TD3DXHandle; out Desc : TD3DXEffectDesc) : HResult; stdcall;

    // Handle operations

    function GetParameter(Parameter : TD3DXHandle; Index : Cardinal) : TD3DXHandle; stdcall;
    function GetParameterByName(Parameter : TD3DXHandle; Name : PAnsiChar) : TD3DXHandle; stdcall;
    function GetParameterBySemantic(Parameter : TD3DXHandle; Semantic : PAnsiChar) : TD3DXHandle; stdcall;
    function GetParameterElement(Parameter : TD3DXHandle; Index : Cardinal) : TD3DXHandle; stdcall;
    function GetTechnique(Index : Cardinal) : TD3DXHandle; stdcall;
    function GetTechniqueByName(Name : PAnsiChar) : TD3DXHandle; stdcall;
    function GetPass(Technique : TD3DXHandle; Index : Cardinal) : TD3DXHandle; stdcall;
    function GetPassByName(Technique : TD3DXHandle; Name : PAnsiChar) : TD3DXHandle; stdcall;
    function GetFunction(Index : Cardinal) : TD3DXHandle; stdcall;
    function GetFunctionByName(Name : PAnsiChar) : TD3DXHandle; stdcall;
    function GetAnnotation(_Object : TD3DXHandle; Index : Cardinal) : TD3DXHandle; stdcall;
    function GetAnnotationByName(_Object : TD3DXHandle; Name : PAnsiChar) : TD3DXHandle; stdcall;

    // Get/Set Parameters
    function SetValue(Parameter : TD3DXHandle; Data : Pointer; Bytes : Cardinal) : HResult; stdcall;
    function GetValue(Parameter : TD3DXHandle; Data : Pointer; Bytes : Cardinal) : HResult; stdcall;
    function SetBool(Parameter : TD3DXHandle; b : Bool) : HResult; stdcall;
    function GetBool(Parameter : TD3DXHandle; out b : Bool) : HResult; stdcall;
    function SetBoolArray(Parameter : TD3DXHandle; b : PBool; Count : Cardinal) : HResult; stdcall;
    function GetBoolArray(Parameter : TD3DXHandle; b : PBool; Count : Cardinal) : HResult; stdcall;
    function SetInt(Parameter : TD3DXHandle; n : Integer) : HResult; stdcall;
    function GetInt(Parameter : TD3DXHandle; out n : Integer) : HResult; stdcall;
    function SetIntArray(Parameter : TD3DXHandle; n : PInteger; Count : Cardinal) : HResult; stdcall;
    function GetIntArray(Parameter : TD3DXHandle; n : PInteger; Count : Cardinal) : HResult; stdcall;
    function SetFloat(Parameter : TD3DXHandle; f : Single) : HResult; stdcall;
    function GetFloat(Parameter : TD3DXHandle; f : PSingle) : HResult; stdcall;
    function SetFloatArray(Parameter : TD3DXHandle; f : PSingle; Count : Cardinal) : HResult; stdcall;
    function GetFloatArray(Parameter : TD3DXHandle; f : PSingle; Count : Cardinal) : HResult; stdcall;
    function SetVector(Parameter : TD3DXHandle; const Vector : TD3DXVector4) : HResult; stdcall;
    function GetVector(Parameter : TD3DXHandle; out Vector : TD3DXVector4) : HResult; stdcall;
    function SetVectorArray(Parameter : TD3DXHandle; Vector : PD3DXVector4; Count : Cardinal) : HResult; stdcall;
    function GetVectorArray(Parameter : TD3DXHandle; Vector : PD3DXVector4; Count : Cardinal) : HResult; stdcall;
    function SetMatrix(Parameter : TD3DXHandle; const Matrix : TD3DXMatrix) : HResult; stdcall;
    function GetMatrix(Parameter : TD3DXHandle; out Matrix : TD3DXMatrix) : HResult; stdcall;
    function SetMatrixArray(Parameter : TD3DXHandle; Matrix : PD3DXMatrix; Count : Cardinal) : HResult; stdcall;
    function GetMatrixArray(Parameter : TD3DXHandle; Matrix : PD3DXMatrix; Count : Cardinal) : HResult; stdcall;
    function SetMatrixPointerArray(Parameter : TD3DXHandle; Matrix : PPD3DXMatrix; Count : Cardinal) : HResult; stdcall;
    function GetMatrixPointerArray(Parameter : TD3DXHandle; Matrix : PPD3DXMatrix; Count : Cardinal) : HResult; stdcall;
    function SetMatrixTranspose(Parameter : TD3DXHandle; const Matrix : TD3DXMatrix) : HResult; stdcall;
    function GetMatrixTranspose(Parameter : TD3DXHandle; out Matrix : TD3DXMatrix) : HResult; stdcall;
    function SetMatrixTransposeArray(Parameter : TD3DXHandle; Matrix : PD3DXMatrix; Count : Cardinal) : HResult; stdcall;
    function GetMatrixTransposeArray(Parameter : TD3DXHandle; Matrix : PD3DXMatrix; Count : Cardinal) : HResult; stdcall;
    function SetMatrixTransposePointerArray(Parameter : TD3DXHandle; Matrix : PPD3DXMatrix; Count : Cardinal) : HResult; stdcall;
    function GetMatrixTransposePointerArray(Parameter : TD3DXHandle; Matrix : PPD3DXMatrix; Count : Cardinal) : HResult; stdcall;
    function SetString(Parameter : TD3DXHANDLE; _String : PAnsiChar) : HResult; stdcall;
    function GetString(Parameter : TD3DXHandle; out _String : PAnsiChar) : HResult; stdcall;
    function SetTexture(Parameter : TD3DXHandle; Texture : IDirect3DBaseTexture9) : HResult; stdcall;
    function GetTexture(Parameter : TD3DXHandle; out Texture : IDirect3DBaseTexture9) : HResult; stdcall;
    function SetPixelShader(Parameter : TD3DXHandle; PShader : IDirect3DPixelShader9) : HResult; stdcall;
    function GetPixelShader(Parameter : TD3DXHandle; out PShader : IDirect3DPixelShader9) : HResult; stdcall;
    function SetVertexShader(Parameter : TD3DXHandle; VShader : IDirect3DVertexShader9) : HResult; stdcall;
    function GetVertexShader(Parameter : TD3DXHandle; out VShader : IDirect3DVertexShader9) : HResult; stdcall;
  end;

(*)
 *******************************************************************************
 * ID3DXEffectStateManager:
 * ------------------------
 * This is a user implemented interface that can be used to manage device
 * state changes made by an Effect.
 *******************************************************************************
(*)

const
  IID_ID3DXEffectStateManager : TGUID = '{5C3A7C39-D378-4801-9F8E-71E861251329}';

type
  ID3DXEffectStateManager = interface (IUnknown)
    ['{5C3A7C39-D378-4801-9F8E-71E861251329}']
    (*** ID3DXEffectStateManager methods ***)

    // The following methods are called by the Effect when it wants to make
    // the corresponding device call.  Note that:
    // 1. Users manage the state and are therefore responsible for making the
    //    the corresponding device calls themselves inside their callbacks.
    // 2. Effects pay attention to the return values of the callbacks, and so
    //    users must pay attention to what they return in their callbacks.

    function SetTransform(State : TD3DTransformStateType; const Matrix : TD3DMatrix) : HResult; stdcall;
    function SetMaterial(const Material : TD3DMaterial9) : HResult; stdcall;
    function SetLight(Index : LongWord; const Light : TD3DLight9) : HResult; stdcall;
    function LightEnable(Index : LongWord; Enable : Bool) : HResult; stdcall;
    function SetRenderState(State : TD3DRenderStateType; Value : LongWord) : HResult; stdcall;
    function SetTexture(Stage : LongWord; Texture : IDirect3DBaseTexture9) : HResult; stdcall;
    function SetTextureStageState(Stage : LongWord; _Type : TD3DTextureStageStateType; Value : LongWord) : HResult; stdcall;
    function SetSamplerState(Sampler : LongWord; _Type : TD3DSamplerStateType; Value : LongWord) : HResult; stdcall;
    function SetNPatchMode(NumSegments : Single) : HResult; stdcall;
    function SetFVF(FVF : LongWord) : HResult; stdcall;
    function SetVertexShader(Shader : IDirect3DVertexShader9) : HResult; stdcall;
    function SetVertexShaderConstantF(RegisterIndex : Cardinal; ConstantData : PSingle; RegisterCount : Cardinal) : HResult; stdcall;
    function SetVertexShaderConstantI(RegisterIndex : Cardinal; ConstantData : PInteger; RegisterCount : Cardinal) : HResult; stdcall;
    function SetVertexShaderConstantB(RegisterIndex : Cardinal; ConstantData : PBool; RegisterCount : Cardinal) : HResult; stdcall;
    function SetPixelShader(Shader : IDirect3DPixelShader9) : HResult; stdcall;
    function SetPixelShaderConstantF(RegisterIndex : Cardinal; ConstantData : PSingle; RegisterCount : Cardinal) : HResult; stdcall;
    function SetPixelShaderConstantI(RegisterIndex : Cardinal; ConstantData : PInteger; RegisterCount : Cardinal) : HResult; stdcall;
    function SetPixelShaderConstantB(RegisterIndex : Cardinal; ConstantData : PBool; RegisterCount : Cardinal) : HResult; stdcall;
  end;

(*)
 *******************************************************************************
 * ID3DXEffect
 *******************************************************************************
(*)

const
  IID_ID3DXEffect : TGUID = '{B589B04A-293D-4516-AF0B-3D7DBCF5AC54}';

type
  ID3DXEffect = Interface;
  PID3DXEffect = ^ID3DXEffect;
  ID3DXEffect = interface (ID3DXBaseEffect)
    ['{B589B04A-293D-4516-AF0B-3D7DBCF5AC54}']
    (*** ID3DXEffect methods ***)
    // Pool
    function GetPool(out Pool : ID3DXEffectPool) : HResult; stdcall;

    // Selecting and setting a technique
    function SetTechnique(Technique : TD3DXHandle) : HResult; stdcall;
    function GetCurrentTechnique : TD3DXHandle; stdcall;
    function ValidateTechnique(Technique : TD3DXHandle) : HResult; stdcall;
    function FindNextValidTechnique(Technique : TD3DXHandle; NextTechnique : PD3DXHandle) : HResult; stdcall;
    function IsParameterUsed(Parameter, Technique : TD3DXHandle) : Bool; stdcall;

    // Using current technique
    function _Begin(out Passes : Cardinal; Flags : LongWord) : HResult; stdcall;
    function Pass(Pass : Cardinal) : HResult; stdcall;
    function _End : HResult; stdcall;

    // Managing D3D Device
    function GetDevice(out Device : IDirect3DDevice9) : HResult; stdcall;
    function OnLostDevice : HResult; stdcall;
    function OnResetDevice : HResult; stdcall;

    // Logging device calls
    function SetStateManager(Manager : ID3DXEffectStateManager) : HResult; stdcall;
    function GetStateManager(out Manager : ID3DXEffectStateManager) : HResult; stdcall;

    // Parameter blocks
    function BeginParameterBlock : HResult; stdcall;
    function EndParameterBlock : TD3DXHandle; stdcall;
    function ApplyParameterBlock(ParameterBlock : TD3DXHandle) : HResult; stdcall;

    // Cloning
    function CloneEffect(Device : IDirect3DDevice9; out Effect : ID3DXEffect) : HResult; stdcall;
  end;

(*)
 *******************************************************************************
 * ID3DXEffectCompiler
 *******************************************************************************
(*)

const
  IID_ID3DXEffectCompiler : TGUID = '{F8EE90D3-FCC6-4f14-8AE8-6374AE968E33}';

type
  PID3DXEffectCompiler = ^ID3DXEffectCompiler;
  ID3DXEffectCompiler = interface (ID3DXBaseEffect)
    ['{F8EE90D3-FCC6-4f14-8AE8-6374AE968E33}']
    (*** ID3DXEffect methods ***)
    // Parameter sharing, specialization, and information
    function SetLiteral(Parameter : TD3DXHandle; Literal : Bool) : HResult; stdcall;
    function GetLiteral(Parameter : TD3DXHandle; out Literal : Bool) : HResult; stdcall;

    // Compilation
    function CompileEffect(Flags : LongWord; out Effect, ErrorMsgs : ID3DXBuffer) : HResult; stdcall;

    function CompileShader(_Function : TD3DXHandle; Target : PAnsiChar; Flags : LongWord; out Shader, ErrorMsgs : ID3DXBuffer; out ConstantTable : ID3DXConstantTable) : HResult; stdcall;
  end;


(*)
 *******************************************************************************
 * APIs
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 * D3DXCreateEffectPool:
 * ---------------------
 * Creates an effect pool.  Pools are used for sharing parameters between
 * multiple effects.  For all effects within a pool, shared parameters of the
 * same name all share the same value.
 *
 * Parameters:
 *  ppPool
 *      Returns the created pool.
 *******************************************************************************
(*)

function D3DXCreateEffectPool(out Pool : ID3DXEffectPool) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectPool(Pool : PID3DXEffectPool) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXCreateEffect:
 * -----------------
 * Creates an effect from an ascii or binary effect description.
 *
 * Parameters:
 *  pDevice
 *      Pointer of the device on which to create the effect
 *  pSrcFile
 *      Name of the file containing the effect description
 *  hSrcModule
 *      Module handle. if NULL, current module will be used.
 *  pSrcResource
 *      Resource name in module
 *  pSrcData
 *      Pointer to effect description
 *  SrcDataSize
 *      Size of the effect description in bytes
 *  pDefines
 *      Optional NULL-terminated array of preprocessor macro definitions.
 *  pInclude
 *      Optional interface pointer to use for handling #include directives.
 *      If this parameter is NULL, #includes will be honored when compiling
 *      from file, and will error when compiling from resource or memory.
 *  pPool
 *      Pointer to ID3DXEffectPool object to use for shared parameters.
 *      If NULL, no parameters will be shared.
 *  ppEffect
 *      Returns a buffer containing created effect.
 *  ppCompilationErrors
 *      Returns a buffer containing any error messages which occurred during
 *      compile.  Or NULL if you do not care about the error messages.
 *******************************************************************************
(*)

function D3DXCreateEffectFromFileA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromFileA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromFileA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromFileA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromFileA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromFileA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromFileA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromFileA(Device : IDirect3DDevice9; SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateEffectFromFileW(Device : IDirect3DDevice9; SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromFileW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromFileW(Device : IDirect3DDevice9; SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromFileW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromFileW(Device : IDirect3DDevice9; SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromFileW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromFileW(Device : IDirect3DDevice9; SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromFileW(Device : IDirect3DDevice9; SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateEffectFromFile(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectFromFileW';{$ELSE}'D3DXCreateEffectFromFileA';{$ENDIF}
function D3DXCreateEffectFromFile(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectFromFileW';{$ELSE}'D3DXCreateEffectFromFileA';{$ENDIF}
function D3DXCreateEffectFromFile(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectFromFileW';{$ELSE}'D3DXCreateEffectFromFileA';{$ENDIF}
function D3DXCreateEffectFromFile(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectFromFileW';{$ELSE}'D3DXCreateEffectFromFileA';{$ENDIF}
function D3DXCreateEffectFromFile(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectFromFileW';{$ELSE}'D3DXCreateEffectFromFileA';{$ENDIF}
function D3DXCreateEffectFromFile(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectFromFileW';{$ELSE}'D3DXCreateEffectFromFileA';{$ENDIF}
function D3DXCreateEffectFromFile(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectFromFileW';{$ELSE}'D3DXCreateEffectFromFileA';{$ENDIF}
function D3DXCreateEffectFromFile(Device : IDirect3DDevice9; SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectFromFileW';{$ELSE}'D3DXCreateEffectFromFileA';{$ENDIF}

function D3DXCreateEffectFromResourceA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromResourceA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromResourceA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromResourceA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromResourceA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromResourceA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromResourceA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromResourceA(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateEffectFromResourceW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromResourceW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromResourceW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromResourceW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromResourceW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromResourceW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromResourceW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectFromResourceW(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateEffectFromResource(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectFromResourceW';{$ELSE}'D3DXCreateEffectFromResourceA';{$ENDIF}
function D3DXCreateEffectFromResource(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectFromResourceW';{$ELSE}'D3DXCreateEffectFromResourceA';{$ENDIF}
function D3DXCreateEffectFromResource(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectFromResourceW';{$ELSE}'D3DXCreateEffectFromResourceA';{$ENDIF}
function D3DXCreateEffectFromResource(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectFromResourceW';{$ELSE}'D3DXCreateEffectFromResourceA';{$ENDIF}
function D3DXCreateEffectFromResource(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectFromResourceW';{$ELSE}'D3DXCreateEffectFromResourceA';{$ENDIF}
function D3DXCreateEffectFromResource(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectFromResourceW';{$ELSE}'D3DXCreateEffectFromResourceA';{$ENDIF}
function D3DXCreateEffectFromResource(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectFromResourceW';{$ELSE}'D3DXCreateEffectFromResourceA';{$ENDIF}
function D3DXCreateEffectFromResource(Device : IDirect3DDevice9; SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectFromResourceW';{$ELSE}'D3DXCreateEffectFromResourceA';{$ENDIF}

function D3DXCreateEffect(Device : IDirect3DDevice9; SrcData : Pointer; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffect(Device : IDirect3DDevice9; SrcData : Pointer; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffect(Device : IDirect3DDevice9; SrcData : Pointer; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffect(Device : IDirect3DDevice9; SrcData : Pointer; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffect(Device : IDirect3DDevice9; SrcData : Pointer; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffect(Device : IDirect3DDevice9; SrcData : Pointer; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; Effect : PID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffect(Device : IDirect3DDevice9; SrcData : Pointer; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffect(Device : IDirect3DDevice9; SrcData : Pointer; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Pool : ID3DXEffectPool; out Effect : ID3DXEffect; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXCreateEffectCompiler:
 * -------------------------
 * Creates an effect from an ascii or binary effect description.
 *
 * Parameters:
 *  pSrcFile
 *      Name of the file containing the effect description
 *  hSrcModule
 *      Module handle. if NULL, current module will be used.
 *  pSrcResource
 *      Resource name in module
 *  pSrcData
 *      Pointer to effect description
 *  SrcDataSize
 *      Size of the effect description in bytes
 *  pDefines
 *      Optional NULL-terminated array of preprocessor macro definitions.
 *  pInclude
 *      Optional interface pointer to use for handling #include directives.
 *      If this parameter is NULL, #includes will be honored when compiling
 *      from file, and will error when compiling from resource or memory.
 *  pPool
 *      Pointer to ID3DXEffectPool object to use for shared parameters.
 *      If NULL, no parameters will be shared.
 *  ppCompiler
 *      Returns a buffer containing created effect compiler.
 *  ppParseErrors
 *      Returns a buffer containing any error messages which occurred during
 *      parse.  Or NULL if you do not care about the error messages.
 *******************************************************************************
(*)

function D3DXCreateEffectCompilerFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromFileA(SrcFile : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromFileA(SrcFile : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateEffectCompilerFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromFileW(SrcFile : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromFileW(SrcFile : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateEffectCompilerFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectCompilerFromFileW';{$ELSE}'D3DXCreateEffectCompilerFromFileA';{$ENDIF}
function D3DXCreateEffectCompilerFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectCompilerFromFileW';{$ELSE}'D3DXCreateEffectCompilerFromFileA';{$ENDIF}
function D3DXCreateEffectCompilerFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectCompilerFromFileW';{$ELSE}'D3DXCreateEffectCompilerFromFileA';{$ENDIF}
function D3DXCreateEffectCompilerFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectCompilerFromFileW';{$ELSE}'D3DXCreateEffectCompilerFromFileA';{$ENDIF}
function D3DXCreateEffectCompilerFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectCompilerFromFileW';{$ELSE}'D3DXCreateEffectCompilerFromFileA';{$ENDIF}
function D3DXCreateEffectCompilerFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectCompilerFromFileW';{$ELSE}'D3DXCreateEffectCompilerFromFileA';{$ENDIF}
function D3DXCreateEffectCompilerFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectCompilerFromFileW';{$ELSE}'D3DXCreateEffectCompilerFromFileA';{$ENDIF}
function D3DXCreateEffectCompilerFromFile(SrcFile : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectCompilerFromFileW';{$ELSE}'D3DXCreateEffectCompilerFromFileA';{$ENDIF}

function D3DXCreateEffectCompilerFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromResourceA(SrcModule : HMODULE; SrcResource : PAnsiChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateEffectCompilerFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompilerFromResourceW(SrcModule : HMODULE; SrcResource : PWideChar; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateEffectCompilerFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectCompilerFromResourceW';{$ELSE}'D3DXCreateEffectCompilerFromResourceA';{$ENDIF}
function D3DXCreateEffectCompilerFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectCompilerFromResourceW';{$ELSE}'D3DXCreateEffectCompilerFromResourceA';{$ENDIF}
function D3DXCreateEffectCompilerFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectCompilerFromResourceW';{$ELSE}'D3DXCreateEffectCompilerFromResourceA';{$ENDIF}
function D3DXCreateEffectCompilerFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectCompilerFromResourceW';{$ELSE}'D3DXCreateEffectCompilerFromResourceA';{$ENDIF}
function D3DXCreateEffectCompilerFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectCompilerFromResourceW';{$ELSE}'D3DXCreateEffectCompilerFromResourceA';{$ENDIF}
function D3DXCreateEffectCompilerFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectCompilerFromResourceW';{$ELSE}'D3DXCreateEffectCompilerFromResourceA';{$ENDIF}
function D3DXCreateEffectCompilerFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectCompilerFromResourceW';{$ELSE}'D3DXCreateEffectCompilerFromResourceA';{$ENDIF}
function D3DXCreateEffectCompilerFromResource(SrcModule : HMODULE; SrcResource : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateEffectCompilerFromResourceW';{$ELSE}'D3DXCreateEffectCompilerFromResourceA';{$ENDIF}

function D3DXCreateEffectCompiler(SrcData : Pointer; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompiler(SrcData : Pointer; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompiler(SrcData : Pointer; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompiler(SrcData : Pointer; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; CompilationErrors : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompiler(SrcData : Pointer; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompiler(SrcData : Pointer; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; Compiler : PID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompiler(SrcData : Pointer; SrcDataLen : Cardinal; Defines : PD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateEffectCompiler(SrcData : Pointer; SrcDataLen : Cardinal; const Defines : TD3DXMacro; Include : ID3DXInclude; Flags : LongWord; out Compiler : ID3DXEffectCompiler; out CompilationErrors : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File    : d3dx9shapes.h
 *  Content : D3DX simple shapes
 *
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 * Functions:
 *******************************************************************************
(*)

(*)
 *******************************************************************************
 * D3DXCreatePolygon:
 * ------------------
 * Creates a mesh containing an n-sided polygon.  The polygon is centered
 * at the origin.
 *
 * Parameters:
 *
 *  pDevice     The D3D device with which the mesh is going to be used.
 *  Length      Length of each side.
 *  Sides       Number of sides the polygon has.  (Must be >= 3)
 *  ppMesh      The mesh object which will be created
 *  ppAdjacency Returns a buffer containing adjacency info.  Can be NULL.
 *******************************************************************************
(*)

function D3DXCreatePolygon(Device : IDirect3DDevice9; Length : Single; Sides : Cardinal; Mesh : PID3DXMesh; Adjacency : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreatePolygon(Device : IDirect3DDevice9; Length : Single; Sides : Cardinal; out Mesh : ID3DXMesh; Adjacency : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreatePolygon(Device : IDirect3DDevice9; Length : Single; Sides : Cardinal; Mesh : PID3DXMesh; out Adjacency : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreatePolygon(Device : IDirect3DDevice9; Length : Single; Sides : Cardinal; out Mesh : ID3DXMesh; out Adjacency : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
        

(*)
 *******************************************************************************
 * D3DXCreateBox:
 * --------------
 * Creates a mesh containing an axis-aligned box.  The box is centered at
 * the origin.
 *
 * Parameters:
 *
 *  pDevice     The D3D device with which the mesh is going to be used.
 *  Width       Width of box (along X-axis)
 *  Height      Height of box (along Y-axis)
 *  Depth       Depth of box (along Z-axis)
 *  ppMesh      The mesh object which will be created
 *  ppAdjacency Returns a buffer containing adjacency info.  Can be NULL.
 *******************************************************************************
(*)

function D3DXCreateBox(Device : IDirect3DDevice9; Width, Height, Depth : Single; Mesh : PID3DXMesh; Adjacency : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateBox(Device : IDirect3DDevice9; Width, Height, Depth : Single; out Mesh : ID3DXMesh; Adjacency : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateBox(Device : IDirect3DDevice9; Width, Height, Depth : Single; Mesh : PID3DXMesh; out Adjacency : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateBox(Device : IDirect3DDevice9; Width, Height, Depth : Single; out Mesh : ID3DXMesh; out Adjacency : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXCreateCylinder:
 * -------------------
 * Creates a mesh containing a cylinder.  The generated cylinder is
 * centered at the origin, and its axis is aligned with the Z-axis.
 *
 * Parameters:
 *
 *  pDevice     The D3D device with which the mesh is going to be used.
 *  Radius1     Radius at -Z end (should be >= 0.0f)
 *  Radius2     Radius at +Z end (should be >= 0.0f)
 *  Length      Length of cylinder (along Z-axis)
 *  Slices      Number of slices about the main axis
 *  Stacks      Number of stacks along the main axis
 *  ppMesh      The mesh object which will be created
 *  ppAdjacency Returns a buffer containing adjacency info.  Can be NULL.
 *******************************************************************************
(*)

function D3DXCreateCylinder(Device : IDirect3DDevice9; Radius1, Radius2, Length : Single; Slices, Stacks : Cardinal; Mesh : PID3DXMesh; Adjacency : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCylinder(Device : IDirect3DDevice9; Radius1, Radius2, Length : Single; Slices, Stacks : Cardinal; out Mesh : ID3DXMesh; Adjacency : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCylinder(Device : IDirect3DDevice9; Radius1, Radius2, Length : Single; Slices, Stacks : Cardinal; Mesh : PID3DXMesh; out Adjacency : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCylinder(Device : IDirect3DDevice9; Radius1, Radius2, Length : Single; Slices, Stacks : Cardinal; out Mesh : ID3DXMesh; out Adjacency : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;


(*)
 *******************************************************************************
 * D3DXCreateSphere:
 * -----------------
 * Creates a mesh containing a sphere.  The sphere is centered at the
 * origin.
 *
 * Parameters:
 *
 *  pDevice     The D3D device with which the mesh is going to be used.
 *  Radius      Radius of the sphere (should be >= 0.0f)
 *  Slices      Number of slices about the main axis
 *  Stacks      Number of stacks along the main axis
 *  ppMesh      The mesh object which will be created
 *  ppAdjacency Returns a buffer containing adjacency info.  Can be NULL.
 *******************************************************************************
(*)

function D3DXCreateSphere(Device : IDirect3DDevice9; Radius : Single; Slices, Stacks : Cardinal; Mesh : PID3DXMesh; Adjacency : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateSphere(Device : IDirect3DDevice9; Radius : Single; Slices, Stacks : Cardinal; out Mesh : ID3DXMesh; Adjacency : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateSphere(Device : IDirect3DDevice9; Radius : Single; Slices, Stacks : Cardinal; Mesh : PID3DXMesh; out Adjacency : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateSphere(Device : IDirect3DDevice9; Radius : Single; Slices, Stacks : Cardinal; out Mesh : ID3DXMesh; out Adjacency : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;


(*)
 *******************************************************************************
 * D3DXCreateTorus:
 * ----------------
 * Creates a mesh containing a torus.  The generated torus is centered at
 * the origin, and its axis is aligned with the Z-axis.
 *
 * Parameters:
 *
 *  pDevice     The D3D device with which the mesh is going to be used.
 *  InnerRadius Inner radius of the torus (should be >= 0.0f)
 *  OuterRadius Outer radius of the torue (should be >= 0.0f)
 *  Sides       Number of sides in a cross-section (must be >= 3)
 *  Rings       Number of rings making up the torus (must be >= 3)
 *  ppMesh      The mesh object which will be created
 *  ppAdjacency Returns a buffer containing adjacency info.  Can be NULL.
 *******************************************************************************
(*)

function D3DXCreateTorus(Device : IDirect3DDevice9; InnerRadius, OuterRadius : Single; Sides, Rings : Cardinal; Mesh : PID3DXMesh; Adjacency : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTorus(Device : IDirect3DDevice9; InnerRadius, OuterRadius : Single; Sides, Rings : Cardinal; out Mesh : ID3DXMesh; Adjacency : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTorus(Device : IDirect3DDevice9; InnerRadius, OuterRadius : Single; Sides, Rings : Cardinal; Mesh : PID3DXMesh; out Adjacency : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTorus(Device : IDirect3DDevice9; InnerRadius, OuterRadius : Single; Sides, Rings : Cardinal; out Mesh : ID3DXMesh; out Adjacency : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXCreateTeapot:
 * -----------------
 * Creates a mesh containing a teapot.
 *
 * Parameters:
 *
 *  pDevice     The D3D device with which the mesh is going to be used.
 *  ppMesh      The mesh object which will be created
 *  ppAdjacency Returns a buffer containing adjacency info.  Can be NULL.
 *******************************************************************************
(*)

function D3DXCreateTeapot(Device : IDirect3DDevice9; Mesh : PID3DXMesh; Adjacency : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTeapot(Device : IDirect3DDevice9; out Mesh : ID3DXMesh; Adjacency : PID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTeapot(Device : IDirect3DDevice9; Mesh : PID3DXMesh; out Adjacency : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTeapot(Device : IDirect3DDevice9; out Mesh : ID3DXMesh; out Adjacency : ID3DXBuffer) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXCreateText:
 * ---------------
 * Creates a mesh containing the specified text using the font associated
 * with the device context.
 *
 * Parameters:
 *
 *  pDevice       The D3D device with which the mesh is going to be used.
 *  hDC           Device context, with desired font selected
 *  pText         Text to generate
 *  Deviation     Maximum chordal deviation from true font outlines
 *  Extrusion     Amount to extrude text in -Z direction
 *  ppMesh        The mesh object which will be created
 *  pGlyphMetrics Address of buffer to receive glyph metric data (or NULL)
 *******************************************************************************
(*)

function D3DXCreateTextA(Device : IDirect3DDevice9; DC : HDC; Text : PAnsiChar; Deviation, Extrusion : Single; Mesh : PID3DXMesh; Adjacency : PID3DXBuffer; GlyphMetrics : PGlyphMetricsFloat) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextA(Device : IDirect3DDevice9; DC : HDC; Text : PAnsiChar; Deviation, Extrusion : Single; out Mesh : ID3DXMesh; Adjacency : PID3DXBuffer; GlyphMetrics : PGlyphMetricsFloat) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextA(Device : IDirect3DDevice9; DC : HDC; Text : PAnsiChar; Deviation, Extrusion : Single; Mesh : PID3DXMesh; out Adjacency : ID3DXBuffer; GlyphMetrics : PGlyphMetricsFloat) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextA(Device : IDirect3DDevice9; DC : HDC; Text : PAnsiChar; Deviation, Extrusion : Single; out Mesh : ID3DXMesh; out Adjacency : ID3DXBuffer; GlyphMetrics : PGlyphMetricsFloat) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateTextW(Device : IDirect3DDevice9; DC : HDC; Text : PWideChar; Deviation, Extrusion : Single; Mesh : PID3DXMesh; Adjacency : PID3DXBuffer; GlyphMetrics : PGlyphMetricsFloat) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextW(Device : IDirect3DDevice9; DC : HDC; Text : PWideChar; Deviation, Extrusion : Single; out Mesh : ID3DXMesh; Adjacency : PID3DXBuffer; GlyphMetrics : PGlyphMetricsFloat) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextW(Device : IDirect3DDevice9; DC : HDC; Text : PWideChar; Deviation, Extrusion : Single; Mesh : PID3DXMesh; out Adjacency : ID3DXBuffer; GlyphMetrics : PGlyphMetricsFloat) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateTextW(Device : IDirect3DDevice9; DC : HDC; Text : PWideChar; Deviation, Extrusion : Single; out Mesh : ID3DXMesh; out Adjacency : ID3DXBuffer; GlyphMetrics : PGlyphMetricsFloat) : HResult; stdcall; overload; external d3dx9dllname;

function D3DXCreateText(Device : IDirect3DDevice9; DC : HDC; Text : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Deviation, Extrusion : Single; Mesh : PID3DXMesh; Adjacency : PID3DXBuffer; GlyphMetrics : PGlyphMetricsFloat) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextW';{$ELSE}'D3DXCreateTextA';{$ENDIF}
function D3DXCreateText(Device : IDirect3DDevice9; DC : HDC; Text : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Deviation, Extrusion : Single; out Mesh : ID3DXMesh; Adjacency : PID3DXBuffer; GlyphMetrics : PGlyphMetricsFloat) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextW';{$ELSE}'D3DXCreateTextA';{$ENDIF}
function D3DXCreateText(Device : IDirect3DDevice9; DC : HDC; Text : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Deviation, Extrusion : Single; Mesh : PID3DXMesh; out Adjacency : ID3DXBuffer; GlyphMetrics : PGlyphMetricsFloat) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextW';{$ELSE}'D3DXCreateTextA';{$ENDIF}
function D3DXCreateText(Device : IDirect3DDevice9; DC : HDC; Text : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; Deviation, Extrusion : Single; out Mesh : ID3DXMesh; out Adjacency : ID3DXBuffer; GlyphMetrics : PGlyphMetricsFloat) : HResult; stdcall; overload; external d3dx9dllname name {$IFDEF UNICODE}'D3DXCreateTextW';{$ELSE}'D3DXCreateTextA';{$ENDIF}

(*)
 *******************************************************************************
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File    : d3dx9anim.h
 *  Content : D3DX mesh types and functions
 *
 *******************************************************************************
(*)

const
  IID_ID3DXAnimationSet           : TGUID = '{A632D591-B584-4d03-BCCD-CBB5331F21AA}';
  IID_ID3DXKeyframedAnimationSet  : TGUID = '{73B6DDE3-7E45-4cc8-834F-B7DCBC04D10A}';
  IID_ID3DXCompressedAnimationSet : TGUID = '{7B7228FD-EA36-4c06-9DA6-E053D688E164}';
  IID_ID3DXAnimationController    : TGUID = '{39F628C0-CD5B-41d6-8E9C-3BBBB66FDA57}';

(*)
 *******************************************************************************
 * D3DXMESHDATATYPE:
 * -----------------
 * This enum defines the type of mesh data present in a MeshData structure
 *******************************************************************************
(*)

type
  TD3DXMeshDataType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DXMESHTYPE_MESH      = $001{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // normal ID3DXMesh data
    D3DXMESHTYPE_PMESH     = $002{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // Progressive Mesh - ID3DXPMesh
    D3DXMESHTYPE_PATCHMESH = $003{$IFNDEF NOENUMS}){$ENDIF};         // Patch MEsh - ID3DXPatchMesh

(*)
 *******************************************************************************
 * D3DXMESHDATA:
 * -------------
 * This struct encapsulates a the mesh data that can be present in a mesh
 * container.  The supported mesh types are pMesh, pPMesh, pPatchMesh.
 * The valid way to access this is determined by the MeshType enum.
 *******************************************************************************
(*)

type
  PD3DXMeshData = ^TD3DXMeshData;
  TD3DXMeshData = packed record
    _Type : TD3DXMeshDataType;

    // current mesh data interface
    case Byte of // You have to typecast the Pointer to the needed Interface.
      0 : (ID3DXMesh : Pointer);
      1 : (ID3DXPMesh : Pointer);
      2 : (ID3DXPatchMesh : Pointer);
   end;

(*)
 *******************************************************************************
 * D3DXMESHCONTAINER:
 * ------------------
 * This struct encapsulates a mesh object in a transformation frame
 * hierarchy. The app can derive from this structure to add other app specific
 * data to this.
 *******************************************************************************
(*)

type
  PD3DXMeshContainer = ^TD3DXMeshContainer;
  TD3DXMeshContainer = packed record
    Name              : PAnsiChar;
    MeshData          : TD3DXMeshData;
    Materials         : PD3DXMaterial;
    Effects           : TD3DXEffectInstance;
    NumMaterials      : LongWord;
    Adjacency         : PLongWord;
    SkinInfo          : ID3DXSkinInfo;
    NextMeshContainer : PD3DXMeshContainer;
  end;

(*)
 *******************************************************************************
 * D3DXFRAME:
 * ----------
 * This struct is the encapsulates a transform frame in a transformation frame
 * hierarchy. The app can derive from this structure to add other app specific
 * data to this
 *******************************************************************************
(*)
type
  PD3DXFrame = ^TD3DXFrame;
  TD3DXFrame = packed record
    Name                 : PAnsiChar;
    TransformationMatrix : TD3DXMatrix;
    MeshContainer        : PD3DXMeshContainer;

    FrameSibling         : PD3DXFrame;
    FrameFirstChild      : PD3DXFrame;
  end;

(*)
 *******************************************************************************
 * ID3DXAllocateHierarchy:
 * -----------------------
 * This interface is implemented by the application to allocate/free frame and
 * mesh container objects. Methods on this are called during loading and
 * destroying frame hierarchies
 *******************************************************************************
(*)

type
  PID3DXAllocateHierarchy = ^ID3DXAllocateHierarchy;
  ID3DXAllocateHierarchy = interface
    (*** ID3DXAllocateHierarchy methods ***)

    (*)
     ***************************************************************************
     * CreateFrame:
     * ------------
     * Requests allocation of a frame object.
     *
     * Parameters:
     *  Name
     *		Name of the frame to be created
     *	ppNewFrame
     *		Returns the created frame object
     ***************************************************************************
    (*)
    function CreateFrame(Name : PAnsiChar; out NewFrame : PD3DXFrame) : HResult; stdcall;

    (*)
     ***************************************************************************
     * CreateMeshContainer:
     * --------------------
     * Requests allocation of a mesh container object.
     *
     * Parameters:
     *  Name
     *		Name of the mesh
     *	pMesh
     *		Pointer to the mesh object if basic polygon data found
     *	pPMesh
     *		Pointer to the progressive mesh object if progressive mesh data found
     *	pPatchMesh
     *		Pointer to the patch mesh object if patch data found
     *	pMaterials
     *		Array of materials used in the mesh
     *	pEffectInstances
     *		Array of effect instances used in the mesh
     *	NumMaterials
     *		Num elements in the pMaterials array
     *	pAdjacency
     *		Adjacency array for the mesh
     *	pSkinInfo
     *		Pointer to the skininfo object if the mesh is skinned
     *	pBoneNames
     *		Array of names, one for each bone in the skinned mesh.
     *		The numberof bones can be found from the pSkinMesh object
     *	pBoneOffsetMatrices
     *		Array of matrices, one for each bone in the skinned mesh.
     ***************************************************************************
    (*)
    function CreateMeshContainer(Name : PAnsiChar; const MeshData : TD3DXMeshData; Materials : PD3DXMaterial; EffectInstances : PD3DXEffectInstance; NumMaterials : LongWord; Adjacency : PLongWord; SkinInfo : ID3DXSkinInfo; out NewMeshContainer : TD3DXMeshContainer) : HResult; stdcall;

    (*)
     ***************************************************************************
     * DestroyFrame:
     * -------------
     * Requests de-allocation of a frame object.
     *
     * Parameters:
     *  pFrameToFree
     *		Pointer to the frame to be de-allocated
     *
     ***************************************************************************
    (*)
    function DestroyFrame(FrameToFree : PD3DXFrame) : HResult; stdcall;

    (*)
     ***************************************************************************
     * DestroyMeshContainer:
     * ---------------------
     * Requests de-allocation of a mesh container object.
     *
     * Parameters:
     *  pMeshContainerToFree
     *		Pointer to the mesh container object to be de-allocated
     *
     ***************************************************************************
    (*)
    function DestroyMeshContainer(const MeshContainerToFree : TD3DXMeshContainer) : HResult; stdcall;
  end;

(*)
 *******************************************************************************
 * ID3DXLoadUserData:
 * ------------------
 * This interface is implemented by the application to load user data in a .X file
 * When user data is found, these callbacks will be used to allow the application
 * to load the data.
 *******************************************************************************
(*)

  PID3DXLoadUserData = ^ID3DXLoadUserData;
  ID3DXLoadUserData = interface
    function LoadTopLevelData(XofChildData : IDirectXFileData) : HResult; stdcall;

    function LoadFrameChildData(Frame : PD3DXFrame; XofChildData : IDirectXFileData) : HResult; stdcall;

    function LoadMeshChildData(const MeshContainer : TD3DXMeshContainer; XofChildData : IDirectXFileData) : HResult; stdcall;
  end;

(*)
 *******************************************************************************
 * ID3DXSaveUserData:
 * ------------------
 * This interface is implemented by the application to save user data in a .X file
 * The callbacks are called for all data saved.  The user can then add any
 * child data objects to the object provided to the callback.
 *******************************************************************************
(*)

  PID3DXSaveUserData = ^ID3DXSaveUserData;
  ID3DXSaveUserData = interface
    function AddFrameChildData(const Frame : TD3DXFrame; XofSave : IDirectXFileSaveObject; XofFrameData : IDirectXFileData) : HResult; stdcall;

    function AddMeshChildData(const MeshContainer : TD3DXMeshContainer; XofSave : IDirectXFileSaveObject; XofFrameData : IDirectXFileData) : HResult; stdcall;

    // NOTE: this is called once per Save.  All top level objects should be added using the
    //    provided interface.  One call adds objects before the frame hierarchy, the other after
    function AddTopLevelDataObjectsPre(XofSave : IDirectXFileSaveObject) : HResult; stdcall;
    function AddTopLevelDataObjectsPost(XofSave : IDirectXFileSaveObject) : HResult; stdcall;

    // callbacks for the user to register and then save templates to the XFile
    function RegisterTemplates(XFileApi : IDirectXFile) : HResult; stdcall;
    function SaveTemplates(XofSave : IDirectXFileSaveObject) : HResult; stdcall;
  end;

(*)
 *******************************************************************************
 * D3DXCALLBACK_SEARCH_FLAGS:
 * --------------------------
 * Flags that can be passed into ID3DXAnimationSet::GetCallback.
 *******************************************************************************
(*)

type
  TD3DXCallbackSearchFlags = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DXCALLBACK_SEARCH_EXCLUDING_INITIAL_POSITION = $01{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // exclude callbacks at the initial position from the search
    D3DXCALLBACK_SEARCH_BEHIND_INITIAL_POSITION    = $02{$IFNDEF NOENUMS}){$ENDIF};         // reverse the callback search direction

type
  TD3DXCallback_Search_Flags = TD3DXCallbackSearchFlags;


(*)
 *******************************************************************************
 * ID3DXAnimationSet:
 * ------------------
 * This interface implements an animation set.
 *******************************************************************************
(*)

  ID3DXAnimationSet = interface(IUnknown)
    ['{54B569AC-0AEF-473e-9704-3FEF317F64AB}']
    (*** ID3DXAnimationSet methods ***)
    // Name
    function GetName : PAnsiChar; stdcall;

    // Period
    function GetPeriod : Double; stdcall;
    function GetPeriodicPosition(Position : Double) : Double; stdcall;  // Maps position into animation period

    // Animation names
    function GetNumAnimations : Cardinal;
    function GetAnimationNameByIndex(Index : Cardinal; out Name : PAnsiChar) : HResult; stdcall;
    function GetAnimationIndexByName(Name : PAnsiChar; out Index : Cardinal) : HResult; stdcall;

    // SRT
    function GetSRT(PeriodicPosition : Double; Animation : Cardinal; out Scale : TD3DXVector3; out Rotate : TD3DXQuaternion; out Translate : TD3DXVector3) : HResult; stdcall;

    // Callbacks
    function GetCallback(Position : Double; Flags : LongWord; out CallbackPosition : Double; out CallbackData : Pointer) : HResult; stdcall;
  end;

(*)
 *******************************************************************************
 * D3DXPLAYBACK_TYPE:
 * ------------------
 * This enum defines the type of animation set loop modes.
 *******************************************************************************
(*)
type
 TD3DXPlaybackType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DXPLAY_LOOP          = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPLAY_ONCE          = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXPLAY_PINGPONG      = 2{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DXPlayback_Type = TD3DXPlaybackType;

(*)
 *******************************************************************************
 * D3DXKEY_VECTOR3:
 * ----------------
 * This structure describes a vector key for use in keyframe animation.
 * It specifies a vector Value at a given Time. This is used for scale and
 * translation keys.
 *******************************************************************************
(*)

type
  PD3DXKeyVector3 = ^TD3DXKeyVector3;
  TD3DXKeyVector3 = packed record
    Time  : Single;
    Value : TD3DXVector3;
  end;
  PD3DXKey_Vector3 = ^TD3DXKey_Vector3; 
  TD3DXKey_Vector3 = TD3DXKeyVector3;

(*)
 *******************************************************************************
 * D3DXKEY_QUATERNION:
 * -------------------
 * This structure describes a quaternion key for use in keyframe animation.
 * It specifies a quaternion Value at a given Time. This is used for rotation
 * keys.
 *******************************************************************************
(*)

type
  PD3DXKeyQuaternion = ^TD3DXKeyQuaternion;
  TD3DXKeyQuaternion = packed record
    Time  : Single;
    Value : TD3DXQuaternion;
  end;
  PD3DXKey_Quaternion = ^TD3DXKey_Quaternion;
  TD3DXKey_Quaternion = TD3DXKeyQuaternion;

(*)
 *******************************************************************************
 * D3DXKEY_CALLBACK:
 * -----------------
 * This structure describes an callback key for use in keyframe animation.
 * It specifies a pointer to user data at a given Time.
 *******************************************************************************
(*)

type
  PD3DXKeyCallback = ^TD3DXKeyCallback;
  TD3DXKeyCallback = packed record
    Time         : Single;
    CallbackData : Pointer;
  end;

  PD3DXKey_Callback = ^TD3DXKey_Callback;
  TD3DXKey_Callback = TD3DXKeyCallback;

(*)
 *******************************************************************************
 * D3DXCOMPRESSION_FLAGS:
 * ----------------------
 * Flags that can be passed into ID3DXKeyframedAnimationSet::Compress.
 *******************************************************************************
(*)
type
  TD3DXCompressionFlags = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DXCOMPRESS_DEFAULT     = $00{$IFNDEF NOENUMS}){$ENDIF};
    
type
  TD3DXCompression_Flags = TD3DXCompressionFlags;


(*)
 *******************************************************************************
 * ID3DXKeyframedAnimationSet:
 * ---------------------------
 * This interface implements a compressable keyframed animation set.
 *******************************************************************************
(*)
  PID3DXKeyframedAnimationSet = ^ID3DXKeyframedAnimationSet;
  ID3DXKeyframedAnimationSet = interface(ID3DXAnimationSet)
    ['{73B6DDE3-7E45-4cc8-834F-B7DCBC04D10A}']
    (*** ID3DXKeyframedAnimationSet methods ***)
    // Playback
    function GetPlaybackType : TD3DXPlaybackType; stdcall;
    function GetSourceTicksPerSecond : Double; stdcall;

    // Scale keys
    function GetNumScaleKeys(Animation : Cardinal) : Cardinal; stdcall;
    function GetScaleKeys(Animation : Cardinal; out ScaleKeys : TD3DXKeyVector3) : HResult; stdcall;

    // Rotation keys
    function GetNumRotationKeys(Animation : Cardinal) : Cardinal;  stdcall;
    function GetRotationKeys(Animation : Cardinal; out RotationKeys : TD3DXKeyQuaternion) : HResult; stdcall;

    // Translation keys
    function GetNumTranslationKeys(Animation : Cardinal) : Cardinal; stdcall;
    function GetTranslationKeys(Animation : Cardinal; out TranslationKeys : TD3DXKeyVector3) : HResult; stdcall;

    // Callback keys
    function GetNumCallbackKeys : Cardinal; stdcall;
    function GetCallbackKeys(out CallbackKeys : TD3DXKeyCallback) : HResult; stdcall;

    // One-time animaton SRT keyframe registration
    function RegisterAnimationSRTKeys(Name : PAnsiChar; NumScaleKeys : Cardinal; NumRotationKeys : Cardinal; NumTranslationKeys : Cardinal; ScaleKeys : PD3DXKeyVector3; RotationKeys : PD3DXKeyQuaternion; TranslationKeys : PD3DXKeyVector3) : HResult; stdcall;

    // Compression
    function Compress(Flags : LongWord; Lossiness : Single; Hierarchy : PD3DXFrame; out CompressedData : ID3DXBuffer) : HResult; stdcall;
  end;

(*)
 *******************************************************************************
 * ID3DXCompressedAnimationSet:
 * ----------------------------
 * This interface implements a compressed keyframed animation set.
 *******************************************************************************
(*)
  PID3DXCompressedAnimationSet = ^ID3DXCompressedAnimationSet;
  ID3DXCompressedAnimationSet = interface(ID3DXAnimationSet)
    ['{7B7228FD-EA36-4c06-9DA6-E053D688E164}']
    (*** ID3DXKeyframedAnimationSet methods ***)

    // Playback
    function GetPlaybackType : TD3DXPlaybackType; stdcall;
    function GetSourceTicksPerSecond : Double; stdcall;

    // Scale keys
    function GetCompressedData(out CompressedData : ID3DXBuffer) : HResult; stdcall;

    // Callback keys
    function GetNumCallbackKeys : Cardinal; stdcall;
    function GetCallbackKeys(out CallbackKeys : TD3DXKeyCallback) : HResult; stdcall;
  end;

(*)
 *******************************************************************************
 * D3DXPRIORITY_TYPE:
 * ------------------
 * This enum defines the type of priority group that a track can be assigned to.
 *******************************************************************************
(*)
type
  TD3DXPriorityType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DXPRIORITY_LOW         = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF} // This track should be blended with all low priority tracks before mixed with the high priority result
    D3DXPRIORITY_HIGH        = 1{$IFNDEF NOENUMS}){$ENDIF};        // This track should be blended with all high priority tracks before mixed with the low priority result

type
  TD3DXPriority_Type = TD3DXPriorityType;

(*)
 *******************************************************************************
 * D3DXTRACK_DESC:
 * ---------------
 * This structure describes the mixing information of an animation track.
 * The mixing information consists of the current position, speed, and blending
 * weight for the track.  The Flags field also specifies whether the track is
 * low or high priority.  Tracks with the same priority are blended together
 * and then the two resulting values are blended using the priority blend factor.
 * A track also has an animation set (stored separately) associated with it.
 *******************************************************************************
(*)
type
  PD3DXTrackDesc = ^TD3DXTrackDesc;
  TD3DXTrackDesc = packed record
    Priority : TD3DXPriorityType;
    Weight   : Single;
    Speed    : Single;
    Position : Double;
    Enable   : Bool;
  end;

  PD3DXTrack_Desc = ^TD3DXTrack_Desc;
  TD3DXTrack_Desc = TD3DXTrackDesc;

(*)
 *******************************************************************************
 * D3DXEVENT_TYPE:
 * ---------------
 * This enum defines the type of events keyable via the animation controller.
 *******************************************************************************
(*)
type
  TD3DXEventType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DXEVENT_TRACKSPEED    = 0{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXEVENT_TRACKWEIGHT   = 1{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXEVENT_TRACKPOSITION = 2{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXEVENT_TRACKENABLE   = 3{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}
    D3DXEVENT_PRIORITYBLEND = 4{$IFNDEF NOENUMS}){$ENDIF};

type
  TD3DXEvent_Type = TD3DXEventType;

(*)
 *******************************************************************************
 * D3DXTRANSITION_TYPE:
 * --------------------
 * This enum defines the type of transtion performed on a event that
 * transitions from one value to another.
 *******************************************************************************
(*)

type
  TD3DXTransitionType = {$IFNDEF NOENUMS}({$ELSE}LongWord;{$ENDIF}
{$IFDEF NOENUMS}const{$ENDIF}
    D3DXTRANSITION_LINEAR        = $000{$IFNDEF NOENUMS},{$ELSE};{$ENDIF}  // Linear transition from one value to the next
    D3DXTRANSITION_EASEINEASEOUT = $001{$IFNDEF NOENUMS}){$ENDIF};         // Ease-In Ease-Out spline transtion from one value to the next

type
  TD3DXTransition_Type = TD3DXTransitionType;    

(*)
 *******************************************************************************
 * D3DXEVENT_DESC:
 * ---------------
 * This structure describes a animation controller event.
 * It gives the event's type, track (if the event is a track event), global
 * start time, duration, transition method, and target value.
 *******************************************************************************
(*)
type
  PD3DXEventDesc = ^TD3DXEventDesc;
  TD3DXEventDesc = packed record
    _Type      : TD3DXEventType;
    Track      : Cardinal;
    StartTime  : Double;
    Duration   : Double;
    Transition : TD3DXTransitionType;
    case Byte of
      0 : (Weight : Single;);
      1 : (Speed  : Single;);
      2 : (Position : Double;);
      3 : (Enable : Bool;);
  end;

  PD3DXEvent_Desc = ^TD3DXEvent_Desc;
  TD3DXEvent_Desc = TD3DXEventDesc;

(*)
 *******************************************************************************
 * D3DXEVENTHANDLE:
 * ----------------
 * Handle values used to efficiently reference animation controller events.
 *******************************************************************************
(*)
type
  PD3DXEventHandle = ^TD3DXEventHandle;
  TD3DXEventHandle = LongWord;

(*)
 *******************************************************************************
 * ID3DXAnimationCallbackHandler:
 * ------------------------------
 * This interface is intended to be implemented by the application, and can
 * be used to handle callbacks in animation sets generated when
 * ID3DXAnimationController::AdvanceTime() is called.
 *******************************************************************************
(*)
  PID3DXAnimationCallbackHandler = ^ID3DXAnimationCallbackHandler;
  ID3DXAnimationCallbackHandler = interface
    (*)
     ***************************************************************************
     * ID3DXAnimationCallbackHandler::HandleCallback:
     * ----------------------------------------------
     * This method gets called when a callback occurs for an animation set in one
     * of the tracks during the ID3DXAnimationController::AdvanceTime() call.
     *
     * Parameters:
     *  Track
     *      Index of the track on which the callback occured.
     *  pCallbackData
     *      Pointer to user owned callback data.
     *
     ***************************************************************************
    (*)
    function HandleCallback(Track : Cardinal; CallbackData : Pointer) : HResult; stdcall;
  end;
  

(*)
 *******************************************************************************
 * ID3DXAnimationController
 * -------------------------
 * This interface implements the main animation functionality. It connects
 * animation sets with the transform frames that are being animated. Allows
 * mixing multiple animations for blended animations or for transistions
 * It adds also has methods to modify blending parameters over time to
 * enable smooth transistions and other effects.
 *******************************************************************************
(*)

type
  ID3DXAnimationController = interface;
  ID3DXAnimationController = interface(IUnknown)
    ['{39F628C0-CD5B-41d6-8E9C-3BBBB66FDA57}']
    (*** ID3DXAnimationController methods ***)
    // Max sizes
    function GetMaxNumAnimationOutputs : Cardinal; stdcall;
    function GetMaxNumAnimationSets : Cardinal; stdcall;
    function GetMaxNumTracks : Cardinal; stdcall;
    function GetMaxNumEvents : Cardinal; stdcall;

    // Animation output registration
    function RegisterAnimationOutput(Name : PAnsiChar; Matrix : PD3DXMatrix; Scale : PD3DXVector3; Rotation : PD3DXQuaternion; Translation : PD3DXVector3) : HResult; stdcall;

    // Animation set registration
    function RegisterAnimationSet(AnimSet : ID3DXAnimationSet) : HResult; stdcall;
    function UnregisterAnimationSet(AnimSet : ID3DXAnimationSet) : HResult; stdcall;

    function GetNumAnimationSets : Cardinal; stdcall;
    function GetAnimationSet(Index : Cardinal; out AnimationSet : ID3DXAnimationSet) : HResult; stdcall;

    // Global time
    function AdvanceTime(TimeDelta : Double; CallbackHandler : ID3DXAnimationCallbackHandler) : HResult; stdcall;
    function ResetTime : HResult; stdcall;
    function GetTime : Double; stdcall;

    // Tracks
    function SetTrackAnimationSet(Track : Cardinal; AnimSet : ID3DXAnimationSet) : HResult; stdcall;
    function GetTrackAnimationSet(Track : Cardinal; out AnimSet : ID3DXAnimationSet) : HResult; stdcall;

    function SetTrackPriority(Track : Cardinal; Priority : TD3DXPriorityType) : HResult; stdcall;

    function SetTrackSpeed(Track : Cardinal; Speed : Single) : HResult; stdcall;
    function SetTrackWeight(Track : Cardinal; Weight : Single) : HResult; stdcall;
    function SetTrackPosition(Track : Cardinal; Position : Double) : HResult; stdcall;
    function SetTrackEnable(Track : Cardinal; Enable : Bool) : HResult; stdcall;

    function SetTrackDesc(Track : Cardinal; var Desc : TD3DXTrackDesc) : HResult; stdcall;
    function GetTrackDesc(Track : Cardinal; out Desc : TD3DXTrackDesc) : HResult; stdcall;

    // Priority blending
    function SetPriorityBlend(BlendWeight : Single) : HResult; stdcall;
    function GetPriorityBlend : Single; stdcall;

    // Event keying
    function KeyTrackSpeed(Track : Cardinal; NewSpeed : Single; StartTime, Duration : Double; Transition : TD3DXTransitionType) : TD3DXEventHandle; stdcall;
    function KeyTrackWeight(Track : Cardinal; NewWeight : Single; StartTime, Duration : Double; Transition : TD3DXTransitionType) : TD3DXEventHandle; stdcall;
    function KeyTrackPosition(Track : Cardinal; NewPosition, StartTime : Double) : TD3DXEventHandle; stdcall;
    function KeyTrackEnable(Track : Cardinal; NewEnable : Bool; StartTime : Double) : TD3DXEventHandle; stdcall;

    function KeyPriorityBlend(NewBlendWeight : Single; StartTime, Duration : Double; Transition : TD3DXTransitionType) : TD3DXEventHandle; stdcall;

    // Event unkeying
    function UnkeyEvent(Event : TD3DXEventHandle) : HResult; stdcall;

    function UnkeyAllTrackEvents(Track : Cardinal) : HResult; stdcall;
    function UnkeyAllPriorityBlends : HResult; stdcall;

    // Event enumeration
    function GetCurrentTrackEvent(Track : Cardinal; EventType : TD3DXEventType) : TD3DXEventHandle; stdcall;
    function GetCurrentPriorityBlend : TD3DXEventHandle; stdcall;

    function GetUpcomingTrackEvent(Track : Cardinal; Event : TD3DXEventHandle) : TD3DXEventHandle; stdcall;
    function GetUpcomingPriorityBlend(Event : TD3DXEventHandle) : TD3DXEventHandle; stdcall;

    function ValidateEvent(Event : TD3DXEventHandle) : HResult; stdcall;

    function GetEventDesc(Event : TD3DXEventHandle; out Desc : TD3DXEventDesc) : HResult; stdcall;

    // Cloning
    function CloneAnimationController(MaxNumAnimationOutputs, MaxNumAnimationSets, MaxNumTracks, MaxNumEvents : Cardinal; out AnimController : ID3DXAnimationController) : HResult; stdcall;
  end;


(*)
 *******************************************************************************
 * D3DXLoadMeshHierarchyFromX:
 * ---------------------------
 * Loads the first frame hierarchy in a .X file.
 *
 * Parameters:
 *  Filename
 *      Name of the .X file
 *  MeshOptions
 *      Mesh creation options for meshes in the file (see d3dx9mesh.h)
 *  pD3DDevice
 *      D3D9 device on which meshes in the file are created in
 *  pAlloc
 *      Allocation interface used to allocate nodes of the frame hierarchy
 *  pUserDataLoader
 *      Application provided interface to allow loading of user data
 *  ppFrameHierarchy
 *      Returns root node pointer of the loaded frame hierarchy
 *  ppAnimController
 *      Returns pointer to an animation controller corresponding to animation
 *		in the .X file. This is created with default max tracks and events
 *
 *******************************************************************************
(*)

function D3DXLoadMeshHierarchyFromXA(Filename : PAnsiChar; MeshOptions : LongWord; D3DDevice : IDirect3DDevice9; Alloc : ID3DXAllocateHierarchy; UserDataLoader : ID3DXLoadUserData; out FrameHierarchy : PD3DXFrame; out AnimController : ID3DXAnimationController) : HResult; stdcall; external d3dx9dllname;

function D3DXLoadMeshHierarchyFromXW(Filename : PWideChar; MeshOptions : LongWord; D3DDevice : IDirect3DDevice9; Alloc : ID3DXAllocateHierarchy; UserDataLoader : ID3DXLoadUserData; out FrameHierarchy : PD3DXFrame; out AnimController : ID3DXAnimationController) : HResult; stdcall; external d3dx9dllname;

function D3DXLoadMeshHierarchyFromX(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; MeshOptions : LongWord; D3DDevice : IDirect3DDevice9; Alloc : ID3DXAllocateHierarchy; UserDataLoader : ID3DXLoadUserData; out FrameHierarchy : PD3DXFrame; out AnimController : ID3DXAnimationController) : HResult; stdcall; external d3dx9dllname name {$IFDEF UNICODE}'D3DXLoadMeshHierarchyFromXW';{$ELSE}'D3DXLoadMeshHierarchyFromXA';{$ENDIF}

function D3DXLoadMeshHierarchyFromXInMemory(Memory : Pointer; SizeOfMemory, MeshOptions : LongWord; D3DDevice : IDirect3DDevice9; Alloc : ID3DXAllocateHierarchy; UserDataLoader : ID3DXLoadUserData; out FrameHierarchy : PD3DXFrame; out AnimController : ID3DXAnimationController) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXSaveMeshHierarchyToFile:
 * ---------------------------
 * Creates a .X file and saves the mesh hierarchy and corresponding animations
 * in it
 *
 * Parameters:
 *  Filename
 *      Name of the .X file
 *  XFormat
 *      Format of the .X file (text or binary, compressed or not, etc)
 *  pFrameRoot
 *      Root node of the hierarchy to be saved
 *  pAnimController
 *      The animation mixer whose animation sets are to be stored
 *  pUserDataSaver
 *      Application provided interface to allow adding of user data to
 *        data objects saved to .X file *
 *******************************************************************************
(*)

function D3DXSaveMeshHierarchyToFileA(Filename : PAnsiChar; XFormat : LongWord; FrameRoot : PD3DXFrame; AnimMixer : ID3DXAnimationController; UserDataSaver : ID3DXSaveUserData) : HResult; stdcall; external d3dx9dllname;

function D3DXSaveMeshHierarchyToFileW(Filename : PWideChar; XFormat : LongWord; FrameRoot : PD3DXFrame; AnimMixer : ID3DXAnimationController; UserDataSaver : ID3DXSaveUserData) : HResult; stdcall; external d3dx9dllname;

function D3DXSaveMeshHierarchyToFile(Filename : {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}; XFormat : LongWord; FrameRoot : PD3DXFrame; AnimMixer : ID3DXAnimationController; UserDataSaver : ID3DXSaveUserData) : HResult; stdcall; external d3dx9dllname name {$IFDEF UNICODE}'D3DXSaveMeshHierarchyToFileW';{$ELSE}'D3DXSaveMeshHierarchyToFileA';{$ENDIF}

(*)
 *******************************************************************************
 * D3DXFrameDestroy:
 * -----------------
 * Destroys the subtree of frames under the root, including the root
 *
 * Parameters:
 *	pFrameRoot
 *		Pointer to the root node
 *  pAlloc
 *      Allocation interface used to de-allocate nodes of the frame hierarchy
 *******************************************************************************
(*)

function D3DXFrameDestroy(FrameRoot : PD3DXFrame; Alloc : ID3DXAllocateHierarchy) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXFrameAppendChild:
 * ---------------------
 * Add a child frame to a frame
 *
 * Parameters:
 *	pFrameParent
 *		Pointer to the parent node
 *  pFrameChild
 *      Pointer to the child node
 *
 *******************************************************************************
(*)
function D3DXFrameAppendChild(FrameParent, FrameChild : PD3DXFrame) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXFrameFind:
 * --------------
 * Finds a frame with the given name.  Returns NULL if no frame found.
 *
 * Parameters:
 *	pFrameRoot
 *		Pointer to the root node
 *  Name
 *      Name of frame to find
 *******************************************************************************
(*)
function D3DXFrameFind(FrameRoot : PD3DXFrame; Name : PAnsiChar) : PD3DXFrame; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXFrameRegisterNamedMatrices:
 * --------------------------
 * Finds all frames that have non-null names and registers each of those frame
 * matrices to the given animation mixer
 *
 * Parameters:
 *	pFrameRoot
 *		Pointer to the root node
 *	pAnimMixer
 *		Pointer to the animation mixer where the matrices are registered
 *******************************************************************************
(*)

function D3DXFrameRegisterNamedMatrices(FrameRoot : PD3DXFrame; AnimMixer : ID3DXAnimationController) : HResult; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXFrameNumNamedMatrices:
 * --------------------------
 * Counts number of frames in a subtree that have non-null names
 *
 * Parameters:
 *	pFrameRoot
 *		Pointer to the root node of the subtree
 * Return Value:
 *		Count of frames
 *******************************************************************************
(*)

function D3DXFrameNumNamedMatrices(FrameRoot : PD3DXFrame) : Cardinal; stdcall; external d3dx9dllname;

(*)
 *******************************************************************************
// D3DXFrameCalculateBoundingSphere:
// ---------------------------------
// Computes the bounding sphere of all the meshes in the frame hierarchy
//
// Parameters:
//	pFrameRoot
//		Pointer to the root node
//	pObjectCenter
//		Returns the center of the bounding sphere
//	pObjectRadius
//		Returns the radius of the bounding sphere
 *******************************************************************************
(*)

function D3DXFrameCalculateBoundingSphere(FrameRoot : PD3DXFrame; ObjectCenter : PD3DXVector3; ObjectRadius : PSingle) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXFrameCalculateBoundingSphere(FrameRoot : PD3DXFrame; out ObjectCenter : TD3DXVector3; ObjectRadius : PSingle) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXFrameCalculateBoundingSphere(FrameRoot : PD3DXFrame; ObjectCenter : PD3DXVector3; out ObjectRadius : Single) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXFrameCalculateBoundingSphere(FrameRoot : PD3DXFrame; out ObjectCenter : TD3DXVector3; out ObjectRadius : Single) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXCreateKeyframedAnimationSet:
 * --------------------------------
 * This function creates a compressable keyframed animations set interface.
 *
 * Parameters:
 *  pName
 *      Name of the animation set
 *  TicksPerSecond
 *      Number of keyframe ticks that elapse per second
 *  Playback
 *      Playback mode of keyframe looping
 *  NumAnimations
 *      Number of SRT animations
 *  NumCallbackKeys
 *      Number of callback keys
 *  pCallbackKeys
 *      Array of callback keys
 *  ppAnimationSet
 *      Returns the animation set interface
 *******************************************************************************
(*)

function D3DXCreateKeyframedAnimationSet(Name : PAnsiChar; TicksPerSecond : Double; Playback : TD3DXPlaybackType; NumAnimations : Cardinal; NumCallbackKeys : Cardinal; const CallbackKeys : TD3DXKeyCallback; out AnimationSet : ID3DXKeyframedAnimationSet) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateKeyframedAnimationSet(Name : PAnsiChar; TicksPerSecond : Double; Playback : TD3DXPlaybackType; NumAnimations : Cardinal; NumCallbackKeys : Cardinal; const CallbackKeys : TD3DXKeyCallback; AnimationSet : PID3DXKeyframedAnimationSet) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXCreateCompressedAnimationSet:
 * --------------------------------
 * This function creates a compressed animations set interface from
 * compressed data.
 *
 * Parameters:
 *  pName
 *      Name of the animation set
 *  TicksPerSecond
 *      Number of keyframe ticks that elapse per second
 *  Playback
 *      Playback mode of keyframe looping
 *  pCompressedData
 *      Compressed animation SRT data
 *  NumCallbackKeys
 *      Number of callback keys
 *  pCallbackKeys
 *      Array of callback keys
 *  ppAnimationSet
 *      Returns the animation set interface
 *
 *******************************************************************************
(*)

function D3DXCreateCompressedAnimationSet(Name : PAnsiChar; TicksPerSecond : Double; Playback : TD3DXPlaybackType; CompressedData : ID3DXBuffer; NumCallbackKeys : Cardinal; const CallbackKeys : TD3DXKeyCallback; out AnimationSet : ID3DXCompressedAnimationSet) : HResult; stdcall; overload; external d3dx9dllname;
function D3DXCreateCompressedAnimationSet(Name : PAnsiChar; TicksPerSecond : Double; Playback : TD3DXPlaybackType; CompressedData : ID3DXBuffer; NumCallbackKeys : Cardinal; const CallbackKeys : TD3DXKeyCallback; AnimationSet : PID3DXCompressedAnimationSet) : HResult; stdcall; overload; external d3dx9dllname;

(*)
 *******************************************************************************
 * D3DXCreateAnimationController:
 * ------------------------------
 * This function creates an animation controller object.
 *
 * Parameters:
 *  MaxNumMatrices
 *      Maximum number of matrices that can be animated
 *  MaxNumAnimationSets
 *      Maximum number of animation sets that can be played
 *  MaxNumTracks
 *      Maximum number of animation sets that can be blended
 *  MaxNumEvents
 *      Maximum number of outstanding events that can be scheduled at any given time
 *  ppAnimController
 *      Returns the animation controller interface
 *******************************************************************************
(*)

function D3DXCreateAnimationController(MaxNumMatrices, MaxNumAnimationSets, MaxNumTracks, MaxNumEvents : Cardinal; out AnimController : ID3DXAnimationController) : HResult; stdcall; external d3dx9dllname;

const
  D3DXERR_CANNOTMODIFYINDEXBUFFER     = HResult(MAKE_D3DHResult + 2900);
  D3DXERR_INVALIDMESH                 = HResult(MAKE_D3DHResult + 2901);
  D3DXERR_CANNOTATTRSORT              = HResult(MAKE_D3DHResult + 2902);
  D3DXERR_SKINNINGNOTSUPPORTED        = HResult(MAKE_D3DHResult + 2903);
  D3DXERR_TOOMANYINFLUENCES           = HResult(MAKE_D3DHResult + 2904);
  D3DXERR_INVALIDDATA                 = HResult(MAKE_D3DHResult + 2905);
  D3DXERR_LOADEDMESHASNODATA          = HResult(MAKE_D3DHResult + 2906);
  D3DXERR_DUPLICATENAMEDFRAGMENT      = HResult(MAKE_D3DHResult + 2907);

implementation

function D3DXToRadian(const Degree : Single) : Single;
begin
  Result := Degree * (D3DX_PI / 180.0);
end;

function D3DXToDegree(const Radian : Single) : Single;
begin
  Result := Radian * (180.0 / D3DX_PI);
end;

(*)
 *******************************************************************************
 * 2D Vector
 *******************************************************************************
(*)

function D3DXVector2(const x, y : Single) : TD3DXVector2;
begin
  Result.x := x;
  Result.y := y;
end;

function D3DXVector2Equal(const V1, V2 : TD3DXVector2) : Boolean;
begin
  Result := (v1.x = v2.x) and (v1.y = v2.y);
end;

(*)
 *******************************************************************************
 * 3D Vector
 *******************************************************************************
(*)

function D3DXVector3(const x, y, z : Single) : TD3DXVector3;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

function D3DXVector3Equal(const v1, v2 : TD3DXVector3) : Boolean;
begin
  Result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z);
end;

(*)
 *******************************************************************************
 * 4D Vector
 *******************************************************************************
(*)

function D3DXVector4(const x, y, z, w : Single) : TD3DXVector4;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.w := w;
end;

function D3DXVector4Equal(const v1, v2 : TD3DXVector4) : Boolean;
begin
  Result:= (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z) and (v1.w = v2.w);
end;

(*)
 *******************************************************************************
 *
 * Matrices
 *
 *******************************************************************************
(*)

function D3DXMatrix(const m00, m01, m02, m03,
                          m10, m11, m12, m13,
                          m20, m21, m22, m23,
                          m30, m31, m32, m33 : Single) : TD3DXMatrix;
begin
  with Result do
  begin
    m[0,0] := m00;
    m[0,1] := m01;
    m[0,2] := m02;
    m[0,3] := m03;
    m[1,0] := m10;
    m[1,1] := m11;
    m[1,2] := m12;
    m[1,3] := m13;
    m[2,0] := m20;
    m[2,1] := m21;
    m[2,2] := m22;
    m[2,3] := m23;
    m[3,0] := m30;
    m[3,1] := m31;
    m[3,2] := m32;
    m[3,3] := m33;
  end;
end;

function D3DXMatrixAdd(const m1, m2 : TD3DXMatrix) : TD3DXMatrix;
var i,j : Byte;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Result.m[i,j] := m1.m[i,j] + m2.m[i,j];
end;

function D3DXMatrixSubtract(const m1, m2 : TD3DXMatrix) : TD3DXMatrix;
var i, j : Byte;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Result.m[i,j] := m1.m[i,j] - m2.m[i,j];
end;

function D3DXMatrixScale(const m : TD3DXMatrix; const ScaleBy : Single) : TD3DXMatrix;
var i, j : Byte;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Result.m[i,j] := m.m[i,j] * ScaleBy;
end;

function D3DXMatrixEqual(const m1, m2 : TD3DXMatrix) : Boolean;
begin
  Result := CompareMem(@m1, @m2, SizeOf(TD3DXMatrix));
end;

(*)
 *******************************************************************************
 *
 * Quaternions
 *
 *******************************************************************************
(*)

function D3DXQuaternion(const x, y, z, w : Single) : TD3DXQuaternion;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.w := w;
end;

function D3DXQuaternionAdd(const q1, q2 : TD3DXQuaternion) : TD3DXQuaternion;
begin
  with Result do
  begin
    x := q1.x + q2.x;
    y := q1.y + q2.y;
    z := q1.z + q2.z;
    w := q1.w + q2.w;
  end;
end;

function D3DXQuaternionSubtract(const q1, q2 : TD3DXQuaternion) : TD3DXQuaternion;
begin
  with Result do
  begin
    x := q1.x - q2.x;
    y := q1.y - q2.y;
    z := q1.z - q2.z;
    w := q1.w - q2.w;
  end;
end;

function D3DXQuaternionEqual(const q1, q2 : TD3DXQuaternion) : Boolean;
begin
  Result := (q1.x = q2.x) and (q1.y = q2.y) and (q1.z = q2.z) and (q1.w = q2.w);
end;

function D3DXQuaternionScale(const q : TD3DXQuaternion; const ScaleBy : Single) : TD3DXQuaternion;
begin
  with Result do
  begin
    x := q.x * ScaleBy;
    y := q.y * ScaleBy;
    z := q.z * ScaleBy;
    w := q.w * ScaleBy;
  end;
end;

(*)
 *******************************************************************************
 *
 * Planes
 *
 *******************************************************************************
(*)

function D3DXPlane(const a, b, c, d : Single) : TD3DXPlane;
begin
  Result.a := a;
  Result.b := b;
  Result.c := c;
  Result.d := d;
end;

function D3DXPlaneEqual(const p1, p2 : TD3DXPlane) : Boolean;
begin
  //Note there may be unlimited definitions of one and the same plane
  Result := (p1.a = p2.a) and (p1.b = p2.b) and (p1.c = p2.c) and (p1.d = p2.d);
end;

(*)
 *******************************************************************************
 *
 * Colors
 *
 *******************************************************************************
(*)

function D3DXColor(const r, g, b, a : Single) : TD3DXColor;
begin
  Result.r := r;
  Result.g := g;
  Result.b := b;
  Result.a := a;
end;

function D3DXColorToLongWord(const c : TD3DXColor) : LongWord;
type TLongWordAsArrayOfByte = array[0..3] of Byte;
  function ColorLimit(const x : Single) : Byte;
  begin
    if x > 1.0 then
      Result := 255
    else
      if x < 0 then
        Result := 0
      else
        Result := Trunc(x * 255.0);
  end;
begin
  with c do
  begin
    TLongWordAsArrayOfByte(Result)[3] := ColorLimit(a);
    TLongWordAsArrayOfByte(Result)[2] := ColorLimit(r);
    TLongWordAsArrayOfByte(Result)[1] := ColorLimit(g);
    TLongWordAsArrayOfByte(Result)[0] := ColorLimit(b);
  end;
end;

function D3DXColorFromLongWord(const c : LongWord) : TD3DXColor;
type TLongWordAsArrayOfByte = array[0..3] of Byte;
begin
  with Result do
  begin
    a := TLongWordAsArrayOfByte(c)[3] / 255;
    r := TLongWordAsArrayOfByte(c)[2] / 255;
    g := TLongWordAsArrayOfByte(c)[1] / 255;
    b := TLongWordAsArrayOfByte(c)[0] / 255;
  end;
end;

function D3DXColorEqual(const c1, c2 : TD3DXColor) : Boolean;
begin
  Result := (c1.r = c2.r) and (c1.g = c2.g) and (c1.b = c2.b) and (c1.a = c2.a);
end;

function D3DXTX_VERSION(_Major, _Minor : Byte) : LongWord;
begin
  Result := (Byte('T') shl 24) or (Byte('X') shl 16) or (_Major shl 8) or (_Minor);
end;

end.
