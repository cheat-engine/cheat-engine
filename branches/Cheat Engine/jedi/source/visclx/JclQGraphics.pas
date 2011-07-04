{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclGraphics.pas.                                                            }
{                                                                                                  }
{ The resampling algorithms and methods used in this library were adapted by Anders Melander from  }
{ the article "General Filtered Image Rescaling" by Dale Schumacher which appeared in the book     }
{ Graphics Gems III, published by Academic Press, Inc. Additional improvements were done by David  }
{ Ullrich and Josha Beukema.                                                                       }
{                                                                                                  }
{ (C)opyright 1997-1999 Anders Melander                                                            }
{                                                                                                  }
{ The Initial Developers of the Original Code are Alex Denissov, Wim De Cleen, Anders Melander     }
{ and Mike Lischke. Portions created by these individuals are Copyright (C) of these individuals.  }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Alexander Radchenko                                                                            }
{   Charlie Calvert                                                                                }
{   Marcel van Brakel                                                                              }
{   Marcin Wieczorek                                                                               }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Dejoy Den (dejoy)                                                                              }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclQGraphics;

{$I jcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Types, QGraphics, JclQGraphUtils,
  JclBase;

type
  EJclGraphicsError = class(EJclError);

  TDynDynIntegerArrayArray = array of TDynIntegerArray;
  TDynPointArray = array of TPoint;
  TDynDynPointArrayArray = array of TDynPointArray;
  TPointF = record
    X: Single;
    Y: Single;
  end;
  TDynPointArrayF = array of TPointF;

  { TJclBitmap32 draw mode }
  TDrawMode = (dmOpaque, dmBlend);

  { stretch filter }
  TStretchFilter = (sfNearest, sfLinear, sfSpline);

  TConversionKind = (ckRed, ckGreen, ckBlue, ckAlpha, ckUniformRGB, ckWeightedRGB);

  { resampling support types }
  TResamplingFilter =
    (rfBox, rfTriangle, rfHermite, rfBell, rfSpline, rfLanczos3, rfMitchell);

  { Matrix declaration for transformation }
  //  modify Jan 28, 2001 for use under BCB5
  //         the compiler show error 245 "language feature ist not available"
  //         we must take a record and under this we can use the static array
  //  Note:  the sourcecode modify general from M[] to M.A[] !!!!!
  //  TMatrix3d = array [0..2, 0..2] of Extended;  // 3x3 double precision

  TMatrix3d = record
    A: array [0..2, 0..2] of Extended;
  end;

  TDynDynPointArrayArrayF = array of TDynPointArrayF;

  TScanLine = array of Integer;
  TScanLines = array of TScanLine;

  TLUT8 = array [Byte] of Byte;
  TGamma = array [Byte] of Byte;
  TColorChannel = (ccRed, ccGreen, ccBlue, ccAlpha);

  TGradientDirection = (gdVertical, gdHorizontal);

  TPolyFillMode = (fmAlternate, fmWinding);
  TJclRegionCombineOperator = (coAnd, coDiff, coOr, coXor);
  TJclRegionBitmapMode = (rmInclude, rmExclude);
  TJclRegionKind = (rkNull, rkSimple, rkComplex, rkError);

//  modify Jan 28, 2001 for use under BCB5
//         the compiler show error 245 "language feature ist not available"
//         wie must take a record and under this we can use the static array
//  Note:  for init the array we used initialisation at the end of this unit
//
//  const
//    IdentityMatrix: TMatrix3d = (
//      (1, 0, 0),
//      (0, 1, 0),
//      (0, 0, 1));

var
  IdentityMatrix: TMatrix3d;

// Classes
type


  TJclTransformation = class(TObject)
  public
    function  GetTransformedBounds(const Src: TRect): TRect; virtual; abstract;
    procedure PrepareTransform; virtual; abstract;
    procedure Transform(DstX, DstY: Integer; out SrcX, SrcY: Integer); virtual; abstract;
    procedure Transform256(DstX, DstY: Integer; out SrcX256, SrcY256: Integer); virtual; abstract;
  end;

  TJclLinearTransformation = class(TJclTransformation)
  private
    FMatrix: TMatrix3d;
  protected
    A: Integer;
    B: Integer;
    C: Integer;
    D: Integer;
    E: Integer;
    F: Integer;
  public
    constructor Create; virtual;
    function  GetTransformedBounds(const Src: TRect): TRect; override;
    procedure PrepareTransform; override;
    procedure Transform(DstX, DstY: Integer; out SrcX, SrcY: Integer); override;
    procedure Transform256(DstX, DstY: Integer; out SrcX256, SrcY256: Integer); override;
    procedure Clear;
    procedure Rotate(Cx, Cy, Alpha: Extended); // degrees
    procedure Skew(Fx, Fy: Extended);
    procedure Scale(Sx, Sy: Extended);
    procedure Translate(Dx, Dy: Extended);
    property Matrix: TMatrix3d read FMatrix write FMatrix;
  end;

// Bitmap Functions
procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter;
  Radius: Single; Source: TGraphic; Target: TBitmap); overload;
procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter;
  Radius: Single; Bitmap: TBitmap); overload;

{$IFDEF MSWINDOWS}
procedure DrawBitmap(DC: HDC; Bitmap: HBITMAP; X, Y, Width, Height: Integer);

function ExtractIconCount(const FileName: string): Integer;
function BitmapToIcon(Bitmap: HBITMAP; cx, cy: Integer): HICON; overload;
function BitmapToIcon(Bitmap, Mask: HBITMAP; cx, cy: Integer): HICON; overload;
function IconToBitmap(Icon: HICON): HBITMAP;
{$ENDIF MSWINDOWS}



{$IFDEF MSWINDOWS}
function FillGradient(DC: HDC; ARect: TRect; ColorCount: Integer;
  StartColor, EndColor: TColor; ADirection: TGradientDirection): Boolean; overload;
{$ENDIF MSWINDOWS}



{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/visclx/JclQGraphics.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\visclx'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  Math,
  {$IFDEF MSWINDOWS}
  CommCtrl, ShellApi,
  {$ENDIF MSWINDOWS}
  JclLogic;

type
  TRGBInt = record
    R: Integer;
    G: Integer;
    B: Integer;
  end;

  PBGRA = ^TBGRA;
  TBGRA = packed record
    B: Byte;
    G: Byte;
    R: Byte;
    A: Byte;
  end;

  PPixelArray = ^TPixelArray;
  TPixelArray = array [0..0] of TBGRA;

  TBitmapFilterFunction = function(Value: Single): Single;

  PContributor = ^TContributor;
  TContributor = record
   Weight: Integer; // Pixel Weight
   Pixel: Integer;  // Source Pixel
  end;

  TContributors = array of TContributor;

  // list of source pixels contributing to a destination pixel
  TContributorEntry = record
   N: Integer;
   Contributors: TContributors;
  end;

  TContributorList = array of TContributorEntry;
  TJclGraphicAccess = class(TGraphic);

const
  DefaultFilterRadius: array [TResamplingFilter] of Single =
    (0.5, 1.0, 1.0, 1.5, 2.0, 3.0, 2.0);
  _RGB: TColor32 = $00FFFFFF;

var
  { Gamma bias for line/pixel antialiasing/shape correction }
  GAMMA_TABLE: TGamma;

threadvar
  // globally used cache for current image (speeds up resampling about 10%)
  CurrentLineR: array of Integer;
  CurrentLineG: array of Integer;
  CurrentLineB: array of Integer;

//=== Helper functions =======================================================

function IntToByte(Value: Integer): Byte;
begin
  Result := Math.Max(0, Math.Min(255, Value));
end;


//=== Internal low level routines ============================================

procedure FillLongword(var X; Count: Integer; Value: Longword);
{asm
// EAX = X
// EDX = Count
// ECX = Value
        TEST    EDX, EDX
        JLE     @@EXIT

        PUSH    EDI
        MOV     EDI, EAX  // Point EDI to destination
        MOV     EAX, ECX
        MOV     ECX, EDX
        REP     STOSD    // Fill count dwords
        POP     EDI
@@EXIT:
end;}
var
  P: PLongword;
begin
  P := @X;
  while Count > 0 do
  begin
    P^ := Value;
    Inc(P);
    Dec(Count);
  end;
end;

function Clamp(Value: Integer): TColor32;
begin
  if Value < 0 then
    Result := 0
  else
  if Value > 255 then
    Result := 255
  else
    Result := Value;
end;

procedure TestSwap(var A, B: Integer);
{asm
// EAX = [A]
// EDX = [B]
        MOV     ECX, [EAX]     // ECX := [A]
        CMP     ECX, [EDX]     // ECX <= [B]? Exit
        JLE     @@EXIT
        //Replaced on more fast code
        //XCHG    ECX, [EDX]     // ECX <-> [B];
        //MOV     [EAX], ECX     // [A] := ECX
        PUSH    EBX
        MOV     EBX,[EDX]      // EBX := [B]
        MOV     [EAX],EBX      // [A] := EBX
        MOV     [EDX],ECX      // [B] := ECX
        POP     EBX
@@EXIT:
end;}
var
  X: Integer;
begin
  X := A; // optimization
  if X > B then
  begin
    A := B;
    B := X;
  end;
end;

function TestClip(var A, B: Integer; Size: Integer): Boolean;
begin
  TestSwap(A, B); // now A = min(A,B) and B = max(A, B)
  if A < 0 then
    A := 0;
  if B >= Size then
    B := Size - 1;
  Result := B >= A;
end;

function Constrain(Value, Lo, Hi: Integer): Integer;
begin
  if Value <= Lo then
    Result := Lo
  else
  if Value >= Hi then
    Result := Hi
  else
    Result := Value;
end;

// Filter functions for stretching of TBitmaps
// f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1

function BitmapHermiteFilter(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1 then
    Result := (2 * Value - 3) * Sqr(Value) + 1
  else
    Result := 0;
end;

// This filter is also known as 'nearest neighbour' Filter.

function BitmapBoxFilter(Value: Single): Single;
begin
  if (Value > -0.5) and (Value <= 0.5) then
    Result := 1.0
  else
    Result := 0.0;
end;

// aka 'linear' or 'bilinear' filter

function BitmapTriangleFilter(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
    Result := 1.0 - Value
  else
    Result := 0.0;
end;

function BitmapBellFilter(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 0.5 then
    Result := 0.75 - Sqr(Value)
  else
  if Value < 1.5 then
  begin
    Value := Value - 1.5;
    Result := 0.5 * Sqr(Value);
  end
  else
    Result := 0.0;
end;

// B-spline filter

function BitmapSplineFilter(Value: Single): Single;
var
  Temp: Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
  begin
    Temp := Sqr(Value);
    Result := 0.5 * Temp * Value - Temp + 2.0 / 3.0;
  end
  else
  if Value < 2.0 then
  begin
    Value := 2.0 - Value;
    Result := Sqr(Value) * Value / 6.0;
  end
  else
    Result := 0.0;
end;

function BitmapLanczos3Filter(Value: Single): Single;

  function SinC(Value: Single): Single;
  begin
    if Value <> 0.0 then
    begin
      Value := Value * Pi;
      Result := System.Sin(Value) / Value;
    end
    else
      Result := 1.0;
  end;

begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 3.0 then
    Result := SinC(Value) * SinC(Value / 3.0)
  else
    Result := 0.0;
end;

function BitmapMitchellFilter(Value: Single): Single;
const
  B = 1.0 / 3.0;
  C = 1.0 / 3.0;
var
  Temp: Single;
begin
  if Value < 0.0 then
    Value := -Value;
  Temp := Sqr(Value);
  if Value < 1.0 then
  begin
    Value := (((12.0 - 9.0 * B - 6.0 * C) * (Value * Temp)) +
      ((-18.0 + 12.0 * B + 6.0 * C) * Temp) +
      (6.0 - 2.0 * B));
    Result := Value / 6.0;
  end
  else
  if Value < 2.0 then
  begin
    Value := (((-B - 6.0 * C) * (Value * Temp)) +
      ((6.0 * B + 30.0 * C) * Temp) +
      ((-12.0 * B - 48.0 * C) * Value) +
      (8.0 * B + 24.0 * C));
    Result := Value / 6.0;
  end
  else
    Result := 0.0;
end;

const
  FilterList: array [TResamplingFilter] of TBitmapFilterFunction =
   (
    BitmapBoxFilter,
    BitmapTriangleFilter,
    BitmapHermiteFilter,
    BitmapBellFilter,
    BitmapSplineFilter,
    BitmapLanczos3Filter,
    BitmapMitchellFilter
   );

procedure FillLineCache(N, Delta: Integer; Line: Pointer);
var
  I: Integer;
  Run: PBGRA;
begin
  Run := Line;
  for I := 0 to N - 1 do
  begin
    CurrentLineR[I] := Run.R;
    CurrentLineG[I] := Run.G;
    CurrentLineB[I] := Run.B;
    Inc(PByte(Run), Delta);
  end;
end;

function ApplyContributors(N: Integer; Contributors: TContributors): TBGRA;
var
  J: Integer;
  RGB: TRGBInt;
  Total,
  Weight: Integer;
  Pixel: Cardinal;
  Contr: PContributor;
begin
  RGB.R := 0;
  RGB.G := 0;
  RGB.B := 0;
  Total := 0;
  Contr := @Contributors[0];
  for J := 0 to N - 1 do
  begin
    Weight := Contr.Weight;
    Inc(Total, Weight);
    Pixel := Contr.Pixel;
    Inc(RGB.R, CurrentLineR[Pixel] * Weight);
    Inc(RGB.G, CurrentLineG[Pixel] * Weight);
    Inc(RGB.B, CurrentLineB[Pixel] * Weight);
    Inc(Contr);
  end;

  if Total = 0 then
  begin
    Result.R := IntToByte(RGB.R shr 8);
    Result.G := IntToByte(RGB.G shr 8);
    Result.B := IntToByte(RGB.B shr 8);
  end
  else
  begin
    Result.R := IntToByte(RGB.R div Total);
    Result.G := IntToByte(RGB.G div Total);
    Result.B := IntToByte(RGB.B div Total);
  end;
end;

// This is the actual scaling routine. Target must be allocated already with
// sufficient size. Source must contain valid data, Radius must not be 0 and
// Filter must not be nil.

procedure DoStretch(Filter: TBitmapFilterFunction; Radius: Single; Source, Target: TBitmap);
var
  ScaleX, ScaleY: Single; // Zoom scale factors
  I, J, K, N: Integer;    // Loop variables
  Center: Single;         // Filter calculation variables
  Width: Single;
  Weight: Integer;        // Filter calculation variables
  Left, Right: Integer;   // Filter calculation variables
  Work: TBitmap;
  ContributorList: TContributorList;
  SourceLine, DestLine: PPixelArray;
  DestPixel: PBGRA;
  Delta, DestDelta: Integer;
  SourceHeight, SourceWidth: Integer;
  TargetHeight, TargetWidth: Integer;
begin
  // shortcut variables
  SourceHeight := Source.Height;
  SourceWidth := Source.Width;
  TargetHeight := Target.Height;
  TargetWidth := Target.Width;
  // create intermediate image to hold horizontal zoom
  Work := TBitmap.Create;
  try
    Work.PixelFormat := pf32bit;
    Work.Height := SourceHeight;
    Work.Width := TargetWidth;
    if SourceWidth = 1 then
      ScaleX := TargetWidth / SourceWidth
    else
      ScaleX := (TargetWidth - 1) / (SourceWidth - 1);
    if SourceHeight = 1 then
      ScaleY := TargetHeight / SourceHeight
    else
      ScaleY := (TargetHeight - 1) / (SourceHeight - 1);

    // pre-calculate filter contributions for a row
    SetLength(ContributorList, TargetWidth);
    // horizontal sub-sampling
    if ScaleX < 1 then
    begin
      // scales from bigger to smaller Width
      Width := Radius / ScaleX;
      for I := 0 to TargetWidth - 1 do
      begin
        ContributorList[I].N := 0;
        Center := I / ScaleX;
        Left := Math.Floor(Center - Width);
        Right := Math.Ceil(Center + Width);
        SetLength(ContributorList[I].Contributors, Right - Left + 1);
        for J := Left to Right do
        begin
          Weight := Round(Filter((Center - J) * ScaleX) * ScaleX * 256);
          if Weight <> 0 then
          begin
            if J < 0 then
              N := -J
            else
            if J >= SourceWidth then
              N := SourceWidth - J + SourceWidth - 1
            else
              N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end;
    end
    else
    begin
      // horizontal super-sampling
      // scales from smaller to bigger Width
      for I := 0 to TargetWidth - 1 do
      begin
        ContributorList[I].N := 0;
        Center := I / ScaleX;
        Left := Math.Floor(Center - Radius);
        Right := Math.Ceil(Center + Radius);
        SetLength(ContributorList[I].Contributors, Right - Left + 1);
        for J := Left to Right do
        begin
          Weight := Round(Filter(Center - J) * 256);
          if Weight <> 0 then
          begin
            if J < 0 then
              N := -J
            else
            if J >= SourceWidth then
              N := SourceWidth - J + SourceWidth - 1
            else
              N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end;
    end;

    // now apply filter to sample horizontally from Src to Work

    SetLength(CurrentLineR, SourceWidth);
    SetLength(CurrentLineG, SourceWidth);
    SetLength(CurrentLineB, SourceWidth);
    for K := 0 to SourceHeight - 1 do
    begin
      SourceLine := Source.ScanLine[K];
      FillLineCache(SourceWidth, SizeOf(TBGRA), SourceLine);
      DestPixel := Work.ScanLine[K];
      for I := 0 to TargetWidth - 1 do
        with ContributorList[I] do
        begin
          DestPixel^ := ApplyContributors(N, ContributorList[I].Contributors);
          // move on to next column
          Inc(DestPixel);
        end;
    end;

    // free the memory allocated for horizontal filter weights, since we need
    // the structure again
    for I := 0 to TargetWidth - 1 do
      ContributorList[I].Contributors := nil;
    ContributorList := nil;

    // pre-calculate filter contributions for a column
    SetLength(ContributorList, TargetHeight);
    // vertical sub-sampling
    if ScaleY < 1 then
    begin
      // scales from bigger to smaller height
      Width := Radius / ScaleY;
      for I := 0 to TargetHeight - 1 do
      begin
        ContributorList[I].N := 0;
        Center := I / ScaleY;
        Left := Math.Floor(Center - Width);
        Right := Math.Ceil(Center + Width);
        SetLength(ContributorList[I].Contributors, Right - Left + 1);
        for J := Left to Right do
        begin
          Weight := Round(Filter((Center - J) * ScaleY) * ScaleY * 256);
          if Weight <> 0 then
          begin
            if J < 0 then
              N := -J
            else
            if J >= SourceHeight then
              N := SourceHeight - J + SourceHeight - 1
            else
              N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end;
    end
    else
    begin
      // vertical super-sampling
      // scales from smaller to bigger height
      for I := 0 to TargetHeight - 1 do
      begin
        ContributorList[I].N := 0;
        Center := I / ScaleY;
        Left := Math.Floor(Center - Radius);
        Right := Math.Ceil(Center + Radius);
        SetLength(ContributorList[I].Contributors, Right - Left + 1);
        for J := Left to Right do
        begin
          Weight := Round(Filter(Center - J) * 256);
          if Weight <> 0 then
          begin
            if J < 0 then
              N := -J
            else
            if J >= SourceHeight then
              N := SourceHeight - J + SourceHeight - 1
            else
              N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end;
    end;

    // apply filter to sample vertically from Work to Target
    SetLength(CurrentLineR, SourceHeight);
    SetLength(CurrentLineG, SourceHeight);
    SetLength(CurrentLineB, SourceHeight);

    SourceLine := Work.ScanLine[0];
    Delta := Integer(Work.ScanLine[1]) - Integer(SourceLine);
    DestLine := Target.ScanLine[0];
    DestDelta := Integer(Target.ScanLine[1]) - Integer(DestLine);
    for K := 0 to TargetWidth - 1 do
    begin
      DestPixel := Pointer(DestLine);
      FillLineCache(SourceHeight, Delta, SourceLine);
      for I := 0 to TargetHeight - 1 do
        with ContributorList[I] do
        begin
          DestPixel^ := ApplyContributors(N, ContributorList[I].Contributors);
          Inc(Integer(DestPixel), DestDelta);
        end;
      Inc(SourceLine);
      Inc(DestLine);
    end;

    // free the memory allocated for vertical filter weights
    for I := 0 to TargetHeight - 1 do
      ContributorList[I].Contributors := nil;
    // this one is done automatically on exit, but is here for completeness
    ContributorList := nil;

  finally
    Work.Free;
    CurrentLineR := nil;
    CurrentLineG := nil;
    CurrentLineB := nil;
    Target.Modified := True;
  end;
end;

// Filter functions for TJclBitmap32
type
  TPointRec = record
    Pos: Integer;
    Weight: Integer;
  end;
  TCluster = array of TPointRec;
  TMappingTable = array of TCluster;
  TFilterFunc = function(Value: Extended): Extended;

function NearestFilter(Value: Extended): Extended;
begin
  if (Value > -0.5) and (Value <= 0.5) then
    Result := 1
  else
    Result := 0;
end;

function LinearFilter(Value: Extended): Extended;
begin
  if Value < -1 then
    Result := 0
  else
  if Value < 0 then
    Result := 1 + Value
  else
  if Value < 1 then
    Result := 1 - Value
  else
    Result := 0;
end;

function SplineFilter(Value: Extended): Extended;
var
  tt: Extended;
begin
  Value := Abs(Value);
  if Value < 1 then
  begin
    tt := Sqr(Value);
    Result := 0.5 * tt * Value - tt + 2 / 3;
  end
  else
  if Value < 2 then
  begin
    Value := 2 - Value;
    Result := 1 / 6 * Sqr(Value) * Value;
  end
  else
    Result := 0;
end;

function BuildMappingTable(DstWidth, SrcFrom, SrcWidth: Integer;
  StretchFilter: TStretchFilter): TMappingTable;
const
  FILTERS: array [TStretchFilter] of TFilterFunc =
    (NearestFilter, LinearFilter, SplineFilter);
var
  Filter: TFilterFunc;
  FilterWidth: Extended;
  Scale, OldScale: Extended;
  Center: Extended;
  Bias: Extended;
  Left, Right: Integer;
  I, J, K: Integer;
  Weight: Integer;
begin
  if SrcWidth = 0 then
  begin
    Result := nil;
    Exit;
  end;
  Filter := FILTERS[StretchFilter];
  if StretchFilter in [sfNearest, sfLinear] then
    FilterWidth := 1
  else
    FilterWidth := 1.5;
  SetLength(Result, DstWidth);
  Scale := (DstWidth - 1) / (SrcWidth - 1);

  if Scale < 1 then
  begin
    OldScale := Scale;
    Scale := 1 / Scale;
    FilterWidth := FilterWidth * Scale;
    for I := 0 to DstWidth - 1 do
    begin
      Center := I * Scale;
      Left := Floor(Center - FilterWidth);
      Right := Ceil(Center + FilterWidth);
      Bias := 0;
      for J := Left to Right do
      begin
        Weight := Round(255 * Filter((Center - J) * OldScale) * OldScale);
        if Weight <> 0 then
        begin
          Bias := Bias + Weight / 255;
          K := Length(Result[I]);
          SetLength(Result[I], K + 1);
          Result[I][K].Pos := Constrain(J + SrcFrom, 0, SrcWidth - 1);
          Result[I][K].Weight := Weight;
        end;
      end;
      if (Bias > 0) and (Bias <> 1) then
      begin
        Bias := 1 / Bias;
        for K := 0 to High(Result[I]) do
          Result[I][K].Weight := Round(Result[I][K].Weight * Bias);
      end;
    end;
  end
  else
  begin
    FilterWidth := 1 / FilterWidth;
    Scale := 1 / Scale;
    for I := 0 to DstWidth - 1 do
    begin
      Center := I * Scale;
      Left := Floor(Center - FilterWidth);
      Right := Ceil(Center + FilterWidth);
      for J := Left to Right do
      begin
        Weight := Round(255 * Filter(Center - J));
        if Weight <> 0 then
        begin
          K := Length(Result[I]);
          SetLength(Result[I], K + 1);
          Result[I][K].Pos := Constrain(J + SrcFrom, 0, SrcWidth - 1);
          Result[I][K].Weight := Weight;
        end;
      end;
    end;
  end;
end;

// Bitmap Functions
// Scales the source graphic to the given size (NewWidth, NewHeight) and stores the Result in Target.
// Filter describes the filter function to be applied and Radius the size of the filter area.
// Is Radius = 0 then the recommended filter area will be used (see DefaultFilterRadius).

procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter;
  Radius: Single; Source: TGraphic; Target: TBitmap);
var
  Temp: TBitmap;
begin
  if Source.Empty then
    Exit;               // do nothing

  if Radius = 0 then
    Radius := DefaultFilterRadius[Filter];

  Temp := TBitmap.Create;
  try
    // To allow Source = Target, the following assignment needs to be done initially
    Temp.Assign(Source);
    Temp.PixelFormat := pf32bit;

    Target.FreeImage;
    Target.PixelFormat := pf32bit;
    Target.Width := NewWidth;
    Target.Height := NewHeight;

      DoStretch(FilterList[Filter], Radius, Temp, Target);
  finally
    Temp.Free;
  end;
end;

procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter;
  Radius: Single; Bitmap: TBitmap);
begin
  Stretch(NewWidth, NewHeight, Filter, Radius, Bitmap, Bitmap);
end;


{$IFDEF MSWINDOWS}
procedure DrawBitmap(DC: HDC; Bitmap: HBITMAP; X, Y, Width, Height: Integer);
var
  MemDC: HDC;
  OldBitmap: HBITMAP;
begin
  MemDC := CreateCompatibleDC(DC);
  OldBitmap := SelectObject(MemDC, Bitmap);
  BitBlt(DC, X, Y, Width, Height, MemDC, 0, 0, SRCCOPY);
  SelectObject(MemDC, OldBitmap);
  DeleteObject(MemDC);
end;
{$ENDIF MSWINDOWS}


{$IFDEF MSWINDOWS}
function ExtractIconCount(const FileName: string): Integer;
begin
  Result := ExtractIcon(HInstance, PChar(FileName), $FFFFFFFF);
end;

function BitmapToIcon(Bitmap: HBITMAP; cx, cy: Integer): HICON;
var
  ImgList: HIMAGELIST;
  I: Integer;
begin
  ImgList := ImageList_Create(cx, cy, ILC_COLOR, 1, 1);
  try
    I := ImageList_Add(ImgList, Bitmap, 0);
    Result := ImageList_GetIcon(ImgList, I, ILD_NORMAL);
  finally
    ImageList_Destroy(ImgList);
  end;
end;

function BitmapToIcon(Bitmap, Mask: HBITMAP; cx, cy: Integer): HICON;
var
  ImgList: HIMAGELIST;
  I: Integer;
begin
  ImgList := ImageList_Create(cx, cy, ILC_COLOR, 1, 1);
  try
    I := ImageList_Add(ImgList, Bitmap, Mask);
    Result := ImageList_GetIcon(ImgList, I, ILD_TRANSPARENT);
  finally
    ImageList_Destroy(ImgList);
  end;
end;

function IconToBitmap(Icon: HICON): HBITMAP;
var
  IconInfo: TIconInfo;
begin
  Result := 0;
  if GetIconInfo(Icon, IconInfo) then
  begin
    DeleteObject(IconInfo.hbmMask);
    Result := IconInfo.hbmColor;
  end;
end;
{$ENDIF MSWINDOWS}




{$IFDEF MSWINDOWS}
function FillGradient(DC: HDC; ARect: TRect; ColorCount: Integer;
  StartColor, EndColor: TColor; ADirection: TGradientDirection): Boolean;
var
  StartRGB: array [0..2] of Byte;
  RGBKoef: array [0..2] of Double;
  Brush: HBRUSH;
  AreaWidth, AreaHeight, I: Integer;
  ColorRect: TRect;
  RectOffset: Double;
begin
  RectOffset := 0;
  Result := False;
  if ColorCount < 1 then
    Exit;
  StartColor := ColorToRGB(StartColor);
  EndColor := ColorToRGB(EndColor);
  StartRGB[0] := GetRValue(StartColor);
  StartRGB[1] := GetGValue(StartColor);
  StartRGB[2] := GetBValue(StartColor);
  RGBKoef[0] := (GetRValue(EndColor) - StartRGB[0]) / ColorCount;
  RGBKoef[1] := (GetGValue(EndColor) - StartRGB[1]) / ColorCount;
  RGBKoef[2] := (GetBValue(EndColor) - StartRGB[2]) / ColorCount;
  AreaWidth := ARect.Right - ARect.Left;
  AreaHeight :=  ARect.Bottom - ARect.Top;
  case ADirection of
    gdHorizontal:
      RectOffset := AreaWidth / ColorCount;
    gdVertical:
      RectOffset := AreaHeight / ColorCount;
  end;
  for I := 0 to ColorCount - 1 do
  begin
    Brush := CreateSolidBrush(RGB(
      StartRGB[0] + Round((I + 1) * RGBKoef[0]),
      StartRGB[1] + Round((I + 1) * RGBKoef[1]),
      StartRGB[2] + Round((I + 1) * RGBKoef[2])));
    case ADirection of
      gdHorizontal:
        SetRect(ColorRect, Round(RectOffset * I), 0, Round(RectOffset * (I + 1)), AreaHeight);
      gdVertical:
        SetRect(ColorRect, 0, Round(RectOffset * I), AreaWidth, Round(RectOffset * (I + 1)));
    end;
    OffsetRect(ColorRect, ARect.Left, ARect.Top);
    FillRect(DC, ColorRect, Brush);
    DeleteObject(Brush);
  end;
  Result := True;
end;
{$ENDIF MSWINDOWS}



//=== Matrices ===============================================================

{ TODO -oWIMDC -cReplace : Insert JclMatrix support }
function _DET(a1, a2, b1, b2: Extended): Extended; overload;
begin
  Result := a1 * b2 - a2 * b1;
end;

function _DET(a1, a2, a3, b1, b2, b3, c1, c2, c3: Extended): Extended; overload;
begin
  Result :=
    a1 * (b2 * c3 - b3 * c2) -
    b1 * (a2 * c3 - a3 * c2) +
    c1 * (a2 * b3 - a3 * b2);
end;

procedure Adjoint(var M: TMatrix3d);
var
  a1, a2, a3: Extended;
  b1, b2, b3: Extended;
  c1, c2, c3: Extended;
begin
  a1 := M.A[0, 0];
  a2 := M.A[0, 1];
  a3 := M.A[0, 2];

  b1 := M.A[1, 0];
  b2 := M.A[1, 1];
  b3 := M.A[1, 2];

  c1 := M.A[2, 0];
  c2 := M.A[2, 1];
  c3 := M.A[2, 2];

  M.A[0, 0]:=  _DET(b2, b3, c2, c3);
  M.A[0, 1]:= -_DET(a2, a3, c2, c3);
  M.A[0, 2]:=  _DET(a2, a3, b2, b3);

  M.A[1, 0]:= -_DET(b1, b3, c1, c3);
  M.A[1, 1]:=  _DET(a1, a3, c1, c3);
  M.A[1, 2]:= -_DET(a1, a3, b1, b3);

  M.A[2, 0]:=  _DET(b1, b2, c1, c2);
  M.A[2, 1]:= -_DET(a1, a2, c1, c2);
  M.A[2, 2]:=  _DET(a1, a2, b1, b2);
end;

function Determinant(const M: TMatrix3d): Extended;
begin
  Result := _DET(
    M.A[0, 0], M.A[1, 0], M.A[2, 0],
    M.A[0, 1], M.A[1, 1], M.A[2, 1],
    M.A[0, 2], M.A[1, 2], M.A[2, 2]);
end;

procedure Scale(var M: TMatrix3d; Factor: Extended);
var
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 2 do
      M.A[I, J] := M.A[I, J] * Factor;
end;

procedure InvertMatrix(var M: TMatrix3d);
var
  Det: Extended;
begin
  Det := Determinant(M);
  if Abs(Det) < 1E-5 then
    M := IdentityMatrix
  else
  begin
    Adjoint(M);
    Scale(M, 1 / Det);
  end;
end;

function Mult(const M1, M2: TMatrix3d): TMatrix3d;
var
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 2 do
      Result.A[I, J] :=
        M1.A[0, J] * M2.A[I, 0] +
        M1.A[1, J] * M2.A[I, 1] +
        M1.A[2, J] * M2.A[I, 2];
end;

type
  TVector3d = array [0..2] of Extended;
  TVector3i = array [0..2] of Integer;

function VectorTransform(const M: TMatrix3d; const V: TVector3d): TVector3d;
begin
  Result[0] := M.A[0, 0] * V[0] + M.A[1, 0] * V[1] + M.A[2, 0] * V[2];
  Result[1] := M.A[0, 1] * V[0] + M.A[1, 1] * V[1] + M.A[2, 1] * V[2];
  Result[2] := M.A[0, 2] * V[0] + M.A[1, 2] * V[1] + M.A[2, 2] * V[2];
end;

//=== { TJclLinearTransformation } ===========================================

constructor TJclLinearTransformation.Create;
begin
  inherited Create;
  Clear;
end;

procedure TJclLinearTransformation.Clear;
begin
  FMatrix := IdentityMatrix;
end;

function TJclLinearTransformation.GetTransformedBounds(const Src: TRect): TRect;
var
  V1, V2, V3, V4: TVector3d;
begin
  V1[0] := Src.Left;
  V1[1] := Src.Top;
  V1[2] := 1;

  V2[0] := Src.Right - 1;
  V2[1] := V1[1];
  V2[2] := 1;

  V3[0] := V1[0];
  V3[1] := Src.Bottom - 1;
  V3[2] := 1;

  V4[0] := V2[0];
  V4[1] := V3[1];
  V4[2] := 1;

  V1 := VectorTransform(Matrix, V1);
  V2 := VectorTransform(Matrix, V2);
  V3 := VectorTransform(Matrix, V3);
  V4 := VectorTransform(Matrix, V4);

  Result.Left   := Round(Min(Min(V1[0], V2[0]), Min(V3[0], V4[0])) - 0.5);
  Result.Right  := Round(Max(Max(V1[0], V2[0]), Max(V3[0], V4[0])) + 0.5);
  Result.Top    := Round(Min(Min(V1[1], V2[1]), Min(V3[1], V4[1])) - 0.5);
  Result.Bottom := Round(Max(Max(V1[1], V2[1]), Max(V3[1], V4[1])) + 0.5);
end;

procedure TJclLinearTransformation.PrepareTransform;
var
  M: TMatrix3d;
begin
  M := Matrix;
  InvertMatrix(M);

  // calculate a fixed point (4096) factors
  A := Round(M.A[0, 0] * 4096);
  B := Round(M.A[1, 0] * 4096);
  C := Round(M.A[2, 0] * 4096);
  D := Round(M.A[0, 1] * 4096);
  E := Round(M.A[1, 1] * 4096);
  F := Round(M.A[2, 1] * 4096);
end;

procedure TJclLinearTransformation.Rotate(Cx, Cy, Alpha: Extended);
var
  S, C: Extended;
  M: TMatrix3d;
begin
  if (Cx <> 0) and (Cy <> 0) then
    Translate(-Cx, -Cy);
  SinCos(DegToRad(Alpha), S, C);
  M := IdentityMatrix;
  M.A[0, 0] := C;
  M.A[1, 0] := S;
  M.A[0, 1] := -S;
  M.A[1, 1] := C;
  FMatrix := Mult(M, FMatrix);
  if (Cx <> 0) and (Cy <> 0) then
    Translate(Cx, Cy);
end;

procedure TJclLinearTransformation.Scale(Sx, Sy: Extended);
var
  M: TMatrix3d;
begin
  M := IdentityMatrix;
  M.A[0, 0] := Sx;
  M.A[1, 1] := Sy;
  FMatrix := Mult(M, FMatrix);
end;

procedure TJclLinearTransformation.Skew(Fx, Fy: Extended);
var
  M: TMatrix3d;
begin
  M := IdentityMatrix;
  M.A[1, 0] := Fx;
  M.A[0, 1] := Fy;
  FMatrix := Mult(M, FMatrix);
end;

procedure TJclLinearTransformation.Transform(DstX, DstY: Integer;
  out SrcX, SrcY: Integer);
begin
  SrcX := Sar(DstX * A + DstY * B + C, 12);
  SrcY := Sar(DstX * D + DstY * E + F, 12);
end;

procedure TJclLinearTransformation.Transform256(DstX, DstY: Integer;
  out SrcX256, SrcY256: Integer);
begin
  SrcX256 := Sar(DstX * A + DstY * B + C, 4);
  SrcY256 := Sar(DstX * D + DstY * E + F, 4);
end;

procedure TJclLinearTransformation.Translate(Dx, Dy: Extended);
var
  M: TMatrix3d;
begin
  M := IdentityMatrix;
  M.A[2, 0] := Dx;
  M.A[2, 1] := Dy;
  FMatrix := Mult(M, FMatrix);
end;

//=== PolyLines and Polygons =================================================


procedure QSortLine(const ALine: TScanLine; L, R: Integer);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := ALine[(L + R) shr 1];
    repeat
      while ALine[I] < P do
        Inc(I);
      while ALine[J] > P do
        Dec(J);
      if I <= J then
      begin
        SwapOrd(ALine[I], ALine[J]);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QSortLine(ALine, L, J);
    L := I;
  until I >= R;
end;

procedure SortLine(const ALine: TScanLine);
var
  L: Integer;
begin
  L := Length(ALine);
  Assert(not Odd(L));
  if L = 2 then
    TestSwap(ALine[0], ALine[1])
  else
  if L > 2 then
    QSortLine(ALine, 0, L - 1);
end;

procedure SortLines(const ScanLines: TScanLines);
var
  I: Integer;
begin
  for I := 0 to High(ScanLines) do
    SortLine(ScanLines[I]);
end;

procedure AddPolygon(const Points: TDynPointArray; BaseY: Integer;
  MaxX, MaxY: Integer; var ScanLines: TScanLines; SubSampleX: Boolean);
var
  I, X1, Y1, X2, Y2: Integer;
  Direction, PrevDirection: Integer; // up = 1 or down = -1

  procedure AddEdgePoint(X, Y: Integer);
  var
    L: Integer;
  begin
    if (Y < 0) or (Y > MaxY) then
      Exit;
    X := Constrain(X, 0, MaxX);
    L := Length(ScanLines[Y - BaseY]);
    SetLength(ScanLines[Y - BaseY], L + 1);
    ScanLines[Y - BaseY][L] := X;
  end;

  procedure DrawEdge(X1, Y1, X2, Y2: Integer);
  var
    X, Y, I: Integer;
    Dx, Dy, Sx, Sy: Integer;
    Delta: Integer;
  begin
    // this function 'renders' a line into the edge (ScanLines) buffer
    if Y2 = Y1 then
      Exit;

    Dx := X2 - X1;
    Dy := Y2 - Y1;

    if Dy > 0 then
      Sy := 1
    else
    begin
      Sy := -1;
      Dy := -Dy;
    end;
    if Dx > 0 then
      Sx := 1
    else
    begin
      Sx := -1;
      Dx := -Dx;
    end;
    Delta := (Dx mod Dy) shr 1;
    X := X1;
    Y := Y1;
    for I := 0 to Dy - 1 do
    begin
      AddEdgePoint(X, Y);
      Inc(Y, Sy);
      Inc(Delta, Dx);
      while Delta > Dy do
      begin
        Inc(X, Sx);
        Dec(Delta, Dy);
      end;
    end;
  end;

begin
  X1 := Points[0].X;
  Y1 := Points[0].Y;
  if SubSampleX then
    X1 := X1 shl 8;

  // find the last Y different from Y1 and assign it to Y0
  PrevDirection := 0;
  for I := High(Points) downto 1 do
  begin
    if Points[I].Y > Y1 then
      PrevDirection := -1
    else
    if Points[I].Y < Y1 then
      PrevDirection := 1
    else
      Continue;
    Break;
  end;
  Assert(PrevDirection <> 0);

  for I := 1 to High(Points) do
  begin
    X2 := Points[I].X;
    Y2 := Points[I].Y;
    if SubSampleX then
      X2 := X2 shl 8;
    if Y1 <> Y2 then
    begin
      DrawEdge(X1, Y1, X2, Y2);
      if Y2 > Y1 then
        Direction := 1 // up
      else
        Direction := -1; // down
      if Direction <> PrevDirection then
      begin
        AddEdgePoint(X1, Y1);
        PrevDirection := Direction;
      end;
    end;
    X1 := X2;
    Y1 := Y2;
  end;
  X2 := Points[0].X;
  Y2 := Points[0].Y;
  if SubSampleX then
    X2 := X2 shl 8;
  if Y1 <> Y2 then
  begin
    DrawEdge(X1, Y1, X2, Y2);
    if Y2 > Y1 then
      Direction := 1
    else
      Direction := -1;
    if Direction <> PrevDirection then
      AddEdgePoint(X1, Y1);
  end;
end;

// Gamma table support for opacities
procedure SetGamma(Gamma: Single);
var
  I: Integer;
begin
  for I := Low(GAMMA_TABLE) to High(GAMMA_TABLE) do
    GAMMA_TABLE[I] := Round(255 * Power(I / 255, Gamma));
end;

//  modify Jan 28, 2001 for use under BCB5
//         the compiler show error 245 "language feature ist not available"
//         we must take a record and under this we can use the static array

procedure SetIdentityMatrix;
begin
  IdentityMatrix.A[0, 0] := 1.0;
  IdentityMatrix.A[0, 1] := 0.0;
  IdentityMatrix.A[0, 2] := 0.0;
  IdentityMatrix.A[1, 0] := 0.0;
  IdentityMatrix.A[1, 1] := 1.0;
  IdentityMatrix.A[1, 2] := 0.0;
  IdentityMatrix.A[2, 0] := 0.0;
  IdentityMatrix.A[2, 1] := 0.0;
  IdentityMatrix.A[2, 2] := 1.0;
end;

initialization
  SetIdentityMatrix;
  SetGamma(0.7);
{$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
