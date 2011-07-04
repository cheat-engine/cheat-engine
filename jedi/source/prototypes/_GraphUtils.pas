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
{ The Original Code is JclGraphUtils.pas.                                                          }
{                                                                                                  }
{ The Initial Developers of the Original Code are Pelle F. S. Liljendal and Marcel van Brakel.     }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Jack N.A. Bakker                                                                               }
{   Mike Lischke                                                                                   }
{   Robert Marquardt (marquardt)                                                                   }
{   Alexander Radchenko                                                                            }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

{$IFNDEF PROTOTYPE}
{$IFDEF VCL}
unit JclGraphUtils;
{$ELSE VisualCLX}
unit JclQGraphUtils;
{$ENDIF VisualCLX}
{$ENDIF ~PROTOTYPE}

interface

{$I jcl.inc}

uses
  {$IFDEF HAS_UNIT_TYPES}
  Types,
  {$ENDIF HAS_UNIT_TYPES}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils,
  {$IFDEF VCL}
  Graphics,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Qt, QGraphics,
  {$ENDIF VisualCLX}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase;

type
  PColor32 = ^TColor32;
  TColor32 = type Longword;
  PColor32Array = ^TColor32Array;
  TColor32Array = array [0..MaxInt div SizeOf(TColor32) - 1] of TColor32;
  PPalette32 = ^TPalette32;
  TPalette32 = array [Byte] of TColor32;
  TArrayOfColor32 = array of TColor32;

  { Blending Function Prototypes }
  TCombineReg  = function(X, Y, W: TColor32): TColor32;
  TCombineMem  = procedure(F: TColor32; var B: TColor32; W: TColor32);
  TBlendReg    = function(F, B: TColor32): TColor32;
  TBlendMem    = procedure(F: TColor32; var B: TColor32);
  TBlendRegEx  = function(F, B, M: TColor32): TColor32;
  TBlendMemEx  = procedure(F: TColor32; var B: TColor32; M: TColor32);
  TBlendLine   = procedure(Src, Dst: PColor32; Count: Integer);
  TBlendLineEx = procedure(Src, Dst: PColor32; Count: Integer; M: TColor32);

  { Auxiliary structure to support TColor manipulation }
  TColorRec = packed record
    case Integer of
      0: (Value: Longint);
      1: (Red, Green, Blue: Byte);
      2: (R, G, B, Flag: Byte);
      {$IFDEF MSWINDOWS}
      3: (Index: Word); // GetSysColor, PaletteIndex
      {$ENDIF MSWINDOWS}
  end;

  TColorVector = record
    case Integer of
      0: (Coord: array [0..2] of Single);
      1: (R, G, B: Single);
      2: (H, L, S: Single);
  end;

  THLSValue = 0..240;
  THLSVector = record
    Hue: THLSValue;
    Luminance: THLSValue;
    Saturation: THLSValue;
  end;

  {$IFDEF VCL}
  TPointArray = array of TPoint;
  PPointArray = ^TPointArray;
  {$ENDIF VCL}

  { position codes for clipping algorithm }
  TClipCode = (ccLeft, ccRight, ccAbove, ccBelow);
  TClipCodes = set of TClipCode;
  PClipCodes = ^TClipCodes;

const
  { Some predefined color constants }
  clBlack32     = TColor32($FF000000);
  clDimGray32   = TColor32($FF3F3F3F);
  clGray32      = TColor32($FF7F7F7F);
  clLightGray32 = TColor32($FFBFBFBF);
  clWhite32     = TColor32($FFFFFFFF);
  clMaroon32    = TColor32($FF7F0000);
  clGreen32     = TColor32($FF007F00);
  clOlive32     = TColor32($FF7F7F00);
  clNavy32      = TColor32($FF00007F);
  clPurple32    = TColor32($FF7F007F);
  clTeal32      = TColor32($FF007F7F);
  clRed32       = TColor32($FFFF0000);
  clLime32      = TColor32($FF00FF00);
  clYellow32    = TColor32($FFFFFF00);
  clBlue32      = TColor32($FF0000FF);
  clFuchsia32   = TColor32($FFFF00FF);
  clAqua32      = TColor32($FF00FFFF);

  { Some semi-transparent color constants }
  clTrWhite32   = TColor32($7FFFFFFF);
  clTrBlack32   = TColor32($7F000000);
  clTrRed32     = TColor32($7FFF0000);
  clTrGreen32   = TColor32($7F00FF00);
  clTrBlue32    = TColor32($7F0000FF);

procedure EMMS;

// Dialog Functions
{$IFDEF MSWINDOWS}
function DialogUnitsToPixelsX(const DialogUnits: Word): Word;
function DialogUnitsToPixelsY(const DialogUnits: Word): Word;
function PixelsToDialogUnitsX(const PixelUnits: Word): Word;
function PixelsToDialogUnitsY(const PixelUnits: Word): Word;
{$ENDIF MSWINDOWS}

// Points
function NullPoint: TPoint;

function PointAssign(const X, Y: Integer): TPoint;
procedure PointCopy(var Dest: TPoint; const Source: TPoint);
function PointEqual(const P1, P2: TPoint): Boolean;
function PointIsNull(const P: TPoint): Boolean;
procedure PointMove(var P: TPoint; const DeltaX, DeltaY: Integer);

// Rectangles
function NullRect: TRect;

function RectAssign(const Left, Top, Right, Bottom: Integer): TRect;
function RectAssignPoints(const TopLeft, BottomRight: TPoint): TRect;
function RectBounds(const Left, Top, Width, Height: Integer): TRect;
function RectCenter(const R: TRect): TPoint;
procedure RectCopy(var Dest: TRect; const Source: TRect);
procedure RectFitToScreen(var R: TRect);  { TODO -cHelp : Doc }
procedure RectGrow(var R: TRect; const Delta: Integer);
procedure RectGrowX(var R: TRect; const Delta: Integer);
procedure RectGrowY(var R: TRect; const Delta: Integer);
function RectEqual(const R1, R2: TRect): Boolean;
function RectHeight(const R: TRect): Integer;
function RectIncludesPoint(const R: TRect; const Pt: TPoint): Boolean;
function RectIncludesRect(const R1, R2: TRect): Boolean;
function RectIntersection(const R1, R2: TRect): TRect;
function RectIntersectRect(const R1, R2: TRect): Boolean;
function RectIsEmpty(const R: TRect): Boolean;
function RectIsNull(const R: TRect): Boolean;
function RectIsSquare(const R: TRect): Boolean;
function RectIsValid(const R: TRect): Boolean;
procedure RectMove(var R: TRect; const DeltaX, DeltaY: Integer);
procedure RectMoveTo(var R: TRect; const X, Y: Integer);
procedure RectNormalize(var R: TRect);
function RectsAreValid(R: array of TRect): Boolean;
function RectUnion(const R1, R2: TRect): TRect;
function RectWidth(const R: TRect): Integer;

// Clipping
function ClipCodes(const X, Y, MinX, MinY, MaxX, MaxY: Float): TClipCodes; overload;
function ClipCodes(const X, Y: Float; const ClipRect: TRect): TClipCodes; overload;
function ClipLine(var X1, Y1, X2, Y2: Integer; const ClipRect: TRect): Boolean; overload;
function ClipLine(var X1, Y1, X2, Y2: Float; const MinX, MinY, MaxX, MaxY: Float;
  Codes: PClipCodes = nil): Boolean; overload;
procedure DrawPolyLine(const Canvas: TCanvas; var Points: TPointArray; const ClipRect: TRect);

// Color
type
  EColorConversionError = class(EJclError);

procedure GetRGBValue(const Color: TColor; out Red, Green, Blue: Byte);
function SetRGBValue(const Red, Green, Blue: Byte): TColor;
function GetColorBlue(const Color: TColor): Byte;
function GetColorFlag(const Color: TColor): Byte;
function GetColorGreen(const Color: TColor): Byte;
function GetColorRed(const Color: TColor): Byte;
function SetColorBlue(const Color: TColor; const Blue: Byte): TColor;
function SetColorFlag(const Color: TColor; const Flag: Byte): TColor;
function SetColorGreen(const Color: TColor; const Green: Byte): TColor;
function SetColorRed(const Color: TColor; const Red: Byte): TColor;

function BrightColor(const Color: TColor; const Pct: Single): TColor;
function BrightColorChannel(const Channel: Byte; const Pct: Single): Byte;
function DarkColor(const Color: TColor; const Pct: Single): TColor;
function DarkColorChannel(const Channel: Byte; const Pct: Single): Byte;

procedure CIED65ToCIED50(var X, Y, Z: Extended);
procedure CMYKToBGR(const Source, Target: Pointer; const BitsPerSample: Byte; Count: Cardinal); overload;
procedure CMYKToBGR(const C, M, Y, K, Target: Pointer; const BitsPerSample: Byte; Count: Cardinal); overload;
procedure CIELABToBGR(const Source, Target: Pointer; const Count: Cardinal); overload;
procedure CIELABToBGR(LSource, aSource, bSource: PByte; const Target: Pointer; const Count: Cardinal); overload;
procedure RGBToBGR(const Source, Target: Pointer; const BitsPerSample: Byte; Count: Cardinal); overload;
procedure RGBToBGR(const R, G, B, Target: Pointer; const BitsPerSample: Byte; Count: Cardinal); overload;
procedure RGBAToBGRA(const Source, Target: Pointer; const BitsPerSample: Byte; Count: Cardinal);

procedure WinColorToOpenGLColor(const Color: TColor; out Red, Green, Blue: Float);
function OpenGLColorToWinColor(const Red, Green, Blue: Float): TColor;

function Color32(WinColor: TColor): TColor32; overload;
function Color32(const R, G, B: Byte; const A: Byte = $FF): TColor32; overload;
function Color32(const Index: Byte; const Palette: TPalette32): TColor32; overload;
function Gray32(const Intensity: Byte; const Alpha: Byte = $FF): TColor32;
function WinColor(const Color32: TColor32): TColor;

function RedComponent(const Color32: TColor32): Integer;
function GreenComponent(const Color32: TColor32): Integer;
function BlueComponent(const Color32: TColor32): Integer;
function AlphaComponent(const Color32: TColor32): Integer;

function Intensity(const R, G, B: Single): Single; overload;
function Intensity(const Color32: TColor32): Integer; overload;

function SetAlpha(const Color32: TColor32; NewAlpha: Integer): TColor32;

procedure HLSToRGB(const H, L, S: Single; out R, G, B: Single); overload;
function HLSToRGB(const HLS: TColorVector): TColorVector; overload;
function HLSToRGB(const Hue, Luminance, Saturation: THLSValue): TColorRef; overload;
procedure RGBToHLS(const R, G, B: Single; out H, L, S: Single); overload;
function RGBToHLS(const RGB: TColorVector): TColorVector; overload;
function RGBToHLS(const RGBColor: TColorRef): THLSVector; overload;

{$IFDEF KEEP_DEPRECATED}
// obsolete; use corresponding HLS aliases instead
procedure HSLToRGB(const H, S, L: Single; out R, G, B: Single); overload;
  {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
procedure RGBToHSL(const R, G, B: Single; out H, S, L: Single); overload;
  {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
{$ENDIF KEEP_DEPRECATED}

// keep HSL identifier to avoid ambiguity with HLS overload
function HSLToRGB(const H, S, L: Single): TColor32; overload;
procedure RGBToHSL(const RGB: TColor32; out H, S, L: Single); overload;

{$IFDEF VCL}
function SetBitmapColors(Bmp: TBitmap; const Colors: array of TColor; StartIndex: Integer): Integer;
{$ENDIF VCL}

// Misc
function ColorToHTML(const Color: TColor): string;

// Petr Vones
{$IFDEF VCL}
function DottedLineTo(const Canvas: TCanvas; const X, Y: Integer): Boolean; overload;
{$ENDIF VCL}
{$IFDEF MSWINDOWS}
function ShortenString(const DC: HDC; const S: WideString; const Width: Integer; const RTL: Boolean;
  EllipsisWidth: Integer = 0): WideString;
{$ENDIF MSWINDOWS}

var
  { Blending Function Variables }
  CombineReg: TCombineReg;
  CombineMem: TCombineMem;

  BlendReg: TBlendReg;
  BlendMem: TBlendMem;

  BlendRegEx: TBlendRegEx;
  BlendMemEx: TBlendMemEx;

  BlendLine: TBlendLine;
  BlendLineEx: TBlendLineEx;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/prototypes/_GraphUtils.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    {$IFDEF VCL}
    LogPath: 'JCL\source\vcl'
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    LogPath: 'JCL\source\visclx'
    {$ENDIF VisualCLX}
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF VCL}
  Classes, Consts,
  {$ENDIF VCL}
  Math,
  JclResources, JclSysInfo, JclLogic;

type
  // resampling support types
  TRGBInt = record
    R: Integer;
    G: Integer;
    B: Integer;
  end;

  PRGBWord = ^TRGBWord;
  TRGBWord = record
    R: Word;
    G: Word;
    B: Word;
  end;

  PRGBAWord = ^TRGBAWord;
  TRGBAWord = record
    R: Word;
    G: Word;
    B: Word;
    A: Word;
  end;

  PBGR = ^TBGR;
  TBGR = packed record
    B: Byte;
    G: Byte;
    R: Byte;
  end;

  PBGRA = ^TBGRA;
  TBGRA = packed record
    B: Byte;
    G: Byte;
    R: Byte;
    A: Byte;
  end;

  PRGB = ^TRGB;
  TRGB = packed record
    R: Byte;
    G: Byte;
    B: Byte;
  end;

  PRGBA = ^TRGBA;
  TRGBA = packed record
    R: Byte;
    G: Byte;
    B: Byte;
    A: Byte;
  end;

const
  { Component masks }
  _R   = TColor32($00FF0000);
  _G   = TColor32($0000FF00);
  _B   = TColor32($000000FF);
  _RGB = TColor32($00FFFFFF);
  Bias = $00800080;

var
  MMX_ACTIVE: Boolean;

{$IFDEF VCL}

procedure OutOfResources;
begin
  raise EOutOfResources.CreateRes(@SOutOfResources);
end;

procedure GDIError;
var
  ErrorCode: Integer;
  Buf: array [0..255] of Char;
begin
  ErrorCode := GetLastError;
  if (ErrorCode <> 0) and (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
    ErrorCode, LOCALE_USER_DEFAULT, Buf, SizeOf(Buf), nil) <> 0) then
    raise EOutOfResources.Create(Buf)
  else
    OutOfResources;
end;

function GDICheck(Value: Integer): Integer;
begin
  if Value = 0 then GDIError;
  Result := Value;
end;

{$ENDIF VCL}

//=== Internal LowLevel ======================================================

function ColorSwap(WinColor: TColor): TColor32;
// this function swaps R and B bytes in ABGR and writes $FF into A component
{asm
// EAX = WinColor
        MOV     ECX, EAX     // ECX = WinColor
        MOV     EDX, EAX     // EDX = WinColor

        AND     ECX, $FF0000 // B component
        AND     EAX, $0000FF // R component
        AND     EDX, $00FF00 // G component

        OR      EAX, $00FF00 // write $FF into A component
        SHR     ECX, 16      // shift B
        SHL     EAX, 16      // shift AR
        OR      ECX, EDX     // ECX = GB
        OR      EAX, ECX     // set GB
end;}
begin
  Result := $FF000000 or                        // A component
    TColor32((WinColor and $0000FF) shl  16) or // R component
    TColor32( WinColor and $00FF00) or          // G component
    TColor32((WinColor and $FF0000) shr 16);    // B component
end;

//=== Blending routines ======================================================

function _CombineReg(X, Y, W: TColor32): TColor32;
{asm
  // combine RGBA channels of colors X and Y with the weight of X given in W
  // Result Z = W * X + (1 - W) * Y (all channels are combined, including alpha)
  // EAX <- X
  // EDX <- Y
  // ECX <- W

  // W = 0 or $FF?
        JCXZ    @1              // CX = 0 ?  => Result := EDX
        CMP     ECX, $FF        // CX = $FF ?  => Result := EAX
        JE      @2

        PUSH    EBX

  // P = W * X
        MOV     EBX, EAX        // EBX  <-  Xa Xr Xg Xb
        AND     EAX, $00FF00FF  // EAX  <-  00 Xr 00 Xb
        AND     EBX, $FF00FF00  // EBX  <-  Xa 00 Xg 00
        IMUL    EAX, ECX        // EAX  <-  Pr ** Pb **
        SHR     EBX, 8          // EBX  <-  00 Xa 00 Xg
        IMUL    EBX, ECX        // EBX  <-  Pa ** Pg **
        ADD     EAX, Bias
        AND     EAX, $FF00FF00  // EAX  <-  Pr 00 Pb 00
        SHR     EAX, 8          // EAX  <-  00 Pr 00 Pb
        ADD     EBX, Bias
        AND     EBX, $FF00FF00  // EBX  <-  Pa 00 Pg 00
        OR      EAX, EBX        // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * Y
        XOR     ECX, $000000FF  // ECX  <-  1 - ECX
        MOV     EBX, EDX        // EBX  <-  Ya Yr Yg Yb
        AND     EDX, $00FF00FF  // EDX  <-  00 Yr 00 Yb
        AND     EBX, $FF00FF00  // EBX  <-  Ya 00 Yg 00
        IMUL    EDX, ECX        // EDX  <-  Qr ** Qb **
        SHR     EBX, 8          // EBX  <-  00 Ya 00 Yg
        IMUL    EBX, ECX        // EBX  <-  Qa ** Qg **
        ADD     EDX, Bias
        AND     EDX, $FF00FF00  // EDX  <-  Qr 00 Qb 00
        SHR     EDX, 8          // EDX  <-  00 Qr ** Qb
        ADD     EBX, Bias
        AND     EBX, $FF00FF00  // EBX  <-  Qa 00 Qg 00
        OR      EBX, EDX        // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX, EBX        // EAX  <-  Za Zr Zg Zb

        POP     EBX
        RET

@1:     MOV     EAX, EDX
@2:     RET
end;}
begin
  // combine RGBA channels of colors X and Y with the weight of X given in W
  // Result Z = W * X + (1 - W) * Y (all channels are combined, including alpha)

  if W = 0 then
    Result := Y        //May be if W <= 0 ???
  else
  if W = $FF then Result := X //May be if W >= $FF ??? Or if W > $FF ???
  else
  begin
    Result :=
      (((((X shr 8 {00Xa00Xg}) and $00FF00FF {00X100X2}) * W {P1**P2**}) +
        Bias) and $FF00FF00 {P100P200}) {Pa00Pg00} or
      (((((X {00Xr00Xb} and $00FF00FF {00X100X2}) * W {P1**P2**}) + Bias) and
        $FF00FF00 {P100P200}) shr 8 {00Pr00Pb}) {PaPrPgPb};

    W := W xor $FF; // W := 1 - W;
    //W := $100 - W; // May be so ???

    Result := Result {PaPrPgPb} + (
      (((((Y shr 8 {00Ya00Yg}) and $00FF00FF {00X100X2}) * W {P1**P2**}) +
        Bias) and $FF00FF00 {P100P200}) {Qa00Qg00} or
      (((((Y {00Yr00Yb} and $00FF00FF {00X100X2}) * W {P1**P2**}) + Bias) and
        $FF00FF00 {P100P200}) shr 8 {00Qr00Qb}) {QaQrQgQb}
      ) {ZaZrZgZb};
  end;
end;

procedure _CombineMem(F: TColor32; var B: TColor32; W: TColor32);
{asm
  // EAX <- F
  // [EDX] <- B
  // ECX <- W
        PUSH    EDX
        MOV     EDX, [EDX]
        CALL    _CombineReg
        POP     EDX
        MOV     [EDX], EAX
end;}
begin
  B := _CombineReg(F, B, W);
end;

function _BlendReg(F, B: TColor32): TColor32;
{asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // Result Z = Fa * Frgb + (1 - Fa) * Brgb
  // EAX <- F
  // EDX <- B
        MOV     ECX, EAX        // ECX  <-  Fa Fr Fg Fb
        SHR     ECX, 24         // ECX  <-  00 00 00 Fa
        JMP    _CombineReg
end;}
begin
  Result := _CombineReg(F, B, F shr 24);
end;

procedure _BlendMem(F: TColor32; var B: TColor32);
{asm
  // EAX <- F
  // [EDX] <- B
        PUSH    EDX
        MOV     ECX, EAX        // ECX  <-  Fa Fr Fg Fb
        SHR     ECX, 24         // ECX  <-  00 00 00 Fa
        MOV     EDX, [EDX]
        CALL    _CombineReg
        POP     EDX
        MOV     [EDX], EAX
end;}
begin
  B := _CombineReg(F, B, F shr 24);
end;

function _BlendRegEx(F, B, M: TColor32): TColor32;
{asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F multiplied by master alpha (M)
  // no checking for M = $FF, if this is the case Graphics32 uses BlendReg
  // Result Z = Fa * M * Frgb + (1 - Fa * M) * Brgb
  // EAX <- F
  // EDX <- B
  // ECX <- M
        MOV     EBX, EAX        // EBX  <-  Fa Fr Fg Fb
        SHR     EBX, 24         // EBX  <-  00 00 00 Fa
        IMUL    ECX, EBX        // ECX  <-  00 00  W **
        SHR     ECX, 8          // ECX  <-  00 00 00  W
        JMP    _CombineReg
end;}
begin
  Result := _CombineReg(F, B, ((F shr 24) * M) shr 8);
end;

procedure _BlendMemEx(F: TColor32; var B: TColor32; M: TColor32);
{asm
  // EAX <- F
  // [EDX] <- B
  // ECX <- M
        PUSH    EBX
        MOV     EBX, EAX        // EBX  <-  Fa Fr Fg Fb
        SHR     EBX, 24         // EBX  <-  00 00 00 Fa
        IMUL    ECX, EBX        // ECX  <-  00 00  W **
        SHR     ECX, 8          // ECX  <-  00 00 00  W

        MOV     EBX, EDX
        MOV     EDX, [EDX]
        CALL    _BlendRegEx
        MOV     [EBX], EAX
        POP     EBX
end;}
begin
  B := _CombineReg(F, B, ((F shr 24) * M) shr 8);
end;


procedure _BlendLine(Src, Dst: PColor32; Count: Integer); assembler;
asm
  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

  // test the counter for zero or negativity
        TEST    ECX, ECX
        JS      @4

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI, EAX        // ESI <- Src
        MOV     EDI, EDX        // EDI <- Dst

  // loop start
@1:     MOV     EAX, [ESI]
        TEST    EAX, $FF000000
        JZ      @3              // complete transparency, proceed to next point

        PUSH    ECX             // store counter

  // Get weight W = Fa * M
        MOV     ECX, EAX        // ECX  <-  Fa Fr Fg Fb
        SHR     ECX, 24         // ECX  <-  00 00 00 Fa

  // Test Fa = 255 ?
        CMP     ECX, $FF
        JZ      @2

  // P = W * F
        MOV     EBX, EAX         // EBX  <-  Fa Fr Fg Fb
        AND     EAX, $00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX, $FF00FF00   // EBX  <-  Fa 00 Fg 00
        IMUL    EAX, ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX, 8           // EBX  <-  00 Fa 00 Fg
        IMUL    EBX, ECX         // EBX  <-  Pa ** Pg **
        ADD     EAX, Bias
        AND     EAX, $FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX, 8           // EAX  <-  00 Pr ** Pb
        ADD     EBX, Bias
        AND     EBX, $FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX, EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * B
        MOV     EDX, [EDI]
        XOR     ECX, $000000FF   // ECX  <-  1 - ECX
        MOV     EBX, EDX         // EBX  <-  Ba Br Bg Bb
        AND     EDX, $00FF00FF   // ESI  <-  00 Br 00 Bb
        AND     EBX, $FF00FF00   // EBX  <-  Ba 00 Bg 00
        IMUL    EDX, ECX         // ESI  <-  Qr ** Qb **
        SHR     EBX, 8           // EBX  <-  00 Ba 00 Bg
        IMUL    EBX, ECX         // EBX  <-  Qa ** Qg **
        ADD     EDX, Bias
        AND     EDX, $FF00FF00   // ESI  <-  Qr 00 Qb 00
        SHR     EDX, 8           // ESI  <-  00 Qr ** Qb
        ADD     EBX, Bias
        AND     EBX, $FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX, EDX         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX, EBX        // EAX  <-  Za Zr Zg Zb
@2:     MOV     [EDI], EAX

        POP     ECX             // restore counter

@3:     ADD     ESI, 4
        ADD     EDI, 4

  // loop end
        DEC     ECX
        JNZ     @1

        POP     EDI
        POP     ESI
        POP     EBX

@4:     RET
end;

procedure _BlendLineEx(Src, Dst: PColor32; Count: Integer; M: TColor32);
begin
  while Count > 0 do
  begin
    _BlendMemEx(Src^, Dst^, M);
    Inc(Src);
    Inc(Dst);
    Dec(Count);
  end;
end;

{ MMX versions }

var
  AlphaTable: Pointer;
  bias_ptr: Pointer;
  alpha_ptr: Pointer;

procedure GenAlphaTable;
var
  I: Integer;
  L: Longword;
  P: ^Longword;
begin
  GetMem(AlphaTable, 257 * 8);
  alpha_ptr := Pointer(Integer(AlphaTable) and $FFFFFFF8);
  if Integer(alpha_ptr) < Integer(AlphaTable) then
    alpha_ptr := Pointer(Integer(alpha_ptr) + 8);
  P := alpha_ptr;
  for I := 0 to 255 do
  begin
    L := I + I shl 16;
    P^ := L;
    Inc(P);
    P^ := L;
    Inc(P);
  end;
  bias_ptr := Pointer(Integer(alpha_ptr) + $80 * 8);
end;

procedure FreeAlphaTable;
begin
  FreeMem(AlphaTable);
  AlphaTable := nil;
end;

procedure EMMS;
begin
  if MMX_ACTIVE then
  asm
          db      $0F, $77               // EMMS
  end;
end;

function M_CombineReg(X, Y, W: TColor32): TColor32; assembler;
asm
  // EAX - Color X
  // EDX - Color Y
  // ECX - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        db $0F, $EF, $C0           // PXOR      MM0, MM0
        db $0F, $6E, $C8           // MOVD      MM1, EAX
        SHL       ECX, 3
        db $0F, $6E, $D2           // MOVD      MM2, EDX
        db $0F, $60, $C8           // PUNPCKLBW MM1, MM0
        db $0F, $60, $D0           // PUNPCKLBW MM2, MM0
        ADD       ECX, alpha_ptr
        db $0F, $F9, $CA           // PSUBW     MM1, MM2
        db $0F, $D5, $09           // PMULLW    MM1, [ECX]
        db $0F, $71, $F2,$08       // PSLLW     MM2, 8
        MOV       ECX, bias_ptr
        db $0F, $FD, $11           // PADDW     MM2, [ECX]
        db $0F, $FD, $CA           // PADDW     MM1, MM2
        db $0F, $71, $D1, $08      // PSRLW     MM1, 8
        db $0F, $67, $C8           // PACKUSWB  MM1, MM0
        db $0F, $7E, $C8           // MOVD      EAX, MM1
end;

procedure M_CombineMem(F: TColor32; var B: TColor32; W: TColor32);
{asm
  // EAX - Color X
  // [EDX] - Color Y
  // ECX - Weight of X [0..255]
  // Result := W * (X - Y) + Y
        PUSH    EDX
        MOV     EDX, [EDX]
        CALL    M_CombineReg
        POP     EDX
        MOV     [EDX], EAX
end;}
begin
  B := M_CombineReg(F, B, W);
end;

function M_BlendReg(F, B: TColor32): TColor32; assembler;
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // EDX <- B
  // Result := Fa * (Frgb - Brgb) + Brgb
        db $0F, $EF, $DB           // PXOR      MM3, MM3
        db $0F, $6E, $C0           // MOVD      MM0, EAX
        db $0F, $6E, $D2           // MOVD      MM2, EDX
        db $0F, $60, $C3           // PUNPCKLBW MM0, MM3
        MOV     ECX, bias_ptr
        db $0F, $60, $D3           // PUNPCKLBW MM2, MM3
        db $0F, $6F, $C8           // MOVQ      MM1, MM0
        db $0F, $69, $C9           // PUNPCKHWD MM1, MM1
        db $0F, $F9, $C2           // PSUBW     MM0, MM2
        db $0F, $6A, $C9           // PUNPCKHDQ MM1, MM1
        db $0F, $71, $F2, $08      // PSLLW     MM2, 8
        db $0F, $D5, $C1           // PMULLW    MM0, MM1
        db $0F, $FD, $11           // PADDW     MM2, [ECX]
        db $0F, $FD, $D0           // PADDW     MM2, MM0
        db $0F, $71, $D2, $08      // PSRLW     MM2, 8
        db $0F, $67, $D3           // PACKUSWB  MM2, MM3
        db $0F, $7E, $D0           // MOVD      EAX, MM2
end;

procedure M_BlendMem(F: TColor32; var B: TColor32);
{asm
  // EAX - Color X
  // [EDX] - Color Y
  // Result := W * (X - Y) + Y
        PUSH    EDX
        MOV     EDX, [EDX]
        CALL    M_BlendReg
        POP     EDX
        MOV     [EDX], EAX
end;}
begin
  B := M_BlendReg(F, B);
end;

function M_BlendRegEx(F, B, M: TColor32): TColor32; assembler;
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // EDX <- B
  // ECX <- M
  // Result := M * Fa * (Frgb - Brgb) + Brgb
        PUSH      EBX
        MOV       EBX, EAX
        SHR       EBX, 24
        IMUL      ECX, EBX
        SHR       ECX, 8
        JZ        @1

        db $0F, $EF, $C0           // PXOR      MM0, MM0
        db $0F, $6E, $C8           // MOVD      MM1, EAX
        SHL       ECX, 3
        db $0F, $6E, $D2           // MOVD      MM2, EDX
        db $0F, $60, $C8           // PUNPCKLBW MM1, MM0
        db $0F, $60, $D0           // PUNPCKLBW MM2, MM0
        ADD       ECX, alpha_ptr
        db $0F, $F9, $CA           // PSUBW     MM1, MM2
        db $0F, $D5, $09           // PMULLW    MM1, [ECX]
        db $0F, $71, $F2, $08      // PSLLW     MM2, 8
        MOV       ECX, bias_ptr
        db $0F, $FD, $11           // PADDW     MM2, [ECX]
        db $0F, $FD, $CA           // PADDW     MM1, MM2
        db $0F, $71, $D1, $08      // PSRLW     MM1, 8
        db $0F, $67, $C8           // PACKUSWB  MM1, MM0
        db $0F, $7E, $C8           // MOVD      EAX, MM1

@1:     MOV       EAX, EDX
        POP       EBX
end;

procedure M_BlendMemEx(F: TColor32; var B: TColor32; M: TColor32);
{asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // [EDX] <- B
  // ECX <- M
  // Result := M * Fa * (Frgb - Brgb) + Brgb
        PUSH    EDX
        MOV     EDX, [EDX]
        CALL    M_BlendRegEx
        POP     EDX
        MOV     [EDX], EAX
end;}
begin
  B := M_BlendRegEx(F, B, M);
end;

procedure M_BlendLine(Src, Dst: PColor32; Count: Integer); assembler;
asm
  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

  // test the counter for zero or negativity
        TEST      ECX, ECX
        JS        @4

        PUSH      ESI
        PUSH      EDI

        MOV       ESI, EAX        // ESI <- Src
        MOV       EDI, EDX        // EDI <- Dst

  // loop start
@1:     MOV       EAX, [ESI]
        TEST      EAX, $FF000000
        JZ        @3              // complete transparency, proceed to next point
        CMP       EAX, $FF000000
        JNC       @2              // opaque pixel, copy without blending

  // blend
        db $0F, $EF, $DB           // PXOR      MM3, MM3
        db $0F, $6E, $C0           // MOVD      MM0, EAX
        db $0F, $6E, $17           // MOVD      MM2, [EDI]
        db $0F, $60, $C3           // PUNPCKLBW MM0, MM3
        MOV       EAX, bias_ptr
        db $0F, $60, $D3           // PUNPCKLBW MM2, MM3
        db $0F, $6F, $C8           // MOVQ      MM1, MM0
        db $0F, $69, $C9           // PUNPCKHWD MM1, MM1
        db $0F, $F9, $C2           // PSUBW     MM0, MM2
        db $0F, $6A, $C9           // PUNPCKHDQ MM1, MM1
        db $0F, $71, $F2, $08      // PSLLW     MM2, 8
        db $0F, $D5, $C1           // PMULLW    MM0, MM1
        db $0F, $FD, $10           // PADDW     MM2, [EAX]
        db $0F, $FD, $D0           // PADDW     MM2, MM0
        db $0F, $71, $D2, $08      // PSRLW     MM2, 8
        db $0F, $67, $D3           // PACKUSWB  MM2, MM3
        db $0F, $7E, $D0           // MOVD      EAX, MM2

@2:     MOV       [EDI], EAX

@3:     ADD       ESI, 4
        ADD       EDI, 4

  // loop end
        DEC       ECX
        JNZ       @1

        POP       EDI
        POP       ESI

@4:     RET
end;

procedure M_BlendLineEx(Src, Dst: PColor32; Count: Integer; M: TColor32); assembler;
asm
  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

  // test the counter for zero or negativity
        TEST      ECX, ECX
        JS        @4

        PUSH      ESI
        PUSH      EDI
        PUSH      EBX

        MOV       ESI, EAX        // ESI <- Src
        MOV       EDI, EDX        // EDI <- Dst
        MOV       EDX, M          // EDX <- Master Alpha

  // loop start
@1:     MOV       EAX, [ESI]
        TEST      EAX, $FF000000
        JZ        @3              // complete transparency, proceed to next point
        MOV       EBX, EAX
        SHR       EBX, 24
        IMUL      EBX, EDX
        SHR       EBX, 8
        JZ        @3              // complete transparency, proceed to next point

  // blend
        db $0F, $EF, $C0           // PXOR      MM0, MM0
        db $0F, $6E, $C8           // MOVD      MM1, EAX
        SHL       EBX, 3
        db $0F, $6E, $17           // MOVD      MM2, [EDI]
        db $0F, $60, $C8           // PUNPCKLBW MM1, MM0
        db $0F, $60, $D0           // PUNPCKLBW MM2, MM0
        ADD       EBX, alpha_ptr
        db $0F, $F9, $CA           // PSUBW     MM1, MM2
        db $0F, $D5, $0B           // PMULLW    MM1, [EBX]
        db $0F, $71, $F2, $08      // PSLLW     MM2, 8
        MOV       EBX, bias_ptr
        db $0F, $FD, $13           // PADDW     MM2, [EBX]
        db $0F, $FD, $CA           // PADDW     MM1, MM2
        db $0F, $71, $D1, $08      // PSRLW     MM1, 8
        db $0F, $67, $C8           // PACKUSWB  MM1, MM0
        db $0F, $7E, $C8           // MOVD      EAX, MM1

@2:     MOV       [EDI], EAX

@3:     ADD       ESI, 4
        ADD       EDI, 4

  // loop end
        DEC       ECX
        JNZ       @1

        POP       EBX
        POP       EDI
        POP       ESI
@4:
end;

{ MMX Detection and linking }

procedure SetupFunctions;
var
  CpuInfo: TCpuInfo;
begin
  //WIMDC
  CpuInfo := CPUID;
  MMX_ACTIVE := (CpuInfo.Features and MMX_FLAG) = MMX_FLAG;
  if MMX_ACTIVE then
  begin
    // link MMX functions
    CombineReg := M_CombineReg;
    CombineMem := M_CombineMem;
    BlendReg := M_BlendReg;
    BlendMem := M_BlendMem;
    BlendRegEx := M_BlendRegEx;
    BlendMemEx := M_BlendMemEx;
    BlendLine := M_BlendLine;
    BlendLineEx := M_BlendLineEx;
  end
  else
  begin
    // link non-MMX functions
    CombineReg := _CombineReg;
    CombineMem := _CombineMem;
    BlendReg := _BlendReg;
    BlendMem := _BlendMem;
    BlendRegEx := _BlendRegEx;
    BlendMemEx := _BlendMemEx;
    BlendLine := _BlendLine;
    BlendLineEx := _BlendLineEx;
  end;
end;

//=== Dialog functions =======================================================

{$IFDEF MSWINDOWS}
function DialogUnitsToPixelsX(const DialogUnits: Word): Word;
begin
  Result := (DialogUnits * LoWord(GetDialogBaseUnits)) div 4;
end;

function DialogUnitsToPixelsY(const DialogUnits: Word): Word;
begin
  Result := (DialogUnits * HiWord(GetDialogBaseUnits)) div 8;
end;

function PixelsToDialogUnitsX(const PixelUnits: Word): Word;
begin
  Result := PixelUnits * 4 div LoWord(GetDialogBaseUnits);
end;

function PixelsToDialogUnitsY(const PixelUnits: Word): Word;
begin
  Result := PixelUnits * 8 div HiWord(GetDialogBaseUnits);
end;
{$ENDIF MSWINDOWS}

//=== Points =================================================================

function NullPoint: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

function PointAssign(const X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

procedure PointCopy(var Dest: TPoint; const Source: TPoint);
begin
  Dest.X := Source.X;
  Dest.Y := Source.Y;
end;

function PointEqual(const P1, P2: TPoint): Boolean;
begin
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);
end;

function PointIsNull(const P: TPoint): Boolean;
begin
  Result := (P.X = 0) and (P.Y = 0);
end;

procedure PointMove(var P: TPoint; const DeltaX, DeltaY: Integer);
begin
  P.X := P.X + DeltaX;
  P.Y := P.Y + DeltaY;
end;

//=== Rectangles =============================================================

function NullRect: TRect;
begin
  with Result do
  begin
    Top := 0;
    Left := 0;
    Bottom := 0;
    Right := 0;
  end;
end;

function RectAssign(const Left, Top, Right, Bottom: Integer): TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function RectAssignPoints(const TopLeft, BottomRight: TPoint): TRect;
begin
  Result.TopLeft := TopLeft;
  Result.BottomRight := BottomRight;
end;

function RectBounds(const Left, Top, Width, Height: Integer): TRect;
begin
  Result := RectAssign(Left, Top, Left + Width, Top + Height);
end;

function RectCenter(const R: TRect): TPoint;
begin
  Result.X := R.Left + (RectWidth(R) div 2);
  Result.Y := R.Top + (RectHeight(R) div 2);
end;

procedure RectCopy(var Dest: TRect; const Source: TRect);
begin
  Dest := Source;
end;

procedure RectFitToScreen(var R: TRect);
var
  X, Y: Integer;
  Delta: Integer;
begin
  {$IFDEF MSWINDOWS}
  X := GetSystemMetrics(SM_CXSCREEN);
  Y := GetSystemMetrics(SM_CYSCREEN);
  {$ELSE ~MSWINDOWS}
  {$IFDEF VisualCLX}
  { TODO : Find a Qt-independent solution }
  X := QWidget_width(QApplication_desktop);
  Y := QWidget_height(QApplication_desktop);
  {$ENDIF VisualCLX}
  {$ENDIF ~MSWINDOWS}
  with R do
  begin
    if Right > X then
    begin
      Delta := Right - Left;
      Right := X;
      Left := Right - Delta;
    end;
    if Left < 0 then
    begin
      Delta := Right - Left;
      Left := 0;
      Right := Left + Delta;
    end;
    if Bottom > Y then
    begin
      Delta := Bottom - Top;
      Bottom := Y;
      Top := Bottom - Delta;
    end;
    if Top < 0 then
    begin
      Delta := Bottom - Top;
      Top := 0;
      Bottom := Top + Delta;
    end;
  end;
end;

procedure RectGrow(var R: TRect; const Delta: Integer);
begin
  with R do
  begin
    Dec(Left, Delta);
    Dec(Top, Delta);
    Inc(Right, Delta);
    Inc(Bottom, Delta);
  end;
end;

procedure RectGrowX(var R: TRect; const Delta: Integer);
begin
  with R do
  begin
    Dec(Left, Delta);
    Inc(Right, Delta);
  end;
end;

procedure RectGrowY(var R: TRect; const Delta: Integer);
begin
  with R do
  begin
    Dec(Top, Delta);
    Inc(Bottom, Delta);
  end;
end;

function RectEqual(const R1, R2: TRect): Boolean;
begin
  Result := (R1.Left = R2.Left) and (R1.Top = R2.Top) and
    (R1.Right = R2.Right) and (R1.Bottom = R2.Bottom);
end;

function RectHeight(const R: TRect): Integer;
begin
  Result := Abs(R.Bottom - R.Top);
end;

function RectIncludesPoint(const R: TRect; const Pt: TPoint): Boolean;
begin
  Result := (Pt.X > R.Left) and (Pt.X < R.Right) and
    (Pt.Y > R.Top) and (Pt.Y < R.Bottom);
end;

function RectIncludesRect(const R1, R2: TRect): Boolean;
begin
  Result := (R1.Left >= R2.Left) and (R1.Top >= R2.Top) and
    (R1.Right <= R2.Right) and (R1.Bottom <= R2.Bottom);
end;

function RectIntersection(const R1, R2: TRect): TRect;
begin
  with Result do
  begin
    Left := JclLogic.Max(R1.Left, R2.Left);
    Top := JclLogic.Max(R1.Top, R2.Top);
    Right := JclLogic.Min(R1.Right, R2.Right);
    Bottom := JclLogic.Min(R1.Bottom, R2.Bottom);
  end;
  if not RectIsValid(Result) then
    Result := NullRect;
end;

function RectIntersectRect(const R1, R2: TRect): Boolean;
begin
  Result := not RectIsNull(RectIntersection(R1, R2));
end;

function RectIsEmpty(const R: TRect): Boolean;
begin
  Result := (R.Right = R.Left) and (R.Bottom = R.Top);
end;

function RectIsNull(const R: TRect): Boolean;
begin
  with R do
    Result := (Left = 0) and (Right = 0) and (Top = 0) and (Bottom = 0);
end;

function RectIsSquare(const R: TRect): Boolean;
begin
  Result := (RectHeight(R) = RectWidth(R));
end;

function RectIsValid(const R: TRect): Boolean;
begin
  with R do
    Result := (Left <= Right) and (Top <= Bottom);
end;

procedure RectMove(var R: TRect; const DeltaX, DeltaY: Integer);
begin
  with R do
  begin
    Inc(Left, DeltaX);
    Inc(Right, DeltaX);
    Inc(Top, DeltaY);
    Inc(Bottom, DeltaY);
  end;
end;

procedure RectMoveTo(var R: TRect; const X, Y: Integer);
begin
  with R do
  begin
    Right := (Right - Left) + X;
    Bottom := (Bottom - Top) + Y;
    Left := X;
    Top := Y;
  end;
end;

procedure RectNormalize(var R: TRect);
var
  Temp: Integer;
begin
  if R.Left > R.Right then
  begin
    Temp := R.Left;
    R.Left := R.Right;
    R.Right := Temp;
  end;
  if R.Top > R.Bottom then
  begin
    Temp := R.Top;
    R.Top := R.Bottom;
    R.Bottom := Temp;
  end;
end;

function RectsAreValid(R: array of TRect): Boolean;
var
  I: Integer;
begin
  if Length(R) = 0 then
  begin
    Result := False;
    Exit;
  end;
  for I := Low(R) to High(R) do
  begin
    with R[I] do
      Result := (Left <= Right) and (Top <= Bottom);
    if not Result then
      Exit;
  end;
  Result := True;
end;

function RectUnion(const R1, R2: TRect): TRect;
begin
  with Result do
  begin
    Left := JclLogic.Min(R1.Left, R2.Left);
    Top := JclLogic.Min(R1.Top, R2.Top);
    Right := JclLogic.Max(R1.Right, R2.Right);
    Bottom := JclLogic.Max(R1.Bottom, R2.Bottom);
  end;
  if not RectIsValid(Result) then
    Result := NullRect;
end;

function RectWidth(const R: TRect): Integer;
begin
  Result := Abs(R.Right - R.Left);
end;

//=== Color ==================================================================

const
  MaxBytePercent = High(Byte) * 0.01;

procedure GetRGBValue(const Color: TColor; out Red, Green, Blue: Byte);
var
  Temp: TColorRec;
begin
  Temp.Value := ColorToRGB(Color);
  Red := Temp.R;
  Green := Temp.G;
  Blue := Temp.B;
end;

function SetRGBValue(const Red, Green, Blue: Byte): TColor;
begin
  TColorRec(Result).Red := Red;
  TColorRec(Result).Green := Green;
  TColorRec(Result).Blue := Blue;
  TColorRec(Result).Flag := 0;
end;

function SetColorFlag(const Color: TColor; const Flag: Byte): TColor;
begin
  Result := Color;
  TColorRec(Result).Flag := Flag;
end;

function GetColorFlag(const Color: TColor): Byte;
begin
  Result := TColorRec(Color).Flag;
end;

function SetColorRed(const Color: TColor; const Red: Byte): TColor;
begin
  Result := ColorToRGB(Color);
  TColorRec(Result).Red := Red;
end;

function GetColorRed(const Color: TColor): Byte;
var
  Temp: TColorRec;
begin
  Temp.Value := ColorToRGB(Color);
  Result := Temp.Red;
end;

function SetColorGreen(const Color: TColor; const Green: Byte): TColor;
begin
  Result := ColorToRGB(Color);
  TColorRec(Result).Green := Green;
end;

function GetColorGreen(const Color: TColor): Byte;
var
  Temp: TColorRec;
begin
  Temp.Value := ColorToRGB(Color);
  Result := Temp.Green;
end;

function SetColorBlue(const Color: TColor; const Blue: Byte): TColor;
begin
  Result := ColorToRGB(Color);
  TColorRec(Result).Blue := Blue;
end;

function GetColorBlue(const Color: TColor): Byte;
var
  Temp: TColorRec;
begin
  Temp.Value := ColorToRGB(Color);
  Result := Temp.Blue;
end;

function BrightColor(const Color: TColor; const Pct: Single): TColor;
var
  Temp: TColorRec;
begin
  Temp.Value := ColorToRGB(Color);
  Temp.R := BrightColorChannel(Temp.R, Pct);
  Temp.G := BrightColorChannel(Temp.G, Pct);
  Temp.B := BrightColorChannel(Temp.B, Pct);
  Result := Temp.Value;
end;

function BrightColorChannel(const Channel: Byte; const Pct: Single): Byte;
var
  Temp: Integer;
begin
  if Pct < 0 then
    Result := DarkColorChannel(Channel, -Pct)
  else
  begin
    Temp := Round(Channel + Pct * MaxBytePercent);
    if Temp > High(Result) then
      Result := High(Result)
    else
      Result := Temp;
  end;
end;

function DarkColor(const Color: TColor; const Pct: Single): TColor;
var
  Temp: TColorRec;
begin
  Temp.Value := ColorToRGB(Color);
  Temp.R := DarkColorChannel(Temp.R, Pct);
  Temp.G := DarkColorChannel(Temp.G, Pct);
  Temp.B := DarkColorChannel(Temp.B, Pct);
  Result := Temp.Value;
end;

function DarkColorChannel(const Channel: Byte; const Pct: Single): Byte;
var
  Temp: Integer;
begin
  if Pct < 0 then
    Result := BrightColorChannel(Channel, -Pct)
  else
  begin
    Temp := Round(Channel - Pct * MaxBytePercent);
    if Temp < Low(Result) then
      Result := Low(Result)
    else
      Result := Temp;
  end;
end;

// Converts values of the XYZ color space using the D65 white point to D50 white point.
// The values were taken from www.srgb.com/hpsrgbprof/sld005.htm

procedure CIED65ToCIED50(var X, Y, Z: Extended);
var
  Xn, Yn, Zn: Extended;
begin
  Xn :=  1.0479 * X + 0.0299 * Y - 0.0502 * Z;
  Yn :=  0.0296 * X + 0.9904 * Y - 0.0171 * Z;
  Zn := -0.0092 * X + 0.0151 * Y + 0.7519 * Z;
  X := Xn;
  Y := Yn;
  Z := Zn;
end;

// converts each color component from a 16bits per sample to 8 bit used in Windows DIBs
// Count is the number of entries in Source and Target

procedure Gray16(const Source, Target: Pointer; Count: Cardinal);
var
  SourceRun: PWord;
  TargetRun: PByte;
begin
  SourceRun := Source;
  TargetRun := Target;
  while Count > 0 do
  begin
    TargetRun^ := SourceRun^ shr 8;
    Inc(SourceRun);
    Inc(TargetRun);
    Dec(Count);
  end;
end;

type
  PCMYK = ^TCMYK;
  TCMYK = packed record
    C: Byte;
    M: Byte;
    Y: Byte;
    K: Byte;
  end;

  PCMYK16 = ^TCMYK16;
  TCMYK16 = packed record
    C: Word;
    M: Word;
    Y: Word;
    K: Word;
  end;

// converts a stream of Count CMYK values to BGR
// BitsPerSample : 8 or 16
// CMYK is C,M,Y,K 4 byte record or 4 word record
// Target is always 3 byte record B, R, G

procedure CMYKToBGR(const Source, Target: Pointer; const BitsPerSample: Byte; Count: Cardinal); overload;
var
  R, G, B, K: Integer;
  I: Integer;
  SourcePtr: PCMYK;
  SourcePtr16: PCMYK16;
  TargetPtr: PByte;
begin
  case BitsPerSample of
    8:
      begin
        SourcePtr := Source;
        TargetPtr := Target;
        Count := Count div 4;
        for I := 0 to Count - 1 do
        begin
          K := SourcePtr.K;
          R := 255 - (SourcePtr.C - MulDiv(SourcePtr.C, K, 255) + K);
          G := 255 - (SourcePtr.M - MulDiv(SourcePtr.M, K, 255) + K);
          B := 255 - (SourcePtr.Y - MulDiv(SourcePtr.Y, K, 255) + K);
          TargetPtr^ := Max(0, Min(255, Byte(B)));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, Byte(G)));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, Byte(R)));
          Inc(TargetPtr);
          Inc(SourcePtr);
        end;
      end;
    16:
      begin
        SourcePtr16 := Source;
        TargetPtr := Target;
        Count := Count div 4;
        for I := 0 to Count - 1 do
        begin
          K := SourcePtr16.K;
          R := 255 - (SourcePtr16.C - MulDiv(SourcePtr16.C, K, 65535) + K) shr 8;
          G := 255 - (SourcePtr16.M - MulDiv(SourcePtr16.M, K, 65535) + K) shr 8;
          B := 255 - (SourcePtr16.Y - MulDiv(SourcePtr16.Y, K, 65535) + K) shr 8;
          TargetPtr^ := Max(0, Min(255, Byte(B)));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, Byte(G)));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, Byte(R)));
          Inc(TargetPtr);
          Inc(SourcePtr16);
        end;
      end;
    else
      raise EColorConversionError.CreateResFmt(@RsBitsPerSampleNotSupported, [BitsPerSample]);
  end;
end;

// converts a stream of Count CMYK values to BGR

procedure CMYKToBGR(const C, M, Y, K, Target: Pointer; const BitsPerSample: Byte; Count: Cardinal); overload;
var
  R, G, B: Integer;
  C8, M8, Y8, K8: PByte;
  C16, M16, Y16, K16: PWord;
  I: Integer;
  TargetPtr: PByte;
begin
  case BitsPerSample of
    8:
      begin
        C8 := C;
        M8 := M;
        Y8 := Y;
        K8 := K;
        TargetPtr := Target;
        Count := Count div 4;
        for I := 0 to Count - 1 do
        begin
          R := 255 - (C8^ - MulDiv(C8^, K8^, 255) + K8^);
          G := 255 - (M8^ - MulDiv(M8^, K8^, 255) + K8^);
          B := 255 - (Y8^ - MulDiv(Y8^, K8^, 255) + K8^);
          TargetPtr^ := Max(0, Min(255, Byte(B)));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, Byte(G)));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, Byte(R)));
          Inc(TargetPtr);
          Inc(C8);
          Inc(M8);
          Inc(Y8);
          Inc(K8);
        end;
      end;
    16:
      begin
        C16 := C;
        M16 := M;
        Y16 := Y;
        K16 := K;
        TargetPtr := Target;
        Count := Count div 4;
        for I := 0 to Count - 1 do
        begin
          R := 255 - (C16^ - MulDiv(C16^, K16^, 65535) + K16^) shr 8;
          G := 255 - (M16^ - MulDiv(M16^, K16^, 65535) + K16^) shr 8;
          B := 255 - (Y16^ - MulDiv(Y16^, K16^, 65535) + K16^) shr 8;
          TargetPtr^ := Max(0, Min(255, Byte(B)));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, Byte(G)));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, Byte(R)));
          Inc(TargetPtr);
          Inc(C16);
          Inc(M16);
          Inc(Y16);
          Inc(K16);
        end;
      end;
    else
      raise EColorConversionError.CreateResFmt(@RsBitsPerSampleNotSupported, [BitsPerSample]);
  end;
end;

// conversion of the CIE L*a*b color space to RGB using a two way approach assuming a D65 white point,
// first a conversion to CIE XYZ is performed and then from there to RGB

procedure CIELABToBGR(const Source, Target: Pointer; const Count: Cardinal); overload;
var
  FinalR,
  FinalG,
  FinalB: Integer;
  L, a, b,
  X, Y, Z, // color values in float format
  T, YYn3: Double;  // intermediate results
  SourcePtr,
  TargetPtr: PByte;
  PixelCount: Cardinal;
begin
  SourcePtr := Source;
  TargetPtr := Target;
  PixelCount := Count div 3;

  while PixelCount > 0 do
  begin
    // L should be in the range of 0..100 but at least Photoshop stores the luminance
    // in the range of 0..255
    L := SourcePtr^ / 2.55;
    Inc(SourcePtr);
    a := Shortint(SourcePtr^);
    Inc(SourcePtr);
    b := Shortint(SourcePtr^);
    Inc(SourcePtr);

    // CIE L*a*b can be calculated from CIE XYZ by:
    // L = 116 * ((Y / Yn)^1/3) - 16   if (Y / Yn) > 0.008856
    // L = 903.3 * Y / Yn              if (Y / Yn) <= 0.008856
    // a = 500 * (f(X / Xn) - f(Y / Yn))
    // b = 200 * (f(Y / Yn) - f(Z / Zn))
    //   where f(t) = t^(1/3) with (Y / Yn) > 0.008856
    //         f(t) = 7.787 * t + 16 / 116 with (Y / Yn) <= 0.008856
    //
    // by reordering the above equations we can calculate CIE L*a*b -> XYZ as follows:
    // L is in the range 0..100 and a as well as b in -127..127
    YYn3 := (L + 16) / 116; // this corresponds to (Y/Yn)^1/3
    if L < 7.9996 then
    begin
      Y := L / 903.3;
      X := a / 3893.5 + Y;
      Z := Y - b / 1557.4;
    end
    else
    begin
      T := YYn3 + a / 500;
      X := T * T * T;
      Y := YYn3 * YYn3 * YYn3;
      T := YYn3 - b / 200;
      Z := T * T * T;
    end;

    // once we have CIE XYZ it is easy (yet quite expensive) to calculate RGB values from this
    FinalR := Round(255.0 * ( 2.998 * X - 1.458 * Y - 0.541 * Z));
    FinalG := Round(255.0 * (-0.952 * X + 1.893 * Y + 0.059 * Z));
    FinalB := Round(255.0 * ( 0.099 * X - 0.198 * Y + 1.099 * Z));

    TargetPtr^ := Max(0, Min(255, Byte(FinalB)));
    Inc(TargetPtr);
    TargetPtr^ := Max(0, Min(255, Byte(FinalG)));
    Inc(TargetPtr);
    TargetPtr^ := Max(0, Min(255, Byte(FinalR)));
    Inc(TargetPtr);

    Dec(PixelCount);
  end;
end;

// conversion of the CIE L*a*b color space to RGB using a two way approach assuming a D65 white point,
// first a conversion to CIE XYZ is performed and then from there to RGB
// The BitsPerSample are not used so why leave it here.

procedure CIELABToBGR(LSource, aSource, bSource: PByte; const Target: Pointer; const Count: Cardinal); overload;
var
  FinalR,
  FinalG,
  FinalB: Integer;
  L, a, b,
  X, Y, Z, // color values in float format
  T, YYn3: Double;  // intermediate results
  TargetPtr: PByte;
  PixelCount: Cardinal;
begin
  TargetPtr := Target;
  PixelCount := Count div 3;

  while PixelCount > 0 do
  begin
    // L should be in the range of 0..100 but at least Photoshop stores the luminance
    // in the range of 0..256
    L := LSource^ / 2.55;
    Inc(LSource);
    a := Shortint(aSource^);
    Inc(aSource);
    b := Shortint(bSource^);
    Inc(bSource);

    // CIE L*a*b can be calculated from CIE XYZ by:
    // L = 116 * ((Y / Yn)^1/3) - 16   if (Y / Yn) > 0.008856
    // L = 903.3 * Y / Yn              if (Y / Yn) <= 0.008856
    // a = 500 * (f(X / Xn) - f(Y / Yn))
    // b = 200 * (f(Y / Yn) - f(Z / Zn))
    //   where f(t) = t^(1/3) with (Y / Yn) > 0.008856
    //         f(t) = 7.787 * t + 16 / 116 with (Y / Yn) <= 0.008856
    //
    // by reordering the above equations we can calculate CIE L*a*b -> XYZ as follows:
    // L is in the range 0..100 and a as well as b in -127..127
    YYn3 := (L + 16) / 116; // this corresponds to (Y/Yn)^1/3
    if L < 7.9996 then
    begin
      Y := L / 903.3;
      X := a / 3893.5 + Y;
      Z := Y - b / 1557.4;
    end
    else
    begin
      T := YYn3 + a / 500;
      X := T * T * T;
      Y := YYn3 * YYn3 * YYn3;
      T := YYn3 - b / 200;
      Z := T * T * T;
    end;

    // once we have CIE XYZ it is easy (yet quite expensive) to calculate RGB values from this
    FinalR := Round(255.0 * ( 2.998 * X - 1.458 * Y - 0.541 * Z));
    FinalG := Round(255.0 * (-0.952 * X + 1.893 * Y + 0.059 * Z));
    FinalB := Round(255.0 * ( 0.099 * X - 0.198 * Y + 1.099 * Z));

    TargetPtr^ := Max(0, Min(255, Byte(FinalB)));
    Inc(TargetPtr);
    TargetPtr^ := Max(0, Min(255, Byte(FinalG)));
    Inc(TargetPtr);
    TargetPtr^ := Max(0, Min(255, Byte(FinalR)));
    Inc(TargetPtr);

    Dec(PixelCount);
  end;
end;

// reorders a stream of "Count" RGB values to BGR, additionally an eventual sample size adjustment is done

procedure RGBToBGR(const Source, Target: Pointer; const BitsPerSample: Byte; Count: Cardinal); overload;
var
  SourceRun16: PRGBWord;
  SourceRun8: PRGB;
  TargetRun: PBGR;
begin
  Count := Count div 3;
  // usually only 8 bit samples are used but Photoshop allows for 16 bit samples
  case BitsPerSample of
    8:
      begin
        SourceRun8 := Source;
        TargetRun := Target;
        while Count > 0 do
        begin
          TargetRun.R := SourceRun8.R;
          TargetRun.G := SourceRun8.G;
          TargetRun.B := SourceRun8.B;
          Inc(SourceRun8);
          Inc(TargetRun);
          Dec(Count);
        end;
      end;
    16:
      begin
        SourceRun16 := Source;
        TargetRun := Target;
        while Count > 0 do
        begin
          TargetRun.R := SourceRun16.R shr 8;
          TargetRun.G := SourceRun16.G shr 8;
          TargetRun.B := SourceRun16.B shr 8;
          Inc(SourceRun16);
          Inc(TargetRun);
          Dec(Count);
        end;
      end;
    else
      raise EColorConversionError.CreateResFmt(@RsBitsPerSampleNotSupported, [BitsPerSample]);
  end;
end;

// reorders a stream of "Count" RGB values to BGR, additionally an eventual sample size adjustment is done

procedure RGBToBGR(const R, G, B, Target: Pointer; const BitsPerSample: Byte; Count: Cardinal); overload;
var
  R8, G8, B8: PByte;
  R16, G16, B16: PWord;
  TargetRun: PByte;
begin
  Count := Count div 3;
  // usually only 8 bits samples are used but Photoshop allows 16 bits samples too
  case BitsPerSample of
    8:
      begin
        R8 := R;
        G8 := G;
        B8 := B;
        TargetRun := Target;
        while Count > 0 do
        begin
          TargetRun^ := B8^;
          Inc(B8);
          Inc(TargetRun);
          TargetRun^ := G8^;
          Inc(G8);
          Inc(TargetRun);
          TargetRun^ := R8^;
          Inc(R8);
          Inc(TargetRun);
          Dec(Count);
        end;
      end;
    16:
      begin
        R16 := R;
        G16 := G;
        B16 := B;
        TargetRun := Target;
        while Count > 0 do
        begin
          TargetRun^ := B16^ shr 8;
          Inc(B16);
          Inc(TargetRun);
          TargetRun^ := G16^ shr 8;
          Inc(G16);
          Inc(TargetRun);
          TargetRun^ := R16^ shr 8;
          Inc(R16);
          Inc(TargetRun);
          Dec(Count);
        end;
      end;
    else
      raise EColorConversionError.CreateResFmt(@RsBitsPerSampleNotSupported, [BitsPerSample]);
  end;
end;

// reorders a stream of "Count" RGBA values to BGRA, additionally an eventual sample
// size adjustment is done

procedure RGBAToBGRA(const Source, Target: Pointer; const BitsPerSample: Byte; Count: Cardinal);
var
  SourceRun16: PRGBAWord;
  SourceRun8: PRGBA;
  TargetRun: PBGRA;
begin
  Count := Count div 4;
  // usually only 8 bit samples are used but Photoshop allows for 16 bit samples
  case BitsPerSample of
    8:
      begin
        SourceRun8 := Source;
        TargetRun := Target;
        while Count > 0 do
        begin
          TargetRun.R := SourceRun8.R;
          TargetRun.G := SourceRun8.G;
          TargetRun.B := SourceRun8.B;
          TargetRun.A := SourceRun8.A;
          Inc(SourceRun8);
          Inc(TargetRun);
          Dec(Count);
        end;
      end;
    16:
      begin
        SourceRun16 := Source;
        TargetRun := Target;
        while Count > 0 do
        begin
          TargetRun.R := SourceRun16.B shr 8;
          TargetRun.G := SourceRun16.G shr 8;
          TargetRun.B := SourceRun16.R shr 8;
          TargetRun.A := SourceRun16.A shr 8;
          Inc(SourceRun16);
          Inc(TargetRun);
          Dec(Count);
        end;
      end;
    else
      raise EColorConversionError.CreateResFmt(@RsBitsPerSampleNotSupported, [BitsPerSample]);
  end;
end;

procedure WinColorToOpenGLColor(const Color: TColor; out Red, Green, Blue: Float);
var
  Temp: TColorRec;
begin
  Temp.Value := ColorToRGB(Color);
  Red   := (Temp.R / High(Temp.R));
  Green := (Temp.G / High(Temp.G));
  Blue  := (Temp.B / High(Temp.B));
end;

function OpenGLColorToWinColor(const Red, Green, Blue: Float): TColor;
var
  Temp: TColorRec;
begin
  Temp.R := Round(Red   * High(Temp.R));
  Temp.G := Round(Green * High(Temp.G));
  Temp.B := Round(Blue  * High(Temp.B));
  Temp.Flag := 0;
  Result := Temp.Value;
end;

function Color32(WinColor: TColor): TColor32; overload;
begin
  WinColor := ColorToRGB(WinColor);
  Result := ColorSwap(WinColor);
end;

function Color32(const R, G, B: Byte; const A: Byte): TColor32; overload;
begin
  Result := A shl 24 + R shl 16 + G shl 8 + B;
end;

function Color32(const Index: Byte; const Palette: TPalette32): TColor32; overload;
begin
  Result := Palette[Index];
end;

function Gray32(const Intensity: Byte; const Alpha: Byte): TColor32;
begin
  Result := TColor32(Alpha) shl 24 + TColor32(Intensity) shl 16 +
    TColor32(Intensity) shl 8 + TColor32(Intensity);
end;

function WinColor(const Color32: TColor32): TColor;
begin
  // the alpha channel byte is set to zero
  Result := (Color32 and _R shr 16) or (Color32 and _G) or
    (Color32 and _B shl 16);
end;

function RedComponent(const Color32: TColor32): Integer;
begin
  Result := Color32 and _R shr 16;
end;

function GreenComponent(const Color32: TColor32): Integer;
begin
  Result := Color32 and _G shr 8;
end;

function BlueComponent(const Color32: TColor32): Integer;
begin
  Result := Color32 and _B;
end;

function AlphaComponent(const Color32: TColor32): Integer;
begin
  Result := Color32 shr 24;
end;

function Intensity(const R, G, B: Single): Single;
const
  RFactor =  61 / 256;
  GFactor = 174 / 256;
  BFactor =  21 / 256;
begin
  Result := RFactor * R + GFactor * G + BFactor * B;
end;

// input:  RGB components
// output: (R * 61 + G * 174 + B * 21) div 256

function Intensity(const Color32: TColor32): Integer;
begin
  Result := (Color32 and _B) * 21      // Blue
    + ((Color32 and _G) shr 8) * 174   // Green
    + ((Color32 and _R) shr 16) * 61;  // Red
  Result := Result shr 8;
end;

function SetAlpha(const Color32: TColor32; NewAlpha: Integer): TColor32;
begin
  Result := (Color32 and _RGB) or (TColor32(NewAlpha) shl 24);
end;

procedure HLSToRGB(const H, L, S: Single; out R, G, B: Single);
var
  M1, M2: Single;

  function HueToColorValue(Hue: Single): Single;
  begin
    Hue := Hue - Floor(Hue);

    if 6 * Hue < 1 then
      Result := M1 + (M2 - M1) * Hue * 6
    else
    if 2 * Hue < 1 then
      Result := M2
    else
    if 3 * Hue < 2 then
      Result := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else
      Result := M1;
  end;
    
begin
  if S = 0 then
  begin
    R := L;
    G := R;
    B := R;
  end
  else
  begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColorValue(H + 1 / 3);
    G := HueToColorValue(H);
    B := HueToColorValue(H - 1 / 3)
  end;
end;

{$IFDEF KEEP_DEPRECATED}
procedure HSLToRGB(const H, S, L: Single; out R, G, B: Single);
begin
  HLSToRGB(H, L, S, R, G, B);
end;
{$ENDIF KEEP_DEPRECATED}

function HSLToRGB(const H, S, L: Single): TColor32;
var
  R, G, B: Single;
begin
  HLSToRGB(H, L, S, R, G, B);
  Result := Color32(Round(R * 255), Round(G * 255), Round(B * 255), 255);
end;

function HLSToRGB(const HLS: TColorVector): TColorVector;
begin
  HLSToRGB(HLS.H, HLS.L, HLS.S, Result.R, Result.G, Result.B);
end;

procedure RGBToHLS(const R, G, B: Single; out H, L, S: Single);
var
  D, Cmax, Cmin: Single;
begin
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  L := (Cmax + Cmin) / 2;

  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end
  else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);
    if R = Cmax then
      H := (G - B) / D
    else
    if G = Cmax then
      H := 2 + (B - R) / D
    else
      H := 4 + (R - G) / D;
    H := H / 6;
    if H < 0 then
      H := H + 1;
  end;
end;

{$IFDEF KEEP_DEPRECATED}
procedure RGBToHSL(const R, G, B: Single; out H, S, L: Single);
begin
  RGBToHLS(R, G, B, H, L, S);
end;
{$ENDIF KEEP_DEPRECATED}

procedure RGBToHSL(const RGB: TColor32; out H, S, L: Single);
begin
  RGBToHLS(RedComponent(RGB) / 255, GreenComponent(RGB) / 255, BlueComponent(RGB) / 255, H, L, S);
end;

function RGBToHLS(const RGB: TColorVector): TColorVector;
begin
  RGBToHLS(RGB.R, RGB.G, RGB.B, Result.H, Result.L, Result.S);
end;

{ Translated C-code from Microsoft Knowledge Base
-------------------------------------------
Converting Colors Between RGB and HLS (HBS)
Article ID: Q29240
Creation Date: 26-APR-1988
Revision Date: 02-NOV-1995
The information in this article applies to:

Microsoft Windows Software Development Kit (SDK) for Windows versions 3.1 and 3.0
Microsoft Win32 Application Programming Interface (API) included with:

    - Microsoft Windows NT versions 3.5 and 3.51
    - Microsoft Windows 95 version 4.0
SUMMARY


The code fragment below converts colors between RGB (Red, Green, Blue) and HLS/HBS (Hue, Lightness, Saturation/Hue, Brightness, Saturation).


MORE INFORMATION


/* Color Conversion Routines --

RGBToHLS() takes a DWORD RGB value, translates it to HLS, and stores the results in the global vars H, L, and S. HLSToRGB takes the current values of H, L, and S and returns the equivalent value in an RGB DWORD.

A point of reference for the algorithms is Foley and Van Dam, "Fundamentals of Interactive Computer Graphics," Pages 618-19. Their algorithm is in floating point. CHART implements a less general (hardwired ranges) integral algorithm.
There are potential round-off errors throughout this sample. ((0.5 + x)/y) without floating point is phrased ((x + (y/2))/y), yielding a very small round-off error. This makes many of the following divisions look strange. */ }

const
  HLSMAX = High(THLSValue);     // H,L, and S vary over 0-HLSMAX
  RGBMAX = 255;                 // R,G, and B vary over 0-RGBMAX
                                // HLSMAX BEST IF DIVISIBLE BY 6
                                // RGBMAX, HLSMAX must each fit in a byte.

// Hue is undefined if Saturation is 0 (grey-scale).
// This value determines where the Hue value is initially set for achromatic colors.
  UNDEFINED = HLSMAX * 2 div 3;

type
  TInternalRGB = packed record
    R: Byte;
    G: Byte;
    B: Byte;
    I: Byte;
  end;

function RGB(R, G, B: Byte): TColor;
begin
  TInternalRGB(Result).R := R;
  TInternalRGB(Result).G := G;
  TInternalRGB(Result).B := B;
  TInternalRGB(Result).I := 0;
end;

function RGBToHLS(const RGBColor: TColorRef): THLSVector;
var
  R, G, B: Integer;              // input RGB values
  H, L, S: Integer;
  Cmax, Cmin: Byte;              // max and min RGB values
  Rdelta,Gdelta,Bdelta: Integer; // intermediate value: % of spread from max
begin
  // get R, G, and B out of DWORD
  R := TInternalRGB(RGBColor).R;
  G := TInternalRGB(RGBColor).G;
  B := TInternalRGB(RGBColor).B;

  // calculate lightness
  Cmax := R;
  if G > Cmax then
    Cmax := G;
  if B > Cmax then
    Cmax := B;

  Cmin := R;
  if G < Cmin then
    Cmin := G;
  if B < Cmin then
    Cmin := B;

  L := (((Cmax + Cmin) * HLSMAX) + RGBMAX) div (2 * RGBMAX);

  if (Cmax = Cmin) then           // r=g=b --> achromatic case
  begin
    S := 0;                       // saturation
    H := UNDEFINED;               // hue
  end
  else
  begin                           // chromatic case
    // saturation
    if L <= (HLSMAX div 2) then
      S := (((Cmax - Cmin) * HLSMAX) + ((Cmax + Cmin) div 2))  div  (Cmax + Cmin)
    else
      S := (((Cmax - Cmin) * HLSMAX) + ((2 * RGBMAX - Cmax - Cmin) div 2)) div (2 * RGBMAX - Cmax - Cmin);

    // hue
    Rdelta := (((Cmax - R) * (HLSMAX div 6)) + ((Cmax - Cmin) div 2)) div (Cmax - Cmin);
    Gdelta := (((Cmax - G) * (HLSMAX div 6)) + ((Cmax - Cmin) div 2)) div (Cmax - Cmin);
    Bdelta := (((Cmax - B) * (HLSMAX div 6)) + ((Cmax - Cmin) div 2)) div (Cmax - Cmin);

    if R = Cmax then
      H := Bdelta - Gdelta
    else
    if G = Cmax then
      H := (HLSMAX div 3) + Rdelta - Bdelta
    else // B = Cmax
      H := ((2 * HLSMAX) div 3) + Gdelta - Rdelta;

    H := H mod HLSMAX;
    if H < 0 then
      Inc(H, HLSMAX);
  end;
  Result.Hue := H;
  Result.Luminance := L;
  Result.Saturation := S;
end;

function HueToRGB(M1, M2, Hue: Integer): Integer;
// utility routine for HLSToRGB
begin
  Hue := Hue mod HLSMAX;
  // range check: note values passed add div subtract thirds of range
  if Hue < 0 then
    Inc(Hue, HLSMAX);

  // return r,g, or b value from this tridrant
  if Hue < (HLSMAX div 6) then
    Result := (M1 + (((M2 - M1)  *  Hue + (HLSMAX div 12)) div (HLSMAX div 6)))
  else
  if Hue < (HLSMAX div 2) then
    Result := M2
  else
  if Hue < ((HLSMAX * 2) div 3) then
    Result := (M1 + (((M2 - M1) * (((HLSMAX * 2) div 3) - Hue) + (HLSMAX div 12)) div (HLSMAX div 6)))
  else
    Result := M1;
end;

function HLSToRGB(const Hue, Luminance, Saturation: THLSValue): TColorRef;
var
  R, G, B: Integer;              // RGB component values
  Magic1, Magic2: Integer;       // calculated magic numbers (really!)
begin
  if Saturation = 0 then         // achromatic case
  begin
    R :=(Luminance * RGBMAX) div HLSMAX;
    G := R;
    B := R;
    if Hue <> UNDEFINED then
    begin
      // ERROR
    end
  end else
  begin                          // chromatic case
    // set up magic numbers
    if (Luminance <= (HLSMAX div 2)) then
      Magic2 := (Luminance * (HLSMAX + Saturation) + (HLSMAX div 2)) div HLSMAX
    else
      Magic2 := Luminance + Saturation - ((Luminance * Saturation) + (HLSMAX div 2)) div HLSMAX;
    Magic1 := 2 * Luminance - Magic2;
    // get RGB, change units from HLSMAX to RGBMAX
    R := (HueToRGB(Magic1, Magic2, Hue + (HLSMAX div 3)) * RGBMAX + (HLSMAX div 2)) div HLSMAX;
    G := (HueToRGB(Magic1, Magic2, Hue) * RGBMAX + (HLSMAX div 2)) div HLSMAX;
    B := (HueToRGB(Magic1, Magic2, Hue - (HLSMAX div 3)) * RGBMAX + (HLSMAX div 2)) div HLSMAX;
  end;
  Result :=  RGB(R, G, B);
end;

{$IFDEF VCL}
function SetBitmapColors(Bmp: TBitmap; const Colors: array of TColor; StartIndex: Integer): Integer;
type
  TRGBQuadArray = array [Byte] of TRGBQuad;
  PRGBQuadArray = ^TRGBQuadArray;
var
  I, RGB: Integer;
  ColorTable: PRGBQuadArray;
  Count: Integer;
begin
  Count := High(Colors)-Low(Colors)+1;
  GetMem(ColorTable, Count * SizeOf(TRGBQuad));
  try
    for I := 0 to Count-1 do
      with ColorTable^[I] do
      begin
        RGB := ColorToRGB(Colors[I]);
        rgbBlue := GetBValue(RGB);
        rgbGreen := GetGValue(RGB);
        rgbRed := GetRValue(RGB);
        rgbReserved := 0;
      end;
    Bmp.HandleType := bmDIB;
    Result := GDICheck(SetDIBColorTable(Bmp.Canvas.Handle, StartIndex, Count, ColorTable^));
  finally
    FreeMem(ColorTable);
  end;
end;
{$ENDIF VCL}

//=== Misc ===================================================================

function ColorToHTML(const Color: TColor): string;
var
  Temp: TColorRec;
begin
  Temp.Value := ColorToRGB(Color);
  Result := Format('#%.2x%.2x%.2x', [Temp.R, Temp.G, Temp.B]);
end;

{$IFDEF VCL}
function DottedLineTo(const Canvas: TCanvas; const X, Y: Integer): Boolean;
const
  DotBits: array [0..7] of Word = ($AA, $55, $AA, $55, $AA, $55, $AA, $55);
var
  Bitmap: HBitmap;
  Brush: HBrush;
  SaveTextColor, SaveBkColor: TColorRef;
  LastPos: TPoint;
  R: TRect;
  DC: HDC;
begin
  DC := Canvas.Handle;
  GetCurrentPositionEx(DC, @LastPos);
  Result := False;
  if LastPos.X = X then
    R := RectAssign(LastPos.X, LastPos.Y, LastPos.X + 1, Y)
  else
  if LastPos.Y = Y then
    R := RectAssign(LastPos.X, LastPos.Y, X, LastPos.Y + 1)
  else
    Exit;
  Bitmap := CreateBitmap(8, 8, 1, 1, @DotBits);
  Brush := CreatePatternBrush(Bitmap);
  SaveTextColor := SetTextColor(DC, ColorToRGB(Canvas.Pen.Color));
  SaveBkColor := SetBkColor(DC, ColorToRGB(Canvas.Brush.Color));
  FillRect(DC, R, Brush);
  MoveToEx(DC, X, Y, nil);
  SetBkColor(DC, SaveBkColor);
  SetTextColor(DC, SaveTextColor);
  DeleteObject(Brush);
  DeleteObject(Bitmap);
  Result := True;
end;
{$ENDIF VCL}

{$IFDEF MSWINDOWS}
// Adjusts the given string S so that it fits into the given width. EllipsisWidth gives the width of
// the three points to be added to the shorted string. If this value is 0 then it will be determined implicitely.
// For higher speed (and multiple entries to be shorted) specify this value explicitely.
// RTL determines if right-to-left reading is active, which is needed to put the ellipsisis on the correct side.
// Note: It is assumed that the string really needs shortage. Check this in advance.

function ShortenString(const DC: HDC; const S: WideString; const Width: Integer; const RTL: Boolean;
  EllipsisWidth: Integer): WideString;
var
  Size: TSize;
  Len: Integer;
  L, H, N, W: Integer;
begin
  Len := Length(S);
  if (Len = 0) or (Width <= 0) then
    Result := ''
  else
  begin
    // Determine width of triple point using the current DC settings (if not already done).
    if EllipsisWidth = 0 then
    begin
      GetTextExtentPoint32W(DC, '...', 3, Size);
      EllipsisWidth := Size.cx;
    end;

    if Width <= EllipsisWidth then
      Result := ''
    else
    begin
      // Do a binary search for the optimal string length which fits into the given width.
      L := 0;
      H := Len;
      N := 0;
      while L <= H do
      begin
        N := (L + H) shr 1;
        GetTextExtentPoint32W(DC, PWideChar(S), N, Size);
        W := Size.cx + EllipsisWidth;
        if W < Width then
          L := N + 1
        else
        begin
          H := N - 1;
          if W = Width then
            L := N;
        end;
      end;

      // Windows 2000+ automatically switches the order in the string. For every other system we have to take care.
      if IsWin2K or not RTL then
        Result := Copy(S, 1, N - 1) + '...'
      else
        Result := '...' + Copy(S, 1, N - 1);
    end;
  end;
end;
{$ENDIF MSWINDOWS}

//=== Clipping ===============================================================

function ClipCodes(const X, Y, MinX, MinY, MaxX, MaxY: Float): TClipCodes;
begin
  Result := [];
  if X > MaxX then
    Include(Result, ccRight)
  else
  if X < MinX then
    Include(Result, ccLeft);
  if Y < MinY then
    Include(Result, ccAbove)
  else
  if Y > MaxY then
    Include(Result, ccBelow);
end;

function ClipCodes(const X, Y: Float; const ClipRect: TRect): TClipCodes;
begin
  Result := ClipCodes(X, Y, ClipRect.Left, ClipRect.Top, ClipRect.Right, ClipRect.Bottom);
end;

function ClipLine(var X1, Y1, X2, Y2: Integer; const ClipRect: TRect): Boolean;
var
  FX1, FY1, FX2, FY2: Float;
begin
  FX1 := X1;
  FY1 := Y1;
  FX2 := X2;
  FY2 := Y2;
  Result := ClipLine(FX1, FY1, FX2, FY2,
    ClipRect.Left, ClipRect.Top, ClipRect.Right, ClipRect.Bottom, nil);
  if Result then
  begin
    X1 := Round(FX1);
    Y1 := Round(FY1);
    X2 := Round(FX2);
    Y2 := Round(FY2);
  end;
end;

function ClipLine(var X1, Y1, X2, Y2: Float; const MinX, MinY, MaxX, MaxY: Float;
  Codes: PClipCodes): Boolean;
var
  Done: Boolean;
  Codes_, Codes1, Codes2: TClipCodes;
  X, Y: Float;

  function ClipCodes(X, Y: Float): TClipCodes;
  begin
    Result := [];
    if X > MaxX then
      Include(Result, ccRight)
    else
    if X < MinX then
      Include(Result, ccLeft);
    if Y < MinY then
      Include(Result, ccAbove)
    else
    if Y > MaxY then
      Include(Result, ccBelow);
  end;

begin
  Result := False;
  Done := False;
  Codes2 := ClipCodes(X2, Y2);
  if Codes <> nil then
  begin
    Codes1 := Codes^;
    Codes^ := Codes2;
  end
  else
    Codes1 := ClipCodes(X1, Y1);
  repeat
    if (Codes1 = []) and (Codes2 = []) then
    begin
      Result := True;
      Done := True;
    end
    else
    if (Codes1 * Codes2) <> [] then
      Done := True
    else
    begin
      if Codes1 <> [] then
        Codes_ := Codes1
      else
        Codes_ := Codes2;
      X := 0;
      Y := 0;
      if ccLeft in Codes_ then
      begin
        Y := Y1 + (Y2 - Y1) * (MinX - X1) / (X2 - X1);
        X := MinX;
      end
      else
      if ccRight in Codes_ then
      begin
        Y := Y1 + (Y2 - Y1) * (MaxX - X1) / (X2 - X1);
        X := MaxX;
      end
      else
      if ccAbove in Codes_ then
      begin
        X := X1 + (X2 - X1) * (MinY - Y1) / (Y2 - Y1);
        Y := MinY;
      end
      else
      if ccBelow in Codes_ then
      begin
        X := X1 + (X2 - X1) * (MaxY - Y1) / (Y2 - Y1);
        Y := MaxY;
      end;
      if Codes_ = Codes1 then
      begin
        X1 := X;
        Y1 := Y;
        Codes1 := ClipCodes(X1, Y1);
      end
      else
      begin
        X2 := X;
        Y2 := Y;
        Codes2 := ClipCodes(X2, Y2);
      end;
    end;
  until Done;
end;

procedure DrawPolyLine(const Canvas: TCanvas; var Points: TPointArray; const ClipRect: TRect);
var
  I: Integer;
  X, Y: Integer;
  X1, Y1, X2, Y2: Float;
  ClipX1, ClipY1, ClipX2, ClipY2: Float;
  Codes1, Codes2: TClipCodes;
begin
  if not RectIsValid(ClipRect) then
    Exit;

  with Points[0] do
  begin
    X1 := X;
    Y1 := Y;
    Canvas.MoveTo(X, Y);
  end;

  ClipX1 := ClipRect.Left;
  ClipY1 := ClipRect.Top;
  ClipX2 := ClipRect.Right;
  ClipY2 := ClipRect.Bottom;

  Codes2 := ClipCodes(X1, Y1, ClipX1, ClipY1, ClipX2, ClipY2);
  for I := 1 to High(Points) do
  begin
    with Points[I] do
    begin
      X2 := X;
      Y2 := Y;
    end;
    Codes1 := Codes2;
    if ClipLine(X1, Y1, X2, Y2, ClipX1, ClipY1, ClipX2, ClipY2, @Codes2) then
    begin
      if Codes1 <> [] then
        Canvas.MoveTo(Round(X1), Round(Y1));
      X := Round(X2);
      Y := Round(Y2);
      Canvas.LineTo(X, Y);
      {$IFDEF VCL}
      if Codes2 <> [] then
        // Draw end point if neccessary
        Canvas.LineTo(X + 1, Y);
      {$ENDIF VCL}
    end;
    with Points[I] do
    begin
      X1 := X;
      Y1 := Y;
    end;
  end;
end;

initialization
  SetupFunctions;
  if MMX_ACTIVE then
    GenAlphaTable;
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  if MMX_ACTIVE then
    FreeAlphaTable;
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
