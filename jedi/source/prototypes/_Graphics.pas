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

{$IFNDEF PROTOTYPE}
{$IFDEF VCL}
unit JclGraphics;
{$ELSE VisualCLX}
unit JclQGraphics;
{$ENDIF VisualCLX}
{$ENDIF ~PROTOTYPE}

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
  {$IFDEF VisualCLX}
  Types, QGraphics, JclQGraphUtils,
  {$ELSE}
  Graphics, JclGraphUtils, Controls,
  {$ENDIF VisualCLX}
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
  {$IFDEF VCL}
  TJclDesktopCanvas = class(TCanvas)
  private
    FDesktop: HDC;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TJclRegion = class;

  TJclRegionInfo = class(TObject)
  private
    FData: Pointer;
    FDataSize: Integer;
    function GetBox: TRect;
  protected
    function GetCount: Integer;
    function GetRect(index: Integer): TRect;
  public
    constructor Create(Region: TJclRegion);
    destructor Destroy; override;
    property Box: TRect read GetBox;
    property Rectangles[Index: Integer]: TRect read GetRect;
    property Count: Integer read GetCount;
  end;

  TJclRegion = class(TObject)
  private
    FHandle: HRGN;
    FBoxRect: TRect;
    FRegionType: Integer;
    FOwnsHandle: Boolean;
    procedure CheckHandle;
  protected
    function GetHandle: HRGN;
    function GetBox: TRect;
    function GetRegionType: TJclRegionKind;
  public
    constructor Create(RegionHandle: HRGN; OwnsHandle: Boolean = True);
    constructor CreateElliptic(const ARect: TRect); overload;
    constructor CreateElliptic(const Top, Left, Bottom, Right: Integer); overload;
    constructor CreatePoly(const Points: TDynPointArray; Count: Integer; FillMode: TPolyFillMode);
    constructor CreatePolyPolygon(const Points: TDynPointArray; const Vertex: TDynIntegerArray;
      Count: Integer; FillMode: TPolyFillMode);
    constructor CreateRect(const ARect: TRect; DummyForBCB: Boolean = False); overload;
    constructor CreateRect(const Top, Left, Bottom, Right: Integer; DummyForBCB: Byte = 0); overload;
    constructor CreateRoundRect(const ARect: TRect; CornerWidth, CornerHeight: Integer); overload;
    constructor CreateRoundRect(const Top, Left, Bottom, Right, CornerWidth, CornerHeight: Integer); overload;
    constructor CreateBitmap(Bitmap: TBitmap; RegionColor: TColor; RegionBitmapMode: TJclRegionBitmapMode);
    constructor CreatePath(Canvas: TCanvas);
    constructor CreateRegionInfo(RegionInfo: TJclRegionInfo);
    constructor CreateMapWindow(InitialRegion: TJclRegion; hWndFrom, hWndTo: THandle); overload;
    constructor CreateMapWindow(InitialRegion: TJclRegion; ControlFrom, ControlTo: TWinControl); overload;
    destructor Destroy; override;
    procedure Clip(Canvas: TCanvas);
    procedure Combine(DestRegion, SrcRegion: TJclRegion; CombineOp: TJclRegionCombineOperator); overload;
    procedure Combine(SrcRegion: TJclRegion; CombineOp: TJclRegionCombineOperator); overload;
    function Copy: TJclRegion;
    function Equals(CompareRegion: TJclRegion): Boolean;
    procedure Fill(Canvas: TCanvas);
    procedure FillGradient(Canvas: TCanvas; ColorCount: Integer; StartColor, EndColor: TColor; ADirection: TGradientDirection);
    procedure Frame(Canvas: TCanvas; FrameWidth, FrameHeight: Integer);
    procedure Invert(Canvas: TCanvas);
    procedure Offset(X, Y: Integer);
    procedure Paint(Canvas: TCanvas);
    function PointIn(X, Y: Integer): Boolean; overload;
    function PointIn(const Point: TPoint): Boolean; overload;
    function RectIn(const ARect: TRect): Boolean; overload;
    function RectIn(Top, Left, Bottom, Right: Integer): Boolean; overload;
    procedure SetWindow(Window: THandle; Redraw: Boolean);
    function GetRegionInfo: TJclRegionInfo;
    property Box: TRect read GetBox;
    property Handle: HRGN read GetHandle;
    property RegionType: TJclRegionKind read GetRegionType;
  end;
  {$ENDIF VCL}

  {$IFDEF Bitmap32}
  { TJclThreadPersistent }
  { TJclThreadPersistent is an ancestor for TJclBitmap32 object. In addition to
    TPersistent methods, it provides thread-safe locking and change notification }
  TJclThreadPersistent = class(TPersistent)
  private
    {$IFDEF VCL}
    FLock: TRTLCriticalSection;
    {$ELSE VCL}
    FLock: TCriticalSection;
    {$ENDIF VCL}
    FLockCount: Integer;
    FUpdateCount: Integer;
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
  protected
    property LockCount: Integer read FLockCount;
    property UpdateCount: Integer read FUpdateCount;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Changing; virtual;
    procedure Changed; virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Lock;
    procedure Unlock;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TJclCustomMap }
  { An ancestor for bitmaps and similar 2D distributions which have width and
    height properties }
  TJclCustomMap = class(TJclThreadPersistent)
  private
    FHeight: Integer;
    FWidth: Integer;
    procedure SetHeight(NewHeight: Integer);
    procedure SetWidth(NewWidth: Integer);
  public
    procedure Delete; virtual;
    function  Empty: Boolean; virtual;
    procedure SetSize(Source: TPersistent); overload;
    procedure SetSize(NewWidth, NewHeight: Integer); overload; virtual;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
  end;

  { TJclBitmap32 }
  { The TJclBitmap32 class is responsible for storage of a bitmap, as well as for drawing in it }
  TJclBitmap32 = class(TJclCustomMap)
  private
    FBitmapInfo: TBitmapInfo;
    FBits: PColor32Array;
    FDrawMode: TDrawMode;
    FFont: TFont;
    FHandle: HBITMAP;
    FHDC: HDC;
    FMasterAlpha: Byte;
    FOuterColor: TColor32; // the value returned when accessing outer areas
    FPenColor: TColor32;
    FStippleCounter: Single;
    FStipplePattern: TArrayOfColor32;
    FStippleStep: Single;
    FStretchFilter: TStretchFilter;
    FResetAlphaOnAssign: Boolean;
    function  GetPixel(X, Y: Integer): TColor32;
    function  GetPixelS(X, Y: Integer): TColor32;
    function  GetPixelPtr(X, Y: Integer): PColor32;
    function  GetScanLine(Y: Integer): PColor32Array;
    procedure SetDrawMode(Value: TDrawMode);
    procedure SetFont(Value: TFont);
    procedure SetMasterAlpha(Value: Byte);
    procedure SetPixel(X, Y: Integer; Value: TColor32);
    procedure SetPixelS(X, Y: Integer; Value: TColor32);
    procedure SetStippleStep(Value: Single);
    procedure SetStretchFilter(Value: TStretchFilter);
  protected
    FontHandle: HFont;
    RasterX: Integer;
    RasterY: Integer;
    RasterXF: Single;
    RasterYF: Single;
    procedure AssignTo(Dst: TPersistent); override;
    function  ClipLine(var X0, Y0, X1, Y1: Integer): Boolean;
    class function ClipLineF(var X0, Y0, X1, Y1: Single; MinX, MaxX, MinY, MaxY: Single): Boolean;
    procedure FontChanged(Sender: TObject);
    procedure SET_T256(X, Y: Integer; C: TColor32);
    procedure SET_TS256(X, Y: Integer; C: TColor32);
    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    property StippleCounter: Single read FStippleCounter;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure SetSize(NewWidth, NewHeight: Integer); override;
    function  Empty: Boolean; override;
    procedure Clear; overload;
    procedure Clear(FillColor: TColor32); overload;
    procedure Delete; override;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    procedure ResetAlpha;

    procedure Draw(DstX, DstY: Integer; Src: TJclBitmap32); overload;
    procedure Draw(DstRect, SrcRect: TRect; Src: TJclBitmap32); overload;
    procedure Draw(DstRect, SrcRect: TRect; hSrc: HDC); overload;

    procedure DrawTo(Dst: TJclBitmap32); overload;
    procedure DrawTo(Dst: TJclBitmap32; DstX, DstY: Integer); overload;
    procedure DrawTo(Dst: TJclBitmap32; DstRect: TRect); overload;
    procedure DrawTo(Dst: TJclBitmap32; DstRect, SrcRect: TRect); overload;
    procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;
    procedure DrawTo(hDst: HDC; DstRect, SrcRect: TRect); overload;

    function  GetPixelB(X, Y: Integer): TColor32;
    procedure SetPixelT(X, Y: Integer; Value: TColor32); overload;
    procedure SetPixelT(var Ptr: PColor32; Value: TColor32); overload;
    procedure SetPixelTS(X, Y: Integer; Value: TColor32);
    procedure SetPixelF(X, Y: Single; Value: TColor32);
    procedure SetPixelFS(X, Y: Single; Value: TColor32);

    procedure SetStipple(NewStipple: TArrayOfColor32); overload;
    procedure SetStipple(NewStipple: array of TColor32); overload;
    procedure ResetStippleCounter;
    function  GetStippleColor: TColor32;

    procedure DrawHorzLine(X1, Y, X2: Integer; Value: TColor32);
    procedure DrawHorzLineS(X1, Y, X2: Integer; Value: TColor32);
    procedure DrawHorzLineT(X1, Y, X2: Integer; Value: TColor32);
    procedure DrawHorzLineTS(X1, Y, X2: Integer; Value: TColor32);
    procedure DrawHorzLineTSP(X1, Y, X2: Integer);

    procedure DrawVertLine(X, Y1, Y2: Integer; Value: TColor32);
    procedure DrawVertLineS(X, Y1, Y2: Integer; Value: TColor32);
    procedure DrawVertLineT(X, Y1, Y2: Integer; Value: TColor32);
    procedure DrawVertLineTS(X, Y1, Y2: Integer; Value: TColor32);
    procedure DrawVertLineTSP(X, Y1, Y2: Integer);

    procedure DrawLine(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure DrawLineS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure DrawLineT(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure DrawLineTS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure DrawLineA(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure DrawLineAS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure DrawLineF(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean = False);
    procedure DrawLineFS(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean = False);
    procedure DrawLineFP(X1, Y1, X2, Y2: Single; L: Boolean = False);
    procedure DrawLineFSP(X1, Y1, X2, Y2: Single; L: Boolean = False);

    procedure MoveTo(X, Y: Integer);
    procedure LineToS(X, Y: Integer);
    procedure LineToTS(X, Y: Integer);
    procedure LineToAS(X, Y: Integer);
    procedure MoveToF(X, Y: Single);
    procedure LineToFS(X, Y: Single);

    procedure FillRect(X1, Y1, X2, Y2: Integer; Value: TColor32);
    procedure FillRectS(X1, Y1, X2, Y2: Integer; Value: TColor32);
    procedure FillRectT(X1, Y1, X2, Y2: Integer; Value: TColor32);
    procedure FillRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32);

    procedure FrameRectS(X1, Y1, X2, Y2: Integer; Value: TColor32);
    procedure FrameRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
    procedure FrameRectTSP(X1, Y1, X2, Y2: Integer); overload;
    procedure RaiseRectTS(X1, Y1, X2, Y2: Integer; Contrast: Integer);

    procedure UpdateFont;
    procedure TextOut(X, Y: Integer; const Text: string); overload;
    procedure TextOut(X, Y: Integer; const ClipRect: TRect; const Text: string); overload;
    procedure TextOut(ClipRect: TRect; const Flags: Cardinal; const Text: string); overload;
    function  TextExtent(const Text: string): TSize;
    function  TextHeight(const Text: string): Integer;
    function  TextWidth(const Text: string): Integer;
    procedure RenderText(X, Y: Integer; const Text: string; AALevel: Integer; Color: TColor32);

    property BitmapHandle: HBITMAP read FHandle;
    property BitmapInfo: TBitmapInfo read FBitmapInfo;
    property Bits: PColor32Array read FBits;
    property Font: TFont read FFont write SetFont;
    property Handle: HDC read FHDC;
    property PenColor: TColor32 read FPenColor write FPenColor;
    property Pixel[X, Y: Integer]: TColor32 read GetPixel write SetPixel; default;
    property PixelS[X, Y: Integer]: TColor32 read GetPixelS write SetPixelS;
    property PixelPtr[X, Y: Integer]: PColor32 read GetPixelPtr;
    property ScanLine[Y: Integer]: PColor32Array read GetScanLine;
    property StippleStep: Single read FStippleStep write SetStippleStep;
  published
    property DrawMode: TDrawMode read FDrawMode write SetDrawMode default dmOpaque;
    property MasterAlpha: Byte read FMasterAlpha write SetMasterAlpha default $FF;
    property OuterColor: TColor32 read FOuterColor write FOuterColor default 0;
    property StretchFilter: TStretchFilter read FStretchFilter write SetStretchFilter default sfNearest;
    property ResetAlphaOnAssign: Boolean read FResetAlphaOnAssign write FResetAlphaOnAssign default true;
    property OnChanging;
    property OnChange;
  end;

  TJclByteMap = class(TJclCustomMap)
  private
    FBytes: TDynByteArray;
    FHeight: Integer;
    FWidth: Integer;
    function GetValue(X, Y: Integer): Byte;
    function GetValPtr(X, Y: Integer): PByte;
    procedure SetValue(X, Y: Integer; Value: Byte);
  protected
    procedure AssignTo(Dst: TPersistent); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function  Empty: Boolean; override;
    procedure Clear(FillValue: Byte);
    procedure ReadFrom(Source: TJclBitmap32; Conversion: TConversionKind);
    procedure SetSize(NewWidth, NewHeight: Integer); override;
    procedure WriteTo(Dest: TJclBitmap32; Conversion: TConversionKind); overload;
    procedure WriteTo(Dest: TJclBitmap32; const Palette: TPalette32); overload;
    property Bytes: TDynByteArray read FBytes;
    property ValPtr[X, Y: Integer]: PByte read GetValPtr;
    property Value[X, Y: Integer]: Byte read GetValue write SetValue; default;
  end;
  {$ENDIF Bitmap32}

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

{$IFDEF VCL}
procedure BitmapToJPeg(const FileName: string);
procedure JPegToBitmap(const FileName: string);

procedure SaveIconToFile(Icon: HICON; const FileName: string);
procedure WriteIcon(Stream: TStream; ColorBitmap, MaskBitmap: HBITMAP;
  WriteLength: Boolean = False); overload;
procedure WriteIcon(Stream: TStream; Icon: HICON; WriteLength: Boolean = False); overload;
procedure GetIconFromBitmap(Icon: TIcon; Bitmap: TBitmap);

function GetAntialiasedBitmap(const Bitmap: TBitmap): TBitmap;
{$ENDIF VCL}

{$IFDEF Bitmap32}
procedure BlockTransfer(Dst: TJclBitmap32; DstX: Integer; DstY: Integer; Src: TJclBitmap32;
  SrcRect: TRect; CombineOp: TDrawMode);

procedure StretchTransfer(Dst: TJclBitmap32; DstRect: TRect; Src: TJclBitmap32; SrcRect: TRect;
  StretchFilter: TStretchFilter; CombineOp: TDrawMode);

procedure Transform(Dst, Src: TJclBitmap32; SrcRect: TRect; Transformation: TJclTransformation);
procedure SetBorderTransparent(ABitmap: TJclBitmap32; ARect: TRect);
{$ENDIF Bitmap32}

{$IFDEF MSWINDOWS}
function FillGradient(DC: HDC; ARect: TRect; ColorCount: Integer;
  StartColor, EndColor: TColor; ADirection: TGradientDirection): Boolean; overload;
{$ENDIF MSWINDOWS}

{$IFDEF VCL}
function CreateRegionFromBitmap(Bitmap: TBitmap; RegionColor: TColor;
  RegionBitmapMode: TJclRegionBitmapMode): HRGN;
procedure ScreenShot(bm: TBitmap; Left, Top, Width, Height: Integer; Window: THandle = HWND_DESKTOP); overload;
procedure ScreenShot(bm: TBitmap; IncludeTaskBar: Boolean = True); overload;
function MapWindowRect(hWndFrom, hWndTo: THandle; ARect: TRect):TRect;
{$ENDIF VCL}

{$IFDEF Bitmap32}
// PolyLines and Polygons
procedure PolyLineTS(Bitmap: TJclBitmap32; const Points: TDynPointArray; Color: TColor32);
procedure PolyLineAS(Bitmap: TJclBitmap32; const Points: TDynPointArray; Color: TColor32);
procedure PolyLineFS(Bitmap: TJclBitmap32; const Points: TDynPointArrayF; Color: TColor32);

procedure PolygonTS(Bitmap: TJclBitmap32; const Points: TDynPointArray; Color: TColor32);
procedure PolygonAS(Bitmap: TJclBitmap32; const Points: TDynPointArray; Color: TColor32);
procedure PolygonFS(Bitmap: TJclBitmap32; const Points: TDynPointArrayF; Color: TColor32);

procedure PolyPolygonTS(Bitmap: TJclBitmap32; const Points: TDynDynPointArrayArray;
  Color: TColor32);
procedure PolyPolygonAS(Bitmap: TJclBitmap32; const Points: TDynDynPointArrayArray;
  Color: TColor32);
procedure PolyPolygonFS(Bitmap: TJclBitmap32; const Points: TDynDynPointArrayArrayF;
  Color: TColor32);

// Filters
procedure AlphaToGrayscale(Dst, Src: TJclBitmap32);
procedure IntensityToAlpha(Dst, Src: TJclBitmap32);
procedure Invert(Dst, Src: TJclBitmap32);
procedure InvertRGB(Dst, Src: TJclBitmap32);
procedure ColorToGrayscale(Dst, Src: TJclBitmap32);
procedure ApplyLUT(Dst, Src: TJclBitmap32; const LUT: TLUT8);
procedure SetGamma(Gamma: Single = 0.7);
{$ENDIF Bitmap32}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/prototypes/_Graphics.pas $';
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
  Math,
  {$IFDEF MSWINDOWS}
  CommCtrl, ShellApi,
  {$IFDEF VCL}
  ClipBrd, JPeg, TypInfo,
  JclResources,
  {$ENDIF VCL}
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

{$IFDEF Bitmap32}
procedure CheckBitmaps(Dst, Src: TJclBitmap32);
begin
  if (Dst = nil) or Dst.Empty then
    raise EJclGraphicsError.CreateRes(@RsDestinationBitmapEmpty);
  if (Src = nil) or Src.Empty then
    raise EJclGraphicsError.CreateRes(@RsSourceBitmapEmpty);
end;

function CheckSrcRect(Src: TJclBitmap32; const SrcRect: TRect): Boolean;
begin
  Result := False;
  if IsRectEmpty(SrcRect) then
    Exit;
  if (SrcRect.Left < 0) or (SrcRect.Right > Src.Width) or
    (SrcRect.Top < 0) or (SrcRect.Bottom > Src.Height) then
    raise EJclGraphicsError.CreateRes(@RsSourceBitmapInvalid);
  Result := True;
end;
{$ENDIF Bitmap32}

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

    {$IFDEF VCL}if not Target.Empty then{$ENDIF VCL}
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

{$IFDEF Bitmap32}
procedure StretchNearest(Dst: TJclBitmap32; DstRect: TRect;
  Src: TJclBitmap32; SrcRect: TRect; CombineOp: TDrawMode);
var
  SrcW, SrcH, DstW, DstH: Integer;
  MapX, MapY: array of Integer;
  DstX, DstY: Integer;
  R: TRect;
  I, J, Y: Integer;
  P: PColor32;
  MstrAlpha: TColor32;
begin
  // check source and destination
  CheckBitmaps(Dst, Src);
  if not CheckSrcRect(Src, SrcRect) then
    Exit;
  if IsRectEmpty(DstRect) then
    Exit;
  IntersectRect(R, DstRect, Rect(0, 0, Dst.Width, Dst.Height));
  if IsRectEmpty(R) then
    Exit;
  if (CombineOp = dmBlend) and (Src.MasterAlpha = 0) then
    Exit;

  SrcW := SrcRect.Right - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;
  DstX := DstRect.Left;
  DstY := DstRect.Top;

  // check if we actually have to stretch anything
  if (SrcW = DstW) and (SrcH = DstH) then
  begin
    BlockTransfer(Dst, DstX, DstY, Src, SrcRect, CombineOp);
    Exit;
  end;

  // build X coord mapping table
  SetLength(MapX, DstW);
  SetLength(MapY, DstH);

  try
    for I := 0 to DstW - 1 do
      MapX[I] := I * (SrcW) div (DstW) + SrcRect.Left;

    // build Y coord mapping table
    for J := 0 to DstH - 1 do
      MapY[J] := J * (SrcH) div (DstH) + SrcRect.Top;

    // transfer pixels
    case CombineOp of
      dmOpaque:
        for J := R.Top to R.Bottom - 1 do
        begin
          Y := MapY[J - DstY];
          P := Dst.PixelPtr[R.Left, J];
          for I := R.Left to R.Right - 1 do
          begin
            P^ := Src[MapX[I - DstX], Y];
            Inc(P);
          end;
        end;
      dmBlend:
        begin
          MstrAlpha := Src.MasterAlpha;
          if MstrAlpha = 255 then
            for J := R.Top to R.Bottom - 1 do
            begin
              Y := MapY[J - DstY];
              P := Dst.PixelPtr[R.Left, J];
              for I := R.Left to R.Right - 1 do
              begin
                BlendMem(Src[MapX[I - DstX], Y], P^);
                Inc(P);
              end;
            end
          else // Master Alpha is in [1..254] range
            for J := R.Top to R.Bottom - 1 do
            begin
              Y := MapY[J - DstY];
              P := Dst.PixelPtr[R.Left, J];
              for I := R.Left to R.Right - 1 do
              begin
                BlendMemEx(Src[MapX[I - DstX], Y], P^, MstrAlpha);
                Inc(P);
              end;
            end;
      end;
    end;
  finally
    EMMS;
    MapX := nil;
    MapY := nil;
  end;
end;

procedure BlockTransfer(Dst: TJclBitmap32; DstX: Integer; DstY: Integer; Src: TJclBitmap32;
  SrcRect: TRect; CombineOp: TDrawMode);
var
  SrcX, SrcY: Integer;
  S, D: TRect;
  J, N: Integer;
  Ps, Pd: PColor32;
  MstrAlpha: TColor32;
begin
  CheckBitmaps(Src, Dst);
  if CombineOp = dmOpaque then
  begin
    BitBlt(Dst.Handle, DstX, DstY, SrcRect.Right - SrcRect.Left,
      SrcRect.Bottom - SrcRect.Top, Src.Handle, SrcRect.Left, SrcRect.Top,
      SRCCOPY);
    Exit;
  end;

  if Src.MasterAlpha = 0 then
    Exit;

  // clip the rectangles with bitmap boundaries
  SrcX := SrcRect.Left;
  SrcY := SrcRect.Top;
  IntersectRect(S, SrcRect, Rect(0, 0, Src.Width, Src.Height));
  OffsetRect(S, DstX - SrcX, DstY - SrcY);
  IntersectRect(D, S, Rect(0, 0, Dst.Width, Dst.Height));
  if IsRectEmpty(D) then
    Exit;

  MstrAlpha := Src.MasterAlpha;
  N := D.Right - D.Left;

  try
    if MstrAlpha = 255 then
      for J := D.Top to D.Bottom - 1 do
      begin
        Ps := Src.PixelPtr[D.Left + SrcX - DstX, J + SrcY - DstY];
        Pd := Dst.PixelPtr[D.Left, J];
        BlendLine(Ps, Pd, N);
      end
    else
      for J := D.Top to D.Bottom - 1 do
      begin
        Ps := Src.PixelPtr[D.Left + SrcX - DstX, J + SrcY - DstY];
        Pd := Dst.PixelPtr[D.Left, J];
        BlendLineEx(Ps, Pd, N, MstrAlpha);
      end;
  finally
    EMMS;
  end;
end;

procedure StretchTransfer(Dst: TJclBitmap32; DstRect: TRect; Src: TJclBitmap32; SrcRect: TRect;
  StretchFilter: TStretchFilter; CombineOp: TDrawMode);
var
  SrcW, SrcH, DstW, DstH: Integer;
  MapX, MapY: TMappingTable;
  DstX, DstY: Integer;
  R: TRect;
  I, J, X, Y: Integer;
  P: PColor32;
  ClusterX, ClusterY: TCluster;
  C, Wt, Cr, Cg, Cb, Ca: Integer;
  MstrAlpha: TColor32;
begin
  // make compiler happy
  MapX := nil;
  MapY := nil;
  ClusterX := nil;
  ClusterY := nil;

  if StretchFilter = sfNearest then
  begin
    StretchNearest(Dst, DstRect, Src, SrcRect, CombineOp);
    Exit;
  end;

  // check source and destination
  CheckBitmaps(Dst, Src);
  if not CheckSrcRect(Src, SrcRect) then
    Exit;
  if IsRectEmpty(DstRect) then
    Exit;
  IntersectRect(R, DstRect, Rect(0, 0, Dst.Width, Dst.Height));
  if IsRectEmpty(R) then
    Exit;
  if (CombineOp = dmBlend) and (Src.MasterAlpha = 0) then
    Exit;

  SrcW := SrcRect.Right - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;
  DstX := DstRect.Left;
  DstY := DstRect.Top;
  MstrAlpha := Src.MasterAlpha;

  // check if we actually have to stretch anything
  if (SrcW = DstW) and (SrcH = DstH) then
  begin
    BlockTransfer(Dst, DstX, DstY, Src, SrcRect, CombineOp);
    Exit;
  end;

  // mapping tables
  MapX := BuildMappingTable(DstW, SrcRect.Left, SrcW, StretchFilter);
  MapY := BuildMappingTable(DstH, SrcRect.Top, SrcH, StretchFilter);
  try
    ClusterX := nil;
    ClusterY := nil;
    if (MapX = nil) or (MapY = nil) then
      Exit;

    // transfer pixels
    for J := R.Top to R.Bottom - 1 do
    begin
      ClusterY := MapY[J - DstY];
      P := Dst.PixelPtr[R.Left, J];
      for I := R.Left to R.Right - 1 do
      begin
        ClusterX := MapX[I - DstX];

        // reset color accumulators
        Ca := 0;
        Cr := 0;
        Cg := 0;
        Cb := 0;

        // now iterate through each cluster
        for Y := 0 to High(ClusterY) do
          for X := 0 to High(ClusterX) do
          begin
            C := Src[ClusterX[X].Pos, ClusterY[Y].Pos];
            Wt := ClusterX[X].Weight * ClusterY[Y].Weight;
            Inc(Ca, C shr 24 * Wt);
            Inc(Cr, (C and $00FF0000) shr 16 * Wt);
            Inc(Cg, (C and $0000FF00) shr 8 * Wt);
            Inc(Cb, (C and $000000FF) * Wt);
          end;
        Ca := Ca and $00FF0000;
        Cr := Cr and $00FF0000;
        Cg := Cg and $00FF0000;
        Cb := Cb and $00FF0000;
        C := (Ca shl 8) or Cr or (Cg shr 8) or (Cb shr 16);

        // combine it with the background
        case CombineOp of
          dmOpaque:
            P^ := C;
          dmBlend:
            BlendMemEx(C, P^, MstrAlpha);
        end;
        Inc(P);
      end;
    end;
  finally
    EMMS;
    MapX := nil;
    MapY := nil;
  end;
end;
{$ENDIF Bitmap32}

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

{$IFDEF VCL}
{ TODO : remove VCL-dependency by replacing pf24bit by pf32bit }

function GetAntialiasedBitmap(const Bitmap: TBitmap): TBitmap;
var
  Antialias: TBitmap;
  X, Y: Integer;
  Line1, Line2, Line: PJclByteArray;
begin
 Assert(Bitmap <> nil);
 if Bitmap.PixelFormat <> pf24bit then
   Bitmap.PixelFormat := pf24bit;
 Antialias := TBitmap.Create;
 with Bitmap do
 begin
   Antialias.PixelFormat := pf24bit;
   Antialias.Width := Width div 2;
   Antialias.Height := Height div 2;
   for Y := 0 to Antialias.Height - 1 do
   begin
     Line1 := ScanLine[Y * 2];
     Line2 := ScanLine[Y * 2 + 1];
     Line := Antialias.ScanLine[Y];
     for X := 0 to Antialias.Width - 1 do
     begin
       Line[X * 3] := (Integer(Line1[X * 6]) + Integer(Line2[X * 6]) +
         Integer(Line1[X * 6 + 3]) + Integer(Line2[X * 6 + 3])) div 4;
       Line[X * 3 + 1] := (Integer(Line1[X * 6 + 1]) + Integer(Line2[X * 6 + 1]) +
         Integer(Line1[X * 6 + 3 + 1]) + Integer(Line2[X * 6 + 3 + 1])) div 4;
       Line[X * 3 + 2] := (Integer(Line1[X * 6 + 2]) + Integer(Line2[X * 6 + 2]) +
         Integer(Line1[X * 6 + 3 + 2]) + Integer(Line2[X * 6 + 3 + 2])) div 4;
     end;
    end;
  end;
  Result := Antialias;
end;

procedure JPegToBitmap(const FileName: string);
var
  Bitmap: TBitmap;
  JPeg: TJPegImage;
begin
  Bitmap := nil;
  JPeg := nil;
  try
    JPeg := TJPegImage.Create;
    JPeg.LoadFromFile(FileName);
    Bitmap := TBitmap.Create;
    Bitmap.Assign(JPeg);
    Bitmap.SaveToFile(ChangeFileExt(FileName, LoadResString(@RsBitmapExtension)));
  finally
    FreeAndNil(Bitmap);
    FreeAndNil(JPeg);
  end;
end;

procedure BitmapToJPeg(const FileName: string);
var
  Bitmap: TBitmap;
  JPeg: TJPegImage;
begin
  Bitmap := nil;
  JPeg := nil;
  try
    Bitmap := TBitmap.Create;
    Bitmap.LoadFromFile(FileName);
    JPeg := TJPegImage.Create;
    JPeg.Assign(Bitmap);
    JPeg.SaveToFile(ChangeFileExt(FileName, LoadResString(@RsJpegExtension)));
  finally
    FreeAndNil(Bitmap);
    FreeAndNil(JPeg);
  end;
end;
{$ENDIF VCL}

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

{$IFDEF VCL}
procedure GetIconFromBitmap(Icon: TIcon; Bitmap: TBitmap);
var
  IconInfo: TIconInfo;
begin
  with TBitmap.Create do
  try
    Assign(Bitmap);
    if not Transparent then
      TransparentColor := clNone;
    IconInfo.fIcon := True;
    IconInfo.hbmMask := MaskHandle;
    IconInfo.hbmColor := Handle;
    Icon.Handle := CreateIconIndirect(IconInfo);
  finally
    Free;
  end;
end;

const
  rc3_Icon = 1;

type
  PCursorOrIcon = ^TCursorOrIcon;
  TCursorOrIcon = packed record
    Reserved: Word;
    wType: Word;
    Count: Word;
  end;

  PIconRec = ^TIconRec;
  TIconRec = packed record
    Width: Byte;
    Height: Byte;
    Colors: Word;
    Reserved1: Word;
    Reserved2: Word;
    DIBSize: Longint;
    DIBOffset: Longint;
  end;

procedure WriteIcon(Stream: TStream; ColorBitmap, MaskBitmap: HBITMAP; WriteLength: Boolean = False);
var
  MonoInfoSize, ColorInfoSize: DWORD;
  MonoBitsSize, ColorBitsSize: DWORD;
  MonoInfo, MonoBits, ColorInfo, ColorBits: Pointer;
  CI: TCursorOrIcon;
  List: TIconRec;
  Length: Longint;
begin
  FillChar(CI, SizeOf(CI), 0);
  FillChar(List, SizeOf(List), 0);
  GetDIBSizes(MaskBitmap, MonoInfoSize, MonoBitsSize);
  GetDIBSizes(ColorBitmap, ColorInfoSize, ColorBitsSize);
  MonoInfo := nil;
  MonoBits := nil;
  ColorInfo := nil;
  ColorBits := nil;
  try
    MonoInfo := AllocMem(MonoInfoSize);
    MonoBits := AllocMem(MonoBitsSize);
    ColorInfo := AllocMem(ColorInfoSize);
    ColorBits := AllocMem(ColorBitsSize);
    GetDIB(MaskBitmap, 0, MonoInfo^, MonoBits^);
    GetDIB(ColorBitmap, 0, ColorInfo^, ColorBits^);
    if WriteLength then
    begin
      Length := SizeOf(CI) + SizeOf(List) + ColorInfoSize +
        ColorBitsSize + MonoBitsSize;
      Stream.Write(Length, SizeOf(Length));
    end;
    with CI do
    begin
      CI.wType := RC3_ICON;
      CI.Count := 1;
    end;
    Stream.Write(CI, SizeOf(CI));
    with List, PBitmapInfoHeader(ColorInfo)^ do
    begin
      Width := biWidth;
      Height := biHeight;
      Colors := biPlanes * biBitCount;
      DIBSize := ColorInfoSize + ColorBitsSize + MonoBitsSize;
      DIBOffset := SizeOf(CI) + SizeOf(List);
    end;
    Stream.Write(List, SizeOf(List));
    with PBitmapInfoHeader(ColorInfo)^ do
      Inc(biHeight, biHeight); { color height includes mono bits }
    Stream.Write(ColorInfo^, ColorInfoSize);
    Stream.Write(ColorBits^, ColorBitsSize);
    Stream.Write(MonoBits^, MonoBitsSize);
  finally
    FreeMem(ColorInfo, ColorInfoSize);
    FreeMem(ColorBits, ColorBitsSize);
    FreeMem(MonoInfo, MonoInfoSize);
    FreeMem(MonoBits, MonoBitsSize);
  end;
end;

// WriteIcon depends on unit Graphics by use of GetDIBSizes and GetDIB

procedure WriteIcon(Stream: TStream; Icon: HICON; WriteLength: Boolean = False);
var
  IconInfo: TIconInfo;
begin
  if GetIconInfo(Icon, IconInfo) then
  try
    WriteIcon(Stream, IconInfo.hbmColor, IconInfo.hbmMask, WriteLength);
  finally
    DeleteObject(IconInfo.hbmColor);
    DeleteObject(IconInfo.hbmMask);
  end
  else
    RaiseLastOSError;
end;

procedure SaveIconToFile(Icon: HICON; const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    WriteIcon(Stream, Icon, False);
  finally
    Stream.Free;
  end;
end;
{$ENDIF VCL}

{$IFDEF Bitmap32}
procedure Transform(Dst, Src: TJclBitmap32; SrcRect: TRect;
  Transformation: TJclTransformation);
var
  SrcBlend: Boolean;
  C, SrcAlpha: TColor32;
  R, DstRect: TRect;
  Pixels: PColor32Array;
  I, J, X, Y: Integer;

  function GET_S256(X, Y: Integer; out C: TColor32): Boolean;
  var
    flrx, flry, celx, cely: Longword;
    C1, C2, C3, C4: TColor32;
    P: PColor32;
  begin
    flrx := X and $FF;
    flry := Y and $FF;

    X := Sar(X,8);
    Y := Sar(Y,8);

    celx := flrx xor 255;
    cely := flry xor 255;

    if (X >= SrcRect.Left) and (X < SrcRect.Right - 1) and
      (Y >= SrcRect.Top) and (Y < SrcRect.Bottom - 1) then
    begin
      // everything is ok take the four values and interpolate them
      P := Src.PixelPtr[X, Y];
      C1 := P^;
      Inc(P);
      C2 := P^;
      Inc(P, Src.Width);
      C4 := P^;
      Dec(P);
      C3 := P^;
      C := CombineReg(CombineReg(C1, C2, celx), CombineReg(C3, C4, celx), cely);
      Result := True;
    end
    else
    begin
      // (X,Y) coordinate is out of the SrcRect, do not interpolate
      C := 0; // just write something to disable compiler warnings
      Result := False;
    end;
  end;
begin
  SrcBlend := (Src.DrawMode = dmBlend);
  SrcAlpha := Src.MasterAlpha; // store it into a local variable

  // clip SrcRect
  R := SrcRect;
  IntersectRect(SrcRect, R, Rect(0, 0, Src.Width, Src.Height));
  if IsRectEmpty(SrcRect) then
    Exit;

  // clip DstRect
  R := Transformation.GetTransformedBounds(SrcRect);
  IntersectRect(DstRect, R, Rect(0, 0, Dst.Width, Dst.Height));
  if IsRectEmpty(DstRect) then
    Exit;

  try
    if Src.StretchFilter <> sfNearest then
      for J := DstRect.Top to DstRect.Bottom - 1 do
      begin
        Pixels := Dst.ScanLine[J];
        for I := DstRect.Left to DstRect.Right - 1 do
        begin
          Transformation.Transform256(I, J, X, Y);
          if GET_S256(X, Y, C) then
            if SrcBlend then
              BlendMemEx(C, Pixels[I], SrcAlpha)
            else
              Pixels[I] := C;
        end;
      end
    else // nearest filter
      for J := DstRect.Top to DstRect.Bottom - 1 do
      begin
        Pixels := Dst.ScanLine[J];
        for I := DstRect.Left to DstRect.Right - 1 do
        begin
          Transformation.Transform(I, J, X, Y);
          if (X >= SrcRect.Left) and (X < SrcRect.Right) and
            (Y >= SrcRect.Top) and (Y < SrcRect.Bottom) then
          begin
            if SrcBlend then
              BlendMemEx(Src.Pixel[X, Y], Pixels[I], SrcAlpha)
            else
              Pixels[I] := Src.Pixel[X, Y];
          end;
        end;
      end;
  finally
    EMMS;
  end;
  Dst.Changed;
end;

procedure SetBorderTransparent(ABitmap: TJclBitmap32; ARect: TRect);
var
  I: Integer;
begin
  if TestClip(ARect.Left, ARect.Right, ABitmap.Width) and
    TestClip(ARect.Top, ARect.Bottom, ABitmap.Height) then
  begin
    ABitmap.Changing;

    for I := ARect.Left to ARect.Right do
      ABitmap[I, ARect.Top] := ABitmap[I, ARect.Top] and $00FFFFFF;

    for I := ARect.Left to ARect.Right do
      ABitmap[I, ARect.Bottom] := ABitmap[I, ARect.Bottom] and $00FFFFFF;

    if ARect.Bottom > ARect.Top + 1 then
      for I := ARect.Top + 1 to ARect.Bottom - 1 do
      begin
        ABitmap[ARect.Left, I] := ABitmap[ARect.Left, I] and $00FFFFFF;
        ABitmap[ARect.Right, I] := ABitmap[ARect.Right, I] and $00FFFFFF;
      end;

    ABitmap.Changed;
  end;
end;
{$ENDIF Bitmap32}

{$IFDEF VCL}
function CreateRegionFromBitmap(Bitmap: TBitmap; RegionColor: TColor;
  RegionBitmapMode: TJclRegionBitmapMode): HRGN;
var
  FBitmap: TBitmap;
  X, Y: Integer;
  StartX: Integer;
  Region: HRGN;
begin
  Result := 0;

  if Bitmap = nil then
    EJclGraphicsError.CreateRes(@RsNoBitmapForRegion);

  if (Bitmap.Width = 0) or (Bitmap.Height = 0) then
    Exit;

  FBitmap := TBitmap.Create;
  try
    FBitmap.Assign(Bitmap);

    for Y := 0 to FBitmap.Height - 1 do
    begin
      X := 0;
      while X < FBitmap.Width do
      begin

        if RegionBitmapMode = rmExclude then
        begin
          while FBitmap.Canvas.Pixels[X,Y] = RegionColor do
          begin
            Inc(X);
            if X = FBitmap.Width then
              Break;
          end;
        end
        else
        begin
          while FBitmap.Canvas.Pixels[X,Y] <> RegionColor do
          begin
            Inc(X);
            if X = FBitmap.Width then
              Break;
          end;
        end;

        if X = FBitmap.Width then
          Break;

        StartX := X;
        if RegionBitmapMode = rmExclude then
        begin
          while FBitmap.Canvas.Pixels[X,Y] <> RegionColor do
          begin
            if X = FBitmap.Width then
              Break;
            Inc(X);
          end;
        end
        else
        begin
          while FBitmap.Canvas.Pixels[X,Y] = RegionColor do
          begin
            if X = FBitmap.Width then
              Break;
            Inc(X);
          end;
        end;

        if Result = 0 then
          Result := CreateRectRgn(StartX, Y, X, Y + 1)
        else
        begin
          Region := CreateRectRgn(StartX, Y, X, Y + 1);
          if Region <> 0 then
          begin
            CombineRgn(Result, Result, Region, RGN_OR);
            DeleteObject(Region);
          end;
        end;
      end;
    end;
  finally
    FBitmap.Free;
  end;
end;

procedure ScreenShot(bm: TBitmap; Left, Top, Width, Height: Integer; Window: THandle); overload;
var
  WinDC: HDC;
  Pal: TMaxLogPalette;
begin
  bm.Width := Width;
  bm.Height := Height;

  // Get the HDC of the window...
  WinDC := GetDC(Window);
  if WinDC = 0 then
    raise EJclGraphicsError.CreateRes(@RsNoDeviceContextForWindow);

  // Palette-device?
  if (GetDeviceCaps(WinDC, RASTERCAPS) and RC_PALETTE) = RC_PALETTE then
  begin
    FillChar(Pal, SizeOf(TMaxLogPalette), #0);  // fill the structure with zeros
    Pal.palVersion := $300;                     // fill in the palette version

    // grab the system palette entries...
    Pal.palNumEntries := GetSystemPaletteEntries(WinDC, 0, 256, Pal.palPalEntry);
    if Pal.PalNumEntries <> 0 then
      bm.Palette := CreatePalette(PLogPalette(@Pal)^);
  end;

  // copy from the screen to our bitmap...
  BitBlt(bm.Canvas.Handle, 0, 0, Width, Height, WinDC, Left, Top, SRCCOPY);

  ReleaseDC(Window, WinDC);        // finally, relase the DC of the window
end;

procedure ScreenShot(bm: TBitmap; IncludeTaskBar: Boolean = True); overload;
var
  R: TRect;
begin
  if IncludeTaskBar then
  begin
    R.Left := 0;
    R.Top := 0;
    R.Right := GetSystemMetrics(SM_CXSCREEN);
    R.Bottom := GetSystemMetrics(SM_CYSCREEN);
  end
  else
    SystemParametersInfo(SPI_GETWORKAREA, 0, @R, 0);
  ScreenShot(bm, R.Left, R.Top, R.Right, R.Bottom, HWND_DESKTOP);
end;

function MapWindowRect(hWndFrom, hWndTo: THandle; ARect:TRect):TRect;
begin
  MapWindowPoints(hWndFrom, hWndTo, ARect, 2);
  Result := ARect;
end;
{$ENDIF VCL}

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

{$IFDEF VCL}
//=== { TJclDesktopCanvas } ==================================================

constructor TJclDesktopCanvas.Create;
begin
  inherited Create;
  FDesktop := GetDC(HWND_DESKTOP);
  Handle := FDesktop;
end;

destructor TJclDesktopCanvas.Destroy;
begin
  Handle := 0;
  ReleaseDC(HWND_DESKTOP, FDesktop);
  inherited Destroy;
end;

//=== { TJclRegionInfo } =====================================================

constructor TJclRegionInfo.Create(Region: TJclRegion);
begin
  inherited Create;
  if Region = nil then
    raise EJclGraphicsError.CreateRes(@RsInvalidRegion);
  FData := nil;
  FDataSize := GetRegionData(Region.Handle, 0, nil);
  GetMem(FData, FDataSize);
  GetRegionData(Region.Handle, FDataSize, FData);
end;

destructor TJclRegionInfo.Destroy;
begin
  if FData <> nil then
    FreeMem(FData);
  inherited Destroy;
end;

function TJclRegionInfo.GetBox: TRect;
begin
  Result := RectAssign(TRgnData(FData^).rdh.rcBound.Left, TRgnData(FData^).rdh.rcBound.Top,
    TRgnData(FData^).rdh.rcBound.Right, TRgnData(FData^).rdh.rcBound.Bottom);
end;

function TJclRegionInfo.GetCount: Integer;
begin
  Result := TRgnData(FData^).rdh.nCount;
end;

function TJclRegionInfo.GetRect(Index: Integer): TRect;
var RectP: PRect;
begin
  if (Index < 0) or (DWORD(Index) >= TRgnData(FData^).rdh.nCount) then
    raise EJclGraphicsError.CreateRes(@RsRegionDataOutOfBound);
  RectP := PRect(PChar(@TRgnData(FData^).Buffer) + (SizeOf(TRect)*Index));
  Result := RectAssign(RectP^.Left, RectP.Top, RectP^.Right, RectP^.Bottom);
end;

//=== { TJclRegion } =========================================================

constructor TJclRegion.Create(RegionHandle: HRGN; OwnsHandle: Boolean = True);
begin
  inherited Create;
  FHandle := RegionHandle;
  FOwnsHandle := OwnsHandle;
  CheckHandle;
  GetBox;
end;

constructor TJclRegion.CreateBitmap(Bitmap: TBitmap; RegionColor: TColor;
  RegionBitmapMode: TJclRegionBitmapMode);
begin
  Create(CreateRegionFromBitmap(Bitmap, RegionColor, RegionBitmapMode), True);
end;

constructor TJclRegion.CreateElliptic(const ARect: TRect);
begin
  Create(CreateEllipticRgnIndirect(ARect), True);
end;

constructor TJclRegion.CreateElliptic(const Top, Left, Bottom, Right: Integer);
begin
  Create(CreateEllipticRgn(Top, Left, Bottom, Right), True);
end;

constructor TJclRegion.CreatePoly(const Points: TDynPointArray; Count: Integer;
  FillMode: TPolyFillMode);
begin
  case FillMode of
    fmAlternate:
      Create(CreatePolygonRgn(Points, Count, ALTERNATE), True);
    fmWinding:
      Create(CreatePolygonRgn(Points, Count, WINDING), True);
  end;
end;

constructor TJclRegion.CreatePolyPolygon(const Points: TDynPointArray;
  const Vertex: TDynIntegerArray; Count: Integer; FillMode: TPolyFillMode);
begin
  case FillMode of
    fmAlternate:
      Create(CreatePolyPolygonRgn(Points, Vertex, Count, ALTERNATE), True);
    fmWinding:
      Create(CreatePolyPolygonRgn(Points, Vertex, Count, WINDING), True);
  end;
end;

constructor TJclRegion.CreateRect(const ARect: TRect; DummyForBCB: Boolean = False);
begin
  Create(CreateRectRgnIndirect(ARect), True);
end;

constructor TJclRegion.CreateRect(const Top, Left, Bottom, Right: Integer; DummyForBCB: Byte = 0);
begin
  Create(CreateRectRgn(Top, Left, Bottom, Right), True);
end;

constructor TJclRegion.CreateRoundRect(const ARect: TRect; CornerWidth,
  CornerHeight: Integer);
begin
  Create(CreateRoundRectRgn(ARect.Top, ARect.Left, ARect.Bottom, ARect.Right,
    CornerWidth, CornerHeight), True);
end;

constructor TJclRegion.CreateRoundRect(const Top, Left, Bottom, Right, CornerWidth,
  CornerHeight: Integer);
begin
  Create(CreateRoundRectRgn(Top, Left, Bottom, Right, CornerWidth, CornerHeight), True);
end;

constructor TJclRegion.CreatePath(Canvas: TCanvas);
begin
  Create(PathToRegion(Canvas.Handle), True);
end;

constructor TJclRegion.CreateRegionInfo(RegionInfo: TJclRegionInfo);
begin
  if RegionInfo = nil then
    raise EJclGraphicsError.CreateRes(@RsInvalidRegionInfo);
  Create(ExtCreateRegion(nil,RegionInfo.FDataSize,TRgnData(RegionInfo.FData^)), True);
end;

constructor TJclRegion.CreateMapWindow(InitialRegion: TJclRegion; hWndFrom, hWndTo: THandle);
var
  RectRegion: HRGN;
  CurrentRegionInfo : TJclRegionInfo;
  SimpleRect: TRect;
  Index:integer;
begin
  Create(CreateRectRgn(0, 0, 0, 0), True);
  if (hWndFrom <> 0) or (hWndTo <> 0 ) then
  begin
    CurrentRegionInfo := InitialRegion.GetRegionInfo;
    try
      for Index := 0 to CurrentRegionInfo.Count-1 do
      begin
        SimpleRect := CurrentRegionInfo.Rectangles[Index];
        SimpleRect := MapWindowRect(hWndFrom,hWndTo,SimpleRect);
        RectRegion := CreateRectRgnIndirect(SimpleRect);
        if RectRegion <> 0 then
        begin
          CombineRgn(Handle, Handle, RectRegion, RGN_OR);
          DeleteObject(RectRegion);
        end;
      end;
    finally
      CurrentRegionInfo.Free;
      GetBox;
    end;
  end;
end;

constructor TJclRegion.CreateMapWindow(InitialRegion: TJclRegion;
  ControlFrom, ControlTo: TWinControl);
begin
  CreateMapWindow(InitialRegion,ControlFrom.Handle,ControlTo.Handle);
end;

destructor TJclRegion.Destroy;
begin
  if FOwnsHandle and (FHandle <> 0) then
    DeleteObject(FHandle);
  inherited Destroy;
end;

procedure TJclRegion.CheckHandle;
begin
  if FHandle = 0 then
  begin
    if FOwnsHandle then
      raise EJclWin32Error.CreateRes(@RsRegionCouldNotCreated)
    else
      raise EJclGraphicsError.CreateRes(@RsInvalidHandleForRegion);
  end;
end;

procedure TJclRegion.Combine(DestRegion, SrcRegion: TJclRegion;
  CombineOp: TJclRegionCombineOperator);
begin
  case CombineOp of
    coAnd:
      FRegionType := CombineRgn(DestRegion.Handle, SrcRegion.Handle, FHandle, RGN_AND);
    coOr:
      FRegionType := CombineRgn(DestRegion.Handle, SrcRegion.Handle, FHandle, RGN_OR);
    coDiff:
      FRegionType := CombineRgn(DestRegion.Handle, SrcRegion.Handle, FHandle, RGN_DIFF);
    coXor:
      FRegionType := CombineRgn(DestRegion.Handle, SrcRegion.Handle, FHandle, RGN_XOR);
  end;
end;

procedure TJclRegion.Combine(SrcRegion: TJclRegion; CombineOp: TJclRegionCombineOperator);
begin
  case CombineOp of
    coAnd:
      FRegionType := CombineRgn(FHandle, SrcRegion.Handle, FHandle, RGN_AND);
    coOr:
      FRegionType := CombineRgn(FHandle, SrcRegion.Handle, FHandle, RGN_OR);
    coDiff:
      FRegionType := CombineRgn(FHandle, SrcRegion.Handle, FHandle, RGN_DIFF);
    coXor:
      FRegionType := CombineRgn(FHandle, SrcRegion.Handle, FHandle, RGN_XOR);
  end;
end;

procedure TJclRegion.Clip(Canvas: TCanvas);
begin
  FRegionType := SelectClipRgn(Canvas.Handle, FHandle);
end;

function TJclRegion.Equals(CompareRegion: TJclRegion): Boolean;
begin
  Result := EqualRgn(CompareRegion.Handle, FHandle);
end;

function TJclRegion.GetHandle: HRGN;
begin
  Result := FHandle;
end;

procedure TJclRegion.Fill(Canvas: TCanvas);
begin
  FillRgn(Canvas.Handle, FHandle, Canvas.Brush.Handle);
end;

procedure TJclRegion.FillGradient(Canvas: TCanvas; ColorCount: Integer;
  StartColor, EndColor: TColor; ADirection: TGradientDirection);
begin
  SelectClipRgn(Canvas.Handle,FHandle);
  {$IFDEF VisualCLX}JclQGraphics{$ELSE}JclGraphics{$ENDIF}.FillGradient(Canvas.Handle, Box, ColorCount, StartColor, EndColor, ADirection);
end;

procedure TJclRegion.Frame(Canvas: TCanvas; FrameWidth, FrameHeight: Integer);
begin
  FrameRgn(Canvas.Handle, FHandle, Canvas.Brush.Handle, FrameWidth, FrameHeight);
end;

function TJclRegion.GetBox: TRect;
begin
  FRegionType := GetRgnBox(FHandle, FBoxRect);
  Result := FBoxRect;
end;

function TJclRegion.GetRegionType: TJclRegionKind;
begin
  case FRegionType of
    NULLREGION:
      Result := rkNull;
    SIMPLEREGION:
      Result := rkSimple;
    COMPLEXREGION:
      Result := rkComplex;
  else
    Result := rkError;
  end;
end;

procedure TJclRegion.Invert(Canvas: TCanvas);
begin
  InvertRgn(Canvas.Handle, FHandle);
end;

procedure TJclRegion.Offset(X, Y: Integer);
begin
  FRegionType := OffsetRgn(FHandle, X, Y);
end;

procedure TJclRegion.Paint(Canvas: TCanvas);
begin
  PaintRgn(Canvas.Handle, FHandle);
end;

function TJclRegion.PointIn(X, Y: Integer): Boolean;
begin
  Result := PtInRegion(FHandle, X, Y);
end;

function TJclRegion.PointIn(const Point: TPoint): Boolean;
begin
  Result := PtInRegion(FHandle, Point.X, Point.Y);
end;

function TJclRegion.RectIn(const ARect: TRect): Boolean;
begin
  Result := RectInRegion(FHandle, ARect);
end;

function TJclRegion.RectIn(Top, Left, Bottom, Right: Integer): Boolean;
begin
  Result := RectInRegion(FHandle, RectAssign(Left, Top, Right, Bottom));
end;

{ Documentation Info (from MSDN): After a successful call to SetWindowRgn, the system owns
                                  the region specified by the region handle hRgn. The system does
                                  not make a copy of the region. Thus, you should not make any
                                  further function calls with this region handle. In particular,
                                  do not delete this region handle. The system deletes the region
                                  handle when it no longer needed. }

procedure TJclRegion.SetWindow(Window: THandle; Redraw: Boolean);
begin
  if SetWindowRgn(Window, FHandle, Redraw) <> 0 then
    FOwnsHandle := False;  // Make sure that we do not release the Handle. If we didn't own it before
                           // please take care that the owner doesn't release it.
end;

function TJclRegion.Copy: TJclRegion;
begin
  Result := TJclRegion.CreateRect(0, 0, 0, 0, 0); // (rom) call correct overloaded constructor for BCB
  CombineRgn(Result.Handle, FHandle, 0, RGN_COPY);
  Result.GetBox;
end;

function TJclRegion.GetRegionInfo: TJclRegionInfo;
begin
  Result := TJclRegionInfo.Create(Self);
end;
{$ENDIF VCL}

{$IFDEF Bitmap32}
//=== { TJclThreadPersistent } ===============================================

constructor TJclThreadPersistent.Create;
begin
  inherited Create;
  {$IFDEF VCL}
  InitializeCriticalSection(FLock);
  {$ELSE ~VCL}
  FLock := TCriticalSection.Create;
  {$ENDIF ~VCL}
end;

destructor TJclThreadPersistent.Destroy;
begin
  {$IFDEF VCL}
  DeleteCriticalSection(FLock);
  {$ELSE ~VCL}
  FLock.Free;
  {$ENDIF ~VCL}
  inherited Destroy;
end;

procedure TJclThreadPersistent.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TJclThreadPersistent.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TJclThreadPersistent.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJclThreadPersistent.EndUpdate;
begin
  Assert(FUpdateCount > 0, LoadResString(@RsAssertUnpairedEndUpdate));
  Dec(FUpdateCount);
end;

procedure TJclThreadPersistent.Lock;
begin
  InterlockedIncrement(FLockCount);
  {$IFDEF VCL}
  EnterCriticalSection(FLock);
  {$ELSE ~VCL}
  FLock.Enter;
  {$ENDIF ~VCL}
end;

procedure TJclThreadPersistent.Unlock;
begin
  {$IFDEF VCL}
  LeaveCriticalSection(FLock);
  {$ELSE ~VCL}
  FLock.Leave;
  {$ENDIF ~VCL}
  InterlockedDecrement(FLockCount);
end;

//=== { TJclCustomMap } ======================================================

procedure TJclCustomMap.Delete;
begin
  SetSize(0, 0);
end;

function TJclCustomMap.Empty: Boolean;
begin
  Result := (Width = 0) or (Height = 0);
end;

procedure TJclCustomMap.SetHeight(NewHeight: Integer);
begin
  SetSize(Width, NewHeight);
end;

procedure TJclCustomMap.SetSize(NewWidth, NewHeight: Integer);
begin
  FWidth := NewWidth;
  FHeight := NewHeight;
end;

procedure TJclCustomMap.SetSize(Source: TPersistent);
var
  WidthInfo, HeightInfo: PPropInfo;
begin
  if Source is TJclCustomMap then
    SetSize(TJclCustomMap(Source).Width, TJclCustomMap(Source).Height)
  else
  if Source is TGraphic then
    SetSize(TGraphic(Source).Width, TGraphic(Source).Height)
  else
  if Source = nil then
    SetSize(0, 0)
  else
  begin
    WidthInfo := GetPropInfo(Source, 'Width', [tkInteger]);
    HeightInfo := GetPropInfo(Source, 'Height', [tkInteger]);
    if Assigned(WidthInfo) and Assigned(HeightInfo) then
      SetSize(GetOrdProp(Source, WidthInfo), GetOrdProp(Source, HeightInfo))
    else
      raise EJclGraphicsError.CreateResFmt(@RsMapSizeFmt,[Source.ClassName]);
  end;
end;

procedure TJclCustomMap.SetWidth(NewWidth: Integer);
begin
  SetSize(NewWidth, Height);
end;

//=== { TJclBitmap32 } =======================================================

constructor TJclBitmap32.Create;
begin
  inherited Create;

  FResetAlphaOnAssign := True;

  FillChar(FBitmapInfo, SizeOf(TBitmapInfo), #0);
  with FBitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
  end;
  FOuterColor := $00000000;  // by default as full transparency black
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  {$IFDEF VCL}
  FFont.OwnerCriticalSection := @FLock;
  {$ENDIF VCL}
  FMasterAlpha := $FF;
  FPenColor := clWhite32;
  FStippleStep := 1;
end;

destructor TJclBitmap32.Destroy;
begin
  Lock;
  try
    FFont.Free;
    SetSize(0, 0);
  finally
    Unlock;
  end;
  inherited Destroy;
end;

procedure TJclBitmap32.SetSize(NewWidth, NewHeight: Integer);
begin
  if NewWidth <= 0 then
    NewWidth := 0;
  if NewHeight <= 0 then
    NewHeight := 0;
  if (NewWidth = Width) and (NewHeight = Height) then
    Exit;

  Changing;

  try
    if FHDC <> 0 then
      DeleteDC(FHDC);
    if FHandle <> 0 then
      DeleteObject(FHandle);
    FBits := nil;
    FWidth := 0;
    FHeight := 0;
    if (NewWidth > 0) and (NewHeight > 0) then
    begin
      with FBitmapInfo.bmiHeader do
      begin
        biWidth := NewWidth;
        biHeight := -NewHeight;
      end;
      FHandle := CreateDIBSection(0, FBitmapInfo, DIB_RGB_COLORS, Pointer(FBits), 0, 0);
      if FBits = nil then
        raise EJclGraphicsError.CreateRes(@RsDibHandleAllocation);

      FHDC := CreateCompatibleDC(0);
      if FHDC = 0 then
      begin
        DeleteObject(FHandle);
        FHandle := 0;
        FBits := nil;
        raise EJclGraphicsError.CreateRes(@RsCreateCompatibleDc);
      end;

      if SelectObject(FHDC, FHandle) = 0 then
      begin
        DeleteDC(FHDC);
        DeleteObject(FHandle);
        FHDC := 0;
        FHandle := 0;
        FBits := nil;
        raise EJclGraphicsError.CreateRes(@RsSelectObjectInDc);
      end;

      FWidth := NewWidth;
      FHeight := NewHeight;
    end;

  finally
    Changed;
  end;
end;

function TJclBitmap32.Empty: Boolean;
begin
  Result := (FHandle = 0);
end;

procedure TJclBitmap32.Clear;
begin
  Clear(clBlack32);
end;

procedure TJclBitmap32.Clear(FillColor: TColor32);
begin
  if Empty then
    Exit;
  Changing;
  FillLongword(Bits[0], Width * Height, FillColor);
  Changed;
end;

procedure TJclBitmap32.Delete;
begin
  Changing;
  SetSize(0, 0);
  Changed;
end;

procedure TJclBitmap32.Assign(Source: TPersistent);
var
  Canvas: TCanvas;
  Picture: TPicture;

  procedure AssignFromBitmap(SrcBmp: TBitmap);
  begin
    SetSize(SrcBmp.Width, SrcBmp.Height);
    if Empty then
      Exit;
    BitBlt(Handle, 0, 0, Width, Height, SrcBmp.Canvas.Handle, 0, 0, SRCCOPY);
    if ResetAlphaOnAssign then
      ResetAlpha;
  end;

begin
  Changing;
  BeginUpdate;
  try
    if Source = nil then
    begin
      SetSize(0, 0);
      Exit;
    end
    else
    if Source is TJclBitmap32 then
    begin
      SetSize(TJclBitmap32(Source).Width, TJclBitmap32(Source).Height);
      Move(TJclBitmap32(Source).Bits[0], Bits[0], Width * Height * 4);
      Exit;
    end
    else
    if Source is TBitmap then
    begin
      AssignFromBitmap(TBitmap(Source));
      Exit;
    end
    else
    if Source is TPicture then
    begin
      with TPicture(Source) do
      begin
        if TPicture(Source).Graphic is TBitmap then
          AssignFromBitmap(TBitmap(TPicture(Source).Graphic))
        else
        begin
          // icons, metafiles etc...
          SetSize(TPicture(Source).Graphic.Width, TPicture(Source).Graphic.Height);
          if Empty then
            Exit;
          Canvas := TCanvas.Create;
          try
            Canvas.Handle := Self.Handle;
            TJclGraphicAccess(Graphic).Draw(Canvas, Rect(0, 0, Width, Height));
            if ResetAlphaOnAssign then
              ResetAlpha;
          finally
            Canvas.Free;
          end;
        end;
      end;
      Exit;
    end
    else
    if Source is TClipboard then
    begin
      Picture := TPicture.Create;
      try
        Picture.Assign(TClipboard(Source));
        SetSize(Picture.Width, Picture.Height);
        if Empty then
          Exit;
        Canvas := TCanvas.Create;
        try
          Canvas.Handle := Self.Handle;
          TJclGraphicAccess(Picture.Graphic).Draw(Canvas, Rect(0, 0, Width, Height));
          if ResetAlphaOnAssign then
            ResetAlpha;
        finally
          Canvas.Free;
        end;
      finally
        Picture.Free;
      end;
      Exit;
    end
    else
      inherited Assign(Source); // default handler
  finally;
    EndUpdate;
    Changed;
  end;
end;

procedure TJclBitmap32.AssignTo(Dst: TPersistent);
var
  Bmp: TBitmap;
begin
  if Dst is TPicture then
  begin
    Bmp := TPicture(Dst).Bitmap;
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf32bit;
    Bmp.Width := Width;
    Bmp.Height := Height;
    DrawTo(Bmp.Canvas.Handle, 0, 0);
  end
  else
  if Dst is TBitmap then
  begin
    Bmp := TBitmap(Dst);
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf32bit;
    Bmp.Width := Width;
    Bmp.Height := Height;
    DrawTo(Bmp.Canvas.Handle, 0, 0);
  end
  else
  if Dst is TClipboard then
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.HandleType := bmDIB;
      Bmp.PixelFormat := pf32bit;
      Bmp.Width := Width;
      Bmp.Height := Height;
      DrawTo(Bmp.Canvas.Handle, 0, 0);
      TClipboard(Dst).Assign(Bmp);
    finally
      Bmp.Free;
    end;
  end
  else
    inherited AssignTo(Dst);
end;

procedure TJclBitmap32.SetPixel(X, Y: Integer; Value: TColor32);
begin
  Bits[X + Y * Width] := Value;
end;

procedure TJclBitmap32.SetPixelS(X, Y: Integer; Value: TColor32);
begin
  if (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
    Bits[X + Y * Width] := Value;
end;

function TJclBitmap32.GetScanLine(Y: Integer): PColor32Array;
begin
  Result := @Bits[Y * FWidth];
end;

function TJclBitmap32.GetPixel(X, Y: Integer): TColor32;
begin
  Result := Bits[X + Y * Width];
end;

function TJclBitmap32.GetPixelS(X, Y: Integer): TColor32;
begin
  if (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
    Result := Bits[X + Y * Width]
  else
    Result := OuterColor;
end;

function TJclBitmap32.GetPixelPtr(X, Y: Integer): PColor32;
begin
  Result := @Bits[X + Y * Width];
end;

procedure TJclBitmap32.Draw(DstX, DstY: Integer; Src: TJclBitmap32);
begin
  Changing;
  if Src <> nil then
    Src.DrawTo(Self, DstX, DstY);
  Changed;
end;

procedure TJclBitmap32.Draw(DstRect, SrcRect: TRect; Src: TJclBitmap32);
begin
  Changing;
  if Src <> nil then
    Src.DrawTo(Self, DstRect, SrcRect);
  Changed;
end;

procedure TJclBitmap32.Draw(DstRect, SrcRect: TRect; hSrc: HDC);
begin
  if Empty then
    Exit;
  Changing;
  StretchBlt(Handle, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
    DstRect.Bottom - DstRect.Top, hSrc, SrcRect.Left, SrcRect.Top,
    SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, SRCCOPY);
  Changed;
end;

procedure TJclBitmap32.DrawTo(Dst: TJclBitmap32);
begin
  if Empty or Dst.Empty then
    Exit;
  Dst.Changing;
  BlockTransfer(Dst, 0, 0, Self, Rect(0, 0, Width, Height), DrawMode);
  Dst.Changed;
end;

procedure TJclBitmap32.DrawTo(Dst: TJclBitmap32; DstX, DstY: Integer);
begin
  if Empty or Dst.Empty then
    Exit;
  Dst.Changing;
  BlockTransfer(Dst, DstX, DstY, Self, Rect(0, 0, Width, Height), DrawMode);
  Dst.Changed;
end;

procedure TJclBitmap32.DrawTo(Dst: TJclBitmap32; DstRect: TRect);
begin
  if Empty or Dst.Empty then
    Exit;
  Dst.Changing;
  StretchTransfer(Dst, DstRect, Self, Rect(0, 0, Width, Height), StretchFilter, DrawMode);
  Dst.Changed;
end;

procedure TJclBitmap32.DrawTo(Dst: TJclBitmap32; DstRect, SrcRect: TRect);
begin
  if Empty or Dst.Empty then
    Exit;
  Dst.Changing;
  StretchTransfer(Dst, DstRect, Self, SrcRect, StretchFilter, DrawMode);
  Dst.Changed;
end;

procedure TJclBitmap32.DrawTo(hDst: HDC; DstX, DstY: Integer);
begin
  if Empty then
    Exit;
  BitBlt(hDst, DstX, DstY, Width, Height, Handle, 0, 0, SRCCOPY);
end;

procedure TJclBitmap32.DrawTo(hDst: HDC; DstRect, SrcRect: TRect);
begin
  if Empty then
    Exit;
  StretchDIBits(hDst,
    DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top,
    SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top,
    Bits, FBitmapInfo, DIB_RGB_COLORS, SRCCOPY);
end;

procedure TJclBitmap32.ResetAlpha;
var
  I: Integer;
  P: PByte;
begin
  Changing;
  P := Pointer(FBits);
  Inc(P, 3);
  for I := 0 to Width * Height - 1 do
  begin
    P^ := $FF;
    Inc(P, 4)
  end;
  Changed;
end;

function TJclBitmap32.GetPixelB(X, Y: Integer): TColor32;
begin
  // this function should never be used on empty bitmaps !!!
  if X < 0 then
    X := 0
  else
  if X >= Width then
    X := Width - 1;
  if Y < 0 then
    Y := 0
  else
  if Y >= Height then
    Y := Height - 1;
  Result := Bits[X + Y * Width];
end;

procedure TJclBitmap32.SetPixelT(X, Y: Integer; Value: TColor32);
begin
  BlendMem(Value, Bits[X + Y * Width]);
  EMMS;
end;

procedure TJclBitmap32.SetPixelT(var Ptr: PColor32; Value: TColor32);
begin
  BlendMem(Value, Ptr^);
  EMMS;
  Inc(Ptr);
end;

procedure TJclBitmap32.SetPixelTS(X, Y: Integer; Value: TColor32);
begin
  if (X >= 0) and (X < Width) and (Y >= 0) and (Y < Width) then
  begin
    BlendMem(Value, Bits[X + Y * Width]);
    EMMS;
  end;
end;

procedure TJclBitmap32.SET_T256(X, Y: Integer; C: TColor32);
var
  flrx, flry, celx, cely: Longword;
  P: PColor32;
  A: TColor32;
begin
  A := C shr 24;  // opacity

  flrx := X and $FF;
  flry := Y and $FF;

  X := Sar(X,8);
  Y := Sar(Y,8);

  celx := A * GAMMA_TABLE[flrx xor 255];
  cely := GAMMA_TABLE[flry xor 255];
  flrx := A * GAMMA_TABLE[flrx];
  flry := GAMMA_TABLE[flry];

  P := @FBits[X + Y * FWidth];

  CombineMem(C, P^, celx * cely shr 16);
  Inc(P);
  CombineMem(C, P^, flrx * cely shr 16);
  Inc(P, FWidth);
  CombineMem(C, P^, flrx * flry shr 16);
  Dec(P);
  CombineMem(C, P^, celx * flry shr 16);
end;

procedure TJclBitmap32.SET_TS256(X, Y: Integer; C: TColor32);
var
  flrx, flry, celx, cely: Longword;
  P: PColor32;
  A: TColor32;
begin
  if (X < -256) or (Y < -256) then
    Exit;

  flrx := X and $FF;
  flry := Y and $FF;

  X := Sar(X,8);
  Y := Sar(Y,8);

  if (X >= FWidth) or (Y >= FHeight) then
    Exit;

  A := C shr 24;  // opacity

  celx := A * GAMMA_TABLE[flrx xor 255];
  cely := GAMMA_TABLE[flry xor 255];
  flrx := A * GAMMA_TABLE[flrx];
  flry := GAMMA_TABLE[flry];

  P := @FBits[X + Y * FWidth];

  if (X >= 0) and (Y >= 0) and (X < FWidth - 1) and (Height < FHeight - 1) then
  begin
    CombineMem(C, P^, celx * cely shr 16);
    Inc(P);
    CombineMem(C, P^, flrx * cely shr 16);
    Inc(P, FWidth);
    CombineMem(C, P^, flrx * flry shr 16);
    Dec(P);
    CombineMem(C, P^, celx * flry shr 16);
  end
  else
  begin
    if (X >= 0) and (Y >= 0) then
      CombineMem(C, P^, celx * cely shr 16);
    Inc(P);
    if (X < FWidth - 1) and (Y >= 0) then
      CombineMem(C, P^, flrx * cely shr 16);
    Inc(P, FWidth);
    if (X < FWidth - 1) and (Y < FHeight - 1) then
      CombineMem(C, P^, flrx * flry shr 16);
    Dec(P);
    if (X >= 0) and (Y < FHeight - 1) then
      CombineMem(C, P^, celx * flry shr 16);
  end;
end;

procedure TJclBitmap32.SetPixelF(X, Y: Single; Value: TColor32);
begin
  SET_T256(Round(X * 256), Round(Y * 256), Value);
  EMMS;
end;

procedure TJclBitmap32.SetPixelFS(X, Y: Single; Value: TColor32);
begin
  SET_TS256(Round(X * 256), Round(Y * 256), Value);
  EMMS;
end;

procedure TJclBitmap32.SetStipple(NewStipple: TArrayOfColor32);
begin
  FStippleCounter := 0;
  FStipplePattern := Copy(NewStipple, 0, Length(NewStipple));
end;

procedure TJclBitmap32.SetStipple(NewStipple: array of TColor32);
var
  L: Integer;
begin
  FStippleCounter := 0;
  L := High(NewStipple) - Low(NewStipple) + 1;
  SetLength(FStipplePattern, L);
  Move(NewStipple[Low(NewStipple)], FStipplePattern[0], L * SizeOf(TColor32));
end;

function TJclBitmap32.GetStippleColor: TColor32;
var
  L: Integer;
  NextIndex, PrevIndex: Integer;
  PrevWeight: Integer;
begin
  L := Length(FStipplePattern);
  if L = 0 then
  begin
    // no pattern defined, just return something and exit
    Result := clBlack32;
    Exit;
  end;
  while FStippleCounter >= L do
    FStippleCounter := FStippleCounter - L;
  while FStippleCounter < 0 do
    FStippleCounter := FStippleCounter + L;
  PrevIndex := Round(FStippleCounter - 0.5);
  PrevWeight := 255 - Round(255 * (FStippleCounter - PrevIndex));
  if PrevIndex < 0 then
    FStippleCounter := L - 1;
  NextIndex := PrevIndex + 1;
  if NextIndex >= L then
    NextIndex := 0;
  if PrevWeight = 255 then
    Result := FStipplePattern[PrevIndex]
  else
  begin
    Result := CombineReg(
      FStipplePattern[PrevIndex],
      FStipplePattern[NextIndex],
      PrevWeight);
    EMMS;
  end;
  FStippleCounter := FStippleCounter + FStippleStep;
end;

procedure TJclBitmap32.SetStippleStep(Value: Single);
begin
  FStippleStep := Value;
end;

procedure TJclBitmap32.ResetStippleCounter;
begin
  FStippleCounter := 0;
end;

procedure TJclBitmap32.DrawHorzLine(X1, Y, X2: Integer; Value: TColor32);
begin
  FillLongword(Bits[X1 + Y * Width], X2 - X1 + 1, Value);
end;

procedure TJclBitmap32.DrawHorzLineS(X1, Y, X2: Integer; Value: TColor32);
begin
  if (Y >= 0) and (Y < Height) and TestClip(X1, X2, Width) then
    DrawHorzLine(X1, Y, X2, Value);
end;

procedure TJclBitmap32.DrawHorzLineT(X1, Y, X2: Integer; Value: TColor32);
var
  I: Integer;
  P: PColor32;
begin
  if X2 < X1 then
    Exit;
  P := PixelPtr[X1, Y];
  for I := X1 to X2 do
  begin
    BlendMem(Value, P^);
    Inc(P);
  end;
  EMMS;
end;

procedure TJclBitmap32.DrawHorzLineTS(X1, Y, X2: Integer; Value: TColor32);
begin
  if (Y >= 0) and (Y < Height) and TestClip(X1, X2, Width) then
    DrawHorzLineT(X1, Y, X2, Value);
end;

procedure TJclBitmap32.DrawHorzLineTSP(X1, Y, X2: Integer);
var
  I: Integer;
begin
  if Empty then
    Exit;
  if (Y >= 0) and (Y < Height) then
  begin
    if ((X1 < 0) and (X2 < 0)) or ((X1 >= Width) and (X2 >= Width)) then
      Exit;
    if X1 < 0 then
      X1 := 0
    else
    if X1 >= Width then
      X1 := Width - 1;
    if X2 < 0 then
      X2 := 0
    else
    if X2 >= Width then
      X2 := Width - 1;

    if X2 >= X1 then
      for I := X1 to X2 do
        SetPixelT(I, Y, GetStippleColor)
    else
      for I := X2 downto X1 do
        SetPixelT(I, Y, GetStippleColor);
  end;
end;

procedure TJclBitmap32.DrawVertLine(X, Y1, Y2: Integer; Value: TColor32);
var
  I: Integer;
  P: PColor32;
begin
  if Y2 < Y1 then
    Exit;
  P := PixelPtr[X, Y1];
  for I := 0 to Y2 - Y1 do
  begin
    P^ := Value;
    Inc(P, Width);
  end;
end;

procedure TJclBitmap32.DrawVertLineS(X, Y1, Y2: Integer; Value: TColor32);
begin
  if (X >= 0) and (X < Width) and TestClip(Y1, Y2, Height) then
    DrawVertLine(X, Y1, Y2, Value);
end;

procedure TJclBitmap32.DrawVertLineT(X, Y1, Y2: Integer; Value: TColor32);
var
  I: Integer;
  P: PColor32;
begin
  P := PixelPtr[X, Y1];
  for I := Y1 to Y2 do
  begin
    BlendMem(Value, P^);
    Inc(P, Width);
  end;
  EMMS;
end;

procedure TJclBitmap32.DrawVertLineTS(X, Y1, Y2: Integer; Value: TColor32);
begin
  if (X >= 0) and (X < Width) and TestClip(Y1, Y2, Height) then
    DrawVertLineT(X, Y1, Y2, Value);
end;

procedure TJclBitmap32.DrawVertLineTSP(X, Y1, Y2: Integer);
var
  I: Integer;
begin
  if Empty then
    Exit;
  if (X >= 0) and (X < Width) then
  begin
    if ((Y1 < 0) and (Y2 < 0)) or ((Y1 >= Height) and (Y2 >= Height)) then
      Exit;
    if Y1 < 0 then
      Y1 := 0
    else
    if Y1 >= Height then
      Y1 := Height - 1;
    if Y2 < 0 then
      Y2 := 0
    else
    if Y2 >= Height then
      Y2 := Height - 1;

    if Y2 >= Y1 then
      for I := Y1 to Y2 do
        SetPixelT(X, I, GetStippleColor)
    else
      for I := Y2 downto Y1 do
        SetPixelT(X, I, GetStippleColor);
  end;
end;

procedure TJclBitmap32.DrawLine(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Dy, Dx, Sy, Sx, I, Delta: Integer;
  P: PColor32;
begin
  Changing;

  try
    Dx := X2 - X1;
    Dy := Y2 - Y1;

    if Dx > 0 then
      Sx := 1
    else
    if Dx < 0 then
    begin
      Dx := -Dx;
      Sx := -1;
    end
    else // Dx = 0
    begin
      if Dy > 0 then
        DrawVertLine(X1, Y1, Y2 - 1, Value)
      else
      if Dy < 0 then
        DrawVertLine(X1, Y2, Y1 - 1, Value);
      if L then
        Pixel[X2, Y2] := Value;
      Exit;
    end;

    if Dy > 0 then
      Sy := 1
    else
    if Dy < 0 then
    begin
      Dy := -Dy;
      Sy := -1;
    end
    else // Dy = 0
    begin
      if Dx > 0 then
        DrawHorzLine(X1, Y1, X2 - 1, Value)
      else
        DrawHorzLine(X2, Y1, X1 - 1, Value);
      if L then
        Pixel[X2, Y2] := Value;
      Exit;
    end;

    P := PixelPtr[X1, Y1];
    Sy := Sy * Width;

    if Dx > Dy then
    begin
      Delta := Dx shr 1;
      for I := 0 to Dx - 1 do
      begin
        P^ := Value;
        Inc(P, Sx);
        Delta := Delta + Dy;
        if Delta > Dx then
        begin
          Inc(P, Sy);
          Delta := Delta - Dx;
        end;
      end;
    end
    else // Dx < Dy
    begin
      Delta := Dy shr 1;
      for I := 0 to Dy - 1 do
      begin
        P^ := Value;
        Inc(P, Sy);
        Delta := Delta + Dx;
        if Delta > Dy then
        begin
          Inc(P, Sx);
          Delta := Delta - Dy;
        end;
      end;
    end;
    if L then
      P^ := Value;
  finally
    Changed;
  end;
end;

function TJclBitmap32.ClipLine(var X0, Y0, X1, Y1: Integer): Boolean;
type
  TEdge = (Left, Right, Top, Bottom);
  TOutCode = set of TEdge;
var
  Accept, AllDone: Boolean;
  OutCode0, OutCode1, OutCodeOut: TOutCode;
  X, Y: Integer;

  procedure CompOutCode(X, Y: Integer; var Code: TOutCode);
  begin
    Code := [];
    if X < 0 then
      Code := Code + [Left];
    if X >= Width then
      Code := Code + [Right];
    if Y < 0 then
      Code := Code + [Top];
    if Y >= Height then
      Code := Code + [Bottom];
  end;

begin
  Accept := False;
  AllDone := False;
  CompOutCode(X0, Y0, OutCode0);
  CompOutCode(X1, Y1, OutCode1);
  repeat
    if (OutCode0 = []) and (OutCode1 = []) then // trivial accept and exit
    begin
      Accept := True;
      AllDone := True;
    end
    else
    if (OutCode0 * OutCode1) <> [] then
      AllDone := True // trivial reject
    else // calculate intersections
    begin
      if OutCode0 <> [] then
        OutCodeOut := OutCode0
      else
        OutCodeOut := OutCode1;
      X := 0;
      Y := 0;
      if Left in OutCodeOut then
        Y := Y0 + (Y1 - Y0) * (-X0) div (X1 - X0)
      else
      if Right in OutCodeOut then
      begin
        Y := Y0 + (Y1 - Y0) * (Width - 1 - X0) div (X1 - X0);
        X := Width - 1;
      end
      else
      if Top in OutCodeOut then
        X := X0 + (X1 - X0) * (-Y0) div (Y1 - Y0)
      else
      if Bottom in OutCodeOut then
      begin
        X := X0 + (X1 - X0) * (Height - 1 - Y0) div (Y1 - Y0);
        Y := Height - 1;
      end;
      if OutCodeOut = OutCode0 then
      begin
        X0 := X;
        Y0 := Y;
        CompOutCode(X0, Y0, OutCode0);
      end
      else
      begin
        X1 := X;
        Y1 := Y;
        CompOutCode(X1, Y1, OutCode1);
      end;
    end;
  until AllDone;
  Result := Accept;
end;

class function TJclBitmap32.ClipLineF(var X0, Y0, X1, Y1: Single;
  MinX, MaxX, MinY, MaxY: Single): Boolean;
type
  TEdge = (Left, Right, Top, Bottom);
  TOutCode = set of TEdge;
var
  Accept, AllDone: Boolean;
  OutCode0, OutCode1, OutCodeOut: TOutCode;
  X, Y: Single;

  procedure CompOutCode(X, Y: Single; var Code: TOutCode);
  begin
    Code := [];
    if X < MinX then
      Code := Code + [Left];
    if X > MaxX then
      Code := Code + [Right];
    if Y < MinY then
      Code := Code + [Top];
    if Y > MaxY then
      Code := Code + [Bottom];
  end;

begin
  Accept := False;
  AllDone := False;
  CompOutCode(X0, Y0, OutCode0);
  CompOutCode(X1, Y1, OutCode1);
  repeat
    if (OutCode0 = []) and (OutCode1 = []) then // trivial accept and exit
    begin
      Accept := True;
      AllDone := True;
    end
    else
    if (OutCode0 * OutCode1) <> [] then
      AllDone := True // trivial reject
    else // calculate intersections
    begin
      if OutCode0 <> [] then
        OutCodeOut := OutCode0
      else
        OutCodeOut := OutCode1;
      X := 0;
      Y := 0;
      if Left in OutCodeOut then
      begin
        Y := Y0 + (Y1 - Y0) * (MinX - X0) / (X1 - X0);
        X := MinX;
      end
      else
      if Right in OutCodeOut then
      begin
        Y := Y0 + (Y1 - Y0) * (MaxX - X0) / (X1 - X0);
        X := MaxX - 1;
      end
      else
      if Top in OutCodeOut then
      begin
        X := X0 + (X1 - X0) * (MinY - Y0) / (Y1 - Y0);
        Y := MinY;
      end
      else
      if Bottom in OutCodeOut then
      begin
        X := X0 + (X1 - X0) * (MaxY - Y0) / (Y1 - Y0);
        Y := MaxY;
      end;
      if OutCodeOut = OutCode0 then
      begin
        X0 := X;
        Y0 := Y;
        CompOutCode(X0, Y0, OutCode0);
      end
      else
      begin
        X1 := X;
        Y1 := Y;
        CompOutCode(X1, Y1, OutCode1);
      end;
    end;
  until AllDone;
  Result := Accept;
end;

procedure TJclBitmap32.DrawLineS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
begin
  if ClipLine(X1, Y1, X2, Y2) then
    DrawLine(X1, Y1, X2, Y2, Value, L);
end;

procedure TJclBitmap32.DrawLineT(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Dy, Dx, Sy, Sx, I, Delta: Integer;
  P: PColor32;
begin
  Changing;
  try
    Dx := X2 - X1;
    Dy := Y2 - Y1;

    if Dx > 0 then
      Sx := 1
    else
      if Dx < 0 then
    begin
      Dx := -Dx;
      Sx := -1;
    end
    else // Dx = 0
    begin
      if Dy > 0 then
        DrawVertLineT(X1, Y1, Y2 - 1, Value)
      else
      if Dy < 0 then
        DrawVertLineT(X1, Y2, Y1 - 1, Value);
      if L then
        SetPixelT(X2, Y2, Value);
      Exit;
    end;

    if Dy > 0 then
      Sy := 1
    else
    if Dy < 0 then
    begin
      Dy := -Dy;
      Sy := -1;
    end
    else // Dy = 0
    begin
      if Dx > 0 then
        DrawHorzLineT(X1, Y1, X2 - 1, Value)
      else
        DrawHorzLineT(X2, Y1, X1 - 1, Value);
      if L then
        SetPixelT(X2, Y2, Value);
      Exit;
    end;

    P := PixelPtr[X1, Y1];
    Sy := Sy * Width;

    try
      if Dx > Dy then
      begin
        Delta := Dx shr 1;
        for I := 0 to Dx - 1 do
        begin
          BlendMem(Value, P^);
          Inc(P, Sx);
          Delta := Delta + Dy;
          if Delta > Dx then
          begin
            Inc(P, Sy);
            Delta := Delta - Dx;
          end;
        end;
      end
      else // Dx < Dy
      begin
        Delta := Dy shr 1;
        for I := 0 to Dy - 1 do
        begin
          BlendMem(Value, P^);
          Inc(P, Sy);
          Delta := Delta + Dx;
          if Delta > Dy then
          begin
            Inc(P, Sx);
            Delta := Delta - Dy;
          end;
        end;
      end;
      if L then
        BlendMem(Value, P^);
    finally
      EMMS;
    end;
  finally
    Changed;
  end;
end;

procedure TJclBitmap32.DrawLineTS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
begin
  if ClipLine(X1, Y1, X2, Y2) then
    DrawLineT(X1, Y1, X2, Y2, Value, L);
end;

procedure TJclBitmap32.DrawLineF(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean);
var
  N, I: Integer;
  px, py, ex, ey, nx, ny, hyp: Integer;
  A: TColor32;
begin
  Changing;
  try
    px := Round(x1 * 65536);
    py := Round(y1 * 65536);
    ex := Round(x2 * 65536);
    ey := Round(y2 * 65536);
    nx := ex - px;
    ny := ey - py;
    hyp := Round(Hypot(nx, ny));
    if L then
      Inc(hyp, 65536);
    if hyp < 256 then
      Exit;
    N := hyp shr 16;
    if N > 0 then
    begin
      nx := Round(nx / hyp * 65536);
      ny := Round(ny / hyp * 65536);
      for I := 0 to N - 1 do
      begin
        SET_T256(px shr 8, py shr 8, Value);
        px := px + nx;
        py := py + ny;
      end;
    end;
    A := Value shr 24;
    hyp := hyp - N shl 16;
    A := A * Longword(hyp) shl 8 and $FF000000;
    SET_T256((px + ex - nx) shr 9, (py + ey - ny) shr 9, Value and _RGB + A);
  finally
    EMMS;
    Changed;
  end;
end;

procedure TJclBitmap32.DrawLineFS(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean);
var
  N, I: Integer;
  px, py, ex, ey, nx, ny, hyp: Integer;
  A: TColor32;
begin
  if ClipLineF(X1, Y1, X2, Y2, 0, FWidth, 0, FHeight) then
    if (X1 < FWidth - 1) and (X2 < FWidth - 1) and
      (Y1 < FHeight - 1) and (Y2 < FHeight - 1) then
      DrawLineF(X1, Y1, X2, Y2, Value, False)
    else // check every pixel
    begin
      Changing;
      try
        px := Round(x1 * 65536);
        py := Round(y1 * 65536);
        ex := Round(x2 * 65536);
        ey := Round(y2 * 65536);
        nx := ex - px;
        ny := ey - py;
        hyp := Round(Hypot(nx, ny));
        if L then
          Inc(Hyp, 65536);
        if hyp < 256 then
          Exit;
        N := hyp shr 16;
        if N > 0 then
        begin
          nx := Round(nx / hyp * 65536);
          ny := Round(ny / hyp * 65536);
          for I := 0 to N - 1 do
          begin
            SET_TS256(px div 256, py div 256, Value);
            px := px + nx;
            py := py + ny;
          end;
        end;
        A := Value shr 24;
        hyp := hyp - N shl 16;
        A := A * Longword(hyp) shl 8 and $FF000000;
        SET_TS256(Sar(px + ex - nx,9), Sar(py + ey - ny,9), Value and _RGB + A);
      finally
        EMMS;
        Changed;
      end;
    end;
end;

procedure TJclBitmap32.DrawLineFP(X1, Y1, X2, Y2: Single; L: Boolean);
var
  N, I: Integer;
  px, py, ex, ey, nx, ny, hyp: Integer;
  A, C: TColor32;
begin
  Changing;
  try
    px := Round(x1 * 65536);
    py := Round(y1 * 65536);
    ex := Round(x2 * 65536);
    ey := Round(y2 * 65536);
    nx := ex - px;
    ny := ey - py;
    hyp := Round(Hypot(nx, ny));
    if L then
      Inc(hyp, 65536);
    if hyp < 256 then
      Exit;
    N := hyp shr 16;
    if N > 0 then
    begin
      nx := Round(nx / hyp * 65536);
      ny := Round(ny / hyp * 65536);
      for I := 0 to N - 1 do
      begin
        C := GetStippleColor;
        SET_T256(px shr 8, py shr 8, C);
        EMMS;
        px := px + nx;
        py := py + ny;
      end;
    end;
    C := GetStippleColor;
    A := C shr 24;
    hyp := hyp - N shl 16;
    A := A * Longword(hyp) shl 8 and $FF000000;
    SET_T256((px + ex - nx) shr 9, (py + ey - ny) shr 9, C and _RGB + A);
    EMMS;
  finally
    Changed;
  end;
end;

procedure TJclBitmap32.DrawLineFSP(X1, Y1, X2, Y2: Single; L: Boolean);
var
  N, I: Integer;
  px, py, ex, ey, nx, ny, hyp: Integer;
  A, C: TColor32;
begin
  if ClipLineF(X1, Y1, X2, Y2, 0, FWidth, 0, FHeight) then
    if (X1 < FWidth - 1) and (X2 < FWidth - 1) and
      (Y1 < FHeight - 1) and (Y2 < FHeight - 1) then
      DrawLineFP(X1, Y1, X2, Y2, False)
    else // check every pixel
    begin
      Changing;
      try
        px := Round(x1 * 65536);
        py := Round(y1 * 65536);
        ex := Round(x2 * 65536);
        ey := Round(y2 * 65536);
        nx := ex - px;
        ny := ey - py;
        hyp := Round(Hypot(nx, ny));
        if L then
          Inc(hyp, 65536);
        if hyp < 256 then
          Exit;
        N := hyp shr 16;
        if N > 0 then
        begin
          nx := Round(nx / hyp * 65536);
          ny := Round(ny / hyp * 65536);
          for I := 0 to N - 1 do
          begin
            C := GetStippleColor;
            SET_TS256(px div 256, py div 256, C);
            EMMS;
            px := px + nx;
            py := py + ny;
          end;
        end;
        C := GetStippleColor;
        A := C shr 24;
        hyp := hyp - N shl 16;
        A := A * Longword(hyp) shl 8 and $FF000000;
        SET_TS256(Sar(px + ex - nx,9), Sar(py + ey - ny,9), C and _RGB + A);
        EMMS;
      finally
        Changed;
      end;
    end;
end;

procedure TJclBitmap32.DrawLineA(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Dx, Dy, Sx, Sy, D: Integer;
  EC, EA: Word;
  CI: Byte;
  P: PColor32;
begin
  if (X1 = X2) or (Y1 = Y2) then
  begin
    DrawLineT(X1, Y1, X2, Y2, Value, L);
    Exit;
  end;

  Dx := X2 - X1;
  Dy := Y2 - Y1;

  if Dx > 0 then
    Sx := 1
  else
  begin
    Sx := -1;
    Dx := -Dx;
  end;

  if Dy > 0 then
    Sy := 1
  else
  begin
    Sy := -1;
    Dy := -Dy;
  end;

  Changing;
  try
    EC := 0;
    BlendMem(Value, Bits[X1 + Y1 * Width]);

    if Dy > Dx then
    begin
      EA := Dx shl 16 div Dy;
      if not L then
        Dec(Dy);
      while Dy > 0 do
      begin
        Dec(Dy);
        D := EC;
        Inc(EC, EA);
        if EC <= D then
          Inc(X1, Sx);
        Inc(Y1, Sy);
        CI := EC shr 8;
        P := @Bits[X1 + Y1 * Width];
        BlendMemEx(Value, P^, GAMMA_TABLE[CI xor 255]);
        Inc(P, Sx);
        BlendMemEx(Value, P^, GAMMA_TABLE[CI]);
      end;
    end
    else // DY <= DX
    begin
      EA := Dy shl 16 div Dx;
      if not L then
        Dec(Dx);
      while Dx > 0 do
      begin
        Dec(Dx);
        D := EC;
        Inc(EC, EA);
        if EC <= D then
          Inc(Y1, Sy);
        Inc(X1, Sx);
        CI := EC shr 8;
        P := @Bits[X1 + Y1 * Width];
        BlendMemEx(Value, P^, GAMMA_TABLE[CI xor 255]);
        if Sy = 1 then
          Inc(P, Width)
        else
          Dec(P, Width);
        BlendMemEx(Value, P^, GAMMA_TABLE[CI]);
      end;
    end;
  finally
    EMMS;
    Changed;
  end;
end;

procedure TJclBitmap32.DrawLineAS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
begin
  if ClipLine(X1, Y1, X2, Y2) then
    DrawLineA(X1, Y1, X2, Y2, Value, L);
end;

procedure TJclBitmap32.MoveTo(X, Y: Integer);
begin
  RasterX := X;
  RasterY := Y;
end;

procedure TJclBitmap32.LineToS(X, Y: Integer);
begin
  DrawLineS(RasterX, RasterY, X, Y, PenColor, False);
  RasterX := X;
  RasterY := Y;
end;

procedure TJclBitmap32.LineToTS(X, Y: Integer);
begin
  DrawLineTS(RasterX, RasterY, X, Y, PenColor, False);
  RasterX := X;
  RasterY := Y;
end;

procedure TJclBitmap32.LineToAS(X, Y: Integer);
begin
  DrawLineAS(RasterX, RasterY, X, Y, PenColor, False);
  RasterX := X;
  RasterY := Y;
end;

procedure TJclBitmap32.MoveToF(X, Y: Single);
begin
  RasterXF := X;
  RasterYF := Y;
end;

procedure TJclBitmap32.LineToFS(X, Y: Single);
begin
  DrawLineFS(RasterXF, RasterYF, X, Y, PenColor, False);
  RasterXF := X;
  RasterYF := Y;
end;

procedure TJclBitmap32.FillRect(X1, Y1, X2, Y2: Integer; Value: TColor32);
var
  J: Integer;
  P: PColor32Array;
begin
  Changing;
  for J := Y1 to Y2 do
  begin
    P := Pointer(GetScanLine(J));
    FillLongword(P[X1], X2 - X1 + 1, Value);
  end;
  Changed;
end;

procedure TJclBitmap32.FillRectS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
  if TestClip(X1, X2, Width) and TestClip(Y1, Y2, Height) then
    FillRect(X1, Y1, X2, Y2, Value);
end;

procedure TJclBitmap32.FillRectT(X1, Y1, X2, Y2: Integer; Value: TColor32);
var
  I, J: Integer;
  P: PColor32;
  A: Integer;
begin
  A := Value shr 24;
  if A = $FF then
    FillRect(X1, Y1, X2, Y2, Value)
  else
  begin
    Changing;
    try
    for J := Y1 to Y2 do
    begin
      P := GetPixelPtr(X1, J);
      for I := X1 to X2 do
      begin
        CombineMem(Value, P^, A);
        Inc(P);
      end;
    end;
    finally
      EMMS;
      Changed;
    end;
  end;
end;

procedure TJclBitmap32.FillRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
  if TestClip(X1, X2, Width) and TestClip(Y1, Y2, Height) then
    FillRectT(X1, Y1, X2, Y2, Value);
end;

procedure TJclBitmap32.FrameRectS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
  Changing;
  TestSwap(X1, X2);
  TestSwap(Y1, Y2);
  DrawHorzLineS(X1, Y1, X2, Value);
  if Y2 > Y1 then
    DrawHorzLineS(X1, Y2, X2, Value);
  if Y2 > Y1 + 1 then
  begin
    DrawVertLineS(X1, Y1 + 1, Y2 - 1, Value);
    if X2 > X1 then
      DrawVertLineS(X2, Y1 + 1, Y2 - 1, Value);
  end;
  Changed;
end;

procedure TJclBitmap32.FrameRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
  Changing;
  TestSwap(X1, X2);
  TestSwap(Y1, Y2);
  DrawHorzLineTS(X1, Y1, X2, Value);
  if Y2 > Y1 then
    DrawHorzLineTS(X1, Y2, X2, Value);
  if Y2 > Y1 + 1 then
  begin
    DrawVertLineTS(X1, Y1 + 1, Y2 - 1, Value);
    if X2 > X1 then
      DrawVertLineTS(X2, Y1 + 1, Y2 - 1, Value);
  end;
  Changed;
end;

procedure TJclBitmap32.FrameRectTSP(X1, Y1, X2, Y2: Integer);
begin
  Changing;
  TestSwap(X1, X2);
  TestSwap(Y1, Y2);
  DrawHorzLineTSP(X1, Y1, X2);
  if Y2 > Y1 + 1 then
  begin
    DrawVertLineTSP(X2, Y1 + 1, Y2 - 1);
    if X2 > X1 then
      DrawVertLineTSP(X1, Y1 + 1, Y2 - 1);
  end;
  if Y2 > Y1 then
    DrawHorzLineTSP(X1, Y2, X2);
  Changed;
end;

procedure TJclBitmap32.RaiseRectTS(X1, Y1, X2, Y2: Integer; Contrast: Integer);
var
  C1, C2: TColor32;
begin
  Changing;
  try
    if Contrast > 0 then
    begin
      C1 := clWhite32;
      C2 := clBlack32;
    end
    else
    if Contrast < 0 then
    begin
      C1 := clBlack32;
      C2 := clWhite32;
      Contrast := -Contrast;
    end
    else
      Exit;
    Contrast := Clamp(Contrast * 255 div 100);
    C1 := SetAlpha(C1, Contrast);
    C2 := SetAlpha(C2, Contrast);
    TestSwap(X1, X2);
    TestSwap(Y1, Y2);
    DrawHorzLineTS(X1, Y1, X2 - 1, C1);
    DrawHorzLineTS(X1 + 1, Y2, X2, C2);
    DrawVertLineTS(X1, Y1, Y2 - 1, C1);
    DrawVertLineTS(X2, Y1 + 1, Y2, C2);
  finally
    Changed;
  end;
end;

procedure TJclBitmap32.LoadFromStream(Stream: TStream);
var
  B: TBitmap;
begin
  Changing;
  B := TBitmap.Create;
  try
    B.LoadFromStream(Stream);
    Assign(B);
  finally
    B.Free;
    Changed;
  end;
end;

procedure TJclBitmap32.SaveToStream(Stream: TStream);
var
  B: TBitmap;
begin
  B := TBitmap.Create;
  try
    AssignTo(B);
    B.SaveToStream(Stream);
  finally
    B.Free;
  end;
end;

procedure TJclBitmap32.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TGraphic)
    else
      Result := not Empty;
  end;

begin
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, DoWrite);
end;

procedure TJclBitmap32.ReadData(Stream: TStream);
var
  w, h: Integer;
begin
  Changing;
  try
    Stream.ReadBuffer(w, 4);
    Stream.ReadBuffer(h, 4);
    SetSize(w, h);
    Stream.ReadBuffer(FBits[0], FWidth * FHeight * 4);
  finally
    Changed;
  end;
end;

procedure TJclBitmap32.WriteData(Stream: TStream);
begin
  Stream.WriteBuffer(FWidth, 4);
  Stream.WriteBuffer(FHeight, 4);
  Stream.WriteBuffer(FBits[0], FWidth * FHeight * 4);
end;

procedure TJclBitmap32.LoadFromFile(const FileName: string);
var
  P: TPicture;
begin
  P := TPicture.Create;
  try
    P.LoadFromFile(FileName);
    Assign(P);
  finally
    P.Free;
  end;
end;

procedure TJclBitmap32.SaveToFile(const FileName: string);
var
  B: TBitmap;
begin
  B := TBitmap.Create;
  try
    AssignTo(B);
    B.SaveToFile(FileName);
  finally
    B.Free;
  end;
end;

procedure TJclBitmap32.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  FontChanged(Self);
end;

procedure TJclBitmap32.FontChanged(Sender: TObject);
begin
  if FontHandle > 0 then
  begin
    SelectObject(Handle, GetStockObject(SYSTEM_FONT));
    FontHandle := 0;
  end;
end;

procedure TJclBitmap32.UpdateFont;
begin
  if FontHandle = 0 then
  begin
    SelectObject(Handle, Font.Handle);
    SetTextColor(Handle, ColorToRGB(Font.Color));
    SetBkMode(Handle, Windows.TRANSPARENT);
  end;
end;

procedure TJclBitmap32.SetDrawMode(Value: TDrawMode);
begin
  if FDrawMode <> Value then
  begin
    Changing;
    FDrawMode := Value;
    Changed;
  end;
end;

procedure TJclBitmap32.SetMasterAlpha(Value: Byte);
begin
  if FMasterAlpha <> Value then
  begin
    Changing;
    FMasterAlpha := Value;
    Changed;
  end;
end;

procedure TJclBitmap32.SetStretchFilter(Value: TStretchFilter);
begin
  if FStretchFilter <> Value then
  begin
    Changing;
    FStretchFilter := Value;
    Changed;
  end;
end;

function TJclBitmap32.TextExtent(const Text: string): TSize;
begin
  UpdateFont;
  Result.cX := 0;
  Result.cY := 0;
  Windows.GetTextExtentPoint32(Handle, PChar(Text), Length(Text), Result);
end;

procedure TJclBitmap32.TextOut(X, Y: Integer; const Text: string);
begin
  Changing;
  UpdateFont;
  ExtTextOut(Handle, X, Y, 0, nil, PChar(Text), Length(Text), nil);
  Changed;
end;

procedure TJclBitmap32.TextOut(X, Y: Integer; const ClipRect: TRect;
  const Text: string);
begin
  Changing;
  UpdateFont;
  ExtTextOut(Handle, X, Y, ETO_CLIPPED, @ClipRect, PChar(Text), Length(Text), nil);
  Changed;
end;

procedure TJclBitmap32.TextOut(ClipRect: TRect; const Flags: Cardinal;
  const Text: string);
begin
  Changing;
  UpdateFont;
  DrawText(Handle, PChar(Text), Length(Text), ClipRect, Flags);
  Changed;
end;

function TJclBitmap32.TextHeight(const Text: string): Integer;
begin
  Result := TextExtent(Text).cY;
end;

function TJclBitmap32.TextWidth(const Text: string): Integer;
begin
  Result := TextExtent(Text).cX;
end;

procedure TJclBitmap32.RenderText(X, Y: Integer; const Text: string; AALevel: Integer; Color: TColor32);
var
  B, B2: TJclBitmap32;
  Sz: TSize;
  C: TColor32;
  I: Integer;
  P: PColor32;
begin
  AALevel := Constrain(AALevel, 0, 4);
  B := TJclBitmap32.Create;
  try
    if AALevel = 0 then
    begin
      Sz := TextExtent(Text + ' ');
      B.SetSize(Sz.cX, Sz.cY);
      B.Font := Font;
      B.Clear(0);
      B.Font.Color := clWhite;
      B.TextOut(0, 0, Text);
    end
    else
    begin
      B2 := TJclBitmap32.Create;
      try
        B2.SetSize(1, 1); // just need some DC here
        B2.Font := Font;
        B2.Font.Size := Font.Size shl AALevel;
        Sz := B2.TextExtent(Text + ' ');
        Sz.cx := (Sz.cx shr AALevel + 1) shl AALevel;
        B2.SetSize(Sz.cx, Sz.cy);
        B2.Clear(0);
        B2.Font.Color := clWhite;
        B2.TextOut(0, 0, Text);
        B2.StretchFilter := sfLinear;
        B.SetSize(Sz.cx shr AALevel, Sz.cy shr AALevel);
        B.Draw(Rect(0, 0, B.Width, B.Height), Rect(0, 0, B2.Width, B2.Height), B2);
      finally
        B2.Free;
      end;
    end;

    // convert intensity and color to alpha
    B.MasterAlpha := Color shr 24;
    Color := Color and $00FFFFFF;
    P := @B.Bits[0];
    for I := 0 to B.Width * B.Height - 1 do
    begin
      C := P^;
      if C <> 0 then
      begin
        C := P^ shl 24; // transfer blue channel to alpha
        C := C + Color;
        P^ := C;
      end;
      Inc(P);
    end;
    B.DrawMode := dmBlend;

    B.DrawTo(Self, X, Y);
  finally
    B.Free;
  end;
end;

//=== { TJclByteMap } ========================================================

destructor TJclByteMap.Destroy;
begin
  FBytes := nil;
  inherited Destroy;
end;

procedure TJclByteMap.Assign(Source: TPersistent);
begin
  Changing;
  BeginUpdate;
  try
    if Source is TJclByteMap then
    begin
      FWidth := TJclByteMap(Source).Width;
      FHeight := TJclByteMap(Source).Height;
      FBytes := Copy(TJclByteMap(Source).Bytes, 0, FWidth * FHeight);
    end
    else
    if Source is TJclBitmap32 then
      ReadFrom(TJclBitmap32(Source), ckWeightedRGB)
    else
      inherited Assign(Source);
  finally
    EndUpdate;
    Changed;
  end;
end;

procedure TJclByteMap.AssignTo(Dst: TPersistent);
begin
  if Dst is TJclBitmap32 then
    WriteTo(TJclBitmap32(Dst), ckUniformRGB)
  else
    inherited AssignTo(Dst);
end;

procedure TJclByteMap.Clear(FillValue: Byte);
begin
  Changing;
  FillChar(Bytes[0], Width * Height, FillValue);
  Changed;
end;

function TJclByteMap.Empty: Boolean;
begin
  Result := Bytes = nil;
end;

function TJclByteMap.GetValPtr(X, Y: Integer): PByte;
begin
  Result := @Bytes[X + Y * Width];
end;

function TJclByteMap.GetValue(X, Y: Integer): Byte;
begin
  Result := Bytes[X + Y * Width];
end;

procedure TJclByteMap.ReadFrom(Source: TJclBitmap32; Conversion: TConversionKind);
var
  W, H, I, N: Integer;
  SrcC: PColor32;
  SrcB, DstB: PByte;
  Value: TColor32;
begin
  Changing;
  BeginUpdate;
  try
    SetSize(Source.Width, Source.Height);
    if Empty then
      Exit;

    W := Source.Width;
    H := Source.Height;
    N := W * H - 1;
    SrcC := Source.PixelPtr[0, 0];
    SrcB := Pointer(SrcC);
    DstB := @Bytes[0];
    case Conversion of
      ckRed:
        begin
          Inc(SrcB, 2);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB);
            Inc(SrcB, 4);
          end;
        end;
      ckGreen:
        begin
          Inc(SrcB, 1);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB);
            Inc(SrcB, 4);
          end;
        end;
      ckBlue:
        begin
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB);
            Inc(SrcB, 4);
          end;
        end;
      ckAlpha:
        begin
          Inc(SrcB, 3);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB);
            Inc(SrcB, 4);
          end;
        end;
      ckUniformRGB:
        begin
          for I := 0 to N do
          begin
            Value := SrcC^;
            Value := (Value and $00FF0000) shr 16 + (Value and $0000FF00) shr 8 +
              (Value and $000000FF);
            Value := Value div 3;
            DstB^ := Value;
            Inc(DstB);
            Inc(SrcC);
          end;
        end;
      ckWeightedRGB:
        begin
          for I := 0 to N do
          begin
            DstB^ := Intensity(SrcC^);
            Inc(DstB);
            Inc(SrcC);
          end;
        end;
    end;
  finally
    EndUpdate;
    Changed;
  end;
end;

procedure TJclByteMap.SetValue(X, Y: Integer; Value: Byte);
begin
  Bytes[X + Y * Width] := Value;
end;

procedure TJclByteMap.SetSize(NewWidth, NewHeight: Integer);
begin
  Changing;
  inherited SetSize(NewWidth, NewHeight);
  SetLength(FBytes, Width * Height);
  Changed;
end;

procedure TJclByteMap.WriteTo(Dest: TJclBitmap32; Conversion: TConversionKind);
var
  W, H, I, N: Integer;
  DstC: PColor32;
  DstB, SrcB: PByte;
begin
  Dest.Changing;
  Dest.BeginUpdate;
  try
    Dest.SetSize(Width, Height);
    if Empty then
      Exit;

    W := Width;
    H := Height;
    N := W * H - 1;
    DstC := Dest.PixelPtr[0, 0];
    DstB := Pointer(DstC);
    SrcB := @Bytes[0];
    case Conversion of
      ckRed:
        begin
          Inc(DstB, 2);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB, 4);
            Inc(SrcB);
          end;
        end;
      ckGreen:
        begin
          Inc(DstB, 1);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB, 4);
            Inc(SrcB);
          end;
        end;
      ckBlue:
        begin
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB, 4);
            Inc(SrcB);
          end;
        end;
      ckAlpha:
        begin
          Inc(DstB, 3);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB, 4);
            Inc(SrcB);
          end;
        end;
      ckUniformRGB, ckWeightedRGB:
        begin
          for I := 0 to N do
          begin
            DstC^ := Gray32(SrcB^, $FF);
            Inc(DstC);
            Inc(SrcB);
          end;
        end;
    end;
  finally
    Dest.EndUpdate;
    Dest.Changed;
  end;
end;

procedure TJclByteMap.WriteTo(Dest: TJclBitmap32; const Palette: TPalette32);
var
  W, H, I, N: Integer;
  DstC: PColor32;
  SrcB: PByte;
begin
  Dest.Changing;
  Dest.BeginUpdate;
  try
    Dest.SetSize(Width, Height);
    if Empty then
      Exit;

    W := Width;
    H := Height;
    N := W * H - 1;
    DstC := Dest.PixelPtr[0, 0];
    SrcB := @Bytes[0];

    for I := 0 to N do
    begin
      DstC^ := Palette[SrcB^];
      Inc(DstC);
      Inc(SrcB);
    end;
  finally
    Dest.EndUpdate;
    Dest.Changed;
  end;
end;
{$ENDIF Bitmap32}

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

{$IFDEF Bitmap32}
procedure PolylineTS(Bitmap: TJclBitmap32; const Points: TDynPointArray;
  Color: TColor32);
var
  I, L: Integer;
  DoAlpha: Boolean;
begin
  DoAlpha := Color and $FF000000 <> $FF000000;
  L := Length(Points);
  if L < 2 then
    Exit;

  Bitmap.Changing;
  Bitmap.BeginUpdate;
  with Points[L - 1] do
    Bitmap.MoveTo(X, Y);
  Bitmap.PenColor := Color;
  if DoAlpha then
    for I := 0 to L - 1 do
      with Points[I] do
        Bitmap.LineToTS(X, Y)
  else
    for I := 0 to L - 1 do
      with Points[I] do
        Bitmap.LineToS(X, Y);
  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure PolyLineAS(Bitmap: TJclBitmap32; const Points: TDynPointArray;
  Color: TColor32);
var
  I, L: Integer;
begin
  L := Length(Points);
  if L < 2 then
    Exit;
  Bitmap.Changing;
  Bitmap.BeginUpdate;
  with Points[L - 1] do
    Bitmap.MoveTo(X, Y);
  Bitmap.PenColor := Color;
  for I := 0 to L - 1 do
    with Points[I] do
      Bitmap.LineToAS(X, Y);
  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure PolylineFS(Bitmap: TJclBitmap32; const Points: TDynPointArrayF;
  Color: TColor32);
var
  I, L: Integer;
begin
  L := Length(Points);
  if L < 2 then
    Exit;
  Bitmap.Changing;
  Bitmap.BeginUpdate;
  with Points[L - 1] do
    Bitmap.MoveToF(X, Y);
  Bitmap.PenColor := Color;
  for I := 0 to L - 1 do
    with Points[I] do
      Bitmap.LineToFS(X, Y);
  Bitmap.EndUpdate;
  Bitmap.Changed;
end;
{$ENDIF Bitmap32}

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
{$IFDEF Bitmap32}

procedure FillLines(Bitmap: TJclBitmap32; BaseY: Integer;
  const ScanLines: TScanLines; Color: TColor32);
var
  I, J, L: Integer;
  Left, Right: Integer;
  DoAlpha: Boolean;
begin
  DoAlpha := Color and $FF000000 <> $FF000000;
  for J := 0 to High(ScanLines) do
  begin
    L := Length(ScanLines[J]); // assuming length is even
    I := 0;
    while I < L do
    begin
      Left := ScanLines[J][I];
      Inc(I);
      Right := ScanLines[J][I];
      if Right > Left then
      begin
        if (Left and $FF) < $80 then
          Left := Left shr 8
        else
          Left := Left shr 8 + 1;
        if (Right and $FF) < $80 then
          Right := Right shr 8
        else
          Right := Right shr 8 + 1;
        if DoAlpha then
          Bitmap.DrawHorzLineT(Left, BaseY + J, Right, Color)
        else
          Bitmap.DrawHorzLine(Left, BaseY + J, Right, Color);
      end;
      Inc(I);
    end;
  end;
end;

procedure FillLines2(Bitmap: TJclBitmap32; BaseY: Integer;
  const ScanLines: TScanLines; Color: TColor32);
var
  I, J, L, N: Integer;
  MinY, MaxY, Y, Top, Bottom: Integer;
  MinX, MaxX, X, Dx: Integer;
  Left, Right: Integer;
  Buffer: array of Integer;
  P: PColor32;
  DoAlpha: Boolean;
begin
  DoAlpha := Color and $FF000000 <> $FF000000;
  // find the range of Y screen coordinates
  MinY := BaseY shr 4;
  MaxY := (BaseY + Length(ScanLines) + 15) shr 4;

  Y := MinY;
  while Y < MaxY do
  begin
    Top := Y shl 4 - BaseY;
    Bottom := Top + 15;
    if Top < 0 then
      Top := 0;
    if Bottom > High(ScanLines) then
      Bottom := High(ScanLines);

    // find left and right edges of the screen scanline
    MinX := 1000000;
    MaxX := -1000000;
    for J := Top to Bottom do
    begin
      L := High(ScanLines[J]);
      Left := ScanLines[J][0] shr 4;
      Right := (ScanLines[J][L] + 15) shr 4;
      if Left < MinX then
        MinX := Left;
      if Right > MaxX then
        MaxX := Right;
    end;

    // allocate the buffer for a screen scanline
    SetLength(Buffer, MaxX - MinX + 2);
    FillLongword(Buffer[0], Length(Buffer), 0);

    // and fill it
    for J := Top to Bottom do
    begin
      I := 0;
      L := Length(ScanLines[J]);
      while I < L do
      begin
        // Left edge
        X := ScanLines[J][I];
        Dx := X and $0F;
        X := X shr 4 - MinX;
        Inc(Buffer[X], Dx xor $0F);
        Inc(Buffer[X + 1], Dx);
        Inc(I);

        // Right edge
        X := ScanLines[J][I];
        Dx := X and $0F;
        X := X shr 4 - MinX;
        Dec(Buffer[X], Dx xor $0F);
        Dec(Buffer[X + 1], Dx);
        Inc(I);
      end;
    end;

    // integrate the buffer
    N := 0;
    for I := 0 to High(Buffer) do
    begin
      Inc(N, Buffer[I]);
      Buffer[I] := N * 273 shr 8; // some bias
    end;

    // draw it to the screen
    P := Bitmap.PixelPtr[MinX, Y];
    try
      if DoAlpha then
        for I := 0 to High(Buffer) do
        begin
          BlendMemEx(Color, P^, Buffer[I]);
          Inc(P);
        end
      else
        for I := 0 to High(Buffer) do
        begin
          N := Buffer[I];
          if N = 255 then
            P^ := Color
          else
            BlendMemEx(Color, P^, Buffer[I]);
          Inc(P);
        end;
    finally
      EMMS;
    end;

    Inc(Y);
  end;
end;

procedure GetMinMax(const Points: TDynPointArray; out MinY, MaxY: Integer);
var
  I, Y: Integer;
begin
  MinY := 100000;
  MaxY := -100000;
  for I := 0 to High(Points) do
  begin
    Y := Points[I].Y;
    if Y < MinY then
      MinY := Y;
    if Y > MaxY then
      MaxY := Y;
  end;
end;

procedure PolygonTS(Bitmap: TJclBitmap32; const Points: TDynPointArray; Color: TColor32);
var
  L, MinY, MaxY: Integer;
  ScanLines: TScanLines;
begin
  L := Length(Points);
  if L < 3 then
    Exit;
  GetMinMax(Points, MinY, MaxY);
  MinY := Constrain(MinY, 0, Bitmap.Height);
  MaxY := Constrain(MaxY, 0, Bitmap.Height);
  if MinY >= MaxY then
    Exit;
  SetLength(ScanLines, MaxY - MinY + 1);
  AddPolygon(Points, MinY, Bitmap.Width shl 8 - 1, Bitmap.Height - 1,
    ScanLines, True);
  SortLines(ScanLines);
  Bitmap.Changing;
  Bitmap.BeginUpdate;
  try
    FillLines(Bitmap, MinY, ScanLines, Color);
  finally
    Bitmap.EndUpdate;
    Bitmap.Changed;
  end;
end;

procedure PolygonAS(Bitmap: TJclBitmap32; const Points: TDynPointArray; Color: TColor32);
var
  L, I, MinY, MaxY: Integer;
  ScanLines: TScanLines;
  PP: TDynPointArray;
begin
  L := Length(Points);
  if L < 3 then
    Exit;
  SetLength(PP, L);
  for I := 0 to L - 1 do
  begin
    PP[I].X := Points[I].X shl 4 + 7;
    PP[I].Y := Points[I].Y shl 4 + 7;
  end;
  GetMinMax(PP, MinY, MaxY);
  MinY := Constrain(MinY, 0, Bitmap.Height shl 4 - 1);
  MaxY := Constrain(MaxY, 0, Bitmap.Height shl 4 - 1);
  if MinY >= MaxY then
    Exit;
  SetLength(ScanLines, MaxY - MinY + 1);
  AddPolygon(PP, MinY, Bitmap.Width  shl 4 - 1, Bitmap.Height shl 4 - 1,
    ScanLines, False);
  SortLines(ScanLines);
  Bitmap.Changing;
  Bitmap.BeginUpdate;
  try
    FillLines2(Bitmap, MinY, ScanLines, Color);
  finally
    Bitmap.EndUpdate;
    Bitmap.Changed;
  end;
end;

procedure PolygonFS(Bitmap: TJclBitmap32; const Points: TDynPointArrayF; Color: TColor32);
var
  L, I, MinY, MaxY: Integer;
  ScanLines: TScanLines;
  PP: TDynPointArray;
begin
  L := Length(Points);
  if L < 3 then
    Exit;
  SetLength(PP, L);
  for I := 0 to L - 1 do
  begin
    PP[I].X := Round(Points[I].X * 16) + 7;
    PP[I].Y := Round(Points[I].Y * 16) + 7;
  end;
  GetMinMax(PP, MinY, MaxY);
  MinY := Constrain(MinY, 0, Bitmap.Height shl 4 - 1);
  MaxY := Constrain(MaxY, 0, Bitmap.Height shl 4 - 1);
  if MinY >= MaxY then
    Exit;
  SetLength(ScanLines, MaxY - MinY + 1);
  AddPolygon(PP, MinY, Bitmap.Width shl 4 - 1, Bitmap.Height shl 4 - 1,
    ScanLines, False);
  SortLines(ScanLines);
  Bitmap.Changing;
  Bitmap.BeginUpdate;
  try
    FillLines2(Bitmap, MinY, ScanLines, Color);
  finally
    Bitmap.EndUpdate;
    Bitmap.Changed;
  end;
end;

procedure PolyPolygonTS(Bitmap: TJclBitmap32; const Points: TDynDynPointArrayArray;
  Color: TColor32);
var
  N, L, min, max, MinY, MaxY: Integer;
  ScanLines: TScanLines;
begin
  MinY := 100000;
  MaxY := -100000;
  for N := 0 to High(Points) do
  begin
    L := Length(Points[N]);
    if L < 3 then
      Exit;
    GetMinMax(Points[N], min, max);
    if min < MinY then
      MinY := min;
    if max > MaxY then
      MaxY := max;
  end;
  MinY := Constrain(MinY, 0, Bitmap.Height - 1);
  MaxY := Constrain(MaxY, 0, Bitmap.Height - 1);
  if MinY >= MaxY then
    Exit;
  SetLength(ScanLines, MaxY - MinY + 1);

  for N := 0 to High(Points) do
    AddPolygon(Points[N], MinY, Bitmap.Width shl 8 - 1 , Bitmap.Height - 1,
      ScanLines, True);

  SortLines(ScanLines);

  Bitmap.Changing;
  FillLines(Bitmap, MinY, ScanLines, Color);
  Bitmap.Changed;
end;

procedure PolyPolygonAS(Bitmap: TJclBitmap32; const Points: TDynDynPointArrayArray;
  Color: TColor32);
var
  N, L, I, min, max, MinY, MaxY: Integer;
  ScanLines: TScanLines;
  PPP: TDynDynPointArrayArray;
begin
  MinY := 100000;
  MaxY := -100000;
  SetLength(PPP, Length(Points));
  for N := 0 to High(Points) do
  begin
    L := Length(Points);
    SetLength(PPP[N], Length(Points[N]));
    for I := 0 to L - 1 do
    begin
      PPP[N][I].X := Points[N][I].X shl 4 + 7;
      PPP[N][I].Y := Points[N][I].Y shl 4 + 7;
    end;
    if L < 3 then
      Continue;
    GetMinMax(PPP[N], min, max);
    if min < MinY then
      MinY := min;
    if max > MaxY then
      MaxY := max;
  end;
  MinY := Constrain(MinY, 0, Bitmap.Height shl 4 - 1);
  MaxY := Constrain(MaxY, 0, Bitmap.Height shl 4 - 1);
  if MinY >= MaxY then
    Exit;
  SetLength(ScanLines, MaxY - MinY + 1);

  for N := 0 to High(PPP) do
  begin
    AddPolygon(PPP[N], MinY, Bitmap.Width shl 4 - 1, Bitmap.Height shl 4 - 1,
      ScanLines, False);
  end;

  SortLines(ScanLines);

  Bitmap.Changing;
  FillLines2(Bitmap, MinY, ScanLines, Color);
  Bitmap.Changed;
end;

procedure PolyPolygonFS(Bitmap: TJclBitmap32; const Points: TDynDynPointArrayArrayF;
  Color: TColor32);
var
  N, L, I, min, max, MinY, MaxY: Integer;
  ScanLines: TScanLines;
  PPP: TDynDynPointArrayArray;
begin
  MinY := 100000;
  MaxY := -100000;
  SetLength(PPP, Length(Points));
  for N := 0 to High(Points) do
  begin
    L := Length(Points);
    SetLength(PPP[N], Length(Points[N]));
    for I := 0 to L - 1 do
    begin
      PPP[N][I].X := Round(Points[N][I].X * 16) + 7;
      PPP[N][I].Y := Round(Points[N][I].Y * 16) + 7;
    end;
    if L < 3 then
      Continue;
    GetMinMax(PPP[N], min, max);
    if min < MinY then
      MinY := min;
    if max > MaxY then
      MaxY := max;
  end;
  MinY := Constrain(MinY, 0, Bitmap.Height shl 4 - 1);
  MaxY := Constrain(MaxY, 0, Bitmap.Height shl 4 - 1);
  if MinY >= MaxY then
    Exit;
  SetLength(ScanLines, MaxY - MinY + 1);

  for N := 0 to High(PPP) do
    AddPolygon(PPP[N], MinY, Bitmap.Width shl 4 - 1, Bitmap.Height shl 4 - 1,
      ScanLines, False);

  SortLines(ScanLines);

  Bitmap.Changing;
  FillLines2(Bitmap, MinY, ScanLines, Color);
  Bitmap.Changed;
end;

//=== Filters ================================================================

procedure CheckParams(Dst, Src: TJclBitmap32);
begin
  if Src = nil then
    raise EJclGraphicsError.CreateRes(@RsSourceBitmapEmpty);
  if Dst = nil then
    raise EJclGraphicsError.CreateRes(@RsDestinationBitmapEmpty);
  Dst.SetSize(Src.Width, Src.Height); // Should this go? See #0001513. It is currently of no use.
end;

procedure AlphaToGrayscale(Dst, Src: TJclBitmap32);
var
  I: Integer;
  D, S: PColor32;
begin
  CheckParams(Dst, Src);
  Dst.Changing;
  Dst.SetSize(Src.Width, Src.Height);
  D := @Dst.Bits[0];
  S := @Src.Bits[0];
  for I := 0 to Src.Width * Src.Height - 1 do
  begin
    D^ := Gray32(AlphaComponent(S^), $FF);
    Inc(S);
    Inc(D);
  end;
  Dst.Changed;
end;

procedure IntensityToAlpha(Dst, Src: TJclBitmap32);
var
  I: Integer;
  D, S: PColor32;
begin
  CheckParams(Dst, Src);
  Dst.Changing;
  Dst.SetSize(Src.Width, Src.Height);
  D := @Dst.Bits[0];
  S := @Src.Bits[0];
  for I := 0 to Src.Width * Src.Height - 1 do
  begin
    D^ := SetAlpha(D^, Intensity(S^));
    Inc(S);
    Inc(D);
  end;
  Dst.Changed;
end;

procedure Invert(Dst, Src: TJclBitmap32);
var
  I: Integer;
  D, S: PColor32;
begin
  CheckParams(Dst, Src);
  Dst.Changing;
  Dst.SetSize(Src.Width, Src.Height);
  D := @Dst.Bits[0];
  S := @Src.Bits[0];
  for I := 0 to Src.Width * Src.Height - 1 do
  begin
    D^ := S^ xor $FFFFFFFF;
    Inc(S);
    Inc(D);
  end;
  Dst.Changed;
end;

procedure InvertRGB(Dst, Src: TJclBitmap32);
var
  I: Integer;
  D, S: PColor32;
begin
  CheckParams(Dst, Src);
  Dst.Changing;
  Dst.SetSize(Src.Width, Src.Height);
  D := @Dst.Bits[0];
  S := @Src.Bits[0];
  for I := 0 to Src.Width * Src.Height - 1 do
  begin
    D^ := S^ xor $00FFFFFF;
    Inc(S);
    Inc(D);
  end;
  Dst.Changed;
end;

procedure ColorToGrayscale(Dst, Src: TJclBitmap32);
var
  I: Integer;
  D, S: PColor32;
begin
  CheckParams(Dst, Src);
  Dst.Changing;
  Dst.SetSize(Src.Width, Src.Height);
  D := @Dst.Bits[0];
  S := @Src.Bits[0];
  for I := 0 to Src.Width * Src.Height - 1 do
  begin
    D^ := Gray32(Intensity(S^), $FF);
    Inc(S);
    Inc(D);
  end;
  Dst.Changed;
end;

procedure ApplyLUT(Dst, Src: TJclBitmap32; const LUT: TLUT8);
var
  I: Integer;
  D, S: PColor32;
  r, g, b: TColor32;
  C: TColor32;
begin
  CheckParams(Dst, Src);

  Dst.Changing;
  Dst.SetSize(Src.Width, Src.Height);
  D := @Dst.Bits[0];
  S := @Src.Bits[0];

  for I := 0 to Src.Width * Src.Height - 1 do
  begin
    C := S^;
    r := C and $00FF0000;
    g := C and $0000FF00;
    r := r shr 16;
    b := C and $000000FF;
    g := g shr 8;
    r := LUT[r];
    g := LUT[g];
    b := LUT[b];
    D^ := $FF000000 or r shl 16 or g shl 8 or b;
    Inc(S);
    Inc(D);
  end;
  Dst.Changed;
end;
{$ENDIF Bitmap32}

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
