{==============================================================================
  Content:  TheTextDrawer, a helper class for drawing of
            fixed-pitched font characters
 ==============================================================================
  The contents of this file are subject to the Mozilla Public License Ver. 1.0
  (the "License"); you may not use this file except in compliance with the
  License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.
 ==============================================================================
  The Original Code is HANAI Tohru's private delphi library.
 ==============================================================================
  The Initial Developer of the Original Code is HANAI Tohru (Japan)
  Portions created by HANAI Tohru are Copyright (C) 1999.
  All Rights Reserved.
 ==============================================================================
  Contributor(s):   HANAI Tohru
 ==============================================================================
  History:  01/19/1999  HANAI Tohru
                        Initial Version
            02/13/1999  HANAI Tohru
                        Changed default intercharacter spacing
            09/09/1999  HANAI Tohru
                        Redesigned all. Simplified interfaces.
                        When drawing text now it uses TextOut + SetTextCharacter-
                        Extra insted ExtTextOut since ExtTextOut has a little
                        heavy behavior.
            09/10/1999  HANAI Tohru
                        Added code to call ExtTextOut because there is a problem
                        when TextOut called with italicized raster type font.
                        After this changing, ExtTextOut is called without the
                        last parameter `lpDx' and be with SetTextCharacterExtra.
                        This pair performs faster than with `lpDx'.
            09/14/1999  HANAI Tohru
                        Changed code for saving/restoring DC
            09/15/1999  HANAI Tohru
                        Added X/Y parameters to ExtTextOut.
            09/16/1999  HANAI Tohru
                        Redesigned for multi-bytes character drawing.
            09/19/1999  HANAI Tohru
                        Since TheTextDrawer grew fat it was split into three
                        classes - TheFontStock, TheTextDrawer and TheTextDrawerEx.
                        Currently it should avoid TheTextDrawer because it is
                        slower than TheTextDrawer.
            09/25/1999  HANAI Tohru
                        Added internally definition of LeadBytes for Delphi 2
            10/01/1999  HANAI Tohru
                        To save font resources, now all fonts data are shared
                        among all of TheFontStock instances. With this changing,
                        there added a new class `TheFontsInfoManager' to manage
                        those shared data.
            10/09/1999  HANAI Tohru
                        Added BaseStyle property to TheFontFont class.
 ==============================================================================}

// $Id: SynTextDrawer.pas,v 1.6 2003/09/19 21:32:48 etrusco Exp $

// SynEdit note: The name had to be changed to get SynEdit to install 
//   together with mwEdit into the same Delphi installation

unit SynTextDrawer;

{$I SynEdit.inc}

interface

uses
  SysUtils,
  Classes,
  Windows,
  Graphics;

const
  FontStyleCount = Ord(High(TFontStyle)) +1;
  FontStyleCombineCount = (1 shl FontStyleCount);
  
type
  TheStockFontPatterns = 0..FontStyleCombineCount -1;

  PheFontData = ^TheFontData;
  TheFontData = record
    Style: TFontStyles;
    Handle: HFont;
    CharAdv: Integer;       // char advance of single-byte code
    DBCharAdv: Integer;     // char advance of double-byte code
    CharHeight: Integer;
  end;

  PheFontsData = ^TheFontsData;
  TheFontsData = array[TheStockFontPatterns] of TheFontData;

  PheSharedFontsInfo = ^TheSharedFontsInfo;
  TheSharedFontsInfo = record
    // reference counters
    RefCount: Integer;
    LockCount: Integer;
    // font information
    BaseFont: TFont;
    BaseLF: TLogFont;
    IsDBCSFont: Boolean;
    IsTrueType: Boolean;
    FontsData: TheFontsData;
  end;

  { TheStockFontManager }

  TheFontsInfoManager = class
  private
    FFontsInfo: TList;
    function FindFontsInfo(const LF: TLogFont): PheSharedFontsInfo;
    function CreateFontsInfo(ABaseFont: TFont;
      const LF: TLogFont): PheSharedFontsInfo;
    procedure DestroyFontHandles(pFontsInfo: PheSharedFontsInfo);
    procedure RetrieveLogFontForComparison(ABaseFont: TFont; var LF: TLogFont);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LockFontsInfo(pFontsInfo: PheSharedFontsInfo);
    procedure UnLockFontsInfo(pFontsInfo: PheSharedFontsInfo);
    function GetFontsInfo(ABaseFont: TFont): PheSharedFontsInfo;
    procedure ReleaseFontsInfo(pFontsInfo: PheSharedFontsInfo);
  end;

  { TheFontStock }

  TheExtTextOutProc = procedure (X, Y: Integer; fuOptions: UINT;
    const ARect: TRect; Text: PChar; Length: Integer) of object;

  EheFontStockException = class(Exception);

  TheFontStock = class
  private
    // private DC
    FDC: HDC;
    FDCRefCount: Integer;

    // Shared fonts
    FpInfo: PheSharedFontsInfo;
    FUsingFontHandles: Boolean;

    // Current font
    FCrntFont: HFONT;
    FCrntStyle: TFontStyles;
    FpCrntFontData: PheFontData;
    // local font info
    FBaseLF: TLogFont;
    function GetBaseFont: TFont;
    function GetIsDBCSFont: Boolean;
    function GetIsTrueType: Boolean;
  protected
    function InternalGetDC: HDC; virtual;
    procedure InternalReleaseDC(Value: HDC); virtual;
    function InternalCreateFont(Style: TFontStyles): HFONT; virtual;
    function CalcFontAdvance(DC: HDC;
      pCharHeight, pDBCharAdvance: PInteger): Integer; virtual;
    function GetCharAdvance: Integer; virtual;
    function GetCharHeight: Integer; virtual;
    function GetDBCharAdvance: Integer; virtual;
    function GetFontData(idx: Integer): PheFontData; virtual;
    procedure UseFontHandles;
    procedure ReleaseFontsInfo;
    procedure SetBaseFont(Value: TFont); virtual;
    procedure SetStyle(Value: TFontStyles); virtual;
    property FontData[idx: Integer]: PheFontData read GetFontData;
    property FontsInfo: PheSharedFontsInfo read FpInfo;
  public
    constructor Create(InitialFont: TFont); virtual;
    destructor Destroy; override;
    procedure ReleaseFontHandles; virtual;
    property BaseFont: TFont read GetBaseFont;
    property Style: TFontStyles read FCrntStyle write SetStyle;
    property FontHandle: HFONT read FCrntFont;
    property CharAdvance: Integer read GetCharAdvance;
    property CharHeight: Integer read GetCharHeight;
    property DBCharAdvance: Integer read GetDBCharAdvance;
    property IsDBCSFont: Boolean read GetIsDBCSFont;
    property IsTrueType: Boolean read GetIsTrueType;
  end;

  { TheTextDrawer }
  EheTextDrawerException = class(Exception);

  TheTextDrawer = class(TObject)
  private
    FDC: HDC;
    FSaveDC: Integer;

    // Font information
    FFontStock: TheFontStock;
    FCalcExtentBaseStyle: TFontStyles;
    FBaseCharWidth: Integer;
    FBaseCharHeight: Integer;

    // current font and properties
    FCrntFont: HFONT;
    FETODist: Pointer;
    FETOSizeInChar: Integer;

    // current font attributes
    FColor: TColor;
    FBkColor: TColor;
    FCharExtra: Integer;

    // Begin/EndDrawing calling count
    FDrawingCount: Integer;
  protected
    procedure ReleaseETODist; virtual;
    procedure AfterStyleSet; virtual;
    procedure DoSetCharExtra(Value: Integer); virtual;
    property StockDC: HDC read FDC;
    property DrawingCount: Integer read FDrawingCount;
    property FontStock: TheFontStock read FFontStock;
    property BaseCharWidth: Integer read FBaseCharWidth;
    property BaseCharHeight: Integer read FBaseCharHeight;
  public
    constructor Create(CalcExtentBaseStyle: TFontStyles; BaseFont: TFont); virtual;
    destructor Destroy; override;
    function GetCharWidth: Integer; virtual;
    function GetCharHeight: Integer; virtual;
    procedure BeginDrawing(DC: HDC); virtual;
    procedure EndDrawing; virtual;
    procedure TextOut(X, Y: Integer; Text: PChar; Length: Integer); virtual;
    procedure ExtTextOut(X, Y: Integer; fuOptions: UINT; const ARect: TRect;
      Text: PChar; Length: Integer); virtual;
    procedure SetBaseFont(Value: TFont); virtual;
    procedure SetBaseStyle(const Value: TFontStyles); virtual;
    procedure SetStyle(Value: TFontStyles); virtual;
    procedure SetForeColor(Value: TColor); virtual;
    procedure SetBackColor(Value: TColor); virtual;
    procedure SetCharExtra(Value: Integer); virtual;
    procedure ReleaseTemporaryResources; virtual;
    property CharWidth: Integer read GetCharWidth;
    property CharHeight: Integer read GetCharHeight;
    property BaseFont: TFont write SetBaseFont;
    property BaseStyle: TFontStyles write SetBaseStyle;
    property ForeColor: TColor write SetForeColor;
    property BackColor: TColor write SetBackColor;
    property Style: TFontStyles write SetStyle;
    property CharExtra: Integer read FCharExtra write SetCharExtra;
  end;

  { TheTextDrawer2 }

  TheTextDrawer2 = class(TheTextDrawer)
  private
    FFonts: array[TheStockFontPatterns] of HFONT;
  public
    procedure SetStyle(Value: TFontStyles); override;
    procedure SetBaseFont(Value: TFont); override;
  end;

  { TheTextDrawerEx }

  TheTextDrawerEx = class(TheTextDrawer)
  private
    // current font properties
    FCrntDx: Integer;
    FCrntDBDx: Integer;               // for a double-byte character
    // Text drawing procedure reference for optimization
    FExtTextOutProc: TheExtTextOutProc;
  protected
    procedure AfterStyleSet; override;
    procedure DoSetCharExtra(Value: Integer); override;
    procedure TextOutOrExtTextOut(X, Y: Integer; fuOptions: UINT;
      const ARect: TRect; Text: PChar; Length: Integer); virtual;
    procedure ExtTextOutFixed(X, Y: Integer; fuOptions: UINT;
      const ARect: TRect; Text: PChar; Length: Integer); virtual;
    procedure ExtTextOutWithETO(X, Y: Integer; fuOptions: UINT;
      const ARect: TRect; Text: PChar; Length: Integer); virtual;
    procedure ExtTextOutForDBCS(X, Y: Integer; fuOptions: UINT;
      const ARect: TRect; Text: PChar; Length: Integer); virtual;
  public
    procedure ExtTextOut(X, Y: Integer; fuOptions: UINT; const ARect: TRect;
      Text: PChar; Length: Integer); override;
  end;

  function GetFontsInfoManager: TheFontsInfoManager;

{$IFNDEF VER93}
{$IFNDEF VER90}
{$IFNDEF VER80}
{$DEFINE HE_ASSERT}
{$DEFINE HE_LEADBYTES}
{$DEFINE HE_COMPAREMEM}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFNDEF HE_LEADBYTES}
type
  TheLeadByteChars = set of Char;

  function SetLeadBytes(const Value: TheLeadByteChars): TheLeadByteChars;
{$ENDIF}

implementation

const
  DBCHAR_CALCULATION_FALED  = $7FFFFFFF;

var
  gFontsInfoManager: TheFontsInfoManager;
{$IFNDEF HE_LEADBYTES}
  LeadBytes: TheLeadByteChars;
{$ENDIF}

{ utility routines }

function GetFontsInfoManager: TheFontsInfoManager;
begin
  if not Assigned(gFontsInfoManager) then
    gFontsInfoManager := TheFontsInfoManager.Create;
  Result := gFontsInfoManager;
end;

function Min(x, y: integer): integer;
begin
  if x < y then Result := x else Result := y;
end;

{$IFNDEF HE_ASSERT}
procedure ASSERT(Expression: Boolean);
begin
  if not Expression then
    raise EheTextDrawerException.Create('Assertion failed.');
end;
{$ENDIF}

{$IFNDEF HE_LEADBYTES}
function SetLeadBytes(const Value: TheLeadByteChars): TheLeadByteChars;
begin
  Result := LeadBytes;
  LeadBytes := Value;
end;
{$ENDIF}

{$IFNDEF HE_COMPAREMEM}
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,1
        SHR     ECX,1
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;
{$ENDIF}

{ TheFontsInfoManager }

procedure TheFontsInfoManager.LockFontsInfo(
  pFontsInfo: PheSharedFontsInfo);
begin
  Inc(pFontsInfo^.LockCount);
end;

constructor TheFontsInfoManager.Create;
begin
  inherited;

  FFontsInfo := TList.Create;
end;

function TheFontsInfoManager.CreateFontsInfo(ABaseFont: TFont;
  const LF: TLogFont): PheSharedFontsInfo;
var
  DC: HDC;
  hOldFont: HFont;
begin
  New(Result);
  FillChar(Result^, SizeOf(TheSharedFontsInfo), 0);
  with Result^ do
    try
      BaseFont := TFont.Create;
      BaseFont.Assign(ABaseFont);
      BaseLF := LF;
      IsTrueType := (0 <> (TRUETYPE_FONTTYPE and LF.lfPitchAndFamily));
      // find out whether the font `IsDBCSFont'
      DC := GetDC(0);
      hOldFont := SelectObject(DC, ABaseFont.Handle);
      IsDBCSFont := (0 <> (GCP_DBCS and GetFontLanguageInfo(DC)));
      SelectObject(DC, hOldFont);
      ReleaseDC(0, DC);
  except
    Result^.BaseFont.Free;
    Dispose(Result);
    raise;
  end;
end;

procedure TheFontsInfoManager.UnlockFontsInfo(
  pFontsInfo: PheSharedFontsInfo);
begin
  with pFontsInfo^ do
  begin
    Dec(LockCount);
    if 0 = LockCount then
      DestroyFontHandles(pFontsInfo);
  end;
end;

destructor TheFontsInfoManager.Destroy;
begin
  gFontsInfoManager := nil;
  
  if Assigned(FFontsInfo) then
  begin
    while FFontsInfo.Count > 0 do
    begin
      ASSERT(1 = PheSharedFontsInfo(FFontsInfo[FFontsInfo.Count - 1])^.RefCount);
      ReleaseFontsInfo(PheSharedFontsInfo(FFontsInfo[FFontsInfo.Count - 1]));
    end;
    FFontsInfo.Free;
  end;

  inherited;
end;

procedure TheFontsInfoManager.DestroyFontHandles(
  pFontsInfo: PheSharedFontsInfo);
var
  i: Integer;
begin
  with pFontsInfo^ do
    for i := Low(TheStockFontPatterns) to High(TheStockFontPatterns) do
      with FontsData[i] do
        if Handle <> 0 then
        begin
          DeleteObject(Handle);
          Handle := 0;
        end;
end;

function TheFontsInfoManager.FindFontsInfo(
  const LF: TLogFont): PheSharedFontsInfo;
var
  i: Integer;
begin
  for i := 0 to FFontsInfo.Count - 1 do
  begin
    Result := PheSharedFontsInfo(FFontsInfo[i]);
    if CompareMem(@(Result^.BaseLF), @LF, SizeOf(TLogFont)) then
      Exit;
  end;
  Result := nil;
end;

function TheFontsInfoManager.GetFontsInfo(ABaseFont: TFont): PheSharedFontsInfo;
var
  LF: TLogFont;
begin
  ASSERT(Assigned(ABaseFont));

  RetrieveLogFontForComparison(ABaseFont, LF);
  Result := FindFontsInfo(LF);
  if not Assigned(Result) then
  begin
    Result := CreateFontsInfo(ABaseFont, LF);
    FFontsInfo.Add(Result);
  end;

  if Assigned(Result) then
    Inc(Result^.RefCount);
end;

procedure TheFontsInfoManager.ReleaseFontsInfo(pFontsInfo: PheSharedFontsInfo);
begin
  ASSERT(Assigned(pFontsInfo));

  with pFontsInfo^ do
  begin
{$IFDEF HE_ASSERT}
    ASSERT(LockCount < RefCount,
      'Call DeactivateFontsInfo before calling this.');
{$ELSE}
    ASSERT(LockCount < RefCount);
{$ENDIF}
    if RefCount > 1 then
      Dec(RefCount)
    else
    begin
      FFontsInfo.Remove(pFontsInfo);
      // free all objects
      BaseFont.Free;
      Dispose(pFontsInfo);
    end;
  end;
end;

procedure TheFontsInfoManager.RetrieveLogFontForComparison(ABaseFont: TFont;
  var LF: TLogFont);
var
  pEnd: PChar;
begin
  GetObject(ABaseFont.Handle, SizeOf(TLogFont), @LF);
  with LF do
  begin
    lfItalic := 0;
    lfUnderline := 0;
    lfStrikeOut := 0;
    pEnd := StrEnd(lfFaceName);
    FillChar(pEnd[1], @lfFaceName[High(lfFaceName)] - pEnd, 0);
  end;
end;

{ TheFontStock }

// CalcFontAdvance : Calculation a advance of a character of a font.
//  [*]hCalcFont will be selected as FDC's font if FDC wouldn't be zero.
function TheFontStock.CalcFontAdvance(DC: HDC;
  pCharHeight, pDBCharAdvance: PInteger): Integer;
var
  TM: TTextMetric;
  ABC: TABC;
  ABC2: TABC;
  w: Integer;
  HasABC: Boolean;
begin
  // Calculate advance of a character.
  // The following code uses ABC widths instead TextMetric.tmAveCharWidth
  // because ABC widths always tells truth but tmAveCharWidth does not.
  // A true-type font will have ABC widths but others like raster type will not
  // so if the function fails then use TextMetric.tmAveCharWidth.
  GetTextMetrics(DC, TM);
  HasABC := GetCharABCWidths(DC, Ord('M'), Ord('M'), ABC);
  if not HasABC then
  begin
    with ABC do
    begin
      abcA := 0;
      abcB := TM.tmAveCharWidth;
      abcC := 0;
    end;
    TM.tmOverhang := 0;
  end;

  // Result(CharWidth)
  with ABC do
    Result := abcA + Integer(abcB) + abcC + TM.tmOverhang;
  // pCharHeight
  if Assigned(pCharHeight) then
    pCharHeight^ := Abs(TM.tmHeight) {+ TM.tmInternalLeading};
  // pDBCharAdvance
  if Assigned(pDBCharAdvance) then
  begin
    pDBCharAdvance^ := DBCHAR_CALCULATION_FALED;
    if IsDBCSFont then
    begin
      case TM.tmCharSet of
        SHIFTJIS_CHARSET:
          if HasABC and
             GetCharABCWidths(DC, $8201, $8201, ABC) and    // max width(maybe)
             GetCharABCWidths(DC, $82A0, $82A0, ABC2) then  // HIRAGANA 'a'
          begin
            with ABC do
              w := abcA + Integer(abcB) + abcC;
            if w > (1.5 * Result) then // it should be over 150% wider than SBChar(I think)
              with ABC2 do
                if w = (abcA + Integer(abcB) + abcC) then
                  pDBCharAdvance^ := w;
          end;
        // About the following character sets,
        // I don't know with what character should be calculated.
{
        ANSI_CHARSET:
        DEFAULT_CHARSET:
        SYMBOL_CHARSET:
        HANGUL_CHARSET:
        GB2312_CHARSET:
        CHINESEBIG5_CHARSET:
        OEM_CHARSET:
        JOHAB_CHARSET:
        HEBREW_CHARSET:
        ARABIC_CHARSET:
        GREEK_CHARSET:
        TURKISH_CHARSET:
        VIETNAMESE_CHARSET:
        THAI_CHARSET:
        EASTEUROPE_CHARSET:
        RUSSIAN_CHARSET:
        MAC_CHARSET:
        BALTIC_CHARSET:
}
      end;
    end;
  end;
end;

constructor TheFontStock.Create(InitialFont: TFont);
begin
  inherited Create;

  SetBaseFont(InitialFont);
end;

destructor TheFontStock.Destroy;
begin
  ReleaseFontsInfo;
  ASSERT(FDCRefCount = 0);

  inherited;
end;

function TheFontStock.GetBaseFont: TFont;
begin
  Result := FpInfo^.BaseFont;
end;

function TheFontStock.GetCharAdvance: Integer;
begin
  Result := FpCrntFontData^.CharAdv;
end;

function TheFontStock.GetCharHeight: Integer;
begin
  Result := FpCrntFontData^.CharHeight;
end;

function TheFontStock.GetDBCharAdvance: Integer;
begin
  Result := FpCrntFontData^.DBCharAdv;
end;

function TheFontStock.GetFontData(idx: Integer): PheFontData;
begin
  Result := @FpInfo^.FontsData[idx];
end;

function TheFontStock.GetIsDBCSFont: Boolean;
begin
  Result := FpInfo^.IsDBCSFont;
end;

function TheFontStock.GetIsTrueType: Boolean;
begin
  Result := FpInfo^.IsTrueType
end;

function TheFontStock.InternalCreateFont(Style: TFontStyles): HFONT;
const
  Bolds: array[Boolean] of Integer = (400, 700);
begin
  with FBaseLF do
  begin
    lfWeight := Bolds[fsBold in Style];
    lfItalic := Ord(BOOL(fsItalic in Style));
    lfUnderline := Ord(BOOL(fsUnderline in Style));
    lfStrikeOut := Ord(BOOL(fsStrikeOut in Style));
  end;
  Result := CreateFontIndirect(FBaseLF);
end;

function TheFontStock.InternalGetDC: HDC;
begin
  if FDCRefCount = 0 then
  begin
    ASSERT(FDC = 0);
    FDC := GetDC(0);
  end;
  Inc(FDCRefCount);
  Result := FDC;
end;

procedure TheFontStock.InternalReleaseDC(Value: HDC);
begin
  Dec(FDCRefCount);
  if FDCRefCount <= 0 then
  begin
    ASSERT((FDC <> 0) and (FDC = Value));
    ReleaseDC(0, FDC);
    FDC := 0;
    ASSERT(FDCRefCount = 0);
  end;
end;

procedure TheFontStock.ReleaseFontHandles;
begin
  if FUsingFontHandles then
    with GetFontsInfoManager do
    begin
      UnlockFontsInfo(FpInfo);
      FUsingFontHandles := False;
    end;
end;

procedure TheFontStock.ReleaseFontsInfo;
begin
  if Assigned(FpInfo) then
    with GetFontsInfoManager do
    begin
      if FUsingFontHandles then
      begin
        UnlockFontsInfo(FpInfo);
        FUsingFontHandles := False;
      end;
      ReleaseFontsInfo(FpInfo);
      FpInfo := nil;
    end;
end;

procedure TheFontStock.SetBaseFont(Value: TFont);
var
  pInfo: PheSharedFontsInfo;
begin
  if Assigned(Value) then
  begin
    pInfo := GetFontsInfoManager.GetFontsInfo(Value);
    if pInfo = FpInfo then
      GetFontsInfoManager.ReleaseFontsInfo(pInfo)
    else
    begin
      ReleaseFontsInfo;
      FpInfo := pInfo;
      FBaseLF := FpInfo^.BaseLF;
      SetStyle(Value.Style);
    end;
  end
  else
    raise EheFontStockException.Create('SetBaseFont: ''Value'' must be specified.');
end;

procedure TheFontStock.SetStyle(Value: TFontStyles);
var
  idx: Integer;
  DC: HDC;
  hOldFont: HFONT;
  p: PheFontData;
begin
{$IFDEF HE_ASSERT}
  ASSERT(SizeOf(TFontStyles) = 1,
    'TheTextDrawer.SetStyle: There''s more than four font styles but the current '+
    'code expects only four styles.');
{$ELSE}
  ASSERT(SizeOf(TFontStyles) = 1);
{$ENDIF}

  idx := Byte(Value);
  ASSERT(idx <= High(TheStockFontPatterns));

  UseFontHandles;
  p := FontData[idx];
  if FpCrntFontData = p then
    Exit;

  FpCrntFontData := p;
  with p^ do
    if Handle <> 0 then
    begin
      FCrntFont := Handle;
      FCrntStyle := Style;
      Exit;
    end;

  // create font
  FCrntFont := InternalCreateFont(Value);
  DC := InternalGetDC;
  hOldFont := SelectObject(DC, FCrntFont);

  // retrieve height and advances of new font
  with FpCrntFontData^ do
  begin
    Handle := FCrntFont;
    if IsDBCSFont then
      CharAdv := CalcFontAdvance(DC, @CharHeight, @DBCharAdv)
    else
      CharAdv := CalcFontAdvance(DC, @CharHeight, nil);
  end;

  SelectObject(DC, hOldFont);
  InternalReleaseDC(DC);
end;

procedure TheFontStock.UseFontHandles;
begin
  if not FUsingFontHandles then
    with GetFontsInfoManager do
    begin
      LockFontsInfo(FpInfo);
      FUsingFontHandles := True;
    end;
end;

{ TheTextDrawer }

constructor TheTextDrawer.Create(CalcExtentBaseStyle: TFontStyles; BaseFont: TFont);
begin
  inherited Create;

  FFontStock := TheFontStock.Create(BaseFont);
  FCalcExtentBaseStyle := CalcExtentBaseStyle;
  SetBaseFont(BaseFont);
  FColor := clWindowText;
  FBkColor := clWindow;
end;

destructor TheTextDrawer.Destroy;
begin
  FFontStock.Free;
  ReleaseETODist;

  inherited;
end;

procedure TheTextDrawer.ReleaseETODist;
begin
  if Assigned(FETODist) then
  begin
    FETOSizeInChar := 0;
    FreeMem(FETODist);
    FETODist := nil;
  end;
end;

procedure TheTextDrawer.BeginDrawing(DC: HDC);
begin
  if (FDC = DC) then
    ASSERT(FDC <> 0)
  else
  begin
    ASSERT((FDC = 0) and (DC <> 0) and (FDrawingCount = 0));
    FDC := DC;
    FSaveDC := SaveDC(DC);
    SelectObject(DC, FCrntFont);
    Windows.SetTextColor(DC, ColorToRGB(FColor));
    Windows.SetBkColor(DC, ColorToRGB(FBkColor));
    DoSetCharExtra(FCharExtra);
  end;
  Inc(FDrawingCount);
end;

procedure TheTextDrawer.EndDrawing;
begin
  ASSERT(FDrawingCount >= 1);
  Dec(FDrawingCount);
  if FDrawingCount <= 0 then
  begin
    if FDC <> 0 then
      RestoreDC(FDC, FSaveDC);
    FSaveDC := 0;
    FDC := 0;
    FDrawingCount := 0;
  end;
end;

function TheTextDrawer.GetCharWidth: Integer;
begin
  Result := FBaseCharWidth + FCharExtra;
end;

function TheTextDrawer.GetCharHeight: Integer;
begin
  Result := FBaseCharHeight;
end;

procedure TheTextDrawer.SetBaseFont(Value: TFont);
begin
  if Assigned(Value) then
  begin
    ReleaseETODist;
    with FFontStock do
    begin
      SetBaseFont(Value);
      Style := FCalcExtentBaseStyle;
      FBaseCharWidth := CharAdvance;
      FBaseCharHeight := CharHeight;
    end;
    SetStyle(Value.Style);
  end
  else
    raise EheTextDrawerException.Create('SetBaseFont: ''Value'' must be specified.');
end;

procedure TheTextDrawer.SetBaseStyle(const Value: TFontStyles);
begin
  if FCalcExtentBaseStyle <> Value then
  begin
    FCalcExtentBaseStyle := Value;
    ReleaseETODist;
    with FFontStock do
    begin
      Style := Value;
      FBaseCharWidth := CharAdvance;
      FBaseCharHeight := CharHeight;
    end;
  end;
end;

procedure TheTextDrawer.SetStyle(Value: TFontStyles);
begin
  with FFontStock do
  begin
    SetStyle(Value);
    Self.FCrntFont := FontHandle;
  end;
  AfterStyleSet;
end;

procedure TheTextDrawer.AfterStyleSet;
begin
  if FDC <> 0 then
    SelectObject(FDC, FCrntFont);
end;

procedure TheTextDrawer.SetForeColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if FDC <> 0 then
      SetTextColor(FDC, ColorToRGB(Value));
  end;
end;

procedure TheTextDrawer.SetBackColor(Value: TColor);
begin
  if FBkColor <> Value then
  begin
    FBkColor := Value;
    if FDC <> 0 then
      Windows.SetBkColor(FDC, ColorToRGB(Value));
  end;
end;

procedure TheTextDrawer.SetCharExtra(Value: Integer);
begin
  if FCharExtra <> Value then
  begin
    FCharExtra := Value;
    DoSetCharExtra(FCharExtra);
  end;
end;

procedure TheTextDrawer.DoSetCharExtra(Value: Integer);
begin
  if FDC <> 0 then
    SetTextCharacterExtra(FDC, Value);
end;

procedure TheTextDrawer.TextOut(X, Y: Integer; Text: PChar;
  Length: Integer);
begin
  Windows.TextOut(FDC, X, Y, Text, Length);
end;

procedure TheTextDrawer.ExtTextOut(X, Y: Integer; fuOptions: UINT;
  const ARect: TRect; Text: PChar; Length: Integer);

  procedure InitETODist(InitValue: Integer);
  const
    EtoBlockSize = $40;          
  var
    NewSize: Integer;
    TmpLen: Integer;
    p: PInteger;
    i: Integer;
  begin
    TmpLen := ((not (EtoBlockSize - 1)) and Length) + EtoBlockSize;
    NewSize := TmpLen * SizeOf(Integer);
    ReallocMem(FETODist, NewSize);
    p := PInteger(Integer(FETODist) + FETOSizeInChar * SizeOf(Integer));
    for i := 1 to TmpLen - FETOSizeInChar do
    begin
      p^ := InitValue;
      Inc(p);
    end;
    FETOSizeInChar := TmpLen;
  end;

begin
  if FETOSizeInChar < Length then
    InitETODist(GetCharWidth);
  Windows.ExtTextOut(FDC, X, Y, fuOptions, @ARect, Text,
    Length, PInteger(FETODist));
end;

procedure TheTextDrawer.ReleaseTemporaryResources;
begin
  FFontStock.ReleaseFontHandles;
end;

{ TheTextDrawer2 }

procedure TheTextDrawer2.SetStyle(Value: TFontStyles);
var
  idx: Integer;
begin
  idx := PByte(@Value)^;
  if FFonts[idx] <> 0 then
  begin
    FCrntFont := FFonts[idx];
    AfterStyleSet;
  end
  else
  begin
    inherited;
    FFonts[idx] := FCrntFont;
  end;
end;

procedure TheTextDrawer2.SetBaseFont(Value: TFont);
var
  i: Integer;
begin
  for i := Low(FFonts) to High(FFonts) do
    FFonts[i] := 0;
  inherited;
end;

{ TheTextDrawerEx }

procedure TheTextDrawerEx.AfterStyleSet;
begin
  inherited;
  with FontStock do
  begin
    FCrntDx := BaseCharWidth - CharAdvance;
    case IsDBCSFont of
      False:
        begin
          if StockDC <> 0 then
            SetTextCharacterExtra(StockDC, CharExtra + FCrntDx);
          if IsTrueType or (not (fsItalic in Style)) then
            FExtTextOutProc := TextOutOrExtTextOut
          else
            FExtTextOutProc := ExtTextOutFixed;
        end;
      True:
        begin
          FCrntDBDx := DBCHAR_CALCULATION_FALED;
          FExtTextOutProc := ExtTextOutWithETO;
        end;
    end;
  end;
end;

procedure TheTextDrawerEx.DoSetCharExtra(Value: Integer);
begin
  if not FontStock.IsDBCSFont then
  begin
    SetBkMode(StockDC, OPAQUE);
    SetTextCharacterExtra(StockDC, Value + FCrntDx);
  end
  else if FCrntDBDx = DBCHAR_CALCULATION_FALED then
    SetTextCharacterExtra(StockDC, Value);
end;

procedure TheTextDrawerEx.ExtTextOut(X, Y: Integer; fuOptions: UINT;
  const ARect: TRect; Text: PChar; Length: Integer);
begin
  FExtTextOutProc(X, Y, fuOptions, ARect, Text, Length);
end;

procedure TheTextDrawerEx.ExtTextOutFixed(X, Y: Integer; fuOptions: UINT;
  const ARect: TRect; Text: PChar; Length: Integer);
begin
  Windows.ExtTextOut(StockDC, X, Y, fuOptions, @ARect, Text, Length, nil);
end;

procedure TheTextDrawerEx.ExtTextOutForDBCS(X, Y: Integer; fuOptions: UINT;
  const ARect: TRect; Text: PChar; Length: Integer);
var
  pCrnt: PChar;
  pTail: PChar;
  pRun: PChar;

  procedure GetSBCharRange;
  begin
    while (pRun <> pTail) and (not (pRun^ in LeadBytes)) do
      Inc(pRun);
  end;

  procedure GetDBCharRange;
  begin
    while (pRun <> pTail) and (pRun^ in LeadBytes) do
      Inc(pRun, 2);
  end;

var
  TmpRect: TRect;
  Len: Integer;
  n: Integer;
begin
  pCrnt := Text;
  pRun := Text;
  pTail := PChar(Integer(Text) + Length);
  TmpRect := ARect;
  while pCrnt < pTail do
  begin
    GetSBCharRange;
    if pRun <> pCrnt then
    begin
      SetTextCharacterExtra(StockDC, FCharExtra + FCrntDx);
      Len := Integer(pRun) - Integer(pCrnt);
      with TmpRect do
      begin
        n := GetCharWidth * Len;
        Right := Min(Left + n + GetCharWidth, ARect.Right);
        Windows.ExtTextOut(StockDC, X, Y, fuOptions, @TmpRect, pCrnt, Len, nil);
        Inc(X, n);
        Inc(Left, n);
      end;
    end;
    pCrnt := pRun;
    if pRun = pTail then
      break;
    
    GetDBCharRange;
    SetTextCharacterExtra(StockDC, FCharExtra + FCrntDBDx);
    Len := Integer(pRun) - Integer(pCrnt);
    with TmpRect do
    begin
      n := GetCharWidth * Len;
      Right := Min(Left + n + GetCharWidth, ARect.Right);
      Windows.ExtTextOut(StockDC, X, Y, fuOptions, @TmpRect, pCrnt, Len, nil);
      Inc(X, n);
      Inc(Left, n);
    end;
    pCrnt := pRun;
  end;

  if (pCrnt = Text) or // maybe Text is not assigned or Length is 0
     (TmpRect.Right < ARect.Right) then
  begin
    SetTextCharacterExtra(StockDC, FCharExtra + FCrntDx);
    Windows.ExtTextOut(StockDC, X, Y, fuOptions, @TmpRect, nil, 0, nil);
  end;
end;

procedure TheTextDrawerEx.ExtTextOutWithETO(X, Y: Integer; fuOptions: UINT;
  const ARect: TRect; Text: PChar; Length: Integer);
begin
  inherited ExtTextOut(X, Y, fuOptions, ARect, Text, Length);
end;

procedure TheTextDrawerEx.TextOutOrExtTextOut(X, Y: Integer;
  fuOptions: UINT; const ARect: TRect; Text: PChar; Length: Integer);
begin
  // this function may be used when:
  //  a. the text does not containing any multi-byte characters
  // AND
  //   a-1. current font is TrueType.
  //   a-2. current font is RasterType and it is not italicic.
  with ARect do
    if Assigned(Text) and (Length > 0) and
       (Left = X) and (Top = Y) and
       ((Bottom - Top) = GetCharHeight) and
       (Left + GetCharWidth * (Length + 1) > Right) then
    Windows.TextOut(StockDC, X, Y, Text, Length)
  else
    Windows.ExtTextOut(StockDC, X, Y, fuOptions, @ARect, Text, Length, nil)
end;

{$IFNDEF HE_LEADBYTES}
procedure InitializeLeadBytes;
var
  c: Char;
begin
  for c := Low(Char) to High(Char) do
    if IsDBCSLeadByte(Byte(c)) then
      Include(LeadBytes, c);
end;
{$ENDIF} // HE_LEADBYTES

initialization

{$IFNDEF HE_LEADBYTES}
  InitializeLeadBytes;
{$ENDIF} 

finalization

  gFontsInfoManager.Free;

end.
