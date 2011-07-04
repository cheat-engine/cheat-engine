{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPrintMargins.pas, released 2000-06-01.

The Initial Author of the Original Code is Morten J. Skovrup.
Portions written by Morten J. Skovrup are copyright 2000 Morten J. Skovrup.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEditPrintMargins.pas,v 1.5 2003/04/30 12:59:48 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------
CONTENTS:
  Class handling all sizes involded when printing.

  Design-time properties
    UnitSystem : The units used to specify sizes in. Internally is allways used mm.
    Left       : Distance from left edge of paper to text.
    Right      : Distance from right edge of paper to text.
    Top        : Distance from top edge of paper to top of text.
    Bottom     : Distance from bottom edge of paper to bottom of text.
    Gutter     : Binding gutter - added to right margin (or left if 2-sided)
    Header     : Distance from top edge of paper to line below header.
    Footer     : Distance from bottom edge of paper to line above footer.
    LeftHFTextIndent  : Distance from left margin to first left-aligned character
                        in header or footer
    RightHFTextIndent : Distance from right margin to last right-aligned character
                        in header or footer
    HFInternalMargin  : Internal margin between top-line and text in header and
                        footer AND between bottom-line and text in header and
                        footer.
    MirrorMargins     : Set if margins should be mirrored (i.e. when printing
                        2-sided).

  Run-time properties
    PLeft   : Left position of text in device units (pixels) - this is the left
              margin minus the left unprintable distance (+ gutter).
    PRight  : Right position of text in device units (pixels) - calculated form
              left.
    PTop    : Top position of text in device units (pixels)  - this is the top
              margin minus the top unprintable distance.
    PBottom : Bottom position of text in device units (pixels) - calculated form
              top.
    PGutter : Binding gutter in device units (pixels)
    PHeader : Header in device units (pixels)
    PFooter : Footer in device units (pixels) - calculated from top
    PLeftHFTextIndent  : Left position of text in header and footer in device
                         units (pixels). Calculated as Left margin + LeftHFTextIndent
    PRightHFTextIndent : Right position of text in header and footer in device
                         units (pixels). Calculated from left
    PHFInternalMargin  : Internal margin in device units (pixels).

  Run-time methods
    InitPage : Called by TSynEditPrint class to initialize margins.
    Assign   : Assign values from another TSynEditPrintMargins object.

-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITPRINTMARGINS}
unit SynEditPrintMargins;
{$ENDIF}
{$M+}

{$I SynEdit.inc }

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditPrintTypes,
  QSynEditPrinterInfo,
{$ELSE}
  Graphics,
  SynEditPrintTypes,
  SynEditPrinterInfo,
{$ENDIF}
  Classes,
  SysUtils;

type
  //Margins class - sorting out dimensions of printable area
  TSynEditPrintMargins = class(TPersistent)
  private
    FLeft, FRight, FTop, FBottom: Double;
    FHeader, FFooter: Double;
    FLeftHFTextIndent: Double;
    FRightHFTextIndent: Double;
    FHFInternalMargin: Double;
    FGutter: Double;
    FMirrorMargins: Boolean;
    FUnitSystem: TUnitSystem;
    function ConvertTo(Value: Double): Double;
    function ConvertFrom(Value: Double): Double;
    function GetBottom: Double;
    function GetFooter: Double;
    function GetGutter: Double;
    function GetHeader: Double;
    function GetLeft: Double;
    function GetRight: Double;
    function GetTop: Double;
    function GetLeftHFTextIndent: Double;
    function GetRightHFTextIndent: Double;
    function GetHFInternalMargin: Double;
    procedure SetBottom(const Value: Double);
    procedure SetFooter(const Value: Double);
    procedure SetGutter(const Value: Double);
    procedure SetHeader(const Value: Double);
    procedure SetLeft(const Value: Double);
    procedure SetRight(const Value: Double);
    procedure SetTop(const Value: Double);
    procedure SetLeftHFTextIndent(const Value: Double);
    procedure SetRightHFTextIndent(const Value: Double);
    procedure SetHFInternalMargin(const Value: Double);
  public
        //When initpage has been called, the following values will reflect the
        //margins in paper units. Note that all values are calculated from
        //left or top of paper (i.e. PRight is distance from left margin)
    PLeft, PRight, PTop, PBottom: Integer;
    PHeader, PFooter: Integer;
    PLeftHFTextIndent: Integer;
    PRightHFTextIndent: Integer;
    PHFInternalMargin: Integer;
    PGutter: Integer;
    constructor Create;
    procedure InitPage(ACanvas: TCanvas; PageNum: Integer;
      PrinterInfo: TSynEditPrinterInfo; LineNumbers,
      LineNumbersInMargin: Boolean; MaxLineNum: Integer);
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  published
    property UnitSystem: TUnitSystem read FUnitSystem write FUnitSystem
      default usMM;
    property Left: Double read GetLeft write SetLeft;
    property Right: Double read GetRight write SetRight;
    property Top: Double read GetTop write SetTop;
    property Bottom: Double read GetBottom write SetBottom;
    property Header: Double read GetHeader write SetHeader;
    property Footer: Double read GetFooter write SetFooter;
    property LeftHFTextIndent: Double read GetLeftHFTextIndent
      write SetLeftHFTextIndent;
    property RightHFTextIndent: Double read GetRightHFTextIndent
      write SetRightHFTextIndent;
    property HFInternalMargin: Double read GetHFInternalMargin
      write SetHFInternalMargin;
    property Gutter: Double read GetGutter write SetGutter;
    property MirrorMargins: Boolean read FMirrorMargins write FMirrorMargins;
  end;

implementation

{ TSynEditPrintMargins }
const
  mmPrInch = 25.4;
  mmPrCm = 10;

constructor TSynEditPrintMargins.Create;
begin
  inherited;
  FUnitSystem := usMM;
  FLeft := DefLeft;
  FRight := DefRight;
  FTop := DefTop;
  FBottom := DefBottom;
  FHeader := DefHeader;
  FFooter := DefFooter;
  FLeftHFTextIndent := DefLeftHFTextIndent;
  FRightHFTextIndent := DefRightHFTextIndent;
  FHFInternalMargin := DefHFInternalMargin;
  FGutter := DefGutter;
  FMirrorMargins := False;
end;

function TSynEditPrintMargins.ConvertTo(Value: Double): Double;
{Convert Value to mm}
begin
  case FUnitSystem of
    usCM: Result := Value * mmPrCm;
    usInch: Result := Value * mmPrInch;
    muThousandthsOfInches: Result := mmPrInch * Value / 1000;
  else
    Result := Value;
  end;
end;

function TSynEditPrintMargins.ConvertFrom(Value: Double): Double;
{Convert from mm to selected UnitSystem}
begin
  case FUnitSystem of
    usCM: Result := Value / mmPrCm;
    usInch: Result := Value / mmPrInch;
    muThousandthsOfInches: Result := 1000 * Value / mmPrInch;
  else
    Result := Value;
  end;
end;

function TSynEditPrintMargins.GetBottom: Double;
begin
  Result := ConvertFrom(FBottom);
end;

function TSynEditPrintMargins.GetFooter: Double;
begin
  Result := ConvertFrom(FFooter);
end;

function TSynEditPrintMargins.GetGutter: Double;
begin
  Result := ConvertFrom(FGutter);
end;

function TSynEditPrintMargins.GetHeader: Double;
begin
  Result := ConvertFrom(FHeader);
end;

function TSynEditPrintMargins.GetLeft: Double;
begin
  Result := ConvertFrom(FLeft);
end;

function TSynEditPrintMargins.GetRight: Double;
begin
  Result := ConvertFrom(FRight);
end;

function TSynEditPrintMargins.GetTop: Double;
begin
  Result := ConvertFrom(FTop);
end;

function TSynEditPrintMargins.GetLeftHFTextIndent: Double;
begin
  Result := ConvertFrom(FLeftHFTextIndent);
end;

function TSynEditPrintMargins.GetRightHFTextIndent: Double;
begin
  Result := ConvertFrom(FRightHFTextIndent);
end;

function TSynEditPrintMargins.GetHFInternalMargin: Double;
begin
  Result := ConvertFrom(FHFInternalMargin);
end;

procedure TSynEditPrintMargins.SetBottom(const Value: Double);
begin
  FBottom := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetFooter(const Value: Double);
begin
  FFooter := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetGutter(const Value: Double);
begin
  FGutter := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetHeader(const Value: Double);
begin
  FHeader := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetLeft(const Value: Double);
begin
  FLeft := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetRight(const Value: Double);
begin
  FRight := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetTop(const Value: Double);
begin
  FTop := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetLeftHFTextIndent(const Value: Double);
begin
  FLeftHFTextIndent := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetRightHFTextIndent(const Value: Double);
begin
  FRightHFTextIndent := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetHFInternalMargin(const Value: Double);
begin
  FHFInternalMargin := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.InitPage(ACanvas: TCanvas; PageNum: Integer;
  PrinterInfo: TSynEditPrinterInfo; LineNumbers, LineNumbersInMargin: Boolean;
  MaxLineNum: Integer);
//Calculate the P... values
begin
  if FMirrorMargins and ((PageNum mod 2) = 0) then begin
    PLeft := PrinterInfo.PixFromLeft(FRight);
    PRight := PrinterInfo.PrintableWidth - PrinterInfo.PixFromRight(FLeft + FGutter);
  end
  else begin
    PLeft := PrinterInfo.PixFromLeft(FLeft + FGutter);
    PRight := PrinterInfo.PrintableWidth - PrinterInfo.PixFromRight(FRight);
  end;
  if LineNumbers and (not LineNumbersInMargin) then
    PLeft := PLeft + ACanvas.TextWidth(IntToStr(MaxLineNum) + ': ');
  PTop := PrinterInfo.PixFromTop(FTop);
  PBottom := PrinterInfo.PrintableHeight - PrinterInfo.PixFromBottom(FBottom);
  PHeader := PrinterInfo.PixFromTop(FHeader);
  PFooter := PrinterInfo.PrintableHeight - PrinterInfo.PixFromBottom(FFooter);
  PHFInternalMargin := Round(PrinterInfo.YPixPrmm * FHFInternalMargin);
  PGutter := Round(PrinterInfo.XPixPrmm * FGutter);
  PRightHFTextIndent := PRight - Round(PrinterInfo.XPixPrmm * FRightHFTextIndent);
  PLeftHFTextIndent := PLeft + Round(PrinterInfo.XPixPrmm * FLeftHFTextIndent);
end;

procedure TSynEditPrintMargins.Assign(Source: TPersistent);
var
  Src: TSynEditPrintMargins;
begin
  if (Source <> nil) and (Source is TSynEditPrintMargins) then begin
    Src := TSynEditPrintMargins(Source);
    FLeft := Src.FLeft;
    FRight := Src.FRight;
    FTop := Src.FTop;
    FBottom := Src.FBottom;
    FHeader := Src.FHeader;
    FFooter := Src.FFooter;
    FLeftHFTextIndent := Src.FLeftHFTextIndent;
    FRightHFTextIndent := Src.FRightHFTextIndent;
    FHFInternalMargin := Src.FHFInternalMargin;
    FGutter := Src.FGutter;
    FMirrorMargins := Src.FMirrorMargins;
    FUnitSystem := Src.FUnitSystem;
  end else
    inherited;
end;

procedure TSynEditPrintMargins.LoadFromStream(AStream: TStream);
begin
  // we read all our values in MM
  with AStream do begin
    Read(FUnitSystem, SizeOf(FUnitSystem));
    Read(FLeft, SizeOf(FLeft));
    Read(FRight, SizeOf(FRight));
    Read(FTop, SizeOf(FTop));
    Read(FBottom, SizeOf(FBottom));
    Read(FHeader, SizeOf(FHeader));
    Read(FFooter, SizeOf(FFooter));
    Read(FLeftHFTextIndent, SizeOf(FLeftHFTextIndent));
    Read(FRightHFTextIndent, SizeOf(FRightHFTextIndent));
    Read(FHFInternalMargin, SizeOf(FHFInternalMargin));
    Read(FGutter, SizeOf(FGutter));
    Read(FMirrorMargins, SizeOf(FMirrorMargins));
  end;
end;

procedure TSynEditPrintMargins.SaveToStream(AStream: TStream);
begin
  // we always write our values in MM
  with AStream do begin
    Write(FUnitSystem, SizeOf(FUnitSystem));
    Write(FLeft, SizeOf(FLeft));
    Write(FRight, SizeOf(FRight));
    Write(FTop, SizeOf(FTop));
    Write(FBottom, SizeOf(FBottom));
    Write(FHeader, SizeOf(FHeader));
    Write(FFooter, SizeOf(FFooter));
    Write(FLeftHFTextIndent, SizeOf(FLeftHFTextIndent));
    Write(FRightHFTextIndent, SizeOf(FRightHFTextIndent));
    Write(FHFInternalMargin, SizeOf(FHFInternalMargin));
    Write(FGutter, SizeOf(FGutter));
    Write(FMirrorMargins, SizeOf(FMirrorMargins));
  end;
end;

end.

