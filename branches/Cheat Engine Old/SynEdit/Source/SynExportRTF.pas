{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynExportRTF.pas, released 2000-04-16.

The Original Code is partly based on the mwRTFExport.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Hieke.
Portions created by Michael Hieke are Copyright 2000 Michael Hieke.
Portions created by James D. Jacobson are Copyright 1999 Martin Waldenburg.
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

$Id: SynExportRTF.pas,v 1.10 2004/07/27 14:24:02 markonjezic Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEXPORTRTF}
unit SynExportRTF;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
  {$IFDEF SYN_CLX}
  Qt, QGraphics,
  QSynEditExport,
  {$ELSE}
  Windows, Graphics, RichEdit,
  SynEditExport,
  {$ENDIF}
  Classes;

type
  TSynExporterRTF = class(TSynCustomExporter)
  private
    fAttributesChanged: boolean;
    fListColors: TList;
    function ColorToRTF(AColor: TColor): string;
    function GetColorIndex(AColor: TColor): integer;
  protected
    procedure FormatAfterLastAttribute; override;
    procedure FormatAttributeDone(BackgroundChanged, ForegroundChanged: boolean;
      FontStylesChanged: TFontStyles); override;
    procedure FormatAttributeInit(BackgroundChanged, ForegroundChanged: boolean;
      FontStylesChanged: TFontStyles); override;
    procedure FormatBeforeFirstAttribute(BackgroundChanged,
      ForegroundChanged: boolean; FontStylesChanged: TFontStyles); override;
    procedure FormatNewLine; override;
    function GetFooter: string; override;
    function GetFormatName: string; override;
    function GetHeader: string; override;
{$IFDEF SYN_MBCSSUPPORT}
    function ReplaceMBCS(Char1, Char2: char): string; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
  published
    property Color;
    property DefaultFilter;
    property Font;
    property Highlighter;
    property Title;
    property UseBackground;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst,
  QSynEditMiscProcs,
{$ELSE}
  SynEditStrConst,
{$ENDIF}
  SysUtils;

{ TSynExporterRTF }

constructor TSynExporterRTF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fListColors := TList.Create;
  fDefaultFilter := SYNS_FilterRTF;
  {*****************}
{$IFNDEF SYN_CLX}
  fClipboardFormat := RegisterClipboardFormat(CF_RTF);
{$ENDIF}
  // setup array of chars to be replaced
  fReplaceReserved['\'] := '\\';
  fReplaceReserved['{'] := '\{';
  fReplaceReserved['}'] := '\}';
end;

destructor TSynExporterRTF.Destroy;
begin
  fListColors.Free;
  fListColors := nil;
  inherited Destroy;
end;

procedure TSynExporterRTF.Clear;
begin
  inherited Clear;
  if Assigned(fListColors) then
    fListColors.Clear;
end;

function TSynExporterRTF.ColorToRTF(AColor: TColor): string;
var
  Col: Integer;
begin
  Col := ColorToRGB(AColor);
  {*****************}
  Result := Format('\red%d\green%d\blue%d;', [GetRValue(Col), GetGValue(Col),
    GetBValue(Col)]);
end;

procedure TSynExporterRTF.FormatAfterLastAttribute;
begin
  // no need to reset the font style here...
end;

procedure TSynExporterRTF.FormatAttributeDone(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
const
  FontTags: array[TFontStyle] of string = ('\b0', '\i0', '\ul0', '\strike0');
var
  AStyle: TFontStyle;
begin
  // nothing to do about the color, but reset the font style
  for AStyle := Low(TFontStyle) to High(TFontStyle) do begin
    if AStyle in FontStylesChanged then begin
      fAttributesChanged := TRUE;
      AddData(FontTags[AStyle]);
    end;
  end;
end;

procedure TSynExporterRTF.FormatAttributeInit(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
const
  FontTags: array[TFontStyle] of string = ('\b', '\i', '\ul', '\strike');
var
  AStyle: TFontStyle;
begin
  // background color
  if BackgroundChanged then begin
    AddData(Format('\cb%d', [GetColorIndex(fLastBG)]));
    fAttributesChanged := TRUE;
  end;
  // text color
  if ForegroundChanged then begin
    AddData(Format('\cf%d', [GetColorIndex(fLastFG)]));
    fAttributesChanged := TRUE;
  end;
  // font styles
  for AStyle := Low(TFontStyle) to High(TFontStyle) do
    if AStyle in FontStylesChanged then begin
      AddData(FontTags[AStyle]);
      fAttributesChanged := TRUE;
    end;
  if fAttributesChanged then begin
    AddData(' ');
    fAttributesChanged := FALSE;
  end;
end;

procedure TSynExporterRTF.FormatBeforeFirstAttribute(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
begin
  FormatAttributeInit(BackgroundChanged, ForegroundChanged, FontStylesChanged);
end;

procedure TSynExporterRTF.FormatNewLine;
begin
  AddData(#13#10'\par ');
end;

function TSynExporterRTF.GetColorIndex(AColor: TColor): integer;
begin
  Result := fListColors.IndexOf(pointer(AColor));
  if Result = -1 then
    Result := fListColors.Add(pointer(AColor));
end;

function TSynExporterRTF.GetFooter: string;
begin
  Result := '}';
end;

function TSynExporterRTF.GetFormatName: string;
begin
  Result := SYNS_ExporterFormatRTF;
end;

function TSynExporterRTF.GetHeader: string;
var
  i: integer;

  function GetFontTable: string;
  begin
{$IFDEF SYN_MBCSSUPPORT}
    if ByteType(Font.Name, 1) <> mbSingleByte then begin
      Result := '{\fonttbl{\f0\fnil\fcharset134 ' +
        ReplaceReservedChars(Font.Name)
    end else
{$ENDIF}
      Result := '{\fonttbl{\f0\fmodern ' + Font.Name;
    Result := Result + ';}}'#13#10;
  end;

begin
  Result := '{\rtf1\ansi\deff0\deftab720' + GetFontTable;
  // all the colors
  Result := Result + '{\colortbl';
  for i := 0 to fListColors.Count - 1 do
    Result := Result + ColorToRTF(TColor(fListColors[i]));
  Result := Result + '}'#13#10;
  // title and creator comment
  Result := Result + '{\info{\comment Generated by the SynEdit RTF ' +
    'exporter}'#13#10;
  Result := Result + '{\title ' + fTitle + '}}'#13#10;
  if fUseBackground then
    Result := Result + { TODO } #13#10;
  Result := Result + Format('\deflang1033\pard\plain\f0\fs%d ',
    [2 * Font.Size]);
end;

{$IFDEF SYN_MBCSSUPPORT}
function TSynExporterRTF.ReplaceMBCS(Char1, Char2: char): string;
begin
  Result := Format('\''%x\''%x', [integer(Char1), integer(Char2)]);
end;
{$ENDIF}

end.

