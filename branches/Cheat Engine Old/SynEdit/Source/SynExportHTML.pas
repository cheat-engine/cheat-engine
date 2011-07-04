{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynExportHTML.pas, released 2000-04-16.

The Original Code is partly based on the mwHTMLExport.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Hieke.
Portions created by Michael Hieke are Copyright 2000 Michael Hieke.
Portions created by James D. Jacobson are Copyright 1999 Martin Waldenburg.
Changes to emit XHTML 1.0 Strict complying code by Maël Hörz.
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

$Id: SynExportHTML.pas,v 1.21 2005/12/02 18:50:01 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEXPORTHTML}
unit SynExportHTML;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Qt,
  QGraphics,
  QSynEditExport,
  QSynEditHighlighter,
{$ELSE}
  Windows,
  Graphics,
  SynEditExport,
  SynEditHighlighter,
{$ENDIF}
  Classes;

type
  TSynExporterHTML = class(TSynCustomExporter)
  private
    fLastAttri: TSynHighlighterAttributes;
    function AttriToCSS(Attri: TSynHighlighterAttributes;
      UniqueAttriName: string): string;
    function AttriToCSSCallback(Highlighter: TSynCustomHighlighter;
      Attri: TSynHighlighterAttributes; UniqueAttriName: string;
      Params: array of Pointer): Boolean;
    function ColorToHTML(AColor: TColor): string;
    function GetStyleName(Highlighter: TSynCustomHighlighter;
      Attri: TSynHighlighterAttributes): string;
    function MakeValidName(Name: string): string;
    function StyleNameCallback(Highlighter: TSynCustomHighlighter;
      Attri: TSynHighlighterAttributes; UniqueAttriName: string;
      Params: array of Pointer): Boolean;
  protected
    fCreateHTMLFragment: boolean;
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
    procedure SetTokenAttribute(Attri: TSynHighlighterAttributes); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Color;
    property CreateHTMLFragment: boolean read fCreateHTMLFragment
      write fCreateHTMLFragment default FALSE;
    property DefaultFilter;
    property Font;
    property Highlighter;
    property Title;
    property UseBackground;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditMiscProcs,
  QSynEditStrConst,
  QSynHighlighterMulti,
{$ELSE}
  SynEditMiscProcs,
  SynEditStrConst,  
  SynHighlighterMulti,
{$ENDIF}
  SysUtils;


{ TSynExporterHTML }

constructor TSynExporterHTML.Create(AOwner: TComponent);
const
  CF_HTML = 'HTML Format';
begin
  inherited Create(AOwner);
  {**************}
  {$IFNDEF SYN_CLX}
  fClipboardFormat := RegisterClipboardFormat(CF_HTML);
  {$ENDIF}
  fDefaultFilter := SYNS_FilterHTML;
  // setup array of chars to be replaced
  fReplaceReserved['&'] := '&amp;';
  fReplaceReserved['<'] := '&lt;';
  fReplaceReserved['>'] := '&gt;';
  fReplaceReserved['"'] := '&quot;';
  fReplaceReserved['™'] := '&trade;';
  fReplaceReserved['©'] := '&copy;';
  fReplaceReserved['®'] := '&reg;';
  fReplaceReserved['À'] := '&Agrave;';
  fReplaceReserved['Á'] := '&Aacute;';
  fReplaceReserved['Â'] := '&Acirc;';
  fReplaceReserved['Ã'] := '&Atilde;';
  fReplaceReserved['Ä'] := '&Auml;';
  fReplaceReserved['Å'] := '&Aring;';
  fReplaceReserved['Æ'] := '&AElig;';
  fReplaceReserved['Ç'] := '&Ccedil;';
  fReplaceReserved['È'] := '&Egrave;';
  fReplaceReserved['É'] := '&Eacute;';
  fReplaceReserved['Ê'] := '&Ecirc;';
  fReplaceReserved['Ë'] := '&Euml;';
  fReplaceReserved['Ì'] := '&Igrave;';
  fReplaceReserved['Í'] := '&Iacute;';
  fReplaceReserved['Î'] := '&Icirc;';
  fReplaceReserved['Ï'] := '&Iuml;';
  fReplaceReserved['Ð'] := '&ETH;';
  fReplaceReserved['Ñ'] := '&Ntilde;';
  fReplaceReserved['Ò'] := '&Ograve;';
  fReplaceReserved['Ó'] := '&Oacute;';
  fReplaceReserved['Ô'] := '&Ocirc;';
  fReplaceReserved['Õ'] := '&Otilde;';
  fReplaceReserved['Ö'] := '&Ouml;';
  fReplaceReserved['Ø'] := '&Oslash;';
  fReplaceReserved['Ù'] := '&Ugrave;';
  fReplaceReserved['Ú'] := '&Uacute;';
  fReplaceReserved['Û'] := '&Ucirc;';
  fReplaceReserved['Ü'] := '&Uuml;';
  fReplaceReserved['Ý'] := '&Yacute;';
  fReplaceReserved['Þ'] := '&THORN;';
  fReplaceReserved['ß'] := '&szlig;';
  fReplaceReserved['à'] := '&agrave;';
  fReplaceReserved['á'] := '&aacute;';
  fReplaceReserved['â'] := '&acirc;';
  fReplaceReserved['ã'] := '&atilde;';
  fReplaceReserved['ä'] := '&auml;';
  fReplaceReserved['å'] := '&aring;';
  fReplaceReserved['æ'] := '&aelig;';
  fReplaceReserved['ç'] := '&ccedil;';
  fReplaceReserved['è'] := '&egrave;';
  fReplaceReserved['é'] := '&eacute;';
  fReplaceReserved['ê'] := '&ecirc;';
  fReplaceReserved['ë'] := '&euml;';
  fReplaceReserved['ì'] := '&igrave;';
  fReplaceReserved['í'] := '&iacute;';
  fReplaceReserved['î'] := '&icirc;';
  fReplaceReserved['ï'] := '&iuml;';
  fReplaceReserved['ð'] := '&eth;';
  fReplaceReserved['ñ'] := '&ntilde;';
  fReplaceReserved['ò'] := '&ograve;';
  fReplaceReserved['ó'] := '&oacute;';
  fReplaceReserved['ô'] := '&ocirc;';
  fReplaceReserved['õ'] := '&otilde;';
  fReplaceReserved['ö'] := '&ouml;';
  fReplaceReserved['ø'] := '&oslash;';
  fReplaceReserved['ù'] := '&ugrave;';
  fReplaceReserved['ú'] := '&uacute;';
  fReplaceReserved['û'] := '&ucirc;';
  fReplaceReserved['ü'] := '&uuml;';
  fReplaceReserved['ý'] := '&yacute;';
  fReplaceReserved['þ'] := '&thorn;';
  fReplaceReserved['ÿ'] := '&yuml;';
  fReplaceReserved['¡'] := '&iexcl;';
  fReplaceReserved['¢'] := '&cent;';
  fReplaceReserved['£'] := '&pound;';
  fReplaceReserved['¤'] := '&curren;';
  fReplaceReserved['¥'] := '&yen;';
  fReplaceReserved['¦'] := '&brvbar;';
  fReplaceReserved['§'] := '&sect;';
  fReplaceReserved['¨'] := '&uml;';
  fReplaceReserved['ª'] := '&ordf;';
  fReplaceReserved['«'] := '&laquo;';
  fReplaceReserved['¬'] := '&shy;';
  fReplaceReserved['¯'] := '&macr;';
  fReplaceReserved['°'] := '&deg;';
  fReplaceReserved['±'] := '&plusmn;';
  fReplaceReserved['²'] := '&sup2;';
  fReplaceReserved['³'] := '&sup3;';
  fReplaceReserved['´'] := '&acute;';
  fReplaceReserved['µ'] := '&micro;';
  fReplaceReserved['·'] := '&middot;';
  fReplaceReserved['¸'] := '&cedil;';
  fReplaceReserved['¹'] := '&sup1;';
  fReplaceReserved['º'] := '&ordm;';
  fReplaceReserved['»'] := '&raquo;';
  fReplaceReserved['¼'] := '&frac14;';
  fReplaceReserved['½'] := '&frac12;';
  fReplaceReserved['¾'] := '&frac34;';
  fReplaceReserved['¿'] := '&iquest;';
  fReplaceReserved['×'] := '&times;';
  fReplaceReserved['÷'] := '&divide';
  fReplaceReserved['€'] := '&euro;';
end;

function TSynExporterHTML.AttriToCSS(Attri: TSynHighlighterAttributes;
  UniqueAttriName: string): string;
var
  StyleName: string;
begin
  StyleName := MakeValidName(UniqueAttriName);

  Result := '.' + StyleName + ' { ';
  if UseBackground and (Attri.Background <> clNone) then
    Result := Result + 'background-color: ' + ColorToHTML(Attri.Background) + '; ';
  if Attri.Foreground <> clNone then
    Result := Result + 'color: ' + ColorToHTML(Attri.Foreground) + '; ';

  if fsBold in Attri.Style then
    Result := Result + 'font-weight: bold; ';
  if fsItalic in Attri.Style then
    Result := Result + 'font-style: italic; ';
  if fsUnderline in Attri.Style then
    Result := Result + 'text-decoration: underline; ';
  if fsStrikeOut in Attri.Style then
    Result := Result + 'text-decoration: line-through; ';

  Result := Result + '}';
end;

function TSynExporterHTML.AttriToCSSCallback(Highlighter: TSynCustomHighlighter;
  Attri: TSynHighlighterAttributes; UniqueAttriName: string;
  Params: array of Pointer): Boolean;
var
  Styles: ^string;
begin
  Styles := Params[0];
  Styles^ := Styles^ + AttriToCSS(Attri, UniqueAttriName) + #13#10;  
  Result := True; // we want all attributes => tell EnumHighlighterAttris to continue
end;

function TSynExporterHTML.ColorToHTML(AColor: TColor): string;
var
  RGBColor: longint;
  RGBValue: byte;
const
  Digits: array[0..15] of char = '0123456789ABCDEF';
begin
  RGBColor := ColorToRGB(AColor);
  Result := '#000000';
  RGBValue := GetRValue(RGBColor);
  if RGBValue > 0 then begin
    Result[2] := Digits[RGBValue shr  4];
    Result[3] := Digits[RGBValue and 15];
  end;
  RGBValue := GetGValue(RGBColor);
  if RGBValue > 0 then begin
    Result[4] := Digits[RGBValue shr  4];
    Result[5] := Digits[RGBValue and 15];
  end;
  RGBValue := GetBValue(RGBColor);
  if RGBValue > 0 then begin
    Result[6] := Digits[RGBValue shr  4];
    Result[7] := Digits[RGBValue and 15];
  end;
end;

procedure TSynExporterHTML.FormatAfterLastAttribute;
begin
  AddData('</span>');
end;

procedure TSynExporterHTML.FormatAttributeDone(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
begin
  AddData('</span>');
end;

procedure TSynExporterHTML.FormatAttributeInit(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
var
  StyleName: string;
begin
  StyleName := GetStyleName(Highlighter, fLastAttri);
  AddData(Format('<span class="%s">', [StyleName]));
end;

procedure TSynExporterHTML.FormatBeforeFirstAttribute(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
var
  StyleName: string;
begin
  StyleName := GetStyleName(Highlighter, fLastAttri);
  AddData(Format('<span class="%s">', [StyleName]));
end;

procedure TSynExporterHTML.FormatNewLine;
begin
  AddNewLine;
end;

function TSynExporterHTML.GetFooter: string;
begin
  Result := '';
  if fExportAsText then
    Result := '</span>'#13#10'</code></pre>'#13#10
  else
    Result := '</code></pre><!--EndFragment-->';
  if not(fCreateHTMLFragment and fExportAsText) then
    Result := Result + '</body>'#13#10'</html>';
end;

function TSynExporterHTML.GetFormatName: string;
begin
  Result := SYNS_ExporterFormatHTML;
end;

function TSynExporterHTML.GetHeader: string;
const
  DescriptionSize = 105;
  FooterSize1 = 47;
  FooterSize2 = 31;
  NativeHeader = 'Version:0.9'#13#10 +
                 'StartHTML:%.10d'#13#10 +
                 'EndHTML:%.10d'#13#10 +
                 'StartFragment:%.10d'#13#10 +
                 'EndFragment:%.10d'#13#10;
  HTMLAsTextHeader = '<?xml version="1.0" encoding="iso-8859-1"?>'#13#10 +
                     '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">'#13#10 +
                     '<html xmlns="http://www.w3.org/1999/xhtml">'#13#10 +
                     '<head>'#13#10 +
                     '<title>%s</title>'#13#10 +
                     '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />'#13#10 +
                     '<meta name="generator" content="SynEdit HTML exporter" />'#13#10 +
                     '<style type="text/css">'#13#10 +
                     '<!--'#13#10 +
                     'body { color: %s; background-color: %s; }'#13#10 +
                     '%s' +
                     '-->'#13#10 +
                     '</style>'#13#10 +
                     '</head>'#13#10 +
                     '<body>'#13#10;
var
  Styles, Header, Header2: string;
begin
  EnumHighlighterAttris(Highlighter, True, AttriToCSSCallback, [@Styles]);

  Header := Format(HTMLAsTextHeader, [Title, ColorToHtml(fFont.Color),
    ColorToHTML(fBackgroundColor), Styles]);

  Result := '';
  if fExportAsText then
  begin
    if not fCreateHTMLFragment then
      Result := Header;

    Result := Result + Format('<pre>'#13#10'<code><span style="font: %dpt %s;">',
      [fFont.Size, fFont.Name]);
  end
  else
  begin
    // Described in http://msdn.microsoft.com/library/sdkdoc/htmlclip/htmlclipboard.htm
    Header2 := '<!--StartFragment--><pre><code>';
    Result := Format(NativeHeader, [DescriptionSize,
      DescriptionSize + Length(Header) + Length(Header2) + GetBufferSize + FooterSize1,
      DescriptionSize + Length(Header),
      DescriptionSize + Length(Header) + Length(Header2) + GetBufferSize + FooterSize2]);
    Result := Result + Header + Header2;
  end;
end;

function TSynExporterHTML.GetStyleName(Highlighter: TSynCustomHighlighter;
  Attri: TSynHighlighterAttributes): string;
begin
  EnumHighlighterAttris(Highlighter, False, StyleNameCallback, [Attri, @Result]);
end;

function TSynExporterHTML.MakeValidName(Name: string): string;
var
  i: Integer;
begin
  Result := LowerCase(Name);
  for i := Length(Result) downto 1 do
    if Result[i] in ['.', '_'] then
      Result[i] := '-'
    else if not(Result[i] in ['a'..'z', '0'..'9', '-']) then
      Delete(Result, i, 1);
end;

procedure TSynExporterHTML.SetTokenAttribute(Attri: TSynHighlighterAttributes);
begin
  fLastAttri := Attri;
  inherited;
end;

function TSynExporterHTML.StyleNameCallback(Highlighter: TSynCustomHighlighter;
    Attri: TSynHighlighterAttributes; UniqueAttriName: string;
    Params: array of Pointer): Boolean;
var
  AttriToFind: TSynHighlighterAttributes;
  StyleName: ^string;
begin
  AttriToFind := Params[0];
  StyleName := Params[1];

  if Attri = AttriToFind then
  begin
    StyleName^ := MakeValidName(UniqueAttriName);
    Result := False; // found => inform EnumHighlighterAttris to stop searching
  end
  else
    Result := True;
end;

end.

