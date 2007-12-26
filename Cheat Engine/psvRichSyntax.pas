{*******************************************************}
{               RichEdit Syntax HighLight               }
{                     version 3.2                       }
{ Author:                                               }
{ Serhiy Perevoznyk                                     }
{ serge_perevoznyk@hotmail.com                          }
{                                                       }
{ Anderson Rodrigues Barbieri                           }
{                                                       }
{*******************************************************}

{The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: psvSyntax.pas, released 2002-30-04
Initial Author of these files is Serhiy Perevoznyk.
All Rights Reserved.}

unit psvRichSyntax;

Interface

uses
  Windows, SysUtils, Classes, Graphics;

type
  TpsvRTFSyntax = class
  private
    FRTFHeader   : string;
    FRTFFooter   : string;
    FNewLine    : string;
    FColorTable : string;
    FFontTable  : string;
    FCodePage   : integer;
  protected
    property   RTFHeader : string read FRTFHeader write FRTFHeader;
    property   RTFFooter : string read FRTFFooter write FRTFFooter;
    property   ColorTable : string read FColorTable write FColorTable;
    property   FontTable  : string read FFontTable write FFontTable;
    procedure  PrepareToken(var AToken : string); virtual;
    function   PrepareOutput(Attr : integer; AToken : string) : string; virtual;
    procedure  Next; virtual;
    function   GetEOL : boolean; virtual;
    function   GetToken : string; virtual;
    function   GetTokenAttribute : integer; virtual;
    function   ColorToStr(AColor: TColor): String; virtual;
    procedure  SetLine(NewValue: string; LineNumber:Integer); virtual;
  public
    constructor Create; virtual;
    procedure   SetText(Atext : string); virtual;
    procedure   CreateFontTable(const AFonts : array of TFont); virtual;
    procedure   CreateColorTable(const AColors : array of TColor); virtual;
    procedure   ConvertToRTFStream(AStream : TStream); virtual;
    function    ConvertToRTFString : string; virtual;
    procedure   SetupDefaultColors; virtual;
    property    CodePage : integer read FCodePage write FCodePage;
  end;

implementation

{ TpsvRTFSyntax }

function TpsvRTFSyntax.ColorToStr(AColor: TColor): String;
begin
 Result:='\red'+IntToStr(GetRValue(ColorToRGB(AColor)))+
          '\green'+IntToStr(GetGValue(ColorToRGB(AColor)))+
          '\blue'+IntToStr(GetBValue(ColorToRGB(AColor)))+';';

end;

procedure TpsvRTFSyntax.ConvertToRTFStream(AStream: TStream);
var
  Attr : integer;
  St : string;
  OutSt : string;
begin
  AStream.Write(RTFHeader[1],Length(RTFHeader));
  AStream.Write(FFontTable[1], Length(FFontTable));
  AStream.Write(RTFFooter[1], Length(RTFFooter));
  AStream.Write(FColorTable[1],Length(FColorTable));
  AStream.Write(FNewLine[1],Length(FNewLine));
  AStream.Write('\f0 ',4);
  While not GetEol do
   begin
     St := GetToken;
     Attr := GetTokenAttribute;
     PrepareToken(St);
     OutSt := PrepareOutput(Attr,St);

     if (St = #13#10) or (St = #13) then
       AStream.Write('\par ',4);

     AStream.Write(OutSt[1],Length(OutSt));
     Next;
   end;
   AStream.Write('\par }',6);  //,6
//  AStream.Write('\par }',6);
end;

function TpsvRTFSyntax.ConvertToRTFString: string;
var St : TStringStream;
begin
  St := TStringStream.Create('');
  ConvertToRTFStream(St);
  Result := St.DataString;
  St.Free;
end;

constructor TpsvRTFSyntax.Create;
var
 Font : TFont;
begin
  inherited;
  FCodePage := 1252;
  Font := TFont.Create;
  Font.Name := 'Courier New';
  CreateFontTable([Font]);
  Font.Free;
  RTFHeader := '{\rtf1\ansi\ansicpg'+IntToStr(FCodePage)+'\deff0\deftab720{\fonttbl';
  RTFFooter :=  '}'#13#10;
  FNewLine := '\deflang1033\pard\plain\f0\fs20';
  FColorTable := '';
end;

procedure TpsvRTFSyntax.CreateColorTable(const AColors: array of TColor);
var I : integer ;
begin
  FColorTable := '{\colortbl\red0\green0\blue0;';
  for I := 0 to Length(AColors) - 1 do
   FColorTable := FColorTable + ColorToStr(AColors[i]);
  FColorTable := FColorTable + '}'+ #13#10;
end;

procedure TpsvRTFSyntax.CreateFontTable(const AFonts: array of TFont);
var  DC: HDC;
     SaveFont: HFont;
     Metrics: TTextMetric;
     Temp:byte;
     I: Integer;
     charset,family:string;
begin
  FFontTable := '';

  for i := 0 to Length(AFonts) - 1 do
  begin
    DC := GetDC(0);
    try
      SaveFont := SelectObject(DC, AFonts[i].Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
    finally
      ReleaseDC(0, DC);
    end;

   case Metrics.tmCharSet of
    ANSI_CHARSET     : charset:='fcharset0';
    DEFAULT_CHARSET  : charset:='fcharset1';
    SYMBOL_CHARSET   : charset:='fcharset2';
    SHIFTJIS_CHARSET : charset:='fcharset128';
    OEM_CHARSET      : charset:='fcharset255';
      else charset:='';
   end;

   Temp:=Metrics.tmPitchAndFamily;
   Temp:= (Temp shr 4) shl 4;

   case Temp of
    FF_DECORATIVE: family:='fdecorative';
    FF_DONTCARE:   family:='fdontcare';
    FF_MODERN:     family:='fmodern';
    FF_ROMAN:      family:='froman';
    FF_SCRIPT:     family:='fscript';
    FF_SWISS:      family:='fswiss';
    else
      family:='froman';
  end;
   FFontTable := FFontTable + '{\f'+IntToStr(i)+'\'+family+'\'+charset+' '+ Afonts[i].name+';}';
 end;
end;

function TpsvRTFSyntax.GetEOL: boolean;
begin
 result := true;
end;

function TpsvRTFSyntax.GetToken: string;
begin
  result := '';
end;

function TpsvRTFSyntax.GetTokenAttribute: integer;
begin
  result := 0;
end;

function TpsvRTFSyntax.PrepareOutput(Attr: integer; AToken : string): string;
begin
  Result := Format('\cf%d %s',[Attr,AToken]);
end;

procedure TpsvRTFSyntax.PrepareToken(var AToken: string);
begin
  //
end;

procedure TpsvRTFSyntax.Next;
begin
 //
end;

procedure TpsvRTFSyntax.SetLine(NewValue: string; LineNumber:Integer);
begin
 //
end;

procedure TpsvRTFSyntax.SetText(AText : string);
begin
  SetLine(AText,1);
end;



procedure TpsvRTFSyntax.SetupDefaultColors;
begin
  //
end;

end.