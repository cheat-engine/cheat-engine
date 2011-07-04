{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditHighlighter.pas, released 2000-04-07.

The Original Code is based on mwHighlighter.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

$Id: SynEditHighlighter.pas,v 1.36 2004/07/10 21:38:29 markonjezic Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITHIGHLIGHTER}
unit SynEditHighlighter;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  kTextDrawer,
  Types,
  QGraphics,
  QSynEditTypes,
  QSynEditMiscClasses,
{$ELSE}
  Graphics,
  Windows,
  Registry,
  IniFiles,
  SynEditTypes,
  SynEditMiscClasses,
{$ENDIF}
  SysUtils,
  Classes;

{$IFNDEF SYN_CLX}
type
  TBetterRegistry = SynEditMiscClasses.TBetterRegistry;
{$ENDIF}

type
  TSynHighlighterAttributes = class(TPersistent)
  private
    fBackground: TColor;
    fBackgroundDefault: TColor;
    fForeground: TColor;
    fForegroundDefault: TColor;
    fName: string;
    fStyle: TFontStyles;
    fStyleDefault: TFontStyles;
    fOnChange: TNotifyEvent;
    procedure Changed; virtual;
    function GetBackgroundColorStored: boolean;
    function GetForegroundColorStored: boolean;
    function GetFontStyleStored: boolean;
    procedure SetBackground(Value: TColor);
    procedure SetForeground(Value: TColor);
    procedure SetStyle(Value: TFontStyles);
    function GetStyleFromInt: integer;
    procedure SetStyleFromInt(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignColorAndStyle(Source: TSynHighlighterAttributes);
    constructor Create(attribName: string);
    procedure InternalSaveDefaultValues;
{$IFNDEF SYN_CLX}
    function LoadFromBorlandRegistry(rootKey: HKEY; attrKey, attrName: string;
      oldStyle: boolean): boolean; virtual;
    function LoadFromRegistry(Reg: TBetterRegistry): boolean;
    function SaveToRegistry(Reg: TBetterRegistry): boolean;
    function LoadFromFile(Ini : TIniFile): boolean;
    function SaveToFile(Ini : TIniFile): boolean;
{$ENDIF}
  public
    property IntegerStyle: integer read GetStyleFromInt write SetStyleFromInt;
    property Name: string read fName;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  published
    property Background: TColor read fBackground write SetBackground
      stored GetBackgroundColorStored;
    property Foreground: TColor read fForeground write SetForeground
      stored GetForegroundColorStored;
    property Style: TFontStyles read fStyle write SetStyle
      stored GetFontStyleStored;
  end;

  TSynHighlighterCapability = (
    hcUserSettings, // supports Enum/UseUserSettings
    hcRegistry      // supports LoadFrom/SaveToRegistry
  );

  TSynHighlighterCapabilities = set of TSynHighlighterCapability;

const
  SYN_ATTR_COMMENT           =   0;
  SYN_ATTR_IDENTIFIER        =   1;
  SYN_ATTR_KEYWORD           =   2;
  SYN_ATTR_STRING            =   3;
  SYN_ATTR_WHITESPACE        =   4;
  SYN_ATTR_SYMBOL            =   5;

type
  TSynCustomHighlighter = class(TComponent)
  private
    fAttributes: TStringList;
    fAttrChangeHooks: TSynNotifyEventChain;
    fUpdateCount: integer;
    fEnabled: Boolean;
    fWordBreakChars: TSynIdentChars;
    procedure SetEnabled(const Value: boolean);
  protected
    fDefaultFilter: string;
    fUpdateChange: boolean;
    procedure Loaded; override;
    procedure AddAttribute(AAttrib: TSynHighlighterAttributes);
    procedure DefHighlightChange(Sender: TObject);
    procedure FreeHighlighterAttributes;
    function GetAttribCount: integer; virtual;
    function GetAttribute(idx: integer): TSynHighlighterAttributes; virtual;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      virtual; abstract;
    function GetDefaultFilter: string; virtual;
    function GetIdentChars: TSynIdentChars; virtual;
    function GetSampleSource: string; virtual;
    function IsFilterStored: boolean; virtual;
    procedure SetAttributesOnChange(AEvent: TNotifyEvent);
    procedure SetDefaultFilter(Value: string); virtual;
    procedure SetSampleSource(Value: string); virtual;
    procedure SetWordBreakChars(AChars: TSynIdentChars); virtual;
  protected
    function GetCapabilitiesProp: TSynHighlighterCapabilities;
    function GetLanguageNameProp: string;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; virtual;
    class function GetLanguageName: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetEol: Boolean; virtual; abstract;
    function GetRange: Pointer; virtual;
    function GetToken: String; virtual; abstract;
    function GetTokenAttribute: TSynHighlighterAttributes; virtual; abstract;
    function GetTokenKind: integer; virtual; abstract;
    function GetTokenPos: Integer; virtual; abstract;
    function IsKeyword(const AKeyword: string): boolean; virtual;
    procedure Next; virtual; abstract;
    procedure NextToEol;
    procedure SetLine(NewValue: String; LineNumber:Integer); virtual; abstract;
    procedure SetRange(Value: Pointer); virtual;
    procedure ResetRange; virtual;
    function UseUserSettings(settingIndex: integer): boolean; virtual;
    procedure EnumUserSettings(Settings: TStrings); virtual;
{$IFNDEF SYN_CLX}
    function LoadFromRegistry(RootKey: HKEY; Key: string): boolean; virtual;
    function SaveToRegistry(RootKey: HKEY; Key: string): boolean; virtual;
    function LoadFromFile(AFileName: String): boolean;
    function SaveToFile(AFileName: String): boolean;
{$ENDIF}
    procedure HookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
    procedure UnhookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
    property IdentChars: TSynIdentChars read GetIdentChars;
    property WordBreakChars: TSynIdentChars read fWordBreakChars write SetWordBreakChars;
    property LanguageName: string read GetLanguageNameProp;
  public
    property AttrCount: integer read GetAttribCount;
    property Attribute[idx: integer]: TSynHighlighterAttributes
      read GetAttribute;
    property Capabilities: TSynHighlighterCapabilities read GetCapabilitiesProp;
    property SampleSource: string read GetSampleSource write SetSampleSource;
    property CommentAttribute: TSynHighlighterAttributes
      index SYN_ATTR_COMMENT read GetDefaultAttribute;
    property IdentifierAttribute: TSynHighlighterAttributes
      index SYN_ATTR_IDENTIFIER read GetDefaultAttribute;
    property KeywordAttribute: TSynHighlighterAttributes
      index SYN_ATTR_KEYWORD read GetDefaultAttribute;
    property StringAttribute: TSynHighlighterAttributes
      index SYN_ATTR_STRING read GetDefaultAttribute;
    property SymbolAttribute: TSynHighlighterAttributes
      index SYN_ATTR_SYMBOL read GetDefaultAttribute;
    property WhitespaceAttribute: TSynHighlighterAttributes
      index SYN_ATTR_WHITESPACE read GetDefaultAttribute;
  published
    property DefaultFilter: string read GetDefaultFilter write SetDefaultFilter
      stored IsFilterStored;
    property Enabled: boolean read fEnabled write SetEnabled default TRUE;
  end;

  TSynCustomHighlighterClass = class of TSynCustomHighlighter;

{$IFNDEF SYN_CPPB_1}
  TSynHighlighterList = class(TList)
  private
    hlList: TList;
    function GetItem(idx: integer): TSynCustomHighlighterClass;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    function FindByName(name: string): integer;
    function FindByClass(comp: TComponent): integer;
    property Items[idx: integer]: TSynCustomHighlighterClass
      read GetItem; default;
  end;

  procedure RegisterPlaceableHighlighter(highlighter:
    TSynCustomHighlighterClass);
  function GetPlaceableHighlighters: TSynHighlighterList;
{$ENDIF}

implementation

{$IFNDEF SYN_CPPB_1}
{ THighlighterList }

function TSynHighlighterList.Count: integer;
begin
  Result := hlList.Count;
end;

constructor TSynHighlighterList.Create;
begin
  inherited Create;
  hlList := TList.Create;
end;

destructor TSynHighlighterList.Destroy;
begin
  hlList.Free;
  inherited;
end;

function TSynHighlighterList.FindByClass(comp: TComponent): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do begin
    if comp is Items[i] then begin
      Result := i;
      Exit;
    end;
  end; //for
end;

function TSynHighlighterList.FindByName(name: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do begin
    if Items[i].GetLanguageName = name then begin
      Result := i;
      Exit;
    end;
  end; //for
end;

function TSynHighlighterList.GetItem(idx: integer): TSynCustomHighlighterClass;
begin
  Result := TSynCustomHighlighterClass(hlList[idx]);
end;

var
  G_PlaceableHighlighters: TSynHighlighterList;

  function GetPlaceableHighlighters: TSynHighlighterList;
  begin
    Result := G_PlaceableHighlighters;
  end;

  procedure RegisterPlaceableHighlighter(highlighter: TSynCustomHighlighterClass);
  begin
    if G_PlaceableHighlighters.hlList.IndexOf(highlighter) < 0 then
      G_PlaceableHighlighters.hlList.Add(highlighter);
  end;
{$ENDIF}

{ TSynHighlighterAttributes }

procedure TSynHighlighterAttributes.Assign(Source: TPersistent);
begin
  if Source is TSynHighlighterAttributes then begin
    fName := TSynHighlighterAttributes(Source).fName;
    AssignColorAndStyle( TSynHighlighterAttributes(Source) );
  end else
    inherited Assign(Source);
end;

procedure TSynHighlighterAttributes.AssignColorAndStyle(Source: TSynHighlighterAttributes);
var
  bChanged: boolean;
begin
  bChanged := FALSE;
  if fBackground <> Source.fBackground then begin
    fBackground := Source.fBackground;
    bChanged := TRUE;
  end;
  if fForeground <> Source.fForeground then begin
    fForeground := Source.fForeground;
    bChanged := TRUE;
  end;
  if fStyle <> Source.fStyle then begin
    fStyle := Source.fStyle;
    bChanged := TRUE;
  end;
  if bChanged then
    Changed;
end;


procedure TSynHighlighterAttributes.Changed;
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

constructor TSynHighlighterAttributes.Create(attribName: string);
begin
  inherited Create;
  Background := clNone;
  Foreground := clNone;
  fName := attribName;
end;

function TSynHighlighterAttributes.GetBackgroundColorStored: boolean;
begin
  Result := fBackground <> fBackgroundDefault;
end;

function TSynHighlighterAttributes.GetForegroundColorStored: boolean;
begin
  Result := fForeground <> fForegroundDefault;
end;

function TSynHighlighterAttributes.GetFontStyleStored: boolean;
begin
  Result := fStyle <> fStyleDefault;
end;

procedure TSynHighlighterAttributes.InternalSaveDefaultValues;
begin
  fForegroundDefault := fForeground;
  fBackgroundDefault := fBackground;
  fStyleDefault := fStyle;
end;

{$IFNDEF SYN_CLX}
function TSynHighlighterAttributes.LoadFromBorlandRegistry(rootKey: HKEY;
  attrKey, attrName: string; oldStyle: boolean): boolean;
  // How the highlighting information is stored:
  // Delphi 1.0:
  //   I don't know and I don't care.
  // Delphi 2.0 & 3.0:
  //   In the registry branch HKCU\Software\Borland\Delphi\x.0\Highlight
  //   where x=2 or x=3.
  //   Each entry is one string value, encoded as
  //     <foreground RGB>,<background RGB>,<font style>,<default fg>,<default Background>,<fg index>,<Background index>
  //   Example:
  //     0,16777215,BI,0,1,0,15
  //     foreground color (RGB): 0
  //     background color (RGB): 16777215 ($FFFFFF)
  //     font style: BI (bold italic), possible flags: B(old), I(talic), U(nderline)
  //     default foreground: no, specified color will be used (black (0) is used when this flag is 1)
  //     default background: yes, white ($FFFFFF, 15) will be used for background
  //     foreground index: 0 (foreground index (Pal16), corresponds to foreground RGB color)
  //     background index: 15 (background index (Pal16), corresponds to background RGB color)
  // Delphi 4.0 & 5.0:
  //   In the registry branch HKCU\Software\Borland\Delphi\4.0\Editor\Highlight.
  //   Each entry is subkey containing several values:
  //     Foreground Color: foreground index (Pal16), 0..15 (dword)
  //     Background Color: background index (Pal16), 0..15 (dword)
  //     Bold: fsBold yes/no, 0/True (string)
  //     Italic: fsItalic yes/no, 0/True (string)
  //     Underline: fsUnderline yes/no, 0/True (string)
  //     Default Foreground: use default foreground (clBlack) yes/no, False/-1 (string)
  //     Default Background: use default backround (clWhite) yes/no, False/-1 (string)
const
  Pal16: array [0..15] of TColor = (clBlack, clMaroon, clGreen, clOlive,
          clNavy, clPurple, clTeal, clLtGray, clDkGray, clRed, clLime,
          clYellow, clBlue, clFuchsia, clAqua, clWhite);

  function LoadOldStyle(rootKey: HKEY; attrKey, attrName: string): boolean;
  var
    descript : string;
    fgColRGB : string;
    bgColRGB : string;
    fontStyle: string;
    fgDefault: string;
    bgDefault: string;
    fgIndex16: string;
    bgIndex16: string;
    reg      : TBetterRegistry;

    function Get(var name: string): string;
    var
      p: integer;
    begin
      p := Pos(',',name);
      if p = 0 then p := Length(name)+1;
      Result := Copy(name,1,p-1);
      name := Copy(name,p+1,Length(name)-p);
    end; { Get }

  begin { LoadOldStyle }
    Result := false;
    try
      reg := TBetterRegistry.Create;
      reg.RootKey := rootKey;
      try
        with reg do begin
          if OpenKeyReadOnly(attrKey) then begin
            try
              if ValueExists(attrName) then begin
                descript := ReadString(attrName);
                fgColRGB  := Get(descript);
                bgColRGB  := Get(descript);
                fontStyle := Get(descript);
                fgDefault := Get(descript);
                bgDefault := Get(descript);
                fgIndex16 := Get(descript);
                bgIndex16 := Get(descript);
                if bgDefault = '1'
                  then Background := clWindow
                  else Background := Pal16[StrToInt(bgIndex16)];
                if fgDefault = '1'
                  then Foreground := clWindowText
                  else Foreground := Pal16[StrToInt(fgIndex16)];
                Style := [];
                if Pos('B',fontStyle) > 0 then Style := Style + [fsBold];
                if Pos('I',fontStyle) > 0 then Style := Style + [fsItalic];
                if Pos('U',fontStyle) > 0 then Style := Style + [fsUnderline];
                Result := true;
              end;
            finally CloseKey; end;
          end; // if
        end; // with
      finally reg.Free; end;
    except end;
  end; { LoadOldStyle }

  function LoadNewStyle(rootKey: HKEY; attrKey, attrName: string): boolean;
  var
    fgColor      : integer;
    bgColor      : integer;
    fontBold     : string;
    fontItalic   : string;
    fontUnderline: string;
    fgDefault    : string;
    bgDefault    : string;
    reg          : TBetterRegistry;

    function IsTrue(value: string): boolean;
    begin
      Result := not ((UpperCase(value) = 'FALSE') or (value = '0'));
    end; { IsTrue }

  begin
    Result := false;
    try
      reg := TBetterRegistry.Create;
      reg.RootKey := rootKey;
      try
        with reg do begin
          if OpenKeyReadOnly(attrKey+'\'+attrName) then begin
            try
              if ValueExists('Foreground Color')
                then fgColor := Pal16[ReadInteger('Foreground Color')]
              else if ValueExists('Foreground Color New') then
                fgColor := StringToColor( ReadString('Foreground Color New') )
              else
                Exit;
              if ValueExists('Background Color')
                then bgColor := Pal16[ReadInteger('Background Color')]
              else if ValueExists('Background Color New') then
                bgColor := StringToColor( ReadString('Background Color New') )
              else
                Exit;
              if ValueExists('Bold')
                then fontBold := ReadString('Bold')
                else Exit;
              if ValueExists('Italic')
                then fontItalic := ReadString('Italic')
                else Exit;
              if ValueExists('Underline')
                then fontUnderline := ReadString('Underline')
                else Exit;
              if ValueExists('Default Foreground')
                then fgDefault := ReadString('Default Foreground')
                else Exit;
              if ValueExists('Default Background')
                then bgDefault := ReadString('Default Background')
                else Exit;
              if IsTrue(bgDefault)
                then Background := clWindow
                else Background := bgColor;
              if IsTrue(fgDefault)
                then Foreground := clWindowText
                else Foreground := fgColor;
              Style := [];
              if IsTrue(fontBold) then Style := Style + [fsBold];
              if IsTrue(fontItalic) then Style := Style + [fsItalic];
              if IsTrue(fontUnderline) then Style := Style + [fsUnderline];
              Result := true;
            finally CloseKey; end;
          end; // if
        end; // with
      finally reg.Free; end;
    except end;
  end; { LoadNewStyle }

begin
  if oldStyle then Result := LoadOldStyle(rootKey, attrKey, attrName)
              else Result := LoadNewStyle(rootKey, attrKey, attrName);
end; { TSynHighlighterAttributes.LoadFromBorlandRegistry }
{$ENDIF}

procedure TSynHighlighterAttributes.SetBackground(Value: TColor);
begin
  if fBackGround <> Value then begin
    fBackGround := Value;
    Changed;
  end;
end;

procedure TSynHighlighterAttributes.SetForeground(Value: TColor);
begin
  if fForeGround <> Value then begin
    fForeGround := Value;
    Changed;
  end;
end;

procedure TSynHighlighterAttributes.SetStyle(Value: TFontStyles);
begin
  if fStyle <> Value then begin
    fStyle := Value;
    Changed;
  end;
end;

{$IFNDEF SYN_CLX}
function TSynHighlighterAttributes.LoadFromRegistry(Reg: TBetterRegistry): boolean;
var
  key: string;
begin
  key := Reg.CurrentPath;
  if Reg.OpenKeyReadOnly(Name) then begin
    if Reg.ValueExists('Background') then
      Background := Reg.ReadInteger('Background');
    if Reg.ValueExists('Foreground') then
      Foreground := Reg.ReadInteger('Foreground');
    if Reg.ValueExists('Style') then
      IntegerStyle := Reg.ReadInteger('Style');
    reg.OpenKeyReadOnly('\' + key);
    Result := true;
  end else
    Result := false;
end;

function TSynHighlighterAttributes.SaveToRegistry(Reg: TBetterRegistry): boolean;
var
  key: string;
begin
  key := Reg.CurrentPath;
  if Reg.OpenKey(Name,true) then begin
    Reg.WriteInteger('Background', Background);
    Reg.WriteInteger('Foreground', Foreground);
    Reg.WriteInteger('Style', IntegerStyle);
    reg.OpenKey('\' + key, false);
    Result := true;
  end else
    Result := false;
end;

function TSynHighlighterAttributes.LoadFromFile(Ini : TIniFile): boolean;
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    Ini.ReadSection(Name, S);
    if S.Count > 0 then
    begin
      if S.IndexOf('Background') <> -1 then
        Background := Ini.ReadInteger(Name, 'Background', Background);
      if S.IndexOf('Foreground') <> -1 then
        Foreground := Ini.ReadInteger(Name, 'Foreground', Foreground);
      if S.IndexOf('Style') <> -1 then
        IntegerStyle := Ini.ReadInteger(Name, 'Style', IntegerStyle);
      Result := true;
    end else Result := false;
  finally
    S.Free;
  end;
end;

function TSynHighlighterAttributes.SaveToFile(Ini : TIniFile): boolean;
begin
  Ini.WriteInteger(Name, 'Background', Background);
  Ini.WriteInteger(Name, 'Foreground', Foreground);
  Ini.WriteInteger(Name, 'Style', IntegerStyle);
  Result := true;
end;

{$ENDIF}

function TSynHighlighterAttributes.GetStyleFromInt: integer;
begin
  if fsBold in Style then Result:= 1 else Result:= 0;
  if fsItalic in Style then Result:= Result + 2;
  if fsUnderline in Style then Result:= Result + 4;
  if fsStrikeout in Style then Result:= Result + 8;
end;

procedure TSynHighlighterAttributes.SetStyleFromInt(const Value: integer);
begin
  if Value and $1 = 0 then  Style:= [] else Style:= [fsBold];
  if Value and $2 <> 0 then Style:= Style + [fsItalic];
  if Value and $4 <> 0 then Style:= Style + [fsUnderline];
  if Value and $8 <> 0 then Style:= Style + [fsStrikeout];
end;

{ TSynCustomHighlighter }

constructor TSynCustomHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fWordBreakChars := TSynWordBreakChars;
  fAttributes := TStringList.Create;
  fAttributes.Duplicates := dupError;
  fAttributes.Sorted := TRUE;
  fAttrChangeHooks := TSynNotifyEventChain.CreateEx(Self);
  fDefaultFilter := '';
  fEnabled := True;
end;

destructor TSynCustomHighlighter.Destroy;
begin
  inherited Destroy;
  FreeHighlighterAttributes;
  fAttributes.Free;
  fAttrChangeHooks.Free;
end;

procedure TSynCustomHighlighter.BeginUpdate;
begin
  Inc(fUpdateCount);
end;

procedure TSynCustomHighlighter.EndUpdate;
begin
  if fUpdateCount > 0 then begin
    Dec(fUpdateCount);
    if (fUpdateCount = 0) and fUpdateChange then begin
      fUpdateChange := FALSE;
      DefHighlightChange( nil );
    end;
  end;
end;

procedure TSynCustomHighlighter.FreeHighlighterAttributes;
var
  i: integer;
begin
  if fAttributes <> nil then begin
    for i := fAttributes.Count - 1 downto 0 do
      TSynHighlighterAttributes(fAttributes.Objects[i]).Free;
    fAttributes.Clear;
  end;
end;

procedure TSynCustomHighlighter.Assign(Source: TPersistent);
var
  Src: TSynCustomHighlighter;
  i, j: integer;
  AttriName: string;
  SrcAttri: TSynHighlighterAttributes;
begin
  if (Source <> nil) and (Source is TSynCustomHighlighter) then begin
    Src := TSynCustomHighlighter(Source);
    for i := 0 to AttrCount - 1 do begin
      // assign first attribute with the same name
      AttriName := Attribute[i].Name;
      for j := 0 to Src.AttrCount - 1 do begin
        SrcAttri := Src.Attribute[j];
        if AttriName = SrcAttri.Name then begin
          Attribute[i].Assign(SrcAttri);
          break;
        end;
      end;
    end;
    // assign the sample source text only if same or descendant class
    if Src is ClassType then
      SampleSource := Src.SampleSource;
    fWordBreakChars := Src.WordBreakChars;
    DefaultFilter := Src.DefaultFilter;
    Enabled := Src.Enabled;
  end else
    inherited Assign(Source);
end;

procedure TSynCustomHighlighter.EnumUserSettings(Settings: TStrings);
begin
  Settings.Clear;
end;

function TSynCustomHighlighter.UseUserSettings(settingIndex: integer): boolean;
begin
  Result := false;
end;

{$IFNDEF SYN_CLX}
function TSynCustomHighlighter.LoadFromRegistry(RootKey: HKEY;
  Key: string): boolean;
var
  r: TBetterRegistry;
  i: integer;
begin
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKeyReadOnly(Key) then begin
      Result := true;
      for i := 0 to AttrCount-1 do
        Result := Attribute[i].LoadFromRegistry(r) and Result;
    end
    else Result := false;
  finally r.Free; end;
end;

function TSynCustomHighlighter.SaveToRegistry(RootKey: HKEY;
  Key: string): boolean;
var
  r: TBetterRegistry;
  i: integer;
begin
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKey(Key,true) then begin
      Result := true;
      for i := 0 to AttrCount-1 do
        Result := Attribute[i].SaveToRegistry(r) and Result;
    end
    else Result := false;
  finally r.Free; end;
end;

function TSynCustomHighlighter.LoadFromFile(AFileName : String): boolean;
var AIni : TIniFile;
    i : Integer;
begin
  AIni := TIniFile.Create(AFileName);
  try
    with AIni do
    begin
      Result := true;
      for i := 0 to AttrCount-1 do
        Result := Attribute[i].LoadFromFile(AIni) and Result;
    end;
  finally
    AIni.Free;
  end;
end;

function TSynCustomHighlighter.SaveToFile(AFileName : String): boolean;
var AIni : TIniFile;
    i: integer;
begin
  AIni := TIniFile.Create(AFileName);
  try
    with AIni do
    begin
      Result := true;
      for i := 0 to AttrCount-1 do
        Result := Attribute[i].SaveToFile(AIni) and Result;
    end;
  finally
    AIni.Free;
  end;
end;

{$ENDIF}

procedure TSynCustomHighlighter.AddAttribute(AAttrib: TSynHighlighterAttributes);
begin
  fAttributes.AddObject(AAttrib.Name, AAttrib);
end;

procedure TSynCustomHighlighter.DefHighlightChange(Sender: TObject);
begin
  if fUpdateCount > 0 then
    fUpdateChange := TRUE
  else if not( csLoading in ComponentState ) then
  begin
    fAttrChangeHooks.Sender := Sender;
    fAttrChangeHooks.Fire;
  end;
end;

function TSynCustomHighlighter.GetAttribCount: integer;
begin
  Result := fAttributes.Count;
end;

function TSynCustomHighlighter.GetAttribute(idx: integer):
  TSynHighlighterAttributes;
begin
  Result := nil;
  if (idx >= 0) and (idx < fAttributes.Count) then
    Result := TSynHighlighterAttributes(fAttributes.Objects[idx]);
end;

class function TSynCustomHighlighter.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := [hcRegistry]; //registry save/load supported by default
end;
                   
function TSynCustomHighlighter.GetCapabilitiesProp: TSynHighlighterCapabilities;
begin
  Result := GetCapabilities;
end;

function TSynCustomHighlighter.GetDefaultFilter: string;
begin
  Result := fDefaultFilter;
end;

function TSynCustomHighlighter.GetIdentChars: TSynIdentChars;
begin
  Result := [#33..#255];
end;

procedure TSynCustomHighlighter.SetWordBreakChars(AChars: TSynIdentChars);
begin
  fWordBreakChars := AChars;
end;


class function TSynCustomHighlighter.GetLanguageName: string;
begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
  raise Exception.CreateFmt('%s.GetLanguageName not implemented', [ClassName]);
{$ENDIF}
  Result := '<Unknown>';
end;

function TSynCustomHighlighter.GetLanguageNameProp: string;
begin
  Result := GetLanguageName;
end;

function TSynCustomHighlighter.GetRange: pointer;
begin
  Result := nil;
end;

function TSynCustomHighlighter.GetSampleSource: string;
begin
  Result := '';
end;

procedure TSynCustomHighlighter.HookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
begin
  fAttrChangeHooks.Add(ANotifyEvent);
end;

function TSynCustomHighlighter.IsFilterStored: boolean;
begin
  Result := TRUE;
end;

function TSynCustomHighlighter.IsKeyword(const AKeyword: string): boolean;
begin
  Result := FALSE;
end;

procedure TSynCustomHighlighter.NextToEol;
begin
  while not GetEol do Next;
end;

procedure TSynCustomHighlighter.ResetRange;
begin
end;

procedure TSynCustomHighlighter.SetAttributesOnChange(AEvent: TNotifyEvent);
var
  i: integer;
  Attri: TSynHighlighterAttributes;
begin
  for i := fAttributes.Count - 1 downto 0 do begin
    Attri := TSynHighlighterAttributes(fAttributes.Objects[i]);
    if Attri <> nil then begin
      Attri.OnChange := AEvent;
      Attri.InternalSaveDefaultValues;
    end;
  end;
end;

procedure TSynCustomHighlighter.SetRange(Value: Pointer);
begin
end;

procedure TSynCustomHighlighter.SetDefaultFilter(Value: string);
begin
  fDefaultFilter := Value;
end;

procedure TSynCustomHighlighter.SetSampleSource(Value: string);
begin
end;

procedure TSynCustomHighlighter.UnhookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
begin
  fAttrChangeHooks.Remove(ANotifyEvent);
end;

procedure TSynCustomHighlighter.SetEnabled(const Value: boolean);
begin
  if fEnabled <> Value then
  begin
    fEnabled := Value;
    DefHighlightChange( nil );
  end;
end;

procedure TSynCustomHighlighter.Loaded;
begin
  inherited;
  DefHighlightChange( nil );
end;

{$IFNDEF SYN_CPPB_1}
initialization
  G_PlaceableHighlighters := TSynHighlighterList.Create;
finalization
  G_PlaceableHighlighters.Free;
  G_PlaceableHighlighters := nil;
{$ENDIF}
end.
