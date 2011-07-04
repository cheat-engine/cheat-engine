{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterURI.pas, released 2003-04-10.
The initial author of this file is Maël Hörz.
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

$Id: SynHighlighterURI.pas,v 1.17 2004/09/03 10:52:41 maelh Exp $

You may retrieve the latest version of SynEdit from the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}
{
@abstract(Provides an URI syntax highlighter for SynEdit)
@author(Maël Hörz)
@created(2003)
@lastmod(2003-10-21)
http://www.mh-nexus.de

The SynHighlighterURI unit implements an URI syntax highlighter for SynEdit.

Recognition of URIs is based on the information provided in the document
"Uniform Resource Identifiers (URI): Generic Syntax" of "The Internet Society",
that can be found at http://www.ietf.org/rfc/rfc2396.txt.

Also interesting is http://www.freesoft.org/CIE/RFC/1738/33.htm which describes
general URL syntax and major protocols.

these protocols are recognized:
-------------------------------
http://
https://
ftp://
mailto:
news: or news://
nntp://
telnet://
gopher://
prospero://
wais://

as well as commonly used shorthands:
------------------------------------
someone@somewhere.org
www.host.org
}

{$IFNDEF QSYNHIGHLIGHTERURI}
unit SynHighlighterURI;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkNull, tkSpace, tkFtpLink, tkGopherLink,
    tkHttpLink, tkHttpsLink, tkMailtoLink, tkNewsLink, tkNntpLink,
    tkProsperoLink, tkTelnetLink, tkWaisLink, tkWebLink, tkUnknown, tkNullChar);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  TAlreadyVisitedURIFunc = function (URI: string): Boolean of object;

  TSynURISyn = class(TSynCustomHighlighter)
  private
    fLine: PChar;
    fLineNumber: Integer;
    fLineStr: string;
    fMayBeProtocol: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    FTokenID: TtkTokenKind;
    fTokenPos: Integer;
    fIdentFuncTable: array[0..97] of TIdentFuncTableFunc;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fURIAttri: TSynHighlighterAttributes;
    fVisitedURIAttri: TSynHighlighterAttributes;
    FAlreadyVisitedURI: TAlreadyVisitedURIFunc;

    function KeyComp(const Key: string): Boolean;
    function KeyHash(ToHash: PChar): Integer;
    procedure InitIdent;
    procedure MakeMethodTables;

    procedure CRProc;
    procedure LFProc;
    procedure NullProc;
    procedure ProtocolProc;
    procedure SpaceProc;
    procedure UnknownProc;

    function AltFunc: TtkTokenKind;
    function FtpFunc: TtkTokenKind;
    function GopherFunc: TtkTokenKind;
    function HttpFunc: TtkTokenKind;
    function HttpsFunc: TtkTokenKind;
    function MailtoFunc: TtkTokenKind;
    function NewsFunc: TtkTokenKind;
    function NntpFunc: TtkTokenKind;
    function ProsperoFunc: TtkTokenKind;
    function TelnetFunc: TtkTokenKind;
    function WaisFunc: TtkTokenKind;
    function WebFunc: TtkTokenKind;

    function IsValidEmailAddress: Boolean;
    function IsValidURI: Boolean;
    function IsValidWebLink: Boolean;

    procedure SetURIAttri(const Value: TSynHighlighterAttributes);
    procedure SetVisitedURIAttri(const Value: TSynHighlighterAttributes);
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
    procedure SetAlreadyVisitedURIFunc(Value: TAlreadyVisitedURIFunc);
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: string; LineNumber: Integer); override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property URIAttri: TSynHighlighterAttributes read fURIAttri write SetURIAttri;
    property VisitedURIAttri: TSynHighlighterAttributes read fVisitedURIAttri
      write SetVisitedURIAttri;
  end;

const
  SYN_ATTR_URI = 6;
  SYN_ATTR_VISITEDURI = 7;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  AlphaNum = ['0'..'9', 'A'..'Z', 'a'..'z'];
  Mark = ['-', '_', '.', '!', '~', '*', '''', '(' , ')'];
  Unreserved = Mark + AlphaNum;
  Reserved = [';', '/', '?', ':', '@', '&', '=', '+', '$', ',', '%', '#'];
  URIChars = Reserved + Unreserved;
  NeverAtEnd = Mark - [''''] + Reserved - ['/', '$'];
  URIBreakChars = [#0..#255] - URIChars - [#0..#32];
  EMailAddressChars = ['.', '_', '-', '@'] + AlphaNum;
  NeverAtEMailAddressEnd = ['.', '@'];

var
  HashTable: array[#0..#255] of Integer;

procedure MakeHashTable;
var
  c: Char;
  u: Byte;
begin
  FillChar(HashTable, sizeof(HashTable), 0);

  for c := 'A' to 'Z' do
  begin
    u := Ord(UpCase(c));
    HashTable[c] := (u * u - 64) div 771;
  end;

  for c := 'a' to 'z' do
  begin
    u := Ord(UpCase(c));
    HashTable[c] := (u * u - 64) div 771;
  end;

  HashTable[':'] := HashTable['Z'] + 1;
  HashTable['/'] := HashTable['Z'] + 2;
end;

procedure TSynURISyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;

  fIdentFuncTable[27] := WebFunc;
  fIdentFuncTable[41] := NewsFunc;
  fIdentFuncTable[53] := MailtoFunc;
  fIdentFuncTable[58] := FtpFunc;
  fIdentFuncTable[63] := WaisFunc;
  fIdentFuncTable[65] := NewsFunc;
  fIdentFuncTable[66] := NntpFunc;
  fIdentFuncTable[67] := HttpFunc;
  fIdentFuncTable[77] := GopherFunc;
  fIdentFuncTable[79] := TelnetFunc;
  fIdentFuncTable[75] := HttpsFunc;
  fIdentFuncTable[97] := ProsperoFunc;
end;

function TSynURISyn.KeyComp(const Key: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to fStringLen do
    if HashTable[fMayBeProtocol[I - 1]] <> HashTable[Key[I]] then
    begin
      Result := False;
      break;
    end;
end;

function TSynURISyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['A'..'Z', 'a'..'z'] do
  begin
    inc(Result, HashTable[ToHash^]);
    inc(ToHash);
  end;

  if ToHash^ = ':' then
  begin
    inc(Result, HashTable[ToHash^]);
    inc(ToHash);

    if ToHash^ = '/' then
    begin
      inc(Result, HashTable[ToHash^]);
      inc(ToHash);

      if ToHash^ = '/' then
      begin
        inc(Result, HashTable[ToHash^]);
        inc(ToHash);
      end;
    end;
  end;
  fStringLen := ToHash - fMayBeProtocol;
end;

function TSynURISyn.AltFunc: TtkTokenKind;
begin
  Result := tkUnknown;
end;

procedure TSynURISyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #13: fProcTable[I] := CRProc;
      #10: fProcTable[I] := LFProc;
      #0: fProcTable[I] := NullProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SpaceProc;
      'A'..'Z', 'a'..'z': fProcTable[I] := ProtocolProc;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynURISyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);

  fURIAttri := TSynHighlighterAttributes.Create(SYNS_AttrURI);
  fURIAttri.Foreground := clBlue;
  fURIAttri.Style := [fsUnderline];
  AddAttribute(fURIAttri);

  fVisitedURIAttri := TSynHighlighterAttributes.Create(SYNS_AttrVisitedURI);
  fVisitedURIAttri.Foreground := clPurple;
  fVisitedURIAttri.Style := [fsUnderline];
  AddAttribute(fVisitedURIAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterURI;
end;

destructor TSynURISyn.Destroy; 
begin
  inherited;
  // the other attributes are automatically freed because of AddAttribute()
  fSpaceAttri.Free;
  fIdentifierAttri.Free;
end;

procedure TSynURISyn.SetLine(NewValue: string; LineNumber: Integer);
begin
  fLineStr := NewValue;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynURISyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynURISyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynURISyn.NullProc;
begin
  if Run < Length(fLineStr) then
  begin
    inc(Run);
    fTokenID := tkNullChar;
  end
  else
    fTokenID := tkNull
end;

procedure TSynURISyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynURISyn.UnknownProc;
begin
  if IsValidEmailAddress then
    fTokenID := tkMailtoLink
  else
  begin
    {$IFDEF SYN_MBCSSUPPORT}
    if FLine[Run] in LeadBytes then
      Inc(Run, 2)
    else
    {$ENDIF}
      inc(Run);
    fTokenID := tkUnknown;
  end;
end;

procedure TSynURISyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynURISyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_URI: Result := fURIAttri;
    SYN_ATTR_VISITEDURI: Result := fVisitedURIAttri;
  else
    Result := nil;
  end;
end;

function TSynURISyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynURISyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynURISyn.GetTokenAttribute: TSynHighlighterAttributes;
var
  Visited: Boolean;
begin
  case GetTokenID of
    tkSpace: Result := fSpaceAttri;
    tkFtpLink, tkGopherLink, tkHttpLink, tkHttpsLink, tkMailtoLink, tkNewsLink,
    tkNntpLink, tkProsperoLink, tkTelnetLink, tkWaisLink, tkWebLink:
    begin
      Visited := False;
      if Assigned(FAlreadyVisitedURI) then
        Visited := FAlreadyVisitedURI(GetToken);
      if Visited then
        Result := fVisitedURIAttri
      else
        Result := fURIAttri;
    end;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynURISyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynURISyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

function TSynURISyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynURISyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars + [#0];
end;

class function TSynURISyn.GetLanguageName: string;
begin
  Result := SYNS_LangURI;
end;

function TSynURISyn.GetSampleSource: string;
begin
  Result := 'Universal Resource Identifier highlighting'#13#10#13#10 +
            'http://www.somewhere.org'#13#10 +
            'ftp://superhost.org/downloads/gems.zip'#13#10 +
            'www.w3c.org'#13#10 +
            'mailto:big@lebowski.edu'#13#10 +
            'douglas@adams.lod'#13#10 +
            'news:comp.lang.pascal.borland';
end;

function TSynURISyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterURI;
end;

procedure TSynURISyn.SetAlreadyVisitedURIFunc(Value: TAlreadyVisitedURIFunc);
begin
  FAlreadyVisitedURI := Value;
end;

procedure TSynURISyn.SetURIAttri(const Value: TSynHighlighterAttributes);
begin
  fURIAttri.Assign(Value);
end;

procedure TSynURISyn.SetVisitedURIAttri(const Value: TSynHighlighterAttributes);
begin
  fVisitedURIAttri.Assign(Value);
end;

function TSynURISyn.IsValidEmailAddress: Boolean;
var
  StartPos, AtPos, DotPos: Integer;
begin     
  StartPos := Run;

  AtPos := -1;
  DotPos := -1;
  while fLine[Run] in EMailAddressChars do
  begin
    if fLine[Run] = '@' then
      AtPos := Run
    else if fLine[Run] = '.' then
      // reject array of dots: "neighbour" dots are not allowed
      if (Run = StartPos) or (DotPos >= 0) and (DotPos = Run - 1) then
        break
      else
        DotPos := Run;
    Inc(Run);
  end;

  while (Run > StartPos) and (fLine[Run - 1] in NeverAtEMailAddressEnd) do
    dec(Run);

  while (DotPos >= Run) or (DotPos > -1) and (fLine[DotPos] <> '.') do
    Dec(DotPos);

  Result := (StartPos < AtPos) and (AtPos < Run - 1) and (DotPos > AtPos + 1);
  if not Result then Run := StartPos;
end;

function TSynURISyn.IsValidURI: Boolean;
var
  ProtocolEndPos, DotPos: Integer;

  function IsRelativePath: Boolean;
  begin
    Result := (DotPos - 1 >= 0) and
      ((fLine[DotPos - 1] = '/') and (fLine[DotPos + 2] = '/')) or
      ((fLine[DotPos - 1] = '\') and (fLine[DotPos + 2] = '\'));
  end;

begin
  ProtocolEndPos := Run;

  DotPos := -1;
  while fLine[Run] in URIChars do
  begin
    if fLine[Run] = '.' then
      // reject array of dots: "neighbour" dots are not allowed
      if (DotPos >= 0) and (DotPos = Run - 1) and not IsRelativePath then
        break
      else
        DotPos := Run;
    inc(Run);
  end;

  while (Run > ProtocolEndPos) and (fLine[Run - 1] in NeverAtEnd) do
    dec(Run);

  Result := Run > ProtocolEndPos;
end;

function TSynURISyn.IsValidWebLink: Boolean;
var
  WWWEndPos, DotPos, SecondDotPos: Integer;

  function IsRelativePath: Boolean;
  begin
    Result := (DotPos - 1 >= 0) and
      ((fLine[DotPos - 1] = '/') and (fLine[DotPos + 2] = '/')) or
      ((fLine[DotPos - 1] = '\') and (fLine[DotPos + 2] = '\'));
  end;

begin
  WWWEndPos := Run;

  DotPos := -1;
  SecondDotPos := -1;
  while fLine[Run] in URIChars do
  begin
    if fLine[Run] = '.' then
      // reject array of dots: "neighbour" dots are not allowed
      if (DotPos >= 0) and (DotPos = Run - 1) and not IsRelativePath then
        break
      else
      begin
        DotPos := Run;
        if SecondDotPos = -2 then SecondDotPos := DotPos;
        if SecondDotPos = -1 then SecondDotPos := -2;
      end;
    inc(Run);
  end;

  while (Run > WWWEndPos) and (fLine[Run - 1] in NeverAtEnd) do
    dec(Run);

  Result := (Run > WWWEndPos) and (fLine[WWWEndPos] = '.') and
            (SecondDotPos > WWWEndPos + 1) and (SecondDotPos < Run);
end;

procedure TSynURISyn.ProtocolProc;
var
  HashKey: Integer;
begin
  if IsValidEmailAddress then
    fTokenID := tkMailtoLink
  else
  begin
    fMayBeProtocol := fLine + Run;
    HashKey := KeyHash(fMayBeProtocol);
    inc(Run, fStringLen);

    if HashKey <= 97 then
      fTokenID := fIdentFuncTable[HashKey]
    else
      fTokenID := tkUnknown;
  end;
end;

function TSynURISyn.FtpFunc: TtkTokenKind;
begin
  if KeyComp('ftp://') and IsValidURI then
    Result := tkFtpLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.GopherFunc: TtkTokenKind;
begin
  if KeyComp('gopher://') and IsValidURI then
    Result := tkGopherLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.HttpFunc: TtkTokenKind;
begin
  if KeyComp('http://') and IsValidURI then
    Result := tkHttpLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.HttpsFunc: TtkTokenKind;
begin
  if KeyComp('https://') and IsValidURI then
    Result := tkHttpsLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.MailtoFunc: TtkTokenKind;
begin
  if KeyComp('mailto:') and IsValidURI then
    Result := tkMailtoLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.NewsFunc: TtkTokenKind;
begin
  if KeyComp('news:') and IsValidURI then
    Result := tkNewsLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.NntpFunc: TtkTokenKind;
begin
  if KeyComp('nntp://') and IsValidURI then
    Result := tkNntpLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.ProsperoFunc: TtkTokenKind;
begin
  if KeyComp('prospero://') and IsValidURI then
    Result := tkProsperoLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.TelnetFunc: TtkTokenKind;
begin
  if KeyComp('telnet://') and IsValidURI then
    Result := tkTelnetLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.WaisFunc: TtkTokenKind;
begin
  if KeyComp('wais://') and IsValidURI then
    Result := tkWaisLink
  else
    Result := tkUnknown;
end;

function TSynURISyn.WebFunc: TtkTokenKind;
begin
  if KeyComp('www') and IsValidWebLink then
    Result := tkWebLink
  else
    Result := tkUnknown;
end;


initialization
  MakeHashTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynURISyn);
{$ENDIF}
end.
