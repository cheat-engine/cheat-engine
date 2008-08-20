{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: GenLex.pas, released 2000-04-19.
Description: Tokenlist used by the generator.

The Original Code is based on mGenLex.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: GenLex.pas,v 1.4 2001/12/27 10:55:55 plpolak Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit GenLex;

interface

uses
  SysUtils, Windows, Messages, Classes, Controls, LongIntList;

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

Type
  TIdTokenKind = (
    IdBeginFunc,
    IdBeginProc,
    IdBraceOpen,
    IdChars,
    IdCharset,
    IdCRLF,
    IdEndFunc,
    IdEndProc,
    IdIdent,
    IdIdentifier,
    IdIdentStart,
    IdKeys,
    IdTokenTypes,
    IdNull,
    IdSensitive,
    IdSpace,
    IdStop,
    IdEnclosedBy,
    IdSampleSource,
    IdUnknown);

type
  TGenLex = class(TObject)
  private
    fIgnoreComments: Boolean;
    fOrigin: PChar;
    fProcTable: array[#0..#255] of procedure of Object;
    fFuncTable: array[#0..#255] of function: TIdTokenKind of Object;
    Run: Integer;
    Walker: LongInt;
    Running: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenizing: Boolean;
    FLinePosList: TLongIntList;
    FTokenPositionsList: TLongIntList;
    fIdentFuncTable: array[0..150] of function: TIdTokenKind of Object;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(aKey: String): Boolean;
    function Func49:TIdTokenKind;
    function Func60:TIdTokenKind;
    function Func67:TIdTokenKind;
    function Func75:TIdTokenKind;
    function Func81:TIdTokenKind;
    function Func89:TIdTokenKind;
    function Func104: TIdTokenKind;
    function Func122:TIdTokenKind;
    function Func130:TIdTokenKind;
    function Func147: TIdTokenKind;
    function Func150: TIdTokenKind;
    procedure BraceOpenProc;
    function BraceOpenFunc:TIdTokenKind;
    procedure CRLFProc;
    function CRLFFunc:TIdTokenKind;
    procedure CharsetProc;
    function CharsetFunc:TIdTokenKind;
    procedure IdentProc;
    function IdentFunc:TIdTokenKind;
    procedure NullProc;
    function NullFunc:TIdTokenKind;
    procedure SpaceProc;
    function SpaceFunc:TIdTokenKind;
    procedure StopProc;
    function StopFunc:TIdTokenKind;
    procedure UnknownProc;
    function UnknownFunc: TIdTokenKind;
    function AltFunc: TIdTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TIdTokenKind;
    procedure SetOrigin(NewValue: PChar);
    procedure SetRunPos(Value: Integer);
    procedure MakeMethodTables;
    function GetRunId: TIdTokenKind;
    function GetRunToken: String;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Tokenize;
    procedure Next;
    property IgnoreComments: Boolean read fIgnoreComments write fIgnoreComments;
    property Origin: PChar read fOrigin write SetOrigin;
    property RunPos: Integer read Run write SetRunPos;
    function NextToken: String;
    function EOF: Boolean;
    property RunId: TIdTokenKind read GetRunId;
    property RunToken: String read GetRunToken;
  published 
  end;

implementation

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    Case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else Identifiers[I] := False;
    end;
    J := UpperCase(I)[1];
    Case I in['_', 'a'..'z', 'A'..'Z'] of
      True: mHashTable[I] := Ord(J) - 64
    else mHashTable[I] := 0;
    end;
  end;
end;

procedure TGenLex.InitIdent;
var
  I: Integer;
begin
  for I := 0 to 150 do
    Case I of
      49: fIdentFuncTable[I] := Func49;
      60: fIdentFuncTable[I] := Func60;
      67: fIdentFuncTable[I] := Func67;
      75: fIdentFuncTable[I] := Func75;
      81: fIdentFuncTable[I] := Func81;
      89: fIdentFuncTable[I] := Func89;
      104: fIdentFuncTable[I] := Func104;
      122: fIdentFuncTable[I] := Func122;
      130: fIdentFuncTable[I] := Func130;
      147: fIdentFuncTable[I] := Func147;
      150: fIdentFuncTable[I] := Func150;
    else
      fIdentFuncTable[I] := AltFunc;
    end;
end;

function TGenLex.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    Inc(Result, mHashTable[ToHash^]);
    Inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end; { KeyHash }

function TGenLex.KeyComp(aKey: String): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if mHashTable[Temp^] <> mHashTable[aKey[i]] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end else Result := False;
end; { KeyComp }

function TGenLex.Func49: TIdTokenKind;
begin
  if KeyComp('Chars') then Result := IdChars else Result := IDIdentifier;
end;

function TGenLex.Func60: TIdTokenKind;
begin
  if KeyComp('Keys') then Result := IdKeys else Result := IDIdentifier;
end;

function TGenLex.Func67: TIdTokenKind;
begin
  if KeyComp('EndFunc') then Result := IdEndFunc else Result := IDIdentifier;
end;

function TGenLex.Func75: TIdTokenKind;
begin
  if KeyComp('EndProc') then Result := IdEndProc else Result := IDIdentifier;
end;

function TGenLex.Func81: TIdTokenKind;
begin
  if KeyComp('BeginFunc') then Result := IdBeginFunc else Result := IDIdentifier;
end;

function TGenLex.Func89: TIdTokenKind;
begin
  if KeyComp('BeginProc') then Result := IdBeginProc else Result := IDIdentifier;
end;

function TGenLex.Func104: TIdTokenKind;
begin
  if KeyComp('EnclosedBy') then Result := IdEnclosedBy else Result := IDIdentifier;
end;

function TGenLex.Func122: TIdTokenKind;
begin
  if KeyComp('Sensitive') then Result := IdSensitive else Result := IDIdentifier;
end;

function TGenLex.Func130: TIdTokenKind;
begin
  if KeyComp('IdentStart') then Result := IdIdentStart else Result := IDIdentifier;
end;

function TGenLex.Func147: TIdTokenKind;
begin
  if KeyComp('SAMPLESOURCE') then Result := IdSampleSource else Result := IDIdentifier;
end;

function TGenLex.Func150: TIdTokenKind;
begin
  if KeyComp('TOKENTYPES') then Result := IdTokenTypes else Result := IDIdentifier;
end;

function TGenLex.AltFunc: TIdTokenKind;
begin
  Result := IdIdentifier;
end;

function TGenLex.IdentKind(MayBe: PChar): TIdTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 151 then
    Result := fIdentFuncTable[HashKey]
  else
    Result := IdIdentifier;
end;

procedure TGenLex.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '{':
        begin
          fProcTable[I] := BraceOpenProc;
          fFuncTable[I] := BraceOpenFunc;
        end;
      #10, #13:
        begin
          fProcTable[I] := CRLFProc;
          fFuncTable[I] := CRLFFunc;
        end;
      #39, '#':
        begin
          fProcTable[I] := CharsetProc;
          fFuncTable[I] := CharsetFunc;
        end;
      'A'..'Z', 'a'..'z', '_':
        begin
          fProcTable[I] := IdentProc;
          fFuncTable[I] := IdentFunc;
        end;
      #0:
        begin
          fProcTable[I] := NullProc;
          fFuncTable[I] := NullFunc;
        end;
      #1..#9, #11, #12, #14..#32:
        begin
          fProcTable[I] := SpaceProc;
          fFuncTable[I] := SpaceFunc;
        end;
      '|':
        begin
          fProcTable[I] := StopProc;
          fFuncTable[I] := StopFunc;
        end;
    else
      begin
        fProcTable[I] := UnknownProc;
        fFuncTable[I] := UnknownFunc;
      end;
    end;
end;

constructor TGenLex.Create;
begin
  inherited Create;
  InitIdent;
  MakeMethodTables;
  fIgnoreComments := False;
  FTokenPositionsList := TLongIntList.Create;
  FLinePosList := TLongIntList.Create;
end; { Create }

destructor TGenLex.Destroy;
begin
  inherited Destroy;
  FTokenPositionsList.Free;
  FLinePosList.Free;
end; { Destroy }

procedure TGenLex.SetOrigin(NewValue: PChar);
begin
  fOrigin := NewValue;
  Run := 0;
  Walker := 0;
  FTokenPositionsList.Clear;
  FTokenPositionsList.Add(0);
  FLinePosList.Clear;
  FLinePosList.Add(0);
end; { SetOrigin }

procedure TGenLex.SetRunPos(Value: Integer);
begin
  Run := Value;
end;

procedure TGenLex.BraceOpenProc;
begin
  Inc(Walker);
  if not fIgnoreComments then
  begin
    while FOrigin[Walker] <> #0 do
    begin
      case FOrigin[Walker] of
        '}':
          begin
            Inc(Walker);
            Break;
          end;
        #10:
          begin
            Inc(Walker);
            if fTokenizing then
              FLinePosList.Add(Walker);
          end;

        #13:
          begin
            if FOrigin[Walker + 1] = #10 then
              Inc(Walker, 2)
            else
              Inc(Walker);
            if fTokenizing then
              FLinePosList.Add(Walker);
          end;
      else
        Inc(Walker);
      end;
    end;
  end;
end;

function TGenLex.BraceOpenFunc:TIdTokenKind;
begin
  Result := IDBraceOpen;
end;

procedure TGenLex.CRLFProc;
begin
  Case FOrigin[Walker] of
    #10: inc(Walker);
    #13:
      Case FOrigin[Walker + 1] of
        #10: inc(Walker, 2);
        else inc(Walker);
      end;
  end;
  if fTokenizing then FLinePosList.Add(Walker);
end;

function TGenLex.CRLFFunc:TIdTokenKind;
begin
  Result := IdCRLF;
end;

procedure TGenLex.CharsetProc;
begin
  while FOrigin[Walker] <> #0 do
  begin
    case FOrigin[Walker] of
      #10, #13: break;
      ':': if FOrigin[Walker + 1] = ':' then break else inc(Walker);
    else inc(Walker);
    end;
  end;
end;

function TGenLex.CharsetFunc:TIdTokenKind;
begin
  Result := IDCharSet;
end;

procedure TGenLex.IdentProc;
begin
  inc(Walker);
  while Identifiers[fOrigin[Walker]] do inc(Walker);
end;

function TGenLex.IdentFunc:TIdTokenKind;
begin
  Result := IdentKind((fOrigin + Running));
end;

procedure TGenLex.NullProc;
begin
  if fTokenizing then
    if not (FOrigin[Walker - 1] in [#10, #13]) then FLinePosList.Add(Walker);
end;

function TGenLex.NullFunc: TIdTokenKind;
begin
  Result := IdNull;
end;

procedure TGenLex.SpaceProc;
begin
  while fOrigin[Walker] in [#1..#9, #11, #12, #14..#32] do inc(Walker);
end;

function TGenLex.SpaceFunc:TIdTokenKind;
begin
  Result := IdSpace;
end;

procedure TGenLex.StopProc;
begin
  inc(Walker);
  while FOrigin[Walker] <> #0 do
  begin
    case FOrigin[Walker] of
      #10: break;
      #13: break;
      '|': begin
             Inc(Walker);
             break;
           end;
    else
      Inc(Walker);
    end;
  end;
end;

function TGenLex.StopFunc:TIdTokenKind;
begin
  Result := IdUnknown;
  if FOrigin[Running + 1] = '>' then
    if FOrigin[Running + 2] = '<' then
      if FOrigin[Running + 3] = '|' then Result := IDStop;
end;

procedure TGenLex.UnknownProc;
begin
  inc(Walker);
end;

function TGenLex.UnknownFunc: TIdTokenKind;
begin
  Result := IdUnknown;
end;

function TGenLex.EOF: Boolean;
begin
  Result := False;
end; { EOF }

function TGenLex.GetRunId: TIdTokenKind;
begin
  Running := FTokenPositionsList[Run];
  Result := fFuncTable[fOrigin[Running]];
end;

function TGenLex.GetRunToken: String;
var
  StartPos, EndPos, StringLen: Integer;
begin
  StartPos := FTokenPositionsList[Run];
  EndPos := FTokenPositionsList[Run + 1];
  StringLen := EndPos - StartPos;
  SetString(Result, (FOrigin + StartPos), Stringlen);
end;

procedure TGenLex.Tokenize;
begin
  fTokenizing := True;
  repeat
    fProcTable[fOrigin[Walker]];
    FTokenPositionsList.Add(Walker);
  until fOrigin[Walker] = #0;
  fTokenizing := False;
end;

procedure TGenLex.Next;
begin
  Inc(Run);
end;

function TGenLex.NextToken: String;
var
  StartPos, EndPos, Len: LongInt;
begin
  StartPos := FTokenPositionsList[Run];
  EndPos := FTokenPositionsList[Run + 1];
  Len := EndPos - StartPos;
  SetString(Result, (FOrigin + StartPos), Len);
  inc(Run);
end;

Initialization
  MakeIdentTable;
end.
