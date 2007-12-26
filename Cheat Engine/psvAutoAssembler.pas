{*******************************************************}
{               RichEdit Syntax HighLight               }
{                     version 3.0                       }
{ Author:                                               }
{ Serhiy Perevoznyk                                     }
{ serge_perevoznyk@hotmail.com                          }
{*******************************************************}

{The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPas.pas, released 2000-04-17.
The Original Code is based on the mwPasSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Martin Waldenburg.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.
The Original Code can be obtained from http://synedit.sourceforge.net/

This code has again be modified to work with cheat engine's auto assembler language
}

unit psvAutoAssembler;

interface

uses
  SysUtils, 
  Windows, 
  Classes, 
  Controls, 
  psvRichSyntax, 
  Graphics,
  assemblerunit;

type
  TtkTokenKind = (tkAsm, tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown, tkRegister);

  TRangeState = (rsANil, rsAnsi, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty,
    rsUnKnown);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  TpsvAARTF = class(TpsvRTFSyntax)
  private
    fAsmStart: Boolean;
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fIdentFuncTable: array[0..222] of TIdentFuncTableFunc;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func9: TtkTokenKind; //ah
    function Func10: TtkTokenKind; //bh
    function Func11: TtkTokenKind; //ch
    function Func12: TtkTokenKind; //dh
    function Func13: TtkTokenKind; //di / al
    function Func14: TtkTokenKind; //bl
    function Func15: TtkTokenKind; //cl
    function Func16: TtkTokenKind; //dl
    function Func18: TtkTokenKind; //edi  / bp
    function Func23: TtkTokenKind; //ebp
    function Func25: TtkTokenKind; //ax / 25
    function Func26: TtkTokenKind; //bx
    function Func27: TtkTokenKind; //cx
    function Func28: TtkTokenKind; //dx / si
    function Func30: TtkTokenKind; //eax / eip
    function Func31: TtkTokenKind; //ebx
    function Func32: TtkTokenKind; //ecx
    function Func33: TtkTokenKind; //edx / esi
    function Func35: TtkTokenKind; //sp
    function Func39: TtkTokenKind; //enable
    function Func40: TtkTokenKind; //esp
    function Func43: TtkTokenKind; //alloc /define
    function Func52: TtkTokenKind; //dealloc / disable
    function Func54: TtkTokenKind; //kalloc
    function Func59: TtkTokenKind; //readmem    
    function Func68: TtkTokenKind; //include
    function Func101: TtkTokenKind; //fullaccess/loadbinary
    function Func108: TtkTokenKind; //CreateThread
    function Func117: TtkTokenKind; //loadlibrary
    function Func187: TtkTokenKind; //registersymbol
    function Func222: TtkTokenKind; //unregistersymbol    

    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure AddressOpProc;
    procedure AsciiCharProc;
    procedure AnsiProc;
    procedure BorProc;
    procedure BraceOpenProc;
    procedure ColonOrGreaterProc;
    procedure CRProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PointProc;
    procedure RoundOpenProc;
    procedure SemicolonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
  protected
    function GetEol: Boolean; override;
    function GetRange: Pointer;
    function GetToken: string; override;
    function GetTokenAttribute: integer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; 
    function GetTokenPos: Integer; 
    procedure Next; override;
    procedure ResetRange; 
    procedure SetLine(NewValue: string; LineNumber:Integer); override;
    procedure SetRange(Value: Pointer);
    procedure  PrepareToken(var AToken : string); override;
    function PrepareOutput(Attr: integer; AToken : string): string; override;
  public
    constructor Create; override;
    procedure SetupDefaultColors; override;
  end;

implementation


var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

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
    J := UpCase(I);
    Case I of
      'a'..'z', 'A'..'Z', '_': mHashTable[I] := Ord(J) - 64;
    else mHashTable[Char(I)] := 0;
    end;
  end;
end;

procedure TpsvAARTF.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[9] := Func9;
  fIdentFuncTable[10] := Func10;
  fIdentFuncTable[11] := Func11;
  fIdentFuncTable[12] := Func12;
  fIdentFuncTable[13] := Func13;
  fIdentFuncTable[14] := Func14;
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[16] := Func16;
  fIdentFuncTable[18] := Func18;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[25] := Func25;
  fIdentFuncTable[26] := Func26;
  fIdentFuncTable[27] := Func27;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[30] := Func30;
  fIdentFuncTable[31] := Func31;
  fIdentFuncTable[32] := Func32;
  fIdentFuncTable[33] := Func33;
  fIdentFuncTable[35] := Func35;
  fIdentFuncTable[39] := Func39;
  fIdentFuncTable[40] := Func40;
  fIdentFuncTable[43] := Func43;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[54] := Func54;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[68] := Func68;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[108] := Func108;
  fIdentFuncTable[117] := Func117;
  fIdentFuncTable[187] := Func187;
  fIdentFuncTable[222] := Func222;
end;

function TpsvAARTF.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  if ToHash^ in ['_', '0'..'9'] then inc(ToHash);
  fStringLen := ToHash - fToIdent;
end; { KeyHash }

function TpsvAARTF.KeyComp(const aKey: string): Boolean;
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


function TpsvAARTF.Func9: TtkTokenKind;
begin
  if KeyComp('ah') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func10: TtkTokenKind;
begin
  if KeyComp('bh') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func11: TtkTokenKind;
begin
  if KeyComp('ch') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func12: TtkTokenKind;
begin
  if KeyComp('dh') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func13: TtkTokenKind;
begin
  if KeyComp('di') then Result := tkRegister else
    if KeyComp('al') then Result := tkRegister else
      Result := tkIdentifier;
end;

function TpsvAARTF.Func14: TtkTokenKind;
begin
  if KeyComp('bl') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func15: TtkTokenKind;
begin
  if KeyComp('cl') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func16: TtkTokenKind;
begin
  if KeyComp('dl') then Result := tkRegister else
    Result := tkIdentifier;
end;


function TpsvAARTF.Func18: TtkTokenKind;
begin
  if KeyComp('edi') then Result := tkRegister else
    if KeyComp('bp') then Result := tkRegister else
      Result := tkIdentifier;
end;

function TpsvAARTF.Func23: TtkTokenKind;
begin
  if KeyComp('ebp') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func25: TtkTokenKind;
begin
  if KeyComp('ax') then Result := tkRegister else
    if KeyComp('ip') then Result := tkRegister else
      Result := tkIdentifier;
end;

function TpsvAARTF.Func26: TtkTokenKind;
begin
  if KeyComp('bx') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func27: TtkTokenKind;
begin
  if KeyComp('cx') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func28: TtkTokenKind;
begin
  if KeyComp('dx') then Result := tkRegister else
    if KeyComp('si') then Result := tkRegister else
      Result := tkIdentifier;
end;

function TpsvAARTF.Func30: TtkTokenKind;
begin
  if KeyComp('eax') then Result := tkRegister else
    if KeyComp('eip') then Result := tkRegister else
      Result := tkIdentifier;
end;

function TpsvAARTF.Func31: TtkTokenKind;
begin
  if KeyComp('ebx') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func32: TtkTokenKind;
begin
  if KeyComp('Label') then Result := tkKey else
    if KeyComp('ecx') then Result := tkRegister else
      Result := tkIdentifier;
end;

function TpsvAARTF.Func33: TtkTokenKind;
begin
  if KeyComp('edx') then Result := tkRegister else
    if KeyComp('esi') then Result := tkRegister else
      Result := tkIdentifier;
end;

function TpsvAARTF.Func35: TtkTokenKind;
begin
  if KeyComp('bp') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func39: TtkTokenKind; //enable
begin
  if KeyComp('enable') then Result := tkspace else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func40: TtkTokenKind; //enable
begin
  if KeyComp('esp') then Result := tkRegister else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func43: TtkTokenKind; //alloc /define
begin
  if KeyComp('alloc') then Result := tkKey else
    if KeyComp('define') then Result := tkKey else
      Result := tkIdentifier;
end;

function TpsvAARTF.Func52: TtkTokenKind; //dealloc
begin
  if KeyComp('dealloc') then Result := tkKey else
    if KeyComp('disable') then Result := tkspace else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func54: TtkTokenKind; //kalloc
begin
  if KeyComp('kalloc') then Result := tkKey else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func59: TtkTokenKind; //readmem
begin
  if KeyComp('readmem') then Result := tkKey else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func68: TtkTokenKind; //include
begin
  if KeyComp('include') then Result := tkKey else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func101: TtkTokenKind;
begin
  if KeyComp('LoadBinary') then Result := tkKey else
    if KeyComp('fullaccess') then Result := tkKey else
      Result := tkIdentifier;
end;

function TpsvAARTF.Func108: TtkTokenKind; //CreateThread
begin
  if KeyComp('createthread') then Result := tkKey else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func117: TtkTokenKind; //loadlibrary
begin
  if KeyComp('loadlibrary') then Result := tkKey else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func187: TtkTokenKind; //registersymbol
begin
  if KeyComp('registersymbol') then Result := tkKey else
    Result := tkIdentifier;
end;

function TpsvAARTF.Func222: TtkTokenKind; //unregistersymbol
begin
  if KeyComp('unregistersymbol') then Result := tkKey else
    Result := tkIdentifier;
end;

function TpsvAARTF.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier
end;

function getfirsttoken(s: string): string;
var i: integer;
begin
  result:=s;
  for i:=1 to length(s) do
  begin
    if (s[i]=' ') or (s[i]=#9) or (s[i]=',') or (s[i]=#10) or (s[i]=#13) then
    begin
      result:=copy(s,1,i-1);
      exit;
    end;
  end;
end;

function TpsvAARTF.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 223 then Result := fIdentFuncTable[HashKey] else
    Result := tkIdentifier;

  if (result=tkIdentifier) and (GetOpcodesIndex(getfirsttoken(maybe))<>-1) then
    result:=tkKey;

end;

procedure TpsvAARTF.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: fProcTable[I] := NullProc;
      #10: fProcTable[I] := LFProc;
      #13: fProcTable[I] := CRProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := SpaceProc;
      '#': fProcTable[I] := AsciiCharProc;
      //'$': fProcTable[I] := IntegerProc;
      #39: fProcTable[I] := StringProc;
      '0'..'9','A'..'F', 'a'..'f': fProcTable[I] := NumberProc;
      'G'..'Z', 'g'..'z', '_':
        fProcTable[I] := IdentProc;
      '{': fProcTable[I] := BraceOpenProc;
      '}', '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
        begin
          case I of
            '(': fProcTable[I] := RoundOpenProc;
            '.': fProcTable[I] := PointProc;
            ';': fProcTable[I] := SemicolonProc;
            '/': fProcTable[I] := SlashProc;
            ':', '>': fProcTable[I] := ColonOrGreaterProc;
            '<': fProcTable[I] := LowerProc;
            '@': fProcTable[I] := AddressOpProc;
          else
            fProcTable[I] := SymbolProc;
          end;
        end;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

constructor TpsvAARTF.Create;
begin
  inherited Create;
  InitIdent;
  MakeMethodTables;
  fRange := rsUnknown;
  fAsmStart := False;
  CreateColorTable([clBlue,   //1  Comment
                    clBlack,  //2  Identifier
                    clBlack,  //3  Key
                    clGreen,  //4  Number
                    clNavy,   //5  Space
                    clRed,    //6  String
                    clBlack,  //7  Symbol
                    clGreen, //8 Unknown
                    clBlack, //9
                    $0080f0
                    ]);
end; { Create }

procedure TpsvAARTF.SetLine(NewValue: string; LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TpsvAARTF.AddressOpProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '@' then inc(Run);
end;

procedure TpsvAARTF.AsciiCharProc;
begin
  fTokenID := tkString;
  inc(Run);
  while FLine[Run] in ['0'..'9'] do inc(Run);
end;

procedure TpsvAARTF.BorProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    else begin
      fTokenID := tkComment;
      repeat
        if fLine[Run] = '}' then begin
          Inc(Run);
          if fRange = rsBorAsm then
            fRange := rsAsm
          else
            fRange := rsUnKnown;
          break;
        end;
        Inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end;
  end;
end;

procedure TpsvAARTF.BraceOpenProc;
begin
  if fRange = rsAsm then
    fRange := rsBorAsm
  else
    fRange := rsBor;
  BorProc;
end;

procedure TpsvAARTF.ColonOrGreaterProc;
begin
//rsANil, rsAnsi, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty,   rsUnKnown
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);
end;

procedure TpsvAARTF.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
end;

procedure TpsvAARTF.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TpsvAARTF.IntegerProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do inc(Run);
end;
            
procedure TpsvAARTF.NumberProc;
begin
  fTokenID := IdentKind((fLine + Run));

  if fTokenID=tkIdentifier then
  begin
    inc(Run);
    fTokenID := tkNumber;
    while FLine[Run] in ['0'..'9', '.', 'a'..'f' , 'A'..'F'] do
    begin
      {case FLine[Run] of
        '.':
          if FLine[Run + 1] = '.' then break;
      end;   }
      inc(Run);
    end;

    if ((FLine[Run]>'G') and (FLine[Run]<='Z')) or ((FLine[Run]>='g') and (FLine[Run]<='z')) then
      fTokenID:=tkIdentifier;
  end
  else
  begin
    inc(Run, fStringLen);
    while Identifiers[fLine[Run]] do inc(Run);
  end;
end;



procedure TpsvAARTF.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TpsvAARTF.LowerProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['=', '>'] then inc(Run);
end;

procedure TpsvAARTF.NullProc;
begin
  fTokenID := tkNull;
end;



procedure TpsvAARTF.PointProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['.', ')'] then inc(Run);
end;

procedure TpsvAARTF.AnsiProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
        Inc(Run, 2);
        if fRange = rsAnsiAsm then
          fRange := rsAsm
        else
          fRange := rsUnKnown;
        break;
      end;
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end;
end;

procedure TpsvAARTF.RoundOpenProc;
begin
  Inc(Run);
  case fLine[Run] of
  {  '*':
      begin
        Inc(Run);
        if fRange = rsAsm then
          fRange := rsAnsiAsm
        else
          fRange := rsAnsi;
        fTokenID := tkComment;
        if not (fLine[Run] in [#0, #10, #13]) then
          AnsiProc;
      end; }
    '.':
      begin
        inc(Run);
        fTokenID := tkSymbol;
      end;
  else
    fTokenID := tkSymbol;
  end;
end;

{begin}
procedure TpsvAARTF.SemicolonProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fRange = rsProperty then
    fRange := rsUnknown;
end;
{end}


procedure TpsvAARTF.SlashProc;
begin
  Inc(Run);
  if fLine[Run] = '/' then
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end
  else
  if fline[run] = '*' then
  begin
      begin
        Inc(Run);
        if fRange = rsAsm then
          fRange := rsAnsiAsm
        else
          fRange := rsAnsi;
        fTokenID := tkComment;
        if not (fLine[Run] in [#0, #10, #13]) then
          AnsiProc;
      end;
  end
  else fTokenID := tkSymbol;
end;

procedure TpsvAARTF.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TpsvAARTF.StringProc;
begin
  fTokenID := tkString;
  Inc(Run);
  while not (fLine[Run] in [#0, #10, #13]) do begin
    if fLine[Run] = #39 then begin
      Inc(Run);
      if fLine[Run] <> #39 then
        break;
    end;
    Inc(Run);
  end;
end;

procedure TpsvAARTF.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TpsvAARTF.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TpsvAARTF.Next;
begin
  fAsmStart := False;
  fTokenPos := Run;
  case fRange of
    rsAnsi, rsAnsiAsm:
      AnsiProc;
    rsBor, rsBorAsm:
      BorProc;
  else
    fProcTable[fLine[Run]];
  end;
{
  if fline[Run]=':' then
  begin
    fTokenID := tkLabel;

    while fline[run]=':' do
      inc(run);
  end;}
end;


function TpsvAARTF.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TpsvAARTF.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TpsvAARTF.GetTokenID: TtkTokenKind;
begin
  if not fAsmStart and (fRange = rsAsm)
    and not (fTokenId in [tkNull, tkComment, tkSpace])
  then
    Result := tkAsm
  else
    Result := fTokenId;
end;

function TpsvAARTF.GetTokenAttribute: integer;
begin
  case GetTokenID of
    tkAsm: Result := 9;
    tkComment: Result := 1;
    tkIdentifier: Result := 2;
    tkKey: Result := 3;
    tkNumber: Result := 4;
    tkSpace: Result := 5;
    tkString: Result := 6;
    tkSymbol: Result := 7;
    tkUnknown: Result := 8;
    tkRegister: result := 10;
  else
    Result := 9;
  end;
end;

function TpsvAARTF.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

function TpsvAARTF.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TpsvAARTF.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure TpsvAARTF.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TpsvAARTF.ResetRange;
begin
  fRange:= rsUnknown;
end;



procedure TpsvAARTF.PrepareToken(var AToken : string);
var St : string;
begin
  St := AToken;
  St := StringReplace(St,'\','\\',[rfReplaceAll]);
  St := StringReplace(St,'{','\{',[rfReplaceAll]);
  St := StringReplace(St,'}','\}',[rfReplaceAll]);
  AToken := St;
end;

function TpsvAARTF.PrepareOutput(Attr: integer; AToken : string): string;
begin
  case Attr of 
    1 : Result  := '\cf1 \i '+ AToken +'\i0 ';
    3 : Result  := '\cf3 \b '+ AToken +'\b0 ';
    10 : Result  := Format('\cf%d \b %s\b0 ',[Attr,AToken]);
  else
   Result := Format('\cf%d %s',[Attr,AToken]);
  end;
end;

procedure TpsvAARTF.SetupDefaultColors;
begin
  CreateColorTable([clRed,   //1  Comment
                    clRed,  //2  Identifier
                    clRed,  //3  Key
                    clRed,  //4  Number
                    clRed,   //5  Space
                    clRed,  //6  String
                    clRed,  //7  Symbol
                    clRed,  //8 Unknown
                    clRed, //9
                    clRed //10
                    ]);
end;

initialization
  MakeIdentTable;


end.




