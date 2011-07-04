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
{ The Original Code is JclStrHashMap.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Barry Kelly.                                       }
{ Portions created by Barry Kelly are Copyright (C) Barry Kelly. All rights reserved.              }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Barry Kelly, Robert Rossmair, Matthias Thoma, Petr Vones                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains a string-pointer associative map. It works by hashing the added strings using }
{ a passed-in traits object.                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStrHashMap;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils,
  JclBase, JclResources;

type
  EJclStringHashMapError = class(EJclError);
  THashValue = Cardinal;

type
  TStringHashMapTraits = class(TObject)
  public
    function Hash(const S: string): Cardinal; virtual; abstract;
    function Compare(const L, R: string): Integer; virtual; abstract;
  end;

function CaseSensitiveTraits: TStringHashMapTraits;
function CaseInsensitiveTraits: TStringHashMapTraits;

type
  {$IFDEF CLR}
  PUserData = TObject;
  PData = TObject;

  TIterateFunc = function(AUserData: PUserData; const AStr: string; var APtr): Boolean;
  TIterateMethod = function(AUserData: PUserData; const AStr: string; var APtr): Boolean of object;
  {$ELSE}
  PUserData = Pointer;
  PData = Pointer;

  TIterateFunc = function(AUserData: PUserData; const AStr: string; var APtr: PData): Boolean;
  TIterateMethod = function(AUserData: PUserData; const AStr: string; var APtr: PData): Boolean of object;
  {$ENDIF CLR}

  {$IFDEF CLR}
  THashNode = class;
  PHashNode = THashNode;
  PPHashNode = PHashNode;
  THashNode = class
    Str: string;
    Ptr: TObject;
    Left: PHashNode;
    Right: PHashNode;
  end;

  { Internal iterate function pointer type used by the protected
    TStringHashMap.NodeIterate method. }
  TNodeIterateFunc = procedure(AUserData: TObject; ANode: PPHashNode);

  THashArray = array of PHashNode;
  PHashArray = THashArray;
  {$ELSE}
  PPHashNode = ^PHashNode;
  PHashNode = ^THashNode;
  THashNode = record
    Str: string;
    Ptr: Pointer;
    Left: PHashNode;
    Right: PHashNode;
  end;

  { Internal iterate function pointer type used by the protected
    TStringHashMap.NodeIterate method. }
  TNodeIterateFunc = procedure(AUserData: Pointer; ANode: PPHashNode);

  PHashArray = ^THashArray;
  THashArray = array [0..MaxInt div SizeOf(PHashNode) - 1] of PHashNode;
  {$ENDIF CLR}


  TStringHashMap = class(TObject)
  private
    FHashSize: Cardinal;
    FCount: Cardinal;
    FList: PHashArray;
    FLeftDelete: Boolean;
    FTraits: TStringHashMapTraits;
    function IterateNode(ANode: PHashNode; AUserData: PUserData; AIterateFunc: TIterateFunc): Boolean;
    function IterateMethodNode(ANode: PHashNode; AUserData: PUserData; AIterateMethod: TIterateMethod): Boolean;
    procedure NodeIterate(ANode: PPHashNode; AUserData: PUserData; AIterateFunc: TNodeIterateFunc);
    procedure SetHashSize(AHashSize: Cardinal);
    procedure DeleteNodes(var Q: PHashNode);
    procedure DeleteNode(var Q: PHashNode);
  protected
    function FindNode(const S: string): PPHashNode;
    function AllocNode: PHashNode; virtual;
    procedure FreeNode(ANode: PHashNode); virtual;
    function GetData(const S: string): PData;
    procedure SetData(const S: string; P: PData);
  public
    constructor Create(ATraits: TStringHashMapTraits; AHashSize: Cardinal);
    destructor Destroy; override;
    procedure Add(const S: string; const P);
    function Remove(const S: string): PData;
    procedure RemoveData(const P);
    procedure Iterate(AUserData: PUserData; AIterateFunc: TIterateFunc);
    procedure IterateMethod(AUserData: PUserData; AIterateMethod: TIterateMethod);
    function Has(const S: string): Boolean;
    function Find(const S: string; var P): Boolean;
    function FindData(const P; var S: string): Boolean;
    procedure Clear;
    property Count: Cardinal read FCount;
    property Data[const S: string]: PData read GetData write SetData; default;
    property Traits: TStringHashMapTraits read FTraits;
    property HashSize: Cardinal read FHashSize write SetHashSize;
  end;

{ Str=case sensitive, text=case insensitive }

function StrHash(const S: string): THashValue;
function TextHash(const S: string): THashValue;
function DataHash(var AValue; ASize: Cardinal): THashValue;
function Iterate_FreeObjects(AUserData: PUserData; const AStr: string; var AData {$IFNDEF CLR}: PData{$ENDIF}): Boolean;
function Iterate_Dispose(AUserData: PUserData; const AStr: string; var AData {$IFNDEF CLR}: PData{$ENDIF}): Boolean;
function Iterate_FreeMem(AUserData: PUserData; const AStr: string; var AData {$IFNDEF CLR}: PData{$ENDIF}): Boolean;

type
  TCaseSensitiveTraits = class(TStringHashMapTraits)
  public
    function Hash(const S: string): Cardinal; override;
    function Compare(const L, R: string): Integer; override;
  end;

  TCaseInsensitiveTraits = class(TStringHashMapTraits)
  public
    function Hash(const S: string): Cardinal; override;
    function Compare(const L, R: string): Integer; override;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclStrHashMap.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

// Case Sensitive & Insensitive Traits
function TCaseSensitiveTraits.Compare(const L, R: string): Integer;
begin
  Result := CompareStr(L, R);
end;

function TCaseSensitiveTraits.Hash(const S: string): Cardinal;
begin
  Result := StrHash(S);
end;

function TCaseInsensitiveTraits.Compare(const L, R: string): Integer;
begin
  Result := CompareText(L, R);
end;

function TCaseInsensitiveTraits.Hash(const S: string): Cardinal;
begin
  Result := TextHash(S);
end;

var
  GlobalCaseSensitiveTraits: TCaseSensitiveTraits;

function CaseSensitiveTraits: TStringHashMapTraits;
begin
  if GlobalCaseSensitiveTraits = nil then
    GlobalCaseSensitiveTraits := TCaseSensitiveTraits.Create;
  Result := GlobalCaseSensitiveTraits;
end;

var
  GlobalCaseInsensitiveTraits: TCaseInsensitiveTraits;

function CaseInsensitiveTraits: TStringHashMapTraits;
begin
  if GlobalCaseInsensitiveTraits = nil then
    GlobalCaseInsensitiveTraits := TCaseInsensitiveTraits.Create;
  Result := GlobalCaseInsensitiveTraits;
end;

function Iterate_FreeObjects(AUserData: PUserData; const AStr: string; var AData {$IFNDEF CLR}: PData{$ENDIF}): Boolean;
begin
  TObject(AData).Free;
  AData := nil;
  Result := True;
end;

function Iterate_Dispose(AUserData: PUserData; const AStr: string; var AData {$IFNDEF CLR}: PData{$ENDIF}): Boolean;
begin
  {$IFDEF CLR}
  TObject(AData).Free;
  {$ELSE}
  Dispose(AData);
  {$ENDIF CLR}
  AData := nil;
  Result := True;
end;

function Iterate_FreeMem(AUserData: PUserData; const AStr: string; var AData {$IFNDEF CLR}: PData{$ENDIF}): Boolean;
begin
  {$IFDEF CLR}
  TObject(AData).Free;
  {$ELSE}
  FreeMem(AData);
  {$ENDIF CLR}
  AData := nil;
  Result := True;
end;

{$IFOPT Q+}
{$DEFINE OVERFLOWCHECKS_ON}
{$Q-}
{$ENDIF}

function StrHash(const S: string): Cardinal;
{$IFDEF CLR}
begin
  Result := 0;
  if S <> nil then
    Result := S.GetHashCode
end;
{$ELSE}
const
  cLongBits = 32;
  cOneEight = 4;
  cThreeFourths = 24;
  cHighBits = $F0000000;
var
  I: Integer;
  P: PChar;
  Temp: Cardinal;
begin
  { TODO : I should really be processing 4 bytes at once... }
  Result := 0;
  P := PChar(S);

  I := Length(S);
  while I > 0 do
  begin
    Result := (Result shl cOneEight) + Ord(P^);
    Temp := Result and cHighBits;
    if Temp <> 0 then
      Result := (Result xor (Temp shr cThreeFourths)) and (not cHighBits);
    Dec(I);
    Inc(P);
  end;
end;
{$ENDIF CLR}

function TextHash(const S: string): Cardinal;
{$IFDEF CLR}
begin
  Result := 0;
  if S <> nil then
    Result := S.GetHashCode
end;
{$ELSE}
const
  cLongBits = 32;
  cOneEight = 4;
  cThreeFourths = 24;
  cHighBits = $F0000000;
var
  I: Integer;
  P: PChar;
  Temp: Cardinal;
begin
  { TODO : I should really be processing 4 bytes at once... }
  Result := 0;
  P := PChar(S);

  I := Length(S);
  while I > 0 do
  begin
    Result := (Result shl cOneEight) + Ord(UpCase(P^));
    Temp := Result and cHighBits;
    if Temp <> 0 then
      Result := (Result xor (Temp shr cThreeFourths)) and (not cHighBits);
    Dec(I);
    Inc(P);
  end;
end;
{$ENDIF CLR}

function DataHash(var AValue; ASize: Cardinal): THashValue;
{$IFDEF CLR}
begin
  Result := 0;
  if TObject(AValue) <> nil then
    Result := TObject(AValue).GetHashCode
end;
{$ELSE}
const
  cLongBits = 32;
  cOneEight = 4;
  cThreeFourths = 24;
  cHighBits = $F0000000;
var
  P: PChar;
  Temp: Cardinal;
begin
  { TODO : I should really be processing 4 bytes at once... }
  Result := 0;
  P := @AValue;

  while ASize > 0 do
  begin
    Result := (Result shl cOneEight) + Ord(P^);
    Temp := Result and cHighBits;
    if Temp <> 0 then
      Result := (Result xor (Temp shr cThreeFourths)) and (not cHighBits);
    Dec(ASize);
    Inc(P);
  end;
end;
{$ENDIF CLR}

{$IFDEF OVERFLOWCHECKS_ON}
{$Q+}
{$ENDIF}

//=== { TStringHashMap } =====================================================

constructor TStringHashMap.Create(ATraits: TStringHashMapTraits; AHashSize: Cardinal);
begin
  inherited Create;
  {$IFDEF CLR}
  Assert(ATraits <> nil, RsStringHashMapNoTraits);
  {$ELSE}
  Assert(ATraits <> nil, LoadResString(@RsStringHashMapNoTraits));
  {$ENDIF CLR}
  SetHashSize(AHashSize);
  FTraits := ATraits;
end;

destructor TStringHashMap.Destroy;
begin
  Clear;
  SetHashSize(0);
  inherited Destroy;
end;

type
  {$IFDEF CLR}
  TCollectNodeNode = class;
  PCollectNodeNode = TCollectNodeNode;
  TCollectNodeNode = class
    Next: PCollectNodeNode;
    Str: string;
    Ptr: TObject;
  end;
  {$ELSE}
  PPCollectNodeNode = ^PCollectNodeNode;
  PCollectNodeNode = ^TCollectNodeNode;
  TCollectNodeNode = record
    Next: PCollectNodeNode;
    Str: string;
    Ptr: Pointer;
  end;
  {$ENDIF CLR}


{$IFNDEF CLR}
procedure NodeIterate_CollectNodes(AUserData: PUserData; ANode: PPHashNode);
var
  PPCnn: PPCollectNodeNode;
  PCnn: PCollectNodeNode;
begin
  PPCnn := PPCollectNodeNode(AUserData);
  New(PCnn);
  PCnn^.Next := PPCnn^;
  PPCnn^ := PCnn;

  PCnn^.Str := ANode^^.Str;
  PCnn^.Ptr := ANode^^.Ptr;
end;
{$ENDIF ~CLR}

procedure TStringHashMap.SetHashSize(AHashSize: Cardinal);
var
  CollectList: PCollectNodeNode;

  procedure CollectNodes;
  var
    I: Integer;
  begin
    CollectList := nil;
    for I := 0 to FHashSize - 1 do
      NodeIterate(@FList^[I], @CollectList, NodeIterate_CollectNodes);
  end;

  procedure InsertNodes;
  var
    PCnn, Tmp: PCollectNodeNode;
  begin
    PCnn := CollectList;
    while PCnn <> nil do
    begin
      Tmp := PCnn^.Next;
      Add(PCnn^.Str, PCnn^.Ptr);
      Dispose(PCnn);
      PCnn := Tmp;
    end;
  end;

begin
  { 4 cases:
    we are empty, and AHashSize = 0 --> nothing to do
    we are full, and AHashSize = 0 --> straight empty
    we are empty, and AHashSize > 0 --> straight allocation
    we are full, and AHashSize > 0 --> rehash }

  if FHashSize = 0 then
  begin
    if AHashSize > 0 then
    begin
      GetMem(FList, AHashSize * SizeOf(FList^[0]));
      FillChar(FList^, AHashSize * SizeOf(FList^[0]), 0);
      FHashSize := AHashSize;
    end;
  end
  else
  begin
    if AHashSize > 0 then
    begin
      { must rehash table }
      CollectNodes;
      Clear;
      ReallocMem(FList, AHashSize * SizeOf(FList^[0]));
      FillChar(FList^, AHashSize * SizeOf(FList^[0]), 0);
      FHashSize := AHashSize;
      InsertNodes;
    end
    else
    begin
      { we are clearing the table - need hash to be empty }
      if FCount > 0 then
        raise EJclStringHashMapError.CreateRes(@RsStringHashMapMustBeEmpty);
      FreeMem(FList);
      FList := nil;
      FHashSize := 0;
    end;
  end;
end;

function TStringHashMap.FindNode(const S: string): PPHashNode;
var
  I: Cardinal;
  R: Integer;
  PPN: PPHashNode;
begin
  { we start at the node offset by S in the hash list }
  I := FTraits.Hash(S) mod FHashSize;

  PPN := @FList^[I];

  if PPN^ <> nil then
    while True do
    begin
      R := FTraits.Compare(S, PPN^^.Str);

      { left, then right, then match }
      if R < 0 then
        PPN := @PPN^^.Left
      else
      if R > 0 then
        PPN := @PPN^^.Right
      else
        Break;

      { check for empty position after drilling left or right }
      if PPN^ = nil then
        Break;
    end;

  Result := PPN;
end;

function TStringHashMap.IterateNode(ANode: PHashNode; AUserData: Pointer;
  AIterateFunc: TIterateFunc): Boolean;
begin
  if ANode <> nil then
  begin
    Result := AIterateFunc(AUserData, ANode^.Str, ANode^.Ptr);
    if not Result then
      Exit;

    Result := IterateNode(ANode^.Left, AUserData, AIterateFunc);
    if not Result then
      Exit;

    Result := IterateNode(ANode^.Right, AUserData, AIterateFunc);
    if not Result then
      Exit;
  end
  else
    Result := True;
end;

function TStringHashMap.IterateMethodNode(ANode: PHashNode; AUserData: Pointer;
  AIterateMethod: TIterateMethod): Boolean;
begin
  if ANode <> nil then
  begin
    Result := AIterateMethod(AUserData, ANode^.Str, ANode^.Ptr);
    if not Result then
      Exit;

    Result := IterateMethodNode(ANode^.Left, AUserData, AIterateMethod);
    if not Result then
      Exit;

    Result := IterateMethodNode(ANode^.Right, AUserData, AIterateMethod);
    if not Result then
      Exit;
  end
  else
    Result := True;
end;

procedure TStringHashMap.NodeIterate(ANode: PPHashNode; AUserData: Pointer;
  AIterateFunc: TNodeIterateFunc);
begin
  if ANode^ <> nil then
  begin
    AIterateFunc(AUserData, ANode);
    NodeIterate(@ANode^.Left, AUserData, AIterateFunc);
    NodeIterate(@ANode^.Right, AUserData, AIterateFunc);
  end;
end;

procedure TStringHashMap.DeleteNode(var Q: PHashNode);
var
  T, R, S: PHashNode;
begin
  { we must delete node Q without destroying binary tree }
  { Knuth 6.2.2 D (pg 432 Vol 3 2nd ed) }

  { alternating between left / right delete to preserve decent
    performance over multiple insertion / deletion }
  FLeftDelete := not FLeftDelete;

  { T will be the node we delete }
  T := Q;

  if FLeftDelete then
  begin
    if T^.Right = nil then
      Q := T^.Left
    else
    begin
      R := T^.Right;
      if R^.Left = nil then
      begin
        R^.Left := T^.Left;
        Q := R;
      end
      else
      begin
        S := R^.Left;
        if S^.Left <> nil then
          repeat
            R := S;
            S := R^.Left;
          until S^.Left = nil;
        { now, S = symmetric successor of Q }
        S^.Left := T^.Left;
        R^.Left :=  S^.Right;
        S^.Right := T^.Right;
        Q := S;
      end;
    end;
  end
  else
  begin
    if T^.Left = nil then
      Q := T^.Right
    else
    begin
      R := T^.Left;
      if R^.Right = nil then
      begin
        R^.Right := T^.Right;
        Q := R;
      end
      else
      begin
        S := R^.Right;
        if S^.Right <> nil then
          repeat
            R := S;
            S := R^.Right;
          until S^.Right = nil;
        { now, S = symmetric predecessor of Q }
        S^.Right := T^.Right;
        R^.Right := S^.Left;
        S^.Left := T^.Left;
        Q := S;
      end;
    end;
  end;

  { we decrement before because the tree is already adjusted
    => any exception in FreeNode MUST be ignored.

    It's unlikely that FreeNode would raise an exception anyway. }
  Dec(FCount);
  FreeNode(T);
end;

procedure TStringHashMap.DeleteNodes(var Q: PHashNode);
begin
  if Q^.Left <> nil then
    DeleteNodes(Q^.Left);
  if Q^.Right <> nil then
    DeleteNodes(Q^.Right);
  FreeNode(Q);
  Q := nil;
end;

function TStringHashMap.AllocNode: PHashNode;
begin
  New(Result);
  Result^.Left := nil;
  Result^.Right := nil;
end;

procedure TStringHashMap.FreeNode(ANode: PHashNode);
begin
  Dispose(ANode);
end;

function TStringHashMap.GetData(const S: string): Pointer;
var
  PPN: PPHashNode;
begin
  PPN := FindNode(S);

  if PPN^ <> nil then
    Result := PPN^^.Ptr
  else
    Result := nil;
end;

procedure TStringHashMap.SetData(const S: string; P: Pointer);
var
  PPN: PPHashNode;
begin
  PPN := FindNode(S);

  if PPN^ <> nil then
    PPN^^.Ptr := P
  else
  begin
    { add }
    PPN^ := AllocNode;
    { we increment after in case of exception }
    Inc(FCount);
    PPN^^.Str := S;
    PPN^^.Ptr := P;
  end;
end;

procedure TStringHashMap.Add(const S: string; const P{: Pointer});
var
  PPN: PPHashNode;
begin
  PPN := FindNode(S);

  { if reordered from SetData because PPN^ = nil is more common for Add }
  if PPN^ = nil then
  begin
    { add }
    PPN^ := AllocNode;
    { we increment after in case of exception }
    Inc(FCount);
    PPN^^.Str := S;
    PPN^^.Ptr := Pointer(P);
  end
  else
    raise EJclStringHashMapError.CreateResFmt(@RsStringHashMapDuplicate, [S]);
end;

type
  PListNode = ^TListNode;
  TListNode = record
    Next: PListNode;
    NodeLoc: PPHashNode;
  end;

  PDataParam = ^TDataParam;
  TDataParam = record
    Head: PListNode;
    Data: Pointer;
  end;

procedure NodeIterate_BuildDataList(AUserData: Pointer; ANode: PPHashNode);
var
  DP: PDataParam;
  T: PListNode;
begin
  DP := PDataParam(AUserData);
  if DP.Data = ANode^^.Ptr then
  begin
    New(T);
    T^.Next := DP.Head;
    T^.NodeLoc := ANode;
    DP.Head := T;
  end;
end;

procedure TStringHashMap.RemoveData(const P{: Pointer});
var
  DP: TDataParam;
  I: Integer;
  N, T: PListNode;
begin
  DP.Data := Pointer(P);
  DP.Head := nil;

  for I := 0 to FHashSize - 1 do
    NodeIterate(@FList^[I], @DP, NodeIterate_BuildDataList);

  N := DP.Head;
  while N <> nil do
  begin
    DeleteNode(N^.NodeLoc^);
    T := N;
    N := N^.Next;
    Dispose(T);
  end;
end;

function TStringHashMap.Remove(const S: string): Pointer;
var
  PPN: PPHashNode;
begin
  PPN := FindNode(S);

  if PPN^ <> nil then
  begin
    Result := PPN^^.Ptr;
    DeleteNode(PPN^);
  end
  else
    raise EJclStringHashMapError.CreateResFmt(@RsStringHashMapInvalidNode, [S]);
end;

procedure TStringHashMap.IterateMethod(AUserData: Pointer;
  AIterateMethod: TIterateMethod);
var
  I: Integer;
begin
  for I := 0 to FHashSize - 1 do
    if not IterateMethodNode(FList^[I], AUserData, AIterateMethod) then
      Break;
end;

procedure TStringHashMap.Iterate(AUserData: Pointer; AIterateFunc: TIterateFunc);
var
  I: Integer;
begin
  for I := 0 to FHashSize - 1 do
    if not IterateNode(FList^[I], AUserData, AIterateFunc) then
      Break;
end;

function TStringHashMap.Has(const S: string): Boolean;
var
  PPN: PPHashNode;
begin
  PPN := FindNode(S);
  Result := PPN^ <> nil;
end;

function TStringHashMap.Find(const S: string; var P{: Pointer}): Boolean;
var
  PPN: PPHashNode;
begin
  PPN := FindNode(S);
  Result := PPN^ <> nil;
  if Result then
    Pointer(P) := PPN^^.Ptr;
end;

type
  PFindDataResult = ^TFindDataResult;
  TFindDataResult = record
    Found: Boolean;
    ValueToFind: Pointer;
    Key: string;
  end;

function Iterate_FindData(AUserData: Pointer; const AStr: string;
  var APtr: Pointer): Boolean;
var
  PFdr: PFindDataResult;
begin
  PFdr := PFindDataResult(AUserData);
  PFdr^.Found := (APtr = PFdr^.ValueToFind);
  Result := not PFdr^.Found;
  if PFdr^.Found then
    PFdr^.Key := AStr;
end;

function TStringHashMap.FindData(const P{: Pointer}; var S: string): Boolean;
var
  PFdr: PFindDataResult;
begin
  New(PFdr);
  try
    PFdr^.Found := False;
    PFdr^.ValueToFind := Pointer(P);
    Iterate(PFdr, Iterate_FindData);
    Result := PFdr^.Found;
    if Result then
      S := PFdr^.Key;
  finally
    Dispose(PFdr);
  end;
end;

procedure TStringHashMap.Clear;
var
  I: Integer;
  PPN: PPHashNode;
begin
  for I := 0 to FHashSize - 1 do
  begin
    PPN := @FList^[I];
    if PPN^ <> nil then
      DeleteNodes(PPN^);
  end;
  FCount := 0;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  FreeAndNil(GlobalCaseInsensitiveTraits);
  FreeAndNil(GlobalCaseSensitiveTraits);

end.

