{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: LongIntList.pas, released 2000-04-19.

The Original Code is based on mwTLongIntList.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.

Contributors:

  For a list of the contributors to the mwEdit project see the
  accompanying Contributors.mwEdit.txt file.

$Id: LongIntList.pas,v 1.1.1.1 2000/07/08 15:54:05 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
unit LongIntList;

interface

uses
  Windows, 
  SysUtils, 
  Messages, 
  Classes, 
  Graphics, 
  Controls, 
  Forms, 
  Dialogs, 
  Menus, 
  StdCtrls, 
  ExtCtrls;

type
  PLongIntArray = ^TLongIntArray;
  TLongIntArray = array[0..0] of LongInt;

  TLongIntList = class(TObject)
  private
    FCapacity: Integer;
    FCount: Integer;
    FLongIntList: PLongIntArray;
  protected
    function GetItems(Index: Integer): LongInt;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    procedure SetItems(Index: Integer; Item: LongInt);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Item: LongInt): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function First: LongInt;
    function IndexOf(Item: LongInt): Integer;
    procedure Insert(Index: Integer; Item: LongInt);
    function Last: LongInt;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: LongInt): Integer;
    procedure Sort;
    procedure DeleteGroup(StartIndex: LongInt; GroupCount: LongInt);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: LongInt read GetItems write SetItems; default;
    property LongIntList: PLongIntArray read FLongIntList;
  end; { TLongIntList }

implementation

constructor TLongIntList.Create;
begin
  inherited Create;
end; { Create }

destructor TLongIntList.Destroy;
begin
  Clear;
  inherited Destroy;
end; { Destroy }

{ Based on a non-recursive QuickSort from the SWAG-Archive.
  ( TV Sorting Unit by Brad Williams ) }

procedure TLongIntList.Sort;
var
  Left, Right, SubArray, SubLeft, SubRight, Temp, Pivot: LongInt;
  Stack: array[1..32] of record First, Last: LongInt; end;
begin
  if Count > 1 then
  begin
    SubArray := 1;
    Stack[SubArray].First := 0;
    Stack[SubArray].Last := Count - 1;
    repeat
      Left := Stack[SubArray].First;
      Right := Stack[SubArray].Last;
      Dec(SubArray);
      repeat
        SubLeft := Left;
        SubRight := Right;
        Pivot := FLongIntList[(Left + Right) shr 1];
        repeat
          while FLongIntList[SubLeft] < Pivot do Inc(SubLeft);
          while FLongIntList[SubRight] > Pivot do Dec(SubRight);
          IF SubLeft <= SubRight then
          begin
            Temp := FLongIntList[SubLeft];
            FLongIntList[SubLeft] := FLongIntList[SubRight];
            FLongIntList[SubRight] := Temp;
            Inc(SubLeft);
            Dec(SubRight);
          end;
        until SubLeft > SubRight;
        IF SubLeft < Right then
        begin
          Inc(SubArray);
          Stack[SubArray].First := SubLeft;
          Stack[SubArray].Last := Right;
        end;
        Right := SubRight;
      until Left >= Right;
    until SubArray = 0;
  end;
end; { Sort }

function TLongIntList.GetItems(Index: Integer): LongInt;
begin
  Result := FLongIntList[Index];
end; { GetItems }

procedure TLongIntList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then FCount := NewCapacity;
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FLongIntList, NewCapacity * SizeOf(LongInt));
    FCapacity := NewCapacity;
  end;
end; { SetCapacity }

procedure TLongIntList.SetCount(NewCount: Integer);
begin
  if NewCount > FCapacity then SetCapacity(NewCount);
  FCount := NewCount;
end; { SetCount }

procedure TLongIntList.SetItems(Index: Integer; Item: LongInt);
begin
  FLongIntList[Index] := Item;
end; { SetItems }

function TLongIntList.Add(Item: LongInt): Integer;
begin
  Result := FCount;
  if Result + 1 >= FCapacity then SetCapacity(FCapacity + 1024);
  FLongIntList[Result] := Item;
  Inc(FCount);
end; { Add }

procedure TLongIntList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end; { Clear }

procedure TLongIntList.Delete(Index: Integer);
begin
  Dec(FCount);
  if Index < FCount then
    System.Move(FLongIntList[Index + 1], FLongIntList[Index],
      (FCount - Index) * SizeOf(LongInt));
end; { Delete }

procedure TLongIntList.DeleteGroup(StartIndex: LongInt; GroupCount: LongInt);
begin
  Dec(FCount, GroupCount);
  if StartIndex < FCount then
    System.Move(FLongIntList[StartIndex + GroupCount], FLongIntList[StartIndex],
      (FCount - StartIndex) * SizeOf(LongInt));
end; { DeleteGroup }

procedure TLongIntList.Exchange(Index1, Index2: Integer);
var
  Item: LongInt;
begin
  Item := FLongIntList[Index1];
  FLongIntList[Index1] := FLongIntList[Index2];
  FLongIntList[Index2] := Item;
end; { Exchange }

function TLongIntList.First: LongInt;
begin
  Result := GetItems(0);
end; { First }

function TLongIntList.IndexOf(Item: LongInt): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FLongIntList[Result] <> Item) do Inc(Result);
  if Result = FCount then Result := -1;
end; { IndexOf }

procedure TLongIntList.Insert(Index: Integer; Item: LongInt);
begin
  if FCount = FCapacity then SetCapacity(FCapacity + 1024);
  if Index < FCount then
    System.Move(FLongIntList[Index], FLongIntList[Index + 1],
      (FCount - Index) * SizeOf(LongInt));
  FLongIntList[Index] := Item;
  Inc(FCount);
end; { Insert }

function TLongIntList.Last: LongInt;
begin
  Result := GetItems(FCount - 1);
end; { Last }

procedure TLongIntList.Move(CurIndex, NewIndex: Integer);
var
  Item: LongInt;
begin
  if CurIndex <> NewIndex then
  begin
    Item := GetItems(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Item);
  end;
end; { Move }

function TLongIntList.Remove(Item: LongInt): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then Delete(Result);
end; { Remove }

end.
