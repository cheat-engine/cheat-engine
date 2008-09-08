{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

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
{ The Original Code is Stack.pas.                                                                  }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-07-20 11:13:23 +0200 (dim., 20 juil. 2008)                         $ }
{ Revision:      $Rev:: 2393                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStacks;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF SUPPORTS_GENERICS}
  {$IFDEF CLR}
  System.Collections.Generic,
  {$ENDIF CLR}
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclContainerIntf, JclSynch;

type
  TJclIntfStack = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclIntfEqualityComparer,
    IJclIntfStack)
  private
    FElements: JclBase.TDynIInterfaceArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIntfStack }
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function Empty: Boolean;
    function Peek: IInterface;
    function Pop: IInterface;
    function Push(const AInterface: IInterface): Boolean;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclAnsiStrStack = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer, IJclAnsiStrEqualityComparer,
    IJclAnsiStrStack)
  private
    FElements: JclBase.TDynAnsiStringArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclAnsiStrStack }
    procedure Clear;
    function Contains(const AString: AnsiString): Boolean;
    function Empty: Boolean;
    function Peek: AnsiString;
    function Pop: AnsiString;
    function Push(const AString: AnsiString): Boolean;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclWideStrStack = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclStrContainer, IJclWideStrContainer, IJclWideStrEqualityComparer,
    IJclWideStrStack)
  private
    FElements: JclBase.TDynWideStringArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclWideStrStack }
    procedure Clear;
    function Contains(const AString: WideString): Boolean;
    function Empty: Boolean;
    function Peek: WideString;
    function Pop: WideString;
    function Push(const AString: WideString): Boolean;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrStack = TJclAnsiStrStack;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrStack = TJclWideStrStack;
  {$ENDIF CONTAINER_WIDESTR}

  TJclSingleStack = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclSingleContainer, IJclSingleEqualityComparer,
    IJclSingleStack)
  private
    FElements: JclBase.TDynSingleArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclSingleStack }
    procedure Clear;
    function Contains(const AValue: Single): Boolean;
    function Empty: Boolean;
    function Peek: Single;
    function Pop: Single;
    function Push(const AValue: Single): Boolean;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclDoubleStack = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclDoubleContainer, IJclDoubleEqualityComparer,
    IJclDoubleStack)
  private
    FElements: JclBase.TDynDoubleArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclDoubleStack }
    procedure Clear;
    function Contains(const AValue: Double): Boolean;
    function Empty: Boolean;
    function Peek: Double;
    function Pop: Double;
    function Push(const AValue: Double): Boolean;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclExtendedStack = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclExtendedContainer, IJclExtendedEqualityComparer,
    IJclExtendedStack)
  private
    FElements: JclBase.TDynExtendedArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclExtendedStack }
    procedure Clear;
    function Contains(const AValue: Extended): Boolean;
    function Empty: Boolean;
    function Peek: Extended;
    function Pop: Extended;
    function Push(const AValue: Extended): Boolean;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatStack = TJclExtendedStack;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatStack = TJclDoubleStack;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatStack = TJclSingleStack;
  {$ENDIF MATH_SINGLE_PRECISION}

  TJclIntegerStack = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclIntegerEqualityComparer,
    IJclIntegerStack)
  private
    FElements: JclBase.TDynIntegerArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIntegerStack }
    procedure Clear;
    function Contains(AValue: Integer): Boolean;
    function Empty: Boolean;
    function Peek: Integer;
    function Pop: Integer;
    function Push(AValue: Integer): Boolean;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclCardinalStack = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclCardinalEqualityComparer,
    IJclCardinalStack)
  private
    FElements: JclBase.TDynCardinalArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclCardinalStack }
    procedure Clear;
    function Contains(AValue: Cardinal): Boolean;
    function Empty: Boolean;
    function Peek: Cardinal;
    function Pop: Cardinal;
    function Push(AValue: Cardinal): Boolean;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclInt64Stack = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclInt64EqualityComparer,
    IJclInt64Stack)
  private
    FElements: JclBase.TDynInt64Array;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclInt64Stack }
    procedure Clear;
    function Contains(const AValue: Int64): Boolean;
    function Empty: Boolean;
    function Peek: Int64;
    function Pop: Int64;
    function Push(const AValue: Int64): Boolean;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  {$IFNDEF CLR}
  TJclPtrStack = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclPtrEqualityComparer,
    IJclPtrStack)
  private
    FElements: JclBase.TDynPointerArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclPtrStack }
    procedure Clear;
    function Contains(APtr: Pointer): Boolean;
    function Empty: Boolean;
    function Peek: Pointer;
    function Pop: Pointer;
    function Push(APtr: Pointer): Boolean;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;
  {$ENDIF ~CLR}

  TJclStack = class(TJclAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclEqualityComparer, IJclObjectOwner,
    IJclStack)
  private
    FElements: JclBase.TDynObjectArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStack }
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function Empty: Boolean;
    function Peek: TObject;
    function Pop: TObject;
    function Push(AObject: TObject): Boolean;
    function Size: Integer;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer; AOwnsObjects: Boolean);
    destructor Destroy; override;
  end;

  {$IFDEF SUPPORTS_GENERICS}

  TJclStack<T> = class(TJclAbstractContainer<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclEqualityComparer<T>, IJclItemOwner<T>,
    IJclStack<T>)
  private
    FElements: TJclBase<T>.TDynArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStack<T> }
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function Empty: Boolean;
    function Peek: T;
    function Pop: T;
    function Push(const AItem: T): Boolean;
    function Size: Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsItems: Boolean);
    destructor Destroy; override;
  end;

  // E = external helper to compare items for equality
  TJclStackE<T> = class(TJclStack<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer,
    IJclStack<T>, IJclItemOwner<T>)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer; AOwnsItems: Boolean);

    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclStackF<T> = class(TJclStack<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer,
    IJclStack<T>, IJclItemOwner<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);
  end;

  // I = items can compare themselves to an other for equality
  TJclStackI<T: IEquatable<T>> = class(TJclStack<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer,
    IJclStack<T>, IJclItemOwner<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;
  {$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclStacks.pas $';
    Revision: '$Revision: 2393 $';
    Date: '$Date: 2008-07-20 11:13:23 +0200 (dim., 20 juil. 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils;

//=== { TJclIntfStack } =======================================================

constructor TJclIntfStack.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclIntfStack.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfStack.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclIntfStack;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfStack then
  begin
    ADest := TJclIntfStack(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclIntfStack.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeObject(FElements[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStack.Contains(const AInterface: IInterface): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElements[I], AInterface) then
      begin
        Result := True;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfStack.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfStack.Peek: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FElements[FSize - 1]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStack.Pop: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
    begin
      Dec(FSize);
      Result := FElements[FSize];
      FElements[FSize] := nil;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStack.Push(const AInterface: IInterface): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = FCapacity then
      AutoGrow;
    Result := FSize < FCapacity;
    if Result then
    begin
      FElements[FSize] := AInterface;
      Inc(FSize);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfStack.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FElements, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStack.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclAnsiStrStack } =======================================================

constructor TJclAnsiStrStack.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclAnsiStrStack.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrStack.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclAnsiStrStack;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrStack then
  begin
    ADest := TJclAnsiStrStack(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclAnsiStrStack.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeString(FElements[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrStack.Contains(const AString: AnsiString): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElements[I], AString) then
      begin
        Result := True;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrStack.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrStack.Peek: AnsiString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FElements[FSize - 1]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrStack.Pop: AnsiString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
    begin
      Dec(FSize);
      Result := FElements[FSize];
      FElements[FSize] := '';
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrStack.Push(const AString: AnsiString): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = FCapacity then
      AutoGrow;
    Result := FSize < FCapacity;
    if Result then
    begin
      FElements[FSize] := AString;
      Inc(FSize);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrStack.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FElements, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrStack.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclWideStrStack } =======================================================

constructor TJclWideStrStack.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclWideStrStack.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrStack.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclWideStrStack;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrStack then
  begin
    ADest := TJclWideStrStack(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclWideStrStack.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeString(FElements[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrStack.Contains(const AString: WideString): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElements[I], AString) then
      begin
        Result := True;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclWideStrStack.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrStack.Peek: WideString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FElements[FSize - 1]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrStack.Pop: WideString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
    begin
      Dec(FSize);
      Result := FElements[FSize];
      FElements[FSize] := '';
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrStack.Push(const AString: WideString): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = FCapacity then
      AutoGrow;
    Result := FSize < FCapacity;
    if Result then
    begin
      FElements[FSize] := AString;
      Inc(FSize);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrStack.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FElements, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrStack.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclSingleStack } =======================================================

constructor TJclSingleStack.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclSingleStack.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclSingleStack.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclSingleStack;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclSingleStack then
  begin
    ADest := TJclSingleStack(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclSingleStack.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeSingle(FElements[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleStack.Contains(const AValue: Single): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElements[I], AValue) then
      begin
        Result := True;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclSingleStack.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleStack.Peek: Single;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FElements[FSize - 1]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleStack.Pop: Single;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
    begin
      Dec(FSize);
      Result := FElements[FSize];
      FElements[FSize] := 0.0;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleStack.Push(const AValue: Single): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = FCapacity then
      AutoGrow;
    Result := FSize < FCapacity;
    if Result then
    begin
      FElements[FSize] := AValue;
      Inc(FSize);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleStack.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FElements, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleStack.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclDoubleStack } =======================================================

constructor TJclDoubleStack.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclDoubleStack.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclDoubleStack.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclDoubleStack;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclDoubleStack then
  begin
    ADest := TJclDoubleStack(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclDoubleStack.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeDouble(FElements[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleStack.Contains(const AValue: Double): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElements[I], AValue) then
      begin
        Result := True;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclDoubleStack.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleStack.Peek: Double;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FElements[FSize - 1]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleStack.Pop: Double;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
    begin
      Dec(FSize);
      Result := FElements[FSize];
      FElements[FSize] := 0.0;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleStack.Push(const AValue: Double): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = FCapacity then
      AutoGrow;
    Result := FSize < FCapacity;
    if Result then
    begin
      FElements[FSize] := AValue;
      Inc(FSize);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleStack.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FElements, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleStack.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclExtendedStack } =======================================================

constructor TJclExtendedStack.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclExtendedStack.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclExtendedStack.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclExtendedStack;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclExtendedStack then
  begin
    ADest := TJclExtendedStack(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclExtendedStack.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeExtended(FElements[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedStack.Contains(const AValue: Extended): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElements[I], AValue) then
      begin
        Result := True;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclExtendedStack.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedStack.Peek: Extended;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FElements[FSize - 1]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedStack.Pop: Extended;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
    begin
      Dec(FSize);
      Result := FElements[FSize];
      FElements[FSize] := 0.0;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedStack.Push(const AValue: Extended): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = FCapacity then
      AutoGrow;
    Result := FSize < FCapacity;
    if Result then
    begin
      FElements[FSize] := AValue;
      Inc(FSize);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedStack.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FElements, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedStack.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclIntegerStack } =======================================================

constructor TJclIntegerStack.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclIntegerStack.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntegerStack.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclIntegerStack;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntegerStack then
  begin
    ADest := TJclIntegerStack(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclIntegerStack.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeInteger(FElements[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerStack.Contains(AValue: Integer): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElements[I], AValue) then
      begin
        Result := True;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntegerStack.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerStack.Peek: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FElements[FSize - 1]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerStack.Pop: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
    begin
      Dec(FSize);
      Result := FElements[FSize];
      FElements[FSize] := 0;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerStack.Push(AValue: Integer): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = FCapacity then
      AutoGrow;
    Result := FSize < FCapacity;
    if Result then
    begin
      FElements[FSize] := AValue;
      Inc(FSize);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerStack.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FElements, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerStack.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclCardinalStack } =======================================================

constructor TJclCardinalStack.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclCardinalStack.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclCardinalStack.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclCardinalStack;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclCardinalStack then
  begin
    ADest := TJclCardinalStack(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclCardinalStack.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeCardinal(FElements[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalStack.Contains(AValue: Cardinal): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElements[I], AValue) then
      begin
        Result := True;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclCardinalStack.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalStack.Peek: Cardinal;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FElements[FSize - 1]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalStack.Pop: Cardinal;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
    begin
      Dec(FSize);
      Result := FElements[FSize];
      FElements[FSize] := 0;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalStack.Push(AValue: Cardinal): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = FCapacity then
      AutoGrow;
    Result := FSize < FCapacity;
    if Result then
    begin
      FElements[FSize] := AValue;
      Inc(FSize);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalStack.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FElements, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalStack.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclInt64Stack } =======================================================

constructor TJclInt64Stack.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclInt64Stack.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclInt64Stack.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclInt64Stack;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclInt64Stack then
  begin
    ADest := TJclInt64Stack(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclInt64Stack.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeInt64(FElements[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Stack.Contains(const AValue: Int64): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElements[I], AValue) then
      begin
        Result := True;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Stack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Stack.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclInt64Stack.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64Stack.Peek: Int64;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FElements[FSize - 1]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Stack.Pop: Int64;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
    begin
      Dec(FSize);
      Result := FElements[FSize];
      FElements[FSize] := 0;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Stack.Push(const AValue: Int64): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = FCapacity then
      AutoGrow;
    Result := FSize < FCapacity;
    if Result then
    begin
      FElements[FSize] := AValue;
      Inc(FSize);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Stack.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FElements, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Stack.Size: Integer;
begin
  Result := FSize;
end;

{$IFNDEF CLR}
//=== { TJclPtrStack } =======================================================

constructor TJclPtrStack.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclPtrStack.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclPtrStack.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclPtrStack;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclPtrStack then
  begin
    ADest := TJclPtrStack(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclPtrStack.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreePointer(FElements[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrStack.Contains(APtr: Pointer): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElements[I], APtr) then
      begin
        Result := True;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrStack.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclPtrStack.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrStack.Peek: Pointer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FElements[FSize - 1]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrStack.Pop: Pointer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
    begin
      Dec(FSize);
      Result := FElements[FSize];
      FElements[FSize] := nil;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrStack.Push(APtr: Pointer): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = FCapacity then
      AutoGrow;
    Result := FSize < FCapacity;
    if Result then
    begin
      FElements[FSize] := APtr;
      Inc(FSize);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrStack.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FElements, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrStack.Size: Integer;
begin
  Result := FSize;
end;
{$ENDIF ~CLR}

//=== { TJclStack } =======================================================

constructor TJclStack.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  SetCapacity(ACapacity);
end;

destructor TJclStack.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclStack.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclStack;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclStack then
  begin
    ADest := TJclStack(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclStack.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeObject(FElements[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack.Contains(AObject: TObject): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElements[I], AObject) then
      begin
        Result := True;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclStack.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclStack.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclStack.Peek: TObject;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FElements[FSize - 1]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack.Pop: TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
    begin
      Dec(FSize);
      Result := FElements[FSize];
      FElements[FSize] := nil;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack.Push(AObject: TObject): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = FCapacity then
      AutoGrow;
    Result := FSize < FCapacity;
    if Result then
    begin
      FElements[FSize] := AObject;
      Inc(FSize);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStack.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FElements, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack.Size: Integer;
begin
  Result := FSize;
end;

{$IFDEF SUPPORTS_GENERICS}

//=== { TJclStack<T> } =======================================================

constructor TJclStack<T>.Create(ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  SetCapacity(ACapacity);
end;

destructor TJclStack<T>.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclStack<T>.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclStack<T>;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclStack<T> then
  begin
    ADest := TJclStack<T>(Dest);
    ADest.Clear;
    ADest.SetCapacity(FSize + 1);
    for I := 0 to FSize - 1 do
      ADest.FElements[I] := FElements[I];
    ADest.FSize := FSize;
  end;
end;

procedure TJclStack<T>.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeItem(FElements[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack<T>.Contains(const AItem: T): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElements[I], AItem) then
      begin
        Result := True;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclStack<T>.Empty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclStack<T>.Peek: T;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if FSize > 0 then
      Result := FElements[FSize - 1]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack<T>.Pop: T;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if FSize > 0 then
    begin
      Dec(FSize);
      Result := FElements[FSize];
      FElements[FSize] := Default(T);
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack<T>.Push(const AItem: T): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = FCapacity then
      AutoGrow;
    Result := FSize < FCapacity;
    if Result then
    begin
      FElements[FSize] := AItem;
      Inc(FSize);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStack<T>.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FElements, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStack<T>.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclStackE<T> } ======================================================

constructor TJclStackE<T>.Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer;
  AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

procedure TJclStackE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclStackE<T> then
    TJclStackE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclStackE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclStackE<T>.Create(FEqualityComparer, FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclStackE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer <> nil then
    Result := EqualityComparer.Equals(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclStackF<T> } ======================================================

constructor TJclStackF<T>.Create(AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  SetEqualityCompare(AEqualityCompare);
end;

function TJclStackF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclStackF<T>.Create(FEqualityCompare, FSize + 1, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclStackI<T> } ======================================================

function TJclStackI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclStackI<T>.Create(FSize + 1, False);
  AssignPropertiesTo(Result);
end;

function TJclStackI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := A.Equals(B);
end;

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
