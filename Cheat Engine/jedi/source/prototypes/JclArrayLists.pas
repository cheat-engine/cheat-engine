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
{ The Original Code is ArrayList.pas.                                                              }
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
{ Last modified: $Date:: 2008-06-05 15:35:37 +0200 (jeu., 05 juin 2008)                          $ }
{ Revision:      $Rev:: 2376                                                                     $ }
{ Author:        $Author:: obones                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclArrayLists;

{$I jcl.inc}

interface

uses
  Classes,
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
{$I containers\JclContainerCommon.imp}
{$I containers\JclArrayLists.int}
{$I containers\JclArrayLists.imp}
type
{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclIntfArrayList,TJclIntfAbstractContainer,IJclIntfCollection,IJclIntfList,IJclIntfArray,IJclIntfIterator,JclBase.TDynIInterfaceArray, IJclIntfEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const ,AInterface,IInterface,GetObject,SetObject)}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclAnsiStrArrayList,TJclAnsiStrAbstractCollection,IJclAnsiStrCollection,IJclAnsiStrList,IJclAnsiStrArray,IJclAnsiStrIterator,JclBase.TDynAnsiStringArray, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrFlatContainer\, IJclAnsiStrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,, override;,,const ,AString,AnsiString,GetString,SetString)}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclWideStrArrayList,TJclWideStrAbstractCollection,IJclWideStrCollection,IJclWideStrList,IJclWideStrArray,IJclWideStrIterator,JclBase.TDynWideStringArray, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrFlatContainer\, IJclWideStrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,, override;,,const ,AString,WideString,GetString,SetString)}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrArrayList = TJclAnsiStrArrayList;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrArrayList = TJclWideStrArrayList;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclSingleArrayList,TJclSingleAbstractContainer,IJclSingleCollection,IJclSingleList,IJclSingleArray,IJclSingleIterator,JclBase.TDynSingleArray, IJclSingleContainer\, IJclSingleEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const ,AValue,Single,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclDoubleArrayList,TJclDoubleAbstractContainer,IJclDoubleCollection,IJclDoubleList,IJclDoubleArray,IJclDoubleIterator,JclBase.TDynDoubleArray, IJclDoubleContainer\, IJclDoubleEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const ,AValue,Double,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclExtendedArrayList,TJclExtendedAbstractContainer,IJclExtendedCollection,IJclExtendedList,IJclExtendedArray,IJclExtendedIterator,JclBase.TDynExtendedArray, IJclExtendedContainer\, IJclExtendedEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const ,AValue,Extended,GetValue,SetValue)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatArrayList = TJclExtendedArrayList;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatArrayList = TJclDoubleArrayList;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatArrayList = TJclSingleArrayList;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclIntegerArrayList,TJclIntegerAbstractContainer,IJclIntegerCollection,IJclIntegerList,IJclIntegerArray,IJclIntegerIterator,JclBase.TDynIntegerArray, IJclIntegerEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,,AValue,Integer,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclCardinalArrayList,TJclCardinalAbstractContainer,IJclCardinalCollection,IJclCardinalList,IJclCardinalArray,IJclCardinalIterator,JclBase.TDynCardinalArray, IJclCardinalEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,,AValue,Cardinal,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclInt64ArrayList,TJclInt64AbstractContainer,IJclInt64Collection,IJclInt64List,IJclInt64Array,IJclInt64Iterator,JclBase.TDynInt64Array, IJclInt64EqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const ,AValue,Int64,GetValue,SetValue)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclPtrArrayList,TJclPtrAbstractContainer,IJclPtrCollection,IJclPtrList,IJclPtrArray,IJclPtrIterator,JclBase.TDynPointerArray, IJclPtrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,,APtr,Pointer,GetPointer,SetPointer)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclArrayList,TJclAbstractContainer,IJclCollection,IJclList,IJclArray,IJclIterator,JclBase.TDynObjectArray, IJclObjectOwner\, IJclEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,; AOwnsObjects: Boolean,,AObject,TObject,GetObject,SetObject)}

  {$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclArrayList<T>,TJclAbstractContainer<T>,IJclCollection<T>,IJclList<T>,IJclArray<T>,IJclIterator<T>,TJclBase<T>.TDynArray, IJclItemOwner<T>\, IJclEqualityComparer<T>\,,,,,,; AOwnsItems: Boolean,const ,AItem,T,GetItem,SetItem)}

  // E = External helper to compare items for equality
  // GetHashCode is not used
  TJclArrayListE<T> = class(TJclArrayList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;

    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclArrayListF<T> = class(TJclArrayList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;
  end;

  // I = Items can compare themselves to others
  TJclArrayListI<T: IEquatable<T>> = class(TJclArrayList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>)
  protected
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  {$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/prototypes/JclArrayLists.pas $';
    Revision: '$Revision: 2376 $';
    Date: '$Date: 2008-06-05 15:35:37 +0200 (jeu., 05 juin 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils;

type
  TItrStart = (isFirst, isLast);

type
{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TIntfItr,IJclIntfIterator,IJclIntfList,const ,AInterface,IInterface,GetObject,SetObject)}

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TIntfItr,IJclIntfIterator,IJclIntfList,const ,AInterface,IInterface,GetObject,SetObject)}

type
{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TAnsiStrItr,IJclAnsiStrIterator,IJclAnsiStrList,const ,AString,AnsiString,GetString,SetString)}

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TAnsiStrItr,IJclAnsiStrIterator,IJclAnsiStrList,const ,AString,AnsiString,GetString,SetString)}

type
{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TWideStrItr,IJclWideStrIterator,IJclWideStrList,const ,AString,WideString,GetString,SetString)}

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TWideStrItr,IJclWideStrIterator,IJclWideStrList,const ,AString,WideString,GetString,SetString)}

type
{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TSingleItr,IJclSingleIterator,IJclSingleList,const ,AValue,Single,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TSingleItr,IJclSingleIterator,IJclSingleList,const ,AValue,Single,GetValue,SetValue)}

type
{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TDoubleItr,IJclDoubleIterator,IJclDoubleList,const ,AValue,Double,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TDoubleItr,IJclDoubleIterator,IJclDoubleList,const ,AValue,Double,GetValue,SetValue)}

type
{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TExtendedItr,IJclExtendedIterator,IJclExtendedList,const ,AValue,Extended,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TExtendedItr,IJclExtendedIterator,IJclExtendedList,const ,AValue,Extended,GetValue,SetValue)}

type
{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TIntegerItr,IJclIntegerIterator,IJclIntegerList,,AValue,Integer,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TIntegerItr,IJclIntegerIterator,IJclIntegerList,,AValue,Integer,GetValue,SetValue)}

type
{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TCardinalItr,IJclCardinalIterator,IJclCardinalList,,AValue,Cardinal,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TCardinalItr,IJclCardinalIterator,IJclCardinalList,,AValue,Cardinal,GetValue,SetValue)}

type
{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TInt64Itr,IJclInt64Iterator,IJclInt64List,const ,AValue,Int64,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TInt64Itr,IJclInt64Iterator,IJclInt64List,const ,AValue,Int64,GetValue,SetValue)}

{$IFNDEF CLR}
type
{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TPtrItr,IJclPtrIterator,IJclPtrList,,APtr,Pointer,GetPointer,SetPointer)}

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TPtrItr,IJclPtrIterator,IJclPtrList,,APtr,Pointer,GetPointer,SetPointer)}
{$ENDIF ~CLR}

type
{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TItr,IJclIterator,IJclList,,AObject,TObject,GetObject,SetObject)}

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TItr,IJclIterator,IJclList,,AObject,TObject,GetObject,SetObject)}

{$IFDEF SUPPORTS_GENERICS}
type
{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TItr<T>,IJclIterator<T>,IJclList<T>,const ,AItem,T,GetItem,SetItem)}

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TItr<T>,IJclIterator<T>,IJclList<T>,const ,AItem,T,GetItem,SetItem)}
{$ENDIF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclIntfArrayList,,,IJclIntfCollection,IJclIntfIterator,TIntfItr,IJclIntfList,const ,AInterface,GetObject,SetObject,FreeObject,IInterface,nil,JclBase.MoveArray)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclAnsiStrArrayList,,,IJclAnsiStrCollection,IJclAnsiStrIterator,TAnsiStrItr,IJclAnsiStrList,const ,AString,GetString,SetString,FreeString,AnsiString,'',JclBase.MoveArray)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclWideStrArrayList,,,IJclWideStrCollection,IJclWideStrIterator,TWideStrItr,IJclWideStrList,const ,AString,GetString,SetString,FreeString,WideString,'',JclBase.MoveArray)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclSingleArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclSingleArrayList,,,IJclSingleCollection,IJclSingleIterator,TSingleItr,IJclSingleList,const ,AValue,GetValue,SetValue,FreeSingle,Single,0.0,JclBase.MoveArray)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclDoubleArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclDoubleArrayList,,,IJclDoubleCollection,IJclDoubleIterator,TDoubleItr,IJclDoubleList,const ,AValue,GetValue,SetValue,FreeDouble,Double,0.0,JclBase.MoveArray)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclExtendedArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclExtendedArrayList,,,IJclExtendedCollection,IJclExtendedIterator,TExtendedItr,IJclExtendedList,const ,AValue,GetValue,SetValue,FreeExtended,Extended,0.0,JclBase.MoveArray)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntegerArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclIntegerArrayList,,,IJclIntegerCollection,IJclIntegerIterator,TIntegerItr,IJclIntegerList,,AValue,GetValue,SetValue,FreeInteger,Integer,0,JclBase.MoveArray)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclCardinalArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclCardinalArrayList,,,IJclCardinalCollection,IJclCardinalIterator,TCardinalItr,IJclCardinalList,,AValue,GetValue,SetValue,FreeCardinal,Cardinal,0,JclBase.MoveArray)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclInt64ArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64ArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclInt64ArrayList,,,IJclInt64Collection,IJclInt64Iterator,TInt64Itr,IJclInt64List,const ,AValue,GetValue,SetValue,FreeInt64,Int64,0,JclBase.MoveArray)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFNDEF CLR}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclPtrArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclPtrArrayList,,,IJclPtrCollection,IJclPtrIterator,TPtrItr,IJclPtrList,,APtr,GetPointer,SetPointer,FreePointer,Pointer,nil,JclBase.MoveArray)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$ENDIF ~CLR}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArrayList.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclArrayList,; AOwnsObjects: Boolean,AOwnsObjects,IJclCollection,IJclIterator,TItr,IJclList,,AObject,GetObject,SetObject,FreeObject,TObject,nil,JclBase.MoveArray)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFDEF SUPPORTS_GENERICS}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclArrayList<T>,; AOwnsItems: Boolean,AOwnsItems,IJclCollection<T>,IJclIterator<T>,TItr<T>,IJclList<T>,const ,AItem,GetItem,SetItem,FreeItem,T,Default(T),TJclBase<T>.MoveArray)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

//=== { TJclArrayListE<T> } ==================================================

constructor TJclArrayListE<T>.Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer;
  AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

constructor TJclArrayListE<T>.Create(const AEqualityComparer: IEqualityComparer<T>;
  const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

procedure TJclArrayListE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclArrayListE<T> then
    TJclArrayListE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclArrayListE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArrayListE<T>.Create(EqualityComparer, FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclArrayListE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer <> nil then
    Result := EqualityComparer.Equals(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclArrayListF<T> } ==================================================

constructor TJclArrayListF<T>.Create(const AEqualityCompare: TEqualityCompare<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  SetEqualityCompare(AEqualityCompare);
end;

constructor TJclArrayListF<T>.Create(const AEqualityCompare: TEqualityCompare<T>; const ACollection: IJclCollection<T>;
  AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  SetEqualityCompare(AEqualityCompare);
end;

function TJclArrayListF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArrayListF<T>.Create(EqualityCompare, FSize, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclArrayListI<T> } ==================================================

function TJclArrayListI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArrayListI<T>.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclArrayListI<T>.ItemsEqual(const A, B: T): Boolean;
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

