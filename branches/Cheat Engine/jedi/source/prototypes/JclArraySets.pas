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
{ The Original Code is ArraySet.pas.                                                               }
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
{ Last modified: $Date:: 2008-06-05 15:35:37 +0200 (jeu., 05 juin 2008)                         $ }
{ Revision:      $Rev:: 2376                                                                     $ }
{ Author:        $Author:: obones                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclArraySets;

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
  JclBase, JclAbstractContainers, JclContainerIntf, JclArrayLists, JclSynch;
{$I containers\JclContainerCommon.imp}
{$I containers\JclArraySets.int}
{$I containers\JclArraySets.imp}
type
(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclIntfArraySet,TJclIntfArrayList,IJclIntfCollection,IJclIntfList,IJclIntfArray,IJclIntfSet, IJclIntfEqualityComparer\, IJclIntfComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AInterface,IInterface)*)

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclAnsiStrArraySet,TJclAnsiStrArrayList,IJclAnsiStrCollection,IJclAnsiStrList,IJclAnsiStrArray,IJclAnsiStrSet, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrFlatContainer\, IJclAnsiStrEqualityComparer\, IJclAnsiStrComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,, override;,const ,AString,AnsiString)*)

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclWideStrArraySet,TJclWideStrArrayList,IJclWideStrCollection,IJclWideStrList,IJclWideStrArray,IJclWideStrSet, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrFlatContainer\, IJclWideStrEqualityComparer\, IJclWideStrComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,, override;,const ,AString,WideString)*)

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrArraySet = TJclAnsiStrArraySet;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrArraySet = TJclWideStrArraySet;
  {$ENDIF CONTAINER_WIDESTR}

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclSingleArraySet,TJclSingleArrayList,IJclSingleCollection,IJclSingleList,IJclSingleArray,IJclSingleSet, IJclSingleContainer\, IJclSingleEqualityComparer\, IJclSingleComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Single)*)

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclDoubleArraySet,TJclDoubleArrayList,IJclDoubleCollection,IJclDoubleList,IJclDoubleArray,IJclDoubleSet, IJclDoubleContainer\, IJclDoubleEqualityComparer\, IJclDoubleComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Double)*)

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclExtendedArraySet,TJclExtendedArrayList,IJclExtendedCollection,IJclExtendedList,IJclExtendedArray,IJclExtendedSet, IJclExtendedContainer\, IJclExtendedEqualityComparer\, IJclExtendedComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Extended)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatArraySet = TJclExtendedArraySet;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatArraySet = TJclDoubleArraySet;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatArraySet = TJclSingleArraySet;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclIntegerArraySet,TJclIntegerArrayList,IJclIntegerCollection,IJclIntegerList,IJclIntegerArray,IJclIntegerSet, IJclIntegerEqualityComparer\, IJclIntegerComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,AValue,Integer)*)

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclCardinalArraySet,TJclCardinalArrayList,IJclCardinalCollection,IJclCardinalList,IJclCardinalArray,IJclCardinalSet, IJclCardinalEqualityComparer\, IJclCardinalComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,AValue,Cardinal)*)

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclInt64ArraySet,TJclInt64ArrayList,IJclInt64Collection,IJclInt64List,IJclInt64Array,IJclInt64Set, IJclInt64EqualityComparer\, IJclInt64Comparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Int64)*)

  {$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclPtrArraySet,TJclPtrArrayList,IJclPtrCollection,IJclPtrList,IJclPtrArray,IJclPtrSet, IJclPtrEqualityComparer\, IJclPtrComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,APtr,Pointer)*)
  {$ENDIF ~CLR}

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclArraySet,TJclArrayList,IJclCollection,IJclList,IJclArray,IJclSet, IJclObjectOwner\, IJclEqualityComparer\, IJclComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,AObject,TObject)*)

  {$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclArraySet<T>,TJclArrayList<T>,IJclCollection<T>,IJclList<T>,IJclArray<T>,IJclSet<T>, IJclItemOwner<T>\, IJclEqualityComparer<T>\, IJclComparer<T>\,,,,,,const ,AItem,T)*)

  // E = External helper to compare items
  TJclArraySetE<T> = class(TJclArraySet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>)
  private
    FComparer: IComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function ItemsCompare(const A, B: T): Integer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AComparer: IComparer<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const AComparer: IComparer<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;

    property Comparer: IComparer<T> read FComparer write FComparer;
  end;

  // F = Function to compare items
  TJclArraySetF<T> = class(TJclArraySet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const ACompare: TCompare<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const ACompare: TCompare<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;
  end;

  // I = Items can compare themselves to others
  TJclArraySetI<T: IComparable<T>> = class(TJclArraySet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>)
  protected
    function ItemsCompare(const A, B: T): Integer; override;
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
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/prototypes/JclArraySets.pas $';
    Revision: '$Revision: 2376 $';
    Date: '$Date: 2008-06-05 15:35:37 +0200 (jeu., 05 juin 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils;

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclIntfArraySet,IJclIntfCollection,IJclIntfIterator,const ,AInterface,IInterface,nil,GetObject)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclAnsiStrArraySet,IJclAnsiStrCollection,IJclAnsiStrIterator,const ,AString,AnsiString,'',GetString)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclWideStrArraySet,IJclWideStrCollection,IJclWideStrIterator,const ,AString,WideString,'',GetString)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclSingleArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclSingleArraySet,IJclSingleCollection,IJclSingleIterator,const ,AValue,Single,0.0,GetValue)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclDoubleArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclDoubleArraySet,IJclDoubleCollection,IJclDoubleIterator,const ,AValue,Double,0.0,GetValue)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclExtendedArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclExtendedArraySet,IJclExtendedCollection,IJclExtendedIterator,const ,AValue,Extended,0.0,GetValue)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntegerArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclIntegerArraySet,IJclIntegerCollection,IJclIntegerIterator,,AValue,Integer,0,GetValue)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclCardinalArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclCardinalArraySet,IJclCardinalCollection,IJclCardinalIterator,,AValue,Cardinal,0,GetValue)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclInt64ArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64ArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclInt64ArraySet,IJclInt64Collection,IJclInt64Iterator,const ,AValue,Int64,0,GetValue)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFNDEF CLR}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclPtrArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclPtrArraySet,IJclPtrCollection,IJclPtrIterator,,APtr,Pointer,nil,GetPointer)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$ENDIF ~CLR}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArraySet.Create(Size, False);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclArraySet,IJclCollection,IJclIterator,,AObject,TObject,nil,GetObject)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFDEF SUPPORTS_GENERICS}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
{$JPPEXPANDMACRO JCLARRAYSETIMP(TJclArraySet<T>,IJclCollection<T>,IJclIterator<T>,const ,AItem,T,Default(T),GetItem)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

//=== { TJclArraySetE<T> } ===================================================

constructor TJclArraySetE<T>.Create(const AComparer: IComparer<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FComparer := AComparer;
end;

constructor TJclArraySetE<T>.Create(const AComparer: IComparer<T>; const ACollection: IJclCollection<T>;
  AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FComparer := AComparer;
end;

procedure TJclArraySetE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclArraySetE<T> then
    TJclArraySetE<T>(Dest).FComparer := Comparer;
end;

function TJclArraySetE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArraySetE<T>.Create(Comparer, Size, False);
  AssignPropertiesTo(Result);
end;

function TJclArraySetE<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Comparer <> nil then
    Result := Comparer.Compare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclArraySetE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Comparer <> nil then
    Result := Comparer.Compare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclArraySetF<T> } ===================================================

constructor TJclArraySetF<T>.Create(const ACompare: TCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  SetCompare(ACompare);
end;

constructor TJclArraySetF<T>.Create(const ACompare: TCompare<T>; const ACollection: IJclCollection<T>;
  AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  SetCompare(ACompare);
end;

function TJclArraySetF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArraySetF<T>.Create(Compare, Size, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclArraySetI<T> } ===================================================

function TJclArraySetI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArraySetI<T>.Create(Size, False);
  AssignPropertiesTo(Result);
end;

function TJclArraySetI<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := A.CompareTo(B);
end;

function TJclArraySetI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := A.CompareTo(B) = 0;
end;

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

