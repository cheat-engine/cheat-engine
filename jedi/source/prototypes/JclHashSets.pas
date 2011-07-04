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
{ The Original Code is HashSet.pas.                                                                }
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

unit JclHashSets;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes,
  {$IFDEF SUPPORTS_GENERICS}
  {$IFDEF CLR}
  System.Collections.Generic,
  {$ENDIF CLR}
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclContainerIntf, JclHashMaps, JclSynch;
{$I containers\JclContainerCommon.imp}
{$I containers\JclHashSets.imp}
{$I containers\JclHashSets.int}
type
  {$IFDEF SUPPORTS_GENERICS}
  TRefUnique = class;
  TRefUnique = class(TEquatable<TRefUnique>)
  end;
  {$ELSE ~SUPPORTS_GENERICS}
  TRefUnique = TObject;
  {$ENDIF ~SUPPORTS_GENERICS}

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclIntfHashSet,TJclIntfAbstractContainer,IJclIntfCollection,IJclIntfSet,IJclIntfMap,IJclIntfIterator, IJclIntfEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,
    constructor Create(ACapacity: Integer); overload;,const ,AInterface,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclAnsiStrHashSet,TJclAnsiStrAbstractCollection,IJclAnsiStrCollection,IJclAnsiStrSet,IJclAnsiStrMap,IJclAnsiStrIterator, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrEqualityComparer\,,,
    { IJclStrContainer }
    function GetCaseSensitive: Boolean; override;
    procedure SetCaseSensitive(Value: Boolean); override;
    { IJclAnsiStrContainer }
    function GetEncoding: TJclAnsiStrEncoding; override;
    procedure SetEncoding(Value: TJclAnsiStrEncoding); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,, override;,
    constructor Create(ACapacity: Integer); overload;,const ,AString,AnsiString)*)

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclWideStrHashSet,TJclWideStrAbstractCollection,IJclWideStrCollection,IJclWideStrSet,IJclWideStrMap,IJclWideStrIterator, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrEqualityComparer\,,,
    { IJclStrContainer }
    function GetCaseSensitive: Boolean; override;
    procedure SetCaseSensitive(Value: Boolean); override;
    { IJclWideStrContainer }
    function GetEncoding: TJclWideStrEncoding; override;
    procedure SetEncoding(Value: TJclWideStrEncoding); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,, override;,
    constructor Create(ACapacity: Integer); overload;,const ,AString,WideString)*)

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrHashSet = TJclAnsiStrHashSet;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrHashSet = TJclWideStrHashSet;
  {$ENDIF CONTAINER_WIDESTR}

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclSingleHashSet,TJclSingleAbstractContainer,IJclSingleCollection,IJclSingleSet,IJclSingleMap,IJclSingleIterator, IJclSingleContainer\, IJclSingleEqualityComparer\,,,
    { IJclSingleContainer }
    function GetPrecision: Single; override;
    procedure SetPrecision(const Value: Single); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,
    constructor Create(ACapacity: Integer); overload;,const ,AValue,Single)*)

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclDoubleHashSet,TJclDoubleAbstractContainer,IJclDoubleCollection,IJclDoubleSet,IJclDoubleMap,IJclDoubleIterator, IJclDoubleContainer\, IJclDoubleEqualityComparer\,,,
    { IJclDoubleContainer }
    function GetPrecision: Double; override;
    procedure SetPrecision(const Value: Double); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,
    constructor Create(ACapacity: Integer); overload;,const ,AValue,Double)*)

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclExtendedHashSet,TJclExtendedAbstractContainer,IJclExtendedCollection,IJclExtendedSet,IJclExtendedMap,IJclExtendedIterator, IJclExtendedContainer\, IJclExtendedEqualityComparer\,,,
    { IJclExtendedContainer }
    function GetPrecision: Extended; override;
    procedure SetPrecision(const Value: Extended); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,
    constructor Create(ACapacity: Integer); overload;,const ,AValue,Extended)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatHashSet = TJclExtendedHashSet;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatHashSet = TJclDoubleHashSet;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatHashSet = TJclSingleHashSet;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclIntegerHashSet,TJclIntegerAbstractContainer,IJclIntegerCollection,IJclIntegerSet,IJclIntegerMap,IJclIntegerIterator, IJclIntegerEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,
    constructor Create(ACapacity: Integer); overload;,,AValue,Integer)*)

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclCardinalHashSet,TJclCardinalAbstractContainer,IJclCardinalCollection,IJclCardinalSet,IJclCardinalMap,IJclCardinalIterator, IJclCardinalEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,
    constructor Create(ACapacity: Integer); overload;,,AValue,Cardinal)*)

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclInt64HashSet,TJclInt64AbstractContainer,IJclInt64Collection,IJclInt64Set,IJclInt64Map,IJclInt64Iterator, IJclInt64EqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,
    constructor Create(ACapacity: Integer); overload;,const ,AValue,Int64)*)

  {$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLHASHSETINT(TJclPtrHashSet,TJclPtrAbstractContainer,IJclPtrCollection,IJclPtrSet,IJclPtrMap,IJclPtrIterator, IJclPtrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,
    constructor Create(ACapacity: Integer); overload;,,AValue,Pointer)*)
  {$ENDIF ~CLR}

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclHashSet,TJclAbstractContainer,IJclCollection,IJclSet,IJclMap,IJclIterator, IJclObjectOwner\, IJclEqualityComparer\,,,
    { IJclObjectOwner }
    function FreeObject(var AObject: TObject): TObject; override;
    function GetOwnsObjects: Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,
    constructor Create(ACapacity: Integer; AOwnsObjects: Boolean); overload;,,AObject,TObject)*)

  {$IFDEF SUPPORTS_GENERICS}

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclHashSet<T>,TJclAbstractContainer<T>,IJclCollection<T>,IJclSet<T>,IJclMap<T\, TRefUnique>,IJclIterator<T>, IJclItemOwner<T>\, IJclEqualityComparer<T>\,,,
    { IJclItemOwner<T> }
    function FreeItem(var AItem: T): T; override;
    function GetOwnsItems: Boolean; override;,,,,const ,AItem,T)*)

  // E = External helper to compare items for equality
  TJclHashSetE<T> = class(TJclHashSet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclCollection<T>, IJclSet<T>,
    IJclItemOwner<T>, IJclEqualityComparer<T>)
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
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; const AMap: IJclMap<T, TRefUnique>); overload;
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; const AComparer: IComparer<T>;
      ACapacity: Integer; AOwnsItems: Boolean); overload;

    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclHashSetF<T> = class(TJclHashSet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclCollection<T>, IJclSet<T>,
    IJclItemOwner<T>, IJclEqualityComparer<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const AMap: IJclMap<T, TRefUnique>); overload;
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const AHash: THashConvert<T>; const ACompare: TCompare<T>;
      ACapacity: Integer; AOwnsItems: Boolean); overload;
  end;

  // I = Items can compare themselves to an other
  TJclHashSetI<T: IEquatable<T>, IComparable<T>, IHashable> = class(TJclHashSet<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable,
    IJclContainer, IJclCollection<T>, IJclSet<T>, IJclItemOwner<T>, IJclEqualityComparer<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AMap: IJclMap<T, TRefUnique>); overload;
    constructor Create(ACapacity: Integer; AOwnsItems: Boolean); overload;
  end;
  {$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/prototypes/JclHashSets.pas $';
    Revision: '$Revision: 2376 $';
    Date: '$Date: 2008-06-05 15:35:37 +0200 (jeu., 05 juin 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

var
  GlobalRefUnique: TRefUnique = nil;

function RefUnique: TRefUnique;
begin
  // We keep the reference till program end. A unique memory address is not
  // possible under a garbage collector.
  if GlobalRefUnique = nil then
    GlobalRefUnique := TRefUnique.Create;
  Result := GlobalRefUnique;
end;

{$JPPDEFINEMACRO CONSTRUCTORADDITIONAL
constructor TJclIntfHashSet.Create(ACapacity: Integer);
begin
  Create(TJclIntfHashMap.Create(ACapacity, False));
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO GETTERADDITIONAL}
{$JPPDEFINEMACRO SETTERADDITIONAL}
{$JPPDEFINEMACRO FREEITEM}
{$JPPDEFINEMACRO GETOWNSITEMS}
(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclIntfHashSet,IJclIntfMap,IJclIntfCollection,IJclIntfIterator,,const ,AInterface,IInterface)*)
{$JPPUNDEFMACRO CONSTRUCTORADDITIONAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO GETTERADDITIONAL}
{$JPPUNDEFMACRO SETTERADDITIONAL}
{$JPPUNDEFMACRO FREEITEM}
{$JPPUNDEFMACRO GETOWNSITEMS}

{$JPPDEFINEMACRO CONSTRUCTORADDITIONAL
constructor TJclAnsiStrHashSet.Create(ACapacity: Integer);
begin
  Create(TJclAnsiStrHashMap.Create(ACapacity, False));
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO GETTERADDITIONAL
function TJclAnsiStrHashSet.GetCaseSensitive: Boolean;
begin
  Result := FMap.GetCaseSensitive;
end;

function TJclAnsiStrHashSet.GetEncoding: TJclAnsiStrEncoding;
begin
  Result := FMap.GetEncoding;
end;
}
{$JPPDEFINEMACRO SETTERADDITIONAL
procedure TJclAnsiStrHashSet.SetCaseSensitive(Value: Boolean);
begin
  FMap.SetCaseSensitive(Value);
end;

procedure TJclAnsiStrHashSet.SetEncoding(Value: TJclAnsiStrEncoding);
begin
  FMap.SetEncoding(Value);
end;
}
{$JPPDEFINEMACRO FREEITEM}
{$JPPDEFINEMACRO GETOWNSITEMS}
(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclAnsiStrHashSet,IJclAnsiStrMap,IJclAnsiStrCollection,IJclAnsiStrIterator,,const ,AString,AnsiString)*)
{$JPPUNDEFMACRO CONSTRUCTORADDITIONAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO GETTERADDITIONAL}
{$JPPUNDEFMACRO SETTERADDITIONAL}
{$JPPUNDEFMACRO FREEITEM}
{$JPPUNDEFMACRO GETOWNSITEMS}

{$JPPDEFINEMACRO CONSTRUCTORADDITIONAL
constructor TJclWideStrHashSet.Create(ACapacity: Integer);
begin
  Create(TJclWideStrHashMap.Create(ACapacity, False));
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO GETTERADDITIONAL
function TJclWideStrHashSet.GetCaseSensitive: Boolean;
begin
  Result := FMap.GetCaseSensitive;
end;

function TJclWideStrHashSet.GetEncoding: TJclWideStrEncoding;
begin
  Result := FMap.GetEncoding;
end;
}
{$JPPDEFINEMACRO SETTERADDITIONAL
procedure TJclWideStrHashSet.SetCaseSensitive(Value: Boolean);
begin
  FMap.SetCaseSensitive(Value);
end;

procedure TJclWideStrHashSet.SetEncoding(Value: TJclWideStrEncoding);
begin
  FMap.SetEncoding(Value);
end;
}
{$JPPDEFINEMACRO FREEITEM}
{$JPPDEFINEMACRO GETOWNSITEMS}
(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclWideStrHashSet,IJclWideStrMap,IJclWideStrCollection,IJclWideStrIterator,,const ,AString,WideString)*)
{$JPPUNDEFMACRO CONSTRUCTORADDITIONAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO GETTERADDITIONAL}
{$JPPUNDEFMACRO SETTERADDITIONAL}
{$JPPUNDEFMACRO FREEITEM}
{$JPPUNDEFMACRO GETOWNSITEMS}

{$JPPDEFINEMACRO CONSTRUCTORADDITIONAL
constructor TJclSingleHashSet.Create(ACapacity: Integer);
begin
  Create(TJclSingleHashMap.Create(ACapacity, False));
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclSingleHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO GETTERADDITIONAL
function TJclSingleHashSet.GetPrecision: Single;
begin
  Result := FMap.GetPrecision;
end;
}
{$JPPDEFINEMACRO SETTERADDITIONAL
procedure TJclSingleHashSet.SetPrecision(const Value: Single);
begin
  FMap.SetPrecision(Value);
end;
}
{$JPPDEFINEMACRO FREEITEM}
{$JPPDEFINEMACRO GETOWNSITEMS}
(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclSingleHashSet,IJclSingleMap,IJclSingleCollection,IJclSingleIterator,,const ,AValue,Single)*)
{$JPPUNDEFMACRO CONSTRUCTORADDITIONAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO GETTERADDITIONAL}
{$JPPUNDEFMACRO SETTERADDITIONAL}
{$JPPUNDEFMACRO FREEITEM}
{$JPPUNDEFMACRO GETOWNSITEMS}

{$JPPDEFINEMACRO CONSTRUCTORADDITIONAL
constructor TJclDoubleHashSet.Create(ACapacity: Integer);
begin
  Create(TJclDoubleHashMap.Create(ACapacity, False));
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclDoubleHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO GETTERADDITIONAL
function TJclDoubleHashSet.GetPrecision: Double;
begin
  Result := FMap.GetPrecision;
end;
}
{$JPPDEFINEMACRO SETTERADDITIONAL
procedure TJclDoubleHashSet.SetPrecision(const Value: Double);
begin
  FMap.SetPrecision(Value);
end;
}
{$JPPDEFINEMACRO FREEITEM}
{$JPPDEFINEMACRO GETOWNSITEMS}
(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclDoubleHashSet,IJclDoubleMap,IJclDoubleCollection,IJclDoubleIterator,,const ,AValue,Double)*)
{$JPPUNDEFMACRO CONSTRUCTORADDITIONAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO GETTERADDITIONAL}
{$JPPUNDEFMACRO SETTERADDITIONAL}
{$JPPUNDEFMACRO FREEITEM}
{$JPPUNDEFMACRO GETOWNSITEMS}

{$JPPDEFINEMACRO CONSTRUCTORADDITIONAL
constructor TJclExtendedHashSet.Create(ACapacity: Integer);
begin
  Create(TJclExtendedHashMap.Create(ACapacity, False));
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclExtendedHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO GETTERADDITIONAL
function TJclExtendedHashSet.GetPrecision: Extended;
begin
  Result := FMap.GetPrecision;
end;
}
{$JPPDEFINEMACRO SETTERADDITIONAL
procedure TJclExtendedHashSet.SetPrecision(const Value: Extended);
begin
  FMap.SetPrecision(Value);
end;
}
{$JPPDEFINEMACRO FREEITEM}
{$JPPDEFINEMACRO GETOWNSITEMS}
(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclExtendedHashSet,IJclExtendedMap,IJclExtendedCollection,IJclExtendedIterator,,const ,AValue,Extended)*)
{$JPPUNDEFMACRO CONSTRUCTORADDITIONAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO GETTERADDITIONAL}
{$JPPUNDEFMACRO SETTERADDITIONAL}
{$JPPUNDEFMACRO FREEITEM}
{$JPPUNDEFMACRO GETOWNSITEMS}

{$JPPDEFINEMACRO CONSTRUCTORADDITIONAL
constructor TJclIntegerHashSet.Create(ACapacity: Integer);
begin
  Create(TJclIntegerHashMap.Create(ACapacity, False));
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntegerHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO GETTERADDITIONAL}
{$JPPDEFINEMACRO SETTERADDITIONAL}
{$JPPDEFINEMACRO FREEITEM}
{$JPPDEFINEMACRO GETOWNSITEMS}
(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclIntegerHashSet,IJclIntegerMap,IJclIntegerCollection,IJclIntegerIterator,,,AValue,Integer)*)
{$JPPUNDEFMACRO CONSTRUCTORADDITIONAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO GETTERADDITIONAL}
{$JPPUNDEFMACRO SETTERADDITIONAL}
{$JPPUNDEFMACRO FREEITEM}
{$JPPUNDEFMACRO GETOWNSITEMS}

{$JPPDEFINEMACRO CONSTRUCTORADDITIONAL
constructor TJclCardinalHashSet.Create(ACapacity: Integer);
begin
  Create(TJclCardinalHashMap.Create(ACapacity, False));
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclCardinalHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO GETTERADDITIONAL}
{$JPPDEFINEMACRO SETTERADDITIONAL}
{$JPPDEFINEMACRO FREEITEM}
{$JPPDEFINEMACRO GETOWNSITEMS}
(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclCardinalHashSet,IJclCardinalMap,IJclCardinalCollection,IJclCardinalIterator,,,AValue,Cardinal)*)
{$JPPUNDEFMACRO CONSTRUCTORADDITIONAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO GETTERADDITIONAL}
{$JPPUNDEFMACRO SETTERADDITIONAL}
{$JPPUNDEFMACRO FREEITEM}
{$JPPUNDEFMACRO GETOWNSITEMS}

{$JPPDEFINEMACRO CONSTRUCTORADDITIONAL
constructor TJclInt64HashSet.Create(ACapacity: Integer);
begin
  Create(TJclInt64HashMap.Create(ACapacity, False));
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclInt64HashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64HashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO GETTERADDITIONAL}
{$JPPDEFINEMACRO SETTERADDITIONAL}
{$JPPDEFINEMACRO FREEITEM}
{$JPPDEFINEMACRO GETOWNSITEMS}
(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclInt64HashSet,IJclInt64Map,IJclInt64Collection,IJclInt64Iterator,,const ,AValue,Int64)*)
{$JPPUNDEFMACRO CONSTRUCTORADDITIONAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO GETTERADDITIONAL}
{$JPPUNDEFMACRO SETTERADDITIONAL}
{$JPPUNDEFMACRO FREEITEM}
{$JPPUNDEFMACRO GETOWNSITEMS}

{$IFNDEF CLR}
{$JPPDEFINEMACRO CONSTRUCTORADDITIONAL
constructor TJclPtrHashSet.Create(ACapacity: Integer);
begin
  Create(TJclPtrHashMap.Create(ACapacity, False));
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclPtrHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO GETTERADDITIONAL}
{$JPPDEFINEMACRO SETTERADDITIONAL}
{$JPPDEFINEMACRO FREEITEM}
{$JPPDEFINEMACRO GETOWNSITEMS}
(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclPtrHashSet,IJclPtrMap,IJclPtrCollection,IJclPtrIterator,,,AValue,Pointer)*)
{$JPPUNDEFMACRO CONSTRUCTORADDITIONAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO GETTERADDITIONAL}
{$JPPUNDEFMACRO SETTERADDITIONAL}
{$JPPUNDEFMACRO FREEITEM}
{$JPPUNDEFMACRO GETOWNSITEMS}
{$ENDIF ~CLR}

{$JPPDEFINEMACRO CONSTRUCTORADDITIONAL
constructor TJclHashSet.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  Create(TJclHashMap.Create(ACapacity, AOwnsObjects, False));
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashSet.Create(GetCapacity, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO GETTERADDITIONAL}
{$JPPDEFINEMACRO SETTERADDITIONAL}
{$JPPDEFINEMACRO FREEITEM
function TJclHashSet.FreeObject(var AObject: TObject): TObject;
begin
  Result := (FMap as IJclKeyOwner).FreeKey(AObject);
end;
}
{$JPPDEFINEMACRO GETOWNSITEMS
function TJclHashSet.GetOwnsObjects: Boolean;
begin
  Result := (FMap as IJclKeyOwner).GetOwnsKeys;
end;
}
(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclHashSet,IJclMap,IJclCollection,IJclIterator,False,,AObject,TObject)*)
{$JPPUNDEFMACRO CONSTRUCTORADDITIONAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO GETTERADDITIONAL}
{$JPPUNDEFMACRO SETTERADDITIONAL}
{$JPPUNDEFMACRO FREEITEM}
{$JPPUNDEFMACRO GETOWNSITEMS}

{$IFDEF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CONSTRUCTORADDITIONAL}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
{$JPPDEFINEMACRO GETTERADDITIONAL}
{$JPPDEFINEMACRO SETTERADDITIONAL}
{$JPPDEFINEMACRO FREEITEM
function TJclHashSet<T>.FreeItem(var AItem: T): T;
begin
  Result := (FMap as IJclPairOwner<T, TRefUnique>).FreeKey(AItem);
end;
}
{$JPPDEFINEMACRO GETOWNSITEMS
function TJclHashSet<T>.GetOwnsItems: Boolean;
begin
  Result := (FMap as IJclPairOwner<T, TRefUnique>).GetOwnsKeys;
end;
}
(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclHashSet<T>,IJclMap<T\, TRefUnique>,IJclCollection<T>,IJclIterator<T>,False,const ,AItem,T)*)
{$JPPUNDEFMACRO CONSTRUCTORADDITIONAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO ITEMSEQUAL(ItemA,ItemB)}
{$JPPUNDEFMACRO GETTERADDITIONAL}
{$JPPUNDEFMACRO SETTERADDITIONAL}
{$JPPUNDEFMACRO FREEITEM}
{$JPPUNDEFMACRO GETOWNSITEMS}

//=== { TJclHashSetE<T> } ====================================================

constructor TJclHashSetE<T>.Create(const AEqualityComparer: IEqualityComparer<T>; const AMap: IJclMap<T, TRefUnique>);
begin
  inherited Create(AMap);
  FEqualityComparer := AEqualityComparer;
end;

constructor TJclHashSetE<T>.Create(const AEqualityComparer: IEqualityComparer<T>; const AComparer: IComparer<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  Create(AEqualityComparer, TJclHashMapE<T, TRefUnique>.Create(AEqualityComparer, RefUnique, AComparer, ACapacity, AOwnsItems, False));
end;

procedure TJclHashSetE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclHashSetE<T> then
    TJclHashSetE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclHashSetE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
var
  AMap: IJclMap<T, TRefUnique>;
begin
  AMap := (FMap as IJclCloneable).Clone as IJclMap<T, TRefUnique>;
  AMap.Clear;
  Result := TJclHashSetE<T>.Create(FEqualityComparer, AMap);
  AssignPropertiesTo(Result);
end;

function TJclHashSetE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer <> nil then
    Result := EqualityComparer.Equals(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclHashSetF<T> } ====================================================

function EqualityCompareEqObjects(const Obj1, Obj2: TRefUnique): Boolean;
begin
  Result := Obj1 = Obj2;
end;

constructor TJclHashSetF<T>.Create(const AEqualityCompare: TEqualityCompare<T>; const AMap: IJclMap<T, TRefUnique>);
begin
  inherited Create(AMap);
  SetEqualityCompare(AEqualityCompare);
end;

constructor TJclHashSetF<T>.Create(const AEqualityCompare: TEqualityCompare<T>; const AHash: THashConvert<T>; const ACompare: TCompare<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  Create(AEqualityCompare, TJclHashMapF<T, TRefUnique>.Create(AEqualityCompare, AHash, EqualityCompareEqObjects, ACompare, ACapacity, AOwnsItems, False));
end;

function TJclHashSetF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
var
  AMap: IJclMap<T, TRefUnique>;
begin
  AMap := (FMap as IJclCloneable).Clone as IJclMap<T, TRefUnique>;
  AMap.Clear;
  Result := TJclHashSetF<T>.Create(FEqualityCompare, AMap);
  AssignPropertiesTo(Result);
end;

//=== { TJclHashSetI<T> } ====================================================

constructor TJclHashSetI<T>.Create(const AMap: IJclMap<T, TRefUnique>);
begin
  inherited Create(AMap);
end;

constructor TJclHashSetI<T>.Create(ACapacity: Integer; AOwnsItems: Boolean);
begin
  Create(TJclHashMapI<T, TRefUnique>.Create(ACapacity, AOwnsItems, False));
end;

function TJclHashSetI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
var
  AMap: IJclMap<T, TRefUnique>;
begin
  AMap := (FMap as IJclCloneable).Clone as IJclMap<T, TRefUnique>;
  AMap.Clear;
  Result := TJclHashSetI<T>.Create(AMap);
  AssignPropertiesTo(Result);
end;

function TJclHashSetI<T>.ItemsEqual(const A, B: T): Boolean;
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

