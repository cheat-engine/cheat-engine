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
{ The Original Code is LinkedList.pas.                                                             }
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
{ Last modified: $Date:: 2008-06-05 15:35:37 +0200 (jeu., 05 juin 2008)                        $ }
{ Revision:      $Rev:: 2376                                                                     $ }
{ Author:        $Author:: obones                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclLinkedLists;

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
  Classes,
  JclBase, JclAbstractContainers, JclContainerIntf, JclSynch;
{$I containers\JclContainerCommon.imp}
{$I containers\JclLinkedLists.imp}
{$I containers\JclLinkedLists.int}
type
(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclIntfLinkedListItem,TJclIntfLinkedList,TJclIntfAbstractContainer,IJclIntfCollection,IJclIntfList,IJclIntfIterator, IJclIntfEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const ,AInterface,IInterface,GetObject,SetObject)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclAnsiStrLinkedListItem,TJclAnsiStrLinkedList,TJclAnsiStrAbstractCollection,IJclAnsiStrCollection,IJclAnsiStrList,IJclAnsiStrIterator, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrFlatContainer\, IJclAnsiStrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,, override;,,const ,AString,AnsiString,GetString,SetString)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclWideStrLinkedListItem,TJclWideStrLinkedList,TJclWideStrAbstractCollection,IJclWideStrCollection,IJclWideStrList,IJclWideStrIterator, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrFlatContainer\, IJclWideStrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,, override;,,const ,AString,WideString,GetString,SetString)*)

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrLinkedList = TJclAnsiStrLinkedList;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrLinkedList = TJclWideStrLinkedList;
  {$ENDIF CONTAINER_WIDESTR}

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclSingleLinkedListItem,TJclSingleLinkedList,TJclSingleAbstractContainer,IJclSingleCollection,IJclSingleList,IJclSingleIterator, IJclSingleContainer\, IJclSingleEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const ,AValue,Single,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclDoubleLinkedListItem,TJclDoubleLinkedList,TJclDoubleAbstractContainer,IJclDoubleCollection,IJclDoubleList,IJclDoubleIterator, IJclDoubleContainer\, IJclDoubleEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const ,AValue,Double,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclExtendedLinkedListItem,TJclExtendedLinkedList,TJclExtendedAbstractContainer,IJclExtendedCollection,IJclExtendedList,IJclExtendedIterator, IJclExtendedContainer\, IJclExtendedEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const ,AValue,Extended,GetValue,SetValue)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatLinkedList = TJclExtendedLinkedList;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatLinkedList = TJclDoubleLinkedList;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatLinkedList = TJclSingleLinkedList;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclIntegerLinkedListItem,TJclIntegerLinkedList,TJclIntegerAbstractContainer,IJclIntegerCollection,IJclIntegerList,IJclIntegerIterator, IJclIntegerEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,,AValue,Integer,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclCardinalLinkedListItem,TJclCardinalLinkedList,TJclCardinalAbstractContainer,IJclCardinalCollection,IJclCardinalList,IJclCardinalIterator, IJclCardinalEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,,AValue,Cardinal,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclInt64LinkedListItem,TJclInt64LinkedList,TJclInt64AbstractContainer,IJclInt64Collection,IJclInt64List,IJclInt64Iterator, IJclInt64EqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const ,AValue,Int64,GetValue,SetValue)*)

{$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclPtrLinkedListItem,TJclPtrLinkedList,TJclPtrAbstractContainer,IJclPtrCollection,IJclPtrList,IJclPtrIterator, IJclPtrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,,APtr,Pointer,GetPointer,SetPointer)*)
{$ENDIF ~CLR}

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclLinkedListItem,TJclLinkedList,TJclAbstractContainer,IJclCollection,IJclList,IJclIterator, IJclObjectOwner\, IJclEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,; AOwnsObjects: Boolean,,AObject,TObject,GetObject,SetObject)*)

  {$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclLinkedListItem<T>,TJclLinkedList<T>,TJclAbstractContainer<T>,IJclCollection<T>,IJclList<T>,IJclIterator<T>, IJclItemOwner<T>\, IJclEqualityComparer<T>\,,,,,,; AOwnsItems: Boolean,const ,AItem,T,GetItem,SetItem)*)

  // E = External helper to compare items
  // GetHashCode is never called
  TJclLinkedListE<T> = class(TJclLinkedList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclCollection<T>, IJclList<T>, IJclEqualityComparer<T>,
    IJclItemOwner<T>)
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
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; const ACollection: IJclCollection<T>;
      AOwnsItems: Boolean);
    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclLinkedListF<T> = class(TJclLinkedList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclCollection<T>, IJclList<T>, IJclEqualityComparer<T>,
    IJclItemOwner<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const ACollection: IJclCollection<T>;
      AOwnsItems: Boolean);
  end;

  // I = Items can compare themselves to an other
  TJclLinkedListI<T: IEquatable<T>> = class(TJclLinkedList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclCollection<T>, IJclList<T>, IJclEqualityComparer<T>,
    IJclItemOwner<T>)
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
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/prototypes/JclLinkedLists.pas $';
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
(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TIntfItr,IJclIntfIterator,IJclIntfList,IJclIntfEqualityComparer,TJclIntfLinkedListItem,const ,AInterface,IInterface,nil,GetObject,SetObject)*)

{$JPPDEFINEMACRO ITEMFREE(Item)Item := nil}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TIntfItr,IJclIntfIterator,IJclIntfList,IJclIntfEqualityComparer,TJclIntfLinkedListItem,const ,AInterface,IInterface,nil,GetObject,SetObject)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

type
(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TAnsiStrItr,IJclAnsiStrIterator,IJclAnsiStrList,IJclAnsiStrEqualityComparer,TJclAnsiStrLinkedListItem,const ,AString,AnsiString,'',GetString,SetString)*)

{$JPPDEFINEMACRO ITEMFREE(Item)Item := ''}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TAnsiStrItr,IJclAnsiStrIterator,IJclAnsiStrList,IJclAnsiStrEqualityComparer,TJclAnsiStrLinkedListItem,const ,AString,AnsiString,'',GetString,SetString)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

type
(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TWideStrItr,IJclWideStrIterator,IJclWideStrList,IJclWideStrEqualityComparer,TJclWideStrLinkedListItem,const ,AString,WideString,'',GetString,SetString)*)

{$JPPDEFINEMACRO ITEMFREE(Item)Item := ''}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TWideStrItr,IJclWideStrIterator,IJclWideStrList,IJclWideStrEqualityComparer,TJclWideStrLinkedListItem,const ,AString,WideString,'',GetString,SetString)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

type
(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TSingleItr,IJclSingleIterator,IJclSingleList,IJclSingleEqualityComparer,TJclSingleLinkedListItem,const ,AValue,Single,0.0,GetValue,SetValue)*)

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0.0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TSingleItr,IJclSingleIterator,IJclSingleList,IJclSingleEqualityComparer,TJclSingleLinkedListItem,const ,AValue,Single,0.0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

type
(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TDoubleItr,IJclDoubleIterator,IJclDoubleList,IJclDoubleEqualityComparer,TJclDoubleLinkedListItem,const ,AValue,Double,0.0,GetValue,SetValue)*)

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0.0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TDoubleItr,IJclDoubleIterator,IJclDoubleList,IJclDoubleEqualityComparer,TJclDoubleLinkedListItem,const ,AValue,Double,0.0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

type
(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TExtendedItr,IJclExtendedIterator,IJclExtendedList,IJclExtendedEqualityComparer,TJclExtendedLinkedListItem,const ,AValue,Extended,0.0,GetValue,SetValue)*)

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0.0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TExtendedItr,IJclExtendedIterator,IJclExtendedList,IJclExtendedEqualityComparer,TJclExtendedLinkedListItem,const ,AValue,Extended,0.0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

type
(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TIntegerItr,IJclIntegerIterator,IJclIntegerList,IJclIntegerEqualityComparer,TJclIntegerLinkedListItem,,AValue,Integer,0,GetValue,SetValue)*)

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TIntegerItr,IJclIntegerIterator,IJclIntegerList,IJclIntegerEqualityComparer,TJclIntegerLinkedListItem,,AValue,Integer,0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

type
(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TCardinalItr,IJclCardinalIterator,IJclCardinalList,IJclCardinalEqualityComparer,TJclCardinalLinkedListItem,,AValue,Cardinal,0,GetValue,SetValue)*)

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TCardinalItr,IJclCardinalIterator,IJclCardinalList,IJclCardinalEqualityComparer,TJclCardinalLinkedListItem,,AValue,Cardinal,0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

type
(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TInt64Itr,IJclInt64Iterator,IJclInt64List,IJclInt64EqualityComparer,TJclInt64LinkedListItem,const ,AValue,Int64,0,GetValue,SetValue)*)

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TInt64Itr,IJclInt64Iterator,IJclInt64List,IJclInt64EqualityComparer,TJclInt64LinkedListItem,const ,AValue,Int64,0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$IFNDEF CLR}
type
(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TPtrItr,IJclPtrIterator,IJclPtrList,IJclPtrEqualityComparer,TJclPtrLinkedListItem,,AValue,Pointer,nil,GetPointer,SetPointer)*)

{$JPPDEFINEMACRO ITEMFREE(Item)Item := nil}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TPtrItr,IJclPtrIterator,IJclPtrList,IJclPtrEqualityComparer,TJclPtrLinkedListItem,,AValue,Pointer,nil,GetPointer,SetPointer)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}
{$ENDIF ~CLR}

type
(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TItr,IJclIterator,IJclList,IJclEqualityComparer,TJclLinkedListItem,,AObject,TObject,nil,GetObject,SetObject)*)

{$JPPDEFINEMACRO ITEMFREE(AObject)(FownList as IJclObjectOwner).FreeObject(AObject)}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TItr,IJclIterator,IJclList,IJclEqualityComparer,TJclLinkedListItem,,AObject,TObject,nil,GetObject,SetObject)*)
{$JPPUNDEFMACRO ITEMFREE(AObject)}

{$IFDEF SUPPORTS_GENERICS}
type
(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TItr<T>,IJclIterator<T>,IJclList<T>,IJclEqualityComparer<T>,TJclLinkedListItem<T>,const ,AItem,T,Default(T),GetItem,SetItem)*)

{$JPPDEFINEMACRO ITEMFREE(AItem)(FownList as IJclItemOwner<T>).FreeItem(AItem)}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TItr<T>,IJclIterator<T>,IJclList<T>,IJclEqualityComparer<T>,TJclLinkedListItem<T>,const ,AItem,T,Default(T),GetItem,SetItem)*)
{$JPPUNDEFMACRO ITEMFREE(AObject)}
{$ENDIF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclIntfLinkedList,TJclIntfLinkedListItem,IJclIntfCollection,IJclIntfList,IJclIntfIterator,TIntfItr,,,const ,AInterface,IInterface,nil,GetObject,SetObject,FreeObject)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclAnsiStrLinkedList,TJclAnsiStrLinkedListItem,IJclAnsiStrCollection,IJclAnsiStrList,IJclAnsiStrIterator,TAnsiStrItr,,,const ,AString,AnsiString,'',GetString,SetString,FreeString)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclWideStrLinkedList,TJclWideStrLinkedListItem,IJclWideStrCollection,IJclWideStrList,IJclWideStrIterator,TWideStrItr,,,const ,AString,WideString,'',GetString,SetString,FreeString)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclSingleLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclSingleLinkedList,TJclSingleLinkedListItem,IJclSingleCollection,IJclSingleList,IJclSingleIterator,TSingleItr,,,const ,AValue,Single,0.0,GetValue,SetValue,FreeSingle)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclDoubleLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclDoubleLinkedList,TJclDoubleLinkedListItem,IJclDoubleCollection,IJclDoubleList,IJclDoubleIterator,TDoubleItr,,,const ,AValue,Double,0.0,GetValue,SetValue,FreeDouble)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclExtendedLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclExtendedLinkedList,TJclExtendedLinkedListItem,IJclExtendedCollection,IJclExtendedList,IJclExtendedIterator,TExtendedItr,,,const ,AValue,Extended,0.0,GetValue,SetValue,FreeExtended)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntegerLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclIntegerLinkedList,TJclIntegerLinkedListItem,IJclIntegerCollection,IJclIntegerList,IJclIntegerIterator,TIntegerItr,,,,AValue,Integer,0,GetValue,SetValue,FreeInteger)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclCardinalLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclCardinalLinkedList,TJclCardinalLinkedListItem,IJclCardinalCollection,IJclCardinalList,IJclCardinalIterator,TCardinalItr,,,,AValue,Cardinal,0,GetValue,SetValue,FreeCardinal)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclInt64LinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64LinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclInt64LinkedList,TJclInt64LinkedListItem,IJclInt64Collection,IJclInt64List,IJclInt64Iterator,TInt64Itr,,,const ,AValue,Int64,0,GetValue,SetValue,FreeInt64)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFNDEF CLR}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclPtrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclPtrLinkedList,TJclPtrLinkedListItem,IJclPtrCollection,IJclPtrList,IJclPtrIterator,TPtrItr,,,,APtr,Pointer,nil,GetPointer,SetPointer,FreePointer)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$ENDIF ~CLR}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedList.Create(nil, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclLinkedList,TJclLinkedListItem,IJclCollection,IJclList,IJclIterator,TItr,; AOwnsObjects: Boolean,AOwnsObjects,,AObject,TObject,nil,GetObject,SetObject,FreeObject)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFDEF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclLinkedList<T>,TJclLinkedListItem<T>,IJclCollection<T>,IJclList<T>,IJclIterator<T>,TItr<T>,; AOwnsItems: Boolean,AOwnsItems,const ,AItem,T,Default(T),GetItem,SetItem,FreeItem)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

//=== { TJclLinkedListE<T> } =================================================

constructor TJclLinkedListE<T>.Create(const AEqualityComparer: IEqualityComparer<T>;
  const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

procedure TJclLinkedListE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclLinkedListE<T> then
    TJclLinkedListE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclLinkedListE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedListE<T>.Create(EqualityComparer, nil, False);
  AssignPropertiesTo(Result);
end;

function TJclLinkedListE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer <> nil then
    Result := EqualityComparer.Equals(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclLinkedListF<T> } =================================================

constructor TJclLinkedListF<T>.Create(const AEqualityCompare: TEqualityCompare<T>;
  const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  SetEqualityCompare(AEqualityCompare);
end;

function TJclLinkedListF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedListF<T>.Create(EqualityCompare, nil, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclLinkedListI<T> } =================================================

function TJclLinkedListI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedListI<T>.Create(nil, False);
  AssignPropertiesTo(Result);
end;

function TJclLinkedListI<T>.ItemsEqual(const A, B: T): Boolean;
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

