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
{ Last modified: $Date:: 2008-07-20 11:13:23 +0200 (dim., 20 juil. 2008)                       $ }
{ Revision:      $Rev:: 2393                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
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

type
  TJclIntfLinkedListItem = class
  public
    Value: IInterface;
    Next: TJclIntfLinkedListItem;
    Previous: TJclIntfLinkedListItem;
  end;

  TJclIntfLinkedList = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclIntfEqualityComparer,
    IJclIntfCollection, IJclIntfList)
  private
    FStart: TJclIntfLinkedListItem;
    FEnd: TJclIntfLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclIntfCollection }
    function Add(const AInterface: IInterface): Boolean;
    function AddAll(const ACollection: IJclIntfCollection): Boolean;
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function ContainsAll(const ACollection: IJclIntfCollection): Boolean;
    function Equals(const ACollection: IJclIntfCollection): Boolean;
    function First: IJclIntfIterator;
    function IsEmpty: Boolean;
    function Last: IJclIntfIterator;
    function Remove(const AInterface: IInterface): Boolean;
    function RemoveAll(const ACollection: IJclIntfCollection): Boolean;
    function RetainAll(const ACollection: IJclIntfCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIntfIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntfList }
    function Insert(Index: Integer; const AInterface: IInterface): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean;
    function GetObject(Index: Integer): IInterface;
    function IndexOf(const AInterface: IInterface): Integer;
    function LastIndexOf(const AInterface: IInterface): Integer;
    function Delete(Index: Integer): IInterface; overload;
    procedure SetObject(Index: Integer; const AInterface: IInterface);
    function SubList(First, Count: Integer): IJclIntfList;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACollection: IJclIntfCollection);
    destructor Destroy; override;
  end;

  TJclAnsiStrLinkedListItem = class
  public
    Value: AnsiString;
    Next: TJclAnsiStrLinkedListItem;
    Previous: TJclAnsiStrLinkedListItem;
  end;

  TJclAnsiStrLinkedList = class(TJclAnsiStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer, IJclAnsiStrFlatContainer, IJclAnsiStrEqualityComparer,
    IJclAnsiStrCollection, IJclAnsiStrList)
  private
    FStart: TJclAnsiStrLinkedListItem;
    FEnd: TJclAnsiStrLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclAnsiStrCollection }
    function Add(const AString: AnsiString): Boolean; override;
    function AddAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    procedure Clear; override;
    function Contains(const AString: AnsiString): Boolean; override;
    function ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Equals(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function First: IJclAnsiStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclAnsiStrIterator; override;
    function Remove(const AString: AnsiString): Boolean; override;
    function RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclAnsiStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclAnsiStrList }
    function Insert(Index: Integer; const AString: AnsiString): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclAnsiStrCollection): Boolean;
    function GetString(Index: Integer): AnsiString;
    function IndexOf(const AString: AnsiString): Integer;
    function LastIndexOf(const AString: AnsiString): Integer;
    function Delete(Index: Integer): AnsiString; overload;
    procedure SetString(Index: Integer; const AString: AnsiString);
    function SubList(First, Count: Integer): IJclAnsiStrList;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACollection: IJclAnsiStrCollection);
    destructor Destroy; override;
  end;

  TJclWideStrLinkedListItem = class
  public
    Value: WideString;
    Next: TJclWideStrLinkedListItem;
    Previous: TJclWideStrLinkedListItem;
  end;

  TJclWideStrLinkedList = class(TJclWideStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclStrContainer, IJclWideStrContainer, IJclWideStrFlatContainer, IJclWideStrEqualityComparer,
    IJclWideStrCollection, IJclWideStrList)
  private
    FStart: TJclWideStrLinkedListItem;
    FEnd: TJclWideStrLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclWideStrCollection }
    function Add(const AString: WideString): Boolean; override;
    function AddAll(const ACollection: IJclWideStrCollection): Boolean; override;
    procedure Clear; override;
    function Contains(const AString: WideString): Boolean; override;
    function ContainsAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Equals(const ACollection: IJclWideStrCollection): Boolean; override;
    function First: IJclWideStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclWideStrIterator; override;
    function Remove(const AString: WideString): Boolean; override;
    function RemoveAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclWideStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclWideStrList }
    function Insert(Index: Integer; const AString: WideString): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclWideStrCollection): Boolean;
    function GetString(Index: Integer): WideString;
    function IndexOf(const AString: WideString): Integer;
    function LastIndexOf(const AString: WideString): Integer;
    function Delete(Index: Integer): WideString; overload;
    procedure SetString(Index: Integer; const AString: WideString);
    function SubList(First, Count: Integer): IJclWideStrList;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACollection: IJclWideStrCollection);
    destructor Destroy; override;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrLinkedList = TJclAnsiStrLinkedList;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrLinkedList = TJclWideStrLinkedList;
  {$ENDIF CONTAINER_WIDESTR}

  TJclSingleLinkedListItem = class
  public
    Value: Single;
    Next: TJclSingleLinkedListItem;
    Previous: TJclSingleLinkedListItem;
  end;

  TJclSingleLinkedList = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclSingleContainer, IJclSingleEqualityComparer,
    IJclSingleCollection, IJclSingleList)
  private
    FStart: TJclSingleLinkedListItem;
    FEnd: TJclSingleLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclSingleCollection }
    function Add(const AValue: Single): Boolean;
    function AddAll(const ACollection: IJclSingleCollection): Boolean;
    procedure Clear;
    function Contains(const AValue: Single): Boolean;
    function ContainsAll(const ACollection: IJclSingleCollection): Boolean;
    function Equals(const ACollection: IJclSingleCollection): Boolean;
    function First: IJclSingleIterator;
    function IsEmpty: Boolean;
    function Last: IJclSingleIterator;
    function Remove(const AValue: Single): Boolean;
    function RemoveAll(const ACollection: IJclSingleCollection): Boolean;
    function RetainAll(const ACollection: IJclSingleCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclSingleIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclSingleList }
    function Insert(Index: Integer; const AValue: Single): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclSingleCollection): Boolean;
    function GetValue(Index: Integer): Single;
    function IndexOf(const AValue: Single): Integer;
    function LastIndexOf(const AValue: Single): Integer;
    function Delete(Index: Integer): Single; overload;
    procedure SetValue(Index: Integer; const AValue: Single);
    function SubList(First, Count: Integer): IJclSingleList;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACollection: IJclSingleCollection);
    destructor Destroy; override;
  end;

  TJclDoubleLinkedListItem = class
  public
    Value: Double;
    Next: TJclDoubleLinkedListItem;
    Previous: TJclDoubleLinkedListItem;
  end;

  TJclDoubleLinkedList = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclDoubleContainer, IJclDoubleEqualityComparer,
    IJclDoubleCollection, IJclDoubleList)
  private
    FStart: TJclDoubleLinkedListItem;
    FEnd: TJclDoubleLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclDoubleCollection }
    function Add(const AValue: Double): Boolean;
    function AddAll(const ACollection: IJclDoubleCollection): Boolean;
    procedure Clear;
    function Contains(const AValue: Double): Boolean;
    function ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
    function Equals(const ACollection: IJclDoubleCollection): Boolean;
    function First: IJclDoubleIterator;
    function IsEmpty: Boolean;
    function Last: IJclDoubleIterator;
    function Remove(const AValue: Double): Boolean;
    function RemoveAll(const ACollection: IJclDoubleCollection): Boolean;
    function RetainAll(const ACollection: IJclDoubleCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclDoubleIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclDoubleList }
    function Insert(Index: Integer; const AValue: Double): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclDoubleCollection): Boolean;
    function GetValue(Index: Integer): Double;
    function IndexOf(const AValue: Double): Integer;
    function LastIndexOf(const AValue: Double): Integer;
    function Delete(Index: Integer): Double; overload;
    procedure SetValue(Index: Integer; const AValue: Double);
    function SubList(First, Count: Integer): IJclDoubleList;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACollection: IJclDoubleCollection);
    destructor Destroy; override;
  end;

  TJclExtendedLinkedListItem = class
  public
    Value: Extended;
    Next: TJclExtendedLinkedListItem;
    Previous: TJclExtendedLinkedListItem;
  end;

  TJclExtendedLinkedList = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclExtendedContainer, IJclExtendedEqualityComparer,
    IJclExtendedCollection, IJclExtendedList)
  private
    FStart: TJclExtendedLinkedListItem;
    FEnd: TJclExtendedLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclExtendedCollection }
    function Add(const AValue: Extended): Boolean;
    function AddAll(const ACollection: IJclExtendedCollection): Boolean;
    procedure Clear;
    function Contains(const AValue: Extended): Boolean;
    function ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
    function Equals(const ACollection: IJclExtendedCollection): Boolean;
    function First: IJclExtendedIterator;
    function IsEmpty: Boolean;
    function Last: IJclExtendedIterator;
    function Remove(const AValue: Extended): Boolean;
    function RemoveAll(const ACollection: IJclExtendedCollection): Boolean;
    function RetainAll(const ACollection: IJclExtendedCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclExtendedIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclExtendedList }
    function Insert(Index: Integer; const AValue: Extended): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclExtendedCollection): Boolean;
    function GetValue(Index: Integer): Extended;
    function IndexOf(const AValue: Extended): Integer;
    function LastIndexOf(const AValue: Extended): Integer;
    function Delete(Index: Integer): Extended; overload;
    procedure SetValue(Index: Integer; const AValue: Extended);
    function SubList(First, Count: Integer): IJclExtendedList;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACollection: IJclExtendedCollection);
    destructor Destroy; override;
  end;

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatLinkedList = TJclExtendedLinkedList;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatLinkedList = TJclDoubleLinkedList;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatLinkedList = TJclSingleLinkedList;
  {$ENDIF MATH_SINGLE_PRECISION}

  TJclIntegerLinkedListItem = class
  public
    Value: Integer;
    Next: TJclIntegerLinkedListItem;
    Previous: TJclIntegerLinkedListItem;
  end;

  TJclIntegerLinkedList = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclIntegerEqualityComparer,
    IJclIntegerCollection, IJclIntegerList)
  private
    FStart: TJclIntegerLinkedListItem;
    FEnd: TJclIntegerLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclIntegerCollection }
    function Add(AValue: Integer): Boolean;
    function AddAll(const ACollection: IJclIntegerCollection): Boolean;
    procedure Clear;
    function Contains(AValue: Integer): Boolean;
    function ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
    function Equals(const ACollection: IJclIntegerCollection): Boolean;
    function First: IJclIntegerIterator;
    function IsEmpty: Boolean;
    function Last: IJclIntegerIterator;
    function Remove(AValue: Integer): Boolean;
    function RemoveAll(const ACollection: IJclIntegerCollection): Boolean;
    function RetainAll(const ACollection: IJclIntegerCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIntegerIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntegerList }
    function Insert(Index: Integer; AValue: Integer): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclIntegerCollection): Boolean;
    function GetValue(Index: Integer): Integer;
    function IndexOf(AValue: Integer): Integer;
    function LastIndexOf(AValue: Integer): Integer;
    function Delete(Index: Integer): Integer; overload;
    procedure SetValue(Index: Integer; AValue: Integer);
    function SubList(First, Count: Integer): IJclIntegerList;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACollection: IJclIntegerCollection);
    destructor Destroy; override;
  end;

  TJclCardinalLinkedListItem = class
  public
    Value: Cardinal;
    Next: TJclCardinalLinkedListItem;
    Previous: TJclCardinalLinkedListItem;
  end;

  TJclCardinalLinkedList = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclCardinalEqualityComparer,
    IJclCardinalCollection, IJclCardinalList)
  private
    FStart: TJclCardinalLinkedListItem;
    FEnd: TJclCardinalLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclCardinalCollection }
    function Add(AValue: Cardinal): Boolean;
    function AddAll(const ACollection: IJclCardinalCollection): Boolean;
    procedure Clear;
    function Contains(AValue: Cardinal): Boolean;
    function ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
    function Equals(const ACollection: IJclCardinalCollection): Boolean;
    function First: IJclCardinalIterator;
    function IsEmpty: Boolean;
    function Last: IJclCardinalIterator;
    function Remove(AValue: Cardinal): Boolean;
    function RemoveAll(const ACollection: IJclCardinalCollection): Boolean;
    function RetainAll(const ACollection: IJclCardinalCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclCardinalIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCardinalList }
    function Insert(Index: Integer; AValue: Cardinal): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclCardinalCollection): Boolean;
    function GetValue(Index: Integer): Cardinal;
    function IndexOf(AValue: Cardinal): Integer;
    function LastIndexOf(AValue: Cardinal): Integer;
    function Delete(Index: Integer): Cardinal; overload;
    procedure SetValue(Index: Integer; AValue: Cardinal);
    function SubList(First, Count: Integer): IJclCardinalList;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACollection: IJclCardinalCollection);
    destructor Destroy; override;
  end;

  TJclInt64LinkedListItem = class
  public
    Value: Int64;
    Next: TJclInt64LinkedListItem;
    Previous: TJclInt64LinkedListItem;
  end;

  TJclInt64LinkedList = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclInt64EqualityComparer,
    IJclInt64Collection, IJclInt64List)
  private
    FStart: TJclInt64LinkedListItem;
    FEnd: TJclInt64LinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclInt64Collection }
    function Add(const AValue: Int64): Boolean;
    function AddAll(const ACollection: IJclInt64Collection): Boolean;
    procedure Clear;
    function Contains(const AValue: Int64): Boolean;
    function ContainsAll(const ACollection: IJclInt64Collection): Boolean;
    function Equals(const ACollection: IJclInt64Collection): Boolean;
    function First: IJclInt64Iterator;
    function IsEmpty: Boolean;
    function Last: IJclInt64Iterator;
    function Remove(const AValue: Int64): Boolean;
    function RemoveAll(const ACollection: IJclInt64Collection): Boolean;
    function RetainAll(const ACollection: IJclInt64Collection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclInt64Iterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclInt64List }
    function Insert(Index: Integer; const AValue: Int64): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclInt64Collection): Boolean;
    function GetValue(Index: Integer): Int64;
    function IndexOf(const AValue: Int64): Integer;
    function LastIndexOf(const AValue: Int64): Integer;
    function Delete(Index: Integer): Int64; overload;
    procedure SetValue(Index: Integer; const AValue: Int64);
    function SubList(First, Count: Integer): IJclInt64List;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACollection: IJclInt64Collection);
    destructor Destroy; override;
  end;

{$IFNDEF CLR}
  TJclPtrLinkedListItem = class
  public
    Value: Pointer;
    Next: TJclPtrLinkedListItem;
    Previous: TJclPtrLinkedListItem;
  end;

  TJclPtrLinkedList = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclPtrEqualityComparer,
    IJclPtrCollection, IJclPtrList)
  private
    FStart: TJclPtrLinkedListItem;
    FEnd: TJclPtrLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPtrCollection }
    function Add(APtr: Pointer): Boolean;
    function AddAll(const ACollection: IJclPtrCollection): Boolean;
    procedure Clear;
    function Contains(APtr: Pointer): Boolean;
    function ContainsAll(const ACollection: IJclPtrCollection): Boolean;
    function Equals(const ACollection: IJclPtrCollection): Boolean;
    function First: IJclPtrIterator;
    function IsEmpty: Boolean;
    function Last: IJclPtrIterator;
    function Remove(APtr: Pointer): Boolean;
    function RemoveAll(const ACollection: IJclPtrCollection): Boolean;
    function RetainAll(const ACollection: IJclPtrCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclPtrIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclPtrList }
    function Insert(Index: Integer; APtr: Pointer): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclPtrCollection): Boolean;
    function GetPointer(Index: Integer): Pointer;
    function IndexOf(APtr: Pointer): Integer;
    function LastIndexOf(APtr: Pointer): Integer;
    function Delete(Index: Integer): Pointer; overload;
    procedure SetPointer(Index: Integer; APtr: Pointer);
    function SubList(First, Count: Integer): IJclPtrList;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACollection: IJclPtrCollection);
    destructor Destroy; override;
  end;
{$ENDIF ~CLR}

  TJclLinkedListItem = class
  public
    Value: TObject;
    Next: TJclLinkedListItem;
    Previous: TJclLinkedListItem;
  end;

  TJclLinkedList = class(TJclAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclObjectOwner, IJclEqualityComparer,
    IJclCollection, IJclList)
  private
    FStart: TJclLinkedListItem;
    FEnd: TJclLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclCollection }
    function Add(AObject: TObject): Boolean;
    function AddAll(const ACollection: IJclCollection): Boolean;
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function ContainsAll(const ACollection: IJclCollection): Boolean;
    function Equals(const ACollection: IJclCollection): Boolean;
    function First: IJclIterator;
    function IsEmpty: Boolean;
    function Last: IJclIterator;
    function Remove(AObject: TObject): Boolean;
    function RemoveAll(const ACollection: IJclCollection): Boolean;
    function RetainAll(const ACollection: IJclCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclList }
    function Insert(Index: Integer; AObject: TObject): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclCollection): Boolean;
    function GetObject(Index: Integer): TObject;
    function IndexOf(AObject: TObject): Integer;
    function LastIndexOf(AObject: TObject): Integer;
    function Delete(Index: Integer): TObject; overload;
    procedure SetObject(Index: Integer; AObject: TObject);
    function SubList(First, Count: Integer): IJclList;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACollection: IJclCollection; AOwnsObjects: Boolean);
    destructor Destroy; override;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  TJclLinkedListItem<T> = class
  public
    Value: T;
    Next: TJclLinkedListItem<T>;
    Previous: TJclLinkedListItem<T>;
  end;

  TJclLinkedList<T> = class(TJclAbstractContainer<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclList<T>)
  private
    FStart: TJclLinkedListItem<T>;
    FEnd: TJclLinkedListItem<T>;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclCollection<T> }
    function Add(const AItem: T): Boolean;
    function AddAll(const ACollection: IJclCollection<T>): Boolean;
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function ContainsAll(const ACollection: IJclCollection<T>): Boolean;
    function Equals(const ACollection: IJclCollection<T>): Boolean;
    function First: IJclIterator<T>;
    function IsEmpty: Boolean;
    function Last: IJclIterator<T>;
    function Remove(const AItem: T): Boolean;
    function RemoveAll(const ACollection: IJclCollection<T>): Boolean;
    function RetainAll(const ACollection: IJclCollection<T>): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIterator<T>;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclList<T> }
    function Insert(Index: Integer; const AItem: T): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean;
    function GetItem(Index: Integer): T;
    function IndexOf(const AItem: T): Integer;
    function LastIndexOf(const AItem: T): Integer;
    function Delete(Index: Integer): T; overload;
    procedure SetItem(Index: Integer; const AItem: T);
    function SubList(First, Count: Integer): IJclList<T>;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
    destructor Destroy; override;
  end;

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
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclLinkedLists.pas $';
    Revision: '$Revision: 2393 $';
    Date: '$Date: 2008-07-20 11:13:23 +0200 (dim., 20 juil. 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils;

type
  TItrStart = (isFirst, isLast);

type
  TIntfItr = class(TJclAbstractIterator, IJclIntfIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclIntfLinkedListItem;
    FStart: TItrStart;
    FOwnList: IJclIntfList;
    FEqualityComparer: IJclIntfEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclIntfIterator }
    function Add(const AInterface: IInterface): Boolean;
    function Equals(const AIterator: IJclIntfIterator): Boolean;
    function GetObject: IInterface;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AInterface: IInterface): Boolean;
    function Next: IInterface;
    function NextIndex: Integer;
    function Previous: IInterface;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetObject(const AInterface: IInterface);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: IInterface read GetObject;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AOwnList: IJclIntfList; ACursor: TJclIntfLinkedListItem; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TIntfItr } ============================================================

constructor TIntfItr.Create(const AOwnList: IJclIntfList; ACursor: TJclIntfLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclIntfEqualityComparer;
end;

function TIntfItr.Add(const AInterface: IInterface): Boolean;
begin
  Result := FOwnList.Add(AInterface);
end;

procedure TIntfItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TIntfItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TIntfItr then
  begin
    ADest := TIntfItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TIntfItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TIntfItr.Create(FOwnList, FCursor, Valid, FStart);
end;

function TIntfItr.Equals(const AIterator: IJclIntfIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TIntfItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TIntfItr then
  begin
    ItrObj := TIntfItr(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TIntfItr.GetObject: IInterface;
begin
  CheckValid;
  Result := nil;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TIntfItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.Insert(const AInterface: IInterface): Boolean;
var
  NewCursor: TJclIntfLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AInterface, nil);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AInterface);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AInterface);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclIntfLinkedListItem.Create;
          NewCursor.Value := AInterface;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TIntfItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TIntfItr.Next: IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TIntfItr.Previous: IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TIntfItr.Remove;
var
  OldCursor: TJclIntfLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := nil;
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntfItr.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntfItr.SetObject(const AInterface: IInterface);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := nil;
    FCursor.Value := AInterface;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

type
  TAnsiStrItr = class(TJclAbstractIterator, IJclAnsiStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclAnsiStrLinkedListItem;
    FStart: TItrStart;
    FOwnList: IJclAnsiStrList;
    FEqualityComparer: IJclAnsiStrEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclAnsiStrIterator }
    function Add(const AString: AnsiString): Boolean;
    function Equals(const AIterator: IJclAnsiStrIterator): Boolean;
    function GetString: AnsiString;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AString: AnsiString): Boolean;
    function Next: AnsiString;
    function NextIndex: Integer;
    function Previous: AnsiString;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetString(const AString: AnsiString);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: AnsiString read GetString;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AOwnList: IJclAnsiStrList; ACursor: TJclAnsiStrLinkedListItem; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TAnsiStrItr } ============================================================

constructor TAnsiStrItr.Create(const AOwnList: IJclAnsiStrList; ACursor: TJclAnsiStrLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclAnsiStrEqualityComparer;
end;

function TAnsiStrItr.Add(const AString: AnsiString): Boolean;
begin
  Result := FOwnList.Add(AString);
end;

procedure TAnsiStrItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TAnsiStrItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TAnsiStrItr then
  begin
    ADest := TAnsiStrItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TAnsiStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TAnsiStrItr.Create(FOwnList, FCursor, Valid, FStart);
end;

function TAnsiStrItr.Equals(const AIterator: IJclAnsiStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TAnsiStrItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TAnsiStrItr then
  begin
    ItrObj := TAnsiStrItr(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TAnsiStrItr.GetString: AnsiString;
begin
  CheckValid;
  Result := '';
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TAnsiStrItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.Insert(const AString: AnsiString): Boolean;
var
  NewCursor: TJclAnsiStrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, '');
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AString);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AString);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclAnsiStrLinkedListItem.Create;
          NewCursor.Value := AString;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TAnsiStrItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TAnsiStrItr.Next: AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TAnsiStrItr.Previous: AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TAnsiStrItr.Remove;
var
  OldCursor: TJclAnsiStrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := '';
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TAnsiStrItr.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TAnsiStrItr.SetString(const AString: AnsiString);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := '';
    FCursor.Value := AString;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

type
  TWideStrItr = class(TJclAbstractIterator, IJclWideStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclWideStrLinkedListItem;
    FStart: TItrStart;
    FOwnList: IJclWideStrList;
    FEqualityComparer: IJclWideStrEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclWideStrIterator }
    function Add(const AString: WideString): Boolean;
    function Equals(const AIterator: IJclWideStrIterator): Boolean;
    function GetString: WideString;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AString: WideString): Boolean;
    function Next: WideString;
    function NextIndex: Integer;
    function Previous: WideString;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetString(const AString: WideString);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: WideString read GetString;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AOwnList: IJclWideStrList; ACursor: TJclWideStrLinkedListItem; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TWideStrItr } ============================================================

constructor TWideStrItr.Create(const AOwnList: IJclWideStrList; ACursor: TJclWideStrLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclWideStrEqualityComparer;
end;

function TWideStrItr.Add(const AString: WideString): Boolean;
begin
  Result := FOwnList.Add(AString);
end;

procedure TWideStrItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TWideStrItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TWideStrItr then
  begin
    ADest := TWideStrItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TWideStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TWideStrItr.Create(FOwnList, FCursor, Valid, FStart);
end;

function TWideStrItr.Equals(const AIterator: IJclWideStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TWideStrItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TWideStrItr then
  begin
    ItrObj := TWideStrItr(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TWideStrItr.GetString: WideString;
begin
  CheckValid;
  Result := '';
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TWideStrItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.Insert(const AString: WideString): Boolean;
var
  NewCursor: TJclWideStrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, '');
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AString);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AString);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclWideStrLinkedListItem.Create;
          NewCursor.Value := AString;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TWideStrItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TWideStrItr.Next: WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TWideStrItr.Previous: WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TWideStrItr.Remove;
var
  OldCursor: TJclWideStrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := '';
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TWideStrItr.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TWideStrItr.SetString(const AString: WideString);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := '';
    FCursor.Value := AString;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

type
  TSingleItr = class(TJclAbstractIterator, IJclSingleIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclSingleLinkedListItem;
    FStart: TItrStart;
    FOwnList: IJclSingleList;
    FEqualityComparer: IJclSingleEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclSingleIterator }
    function Add(const AValue: Single): Boolean;
    function Equals(const AIterator: IJclSingleIterator): Boolean;
    function GetValue: Single;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Single): Boolean;
    function Next: Single;
    function NextIndex: Integer;
    function Previous: Single;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(const AValue: Single);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Single read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AOwnList: IJclSingleList; ACursor: TJclSingleLinkedListItem; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TSingleItr } ============================================================

constructor TSingleItr.Create(const AOwnList: IJclSingleList; ACursor: TJclSingleLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclSingleEqualityComparer;
end;

function TSingleItr.Add(const AValue: Single): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TSingleItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TSingleItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TSingleItr then
  begin
    ADest := TSingleItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TSingleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TSingleItr.Create(FOwnList, FCursor, Valid, FStart);
end;

function TSingleItr.Equals(const AIterator: IJclSingleIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TSingleItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TSingleItr then
  begin
    ItrObj := TSingleItr(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TSingleItr.GetValue: Single;
begin
  CheckValid;
  Result := 0.0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TSingleItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.Insert(const AValue: Single): Boolean;
var
  NewCursor: TJclSingleLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AValue);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AValue);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclSingleLinkedListItem.Create;
          NewCursor.Value := AValue;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TSingleItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TSingleItr.Next: Single;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TSingleItr.Previous: Single;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TSingleItr.Remove;
var
  OldCursor: TJclSingleLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := 0.0;
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TSingleItr.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TSingleItr.SetValue(const AValue: Single);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := 0.0;
    FCursor.Value := AValue;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;


type
  TDoubleItr = class(TJclAbstractIterator, IJclDoubleIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclDoubleLinkedListItem;
    FStart: TItrStart;
    FOwnList: IJclDoubleList;
    FEqualityComparer: IJclDoubleEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclDoubleIterator }
    function Add(const AValue: Double): Boolean;
    function Equals(const AIterator: IJclDoubleIterator): Boolean;
    function GetValue: Double;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Double): Boolean;
    function Next: Double;
    function NextIndex: Integer;
    function Previous: Double;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(const AValue: Double);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Double read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AOwnList: IJclDoubleList; ACursor: TJclDoubleLinkedListItem; AValid: Boolean; AStart: TItrStart);
  end;


//=== { TDoubleItr } ============================================================

constructor TDoubleItr.Create(const AOwnList: IJclDoubleList; ACursor: TJclDoubleLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclDoubleEqualityComparer;
end;

function TDoubleItr.Add(const AValue: Double): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TDoubleItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TDoubleItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TDoubleItr then
  begin
    ADest := TDoubleItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TDoubleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TDoubleItr.Create(FOwnList, FCursor, Valid, FStart);
end;

function TDoubleItr.Equals(const AIterator: IJclDoubleIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TDoubleItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TDoubleItr then
  begin
    ItrObj := TDoubleItr(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TDoubleItr.GetValue: Double;
begin
  CheckValid;
  Result := 0.0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TDoubleItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.Insert(const AValue: Double): Boolean;
var
  NewCursor: TJclDoubleLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AValue);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AValue);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclDoubleLinkedListItem.Create;
          NewCursor.Value := AValue;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TDoubleItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TDoubleItr.Next: Double;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TDoubleItr.Previous: Double;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TDoubleItr.Remove;
var
  OldCursor: TJclDoubleLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := 0.0;
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TDoubleItr.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TDoubleItr.SetValue(const AValue: Double);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := 0.0;
    FCursor.Value := AValue;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

type
  TExtendedItr = class(TJclAbstractIterator, IJclExtendedIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclExtendedLinkedListItem;
    FStart: TItrStart;
    FOwnList: IJclExtendedList;
    FEqualityComparer: IJclExtendedEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclExtendedIterator }
    function Add(const AValue: Extended): Boolean;
    function Equals(const AIterator: IJclExtendedIterator): Boolean;
    function GetValue: Extended;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Extended): Boolean;
    function Next: Extended;
    function NextIndex: Integer;
    function Previous: Extended;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(const AValue: Extended);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Extended read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AOwnList: IJclExtendedList; ACursor: TJclExtendedLinkedListItem; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TExtendedItr } ============================================================

constructor TExtendedItr.Create(const AOwnList: IJclExtendedList; ACursor: TJclExtendedLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclExtendedEqualityComparer;
end;

function TExtendedItr.Add(const AValue: Extended): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TExtendedItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TExtendedItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TExtendedItr then
  begin
    ADest := TExtendedItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TExtendedItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TExtendedItr.Create(FOwnList, FCursor, Valid, FStart);
end;

function TExtendedItr.Equals(const AIterator: IJclExtendedIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TExtendedItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TExtendedItr then
  begin
    ItrObj := TExtendedItr(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TExtendedItr.GetValue: Extended;
begin
  CheckValid;
  Result := 0.0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TExtendedItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.Insert(const AValue: Extended): Boolean;
var
  NewCursor: TJclExtendedLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AValue);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AValue);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclExtendedLinkedListItem.Create;
          NewCursor.Value := AValue;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TExtendedItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TExtendedItr.Next: Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TExtendedItr.Previous: Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TExtendedItr.Remove;
var
  OldCursor: TJclExtendedLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := 0.0;
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TExtendedItr.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TExtendedItr.SetValue(const AValue: Extended);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := 0.0;
    FCursor.Value := AValue;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

type
  TIntegerItr = class(TJclAbstractIterator, IJclIntegerIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclIntegerLinkedListItem;
    FStart: TItrStart;
    FOwnList: IJclIntegerList;
    FEqualityComparer: IJclIntegerEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclIntegerIterator }
    function Add(AValue: Integer): Boolean;
    function Equals(const AIterator: IJclIntegerIterator): Boolean;
    function GetValue: Integer;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AValue: Integer): Boolean;
    function Next: Integer;
    function NextIndex: Integer;
    function Previous: Integer;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(AValue: Integer);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Integer read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AOwnList: IJclIntegerList; ACursor: TJclIntegerLinkedListItem; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TIntegerItr } ============================================================

constructor TIntegerItr.Create(const AOwnList: IJclIntegerList; ACursor: TJclIntegerLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclIntegerEqualityComparer;
end;

function TIntegerItr.Add(AValue: Integer): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TIntegerItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TIntegerItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TIntegerItr then
  begin
    ADest := TIntegerItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TIntegerItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TIntegerItr.Create(FOwnList, FCursor, Valid, FStart);
end;

function TIntegerItr.Equals(const AIterator: IJclIntegerIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TIntegerItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TIntegerItr then
  begin
    ItrObj := TIntegerItr(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TIntegerItr.GetValue: Integer;
begin
  CheckValid;
  Result := 0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TIntegerItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.Insert(AValue: Integer): Boolean;
var
  NewCursor: TJclIntegerLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AValue);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AValue);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclIntegerLinkedListItem.Create;
          NewCursor.Value := AValue;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TIntegerItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TIntegerItr.Next: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TIntegerItr.Previous: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TIntegerItr.Remove;
var
  OldCursor: TJclIntegerLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := 0;
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntegerItr.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntegerItr.SetValue(AValue: Integer);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := 0;
    FCursor.Value := AValue;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

type
  TCardinalItr = class(TJclAbstractIterator, IJclCardinalIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclCardinalLinkedListItem;
    FStart: TItrStart;
    FOwnList: IJclCardinalList;
    FEqualityComparer: IJclCardinalEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclCardinalIterator }
    function Add(AValue: Cardinal): Boolean;
    function Equals(const AIterator: IJclCardinalIterator): Boolean;
    function GetValue: Cardinal;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AValue: Cardinal): Boolean;
    function Next: Cardinal;
    function NextIndex: Integer;
    function Previous: Cardinal;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(AValue: Cardinal);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Cardinal read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AOwnList: IJclCardinalList; ACursor: TJclCardinalLinkedListItem; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TCardinalItr } ============================================================

constructor TCardinalItr.Create(const AOwnList: IJclCardinalList; ACursor: TJclCardinalLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclCardinalEqualityComparer;
end;

function TCardinalItr.Add(AValue: Cardinal): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TCardinalItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TCardinalItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TCardinalItr then
  begin
    ADest := TCardinalItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TCardinalItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TCardinalItr.Create(FOwnList, FCursor, Valid, FStart);
end;

function TCardinalItr.Equals(const AIterator: IJclCardinalIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TCardinalItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TCardinalItr then
  begin
    ItrObj := TCardinalItr(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TCardinalItr.GetValue: Cardinal;
begin
  CheckValid;
  Result := 0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TCardinalItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.Insert(AValue: Cardinal): Boolean;
var
  NewCursor: TJclCardinalLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AValue);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AValue);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclCardinalLinkedListItem.Create;
          NewCursor.Value := AValue;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TCardinalItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TCardinalItr.Next: Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TCardinalItr.Previous: Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TCardinalItr.Remove;
var
  OldCursor: TJclCardinalLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := 0;
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TCardinalItr.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TCardinalItr.SetValue(AValue: Cardinal);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := 0;
    FCursor.Value := AValue;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

type
  TInt64Itr = class(TJclAbstractIterator, IJclInt64Iterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclInt64LinkedListItem;
    FStart: TItrStart;
    FOwnList: IJclInt64List;
    FEqualityComparer: IJclInt64EqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclInt64Iterator }
    function Add(const AValue: Int64): Boolean;
    function Equals(const AIterator: IJclInt64Iterator): Boolean;
    function GetValue: Int64;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Int64): Boolean;
    function Next: Int64;
    function NextIndex: Integer;
    function Previous: Int64;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(const AValue: Int64);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Int64 read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AOwnList: IJclInt64List; ACursor: TJclInt64LinkedListItem; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TInt64Itr } ============================================================

constructor TInt64Itr.Create(const AOwnList: IJclInt64List; ACursor: TJclInt64LinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclInt64EqualityComparer;
end;

function TInt64Itr.Add(const AValue: Int64): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TInt64Itr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TInt64Itr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TInt64Itr then
  begin
    ADest := TInt64Itr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TInt64Itr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInt64Itr.Create(FOwnList, FCursor, Valid, FStart);
end;

function TInt64Itr.Equals(const AIterator: IJclInt64Iterator): Boolean;
var
  Obj: TObject;
  ItrObj: TInt64Itr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TInt64Itr then
  begin
    ItrObj := TInt64Itr(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TInt64Itr.GetValue: Int64;
begin
  CheckValid;
  Result := 0;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TInt64Itr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.Insert(const AValue: Int64): Boolean;
var
  NewCursor: TJclInt64LinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AValue);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AValue);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclInt64LinkedListItem.Create;
          NewCursor.Value := AValue;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TInt64Itr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TInt64Itr.Next: Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TInt64Itr.Previous: Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TInt64Itr.Remove;
var
  OldCursor: TJclInt64LinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := 0;
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TInt64Itr.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TInt64Itr.SetValue(const AValue: Int64);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := 0;
    FCursor.Value := AValue;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFNDEF CLR}
type
  TPtrItr = class(TJclAbstractIterator, IJclPtrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclPtrLinkedListItem;
    FStart: TItrStart;
    FOwnList: IJclPtrList;
    FEqualityComparer: IJclPtrEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclPtrIterator }
    function Add(AValue: Pointer): Boolean;
    function Equals(const AIterator: IJclPtrIterator): Boolean;
    function GetPointer: Pointer;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AValue: Pointer): Boolean;
    function Next: Pointer;
    function NextIndex: Integer;
    function Previous: Pointer;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetPointer(AValue: Pointer);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Pointer read GetPointer;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AOwnList: IJclPtrList; ACursor: TJclPtrLinkedListItem; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TPtrItr } ============================================================

constructor TPtrItr.Create(const AOwnList: IJclPtrList; ACursor: TJclPtrLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclPtrEqualityComparer;
end;

function TPtrItr.Add(AValue: Pointer): Boolean;
begin
  Result := FOwnList.Add(AValue);
end;

procedure TPtrItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TPtrItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TPtrItr then
  begin
    ADest := TPtrItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TPtrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPtrItr.Create(FOwnList, FCursor, Valid, FStart);
end;

function TPtrItr.Equals(const AIterator: IJclPtrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TPtrItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TPtrItr then
  begin
    ItrObj := TPtrItr(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TPtrItr.GetPointer: Pointer;
begin
  CheckValid;
  Result := nil;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TPtrItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.Insert(AValue: Pointer): Boolean;
var
  NewCursor: TJclPtrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, nil);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AValue);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AValue);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclPtrLinkedListItem.Create;
          NewCursor.Value := AValue;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TPtrItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TPtrItr.Next: Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TPtrItr.Previous: Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TPtrItr.Remove;
var
  OldCursor: TJclPtrLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := nil;
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TPtrItr.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TPtrItr.SetPointer(AValue: Pointer);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := nil;
    FCursor.Value := AValue;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF ~CLR}

type
  TItr = class(TJclAbstractIterator, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclLinkedListItem;
    FStart: TItrStart;
    FOwnList: IJclList;
    FEqualityComparer: IJclEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclIterator }
    function Add(AObject: TObject): Boolean;
    function Equals(const AIterator: IJclIterator): Boolean;
    function GetObject: TObject;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AObject: TObject): Boolean;
    function Next: TObject;
    function NextIndex: Integer;
    function Previous: TObject;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetObject(AObject: TObject);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: TObject read GetObject;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AOwnList: IJclList; ACursor: TJclLinkedListItem; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TItr } ============================================================

constructor TItr.Create(const AOwnList: IJclList; ACursor: TJclLinkedListItem; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclEqualityComparer;
end;

function TItr.Add(AObject: TObject): Boolean;
begin
  Result := FOwnList.Add(AObject);
end;

procedure TItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TItr then
  begin
    ADest := TItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TItr.Create(FOwnList, FCursor, Valid, FStart);
end;

function TItr.Equals(const AIterator: IJclIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TItr then
  begin
    ItrObj := TItr(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TItr.GetObject: TObject;
begin
  CheckValid;
  Result := nil;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.Insert(AObject: TObject): Boolean;
var
  NewCursor: TJclLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AObject, nil);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AObject);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AObject);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclLinkedListItem.Create;
          NewCursor.Value := AObject;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TItr.Next: TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TItr.Previous: TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TItr.Remove;
var
  OldCursor: TJclLinkedListItem;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      (FownList as IJclObjectOwner).FreeObject(FCursor.Value);
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr.SetObject(AObject: TObject);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    (FownList as IJclObjectOwner).FreeObject(FCursor.Value);
    FCursor.Value := AObject;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_GENERICS}
type
  TItr<T> = class(TJclAbstractIterator, IJclIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclLinkedListItem<T>;
    FStart: TItrStart;
    FOwnList: IJclList<T>;
    FEqualityComparer: IJclEqualityComparer<T>;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclIterator<T> }
    function Add(const AItem: T): Boolean;
    function Equals(const AIterator: IJclIterator<T>): Boolean;
    function GetItem: T;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AItem: T): Boolean;
    function Next: T;
    function NextIndex: Integer;
    function Previous: T;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetItem(const AItem: T);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: T read GetItem;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AOwnList: IJclList<T>; ACursor: TJclLinkedListItem<T>; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TItr<T> } ============================================================

constructor TItr<T>.Create(const AOwnList: IJclList<T>; ACursor: TJclLinkedListItem<T>; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnList := AOwnList;
  FStart := AStart;
  FEqualityComparer := AOwnList as IJclEqualityComparer<T>;
end;

function TItr<T>.Add(const AItem: T): Boolean;
begin
  Result := FOwnList.Add(AItem);
end;

procedure TItr<T>.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TItr<T>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TItr<T> then
  begin
    ADest := TItr<T>(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TItr<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TItr<T>.Create(FOwnList, FCursor, Valid, FStart);
end;

function TItr<T>.Equals(const AIterator: IJclIterator<T>): Boolean;
var
  Obj: TObject;
  ItrObj: TItr<T>;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TItr<T> then
  begin
    ItrObj := TItr<T>(Obj);
    Result := (FOwnList = ItrObj.FOwnList) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TItr<T>.GetItem: T;
begin
  CheckValid;
  Result := Default(T);
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TItr<T>.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.Insert(const AItem: T): Boolean;
var
  NewCursor: TJclLinkedListItem<T>;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AItem, Default(T));
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AItem);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AItem);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclLinkedListItem<T>.Create;
          NewCursor.Value := AItem;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TItr<T>.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TItr<T>.Next: T;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := Default(T);
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TItr<T>.Previous: T;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := Default(T);
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TItr<T>.Remove;
var
  OldCursor: TJclLinkedListItem<T>;
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      (FownList as IJclItemOwner<T>).FreeItem(FCursor.Value);
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr<T>.Reset;
begin
  {$IFDEF THREADSAFE}
  FOwnList.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          while (FCursor <> nil) and (FCursor.Previous <> nil) do
            FCursor := FCursor.Previous;
        end;
      isLast:
        begin
          while (FCursor <> nil) and (FCursor.Next <> nil) do
            FCursor := FCursor.Next;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr<T>.SetItem(const AItem: T);
begin
  if FOwnList.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnList.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    (FownList as IJclItemOwner<T>).FreeItem(FCursor.Value);
    FCursor.Value := AItem;
  {$IFDEF THREADSAFE}
  finally
    FOwnList.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_GENERICS}

//=== { TJclLinkedList<T> } ==================================================

constructor TJclIntfLinkedList.Create(const ACollection: IJclIntfCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclIntfLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntfLinkedList.Add(const AInterface: IInterface): Boolean;
var
  NewItem: TJclIntfLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AInterface, nil);
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AInterface, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclIntfLinkedListItem.Create;
        NewItem.Value := AInterface;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.AddAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  Item: IInterface;
  AddItem: Boolean;
  NewItem: TJclIntfLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclIntfLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclIntfCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclIntfCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclIntfLinkedList.Clear;
var
  Old, Current: TJclIntfLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeObject(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.Contains(const AInterface: IInterface): Boolean;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AInterface) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

function TJclIntfLinkedList.Delete(Index: Integer): IInterface;
var
  Current: TJclIntfLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeObject(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.Equals(const ACollection: IJclIntfCollection): Boolean;
var
  It, ItSelf: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.First: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntfLinkedList.GetEnumerator: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntfLinkedList.GetObject(Index: Integer): IInterface;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
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

function TJclIntfLinkedList.IndexOf(const AInterface: IInterface): Integer;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AInterface) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.Insert(Index: Integer; const AInterface: IInterface): Boolean;
var
  Current, NewItem: TJclIntfLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AInterface, nil);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AInterface, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclIntfLinkedListItem.Create;
        NewItem.Value := AInterface;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  Current, NewItem, Test: TJclIntfLinkedListItem;
  AddItem: Boolean;
  Item: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclIntfLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclIntfLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclIntfLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfLinkedList.Last: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, FEnd, False, isLast);
end;

function TJclIntfLinkedList.LastIndexOf(const AInterface: IInterface): Integer;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AInterface) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.Remove(const AInterface: IInterface): Boolean;
var
  Current: TJclIntfLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AInterface) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreeObject(Current.Value);
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.RetainAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfLinkedList.SetObject(Index: Integer; const AInterface: IInterface);
var
  Current: TJclIntfLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AInterface, nil);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AInterface, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeObject(Current.Value);
            Current.Value := AInterface;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfLinkedList.SubList(First, Count: Integer): IJclIntfList;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclIntfList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclLinkedList<T> } ==================================================

constructor TJclAnsiStrLinkedList.Create(const ACollection: IJclAnsiStrCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclAnsiStrLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclAnsiStrLinkedList.Add(const AString: AnsiString): Boolean;
var
  NewItem: TJclAnsiStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AString, '');
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AString, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclAnsiStrLinkedListItem.Create;
        NewItem.Value := AString;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
  Item: AnsiString;
  AddItem: Boolean;
  NewItem: TJclAnsiStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclAnsiStrLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclAnsiStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclAnsiStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclAnsiStrLinkedList.Clear;
var
  Old, Current: TJclAnsiStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeString(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.Contains(const AString: AnsiString): Boolean;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AString) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrLinkedList.Delete(Index: Integer): AnsiString;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeString(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.Equals(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It, ItSelf: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.First: IJclAnsiStrIterator;
begin
  Result := TAnsiStrItr.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclAnsiStrLinkedList.GetEnumerator: IJclAnsiStrIterator;
begin
  Result := TAnsiStrItr.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclAnsiStrLinkedList.GetString(Index: Integer): AnsiString;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
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

function TJclAnsiStrLinkedList.IndexOf(const AString: AnsiString): Integer;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AString) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.Insert(Index: Integer; const AString: AnsiString): Boolean;
var
  Current, NewItem: TJclAnsiStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AString, '');
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AString, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclAnsiStrLinkedListItem.Create;
        NewItem.Value := AString;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.InsertAll(Index: Integer; const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
  Current, NewItem, Test: TJclAnsiStrLinkedListItem;
  AddItem: Boolean;
  Item: AnsiString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclAnsiStrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclAnsiStrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclAnsiStrLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrLinkedList.Last: IJclAnsiStrIterator;
begin
  Result := TAnsiStrItr.Create(Self, FEnd, False, isLast);
end;

function TJclAnsiStrLinkedList.LastIndexOf(const AString: AnsiString): Integer;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AString) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.Remove(const AString: AnsiString): Boolean;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AString) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreeString(Current.Value);
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.RetainAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrLinkedList.SetString(Index: Integer; const AString: AnsiString);
var
  Current: TJclAnsiStrLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AString, '');
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AString, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeString(Current.Value);
            Current.Value := AString;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrLinkedList.SubList(First, Count: Integer): IJclAnsiStrList;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclAnsiStrList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclLinkedList<T> } ==================================================

constructor TJclWideStrLinkedList.Create(const ACollection: IJclWideStrCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclWideStrLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclWideStrLinkedList.Add(const AString: WideString): Boolean;
var
  NewItem: TJclWideStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AString, '');
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AString, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclWideStrLinkedListItem.Create;
        NewItem.Value := AString;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.AddAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
  Item: WideString;
  AddItem: Boolean;
  NewItem: TJclWideStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclWideStrLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclWideStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclWideStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclWideStrLinkedList.Clear;
var
  Old, Current: TJclWideStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeString(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.Contains(const AString: WideString): Boolean;
var
  Current: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AString) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.ContainsAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

function TJclWideStrLinkedList.Delete(Index: Integer): WideString;
var
  Current: TJclWideStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeString(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.Equals(const ACollection: IJclWideStrCollection): Boolean;
var
  It, ItSelf: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.First: IJclWideStrIterator;
begin
  Result := TWideStrItr.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclWideStrLinkedList.GetEnumerator: IJclWideStrIterator;
begin
  Result := TWideStrItr.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclWideStrLinkedList.GetString(Index: Integer): WideString;
var
  Current: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
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

function TJclWideStrLinkedList.IndexOf(const AString: WideString): Integer;
var
  Current: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AString) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.Insert(Index: Integer; const AString: WideString): Boolean;
var
  Current, NewItem: TJclWideStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AString, '');
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AString, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclWideStrLinkedListItem.Create;
        NewItem.Value := AString;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.InsertAll(Index: Integer; const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
  Current, NewItem, Test: TJclWideStrLinkedListItem;
  AddItem: Boolean;
  Item: WideString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclWideStrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclWideStrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclWideStrLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrLinkedList.Last: IJclWideStrIterator;
begin
  Result := TWideStrItr.Create(Self, FEnd, False, isLast);
end;

function TJclWideStrLinkedList.LastIndexOf(const AString: WideString): Integer;
var
  Current: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AString) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.Remove(const AString: WideString): Boolean;
var
  Current: TJclWideStrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AString) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreeString(Current.Value);
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.RemoveAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.RetainAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrLinkedList.SetString(Index: Integer; const AString: WideString);
var
  Current: TJclWideStrLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AString, '');
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AString, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeString(Current.Value);
            Current.Value := AString;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrLinkedList.SubList(First, Count: Integer): IJclWideStrList;
var
  Current: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclWideStrList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclLinkedList<T> } ==================================================

constructor TJclSingleLinkedList.Create(const ACollection: IJclSingleCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclSingleLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclSingleLinkedList.Add(const AValue: Single): Boolean;
var
  NewItem: TJclSingleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AValue, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclSingleLinkedListItem.Create;
        NewItem.Value := AValue;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.AddAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
  Item: Single;
  AddItem: Boolean;
  NewItem: TJclSingleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclSingleLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclSingleCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclSingleCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclSingleLinkedList.Clear;
var
  Old, Current: TJclSingleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeSingle(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.Contains(const AValue: Single): Boolean;
var
  Current: TJclSingleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.ContainsAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

function TJclSingleLinkedList.Delete(Index: Integer): Single;
var
  Current: TJclSingleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeSingle(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.Equals(const ACollection: IJclSingleCollection): Boolean;
var
  It, ItSelf: IJclSingleIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.First: IJclSingleIterator;
begin
  Result := TSingleItr.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclSingleLinkedList.GetEnumerator: IJclSingleIterator;
begin
  Result := TSingleItr.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclSingleLinkedList.GetValue(Index: Integer): Single;
var
  Current: TJclSingleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
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

function TJclSingleLinkedList.IndexOf(const AValue: Single): Integer;
var
  Current: TJclSingleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.Insert(Index: Integer; const AValue: Single): Boolean;
var
  Current, NewItem: TJclSingleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclSingleLinkedListItem.Create;
        NewItem.Value := AValue;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.InsertAll(Index: Integer; const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
  Current, NewItem, Test: TJclSingleLinkedListItem;
  AddItem: Boolean;
  Item: Single;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclSingleLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclSingleLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclSingleLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleLinkedList.Last: IJclSingleIterator;
begin
  Result := TSingleItr.Create(Self, FEnd, False, isLast);
end;

function TJclSingleLinkedList.LastIndexOf(const AValue: Single): Integer;
var
  Current: TJclSingleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.Remove(const AValue: Single): Boolean;
var
  Current: TJclSingleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreeSingle(Current.Value);
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.RemoveAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.RetainAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleLinkedList.SetValue(Index: Integer; const AValue: Single);
var
  Current: TJclSingleLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeSingle(Current.Value);
            Current.Value := AValue;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleLinkedList.SubList(First, Count: Integer): IJclSingleList;
var
  Current: TJclSingleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclSingleList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclLinkedList<T> } ==================================================

constructor TJclDoubleLinkedList.Create(const ACollection: IJclDoubleCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclDoubleLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclDoubleLinkedList.Add(const AValue: Double): Boolean;
var
  NewItem: TJclDoubleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AValue, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclDoubleLinkedListItem.Create;
        NewItem.Value := AValue;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.AddAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
  Item: Double;
  AddItem: Boolean;
  NewItem: TJclDoubleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclDoubleLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclDoubleCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclDoubleCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclDoubleLinkedList.Clear;
var
  Old, Current: TJclDoubleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeDouble(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.Contains(const AValue: Double): Boolean;
var
  Current: TJclDoubleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

function TJclDoubleLinkedList.Delete(Index: Integer): Double;
var
  Current: TJclDoubleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeDouble(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.Equals(const ACollection: IJclDoubleCollection): Boolean;
var
  It, ItSelf: IJclDoubleIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.First: IJclDoubleIterator;
begin
  Result := TDoubleItr.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclDoubleLinkedList.GetEnumerator: IJclDoubleIterator;
begin
  Result := TDoubleItr.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclDoubleLinkedList.GetValue(Index: Integer): Double;
var
  Current: TJclDoubleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
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

function TJclDoubleLinkedList.IndexOf(const AValue: Double): Integer;
var
  Current: TJclDoubleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.Insert(Index: Integer; const AValue: Double): Boolean;
var
  Current, NewItem: TJclDoubleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclDoubleLinkedListItem.Create;
        NewItem.Value := AValue;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.InsertAll(Index: Integer; const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
  Current, NewItem, Test: TJclDoubleLinkedListItem;
  AddItem: Boolean;
  Item: Double;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclDoubleLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclDoubleLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclDoubleLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleLinkedList.Last: IJclDoubleIterator;
begin
  Result := TDoubleItr.Create(Self, FEnd, False, isLast);
end;

function TJclDoubleLinkedList.LastIndexOf(const AValue: Double): Integer;
var
  Current: TJclDoubleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.Remove(const AValue: Double): Boolean;
var
  Current: TJclDoubleLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreeDouble(Current.Value);
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.RemoveAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.RetainAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleLinkedList.SetValue(Index: Integer; const AValue: Double);
var
  Current: TJclDoubleLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeDouble(Current.Value);
            Current.Value := AValue;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleLinkedList.SubList(First, Count: Integer): IJclDoubleList;
var
  Current: TJclDoubleLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclDoubleList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclLinkedList<T> } ==================================================

constructor TJclExtendedLinkedList.Create(const ACollection: IJclExtendedCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclExtendedLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclExtendedLinkedList.Add(const AValue: Extended): Boolean;
var
  NewItem: TJclExtendedLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AValue, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclExtendedLinkedListItem.Create;
        NewItem.Value := AValue;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.AddAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
  Item: Extended;
  AddItem: Boolean;
  NewItem: TJclExtendedLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclExtendedLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclExtendedCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclExtendedCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclExtendedLinkedList.Clear;
var
  Old, Current: TJclExtendedLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeExtended(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.Contains(const AValue: Extended): Boolean;
var
  Current: TJclExtendedLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

function TJclExtendedLinkedList.Delete(Index: Integer): Extended;
var
  Current: TJclExtendedLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeExtended(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.Equals(const ACollection: IJclExtendedCollection): Boolean;
var
  It, ItSelf: IJclExtendedIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.First: IJclExtendedIterator;
begin
  Result := TExtendedItr.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclExtendedLinkedList.GetEnumerator: IJclExtendedIterator;
begin
  Result := TExtendedItr.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclExtendedLinkedList.GetValue(Index: Integer): Extended;
var
  Current: TJclExtendedLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
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

function TJclExtendedLinkedList.IndexOf(const AValue: Extended): Integer;
var
  Current: TJclExtendedLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.Insert(Index: Integer; const AValue: Extended): Boolean;
var
  Current, NewItem: TJclExtendedLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclExtendedLinkedListItem.Create;
        NewItem.Value := AValue;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.InsertAll(Index: Integer; const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
  Current, NewItem, Test: TJclExtendedLinkedListItem;
  AddItem: Boolean;
  Item: Extended;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclExtendedLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclExtendedLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclExtendedLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedLinkedList.Last: IJclExtendedIterator;
begin
  Result := TExtendedItr.Create(Self, FEnd, False, isLast);
end;

function TJclExtendedLinkedList.LastIndexOf(const AValue: Extended): Integer;
var
  Current: TJclExtendedLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.Remove(const AValue: Extended): Boolean;
var
  Current: TJclExtendedLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreeExtended(Current.Value);
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.RemoveAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.RetainAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedLinkedList.SetValue(Index: Integer; const AValue: Extended);
var
  Current: TJclExtendedLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeExtended(Current.Value);
            Current.Value := AValue;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedLinkedList.SubList(First, Count: Integer): IJclExtendedList;
var
  Current: TJclExtendedLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclExtendedList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclLinkedList<T> } ==================================================

constructor TJclIntegerLinkedList.Create(const ACollection: IJclIntegerCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclIntegerLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntegerLinkedList.Add(AValue: Integer): Boolean;
var
  NewItem: TJclIntegerLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AValue, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclIntegerLinkedListItem.Create;
        NewItem.Value := AValue;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.AddAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
  Item: Integer;
  AddItem: Boolean;
  NewItem: TJclIntegerLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclIntegerLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclIntegerCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclIntegerCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclIntegerLinkedList.Clear;
var
  Old, Current: TJclIntegerLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeInteger(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.Contains(AValue: Integer): Boolean;
var
  Current: TJclIntegerLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

function TJclIntegerLinkedList.Delete(Index: Integer): Integer;
var
  Current: TJclIntegerLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeInteger(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.Equals(const ACollection: IJclIntegerCollection): Boolean;
var
  It, ItSelf: IJclIntegerIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.First: IJclIntegerIterator;
begin
  Result := TIntegerItr.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntegerLinkedList.GetEnumerator: IJclIntegerIterator;
begin
  Result := TIntegerItr.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntegerLinkedList.GetValue(Index: Integer): Integer;
var
  Current: TJclIntegerLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
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

function TJclIntegerLinkedList.IndexOf(AValue: Integer): Integer;
var
  Current: TJclIntegerLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.Insert(Index: Integer; AValue: Integer): Boolean;
var
  Current, NewItem: TJclIntegerLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclIntegerLinkedListItem.Create;
        NewItem.Value := AValue;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.InsertAll(Index: Integer; const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
  Current, NewItem, Test: TJclIntegerLinkedListItem;
  AddItem: Boolean;
  Item: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclIntegerLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclIntegerLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclIntegerLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerLinkedList.Last: IJclIntegerIterator;
begin
  Result := TIntegerItr.Create(Self, FEnd, False, isLast);
end;

function TJclIntegerLinkedList.LastIndexOf(AValue: Integer): Integer;
var
  Current: TJclIntegerLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.Remove(AValue: Integer): Boolean;
var
  Current: TJclIntegerLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreeInteger(Current.Value);
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.RemoveAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.RetainAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerLinkedList.SetValue(Index: Integer; AValue: Integer);
var
  Current: TJclIntegerLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeInteger(Current.Value);
            Current.Value := AValue;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerLinkedList.SubList(First, Count: Integer): IJclIntegerList;
var
  Current: TJclIntegerLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclIntegerList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclLinkedList<T> } ==================================================

constructor TJclCardinalLinkedList.Create(const ACollection: IJclCardinalCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclCardinalLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclCardinalLinkedList.Add(AValue: Cardinal): Boolean;
var
  NewItem: TJclCardinalLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AValue, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclCardinalLinkedListItem.Create;
        NewItem.Value := AValue;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.AddAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
  Item: Cardinal;
  AddItem: Boolean;
  NewItem: TJclCardinalLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclCardinalLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclCardinalCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclCardinalCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclCardinalLinkedList.Clear;
var
  Old, Current: TJclCardinalLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeCardinal(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.Contains(AValue: Cardinal): Boolean;
var
  Current: TJclCardinalLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

function TJclCardinalLinkedList.Delete(Index: Integer): Cardinal;
var
  Current: TJclCardinalLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeCardinal(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.Equals(const ACollection: IJclCardinalCollection): Boolean;
var
  It, ItSelf: IJclCardinalIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.First: IJclCardinalIterator;
begin
  Result := TCardinalItr.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclCardinalLinkedList.GetEnumerator: IJclCardinalIterator;
begin
  Result := TCardinalItr.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclCardinalLinkedList.GetValue(Index: Integer): Cardinal;
var
  Current: TJclCardinalLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
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

function TJclCardinalLinkedList.IndexOf(AValue: Cardinal): Integer;
var
  Current: TJclCardinalLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.Insert(Index: Integer; AValue: Cardinal): Boolean;
var
  Current, NewItem: TJclCardinalLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclCardinalLinkedListItem.Create;
        NewItem.Value := AValue;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.InsertAll(Index: Integer; const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
  Current, NewItem, Test: TJclCardinalLinkedListItem;
  AddItem: Boolean;
  Item: Cardinal;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclCardinalLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclCardinalLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclCardinalLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalLinkedList.Last: IJclCardinalIterator;
begin
  Result := TCardinalItr.Create(Self, FEnd, False, isLast);
end;

function TJclCardinalLinkedList.LastIndexOf(AValue: Cardinal): Integer;
var
  Current: TJclCardinalLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.Remove(AValue: Cardinal): Boolean;
var
  Current: TJclCardinalLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreeCardinal(Current.Value);
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.RemoveAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.RetainAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalLinkedList.SetValue(Index: Integer; AValue: Cardinal);
var
  Current: TJclCardinalLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeCardinal(Current.Value);
            Current.Value := AValue;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalLinkedList.SubList(First, Count: Integer): IJclCardinalList;
var
  Current: TJclCardinalLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclCardinalList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclLinkedList<T> } ==================================================

constructor TJclInt64LinkedList.Create(const ACollection: IJclInt64Collection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclInt64LinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclInt64LinkedList.Add(const AValue: Int64): Boolean;
var
  NewItem: TJclInt64LinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AValue, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclInt64LinkedListItem.Create;
        NewItem.Value := AValue;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.AddAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
  Item: Int64;
  AddItem: Boolean;
  NewItem: TJclInt64LinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclInt64LinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64LinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclInt64Collection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclInt64Collection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclInt64LinkedList.Clear;
var
  Old, Current: TJclInt64LinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeInt64(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.Contains(const AValue: Int64): Boolean;
var
  Current: TJclInt64LinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.ContainsAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64LinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

function TJclInt64LinkedList.Delete(Index: Integer): Int64;
var
  Current: TJclInt64LinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeInt64(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.Equals(const ACollection: IJclInt64Collection): Boolean;
var
  It, ItSelf: IJclInt64Iterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.First: IJclInt64Iterator;
begin
  Result := TInt64Itr.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclInt64LinkedList.GetEnumerator: IJclInt64Iterator;
begin
  Result := TInt64Itr.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclInt64LinkedList.GetValue(Index: Integer): Int64;
var
  Current: TJclInt64LinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
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

function TJclInt64LinkedList.IndexOf(const AValue: Int64): Integer;
var
  Current: TJclInt64LinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.Insert(Index: Integer; const AValue: Int64): Boolean;
var
  Current, NewItem: TJclInt64LinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclInt64LinkedListItem.Create;
        NewItem.Value := AValue;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.InsertAll(Index: Integer; const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
  Current, NewItem, Test: TJclInt64LinkedListItem;
  AddItem: Boolean;
  Item: Int64;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclInt64LinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclInt64LinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclInt64LinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64LinkedList.Last: IJclInt64Iterator;
begin
  Result := TInt64Itr.Create(Self, FEnd, False, isLast);
end;

function TJclInt64LinkedList.LastIndexOf(const AValue: Int64): Integer;
var
  Current: TJclInt64LinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AValue) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.Remove(const AValue: Int64): Boolean;
var
  Current: TJclInt64LinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AValue) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreeInt64(Current.Value);
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.RemoveAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.RetainAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64LinkedList.SetValue(Index: Integer; const AValue: Int64);
var
  Current: TJclInt64LinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AValue, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeInt64(Current.Value);
            Current.Value := AValue;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64LinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64LinkedList.SubList(First, Count: Integer): IJclInt64List;
var
  Current: TJclInt64LinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclInt64List;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFNDEF CLR}
//=== { TJclLinkedList<T> } ==================================================

constructor TJclPtrLinkedList.Create(const ACollection: IJclPtrCollection);
begin
  inherited Create();
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclPtrLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclPtrLinkedList.Add(APtr: Pointer): Boolean;
var
  NewItem: TJclPtrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(APtr, nil);
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(APtr, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclPtrLinkedListItem.Create;
        NewItem.Value := APtr;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.AddAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
  Item: Pointer;
  AddItem: Boolean;
  NewItem: TJclPtrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclPtrLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclPtrCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclPtrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclPtrLinkedList.Clear;
var
  Old, Current: TJclPtrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreePointer(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.Contains(APtr: Pointer): Boolean;
var
  Current: TJclPtrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, APtr) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.ContainsAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

function TJclPtrLinkedList.Delete(Index: Integer): Pointer;
var
  Current: TJclPtrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreePointer(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.Equals(const ACollection: IJclPtrCollection): Boolean;
var
  It, ItSelf: IJclPtrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.First: IJclPtrIterator;
begin
  Result := TPtrItr.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclPtrLinkedList.GetEnumerator: IJclPtrIterator;
begin
  Result := TPtrItr.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclPtrLinkedList.GetPointer(Index: Integer): Pointer;
var
  Current: TJclPtrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
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

function TJclPtrLinkedList.IndexOf(APtr: Pointer): Integer;
var
  Current: TJclPtrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, APtr) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.Insert(Index: Integer; APtr: Pointer): Boolean;
var
  Current, NewItem: TJclPtrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(APtr, nil);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(APtr, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclPtrLinkedListItem.Create;
        NewItem.Value := APtr;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.InsertAll(Index: Integer; const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
  Current, NewItem, Test: TJclPtrLinkedListItem;
  AddItem: Boolean;
  Item: Pointer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclPtrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclPtrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclPtrLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrLinkedList.Last: IJclPtrIterator;
begin
  Result := TPtrItr.Create(Self, FEnd, False, isLast);
end;

function TJclPtrLinkedList.LastIndexOf(APtr: Pointer): Integer;
var
  Current: TJclPtrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, APtr) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.Remove(APtr: Pointer): Boolean;
var
  Current: TJclPtrLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, APtr) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreePointer(Current.Value);
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.RemoveAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.RetainAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrLinkedList.SetPointer(Index: Integer; APtr: Pointer);
var
  Current: TJclPtrLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(APtr, nil);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(APtr, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreePointer(Current.Value);
            Current.Value := APtr;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrLinkedList.SubList(First, Count: Integer): IJclPtrList;
var
  Current: TJclPtrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclPtrList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF ~CLR}

//=== { TJclLinkedList<T> } ==================================================

constructor TJclLinkedList.Create(const ACollection: IJclCollection; AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclLinkedList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclLinkedList.Add(AObject: TObject): Boolean;
var
  NewItem: TJclLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AObject, nil);
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AObject, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclLinkedListItem.Create;
        NewItem.Value := AObject;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.AddAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  Item: TObject;
  AddItem: Boolean;
  NewItem: TJclLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclLinkedListItem.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclLinkedList.Clear;
var
  Old, Current: TJclLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeObject(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.Contains(AObject: TObject): Boolean;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AObject) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.ContainsAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedList.Create(nil, False);
  AssignPropertiesTo(Result);
end;

function TJclLinkedList.Delete(Index: Integer): TObject;
var
  Current: TJclLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeObject(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.Equals(const ACollection: IJclCollection): Boolean;
var
  It, ItSelf: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.First: IJclIterator;
begin
  Result := TItr.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclLinkedList.GetEnumerator: IJclIterator;
begin
  Result := TItr.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclLinkedList.GetObject(Index: Integer): TObject;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
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

function TJclLinkedList.IndexOf(AObject: TObject): Integer;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AObject) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.Insert(Index: Integer; AObject: TObject): Boolean;
var
  Current, NewItem: TJclLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AObject, nil);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AObject, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclLinkedListItem.Create;
        NewItem.Value := AObject;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.InsertAll(Index: Integer; const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  Current, NewItem, Test: TJclLinkedListItem;
  AddItem: Boolean;
  Item: TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclLinkedList.Last: IJclIterator;
begin
  Result := TItr.Create(Self, FEnd, False, isLast);
end;

function TJclLinkedList.LastIndexOf(AObject: TObject): Integer;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AObject) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.Remove(AObject: TObject): Boolean;
var
  Current: TJclLinkedListItem;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AObject) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreeObject(Current.Value);
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.RemoveAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.RetainAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedList.SetObject(Index: Integer; AObject: TObject);
var
  Current: TJclLinkedListItem;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AObject, nil);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AObject, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeObject(Current.Value);
            Current.Value := AObject;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclLinkedList.SubList(First, Count: Integer): IJclList;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclList;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_GENERICS}

//=== { TJclLinkedList<T> } ==================================================

constructor TJclLinkedList<T>.Create(const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclLinkedList<T>.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclLinkedList<T>.Add(const AItem: T): Boolean;
var
  NewItem: TJclLinkedListItem<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AItem, Default(T));
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AItem, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclLinkedListItem<T>.Create;
        NewItem.Value := AItem;
        if FStart <> nil then
        begin
          NewItem.Next := nil;
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
        end
        else
        begin
          FStart := NewItem;
          FEnd := NewItem;
        end;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
  Item: T;
  AddItem: Boolean;
  NewItem: TJclLinkedListItem<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
    begin
      Item := It.Next;
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, Default(T));
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclLinkedListItem<T>.Create;
          NewItem.Value := Item;
          if FStart <> nil then
          begin
            NewItem.Next := nil;
            NewItem.Previous := FEnd;
            FEnd.Next := NewItem;
            FEnd := NewItem;
          end
          else
          begin
            FStart := NewItem;
            FEnd := NewItem;
          end;
          Inc(FSize);
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedList<T>.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclCollection<T>;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclCollection<T>, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclLinkedList<T>.Clear;
var
  Old, Current: TJclLinkedListItem<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeItem(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.Contains(const AItem: T): Boolean;
var
  Current: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AItem) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.ContainsAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclLinkedList<T>.Delete(Index: Integer): T;
var
  Current: TJclLinkedListItem<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if (Index >= 0) and (Index < FSize) then
    begin
      Current := FStart;
      while Current <> nil do
      begin
        if Index = 0 then
        begin
          if Current.Previous <> nil then
            Current.Previous.Next := Current.Next
          else
            FStart := Current.Next;
          if Current.Next <> nil then
            Current.Next.Previous := Current.Previous
          else
            FEnd := Current.Previous;
          Result := FreeItem(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.Equals(const ACollection: IJclCollection<T>): Boolean;
var
  It, ItSelf: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.First: IJclIterator<T>;
begin
  Result := TItr<T>.Create(Self, FStart, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclLinkedList<T>.GetEnumerator: IJclIterator<T>;
begin
  Result := TItr<T>.Create(Self, FStart, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclLinkedList<T>.GetItem(Index: Integer): T;
var
  Current: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value
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

function TJclLinkedList<T>.IndexOf(const AItem: T): Integer;
var
  Current: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AItem) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.Insert(Index: Integer; const AItem: T): Boolean;
var
  Current, NewItem: TJclLinkedListItem<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AItem, Default(T));
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AItem, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclLinkedListItem<T>.Create;
        NewItem.Value := AItem;
        if Index = 0 then
        begin
          NewItem.Next := FStart;
          if FStart <> nil then
            FStart.Previous := NewItem;
          FStart := NewItem;
          if FSize = 0 then
            FEnd := NewItem;
          Inc(FSize);
        end
        else
        if Index = FSize then
        begin
          NewItem.Previous := FEnd;
          FEnd.Next := NewItem;
          FEnd := NewItem;
          Inc(FSize);
        end
        else
        begin
          Current := FStart;
          while (Current <> nil) and (Index > 0) do
          begin
            Current := Current.Next;
            Dec(Index);
          end;
          if Current <> nil then
          begin
            NewItem.Next := Current;
            NewItem.Previous := Current.Previous;
            if Current.Previous <> nil then
              Current.Previous.Next := NewItem;
            Current.Previous := NewItem;
            Inc(FSize);
          end;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
  Current, NewItem, Test: TJclLinkedListItem<T>;
  AddItem: Boolean;
  Item: T;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    Result := True;
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, Default(T));
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclLinkedListItem<T>.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, Default(T));
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclLinkedListItem<T>.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, Default(T));
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclLinkedListItem<T>.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclLinkedList<T>.Last: IJclIterator<T>;
begin
  Result := TItr<T>.Create(Self, FEnd, False, isLast);
end;

function TJclLinkedList<T>.LastIndexOf(const AItem: T): Integer;
var
  Current: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AItem) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.Remove(const AItem: T): Boolean;
var
  Current: TJclLinkedListItem<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AItem) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreeItem(Current.Value);
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.RemoveAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.RetainAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedList<T>.SetItem(Index: Integer; const AItem: T);
var
  Current: TJclLinkedListItem<T>;
  ReplaceItem: Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AItem, Default(T));
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AItem, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeItem(Current.Value);
            Current.Value := AItem;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.Size: Integer;
begin
  Result := FSize;
end;

function TJclLinkedList<T>.SubList(First, Count: Integer): IJclList<T>;
var
  Current: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclList<T>;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

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

