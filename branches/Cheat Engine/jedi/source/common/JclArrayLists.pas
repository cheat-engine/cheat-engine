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
{ Last modified: $Date:: 2008-06-25 21:42:08 +0200 (mer., 25 juin 2008)                          $ }
{ Revision:      $Rev:: 2387                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
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

type
  TJclIntfArrayList = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
     IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclIntfEqualityComparer,
     IJclIntfCollection, IJclIntfList, IJclIntfArray)
  private
    FElementData: JclBase.TDynIInterfaceArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclIntfCollection); overload;
    destructor Destroy; override;
  end;

  TJclAnsiStrArrayList = class(TJclAnsiStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
     IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer, IJclAnsiStrFlatContainer, IJclAnsiStrEqualityComparer,
     IJclAnsiStrCollection, IJclAnsiStrList, IJclAnsiStrArray)
  private
    FElementData: JclBase.TDynAnsiStringArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclAnsiStrCollection); overload;
    destructor Destroy; override;
  end;

  TJclWideStrArrayList = class(TJclWideStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
     IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclStrContainer, IJclWideStrContainer, IJclWideStrFlatContainer, IJclWideStrEqualityComparer,
     IJclWideStrCollection, IJclWideStrList, IJclWideStrArray)
  private
    FElementData: JclBase.TDynWideStringArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclWideStrCollection); overload;
    destructor Destroy; override;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrArrayList = TJclAnsiStrArrayList;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrArrayList = TJclWideStrArrayList;
  {$ENDIF CONTAINER_WIDESTR}

  TJclSingleArrayList = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
     IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclSingleContainer, IJclSingleEqualityComparer,
     IJclSingleCollection, IJclSingleList, IJclSingleArray)
  private
    FElementData: JclBase.TDynSingleArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclSingleCollection); overload;
    destructor Destroy; override;
  end;

  TJclDoubleArrayList = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
     IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclDoubleContainer, IJclDoubleEqualityComparer,
     IJclDoubleCollection, IJclDoubleList, IJclDoubleArray)
  private
    FElementData: JclBase.TDynDoubleArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclDoubleCollection); overload;
    destructor Destroy; override;
  end;

  TJclExtendedArrayList = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
     IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclExtendedContainer, IJclExtendedEqualityComparer,
     IJclExtendedCollection, IJclExtendedList, IJclExtendedArray)
  private
    FElementData: JclBase.TDynExtendedArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclExtendedCollection); overload;
    destructor Destroy; override;
  end;

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatArrayList = TJclExtendedArrayList;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatArrayList = TJclDoubleArrayList;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatArrayList = TJclSingleArrayList;
  {$ENDIF MATH_SINGLE_PRECISION}

  TJclIntegerArrayList = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
     IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclIntegerEqualityComparer,
     IJclIntegerCollection, IJclIntegerList, IJclIntegerArray)
  private
    FElementData: JclBase.TDynIntegerArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclIntegerCollection); overload;
    destructor Destroy; override;
  end;

  TJclCardinalArrayList = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
     IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclCardinalEqualityComparer,
     IJclCardinalCollection, IJclCardinalList, IJclCardinalArray)
  private
    FElementData: JclBase.TDynCardinalArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclCardinalCollection); overload;
    destructor Destroy; override;
  end;

  TJclInt64ArrayList = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
     IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclInt64EqualityComparer,
     IJclInt64Collection, IJclInt64List, IJclInt64Array)
  private
    FElementData: JclBase.TDynInt64Array;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclInt64Collection); overload;
    destructor Destroy; override;
  end;

  {$IFNDEF CLR}
  TJclPtrArrayList = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
     IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclPtrEqualityComparer,
     IJclPtrCollection, IJclPtrList, IJclPtrArray)
  private
    FElementData: JclBase.TDynPointerArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclPtrCollection); overload;
    destructor Destroy; override;
  end;
  {$ENDIF ~CLR}

  TJclArrayList = class(TJclAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
     IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclObjectOwner, IJclEqualityComparer,
     IJclCollection, IJclList, IJclArray)
  private
    FElementData: JclBase.TDynObjectArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer; AOwnsObjects: Boolean); overload;
    constructor Create(const ACollection: IJclCollection; AOwnsObjects: Boolean); overload;
    destructor Destroy; override;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  TJclArrayList<T> = class(TJclAbstractContainer<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
     IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
     IJclCollection<T>, IJclList<T>, IJclArray<T>)
  private
    FElementData: TJclBase<T>.TDynArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
  public
    constructor Create(ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;
    destructor Destroy; override;
  end;

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
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclArrayLists.pas $';
    Revision: '$Revision: 2387 $';
    Date: '$Date: 2008-06-25 21:42:08 +0200 (mer., 25 juin 2008) $';
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
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: IJclIntfList;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
  public
    constructor Create(const AOwnList: IJclIntfList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TIntfItr } ===============================================================

constructor TIntfItr.Create(const AOwnList: IJclIntfList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
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
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
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
  Result := FOwnList.GetObject(FCursor);
end;

function TIntfItr.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TIntfItr.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TIntfItr.Insert(const AInterface: IInterface): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AInterface);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TIntfItr.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TIntfItr.Next: IInterface;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetObject(FCursor);
end;

function TIntfItr.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TIntfItr.Previous: IInterface;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetObject(FCursor);
end;

function TIntfItr.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TIntfItr.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TIntfItr.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TIntfItr.SetObject(const AInterface: IInterface);
begin
  CheckValid;
  FOwnList.SetObject(FCursor, AInterface);
end;

type
  TAnsiStrItr = class(TJclAbstractIterator, IJclAnsiStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: IJclAnsiStrList;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
  public
    constructor Create(const AOwnList: IJclAnsiStrList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TAnsiStrItr } ===============================================================

constructor TAnsiStrItr.Create(const AOwnList: IJclAnsiStrList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
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
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
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
  Result := FOwnList.GetString(FCursor);
end;

function TAnsiStrItr.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TAnsiStrItr.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TAnsiStrItr.Insert(const AString: AnsiString): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AString);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TAnsiStrItr.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TAnsiStrItr.Next: AnsiString;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetString(FCursor);
end;

function TAnsiStrItr.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TAnsiStrItr.Previous: AnsiString;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetString(FCursor);
end;

function TAnsiStrItr.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TAnsiStrItr.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TAnsiStrItr.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TAnsiStrItr.SetString(const AString: AnsiString);
begin
  CheckValid;
  FOwnList.SetString(FCursor, AString);
end;

type
  TWideStrItr = class(TJclAbstractIterator, IJclWideStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: IJclWideStrList;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
  public
    constructor Create(const AOwnList: IJclWideStrList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TWideStrItr } ===============================================================

constructor TWideStrItr.Create(const AOwnList: IJclWideStrList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
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
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
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
  Result := FOwnList.GetString(FCursor);
end;

function TWideStrItr.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TWideStrItr.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TWideStrItr.Insert(const AString: WideString): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AString);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TWideStrItr.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TWideStrItr.Next: WideString;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetString(FCursor);
end;

function TWideStrItr.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TWideStrItr.Previous: WideString;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetString(FCursor);
end;

function TWideStrItr.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TWideStrItr.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TWideStrItr.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TWideStrItr.SetString(const AString: WideString);
begin
  CheckValid;
  FOwnList.SetString(FCursor, AString);
end;

type
  TSingleItr = class(TJclAbstractIterator, IJclSingleIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: IJclSingleList;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
  public
    constructor Create(const AOwnList: IJclSingleList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TSingleItr } ===============================================================

constructor TSingleItr.Create(const AOwnList: IJclSingleList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
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
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
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
  Result := FOwnList.GetValue(FCursor);
end;

function TSingleItr.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TSingleItr.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TSingleItr.Insert(const AValue: Single): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AValue);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TSingleItr.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TSingleItr.Next: Single;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TSingleItr.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TSingleItr.Previous: Single;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TSingleItr.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TSingleItr.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TSingleItr.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TSingleItr.SetValue(const AValue: Single);
begin
  CheckValid;
  FOwnList.SetValue(FCursor, AValue);
end;

type
  TDoubleItr = class(TJclAbstractIterator, IJclDoubleIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: IJclDoubleList;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
  public
    constructor Create(const AOwnList: IJclDoubleList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TDoubleItr } ===============================================================

constructor TDoubleItr.Create(const AOwnList: IJclDoubleList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
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
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
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
  Result := FOwnList.GetValue(FCursor);
end;

function TDoubleItr.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TDoubleItr.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TDoubleItr.Insert(const AValue: Double): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AValue);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TDoubleItr.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TDoubleItr.Next: Double;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TDoubleItr.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TDoubleItr.Previous: Double;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TDoubleItr.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TDoubleItr.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TDoubleItr.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TDoubleItr.SetValue(const AValue: Double);
begin
  CheckValid;
  FOwnList.SetValue(FCursor, AValue);
end;

type
  TExtendedItr = class(TJclAbstractIterator, IJclExtendedIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: IJclExtendedList;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
  public
    constructor Create(const AOwnList: IJclExtendedList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TExtendedItr } ===============================================================

constructor TExtendedItr.Create(const AOwnList: IJclExtendedList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
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
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
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
  Result := FOwnList.GetValue(FCursor);
end;

function TExtendedItr.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TExtendedItr.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TExtendedItr.Insert(const AValue: Extended): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AValue);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TExtendedItr.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TExtendedItr.Next: Extended;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TExtendedItr.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TExtendedItr.Previous: Extended;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TExtendedItr.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TExtendedItr.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TExtendedItr.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TExtendedItr.SetValue(const AValue: Extended);
begin
  CheckValid;
  FOwnList.SetValue(FCursor, AValue);
end;

type
  TIntegerItr = class(TJclAbstractIterator, IJclIntegerIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: IJclIntegerList;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
  public
    constructor Create(const AOwnList: IJclIntegerList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TIntegerItr } ===============================================================

constructor TIntegerItr.Create(const AOwnList: IJclIntegerList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
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
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
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
  Result := FOwnList.GetValue(FCursor);
end;

function TIntegerItr.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TIntegerItr.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TIntegerItr.Insert(AValue: Integer): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AValue);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TIntegerItr.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TIntegerItr.Next: Integer;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TIntegerItr.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TIntegerItr.Previous: Integer;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TIntegerItr.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TIntegerItr.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TIntegerItr.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TIntegerItr.SetValue(AValue: Integer);
begin
  CheckValid;
  FOwnList.SetValue(FCursor, AValue);
end;

type
  TCardinalItr = class(TJclAbstractIterator, IJclCardinalIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: IJclCardinalList;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
  public
    constructor Create(const AOwnList: IJclCardinalList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TCardinalItr } ===============================================================

constructor TCardinalItr.Create(const AOwnList: IJclCardinalList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
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
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
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
  Result := FOwnList.GetValue(FCursor);
end;

function TCardinalItr.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TCardinalItr.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TCardinalItr.Insert(AValue: Cardinal): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AValue);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TCardinalItr.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TCardinalItr.Next: Cardinal;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TCardinalItr.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TCardinalItr.Previous: Cardinal;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TCardinalItr.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TCardinalItr.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TCardinalItr.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TCardinalItr.SetValue(AValue: Cardinal);
begin
  CheckValid;
  FOwnList.SetValue(FCursor, AValue);
end;

type
  TInt64Itr = class(TJclAbstractIterator, IJclInt64Iterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: IJclInt64List;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
  public
    constructor Create(const AOwnList: IJclInt64List; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TInt64Itr } ===============================================================

constructor TInt64Itr.Create(const AOwnList: IJclInt64List; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
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
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
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
  Result := FOwnList.GetValue(FCursor);
end;

function TInt64Itr.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TInt64Itr.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TInt64Itr.Insert(const AValue: Int64): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AValue);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TInt64Itr.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TInt64Itr.Next: Int64;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TInt64Itr.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TInt64Itr.Previous: Int64;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetValue(FCursor);
end;

function TInt64Itr.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TInt64Itr.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TInt64Itr.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TInt64Itr.SetValue(const AValue: Int64);
begin
  CheckValid;
  FOwnList.SetValue(FCursor, AValue);
end;

{$IFNDEF CLR}
type
  TPtrItr = class(TJclAbstractIterator, IJclPtrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: IJclPtrList;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclPtrIterator }
    function Add(APtr: Pointer): Boolean;
    function Equals(const AIterator: IJclPtrIterator): Boolean;
    function GetPointer: Pointer;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(APtr: Pointer): Boolean;
    function Next: Pointer;
    function NextIndex: Integer;
    function Previous: Pointer;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetPointer(APtr: Pointer);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Pointer read GetPointer;
    {$ENDIF SUPPORTS_FOR_IN}
  public
    constructor Create(const AOwnList: IJclPtrList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TPtrItr } ===============================================================

constructor TPtrItr.Create(const AOwnList: IJclPtrList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
end;

function TPtrItr.Add(APtr: Pointer): Boolean;
begin
  Result := FOwnList.Add(APtr);
end;

procedure TPtrItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TPtrItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TPtrItr then
  begin
    ADest := TPtrItr(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
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
  Result := FOwnList.GetPointer(FCursor);
end;

function TPtrItr.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TPtrItr.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TPtrItr.Insert(APtr: Pointer): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, APtr);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TPtrItr.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TPtrItr.Next: Pointer;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetPointer(FCursor);
end;

function TPtrItr.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TPtrItr.Previous: Pointer;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetPointer(FCursor);
end;

function TPtrItr.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TPtrItr.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TPtrItr.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TPtrItr.SetPointer(APtr: Pointer);
begin
  CheckValid;
  FOwnList.SetPointer(FCursor, APtr);
end;
{$ENDIF ~CLR}

type
  TItr = class(TJclAbstractIterator, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: IJclList;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
  public
    constructor Create(const AOwnList: IJclList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TItr } ===============================================================

constructor TItr.Create(const AOwnList: IJclList; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
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
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
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
  Result := FOwnList.GetObject(FCursor);
end;

function TItr.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TItr.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TItr.Insert(AObject: TObject): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AObject);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TItr.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TItr.Next: TObject;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetObject(FCursor);
end;

function TItr.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TItr.Previous: TObject;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetObject(FCursor);
end;

function TItr.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TItr.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TItr.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TItr.SetObject(AObject: TObject);
begin
  CheckValid;
  FOwnList.SetObject(FCursor, AObject);
end;

{$IFDEF SUPPORTS_GENERICS}
type
  TItr<T> = class(TJclAbstractIterator, IJclIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FStart: TItrStart;
    FOwnList: IJclList<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
  public
    constructor Create(const AOwnList: IJclList<T>; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
  end;

//=== { TItr<T> } ===============================================================

constructor TItr<T>.Create(const AOwnList: IJclList<T>; ACursor: Integer; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FOwnList := AOwnList;
  FStart := AStart;
  FCursor := ACursor;
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
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
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
  Result := FOwnList.GetItem(FCursor);
end;

function TItr<T>.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TItr<T>.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TItr<T>.Insert(const AItem: T): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AItem);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TItr<T>.MoveNext: Boolean;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FCursor < FOwnList.Size;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TItr<T>.Next: T;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetItem(FCursor);
end;

function TItr<T>.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TItr<T>.Previous: T;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetItem(FCursor);
end;

function TItr<T>.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TItr<T>.Remove;
begin
  CheckValid;
  Valid := False;
  FOwnList.Delete(FCursor);
end;

procedure TItr<T>.Reset;
begin
  Valid := False;
  case FStart of
    isFirst:
      FCursor := 0;
    isLast:
      FCursor := FOwnList.Size - 1;
  end;
end;

procedure TItr<T>.SetItem(const AItem: T);
begin
  CheckValid;
  FOwnList.SetItem(FCursor, AItem);
end;
{$ENDIF SUPPORTS_GENERICS}

//=== { TJclIntfArrayList } ======================================================

constructor TJclIntfArrayList.Create(ACapacity: Integer);
begin
  inherited Create();
  FSize := 0;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FElementData, FCapacity);
end;

constructor TJclIntfArrayList.Create(const ACollection: IJclIntfCollection);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  Create();
  FSize := 0;
  FCapacity := ACollection.Size;
  SetLength(FElementData, FCapacity);
  AddAll(ACollection);
end;

destructor TJclIntfArrayList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntfArrayList.Add(const AInterface: IInterface): Boolean;
var
  Index: Integer;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AInterface, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FElementData[FSize] := AInterface;
          Inc(FSize);
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

function TJclIntfArrayList.AddAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  Item: IInterface;
  AddItem: Boolean;
  Index: Integer;
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
      // (rom) inlining Add() gives about 5 percent performance increase
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
          for Index := 0 to FSize - 1 do
            if ItemsEqual(Item, FElementData[Index]) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
        if AddItem then
        begin
          if FSize = FCapacity then
            AutoGrow;
          AddItem := FSize < FCapacity;
          if AddItem then
          begin
            FElementData[FSize] := Item;
            Inc(FSize);
          end;
        end;
      end;
      Result := Result and AddItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfArrayList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclIntfArrayList;
  ACollection: IJclIntfCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfArrayList then
  begin
    ADest := TJclIntfArrayList(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclIntfCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclIntfArrayList.Clear;
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
      FreeObject(FElementData[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.Contains(const AInterface: IInterface): Boolean;
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
      if ItemsEqual(FElementData[I], AInterface) then
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

function TJclIntfArrayList.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfArrayList.Delete(Index: Integer): IInterface;
  // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
  // complaining about possible unaffected result.
  function RaiseOutOfBoundsError: IInterface;
  begin
    raise EJclOutOfBoundsError.Create;
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FreeObject(FElementData[Index]);
      if Index < (FSize - 1) then
        JclBase.MoveArray(FElementData, Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.Equals(const ACollection: IJclIntfCollection): Boolean;
var
  I: Integer;
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
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FElementData[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.First: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntfArrayList.GetEnumerator: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntfArrayList.GetObject(Index: Integer): IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) or (Index < FSize) then
      Result := FElementData[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.IndexOf(const AInterface: IInterface): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElementData[I], AInterface) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.Insert(Index: Integer; const AInterface: IInterface): Boolean;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AInterface, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            JclBase.MoveArray(FElementData, Index, Index + 1, FSize - Index);
          FElementData[Index] := AInterface;
          Inc(FSize);
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

function TJclIntfArrayList.InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;

    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfArrayList.Last: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, FSize - 1, False, isLast);
end;

function TJclIntfArrayList.LastIndexOf(const AInterface: IInterface): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AInterface) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.Remove(const AInterface: IInterface): Boolean;
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
    Result := False;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AInterface) then
      begin
        FreeObject(FElementData[I]);
        if I < (FSize - 1) then
          JclBase.MoveArray(FElementData, I + 1, I, FSize - I - 1);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfArrayList.RetainAll(const ACollection: IJclIntfCollection): Boolean;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FElementData[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfArrayList.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FElementData, Value);
      inherited SetCapacity(Value);
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

procedure TJclIntfArrayList.SetObject(Index: Integer; const AInterface: IInterface);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AInterface, nil);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(FElementData[I], AInterface) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeObject(FElementData[Index]);
        FElementData[Index] := AInterface;
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

function TJclIntfArrayList.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfArrayList.SubList(First, Count: Integer): IJclIntfList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclIntfList;
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclAnsiStrArrayList } ======================================================

constructor TJclAnsiStrArrayList.Create(ACapacity: Integer);
begin
  inherited Create();
  FSize := 0;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FElementData, FCapacity);
end;

constructor TJclAnsiStrArrayList.Create(const ACollection: IJclAnsiStrCollection);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  Create();
  FSize := 0;
  FCapacity := ACollection.Size;
  SetLength(FElementData, FCapacity);
  AddAll(ACollection);
end;

destructor TJclAnsiStrArrayList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclAnsiStrArrayList.Add(const AString: AnsiString): Boolean;
var
  Index: Integer;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AString, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FElementData[FSize] := AString;
          Inc(FSize);
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

function TJclAnsiStrArrayList.AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
  Item: AnsiString;
  AddItem: Boolean;
  Index: Integer;
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
      // (rom) inlining Add() gives about 5 percent performance increase
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
          for Index := 0 to FSize - 1 do
            if ItemsEqual(Item, FElementData[Index]) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
        if AddItem then
        begin
          if FSize = FCapacity then
            AutoGrow;
          AddItem := FSize < FCapacity;
          if AddItem then
          begin
            FElementData[FSize] := Item;
            Inc(FSize);
          end;
        end;
      end;
      Result := Result and AddItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrArrayList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclAnsiStrArrayList;
  ACollection: IJclAnsiStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrArrayList then
  begin
    ADest := TJclAnsiStrArrayList(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclAnsiStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclAnsiStrArrayList.Clear;
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
      FreeString(FElementData[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrArrayList.Contains(const AString: AnsiString): Boolean;
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
      if ItemsEqual(FElementData[I], AString) then
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

function TJclAnsiStrArrayList.ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean;
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

function TJclAnsiStrArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrArrayList.Delete(Index: Integer): AnsiString;
  // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
  // complaining about possible unaffected result.
  function RaiseOutOfBoundsError: AnsiString;
  begin
    raise EJclOutOfBoundsError.Create;
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FreeString(FElementData[Index]);
      if Index < (FSize - 1) then
        JclBase.MoveArray(FElementData, Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrArrayList.Equals(const ACollection: IJclAnsiStrCollection): Boolean;
var
  I: Integer;
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
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FElementData[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrArrayList.First: IJclAnsiStrIterator;
begin
  Result := TAnsiStrItr.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclAnsiStrArrayList.GetEnumerator: IJclAnsiStrIterator;
begin
  Result := TAnsiStrItr.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclAnsiStrArrayList.GetString(Index: Integer): AnsiString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (Index >= 0) or (Index < FSize) then
      Result := FElementData[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrArrayList.IndexOf(const AString: AnsiString): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElementData[I], AString) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrArrayList.Insert(Index: Integer; const AString: AnsiString): Boolean;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AString, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            JclBase.MoveArray(FElementData, Index, Index + 1, FSize - Index);
          FElementData[Index] := AString;
          Inc(FSize);
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

function TJclAnsiStrArrayList.InsertAll(Index: Integer; const ACollection: IJclAnsiStrCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;

    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrArrayList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrArrayList.Last: IJclAnsiStrIterator;
begin
  Result := TAnsiStrItr.Create(Self, FSize - 1, False, isLast);
end;

function TJclAnsiStrArrayList.LastIndexOf(const AString: AnsiString): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AString) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrArrayList.Remove(const AString: AnsiString): Boolean;
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
    Result := False;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AString) then
      begin
        FreeString(FElementData[I]);
        if I < (FSize - 1) then
          JclBase.MoveArray(FElementData, I + 1, I, FSize - I - 1);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrArrayList.RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean;
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

function TJclAnsiStrArrayList.RetainAll(const ACollection: IJclAnsiStrCollection): Boolean;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FElementData[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrArrayList.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FElementData, Value);
      inherited SetCapacity(Value);
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

procedure TJclAnsiStrArrayList.SetString(Index: Integer; const AString: AnsiString);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AString, '');
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(FElementData[I], AString) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeString(FElementData[Index]);
        FElementData[Index] := AString;
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

function TJclAnsiStrArrayList.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrArrayList.SubList(First, Count: Integer): IJclAnsiStrList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclAnsiStrList;
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclWideStrArrayList } ======================================================

constructor TJclWideStrArrayList.Create(ACapacity: Integer);
begin
  inherited Create();
  FSize := 0;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FElementData, FCapacity);
end;

constructor TJclWideStrArrayList.Create(const ACollection: IJclWideStrCollection);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  Create();
  FSize := 0;
  FCapacity := ACollection.Size;
  SetLength(FElementData, FCapacity);
  AddAll(ACollection);
end;

destructor TJclWideStrArrayList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclWideStrArrayList.Add(const AString: WideString): Boolean;
var
  Index: Integer;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AString, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FElementData[FSize] := AString;
          Inc(FSize);
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

function TJclWideStrArrayList.AddAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
  Item: WideString;
  AddItem: Boolean;
  Index: Integer;
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
      // (rom) inlining Add() gives about 5 percent performance increase
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
          for Index := 0 to FSize - 1 do
            if ItemsEqual(Item, FElementData[Index]) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
        if AddItem then
        begin
          if FSize = FCapacity then
            AutoGrow;
          AddItem := FSize < FCapacity;
          if AddItem then
          begin
            FElementData[FSize] := Item;
            Inc(FSize);
          end;
        end;
      end;
      Result := Result and AddItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrArrayList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclWideStrArrayList;
  ACollection: IJclWideStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrArrayList then
  begin
    ADest := TJclWideStrArrayList(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclWideStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclWideStrArrayList.Clear;
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
      FreeString(FElementData[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrArrayList.Contains(const AString: WideString): Boolean;
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
      if ItemsEqual(FElementData[I], AString) then
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

function TJclWideStrArrayList.ContainsAll(const ACollection: IJclWideStrCollection): Boolean;
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

function TJclWideStrArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclWideStrArrayList.Delete(Index: Integer): WideString;
  // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
  // complaining about possible unaffected result.
  function RaiseOutOfBoundsError: WideString;
  begin
    raise EJclOutOfBoundsError.Create;
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FreeString(FElementData[Index]);
      if Index < (FSize - 1) then
        JclBase.MoveArray(FElementData, Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrArrayList.Equals(const ACollection: IJclWideStrCollection): Boolean;
var
  I: Integer;
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
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FElementData[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrArrayList.First: IJclWideStrIterator;
begin
  Result := TWideStrItr.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclWideStrArrayList.GetEnumerator: IJclWideStrIterator;
begin
  Result := TWideStrItr.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclWideStrArrayList.GetString(Index: Integer): WideString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (Index >= 0) or (Index < FSize) then
      Result := FElementData[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrArrayList.IndexOf(const AString: WideString): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElementData[I], AString) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrArrayList.Insert(Index: Integer; const AString: WideString): Boolean;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AString, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            JclBase.MoveArray(FElementData, Index, Index + 1, FSize - Index);
          FElementData[Index] := AString;
          Inc(FSize);
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

function TJclWideStrArrayList.InsertAll(Index: Integer; const ACollection: IJclWideStrCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;

    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrArrayList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrArrayList.Last: IJclWideStrIterator;
begin
  Result := TWideStrItr.Create(Self, FSize - 1, False, isLast);
end;

function TJclWideStrArrayList.LastIndexOf(const AString: WideString): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AString) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrArrayList.Remove(const AString: WideString): Boolean;
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
    Result := False;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AString) then
      begin
        FreeString(FElementData[I]);
        if I < (FSize - 1) then
          JclBase.MoveArray(FElementData, I + 1, I, FSize - I - 1);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrArrayList.RemoveAll(const ACollection: IJclWideStrCollection): Boolean;
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

function TJclWideStrArrayList.RetainAll(const ACollection: IJclWideStrCollection): Boolean;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FElementData[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrArrayList.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FElementData, Value);
      inherited SetCapacity(Value);
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

procedure TJclWideStrArrayList.SetString(Index: Integer; const AString: WideString);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AString, '');
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(FElementData[I], AString) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeString(FElementData[Index]);
        FElementData[Index] := AString;
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

function TJclWideStrArrayList.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrArrayList.SubList(First, Count: Integer): IJclWideStrList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclWideStrList;
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclSingleArrayList } ======================================================

constructor TJclSingleArrayList.Create(ACapacity: Integer);
begin
  inherited Create();
  FSize := 0;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FElementData, FCapacity);
end;

constructor TJclSingleArrayList.Create(const ACollection: IJclSingleCollection);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  Create();
  FSize := 0;
  FCapacity := ACollection.Size;
  SetLength(FElementData, FCapacity);
  AddAll(ACollection);
end;

destructor TJclSingleArrayList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclSingleArrayList.Add(const AValue: Single): Boolean;
var
  Index: Integer;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FElementData[FSize] := AValue;
          Inc(FSize);
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

function TJclSingleArrayList.AddAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
  Item: Single;
  AddItem: Boolean;
  Index: Integer;
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
      // (rom) inlining Add() gives about 5 percent performance increase
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
          for Index := 0 to FSize - 1 do
            if ItemsEqual(Item, FElementData[Index]) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
        if AddItem then
        begin
          if FSize = FCapacity then
            AutoGrow;
          AddItem := FSize < FCapacity;
          if AddItem then
          begin
            FElementData[FSize] := Item;
            Inc(FSize);
          end;
        end;
      end;
      Result := Result and AddItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleArrayList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclSingleArrayList;
  ACollection: IJclSingleCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclSingleArrayList then
  begin
    ADest := TJclSingleArrayList(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclSingleCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclSingleArrayList.Clear;
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
      FreeSingle(FElementData[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleArrayList.Contains(const AValue: Single): Boolean;
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
      if ItemsEqual(FElementData[I], AValue) then
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

function TJclSingleArrayList.ContainsAll(const ACollection: IJclSingleCollection): Boolean;
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

function TJclSingleArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclSingleArrayList.Delete(Index: Integer): Single;
  // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
  // complaining about possible unaffected result.
  function RaiseOutOfBoundsError: Single;
  begin
    raise EJclOutOfBoundsError.Create;
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FreeSingle(FElementData[Index]);
      if Index < (FSize - 1) then
        JclBase.MoveArray(FElementData, Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleArrayList.Equals(const ACollection: IJclSingleCollection): Boolean;
var
  I: Integer;
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
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FElementData[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleArrayList.First: IJclSingleIterator;
begin
  Result := TSingleItr.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclSingleArrayList.GetEnumerator: IJclSingleIterator;
begin
  Result := TSingleItr.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclSingleArrayList.GetValue(Index: Integer): Single;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (Index >= 0) or (Index < FSize) then
      Result := FElementData[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleArrayList.IndexOf(const AValue: Single): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleArrayList.Insert(Index: Integer; const AValue: Single): Boolean;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            JclBase.MoveArray(FElementData, Index, Index + 1, FSize - Index);
          FElementData[Index] := AValue;
          Inc(FSize);
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

function TJclSingleArrayList.InsertAll(Index: Integer; const ACollection: IJclSingleCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;

    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleArrayList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleArrayList.Last: IJclSingleIterator;
begin
  Result := TSingleItr.Create(Self, FSize - 1, False, isLast);
end;

function TJclSingleArrayList.LastIndexOf(const AValue: Single): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleArrayList.Remove(const AValue: Single): Boolean;
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
    Result := False;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        FreeSingle(FElementData[I]);
        if I < (FSize - 1) then
          JclBase.MoveArray(FElementData, I + 1, I, FSize - I - 1);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleArrayList.RemoveAll(const ACollection: IJclSingleCollection): Boolean;
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

function TJclSingleArrayList.RetainAll(const ACollection: IJclSingleCollection): Boolean;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FElementData[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleArrayList.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FElementData, Value);
      inherited SetCapacity(Value);
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

procedure TJclSingleArrayList.SetValue(Index: Integer; const AValue: Single);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(FElementData[I], AValue) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeSingle(FElementData[Index]);
        FElementData[Index] := AValue;
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

function TJclSingleArrayList.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleArrayList.SubList(First, Count: Integer): IJclSingleList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclSingleList;
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclDoubleArrayList } ======================================================

constructor TJclDoubleArrayList.Create(ACapacity: Integer);
begin
  inherited Create();
  FSize := 0;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FElementData, FCapacity);
end;

constructor TJclDoubleArrayList.Create(const ACollection: IJclDoubleCollection);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  Create();
  FSize := 0;
  FCapacity := ACollection.Size;
  SetLength(FElementData, FCapacity);
  AddAll(ACollection);
end;

destructor TJclDoubleArrayList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclDoubleArrayList.Add(const AValue: Double): Boolean;
var
  Index: Integer;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FElementData[FSize] := AValue;
          Inc(FSize);
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

function TJclDoubleArrayList.AddAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
  Item: Double;
  AddItem: Boolean;
  Index: Integer;
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
      // (rom) inlining Add() gives about 5 percent performance increase
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
          for Index := 0 to FSize - 1 do
            if ItemsEqual(Item, FElementData[Index]) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
        if AddItem then
        begin
          if FSize = FCapacity then
            AutoGrow;
          AddItem := FSize < FCapacity;
          if AddItem then
          begin
            FElementData[FSize] := Item;
            Inc(FSize);
          end;
        end;
      end;
      Result := Result and AddItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleArrayList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclDoubleArrayList;
  ACollection: IJclDoubleCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclDoubleArrayList then
  begin
    ADest := TJclDoubleArrayList(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclDoubleCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclDoubleArrayList.Clear;
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
      FreeDouble(FElementData[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleArrayList.Contains(const AValue: Double): Boolean;
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
      if ItemsEqual(FElementData[I], AValue) then
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

function TJclDoubleArrayList.ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
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

function TJclDoubleArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclDoubleArrayList.Delete(Index: Integer): Double;
  // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
  // complaining about possible unaffected result.
  function RaiseOutOfBoundsError: Double;
  begin
    raise EJclOutOfBoundsError.Create;
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FreeDouble(FElementData[Index]);
      if Index < (FSize - 1) then
        JclBase.MoveArray(FElementData, Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleArrayList.Equals(const ACollection: IJclDoubleCollection): Boolean;
var
  I: Integer;
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
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FElementData[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleArrayList.First: IJclDoubleIterator;
begin
  Result := TDoubleItr.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclDoubleArrayList.GetEnumerator: IJclDoubleIterator;
begin
  Result := TDoubleItr.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclDoubleArrayList.GetValue(Index: Integer): Double;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (Index >= 0) or (Index < FSize) then
      Result := FElementData[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleArrayList.IndexOf(const AValue: Double): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleArrayList.Insert(Index: Integer; const AValue: Double): Boolean;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            JclBase.MoveArray(FElementData, Index, Index + 1, FSize - Index);
          FElementData[Index] := AValue;
          Inc(FSize);
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

function TJclDoubleArrayList.InsertAll(Index: Integer; const ACollection: IJclDoubleCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;

    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleArrayList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleArrayList.Last: IJclDoubleIterator;
begin
  Result := TDoubleItr.Create(Self, FSize - 1, False, isLast);
end;

function TJclDoubleArrayList.LastIndexOf(const AValue: Double): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleArrayList.Remove(const AValue: Double): Boolean;
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
    Result := False;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        FreeDouble(FElementData[I]);
        if I < (FSize - 1) then
          JclBase.MoveArray(FElementData, I + 1, I, FSize - I - 1);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleArrayList.RemoveAll(const ACollection: IJclDoubleCollection): Boolean;
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

function TJclDoubleArrayList.RetainAll(const ACollection: IJclDoubleCollection): Boolean;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FElementData[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleArrayList.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FElementData, Value);
      inherited SetCapacity(Value);
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

procedure TJclDoubleArrayList.SetValue(Index: Integer; const AValue: Double);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(FElementData[I], AValue) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeDouble(FElementData[Index]);
        FElementData[Index] := AValue;
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

function TJclDoubleArrayList.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleArrayList.SubList(First, Count: Integer): IJclDoubleList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclDoubleList;
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclExtendedArrayList } ======================================================

constructor TJclExtendedArrayList.Create(ACapacity: Integer);
begin
  inherited Create();
  FSize := 0;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FElementData, FCapacity);
end;

constructor TJclExtendedArrayList.Create(const ACollection: IJclExtendedCollection);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  Create();
  FSize := 0;
  FCapacity := ACollection.Size;
  SetLength(FElementData, FCapacity);
  AddAll(ACollection);
end;

destructor TJclExtendedArrayList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclExtendedArrayList.Add(const AValue: Extended): Boolean;
var
  Index: Integer;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FElementData[FSize] := AValue;
          Inc(FSize);
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

function TJclExtendedArrayList.AddAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
  Item: Extended;
  AddItem: Boolean;
  Index: Integer;
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
      // (rom) inlining Add() gives about 5 percent performance increase
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0.0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
          for Index := 0 to FSize - 1 do
            if ItemsEqual(Item, FElementData[Index]) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
        if AddItem then
        begin
          if FSize = FCapacity then
            AutoGrow;
          AddItem := FSize < FCapacity;
          if AddItem then
          begin
            FElementData[FSize] := Item;
            Inc(FSize);
          end;
        end;
      end;
      Result := Result and AddItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedArrayList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclExtendedArrayList;
  ACollection: IJclExtendedCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclExtendedArrayList then
  begin
    ADest := TJclExtendedArrayList(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclExtendedCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclExtendedArrayList.Clear;
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
      FreeExtended(FElementData[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedArrayList.Contains(const AValue: Extended): Boolean;
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
      if ItemsEqual(FElementData[I], AValue) then
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

function TJclExtendedArrayList.ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
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

function TJclExtendedArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclExtendedArrayList.Delete(Index: Integer): Extended;
  // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
  // complaining about possible unaffected result.
  function RaiseOutOfBoundsError: Extended;
  begin
    raise EJclOutOfBoundsError.Create;
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FreeExtended(FElementData[Index]);
      if Index < (FSize - 1) then
        JclBase.MoveArray(FElementData, Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedArrayList.Equals(const ACollection: IJclExtendedCollection): Boolean;
var
  I: Integer;
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
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FElementData[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedArrayList.First: IJclExtendedIterator;
begin
  Result := TExtendedItr.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclExtendedArrayList.GetEnumerator: IJclExtendedIterator;
begin
  Result := TExtendedItr.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclExtendedArrayList.GetValue(Index: Integer): Extended;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (Index >= 0) or (Index < FSize) then
      Result := FElementData[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedArrayList.IndexOf(const AValue: Extended): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedArrayList.Insert(Index: Integer; const AValue: Extended): Boolean;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            JclBase.MoveArray(FElementData, Index, Index + 1, FSize - Index);
          FElementData[Index] := AValue;
          Inc(FSize);
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

function TJclExtendedArrayList.InsertAll(Index: Integer; const ACollection: IJclExtendedCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;

    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedArrayList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedArrayList.Last: IJclExtendedIterator;
begin
  Result := TExtendedItr.Create(Self, FSize - 1, False, isLast);
end;

function TJclExtendedArrayList.LastIndexOf(const AValue: Extended): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedArrayList.Remove(const AValue: Extended): Boolean;
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
    Result := False;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        FreeExtended(FElementData[I]);
        if I < (FSize - 1) then
          JclBase.MoveArray(FElementData, I + 1, I, FSize - I - 1);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedArrayList.RemoveAll(const ACollection: IJclExtendedCollection): Boolean;
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

function TJclExtendedArrayList.RetainAll(const ACollection: IJclExtendedCollection): Boolean;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FElementData[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedArrayList.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FElementData, Value);
      inherited SetCapacity(Value);
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

procedure TJclExtendedArrayList.SetValue(Index: Integer; const AValue: Extended);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0.0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(FElementData[I], AValue) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeExtended(FElementData[Index]);
        FElementData[Index] := AValue;
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

function TJclExtendedArrayList.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedArrayList.SubList(First, Count: Integer): IJclExtendedList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclExtendedList;
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclIntegerArrayList } ======================================================

constructor TJclIntegerArrayList.Create(ACapacity: Integer);
begin
  inherited Create();
  FSize := 0;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FElementData, FCapacity);
end;

constructor TJclIntegerArrayList.Create(const ACollection: IJclIntegerCollection);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  Create();
  FSize := 0;
  FCapacity := ACollection.Size;
  SetLength(FElementData, FCapacity);
  AddAll(ACollection);
end;

destructor TJclIntegerArrayList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntegerArrayList.Add(AValue: Integer): Boolean;
var
  Index: Integer;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FElementData[FSize] := AValue;
          Inc(FSize);
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

function TJclIntegerArrayList.AddAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
  Item: Integer;
  AddItem: Boolean;
  Index: Integer;
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
      // (rom) inlining Add() gives about 5 percent performance increase
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
          for Index := 0 to FSize - 1 do
            if ItemsEqual(Item, FElementData[Index]) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
        if AddItem then
        begin
          if FSize = FCapacity then
            AutoGrow;
          AddItem := FSize < FCapacity;
          if AddItem then
          begin
            FElementData[FSize] := Item;
            Inc(FSize);
          end;
        end;
      end;
      Result := Result and AddItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerArrayList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclIntegerArrayList;
  ACollection: IJclIntegerCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntegerArrayList then
  begin
    ADest := TJclIntegerArrayList(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclIntegerCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclIntegerArrayList.Clear;
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
      FreeInteger(FElementData[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerArrayList.Contains(AValue: Integer): Boolean;
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
      if ItemsEqual(FElementData[I], AValue) then
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

function TJclIntegerArrayList.ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
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

function TJclIntegerArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntegerArrayList.Delete(Index: Integer): Integer;
  // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
  // complaining about possible unaffected result.
  function RaiseOutOfBoundsError: Integer;
  begin
    raise EJclOutOfBoundsError.Create;
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FreeInteger(FElementData[Index]);
      if Index < (FSize - 1) then
        JclBase.MoveArray(FElementData, Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerArrayList.Equals(const ACollection: IJclIntegerCollection): Boolean;
var
  I: Integer;
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
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FElementData[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerArrayList.First: IJclIntegerIterator;
begin
  Result := TIntegerItr.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntegerArrayList.GetEnumerator: IJclIntegerIterator;
begin
  Result := TIntegerItr.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntegerArrayList.GetValue(Index: Integer): Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (Index >= 0) or (Index < FSize) then
      Result := FElementData[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerArrayList.IndexOf(AValue: Integer): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerArrayList.Insert(Index: Integer; AValue: Integer): Boolean;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            JclBase.MoveArray(FElementData, Index, Index + 1, FSize - Index);
          FElementData[Index] := AValue;
          Inc(FSize);
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

function TJclIntegerArrayList.InsertAll(Index: Integer; const ACollection: IJclIntegerCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;

    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerArrayList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerArrayList.Last: IJclIntegerIterator;
begin
  Result := TIntegerItr.Create(Self, FSize - 1, False, isLast);
end;

function TJclIntegerArrayList.LastIndexOf(AValue: Integer): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerArrayList.Remove(AValue: Integer): Boolean;
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
    Result := False;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        FreeInteger(FElementData[I]);
        if I < (FSize - 1) then
          JclBase.MoveArray(FElementData, I + 1, I, FSize - I - 1);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerArrayList.RemoveAll(const ACollection: IJclIntegerCollection): Boolean;
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

function TJclIntegerArrayList.RetainAll(const ACollection: IJclIntegerCollection): Boolean;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FElementData[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerArrayList.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FElementData, Value);
      inherited SetCapacity(Value);
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

procedure TJclIntegerArrayList.SetValue(Index: Integer; AValue: Integer);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(FElementData[I], AValue) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeInteger(FElementData[Index]);
        FElementData[Index] := AValue;
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

function TJclIntegerArrayList.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerArrayList.SubList(First, Count: Integer): IJclIntegerList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclIntegerList;
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclCardinalArrayList } ======================================================

constructor TJclCardinalArrayList.Create(ACapacity: Integer);
begin
  inherited Create();
  FSize := 0;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FElementData, FCapacity);
end;

constructor TJclCardinalArrayList.Create(const ACollection: IJclCardinalCollection);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  Create();
  FSize := 0;
  FCapacity := ACollection.Size;
  SetLength(FElementData, FCapacity);
  AddAll(ACollection);
end;

destructor TJclCardinalArrayList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclCardinalArrayList.Add(AValue: Cardinal): Boolean;
var
  Index: Integer;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FElementData[FSize] := AValue;
          Inc(FSize);
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

function TJclCardinalArrayList.AddAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
  Item: Cardinal;
  AddItem: Boolean;
  Index: Integer;
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
      // (rom) inlining Add() gives about 5 percent performance increase
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
          for Index := 0 to FSize - 1 do
            if ItemsEqual(Item, FElementData[Index]) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
        if AddItem then
        begin
          if FSize = FCapacity then
            AutoGrow;
          AddItem := FSize < FCapacity;
          if AddItem then
          begin
            FElementData[FSize] := Item;
            Inc(FSize);
          end;
        end;
      end;
      Result := Result and AddItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalArrayList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclCardinalArrayList;
  ACollection: IJclCardinalCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclCardinalArrayList then
  begin
    ADest := TJclCardinalArrayList(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclCardinalCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclCardinalArrayList.Clear;
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
      FreeCardinal(FElementData[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalArrayList.Contains(AValue: Cardinal): Boolean;
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
      if ItemsEqual(FElementData[I], AValue) then
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

function TJclCardinalArrayList.ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
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

function TJclCardinalArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclCardinalArrayList.Delete(Index: Integer): Cardinal;
  // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
  // complaining about possible unaffected result.
  function RaiseOutOfBoundsError: Cardinal;
  begin
    raise EJclOutOfBoundsError.Create;
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FreeCardinal(FElementData[Index]);
      if Index < (FSize - 1) then
        JclBase.MoveArray(FElementData, Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalArrayList.Equals(const ACollection: IJclCardinalCollection): Boolean;
var
  I: Integer;
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
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FElementData[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalArrayList.First: IJclCardinalIterator;
begin
  Result := TCardinalItr.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclCardinalArrayList.GetEnumerator: IJclCardinalIterator;
begin
  Result := TCardinalItr.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclCardinalArrayList.GetValue(Index: Integer): Cardinal;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (Index >= 0) or (Index < FSize) then
      Result := FElementData[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalArrayList.IndexOf(AValue: Cardinal): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalArrayList.Insert(Index: Integer; AValue: Cardinal): Boolean;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            JclBase.MoveArray(FElementData, Index, Index + 1, FSize - Index);
          FElementData[Index] := AValue;
          Inc(FSize);
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

function TJclCardinalArrayList.InsertAll(Index: Integer; const ACollection: IJclCardinalCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;

    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalArrayList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalArrayList.Last: IJclCardinalIterator;
begin
  Result := TCardinalItr.Create(Self, FSize - 1, False, isLast);
end;

function TJclCardinalArrayList.LastIndexOf(AValue: Cardinal): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalArrayList.Remove(AValue: Cardinal): Boolean;
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
    Result := False;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        FreeCardinal(FElementData[I]);
        if I < (FSize - 1) then
          JclBase.MoveArray(FElementData, I + 1, I, FSize - I - 1);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalArrayList.RemoveAll(const ACollection: IJclCardinalCollection): Boolean;
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

function TJclCardinalArrayList.RetainAll(const ACollection: IJclCardinalCollection): Boolean;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FElementData[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalArrayList.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FElementData, Value);
      inherited SetCapacity(Value);
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

procedure TJclCardinalArrayList.SetValue(Index: Integer; AValue: Cardinal);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(FElementData[I], AValue) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeCardinal(FElementData[Index]);
        FElementData[Index] := AValue;
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

function TJclCardinalArrayList.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalArrayList.SubList(First, Count: Integer): IJclCardinalList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclCardinalList;
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclInt64ArrayList } ======================================================

constructor TJclInt64ArrayList.Create(ACapacity: Integer);
begin
  inherited Create();
  FSize := 0;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FElementData, FCapacity);
end;

constructor TJclInt64ArrayList.Create(const ACollection: IJclInt64Collection);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  Create();
  FSize := 0;
  FCapacity := ACollection.Size;
  SetLength(FElementData, FCapacity);
  AddAll(ACollection);
end;

destructor TJclInt64ArrayList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclInt64ArrayList.Add(const AValue: Int64): Boolean;
var
  Index: Integer;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FElementData[FSize] := AValue;
          Inc(FSize);
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

function TJclInt64ArrayList.AddAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
  Item: Int64;
  AddItem: Boolean;
  Index: Integer;
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
      // (rom) inlining Add() gives about 5 percent performance increase
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, 0);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
          for Index := 0 to FSize - 1 do
            if ItemsEqual(Item, FElementData[Index]) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
        if AddItem then
        begin
          if FSize = FCapacity then
            AutoGrow;
          AddItem := FSize < FCapacity;
          if AddItem then
          begin
            FElementData[FSize] := Item;
            Inc(FSize);
          end;
        end;
      end;
      Result := Result and AddItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64ArrayList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclInt64ArrayList;
  ACollection: IJclInt64Collection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclInt64ArrayList then
  begin
    ADest := TJclInt64ArrayList(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclInt64Collection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclInt64ArrayList.Clear;
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
      FreeInt64(FElementData[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64ArrayList.Contains(const AValue: Int64): Boolean;
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
      if ItemsEqual(FElementData[I], AValue) then
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

function TJclInt64ArrayList.ContainsAll(const ACollection: IJclInt64Collection): Boolean;
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

function TJclInt64ArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64ArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclInt64ArrayList.Delete(Index: Integer): Int64;
  // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
  // complaining about possible unaffected result.
  function RaiseOutOfBoundsError: Int64;
  begin
    raise EJclOutOfBoundsError.Create;
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FreeInt64(FElementData[Index]);
      if Index < (FSize - 1) then
        JclBase.MoveArray(FElementData, Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64ArrayList.Equals(const ACollection: IJclInt64Collection): Boolean;
var
  I: Integer;
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
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FElementData[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64ArrayList.First: IJclInt64Iterator;
begin
  Result := TInt64Itr.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclInt64ArrayList.GetEnumerator: IJclInt64Iterator;
begin
  Result := TInt64Itr.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclInt64ArrayList.GetValue(Index: Integer): Int64;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (Index >= 0) or (Index < FSize) then
      Result := FElementData[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64ArrayList.IndexOf(const AValue: Int64): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64ArrayList.Insert(Index: Integer; const AValue: Int64): Boolean;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AValue, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            JclBase.MoveArray(FElementData, Index, Index + 1, FSize - Index);
          FElementData[Index] := AValue;
          Inc(FSize);
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

function TJclInt64ArrayList.InsertAll(Index: Integer; const ACollection: IJclInt64Collection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;

    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64ArrayList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64ArrayList.Last: IJclInt64Iterator;
begin
  Result := TInt64Itr.Create(Self, FSize - 1, False, isLast);
end;

function TJclInt64ArrayList.LastIndexOf(const AValue: Int64): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64ArrayList.Remove(const AValue: Int64): Boolean;
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
    Result := False;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AValue) then
      begin
        FreeInt64(FElementData[I]);
        if I < (FSize - 1) then
          JclBase.MoveArray(FElementData, I + 1, I, FSize - I - 1);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64ArrayList.RemoveAll(const ACollection: IJclInt64Collection): Boolean;
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

function TJclInt64ArrayList.RetainAll(const ACollection: IJclInt64Collection): Boolean;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FElementData[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64ArrayList.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FElementData, Value);
      inherited SetCapacity(Value);
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

procedure TJclInt64ArrayList.SetValue(Index: Integer; const AValue: Int64);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AValue, 0);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(FElementData[I], AValue) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeInt64(FElementData[Index]);
        FElementData[Index] := AValue;
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

function TJclInt64ArrayList.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64ArrayList.SubList(First, Count: Integer): IJclInt64List;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclInt64List;
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFNDEF CLR}
//=== { TJclPtrArrayList } ======================================================

constructor TJclPtrArrayList.Create(ACapacity: Integer);
begin
  inherited Create();
  FSize := 0;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FElementData, FCapacity);
end;

constructor TJclPtrArrayList.Create(const ACollection: IJclPtrCollection);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  Create();
  FSize := 0;
  FCapacity := ACollection.Size;
  SetLength(FElementData, FCapacity);
  AddAll(ACollection);
end;

destructor TJclPtrArrayList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclPtrArrayList.Add(APtr: Pointer): Boolean;
var
  Index: Integer;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(APtr, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FElementData[FSize] := APtr;
          Inc(FSize);
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

function TJclPtrArrayList.AddAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
  Item: Pointer;
  AddItem: Boolean;
  Index: Integer;
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
      // (rom) inlining Add() gives about 5 percent performance increase
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
          for Index := 0 to FSize - 1 do
            if ItemsEqual(Item, FElementData[Index]) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
        if AddItem then
        begin
          if FSize = FCapacity then
            AutoGrow;
          AddItem := FSize < FCapacity;
          if AddItem then
          begin
            FElementData[FSize] := Item;
            Inc(FSize);
          end;
        end;
      end;
      Result := Result and AddItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrArrayList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclPtrArrayList;
  ACollection: IJclPtrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclPtrArrayList then
  begin
    ADest := TJclPtrArrayList(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclPtrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclPtrArrayList.Clear;
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
      FreePointer(FElementData[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrArrayList.Contains(APtr: Pointer): Boolean;
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
      if ItemsEqual(FElementData[I], APtr) then
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

function TJclPtrArrayList.ContainsAll(const ACollection: IJclPtrCollection): Boolean;
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

function TJclPtrArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclPtrArrayList.Delete(Index: Integer): Pointer;
  // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
  // complaining about possible unaffected result.
  function RaiseOutOfBoundsError: Pointer;
  begin
    raise EJclOutOfBoundsError.Create;
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FreePointer(FElementData[Index]);
      if Index < (FSize - 1) then
        JclBase.MoveArray(FElementData, Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrArrayList.Equals(const ACollection: IJclPtrCollection): Boolean;
var
  I: Integer;
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
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FElementData[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrArrayList.First: IJclPtrIterator;
begin
  Result := TPtrItr.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclPtrArrayList.GetEnumerator: IJclPtrIterator;
begin
  Result := TPtrItr.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclPtrArrayList.GetPointer(Index: Integer): Pointer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) or (Index < FSize) then
      Result := FElementData[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrArrayList.IndexOf(APtr: Pointer): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElementData[I], APtr) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrArrayList.Insert(Index: Integer; APtr: Pointer): Boolean;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(APtr, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            JclBase.MoveArray(FElementData, Index, Index + 1, FSize - Index);
          FElementData[Index] := APtr;
          Inc(FSize);
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

function TJclPtrArrayList.InsertAll(Index: Integer; const ACollection: IJclPtrCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;

    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrArrayList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrArrayList.Last: IJclPtrIterator;
begin
  Result := TPtrItr.Create(Self, FSize - 1, False, isLast);
end;

function TJclPtrArrayList.LastIndexOf(APtr: Pointer): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], APtr) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrArrayList.Remove(APtr: Pointer): Boolean;
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
    Result := False;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], APtr) then
      begin
        FreePointer(FElementData[I]);
        if I < (FSize - 1) then
          JclBase.MoveArray(FElementData, I + 1, I, FSize - I - 1);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrArrayList.RemoveAll(const ACollection: IJclPtrCollection): Boolean;
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

function TJclPtrArrayList.RetainAll(const ACollection: IJclPtrCollection): Boolean;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FElementData[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrArrayList.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FElementData, Value);
      inherited SetCapacity(Value);
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

procedure TJclPtrArrayList.SetPointer(Index: Integer; APtr: Pointer);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(APtr, nil);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(FElementData[I], APtr) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreePointer(FElementData[Index]);
        FElementData[Index] := APtr;
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

function TJclPtrArrayList.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrArrayList.SubList(First, Count: Integer): IJclPtrList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclPtrList;
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF ~CLR}

//=== { TJclArrayList } ======================================================

constructor TJclArrayList.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  FSize := 0;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FElementData, FCapacity);
end;

constructor TJclArrayList.Create(const ACollection: IJclCollection; AOwnsObjects: Boolean);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  Create(AOwnsObjects);
  FSize := 0;
  FCapacity := ACollection.Size;
  SetLength(FElementData, FCapacity);
  AddAll(ACollection);
end;

destructor TJclArrayList.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclArrayList.Add(AObject: TObject): Boolean;
var
  Index: Integer;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AObject, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FElementData[FSize] := AObject;
          Inc(FSize);
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

function TJclArrayList.AddAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  Item: TObject;
  AddItem: Boolean;
  Index: Integer;
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
      // (rom) inlining Add() gives about 5 percent performance increase
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
          for Index := 0 to FSize - 1 do
            if ItemsEqual(Item, FElementData[Index]) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
        if AddItem then
        begin
          if FSize = FCapacity then
            AutoGrow;
          AddItem := FSize < FCapacity;
          if AddItem then
          begin
            FElementData[FSize] := Item;
            Inc(FSize);
          end;
        end;
      end;
      Result := Result and AddItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclArrayList;
  ACollection: IJclCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclArrayList then
  begin
    ADest := TJclArrayList(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclArrayList.Clear;
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
      FreeObject(FElementData[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.Contains(AObject: TObject): Boolean;
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
      if ItemsEqual(FElementData[I], AObject) then
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

function TJclArrayList.ContainsAll(const ACollection: IJclCollection): Boolean;
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

function TJclArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArrayList.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclArrayList.Delete(Index: Integer): TObject;
  // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
  // complaining about possible unaffected result.
  function RaiseOutOfBoundsError: TObject;
  begin
    raise EJclOutOfBoundsError.Create;
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FreeObject(FElementData[Index]);
      if Index < (FSize - 1) then
        JclBase.MoveArray(FElementData, Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.Equals(const ACollection: IJclCollection): Boolean;
var
  I: Integer;
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
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FElementData[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.First: IJclIterator;
begin
  Result := TItr.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclArrayList.GetEnumerator: IJclIterator;
begin
  Result := TItr.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclArrayList.GetObject(Index: Integer): TObject;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) or (Index < FSize) then
      Result := FElementData[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.IndexOf(AObject: TObject): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElementData[I], AObject) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.Insert(Index: Integer; AObject: TObject): Boolean;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AObject, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            JclBase.MoveArray(FElementData, Index, Index + 1, FSize - Index);
          FElementData[Index] := AObject;
          Inc(FSize);
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

function TJclArrayList.InsertAll(Index: Integer; const ACollection: IJclCollection): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;

    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclArrayList.Last: IJclIterator;
begin
  Result := TItr.Create(Self, FSize - 1, False, isLast);
end;

function TJclArrayList.LastIndexOf(AObject: TObject): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AObject) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.Remove(AObject: TObject): Boolean;
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
    Result := False;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AObject) then
      begin
        FreeObject(FElementData[I]);
        if I < (FSize - 1) then
          JclBase.MoveArray(FElementData, I + 1, I, FSize - I - 1);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.RemoveAll(const ACollection: IJclCollection): Boolean;
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

function TJclArrayList.RetainAll(const ACollection: IJclCollection): Boolean;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FElementData[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FElementData, Value);
      inherited SetCapacity(Value);
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

procedure TJclArrayList.SetObject(Index: Integer; AObject: TObject);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AObject, nil);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(FElementData[I], AObject) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeObject(FElementData[Index]);
        FElementData[Index] := AObject;
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

function TJclArrayList.Size: Integer;
begin
  Result := FSize;
end;

function TJclArrayList.SubList(First, Count: Integer): IJclList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclList;
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_GENERICS}
//=== { TJclArrayList<T> } ======================================================

constructor TJclArrayList<T>.Create(ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FSize := 0;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FElementData, FCapacity);
end;

constructor TJclArrayList<T>.Create(const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  Create(AOwnsItems);
  FSize := 0;
  FCapacity := ACollection.Size;
  SetLength(FElementData, FCapacity);
  AddAll(ACollection);
end;

destructor TJclArrayList<T>.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclArrayList<T>.Add(const AItem: T): Boolean;
var
  Index: Integer;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AItem, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FElementData[FSize] := AItem;
          Inc(FSize);
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

function TJclArrayList<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
  Item: T;
  AddItem: Boolean;
  Index: Integer;
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
      // (rom) inlining Add() gives about 5 percent performance increase
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, Default(T));
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
          for Index := 0 to FSize - 1 do
            if ItemsEqual(Item, FElementData[Index]) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
        if AddItem then
        begin
          if FSize = FCapacity then
            AutoGrow;
          AddItem := FSize < FCapacity;
          if AddItem then
          begin
            FElementData[FSize] := Item;
            Inc(FSize);
          end;
        end;
      end;
      Result := Result and AddItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList<T>.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclArrayList<T>;
  ACollection: IJclCollection<T>;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclArrayList<T> then
  begin
    ADest := TJclArrayList<T>(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclCollection<T>, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclArrayList<T>.Clear;
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
      FreeItem(FElementData[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.Contains(const AItem: T): Boolean;
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
      if ItemsEqual(FElementData[I], AItem) then
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

function TJclArrayList<T>.ContainsAll(const ACollection: IJclCollection<T>): Boolean;
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


function TJclArrayList<T>.Delete(Index: Integer): T;
  // fix ambiguous warnings when compiled on Delphi.net and earlier versions of Delphi.win32
  // complaining about possible unaffected result.
  function RaiseOutOfBoundsError: T;
  begin
    raise EJclOutOfBoundsError.Create;
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) and (Index < FSize) then
    begin
      Result := FreeItem(FElementData[Index]);
      if Index < (FSize - 1) then
        TJclBase<T>.MoveArray(FElementData, Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := RaiseOutOfBoundsError;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.Equals(const ACollection: IJclCollection<T>): Boolean;
var
  I: Integer;
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
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if not ItemsEqual(FElementData[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.First: IJclIterator<T>;
begin
  Result := TItr<T>.Create(Self, 0, False, isFirst);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclArrayList<T>.GetEnumerator: IJclIterator<T>;
begin
  Result := TItr<T>.Create(Self, 0, False, isFirst);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclArrayList<T>.GetItem(Index: Integer): T;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if (Index >= 0) or (Index < FSize) then
      Result := FElementData[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.IndexOf(const AItem: T): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElementData[I], AItem) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.Insert(Index: Integer; const AItem: T): Boolean;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AItem, FElementData[Index]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;

      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          if Index < FSize then
            TJclBase<T>.MoveArray(FElementData, Index, Index + 1, FSize - Index);
          FElementData[Index] := AItem;
          Inc(FSize);
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

function TJclArrayList<T>.InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean;
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
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;

    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclArrayList<T>.Last: IJclIterator<T>;
begin
  Result := TItr<T>.Create(Self, FSize - 1, False, isLast);
end;

function TJclArrayList<T>.LastIndexOf(const AItem: T): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AItem) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.Remove(const AItem: T): Boolean;
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
    Result := False;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AItem) then
      begin
        FreeItem(FElementData[I]);
        if I < (FSize - 1) then
          TJclBase<T>.MoveArray(FElementData, I + 1, I, FSize - I - 1);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.RemoveAll(const ACollection: IJclCollection<T>): Boolean;
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

function TJclArrayList<T>.RetainAll(const ACollection: IJclCollection<T>): Boolean;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FElementData[I]) then
        Delete(I);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList<T>.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FElementData, Value);
      inherited SetCapacity(Value);
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

procedure TJclArrayList<T>.SetItem(Index: Integer; const AItem: T);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AItem, Default(T));
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(FElementData[I], AItem) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeItem(FElementData[Index]);
        FElementData[Index] := AItem;
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

function TJclArrayList<T>.Size: Integer;
begin
  Result := FSize;
end;

function TJclArrayList<T>.SubList(First, Count: Integer): IJclList<T>;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclList<T>;
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

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

