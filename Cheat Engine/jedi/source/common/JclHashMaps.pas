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
{ The Original Code is HashMap.pas.                                                                }
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
{ Last modified: $Date:: 2008-07-20 11:13:23 +0200 (dim., 20 juil. 2008)                        $ }
{ Revision:      $Rev:: 2393                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclHashMaps;

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
  // Hash Function
  // Result must be in 0..Range-1
  TJclHashFunction = function(Key, Range: Integer): Integer;

  TJclIntfIntfHashEntry = record
    Key: IInterface;
    Value: IInterface;
  end;

  TJclIntfIntfHashEntryArray = array of TJclIntfIntfHashEntry;

  TJclIntfIntfBucket = class
  public
    Size: Integer;
    Entries: TJclIntfIntfHashEntryArray;
  end;

  TJclIntfIntfBucketArray = array of TJclIntfIntfBucket;

  TJclIntfIntfHashMap = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntfIntfMap)
  private
    FBuckets: TJclIntfIntfBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclIntfIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfIntfMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclIntfIntfMap): Boolean;
    function GetValue(const Key: IInterface): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfIntfMap);
    procedure PutValue(const Key: IInterface; const Value: IInterface);
    function Remove(const Key: IInterface): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclAnsiStrIntfHashEntry = record
    Key: AnsiString;
    Value: IInterface;
  end;

  TJclAnsiStrIntfHashEntryArray = array of TJclAnsiStrIntfHashEntry;

  TJclAnsiStrIntfBucket = class
  public
    Size: Integer;
    Entries: TJclAnsiStrIntfHashEntryArray;
  end;

  TJclAnsiStrIntfBucketArray = array of TJclAnsiStrIntfBucket;

  TJclAnsiStrIntfHashMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer,
    IJclAnsiStrIntfMap)
  private
    FBuckets: TJclAnsiStrIntfBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclAnsiStrIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclAnsiStrIntfMap }
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclAnsiStrIntfMap): Boolean;
    function GetValue(const Key: AnsiString): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): AnsiString;
    function KeySet: IJclAnsiStrSet;
    procedure PutAll(const AMap: IJclAnsiStrIntfMap);
    procedure PutValue(const Key: AnsiString; const Value: IInterface);
    function Remove(const Key: AnsiString): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: AnsiString): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfAnsiStrHashEntry = record
    Key: IInterface;
    Value: AnsiString;
  end;

  TJclIntfAnsiStrHashEntryArray = array of TJclIntfAnsiStrHashEntry;

  TJclIntfAnsiStrBucket = class
  public
    Size: Integer;
    Entries: TJclIntfAnsiStrHashEntryArray;
  end;

  TJclIntfAnsiStrBucketArray = array of TJclIntfAnsiStrBucket;

  TJclIntfAnsiStrHashMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer,
    IJclIntfAnsiStrMap)
  private
    FBuckets: TJclIntfAnsiStrBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclIntfAnsiStrHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfAnsiStrMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: AnsiString): Boolean;
    function Equals(const AMap: IJclIntfAnsiStrMap): Boolean;
    function GetValue(const Key: IInterface): AnsiString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: AnsiString): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfAnsiStrMap);
    procedure PutValue(const Key: IInterface; const Value: AnsiString);
    function Remove(const Key: IInterface): AnsiString;
    function Size: Integer;
    function Values: IJclAnsiStrCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: AnsiString): AnsiString;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: AnsiString): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclAnsiStrAnsiStrHashEntry = record
    Key: AnsiString;
    Value: AnsiString;
  end;

  TJclAnsiStrAnsiStrHashEntryArray = array of TJclAnsiStrAnsiStrHashEntry;

  TJclAnsiStrAnsiStrBucket = class
  public
    Size: Integer;
    Entries: TJclAnsiStrAnsiStrHashEntryArray;
  end;

  TJclAnsiStrAnsiStrBucketArray = array of TJclAnsiStrAnsiStrBucket;

  TJclAnsiStrAnsiStrHashMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer,
    IJclAnsiStrAnsiStrMap)
  private
    FBuckets: TJclAnsiStrAnsiStrBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclAnsiStrAnsiStrHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclAnsiStrAnsiStrMap }
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(const Value: AnsiString): Boolean;
    function Equals(const AMap: IJclAnsiStrAnsiStrMap): Boolean;
    function GetValue(const Key: AnsiString): AnsiString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: AnsiString): AnsiString;
    function KeySet: IJclAnsiStrSet;
    procedure PutAll(const AMap: IJclAnsiStrAnsiStrMap);
    procedure PutValue(const Key: AnsiString; const Value: AnsiString);
    function Remove(const Key: AnsiString): AnsiString;
    function Size: Integer;
    function Values: IJclAnsiStrCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: AnsiString): AnsiString;
    function KeysEqual(const A, B: AnsiString): Boolean;
    function ValuesEqual(const A, B: AnsiString): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclWideStrIntfHashEntry = record
    Key: WideString;
    Value: IInterface;
  end;

  TJclWideStrIntfHashEntryArray = array of TJclWideStrIntfHashEntry;

  TJclWideStrIntfBucket = class
  public
    Size: Integer;
    Entries: TJclWideStrIntfHashEntryArray;
  end;

  TJclWideStrIntfBucketArray = array of TJclWideStrIntfBucket;

  TJclWideStrIntfHashMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer,
    IJclWideStrIntfMap)
  private
    FBuckets: TJclWideStrIntfBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclWideStrIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclWideStrIntfMap }
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclWideStrIntfMap): Boolean;
    function GetValue(const Key: WideString): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): WideString;
    function KeySet: IJclWideStrSet;
    procedure PutAll(const AMap: IJclWideStrIntfMap);
    procedure PutValue(const Key: WideString; const Value: IInterface);
    function Remove(const Key: WideString): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: WideString): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfWideStrHashEntry = record
    Key: IInterface;
    Value: WideString;
  end;

  TJclIntfWideStrHashEntryArray = array of TJclIntfWideStrHashEntry;

  TJclIntfWideStrBucket = class
  public
    Size: Integer;
    Entries: TJclIntfWideStrHashEntryArray;
  end;

  TJclIntfWideStrBucketArray = array of TJclIntfWideStrBucket;

  TJclIntfWideStrHashMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer,
    IJclIntfWideStrMap)
  private
    FBuckets: TJclIntfWideStrBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclIntfWideStrHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfWideStrMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: WideString): Boolean;
    function Equals(const AMap: IJclIntfWideStrMap): Boolean;
    function GetValue(const Key: IInterface): WideString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: WideString): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfWideStrMap);
    procedure PutValue(const Key: IInterface; const Value: WideString);
    function Remove(const Key: IInterface): WideString;
    function Size: Integer;
    function Values: IJclWideStrCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: WideString): WideString;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: WideString): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclWideStrWideStrHashEntry = record
    Key: WideString;
    Value: WideString;
  end;

  TJclWideStrWideStrHashEntryArray = array of TJclWideStrWideStrHashEntry;

  TJclWideStrWideStrBucket = class
  public
    Size: Integer;
    Entries: TJclWideStrWideStrHashEntryArray;
  end;

  TJclWideStrWideStrBucketArray = array of TJclWideStrWideStrBucket;

  TJclWideStrWideStrHashMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer,
    IJclWideStrWideStrMap)
  private
    FBuckets: TJclWideStrWideStrBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclWideStrWideStrHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclWideStrWideStrMap }
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(const Value: WideString): Boolean;
    function Equals(const AMap: IJclWideStrWideStrMap): Boolean;
    function GetValue(const Key: WideString): WideString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: WideString): WideString;
    function KeySet: IJclWideStrSet;
    procedure PutAll(const AMap: IJclWideStrWideStrMap);
    procedure PutValue(const Key: WideString; const Value: WideString);
    function Remove(const Key: WideString): WideString;
    function Size: Integer;
    function Values: IJclWideStrCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: WideString): WideString;
    function KeysEqual(const A, B: WideString): Boolean;
    function ValuesEqual(const A, B: WideString): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrIntfHashMap = TJclAnsiStrIntfHashMap;
  TJclIntfStrHashMap = TJclIntfAnsiStrHashMap;
  TJclStrStrHashMap = TJclAnsiStrAnsiStrHashMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrIntfHashMap = TJclWideStrIntfHashMap;
  TJclIntfStrHashMap = TJclIntfWideStrHashMap;
  TJclStrStrHashMap = TJclWideStrWideStrHashMap;
  {$ENDIF CONTAINER_WIDESTR}

  TJclSingleIntfHashEntry = record
    Key: Single;
    Value: IInterface;
  end;

  TJclSingleIntfHashEntryArray = array of TJclSingleIntfHashEntry;

  TJclSingleIntfBucket = class
  public
    Size: Integer;
    Entries: TJclSingleIntfHashEntryArray;
  end;

  TJclSingleIntfBucketArray = array of TJclSingleIntfBucket;

  TJclSingleIntfHashMap = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclSingleContainer,
    IJclSingleIntfMap)
  private
    FBuckets: TJclSingleIntfBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclSingleIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclSingleIntfMap }
    procedure Clear;
    function ContainsKey(const Key: Single): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclSingleIntfMap): Boolean;
    function GetValue(const Key: Single): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Single;
    function KeySet: IJclSingleSet;
    procedure PutAll(const AMap: IJclSingleIntfMap);
    procedure PutValue(const Key: Single; const Value: IInterface);
    function Remove(const Key: Single): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: Single): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfSingleHashEntry = record
    Key: IInterface;
    Value: Single;
  end;

  TJclIntfSingleHashEntryArray = array of TJclIntfSingleHashEntry;

  TJclIntfSingleBucket = class
  public
    Size: Integer;
    Entries: TJclIntfSingleHashEntryArray;
  end;

  TJclIntfSingleBucketArray = array of TJclIntfSingleBucket;

  TJclIntfSingleHashMap = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclSingleContainer,
    IJclIntfSingleMap)
  private
    FBuckets: TJclIntfSingleBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclIntfSingleHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfSingleMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: Single): Boolean;
    function Equals(const AMap: IJclIntfSingleMap): Boolean;
    function GetValue(const Key: IInterface): Single;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Single): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfSingleMap);
    procedure PutValue(const Key: IInterface; const Value: Single);
    function Remove(const Key: IInterface): Single;
    function Size: Integer;
    function Values: IJclSingleCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Single): Single;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: Single): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclSingleSingleHashEntry = record
    Key: Single;
    Value: Single;
  end;

  TJclSingleSingleHashEntryArray = array of TJclSingleSingleHashEntry;

  TJclSingleSingleBucket = class
  public
    Size: Integer;
    Entries: TJclSingleSingleHashEntryArray;
  end;

  TJclSingleSingleBucketArray = array of TJclSingleSingleBucket;

  TJclSingleSingleHashMap = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclSingleContainer,
    IJclSingleSingleMap)
  private
    FBuckets: TJclSingleSingleBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclSingleSingleHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclSingleSingleMap }
    procedure Clear;
    function ContainsKey(const Key: Single): Boolean;
    function ContainsValue(const Value: Single): Boolean;
    function Equals(const AMap: IJclSingleSingleMap): Boolean;
    function GetValue(const Key: Single): Single;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Single): Single;
    function KeySet: IJclSingleSet;
    procedure PutAll(const AMap: IJclSingleSingleMap);
    procedure PutValue(const Key: Single; const Value: Single);
    function Remove(const Key: Single): Single;
    function Size: Integer;
    function Values: IJclSingleCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function FreeValue(var Value: Single): Single;
    function KeysEqual(const A, B: Single): Boolean;
    function ValuesEqual(const A, B: Single): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclDoubleIntfHashEntry = record
    Key: Double;
    Value: IInterface;
  end;

  TJclDoubleIntfHashEntryArray = array of TJclDoubleIntfHashEntry;

  TJclDoubleIntfBucket = class
  public
    Size: Integer;
    Entries: TJclDoubleIntfHashEntryArray;
  end;

  TJclDoubleIntfBucketArray = array of TJclDoubleIntfBucket;

  TJclDoubleIntfHashMap = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclDoubleContainer,
    IJclDoubleIntfMap)
  private
    FBuckets: TJclDoubleIntfBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclDoubleIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclDoubleIntfMap }
    procedure Clear;
    function ContainsKey(const Key: Double): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclDoubleIntfMap): Boolean;
    function GetValue(const Key: Double): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Double;
    function KeySet: IJclDoubleSet;
    procedure PutAll(const AMap: IJclDoubleIntfMap);
    procedure PutValue(const Key: Double; const Value: IInterface);
    function Remove(const Key: Double): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: Double): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfDoubleHashEntry = record
    Key: IInterface;
    Value: Double;
  end;

  TJclIntfDoubleHashEntryArray = array of TJclIntfDoubleHashEntry;

  TJclIntfDoubleBucket = class
  public
    Size: Integer;
    Entries: TJclIntfDoubleHashEntryArray;
  end;

  TJclIntfDoubleBucketArray = array of TJclIntfDoubleBucket;

  TJclIntfDoubleHashMap = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclDoubleContainer,
    IJclIntfDoubleMap)
  private
    FBuckets: TJclIntfDoubleBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclIntfDoubleHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfDoubleMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: Double): Boolean;
    function Equals(const AMap: IJclIntfDoubleMap): Boolean;
    function GetValue(const Key: IInterface): Double;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Double): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfDoubleMap);
    procedure PutValue(const Key: IInterface; const Value: Double);
    function Remove(const Key: IInterface): Double;
    function Size: Integer;
    function Values: IJclDoubleCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Double): Double;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: Double): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclDoubleDoubleHashEntry = record
    Key: Double;
    Value: Double;
  end;

  TJclDoubleDoubleHashEntryArray = array of TJclDoubleDoubleHashEntry;

  TJclDoubleDoubleBucket = class
  public
    Size: Integer;
    Entries: TJclDoubleDoubleHashEntryArray;
  end;

  TJclDoubleDoubleBucketArray = array of TJclDoubleDoubleBucket;

  TJclDoubleDoubleHashMap = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclDoubleContainer,
    IJclDoubleDoubleMap)
  private
    FBuckets: TJclDoubleDoubleBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclDoubleDoubleHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclDoubleDoubleMap }
    procedure Clear;
    function ContainsKey(const Key: Double): Boolean;
    function ContainsValue(const Value: Double): Boolean;
    function Equals(const AMap: IJclDoubleDoubleMap): Boolean;
    function GetValue(const Key: Double): Double;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Double): Double;
    function KeySet: IJclDoubleSet;
    procedure PutAll(const AMap: IJclDoubleDoubleMap);
    procedure PutValue(const Key: Double; const Value: Double);
    function Remove(const Key: Double): Double;
    function Size: Integer;
    function Values: IJclDoubleCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function FreeValue(var Value: Double): Double;
    function KeysEqual(const A, B: Double): Boolean;
    function ValuesEqual(const A, B: Double): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclExtendedIntfHashEntry = record
    Key: Extended;
    Value: IInterface;
  end;

  TJclExtendedIntfHashEntryArray = array of TJclExtendedIntfHashEntry;

  TJclExtendedIntfBucket = class
  public
    Size: Integer;
    Entries: TJclExtendedIntfHashEntryArray;
  end;

  TJclExtendedIntfBucketArray = array of TJclExtendedIntfBucket;

  TJclExtendedIntfHashMap = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclExtendedContainer,
    IJclExtendedIntfMap)
  private
    FBuckets: TJclExtendedIntfBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclExtendedIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclExtendedIntfMap }
    procedure Clear;
    function ContainsKey(const Key: Extended): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclExtendedIntfMap): Boolean;
    function GetValue(const Key: Extended): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Extended;
    function KeySet: IJclExtendedSet;
    procedure PutAll(const AMap: IJclExtendedIntfMap);
    procedure PutValue(const Key: Extended; const Value: IInterface);
    function Remove(const Key: Extended): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: Extended): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfExtendedHashEntry = record
    Key: IInterface;
    Value: Extended;
  end;

  TJclIntfExtendedHashEntryArray = array of TJclIntfExtendedHashEntry;

  TJclIntfExtendedBucket = class
  public
    Size: Integer;
    Entries: TJclIntfExtendedHashEntryArray;
  end;

  TJclIntfExtendedBucketArray = array of TJclIntfExtendedBucket;

  TJclIntfExtendedHashMap = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclExtendedContainer,
    IJclIntfExtendedMap)
  private
    FBuckets: TJclIntfExtendedBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclIntfExtendedHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfExtendedMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: Extended): Boolean;
    function Equals(const AMap: IJclIntfExtendedMap): Boolean;
    function GetValue(const Key: IInterface): Extended;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Extended): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfExtendedMap);
    procedure PutValue(const Key: IInterface; const Value: Extended);
    function Remove(const Key: IInterface): Extended;
    function Size: Integer;
    function Values: IJclExtendedCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Extended): Extended;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: Extended): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclExtendedExtendedHashEntry = record
    Key: Extended;
    Value: Extended;
  end;

  TJclExtendedExtendedHashEntryArray = array of TJclExtendedExtendedHashEntry;

  TJclExtendedExtendedBucket = class
  public
    Size: Integer;
    Entries: TJclExtendedExtendedHashEntryArray;
  end;

  TJclExtendedExtendedBucketArray = array of TJclExtendedExtendedBucket;

  TJclExtendedExtendedHashMap = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclExtendedContainer,
    IJclExtendedExtendedMap)
  private
    FBuckets: TJclExtendedExtendedBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclExtendedExtendedHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclExtendedExtendedMap }
    procedure Clear;
    function ContainsKey(const Key: Extended): Boolean;
    function ContainsValue(const Value: Extended): Boolean;
    function Equals(const AMap: IJclExtendedExtendedMap): Boolean;
    function GetValue(const Key: Extended): Extended;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Extended): Extended;
    function KeySet: IJclExtendedSet;
    procedure PutAll(const AMap: IJclExtendedExtendedMap);
    procedure PutValue(const Key: Extended; const Value: Extended);
    function Remove(const Key: Extended): Extended;
    function Size: Integer;
    function Values: IJclExtendedCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function FreeValue(var Value: Extended): Extended;
    function KeysEqual(const A, B: Extended): Boolean;
    function ValuesEqual(const A, B: Extended): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatIntfHashMap = TJclExtendedIntfHashMap;
  TJclIntfFloatHashMap = TJclIntfExtendedHashMap;
  TJclFloatFloatHashMap = TJclExtendedExtendedHashMap;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatIntfHashMap = TJclDoubleIntfHashMap;
  TJclIntfFloatHashMap = TJclIntfDoubleHashMap;
  TJclFloatFloatHashMap = TJclDoubleDoubleHashMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatIntfHashMap = TJclSingleIntfHashMap;
  TJclIntfFloatHashMap = TJclIntfSingleHashMap;
  TJclFloatFloatHashMap = TJclSingleSingleHashMap;
  {$ENDIF MATH_SINGLE_PRECISION}

  TJclIntegerIntfHashEntry = record
    Key: Integer;
    Value: IInterface;
  end;

  TJclIntegerIntfHashEntryArray = array of TJclIntegerIntfHashEntry;

  TJclIntegerIntfBucket = class
  public
    Size: Integer;
    Entries: TJclIntegerIntfHashEntryArray;
  end;

  TJclIntegerIntfBucketArray = array of TJclIntegerIntfBucket;

  TJclIntegerIntfHashMap = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntegerIntfMap)
  private
    FBuckets: TJclIntegerIntfBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclIntegerIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntegerIntfMap }
    procedure Clear;
    function ContainsKey(Key: Integer): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclIntegerIntfMap): Boolean;
    function GetValue(Key: Integer): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Integer;
    function KeySet: IJclIntegerSet;
    procedure PutAll(const AMap: IJclIntegerIntfMap);
    procedure PutValue(Key: Integer; const Value: IInterface);
    function Remove(Key: Integer): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(A, B: Integer): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfIntegerHashEntry = record
    Key: IInterface;
    Value: Integer;
  end;

  TJclIntfIntegerHashEntryArray = array of TJclIntfIntegerHashEntry;

  TJclIntfIntegerBucket = class
  public
    Size: Integer;
    Entries: TJclIntfIntegerHashEntryArray;
  end;

  TJclIntfIntegerBucketArray = array of TJclIntfIntegerBucket;

  TJclIntfIntegerHashMap = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntfIntegerMap)
  private
    FBuckets: TJclIntfIntegerBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclIntfIntegerHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfIntegerMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: Integer): Boolean;
    function Equals(const AMap: IJclIntfIntegerMap): Boolean;
    function GetValue(const Key: IInterface): Integer;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Integer): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfIntegerMap);
    procedure PutValue(const Key: IInterface; Value: Integer);
    function Remove(const Key: IInterface): Integer;
    function Size: Integer;
    function Values: IJclIntegerCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Integer): Integer;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(A, B: Integer): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntegerIntegerHashEntry = record
    Key: Integer;
    Value: Integer;
  end;

  TJclIntegerIntegerHashEntryArray = array of TJclIntegerIntegerHashEntry;

  TJclIntegerIntegerBucket = class
  public
    Size: Integer;
    Entries: TJclIntegerIntegerHashEntryArray;
  end;

  TJclIntegerIntegerBucketArray = array of TJclIntegerIntegerBucket;

  TJclIntegerIntegerHashMap = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntegerIntegerMap)
  private
    FBuckets: TJclIntegerIntegerBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclIntegerIntegerHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntegerIntegerMap }
    procedure Clear;
    function ContainsKey(Key: Integer): Boolean;
    function ContainsValue(Value: Integer): Boolean;
    function Equals(const AMap: IJclIntegerIntegerMap): Boolean;
    function GetValue(Key: Integer): Integer;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Integer): Integer;
    function KeySet: IJclIntegerSet;
    procedure PutAll(const AMap: IJclIntegerIntegerMap);
    procedure PutValue(Key: Integer; Value: Integer);
    function Remove(Key: Integer): Integer;
    function Size: Integer;
    function Values: IJclIntegerCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function FreeValue(var Value: Integer): Integer;
    function KeysEqual(A, B: Integer): Boolean;
    function ValuesEqual(A, B: Integer): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclCardinalIntfHashEntry = record
    Key: Cardinal;
    Value: IInterface;
  end;

  TJclCardinalIntfHashEntryArray = array of TJclCardinalIntfHashEntry;

  TJclCardinalIntfBucket = class
  public
    Size: Integer;
    Entries: TJclCardinalIntfHashEntryArray;
  end;

  TJclCardinalIntfBucketArray = array of TJclCardinalIntfBucket;

  TJclCardinalIntfHashMap = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclCardinalIntfMap)
  private
    FBuckets: TJclCardinalIntfBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclCardinalIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCardinalIntfMap }
    procedure Clear;
    function ContainsKey(Key: Cardinal): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclCardinalIntfMap): Boolean;
    function GetValue(Key: Cardinal): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Cardinal;
    function KeySet: IJclCardinalSet;
    procedure PutAll(const AMap: IJclCardinalIntfMap);
    procedure PutValue(Key: Cardinal; const Value: IInterface);
    function Remove(Key: Cardinal): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(A, B: Cardinal): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfCardinalHashEntry = record
    Key: IInterface;
    Value: Cardinal;
  end;

  TJclIntfCardinalHashEntryArray = array of TJclIntfCardinalHashEntry;

  TJclIntfCardinalBucket = class
  public
    Size: Integer;
    Entries: TJclIntfCardinalHashEntryArray;
  end;

  TJclIntfCardinalBucketArray = array of TJclIntfCardinalBucket;

  TJclIntfCardinalHashMap = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntfCardinalMap)
  private
    FBuckets: TJclIntfCardinalBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclIntfCardinalHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCardinalMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: Cardinal): Boolean;
    function Equals(const AMap: IJclIntfCardinalMap): Boolean;
    function GetValue(const Key: IInterface): Cardinal;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Cardinal): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfCardinalMap);
    procedure PutValue(const Key: IInterface; Value: Cardinal);
    function Remove(const Key: IInterface): Cardinal;
    function Size: Integer;
    function Values: IJclCardinalCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Cardinal): Cardinal;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(A, B: Cardinal): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclCardinalCardinalHashEntry = record
    Key: Cardinal;
    Value: Cardinal;
  end;

  TJclCardinalCardinalHashEntryArray = array of TJclCardinalCardinalHashEntry;

  TJclCardinalCardinalBucket = class
  public
    Size: Integer;
    Entries: TJclCardinalCardinalHashEntryArray;
  end;

  TJclCardinalCardinalBucketArray = array of TJclCardinalCardinalBucket;

  TJclCardinalCardinalHashMap = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclCardinalCardinalMap)
  private
    FBuckets: TJclCardinalCardinalBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclCardinalCardinalHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCardinalCardinalMap }
    procedure Clear;
    function ContainsKey(Key: Cardinal): Boolean;
    function ContainsValue(Value: Cardinal): Boolean;
    function Equals(const AMap: IJclCardinalCardinalMap): Boolean;
    function GetValue(Key: Cardinal): Cardinal;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Cardinal): Cardinal;
    function KeySet: IJclCardinalSet;
    procedure PutAll(const AMap: IJclCardinalCardinalMap);
    procedure PutValue(Key: Cardinal; Value: Cardinal);
    function Remove(Key: Cardinal): Cardinal;
    function Size: Integer;
    function Values: IJclCardinalCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function FreeValue(var Value: Cardinal): Cardinal;
    function KeysEqual(A, B: Cardinal): Boolean;
    function ValuesEqual(A, B: Cardinal): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclInt64IntfHashEntry = record
    Key: Int64;
    Value: IInterface;
  end;

  TJclInt64IntfHashEntryArray = array of TJclInt64IntfHashEntry;

  TJclInt64IntfBucket = class
  public
    Size: Integer;
    Entries: TJclInt64IntfHashEntryArray;
  end;

  TJclInt64IntfBucketArray = array of TJclInt64IntfBucket;

  TJclInt64IntfHashMap = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclInt64IntfMap)
  private
    FBuckets: TJclInt64IntfBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclInt64IntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclInt64IntfMap }
    procedure Clear;
    function ContainsKey(const Key: Int64): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclInt64IntfMap): Boolean;
    function GetValue(const Key: Int64): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Int64;
    function KeySet: IJclInt64Set;
    procedure PutAll(const AMap: IJclInt64IntfMap);
    procedure PutValue(const Key: Int64; const Value: IInterface);
    function Remove(const Key: Int64): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: Int64): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfInt64HashEntry = record
    Key: IInterface;
    Value: Int64;
  end;

  TJclIntfInt64HashEntryArray = array of TJclIntfInt64HashEntry;

  TJclIntfInt64Bucket = class
  public
    Size: Integer;
    Entries: TJclIntfInt64HashEntryArray;
  end;

  TJclIntfInt64BucketArray = array of TJclIntfInt64Bucket;

  TJclIntfInt64HashMap = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntfInt64Map)
  private
    FBuckets: TJclIntfInt64BucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclIntfInt64HashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfInt64Map }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: Int64): Boolean;
    function Equals(const AMap: IJclIntfInt64Map): Boolean;
    function GetValue(const Key: IInterface): Int64;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Int64): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfInt64Map);
    procedure PutValue(const Key: IInterface; const Value: Int64);
    function Remove(const Key: IInterface): Int64;
    function Size: Integer;
    function Values: IJclInt64Collection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Int64): Int64;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: Int64): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclInt64Int64HashEntry = record
    Key: Int64;
    Value: Int64;
  end;

  TJclInt64Int64HashEntryArray = array of TJclInt64Int64HashEntry;

  TJclInt64Int64Bucket = class
  public
    Size: Integer;
    Entries: TJclInt64Int64HashEntryArray;
  end;

  TJclInt64Int64BucketArray = array of TJclInt64Int64Bucket;

  TJclInt64Int64HashMap = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclInt64Int64Map)
  private
    FBuckets: TJclInt64Int64BucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclInt64Int64HashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclInt64Int64Map }
    procedure Clear;
    function ContainsKey(const Key: Int64): Boolean;
    function ContainsValue(const Value: Int64): Boolean;
    function Equals(const AMap: IJclInt64Int64Map): Boolean;
    function GetValue(const Key: Int64): Int64;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Int64): Int64;
    function KeySet: IJclInt64Set;
    procedure PutAll(const AMap: IJclInt64Int64Map);
    procedure PutValue(const Key: Int64; const Value: Int64);
    function Remove(const Key: Int64): Int64;
    function Size: Integer;
    function Values: IJclInt64Collection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function FreeValue(var Value: Int64): Int64;
    function KeysEqual(const A, B: Int64): Boolean;
    function ValuesEqual(const A, B: Int64): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  {$IFNDEF CLR}
  TJclPtrIntfHashEntry = record
    Key: Pointer;
    Value: IInterface;
  end;

  TJclPtrIntfHashEntryArray = array of TJclPtrIntfHashEntry;

  TJclPtrIntfBucket = class
  public
    Size: Integer;
    Entries: TJclPtrIntfHashEntryArray;
  end;

  TJclPtrIntfBucketArray = array of TJclPtrIntfBucket;

  TJclPtrIntfHashMap = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclPtrIntfMap)
  private
    FBuckets: TJclPtrIntfBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclPtrIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclPtrIntfMap }
    procedure Clear;
    function ContainsKey(Key: Pointer): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclPtrIntfMap): Boolean;
    function GetValue(Key: Pointer): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Pointer;
    function KeySet: IJclPtrSet;
    procedure PutAll(const AMap: IJclPtrIntfMap);
    procedure PutValue(Key: Pointer; const Value: IInterface);
    function Remove(Key: Pointer): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(A, B: Pointer): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfPtrHashEntry = record
    Key: IInterface;
    Value: Pointer;
  end;

  TJclIntfPtrHashEntryArray = array of TJclIntfPtrHashEntry;

  TJclIntfPtrBucket = class
  public
    Size: Integer;
    Entries: TJclIntfPtrHashEntryArray;
  end;

  TJclIntfPtrBucketArray = array of TJclIntfPtrBucket;

  TJclIntfPtrHashMap = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntfPtrMap)
  private
    FBuckets: TJclIntfPtrBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclIntfPtrHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfPtrMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: Pointer): Boolean;
    function Equals(const AMap: IJclIntfPtrMap): Boolean;
    function GetValue(const Key: IInterface): Pointer;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Pointer): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfPtrMap);
    procedure PutValue(const Key: IInterface; Value: Pointer);
    function Remove(const Key: IInterface): Pointer;
    function Size: Integer;
    function Values: IJclPtrCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Pointer): Pointer;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(A, B: Pointer): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclPtrPtrHashEntry = record
    Key: Pointer;
    Value: Pointer;
  end;

  TJclPtrPtrHashEntryArray = array of TJclPtrPtrHashEntry;

  TJclPtrPtrBucket = class
  public
    Size: Integer;
    Entries: TJclPtrPtrHashEntryArray;
  end;

  TJclPtrPtrBucketArray = array of TJclPtrPtrBucket;

  TJclPtrPtrHashMap = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclPtrPtrMap)
  private
    FBuckets: TJclPtrPtrBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclPtrPtrHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclPtrPtrMap }
    procedure Clear;
    function ContainsKey(Key: Pointer): Boolean;
    function ContainsValue(Value: Pointer): Boolean;
    function Equals(const AMap: IJclPtrPtrMap): Boolean;
    function GetValue(Key: Pointer): Pointer;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Pointer): Pointer;
    function KeySet: IJclPtrSet;
    procedure PutAll(const AMap: IJclPtrPtrMap);
    procedure PutValue(Key: Pointer; Value: Pointer);
    function Remove(Key: Pointer): Pointer;
    function Size: Integer;
    function Values: IJclPtrCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function FreeValue(var Value: Pointer): Pointer;
    function KeysEqual(A, B: Pointer): Boolean;
    function ValuesEqual(A, B: Pointer): Boolean;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;
  {$ENDIF ~CLR}

  TJclIntfHashEntry = record
    Key: IInterface;
    Value: TObject;
  end;

  TJclIntfHashEntryArray = array of TJclIntfHashEntry;

  TJclIntfBucket = class
  public
    Size: Integer;
    Entries: TJclIntfHashEntryArray;
  end;

  TJclIntfBucketArray = array of TJclIntfBucket;

  TJclIntfHashMap = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclValueOwner,
    IJclIntfMap)
  private
    FBuckets: TJclIntfBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclIntfMap): Boolean;
    function GetValue(const Key: IInterface): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfMap);
    procedure PutValue(const Key: IInterface; Value: TObject);
    function Remove(const Key: IInterface): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  TJclAnsiStrHashEntry = record
    Key: AnsiString;
    Value: TObject;
  end;

  TJclAnsiStrHashEntryArray = array of TJclAnsiStrHashEntry;

  TJclAnsiStrBucket = class
  public
    Size: Integer;
    Entries: TJclAnsiStrHashEntryArray;
  end;

  TJclAnsiStrBucketArray = array of TJclAnsiStrBucket;

  TJclAnsiStrHashMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer, IJclValueOwner,
    IJclAnsiStrMap)
  private
    FBuckets: TJclAnsiStrBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclAnsiStrHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclAnsiStrMap }
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclAnsiStrMap): Boolean;
    function GetValue(const Key: AnsiString): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): AnsiString;
    function KeySet: IJclAnsiStrSet;
    procedure PutAll(const AMap: IJclAnsiStrMap);
    procedure PutValue(const Key: AnsiString; Value: TObject);
    function Remove(const Key: AnsiString): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function KeysEqual(const A, B: AnsiString): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  TJclWideStrHashEntry = record
    Key: WideString;
    Value: TObject;
  end;

  TJclWideStrHashEntryArray = array of TJclWideStrHashEntry;

  TJclWideStrBucket = class
  public
    Size: Integer;
    Entries: TJclWideStrHashEntryArray;
  end;

  TJclWideStrBucketArray = array of TJclWideStrBucket;

  TJclWideStrHashMap = class(TJclwideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer, IJclValueOwner,
    IJclWideStrMap)
  private
    FBuckets: TJclWideStrBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclWideStrHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclWideStrMap }
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclWideStrMap): Boolean;
    function GetValue(const Key: WideString): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): WideString;
    function KeySet: IJclWideStrSet;
    procedure PutAll(const AMap: IJclWideStrMap);
    procedure PutValue(const Key: WideString; Value: TObject);
    function Remove(const Key: WideString): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function KeysEqual(const A, B: WideString): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrHashMap = TJclAnsiStrHashMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrHashMap = TJclWideStrHashMap;
  {$ENDIF CONTAINER_WIDESTR}

  TJclSingleHashEntry = record
    Key: Single;
    Value: TObject;
  end;

  TJclSingleHashEntryArray = array of TJclSingleHashEntry;

  TJclSingleBucket = class
  public
    Size: Integer;
    Entries: TJclSingleHashEntryArray;
  end;

  TJclSingleBucketArray = array of TJclSingleBucket;

  TJclSingleHashMap = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclSingleContainer, IJclValueOwner,
    IJclSingleMap)
  private
    FBuckets: TJclSingleBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclSingleHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclSingleMap }
    procedure Clear;
    function ContainsKey(const Key: Single): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclSingleMap): Boolean;
    function GetValue(const Key: Single): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Single;
    function KeySet: IJclSingleSet;
    procedure PutAll(const AMap: IJclSingleMap);
    procedure PutValue(const Key: Single; Value: TObject);
    function Remove(const Key: Single): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function KeysEqual(const A, B: Single): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  TJclDoubleHashEntry = record
    Key: Double;
    Value: TObject;
  end;

  TJclDoubleHashEntryArray = array of TJclDoubleHashEntry;

  TJclDoubleBucket = class
  public
    Size: Integer;
    Entries: TJclDoubleHashEntryArray;
  end;

  TJclDoubleBucketArray = array of TJclDoubleBucket;

  TJclDoubleHashMap = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclDoubleContainer, IJclValueOwner,
    IJclDoubleMap)
  private
    FBuckets: TJclDoubleBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclDoubleHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclDoubleMap }
    procedure Clear;
    function ContainsKey(const Key: Double): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclDoubleMap): Boolean;
    function GetValue(const Key: Double): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Double;
    function KeySet: IJclDoubleSet;
    procedure PutAll(const AMap: IJclDoubleMap);
    procedure PutValue(const Key: Double; Value: TObject);
    function Remove(const Key: Double): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function KeysEqual(const A, B: Double): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  TJclExtendedHashEntry = record
    Key: Extended;
    Value: TObject;
  end;

  TJclExtendedHashEntryArray = array of TJclExtendedHashEntry;

  TJclExtendedBucket = class
  public
    Size: Integer;
    Entries: TJclExtendedHashEntryArray;
  end;

  TJclExtendedBucketArray = array of TJclExtendedBucket;

  TJclExtendedHashMap = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclExtendedContainer, IJclValueOwner,
    IJclExtendedMap)
  private
    FBuckets: TJclExtendedBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclExtendedHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclExtendedMap }
    procedure Clear;
    function ContainsKey(const Key: Extended): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclExtendedMap): Boolean;
    function GetValue(const Key: Extended): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Extended;
    function KeySet: IJclExtendedSet;
    procedure PutAll(const AMap: IJclExtendedMap);
    procedure PutValue(const Key: Extended; Value: TObject);
    function Remove(const Key: Extended): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function KeysEqual(const A, B: Extended): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatHashMap = TJclExtendedHashMap;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatHashMap = TJclDoubleHashMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatHashMap = TJclSingleHashMap;
  {$ENDIF MATH_SINGLE_PRECISION}

  TJclIntegerHashEntry = record
    Key: Integer;
    Value: TObject;
  end;

  TJclIntegerHashEntryArray = array of TJclIntegerHashEntry;

  TJclIntegerBucket = class
  public
    Size: Integer;
    Entries: TJclIntegerHashEntryArray;
  end;

  TJclIntegerBucketArray = array of TJclIntegerBucket;

  TJclIntegerHashMap = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclValueOwner,
    IJclIntegerMap)
  private
    FBuckets: TJclIntegerBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclIntegerHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntegerMap }
    procedure Clear;
    function ContainsKey(Key: Integer): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclIntegerMap): Boolean;
    function GetValue(Key: Integer): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Integer;
    function KeySet: IJclIntegerSet;
    procedure PutAll(const AMap: IJclIntegerMap);
    procedure PutValue(Key: Integer; Value: TObject);
    function Remove(Key: Integer): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function KeysEqual(A, B: Integer): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  TJclCardinalHashEntry = record
    Key: Cardinal;
    Value: TObject;
  end;

  TJclCardinalHashEntryArray = array of TJclCardinalHashEntry;

  TJclCardinalBucket = class
  public
    Size: Integer;
    Entries: TJclCardinalHashEntryArray;
  end;

  TJclCardinalBucketArray = array of TJclCardinalBucket;

  TJclCardinalHashMap = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclValueOwner,
    IJclCardinalMap)
  private
    FBuckets: TJclCardinalBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclCardinalHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCardinalMap }
    procedure Clear;
    function ContainsKey(Key: Cardinal): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclCardinalMap): Boolean;
    function GetValue(Key: Cardinal): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Cardinal;
    function KeySet: IJclCardinalSet;
    procedure PutAll(const AMap: IJclCardinalMap);
    procedure PutValue(Key: Cardinal; Value: TObject);
    function Remove(Key: Cardinal): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function KeysEqual(A, B: Cardinal): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  TJclInt64HashEntry = record
    Key: Int64;
    Value: TObject;
  end;

  TJclInt64HashEntryArray = array of TJclInt64HashEntry;

  TJclInt64Bucket = class
  public
    Size: Integer;
    Entries: TJclInt64HashEntryArray;
  end;

  TJclInt64BucketArray = array of TJclInt64Bucket;

  TJclInt64HashMap = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclValueOwner,
    IJclInt64Map)
  private
    FBuckets: TJclInt64BucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclInt64HashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclInt64Map }
    procedure Clear;
    function ContainsKey(const Key: Int64): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclInt64Map): Boolean;
    function GetValue(const Key: Int64): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Int64;
    function KeySet: IJclInt64Set;
    procedure PutAll(const AMap: IJclInt64Map);
    procedure PutValue(const Key: Int64; Value: TObject);
    function Remove(const Key: Int64): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function KeysEqual(const A, B: Int64): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  {$IFNDEF CLR}
  TJclPtrHashEntry = record
    Key: Pointer;
    Value: TObject;
  end;

  TJclPtrHashEntryArray = array of TJclPtrHashEntry;

  TJclPtrBucket = class
  public
    Size: Integer;
    Entries: TJclPtrHashEntryArray;
  end;

  TJclPtrBucketArray = array of TJclPtrBucket;

  TJclPtrHashMap = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclValueOwner,
    IJclPtrMap)
  private
    FBuckets: TJclPtrBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclPtrHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclPtrMap }
    procedure Clear;
    function ContainsKey(Key: Pointer): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclPtrMap): Boolean;
    function GetValue(Key: Pointer): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Pointer;
    function KeySet: IJclPtrSet;
    procedure PutAll(const AMap: IJclPtrMap);
    procedure PutValue(Key: Pointer; Value: TObject);
    function Remove(Key: Pointer): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function KeysEqual(A, B: Pointer): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsValues: Boolean read FOwnsValues;
  end;
  {$ENDIF ~CLR}

  TJclHashEntry = record
    Key: TObject;
    Value: TObject;
  end;

  TJclHashEntryArray = array of TJclHashEntry;

  TJclBucket = class
  public
    Size: Integer;
    Entries: TJclHashEntryArray;
  end;

  TJclBucketArray = array of TJclBucket;

  TJclHashMap = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclKeyOwner, IJclValueOwner,
    IJclMap)
  private
    FBuckets: TJclBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclHashEntryArray; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclMap }
    procedure Clear;
    function ContainsKey(Key: TObject): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclMap): Boolean;
    function GetValue(Key: TObject): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): TObject;
    function KeySet: IJclSet;
    procedure PutAll(const AMap: IJclMap);
    procedure PutValue(Key: TObject; Value: TObject);
    function Remove(Key: TObject): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclKeyOwner }
    function FreeKey(var Key: TObject): TObject;
    function GetOwnsKeys: Boolean;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function Hash(AObject: TObject): Integer;
    function KeysEqual(A, B: TObject): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  TJclHashEntry<TKey,TValue> = record
    Key: TKey;
    Value: TValue;
  end;

  TJclHashEntryArray<TKey,TValue> = array of TJclHashEntry<TKey,TValue>;

  TJclBucket<TKey,TValue> = class
  public
    Size: Integer;
    Entries: TJclHashEntryArray<TKey,TValue>;
  end;

  TJclBucketArray<TKey,TValue> = array of TJclBucket<TKey,TValue>;

  TJclHashMap<TKey,TValue> = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclPairOwner<TKey, TValue>,
    IJclMap<TKey,TValue>)
  private
    FBuckets: TJclBucketArray<TKey,TValue>;
    FHashFunction: TJclHashFunction;
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclHashEntryArray<TKey,TValue>; FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclMap<TKey,TValue> }
    procedure Clear;
    function ContainsKey(const Key: TKey): Boolean;
    function ContainsValue(const Value: TValue): Boolean;
    function Equals(const AMap: IJclMap<TKey,TValue>): Boolean;
    function GetValue(const Key: TKey): TValue;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: TValue): TKey;
    function KeySet: IJclSet<TKey>;
    procedure PutAll(const AMap: IJclMap<TKey,TValue>);
    procedure PutValue(const Key: TKey; const Value: TValue);
    function Remove(const Key: TKey): TValue;
    function Size: Integer;
    function Values: IJclCollection<TValue>;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclPairOwner }
    function FreeKey(var Key: TKey): TKey;
    function FreeValue(var Value: TValue): TValue;
    function GetOwnsKeys: Boolean;
    function GetOwnsValues: Boolean;
    function Hash(const AKey: TKey): Integer; virtual; abstract;
    function KeysEqual(const A, B: TKey): Boolean; virtual; abstract;
    function ValuesEqual(const A, B: TValue): Boolean; virtual; abstract;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; virtual; abstract;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; virtual; abstract;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  // E = external helper to compare and hash items
  // KeyComparer is used only when getting KeySet
  // GetHashCode and Equals methods of KeyEqualityComparer are used
  // GetHashCode of ValueEqualityComparer is not used
  TJclHashMapE<TKey, TValue> = class(TJclHashMap<TKey, TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  private
    FKeyEqualityComparer: IEqualityComparer<TKey>;
    FKeyComparer: IComparer<TKey>;
    FValueEqualityComparer: IEqualityComparer<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AKeyEqualityComparer: IEqualityComparer<TKey>;
      const AValueEqualityComparer: IEqualityComparer<TValue>;
      const AKeyComparer: IComparer<TKey>; ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyEqualityComparer: IEqualityComparer<TKey> read FKeyEqualityComparer write FKeyEqualityComparer;
    property KeyComparer: IComparer<TKey> read FKeyComparer write FKeyComparer;
    property ValueEqualityComparer: IEqualityComparer<TValue> read FValueEqualityComparer write FValueEqualityComparer;
  end;

  // F = Functions to compare and hash items
  // KeyComparer is used only when getting KeySet
  TJclHashMapF<TKey, TValue> = class(TJclHashMap<TKey, TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  private
    FKeyEqualityCompare: TEqualityCompare<TKey>;
    FKeyHash: THashConvert<TKey>;
    FKeyCompare: TCompare<TKey>;
    FValueEqualityCompare: TEqualityCompare<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(AKeyEqualityCompare: TEqualityCompare<TKey>; AKeyHash: THashConvert<TKey>;
      AValueEqualityCompare: TEqualityCompare<TValue>; AKeyCompare: TCompare<TKey>;
      ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyEqualityCompare: TEqualityCompare<TKey> read FKeyEqualityCompare write FKeyEqualityCompare;
    property KeyCompare: TCompare<TKey> read FKeyCompare write FKeyCompare;
    property KeyHash: THashConvert<TKey> read FKeyHash write FKeyHash;
    property ValueEqualityCompare: TEqualityCompare<TValue> read FValueEqualityCompare write FValueEqualityCompare;
  end;

  // I = items can compare themselves to an other, items can create hash value from themselves
  TJclHashMapI<TKey: IComparable<TKey>, IEquatable<TKey>, IHashable; TValue: IEquatable<TValue>> = class(TJclHashMap<TKey, TValue>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer,
    IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  protected
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;
  {$ENDIF SUPPORTS_GENERICS}

function HashMul(Key, Range: Integer): Integer;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclHashMaps.pas $';
    Revision: '$Revision: 2393 $';
    Date: '$Date: 2008-07-20 11:13:23 +0200 (dim., 20 juil. 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils,
  JclArrayLists, JclArraySets, JclResources;

function HashMul(Key, Range: Integer): Integer;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(Range * (Frac(Key * A)));
end;

//=== { TJclIntfIntfHashMap } ==========================================

constructor TJclIntfIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfIntfBucket;
  ADest: TJclIntfIntfHashMap;
  AMap: IJclIntfIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfIntfHashMap then
    begin
      ADest := TJclIntfIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfIntfBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfIntfHashMap then
    TJclIntfIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfIntfHashMap.Equals(const AMap: IJclIntfIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclIntfIntfHashMap.GetValue(const Key: IInterface): IInterface;
var
  I: Integer;
  Bucket: TJclIntfIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclIntfIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfIntfHashMap.KeyOfValue(const Value: IInterface): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclIntfIntfHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.MoveArray(var List: TJclIntfIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.PutAll(const AMap: IJclIntfIntfMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.PutValue(const Key: IInterface; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclIntfIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfIntfHashMap.Remove(const Key: IInterface): IInterface;
var
  Bucket: TJclIntfIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


//=== { TJclAnsiStrIntfHashMap } ==========================================

constructor TJclAnsiStrIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclAnsiStrIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclAnsiStrIntfBucket;
  ADest: TJclAnsiStrIntfHashMap;
  AMap: IJclAnsiStrIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclAnsiStrIntfHashMap then
    begin
      ADest := TJclAnsiStrIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclAnsiStrIntfBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclAnsiStrIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclAnsiStrIntfHashMap then
    TJclAnsiStrIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclAnsiStrIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.ContainsKey(const Key: AnsiString): Boolean;
var
  I: Integer;
  Bucket: TJclAnsiStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclAnsiStrIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrIntfHashMap.Equals(const AMap: IJclAnsiStrIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclAnsiStrIntfHashMap.GetValue(const Key: AnsiString): IInterface;
var
  I: Integer;
  Bucket: TJclAnsiStrIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclAnsiStrIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrIntfHashMap.KeyOfValue(const Value: IInterface): AnsiString;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclAnsiStrIntfHashMap.KeySet: IJclAnsiStrSet;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrIntfHashMap.MoveArray(var List: TJclAnsiStrIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclAnsiStrIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclAnsiStrIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrIntfHashMap.PutAll(const AMap: IJclAnsiStrIntfMap);
var
  It: IJclAnsiStrIterator;
  Key: AnsiString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrIntfHashMap.PutValue(const Key: AnsiString; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclAnsiStrIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclAnsiStrIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclAnsiStrIntfHashMap.Remove(const Key: AnsiString): IInterface;
var
  Bucket: TJclAnsiStrIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


//=== { TJclIntfAnsiStrHashMap } ==========================================

constructor TJclIntfAnsiStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfAnsiStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfAnsiStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfAnsiStrBucket;
  ADest: TJclIntfAnsiStrHashMap;
  AMap: IJclIntfAnsiStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfAnsiStrHashMap then
    begin
      ADest := TJclIntfAnsiStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfAnsiStrBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfAnsiStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfAnsiStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfAnsiStrHashMap then
    TJclIntfAnsiStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfAnsiStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfAnsiStrHashMap.ContainsValue(const Value: AnsiString): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfAnsiStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfAnsiStrHashMap.Equals(const AMap: IJclIntfAnsiStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfAnsiStrHashMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;



function TJclIntfAnsiStrHashMap.GetValue(const Key: IInterface): AnsiString;
var
  I: Integer;
  Bucket: TJclIntfAnsiStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfAnsiStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfAnsiStrHashMap.KeyOfValue(const Value: AnsiString): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfAnsiStrHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfAnsiStrHashMap.MoveArray(var List: TJclIntfAnsiStrHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfAnsiStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfAnsiStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfAnsiStrHashMap.PutAll(const AMap: IJclIntfAnsiStrMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfAnsiStrHashMap.PutValue(const Key: IInterface; const Value: AnsiString);
var
  Index: Integer;
  Bucket: TJclIntfAnsiStrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, '')) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfAnsiStrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfAnsiStrHashMap.Remove(const Key: IInterface): AnsiString;
var
  Bucket: TJclIntfAnsiStrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfAnsiStrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfAnsiStrHashMap.Values: IJclAnsiStrCollection;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.ValuesEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


//=== { TJclAnsiStrAnsiStrHashMap } ==========================================

constructor TJclAnsiStrAnsiStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclAnsiStrAnsiStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrAnsiStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclAnsiStrAnsiStrBucket;
  ADest: TJclAnsiStrAnsiStrHashMap;
  AMap: IJclAnsiStrAnsiStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclAnsiStrAnsiStrHashMap then
    begin
      ADest := TJclAnsiStrAnsiStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclAnsiStrAnsiStrBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclAnsiStrAnsiStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrAnsiStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclAnsiStrAnsiStrHashMap then
    TJclAnsiStrAnsiStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclAnsiStrAnsiStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.ContainsKey(const Key: AnsiString): Boolean;
var
  I: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclAnsiStrAnsiStrHashMap.ContainsValue(const Value: AnsiString): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrAnsiStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrAnsiStrHashMap.Equals(const AMap: IJclAnsiStrAnsiStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrAnsiStrHashMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;



function TJclAnsiStrAnsiStrHashMap.GetValue(const Key: AnsiString): AnsiString;
var
  I: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclAnsiStrAnsiStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrAnsiStrHashMap.KeyOfValue(const Value: AnsiString): AnsiString;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclAnsiStrAnsiStrHashMap.KeySet: IJclAnsiStrSet;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrAnsiStrHashMap.MoveArray(var List: TJclAnsiStrAnsiStrHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclAnsiStrAnsiStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrAnsiStrHashMap.PutAll(const AMap: IJclAnsiStrAnsiStrMap);
var
  It: IJclAnsiStrIterator;
  Key: AnsiString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrAnsiStrHashMap.PutValue(const Key: AnsiString; const Value: AnsiString);
var
  Index: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, '')) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclAnsiStrAnsiStrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclAnsiStrAnsiStrHashMap.Remove(const Key: AnsiString): AnsiString;
var
  Bucket: TJclAnsiStrAnsiStrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrAnsiStrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrAnsiStrHashMap.Values: IJclAnsiStrCollection;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.ValuesEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


//=== { TJclWideStrIntfHashMap } ==========================================

constructor TJclWideStrIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclWideStrIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclWideStrIntfBucket;
  ADest: TJclWideStrIntfHashMap;
  AMap: IJclWideStrIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclWideStrIntfHashMap then
    begin
      ADest := TJclWideStrIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclWideStrIntfBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclWideStrIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclWideStrIntfHashMap then
    TJclWideStrIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclWideStrIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.ContainsKey(const Key: WideString): Boolean;
var
  I: Integer;
  Bucket: TJclWideStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclWideStrIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclWideStrIntfHashMap.Equals(const AMap: IJclWideStrIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclWideStrIntfHashMap.GetValue(const Key: WideString): IInterface;
var
  I: Integer;
  Bucket: TJclWideStrIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclWideStrIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrIntfHashMap.KeyOfValue(const Value: IInterface): WideString;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclWideStrIntfHashMap.KeySet: IJclWideStrSet;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrIntfHashMap.MoveArray(var List: TJclWideStrIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclWideStrIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclWideStrIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrIntfHashMap.PutAll(const AMap: IJclWideStrIntfMap);
var
  It: IJclWideStrIterator;
  Key: WideString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrIntfHashMap.PutValue(const Key: WideString; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclWideStrIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclWideStrIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclWideStrIntfHashMap.Remove(const Key: WideString): IInterface;
var
  Bucket: TJclWideStrIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


//=== { TJclIntfWideStrHashMap } ==========================================

constructor TJclIntfWideStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfWideStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfWideStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfWideStrBucket;
  ADest: TJclIntfWideStrHashMap;
  AMap: IJclIntfWideStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfWideStrHashMap then
    begin
      ADest := TJclIntfWideStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfWideStrBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfWideStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfWideStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfWideStrHashMap then
    TJclIntfWideStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfWideStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfWideStrHashMap.ContainsValue(const Value: WideString): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfWideStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfWideStrHashMap.Equals(const AMap: IJclIntfWideStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfWideStrHashMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;



function TJclIntfWideStrHashMap.GetValue(const Key: IInterface): WideString;
var
  I: Integer;
  Bucket: TJclIntfWideStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfWideStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfWideStrHashMap.KeyOfValue(const Value: WideString): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfWideStrHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfWideStrHashMap.MoveArray(var List: TJclIntfWideStrHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfWideStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfWideStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfWideStrHashMap.PutAll(const AMap: IJclIntfWideStrMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfWideStrHashMap.PutValue(const Key: IInterface; const Value: WideString);
var
  Index: Integer;
  Bucket: TJclIntfWideStrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, '')) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfWideStrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfWideStrHashMap.Remove(const Key: IInterface): WideString;
var
  Bucket: TJclIntfWideStrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfWideStrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfWideStrHashMap.Values: IJclWideStrCollection;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.ValuesEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


//=== { TJclWideStrWideStrHashMap } ==========================================

constructor TJclWideStrWideStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclWideStrWideStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrWideStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclWideStrWideStrBucket;
  ADest: TJclWideStrWideStrHashMap;
  AMap: IJclWideStrWideStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclWideStrWideStrHashMap then
    begin
      ADest := TJclWideStrWideStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclWideStrWideStrBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclWideStrWideStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrWideStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclWideStrWideStrHashMap then
    TJclWideStrWideStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclWideStrWideStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.ContainsKey(const Key: WideString): Boolean;
var
  I: Integer;
  Bucket: TJclWideStrWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclWideStrWideStrHashMap.ContainsValue(const Value: WideString): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrWideStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclWideStrWideStrHashMap.Equals(const AMap: IJclWideStrWideStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrWideStrHashMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;



function TJclWideStrWideStrHashMap.GetValue(const Key: WideString): WideString;
var
  I: Integer;
  Bucket: TJclWideStrWideStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclWideStrWideStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrWideStrHashMap.KeyOfValue(const Value: WideString): WideString;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclWideStrWideStrHashMap.KeySet: IJclWideStrSet;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrWideStrHashMap.MoveArray(var List: TJclWideStrWideStrHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclWideStrWideStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclWideStrWideStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrWideStrHashMap.PutAll(const AMap: IJclWideStrWideStrMap);
var
  It: IJclWideStrIterator;
  Key: WideString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrWideStrHashMap.PutValue(const Key: WideString; const Value: WideString);
var
  Index: Integer;
  Bucket: TJclWideStrWideStrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, '')) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclWideStrWideStrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclWideStrWideStrHashMap.Remove(const Key: WideString): WideString;
var
  Bucket: TJclWideStrWideStrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrWideStrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrWideStrHashMap.Values: IJclWideStrCollection;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.ValuesEqual(const A, B: Widestring): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


//=== { TJclSingleIntfHashMap } ==========================================

constructor TJclSingleIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclSingleIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclSingleIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclSingleIntfBucket;
  ADest: TJclSingleIntfHashMap;
  AMap: IJclSingleIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclSingleIntfHashMap then
    begin
      ADest := TJclSingleIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclSingleIntfBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclSingleIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclSingleIntfHashMap then
    TJclSingleIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclSingleIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclSingleIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.ContainsKey(const Key: Single): Boolean;
var
  I: Integer;
  Bucket: TJclSingleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclSingleIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclSingleIntfHashMap.Equals(const AMap: IJclSingleIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclSingleIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclSingleIntfHashMap.GetValue(const Key: Single): IInterface;
var
  I: Integer;
  Bucket: TJclSingleIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclSingleIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleIntfHashMap.KeyOfValue(const Value: IInterface): Single;
var
  I, J: Integer;
  Bucket: TJclSingleIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.KeysEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclSingleIntfHashMap.KeySet: IJclSingleSet;
var
  I, J: Integer;
  Bucket: TJclSingleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleIntfHashMap.MoveArray(var List: TJclSingleIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclSingleIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclSingleIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleIntfHashMap.PutAll(const AMap: IJclSingleIntfMap);
var
  It: IJclSingleIterator;
  Key: Single;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleIntfHashMap.PutValue(const Key: Single; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclSingleIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclSingleIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclSingleIntfHashMap.Remove(const Key: Single): IInterface;
var
  Bucket: TJclSingleIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclSingleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


//=== { TJclIntfSingleHashMap } ==========================================

constructor TJclIntfSingleHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfSingleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfSingleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfSingleBucket;
  ADest: TJclIntfSingleHashMap;
  AMap: IJclIntfSingleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfSingleHashMap then
    begin
      ADest := TJclIntfSingleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfSingleBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfSingleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSingleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfSingleHashMap then
    TJclIntfSingleHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfSingleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfSingleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfSingleHashMap.ContainsValue(const Value: Single): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfSingleHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfSingleHashMap.Equals(const AMap: IJclIntfSingleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfSingleHashMap.FreeValue(var Value: Single): Single;
begin
  Result := Value;
  Value := 0.0;
end;



function TJclIntfSingleHashMap.GetValue(const Key: IInterface): Single;
var
  I: Integer;
  Bucket: TJclIntfSingleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfSingleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfSingleHashMap.KeyOfValue(const Value: Single): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfSingleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfSingleHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSingleHashMap.MoveArray(var List: TJclIntfSingleHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfSingleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfSingleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSingleHashMap.PutAll(const AMap: IJclIntfSingleMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSingleHashMap.PutValue(const Key: IInterface; const Value: Single);
var
  Index: Integer;
  Bucket: TJclIntfSingleBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0.0)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfSingleBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfSingleHashMap.Remove(const Key: IInterface): Single;
var
  Bucket: TJclIntfSingleBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSingleHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfSingleHashMap.Values: IJclSingleCollection;
var
  I, J: Integer;
  Bucket: TJclIntfSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.ValuesEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


//=== { TJclSingleSingleHashMap } ==========================================

constructor TJclSingleSingleHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclSingleSingleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclSingleSingleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclSingleSingleBucket;
  ADest: TJclSingleSingleHashMap;
  AMap: IJclSingleSingleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclSingleSingleHashMap then
    begin
      ADest := TJclSingleSingleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclSingleSingleBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclSingleSingleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleSingleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclSingleSingleHashMap then
    TJclSingleSingleHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclSingleSingleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclSingleSingleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.ContainsKey(const Key: Single): Boolean;
var
  I: Integer;
  Bucket: TJclSingleSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclSingleSingleHashMap.ContainsValue(const Value: Single): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleSingleHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclSingleSingleHashMap.Equals(const AMap: IJclSingleSingleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclSingleSingleHashMap.FreeValue(var Value: Single): Single;
begin
  Result := Value;
  Value := 0.0;
end;



function TJclSingleSingleHashMap.GetValue(const Key: Single): Single;
var
  I: Integer;
  Bucket: TJclSingleSingleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclSingleSingleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleSingleHashMap.KeyOfValue(const Value: Single): Single;
var
  I, J: Integer;
  Bucket: TJclSingleSingleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.KeysEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclSingleSingleHashMap.KeySet: IJclSingleSet;
var
  I, J: Integer;
  Bucket: TJclSingleSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleSingleHashMap.MoveArray(var List: TJclSingleSingleHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclSingleSingleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclSingleSingleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleSingleHashMap.PutAll(const AMap: IJclSingleSingleMap);
var
  It: IJclSingleIterator;
  Key: Single;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleSingleHashMap.PutValue(const Key: Single; const Value: Single);
var
  Index: Integer;
  Bucket: TJclSingleSingleBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, 0.0)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclSingleSingleBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclSingleSingleHashMap.Remove(const Key: Single): Single;
var
  Bucket: TJclSingleSingleBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleSingleHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleSingleHashMap.Values: IJclSingleCollection;
var
  I, J: Integer;
  Bucket: TJclSingleSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.ValuesEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


//=== { TJclDoubleIntfHashMap } ==========================================

constructor TJclDoubleIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclDoubleIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclDoubleIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclDoubleIntfBucket;
  ADest: TJclDoubleIntfHashMap;
  AMap: IJclDoubleIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclDoubleIntfHashMap then
    begin
      ADest := TJclDoubleIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclDoubleIntfBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclDoubleIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclDoubleIntfHashMap then
    TJclDoubleIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclDoubleIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.ContainsKey(const Key: Double): Boolean;
var
  I: Integer;
  Bucket: TJclDoubleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclDoubleIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclDoubleIntfHashMap.Equals(const AMap: IJclDoubleIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclDoubleIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclDoubleIntfHashMap.GetValue(const Key: Double): IInterface;
var
  I: Integer;
  Bucket: TJclDoubleIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclDoubleIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleIntfHashMap.KeyOfValue(const Value: IInterface): Double;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.KeysEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclDoubleIntfHashMap.KeySet: IJclDoubleSet;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleIntfHashMap.MoveArray(var List: TJclDoubleIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclDoubleIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclDoubleIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleIntfHashMap.PutAll(const AMap: IJclDoubleIntfMap);
var
  It: IJclDoubleIterator;
  Key: Double;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleIntfHashMap.PutValue(const Key: Double; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclDoubleIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclDoubleIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclDoubleIntfHashMap.Remove(const Key: Double): IInterface;
var
  Bucket: TJclDoubleIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


//=== { TJclIntfDoubleHashMap } ==========================================

constructor TJclIntfDoubleHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfDoubleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfDoubleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfDoubleBucket;
  ADest: TJclIntfDoubleHashMap;
  AMap: IJclIntfDoubleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfDoubleHashMap then
    begin
      ADest := TJclIntfDoubleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfDoubleBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfDoubleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfDoubleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfDoubleHashMap then
    TJclIntfDoubleHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfDoubleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfDoubleHashMap.ContainsValue(const Value: Double): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfDoubleHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfDoubleHashMap.Equals(const AMap: IJclIntfDoubleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfDoubleHashMap.FreeValue(var Value: Double): Double;
begin
  Result := Value;
  Value := 0.0;
end;



function TJclIntfDoubleHashMap.GetValue(const Key: IInterface): Double;
var
  I: Integer;
  Bucket: TJclIntfDoubleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfDoubleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfDoubleHashMap.KeyOfValue(const Value: Double): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfDoubleHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfDoubleHashMap.MoveArray(var List: TJclIntfDoubleHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfDoubleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfDoubleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfDoubleHashMap.PutAll(const AMap: IJclIntfDoubleMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfDoubleHashMap.PutValue(const Key: IInterface; const Value: Double);
var
  Index: Integer;
  Bucket: TJclIntfDoubleBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0.0)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfDoubleBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfDoubleHashMap.Remove(const Key: IInterface): Double;
var
  Bucket: TJclIntfDoubleBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfDoubleHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfDoubleHashMap.Values: IJclDoubleCollection;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.ValuesEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


//=== { TJclDoubleDoubleHashMap } ==========================================

constructor TJclDoubleDoubleHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclDoubleDoubleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclDoubleDoubleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclDoubleDoubleBucket;
  ADest: TJclDoubleDoubleHashMap;
  AMap: IJclDoubleDoubleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclDoubleDoubleHashMap then
    begin
      ADest := TJclDoubleDoubleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclDoubleDoubleBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclDoubleDoubleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleDoubleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclDoubleDoubleHashMap then
    TJclDoubleDoubleHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclDoubleDoubleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.ContainsKey(const Key: Double): Boolean;
var
  I: Integer;
  Bucket: TJclDoubleDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclDoubleDoubleHashMap.ContainsValue(const Value: Double): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleDoubleHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclDoubleDoubleHashMap.Equals(const AMap: IJclDoubleDoubleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclDoubleDoubleHashMap.FreeValue(var Value: Double): Double;
begin
  Result := Value;
  Value := 0.0;
end;



function TJclDoubleDoubleHashMap.GetValue(const Key: Double): Double;
var
  I: Integer;
  Bucket: TJclDoubleDoubleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclDoubleDoubleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleDoubleHashMap.KeyOfValue(const Value: Double): Double;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.KeysEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclDoubleDoubleHashMap.KeySet: IJclDoubleSet;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleDoubleHashMap.MoveArray(var List: TJclDoubleDoubleHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclDoubleDoubleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclDoubleDoubleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleDoubleHashMap.PutAll(const AMap: IJclDoubleDoubleMap);
var
  It: IJclDoubleIterator;
  Key: Double;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleDoubleHashMap.PutValue(const Key: Double; const Value: Double);
var
  Index: Integer;
  Bucket: TJclDoubleDoubleBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, 0.0)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclDoubleDoubleBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclDoubleDoubleHashMap.Remove(const Key: Double): Double;
var
  Bucket: TJclDoubleDoubleBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleDoubleHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleDoubleHashMap.Values: IJclDoubleCollection;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.ValuesEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


//=== { TJclExtendedIntfHashMap } ==========================================

constructor TJclExtendedIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclExtendedIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclExtendedIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclExtendedIntfBucket;
  ADest: TJclExtendedIntfHashMap;
  AMap: IJclExtendedIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclExtendedIntfHashMap then
    begin
      ADest := TJclExtendedIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclExtendedIntfBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclExtendedIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclExtendedIntfHashMap then
    TJclExtendedIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclExtendedIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.ContainsKey(const Key: Extended): Boolean;
var
  I: Integer;
  Bucket: TJclExtendedIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclExtendedIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclExtendedIntfHashMap.Equals(const AMap: IJclExtendedIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclExtendedIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclExtendedIntfHashMap.GetValue(const Key: Extended): IInterface;
var
  I: Integer;
  Bucket: TJclExtendedIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclExtendedIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedIntfHashMap.KeyOfValue(const Value: IInterface): Extended;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.KeysEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclExtendedIntfHashMap.KeySet: IJclExtendedSet;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedIntfHashMap.MoveArray(var List: TJclExtendedIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclExtendedIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclExtendedIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedIntfHashMap.PutAll(const AMap: IJclExtendedIntfMap);
var
  It: IJclExtendedIterator;
  Key: Extended;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedIntfHashMap.PutValue(const Key: Extended; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclExtendedIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclExtendedIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclExtendedIntfHashMap.Remove(const Key: Extended): IInterface;
var
  Bucket: TJclExtendedIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


//=== { TJclIntfExtendedHashMap } ==========================================

constructor TJclIntfExtendedHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfExtendedHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfExtendedHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfExtendedBucket;
  ADest: TJclIntfExtendedHashMap;
  AMap: IJclIntfExtendedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfExtendedHashMap then
    begin
      ADest := TJclIntfExtendedHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfExtendedBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfExtendedMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfExtendedHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfExtendedHashMap then
    TJclIntfExtendedHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfExtendedHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfExtendedHashMap.ContainsValue(const Value: Extended): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfExtendedHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfExtendedHashMap.Equals(const AMap: IJclIntfExtendedMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfExtendedHashMap.FreeValue(var Value: Extended): Extended;
begin
  Result := Value;
  Value := 0.0;
end;



function TJclIntfExtendedHashMap.GetValue(const Key: IInterface): Extended;
var
  I: Integer;
  Bucket: TJclIntfExtendedBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfExtendedHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfExtendedHashMap.KeyOfValue(const Value: Extended): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfExtendedHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfExtendedHashMap.MoveArray(var List: TJclIntfExtendedHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfExtendedHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfExtendedBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfExtendedHashMap.PutAll(const AMap: IJclIntfExtendedMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfExtendedHashMap.PutValue(const Key: IInterface; const Value: Extended);
var
  Index: Integer;
  Bucket: TJclIntfExtendedBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0.0)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfExtendedBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfExtendedHashMap.Remove(const Key: IInterface): Extended;
var
  Bucket: TJclIntfExtendedBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfExtendedHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfExtendedHashMap.Values: IJclExtendedCollection;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.ValuesEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


//=== { TJclExtendedExtendedHashMap } ==========================================

constructor TJclExtendedExtendedHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclExtendedExtendedHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclExtendedExtendedHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclExtendedExtendedBucket;
  ADest: TJclExtendedExtendedHashMap;
  AMap: IJclExtendedExtendedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclExtendedExtendedHashMap then
    begin
      ADest := TJclExtendedExtendedHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclExtendedExtendedBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclExtendedExtendedMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedExtendedHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclExtendedExtendedHashMap then
    TJclExtendedExtendedHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclExtendedExtendedHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.ContainsKey(const Key: Extended): Boolean;
var
  I: Integer;
  Bucket: TJclExtendedExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclExtendedExtendedHashMap.ContainsValue(const Value: Extended): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedExtendedHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclExtendedExtendedHashMap.Equals(const AMap: IJclExtendedExtendedMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclExtendedExtendedHashMap.FreeValue(var Value: Extended): Extended;
begin
  Result := Value;
  Value := 0.0;
end;



function TJclExtendedExtendedHashMap.GetValue(const Key: Extended): Extended;
var
  I: Integer;
  Bucket: TJclExtendedExtendedBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclExtendedExtendedHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedExtendedHashMap.KeyOfValue(const Value: Extended): Extended;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.KeysEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclExtendedExtendedHashMap.KeySet: IJclExtendedSet;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedExtendedHashMap.MoveArray(var List: TJclExtendedExtendedHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclExtendedExtendedHashMap.Pack;
var
  I: Integer;
  Bucket: TJclExtendedExtendedBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedExtendedHashMap.PutAll(const AMap: IJclExtendedExtendedMap);
var
  It: IJclExtendedIterator;
  Key: Extended;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedExtendedHashMap.PutValue(const Key: Extended; const Value: Extended);
var
  Index: Integer;
  Bucket: TJclExtendedExtendedBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, 0.0)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclExtendedExtendedBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclExtendedExtendedHashMap.Remove(const Key: Extended): Extended;
var
  Bucket: TJclExtendedExtendedBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedExtendedHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedExtendedHashMap.Values: IJclExtendedCollection;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.ValuesEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


//=== { TJclIntegerIntfHashMap } ==========================================

constructor TJclIntegerIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntegerIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntegerIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntegerIntfBucket;
  ADest: TJclIntegerIntfHashMap;
  AMap: IJclIntegerIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntegerIntfHashMap then
    begin
      ADest := TJclIntegerIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntegerIntfBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntegerIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntegerIntfHashMap then
    TJclIntegerIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntegerIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.ContainsKey(Key: Integer): Boolean;
var
  I: Integer;
  Bucket: TJclIntegerIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntegerIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntegerIntfHashMap.Equals(const AMap: IJclIntegerIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

function TJclIntegerIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclIntegerIntfHashMap.GetValue(Key: Integer): IInterface;
var
  I: Integer;
  Bucket: TJclIntegerIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclIntegerIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerIntfHashMap.KeyOfValue(const Value: IInterface): Integer;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.KeysEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclIntegerIntfHashMap.KeySet: IJclIntegerSet;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntfHashMap.MoveArray(var List: TJclIntegerIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntegerIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntegerIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntfHashMap.PutAll(const AMap: IJclIntegerIntfMap);
var
  It: IJclIntegerIterator;
  Key: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntfHashMap.PutValue(Key: Integer; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclIntegerIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntegerIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntegerIntfHashMap.Remove(Key: Integer): IInterface;
var
  Bucket: TJclIntegerIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


//=== { TJclIntfIntegerHashMap } ==========================================

constructor TJclIntfIntegerHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfIntegerHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfIntegerHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfIntegerBucket;
  ADest: TJclIntfIntegerHashMap;
  AMap: IJclIntfIntegerMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfIntegerHashMap then
    begin
      ADest := TJclIntfIntegerHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfIntegerBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfIntegerMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntegerHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfIntegerHashMap then
    TJclIntfIntegerHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfIntegerHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfIntegerHashMap.ContainsValue(Value: Integer): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntegerHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfIntegerHashMap.Equals(const AMap: IJclIntfIntegerMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfIntegerHashMap.FreeValue(var Value: Integer): Integer;
begin
  Result := Value;
  Value := 0;
end;



function TJclIntfIntegerHashMap.GetValue(const Key: IInterface): Integer;
var
  I: Integer;
  Bucket: TJclIntfIntegerBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfIntegerHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfIntegerHashMap.KeyOfValue(Value: Integer): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfIntegerHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntegerHashMap.MoveArray(var List: TJclIntfIntegerHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfIntegerHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfIntegerBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntegerHashMap.PutAll(const AMap: IJclIntfIntegerMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntegerHashMap.PutValue(const Key: IInterface; Value: Integer);
var
  Index: Integer;
  Bucket: TJclIntfIntegerBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfIntegerBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfIntegerHashMap.Remove(const Key: IInterface): Integer;
var
  Bucket: TJclIntfIntegerBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntegerHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfIntegerHashMap.Values: IJclIntegerCollection;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.ValuesEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


//=== { TJclIntegerIntegerHashMap } ==========================================

constructor TJclIntegerIntegerHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntegerIntegerHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntegerIntegerHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntegerIntegerBucket;
  ADest: TJclIntegerIntegerHashMap;
  AMap: IJclIntegerIntegerMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntegerIntegerHashMap then
    begin
      ADest := TJclIntegerIntegerHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntegerIntegerBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntegerIntegerMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntegerHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntegerIntegerHashMap then
    TJclIntegerIntegerHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntegerIntegerHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.ContainsKey(Key: Integer): Boolean;
var
  I: Integer;
  Bucket: TJclIntegerIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntegerIntegerHashMap.ContainsValue(Value: Integer): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerIntegerHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntegerIntegerHashMap.Equals(const AMap: IJclIntegerIntegerMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

function TJclIntegerIntegerHashMap.FreeValue(var Value: Integer): Integer;
begin
  Result := Value;
  Value := 0;
end;



function TJclIntegerIntegerHashMap.GetValue(Key: Integer): Integer;
var
  I: Integer;
  Bucket: TJclIntegerIntegerBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclIntegerIntegerHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerIntegerHashMap.KeyOfValue(Value: Integer): Integer;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.KeysEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclIntegerIntegerHashMap.KeySet: IJclIntegerSet;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntegerHashMap.MoveArray(var List: TJclIntegerIntegerHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntegerIntegerHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntegerIntegerBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntegerHashMap.PutAll(const AMap: IJclIntegerIntegerMap);
var
  It: IJclIntegerIterator;
  Key: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntegerHashMap.PutValue(Key: Integer; Value: Integer);
var
  Index: Integer;
  Bucket: TJclIntegerIntegerBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, 0)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntegerIntegerBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntegerIntegerHashMap.Remove(Key: Integer): Integer;
var
  Bucket: TJclIntegerIntegerBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntegerHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerIntegerHashMap.Values: IJclIntegerCollection;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.ValuesEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


//=== { TJclCardinalIntfHashMap } ==========================================

constructor TJclCardinalIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclCardinalIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclCardinalIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclCardinalIntfBucket;
  ADest: TJclCardinalIntfHashMap;
  AMap: IJclCardinalIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclCardinalIntfHashMap then
    begin
      ADest := TJclCardinalIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclCardinalIntfBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclCardinalIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclCardinalIntfHashMap then
    TJclCardinalIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclCardinalIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.ContainsKey(Key: Cardinal): Boolean;
var
  I: Integer;
  Bucket: TJclCardinalIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclCardinalIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclCardinalIntfHashMap.Equals(const AMap: IJclCardinalIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

function TJclCardinalIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclCardinalIntfHashMap.GetValue(Key: Cardinal): IInterface;
var
  I: Integer;
  Bucket: TJclCardinalIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclCardinalIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalIntfHashMap.KeyOfValue(const Value: IInterface): Cardinal;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.KeysEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclCardinalIntfHashMap.KeySet: IJclCardinalSet;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalIntfHashMap.MoveArray(var List: TJclCardinalIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclCardinalIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclCardinalIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalIntfHashMap.PutAll(const AMap: IJclCardinalIntfMap);
var
  It: IJclCardinalIterator;
  Key: Cardinal;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalIntfHashMap.PutValue(Key: Cardinal; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclCardinalIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclCardinalIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclCardinalIntfHashMap.Remove(Key: Cardinal): IInterface;
var
  Bucket: TJclCardinalIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


//=== { TJclIntfCardinalHashMap } ==========================================

constructor TJclIntfCardinalHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfCardinalHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfCardinalHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfCardinalBucket;
  ADest: TJclIntfCardinalHashMap;
  AMap: IJclIntfCardinalMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfCardinalHashMap then
    begin
      ADest := TJclIntfCardinalHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfCardinalBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfCardinalMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfCardinalHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfCardinalHashMap then
    TJclIntfCardinalHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfCardinalHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfCardinalHashMap.ContainsValue(Value: Cardinal): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfCardinalHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfCardinalHashMap.Equals(const AMap: IJclIntfCardinalMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfCardinalHashMap.FreeValue(var Value: Cardinal): Cardinal;
begin
  Result := Value;
  Value := 0;
end;



function TJclIntfCardinalHashMap.GetValue(const Key: IInterface): Cardinal;
var
  I: Integer;
  Bucket: TJclIntfCardinalBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfCardinalHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfCardinalHashMap.KeyOfValue(Value: Cardinal): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfCardinalHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfCardinalHashMap.MoveArray(var List: TJclIntfCardinalHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfCardinalHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfCardinalBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfCardinalHashMap.PutAll(const AMap: IJclIntfCardinalMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfCardinalHashMap.PutValue(const Key: IInterface; Value: Cardinal);
var
  Index: Integer;
  Bucket: TJclIntfCardinalBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfCardinalBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfCardinalHashMap.Remove(const Key: IInterface): Cardinal;
var
  Bucket: TJclIntfCardinalBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfCardinalHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfCardinalHashMap.Values: IJclCardinalCollection;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.ValuesEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


//=== { TJclCardinalCardinalHashMap } ==========================================

constructor TJclCardinalCardinalHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclCardinalCardinalHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclCardinalCardinalHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclCardinalCardinalBucket;
  ADest: TJclCardinalCardinalHashMap;
  AMap: IJclCardinalCardinalMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclCardinalCardinalHashMap then
    begin
      ADest := TJclCardinalCardinalHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclCardinalCardinalBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclCardinalCardinalMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalCardinalHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclCardinalCardinalHashMap then
    TJclCardinalCardinalHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclCardinalCardinalHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.ContainsKey(Key: Cardinal): Boolean;
var
  I: Integer;
  Bucket: TJclCardinalCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclCardinalCardinalHashMap.ContainsValue(Value: Cardinal): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalCardinalHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclCardinalCardinalHashMap.Equals(const AMap: IJclCardinalCardinalMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

function TJclCardinalCardinalHashMap.FreeValue(var Value: Cardinal): Cardinal;
begin
  Result := Value;
  Value := 0;
end;



function TJclCardinalCardinalHashMap.GetValue(Key: Cardinal): Cardinal;
var
  I: Integer;
  Bucket: TJclCardinalCardinalBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclCardinalCardinalHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalCardinalHashMap.KeyOfValue(Value: Cardinal): Cardinal;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.KeysEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclCardinalCardinalHashMap.KeySet: IJclCardinalSet;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalCardinalHashMap.MoveArray(var List: TJclCardinalCardinalHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclCardinalCardinalHashMap.Pack;
var
  I: Integer;
  Bucket: TJclCardinalCardinalBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalCardinalHashMap.PutAll(const AMap: IJclCardinalCardinalMap);
var
  It: IJclCardinalIterator;
  Key: Cardinal;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalCardinalHashMap.PutValue(Key: Cardinal; Value: Cardinal);
var
  Index: Integer;
  Bucket: TJclCardinalCardinalBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, 0)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclCardinalCardinalBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclCardinalCardinalHashMap.Remove(Key: Cardinal): Cardinal;
var
  Bucket: TJclCardinalCardinalBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalCardinalHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalCardinalHashMap.Values: IJclCardinalCollection;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.ValuesEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


//=== { TJclInt64IntfHashMap } ==========================================

constructor TJclInt64IntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclInt64IntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclInt64IntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclInt64IntfBucket;
  ADest: TJclInt64IntfHashMap;
  AMap: IJclInt64IntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclInt64IntfHashMap then
    begin
      ADest := TJclInt64IntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclInt64IntfBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclInt64IntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64IntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclInt64IntfHashMap then
    TJclInt64IntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclInt64IntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclInt64IntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.ContainsKey(const Key: Int64): Boolean;
var
  I: Integer;
  Bucket: TJclInt64IntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclInt64IntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64IntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64IntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclInt64IntfHashMap.Equals(const AMap: IJclInt64IntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64IntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

function TJclInt64IntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclInt64IntfHashMap.GetValue(const Key: Int64): IInterface;
var
  I: Integer;
  Bucket: TJclInt64IntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclInt64IntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64IntfHashMap.KeyOfValue(const Value: IInterface): Int64;
var
  I, J: Integer;
  Bucket: TJclInt64IntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.KeysEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclInt64IntfHashMap.KeySet: IJclInt64Set;
var
  I, J: Integer;
  Bucket: TJclInt64IntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64IntfHashMap.MoveArray(var List: TJclInt64IntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclInt64IntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclInt64IntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64IntfHashMap.PutAll(const AMap: IJclInt64IntfMap);
var
  It: IJclInt64Iterator;
  Key: Int64;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64IntfHashMap.PutValue(const Key: Int64; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclInt64IntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclInt64IntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclInt64IntfHashMap.Remove(const Key: Int64): IInterface;
var
  Bucket: TJclInt64IntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64IntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64IntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclInt64IntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


//=== { TJclIntfInt64HashMap } ==========================================

constructor TJclIntfInt64HashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfInt64HashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfInt64HashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfInt64Bucket;
  ADest: TJclIntfInt64HashMap;
  AMap: IJclIntfInt64Map;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfInt64HashMap then
    begin
      ADest := TJclIntfInt64HashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfInt64Bucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfInt64Map, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfInt64HashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfInt64HashMap then
    TJclIntfInt64HashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfInt64HashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfInt64Bucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfInt64HashMap.ContainsValue(const Value: Int64): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfInt64HashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfInt64HashMap.Equals(const AMap: IJclIntfInt64Map): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfInt64HashMap.FreeValue(var Value: Int64): Int64;
begin
  Result := Value;
  Value := 0;
end;



function TJclIntfInt64HashMap.GetValue(const Key: IInterface): Int64;
var
  I: Integer;
  Bucket: TJclIntfInt64Bucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfInt64HashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfInt64HashMap.KeyOfValue(const Value: Int64): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfInt64Bucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfInt64HashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfInt64HashMap.MoveArray(var List: TJclIntfInt64HashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfInt64HashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfInt64Bucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfInt64HashMap.PutAll(const AMap: IJclIntfInt64Map);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfInt64HashMap.PutValue(const Key: IInterface; const Value: Int64);
var
  Index: Integer;
  Bucket: TJclIntfInt64Bucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfInt64Bucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfInt64HashMap.Remove(const Key: IInterface): Int64;
var
  Bucket: TJclIntfInt64Bucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfInt64HashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfInt64HashMap.Values: IJclInt64Collection;
var
  I, J: Integer;
  Bucket: TJclIntfInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.ValuesEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


//=== { TJclInt64Int64HashMap } ==========================================

constructor TJclInt64Int64HashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclInt64Int64HashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclInt64Int64HashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclInt64Int64Bucket;
  ADest: TJclInt64Int64HashMap;
  AMap: IJclInt64Int64Map;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclInt64Int64HashMap then
    begin
      ADest := TJclInt64Int64HashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclInt64Int64Bucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclInt64Int64Map, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Int64HashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclInt64Int64HashMap then
    TJclInt64Int64HashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclInt64Int64HashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclInt64Int64Bucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.ContainsKey(const Key: Int64): Boolean;
var
  I: Integer;
  Bucket: TJclInt64Int64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclInt64Int64HashMap.ContainsValue(const Value: Int64): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64Int64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Int64HashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclInt64Int64HashMap.Equals(const AMap: IJclInt64Int64Map): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64Int64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

function TJclInt64Int64HashMap.FreeValue(var Value: Int64): Int64;
begin
  Result := Value;
  Value := 0;
end;



function TJclInt64Int64HashMap.GetValue(const Key: Int64): Int64;
var
  I: Integer;
  Bucket: TJclInt64Int64Bucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclInt64Int64HashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64Int64HashMap.KeyOfValue(const Value: Int64): Int64;
var
  I, J: Integer;
  Bucket: TJclInt64Int64Bucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.KeysEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclInt64Int64HashMap.KeySet: IJclInt64Set;
var
  I, J: Integer;
  Bucket: TJclInt64Int64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Int64HashMap.MoveArray(var List: TJclInt64Int64HashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclInt64Int64HashMap.Pack;
var
  I: Integer;
  Bucket: TJclInt64Int64Bucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Int64HashMap.PutAll(const AMap: IJclInt64Int64Map);
var
  It: IJclInt64Iterator;
  Key: Int64;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Int64HashMap.PutValue(const Key: Int64; const Value: Int64);
var
  Index: Integer;
  Bucket: TJclInt64Int64Bucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, 0)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclInt64Int64Bucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclInt64Int64HashMap.Remove(const Key: Int64): Int64;
var
  Bucket: TJclInt64Int64Bucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Int64HashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64Int64HashMap.Values: IJclInt64Collection;
var
  I, J: Integer;
  Bucket: TJclInt64Int64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.ValuesEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


{$IFNDEF CLR}
//=== { TJclPtrIntfHashMap } ==========================================

constructor TJclPtrIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclPtrIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclPtrIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclPtrIntfBucket;
  ADest: TJclPtrIntfHashMap;
  AMap: IJclPtrIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclPtrIntfHashMap then
    begin
      ADest := TJclPtrIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclPtrIntfBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclPtrIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclPtrIntfHashMap then
    TJclPtrIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclPtrIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclPtrIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.ContainsKey(Key: Pointer): Boolean;
var
  I: Integer;
  Bucket: TJclPtrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclPtrIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclPtrIntfHashMap.Equals(const AMap: IJclPtrIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

function TJclPtrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclPtrIntfHashMap.GetValue(Key: Pointer): IInterface;
var
  I: Integer;
  Bucket: TJclPtrIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclPtrIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrIntfHashMap.KeyOfValue(const Value: IInterface): Pointer;
var
  I, J: Integer;
  Bucket: TJclPtrIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.KeysEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclPtrIntfHashMap.KeySet: IJclPtrSet;
var
  I, J: Integer;
  Bucket: TJclPtrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrIntfHashMap.MoveArray(var List: TJclPtrIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclPtrIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclPtrIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrIntfHashMap.PutAll(const AMap: IJclPtrIntfMap);
var
  It: IJclPtrIterator;
  Key: Pointer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrIntfHashMap.PutValue(Key: Pointer; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclPtrIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclPtrIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclPtrIntfHashMap.Remove(Key: Pointer): IInterface;
var
  Bucket: TJclPtrIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclPtrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


//=== { TJclIntfPtrHashMap } ==========================================

constructor TJclIntfPtrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfPtrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfPtrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfPtrBucket;
  ADest: TJclIntfPtrHashMap;
  AMap: IJclIntfPtrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfPtrHashMap then
    begin
      ADest := TJclIntfPtrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfPtrBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfPtrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfPtrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfPtrHashMap then
    TJclIntfPtrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfPtrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfPtrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfPtrHashMap.ContainsValue(Value: Pointer): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfPtrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfPtrHashMap.Equals(const AMap: IJclIntfPtrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfPtrHashMap.FreeValue(var Value: Pointer): Pointer;
begin
  Result := Value;
  Value := nil;
end;



function TJclIntfPtrHashMap.GetValue(const Key: IInterface): Pointer;
var
  I: Integer;
  Bucket: TJclIntfPtrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfPtrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfPtrHashMap.KeyOfValue(Value: Pointer): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfPtrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfPtrHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfPtrHashMap.MoveArray(var List: TJclIntfPtrHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfPtrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfPtrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfPtrHashMap.PutAll(const AMap: IJclIntfPtrMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfPtrHashMap.PutValue(const Key: IInterface; Value: Pointer);
var
  Index: Integer;
  Bucket: TJclIntfPtrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfPtrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfPtrHashMap.Remove(const Key: IInterface): Pointer;
var
  Bucket: TJclIntfPtrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfPtrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfPtrHashMap.Values: IJclPtrCollection;
var
  I, J: Integer;
  Bucket: TJclIntfPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.ValuesEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;


//=== { TJclPtrPtrHashMap } ==========================================

constructor TJclPtrPtrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclPtrPtrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclPtrPtrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclPtrPtrBucket;
  ADest: TJclPtrPtrHashMap;
  AMap: IJclPtrPtrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclPtrPtrHashMap then
    begin
      ADest := TJclPtrPtrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclPtrPtrBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclPtrPtrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrPtrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclPtrPtrHashMap then
    TJclPtrPtrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclPtrPtrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclPtrPtrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.ContainsKey(Key: Pointer): Boolean;
var
  I: Integer;
  Bucket: TJclPtrPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclPtrPtrHashMap.ContainsValue(Value: Pointer): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrPtrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclPtrPtrHashMap.Equals(const AMap: IJclPtrPtrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

function TJclPtrPtrHashMap.FreeValue(var Value: Pointer): Pointer;
begin
  Result := Value;
  Value := nil;
end;



function TJclPtrPtrHashMap.GetValue(Key: Pointer): Pointer;
var
  I: Integer;
  Bucket: TJclPtrPtrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclPtrPtrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrPtrHashMap.KeyOfValue(Value: Pointer): Pointer;
var
  I, J: Integer;
  Bucket: TJclPtrPtrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.KeysEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclPtrPtrHashMap.KeySet: IJclPtrSet;
var
  I, J: Integer;
  Bucket: TJclPtrPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrPtrHashMap.MoveArray(var List: TJclPtrPtrHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclPtrPtrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclPtrPtrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrPtrHashMap.PutAll(const AMap: IJclPtrPtrMap);
var
  It: IJclPtrIterator;
  Key: Pointer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrPtrHashMap.PutValue(Key: Pointer; Value: Pointer);
var
  Index: Integer;
  Bucket: TJclPtrPtrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclPtrPtrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclPtrPtrHashMap.Remove(Key: Pointer): Pointer;
var
  Bucket: TJclPtrPtrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrPtrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrPtrHashMap.Values: IJclPtrCollection;
var
  I, J: Integer;
  Bucket: TJclPtrPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.ValuesEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$ENDIF ~CLR}

//=== { TJclIntfHashMap } ==========================================

constructor TJclIntfHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfBucket;
  ADest: TJclIntfHashMap;
  AMap: IJclIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfHashMap then
    begin
      ADest := TJclIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfHashMap then
    TJclIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntfHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclIntfHashMap.Equals(const AMap: IJclIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;


function TJclIntfHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclIntfHashMap.GetValue(const Key: IInterface): TObject;
var
  I: Integer;
  Bucket: TJclIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfHashMap.KeyOfValue(Value: TObject): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashMap.MoveArray(var List: TJclIntfHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashMap.PutAll(const AMap: IJclIntfMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashMap.PutValue(const Key: IInterface; Value: TObject);
var
  Index: Integer;
  Bucket: TJclIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntfHashMap.Remove(const Key: IInterface): TObject;
var
  Bucket: TJclIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


//=== { TJclAnsiStrHashMap } ==========================================

constructor TJclAnsiStrHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclAnsiStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclAnsiStrBucket;
  ADest: TJclAnsiStrHashMap;
  AMap: IJclAnsiStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclAnsiStrHashMap then
    begin
      ADest := TJclAnsiStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclAnsiStrBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclAnsiStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclAnsiStrHashMap then
    TJclAnsiStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclAnsiStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclAnsiStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.ContainsKey(const Key: AnsiString): Boolean;
var
  I: Integer;
  Bucket: TJclAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclAnsiStrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrHashMap.Equals(const AMap: IJclAnsiStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;


function TJclAnsiStrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclAnsiStrHashMap.GetValue(const Key: AnsiString): TObject;
var
  I: Integer;
  Bucket: TJclAnsiStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclAnsiStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrHashMap.KeyOfValue(Value: TObject): AnsiString;
var
  I, J: Integer;
  Bucket: TJclAnsiStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclAnsiStrHashMap.KeySet: IJclAnsiStrSet;
var
  I, J: Integer;
  Bucket: TJclAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashMap.MoveArray(var List: TJclAnsiStrHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclAnsiStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclAnsiStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashMap.PutAll(const AMap: IJclAnsiStrMap);
var
  It: IJclAnsiStrIterator;
  Key: AnsiString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashMap.PutValue(const Key: AnsiString; Value: TObject);
var
  Index: Integer;
  Bucket: TJclAnsiStrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclAnsiStrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclAnsiStrHashMap.Remove(const Key: AnsiString): TObject;
var
  Bucket: TJclAnsiStrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


//=== { TJclWideStrHashMap } ==========================================

constructor TJclWideStrHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclWideStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclWideStrBucket;
  ADest: TJclWideStrHashMap;
  AMap: IJclWideStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclWideStrHashMap then
    begin
      ADest := TJclWideStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclWideStrBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclWideStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclWideStrHashMap then
    TJclWideStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclWideStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclWideStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.ContainsKey(const Key: WideString): Boolean;
var
  I: Integer;
  Bucket: TJclWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclWideStrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclWideStrHashMap.Equals(const AMap: IJclWideStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;


function TJclWideStrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclWideStrHashMap.GetValue(const Key: WideString): TObject;
var
  I: Integer;
  Bucket: TJclWideStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclWideStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrHashMap.KeyOfValue(Value: TObject): WideString;
var
  I, J: Integer;
  Bucket: TJclWideStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclWideStrHashMap.KeySet: IJclWideStrSet;
var
  I, J: Integer;
  Bucket: TJclWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashMap.MoveArray(var List: TJclWideStrHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclWideStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclWideStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashMap.PutAll(const AMap: IJclWideStrMap);
var
  It: IJclWideStrIterator;
  Key: WideString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashMap.PutValue(const Key: WideString; Value: TObject);
var
  Index: Integer;
  Bucket: TJclWideStrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclWideStrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclWideStrHashMap.Remove(const Key: WideString): TObject;
var
  Bucket: TJclWideStrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


//=== { TJclSingleHashMap } ==========================================

constructor TJclSingleHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclSingleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclSingleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclSingleBucket;
  ADest: TJclSingleHashMap;
  AMap: IJclSingleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclSingleHashMap then
    begin
      ADest := TJclSingleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclSingleBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclSingleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclSingleHashMap then
    TJclSingleHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclSingleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclSingleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.ContainsKey(const Key: Single): Boolean;
var
  I: Integer;
  Bucket: TJclSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclSingleHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclSingleHashMap.Equals(const AMap: IJclSingleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclSingleHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;


function TJclSingleHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclSingleHashMap.GetValue(const Key: Single): TObject;
var
  I: Integer;
  Bucket: TJclSingleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclSingleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleHashMap.KeyOfValue(Value: TObject): Single;
var
  I, J: Integer;
  Bucket: TJclSingleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.KeysEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclSingleHashMap.KeySet: IJclSingleSet;
var
  I, J: Integer;
  Bucket: TJclSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashMap.MoveArray(var List: TJclSingleHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclSingleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclSingleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashMap.PutAll(const AMap: IJclSingleMap);
var
  It: IJclSingleIterator;
  Key: Single;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashMap.PutValue(const Key: Single; Value: TObject);
var
  Index: Integer;
  Bucket: TJclSingleBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclSingleBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclSingleHashMap.Remove(const Key: Single): TObject;
var
  Bucket: TJclSingleBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


//=== { TJclDoubleHashMap } ==========================================

constructor TJclDoubleHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclDoubleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclDoubleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclDoubleBucket;
  ADest: TJclDoubleHashMap;
  AMap: IJclDoubleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclDoubleHashMap then
    begin
      ADest := TJclDoubleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclDoubleBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclDoubleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclDoubleHashMap then
    TJclDoubleHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclDoubleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclDoubleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.ContainsKey(const Key: Double): Boolean;
var
  I: Integer;
  Bucket: TJclDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclDoubleHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclDoubleHashMap.Equals(const AMap: IJclDoubleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclDoubleHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;


function TJclDoubleHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclDoubleHashMap.GetValue(const Key: Double): TObject;
var
  I: Integer;
  Bucket: TJclDoubleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclDoubleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleHashMap.KeyOfValue(Value: TObject): Double;
var
  I, J: Integer;
  Bucket: TJclDoubleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.KeysEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclDoubleHashMap.KeySet: IJclDoubleSet;
var
  I, J: Integer;
  Bucket: TJclDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashMap.MoveArray(var List: TJclDoubleHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclDoubleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclDoubleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashMap.PutAll(const AMap: IJclDoubleMap);
var
  It: IJclDoubleIterator;
  Key: Double;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashMap.PutValue(const Key: Double; Value: TObject);
var
  Index: Integer;
  Bucket: TJclDoubleBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclDoubleBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclDoubleHashMap.Remove(const Key: Double): TObject;
var
  Bucket: TJclDoubleBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


//=== { TJclExtendedHashMap } ==========================================

constructor TJclExtendedHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclExtendedHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclExtendedHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclExtendedBucket;
  ADest: TJclExtendedHashMap;
  AMap: IJclExtendedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclExtendedHashMap then
    begin
      ADest := TJclExtendedHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclExtendedBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclExtendedMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclExtendedHashMap then
    TJclExtendedHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclExtendedHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclExtendedBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.ContainsKey(const Key: Extended): Boolean;
var
  I: Integer;
  Bucket: TJclExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclExtendedHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclExtendedHashMap.Equals(const AMap: IJclExtendedMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclExtendedHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;


function TJclExtendedHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclExtendedHashMap.GetValue(const Key: Extended): TObject;
var
  I: Integer;
  Bucket: TJclExtendedBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclExtendedHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedHashMap.KeyOfValue(Value: TObject): Extended;
var
  I, J: Integer;
  Bucket: TJclExtendedBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.KeysEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclExtendedHashMap.KeySet: IJclExtendedSet;
var
  I, J: Integer;
  Bucket: TJclExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashMap.MoveArray(var List: TJclExtendedHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0.0;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclExtendedHashMap.Pack;
var
  I: Integer;
  Bucket: TJclExtendedBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashMap.PutAll(const AMap: IJclExtendedMap);
var
  It: IJclExtendedIterator;
  Key: Extended;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashMap.PutValue(const Key: Extended; Value: TObject);
var
  Index: Integer;
  Bucket: TJclExtendedBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclExtendedBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclExtendedHashMap.Remove(const Key: Extended): TObject;
var
  Bucket: TJclExtendedBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


//=== { TJclIntegerHashMap } ==========================================

constructor TJclIntegerHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntegerHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntegerHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntegerBucket;
  ADest: TJclIntegerHashMap;
  AMap: IJclIntegerMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntegerHashMap then
    begin
      ADest := TJclIntegerHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntegerBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclIntegerMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntegerHashMap then
    TJclIntegerHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntegerHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntegerBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.ContainsKey(Key: Integer): Boolean;
var
  I: Integer;
  Bucket: TJclIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclIntegerHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclIntegerHashMap.Equals(const AMap: IJclIntegerMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

function TJclIntegerHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;


function TJclIntegerHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclIntegerHashMap.GetValue(Key: Integer): TObject;
var
  I: Integer;
  Bucket: TJclIntegerBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclIntegerHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerHashMap.KeyOfValue(Value: TObject): Integer;
var
  I, J: Integer;
  Bucket: TJclIntegerBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.KeysEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclIntegerHashMap.KeySet: IJclIntegerSet;
var
  I, J: Integer;
  Bucket: TJclIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashMap.MoveArray(var List: TJclIntegerHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntegerHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntegerBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashMap.PutAll(const AMap: IJclIntegerMap);
var
  It: IJclIntegerIterator;
  Key: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashMap.PutValue(Key: Integer; Value: TObject);
var
  Index: Integer;
  Bucket: TJclIntegerBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntegerBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclIntegerHashMap.Remove(Key: Integer): TObject;
var
  Bucket: TJclIntegerBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


//=== { TJclCardinalHashMap } ==========================================

constructor TJclCardinalHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclCardinalHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclCardinalHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclCardinalBucket;
  ADest: TJclCardinalHashMap;
  AMap: IJclCardinalMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclCardinalHashMap then
    begin
      ADest := TJclCardinalHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclCardinalBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclCardinalMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclCardinalHashMap then
    TJclCardinalHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclCardinalHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclCardinalBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.ContainsKey(Key: Cardinal): Boolean;
var
  I: Integer;
  Bucket: TJclCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclCardinalHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclCardinalHashMap.Equals(const AMap: IJclCardinalMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

function TJclCardinalHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;


function TJclCardinalHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclCardinalHashMap.GetValue(Key: Cardinal): TObject;
var
  I: Integer;
  Bucket: TJclCardinalBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclCardinalHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalHashMap.KeyOfValue(Value: TObject): Cardinal;
var
  I, J: Integer;
  Bucket: TJclCardinalBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.KeysEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclCardinalHashMap.KeySet: IJclCardinalSet;
var
  I, J: Integer;
  Bucket: TJclCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashMap.MoveArray(var List: TJclCardinalHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclCardinalHashMap.Pack;
var
  I: Integer;
  Bucket: TJclCardinalBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashMap.PutAll(const AMap: IJclCardinalMap);
var
  It: IJclCardinalIterator;
  Key: Cardinal;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashMap.PutValue(Key: Cardinal; Value: TObject);
var
  Index: Integer;
  Bucket: TJclCardinalBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclCardinalBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclCardinalHashMap.Remove(Key: Cardinal): TObject;
var
  Bucket: TJclCardinalBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


//=== { TJclInt64HashMap } ==========================================

constructor TJclInt64HashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclInt64HashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclInt64HashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclInt64Bucket;
  ADest: TJclInt64HashMap;
  AMap: IJclInt64Map;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclInt64HashMap then
    begin
      ADest := TJclInt64HashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclInt64Bucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclInt64Map, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclInt64HashMap then
    TJclInt64HashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclInt64HashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclInt64Bucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.ContainsKey(const Key: Int64): Boolean;
var
  I: Integer;
  Bucket: TJclInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclInt64HashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64HashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclInt64HashMap.Equals(const AMap: IJclInt64Map): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

function TJclInt64HashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;


function TJclInt64HashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclInt64HashMap.GetValue(const Key: Int64): TObject;
var
  I: Integer;
  Bucket: TJclInt64Bucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclInt64HashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64HashMap.KeyOfValue(Value: TObject): Int64;
var
  I, J: Integer;
  Bucket: TJclInt64Bucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.KeysEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclInt64HashMap.KeySet: IJclInt64Set;
var
  I, J: Integer;
  Bucket: TJclInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashMap.MoveArray(var List: TJclInt64HashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := 0;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclInt64HashMap.Pack;
var
  I: Integer;
  Bucket: TJclInt64Bucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashMap.PutAll(const AMap: IJclInt64Map);
var
  It: IJclInt64Iterator;
  Key: Int64;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashMap.PutValue(const Key: Int64; Value: TObject);
var
  Index: Integer;
  Bucket: TJclInt64Bucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclInt64Bucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclInt64HashMap.Remove(const Key: Int64): TObject;
var
  Bucket: TJclInt64Bucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64HashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


{$IFNDEF CLR}
//=== { TJclPtrHashMap } ==========================================

constructor TJclPtrHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclPtrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclPtrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclPtrBucket;
  ADest: TJclPtrHashMap;
  AMap: IJclPtrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclPtrHashMap then
    begin
      ADest := TJclPtrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclPtrBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclPtrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclPtrHashMap then
    TJclPtrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclPtrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclPtrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.ContainsKey(Key: Pointer): Boolean;
var
  I: Integer;
  Bucket: TJclPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclPtrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclPtrHashMap.Equals(const AMap: IJclPtrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

function TJclPtrHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;


function TJclPtrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclPtrHashMap.GetValue(Key: Pointer): TObject;
var
  I: Integer;
  Bucket: TJclPtrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclPtrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrHashMap.KeyOfValue(Value: TObject): Pointer;
var
  I, J: Integer;
  Bucket: TJclPtrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.KeysEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclPtrHashMap.KeySet: IJclPtrSet;
var
  I, J: Integer;
  Bucket: TJclPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashMap.MoveArray(var List: TJclPtrHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclPtrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclPtrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashMap.PutAll(const AMap: IJclPtrMap);
var
  It: IJclPtrIterator;
  Key: Pointer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashMap.PutValue(Key: Pointer; Value: TObject);
var
  Index: Integer;
  Bucket: TJclPtrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclPtrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclPtrHashMap.Remove(Key: Pointer): TObject;
var
  Bucket: TJclPtrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$ENDIF ~CLR}

//=== { TJclHashMap } ==========================================

constructor TJclHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create;
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclBucket;
  ADest: TJclHashMap;
  AMap: IJclMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclHashMap then
    begin
      ADest := TJclHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclHashMap then
    TJclHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.ContainsKey(Key: TObject): Boolean;
var
  I: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMap.Create(FCapacity, False, False);
  AssignPropertiesTo(Result);
end;

function TJclHashMap.Equals(const AMap: IJclMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.FreeKey(var Key: TObject): TObject;
begin
  if FOwnsKeys then
  begin
    Result := nil;
    FreeAndNil(Key);
  end
  else
  begin
    Result := Key;
    Key := nil;
  end;
end;

function TJclHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;

function TJclHashMap.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

function TJclHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclHashMap.GetValue(Key: TObject): TObject;
var
  I: Integer;
  Bucket: TJclBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.Hash(AObject: TObject): Integer;
begin
  Result := Integer(AObject);
end;

function TJclHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclHashMap.KeyOfValue(Value: TObject): TObject;
var
  I, J: Integer;
  Bucket: TJclBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.KeysEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclHashMap.KeySet: IJclSet;
var
  I, J: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArraySet.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.MoveArray(var List: TJclHashEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclHashMap.Pack;
var
  I: Integer;
  Bucket: TJclBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.PutAll(const AMap: IJclMap);
var
  It: IJclIterator;
  Key: TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.PutValue(Key: TObject; Value: TObject);
var
  Index: Integer;
  Bucket: TJclBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclHashMap.Remove(Key: TObject): TObject;
var
  Bucket: TJclBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


{$IFDEF SUPPORTS_GENERICS}

//=== { TJclHashMap<TKey, TValue> } ==========================================

constructor TJclHashMap<TKey, TValue>.Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create;
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclHashMap<TKey, TValue>.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclHashMap<TKey, TValue>.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclBucket<TKey, TValue>;
  ADest: TJclHashMap<TKey, TValue>;
  AMap: IJclMap<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclHashMap<TKey, TValue> then
    begin
      ADest := TJclHashMap<TKey, TValue>(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclBucket<TKey, TValue>.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    end
    else
    if Supports(IInterface(Dest), IJclMap<TKey, TValue>, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclHashMap<TKey, TValue> then
    TJclHashMap<TKey, TValue>(Dest).HashFunction := HashFunction;
end;

procedure TJclHashMap<TKey, TValue>.Clear;
var
  I, J: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;
var
  I: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
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

function TJclHashMap<TKey, TValue>.ContainsValue(const Value: TValue): Boolean;
var
  I, J: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclHashMap<TKey, TValue>.Equals(const AMap: IJclMap<TKey, TValue>): Boolean;
var
  I, J: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.FreeKey(var Key: TKey): TKey;
begin
  if FOwnsKeys then
  begin
    Result := Default(TKey);
    FreeAndNil(Key);
  end
  else
  begin
    Result := Key;
    Key := Default(TKey);
  end;
end;

function TJclHashMap<TKey, TValue>.FreeValue(var Value: TValue): TValue;
begin
  if FOwnsValues then
  begin
    Result := Default(TValue);
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := Default(TValue);
  end;
end;

function TJclHashMap<TKey, TValue>.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

function TJclHashMap<TKey, TValue>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclHashMap<TKey, TValue>.GetValue(const Key: TKey): TValue;
var
  I: Integer;
  Bucket: TJclBucket<TKey, TValue>;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := Default(TValue);
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclHashMap<TKey, TValue>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclHashMap<TKey, TValue>.KeyOfValue(const Value: TValue): TKey;
var
  I, J: Integer;
  Bucket: TJclBucket<TKey, TValue>;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := Default(TKey);
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclHashMap<TKey, TValue>.KeySet: IJclSet<TKey>;
var
  I, J: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyArraySet(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.MoveArray(var List: TJclHashEntryArray<TKey, TValue>; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        List[FromIndex + I].Key := Default(TKey);
        List[FromIndex + I].Value := Default(TValue);
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := Default(TKey);
        List[FromIndex + I].Value := Default(TValue);
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := Default(TKey);
        List[FromIndex + I].Value := Default(TValue);
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := Default(TKey);
        List[FromIndex + I].Value := Default(TValue);
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclHashMap<TKey, TValue>.Pack;
var
  I: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.PutAll(const AMap: IJclMap<TKey, TValue>);
var
  It: IJclIterator<TKey>;
  Key: TKey;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.PutValue(const Key: TKey; const Value: TValue);
var
  Index: Integer;
  Bucket: TJclBucket<TKey, TValue>;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, Default(TKey)) and not ValuesEqual(Value, Default(TValue))) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclBucket<TKey, TValue>.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
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

function TJclHashMap<TKey, TValue>.Remove(const Key: TKey): TValue;
var
  Bucket: TJclBucket<TKey, TValue>;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Default(TValue);
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.Size: Integer;
begin
  Result := FSize;
end;

function TJclHashMap<TKey, TValue>.Values: IJclCollection<TValue>;
var
  I, J: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyArrayList(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;



//=== { TJclHashMapE<TKey, TValue> } =========================================

constructor TJclHashMapE<TKey, TValue>.Create(const AKeyEqualityComparer: IEqualityComparer<TKey>;
  const AValueEqualityComparer: IEqualityComparer<TValue>; const AKeyComparer: IComparer<TKey>; ACapacity: Integer;
  AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(ACapacity, AOwnsKeys, AOwnsValues);
  FKeyEqualityComparer := AKeyEqualityComparer;
  FValueEqualityComparer := AValueEqualityComparer;
  FKeyComparer := AKeyComparer;
end;

procedure TJclHashMapE<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclHashMapE<TKey, TValue>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclHashMapE<TKey, TValue> then
  begin
    ADest := TJclHashMapE<TKey, TValue>(Dest);
    ADest.FKeyEqualityComparer := FKeyEqualityComparer;
    ADest.FValueEqualityComparer := FValueEqualityComparer;
    ADest.FKeyComparer := FKeyComparer;
  end;
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  Result := TJclArrayListE<TValue>.Create(ValueEqualityComparer, ACapacity, AOwnsObjects);
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TJclArraySetE<TKey>.Create(KeyComparer, ACapacity, AOwnsObjects);
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapE<TKey, TValue>.Create(KeyEqualityComparer, ValueEqualityComparer,
    KeyComparer, FCapacity, False, False);
  AssignPropertiesTo(Result);
end;

function TJclHashMapE<TKey, TValue>.Hash(const AKey: TKey): Integer;
begin
  if KeyEqualityComparer = nil then
    raise EJclNoHashConverterError.Create;
  Result := KeyEqualityComparer.GetHashCode(AKey);
end;

function TJclHashMapE<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  if KeyEqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := KeyEqualityComparer.Equals(A, B);
end;

function TJclHashMapE<TKey, TValue>.ValuesEqual(const A, B: TValue): Boolean;
begin
  if ValueEqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := ValueEqualityComparer.Equals(A, B);
end;

//=== { TJclHashMapF<TKey, TValue> } =========================================

constructor TJclHashMapF<TKey, TValue>.Create(AKeyEqualityCompare: TEqualityCompare<TKey>;
  AKeyHash: THashConvert<TKey>; AValueEqualityCompare: TEqualityCompare<TValue>; AKeyCompare: TCompare<TKey>;
  ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(ACapacity, AOwnsKeys, AOwnsValues);
  FKeyEqualityCompare := AKeyEqualityCompare;
  FKeyHash := AKeyHash;
  FValueEqualityCompare := AValueEqualityCompare;
  FKeyCompare := AKeyCompare;
end;

procedure TJclHashMapF<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclHashMapF<TKey, TValue>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclHashMapF<TKey, TValue> then
  begin
    ADest := TJclHashMapF<TKey, TValue>(Dest);
    ADest.FKeyEqualityCompare := FKeyEqualityCompare;
    ADest.FKeyHash := FKeyHash;
    ADest.FValueEqualityCompare := FValueEqualityCompare;
    ADest.FKeyCompare := FKeyCompare;
  end;
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  Result := TJclArrayListF<TValue>.Create(ValueEqualityCompare, ACapacity, AOwnsObjects);
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TJclArraySetF<TKey>.Create(KeyCompare, ACapacity, AOwnsObjects);
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapF<TKey, TValue>.Create(KeyEqualityCompare, KeyHash, ValueEqualityCompare, KeyCompare, FCapacity,
    False, False);
  AssignPropertiesTo(Result);
end;

function TJclHashMapF<TKey, TValue>.Hash(const AKey: TKey): Integer;
begin
  if not Assigned(KeyHash) then
    raise EJclNoHashConverterError.Create;
  Result := KeyHash(AKey);
end;

function TJclHashMapF<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  if not Assigned(KeyEqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := KeyEqualityCompare(A, B);
end;

function TJclHashMapF<TKey, TValue>.ValuesEqual(const A, B: TValue): Boolean;
begin
  if not Assigned(ValueEqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := ValueEqualityCompare(A, B);
end;

//=== { TJclHashMapI<TKey, TValue> } =========================================

function TJclHashMapI<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  Result := TJclArrayListI<TValue>.Create(ACapacity, AOwnsObjects);
end;

function TJclHashMapI<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TJclArraySetI<TKey>.Create(ACapacity, AOwnsObjects);
end;

function TJclHashMapI<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapI<TKey, TValue>.Create(FCapacity, False, False);
  AssignPropertiesTo(Result);
end;

function TJclHashMapI<TKey, TValue>.Hash(const AKey: TKey): Integer;
begin
  Result := AKey.GetHashCode;
end;

function TJclHashMapI<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  Result := A.Equals(B);
end;

function TJclHashMapI<TKey, TValue>.ValuesEqual(const A, B: TValue): Boolean;
begin
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

