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
{ The Original Code is JclSortedMaps.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet. Portions created by                }
{ Florent Ouchet are Copyright (C) Florent Ouchet <outchy att users dott sourceforge dott net      }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
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

unit JclSortedMaps;

interface

{$I jcl.inc}

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
  TJclIntfIntfSortedEntry = record
    Key: IInterface;
    Value: IInterface;
  end;

  TJclIntfIntfSortedEntryArray = array of TJclIntfIntfSortedEntry;

  TJclIntfIntfSortedMap = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntfIntfMap, IJclIntfIntfSortedMap)
  private
    FEntries: TJclIntfIntfSortedEntryArray;
    function BinarySearch(const Key: IInterface): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntfIntfSortedMap }
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfIntfSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfIntfSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfIntfSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A, B: IInterface): Integer;
    function ValuesCompare(const A, B: IInterface): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclAnsiStrIntfSortedEntry = record
    Key: AnsiString;
    Value: IInterface;
  end;

  TJclAnsiStrIntfSortedEntryArray = array of TJclAnsiStrIntfSortedEntry;

  TJclAnsiStrIntfSortedMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer,
    IJclAnsiStrIntfMap, IJclAnsiStrIntfSortedMap)
  private
    FEntries: TJclAnsiStrIntfSortedEntryArray;
    function BinarySearch(const Key: AnsiString): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclAnsiStrIntfSortedMap }
    function FirstKey: AnsiString;
    function HeadMap(const ToKey: AnsiString): IJclAnsiStrIntfSortedMap;
    function LastKey: AnsiString;
    function SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrIntfSortedMap;
    function TailMap(const FromKey: AnsiString): IJclAnsiStrIntfSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A, B: AnsiString): Integer;
    function ValuesCompare(const A, B: IInterface): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclIntfAnsiStrSortedEntry = record
    Key: IInterface;
    Value: AnsiString;
  end;

  TJclIntfAnsiStrSortedEntryArray = array of TJclIntfAnsiStrSortedEntry;

  TJclIntfAnsiStrSortedMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer,
    IJclIntfAnsiStrMap, IJclIntfAnsiStrSortedMap)
  private
    FEntries: TJclIntfAnsiStrSortedEntryArray;
    function BinarySearch(const Key: IInterface): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntfAnsiStrSortedMap }
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfAnsiStrSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfAnsiStrSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfAnsiStrSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: AnsiString): AnsiString;
    function KeysCompare(const A, B: IInterface): Integer;
    function ValuesCompare(const A, B: AnsiString): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclAnsiStrAnsiStrSortedEntry = record
    Key: AnsiString;
    Value: AnsiString;
  end;

  TJclAnsiStrAnsiStrSortedEntryArray = array of TJclAnsiStrAnsiStrSortedEntry;

  TJclAnsiStrAnsiStrSortedMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer,
    IJclAnsiStrAnsiStrMap, IJclAnsiStrAnsiStrSortedMap)
  private
    FEntries: TJclAnsiStrAnsiStrSortedEntryArray;
    function BinarySearch(const Key: AnsiString): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclAnsiStrAnsiStrSortedMap }
    function FirstKey: AnsiString;
    function HeadMap(const ToKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
    function LastKey: AnsiString;
    function SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
    function TailMap(const FromKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: AnsiString): AnsiString;
    function KeysCompare(const A, B: AnsiString): Integer;
    function ValuesCompare(const A, B: AnsiString): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclWideStrIntfSortedEntry = record
    Key: WideString;
    Value: IInterface;
  end;

  TJclWideStrIntfSortedEntryArray = array of TJclWideStrIntfSortedEntry;

  TJclWideStrIntfSortedMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer,
    IJclWideStrIntfMap, IJclWideStrIntfSortedMap)
  private
    FEntries: TJclWideStrIntfSortedEntryArray;
    function BinarySearch(const Key: WideString): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclWideStrIntfSortedMap }
    function FirstKey: WideString;
    function HeadMap(const ToKey: WideString): IJclWideStrIntfSortedMap;
    function LastKey: WideString;
    function SubMap(const FromKey, ToKey: WideString): IJclWideStrIntfSortedMap;
    function TailMap(const FromKey: WideString): IJclWideStrIntfSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A, B: WideString): Integer;
    function ValuesCompare(const A, B: IInterface): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclIntfWideStrSortedEntry = record
    Key: IInterface;
    Value: WideString;
  end;

  TJclIntfWideStrSortedEntryArray = array of TJclIntfWideStrSortedEntry;

  TJclIntfWideStrSortedMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer,
    IJclIntfWideStrMap, IJclIntfWideStrSortedMap)
  private
    FEntries: TJclIntfWideStrSortedEntryArray;
    function BinarySearch(const Key: IInterface): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntfWideStrSortedMap }
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfWideStrSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfWideStrSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfWideStrSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: WideString): WideString;
    function KeysCompare(const A, B: IInterface): Integer;
    function ValuesCompare(const A, B: WideString): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclWideStrWideStrSortedEntry = record
    Key: WideString;
    Value: WideString;
  end;

  TJclWideStrWideStrSortedEntryArray = array of TJclWideStrWideStrSortedEntry;

  TJclWideStrWideStrSortedMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer,
    IJclWideStrWideStrMap, IJclWideStrWideStrSortedMap)
  private
    FEntries: TJclWideStrWideStrSortedEntryArray;
    function BinarySearch(const Key: WideString): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclWideStrWideStrSortedMap }
    function FirstKey: WideString;
    function HeadMap(const ToKey: WideString): IJclWideStrWideStrSortedMap;
    function LastKey: WideString;
    function SubMap(const FromKey, ToKey: WideString): IJclWideStrWideStrSortedMap;
    function TailMap(const FromKey: WideString): IJclWideStrWideStrSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: WideString): WideString;
    function KeysCompare(const A, B: WideString): Integer;
    function ValuesCompare(const A, B: WideString): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrIntfSortedMap = TJclAnsiStrIntfSortedMap;
  TJclIntfStrSortedMap = TJclIntfAnsiStrSortedMap;
  TJclStrStrSortedMap = TJclAnsiStrAnsiStrSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrIntfSortedMap = TJclWideStrIntfSortedMap;
  TJclIntfStrSortedMap = TJclIntfWideStrSortedMap;
  TJclStrStrSortedMap = TJclWideStrWideStrSortedMap;
  {$ENDIF CONTAINER_WIDESTR}

  TJclSingleIntfSortedEntry = record
    Key: Single;
    Value: IInterface;
  end;

  TJclSingleIntfSortedEntryArray = array of TJclSingleIntfSortedEntry;

  TJclSingleIntfSortedMap = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclSingleContainer,
    IJclSingleIntfMap, IJclSingleIntfSortedMap)
  private
    FEntries: TJclSingleIntfSortedEntryArray;
    function BinarySearch(const Key: Single): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclSingleIntfSortedMap }
    function FirstKey: Single;
    function HeadMap(const ToKey: Single): IJclSingleIntfSortedMap;
    function LastKey: Single;
    function SubMap(const FromKey, ToKey: Single): IJclSingleIntfSortedMap;
    function TailMap(const FromKey: Single): IJclSingleIntfSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A, B: Single): Integer;
    function ValuesCompare(const A, B: IInterface): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclIntfSingleSortedEntry = record
    Key: IInterface;
    Value: Single;
  end;

  TJclIntfSingleSortedEntryArray = array of TJclIntfSingleSortedEntry;

  TJclIntfSingleSortedMap = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclSingleContainer,
    IJclIntfSingleMap, IJclIntfSingleSortedMap)
  private
    FEntries: TJclIntfSingleSortedEntryArray;
    function BinarySearch(const Key: IInterface): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntfSingleSortedMap }
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfSingleSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfSingleSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfSingleSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Single): Single;
    function KeysCompare(const A, B: IInterface): Integer;
    function ValuesCompare(const A, B: Single): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclSingleSingleSortedEntry = record
    Key: Single;
    Value: Single;
  end;

  TJclSingleSingleSortedEntryArray = array of TJclSingleSingleSortedEntry;

  TJclSingleSingleSortedMap = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclSingleContainer,
    IJclSingleSingleMap, IJclSingleSingleSortedMap)
  private
    FEntries: TJclSingleSingleSortedEntryArray;
    function BinarySearch(const Key: Single): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclSingleSingleSortedMap }
    function FirstKey: Single;
    function HeadMap(const ToKey: Single): IJclSingleSingleSortedMap;
    function LastKey: Single;
    function SubMap(const FromKey, ToKey: Single): IJclSingleSingleSortedMap;
    function TailMap(const FromKey: Single): IJclSingleSingleSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function FreeValue(var Value: Single): Single;
    function KeysCompare(const A, B: Single): Integer;
    function ValuesCompare(const A, B: Single): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclDoubleIntfSortedEntry = record
    Key: Double;
    Value: IInterface;
  end;

  TJclDoubleIntfSortedEntryArray = array of TJclDoubleIntfSortedEntry;

  TJclDoubleIntfSortedMap = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclDoubleContainer,
    IJclDoubleIntfMap, IJclDoubleIntfSortedMap)
  private
    FEntries: TJclDoubleIntfSortedEntryArray;
    function BinarySearch(const Key: Double): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclDoubleIntfSortedMap }
    function FirstKey: Double;
    function HeadMap(const ToKey: Double): IJclDoubleIntfSortedMap;
    function LastKey: Double;
    function SubMap(const FromKey, ToKey: Double): IJclDoubleIntfSortedMap;
    function TailMap(const FromKey: Double): IJclDoubleIntfSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A, B: Double): Integer;
    function ValuesCompare(const A, B: IInterface): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclIntfDoubleSortedEntry = record
    Key: IInterface;
    Value: Double;
  end;

  TJclIntfDoubleSortedEntryArray = array of TJclIntfDoubleSortedEntry;

  TJclIntfDoubleSortedMap = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclDoubleContainer,
    IJclIntfDoubleMap, IJclIntfDoubleSortedMap)
  private
    FEntries: TJclIntfDoubleSortedEntryArray;
    function BinarySearch(const Key: IInterface): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntfDoubleSortedMap }
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfDoubleSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfDoubleSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfDoubleSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Double): Double;
    function KeysCompare(const A, B: IInterface): Integer;
    function ValuesCompare(const A, B: Double): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclDoubleDoubleSortedEntry = record
    Key: Double;
    Value: Double;
  end;

  TJclDoubleDoubleSortedEntryArray = array of TJclDoubleDoubleSortedEntry;

  TJclDoubleDoubleSortedMap = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclDoubleContainer,
    IJclDoubleDoubleMap, IJclDoubleDoubleSortedMap)
  private
    FEntries: TJclDoubleDoubleSortedEntryArray;
    function BinarySearch(const Key: Double): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclDoubleDoubleSortedMap }
    function FirstKey: Double;
    function HeadMap(const ToKey: Double): IJclDoubleDoubleSortedMap;
    function LastKey: Double;
    function SubMap(const FromKey, ToKey: Double): IJclDoubleDoubleSortedMap;
    function TailMap(const FromKey: Double): IJclDoubleDoubleSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function FreeValue(var Value: Double): Double;
    function KeysCompare(const A, B: Double): Integer;
    function ValuesCompare(const A, B: Double): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclExtendedIntfSortedEntry = record
    Key: Extended;
    Value: IInterface;
  end;

  TJclExtendedIntfSortedEntryArray = array of TJclExtendedIntfSortedEntry;

  TJclExtendedIntfSortedMap = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclExtendedContainer,
    IJclExtendedIntfMap, IJclExtendedIntfSortedMap)
  private
    FEntries: TJclExtendedIntfSortedEntryArray;
    function BinarySearch(const Key: Extended): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclExtendedIntfSortedMap }
    function FirstKey: Extended;
    function HeadMap(const ToKey: Extended): IJclExtendedIntfSortedMap;
    function LastKey: Extended;
    function SubMap(const FromKey, ToKey: Extended): IJclExtendedIntfSortedMap;
    function TailMap(const FromKey: Extended): IJclExtendedIntfSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A, B: Extended): Integer;
    function ValuesCompare(const A, B: IInterface): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclIntfExtendedSortedEntry = record
    Key: IInterface;
    Value: Extended;
  end;

  TJclIntfExtendedSortedEntryArray = array of TJclIntfExtendedSortedEntry;

  TJclIntfExtendedSortedMap = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclExtendedContainer,
    IJclIntfExtendedMap, IJclIntfExtendedSortedMap)
  private
    FEntries: TJclIntfExtendedSortedEntryArray;
    function BinarySearch(const Key: IInterface): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntfExtendedSortedMap }
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfExtendedSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfExtendedSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfExtendedSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Extended): Extended;
    function KeysCompare(const A, B: IInterface): Integer;
    function ValuesCompare(const A, B: Extended): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclExtendedExtendedSortedEntry = record
    Key: Extended;
    Value: Extended;
  end;

  TJclExtendedExtendedSortedEntryArray = array of TJclExtendedExtendedSortedEntry;

  TJclExtendedExtendedSortedMap = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclExtendedContainer,
    IJclExtendedExtendedMap, IJclExtendedExtendedSortedMap)
  private
    FEntries: TJclExtendedExtendedSortedEntryArray;
    function BinarySearch(const Key: Extended): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclExtendedExtendedSortedMap }
    function FirstKey: Extended;
    function HeadMap(const ToKey: Extended): IJclExtendedExtendedSortedMap;
    function LastKey: Extended;
    function SubMap(const FromKey, ToKey: Extended): IJclExtendedExtendedSortedMap;
    function TailMap(const FromKey: Extended): IJclExtendedExtendedSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function FreeValue(var Value: Extended): Extended;
    function KeysCompare(const A, B: Extended): Integer;
    function ValuesCompare(const A, B: Extended): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatIntfSortedMap = TJclExtendedIntfSortedMap;
  TJclIntfFloatSortedMap = TJclIntfExtendedSortedMap;
  TJclFloatFloatSortedMap = TJclExtendedExtendedSortedMap;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatIntfSortedMap = TJclDoubleIntfSortedMap;
  TJclIntfFloatSortedMap = TJclIntfDoubleSortedMap;
  TJclFloatFloatSortedMap = TJclDoubleDoubleSortedMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatIntfSortedMap = TJclSingleIntfSortedMap;
  TJclIntfFloatSortedMap = TJclIntfSingleSortedMap;
  TJclFloatFloatSortedMap = TJclSingleSingleSortedMap;
  {$ENDIF MATH_SINGLE_PRECISION}

  TJclIntegerIntfSortedEntry = record
    Key: Integer;
    Value: IInterface;
  end;

  TJclIntegerIntfSortedEntryArray = array of TJclIntegerIntfSortedEntry;

  TJclIntegerIntfSortedMap = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntegerIntfMap, IJclIntegerIntfSortedMap)
  private
    FEntries: TJclIntegerIntfSortedEntryArray;
    function BinarySearch(Key: Integer): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntegerIntfSortedMap }
    function FirstKey: Integer;
    function HeadMap(ToKey: Integer): IJclIntegerIntfSortedMap;
    function LastKey: Integer;
    function SubMap(FromKey, ToKey: Integer): IJclIntegerIntfSortedMap;
    function TailMap(FromKey: Integer): IJclIntegerIntfSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(A, B: Integer): Integer;
    function ValuesCompare(const A, B: IInterface): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclIntfIntegerSortedEntry = record
    Key: IInterface;
    Value: Integer;
  end;

  TJclIntfIntegerSortedEntryArray = array of TJclIntfIntegerSortedEntry;

  TJclIntfIntegerSortedMap = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntfIntegerMap, IJclIntfIntegerSortedMap)
  private
    FEntries: TJclIntfIntegerSortedEntryArray;
    function BinarySearch(const Key: IInterface): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntfIntegerSortedMap }
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfIntegerSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfIntegerSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfIntegerSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Integer): Integer;
    function KeysCompare(const A, B: IInterface): Integer;
    function ValuesCompare(A, B: Integer): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclIntegerIntegerSortedEntry = record
    Key: Integer;
    Value: Integer;
  end;

  TJclIntegerIntegerSortedEntryArray = array of TJclIntegerIntegerSortedEntry;

  TJclIntegerIntegerSortedMap = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntegerIntegerMap, IJclIntegerIntegerSortedMap)
  private
    FEntries: TJclIntegerIntegerSortedEntryArray;
    function BinarySearch(Key: Integer): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntegerIntegerSortedMap }
    function FirstKey: Integer;
    function HeadMap(ToKey: Integer): IJclIntegerIntegerSortedMap;
    function LastKey: Integer;
    function SubMap(FromKey, ToKey: Integer): IJclIntegerIntegerSortedMap;
    function TailMap(FromKey: Integer): IJclIntegerIntegerSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function FreeValue(var Value: Integer): Integer;
    function KeysCompare(A, B: Integer): Integer;
    function ValuesCompare(A, B: Integer): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclCardinalIntfSortedEntry = record
    Key: Cardinal;
    Value: IInterface;
  end;

  TJclCardinalIntfSortedEntryArray = array of TJclCardinalIntfSortedEntry;

  TJclCardinalIntfSortedMap = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclCardinalIntfMap, IJclCardinalIntfSortedMap)
  private
    FEntries: TJclCardinalIntfSortedEntryArray;
    function BinarySearch(Key: Cardinal): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclCardinalIntfSortedMap }
    function FirstKey: Cardinal;
    function HeadMap(ToKey: Cardinal): IJclCardinalIntfSortedMap;
    function LastKey: Cardinal;
    function SubMap(FromKey, ToKey: Cardinal): IJclCardinalIntfSortedMap;
    function TailMap(FromKey: Cardinal): IJclCardinalIntfSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(A, B: Cardinal): Integer;
    function ValuesCompare(const A, B: IInterface): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclIntfCardinalSortedEntry = record
    Key: IInterface;
    Value: Cardinal;
  end;

  TJclIntfCardinalSortedEntryArray = array of TJclIntfCardinalSortedEntry;

  TJclIntfCardinalSortedMap = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntfCardinalMap, IJclIntfCardinalSortedMap)
  private
    FEntries: TJclIntfCardinalSortedEntryArray;
    function BinarySearch(const Key: IInterface): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntfCardinalSortedMap }
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfCardinalSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfCardinalSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfCardinalSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Cardinal): Cardinal;
    function KeysCompare(const A, B: IInterface): Integer;
    function ValuesCompare(A, B: Cardinal): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclCardinalCardinalSortedEntry = record
    Key: Cardinal;
    Value: Cardinal;
  end;

  TJclCardinalCardinalSortedEntryArray = array of TJclCardinalCardinalSortedEntry;

  TJclCardinalCardinalSortedMap = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclCardinalCardinalMap, IJclCardinalCardinalSortedMap)
  private
    FEntries: TJclCardinalCardinalSortedEntryArray;
    function BinarySearch(Key: Cardinal): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclCardinalCardinalSortedMap }
    function FirstKey: Cardinal;
    function HeadMap(ToKey: Cardinal): IJclCardinalCardinalSortedMap;
    function LastKey: Cardinal;
    function SubMap(FromKey, ToKey: Cardinal): IJclCardinalCardinalSortedMap;
    function TailMap(FromKey: Cardinal): IJclCardinalCardinalSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function FreeValue(var Value: Cardinal): Cardinal;
    function KeysCompare(A, B: Cardinal): Integer;
    function ValuesCompare(A, B: Cardinal): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclInt64IntfSortedEntry = record
    Key: Int64;
    Value: IInterface;
  end;

  TJclInt64IntfSortedEntryArray = array of TJclInt64IntfSortedEntry;

  TJclInt64IntfSortedMap = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclInt64IntfMap, IJclInt64IntfSortedMap)
  private
    FEntries: TJclInt64IntfSortedEntryArray;
    function BinarySearch(const Key: Int64): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclInt64IntfSortedMap }
    function FirstKey: Int64;
    function HeadMap(const ToKey: Int64): IJclInt64IntfSortedMap;
    function LastKey: Int64;
    function SubMap(const FromKey, ToKey: Int64): IJclInt64IntfSortedMap;
    function TailMap(const FromKey: Int64): IJclInt64IntfSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A, B: Int64): Integer;
    function ValuesCompare(const A, B: IInterface): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclIntfInt64SortedEntry = record
    Key: IInterface;
    Value: Int64;
  end;

  TJclIntfInt64SortedEntryArray = array of TJclIntfInt64SortedEntry;

  TJclIntfInt64SortedMap = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntfInt64Map, IJclIntfInt64SortedMap)
  private
    FEntries: TJclIntfInt64SortedEntryArray;
    function BinarySearch(const Key: IInterface): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntfInt64SortedMap }
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfInt64SortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfInt64SortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfInt64SortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Int64): Int64;
    function KeysCompare(const A, B: IInterface): Integer;
    function ValuesCompare(const A, B: Int64): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclInt64Int64SortedEntry = record
    Key: Int64;
    Value: Int64;
  end;

  TJclInt64Int64SortedEntryArray = array of TJclInt64Int64SortedEntry;

  TJclInt64Int64SortedMap = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclInt64Int64Map, IJclInt64Int64SortedMap)
  private
    FEntries: TJclInt64Int64SortedEntryArray;
    function BinarySearch(const Key: Int64): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclInt64Int64SortedMap }
    function FirstKey: Int64;
    function HeadMap(const ToKey: Int64): IJclInt64Int64SortedMap;
    function LastKey: Int64;
    function SubMap(const FromKey, ToKey: Int64): IJclInt64Int64SortedMap;
    function TailMap(const FromKey: Int64): IJclInt64Int64SortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function FreeValue(var Value: Int64): Int64;
    function KeysCompare(const A, B: Int64): Integer;
    function ValuesCompare(const A, B: Int64): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  {$IFNDEF CLR}
  TJclPtrIntfSortedEntry = record
    Key: Pointer;
    Value: IInterface;
  end;

  TJclPtrIntfSortedEntryArray = array of TJclPtrIntfSortedEntry;

  TJclPtrIntfSortedMap = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclPtrIntfMap, IJclPtrIntfSortedMap)
  private
    FEntries: TJclPtrIntfSortedEntryArray;
    function BinarySearch(Key: Pointer): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclPtrIntfSortedMap }
    function FirstKey: Pointer;
    function HeadMap(ToKey: Pointer): IJclPtrIntfSortedMap;
    function LastKey: Pointer;
    function SubMap(FromKey, ToKey: Pointer): IJclPtrIntfSortedMap;
    function TailMap(FromKey: Pointer): IJclPtrIntfSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(A, B: Pointer): Integer;
    function ValuesCompare(const A, B: IInterface): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclIntfPtrSortedEntry = record
    Key: IInterface;
    Value: Pointer;
  end;

  TJclIntfPtrSortedEntryArray = array of TJclIntfPtrSortedEntry;

  TJclIntfPtrSortedMap = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntfPtrMap, IJclIntfPtrSortedMap)
  private
    FEntries: TJclIntfPtrSortedEntryArray;
    function BinarySearch(const Key: IInterface): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntfPtrSortedMap }
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfPtrSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfPtrSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfPtrSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Pointer): Pointer;
    function KeysCompare(const A, B: IInterface): Integer;
    function ValuesCompare(A, B: Pointer): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  TJclPtrPtrSortedEntry = record
    Key: Pointer;
    Value: Pointer;
  end;

  TJclPtrPtrSortedEntryArray = array of TJclPtrPtrSortedEntry;

  TJclPtrPtrSortedMap = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclPtrPtrMap, IJclPtrPtrSortedMap)
  private
    FEntries: TJclPtrPtrSortedEntryArray;
    function BinarySearch(Key: Pointer): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclPtrPtrSortedMap }
    function FirstKey: Pointer;
    function HeadMap(ToKey: Pointer): IJclPtrPtrSortedMap;
    function LastKey: Pointer;
    function SubMap(FromKey, ToKey: Pointer): IJclPtrPtrSortedMap;
    function TailMap(FromKey: Pointer): IJclPtrPtrSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function FreeValue(var Value: Pointer): Pointer;
    function KeysCompare(A, B: Pointer): Integer;
    function ValuesCompare(A, B: Pointer): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;
  {$ENDIF ~CLR}

  TJclIntfSortedEntry = record
    Key: IInterface;
    Value: TObject;
  end;

  TJclIntfSortedEntryArray = array of TJclIntfSortedEntry;

  TJclIntfSortedMap = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclValueOwner,
    IJclIntfMap, IJclIntfSortedMap)
  private
    FEntries: TJclIntfSortedEntryArray;
    FOwnsValues: Boolean;
    function BinarySearch(const Key: IInterface): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntfSortedMap }
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function KeysCompare(const A, B: IInterface): Integer;
    function ValuesCompare(A, B: TObject): Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  TJclAnsiStrSortedEntry = record
    Key: AnsiString;
    Value: TObject;
  end;

  TJclAnsiStrSortedEntryArray = array of TJclAnsiStrSortedEntry;

  TJclAnsiStrSortedMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer, IJclValueOwner,
    IJclAnsiStrMap, IJclAnsiStrSortedMap)
  private
    FEntries: TJclAnsiStrSortedEntryArray;
    FOwnsValues: Boolean;
    function BinarySearch(const Key: AnsiString): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclAnsiStrSortedMap }
    function FirstKey: AnsiString;
    function HeadMap(const ToKey: AnsiString): IJclAnsiStrSortedMap;
    function LastKey: AnsiString;
    function SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrSortedMap;
    function TailMap(const FromKey: AnsiString): IJclAnsiStrSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function KeysCompare(const A, B: AnsiString): Integer;
    function ValuesCompare(A, B: TObject): Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  TJclWideStrSortedEntry = record
    Key: WideString;
    Value: TObject;
  end;

  TJclWideStrSortedEntryArray = array of TJclWideStrSortedEntry;

  TJclWideStrSortedMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer, IJclValueOwner,
    IJclWideStrMap, IJclWideStrSortedMap)
  private
    FEntries: TJclWideStrSortedEntryArray;
    FOwnsValues: Boolean;
    function BinarySearch(const Key: WideString): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclWideStrSortedMap }
    function FirstKey: WideString;
    function HeadMap(const ToKey: WideString): IJclWideStrSortedMap;
    function LastKey: WideString;
    function SubMap(const FromKey, ToKey: WideString): IJclWideStrSortedMap;
    function TailMap(const FromKey: WideString): IJclWideStrSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function KeysCompare(const A, B: WideString): Integer;
    function ValuesCompare(A, B: TObject): Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrSortedMap = TJclAnsiStrSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrSortedMap = TJclWideStrSortedMap;
  {$ENDIF CONTAINER_WIDESTR}

  TJclSingleSortedEntry = record
    Key: Single;
    Value: TObject;
  end;

  TJclSingleSortedEntryArray = array of TJclSingleSortedEntry;

  TJclSingleSortedMap = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclSingleContainer, IJclValueOwner,
    IJclSingleMap, IJclSingleSortedMap)
  private
    FEntries: TJclSingleSortedEntryArray;
    FOwnsValues: Boolean;
    function BinarySearch(const Key: Single): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclSingleSortedMap }
    function FirstKey: Single;
    function HeadMap(const ToKey: Single): IJclSingleSortedMap;
    function LastKey: Single;
    function SubMap(const FromKey, ToKey: Single): IJclSingleSortedMap;
    function TailMap(const FromKey: Single): IJclSingleSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function KeysCompare(const A, B: Single): Integer;
    function ValuesCompare(A, B: TObject): Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  TJclDoubleSortedEntry = record
    Key: Double;
    Value: TObject;
  end;

  TJclDoubleSortedEntryArray = array of TJclDoubleSortedEntry;

  TJclDoubleSortedMap = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclDoubleContainer, IJclValueOwner,
    IJclDoubleMap, IJclDoubleSortedMap)
  private
    FEntries: TJclDoubleSortedEntryArray;
    FOwnsValues: Boolean;
    function BinarySearch(const Key: Double): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclDoubleSortedMap }
    function FirstKey: Double;
    function HeadMap(const ToKey: Double): IJclDoubleSortedMap;
    function LastKey: Double;
    function SubMap(const FromKey, ToKey: Double): IJclDoubleSortedMap;
    function TailMap(const FromKey: Double): IJclDoubleSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function KeysCompare(const A, B: Double): Integer;
    function ValuesCompare(A, B: TObject): Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  TJclExtendedSortedEntry = record
    Key: Extended;
    Value: TObject;
  end;

  TJclExtendedSortedEntryArray = array of TJclExtendedSortedEntry;

  TJclExtendedSortedMap = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclExtendedContainer, IJclValueOwner,
    IJclExtendedMap, IJclExtendedSortedMap)
  private
    FEntries: TJclExtendedSortedEntryArray;
    FOwnsValues: Boolean;
    function BinarySearch(const Key: Extended): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclExtendedSortedMap }
    function FirstKey: Extended;
    function HeadMap(const ToKey: Extended): IJclExtendedSortedMap;
    function LastKey: Extended;
    function SubMap(const FromKey, ToKey: Extended): IJclExtendedSortedMap;
    function TailMap(const FromKey: Extended): IJclExtendedSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function KeysCompare(const A, B: Extended): Integer;
    function ValuesCompare(A, B: TObject): Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatSortedMap = TJclExtendedSortedMap;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatSortedMap = TJclDoubleSortedMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatSortedMap = TJclSingleSortedMap;
  {$ENDIF MATH_SINGLE_PRECISION}

  TJclIntegerSortedEntry = record
    Key: Integer;
    Value: TObject;
  end;

  TJclIntegerSortedEntryArray = array of TJclIntegerSortedEntry;

  TJclIntegerSortedMap = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclValueOwner,
    IJclIntegerMap, IJclIntegerSortedMap)
  private
    FEntries: TJclIntegerSortedEntryArray;
    FOwnsValues: Boolean;
    function BinarySearch(Key: Integer): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntegerSortedMap }
    function FirstKey: Integer;
    function HeadMap(ToKey: Integer): IJclIntegerSortedMap;
    function LastKey: Integer;
    function SubMap(FromKey, ToKey: Integer): IJclIntegerSortedMap;
    function TailMap(FromKey: Integer): IJclIntegerSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function KeysCompare(A, B: Integer): Integer;
    function ValuesCompare(A, B: TObject): Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  TJclCardinalSortedEntry = record
    Key: Cardinal;
    Value: TObject;
  end;

  TJclCardinalSortedEntryArray = array of TJclCardinalSortedEntry;

  TJclCardinalSortedMap = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclValueOwner,
    IJclCardinalMap, IJclCardinalSortedMap)
  private
    FEntries: TJclCardinalSortedEntryArray;
    FOwnsValues: Boolean;
    function BinarySearch(Key: Cardinal): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclCardinalSortedMap }
    function FirstKey: Cardinal;
    function HeadMap(ToKey: Cardinal): IJclCardinalSortedMap;
    function LastKey: Cardinal;
    function SubMap(FromKey, ToKey: Cardinal): IJclCardinalSortedMap;
    function TailMap(FromKey: Cardinal): IJclCardinalSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function KeysCompare(A, B: Cardinal): Integer;
    function ValuesCompare(A, B: TObject): Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  TJclInt64SortedEntry = record
    Key: Int64;
    Value: TObject;
  end;

  TJclInt64SortedEntryArray = array of TJclInt64SortedEntry;

  TJclInt64SortedMap = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclValueOwner,
    IJclInt64Map, IJclInt64SortedMap)
  private
    FEntries: TJclInt64SortedEntryArray;
    FOwnsValues: Boolean;
    function BinarySearch(const Key: Int64): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclInt64SortedMap }
    function FirstKey: Int64;
    function HeadMap(const ToKey: Int64): IJclInt64SortedMap;
    function LastKey: Int64;
    function SubMap(const FromKey, ToKey: Int64): IJclInt64SortedMap;
    function TailMap(const FromKey: Int64): IJclInt64SortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function KeysCompare(const A, B: Int64): Integer;
    function ValuesCompare(A, B: TObject): Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  {$IFNDEF CLR}
  TJclPtrSortedEntry = record
    Key: Pointer;
    Value: TObject;
  end;

  TJclPtrSortedEntryArray = array of TJclPtrSortedEntry;

  TJclPtrSortedMap = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclValueOwner,
    IJclPtrMap, IJclPtrSortedMap)
  private
    FEntries: TJclPtrSortedEntryArray;
    FOwnsValues: Boolean;
    function BinarySearch(Key: Pointer): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclPtrSortedMap }
    function FirstKey: Pointer;
    function HeadMap(ToKey: Pointer): IJclPtrSortedMap;
    function LastKey: Pointer;
    function SubMap(FromKey, ToKey: Pointer): IJclPtrSortedMap;
    function TailMap(FromKey: Pointer): IJclPtrSortedMap;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function KeysCompare(A, B: Pointer): Integer;
    function ValuesCompare(A, B: TObject): Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property OwnsValues: Boolean read FOwnsValues;
  end;
  {$ENDIF ~CLR}

  TJclSortedEntry = record
    Key: TObject;
    Value: TObject;
  end;

  TJclSortedEntryArray = array of TJclSortedEntry;

  TJclSortedMap = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclKeyOwner, IJclValueOwner,
    IJclMap, IJclSortedMap)
  private
    FEntries: TJclSortedEntryArray;
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
    function BinarySearch(Key: TObject): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclSortedMap }
    function FirstKey: TObject;
    function HeadMap(ToKey: TObject): IJclSortedMap;
    function LastKey: TObject;
    function SubMap(FromKey, ToKey: TObject): IJclSortedMap;
    function TailMap(FromKey: TObject): IJclSortedMap;
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
    function KeysCompare(A, B: TObject): Integer;
    function ValuesCompare(A, B: TObject): Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
    destructor Destroy; override;
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  TJclSortedEntry<TKey,TValue> = record
    Key: TKey;
    Value: TValue;
  end;

  TJclSortedEntryArray<TKey,TValue> = array of TJclSortedEntry<TKey,TValue>;

  TJclSortedMap<TKey,TValue> = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclPairOwner<TKey,TValue>,
    IJclMap<TKey,TValue>, IJclSortedMap<TKey,TValue>)
  private
    FEntries: TJclSortedEntryArray<TKey,TValue>;
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
    function BinarySearch(const Key: TKey): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclSortedMap<TKey,TValue> }
    function FirstKey: TKey;
    function HeadMap(const ToKey: TKey): IJclSortedMap<TKey,TValue>;
    function LastKey: TKey;
    function SubMap(const FromKey, ToKey: TKey): IJclSortedMap<TKey,TValue>;
    function TailMap(const FromKey: TKey): IJclSortedMap<TKey,TValue>;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclPairOwner }
    function FreeKey(var Key: TKey): TKey;
    function FreeValue(var Value: TValue): TValue;
    function GetOwnsKeys: Boolean;
    function GetOwnsValues: Boolean;
    function KeysCompare(const A, B: TKey): Integer; virtual; abstract;
    function ValuesCompare(const A, B: TValue): Integer; virtual; abstract;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; virtual; abstract;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; virtual; abstract;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
    destructor Destroy; override;
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  // E = external helper to compare items
  TJclSortedMapE<TKey, TValue> = class(TJclSortedMap<TKey,TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclSortedMap<TKey,TValue>, IJclPairOwner<TKey,TValue>)
  private
    FKeyComparer: IComparer<TKey>;
    FValueComparer: IComparer<TValue>;
    FValueEqualityComparer: IEqualityComparer<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function KeysCompare(const A, B: TKey): Integer; override;
    function ValuesCompare(const A, B: TValue): Integer; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AKeyComparer: IComparer<TKey>; const AValueComparer: IComparer<TValue>;
      const AValueEqualityComparer: IEqualityComparer<TValue>; ACapacity: Integer; AOwnsValues: Boolean;
      AOwnsKeys: Boolean);

    property KeyComparer: IComparer<TKey> read FKeyComparer write FKeyComparer;
    property ValueComparer: IComparer<TValue> read FValueComparer write FValueComparer;
    property ValueEqualityComparer: IEqualityComparer<TValue> read FValueEqualityComparer write FValueEqualityComparer;
  end;

  // F = Functions to compare items
  TJclSortedMapF<TKey, TValue> = class(TJclSortedMap<TKey, TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclSortedMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  private
    FKeyCompare: TCompare<TKey>;
    FValueCompare: TCompare<TValue>;
    FValueEqualityCompare: TEqualityCompare<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function KeysCompare(const A, B: TKey): Integer; override;
    function ValuesCompare(const A, B: TValue): Integer; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(AKeyCompare: TCompare<TKey>; AValueCompare: TCompare<TValue>;
      AValueEqualityCompare: TEqualityCompare<TValue>; ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyCompare: TCompare<TKey> read FKeyCompare write FKeyCompare;
    property ValueCompare: TCompare<TValue> read FValueCompare write FValueCompare;
    property ValueEqualityCompare: TEqualityCompare<TValue> read FValueEqualityCompare write FValueEqualityCompare;
  end;

  // I = items can compare themselves to an other
  TJclSortedMapI<TKey: IComparable<TKey>; TValue: IComparable<TValue>, IEquatable<TValue>> = class(TJclSortedMap<TKey, TValue>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer,
    IJclMap<TKey,TValue>, IJclSortedMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  protected
    function KeysCompare(const A, B: TKey): Integer; override;
    function ValuesCompare(const A, B: TValue): Integer; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;
  {$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclSortedMaps.pas $';
    Revision: '$Revision: 2393 $';
    Date: '$Date: 2008-07-20 11:13:23 +0200 (dim., 20 juil. 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils,
  JclArrayLists,
  JclArraySets;

//=== { TJclIntfIntfSortedMap } ==============================================

constructor TJclIntfIntfSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclIntfIntfSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfIntfSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntfIntfSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfIntfSortedMap then
  begin
    MyDest := TJclIntfIntfSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntfIntfSortedMap.BinarySearch(const Key: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.ContainsKey(const Key: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.ContainsValue(const Value: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntfIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfIntfSortedMap.Equals(const AMap: IJclIntfIntfMap): Boolean;
var
  It: IJclIntfIterator;
  Index: Integer;
  AKey: IInterface;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.FirstKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclIntfIntfSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclIntfIntfSortedMap.GetValue(const Key: IInterface): IInterface;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.HeadMap(const ToKey: IInterface): IJclIntfIntfSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntfIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfIntfSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.KeyOfValue(const Value: IInterface): IInterface;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntfIntfSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclIntfIntfSortedMap.KeySet: IJclIntfSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.LastKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclIntfIntfSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfIntfSortedMap.PutAll(const AMap: IJclIntfIntfMap);
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

procedure TJclIntfIntfSortedMap.PutValue(const Key: IInterface; const Value: IInterface);
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
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclIntfIntfSortedMap.Remove(const Key: IInterface): IInterface;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntfIntfSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfIntfSortedMap.SubMap(const FromKey, ToKey: IInterface): IJclIntfIntfSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntfIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.TailMap(const FromKey: IInterface): IJclIntfIntfSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclIntfIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.Values: IJclIntfCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  Result := ItemsCompare(A, B);
end;


//=== { TJclAnsiStrIntfSortedMap } ==============================================

constructor TJclAnsiStrIntfSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclAnsiStrIntfSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrIntfSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclAnsiStrIntfSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrIntfSortedMap then
  begin
    MyDest := TJclAnsiStrIntfSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclAnsiStrIntfSortedMap.BinarySearch(const Key: AnsiString): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrIntfSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.ContainsKey(const Key: AnsiString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.ContainsValue(const Value: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclAnsiStrIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrIntfSortedMap.Equals(const AMap: IJclAnsiStrIntfMap): Boolean;
var
  It: IJclAnsiStrIterator;
  Index: Integer;
  AKey: AnsiString;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.FirstKey: AnsiString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclAnsiStrIntfSortedMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclAnsiStrIntfSortedMap.GetValue(const Key: AnsiString): IInterface;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.HeadMap(const ToKey: AnsiString): IJclAnsiStrIntfSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclAnsiStrIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrIntfSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.KeyOfValue(const Value: IInterface): AnsiString;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := '';
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclAnsiStrIntfSortedMap.KeysCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclAnsiStrIntfSortedMap.KeySet: IJclAnsiStrSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.LastKey: AnsiString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclAnsiStrIntfSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclAnsiStrIntfSortedMap.PutAll(const AMap: IJclAnsiStrIntfMap);
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

procedure TJclAnsiStrIntfSortedMap.PutValue(const Key: AnsiString; const Value: IInterface);
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
    if FAllowDefaultElements or ((KeysCompare(Key, '') <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclAnsiStrIntfSortedMap.Remove(const Key: AnsiString): IInterface;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrIntfSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclAnsiStrIntfSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrIntfSortedMap.SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrIntfSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclAnsiStrIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.TailMap(const FromKey: AnsiString): IJclAnsiStrIntfSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclAnsiStrIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.Values: IJclIntfCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


//=== { TJclIntfAnsiStrSortedMap } ==============================================

constructor TJclIntfAnsiStrSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclIntfAnsiStrSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfAnsiStrSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntfAnsiStrSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfAnsiStrSortedMap then
  begin
    MyDest := TJclIntfAnsiStrSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntfAnsiStrSortedMap.BinarySearch(const Key: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfAnsiStrSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.ContainsKey(const Key: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.ContainsValue(const Value: AnsiString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntfAnsiStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfAnsiStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfAnsiStrSortedMap.Equals(const AMap: IJclIntfAnsiStrMap): Boolean;
var
  It: IJclIntfIterator;
  Index: Integer;
  AKey: IInterface;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.FirstKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclIntfAnsiStrSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfAnsiStrSortedMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;

function TJclIntfAnsiStrSortedMap.GetValue(const Key: IInterface): AnsiString;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := '';
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.HeadMap(const ToKey: IInterface): IJclIntfAnsiStrSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntfAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfAnsiStrSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.KeyOfValue(const Value: AnsiString): IInterface;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntfAnsiStrSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfAnsiStrSortedMap.KeySet: IJclIntfSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.LastKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclIntfAnsiStrSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfAnsiStrSortedMap.PutAll(const AMap: IJclIntfAnsiStrMap);
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

procedure TJclIntfAnsiStrSortedMap.PutValue(const Key: IInterface; const Value: AnsiString);
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
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, '') <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclIntfAnsiStrSortedMap.Remove(const Key: IInterface): AnsiString;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfAnsiStrSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntfAnsiStrSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfAnsiStrSortedMap.SubMap(const FromKey, ToKey: IInterface): IJclIntfAnsiStrSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntfAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfAnsiStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.TailMap(const FromKey: IInterface): IJclIntfAnsiStrSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclIntfAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfAnsiStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.Values: IJclAnsiStrCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.ValuesCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;


//=== { TJclAnsiStrAnsiStrSortedMap } ==============================================

constructor TJclAnsiStrAnsiStrSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclAnsiStrAnsiStrSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrAnsiStrSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclAnsiStrAnsiStrSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrAnsiStrSortedMap then
  begin
    MyDest := TJclAnsiStrAnsiStrSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclAnsiStrAnsiStrSortedMap.BinarySearch(const Key: AnsiString): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrAnsiStrSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.ContainsKey(const Key: AnsiString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.ContainsValue(const Value: AnsiString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclAnsiStrAnsiStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrAnsiStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrAnsiStrSortedMap.Equals(const AMap: IJclAnsiStrAnsiStrMap): Boolean;
var
  It: IJclAnsiStrIterator;
  Index: Integer;
  AKey: AnsiString;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.FirstKey: AnsiString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclAnsiStrAnsiStrSortedMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrAnsiStrSortedMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;

function TJclAnsiStrAnsiStrSortedMap.GetValue(const Key: AnsiString): AnsiString;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := '';
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.HeadMap(const ToKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclAnsiStrAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrAnsiStrSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.KeyOfValue(const Value: AnsiString): AnsiString;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := '';
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclAnsiStrAnsiStrSortedMap.KeysCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclAnsiStrAnsiStrSortedMap.KeySet: IJclAnsiStrSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.LastKey: AnsiString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclAnsiStrAnsiStrSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclAnsiStrAnsiStrSortedMap.PutAll(const AMap: IJclAnsiStrAnsiStrMap);
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

procedure TJclAnsiStrAnsiStrSortedMap.PutValue(const Key: AnsiString; const Value: AnsiString);
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
    if FAllowDefaultElements or ((KeysCompare(Key, '') <> 0) and (ValuesCompare(Value, '') <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclAnsiStrAnsiStrSortedMap.Remove(const Key: AnsiString): AnsiString;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrAnsiStrSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclAnsiStrAnsiStrSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrAnsiStrSortedMap.SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclAnsiStrAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrAnsiStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.TailMap(const FromKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclAnsiStrAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrAnsiStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.Values: IJclAnsiStrCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.ValuesCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;


//=== { TJclWideStrIntfSortedMap } ==============================================

constructor TJclWideStrIntfSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclWideStrIntfSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrIntfSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclWideStrIntfSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrIntfSortedMap then
  begin
    MyDest := TJclWideStrIntfSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclWideStrIntfSortedMap.BinarySearch(const Key: WideString): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrIntfSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.ContainsKey(const Key: WideString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.ContainsValue(const Value: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclWideStrIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclWideStrIntfSortedMap.Equals(const AMap: IJclWideStrIntfMap): Boolean;
var
  It: IJclWideStrIterator;
  Index: Integer;
  AKey: WideString;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.FirstKey: WideString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclWideStrIntfSortedMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclWideStrIntfSortedMap.GetValue(const Key: WideString): IInterface;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.HeadMap(const ToKey: WideString): IJclWideStrIntfSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclWideStrIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrIntfSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.KeyOfValue(const Value: IInterface): WideString;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := '';
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclWideStrIntfSortedMap.KeysCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclWideStrIntfSortedMap.KeySet: IJclWideStrSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.LastKey: WideString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclWideStrIntfSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclWideStrIntfSortedMap.PutAll(const AMap: IJclWideStrIntfMap);
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

procedure TJclWideStrIntfSortedMap.PutValue(const Key: WideString; const Value: IInterface);
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
    if FAllowDefaultElements or ((KeysCompare(Key, '') <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclWideStrIntfSortedMap.Remove(const Key: WideString): IInterface;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrIntfSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclWideStrIntfSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrIntfSortedMap.SubMap(const FromKey, ToKey: WideString): IJclWideStrIntfSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclWideStrIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.TailMap(const FromKey: WideString): IJclWideStrIntfSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclWideStrIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.Values: IJclIntfCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


//=== { TJclIntfWideStrSortedMap } ==============================================

constructor TJclIntfWideStrSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclIntfWideStrSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfWideStrSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntfWideStrSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfWideStrSortedMap then
  begin
    MyDest := TJclIntfWideStrSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntfWideStrSortedMap.BinarySearch(const Key: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfWideStrSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.ContainsKey(const Key: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.ContainsValue(const Value: WideString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntfWideStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfWideStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfWideStrSortedMap.Equals(const AMap: IJclIntfWideStrMap): Boolean;
var
  It: IJclIntfIterator;
  Index: Integer;
  AKey: IInterface;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.FirstKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclIntfWideStrSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfWideStrSortedMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;



function TJclIntfWideStrSortedMap.GetValue(const Key: IInterface): WideString;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := '';
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.HeadMap(const ToKey: IInterface): IJclIntfWideStrSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntfWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfWideStrSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.KeyOfValue(const Value: WideString): IInterface;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntfWideStrSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfWideStrSortedMap.KeySet: IJclIntfSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.LastKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclIntfWideStrSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfWideStrSortedMap.PutAll(const AMap: IJclIntfWideStrMap);
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

procedure TJclIntfWideStrSortedMap.PutValue(const Key: IInterface; const Value: WideString);
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
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, '') <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclIntfWideStrSortedMap.Remove(const Key: IInterface): WideString;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfWideStrSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntfWideStrSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfWideStrSortedMap.SubMap(const FromKey, ToKey: IInterface): IJclIntfWideStrSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntfWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfWideStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.TailMap(const FromKey: IInterface): IJclIntfWideStrSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclIntfWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfWideStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.Values: IJclWideStrCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.ValuesCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;


//=== { TJclWideStrWideStrSortedMap } ==============================================

constructor TJclWideStrWideStrSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclWideStrWideStrSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrWideStrSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclWideStrWideStrSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrWideStrSortedMap then
  begin
    MyDest := TJclWideStrWideStrSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclWideStrWideStrSortedMap.BinarySearch(const Key: WideString): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrWideStrSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.ContainsKey(const Key: WideString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.ContainsValue(const Value: WideString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclWideStrWideStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrWideStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclWideStrWideStrSortedMap.Equals(const AMap: IJclWideStrWideStrMap): Boolean;
var
  It: IJclWideStrIterator;
  Index: Integer;
  AKey: WideString;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.FirstKey: WideString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclWideStrWideStrSortedMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrWideStrSortedMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;



function TJclWideStrWideStrSortedMap.GetValue(const Key: WideString): WideString;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := '';
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.HeadMap(const ToKey: WideString): IJclWideStrWideStrSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclWideStrWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrWideStrSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.KeyOfValue(const Value: WideString): WideString;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := '';
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclWideStrWideStrSortedMap.KeysCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclWideStrWideStrSortedMap.KeySet: IJclWideStrSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.LastKey: WideString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclWideStrWideStrSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclWideStrWideStrSortedMap.PutAll(const AMap: IJclWideStrWideStrMap);
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

procedure TJclWideStrWideStrSortedMap.PutValue(const Key: WideString; const Value: WideString);
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
    if FAllowDefaultElements or ((KeysCompare(Key, '') <> 0) and (ValuesCompare(Value, '') <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclWideStrWideStrSortedMap.Remove(const Key: WideString): WideString;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrWideStrSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclWideStrWideStrSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrWideStrSortedMap.SubMap(const FromKey, ToKey: WideString): IJclWideStrWideStrSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclWideStrWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrWideStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.TailMap(const FromKey: WideString): IJclWideStrWideStrSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclWideStrWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrWideStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.Values: IJclWideStrCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.ValuesCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;


//=== { TJclSingleIntfSortedMap } ==============================================

constructor TJclSingleIntfSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclSingleIntfSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclSingleIntfSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclSingleIntfSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclSingleIntfSortedMap then
  begin
    MyDest := TJclSingleIntfSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclSingleIntfSortedMap.BinarySearch(const Key: Single): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleIntfSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfSortedMap.ContainsKey(const Key: Single): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfSortedMap.ContainsValue(const Value: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclSingleIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclSingleIntfSortedMap.Equals(const AMap: IJclSingleIntfMap): Boolean;
var
  It: IJclSingleIterator;
  Index: Integer;
  AKey: Single;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfSortedMap.FirstKey: Single;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclSingleIntfSortedMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclSingleIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclSingleIntfSortedMap.GetValue(const Key: Single): IInterface;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfSortedMap.HeadMap(const ToKey: Single): IJclSingleIntfSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclSingleIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSingleIntfSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfSortedMap.KeyOfValue(const Value: IInterface): Single;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0.0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclSingleIntfSortedMap.KeysCompare(const A, B: Single): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclSingleIntfSortedMap.KeySet: IJclSingleSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfSortedMap.LastKey: Single;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclSingleIntfSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclSingleIntfSortedMap.PutAll(const AMap: IJclSingleIntfMap);
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

procedure TJclSingleIntfSortedMap.PutValue(const Key: Single; const Value: IInterface);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0.0) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclSingleIntfSortedMap.Remove(const Key: Single): IInterface;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleIntfSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclSingleIntfSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleIntfSortedMap.SubMap(const FromKey, ToKey: Single): IJclSingleIntfSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclSingleIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSingleIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfSortedMap.TailMap(const FromKey: Single): IJclSingleIntfSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclSingleIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSingleIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfSortedMap.Values: IJclIntfCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


//=== { TJclIntfSingleSortedMap } ==============================================

constructor TJclIntfSingleSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclIntfSingleSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfSingleSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntfSingleSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfSingleSortedMap then
  begin
    MyDest := TJclIntfSingleSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntfSingleSortedMap.BinarySearch(const Key: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSingleSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleSortedMap.ContainsKey(const Key: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleSortedMap.ContainsValue(const Value: Single): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntfSingleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfSingleSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfSingleSortedMap.Equals(const AMap: IJclIntfSingleMap): Boolean;
var
  It: IJclIntfIterator;
  Index: Integer;
  AKey: IInterface;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleSortedMap.FirstKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclIntfSingleSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfSingleSortedMap.FreeValue(var Value: Single): Single;
begin
  Result := Value;
  Value := 0.0;
end;



function TJclIntfSingleSortedMap.GetValue(const Key: IInterface): Single;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := 0.0;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleSortedMap.HeadMap(const ToKey: IInterface): IJclIntfSingleSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntfSingleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfSingleSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleSortedMap.KeyOfValue(const Value: Single): IInterface;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntfSingleSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfSingleSortedMap.KeySet: IJclIntfSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleSortedMap.LastKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclIntfSingleSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfSingleSortedMap.PutAll(const AMap: IJclIntfSingleMap);
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

procedure TJclIntfSingleSortedMap.PutValue(const Key: IInterface; const Value: Single);
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
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, 0.0) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclIntfSingleSortedMap.Remove(const Key: IInterface): Single;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSingleSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntfSingleSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfSingleSortedMap.SubMap(const FromKey, ToKey: IInterface): IJclIntfSingleSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntfSingleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfSingleSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleSortedMap.TailMap(const FromKey: IInterface): IJclIntfSingleSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclIntfSingleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfSingleSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleSortedMap.Values: IJclSingleCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleSortedMap.ValuesCompare(const A, B: Single): Integer;
begin
  Result := ItemsCompare(A, B);
end;


//=== { TJclSingleSingleSortedMap } ==============================================

constructor TJclSingleSingleSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclSingleSingleSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclSingleSingleSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclSingleSingleSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclSingleSingleSortedMap then
  begin
    MyDest := TJclSingleSingleSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclSingleSingleSortedMap.BinarySearch(const Key: Single): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleSingleSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleSortedMap.ContainsKey(const Key: Single): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleSortedMap.ContainsValue(const Value: Single): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclSingleSingleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleSingleSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclSingleSingleSortedMap.Equals(const AMap: IJclSingleSingleMap): Boolean;
var
  It: IJclSingleIterator;
  Index: Integer;
  AKey: Single;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleSortedMap.FirstKey: Single;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclSingleSingleSortedMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclSingleSingleSortedMap.FreeValue(var Value: Single): Single;
begin
  Result := Value;
  Value := 0.0;
end;



function TJclSingleSingleSortedMap.GetValue(const Key: Single): Single;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := 0.0;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleSortedMap.HeadMap(const ToKey: Single): IJclSingleSingleSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclSingleSingleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSingleSingleSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleSortedMap.KeyOfValue(const Value: Single): Single;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0.0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclSingleSingleSortedMap.KeysCompare(const A, B: Single): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclSingleSingleSortedMap.KeySet: IJclSingleSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleSortedMap.LastKey: Single;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclSingleSingleSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclSingleSingleSortedMap.PutAll(const AMap: IJclSingleSingleMap);
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

procedure TJclSingleSingleSortedMap.PutValue(const Key: Single; const Value: Single);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0.0) <> 0) and (ValuesCompare(Value, 0.0) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclSingleSingleSortedMap.Remove(const Key: Single): Single;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleSingleSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclSingleSingleSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleSingleSortedMap.SubMap(const FromKey, ToKey: Single): IJclSingleSingleSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclSingleSingleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSingleSingleSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleSortedMap.TailMap(const FromKey: Single): IJclSingleSingleSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclSingleSingleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSingleSingleSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleSortedMap.Values: IJclSingleCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleSortedMap.ValuesCompare(const A, B: Single): Integer;
begin
  Result := ItemsCompare(A, B);
end;


//=== { TJclDoubleIntfSortedMap } ==============================================

constructor TJclDoubleIntfSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclDoubleIntfSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclDoubleIntfSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclDoubleIntfSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclDoubleIntfSortedMap then
  begin
    MyDest := TJclDoubleIntfSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclDoubleIntfSortedMap.BinarySearch(const Key: Double): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleIntfSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfSortedMap.ContainsKey(const Key: Double): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfSortedMap.ContainsValue(const Value: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclDoubleIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclDoubleIntfSortedMap.Equals(const AMap: IJclDoubleIntfMap): Boolean;
var
  It: IJclDoubleIterator;
  Index: Integer;
  AKey: Double;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfSortedMap.FirstKey: Double;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclDoubleIntfSortedMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclDoubleIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclDoubleIntfSortedMap.GetValue(const Key: Double): IInterface;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfSortedMap.HeadMap(const ToKey: Double): IJclDoubleIntfSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclDoubleIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclDoubleIntfSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfSortedMap.KeyOfValue(const Value: IInterface): Double;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0.0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclDoubleIntfSortedMap.KeysCompare(const A, B: Double): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclDoubleIntfSortedMap.KeySet: IJclDoubleSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfSortedMap.LastKey: Double;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclDoubleIntfSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclDoubleIntfSortedMap.PutAll(const AMap: IJclDoubleIntfMap);
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

procedure TJclDoubleIntfSortedMap.PutValue(const Key: Double; const Value: IInterface);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0.0) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclDoubleIntfSortedMap.Remove(const Key: Double): IInterface;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleIntfSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclDoubleIntfSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleIntfSortedMap.SubMap(const FromKey, ToKey: Double): IJclDoubleIntfSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclDoubleIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclDoubleIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfSortedMap.TailMap(const FromKey: Double): IJclDoubleIntfSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclDoubleIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclDoubleIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfSortedMap.Values: IJclIntfCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


//=== { TJclIntfDoubleSortedMap } ==============================================

constructor TJclIntfDoubleSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclIntfDoubleSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfDoubleSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntfDoubleSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfDoubleSortedMap then
  begin
    MyDest := TJclIntfDoubleSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntfDoubleSortedMap.BinarySearch(const Key: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfDoubleSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleSortedMap.ContainsKey(const Key: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleSortedMap.ContainsValue(const Value: Double): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntfDoubleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfDoubleSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfDoubleSortedMap.Equals(const AMap: IJclIntfDoubleMap): Boolean;
var
  It: IJclIntfIterator;
  Index: Integer;
  AKey: IInterface;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleSortedMap.FirstKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclIntfDoubleSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfDoubleSortedMap.FreeValue(var Value: Double): Double;
begin
  Result := Value;
  Value := 0.0;
end;



function TJclIntfDoubleSortedMap.GetValue(const Key: IInterface): Double;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := 0.0;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleSortedMap.HeadMap(const ToKey: IInterface): IJclIntfDoubleSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntfDoubleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfDoubleSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleSortedMap.KeyOfValue(const Value: Double): IInterface;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntfDoubleSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfDoubleSortedMap.KeySet: IJclIntfSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleSortedMap.LastKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclIntfDoubleSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfDoubleSortedMap.PutAll(const AMap: IJclIntfDoubleMap);
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

procedure TJclIntfDoubleSortedMap.PutValue(const Key: IInterface; const Value: Double);
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
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, 0.0) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclIntfDoubleSortedMap.Remove(const Key: IInterface): Double;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfDoubleSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntfDoubleSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfDoubleSortedMap.SubMap(const FromKey, ToKey: IInterface): IJclIntfDoubleSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntfDoubleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfDoubleSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleSortedMap.TailMap(const FromKey: IInterface): IJclIntfDoubleSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclIntfDoubleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfDoubleSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleSortedMap.Values: IJclDoubleCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleSortedMap.ValuesCompare(const A, B: Double): Integer;
begin
  Result := ItemsCompare(A, B);
end;


//=== { TJclDoubleDoubleSortedMap } ==============================================

constructor TJclDoubleDoubleSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclDoubleDoubleSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclDoubleDoubleSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclDoubleDoubleSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclDoubleDoubleSortedMap then
  begin
    MyDest := TJclDoubleDoubleSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclDoubleDoubleSortedMap.BinarySearch(const Key: Double): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleDoubleSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleSortedMap.ContainsKey(const Key: Double): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleSortedMap.ContainsValue(const Value: Double): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclDoubleDoubleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleDoubleSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclDoubleDoubleSortedMap.Equals(const AMap: IJclDoubleDoubleMap): Boolean;
var
  It: IJclDoubleIterator;
  Index: Integer;
  AKey: Double;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleSortedMap.FirstKey: Double;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclDoubleDoubleSortedMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclDoubleDoubleSortedMap.FreeValue(var Value: Double): Double;
begin
  Result := Value;
  Value := 0.0;
end;



function TJclDoubleDoubleSortedMap.GetValue(const Key: Double): Double;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := 0.0;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleSortedMap.HeadMap(const ToKey: Double): IJclDoubleDoubleSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclDoubleDoubleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclDoubleDoubleSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleSortedMap.KeyOfValue(const Value: Double): Double;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0.0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclDoubleDoubleSortedMap.KeysCompare(const A, B: Double): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclDoubleDoubleSortedMap.KeySet: IJclDoubleSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleSortedMap.LastKey: Double;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclDoubleDoubleSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclDoubleDoubleSortedMap.PutAll(const AMap: IJclDoubleDoubleMap);
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

procedure TJclDoubleDoubleSortedMap.PutValue(const Key: Double; const Value: Double);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0.0) <> 0) and (ValuesCompare(Value, 0.0) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclDoubleDoubleSortedMap.Remove(const Key: Double): Double;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleDoubleSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclDoubleDoubleSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleDoubleSortedMap.SubMap(const FromKey, ToKey: Double): IJclDoubleDoubleSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclDoubleDoubleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclDoubleDoubleSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleSortedMap.TailMap(const FromKey: Double): IJclDoubleDoubleSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclDoubleDoubleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclDoubleDoubleSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleSortedMap.Values: IJclDoubleCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleSortedMap.ValuesCompare(const A, B: Double): Integer;
begin
  Result := ItemsCompare(A, B);
end;


//=== { TJclExtendedIntfSortedMap } ==============================================

constructor TJclExtendedIntfSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclExtendedIntfSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclExtendedIntfSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclExtendedIntfSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclExtendedIntfSortedMap then
  begin
    MyDest := TJclExtendedIntfSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclExtendedIntfSortedMap.BinarySearch(const Key: Extended): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedIntfSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfSortedMap.ContainsKey(const Key: Extended): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfSortedMap.ContainsValue(const Value: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclExtendedIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclExtendedIntfSortedMap.Equals(const AMap: IJclExtendedIntfMap): Boolean;
var
  It: IJclExtendedIterator;
  Index: Integer;
  AKey: Extended;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfSortedMap.FirstKey: Extended;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclExtendedIntfSortedMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclExtendedIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclExtendedIntfSortedMap.GetValue(const Key: Extended): IInterface;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfSortedMap.HeadMap(const ToKey: Extended): IJclExtendedIntfSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclExtendedIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclExtendedIntfSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfSortedMap.KeyOfValue(const Value: IInterface): Extended;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0.0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclExtendedIntfSortedMap.KeysCompare(const A, B: Extended): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclExtendedIntfSortedMap.KeySet: IJclExtendedSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfSortedMap.LastKey: Extended;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclExtendedIntfSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclExtendedIntfSortedMap.PutAll(const AMap: IJclExtendedIntfMap);
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

procedure TJclExtendedIntfSortedMap.PutValue(const Key: Extended; const Value: IInterface);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0.0) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclExtendedIntfSortedMap.Remove(const Key: Extended): IInterface;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedIntfSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclExtendedIntfSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedIntfSortedMap.SubMap(const FromKey, ToKey: Extended): IJclExtendedIntfSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclExtendedIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclExtendedIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfSortedMap.TailMap(const FromKey: Extended): IJclExtendedIntfSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclExtendedIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclExtendedIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfSortedMap.Values: IJclIntfCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


//=== { TJclIntfExtendedSortedMap } ==============================================

constructor TJclIntfExtendedSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclIntfExtendedSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfExtendedSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntfExtendedSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfExtendedSortedMap then
  begin
    MyDest := TJclIntfExtendedSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntfExtendedSortedMap.BinarySearch(const Key: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfExtendedSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedSortedMap.ContainsKey(const Key: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedSortedMap.ContainsValue(const Value: Extended): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntfExtendedSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfExtendedSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfExtendedSortedMap.Equals(const AMap: IJclIntfExtendedMap): Boolean;
var
  It: IJclIntfIterator;
  Index: Integer;
  AKey: IInterface;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedSortedMap.FirstKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclIntfExtendedSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfExtendedSortedMap.FreeValue(var Value: Extended): Extended;
begin
  Result := Value;
  Value := 0.0;
end;



function TJclIntfExtendedSortedMap.GetValue(const Key: IInterface): Extended;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := 0.0;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedSortedMap.HeadMap(const ToKey: IInterface): IJclIntfExtendedSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntfExtendedSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfExtendedSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedSortedMap.KeyOfValue(const Value: Extended): IInterface;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntfExtendedSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfExtendedSortedMap.KeySet: IJclIntfSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedSortedMap.LastKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclIntfExtendedSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfExtendedSortedMap.PutAll(const AMap: IJclIntfExtendedMap);
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

procedure TJclIntfExtendedSortedMap.PutValue(const Key: IInterface; const Value: Extended);
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
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, 0.0) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclIntfExtendedSortedMap.Remove(const Key: IInterface): Extended;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfExtendedSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntfExtendedSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfExtendedSortedMap.SubMap(const FromKey, ToKey: IInterface): IJclIntfExtendedSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntfExtendedSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfExtendedSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedSortedMap.TailMap(const FromKey: IInterface): IJclIntfExtendedSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclIntfExtendedSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfExtendedSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedSortedMap.Values: IJclExtendedCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedSortedMap.ValuesCompare(const A, B: Extended): Integer;
begin
  Result := ItemsCompare(A, B);
end;


//=== { TJclExtendedExtendedSortedMap } ==============================================

constructor TJclExtendedExtendedSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclExtendedExtendedSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclExtendedExtendedSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclExtendedExtendedSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclExtendedExtendedSortedMap then
  begin
    MyDest := TJclExtendedExtendedSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclExtendedExtendedSortedMap.BinarySearch(const Key: Extended): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedExtendedSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedSortedMap.ContainsKey(const Key: Extended): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedSortedMap.ContainsValue(const Value: Extended): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclExtendedExtendedSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedExtendedSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclExtendedExtendedSortedMap.Equals(const AMap: IJclExtendedExtendedMap): Boolean;
var
  It: IJclExtendedIterator;
  Index: Integer;
  AKey: Extended;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedSortedMap.FirstKey: Extended;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclExtendedExtendedSortedMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclExtendedExtendedSortedMap.FreeValue(var Value: Extended): Extended;
begin
  Result := Value;
  Value := 0.0;
end;



function TJclExtendedExtendedSortedMap.GetValue(const Key: Extended): Extended;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := 0.0;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedSortedMap.HeadMap(const ToKey: Extended): IJclExtendedExtendedSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclExtendedExtendedSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclExtendedExtendedSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedSortedMap.KeyOfValue(const Value: Extended): Extended;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0.0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclExtendedExtendedSortedMap.KeysCompare(const A, B: Extended): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclExtendedExtendedSortedMap.KeySet: IJclExtendedSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedSortedMap.LastKey: Extended;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclExtendedExtendedSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclExtendedExtendedSortedMap.PutAll(const AMap: IJclExtendedExtendedMap);
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

procedure TJclExtendedExtendedSortedMap.PutValue(const Key: Extended; const Value: Extended);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0.0) <> 0) and (ValuesCompare(Value, 0.0) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclExtendedExtendedSortedMap.Remove(const Key: Extended): Extended;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := 0.0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedExtendedSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclExtendedExtendedSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedExtendedSortedMap.SubMap(const FromKey, ToKey: Extended): IJclExtendedExtendedSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclExtendedExtendedSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclExtendedExtendedSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedSortedMap.TailMap(const FromKey: Extended): IJclExtendedExtendedSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclExtendedExtendedSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclExtendedExtendedSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedSortedMap.Values: IJclExtendedCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedSortedMap.ValuesCompare(const A, B: Extended): Integer;
begin
  Result := ItemsCompare(A, B);
end;


//=== { TJclIntegerIntfSortedMap } ==============================================

constructor TJclIntegerIntfSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclIntegerIntfSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntegerIntfSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntegerIntfSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntegerIntfSortedMap then
  begin
    MyDest := TJclIntegerIntfSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntegerIntfSortedMap.BinarySearch(Key: Integer): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntfSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfSortedMap.ContainsKey(Key: Integer): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfSortedMap.ContainsValue(const Value: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntegerIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntegerIntfSortedMap.Equals(const AMap: IJclIntegerIntfMap): Boolean;
var
  It: IJclIntegerIterator;
  Index: Integer;
  AKey: Integer;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfSortedMap.FirstKey: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclIntegerIntfSortedMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

function TJclIntegerIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclIntegerIntfSortedMap.GetValue(Key: Integer): IInterface;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfSortedMap.HeadMap(ToKey: Integer): IJclIntegerIntfSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntegerIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntegerIntfSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfSortedMap.KeyOfValue(const Value: IInterface): Integer;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntegerIntfSortedMap.KeysCompare(A, B: Integer): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclIntegerIntfSortedMap.KeySet: IJclIntegerSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfSortedMap.LastKey: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclIntegerIntfSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntegerIntfSortedMap.PutAll(const AMap: IJclIntegerIntfMap);
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

procedure TJclIntegerIntfSortedMap.PutValue(Key: Integer; const Value: IInterface);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclIntegerIntfSortedMap.Remove(Key: Integer): IInterface;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntfSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntegerIntfSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerIntfSortedMap.SubMap(FromKey, ToKey: Integer): IJclIntegerIntfSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntegerIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntegerIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfSortedMap.TailMap(FromKey: Integer): IJclIntegerIntfSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclIntegerIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntegerIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfSortedMap.Values: IJclIntfCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


//=== { TJclIntfIntegerSortedMap } ==============================================

constructor TJclIntfIntegerSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclIntfIntegerSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfIntegerSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntfIntegerSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfIntegerSortedMap then
  begin
    MyDest := TJclIntfIntegerSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntfIntegerSortedMap.BinarySearch(const Key: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntegerSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerSortedMap.ContainsKey(const Key: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerSortedMap.ContainsValue(Value: Integer): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntfIntegerSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntegerSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfIntegerSortedMap.Equals(const AMap: IJclIntfIntegerMap): Boolean;
var
  It: IJclIntfIterator;
  Index: Integer;
  AKey: IInterface;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerSortedMap.FirstKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclIntfIntegerSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfIntegerSortedMap.FreeValue(var Value: Integer): Integer;
begin
  Result := Value;
  Value := 0;
end;



function TJclIntfIntegerSortedMap.GetValue(const Key: IInterface): Integer;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := 0;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerSortedMap.HeadMap(const ToKey: IInterface): IJclIntfIntegerSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntfIntegerSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfIntegerSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerSortedMap.KeyOfValue(Value: Integer): IInterface;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntfIntegerSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfIntegerSortedMap.KeySet: IJclIntfSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerSortedMap.LastKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclIntfIntegerSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfIntegerSortedMap.PutAll(const AMap: IJclIntfIntegerMap);
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

procedure TJclIntfIntegerSortedMap.PutValue(const Key: IInterface; Value: Integer);
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
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, 0) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclIntfIntegerSortedMap.Remove(const Key: IInterface): Integer;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntegerSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntfIntegerSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfIntegerSortedMap.SubMap(const FromKey, ToKey: IInterface): IJclIntfIntegerSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntfIntegerSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfIntegerSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerSortedMap.TailMap(const FromKey: IInterface): IJclIntfIntegerSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclIntfIntegerSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfIntegerSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerSortedMap.Values: IJclIntegerCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerSortedMap.ValuesCompare(A, B: Integer): Integer;
begin
  Result := ItemsCompare(A, B);
end;


//=== { TJclIntegerIntegerSortedMap } ==============================================

constructor TJclIntegerIntegerSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclIntegerIntegerSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntegerIntegerSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntegerIntegerSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntegerIntegerSortedMap then
  begin
    MyDest := TJclIntegerIntegerSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntegerIntegerSortedMap.BinarySearch(Key: Integer): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntegerSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerSortedMap.ContainsKey(Key: Integer): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerSortedMap.ContainsValue(Value: Integer): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntegerIntegerSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerIntegerSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntegerIntegerSortedMap.Equals(const AMap: IJclIntegerIntegerMap): Boolean;
var
  It: IJclIntegerIterator;
  Index: Integer;
  AKey: Integer;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerSortedMap.FirstKey: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclIntegerIntegerSortedMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

function TJclIntegerIntegerSortedMap.FreeValue(var Value: Integer): Integer;
begin
  Result := Value;
  Value := 0;
end;



function TJclIntegerIntegerSortedMap.GetValue(Key: Integer): Integer;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := 0;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerSortedMap.HeadMap(ToKey: Integer): IJclIntegerIntegerSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntegerIntegerSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntegerIntegerSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerSortedMap.KeyOfValue(Value: Integer): Integer;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntegerIntegerSortedMap.KeysCompare(A, B: Integer): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclIntegerIntegerSortedMap.KeySet: IJclIntegerSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerSortedMap.LastKey: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclIntegerIntegerSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntegerIntegerSortedMap.PutAll(const AMap: IJclIntegerIntegerMap);
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

procedure TJclIntegerIntegerSortedMap.PutValue(Key: Integer; Value: Integer);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0) <> 0) and (ValuesCompare(Value, 0) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclIntegerIntegerSortedMap.Remove(Key: Integer): Integer;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntegerSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntegerIntegerSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerIntegerSortedMap.SubMap(FromKey, ToKey: Integer): IJclIntegerIntegerSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntegerIntegerSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntegerIntegerSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerSortedMap.TailMap(FromKey: Integer): IJclIntegerIntegerSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclIntegerIntegerSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntegerIntegerSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerSortedMap.Values: IJclIntegerCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerSortedMap.ValuesCompare(A, B: Integer): Integer;
begin
  Result := ItemsCompare(A, B);
end;


//=== { TJclCardinalIntfSortedMap } ==============================================

constructor TJclCardinalIntfSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclCardinalIntfSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclCardinalIntfSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclCardinalIntfSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclCardinalIntfSortedMap then
  begin
    MyDest := TJclCardinalIntfSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclCardinalIntfSortedMap.BinarySearch(Key: Cardinal): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalIntfSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfSortedMap.ContainsKey(Key: Cardinal): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfSortedMap.ContainsValue(const Value: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclCardinalIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclCardinalIntfSortedMap.Equals(const AMap: IJclCardinalIntfMap): Boolean;
var
  It: IJclCardinalIterator;
  Index: Integer;
  AKey: Cardinal;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfSortedMap.FirstKey: Cardinal;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclCardinalIntfSortedMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

function TJclCardinalIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclCardinalIntfSortedMap.GetValue(Key: Cardinal): IInterface;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfSortedMap.HeadMap(ToKey: Cardinal): IJclCardinalIntfSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclCardinalIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclCardinalIntfSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfSortedMap.KeyOfValue(const Value: IInterface): Cardinal;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclCardinalIntfSortedMap.KeysCompare(A, B: Cardinal): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclCardinalIntfSortedMap.KeySet: IJclCardinalSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfSortedMap.LastKey: Cardinal;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclCardinalIntfSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclCardinalIntfSortedMap.PutAll(const AMap: IJclCardinalIntfMap);
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

procedure TJclCardinalIntfSortedMap.PutValue(Key: Cardinal; const Value: IInterface);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclCardinalIntfSortedMap.Remove(Key: Cardinal): IInterface;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalIntfSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclCardinalIntfSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalIntfSortedMap.SubMap(FromKey, ToKey: Cardinal): IJclCardinalIntfSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclCardinalIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclCardinalIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfSortedMap.TailMap(FromKey: Cardinal): IJclCardinalIntfSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclCardinalIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclCardinalIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfSortedMap.Values: IJclIntfCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


//=== { TJclIntfCardinalSortedMap } ==============================================

constructor TJclIntfCardinalSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclIntfCardinalSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfCardinalSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntfCardinalSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfCardinalSortedMap then
  begin
    MyDest := TJclIntfCardinalSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntfCardinalSortedMap.BinarySearch(const Key: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfCardinalSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalSortedMap.ContainsKey(const Key: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalSortedMap.ContainsValue(Value: Cardinal): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntfCardinalSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfCardinalSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfCardinalSortedMap.Equals(const AMap: IJclIntfCardinalMap): Boolean;
var
  It: IJclIntfIterator;
  Index: Integer;
  AKey: IInterface;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalSortedMap.FirstKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclIntfCardinalSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfCardinalSortedMap.FreeValue(var Value: Cardinal): Cardinal;
begin
  Result := Value;
  Value := 0;
end;



function TJclIntfCardinalSortedMap.GetValue(const Key: IInterface): Cardinal;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := 0;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalSortedMap.HeadMap(const ToKey: IInterface): IJclIntfCardinalSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntfCardinalSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfCardinalSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalSortedMap.KeyOfValue(Value: Cardinal): IInterface;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntfCardinalSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfCardinalSortedMap.KeySet: IJclIntfSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalSortedMap.LastKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclIntfCardinalSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfCardinalSortedMap.PutAll(const AMap: IJclIntfCardinalMap);
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

procedure TJclIntfCardinalSortedMap.PutValue(const Key: IInterface; Value: Cardinal);
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
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, 0) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclIntfCardinalSortedMap.Remove(const Key: IInterface): Cardinal;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfCardinalSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntfCardinalSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfCardinalSortedMap.SubMap(const FromKey, ToKey: IInterface): IJclIntfCardinalSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntfCardinalSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfCardinalSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalSortedMap.TailMap(const FromKey: IInterface): IJclIntfCardinalSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclIntfCardinalSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfCardinalSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalSortedMap.Values: IJclCardinalCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalSortedMap.ValuesCompare(A, B: Cardinal): Integer;
begin
  Result := ItemsCompare(A, B);
end;


//=== { TJclCardinalCardinalSortedMap } ==============================================

constructor TJclCardinalCardinalSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclCardinalCardinalSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclCardinalCardinalSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclCardinalCardinalSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclCardinalCardinalSortedMap then
  begin
    MyDest := TJclCardinalCardinalSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclCardinalCardinalSortedMap.BinarySearch(Key: Cardinal): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalCardinalSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalSortedMap.ContainsKey(Key: Cardinal): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalSortedMap.ContainsValue(Value: Cardinal): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclCardinalCardinalSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalCardinalSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclCardinalCardinalSortedMap.Equals(const AMap: IJclCardinalCardinalMap): Boolean;
var
  It: IJclCardinalIterator;
  Index: Integer;
  AKey: Cardinal;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalSortedMap.FirstKey: Cardinal;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclCardinalCardinalSortedMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

function TJclCardinalCardinalSortedMap.FreeValue(var Value: Cardinal): Cardinal;
begin
  Result := Value;
  Value := 0;
end;



function TJclCardinalCardinalSortedMap.GetValue(Key: Cardinal): Cardinal;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := 0;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalSortedMap.HeadMap(ToKey: Cardinal): IJclCardinalCardinalSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclCardinalCardinalSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclCardinalCardinalSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalSortedMap.KeyOfValue(Value: Cardinal): Cardinal;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclCardinalCardinalSortedMap.KeysCompare(A, B: Cardinal): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclCardinalCardinalSortedMap.KeySet: IJclCardinalSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalSortedMap.LastKey: Cardinal;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclCardinalCardinalSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclCardinalCardinalSortedMap.PutAll(const AMap: IJclCardinalCardinalMap);
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

procedure TJclCardinalCardinalSortedMap.PutValue(Key: Cardinal; Value: Cardinal);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0) <> 0) and (ValuesCompare(Value, 0) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclCardinalCardinalSortedMap.Remove(Key: Cardinal): Cardinal;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalCardinalSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclCardinalCardinalSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalCardinalSortedMap.SubMap(FromKey, ToKey: Cardinal): IJclCardinalCardinalSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclCardinalCardinalSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclCardinalCardinalSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalSortedMap.TailMap(FromKey: Cardinal): IJclCardinalCardinalSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclCardinalCardinalSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclCardinalCardinalSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalSortedMap.Values: IJclCardinalCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalSortedMap.ValuesCompare(A, B: Cardinal): Integer;
begin
  Result := ItemsCompare(A, B);
end;


//=== { TJclInt64IntfSortedMap } ==============================================

constructor TJclInt64IntfSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclInt64IntfSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclInt64IntfSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclInt64IntfSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclInt64IntfSortedMap then
  begin
    MyDest := TJclInt64IntfSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclInt64IntfSortedMap.BinarySearch(const Key: Int64): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64IntfSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfSortedMap.ContainsKey(const Key: Int64): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfSortedMap.ContainsValue(const Value: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclInt64IntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64IntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclInt64IntfSortedMap.Equals(const AMap: IJclInt64IntfMap): Boolean;
var
  It: IJclInt64Iterator;
  Index: Integer;
  AKey: Int64;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfSortedMap.FirstKey: Int64;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclInt64IntfSortedMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

function TJclInt64IntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclInt64IntfSortedMap.GetValue(const Key: Int64): IInterface;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfSortedMap.HeadMap(const ToKey: Int64): IJclInt64IntfSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclInt64IntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclInt64IntfSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfSortedMap.KeyOfValue(const Value: IInterface): Int64;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclInt64IntfSortedMap.KeysCompare(const A, B: Int64): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclInt64IntfSortedMap.KeySet: IJclInt64Set;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfSortedMap.LastKey: Int64;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclInt64IntfSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclInt64IntfSortedMap.PutAll(const AMap: IJclInt64IntfMap);
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

procedure TJclInt64IntfSortedMap.PutValue(const Key: Int64; const Value: IInterface);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclInt64IntfSortedMap.Remove(const Key: Int64): IInterface;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64IntfSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclInt64IntfSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64IntfSortedMap.SubMap(const FromKey, ToKey: Int64): IJclInt64IntfSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclInt64IntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclInt64IntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfSortedMap.TailMap(const FromKey: Int64): IJclInt64IntfSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclInt64IntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclInt64IntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfSortedMap.Values: IJclIntfCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


//=== { TJclIntfInt64SortedMap } ==============================================

constructor TJclIntfInt64SortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclIntfInt64SortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfInt64SortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntfInt64SortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfInt64SortedMap then
  begin
    MyDest := TJclIntfInt64SortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntfInt64SortedMap.BinarySearch(const Key: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfInt64SortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64SortedMap.ContainsKey(const Key: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64SortedMap.ContainsValue(const Value: Int64): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntfInt64SortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfInt64SortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfInt64SortedMap.Equals(const AMap: IJclIntfInt64Map): Boolean;
var
  It: IJclIntfIterator;
  Index: Integer;
  AKey: IInterface;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64SortedMap.FirstKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclIntfInt64SortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfInt64SortedMap.FreeValue(var Value: Int64): Int64;
begin
  Result := Value;
  Value := 0;
end;



function TJclIntfInt64SortedMap.GetValue(const Key: IInterface): Int64;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := 0;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64SortedMap.HeadMap(const ToKey: IInterface): IJclIntfInt64SortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntfInt64SortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfInt64SortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64SortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64SortedMap.KeyOfValue(const Value: Int64): IInterface;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntfInt64SortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfInt64SortedMap.KeySet: IJclIntfSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64SortedMap.LastKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclIntfInt64SortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfInt64SortedMap.PutAll(const AMap: IJclIntfInt64Map);
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

procedure TJclIntfInt64SortedMap.PutValue(const Key: IInterface; const Value: Int64);
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
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, 0) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclIntfInt64SortedMap.Remove(const Key: IInterface): Int64;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfInt64SortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntfInt64SortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfInt64SortedMap.SubMap(const FromKey, ToKey: IInterface): IJclIntfInt64SortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntfInt64SortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfInt64SortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64SortedMap.TailMap(const FromKey: IInterface): IJclIntfInt64SortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclIntfInt64SortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfInt64SortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64SortedMap.Values: IJclInt64Collection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64SortedMap.ValuesCompare(const A, B: Int64): Integer;
begin
  Result := ItemsCompare(A, B);
end;


//=== { TJclInt64Int64SortedMap } ==============================================

constructor TJclInt64Int64SortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclInt64Int64SortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclInt64Int64SortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclInt64Int64SortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclInt64Int64SortedMap then
  begin
    MyDest := TJclInt64Int64SortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclInt64Int64SortedMap.BinarySearch(const Key: Int64): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Int64SortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64SortedMap.ContainsKey(const Key: Int64): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64SortedMap.ContainsValue(const Value: Int64): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclInt64Int64SortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Int64SortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclInt64Int64SortedMap.Equals(const AMap: IJclInt64Int64Map): Boolean;
var
  It: IJclInt64Iterator;
  Index: Integer;
  AKey: Int64;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64SortedMap.FirstKey: Int64;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclInt64Int64SortedMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

function TJclInt64Int64SortedMap.FreeValue(var Value: Int64): Int64;
begin
  Result := Value;
  Value := 0;
end;



function TJclInt64Int64SortedMap.GetValue(const Key: Int64): Int64;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := 0;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64SortedMap.HeadMap(const ToKey: Int64): IJclInt64Int64SortedMap;
var
  ToIndex: Integer;
  NewMap: TJclInt64Int64SortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclInt64Int64SortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64SortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64SortedMap.KeyOfValue(const Value: Int64): Int64;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclInt64Int64SortedMap.KeysCompare(const A, B: Int64): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclInt64Int64SortedMap.KeySet: IJclInt64Set;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64SortedMap.LastKey: Int64;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclInt64Int64SortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclInt64Int64SortedMap.PutAll(const AMap: IJclInt64Int64Map);
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

procedure TJclInt64Int64SortedMap.PutValue(const Key: Int64; const Value: Int64);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0) <> 0) and (ValuesCompare(Value, 0) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclInt64Int64SortedMap.Remove(const Key: Int64): Int64;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Int64SortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclInt64Int64SortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64Int64SortedMap.SubMap(const FromKey, ToKey: Int64): IJclInt64Int64SortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclInt64Int64SortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclInt64Int64SortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64SortedMap.TailMap(const FromKey: Int64): IJclInt64Int64SortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclInt64Int64SortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclInt64Int64SortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64SortedMap.Values: IJclInt64Collection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64SortedMap.ValuesCompare(const A, B: Int64): Integer;
begin
  Result := ItemsCompare(A, B);
end;


{$IFNDEF CLR}
//=== { TJclPtrIntfSortedMap } ==============================================

constructor TJclPtrIntfSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclPtrIntfSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclPtrIntfSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclPtrIntfSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclPtrIntfSortedMap then
  begin
    MyDest := TJclPtrIntfSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclPtrIntfSortedMap.BinarySearch(Key: Pointer): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrIntfSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfSortedMap.ContainsKey(Key: Pointer): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfSortedMap.ContainsValue(const Value: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclPtrIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclPtrIntfSortedMap.Equals(const AMap: IJclPtrIntfMap): Boolean;
var
  It: IJclPtrIterator;
  Index: Integer;
  AKey: Pointer;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfSortedMap.FirstKey: Pointer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclPtrIntfSortedMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

function TJclPtrIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclPtrIntfSortedMap.GetValue(Key: Pointer): IInterface;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfSortedMap.HeadMap(ToKey: Pointer): IJclPtrIntfSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclPtrIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclPtrIntfSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfSortedMap.KeyOfValue(const Value: IInterface): Pointer;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclPtrIntfSortedMap.KeysCompare(A, B: Pointer): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclPtrIntfSortedMap.KeySet: IJclPtrSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfSortedMap.LastKey: Pointer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclPtrIntfSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclPtrIntfSortedMap.PutAll(const AMap: IJclPtrIntfMap);
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

procedure TJclPtrIntfSortedMap.PutValue(Key: Pointer; const Value: IInterface);
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
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclPtrIntfSortedMap.Remove(Key: Pointer): IInterface;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrIntfSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclPtrIntfSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrIntfSortedMap.SubMap(FromKey, ToKey: Pointer): IJclPtrIntfSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclPtrIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclPtrIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfSortedMap.TailMap(FromKey: Pointer): IJclPtrIntfSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclPtrIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclPtrIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfSortedMap.Values: IJclIntfCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


//=== { TJclIntfPtrSortedMap } ==============================================

constructor TJclIntfPtrSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclIntfPtrSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfPtrSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntfPtrSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfPtrSortedMap then
  begin
    MyDest := TJclIntfPtrSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntfPtrSortedMap.BinarySearch(const Key: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfPtrSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrSortedMap.ContainsKey(const Key: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrSortedMap.ContainsValue(Value: Pointer): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntfPtrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfPtrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfPtrSortedMap.Equals(const AMap: IJclIntfPtrMap): Boolean;
var
  It: IJclIntfIterator;
  Index: Integer;
  AKey: IInterface;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrSortedMap.FirstKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclIntfPtrSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfPtrSortedMap.FreeValue(var Value: Pointer): Pointer;
begin
  Result := Value;
  Value := nil;
end;



function TJclIntfPtrSortedMap.GetValue(const Key: IInterface): Pointer;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrSortedMap.HeadMap(const ToKey: IInterface): IJclIntfPtrSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntfPtrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfPtrSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrSortedMap.KeyOfValue(Value: Pointer): IInterface;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntfPtrSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfPtrSortedMap.KeySet: IJclIntfSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrSortedMap.LastKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclIntfPtrSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfPtrSortedMap.PutAll(const AMap: IJclIntfPtrMap);
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

procedure TJclIntfPtrSortedMap.PutValue(const Key: IInterface; Value: Pointer);
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
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclIntfPtrSortedMap.Remove(const Key: IInterface): Pointer;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfPtrSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntfPtrSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfPtrSortedMap.SubMap(const FromKey, ToKey: IInterface): IJclIntfPtrSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntfPtrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfPtrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrSortedMap.TailMap(const FromKey: IInterface): IJclIntfPtrSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclIntfPtrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfPtrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrSortedMap.Values: IJclPtrCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrSortedMap.ValuesCompare(A, B: Pointer): Integer;
begin
  Result := ItemsCompare(A, B);
end;


//=== { TJclPtrPtrSortedMap } ==============================================

constructor TJclPtrPtrSortedMap.Create(ACapacity: Integer);
begin
  inherited Create();
  SetCapacity(ACapacity);
end;

destructor TJclPtrPtrSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclPtrPtrSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclPtrPtrSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclPtrPtrSortedMap then
  begin
    MyDest := TJclPtrPtrSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclPtrPtrSortedMap.BinarySearch(Key: Pointer): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrPtrSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrSortedMap.ContainsKey(Key: Pointer): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrSortedMap.ContainsValue(Value: Pointer): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclPtrPtrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrPtrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclPtrPtrSortedMap.Equals(const AMap: IJclPtrPtrMap): Boolean;
var
  It: IJclPtrIterator;
  Index: Integer;
  AKey: Pointer;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrSortedMap.FirstKey: Pointer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclPtrPtrSortedMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

function TJclPtrPtrSortedMap.FreeValue(var Value: Pointer): Pointer;
begin
  Result := Value;
  Value := nil;
end;



function TJclPtrPtrSortedMap.GetValue(Key: Pointer): Pointer;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrSortedMap.HeadMap(ToKey: Pointer): IJclPtrPtrSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclPtrPtrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclPtrPtrSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrSortedMap.KeyOfValue(Value: Pointer): Pointer;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclPtrPtrSortedMap.KeysCompare(A, B: Pointer): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclPtrPtrSortedMap.KeySet: IJclPtrSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrSortedMap.LastKey: Pointer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclPtrPtrSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclPtrPtrSortedMap.PutAll(const AMap: IJclPtrPtrMap);
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

procedure TJclPtrPtrSortedMap.PutValue(Key: Pointer; Value: Pointer);
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
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclPtrPtrSortedMap.Remove(Key: Pointer): Pointer;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrPtrSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclPtrPtrSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrPtrSortedMap.SubMap(FromKey, ToKey: Pointer): IJclPtrPtrSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclPtrPtrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclPtrPtrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrSortedMap.TailMap(FromKey: Pointer): IJclPtrPtrSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclPtrPtrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclPtrPtrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrSortedMap.Values: IJclPtrCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrSortedMap.ValuesCompare(A, B: Pointer): Integer;
begin
  Result := ItemsCompare(A, B);
end;

{$ENDIF ~CLR}

//=== { TJclIntfSortedMap } ==============================================

constructor TJclIntfSortedMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create();
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
end;

destructor TJclIntfSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntfSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfSortedMap then
  begin
    MyDest := TJclIntfSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntfSortedMap.BinarySearch(const Key: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.ContainsKey(const Key: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.ContainsValue(Value: TObject): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclIntfSortedMap.Equals(const AMap: IJclIntfMap): Boolean;
var
  It: IJclIntfIterator;
  Index: Integer;
  AKey: IInterface;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.FirstKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclIntfSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfSortedMap.FreeValue(var Value: TObject): TObject;
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


function TJclIntfSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclIntfSortedMap.GetValue(const Key: IInterface): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.HeadMap(const ToKey: IInterface): IJclIntfSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.KeyOfValue(Value: TObject): IInterface;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntfSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfSortedMap.KeySet: IJclIntfSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.LastKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclIntfSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfSortedMap.PutAll(const AMap: IJclIntfMap);
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

procedure TJclIntfSortedMap.PutValue(const Key: IInterface; Value: TObject);
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
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclIntfSortedMap.Remove(const Key: IInterface): TObject;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntfSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfSortedMap.SubMap(const FromKey, ToKey: IInterface): IJclIntfSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.TailMap(const FromKey: IInterface): IJclIntfSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.Values: IJclCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


//=== { TJclAnsiStrSortedMap } ==============================================

constructor TJclAnsiStrSortedMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create();
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
end;

destructor TJclAnsiStrSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclAnsiStrSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrSortedMap then
  begin
    MyDest := TJclAnsiStrSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclAnsiStrSortedMap.BinarySearch(const Key: AnsiString): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.ContainsKey(const Key: AnsiString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.ContainsValue(Value: TObject): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclAnsiStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrSortedMap.Equals(const AMap: IJclAnsiStrMap): Boolean;
var
  It: IJclAnsiStrIterator;
  Index: Integer;
  AKey: AnsiString;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.FirstKey: AnsiString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclAnsiStrSortedMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrSortedMap.FreeValue(var Value: TObject): TObject;
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


function TJclAnsiStrSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclAnsiStrSortedMap.GetValue(const Key: AnsiString): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.HeadMap(const ToKey: AnsiString): IJclAnsiStrSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.KeyOfValue(Value: TObject): AnsiString;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := '';
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclAnsiStrSortedMap.KeysCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclAnsiStrSortedMap.KeySet: IJclAnsiStrSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.LastKey: AnsiString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclAnsiStrSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclAnsiStrSortedMap.PutAll(const AMap: IJclAnsiStrMap);
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

procedure TJclAnsiStrSortedMap.PutValue(const Key: AnsiString; Value: TObject);
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
    if FAllowDefaultElements or ((KeysCompare(Key, '') <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclAnsiStrSortedMap.Remove(const Key: AnsiString): TObject;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclAnsiStrSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrSortedMap.SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.TailMap(const FromKey: AnsiString): IJclAnsiStrSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.Values: IJclCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


//=== { TJclWideStrSortedMap } ==============================================

constructor TJclWideStrSortedMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create();
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
end;

destructor TJclWideStrSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclWideStrSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrSortedMap then
  begin
    MyDest := TJclWideStrSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclWideStrSortedMap.BinarySearch(const Key: WideString): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.ContainsKey(const Key: WideString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.ContainsValue(Value: TObject): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclWideStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclWideStrSortedMap.Equals(const AMap: IJclWideStrMap): Boolean;
var
  It: IJclWideStrIterator;
  Index: Integer;
  AKey: WideString;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.FirstKey: WideString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclWideStrSortedMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrSortedMap.FreeValue(var Value: TObject): TObject;
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


function TJclWideStrSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclWideStrSortedMap.GetValue(const Key: WideString): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.HeadMap(const ToKey: WideString): IJclWideStrSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.KeyOfValue(Value: TObject): WideString;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := '';
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclWideStrSortedMap.KeysCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclWideStrSortedMap.KeySet: IJclWideStrSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.LastKey: WideString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclWideStrSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclWideStrSortedMap.PutAll(const AMap: IJclWideStrMap);
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

procedure TJclWideStrSortedMap.PutValue(const Key: WideString; Value: TObject);
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
    if FAllowDefaultElements or ((KeysCompare(Key, '') <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclWideStrSortedMap.Remove(const Key: WideString): TObject;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclWideStrSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrSortedMap.SubMap(const FromKey, ToKey: WideString): IJclWideStrSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.TailMap(const FromKey: WideString): IJclWideStrSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.Values: IJclCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


//=== { TJclSingleSortedMap } ==============================================

constructor TJclSingleSortedMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create();
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
end;

destructor TJclSingleSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclSingleSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclSingleSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclSingleSortedMap then
  begin
    MyDest := TJclSingleSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclSingleSortedMap.BinarySearch(const Key: Single): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSortedMap.ContainsKey(const Key: Single): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSortedMap.ContainsValue(Value: TObject): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclSingleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclSingleSortedMap.Equals(const AMap: IJclSingleMap): Boolean;
var
  It: IJclSingleIterator;
  Index: Integer;
  AKey: Single;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSortedMap.FirstKey: Single;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclSingleSortedMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclSingleSortedMap.FreeValue(var Value: TObject): TObject;
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


function TJclSingleSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclSingleSortedMap.GetValue(const Key: Single): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSortedMap.HeadMap(const ToKey: Single): IJclSingleSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclSingleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSingleSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSortedMap.KeyOfValue(Value: TObject): Single;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0.0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclSingleSortedMap.KeysCompare(const A, B: Single): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclSingleSortedMap.KeySet: IJclSingleSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSortedMap.LastKey: Single;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclSingleSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclSingleSortedMap.PutAll(const AMap: IJclSingleMap);
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

procedure TJclSingleSortedMap.PutValue(const Key: Single; Value: TObject);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0.0) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclSingleSortedMap.Remove(const Key: Single): TObject;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclSingleSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleSortedMap.SubMap(const FromKey, ToKey: Single): IJclSingleSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclSingleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSingleSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSortedMap.TailMap(const FromKey: Single): IJclSingleSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclSingleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSingleSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSortedMap.Values: IJclCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


//=== { TJclDoubleSortedMap } ==============================================

constructor TJclDoubleSortedMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create();
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
end;

destructor TJclDoubleSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclDoubleSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclDoubleSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclDoubleSortedMap then
  begin
    MyDest := TJclDoubleSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclDoubleSortedMap.BinarySearch(const Key: Double): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleSortedMap.ContainsKey(const Key: Double): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleSortedMap.ContainsValue(Value: TObject): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclDoubleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclDoubleSortedMap.Equals(const AMap: IJclDoubleMap): Boolean;
var
  It: IJclDoubleIterator;
  Index: Integer;
  AKey: Double;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleSortedMap.FirstKey: Double;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclDoubleSortedMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclDoubleSortedMap.FreeValue(var Value: TObject): TObject;
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


function TJclDoubleSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclDoubleSortedMap.GetValue(const Key: Double): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleSortedMap.HeadMap(const ToKey: Double): IJclDoubleSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclDoubleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclDoubleSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleSortedMap.KeyOfValue(Value: TObject): Double;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0.0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclDoubleSortedMap.KeysCompare(const A, B: Double): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclDoubleSortedMap.KeySet: IJclDoubleSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleSortedMap.LastKey: Double;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclDoubleSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclDoubleSortedMap.PutAll(const AMap: IJclDoubleMap);
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

procedure TJclDoubleSortedMap.PutValue(const Key: Double; Value: TObject);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0.0) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclDoubleSortedMap.Remove(const Key: Double): TObject;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclDoubleSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleSortedMap.SubMap(const FromKey, ToKey: Double): IJclDoubleSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclDoubleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclDoubleSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleSortedMap.TailMap(const FromKey: Double): IJclDoubleSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclDoubleSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclDoubleSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleSortedMap.Values: IJclCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


//=== { TJclExtendedSortedMap } ==============================================

constructor TJclExtendedSortedMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create();
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
end;

destructor TJclExtendedSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclExtendedSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclExtendedSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclExtendedSortedMap then
  begin
    MyDest := TJclExtendedSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclExtendedSortedMap.BinarySearch(const Key: Extended): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedSortedMap.ContainsKey(const Key: Extended): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedSortedMap.ContainsValue(Value: TObject): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclExtendedSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclExtendedSortedMap.Equals(const AMap: IJclExtendedMap): Boolean;
var
  It: IJclExtendedIterator;
  Index: Integer;
  AKey: Extended;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedSortedMap.FirstKey: Extended;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclExtendedSortedMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclExtendedSortedMap.FreeValue(var Value: TObject): TObject;
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


function TJclExtendedSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclExtendedSortedMap.GetValue(const Key: Extended): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedSortedMap.HeadMap(const ToKey: Extended): IJclExtendedSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclExtendedSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclExtendedSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedSortedMap.KeyOfValue(Value: TObject): Extended;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0.0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclExtendedSortedMap.KeysCompare(const A, B: Extended): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclExtendedSortedMap.KeySet: IJclExtendedSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedSortedMap.LastKey: Extended;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclExtendedSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0.0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclExtendedSortedMap.PutAll(const AMap: IJclExtendedMap);
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

procedure TJclExtendedSortedMap.PutValue(const Key: Extended; Value: TObject);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0.0) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclExtendedSortedMap.Remove(const Key: Extended): TObject;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclExtendedSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedSortedMap.SubMap(const FromKey, ToKey: Extended): IJclExtendedSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclExtendedSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclExtendedSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedSortedMap.TailMap(const FromKey: Extended): IJclExtendedSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclExtendedSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclExtendedSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedSortedMap.Values: IJclCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


//=== { TJclIntegerSortedMap } ==============================================

constructor TJclIntegerSortedMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create();
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
end;

destructor TJclIntegerSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntegerSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntegerSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntegerSortedMap then
  begin
    MyDest := TJclIntegerSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntegerSortedMap.BinarySearch(Key: Integer): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerSortedMap.ContainsKey(Key: Integer): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerSortedMap.ContainsValue(Value: TObject): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntegerSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclIntegerSortedMap.Equals(const AMap: IJclIntegerMap): Boolean;
var
  It: IJclIntegerIterator;
  Index: Integer;
  AKey: Integer;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerSortedMap.FirstKey: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclIntegerSortedMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

function TJclIntegerSortedMap.FreeValue(var Value: TObject): TObject;
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


function TJclIntegerSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclIntegerSortedMap.GetValue(Key: Integer): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerSortedMap.HeadMap(ToKey: Integer): IJclIntegerSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntegerSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntegerSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerSortedMap.KeyOfValue(Value: TObject): Integer;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntegerSortedMap.KeysCompare(A, B: Integer): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclIntegerSortedMap.KeySet: IJclIntegerSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerSortedMap.LastKey: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclIntegerSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntegerSortedMap.PutAll(const AMap: IJclIntegerMap);
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

procedure TJclIntegerSortedMap.PutValue(Key: Integer; Value: TObject);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclIntegerSortedMap.Remove(Key: Integer): TObject;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntegerSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerSortedMap.SubMap(FromKey, ToKey: Integer): IJclIntegerSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntegerSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntegerSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerSortedMap.TailMap(FromKey: Integer): IJclIntegerSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclIntegerSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntegerSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerSortedMap.Values: IJclCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


//=== { TJclCardinalSortedMap } ==============================================

constructor TJclCardinalSortedMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create();
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
end;

destructor TJclCardinalSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclCardinalSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclCardinalSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclCardinalSortedMap then
  begin
    MyDest := TJclCardinalSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclCardinalSortedMap.BinarySearch(Key: Cardinal): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalSortedMap.ContainsKey(Key: Cardinal): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalSortedMap.ContainsValue(Value: TObject): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclCardinalSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclCardinalSortedMap.Equals(const AMap: IJclCardinalMap): Boolean;
var
  It: IJclCardinalIterator;
  Index: Integer;
  AKey: Cardinal;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalSortedMap.FirstKey: Cardinal;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclCardinalSortedMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

function TJclCardinalSortedMap.FreeValue(var Value: TObject): TObject;
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


function TJclCardinalSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclCardinalSortedMap.GetValue(Key: Cardinal): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalSortedMap.HeadMap(ToKey: Cardinal): IJclCardinalSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclCardinalSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclCardinalSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalSortedMap.KeyOfValue(Value: TObject): Cardinal;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclCardinalSortedMap.KeysCompare(A, B: Cardinal): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclCardinalSortedMap.KeySet: IJclCardinalSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalSortedMap.LastKey: Cardinal;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclCardinalSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclCardinalSortedMap.PutAll(const AMap: IJclCardinalMap);
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

procedure TJclCardinalSortedMap.PutValue(Key: Cardinal; Value: TObject);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclCardinalSortedMap.Remove(Key: Cardinal): TObject;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclCardinalSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalSortedMap.SubMap(FromKey, ToKey: Cardinal): IJclCardinalSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclCardinalSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclCardinalSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalSortedMap.TailMap(FromKey: Cardinal): IJclCardinalSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclCardinalSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclCardinalSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalSortedMap.Values: IJclCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


//=== { TJclInt64SortedMap } ==============================================

constructor TJclInt64SortedMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create();
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
end;

destructor TJclInt64SortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclInt64SortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclInt64SortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclInt64SortedMap then
  begin
    MyDest := TJclInt64SortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclInt64SortedMap.BinarySearch(const Key: Int64): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64SortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64SortedMap.ContainsKey(const Key: Int64): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64SortedMap.ContainsValue(Value: TObject): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclInt64SortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64SortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclInt64SortedMap.Equals(const AMap: IJclInt64Map): Boolean;
var
  It: IJclInt64Iterator;
  Index: Integer;
  AKey: Int64;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64SortedMap.FirstKey: Int64;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclInt64SortedMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

function TJclInt64SortedMap.FreeValue(var Value: TObject): TObject;
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


function TJclInt64SortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclInt64SortedMap.GetValue(const Key: Int64): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64SortedMap.HeadMap(const ToKey: Int64): IJclInt64SortedMap;
var
  ToIndex: Integer;
  NewMap: TJclInt64SortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclInt64SortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64SortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64SortedMap.KeyOfValue(Value: TObject): Int64;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := 0;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclInt64SortedMap.KeysCompare(const A, B: Int64): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclInt64SortedMap.KeySet: IJclInt64Set;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64SortedMap.LastKey: Int64;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclInt64SortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := 0;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclInt64SortedMap.PutAll(const AMap: IJclInt64Map);
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

procedure TJclInt64SortedMap.PutValue(const Key: Int64; Value: TObject);
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
    if FAllowDefaultElements or ((KeysCompare(Key, 0) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclInt64SortedMap.Remove(const Key: Int64): TObject;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64SortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclInt64SortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64SortedMap.SubMap(const FromKey, ToKey: Int64): IJclInt64SortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclInt64SortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclInt64SortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64SortedMap.TailMap(const FromKey: Int64): IJclInt64SortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclInt64SortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclInt64SortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64SortedMap.Values: IJclCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64SortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


{$IFNDEF CLR}
//=== { TJclPtrSortedMap } ==============================================

constructor TJclPtrSortedMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create();
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
end;

destructor TJclPtrSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclPtrSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclPtrSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclPtrSortedMap then
  begin
    MyDest := TJclPtrSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclPtrSortedMap.BinarySearch(Key: Pointer): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrSortedMap.ContainsKey(Key: Pointer): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrSortedMap.ContainsValue(Value: TObject): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclPtrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclPtrSortedMap.Equals(const AMap: IJclPtrMap): Boolean;
var
  It: IJclPtrIterator;
  Index: Integer;
  AKey: Pointer;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrSortedMap.FirstKey: Pointer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclPtrSortedMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

function TJclPtrSortedMap.FreeValue(var Value: TObject): TObject;
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


function TJclPtrSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclPtrSortedMap.GetValue(Key: Pointer): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrSortedMap.HeadMap(ToKey: Pointer): IJclPtrSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclPtrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclPtrSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrSortedMap.KeyOfValue(Value: TObject): Pointer;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclPtrSortedMap.KeysCompare(A, B: Pointer): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclPtrSortedMap.KeySet: IJclPtrSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrSortedMap.LastKey: Pointer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclPtrSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclPtrSortedMap.PutAll(const AMap: IJclPtrMap);
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

procedure TJclPtrSortedMap.PutValue(Key: Pointer; Value: TObject);
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
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclPtrSortedMap.Remove(Key: Pointer): TObject;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclPtrSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrSortedMap.SubMap(FromKey, ToKey: Pointer): IJclPtrSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclPtrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclPtrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrSortedMap.TailMap(FromKey: Pointer): IJclPtrSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclPtrSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclPtrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrSortedMap.Values: IJclCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

{$ENDIF ~CLR}

//=== { TJclSortedMap } ==============================================

constructor TJclSortedMap.Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create();
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
end;

destructor TJclSortedMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclSortedMap then
  begin
    MyDest := TJclSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclSortedMap.BinarySearch(Key: TObject): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSortedMap.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.ContainsKey(Key: TObject): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.ContainsValue(Value: TObject): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSortedMap.Create(FSize, False, False);
  AssignPropertiesTo(Result);
end;

function TJclSortedMap.Equals(const AMap: IJclMap): Boolean;
var
  It: IJclIterator;
  Index: Integer;
  AKey: TObject;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.FirstKey: TObject;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclSortedMap.FreeKey(var Key: TObject): TObject;
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

function TJclSortedMap.FreeValue(var Value: TObject): TObject;
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

function TJclSortedMap.GetOWnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

function TJclSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclSortedMap.GetValue(Key: TObject): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.HeadMap(ToKey: TObject): IJclSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.KeyOfValue(Value: TObject): TObject;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclSortedMap.KeysCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclSortedMap.KeySet: IJclSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArraySet.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.LastKey: TObject;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclSortedMap.PutAll(const AMap: IJclMap);
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

procedure TJclSortedMap.PutValue(Key: TObject; Value: TObject);
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
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclSortedMap.Remove(Key: TObject): TObject;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSortedMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclSortedMap.SubMap(FromKey, ToKey: TObject): IJclSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.TailMap(FromKey: TObject): IJclSortedMap;
var
  FromIndex, Index: Integer;
  NewMap: TJclSortedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.Values: IJclCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;


{$IFDEF SUPPORTS_GENERICS}

//=== { TJclSortedMap<TKey,TValue> } ==============================================

constructor TJclSortedMap<TKey,TValue>.Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create();
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
end;

destructor TJclSortedMap<TKey,TValue>.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclSortedMap<TKey,TValue>.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclSortedMap<TKey,TValue>;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclSortedMap<TKey,TValue> then
  begin
    MyDest := TJclSortedMap<TKey,TValue>(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclSortedMap<TKey,TValue>.BinarySearch(const Key: TKey): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSortedMap<TKey,TValue>.Clear;
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
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.ContainsKey(const Key: TKey): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.ContainsValue(const Value: TValue): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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


function TJclSortedMap<TKey,TValue>.Equals(const AMap: IJclMap<TKey,TValue>): Boolean;
var
  It: IJclIterator<TKey>;
  Index: Integer;
  AKey: TKey;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.FirstKey: TKey;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := Default(TKey);
    if FSize > 0 then
      Result := FEntries[0].Key
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

function TJclSortedMap<TKey,TValue>.FreeKey(var Key: TKey): TKey;
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

function TJclSortedMap<TKey,TValue>.FreeValue(var Value: TValue): TValue;
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

function TJclSortedMap<TKey,TValue>.GetOWnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

function TJclSortedMap<TKey,TValue>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclSortedMap<TKey,TValue>.GetValue(const Key: TKey): TValue;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := Default(TValue);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.HeadMap(const ToKey: TKey): IJclSortedMap<TKey,TValue>;
var
  ToIndex: Integer;
  NewMap: TJclSortedMap<TKey,TValue>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSortedMap<TKey,TValue>;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
      while ToIndex >= 0 do
      begin
        NewMap.FEntries[ToIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.KeyOfValue(const Value: TValue): TKey;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
   Found := False;
    Result := Default(TKey);
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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


function TJclSortedMap<TKey,TValue>.KeySet: IJclSet<TKey>;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyArraySet(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.LastKey: TKey;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := Default(TKey);
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
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

procedure TJclSortedMap<TKey,TValue>.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := Default(TKey);
        FEntries[FromIndex + I].Value := Default(TValue);
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := Default(TKey);
        FEntries[FromIndex + I].Value := Default(TValue);
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := Default(TKey);
        FEntries[FromIndex + I].Value := Default(TValue);
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := Default(TKey);
        FEntries[FromIndex + I].Value := Default(TValue);
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclSortedMap<TKey,TValue>.PutAll(const AMap: IJclMap<TKey,TValue>);
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

procedure TJclSortedMap<TKey,TValue>.PutValue(const Key: TKey; const Value: TValue);
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
    if FAllowDefaultElements or ((KeysCompare(Key, Default(TKey)) <> 0) and (ValuesCompare(Value, Default(TValue)) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
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

function TJclSortedMap<TKey,TValue>.Remove(const Key: TKey): TValue;
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
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := Default(TValue);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSortedMap<TKey,TValue>.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclSortedMap<TKey,TValue>.Size: Integer;
begin
  Result := FSize;
end;

function TJclSortedMap<TKey,TValue>.SubMap(const FromKey, ToKey: TKey): IJclSortedMap<TKey,TValue>;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclSortedMap<TKey,TValue>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSortedMap<TKey,TValue>;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
      while ToIndex >= FromIndex do
      begin
        NewMap.FEntries[ToIndex - FromIndex] := FEntries[ToIndex];
        Dec(ToIndex);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.TailMap(const FromKey: TKey): IJclSortedMap<TKey,TValue>;
var
  FromIndex, Index: Integer;
  NewMap: TJclSortedMap<TKey,TValue>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSortedMap<TKey,TValue>;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
      Index := FromIndex;
      while Index < FSize do
      begin
        NewMap.FEntries[Index - FromIndex] := FEntries[Index];
        Inc(Index);
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.Values: IJclCollection<TValue>;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyArrayList(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;



//=== { TJclSortedMapE<TKey, TValue> } =======================================

constructor TJclSortedMapE<TKey, TValue>.Create(const AKeyComparer: IComparer<TKey>;
  const AValueComparer: IComparer<TValue>; const AValueEqualityComparer: IEqualityComparer<TValue>; ACapacity: Integer;
  AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(ACapacity, AOwnsValues, AOwnsKeys);
  FKeyComparer := AKeyComparer;
  FValueComparer := AValueComparer;
  FValueEqualityComparer := AValueEqualityComparer;
end;

procedure TJclSortedMapE<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclSortedMapE<TKey, TValue>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclSortedMapE<TKey, TValue> then
  begin
    ADest := TJclSortedMapE<TKey, TValue>(Dest);
    ADest.FKeyComparer := FKeyComparer;
    ADest.FValueComparer := FValueComparer;
  end;
end;

function TJclSortedMapE<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer;
  AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  if FValueEqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := TJclArrayListE<TValue>.Create(FValueEqualityComparer, ACapacity, AOwnsObjects);
end;

function TJclSortedMapE<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSortedMapE<TKey, TValue>.Create(FKeyComparer, FValueComparer, FValueEqualityComparer, FCapacity,
    FOwnsValues, FOwnsKeys);
  AssignPropertiesTo(Result);
end;

function TJclSortedMapE<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TJclArraySetE<TKey>.Create(FKeyComparer, FCapacity, AOwnsObjects);
end;

function TJclSortedMapE<TKey, TValue>.KeysCompare(const A, B: TKey): Integer;
begin
  if KeyComparer = nil then
    raise EJclNoComparerError.Create;
  Result := KeyComparer.Compare(A, B);
end;

function TJclSortedMapE<TKey, TValue>.ValuesCompare(const A, B: TValue): Integer;
begin
  if ValueComparer = nil then
    raise EJclNoComparerError.Create;
  Result := ValueComparer.Compare(A, B);
end;

//=== { TJclSortedMapF<TKey, TValue> } =======================================

constructor TJclSortedMapF<TKey, TValue>.Create(AKeyCompare: TCompare<TKey>; AValueCompare: TCompare<TValue>;
  AValueEqualityCompare: TEqualityCompare<TValue>; ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(ACapacity, AOwnsValues, AOwnsKeys);
  FKeyCompare := AKeyCompare;
  FValueCompare := AValueCompare;
  FValueEqualityCompare := AValueEqualityCompare;
end;

procedure TJclSortedMapF<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclSortedMapF<TKey, TValue>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclSortedMapF<TKey, TValue> then
  begin
    ADest := TJclSortedMapF<TKey, TValue>(Dest);
    ADest.FKeyCompare := FKeyCompare;
    ADest.FValueCompare := FValueCompare;
  end;
end;

function TJclSortedMapF<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer;
  AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  if not Assigned(FValueEqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := TJclArrayListF<TValue>.Create(FValueEqualityCompare, ACapacity, AOwnsObjects);
end;

function TJclSortedMapF<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSortedMapF<TKey, TValue>.Create(FKeyCompare, FValueCompare, FValueEqualityCompare, FCapacity,
    FOwnsValues, FOwnsKeys);
  AssignPropertiesTo(Result);
end;

function TJclSortedMapF<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TJclArraySetF<TKey>.Create(FKeyCompare, FCapacity, AOwnsObjects);
end;

function TJclSortedMapF<TKey, TValue>.KeysCompare(const A, B: TKey): Integer;
begin
  if not Assigned(KeyCompare) then
    raise EJclNoComparerError.Create;
  Result := KeyCompare(A, B);
end;

function TJclSortedMapF<TKey, TValue>.ValuesCompare(const A, B: TValue): Integer;
begin
  if not Assigned(ValueCompare) then
    raise EJclNoComparerError.Create;
  Result := ValueCompare(A, B);
end;

//=== { TJclSortedMapI<TKey, TValue> } =======================================

function TJclSortedMapI<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer;
  AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  Result := TJclArrayListI<TValue>.Create(ACapacity, AOwnsObjects);
end;

function TJclSortedMapI<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSortedMapI<TKey, TValue>.Create(FCapacity, FOwnsValues, FOwnsKeys);
  AssignPropertiesTo(Result);
end;

function TJclSortedMapI<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TJclArraySetI<TKey>.Create(FCapacity, AOwnsObjects);
end;

function TJclSortedMapI<TKey, TValue>.KeysCompare(const A, B: TKey): Integer;
begin
  Result := A.CompareTo(B);
end;

function TJclSortedMapI<TKey, TValue>.ValuesCompare(const A, B: TValue): Integer;
begin
  Result := A.CompareTo(B);
end;

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
