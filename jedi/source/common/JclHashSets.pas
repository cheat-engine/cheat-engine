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
{ Last modified: $Date:: 2008-07-20 11:13:23 +0200 (dim., 20 juil. 2008)                         $ }
{ Revision:      $Rev:: 2393                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
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

type
  {$IFDEF SUPPORTS_GENERICS}
  TRefUnique = class;
  TRefUnique = class(TEquatable<TRefUnique>)
  end;
  {$ELSE ~SUPPORTS_GENERICS}
  TRefUnique = TObject;
  {$ENDIF ~SUPPORTS_GENERICS}

  TJclIntfHashSet = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclIntfEqualityComparer,
    IJclIntfCollection, IJclIntfSet)
  private
    FMap: IJclIntfMap;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    function GetAutoPackParameter: Integer; override;
    function GetAutoPackStrategy: TJclAutoPackStrategy; override;
    function GetCapacity: Integer; override;
    procedure Pack; override;
    procedure SetAutoPackParameter(Value: Integer); override;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); override;
    procedure SetCapacity(Value: Integer); override;
    { IJclContainer }
    function GetAllowDefaultElements: Boolean; override;
    function GetDuplicates: TDuplicates; override;
    function GetReadOnly: Boolean; override;
    function GetRemoveSingleElement: Boolean; override;
    function GetReturnDefaultElements: Boolean; override;
    function GetThreadSafe: Boolean; override;
    procedure SetAllowDefaultElements(Value: Boolean); override;
    procedure SetDuplicates(Value: TDuplicates); override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure SetRemoveSingleElement(Value: Boolean); override;
    procedure SetReturnDefaultElements(Value: Boolean); override;
    procedure SetThreadSafe(Value: Boolean); override;
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
    { IJclIntfSet }
    procedure Intersect(const ACollection: IJclIntfCollection);
    procedure Subtract(const ACollection: IJclIntfCollection);
    procedure Union(const ACollection: IJclIntfCollection);
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AMap: IJclIntfMap); overload;
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
  end;

  TJclAnsiStrHashSet = class(TJclAnsiStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer, IJclAnsiStrEqualityComparer,
    IJclAnsiStrCollection, IJclAnsiStrSet)
  private
    FMap: IJclAnsiStrMap;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    function GetAutoPackParameter: Integer; override;
    function GetAutoPackStrategy: TJclAutoPackStrategy; override;
    function GetCapacity: Integer; override;
    procedure Pack; override;
    procedure SetAutoPackParameter(Value: Integer); override;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); override;
    procedure SetCapacity(Value: Integer); override;
    { IJclContainer }
    function GetAllowDefaultElements: Boolean; override;
    function GetDuplicates: TDuplicates; override;
    function GetReadOnly: Boolean; override;
    function GetRemoveSingleElement: Boolean; override;
    function GetReturnDefaultElements: Boolean; override;
    function GetThreadSafe: Boolean; override;
    procedure SetAllowDefaultElements(Value: Boolean); override;
    procedure SetDuplicates(Value: TDuplicates); override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure SetRemoveSingleElement(Value: Boolean); override;
    procedure SetReturnDefaultElements(Value: Boolean); override;
    procedure SetThreadSafe(Value: Boolean); override;
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
    { IJclAnsiStrSet }
    procedure Intersect(const ACollection: IJclAnsiStrCollection);
    procedure Subtract(const ACollection: IJclAnsiStrCollection);
    procedure Union(const ACollection: IJclAnsiStrCollection);
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStrContainer }
    function GetCaseSensitive: Boolean; override;
    procedure SetCaseSensitive(Value: Boolean); override;
    { IJclAnsiStrContainer }
    function GetEncoding: TJclAnsiStrEncoding; override;
    procedure SetEncoding(Value: TJclAnsiStrEncoding); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AMap: IJclAnsiStrMap); overload;
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
  end;

  TJclWideStrHashSet = class(TJclWideStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer, IJclWideStrEqualityComparer,
    IJclWideStrCollection, IJclWideStrSet)
  private
    FMap: IJclWideStrMap;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    function GetAutoPackParameter: Integer; override;
    function GetAutoPackStrategy: TJclAutoPackStrategy; override;
    function GetCapacity: Integer; override;
    procedure Pack; override;
    procedure SetAutoPackParameter(Value: Integer); override;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); override;
    procedure SetCapacity(Value: Integer); override;
    { IJclContainer }
    function GetAllowDefaultElements: Boolean; override;
    function GetDuplicates: TDuplicates; override;
    function GetReadOnly: Boolean; override;
    function GetRemoveSingleElement: Boolean; override;
    function GetReturnDefaultElements: Boolean; override;
    function GetThreadSafe: Boolean; override;
    procedure SetAllowDefaultElements(Value: Boolean); override;
    procedure SetDuplicates(Value: TDuplicates); override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure SetRemoveSingleElement(Value: Boolean); override;
    procedure SetReturnDefaultElements(Value: Boolean); override;
    procedure SetThreadSafe(Value: Boolean); override;
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
    { IJclWideStrSet }
    procedure Intersect(const ACollection: IJclWideStrCollection);
    procedure Subtract(const ACollection: IJclWideStrCollection);
    procedure Union(const ACollection: IJclWideStrCollection);
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStrContainer }
    function GetCaseSensitive: Boolean; override;
    procedure SetCaseSensitive(Value: Boolean); override;
    { IJclWideStrContainer }
    function GetEncoding: TJclWideStrEncoding; override;
    procedure SetEncoding(Value: TJclWideStrEncoding); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AMap: IJclWideStrMap); overload;
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrHashSet = TJclAnsiStrHashSet;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrHashSet = TJclWideStrHashSet;
  {$ENDIF CONTAINER_WIDESTR}

  TJclSingleHashSet = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclSingleContainer, IJclSingleEqualityComparer,
    IJclSingleCollection, IJclSingleSet)
  private
    FMap: IJclSingleMap;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    function GetAutoPackParameter: Integer; override;
    function GetAutoPackStrategy: TJclAutoPackStrategy; override;
    function GetCapacity: Integer; override;
    procedure Pack; override;
    procedure SetAutoPackParameter(Value: Integer); override;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); override;
    procedure SetCapacity(Value: Integer); override;
    { IJclContainer }
    function GetAllowDefaultElements: Boolean; override;
    function GetDuplicates: TDuplicates; override;
    function GetReadOnly: Boolean; override;
    function GetRemoveSingleElement: Boolean; override;
    function GetReturnDefaultElements: Boolean; override;
    function GetThreadSafe: Boolean; override;
    procedure SetAllowDefaultElements(Value: Boolean); override;
    procedure SetDuplicates(Value: TDuplicates); override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure SetRemoveSingleElement(Value: Boolean); override;
    procedure SetReturnDefaultElements(Value: Boolean); override;
    procedure SetThreadSafe(Value: Boolean); override;
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
    { IJclSingleSet }
    procedure Intersect(const ACollection: IJclSingleCollection);
    procedure Subtract(const ACollection: IJclSingleCollection);
    procedure Union(const ACollection: IJclSingleCollection);
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclSingleContainer }
    function GetPrecision: Single; override;
    procedure SetPrecision(const Value: Single); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AMap: IJclSingleMap); overload;
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
  end;

  TJclDoubleHashSet = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclDoubleContainer, IJclDoubleEqualityComparer,
    IJclDoubleCollection, IJclDoubleSet)
  private
    FMap: IJclDoubleMap;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    function GetAutoPackParameter: Integer; override;
    function GetAutoPackStrategy: TJclAutoPackStrategy; override;
    function GetCapacity: Integer; override;
    procedure Pack; override;
    procedure SetAutoPackParameter(Value: Integer); override;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); override;
    procedure SetCapacity(Value: Integer); override;
    { IJclContainer }
    function GetAllowDefaultElements: Boolean; override;
    function GetDuplicates: TDuplicates; override;
    function GetReadOnly: Boolean; override;
    function GetRemoveSingleElement: Boolean; override;
    function GetReturnDefaultElements: Boolean; override;
    function GetThreadSafe: Boolean; override;
    procedure SetAllowDefaultElements(Value: Boolean); override;
    procedure SetDuplicates(Value: TDuplicates); override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure SetRemoveSingleElement(Value: Boolean); override;
    procedure SetReturnDefaultElements(Value: Boolean); override;
    procedure SetThreadSafe(Value: Boolean); override;
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
    { IJclDoubleSet }
    procedure Intersect(const ACollection: IJclDoubleCollection);
    procedure Subtract(const ACollection: IJclDoubleCollection);
    procedure Union(const ACollection: IJclDoubleCollection);
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclDoubleContainer }
    function GetPrecision: Double; override;
    procedure SetPrecision(const Value: Double); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AMap: IJclDoubleMap); overload;
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
  end;

  TJclExtendedHashSet = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclExtendedContainer, IJclExtendedEqualityComparer,
    IJclExtendedCollection, IJclExtendedSet)
  private
    FMap: IJclExtendedMap;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    function GetAutoPackParameter: Integer; override;
    function GetAutoPackStrategy: TJclAutoPackStrategy; override;
    function GetCapacity: Integer; override;
    procedure Pack; override;
    procedure SetAutoPackParameter(Value: Integer); override;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); override;
    procedure SetCapacity(Value: Integer); override;
    { IJclContainer }
    function GetAllowDefaultElements: Boolean; override;
    function GetDuplicates: TDuplicates; override;
    function GetReadOnly: Boolean; override;
    function GetRemoveSingleElement: Boolean; override;
    function GetReturnDefaultElements: Boolean; override;
    function GetThreadSafe: Boolean; override;
    procedure SetAllowDefaultElements(Value: Boolean); override;
    procedure SetDuplicates(Value: TDuplicates); override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure SetRemoveSingleElement(Value: Boolean); override;
    procedure SetReturnDefaultElements(Value: Boolean); override;
    procedure SetThreadSafe(Value: Boolean); override;
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
    { IJclExtendedSet }
    procedure Intersect(const ACollection: IJclExtendedCollection);
    procedure Subtract(const ACollection: IJclExtendedCollection);
    procedure Union(const ACollection: IJclExtendedCollection);
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclExtendedContainer }
    function GetPrecision: Extended; override;
    procedure SetPrecision(const Value: Extended); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AMap: IJclExtendedMap); overload;
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
  end;

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatHashSet = TJclExtendedHashSet;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatHashSet = TJclDoubleHashSet;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatHashSet = TJclSingleHashSet;
  {$ENDIF MATH_SINGLE_PRECISION}

  TJclIntegerHashSet = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclIntegerEqualityComparer,
    IJclIntegerCollection, IJclIntegerSet)
  private
    FMap: IJclIntegerMap;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    function GetAutoPackParameter: Integer; override;
    function GetAutoPackStrategy: TJclAutoPackStrategy; override;
    function GetCapacity: Integer; override;
    procedure Pack; override;
    procedure SetAutoPackParameter(Value: Integer); override;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); override;
    procedure SetCapacity(Value: Integer); override;
    { IJclContainer }
    function GetAllowDefaultElements: Boolean; override;
    function GetDuplicates: TDuplicates; override;
    function GetReadOnly: Boolean; override;
    function GetRemoveSingleElement: Boolean; override;
    function GetReturnDefaultElements: Boolean; override;
    function GetThreadSafe: Boolean; override;
    procedure SetAllowDefaultElements(Value: Boolean); override;
    procedure SetDuplicates(Value: TDuplicates); override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure SetRemoveSingleElement(Value: Boolean); override;
    procedure SetReturnDefaultElements(Value: Boolean); override;
    procedure SetThreadSafe(Value: Boolean); override;
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
    { IJclIntegerSet }
    procedure Intersect(const ACollection: IJclIntegerCollection);
    procedure Subtract(const ACollection: IJclIntegerCollection);
    procedure Union(const ACollection: IJclIntegerCollection);
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AMap: IJclIntegerMap); overload;
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
  end;

  TJclCardinalHashSet = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclCardinalEqualityComparer,
    IJclCardinalCollection, IJclCardinalSet)
  private
    FMap: IJclCardinalMap;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    function GetAutoPackParameter: Integer; override;
    function GetAutoPackStrategy: TJclAutoPackStrategy; override;
    function GetCapacity: Integer; override;
    procedure Pack; override;
    procedure SetAutoPackParameter(Value: Integer); override;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); override;
    procedure SetCapacity(Value: Integer); override;
    { IJclContainer }
    function GetAllowDefaultElements: Boolean; override;
    function GetDuplicates: TDuplicates; override;
    function GetReadOnly: Boolean; override;
    function GetRemoveSingleElement: Boolean; override;
    function GetReturnDefaultElements: Boolean; override;
    function GetThreadSafe: Boolean; override;
    procedure SetAllowDefaultElements(Value: Boolean); override;
    procedure SetDuplicates(Value: TDuplicates); override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure SetRemoveSingleElement(Value: Boolean); override;
    procedure SetReturnDefaultElements(Value: Boolean); override;
    procedure SetThreadSafe(Value: Boolean); override;
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
    { IJclCardinalSet }
    procedure Intersect(const ACollection: IJclCardinalCollection);
    procedure Subtract(const ACollection: IJclCardinalCollection);
    procedure Union(const ACollection: IJclCardinalCollection);
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AMap: IJclCardinalMap); overload;
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
  end;

  TJclInt64HashSet = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclInt64EqualityComparer,
    IJclInt64Collection, IJclInt64Set)
  private
    FMap: IJclInt64Map;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    function GetAutoPackParameter: Integer; override;
    function GetAutoPackStrategy: TJclAutoPackStrategy; override;
    function GetCapacity: Integer; override;
    procedure Pack; override;
    procedure SetAutoPackParameter(Value: Integer); override;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); override;
    procedure SetCapacity(Value: Integer); override;
    { IJclContainer }
    function GetAllowDefaultElements: Boolean; override;
    function GetDuplicates: TDuplicates; override;
    function GetReadOnly: Boolean; override;
    function GetRemoveSingleElement: Boolean; override;
    function GetReturnDefaultElements: Boolean; override;
    function GetThreadSafe: Boolean; override;
    procedure SetAllowDefaultElements(Value: Boolean); override;
    procedure SetDuplicates(Value: TDuplicates); override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure SetRemoveSingleElement(Value: Boolean); override;
    procedure SetReturnDefaultElements(Value: Boolean); override;
    procedure SetThreadSafe(Value: Boolean); override;
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
    { IJclInt64Set }
    procedure Intersect(const ACollection: IJclInt64Collection);
    procedure Subtract(const ACollection: IJclInt64Collection);
    procedure Union(const ACollection: IJclInt64Collection);
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AMap: IJclInt64Map); overload;
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
  end;

  {$IFNDEF CLR}
  TJclPtrHashSet = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclPtrEqualityComparer,
    IJclPtrCollection, IJclPtrSet)
  private
    FMap: IJclPtrMap;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    function GetAutoPackParameter: Integer; override;
    function GetAutoPackStrategy: TJclAutoPackStrategy; override;
    function GetCapacity: Integer; override;
    procedure Pack; override;
    procedure SetAutoPackParameter(Value: Integer); override;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); override;
    procedure SetCapacity(Value: Integer); override;
    { IJclContainer }
    function GetAllowDefaultElements: Boolean; override;
    function GetDuplicates: TDuplicates; override;
    function GetReadOnly: Boolean; override;
    function GetRemoveSingleElement: Boolean; override;
    function GetReturnDefaultElements: Boolean; override;
    function GetThreadSafe: Boolean; override;
    procedure SetAllowDefaultElements(Value: Boolean); override;
    procedure SetDuplicates(Value: TDuplicates); override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure SetRemoveSingleElement(Value: Boolean); override;
    procedure SetReturnDefaultElements(Value: Boolean); override;
    procedure SetThreadSafe(Value: Boolean); override;
    { IJclPtrCollection }
    function Add(AValue: Pointer): Boolean;
    function AddAll(const ACollection: IJclPtrCollection): Boolean;
    procedure Clear;
    function Contains(AValue: Pointer): Boolean;
    function ContainsAll(const ACollection: IJclPtrCollection): Boolean;
    function Equals(const ACollection: IJclPtrCollection): Boolean;
    function First: IJclPtrIterator;
    function IsEmpty: Boolean;
    function Last: IJclPtrIterator;
    function Remove(AValue: Pointer): Boolean;
    function RemoveAll(const ACollection: IJclPtrCollection): Boolean;
    function RetainAll(const ACollection: IJclPtrCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclPtrIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclPtrSet }
    procedure Intersect(const ACollection: IJclPtrCollection);
    procedure Subtract(const ACollection: IJclPtrCollection);
    procedure Union(const ACollection: IJclPtrCollection);
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AMap: IJclPtrMap); overload;
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
  end;
  {$ENDIF ~CLR}

  TJclHashSet = class(TJclAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclObjectOwner, IJclEqualityComparer,
    IJclCollection, IJclSet)
  private
    FMap: IJclMap;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    function GetAutoPackParameter: Integer; override;
    function GetAutoPackStrategy: TJclAutoPackStrategy; override;
    function GetCapacity: Integer; override;
    procedure Pack; override;
    procedure SetAutoPackParameter(Value: Integer); override;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); override;
    procedure SetCapacity(Value: Integer); override;
    { IJclContainer }
    function GetAllowDefaultElements: Boolean; override;
    function GetDuplicates: TDuplicates; override;
    function GetReadOnly: Boolean; override;
    function GetRemoveSingleElement: Boolean; override;
    function GetReturnDefaultElements: Boolean; override;
    function GetThreadSafe: Boolean; override;
    procedure SetAllowDefaultElements(Value: Boolean); override;
    procedure SetDuplicates(Value: TDuplicates); override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure SetRemoveSingleElement(Value: Boolean); override;
    procedure SetReturnDefaultElements(Value: Boolean); override;
    procedure SetThreadSafe(Value: Boolean); override;
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
    { IJclSet }
    procedure Intersect(const ACollection: IJclCollection);
    procedure Subtract(const ACollection: IJclCollection);
    procedure Union(const ACollection: IJclCollection);
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclObjectOwner }
    function FreeObject(var AObject: TObject): TObject; override;
    function GetOwnsObjects: Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AMap: IJclMap); overload;
    constructor Create(ACapacity: Integer; AOwnsObjects: Boolean); overload;
    destructor Destroy; override;
  end;

  {$IFDEF SUPPORTS_GENERICS}

  TJclHashSet<T> = class(TJclAbstractContainer<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclSet<T>)
  private
    FMap: IJclMap<T, TRefUnique>;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    function GetAutoPackParameter: Integer; override;
    function GetAutoPackStrategy: TJclAutoPackStrategy; override;
    function GetCapacity: Integer; override;
    procedure Pack; override;
    procedure SetAutoPackParameter(Value: Integer); override;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); override;
    procedure SetCapacity(Value: Integer); override;
    { IJclContainer }
    function GetAllowDefaultElements: Boolean; override;
    function GetDuplicates: TDuplicates; override;
    function GetReadOnly: Boolean; override;
    function GetRemoveSingleElement: Boolean; override;
    function GetReturnDefaultElements: Boolean; override;
    function GetThreadSafe: Boolean; override;
    procedure SetAllowDefaultElements(Value: Boolean); override;
    procedure SetDuplicates(Value: TDuplicates); override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure SetRemoveSingleElement(Value: Boolean); override;
    procedure SetReturnDefaultElements(Value: Boolean); override;
    procedure SetThreadSafe(Value: Boolean); override;
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
    { IJclSet<T> }
    procedure Intersect(const ACollection: IJclCollection<T>);
    procedure Subtract(const ACollection: IJclCollection<T>);
    procedure Union(const ACollection: IJclCollection<T>);
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclItemOwner<T> }
    function FreeItem(var AItem: T): T; override;
    function GetOwnsItems: Boolean; override;
  public
    constructor Create(const AMap: IJclMap<T, TRefUnique>); overload;
    destructor Destroy; override;
  end;

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
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclHashSets.pas $';
    Revision: '$Revision: 2393 $';
    Date: '$Date: 2008-07-20 11:13:23 +0200 (dim., 20 juil. 2008) $';
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

//=== { TJclIntfHashSet } =====================================================

constructor TJclIntfHashSet.Create(const AMap: IJclIntfMap);
begin
  inherited Create();
  FMap := AMap;
end;

constructor TJclIntfHashSet.Create(ACapacity: Integer);
begin
  Create(TJclIntfHashMap.Create(ACapacity, False));
end;

destructor TJclIntfHashSet.Destroy;
begin
  FMap := nil;
  inherited Destroy;
end;

function TJclIntfHashSet.Add(const AInterface: IInterface): Boolean;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := not FMap.ContainsKey(AInterface);
    if Result then
      FMap.PutValue(AInterface, RefUnique);
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.AddAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfHashSet then
    TJclIntfHashSet(Dest).FMap := (FMap as IJclIntfCloneable).Clone as IJclIntfMap;
end;

procedure TJclIntfHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclIntfHashSet.Contains(const AInterface: IInterface): Boolean;
begin
  Result := FMap.ContainsKey(AInterface);
end;

function TJclIntfHashSet.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfHashSet.Equals(const ACollection: IJclIntfCollection): Boolean;
var
  It, ItMap: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FMap.Size <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
      begin
        Result := False;
        Exit;
      end;
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.First: IJclIntfIterator;
begin
  Result := FMap.KeySet.First;
end;

function TJclIntfHashSet.GetAutoPackParameter: Integer;
begin
  Result := (FMap as IJclPackable).GetAutoPackParameter;
end;

function TJclIntfHashSet.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := (FMap as IJclPackable).GetAutoPackStrategy;
end;

function TJclIntfHashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

function TJclIntfHashSet.GetAllowDefaultElements: Boolean;
begin
  Result := FMap.AllowDefaultElements;
end;

function TJclIntfHashSet.GetDuplicates: TDuplicates;
begin
  Result := FMap.Duplicates;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntfHashSet.GetEnumerator: IJclIntfIterator;
begin
  Result := FMap.KeySet.First;
end;
{$ENDIF SUPPORTS_FOR_IN}


function TJclIntfHashSet.GetReadOnly: Boolean;
begin
  Result := FMap.ReadOnly;
end;

function TJclIntfHashSet.GetRemoveSingleElement: Boolean;
begin
  Result := FMap.RemoveSingleElement;
end;

function TJclIntfHashSet.GetReturnDefaultElements: Boolean;
begin
  Result := FMap.ReturnDefaultElements;
end;

function TJclIntfHashSet.GetThreadSafe: Boolean;
begin
  Result := FMap.ThreadSafe;
end;

procedure TJclIntfHashSet.Intersect(const ACollection: IJclIntfCollection);
begin
  RetainAll(ACollection);
end;

function TJclIntfHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclIntfHashSet.Last: IJclIntfIterator;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclIntfHashSet.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclIntfHashSet.Remove(const AInterface: IInterface): Boolean;
begin
  Result := FMap.Remove(AInterface) = RefUnique;
end;

function TJclIntfHashSet.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  ARefUnique: TRefUnique;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ARefUnique := RefUnique;
    It := ACollection.First;
    while It.HasNext do
      Result := (FMap.Remove(It.Next) = ARefUnique) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.RetainAll(const ACollection: IJclIntfCollection): Boolean;
var
  ItMap: IJclIntfIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ACollection.Contains(ItMap.Next) then
        ItMap.Remove;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashSet.SetAutoPackParameter(Value: Integer);
begin
  (FMap as IJclPackable).SetAutoPackParameter(Value);
end;

procedure TJclIntfHashSet.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  (FMap as IJclPackable).SetAutoPackStrategy(Value);
end;

procedure TJclIntfHashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

procedure TJclIntfHashSet.SetAllowDefaultElements(Value: Boolean);
begin
  FMap.AllowDefaultElements := Value;
end;

procedure TJclIntfHashSet.SetDuplicates(Value: TDuplicates);
begin
  FMap.Duplicates := Value;
end;

procedure TJclIntfHashSet.SetReadOnly(Value: Boolean);
begin
  FMap.ReadOnly := Value;
end;

procedure TJclIntfHashSet.SetRemoveSingleElement(Value: Boolean);
begin
  FMap.RemoveSingleElement := Value;
end;

procedure TJclIntfHashSet.SetReturnDefaultElements(Value: Boolean);
begin
  FMap.ReturnDefaultElements := Value;
end;

procedure TJclIntfHashSet.SetThreadSafe(Value: Boolean);
begin
  FMap.ThreadSafe := Value;
end;

function TJclIntfHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclIntfHashSet.Subtract(const ACollection: IJclIntfCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclIntfHashSet.Union(const ACollection: IJclIntfCollection);
begin
  AddAll(ACollection);
end;


//=== { TJclAnsiStrHashSet } =====================================================

constructor TJclAnsiStrHashSet.Create(const AMap: IJclAnsiStrMap);
begin
  inherited Create();
  FMap := AMap;
end;

constructor TJclAnsiStrHashSet.Create(ACapacity: Integer);
begin
  Create(TJclAnsiStrHashMap.Create(ACapacity, False));
end;

destructor TJclAnsiStrHashSet.Destroy;
begin
  FMap := nil;
  inherited Destroy;
end;

function TJclAnsiStrHashSet.Add(const AString: AnsiString): Boolean;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := not FMap.ContainsKey(AString);
    if Result then
      FMap.PutValue(AString, RefUnique);
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSet.AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrHashSet then
    TJclAnsiStrHashSet(Dest).FMap := (FMap as IJclIntfCloneable).Clone as IJclAnsiStrMap;
end;

procedure TJclAnsiStrHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclAnsiStrHashSet.Contains(const AString: AnsiString): Boolean;
begin
  Result := FMap.ContainsKey(AString);
end;

function TJclAnsiStrHashSet.ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrHashSet.Equals(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It, ItMap: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FMap.Size <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
      begin
        Result := False;
        Exit;
      end;
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSet.First: IJclAnsiStrIterator;
begin
  Result := FMap.KeySet.First;
end;

function TJclAnsiStrHashSet.GetAutoPackParameter: Integer;
begin
  Result := (FMap as IJclPackable).GetAutoPackParameter;
end;

function TJclAnsiStrHashSet.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := (FMap as IJclPackable).GetAutoPackStrategy;
end;

function TJclAnsiStrHashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

function TJclAnsiStrHashSet.GetAllowDefaultElements: Boolean;
begin
  Result := FMap.AllowDefaultElements;
end;

function TJclAnsiStrHashSet.GetDuplicates: TDuplicates;
begin
  Result := FMap.Duplicates;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclAnsiStrHashSet.GetEnumerator: IJclAnsiStrIterator;
begin
  Result := FMap.KeySet.First;
end;
{$ENDIF SUPPORTS_FOR_IN}


function TJclAnsiStrHashSet.GetReadOnly: Boolean;
begin
  Result := FMap.ReadOnly;
end;

function TJclAnsiStrHashSet.GetRemoveSingleElement: Boolean;
begin
  Result := FMap.RemoveSingleElement;
end;

function TJclAnsiStrHashSet.GetReturnDefaultElements: Boolean;
begin
  Result := FMap.ReturnDefaultElements;
end;

function TJclAnsiStrHashSet.GetThreadSafe: Boolean;
begin
  Result := FMap.ThreadSafe;
end;

function TJclAnsiStrHashSet.GetCaseSensitive: Boolean;
begin
  Result := FMap.GetCaseSensitive;
end;

function TJclAnsiStrHashSet.GetEncoding: TJclAnsiStrEncoding;
begin
  Result := FMap.GetEncoding;
end;

procedure TJclAnsiStrHashSet.Intersect(const ACollection: IJclAnsiStrCollection);
begin
  RetainAll(ACollection);
end;

function TJclAnsiStrHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclAnsiStrHashSet.Last: IJclAnsiStrIterator;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclAnsiStrHashSet.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclAnsiStrHashSet.Remove(const AString: AnsiString): Boolean;
begin
  Result := FMap.Remove(AString) = RefUnique;
end;

function TJclAnsiStrHashSet.RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
  ARefUnique: TRefUnique;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ARefUnique := RefUnique;
    It := ACollection.First;
    while It.HasNext do
      Result := (FMap.Remove(It.Next) = ARefUnique) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSet.RetainAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  ItMap: IJclAnsiStrIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ACollection.Contains(ItMap.Next) then
        ItMap.Remove;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashSet.SetAutoPackParameter(Value: Integer);
begin
  (FMap as IJclPackable).SetAutoPackParameter(Value);
end;

procedure TJclAnsiStrHashSet.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  (FMap as IJclPackable).SetAutoPackStrategy(Value);
end;

procedure TJclAnsiStrHashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

procedure TJclAnsiStrHashSet.SetAllowDefaultElements(Value: Boolean);
begin
  FMap.AllowDefaultElements := Value;
end;

procedure TJclAnsiStrHashSet.SetDuplicates(Value: TDuplicates);
begin
  FMap.Duplicates := Value;
end;

procedure TJclAnsiStrHashSet.SetReadOnly(Value: Boolean);
begin
  FMap.ReadOnly := Value;
end;

procedure TJclAnsiStrHashSet.SetRemoveSingleElement(Value: Boolean);
begin
  FMap.RemoveSingleElement := Value;
end;

procedure TJclAnsiStrHashSet.SetReturnDefaultElements(Value: Boolean);
begin
  FMap.ReturnDefaultElements := Value;
end;

procedure TJclAnsiStrHashSet.SetThreadSafe(Value: Boolean);
begin
  FMap.ThreadSafe := Value;
end;

procedure TJclAnsiStrHashSet.SetCaseSensitive(Value: Boolean);
begin
  FMap.SetCaseSensitive(Value);
end;

procedure TJclAnsiStrHashSet.SetEncoding(Value: TJclAnsiStrEncoding);
begin
  FMap.SetEncoding(Value);
end;

function TJclAnsiStrHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclAnsiStrHashSet.Subtract(const ACollection: IJclAnsiStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclAnsiStrHashSet.Union(const ACollection: IJclAnsiStrCollection);
begin
  AddAll(ACollection);
end;


//=== { TJclWideStrHashSet } =====================================================

constructor TJclWideStrHashSet.Create(const AMap: IJclWideStrMap);
begin
  inherited Create();
  FMap := AMap;
end;

constructor TJclWideStrHashSet.Create(ACapacity: Integer);
begin
  Create(TJclWideStrHashMap.Create(ACapacity, False));
end;

destructor TJclWideStrHashSet.Destroy;
begin
  FMap := nil;
  inherited Destroy;
end;

function TJclWideStrHashSet.Add(const AString: WideString): Boolean;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := not FMap.ContainsKey(AString);
    if Result then
      FMap.PutValue(AString, RefUnique);
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSet.AddAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrHashSet then
    TJclWideStrHashSet(Dest).FMap := (FMap as IJclIntfCloneable).Clone as IJclWideStrMap;
end;

procedure TJclWideStrHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclWideStrHashSet.Contains(const AString: WideString): Boolean;
begin
  Result := FMap.ContainsKey(AString);
end;

function TJclWideStrHashSet.ContainsAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

function TJclWideStrHashSet.Equals(const ACollection: IJclWideStrCollection): Boolean;
var
  It, ItMap: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FMap.Size <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
      begin
        Result := False;
        Exit;
      end;
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSet.First: IJclWideStrIterator;
begin
  Result := FMap.KeySet.First;
end;


function TJclWideStrHashSet.GetAutoPackParameter: Integer;
begin
  Result := (FMap as IJclPackable).GetAutoPackParameter;
end;

function TJclWideStrHashSet.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := (FMap as IJclPackable).GetAutoPackStrategy;
end;

function TJclWideStrHashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

function TJclWideStrHashSet.GetAllowDefaultElements: Boolean;
begin
  Result := FMap.AllowDefaultElements;
end;

function TJclWideStrHashSet.GetDuplicates: TDuplicates;
begin
  Result := FMap.Duplicates;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclWideStrHashSet.GetEnumerator: IJclWideStrIterator;
begin
  Result := FMap.KeySet.First;
end;
{$ENDIF SUPPORTS_FOR_IN}



function TJclWideStrHashSet.GetReadOnly: Boolean;
begin
  Result := FMap.ReadOnly;
end;

function TJclWideStrHashSet.GetRemoveSingleElement: Boolean;
begin
  Result := FMap.RemoveSingleElement;
end;

function TJclWideStrHashSet.GetReturnDefaultElements: Boolean;
begin
  Result := FMap.ReturnDefaultElements;
end;

function TJclWideStrHashSet.GetThreadSafe: Boolean;
begin
  Result := FMap.ThreadSafe;
end;

function TJclWideStrHashSet.GetCaseSensitive: Boolean;
begin
  Result := FMap.GetCaseSensitive;
end;

function TJclWideStrHashSet.GetEncoding: TJclWideStrEncoding;
begin
  Result := FMap.GetEncoding;
end;

procedure TJclWideStrHashSet.Intersect(const ACollection: IJclWideStrCollection);
begin
  RetainAll(ACollection);
end;

function TJclWideStrHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclWideStrHashSet.Last: IJclWideStrIterator;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclWideStrHashSet.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclWideStrHashSet.Remove(const AString: WideString): Boolean;
begin
  Result := FMap.Remove(AString) = RefUnique;
end;

function TJclWideStrHashSet.RemoveAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
  ARefUnique: TRefUnique;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ARefUnique := RefUnique;
    It := ACollection.First;
    while It.HasNext do
      Result := (FMap.Remove(It.Next) = ARefUnique) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSet.RetainAll(const ACollection: IJclWideStrCollection): Boolean;
var
  ItMap: IJclWideStrIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ACollection.Contains(ItMap.Next) then
        ItMap.Remove;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashSet.SetAutoPackParameter(Value: Integer);
begin
  (FMap as IJclPackable).SetAutoPackParameter(Value);
end;

procedure TJclWideStrHashSet.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  (FMap as IJclPackable).SetAutoPackStrategy(Value);
end;

procedure TJclWideStrHashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

procedure TJclWideStrHashSet.SetAllowDefaultElements(Value: Boolean);
begin
  FMap.AllowDefaultElements := Value;
end;

procedure TJclWideStrHashSet.SetDuplicates(Value: TDuplicates);
begin
  FMap.Duplicates := Value;
end;

procedure TJclWideStrHashSet.SetReadOnly(Value: Boolean);
begin
  FMap.ReadOnly := Value;
end;

procedure TJclWideStrHashSet.SetRemoveSingleElement(Value: Boolean);
begin
  FMap.RemoveSingleElement := Value;
end;

procedure TJclWideStrHashSet.SetReturnDefaultElements(Value: Boolean);
begin
  FMap.ReturnDefaultElements := Value;
end;

procedure TJclWideStrHashSet.SetThreadSafe(Value: Boolean);
begin
  FMap.ThreadSafe := Value;
end;

procedure TJclWideStrHashSet.SetCaseSensitive(Value: Boolean);
begin
  FMap.SetCaseSensitive(Value);
end;

procedure TJclWideStrHashSet.SetEncoding(Value: TJclWideStrEncoding);
begin
  FMap.SetEncoding(Value);
end;

function TJclWideStrHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclWideStrHashSet.Subtract(const ACollection: IJclWideStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclWideStrHashSet.Union(const ACollection: IJclWideStrCollection);
begin
  AddAll(ACollection);
end;


//=== { TJclSingleHashSet } =====================================================

constructor TJclSingleHashSet.Create(const AMap: IJclSingleMap);
begin
  inherited Create();
  FMap := AMap;
end;

constructor TJclSingleHashSet.Create(ACapacity: Integer);
begin
  Create(TJclSingleHashMap.Create(ACapacity, False));
end;

destructor TJclSingleHashSet.Destroy;
begin
  FMap := nil;
  inherited Destroy;
end;

function TJclSingleHashSet.Add(const AValue: Single): Boolean;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := not FMap.ContainsKey(AValue);
    if Result then
      FMap.PutValue(AValue, RefUnique);
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSet.AddAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclSingleHashSet then
    TJclSingleHashSet(Dest).FMap := (FMap as IJclIntfCloneable).Clone as IJclSingleMap;
end;

procedure TJclSingleHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclSingleHashSet.Contains(const AValue: Single): Boolean;
begin
  Result := FMap.ContainsKey(AValue);
end;

function TJclSingleHashSet.ContainsAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

function TJclSingleHashSet.Equals(const ACollection: IJclSingleCollection): Boolean;
var
  It, ItMap: IJclSingleIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FMap.Size <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
      begin
        Result := False;
        Exit;
      end;
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSet.First: IJclSingleIterator;
begin
  Result := FMap.KeySet.First;
end;


function TJclSingleHashSet.GetAutoPackParameter: Integer;
begin
  Result := (FMap as IJclPackable).GetAutoPackParameter;
end;

function TJclSingleHashSet.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := (FMap as IJclPackable).GetAutoPackStrategy;
end;

function TJclSingleHashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

function TJclSingleHashSet.GetAllowDefaultElements: Boolean;
begin
  Result := FMap.AllowDefaultElements;
end;

function TJclSingleHashSet.GetDuplicates: TDuplicates;
begin
  Result := FMap.Duplicates;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclSingleHashSet.GetEnumerator: IJclSingleIterator;
begin
  Result := FMap.KeySet.First;
end;
{$ENDIF SUPPORTS_FOR_IN}



function TJclSingleHashSet.GetReadOnly: Boolean;
begin
  Result := FMap.ReadOnly;
end;

function TJclSingleHashSet.GetRemoveSingleElement: Boolean;
begin
  Result := FMap.RemoveSingleElement;
end;

function TJclSingleHashSet.GetReturnDefaultElements: Boolean;
begin
  Result := FMap.ReturnDefaultElements;
end;

function TJclSingleHashSet.GetThreadSafe: Boolean;
begin
  Result := FMap.ThreadSafe;
end;

function TJclSingleHashSet.GetPrecision: Single;
begin
  Result := FMap.GetPrecision;
end;

procedure TJclSingleHashSet.Intersect(const ACollection: IJclSingleCollection);
begin
  RetainAll(ACollection);
end;

function TJclSingleHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclSingleHashSet.Last: IJclSingleIterator;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclSingleHashSet.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclSingleHashSet.Remove(const AValue: Single): Boolean;
begin
  Result := FMap.Remove(AValue) = RefUnique;
end;

function TJclSingleHashSet.RemoveAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
  ARefUnique: TRefUnique;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ARefUnique := RefUnique;
    It := ACollection.First;
    while It.HasNext do
      Result := (FMap.Remove(It.Next) = ARefUnique) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashSet.RetainAll(const ACollection: IJclSingleCollection): Boolean;
var
  ItMap: IJclSingleIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ACollection.Contains(ItMap.Next) then
        ItMap.Remove;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashSet.SetAutoPackParameter(Value: Integer);
begin
  (FMap as IJclPackable).SetAutoPackParameter(Value);
end;

procedure TJclSingleHashSet.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  (FMap as IJclPackable).SetAutoPackStrategy(Value);
end;

procedure TJclSingleHashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

procedure TJclSingleHashSet.SetAllowDefaultElements(Value: Boolean);
begin
  FMap.AllowDefaultElements := Value;
end;

procedure TJclSingleHashSet.SetDuplicates(Value: TDuplicates);
begin
  FMap.Duplicates := Value;
end;

procedure TJclSingleHashSet.SetReadOnly(Value: Boolean);
begin
  FMap.ReadOnly := Value;
end;

procedure TJclSingleHashSet.SetRemoveSingleElement(Value: Boolean);
begin
  FMap.RemoveSingleElement := Value;
end;

procedure TJclSingleHashSet.SetReturnDefaultElements(Value: Boolean);
begin
  FMap.ReturnDefaultElements := Value;
end;

procedure TJclSingleHashSet.SetThreadSafe(Value: Boolean);
begin
  FMap.ThreadSafe := Value;
end;

procedure TJclSingleHashSet.SetPrecision(const Value: Single);
begin
  FMap.SetPrecision(Value);
end;

function TJclSingleHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclSingleHashSet.Subtract(const ACollection: IJclSingleCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclSingleHashSet.Union(const ACollection: IJclSingleCollection);
begin
  AddAll(ACollection);
end;


//=== { TJclDoubleHashSet } =====================================================

constructor TJclDoubleHashSet.Create(const AMap: IJclDoubleMap);
begin
  inherited Create();
  FMap := AMap;
end;

constructor TJclDoubleHashSet.Create(ACapacity: Integer);
begin
  Create(TJclDoubleHashMap.Create(ACapacity, False));
end;

destructor TJclDoubleHashSet.Destroy;
begin
  FMap := nil;
  inherited Destroy;
end;

function TJclDoubleHashSet.Add(const AValue: Double): Boolean;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := not FMap.ContainsKey(AValue);
    if Result then
      FMap.PutValue(AValue, RefUnique);
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSet.AddAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclDoubleHashSet then
    TJclDoubleHashSet(Dest).FMap := (FMap as IJclIntfCloneable).Clone as IJclDoubleMap;
end;

procedure TJclDoubleHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclDoubleHashSet.Contains(const AValue: Double): Boolean;
begin
  Result := FMap.ContainsKey(AValue);
end;

function TJclDoubleHashSet.ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

function TJclDoubleHashSet.Equals(const ACollection: IJclDoubleCollection): Boolean;
var
  It, ItMap: IJclDoubleIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FMap.Size <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
      begin
        Result := False;
        Exit;
      end;
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSet.First: IJclDoubleIterator;
begin
  Result := FMap.KeySet.First;
end;


function TJclDoubleHashSet.GetAutoPackParameter: Integer;
begin
  Result := (FMap as IJclPackable).GetAutoPackParameter;
end;

function TJclDoubleHashSet.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := (FMap as IJclPackable).GetAutoPackStrategy;
end;

function TJclDoubleHashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

function TJclDoubleHashSet.GetAllowDefaultElements: Boolean;
begin
  Result := FMap.AllowDefaultElements;
end;

function TJclDoubleHashSet.GetDuplicates: TDuplicates;
begin
  Result := FMap.Duplicates;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclDoubleHashSet.GetEnumerator: IJclDoubleIterator;
begin
  Result := FMap.KeySet.First;
end;
{$ENDIF SUPPORTS_FOR_IN}



function TJclDoubleHashSet.GetReadOnly: Boolean;
begin
  Result := FMap.ReadOnly;
end;

function TJclDoubleHashSet.GetRemoveSingleElement: Boolean;
begin
  Result := FMap.RemoveSingleElement;
end;

function TJclDoubleHashSet.GetReturnDefaultElements: Boolean;
begin
  Result := FMap.ReturnDefaultElements;
end;

function TJclDoubleHashSet.GetThreadSafe: Boolean;
begin
  Result := FMap.ThreadSafe;
end;

function TJclDoubleHashSet.GetPrecision: Double;
begin
  Result := FMap.GetPrecision;
end;

procedure TJclDoubleHashSet.Intersect(const ACollection: IJclDoubleCollection);
begin
  RetainAll(ACollection);
end;

function TJclDoubleHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclDoubleHashSet.Last: IJclDoubleIterator;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclDoubleHashSet.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclDoubleHashSet.Remove(const AValue: Double): Boolean;
begin
  Result := FMap.Remove(AValue) = RefUnique;
end;

function TJclDoubleHashSet.RemoveAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
  ARefUnique: TRefUnique;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ARefUnique := RefUnique;
    It := ACollection.First;
    while It.HasNext do
      Result := (FMap.Remove(It.Next) = ARefUnique) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashSet.RetainAll(const ACollection: IJclDoubleCollection): Boolean;
var
  ItMap: IJclDoubleIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ACollection.Contains(ItMap.Next) then
        ItMap.Remove;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashSet.SetAutoPackParameter(Value: Integer);
begin
  (FMap as IJclPackable).SetAutoPackParameter(Value);
end;

procedure TJclDoubleHashSet.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  (FMap as IJclPackable).SetAutoPackStrategy(Value);
end;

procedure TJclDoubleHashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

procedure TJclDoubleHashSet.SetAllowDefaultElements(Value: Boolean);
begin
  FMap.AllowDefaultElements := Value;
end;

procedure TJclDoubleHashSet.SetDuplicates(Value: TDuplicates);
begin
  FMap.Duplicates := Value;
end;

procedure TJclDoubleHashSet.SetReadOnly(Value: Boolean);
begin
  FMap.ReadOnly := Value;
end;

procedure TJclDoubleHashSet.SetRemoveSingleElement(Value: Boolean);
begin
  FMap.RemoveSingleElement := Value;
end;

procedure TJclDoubleHashSet.SetReturnDefaultElements(Value: Boolean);
begin
  FMap.ReturnDefaultElements := Value;
end;

procedure TJclDoubleHashSet.SetThreadSafe(Value: Boolean);
begin
  FMap.ThreadSafe := Value;
end;

procedure TJclDoubleHashSet.SetPrecision(const Value: Double);
begin
  FMap.SetPrecision(Value);
end;

function TJclDoubleHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclDoubleHashSet.Subtract(const ACollection: IJclDoubleCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclDoubleHashSet.Union(const ACollection: IJclDoubleCollection);
begin
  AddAll(ACollection);
end;


//=== { TJclExtendedHashSet } =====================================================

constructor TJclExtendedHashSet.Create(const AMap: IJclExtendedMap);
begin
  inherited Create();
  FMap := AMap;
end;

constructor TJclExtendedHashSet.Create(ACapacity: Integer);
begin
  Create(TJclExtendedHashMap.Create(ACapacity, False));
end;

destructor TJclExtendedHashSet.Destroy;
begin
  FMap := nil;
  inherited Destroy;
end;

function TJclExtendedHashSet.Add(const AValue: Extended): Boolean;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := not FMap.ContainsKey(AValue);
    if Result then
      FMap.PutValue(AValue, RefUnique);
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSet.AddAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclExtendedHashSet then
    TJclExtendedHashSet(Dest).FMap := (FMap as IJclIntfCloneable).Clone as IJclExtendedMap;
end;

procedure TJclExtendedHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclExtendedHashSet.Contains(const AValue: Extended): Boolean;
begin
  Result := FMap.ContainsKey(AValue);
end;

function TJclExtendedHashSet.ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

function TJclExtendedHashSet.Equals(const ACollection: IJclExtendedCollection): Boolean;
var
  It, ItMap: IJclExtendedIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FMap.Size <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
      begin
        Result := False;
        Exit;
      end;
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSet.First: IJclExtendedIterator;
begin
  Result := FMap.KeySet.First;
end;


function TJclExtendedHashSet.GetAutoPackParameter: Integer;
begin
  Result := (FMap as IJclPackable).GetAutoPackParameter;
end;

function TJclExtendedHashSet.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := (FMap as IJclPackable).GetAutoPackStrategy;
end;

function TJclExtendedHashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

function TJclExtendedHashSet.GetAllowDefaultElements: Boolean;
begin
  Result := FMap.AllowDefaultElements;
end;

function TJclExtendedHashSet.GetDuplicates: TDuplicates;
begin
  Result := FMap.Duplicates;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclExtendedHashSet.GetEnumerator: IJclExtendedIterator;
begin
  Result := FMap.KeySet.First;
end;
{$ENDIF SUPPORTS_FOR_IN}



function TJclExtendedHashSet.GetReadOnly: Boolean;
begin
  Result := FMap.ReadOnly;
end;

function TJclExtendedHashSet.GetRemoveSingleElement: Boolean;
begin
  Result := FMap.RemoveSingleElement;
end;

function TJclExtendedHashSet.GetReturnDefaultElements: Boolean;
begin
  Result := FMap.ReturnDefaultElements;
end;

function TJclExtendedHashSet.GetThreadSafe: Boolean;
begin
  Result := FMap.ThreadSafe;
end;

function TJclExtendedHashSet.GetPrecision: Extended;
begin
  Result := FMap.GetPrecision;
end;

procedure TJclExtendedHashSet.Intersect(const ACollection: IJclExtendedCollection);
begin
  RetainAll(ACollection);
end;

function TJclExtendedHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclExtendedHashSet.Last: IJclExtendedIterator;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclExtendedHashSet.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclExtendedHashSet.Remove(const AValue: Extended): Boolean;
begin
  Result := FMap.Remove(AValue) = RefUnique;
end;

function TJclExtendedHashSet.RemoveAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
  ARefUnique: TRefUnique;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ARefUnique := RefUnique;
    It := ACollection.First;
    while It.HasNext do
      Result := (FMap.Remove(It.Next) = ARefUnique) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashSet.RetainAll(const ACollection: IJclExtendedCollection): Boolean;
var
  ItMap: IJclExtendedIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ACollection.Contains(ItMap.Next) then
        ItMap.Remove;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashSet.SetAutoPackParameter(Value: Integer);
begin
  (FMap as IJclPackable).SetAutoPackParameter(Value);
end;

procedure TJclExtendedHashSet.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  (FMap as IJclPackable).SetAutoPackStrategy(Value);
end;

procedure TJclExtendedHashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

procedure TJclExtendedHashSet.SetAllowDefaultElements(Value: Boolean);
begin
  FMap.AllowDefaultElements := Value;
end;

procedure TJclExtendedHashSet.SetDuplicates(Value: TDuplicates);
begin
  FMap.Duplicates := Value;
end;

procedure TJclExtendedHashSet.SetReadOnly(Value: Boolean);
begin
  FMap.ReadOnly := Value;
end;

procedure TJclExtendedHashSet.SetRemoveSingleElement(Value: Boolean);
begin
  FMap.RemoveSingleElement := Value;
end;

procedure TJclExtendedHashSet.SetReturnDefaultElements(Value: Boolean);
begin
  FMap.ReturnDefaultElements := Value;
end;

procedure TJclExtendedHashSet.SetThreadSafe(Value: Boolean);
begin
  FMap.ThreadSafe := Value;
end;

procedure TJclExtendedHashSet.SetPrecision(const Value: Extended);
begin
  FMap.SetPrecision(Value);
end;

function TJclExtendedHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclExtendedHashSet.Subtract(const ACollection: IJclExtendedCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclExtendedHashSet.Union(const ACollection: IJclExtendedCollection);
begin
  AddAll(ACollection);
end;


//=== { TJclIntegerHashSet } =====================================================

constructor TJclIntegerHashSet.Create(const AMap: IJclIntegerMap);
begin
  inherited Create();
  FMap := AMap;
end;

constructor TJclIntegerHashSet.Create(ACapacity: Integer);
begin
  Create(TJclIntegerHashMap.Create(ACapacity, False));
end;

destructor TJclIntegerHashSet.Destroy;
begin
  FMap := nil;
  inherited Destroy;
end;

function TJclIntegerHashSet.Add(AValue: Integer): Boolean;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := not FMap.ContainsKey(AValue);
    if Result then
      FMap.PutValue(AValue, RefUnique);
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSet.AddAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntegerHashSet then
    TJclIntegerHashSet(Dest).FMap := (FMap as IJclIntfCloneable).Clone as IJclIntegerMap;
end;

procedure TJclIntegerHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclIntegerHashSet.Contains(AValue: Integer): Boolean;
begin
  Result := FMap.ContainsKey(AValue);
end;

function TJclIntegerHashSet.ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntegerHashSet.Equals(const ACollection: IJclIntegerCollection): Boolean;
var
  It, ItMap: IJclIntegerIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FMap.Size <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
      begin
        Result := False;
        Exit;
      end;
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSet.First: IJclIntegerIterator;
begin
  Result := FMap.KeySet.First;
end;


function TJclIntegerHashSet.GetAutoPackParameter: Integer;
begin
  Result := (FMap as IJclPackable).GetAutoPackParameter;
end;

function TJclIntegerHashSet.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := (FMap as IJclPackable).GetAutoPackStrategy;
end;

function TJclIntegerHashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

function TJclIntegerHashSet.GetAllowDefaultElements: Boolean;
begin
  Result := FMap.AllowDefaultElements;
end;

function TJclIntegerHashSet.GetDuplicates: TDuplicates;
begin
  Result := FMap.Duplicates;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntegerHashSet.GetEnumerator: IJclIntegerIterator;
begin
  Result := FMap.KeySet.First;
end;
{$ENDIF SUPPORTS_FOR_IN}



function TJclIntegerHashSet.GetReadOnly: Boolean;
begin
  Result := FMap.ReadOnly;
end;

function TJclIntegerHashSet.GetRemoveSingleElement: Boolean;
begin
  Result := FMap.RemoveSingleElement;
end;

function TJclIntegerHashSet.GetReturnDefaultElements: Boolean;
begin
  Result := FMap.ReturnDefaultElements;
end;

function TJclIntegerHashSet.GetThreadSafe: Boolean;
begin
  Result := FMap.ThreadSafe;
end;


procedure TJclIntegerHashSet.Intersect(const ACollection: IJclIntegerCollection);
begin
  RetainAll(ACollection);
end;

function TJclIntegerHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclIntegerHashSet.Last: IJclIntegerIterator;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclIntegerHashSet.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclIntegerHashSet.Remove(AValue: Integer): Boolean;
begin
  Result := FMap.Remove(AValue) = RefUnique;
end;

function TJclIntegerHashSet.RemoveAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
  ARefUnique: TRefUnique;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ARefUnique := RefUnique;
    It := ACollection.First;
    while It.HasNext do
      Result := (FMap.Remove(It.Next) = ARefUnique) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashSet.RetainAll(const ACollection: IJclIntegerCollection): Boolean;
var
  ItMap: IJclIntegerIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ACollection.Contains(ItMap.Next) then
        ItMap.Remove;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashSet.SetAutoPackParameter(Value: Integer);
begin
  (FMap as IJclPackable).SetAutoPackParameter(Value);
end;

procedure TJclIntegerHashSet.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  (FMap as IJclPackable).SetAutoPackStrategy(Value);
end;

procedure TJclIntegerHashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

procedure TJclIntegerHashSet.SetAllowDefaultElements(Value: Boolean);
begin
  FMap.AllowDefaultElements := Value;
end;

procedure TJclIntegerHashSet.SetDuplicates(Value: TDuplicates);
begin
  FMap.Duplicates := Value;
end;

procedure TJclIntegerHashSet.SetReadOnly(Value: Boolean);
begin
  FMap.ReadOnly := Value;
end;

procedure TJclIntegerHashSet.SetRemoveSingleElement(Value: Boolean);
begin
  FMap.RemoveSingleElement := Value;
end;

procedure TJclIntegerHashSet.SetReturnDefaultElements(Value: Boolean);
begin
  FMap.ReturnDefaultElements := Value;
end;

procedure TJclIntegerHashSet.SetThreadSafe(Value: Boolean);
begin
  FMap.ThreadSafe := Value;
end;


function TJclIntegerHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclIntegerHashSet.Subtract(const ACollection: IJclIntegerCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclIntegerHashSet.Union(const ACollection: IJclIntegerCollection);
begin
  AddAll(ACollection);
end;


//=== { TJclCardinalHashSet } =====================================================

constructor TJclCardinalHashSet.Create(const AMap: IJclCardinalMap);
begin
  inherited Create();
  FMap := AMap;
end;

constructor TJclCardinalHashSet.Create(ACapacity: Integer);
begin
  Create(TJclCardinalHashMap.Create(ACapacity, False));
end;

destructor TJclCardinalHashSet.Destroy;
begin
  FMap := nil;
  inherited Destroy;
end;

function TJclCardinalHashSet.Add(AValue: Cardinal): Boolean;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := not FMap.ContainsKey(AValue);
    if Result then
      FMap.PutValue(AValue, RefUnique);
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSet.AddAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclCardinalHashSet then
    TJclCardinalHashSet(Dest).FMap := (FMap as IJclIntfCloneable).Clone as IJclCardinalMap;
end;

procedure TJclCardinalHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclCardinalHashSet.Contains(AValue: Cardinal): Boolean;
begin
  Result := FMap.ContainsKey(AValue);
end;

function TJclCardinalHashSet.ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

function TJclCardinalHashSet.Equals(const ACollection: IJclCardinalCollection): Boolean;
var
  It, ItMap: IJclCardinalIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FMap.Size <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
      begin
        Result := False;
        Exit;
      end;
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSet.First: IJclCardinalIterator;
begin
  Result := FMap.KeySet.First;
end;


function TJclCardinalHashSet.GetAutoPackParameter: Integer;
begin
  Result := (FMap as IJclPackable).GetAutoPackParameter;
end;

function TJclCardinalHashSet.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := (FMap as IJclPackable).GetAutoPackStrategy;
end;

function TJclCardinalHashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

function TJclCardinalHashSet.GetAllowDefaultElements: Boolean;
begin
  Result := FMap.AllowDefaultElements;
end;

function TJclCardinalHashSet.GetDuplicates: TDuplicates;
begin
  Result := FMap.Duplicates;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclCardinalHashSet.GetEnumerator: IJclCardinalIterator;
begin
  Result := FMap.KeySet.First;
end;
{$ENDIF SUPPORTS_FOR_IN}



function TJclCardinalHashSet.GetReadOnly: Boolean;
begin
  Result := FMap.ReadOnly;
end;

function TJclCardinalHashSet.GetRemoveSingleElement: Boolean;
begin
  Result := FMap.RemoveSingleElement;
end;

function TJclCardinalHashSet.GetReturnDefaultElements: Boolean;
begin
  Result := FMap.ReturnDefaultElements;
end;

function TJclCardinalHashSet.GetThreadSafe: Boolean;
begin
  Result := FMap.ThreadSafe;
end;


procedure TJclCardinalHashSet.Intersect(const ACollection: IJclCardinalCollection);
begin
  RetainAll(ACollection);
end;

function TJclCardinalHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclCardinalHashSet.Last: IJclCardinalIterator;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclCardinalHashSet.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclCardinalHashSet.Remove(AValue: Cardinal): Boolean;
begin
  Result := FMap.Remove(AValue) = RefUnique;
end;

function TJclCardinalHashSet.RemoveAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
  ARefUnique: TRefUnique;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ARefUnique := RefUnique;
    It := ACollection.First;
    while It.HasNext do
      Result := (FMap.Remove(It.Next) = ARefUnique) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashSet.RetainAll(const ACollection: IJclCardinalCollection): Boolean;
var
  ItMap: IJclCardinalIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ACollection.Contains(ItMap.Next) then
        ItMap.Remove;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashSet.SetAutoPackParameter(Value: Integer);
begin
  (FMap as IJclPackable).SetAutoPackParameter(Value);
end;

procedure TJclCardinalHashSet.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  (FMap as IJclPackable).SetAutoPackStrategy(Value);
end;

procedure TJclCardinalHashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

procedure TJclCardinalHashSet.SetAllowDefaultElements(Value: Boolean);
begin
  FMap.AllowDefaultElements := Value;
end;

procedure TJclCardinalHashSet.SetDuplicates(Value: TDuplicates);
begin
  FMap.Duplicates := Value;
end;

procedure TJclCardinalHashSet.SetReadOnly(Value: Boolean);
begin
  FMap.ReadOnly := Value;
end;

procedure TJclCardinalHashSet.SetRemoveSingleElement(Value: Boolean);
begin
  FMap.RemoveSingleElement := Value;
end;

procedure TJclCardinalHashSet.SetReturnDefaultElements(Value: Boolean);
begin
  FMap.ReturnDefaultElements := Value;
end;

procedure TJclCardinalHashSet.SetThreadSafe(Value: Boolean);
begin
  FMap.ThreadSafe := Value;
end;


function TJclCardinalHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclCardinalHashSet.Subtract(const ACollection: IJclCardinalCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclCardinalHashSet.Union(const ACollection: IJclCardinalCollection);
begin
  AddAll(ACollection);
end;


//=== { TJclInt64HashSet } =====================================================

constructor TJclInt64HashSet.Create(const AMap: IJclInt64Map);
begin
  inherited Create();
  FMap := AMap;
end;

constructor TJclInt64HashSet.Create(ACapacity: Integer);
begin
  Create(TJclInt64HashMap.Create(ACapacity, False));
end;

destructor TJclInt64HashSet.Destroy;
begin
  FMap := nil;
  inherited Destroy;
end;

function TJclInt64HashSet.Add(const AValue: Int64): Boolean;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := not FMap.ContainsKey(AValue);
    if Result then
      FMap.PutValue(AValue, RefUnique);
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSet.AddAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclInt64HashSet then
    TJclInt64HashSet(Dest).FMap := (FMap as IJclIntfCloneable).Clone as IJclInt64Map;
end;

procedure TJclInt64HashSet.Clear;
begin
  FMap.Clear;
end;

function TJclInt64HashSet.Contains(const AValue: Int64): Boolean;
begin
  Result := FMap.ContainsKey(AValue);
end;

function TJclInt64HashSet.ContainsAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64HashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

function TJclInt64HashSet.Equals(const ACollection: IJclInt64Collection): Boolean;
var
  It, ItMap: IJclInt64Iterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FMap.Size <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
      begin
        Result := False;
        Exit;
      end;
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSet.First: IJclInt64Iterator;
begin
  Result := FMap.KeySet.First;
end;


function TJclInt64HashSet.GetAutoPackParameter: Integer;
begin
  Result := (FMap as IJclPackable).GetAutoPackParameter;
end;

function TJclInt64HashSet.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := (FMap as IJclPackable).GetAutoPackStrategy;
end;

function TJclInt64HashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

function TJclInt64HashSet.GetAllowDefaultElements: Boolean;
begin
  Result := FMap.AllowDefaultElements;
end;

function TJclInt64HashSet.GetDuplicates: TDuplicates;
begin
  Result := FMap.Duplicates;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclInt64HashSet.GetEnumerator: IJclInt64Iterator;
begin
  Result := FMap.KeySet.First;
end;
{$ENDIF SUPPORTS_FOR_IN}



function TJclInt64HashSet.GetReadOnly: Boolean;
begin
  Result := FMap.ReadOnly;
end;

function TJclInt64HashSet.GetRemoveSingleElement: Boolean;
begin
  Result := FMap.RemoveSingleElement;
end;

function TJclInt64HashSet.GetReturnDefaultElements: Boolean;
begin
  Result := FMap.ReturnDefaultElements;
end;

function TJclInt64HashSet.GetThreadSafe: Boolean;
begin
  Result := FMap.ThreadSafe;
end;


procedure TJclInt64HashSet.Intersect(const ACollection: IJclInt64Collection);
begin
  RetainAll(ACollection);
end;

function TJclInt64HashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclInt64HashSet.Last: IJclInt64Iterator;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclInt64HashSet.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclInt64HashSet.Remove(const AValue: Int64): Boolean;
begin
  Result := FMap.Remove(AValue) = RefUnique;
end;

function TJclInt64HashSet.RemoveAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
  ARefUnique: TRefUnique;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ARefUnique := RefUnique;
    It := ACollection.First;
    while It.HasNext do
      Result := (FMap.Remove(It.Next) = ARefUnique) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashSet.RetainAll(const ACollection: IJclInt64Collection): Boolean;
var
  ItMap: IJclInt64Iterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ACollection.Contains(ItMap.Next) then
        ItMap.Remove;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashSet.SetAutoPackParameter(Value: Integer);
begin
  (FMap as IJclPackable).SetAutoPackParameter(Value);
end;

procedure TJclInt64HashSet.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  (FMap as IJclPackable).SetAutoPackStrategy(Value);
end;

procedure TJclInt64HashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

procedure TJclInt64HashSet.SetAllowDefaultElements(Value: Boolean);
begin
  FMap.AllowDefaultElements := Value;
end;

procedure TJclInt64HashSet.SetDuplicates(Value: TDuplicates);
begin
  FMap.Duplicates := Value;
end;

procedure TJclInt64HashSet.SetReadOnly(Value: Boolean);
begin
  FMap.ReadOnly := Value;
end;

procedure TJclInt64HashSet.SetRemoveSingleElement(Value: Boolean);
begin
  FMap.RemoveSingleElement := Value;
end;

procedure TJclInt64HashSet.SetReturnDefaultElements(Value: Boolean);
begin
  FMap.ReturnDefaultElements := Value;
end;

procedure TJclInt64HashSet.SetThreadSafe(Value: Boolean);
begin
  FMap.ThreadSafe := Value;
end;


function TJclInt64HashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclInt64HashSet.Subtract(const ACollection: IJclInt64Collection);
begin
  RemoveAll(ACollection);
end;

procedure TJclInt64HashSet.Union(const ACollection: IJclInt64Collection);
begin
  AddAll(ACollection);
end;


{$IFNDEF CLR}
//=== { TJclPtrHashSet } =====================================================

constructor TJclPtrHashSet.Create(const AMap: IJclPtrMap);
begin
  inherited Create();
  FMap := AMap;
end;

constructor TJclPtrHashSet.Create(ACapacity: Integer);
begin
  Create(TJclPtrHashMap.Create(ACapacity, False));
end;

destructor TJclPtrHashSet.Destroy;
begin
  FMap := nil;
  inherited Destroy;
end;

function TJclPtrHashSet.Add(AValue: Pointer): Boolean;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := not FMap.ContainsKey(AValue);
    if Result then
      FMap.PutValue(AValue, RefUnique);
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSet.AddAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclPtrHashSet then
    TJclPtrHashSet(Dest).FMap := (FMap as IJclIntfCloneable).Clone as IJclPtrMap;
end;

procedure TJclPtrHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclPtrHashSet.Contains(AValue: Pointer): Boolean;
begin
  Result := FMap.ContainsKey(AValue);
end;

function TJclPtrHashSet.ContainsAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

function TJclPtrHashSet.Equals(const ACollection: IJclPtrCollection): Boolean;
var
  It, ItMap: IJclPtrIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FMap.Size <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
      begin
        Result := False;
        Exit;
      end;
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSet.First: IJclPtrIterator;
begin
  Result := FMap.KeySet.First;
end;


function TJclPtrHashSet.GetAutoPackParameter: Integer;
begin
  Result := (FMap as IJclPackable).GetAutoPackParameter;
end;

function TJclPtrHashSet.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := (FMap as IJclPackable).GetAutoPackStrategy;
end;

function TJclPtrHashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

function TJclPtrHashSet.GetAllowDefaultElements: Boolean;
begin
  Result := FMap.AllowDefaultElements;
end;

function TJclPtrHashSet.GetDuplicates: TDuplicates;
begin
  Result := FMap.Duplicates;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclPtrHashSet.GetEnumerator: IJclPtrIterator;
begin
  Result := FMap.KeySet.First;
end;
{$ENDIF SUPPORTS_FOR_IN}



function TJclPtrHashSet.GetReadOnly: Boolean;
begin
  Result := FMap.ReadOnly;
end;

function TJclPtrHashSet.GetRemoveSingleElement: Boolean;
begin
  Result := FMap.RemoveSingleElement;
end;

function TJclPtrHashSet.GetReturnDefaultElements: Boolean;
begin
  Result := FMap.ReturnDefaultElements;
end;

function TJclPtrHashSet.GetThreadSafe: Boolean;
begin
  Result := FMap.ThreadSafe;
end;


procedure TJclPtrHashSet.Intersect(const ACollection: IJclPtrCollection);
begin
  RetainAll(ACollection);
end;

function TJclPtrHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclPtrHashSet.Last: IJclPtrIterator;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclPtrHashSet.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclPtrHashSet.Remove(AValue: Pointer): Boolean;
begin
  Result := FMap.Remove(AValue) = RefUnique;
end;

function TJclPtrHashSet.RemoveAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
  ARefUnique: TRefUnique;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ARefUnique := RefUnique;
    It := ACollection.First;
    while It.HasNext do
      Result := (FMap.Remove(It.Next) = ARefUnique) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashSet.RetainAll(const ACollection: IJclPtrCollection): Boolean;
var
  ItMap: IJclPtrIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ACollection.Contains(ItMap.Next) then
        ItMap.Remove;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashSet.SetAutoPackParameter(Value: Integer);
begin
  (FMap as IJclPackable).SetAutoPackParameter(Value);
end;

procedure TJclPtrHashSet.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  (FMap as IJclPackable).SetAutoPackStrategy(Value);
end;

procedure TJclPtrHashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

procedure TJclPtrHashSet.SetAllowDefaultElements(Value: Boolean);
begin
  FMap.AllowDefaultElements := Value;
end;

procedure TJclPtrHashSet.SetDuplicates(Value: TDuplicates);
begin
  FMap.Duplicates := Value;
end;

procedure TJclPtrHashSet.SetReadOnly(Value: Boolean);
begin
  FMap.ReadOnly := Value;
end;

procedure TJclPtrHashSet.SetRemoveSingleElement(Value: Boolean);
begin
  FMap.RemoveSingleElement := Value;
end;

procedure TJclPtrHashSet.SetReturnDefaultElements(Value: Boolean);
begin
  FMap.ReturnDefaultElements := Value;
end;

procedure TJclPtrHashSet.SetThreadSafe(Value: Boolean);
begin
  FMap.ThreadSafe := Value;
end;


function TJclPtrHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclPtrHashSet.Subtract(const ACollection: IJclPtrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclPtrHashSet.Union(const ACollection: IJclPtrCollection);
begin
  AddAll(ACollection);
end;

{$ENDIF ~CLR}

//=== { TJclHashSet } =====================================================

constructor TJclHashSet.Create(const AMap: IJclMap);
begin
  inherited Create(False);
  FMap := AMap;
end;

constructor TJclHashSet.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  Create(TJclHashMap.Create(ACapacity, AOwnsObjects, False));
end;

destructor TJclHashSet.Destroy;
begin
  FMap := nil;
  inherited Destroy;
end;

function TJclHashSet.Add(AObject: TObject): Boolean;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := not FMap.ContainsKey(AObject);
    if Result then
      FMap.PutValue(AObject, RefUnique);
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.AddAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclHashSet then
    TJclHashSet(Dest).FMap := (FMap as IJclIntfCloneable).Clone as IJclMap;
end;

procedure TJclHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclHashSet.Contains(AObject: TObject): Boolean;
begin
  Result := FMap.ContainsKey(AObject);
end;

function TJclHashSet.ContainsAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashSet.Create(GetCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclHashSet.Equals(const ACollection: IJclCollection): Boolean;
var
  It, ItMap: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FMap.Size <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
      begin
        Result := False;
        Exit;
      end;
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.First: IJclIterator;
begin
  Result := FMap.KeySet.First;
end;

function TJclHashSet.FreeObject(var AObject: TObject): TObject;
begin
  Result := (FMap as IJclKeyOwner).FreeKey(AObject);
end;

function TJclHashSet.GetAutoPackParameter: Integer;
begin
  Result := (FMap as IJclPackable).GetAutoPackParameter;
end;

function TJclHashSet.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := (FMap as IJclPackable).GetAutoPackStrategy;
end;

function TJclHashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

function TJclHashSet.GetAllowDefaultElements: Boolean;
begin
  Result := FMap.AllowDefaultElements;
end;

function TJclHashSet.GetDuplicates: TDuplicates;
begin
  Result := FMap.Duplicates;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclHashSet.GetEnumerator: IJclIterator;
begin
  Result := FMap.KeySet.First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclHashSet.GetOwnsObjects: Boolean;
begin
  Result := (FMap as IJclKeyOwner).GetOwnsKeys;
end;


function TJclHashSet.GetReadOnly: Boolean;
begin
  Result := FMap.ReadOnly;
end;

function TJclHashSet.GetRemoveSingleElement: Boolean;
begin
  Result := FMap.RemoveSingleElement;
end;

function TJclHashSet.GetReturnDefaultElements: Boolean;
begin
  Result := FMap.ReturnDefaultElements;
end;

function TJclHashSet.GetThreadSafe: Boolean;
begin
  Result := FMap.ThreadSafe;
end;


procedure TJclHashSet.Intersect(const ACollection: IJclCollection);
begin
  RetainAll(ACollection);
end;

function TJclHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclHashSet.Last: IJclIterator;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclHashSet.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclHashSet.Remove(AObject: TObject): Boolean;
begin
  Result := FMap.Remove(AObject) = RefUnique;
end;

function TJclHashSet.RemoveAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  ARefUnique: TRefUnique;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ARefUnique := RefUnique;
    It := ACollection.First;
    while It.HasNext do
      Result := (FMap.Remove(It.Next) = ARefUnique) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.RetainAll(const ACollection: IJclCollection): Boolean;
var
  ItMap: IJclIterator;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ACollection.Contains(ItMap.Next) then
        ItMap.Remove;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSet.SetAutoPackParameter(Value: Integer);
begin
  (FMap as IJclPackable).SetAutoPackParameter(Value);
end;

procedure TJclHashSet.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  (FMap as IJclPackable).SetAutoPackStrategy(Value);
end;

procedure TJclHashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

procedure TJclHashSet.SetAllowDefaultElements(Value: Boolean);
begin
  FMap.AllowDefaultElements := Value;
end;

procedure TJclHashSet.SetDuplicates(Value: TDuplicates);
begin
  FMap.Duplicates := Value;
end;

procedure TJclHashSet.SetReadOnly(Value: Boolean);
begin
  FMap.ReadOnly := Value;
end;

procedure TJclHashSet.SetRemoveSingleElement(Value: Boolean);
begin
  FMap.RemoveSingleElement := Value;
end;

procedure TJclHashSet.SetReturnDefaultElements(Value: Boolean);
begin
  FMap.ReturnDefaultElements := Value;
end;

procedure TJclHashSet.SetThreadSafe(Value: Boolean);
begin
  FMap.ThreadSafe := Value;
end;


function TJclHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclHashSet.Subtract(const ACollection: IJclCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclHashSet.Union(const ACollection: IJclCollection);
begin
  AddAll(ACollection);
end;








{$IFDEF SUPPORTS_GENERICS}







//=== { TJclHashSet<T> } =====================================================

constructor TJclHashSet<T>.Create(const AMap: IJclMap<T, TRefUnique>);
begin
  inherited Create(False);
  FMap := AMap;
end;


destructor TJclHashSet<T>.Destroy;
begin
  FMap := nil;
  inherited Destroy;
end;

function TJclHashSet<T>.Add(const AItem: T): Boolean;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := not FMap.ContainsKey(AItem);
    if Result then
      FMap.PutValue(AItem, RefUnique);
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSet<T>.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclHashSet<T> then
    TJclHashSet<T>(Dest).FMap := (FMap as IJclIntfCloneable).Clone as IJclMap<T, TRefUnique>;
end;

procedure TJclHashSet<T>.Clear;
begin
  FMap.Clear;
end;

function TJclHashSet<T>.Contains(const AItem: T): Boolean;
begin
  Result := FMap.ContainsKey(AItem);
end;

function TJclHashSet<T>.ContainsAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclHashSet<T>.Equals(const ACollection: IJclCollection<T>): Boolean;
var
  It, ItMap: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  FMap.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FMap.Size <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
      begin
        Result := False;
        Exit;
      end;
  {$IFDEF THREADSAFE}
  finally
    FMap.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.First: IJclIterator<T>;
begin
  Result := FMap.KeySet.First;
end;

function TJclHashSet<T>.FreeItem(var AItem: T): T;
begin
  Result := (FMap as IJclPairOwner<T, TRefUnique>).FreeKey(AItem);
end;

function TJclHashSet<T>.GetAutoPackParameter: Integer;
begin
  Result := (FMap as IJclPackable).GetAutoPackParameter;
end;

function TJclHashSet<T>.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := (FMap as IJclPackable).GetAutoPackStrategy;
end;

function TJclHashSet<T>.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

function TJclHashSet<T>.GetAllowDefaultElements: Boolean;
begin
  Result := FMap.AllowDefaultElements;
end;

function TJclHashSet<T>.GetDuplicates: TDuplicates;
begin
  Result := FMap.Duplicates;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclHashSet<T>.GetEnumerator: IJclIterator<T>;
begin
  Result := FMap.KeySet.First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclHashSet<T>.GetOwnsItems: Boolean;
begin
  Result := (FMap as IJclPairOwner<T, TRefUnique>).GetOwnsKeys;
end;


function TJclHashSet<T>.GetReadOnly: Boolean;
begin
  Result := FMap.ReadOnly;
end;

function TJclHashSet<T>.GetRemoveSingleElement: Boolean;
begin
  Result := FMap.RemoveSingleElement;
end;

function TJclHashSet<T>.GetReturnDefaultElements: Boolean;
begin
  Result := FMap.ReturnDefaultElements;
end;

function TJclHashSet<T>.GetThreadSafe: Boolean;
begin
  Result := FMap.ThreadSafe;
end;


procedure TJclHashSet<T>.Intersect(const ACollection: IJclCollection<T>);
begin
  RetainAll(ACollection);
end;

function TJclHashSet<T>.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclHashSet<T>.Last: IJclIterator<T>;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclHashSet<T>.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclHashSet<T>.Remove(const AItem: T): Boolean;
begin
  Result := FMap.Remove(AItem) = RefUnique;
end;

function TJclHashSet<T>.RemoveAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
  ARefUnique: TRefUnique;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ARefUnique := RefUnique;
    It := ACollection.First;
    while It.HasNext do
      Result := (FMap.Remove(It.Next) = ARefUnique) and Result;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.RetainAll(const ACollection: IJclCollection<T>): Boolean;
var
  ItMap: IJclIterator<T>;
begin
  if FMap.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FMap.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ACollection.Contains(ItMap.Next) then
        ItMap.Remove;
  {$IFDEF THREADSAFE}
  finally
    FMap.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSet<T>.SetAutoPackParameter(Value: Integer);
begin
  (FMap as IJclPackable).SetAutoPackParameter(Value);
end;

procedure TJclHashSet<T>.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  (FMap as IJclPackable).SetAutoPackStrategy(Value);
end;

procedure TJclHashSet<T>.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

procedure TJclHashSet<T>.SetAllowDefaultElements(Value: Boolean);
begin
  FMap.AllowDefaultElements := Value;
end;

procedure TJclHashSet<T>.SetDuplicates(Value: TDuplicates);
begin
  FMap.Duplicates := Value;
end;

procedure TJclHashSet<T>.SetReadOnly(Value: Boolean);
begin
  FMap.ReadOnly := Value;
end;

procedure TJclHashSet<T>.SetRemoveSingleElement(Value: Boolean);
begin
  FMap.RemoveSingleElement := Value;
end;

procedure TJclHashSet<T>.SetReturnDefaultElements(Value: Boolean);
begin
  FMap.ReturnDefaultElements := Value;
end;

procedure TJclHashSet<T>.SetThreadSafe(Value: Boolean);
begin
  FMap.ThreadSafe := Value;
end;


function TJclHashSet<T>.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclHashSet<T>.Subtract(const ACollection: IJclCollection<T>);
begin
  RemoveAll(ACollection);
end;

procedure TJclHashSet<T>.Union(const ACollection: IJclCollection<T>);
begin
  AddAll(ACollection);
end;


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

