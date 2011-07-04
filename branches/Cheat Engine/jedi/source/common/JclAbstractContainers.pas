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
{ The Original Code is AbstractContainer.pas and DCL_Util.pas.                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Daniele Teti (dade2004)                                                                        }
{   Robert Marquardt (marquardt)                                                                   }
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

unit JclAbstractContainers;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  SysUtils, Classes, JclBase, JclContainerIntf, JclSynch, JclSysUtils;

type
  {$IFDEF KEEP_DEPRECATED}
  TJclIntfCriticalSection = JclSysUtils.TJclIntfCriticalSection;
  {$ENDIF KEEP_DEPRECATED}

  TJclAbstractLockable = class(TInterfacedObject {$IFDEF THREADSAFE}, IJclLockable {$ENDIF THREADSAFE})
  {$IFDEF THREADSAFE}
  protected
    FThreadSafe: Boolean;
    FSyncReaderWriter: TJclMultiReadExclusiveWrite;
    procedure ReadLock;
    procedure ReadUnlock;
    procedure WriteLock;
    procedure WriteUnlock;
  public
    constructor Create;
    destructor Destroy; override;

    property SyncReaderWriter: TJclMultiReadExclusiveWrite read FSyncReaderWriter;
  {$ENDIF THREADSAFE}
  end;

  TJclAbstractContainerBase = class(TJclAbstractLockable, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer)
  protected
    FAllowDefaultElements: Boolean;
    FDuplicates: TDuplicates;
    FRemoveSingleElement: Boolean;
    FReturnDefaultElements: Boolean;
    FReadOnly: Boolean;
    FCapacity: Integer;
    FSize: Integer;
    FAutoGrowParameter: Integer;
    FAutoGrowStrategy: TJclAutoGrowStrategy;
    FAutoPackParameter: Integer;
    FAutoPackStrategy: TJclAutoPackStrategy;
    procedure AutoGrow; virtual;
    procedure AutoPack; virtual;
    function CheckDuplicate: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; virtual; abstract;
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); virtual;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); virtual;
    { IJclContainer }
    procedure Assign(const Source: IJclContainer);
    procedure AssignTo(const Dest: IJclContainer);
    function GetAllowDefaultElements: Boolean; virtual;
    function GetContainerReference: TObject;
    function GetDuplicates: TDuplicates; virtual;
    function GetReadOnly: Boolean; virtual;
    function GetRemoveSingleElement: Boolean; virtual;
    function GetReturnDefaultElements: Boolean; virtual;
    function GetThreadSafe: Boolean; virtual;
    procedure SetAllowDefaultElements(Value: Boolean); virtual;
    procedure SetDuplicates(Value: TDuplicates); virtual;
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure SetRemoveSingleElement(Value: Boolean); virtual;
    procedure SetReturnDefaultElements(Value: Boolean); virtual;
    procedure SetThreadSafe(Value: Boolean); virtual;
    { IJclCloneable }
    function ObjectClone: TObject;
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
    // IJclGrowable is not in interface list because some descendants won't use this code
    { IJclGrowable }
    function CalcGrowCapacity(ACapacity, ASize: Integer): Integer; virtual;
    function GetAutoGrowParameter: Integer; virtual;
    function GetAutoGrowStrategy: TJclAutoGrowStrategy; virtual;
    procedure Grow; virtual;
    procedure SetAutoGrowParameter(Value: Integer); virtual;
    procedure SetAutoGrowStrategy(Value: TJclAutoGrowStrategy); virtual;
    // IJclPackable is not in interface list because some descendants won't use this code
    { IJclPackable }
    function CalcPackCapacity(ACapacity, ASize: Integer): Integer; virtual;
    function GetAutoPackParameter: Integer; virtual;
    function GetAutoPackStrategy: TJclAutoPackStrategy; virtual;
    function GetCapacity: Integer; virtual;
    procedure Pack; virtual;
    procedure SetAutoPackParameter(Value: Integer); virtual;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); virtual;
    procedure SetCapacity(Value: Integer); virtual;
  public
    constructor Create;
    property AllowDefaultElements: Boolean read GetAllowDefaultElements write SetAllowDefaultElements;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property RemoveSingleElement: Boolean read GetRemoveSingleElement write SetRemoveSingleElement;
    property ReturnDefaultElements: Boolean read GetReturnDefaultElements write SetReturnDefaultElements;
    property ThreadSafe: Boolean read GetThreadSafe write SetThreadSafe;
    property AutoGrowParameter: Integer read GetAutoGrowParameter write SetAutoGrowParameter;
    property AutoGrowStrategy: TJclAutoGrowStrategy read GetAutoGrowStrategy write SetAutoGrowStrategy;
    property AutoPackParameter: Integer read GetAutoPackParameter write SetAutoPackParameter;
    property AutoPackStrategy: TJclAutoPackStrategy read GetAutoPackStrategy write SetAutoPackStrategy;
  end;

  TJclAbstractIterator = class(TJclAbstractLockable, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclAbstractIterator)
  private
    FValid: Boolean;
  protected
    procedure CheckValid;
    function CreateEmptyIterator: TJclAbstractIterator; virtual; abstract;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); virtual;
    { IJclAbstractIterator }
    procedure Assign(const Source: IJclAbstractIterator);
    procedure AssignTo(const Dest: IJclAbstractIterator);
    function GetIteratorReference: TObject;
    { IJclCloneable }
    function ObjectClone: TObject;
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(AValid: Boolean);
    property Valid: Boolean read FValid write FValid;
  end;

  TJclIntfAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclIntfEqualityComparer, IJclIntfComparer, IJclIntfHashConverter)
  protected
    FEqualityCompare: TIntfEqualityCompare;
    FCompare: TIntfCompare;
    FHashConvert: TIntfHashConvert;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function FreeObject(var AInterface: IInterface): IInterface;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIntfEqualityComparer }
    function GetEqualityCompare: TIntfEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TIntfEqualityCompare); virtual;
    function ItemsEqual(const A, B: IInterface): Boolean; virtual;
    { IJclIntfComparer }
    function GetCompare: TIntfCompare; virtual;
    procedure SetCompare(Value: TIntfCompare); virtual;
    function ItemsCompare(const A, B: IInterface): Integer; virtual;
    { IJclIntfHashConverter }
    function GetHashConvert: TIntfHashConvert; virtual;
    procedure SetHashConvert(Value: TIntfHashConvert); virtual;
    function Hash(const AInterface: IInterface): Integer; virtual;
  public
    property EqualityCompare: TIntfEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    property Compare: TIntfCompare read GetCompare write SetCompare;
    property HashConvert: TIntfHashConvert read GetHashConvert write SetHashConvert;
  end;

  TJclStrAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclStrContainer)
  protected
    FCaseSensitive: Boolean;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStrContainer }
    function GetCaseSensitive: Boolean; virtual;
    procedure SetCaseSensitive(Value: Boolean); virtual;
  public
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
  end;

  TJclAnsiStrAbstractContainer = class(TJclStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer,
    IJclAnsiStrEqualityComparer, IJclAnsiStrComparer, IJclAnsiStrHashConverter)
  protected
    FEncoding: TJclAnsiStrEncoding;
    FEqualityCompare: TAnsiStrEqualityCompare;
    FCompare: TAnsiStrCompare;
    FHashConvert: TAnsiStrHashConvert;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function FreeString(var AString: AnsiString): AnsiString;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclAnsiStrContainer }
    function GetEncoding: TJclAnsiStrEncoding; virtual;
    procedure SetEncoding(Value: TJclAnsiStrEncoding); virtual;
    { IJclAnsiStrEqualityComparer }
    function GetEqualityCompare: TAnsiStrEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TAnsiStrEqualityCompare); virtual;
    function ItemsEqual(const A, B: AnsiString): Boolean; virtual;
    { IJclAnsiStrComparer }
    function GetCompare: TAnsiStrCompare; virtual;
    procedure SetCompare(Value: TAnsiStrCompare); virtual;
    function ItemsCompare(const A, B: AnsiString): Integer; virtual;
    { IJclAnsiStrHashConverter }
    function GetHashConvert: TAnsiStrHashConvert; virtual;
    procedure SetHashConvert(Value: TAnsiStrHashConvert); virtual;
    function Hash(const AString: AnsiString): Integer; virtual;
  public
    property Encoding: TJclAnsiStrEncoding read GetEncoding write SetEncoding;
    property EqualityCompare: TAnsiStrEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    property Compare: TAnsiStrCompare read GetCompare write SetCompare;
    property HashConvert: TAnsiStrHashConvert read GetHashConvert write SetHashConvert;
  end;

  TJclWideStrAbstractContainer = class(TJclStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclStrContainer, IJclWideStrContainer,
    IJclWideStrEqualityComparer, IJclWideStrComparer, IJclWideStrHashConverter)
  protected
    FEncoding: TJclWideStrEncoding;
    FEqualityCompare: TWideStrEqualityCompare;
    FCompare: TWideStrCompare;
    FHashConvert: TWideStrHashConvert;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function FreeString(var AString: WideString): WideString;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclWideStrContainer }
    function GetEncoding: TJclWideStrEncoding; virtual;
    procedure SetEncoding(Value: TJclWideStrEncoding); virtual;
    { IJclWideStrEqualityComparer }
    function GetEqualityCompare: TWideStrEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TWideStrEqualityCompare); virtual;
    function ItemsEqual(const A, B: WideString): Boolean; virtual;
    { IJclWideStrComparer }
    function GetCompare: TWideStrCompare; virtual;
    procedure SetCompare(Value: TWideStrCompare); virtual;
    function ItemsCompare(const A, B: WideString): Integer; virtual;
    { IJclWideStrHashConverter }
    function GetHashConvert: TWideStrHashConvert; virtual;
    procedure SetHashConvert(Value: TWideStrHashConvert); virtual;
    function Hash(const AString: WideString): Integer; virtual;
  public
    property Encoding: TJclWideStrEncoding read GetEncoding write SetEncoding;
    property EqualityCompare: TWideStrEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    property Compare: TWideStrCompare read GetCompare write SetCompare;
    property HashConvert: TWideStrHashConvert read GetHashConvert write SetHashConvert;
  end;

  TJclSingleAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclSingleContainer, IJclSingleEqualityComparer,
    IJclSingleComparer, IJclSingleHashConverter)
  protected
    FPrecision: Single;
    FEqualityCompare: TSingleEqualityCompare;
    FCompare: TSingleCompare;
    FHashConvert: TSingleHashConvert;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function FreeSingle(var AValue: Single): Single;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclSingleEqualityComparer }
    function GetEqualityCompare: TSingleEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TSingleEqualityCompare); virtual;
    function ItemsEqual(const A, B: Single): Boolean; virtual;
    { IJclSingleComparer }
    function GetCompare: TSingleCompare; virtual;
    procedure SetCompare(Value: TSingleCompare); virtual;
    function ItemsCompare(const A, B: Single): Integer; virtual;
    { IJclSingleContainer }
    function GetPrecision: Single; virtual;
    procedure SetPrecision(const Value: Single); virtual;
    { IJclSingleHashConverter }
    function GetHashConvert: TSingleHashConvert; virtual;
    procedure SetHashConvert(Value: TSingleHashConvert); virtual;
    function Hash(const AValue: Single): Integer; virtual;
  public
    property Precision: Single read GetPrecision write SetPrecision;
    property EqualityCompare: TSingleEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    property Compare: TSingleCompare read GetCompare write SetCompare;
    property HashConvert: TSingleHashConvert read GetHashConvert write SetHashConvert;
  end;

  TJclDoubleAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclDoubleContainer, IJclDoubleEqualityComparer,
    IJclDoubleComparer, IJclDoubleHashConverter)
  protected
    FPrecision: Double;
    FEqualityCompare: TDoubleEqualityCompare;
    FCompare: TDoubleCompare;
    FHashConvert: TDoubleHashConvert;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function FreeDouble(var AValue: Double): Double;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclDoubleEqualityComparer }
    function GetEqualityCompare: TDoubleEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TDoubleEqualityCompare); virtual;
    function ItemsEqual(const A, B: Double): Boolean; virtual;
    { IJclDoubleComparer }
    function GetCompare: TDoubleCompare; virtual;
    procedure SetCompare(Value: TDoubleCompare); virtual;
    function ItemsCompare(const A, B: Double): Integer; virtual;
    { IJclDoubleContainer }
    function GetPrecision: Double; virtual;
    procedure SetPrecision(const Value: Double); virtual;
    { IJclDoubleHashConverter }
    function GetHashConvert: TDoubleHashConvert; virtual;
    procedure SetHashConvert(Value: TDoubleHashConvert); virtual;
    function Hash(const AValue: Double): Integer; virtual;
  public
    property Precision: Double read GetPrecision write SetPrecision;
    property EqualityCompare: TDoubleEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    property Compare: TDoubleCompare read GetCompare write SetCompare;
    property HashConvert: TDoubleHashConvert read GetHashConvert write SetHashConvert;
  end;

  TJclExtendedAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclExtendedContainer, IJclExtendedEqualityComparer,
    IJclExtendedComparer, IJclExtendedHashConverter)
  protected
    FPrecision: Extended;
    FEqualityCompare: TExtendedEqualityCompare;
    FCompare: TExtendedCompare;
    FHashConvert: TExtendedHashConvert;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function FreeExtended(var AValue: Extended): Extended;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclExtendedEqualityComparer }
    function GetEqualityCompare: TExtendedEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TExtendedEqualityCompare); virtual;
    function ItemsEqual(const A, B: Extended): Boolean; virtual;
    { IJclExtendedComparer }
    function GetCompare: TExtendedCompare; virtual;
    procedure SetCompare(Value: TExtendedCompare); virtual;
    function ItemsCompare(const A, B: Extended): Integer; virtual;
    { IJclExtendedContainer }
    function GetPrecision: Extended; virtual;
    procedure SetPrecision(const Value: Extended); virtual;
    { IJclExtendedHashConverter }
    function GetHashConvert: TExtendedHashConvert; virtual;
    procedure SetHashConvert(Value: TExtendedHashConvert); virtual;
    function Hash(const AValue: Extended): Integer; virtual;
  public
    property Precision: Extended read GetPrecision write SetPrecision;
    property EqualityCompare: TExtendedEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    property Compare: TExtendedCompare read GetCompare write SetCompare;
    property HashConvert: TExtendedHashConvert read GetHashConvert write SetHashConvert;
  end;

  TJclIntegerAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclIntegerEqualityComparer, IJclIntegerComparer,
    IJclIntegerHashConverter)
  protected
    FEqualityCompare: TIntegerEqualityCompare;
    FCompare: TIntegerCompare;
    FHashConvert: TIntegerHashConvert;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function FreeInteger(var AValue: Integer): Integer;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIntegerEqualityComparer }
    function GetEqualityCompare: TIntegerEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TIntegerEqualityCompare); virtual;
    function ItemsEqual(A, B: Integer): Boolean; virtual;
    { IJclIntegerComparer }
    function GetCompare: TIntegerCompare; virtual;
    procedure SetCompare(Value: TIntegerCompare); virtual;
    function ItemsCompare(A, B: Integer): Integer; virtual;
    { IJclIntegerHashConverter }
    function GetHashConvert: TIntegerHashConvert; virtual;
    procedure SetHashConvert(Value: TIntegerHashConvert); virtual;
    function Hash(AValue: Integer): Integer; virtual;
  public
    property EqualityCompare: TIntegerEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    property Compare: TIntegerCompare read GetCompare write SetCompare;
    property HashConvert: TIntegerHashConvert read GetHashConvert write SetHashConvert;
  end;

  TJclCardinalAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclCardinalEqualityComparer, IJclCardinalComparer,
    IJclCardinalHashConverter)
  protected
    FEqualityCompare: TCardinalEqualityCompare;
    FCompare: TCardinalCompare;
    FHashConvert: TCardinalHashConvert;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function FreeCardinal(var AValue: Cardinal): Cardinal;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIntegerEqualityComparer }
    function GetEqualityCompare: TCardinalEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TCardinalEqualityCompare); virtual;
    function ItemsEqual(A, B: Cardinal): Boolean; virtual;
    { IJclIntegerComparer }
    function GetCompare: TCardinalCompare; virtual;
    procedure SetCompare(Value: TCardinalCompare); virtual;
    function ItemsCompare(A, B: Cardinal): Integer; virtual;
    { IJclIntegerHashConverter }
    function GetHashConvert: TCardinalHashConvert; virtual;
    procedure SetHashConvert(Value: TCardinalHashConvert); virtual;
    function Hash(AValue: Cardinal): Integer; virtual;
  public
    property EqualityCompare: TCardinalEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    property Compare: TCardinalCompare read GetCompare write SetCompare;
    property HashConvert: TCardinalHashConvert read GetHashConvert write SetHashConvert;
  end;

  TJclInt64AbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclInt64EqualityComparer, IJclInt64Comparer,
    IJclInt64HashConverter)
  protected
    FEqualityCompare: TInt64EqualityCompare;
    FCompare: TInt64Compare;
    FHashConvert: TInt64HashConvert;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function FreeInt64(var AValue: Int64): Int64;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclInt64EqualityComparer }
    function GetEqualityCompare: TInt64EqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TInt64EqualityCompare); virtual;
    function ItemsEqual(const A, B: Int64): Boolean; virtual;
    { IJclInt64Comparer }
    function GetCompare: TInt64Compare; virtual;
    procedure SetCompare(Value: TInt64Compare); virtual;
    function ItemsCompare(const A, B: Int64): Integer; virtual;
    { IJclInt64HashConverter }
    function GetHashConvert: TInt64HashConvert; virtual;
    procedure SetHashConvert(Value: TInt64HashConvert); virtual;
    function Hash(const AValue: Int64): Integer; virtual;
  public
    property EqualityCompare: TInt64EqualityCompare read GetEqualityCompare write SetEqualityCompare;
    property Compare: TInt64Compare read GetCompare write SetCompare;
    property HashConvert: TInt64HashConvert read GetHashConvert write SetHashConvert;
  end;

  {$IFNDEF CLR}
  TJclPtrAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclPtrEqualityComparer, IJclPtrComparer, IJclPtrHashConverter)
  protected
    FEqualityCompare: TPtrEqualityCompare;
    FCompare: TPtrCompare;
    FHashConvert: TPtrHashConvert;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function FreePointer(var APtr: Pointer): Pointer;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclPtrEqualityComparer }
    function GetEqualityCompare: TPtrEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TPtrEqualityCompare); virtual;
    function ItemsEqual(A, B: Pointer): Boolean; virtual;
    { IJclPtrComparer }
    function GetCompare: TPtrCompare; virtual;
    procedure SetCompare(Value: TPtrCompare); virtual;
    function ItemsCompare(A, B: Pointer): Integer; virtual;
    { IJclPtrHashConverter }
    function GetHashConvert: TPtrHashConvert; virtual;
    procedure SetHashConvert(Value: TPtrHashConvert); virtual;
    function Hash(APtr: Pointer): Integer; virtual;
  public
    property EqualityCompare: TPtrEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    property Compare: TPtrCompare read GetCompare write SetCompare;
    property HashConvert: TPtrHashConvert read GetHashConvert write SetHashConvert;
  end;
  {$ENDIF ~CLR}

  TJclAbstractContainer = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclObjectOwner, IJclEqualityComparer, IJclComparer,
    IJclHashConverter)
  protected
    FOwnsObjects: Boolean;
    FEqualityCompare: TEqualityCompare;
    FCompare: TCompare;
    FHashConvert: THashConvert;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclEqualityComparer }
    function GetEqualityCompare: TEqualityCompare; virtual;
    procedure SetEqualityCompare(Value: TEqualityCompare); virtual;
    function ItemsEqual(A, B: TObject): Boolean; virtual;
    { IJclComparer }
    function GetCompare: TCompare; virtual;
    procedure SetCompare(Value: TCompare); virtual;
    function ItemsCompare(A, B: TObject): Integer; virtual;
    { IJclObjectOwner }
    function FreeObject(var AObject: TObject): TObject; virtual;
    function GetOwnsObjects: Boolean; virtual;
    { IJclHashConverter }
    function GetHashConvert: THashConvert; virtual;
    procedure SetHashConvert(Value: THashConvert); virtual;
    function Hash(AObject: TObject): Integer; virtual;
  public
    constructor Create(AOwnsObjects: Boolean);
    property OwnsObjects: Boolean read FOwnsObjects;
    property EqualityCompare: TEqualityCompare read GetEqualityCompare write SetEqualityCompare;
    property Compare: TCompare read GetCompare write SetCompare;
    property HashConvert: THashConvert read GetHashConvert write SetHashConvert;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  TJclAbstractContainer<T> = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclCloneable, IJclIntfCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclHashConverter<T>)
  protected
    FOwnsItems: Boolean;
    FEqualityCompare: TEqualityCompare<T>;
    FCompare: TCompare<T>;
    FHashConvert: THashConvert<T>;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclEqualityComparer<T> }
    function GetEqualityCompare: TEqualityCompare<T>; virtual;
    procedure SetEqualityCompare(Value: TEqualityCompare<T>); virtual;
    function ItemsEqual(const A, B: T): Boolean; virtual;
    { IJclComparer<T> }
    function GetCompare: TCompare<T>; virtual;
    procedure SetCompare(Value: TCompare<T>); virtual;
    function ItemsCompare(const A, B: T): Integer; virtual;
    { IJclItemOwner<T> }
    function FreeItem(var AItem: T): T; virtual;
    function GetOwnsItems: Boolean; virtual;
    { IJclHashConverter<T> }
    function GetHashConvert: THashConvert<T>; virtual;
    procedure SetHashConvert(Value: THashConvert<T>); virtual;
    function Hash(const AItem: T): Integer; virtual;
  public
    constructor Create(AOwnsItems: Boolean);
    property OwnsItems: Boolean read FOwnsItems;
    property EqualityCompare: TEqualityCompare<T> read GetEqualityCompare write SetEqualityCompare;
    property Compare: TCompare<T> read GetCompare write SetCompare;
    property HashConvert: THashConvert<T> read GetHashConvert write SetHashConvert;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  TJclAnsiStrAbstractCollection = class(TJclAnsiStrAbstractContainer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclCloneable, IJclIntfCloneable, IJclContainer,
    IJclStrContainer, IJclAnsiStrContainer, IJclAnsiStrFlatContainer, IJclAnsiStrCollection,
    IJclAnsiStrEqualityComparer, IJclAnsiStrComparer)
  protected
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclAnsiStrCollection }
    function Add(const AString: AnsiString): Boolean; virtual; abstract;
    function AddAll(const ACollection: IJclAnsiStrCollection): Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    function Contains(const AString: AnsiString): Boolean; virtual; abstract;
    function ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean; virtual; abstract;
    function Equals(const ACollection: IJclAnsiStrCollection): Boolean; virtual; abstract;
    function First: IJclAnsiStrIterator; virtual; abstract;
    function IsEmpty: Boolean; virtual; abstract;
    function Last: IJclAnsiStrIterator; virtual; abstract;
    function Remove(const AString: AnsiString): Boolean; overload; virtual; abstract;
    function RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean; virtual; abstract;
    function RetainAll(const ACollection: IJclAnsiStrCollection): Boolean; virtual; abstract;
    function Size: Integer; virtual; abstract;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclAnsistrIterator; virtual; abstract;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclAnsiStrFlatContainer }
    procedure LoadFromStrings(Strings: TStrings);
    procedure SaveToStrings(Strings: TStrings);
    procedure AppendToStrings(Strings: TStrings);
    procedure AppendFromStrings(Strings: TStrings);
    function GetAsStrings: TStrings;
    function GetAsDelimited(const Separator: AnsiString = AnsiLineBreak): AnsiString;
    procedure AppendDelimited(const AString: AnsiString; const Separator: AnsiString = AnsiLineBreak);
    procedure LoadDelimited(const AString: AnsiString; const Separator: AnsiString = AnsiLineBreak);
  end;

  TJclWideStrAbstractCollection = class(TJclWideStrAbstractContainer,
    {$IFDEF THREADSAFE}IJclLockable,{$ENDIF THREADSAFE} IJclCloneable, IJclIntfCloneable, IJclContainer,
    IJclStrContainer, IJclWideStrContainer, IJclWideStrFlatContainer, IJclWideStrCollection,
    IJclWideStrEqualityComparer, IJclWideStrComparer)
  protected
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclWideStrCollection }
    function Add(const AString: WideString): Boolean; virtual; abstract;
    function AddAll(const ACollection: IJclWideStrCollection): Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    function Contains(const AString: WideString): Boolean; virtual; abstract;
    function ContainsAll(const ACollection: IJclWideStrCollection): Boolean; virtual; abstract;
    function Equals(const ACollection: IJclWideStrCollection): Boolean; virtual; abstract;
    function First: IJclWideStrIterator; virtual; abstract;
    function IsEmpty: Boolean; virtual; abstract;
    function Last: IJclWideStrIterator; virtual; abstract;
    function Remove(const AString: WideString): Boolean; overload; virtual; abstract;
    function RemoveAll(const ACollection: IJclWideStrCollection): Boolean; virtual; abstract;
    function RetainAll(const ACollection: IJclWideStrCollection): Boolean; virtual; abstract;
    function Size: Integer; virtual; abstract;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclWideStrIterator; virtual; abstract;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclWideStrFlatContainer }
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclAbstractContainers.pas $';
    Revision: '$Revision: 2376 $';
    Date: '$Date: 2008-06-05 15:35:37 +0200 (jeu., 05 juin 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

{$IFNDEF RTL140_UP}
uses
  JclWideStrings;
{$ENDIF ~RTL140_UP}

//=== { TJclAbstractLockable } ===============================================

{$IFDEF THREADSAFE}

constructor TJclAbstractLockable.Create;
begin
  inherited Create;
  FThreadSafe := True;
  FSyncReaderWriter := TJclMultiReadExclusiveWrite.Create{$IFNDEF CLR}(mpReaders){$ENDIF ~CLR};
end;

destructor TJclAbstractLockable.Destroy;
begin
  FSyncReaderWriter.Free;
  inherited Destroy;
end;

procedure TJclAbstractLockable.ReadLock;
begin
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
end;

procedure TJclAbstractLockable.ReadUnlock;
begin
  if FThreadSafe then
    SyncReaderWriter.EndRead;
end;

procedure TJclAbstractLockable.WriteLock;
begin
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
end;

procedure TJclAbstractLockable.WriteUnlock;
begin
  if FThreadSafe then
    SyncReaderWriter.EndWrite;
end;
{$ENDIF THREADSAFE}

//=== { TJclAbstractContainerBase } ==========================================

constructor TJclAbstractContainerBase.Create;
begin
  inherited Create;

  FAllowDefaultElements := True;
  FDuplicates := dupAccept;
  FRemoveSingleElement := True;
  FReturnDefaultElements := True;
  FAutoGrowStrategy := agsProportional;
  FAutoGrowParameter := 4;
  FAutoPackStrategy := apsDisabled;
  FAutoPackParameter := 4;
end;

procedure TJclAbstractContainerBase.Assign(const Source: IJclContainer);
begin
  Source.AssignTo(Self);
end;

procedure TJclAbstractContainerBase.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  // override to customize
  if Dest.ReadOnly then
    raise EJclReadOnlyError.Create;
end;

procedure TJclAbstractContainerBase.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  // override to customize
  Dest.AllowDefaultElements := AllowDefaultElements;
  Dest.Duplicates := Duplicates;
  Dest.RemoveSingleElement := RemoveSingleElement;
  Dest.ReturnDefaultElements := ReturnDefaultElements;
  Dest.AutoGrowParameter := AutoGrowParameter;
  Dest.AutoGrowStrategy := AutoGrowStrategy;
  Dest.AutoPackParameter := AutoPackParameter;
  Dest.AutoPackStrategy := AutoPackStrategy;
end;

procedure TJclAbstractContainerBase.AssignTo(const Dest: IJclContainer);
var
  DestObject: TObject;
begin
  DestObject := Dest.GetContainerReference;
  if DestObject is TJclAbstractContainerBase then
  begin
    AssignPropertiesTo(TJclAbstractContainerBase(DestObject));
    AssignDataTo(TJclAbstractContainerBase(DestObject));
  end
  else
    raise EJclAssignError.Create;
end;

procedure TJclAbstractContainerBase.AutoGrow;
begin
  SetCapacity(CalcGrowCapacity(FCapacity, FSize));
end;

procedure TJclAbstractContainerBase.AutoPack;
begin
  SetCapacity(CalcPackCapacity(FCapacity, FSize));
end;

function TJclAbstractContainerBase.CalcGrowCapacity(ACapacity, ASize: Integer): Integer;
var
  Increment: Integer;
begin
  Result := ACapacity;
  if ASize = ACapacity then
  begin
    case FAutoGrowStrategy of
      agsDisabled: ;
      agsAgressive:
        Result := ACapacity + 1;
      agsProportional:
        begin
          Increment := ACapacity div FAutoGrowParameter;
          if Increment = 0 then
            Increment := 1;
          Result := ACapacity + Increment;
        end;
      agsIncremental:
        Result := ACapacity + FAutoGrowParameter;
    end;
  end;
end;

function TJclAbstractContainerBase.CalcPackCapacity(ACapacity, ASize: Integer): Integer;
var
  Decrement: Integer;
begin
  Result := ACapacity;
  if ASize < ACapacity then
  begin
    case FAutoPackStrategy of
      apsDisabled:
        Decrement := 0;
      apsAgressive:
        Decrement := 1;
      apsProportional:
        Decrement := ACapacity div FAutoPackParameter;
      apsIncremental:
        Decrement := FAutoPackParameter;
    else
      Decrement := 0;
    end;
    if (Decrement > 0) and ((ASize + Decrement) <= ACapacity) then
      Result := ASize;
  end;
end;

function TJclAbstractContainerBase.CheckDuplicate: Boolean;
begin
  case FDuplicates of
    dupIgnore:
      Result := False;
    dupAccept:
      Result := True;
    //dupError: ;
  else
    raise EJclDuplicateElementError.Create;
  end;
end;

function TJclAbstractContainerBase.ObjectClone: TObject;
var
  NewContainer: TJclAbstractContainerBase;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewContainer := CreateEmptyContainer;
    AssignDataTo(NewContainer);
    Result := NewContainer;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAbstractContainerBase.GetAllowDefaultElements: Boolean;
begin
  Result := FAllowDefaultElements;
end;

function TJclAbstractContainerBase.GetAutoGrowParameter: Integer;
begin
  Result := FAutoGrowParameter;
end;

function TJclAbstractContainerBase.GetAutoGrowStrategy: TJclAutoGrowStrategy;
begin
  Result := FAutoGrowStrategy;
end;

function TJclAbstractContainerBase.GetAutoPackParameter: Integer;
begin
  Result := FAutoPackParameter;
end;

function TJclAbstractContainerBase.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := FAutoPackStrategy;
end;

function TJclAbstractContainerBase.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TJclAbstractContainerBase.GetContainerReference: TObject;
begin
  Result := Self;
end;

function TJclAbstractContainerBase.GetDuplicates: TDuplicates;
begin
  Result := FDuplicates;
end;

function TJclAbstractContainerBase.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TJclAbstractContainerBase.GetRemoveSingleElement: Boolean;
begin
  Result := FRemoveSingleElement;
end;

function TJclAbstractContainerBase.GetReturnDefaultElements: Boolean;
begin
  Result := FReturnDefaultElements;
end;

function TJclAbstractContainerBase.GetThreadSafe: Boolean;
begin
  {$IFDEF THREADSAFE}
  Result := FThreadSafe;
  {$ELSE ~THREADSAFE}
  Result := False;
  {$ENDIF ~THREADSAFE}
end;

procedure TJclAbstractContainerBase.Grow;
begin
  // override to customize
  SetCapacity(CalcGrowCapacity(FCapacity, FSize));
end;

function TJclAbstractContainerBase.IntfClone: IInterface;
var
  NewContainer: TJclAbstractContainerBase;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewContainer := CreateEmptyContainer;
    AssignDataTo(NewContainer);
    Result := NewContainer;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAbstractContainerBase.Pack;
begin
  // override to customize
  SetCapacity(CalcPackCapacity(FCapacity, FSize));
end;

procedure TJclAbstractContainerBase.SetAllowDefaultElements(Value: Boolean);
begin
  FAllowDefaultElements := Value;
end;

procedure TJclAbstractContainerBase.SetAutoGrowParameter(Value: Integer);
begin
  FAutoGrowParameter := Value;
end;

procedure TJclAbstractContainerBase.SetAutoGrowStrategy(Value: TJclAutoGrowStrategy);
begin
  FAutoGrowStrategy := Value;
end;

procedure TJclAbstractContainerBase.SetAutoPackParameter(Value: Integer);
begin
  FAutoPackParameter := Value;
end;

procedure TJclAbstractContainerBase.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  FAutoPackStrategy := Value;
end;

procedure TJclAbstractContainerBase.SetCapacity(Value: Integer);
begin
  FCapacity := Value;
end;

procedure TJclAbstractContainerBase.SetDuplicates(Value: TDuplicates);
begin
  FDuplicates := Value;
end;

procedure TJclAbstractContainerBase.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TJclAbstractContainerBase.SetRemoveSingleElement(Value: Boolean);
begin
  FRemoveSingleElement := Value;
end;

procedure TJclAbstractContainerBase.SetReturnDefaultElements(Value: Boolean);
begin
  FReturnDefaultElements := Value;
end;

procedure TJclAbstractContainerBase.SetThreadSafe(Value: Boolean);
begin
  {$IFDEF THREADSAFE}
  FThreadSafe := Value;
  {$ELSE ~THREADSAFE}
  if Value then
    raise EJclOperationNotSupportedError.Create;
  {$ENDIF ~THREADSAFE}
end;

//=== { TJclAbstractIterator } ===============================================

constructor TJclAbstractIterator.Create(AValid: Boolean);
begin
  inherited Create;
  FValid := AValid;
end;

procedure TJclAbstractIterator.Assign(const Source: IJclAbstractIterator);
begin
  Source.AssignTo(Self);
end;

procedure TJclAbstractIterator.AssignPropertiesTo(Dest: TJclAbstractIterator);
begin
  Dest.FValid := FValid;
end;

procedure TJclAbstractIterator.AssignTo(const Dest: IJclAbstractIterator);
var
  DestObject: TObject;
begin
  DestObject := Dest.GetIteratorReference;
  if DestObject is TJclAbstractIterator then
    AssignPropertiesTo(TJclAbstractIterator(DestObject))
  else
    raise EJclAssignError.Create;
end;

procedure TJclAbstractIterator.CheckValid;
begin
  if not Valid then
    raise EJclIllegalStateOperationError.Create;
end;

function TJclAbstractIterator.ObjectClone: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyIterator;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAbstractIterator.GetIteratorReference: TObject;
begin
  Result := Self;
end;

function TJclAbstractIterator.IntfClone: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyIterator;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclIntfAbstractContainer } ==========================================

procedure TJclIntfAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclIntfAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclIntfAbstractContainer then
  begin
    ADest := TJclIntfAbstractContainer(Dest);
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclIntfAbstractContainer.FreeObject(var AInterface: IInterface): IInterface;
begin
  Result := AInterface;
  AInterface := nil;
end;

function TJclIntfAbstractContainer.GetCompare: TIntfCompare;
begin
  Result := FCompare;
end;

function TJclIntfAbstractContainer.GetEqualityCompare: TIntfEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclIntfAbstractContainer.GetHashConvert: TIntfHashConvert;
begin
  Result := FHashConvert;
end;

function TJclIntfAbstractContainer.Hash(const AInterface: IInterface): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AInterface)
  else
    Result := Integer(AInterface);
end;

function TJclIntfAbstractContainer.ItemsCompare(const A, B: IInterface): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclIntfAbstractContainer.ItemsEqual(const A, B: IInterface): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := Integer(A) = Integer(B);
end;

procedure TJclIntfAbstractContainer.SetCompare(Value: TIntfCompare);
begin
  FCompare := Value;
end;

procedure TJclIntfAbstractContainer.SetEqualityCompare(Value: TIntfEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclIntfAbstractContainer.SetHashConvert(Value: TIntfHashConvert);
begin
  FHashConvert := Value;
end;

//=== { TJclStrAbstractContainer } ===========================================

procedure TJclStrAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclStrAbstractContainer then
    TJclStrAbstractContainer(Dest).SetCaseSensitive(GetCaseSensitive);
end;

function TJclStrAbstractContainer.GetCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

procedure TJclStrAbstractContainer.SetCaseSensitive(Value: Boolean);
begin
  FCaseSensitive := Value;
end;

//=== { TJclAnsiStrAbstractContainer } =======================================

procedure TJclAnsiStrAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclAnsiStrAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclAnsiStrAbstractContainer then
  begin
    ADest := TJclAnsiStrAbstractContainer(Dest);
    ADest.Encoding := Encoding;
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclAnsiStrAbstractContainer.FreeString(var AString: AnsiString): AnsiString;
begin
  Result := AString;
  AString := '';
end;

function TJclAnsiStrAbstractContainer.GetCompare: TAnsiStrCompare;
begin
  Result := FCompare;
end;

function TJclAnsiStrAbstractContainer.GetEncoding: TJclAnsiStrEncoding;
begin
  Result := FEncoding;
end;

function TJclAnsiStrAbstractContainer.GetEqualityCompare: TAnsiStrEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclAnsiStrAbstractContainer.GetHashConvert: TAnsiStrHashConvert;
begin
  Result := FHashConvert;
end;

function TJclAnsiStrAbstractContainer.Hash(const AString: AnsiString): Integer;
var
  I: Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AString)
  else
  begin
    Result := 0;
    case FEncoding of
      seISO:
        if FCaseSensitive then
          for I := 1 to Length(AString) do
            Inc(Result, Ord(AString[I]) * (I - 1) * 256)
        else
          for I := 1 to Length(AString) do
            Inc(Result, Ord(UpCase(AString[I])) * (I - 1) * 256);
      //seUTF8:
    else
      raise EJclOperationNotSupportedError.Create;
    end;
  end;
end;

function TJclAnsiStrAbstractContainer.ItemsCompare(const A, B: AnsiString): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
  begin
    case FEncoding of
      seISO:
        if FCaseSensitive then
          Result := CompareStr(A, B)
        else
          Result := CompareText(A, B);
      //seUTF8:
    else
      raise EJclOperationNotSupportedError.Create;
    end;
  end;
end;

function TJclAnsiStrAbstractContainer.ItemsEqual(const A, B: AnsiString): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
  begin
    case FEncoding of
      seISO:
        if FCaseSensitive then
          Result := CompareStr(A, B) = 0
        else
          Result := CompareText(A, B) = 0;
      //seUTF8:
    else
      raise EJclOperationNotSupportedError.Create;
    end;
  end;
end;

procedure TJclAnsiStrAbstractContainer.SetCompare(Value: TAnsiStrCompare);
begin
  FCompare := Value;
end;

procedure TJclAnsiStrAbstractContainer.SetEncoding(Value: TJclAnsiStrEncoding);
begin
  FEncoding := Value;
end;

procedure TJclAnsiStrAbstractContainer.SetEqualityCompare(Value: TAnsiStrEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclAnsiStrAbstractContainer.SetHashConvert(Value: TAnsiStrHashConvert);
begin
  FHashConvert := Value;
end;

//=== { TJclWideStrContainer } ===============================================

procedure TJclWideStrAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclWideStrAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclWideStrAbstractContainer then
  begin
    ADest := TJclWideStrAbstractContainer(Dest);
    ADest.Encoding := Encoding;
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclWideStrAbstractContainer.FreeString(var AString: WideString): WideString;
begin
  Result := AString;
  AString := '';
end;

function TJclWideStrAbstractContainer.GetCompare: TWideStrCompare;
begin
  Result := FCompare;
end;

function TJclWideStrAbstractContainer.GetEncoding: TJclWideStrEncoding;
begin
  Result := FEncoding;
end;

function TJclWideStrAbstractContainer.GetEqualityCompare: TWideStrEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclWideStrAbstractContainer.GetHashConvert: TWideStrHashConvert;
begin
  Result := FHashConvert;
end;

function TJclWideStrAbstractContainer.Hash(const AString: WideString): Integer;
var
  I: Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AString)
  else
  begin
    Result := 0;
    case FEncoding of
      weUCS2:
        //if FCaseSensitive then
          for I := 1 to Length(AString) do
            Inc(Result, Ord(AString[I]) * (I - 1) * 65536)
        //else
        //  for I := 1 to Length(AString) do
        //    Inc(Result, Ord(AString[I]) * (I - 1) * 65536); // TODO: case folding
      //weUTF16:
    else
      raise EJclOperationNotSupportedError.Create;
    end;
  end;
end;

function TJclWideStrAbstractContainer.ItemsCompare(const A, B: WideString): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
  begin
    case FEncoding of
      weUCS2:
        if FCaseSensitive then
          Result := WideCompareStr(A, B)
        else
          Result := WideCompareText(A, B);
      //weUTF16:
    else
      raise EJclOperationNotSupportedError.Create;
    end;
  end;
end;

function TJclWideStrAbstractContainer.ItemsEqual(const A, B: WideString): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
  begin
    case FEncoding of
      weUCS2:
        if FCaseSensitive then
          Result := WideCompareStr(A, B) = 0
        else
          Result := WideCompareText(A, B) = 0;
      //weUTF16:
    else
      raise EJclOperationNotSupportedError.Create;
    end;
  end;
end;

procedure TJclWideStrAbstractContainer.SetCompare(Value: TWideStrCompare);
begin
  FCompare := Value;
end;

procedure TJclWideStrAbstractContainer.SetEncoding(Value: TJclWideStrEncoding);
begin
  FEncoding := Value;
end;

procedure TJclWideStrAbstractContainer.SetEqualityCompare(Value: TWideStrEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclWideStrAbstractContainer.SetHashConvert(Value: TWideStrHashConvert);
begin
  FHashConvert := Value;
end;

//=== { TJclSingleAbstractContainer } ========================================

procedure TJclSingleAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclSingleAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclSingleAbstractContainer then
  begin
    ADest := TJclSingleAbstractContainer(Dest);
    ADest.Precision := Precision;
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclSingleAbstractContainer.FreeSingle(var AValue: Single): Single;
begin
  Result := AValue;
  AValue := 0.0;
end;

function TJclSingleAbstractContainer.GetCompare: TSingleCompare;
begin
  Result := FCompare;
end;

function TJclSingleAbstractContainer.GetEqualityCompare: TSingleEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclSingleAbstractContainer.GetHashConvert: TSingleHashConvert;
begin
  Result := FHashConvert;
end;

function TJclSingleAbstractContainer.GetPrecision: Single;
begin
  Result := FPrecision;
end;

function TJclSingleAbstractContainer.Hash(const AValue: Single): Integer;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AValue)
  else
    Result := Round(MaxInt * Frac(AValue * A));
end;

function TJclSingleAbstractContainer.ItemsCompare(const A, B: Single): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
  if Abs(A - B) <= FPrecision then
    Result := 0
  else
  if A > B then
    Result := 1
  else
    Result := -1;
end;

function TJclSingleAbstractContainer.ItemsEqual(const A, B: Single): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := Abs(A - B) <= FPrecision;
end;

procedure TJclSingleAbstractContainer.SetCompare(Value: TSingleCompare);
begin
  FCompare := Value;
end;

procedure TJclSingleAbstractContainer.SetEqualityCompare(Value: TSingleEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclSingleAbstractContainer.SetHashConvert(Value: TSingleHashConvert);
begin
  FHashConvert := Value;
end;

procedure TJclSingleAbstractContainer.SetPrecision(const Value: Single);
begin
  FPrecision := Value;
end;

//=== { TJclDoubleAbstractContainer } ========================================

procedure TJclDoubleAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclDoubleAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclDoubleAbstractContainer then
  begin
    ADest := TJclDoubleAbstractContainer(Dest);
    ADest.Precision := Precision;
    ADest.Compare := Compare;
    ADest.EqualityCompare := EqualityCompare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclDoubleAbstractContainer.FreeDouble(var AValue: Double): Double;
begin
  Result := AValue;
  AValue := 0.0;
end;

function TJclDoubleAbstractContainer.GetCompare: TDoubleCompare;
begin
  Result := FCompare;
end;

function TJclDoubleAbstractContainer.GetEqualityCompare: TDoubleEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclDoubleAbstractContainer.GetHashConvert: TDoubleHashConvert;
begin
  Result := FHashConvert;
end;

function TJclDoubleAbstractContainer.GetPrecision: Double;
begin
  Result := FPrecision;
end;

function TJclDoubleAbstractContainer.Hash(const AValue: Double): Integer;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AValue)
  else
    Result := Round(MaxInt * Frac(AValue * A));
end;

function TJclDoubleAbstractContainer.ItemsCompare(const A, B: Double): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
  if Abs(A - B) <= FPrecision then
    Result := 0
  else
  if A > B then
    Result := 1
  else
    Result := -1;
end;

function TJclDoubleAbstractContainer.ItemsEqual(const A, B: Double): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := Abs(A - B) <= FPrecision;
end;

procedure TJclDoubleAbstractContainer.SetCompare(Value: TDoubleCompare);
begin
  FCompare := Value;
end;

procedure TJclDoubleAbstractContainer.SetEqualityCompare(Value: TDoubleEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclDoubleAbstractContainer.SetHashConvert(Value: TDoubleHashConvert);
begin
  FHashConvert := Value;
end;

procedure TJclDoubleAbstractContainer.SetPrecision(const Value: Double);
begin
  FPrecision := Value;
end;

//=== { TJclExtendedAbstractContainer } ======================================

procedure TJclExtendedAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclExtendedAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclExtendedAbstractContainer then
  begin
    ADest := TJclExtendedAbstractContainer(Dest);
    ADest.Precision := Precision;
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclExtendedAbstractContainer.FreeExtended(var AValue: Extended): Extended;
begin
  Result := AValue;
  AValue := 0.0;
end;

function TJclExtendedAbstractContainer.GetCompare: TExtendedCompare;
begin
  Result := FCompare;
end;

function TJclExtendedAbstractContainer.GetEqualityCompare: TExtendedEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclExtendedAbstractContainer.GetHashConvert: TExtendedHashConvert;
begin
  Result := FHashConvert;
end;

function TJclExtendedAbstractContainer.GetPrecision: Extended;
begin
  Result := FPrecision;
end;

function TJclExtendedAbstractContainer.Hash(const AValue: Extended): Integer;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AValue)
  else
    Result := Round(MaxInt * Frac(AValue * A));
end;

function TJclExtendedAbstractContainer.ItemsCompare(const A, B: Extended): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
  if Abs(A - B) <= FPrecision then
    Result := 0
  else
  if A > B then
    Result := 1
  else
    Result := -1;
end;

function TJclExtendedAbstractContainer.ItemsEqual(const A, B: Extended): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := Abs(A - B) <= FPrecision;
end;

procedure TJclExtendedAbstractContainer.SetCompare(Value: TExtendedCompare);
begin
  FCompare := Value;
end;

procedure TJclExtendedAbstractContainer.SetEqualityCompare(Value: TExtendedEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclExtendedAbstractContainer.SetHashConvert(Value: TExtendedHashConvert);
begin
  FHashConvert := Value;
end;

procedure TJclExtendedAbstractContainer.SetPrecision(const Value: Extended);
begin
  FPrecision := Value;
end;

//=== { TJclIntegerAbstractContainer } =======================================

procedure TJclIntegerAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclIntegerAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclIntegerAbstractContainer then
  begin
    ADest := TJclIntegerAbstractContainer(Dest);
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclIntegerAbstractContainer.FreeInteger(var AValue: Integer): Integer;
begin
  Result := AValue;
  AValue := 0;
end;

function TJclIntegerAbstractContainer.GetCompare: TIntegerCompare;
begin
  Result := FCompare;
end;

function TJclIntegerAbstractContainer.GetEqualityCompare: TIntegerEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclIntegerAbstractContainer.GetHashConvert: TIntegerHashConvert;
begin
  Result := FHashConvert;
end;

function TJclIntegerAbstractContainer.Hash(AValue: Integer): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AValue)
  else
    Result := AValue;
end;

function TJclIntegerAbstractContainer.ItemsCompare(A, B: Integer): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
  if A > B then
    Result := 1
  else
  if A < B then
    Result := -1
  else
    Result := 0;
end;

function TJclIntegerAbstractContainer.ItemsEqual(A, B: Integer): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := A = B;
end;

procedure TJclIntegerAbstractContainer.SetCompare(Value: TIntegerCompare);
begin
  FCompare := Value;
end;

procedure TJclIntegerAbstractContainer.SetEqualityCompare(Value: TIntegerEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclIntegerAbstractContainer.SetHashConvert(Value: TIntegerHashConvert);
begin
  FHashConvert := Value;
end;

//=== { TJclCardinalAbstractContainer } ======================================

procedure TJclCardinalAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclCardinalAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclCardinalAbstractContainer then
  begin
    ADest := TJclCardinalAbstractContainer(Dest);
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclCardinalAbstractContainer.FreeCardinal(var AValue: Cardinal): Cardinal;
begin
  Result := AValue;
  AValue := 0;
end;

function TJclCardinalAbstractContainer.GetCompare: TCardinalCompare;
begin
  Result := FCompare;
end;

function TJclCardinalAbstractContainer.GetEqualityCompare: TCardinalEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclCardinalAbstractContainer.GetHashConvert: TCardinalHashConvert;
begin
  Result := FHashConvert;
end;

function TJclCardinalAbstractContainer.Hash(AValue: Cardinal): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AValue)
  else
    Result := AValue and MaxInt;
end;

function TJclCardinalAbstractContainer.ItemsCompare(A, B: Cardinal): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
  if A > B then
    Result := 1
  else
  if A < B then
    Result := -1
  else
    Result := 0;
end;

function TJclCardinalAbstractContainer.ItemsEqual(A, B: Cardinal): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := A = B;
end;

procedure TJclCardinalAbstractContainer.SetCompare(Value: TCardinalCompare);
begin
  FCompare := Value;
end;

procedure TJclCardinalAbstractContainer.SetEqualityCompare(Value: TCardinalEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclCardinalAbstractContainer.SetHashConvert(Value: TCardinalHashConvert);
begin
  FHashConvert := Value;
end;

//=== { TJclInt64AbstractContainer } =========================================

procedure TJclInt64AbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclInt64AbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclInt64AbstractContainer then
  begin
    ADest := TJclInt64AbstractContainer(Dest);
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclInt64AbstractContainer.FreeInt64(var AValue: Int64): Int64;
begin
  Result := AValue;
  AValue := 0;
end;

function TJclInt64AbstractContainer.GetCompare: TInt64Compare;
begin
  Result := FCompare;
end;

function TJclInt64AbstractContainer.GetEqualityCompare: TInt64EqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclInt64AbstractContainer.GetHashConvert: TInt64HashConvert;
begin
  Result := FHashConvert;
end;

function TJclInt64AbstractContainer.Hash(const AValue: Int64): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AValue)
  else
    Result := AValue and MaxInt;
end;

function TJclInt64AbstractContainer.ItemsCompare(const A, B: Int64): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
  if A > B then
    Result := 1
  else
  if A < B then
    Result := -1
  else
    Result := 0;
end;

function TJclInt64AbstractContainer.ItemsEqual(const A, B: Int64): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := A = B;
end;

procedure TJclInt64AbstractContainer.SetCompare(Value: TInt64Compare);
begin
  FCompare := Value;
end;

procedure TJclInt64AbstractContainer.SetEqualityCompare(Value: TInt64EqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclInt64AbstractContainer.SetHashConvert(Value: TInt64HashConvert);
begin
  FHashConvert := Value;
end;

{$IFNDEF CLR}

//=== { TJclPtrAbstractContainer } ===========================================

procedure TJclPtrAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclPtrAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclPtrAbstractContainer then
  begin
    ADest := TJclPtrAbstractContainer(Dest);
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclPtrAbstractContainer.FreePointer(var APtr: Pointer): Pointer;
begin
  Result := APtr;
  APtr := nil;
end;

function TJclPtrAbstractContainer.GetCompare: TPtrCompare;
begin
  Result := FCompare;
end;

function TJclPtrAbstractContainer.GetEqualityCompare: TPtrEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclPtrAbstractContainer.GetHashConvert: TPtrHashConvert;
begin
  Result := FHashConvert;
end;

function TJclPtrAbstractContainer.Hash(APtr: Pointer): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(APtr)
  else
    Result := Integer(APtr) and MaxInt;
end;

function TJclPtrAbstractContainer.ItemsCompare(A, B: Pointer): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclPtrAbstractContainer.ItemsEqual(A, B: Pointer): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := Integer(A) = Integer(B);
end;
procedure TJclPtrAbstractContainer.SetCompare(Value: TPtrCompare);
begin
  FCompare := Value;
end;

procedure TJclPtrAbstractContainer.SetEqualityCompare(Value: TPtrEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclPtrAbstractContainer.SetHashConvert(Value: TPtrHashConvert);
begin
  FHashConvert := Value;
end;

{$ENDIF ~CLR}

//=== { TJclAbstractContainer } ==============================================

constructor TJclAbstractContainer.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

procedure TJclAbstractContainer.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclAbstractContainer;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclAbstractContainer then
  begin
    ADest := TJclAbstractContainer(Dest);
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclAbstractContainer.FreeObject(var AObject: TObject): TObject;
begin
  if FOwnsObjects then
  begin
    Result := nil;
    FreeAndNil(AObject);
  end
  else
  begin
    Result := AObject;
    AObject := nil;
  end;
end;

function TJclAbstractContainer.GetCompare: TCompare;
begin
  Result := FCompare;
end;

function TJclAbstractContainer.GetEqualityCompare: TEqualityCompare;
begin
  Result := FEqualityCompare;
end;

function TJclAbstractContainer.GetHashConvert: THashConvert;
begin
  Result := FHashConvert;
end;

function TJclAbstractContainer.GetOwnsObjects: Boolean;
begin
  Result := FOwnsObjects;
end;

function TJclAbstractContainer.Hash(AObject: TObject): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AObject)
  else
    Result := Integer(AObject);
end;

function TJclAbstractContainer.ItemsCompare(A, B: TObject): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
  if Integer(A) > Integer(B) then
    Result := 1
  else
  if Integer(A) < Integer(B) then
    Result := -1
  else
    Result := 0;
end;

function TJclAbstractContainer.ItemsEqual(A, B: TObject): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := Integer(A) = Integer(B);
end;

procedure TJclAbstractContainer.SetCompare(Value: TCompare);
begin
  FCompare := Value;
end;

procedure TJclAbstractContainer.SetEqualityCompare(Value: TEqualityCompare);
begin
  FEqualityCompare := Value;
end;

procedure TJclAbstractContainer.SetHashConvert(Value: THashConvert);
begin
  FHashConvert := Value;
end;

{$IFDEF SUPPORTS_GENERICS}
//=== { TJclAbstractContainer<T> } ===========================================

constructor TJclAbstractContainer<T>.Create(AOwnsItems: Boolean);
begin
  inherited Create;
  FOwnsItems := AOwnsItems;
end;

procedure TJclAbstractContainer<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclAbstractContainer<T>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclAbstractContainer<T> then
  begin
    ADest := TJclAbstractContainer<T>(Dest);
    ADest.EqualityCompare := EqualityCompare;
    ADest.Compare := Compare;
    ADest.HashConvert := HashConvert;
  end;
end;

function TJclAbstractContainer<T>.FreeItem(var AItem: T): T;
begin
  if FOwnsItems then
  begin
    Result := Default(T);
    FreeAndNil(AItem);
  end
  else
  begin
    Result := AItem;
    AItem := Default(T);
  end;
end;

function TJclAbstractContainer<T>.GetCompare: TCompare<T>;
begin
  Result := FCompare;
end;

function TJclAbstractContainer<T>.GetEqualityCompare: TEqualityCompare<T>;
begin
  Result := FEqualityCompare;
end;

function TJclAbstractContainer<T>.GetHashConvert: THashConvert<T>;
begin
  Result := FHashConvert;
end;

function TJclAbstractContainer<T>.GetOwnsItems: Boolean;
begin
  Result := FOwnsItems;
end;

function TJclAbstractContainer<T>.Hash(const AItem: T): Integer;
begin
  if Assigned(FHashConvert) then
    Result := FHashConvert(AItem)
  else
    raise EJclOperationNotSupportedError.Create;
end;

function TJclAbstractContainer<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    raise EJclOperationNotSupportedError.Create;
end;

function TJclAbstractContainer<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAbstractContainer<T>.SetCompare(Value: TCompare<T>);
begin
  FCompare := Value;
end;

procedure TJclAbstractContainer<T>.SetEqualityCompare(Value: TEqualityCompare<T>);
begin
  FEqualityCompare := Value;
end;

procedure TJclAbstractContainer<T>.SetHashConvert(Value: THashConvert<T>);
begin
  FHashConvert := Value;
end;

{$ENDIF SUPPORTS_GENERICS}

//=== { TJclAnsiStrCollection } ==============================================

// TODO: common implementation, need a function to search for a string starting from
// a predefined index
procedure TJclAnsiStrAbstractCollection.AppendDelimited(const AString, Separator: AnsiString);
{$IFDEF CLR}
var
  I, StartIndex: Integer;
  BString: string;
begin
  I := Pos(Separator, AString);
  if I <> 0 then
  begin
    BString := AString;
    Dec(I); // to .NET string index base
    StartIndex := 0;
    repeat
      Add(BString.Substring(StartIndex, I - StartIndex + 1));
      StartIndex := I + 1;
      I := BString.IndexOf(Separator, StartIndex);
    until I < 0;
  end
  else
    Add(AString);
end;
{$ELSE}
var
  Item: AnsiString;
  SepLen: Integer;
  PString, PSep, PPos: PChar;
begin
  PString := PChar(AString);
  PSep := PChar(Separator);
  PPos := StrPos(PString, PSep);
  if PPos <> nil then
  begin
    SepLen := StrLen(PSep);
    repeat
      //SetLength(Item, PPos - PString + 1);
      SetLength(Item, PPos - PString);
      Move(PString^, Item[1], PPos - PString);
      //Item[PPos - PString + 1] := #0;
      Add(Item);
      PString := PPos + SepLen;
      PPos := StrPos(PString, PSep);
    until PPos = nil;
    if StrLen(PString) > 0 then //ex. hello#world
      Add(PString);
  end
  else //There isnt a Separator in AString
    Add(AString);
end;
{$ENDIF CLR}

procedure TJclAnsiStrAbstractCollection.AppendFromStrings(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Add(Strings[I]);
end;

procedure TJclAnsiStrAbstractCollection.AppendToStrings(Strings: TStrings);
var
  It: IJclAnsiStrIterator;
begin
  It := First;
  Strings.BeginUpdate;
  try
    while It.HasNext do
      Strings.Add(It.Next);
  finally
    Strings.EndUpdate;
  end;
end;

function TJclAnsiStrAbstractCollection.GetAsDelimited(const Separator: AnsiString): AnsiString;
var
  It: IJclAnsiStrIterator;
begin
  It := First;
  Result := '';
  if It.HasNext then
    Result := It.Next;
  while It.HasNext do
    Result := Result + Separator + It.Next;
end;

function TJclAnsiStrAbstractCollection.GetAsStrings: TStrings;
begin
  Result := TStringList.Create;
  try
    AppendToStrings(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TJclAnsiStrAbstractCollection.LoadDelimited(const AString, Separator: AnsiString);
begin
  Clear;
  AppendDelimited(AString, Separator);
end;

procedure TJclAnsiStrAbstractCollection.LoadFromStrings(Strings: TStrings);
begin
  Clear;
  AppendFromStrings(Strings);
end;

procedure TJclAnsiStrAbstractCollection.SaveToStrings(Strings: TStrings);
begin
  Strings.Clear;
  AppendToStrings(Strings);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

