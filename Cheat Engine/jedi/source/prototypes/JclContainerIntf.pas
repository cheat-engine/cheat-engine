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
{ The Original Code is DCL_intf.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-01-15 23:30:26 +0100 (mar., 15 janv. 2008)                         $ }
{ Revision:      $Rev:: 2309                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclContainerIntf;

{$I jcl.inc}
{$I containers\JclContainerIntf.int}
interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes,
  JclBase;

{$IFDEF BCB6}
{$DEFINE BUGGY_DEFAULT_INDEXED_PROP}
{$ENDIF BCB6}
{$IFDEF BCB10}
{$DEFINE BUGGY_DEFAULT_INDEXED_PROP}
{$ENDIF BCB10}
{$IFDEF BCB11}
{$DEFINE BUGGY_DEFAULT_INDEXED_PROP}
{$ENDIF BCB11}

const
  DefaultContainerCapacity = 16;

type
  // function pointer types

  // apply functions Type -> Type
  {$JPPEXPANDMACRO APPLYFUNCTION(TIntfApplyFunction,const ,AInterface,IInterface)}
  {$JPPEXPANDMACRO APPLYFUNCTION(TAnsiStrApplyFunction,const ,AString,AnsiString)}
  {$JPPEXPANDMACRO APPLYFUNCTION(TWideStrApplyFunction,const ,AString,WideString)}
  {$IFDEF CONTAINER_ANSISTR}
  TStrApplyFunction = TAnsiStrApplyFunction;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TStrApplyFunction = TWideStrApplyFunction;
  {$ENDIF CONTAINER_WIDESTR}
  {$JPPEXPANDMACRO APPLYFUNCTION(TSingleApplyFunction,const ,AValue,Single)}
  {$JPPEXPANDMACRO APPLYFUNCTION(TDoubleApplyFunction,const ,AValue,Double)}
  {$JPPEXPANDMACRO APPLYFUNCTION(TExtendedApplyFunction,const ,AValue,Extended)}
  {$IFDEF MATH_SINGLE_PRECISION}
  TFloatApplyFunction = TSingleApplyFunction;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TFloatApplyFunction = TDoubleApplyFunction;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TFloatApplyFunction = TExtendedApplyFunction;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$JPPEXPANDMACRO APPLYFUNCTION(TIntegerApplyFunction,,AValue,Integer)}
  {$JPPEXPANDMACRO APPLYFUNCTION(TCardinalApplyFunction,,AValue,Cardinal)}
  {$JPPEXPANDMACRO APPLYFUNCTION(TInt64ApplyFunction,const ,AValue,Int64)}
  {$IFNDEF CLR}
  {$JPPEXPANDMACRO APPLYFUNCTION(TPtrApplyFunction,,APtr,Pointer)}
  {$ENDIF ~CLR}
  {$JPPEXPANDMACRO APPLYFUNCTION(TApplyFunction,,AObject,TObject)}
  {$IFDEF SUPPORTS_GENERICS}
  {$JPPEXPANDMACRO APPLYFUNCTION(TApplyFunction<T>,const ,AItem,T)}
  {$ENDIF SUPPORTS_GENERICS}

  // comparison functions Type -> Type -> Integer
  {$JPPEXPANDMACRO COMPAREFUNCTION(TIntfCompare,const ,IInterface)}
  {$JPPEXPANDMACRO COMPAREFUNCTION(TAnsiStrCompare,const ,AnsiString)}
  {$JPPEXPANDMACRO COMPAREFUNCTION(TWideStrCompare,const ,WideString)}
  {$IFDEF CONTAINER_ANSISTR}
  TStrCompare = TAnsiStrCompare;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TStrCompare = TWideStrCompare;
  {$ENDIF CONTAINER_WIDESTR}
  {$JPPEXPANDMACRO COMPAREFUNCTION(TSingleCompare,const ,Single)}
  {$JPPEXPANDMACRO COMPAREFUNCTION(TDoubleCompare,const ,Double)}
  {$JPPEXPANDMACRO COMPAREFUNCTION(TExtendedCompare,const ,Extended)}
  {$IFDEF MATH_SINGLE_PRECISION}
  TFloatCompare = TSingleCompare;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TFloatCompare = TDoubleCompare;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TFloatCompare = TExtendedCompare;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$JPPEXPANDMACRO COMPAREFUNCTION(TIntegerCompare,,Integer)}
  {$JPPEXPANDMACRO COMPAREFUNCTION(TCardinalCompare,,Cardinal)}
  {$JPPEXPANDMACRO COMPAREFUNCTION(TInt64Compare,,Int64)}
  {$IFNDEF CLR}
  {$JPPEXPANDMACRO COMPAREFUNCTION(TPtrCompare,,Pointer)}
  {$ENDIF ~CLR}
  {$JPPEXPANDMACRO COMPAREFUNCTION(TCompare,,TObject)}
  {$IFDEF SUPPORTS_GENERICS}
  {$JPPEXPANDMACRO COMPAREFUNCTION(TCompare<T>,const ,T)}
  {$ENDIF SUPPORTS_GENERICS}

  // comparison for equality functions Type -> Type -> Boolean
  {$JPPEXPANDMACRO EQUALITYCOMPAREFUNCTION(TIntfEqualityCompare,const ,IInterface)}
  {$JPPEXPANDMACRO EQUALITYCOMPAREFUNCTION(TAnsiStrEqualityCompare,const ,AnsiString)}
  {$JPPEXPANDMACRO EQUALITYCOMPAREFUNCTION(TWideStrEqualityCompare,const ,WideString)}
  {$IFDEF CONTAINER_ANSISTR}
  TStrEqualityCompare = TAnsiStrEqualityCompare;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TStrEqualityCompare = TWideStrEqualityCompare;
  {$ENDIF CONTAINER_WIDESTR}
  {$JPPEXPANDMACRO EQUALITYCOMPAREFUNCTION(TSingleEqualityCompare,const ,Single)}
  {$JPPEXPANDMACRO EQUALITYCOMPAREFUNCTION(TDoubleEqualityCompare,const ,Double)}
  {$JPPEXPANDMACRO EQUALITYCOMPAREFUNCTION(TExtendedEqualityCompare,const ,Extended)}
  {$IFDEF MATH_SINGLE_PRECISION}
  TFloatEqualityCompare = TSingleEqualityCompare;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TFloatEqualityCompare = TDoubleEqualityCompare;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TFloatEqualityCompare = TExtendedEqualityCompare;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$JPPEXPANDMACRO EQUALITYCOMPAREFUNCTION(TIntegerEqualityCompare,,Integer)}
  {$JPPEXPANDMACRO EQUALITYCOMPAREFUNCTION(TCardinalEqualityCompare,,Cardinal)}
  {$JPPEXPANDMACRO EQUALITYCOMPAREFUNCTION(TInt64EqualityCompare,const ,Int64)}
  {$IFNDEF CLR}
  {$JPPEXPANDMACRO EQUALITYCOMPAREFUNCTION(TPtrEqualityCompare,,Pointer)}
  {$ENDIF ~CLR}
  {$JPPEXPANDMACRO EQUALITYCOMPAREFUNCTION(TEqualityCompare,,TObject)}
  {$IFDEF SUPPORTS_GENERICS}
  {$JPPEXPANDMACRO EQUALITYCOMPAREFUNCTION(TEqualityCompare<T>,const ,T)}
  {$ENDIF SUPPORTS_GENERICS}

  // hash functions Type -> Integer
  {$JPPEXPANDMACRO HASHFUNCTION(TIntfHashConvert,const ,AInterface,IInterface)}
  {$JPPEXPANDMACRO HASHFUNCTION(TAnsiStrHashConvert,const ,AString,AnsiString)}
  {$JPPEXPANDMACRO HASHFUNCTION(TWideStrHashConvert,const ,AString,WideString)}
  {$IFDEF CONTAINER_ANSISTR}
  TStrHashConvert = TAnsiStrHashConvert;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TStrHashConvert = TWideStrHashConvert;
  {$ENDIF CONTAINER_WIDESTR}
  {$JPPEXPANDMACRO HASHFUNCTION(TSingleHashConvert,const ,AValue,Single)}
  {$JPPEXPANDMACRO HASHFUNCTION(TDoubleHashConvert,const ,AValue,Double)}
  {$JPPEXPANDMACRO HASHFUNCTION(TExtendedHashConvert,const ,AValue,Extended)}
  {$IFDEF MATH_SINGLE_PRECISION}
  TFloatHashConvert = TSingleHashConvert;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TFloatHashConvert = TDoubleHashConvert;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TFloatHashConvert = TExtendedHashConvert;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$JPPEXPANDMACRO HASHFUNCTION(TIntegerHashConvert,,AValue,Integer)}
  {$JPPEXPANDMACRO HASHFUNCTION(TCardinalHashConvert,,AValue,Cardinal)}
  {$JPPEXPANDMACRO HASHFUNCTION(TInt64HashConvert,const ,AValue,Int64)}
  {$IFNDEF CLR}
  {$JPPEXPANDMACRO HASHFUNCTION(TPtrHashConvert,,APtr,Pointer)}
  {$ENDIF ~CLR}
  {$JPPEXPANDMACRO HASHFUNCTION(THashConvert,,AObject,TObject)}
  {$IFDEF SUPPORTS_GENERICS}
  {$JPPEXPANDMACRO HASHFUNCTION(THashConvert<T>,const ,AItem,T)}
  {$ENDIF SUPPORTS_GENERICS}

  IJclLockable = interface
    ['{524AD65E-AE1B-4BC6-91C8-8181F0198BA9}']
    procedure ReadLock;
    procedure ReadUnlock;
    procedure WriteLock;
    procedure WriteUnlock;
  end;

  IJclAbstractIterator = interface{$IFDEF THREADSAFE}(IJclLockable){$ENDIF THREADSAFE}
    ['{1064D0B4-D9FC-475D-88BE-520490013B46}']
    procedure Assign(const Source: IJclAbstractIterator);
    procedure AssignTo(const Dest: IJclAbstractIterator);
    function GetIteratorReference: TObject;
  end;

  IJclContainer = interface{$IFDEF THREADSAFE}(IJclLockable){$ENDIF THREADSAFE}
    ['{C517175A-028E-486A-BF27-5EF7FC3101D9}']
    procedure Assign(const Source: IJclContainer);
    procedure AssignTo(const Dest: IJclContainer);
    function GetAllowDefaultElements: Boolean;
    function GetContainerReference: TObject;
    function GetDuplicates: TDuplicates;
    function GetReadOnly: Boolean;
    function GetRemoveSingleElement: Boolean;
    function GetReturnDefaultElements: Boolean;
    function GetThreadSafe: Boolean;
    procedure SetAllowDefaultElements(Value: Boolean);
    procedure SetDuplicates(Value: TDuplicates);
    procedure SetReadOnly(Value: Boolean);
    procedure SetRemoveSingleElement(Value: Boolean);
    procedure SetReturnDefaultElements(Value: Boolean);
    procedure SetThreadSafe(Value: Boolean);
    property AllowDefaultElements: Boolean read GetAllowDefaultElements write SetAllowDefaultElements;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property RemoveSingleElement: Boolean read GetRemoveSingleElement write SetRemoveSingleElement;
    property ReturnDefaultElements: Boolean read GetReturnDefaultElements write SetReturnDefaultElements;
    property ThreadSafe: Boolean read GetThreadSafe write SetThreadSafe;
  end;

  IJclStrContainer = interface(IJclContainer)
    ['{9753E1D7-F093-4D5C-8B32-40403F6F700E}']
    function GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(Value: Boolean);
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
  end;

  TJclAnsiStrEncoding = (seISO {, seUTF8}); // TODO: make JclUnicode compatible with Linux and .NET

  IJclAnsiStrContainer = interface(IJclStrContainer)
    ['{F8239357-B96F-46F1-A48E-B5DF25B5F1FA}']
    function GetEncoding: TJclAnsiStrEncoding;
    procedure SetEncoding(Value: TJclAnsiStrEncoding);
    property Encoding: TJclAnsiStrEncoding read GetEncoding write SetEncoding;
  end;

  IJclAnsiStrFlatContainer = interface(IJclAnsiStrContainer)
    ['{8A45A4D4-6317-4CDF-8314-C3E5CC6899F4}']
    procedure LoadFromStrings(Strings: TStrings);
    procedure SaveToStrings(Strings: TStrings);
    procedure AppendToStrings(Strings: TStrings);
    procedure AppendFromStrings(Strings: TStrings);
    function GetAsStrings: TStrings;
    function GetAsDelimited(const Separator: AnsiString = AnsiLineBreak): AnsiString;
    procedure AppendDelimited(const AString: AnsiString; const Separator: AnsiString = AnsiLineBreak);
    procedure LoadDelimited(const AString: AnsiString; const Separator: AnsiString = AnsiLineBreak);
  end;

  TJclWideStrEncoding = (weUCS2 {, wsUTF16}); // TODO: make JclUnicode compatible with Linux and .NET

  IJclWideStrContainer = interface(IJclStrContainer)
    ['{875E1AC4-CA22-46BC-8999-048E5B9BF11D}']
    function GetEncoding: TJclWideStrEncoding;
    procedure SetEncoding(Value: TJclWideStrEncoding);
    property Encoding: TJclWideStrEncoding read GetEncoding write SetEncoding;
  end;

  IJclWideStrFlatContainer = interface(IJclWideStrContainer)
    ['{5B001B93-CA1C-47A8-98B8-451CCB444930}']
    {procedure LoadFromStrings(Strings: TWideStrings);
    procedure SaveToStrings(Strings: TWideStrings);
    procedure AppendToStrings(Strings: TWideStrings);
    procedure AppendFromStrings(Strings: TWideStrings);
    function GetAsStrings: TWideStrings;
    function GetAsDelimited(const Separator: WideString = WideLineBreak): WideString;
    procedure AppendDelimited(const AString: WideString; const Separator: WideString = WideLineBreak);
    procedure LoadDelimited(const AString: WideString; const Separator: WideString = WideLineBreak);}
  end;

  IJclSingleContainer = interface(IJclContainer)
    ['{22BE88BD-87D1-4B4D-9FAB-F1B6D555C6A9}']
    function GetPrecision: Single;
    procedure SetPrecision(const Value: Single);
    property Precision: Single read GetPrecision write SetPrecision;
  end;

  IJclDoubleContainer = interface(IJclContainer)
    ['{372B9354-DF6D-4CAA-A5A9-C50E1FEE5525}']
    function GetPrecision: Double;
    procedure SetPrecision(const Value: Double);
    property Precision: Double read GetPrecision write SetPrecision;
  end;

  IJclExtendedContainer = interface(IJclContainer)
    ['{431A6482-FD5C-45A7-BE53-339A3CF75AC9}']
    function GetPrecision: Extended;
    procedure SetPrecision(const Value: Extended);
    property Precision: Extended read GetPrecision write SetPrecision;
  end;

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatContainer = IJclExtendedContainer;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatContainer = IJclDoubleContainer;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatContainer = IJclSingleContainer;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO EQUALITYCOMPARER(IJclIntfEqualityComparer,5CC2DF51-BE56-4D02-A171-31BAAC097632,TIntfEqualityCompare,const ,IInterface)}

{$JPPEXPANDMACRO EQUALITYCOMPARER(IJclAnsiStrEqualityComparer,E3DB9016-F0D0-4CE0-B156-4C5DCA47FD3B,TAnsiStrEqualityCompare,const ,AnsiString)}

{$JPPEXPANDMACRO EQUALITYCOMPARER(IJclWideStrEqualityComparer,2E5696C9-8374-4347-9DC9-B3722F47F5FB,TWideStrEqualityCompare,const ,WideString)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrEqualityComparer = IJclAnsiStrEqualityComparer;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrEqualityComparer = IJclWideStrEqualityComparer;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO EQUALITYCOMPARER(IJclSingleEqualityComparer,4835BC5B-1A87-4864-BFE1-778F3BAF26B1,TSingleEqualityCompare,const ,Single)}

{$JPPEXPANDMACRO EQUALITYCOMPARER(IJclDoubleEqualityComparer,15F0A9F0-D5DC-4978-8CDB-53B6E510262C,TDoubleEqualityCompare,const ,Double)}

{$JPPEXPANDMACRO EQUALITYCOMPARER(IJclExtendedEqualityComparer,149883D5-4138-4570-8C5C-99F186B7E646,TExtendedEqualityCompare,const ,Extended)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatEqualityComparer = IJclExtendedEqualityComparer;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatEqualityComparer = IJclDoubleEqualityComparer;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatEqualityComparer = IJclSingleEqualityComparer;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO EQUALITYCOMPARER(IJclIntegerEqualityComparer,AABC35E6-A779-4A44-B748-27BFCB34FDFB,TIntegerEqualityCompare,,Integer)}

{$JPPEXPANDMACRO EQUALITYCOMPARER(IJclCardinalEqualityComparer,B2DECF81-6ECE-4D9F-80E1-C8C884DB407C,TCardinalEqualityCompare,,Cardinal)}

{$JPPEXPANDMACRO EQUALITYCOMPARER(IJclInt64EqualityComparer,8B2825E2-0C81-42BA-AC0D-104344CE7E56,TInt64EqualityCompare,const ,Int64)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO EQUALITYCOMPARER(IJclPtrEqualityComparer,C6B7CBF9-ECD9-4D70-85CC-4E2367A1D806,TPtrEqualityCompare,,Pointer)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO EQUALITYCOMPARER(IJclEqualityComparer,82C67986-8365-44AB-8D56-7B0CF4F6B918,TEqualityCompare,,TObject)}

  {$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO EQUALITYCOMPARER(IJclEqualityComparer<T>,4AF79AD6-D9F4-424B-BEAA-68857F9222B4,TEqualityCompare<T>,const ,T)}
  {$ENDIF SUPPORTS_GENERICS}

{$JPPEXPANDMACRO COMPARER(IJclIntfComparer,EB41B843-184B-420D-B5DA-27D055B4CD55,TIntfCompare,const ,IInterface)}

{$JPPEXPANDMACRO COMPARER(IJclAnsiStrComparer,09063CBB-9226-4734-B2A0-A178C2343176,TAnsiStrCompare,const ,AnsiString)}

{$JPPEXPANDMACRO COMPARER(IJclWideStrComparer,7A24AEDA-25B1-4E73-B2E9-5D74011E4C9C,TWideStrCompare,const ,WideString)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrComparer = IJclAnsiStrComparer;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrComparer = IJclWideStrComparer;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO COMPARER(IJclSingleComparer,008225CE-075E-4450-B9DE-9863CB6D347C,TSingleCompare,const ,Single)}

{$JPPEXPANDMACRO COMPARER(IJclDoubleComparer,BC245D7F-7EB9-43D0-81B4-EE215486A5AA,TDoubleCompare,const ,Double)}

{$JPPEXPANDMACRO COMPARER(IJclExtendedComparer,92657C66-C18D-4BF8-A538-A3B0140320BB,TExtendedCompare,const ,Extended)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatComparer = IJclExtendedComparer;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatComparer = IJclDoubleComparer;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatComparer = IJclSingleComparer;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO COMPARER(IJclIntegerComparer,362C3A6A-CBC1-4D5F-8652-158913DC9865,TIntegerCompare,,Integer)}

{$JPPEXPANDMACRO COMPARER(IJclCardinalComparer,56E44725-00B9-4530-8CC2-72DCA9171EE0,TCardinalCompare,,Cardinal)}

{$JPPEXPANDMACRO COMPARER(IJclInt64Comparer,87C935BF-3A42-4F1F-A474-9C823939EE1C,TInt64Compare,const ,Int64)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO COMPARER(IJclPtrComparer,85557D4C-A036-477E-BA73-B5EEF43A8696,TPtrCompare,,Pointer)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO COMPARER(IJclComparer,7B376028-56DC-4C4A-86A9-1AC19E3EDF75,TCompare,,TObject)}

  {$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO COMPARER(IJclComparer<T>,830AFC8C-AA06-46F5-AABD-8EB46B2A9986,TCompare<T>,const ,T)}
  {$ENDIF SUPPORTS_GENERICS}

{$JPPEXPANDMACRO HASHCONVERTER(IJclIntfHashConverter,7BAA0791-3B45-4D0F-9CD8-D13B81694786,TIntfHashConvert,const ,AInterface,IInterface)}

{$JPPEXPANDMACRO HASHCONVERTER(IJclAnsiStrHashConverter,9841014E-8A31-4C79-8AD5-EB03C4E85533,TAnsiStrHashConvert,const ,AString,AnsiString)}

{$JPPEXPANDMACRO HASHCONVERTER(IJclWideStrHashConverter,2584118F-19AE-443E-939B-0DB18BCD0117,TWideStrHashConvert,const ,AString,WideString)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrHashConverter = IJclAnsiStrHashConverter;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrHashConverter = IJclWideStrHashConverter;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO HASHCONVERTER(IJclSingleHashConverter,20F0E481-F1D2-48B6-A95D-FBB56AF119F5,TSingleHashConvert,const ,AValue,Single)}

{$JPPEXPANDMACRO HASHCONVERTER(IJclDoubleHashConverter,193A2881-535B-4AF4-B0C3-6845A2800F80,TDoubleHashConvert,const ,AValue,Double)}

{$JPPEXPANDMACRO HASHCONVERTER(IJclExtendedHashConverter,77CECDB9-2774-4FDC-8E5A-A80325626434,TExtendedHashConvert,const ,AValue,Extended)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatHashConverter = IJclExtendedHashConverter;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatHashConverter = IJclDoubleHashConverter;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatHashConverter = IJclSingleHashConverter;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO HASHCONVERTER(IJclIntegerHashConverter,92C540B2-C16C-47E4-995A-644BE71878B1,TIntegerHashConvert,,AValue,Integer)}

{$JPPEXPANDMACRO HASHCONVERTER(IJclCardinalHashConverter,2DF04C8A-16B8-4712-BC5D-AD35014EC9F7,TCardinalHashConvert,,AValue,Cardinal)}

{$JPPEXPANDMACRO HASHCONVERTER(IJclInt64HashConverter,96CF2A71-9185-4E26-B283-457ABC3584E7,TInt64HashConvert,const ,AValue,Int64)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO HASHCONVERTER(IJclPtrHashConverter,D704CC67-CFED-44E6-9504-65D5E468FCAF,TPtrHashConvert,,Ptr,Pointer)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO HASHCONVERTER(IJclHashConverter,2D0DD6F4-162E-41D6-8A34-489E7EACABCD,THashConvert,,AObject,TObject)}

  {$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO HASHCONVERTER(IJclHashConverter<T>,300AEA0E-7433-4C3E-99A6-E533212ACF42,THashConvert<T>,const ,AItem,T)}
  {$ENDIF SUPPORTS_GENERICS}

  IJclIntfCloneable = interface
    ['{BCF77740-FB60-4306-9BD1-448AADE5FF4E}']
    function Clone: IInterface;
  end;

  IJclCloneable = interface
    ['{D224AE70-2C93-4998-9479-1D513D75F2B2}']
    function Clone: TObject;
  end;

  TJclAutoPackStrategy = (apsDisabled, apsAgressive, apsProportional, apsIncremental);

  // parameter signification depends on strategy
  //  - Disabled = unused (arrays are never packed)
  //  - Agressive = unused (arrays are always packed)
  //  - Proportional = ratio of empty slots before the array is packed
  //    number of empty slots is computed by this formula: Capacity div Parameter
  //  - Incremental = amount of empty slots before the array is packed

  IJclPackable = interface
    ['{03802D2B-E0AB-4300-A777-0B8A2BD993DF}']
    function CalcGrowCapacity(ACapacity, ASize: Integer): Integer;
    function GetAutoPackParameter: Integer;
    function GetAutoPackStrategy: TJclAutoPackStrategy;
    function GetCapacity: Integer;
    procedure Pack; // reduce used memory by eliminating empty storage area (force)
    procedure SetAutoPackParameter(Value: Integer);
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy);
    procedure SetCapacity(Value: Integer);
    property AutoPackParameter: Integer read GetAutoPackParameter write SetAutoPackParameter;
    property AutoPackStrategy: TJclAutoPackStrategy read GetAutoPackStrategy write SetAutoPackStrategy;
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  TJclAutoGrowStrategy = (agsDisabled, agsAgressive, agsProportional, agsIncremental);

  // parameter signification depends on strategy
  //  - Disabled = unused (arrays never grow)
  //  - Agressive = unused (arrays always grow by 1 element)
  //  - Proportional = ratio of empty slots to add to the array
  //    number of empty slots is computed by this formula: Capacity div Parameter
  //  - Incremental = amount of empty slots to add to the array

  IJclGrowable = interface(IJclPackable)
    ['{C71E8586-5688-444C-9BDD-9969D988123B}']
    function CalcPackCapacity(ACapacity, ASize: Integer): Integer;
    function GetAutoGrowParameter: Integer;
    function GetAutoGrowStrategy: TJclAutoGrowStrategy;
    procedure Grow;
    procedure SetAutoGrowParameter(Value: Integer);
    procedure SetAutoGrowStrategy(Value: TJclAutoGrowStrategy);
    property AutoGrowParameter: Integer read GetAutoGrowParameter write SetAutoGrowParameter;
    property AutoGrowStrategy: TJclAutoGrowStrategy read GetAutoGrowStrategy write SetAutoGrowStrategy;
  end;

  IJclObjectOwner = interface
    ['{5157EA13-924E-4A56-995D-36956441025C}']
    function FreeObject(var AObject: TObject): TObject;
    function GetOwnsObjects: Boolean;
    property OwnsObjects: Boolean read GetOwnsObjects;
  end;

  IJclKeyOwner = interface
    ['{8BE209E6-2F85-44FD-B0CD-A8363C95349A}']
    function FreeKey(var Key: TObject): TObject;
    function GetOwnsKeys: Boolean;
    property OwnsKeys: Boolean read GetOwnsKeys;
  end;

  IJclValueOwner = interface
    ['{3BCD98CE-7056-416A-A9E7-AE3AB2A62E54}']
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    property OwnsValues: Boolean read GetOwnsValues;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  IJclItemOwner<T> = interface
    ['{0CC220C1-E705-4B21-9F53-4AD340952165}']
    function FreeItem(var AItem: T): T;
    function GetOwnsItems: Boolean;
    property OwnsItems: Boolean read GetOwnsItems;
  end;

  IJclPairOwner<TKey, TValue> = interface
    ['{321C1FF7-AA2E-4229-966A-7EC6417EA16D}']
    function FreeKey(var Key: TKey): TKey;
    function FreeValue(var Value: TValue): TValue;
    function GetOwnsKeys: Boolean;
    function GetOwnsValues: Boolean;
    property OwnsKeys: Boolean read GetOwnsKeys;
    property OwnsValues: Boolean read GetOwnsValues;
  end;
  {$ENDIF SUPPORTS_GENERICS}

{$JPPEXPANDMACRO ITERATOR(IJclIntfIterator,IJclAbstractIterator,E121A98A-7C43-4587-806B-9189E8B2F106,const ,AInterface,IInterface,GetObject,SetObject)}

{$JPPEXPANDMACRO ITERATOR(IJclAnsiStrIterator,IJclAbstractIterator,D5D4B681-F902-49C7-B9E1-73007C9D64F0,const ,AString,AnsiString,GetString,SetString)}

{$JPPEXPANDMACRO ITERATOR(IJclWideStrIterator,IJclAbstractIterator,F03BC7D4-CCDA-4C4A-AF3A-E51FDCDE8ADE,const ,AString,WideString,GetString,SetString)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrIterator = IJclAnsiStrIterator;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrIterator = IJclWideStrIterator;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO ITERATOR(IJclSingleIterator,IJclAbstractIterator,FD1124F8-CB2B-4AD7-B12D-C05702F4204B,const ,AValue,Single,GetValue,SetValue)}

{$JPPEXPANDMACRO ITERATOR(IJclDoubleIterator,IJclAbstractIterator,004C154A-281C-4DA7-BF64-F3EE80ACF640,const ,AValue,Double,GetValue,SetValue)}

{$JPPEXPANDMACRO ITERATOR(IJclExtendedIterator,IJclAbstractIterator,B89877A5-DED4-4CD9-AB90-C7D062111DE0,const ,AValue,Extended,GetValue,SetValue)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatIterator = IJclExtendedIterator;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatIterator = IJclDoubleIterator;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatIterator = IJclSingleIterator;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO ITERATOR(IJclIntegerIterator,IJclAbstractIterator,1406A991-4574-48A1-83FE-2EDCA03908BE,,AValue,Integer,GetValue,SetValue)}

{$JPPEXPANDMACRO ITERATOR(IJclCardinalIterator,IJclAbstractIterator,72847A34-C8C4-4592-9447-CEB8161E33AD,,AValue,Cardinal,GetValue,SetValue)}

{$JPPEXPANDMACRO ITERATOR(IJclInt64Iterator,IJclAbstractIterator,573E5A51-BF76-43D7-9F93-46305BED20A8,const ,AValue,Int64,GetValue,SetValue)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO ITERATOR(IJclPtrIterator,IJclAbstractIterator,62B5501C-07AA-4D00-A85B-713B39912CDF,,APtr,Pointer,GetPointer,SetPointer)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO ITERATOR(IJclIterator,IJclAbstractIterator,997DF9B7-9AA2-4239-8B94-14DFFD26D790,,AObject,TObject,GetObject,SetObject)}

  {$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO ITERATOR(IJclIterator<T>,IJclAbstractIterator,6E8547A4-5B5D-4831-8AE3-9C6D04071B11,const ,AItem,T,GetItem,SetItem)}
  {$ENDIF SUPPORTS_GENERICS}

{$JPPEXPANDMACRO TREEITERATOR(IJclIntfTreeIterator,IJclIntfIterator,C97379BF-C6A9-4A90-9D7A-152E9BAD314F,const ,AInterface,IInterface)}

{$JPPEXPANDMACRO TREEITERATOR(IJclAnsiStrTreeIterator,IJclAnsiStrIterator,66BC5C76-758C-4E72-ABF1-EB02CF851C6D,const ,AString,AnsiString)}

{$JPPEXPANDMACRO TREEITERATOR(IJclWideStrTreeIterator,IJclWideStrIterator,B3168A3B-5A90-4ABF-855F-3D2B3AB6EE7F,const ,AString,WideString)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrTreeIterator = IJclAnsiStrTreeIterator;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrTreeIterator = IJclWideStrTreeIterator;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO TREEITERATOR(IJclSingleTreeIterator,IJclSingleIterator,17BFDE9D-DBF7-4DC8-AC74-919C717B4726,const ,AValue,Single)}

{$JPPEXPANDMACRO TREEITERATOR(IJclDoubleTreeIterator,IJclDoubleIterator,EB39B84E-D3C5-496E-A521-B8BF24579252,const ,AValue,Double)}

{$JPPEXPANDMACRO TREEITERATOR(IJclExtendedTreeIterator,IJclExtendedIterator,1B40A544-FC5D-454C-8E42-CE17B015E65C,const ,AValue,Extended)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatTreeIterator = IJclExtendedTreeIterator;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatTreeIterator = IJclDoubleTreeIterator;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatTreeIterator = IJclSingleTreeIterator;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO TREEITERATOR(IJclIntegerTreeIterator,IJclIntegerIterator,88EDC5C5-CA41-41AF-9838-AA19D07E69F5,,AValue,Integer)}

{$JPPEXPANDMACRO TREEITERATOR(IJclCardinalTreeIterator,IJclCardinalIterator,FDBF493F-F79D-46EB-A59D-7193B6E6A860,,AValue,Cardinal)}

{$JPPEXPANDMACRO TREEITERATOR(IJclInt64TreeIterator,IJclInt64Iterator,C5A5E504-E19B-43AC-90B9-E4B8984BFA23,const ,AValue,Int64)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO TREEITERATOR(IJclPtrTreeIterator,IJclPtrIterator,ED4C08E6-60FC-4ED3-BD19-E6605B9BD943,,APtr,Pointer)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO TREEITERATOR(IJclTreeIterator,IJclIterator,8B4863B0-B6B9-426E-B5B8-7AF71D264237,,AObject,TObject)}

  {$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO TREEITERATOR(IJclTreeIterator<T>,IJclIterator<T>,29A06DA4-D93A-40A5-8581-0FE85BC8384B,const ,AItem,T)}
  {$ENDIF SUPPORTS_GENERICS}

{$JPPEXPANDMACRO BINTREEITERATOR(IJclIntfBinaryTreeIterator,IJclIntfTreeIterator,8BE874B2-0075-4EE0-8F49-665FC894D923,IInterface)}

{$JPPEXPANDMACRO BINTREEITERATOR(IJclAnsiStrBinaryTreeIterator,IJclAnsiStrTreeIterator,34A4A300-042C-43A9-AC23-8FC1B76BFB25,AnsiString)}

{$JPPEXPANDMACRO BINTREEITERATOR(IJclWideStrBinaryTreeIterator,IJclWideStrTreeIterator,17C08EB9-6880-469E-878A-8F5EBFE905B1,WideString)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrBinaryTreeIterator = IJclAnsiStrBinaryTreeIterator;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrBinaryTreeIterator = IJclWideStrBinaryTreeIterator;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO BINTREEITERATOR(IJclSingleBinaryTreeIterator,IJclSingleTreeIterator,BC6FFB13-FA1C-4077-8273-F25A3119168B,Single)}

{$JPPEXPANDMACRO BINTREEITERATOR(IJclDoubleBinaryTreeIterator,IJclDoubleTreeIterator,CE48083C-D60C-4315-BC14-8CE77AC3269E,Double)}

{$JPPEXPANDMACRO BINTREEITERATOR(IJclExtendedBinaryTreeIterator,IJclExtendedTreeIterator,8A9FAE2A-5EF5-4165-8E8D-51F2102A4580,Extended)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatBinaryTreeIterator = IJclExtendedBinaryTreeIterator;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatBinaryTreeIterator = IJclDoubleBinaryTreeIterator;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatBinaryTreeIterator = IJclSingleBinaryTreeIterator;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO BINTREEITERATOR(IJclIntegerBinaryTreeIterator,IJclIntegerTreeIterator,FE2BF57D-D10D-4B0C-903D-BB61700FBA0A,Integer)}

{$JPPEXPANDMACRO BINTREEITERATOR(IJclCardinalBinaryTreeIterator,IJclCardinalTreeIterator,AAA358F5-95A1-480F-8E2A-09028BA6C397,Cardinal)}

{$JPPEXPANDMACRO BINTREEITERATOR(IJclInt64BinaryTreeIterator,IJclInt64TreeIterator,5605E164-5CDD-40B1-9323-DE1CB584E289,Int64)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO BINTREEITERATOR(IJclPtrBinaryTreeIterator,IJclPtrTreeIterator,75D3DF0D-C491-43F7-B078-E658197E8051,Pointer)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO BINTREEITERATOR(IJclBinaryTreeIterator,IJclTreeIterator,821DE28D-631C-4F23-A0B2-CC0F35B4C64D,TObject)}

  {$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO BINTREEITERATOR(IJclBinaryTreeIterator<T>,IJclTreeIterator<T>,0CF5B0FC-C644-458C-BF48-2E093DAFEC26,T)}
  {$ENDIF SUPPORTS_GENERICS}

{$JPPEXPANDMACRO COLLECTION(IJclIntfCollection,IJclContainer,8E178463-4575-487A-B4D5-DC2AED3C7ACA,const ,AInterface,IInterface,IJclIntfIterator)}

{$JPPEXPANDMACRO COLLECTION(IJclAnsiStrCollection,IJclAnsiStrFlatContainer,3E3CFC19-E8AF-4DD7-91FA-2DF2895FC7B9,const ,AString,AnsiString,IJclAnsiStrIterator)}

{$JPPEXPANDMACRO COLLECTION(IJclWideStrCollection,IJclWideStrFlatContainer,CDCC0F94-4DD0-4F25-B441-6AE55D5C7466,const ,AString,WideString,IJclWideStrIterator)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrCollection = IJclAnsiStrCollection;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrCollection = IJclWideStrCollection;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO COLLECTION(IJclSingleCollection,IJclSingleContainer,1D34D474-6588-441E-B2B3-8C021A37ED89,const ,AValue,Single,IJclSingleIterator)}

{$JPPEXPANDMACRO COLLECTION(IJclDoubleCollection,IJclDoubleContainer,E54C7717-C33A-4F1B-860C-4F60F303EAD3,const ,AValue,Double,IJclDoubleIterator)}

{$JPPEXPANDMACRO COLLECTION(IJclExtendedCollection,IJclExtendedContainer,2A1341CB-B997-4E3B-B1CA-6D60AE853C55,const ,AValue,Extended,IJclExtendedIterator)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatCollection = IJclExtendedCollection;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatCollection = IJclDoubleCollection;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatCollection = IJclSingleCollection;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO COLLECTION(IJclIntegerCollection,IJclContainer,AF69890D-22D1-4D89-8FFD-5FAD7E0638BA,,AValue,Integer,IJclIntegerIterator)}

{$JPPEXPANDMACRO COLLECTION(IJclCardinalCollection,IJclContainer,CFBD0344-58C8-4FA2-B4D7-D21D77DFBF80,,AValue,Cardinal,IJclCardinalIterator)}

{$JPPEXPANDMACRO COLLECTION(IJclInt64Collection,IJclContainer,93A45BDE-3C4C-48D6-9874-5322914DFDDA,const ,AValue,Int64,IJclInt64Iterator)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO COLLECTION(IJclPtrCollection,IJclContainer,02E909A7-5B1D-40D4-82EA-A0CD97D5C811,,APtr,Pointer,IJclPtrIterator)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO COLLECTION(IJclCollection,IJclContainer,58947EF1-CD21-4DD1-AE3D-225C3AAD7EE5,,AObject,TObject,IJclIterator)}

  {$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO COLLECTION(IJclCollection<T>,IJclContainer,67EE8AF3-19B0-4DCA-A730-3C9B261B8EC5,const ,AItem,T,IJclIterator<T>)}
  {$ENDIF SUPPORTS_GENERICS}

{$JPPEXPANDMACRO LIST(IJclIntfList,IJclIntfCollection,E14EDA4B-1DAA-4013-9E6C-CDCB365C7CF9,const ,AInterface,IInterface,GetObject,SetObject,Objects)}

{$JPPEXPANDMACRO LIST(IJclAnsiStrList,IJclAnsiStrCollection,07DD7644-EAC6-4059-99FC-BEB7FBB73186,const ,AString,AnsiString,GetString,SetString,Strings)}

{$JPPEXPANDMACRO LIST(IJclWideStrList,IJclWideStrCollection,C9955874-6AC0-4CE0-8CC0-606A3F1702C6,const ,AString,WideString,GetString,SetString,Strings)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrList = IJclAnsiStrList;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrList = IJclWideStrList;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO LIST(IJclSingleList,IJclSingleCollection,D081324C-70A4-4AAC-BA42-7557F0262826,const ,AValue,Single,GetValue,SetValue,Values)}

{$JPPEXPANDMACRO LIST(IJclDoubleList,IJclDoubleCollection,ECA58515-3903-4312-9486-3214E03F35AB,const ,AValue,Double,GetValue,SetValue,Values)}

{$JPPEXPANDMACRO LIST(IJclExtendedList,IJclExtendedCollection,7463F954-F8DF-4B02-A284-FCB98746248E,const ,AValue,Extended,GetValue,SetValue,Values)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatList = IJclExtendedList;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatList = IJclDoubleList;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatList = IJclSingleList;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO LIST(IJclIntegerList,IJclIntegerCollection,339BE91B-557D-4CE0-A854-1CBD4FE31725,,AValue,Integer,GetValue,SetValue,Values)}

{$JPPEXPANDMACRO LIST(IJclCardinalList,IJclCardinalCollection,02B09EA8-DE6F-4A18-AA57-C3533E6AC4E3,,AValue,Cardinal,GetValue,SetValue,Values)}

{$JPPEXPANDMACRO LIST(IJclInt64List,IJclInt64Collection,E8D49200-91D3-4BD0-A59B-B93EC7E2074B,const ,AValue,Int64,GetValue,SetValue,Values)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO LIST(IJclPtrList,IJclPtrCollection,2CF5CF1F-C012-480C-A4CE-38BDAFB15D05,,APtr,Pointer,GetPointer,SetPointer,Pointers)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO LIST(IJclList,IJclCollection,8ABC70AC-5C06-43EA-AFE0-D066379BCC28,,AObject,TObject,GetObject,SetObject,Objects)}

  {$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO LIST(IJclList<T>,IJclCollection<T>,3B4BE3D7-8FF7-4163-91DF-3F73AE6935E7,const ,AItem,T,GetItem,SetItem,Items)}
  {$ENDIF SUPPORTS_GENERICS}

  // Pointer functions for sort algorithms
  {$JPPEXPANDMACRO SORTPROC(TIntfSortProc,IJclIntfList,TIntfCompare)}
  {$JPPEXPANDMACRO SORTPROC(TAnsiStrSortProc,IJclAnsiStrList,TAnsiStrCompare)}
  {$JPPEXPANDMACRO SORTPROC(TWideStrSortProc,IJclWideStrList,TWideStrCompare)}
  {$IFDEF CONTAINER_ANSISTR}
  TStrSortProc = TAnsiStrSortProc;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TStrSortProc = TWideStrSortProc;
  {$ENDIF CONTAINER_WIDESTR}
  {$JPPEXPANDMACRO SORTPROC(TSingleSortProc,IJclSingleList,TSingleCompare)}
  {$JPPEXPANDMACRO SORTPROC(TDoubleSortProc,IJclDoubleList,TDoubleCompare)}
  {$JPPEXPANDMACRO SORTPROC(TExtendedSortProc,IJclExtendedList,TExtendedCompare)}
  {$JPPEXPANDMACRO SORTPROC(TIntegerSortProc,IJclIntegerList,TIntegerCompare)}
  {$JPPEXPANDMACRO SORTPROC(TCardinalSortProc,IJclCardinalList,TCardinalCompare)}
  {$JPPEXPANDMACRO SORTPROC(TInt64SortProc,IJclInt64List,TInt64Compare)}
  {$IFNDEF CLR}
  {$JPPEXPANDMACRO SORTPROC(TPtrSortProc,IJclPtrList,TPtrCompare)}
  {$ENDIF ~CLR}
  {$JPPEXPANDMACRO SORTPROC(TSortProc,IJclList,TCompare)}
  {$IFDEF SUPPORTS_GENERICS}
  {$JPPEXPANDMACRO SORTPROC(TSortProc<T>,IJclList<T>,TCompare<T>)}
  {$ENDIF SUPPORTS_GENERICS}

{$JPPEXPANDMACRO ARRAY(IJclIntfArray,IJclIntfList,B055B427-7817-43FC-97D4-AD1845643D63,const ,AInterface,IInterface,GetObject,SetObject,Objects)}

{$JPPEXPANDMACRO ARRAY(IJclAnsiStrArray,IJclAnsiStrList,4953EA83-9288-4537-9D10-544D1C992B62,const ,AString,AnsiString,GetString,SetString,Strings)}

{$JPPEXPANDMACRO ARRAY(IJclWideStrArray,IJclWideStrList,3CE09F9A-5CB4-4867-80D5-C2313D278D69,const ,AString,WideString,GetString,SetString,Strings)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrArray = IJclAnsiStrArray;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrArray = IJclWideStrArray;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO ARRAY(IJclSingleArray,IJclSingleList,B96E2A4D-D750-4B65-B975-C619A05A29F6,const ,AValue,Single,GetValue,SetValue,Values)}

{$JPPEXPANDMACRO ARRAY(IJclDoubleArray,IJclDoubleList,67E66324-9757-4E85-8ECD-53396910FB39,const ,AValue,Double,GetValue,SetValue,Values)}

{$JPPEXPANDMACRO ARRAY(IJclExtendedArray,IJclExtendedList,D43E8D18-26B3-41A2-8D52-ED7EA2FE1AB7,const ,AValue,Extended,GetValue,SetValue,Values)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatArray = IJclExtendedArray;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatArray = IJclDoubleArray;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatArray = IJclSingleArray;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO ARRAY(IJclIntegerArray,IJclIntegerList,2B7C8B33-C0BD-4EC3-9764-63866E174781,,AValue,Integer,GetValue,SetValue,Values)}

{$JPPEXPANDMACRO ARRAY(IJclCardinalArray,IJclCardinalList,C451F2F8-65C6-4C29-99A0-CC9C15356418,,AValue,Cardinal,GetValue,SetValue,Values)}

{$JPPEXPANDMACRO ARRAY(IJclInt64Array,IJclInt64List,D947C43D-2D04-442A-A707-39EDE7D96FC9,const ,AValue,Int64,GetValue,SetValue,Values)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO ARRAY(IJclPtrArray,IJclPtrList,D43E8D18-26B3-41A2-8D52-ED7EA2FE1AB7,,APtr,Pointer,GetPointer,SetPointer,Pointers)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO ARRAY(IJclArray,IJclList,A69F6D35-54B2-4361-852E-097ED75E648A,,AObject,TObject,GetObject,SetObject,Objects)}

  {$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO ARRAY(IJclArray<T>,IJclList<T>,38810C13-E35E-428A-B84F-D25FB994BE8E,const ,AItem,T,GetItem,SetItem,Items)}
  {$ENDIF SUPPORTS_GENERICS}

{$JPPEXPANDMACRO SET(IJclIntfSet,IJclIntfCollection,E2D28852-9774-49B7-A739-5DBA2B705924)}

{$JPPEXPANDMACRO SET(IJclAnsiStrSet,IJclAnsiStrCollection,72204D85-2B68-4914-B9F2-09E5180C12E9)}

{$JPPEXPANDMACRO SET(IJclWideStrSet,IJclWideStrCollection,08009E0A-ABDD-46AB-8CEE-407D4723E17C)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrSet = IJclAnsiStrSet;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrSet = IJclWideStrSet;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO SET(IJclSingleSet,IJclSingleCollection,36E34A78-6A29-4503-97D5-4BF53538CEC0)}

{$JPPEXPANDMACRO SET(IJclDoubleSet,IJclDoubleCollection,4E1E4847-E934-4811-A26C-5FC8E772A623)}

{$JPPEXPANDMACRO SET(IJclExtendedSet,IJclExtendedCollection,3B9CF52D-1C49-4388-A7B3-9BEE1821FFD4)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatSet = IJclExtendedSet;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatSet = IJclDoubleSet;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatSet = IJclSingleSet;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO SET(IJclIntegerSet,IJclIntegerCollection,5E4D29AF-F508-465B-9008-D11FF82F25FE)}

{$JPPEXPANDMACRO SET(IJclCardinalSet,IJclCardinalCollection,09858637-CE8F-42E6-97E0-2786CD68387B)}

{$JPPEXPANDMACRO SET(IJclInt64Set,IJclInt64Collection,ACB3127A-48EE-4F9F-B988-6AE9057780E9)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO SET(IJclPtrSet,IJclPtrCollection,26717C68-4F83-4CCB-973A-7324FBD09632)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO SET(IJclSet,IJclCollection,0B7CDB90-8588-4260-A54C-D87101C669EA)}

  {$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO SET(IJclSet<T>,IJclCollection<T>,0B7CDB90-8588-4260-A54C-D87101C669EA)}
  {$ENDIF SUPPORTS_GENERICS}

  TJclTraverseOrder = (toPreOrder, toOrder, toPostOrder);

{$JPPEXPANDMACRO TREE(IJclIntfTree,IJclIntfCollection,5A21688F-113D-41B4-A17C-54BDB0BD6559,IJclIntfTreeIterator)}

{$JPPEXPANDMACRO TREE(IJclAnsiStrTree,IJclAnsiStrCollection,1E1896C0-0497-47DF-83AF-A9422084636C,IJclAnsiStrTreeIterator)}

{$JPPEXPANDMACRO TREE(IJclWideStrTree,IJclWideStrCollection,E325615A-7A20-4788-87FA-9051002CCD91,IJclWideStrTreeIterator)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrTree = IJclAnsiStrTree;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrTree = IJclWideStrTree;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO TREE(IJclSingleTree,IJclSingleCollection,A90A51BC-EBD7-40D3-B0A0-C9987E7A83D0,IJclSingleTreeIterator)}

{$JPPEXPANDMACRO TREE(IJclDoubleTree,IJclDoubleCollection,69DA85B1-A0DD-407B-B5CF-5EB7C6D4B82D,IJclDoubleTreeIterator)}

{$JPPEXPANDMACRO TREE(IJclExtendedTree,IJclExtendedCollection,9ACCCAFD-B617-43DC-AAF9-916BE324A17E,IJclExtendedTreeIterator)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatTree = IJclExtendedTree;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatTree = IJclDoubleTree;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatTree = IJclSingleTree;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO TREE(IJclIntegerTree,IJclIntegerCollection,40A6F934-E5F3-4C74-AC02-227035C8C3C6,IJclIntegerTreeIterator)}

{$JPPEXPANDMACRO TREE(IJclCardinalTree,IJclCardinalCollection,6C76C668-50C8-42A2-B72B-79BF102E270D,IJclCardinalTreeIterator)}

{$JPPEXPANDMACRO TREE(IJclInt64Tree,IJclInt64Collection,1925B973-8B75-4A79-A993-DF2598FF19BE,IJclInt64TreeIterator)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO TREE(IJclPtrTree,IJclPtrCollection,2C1ACA3E-3F23-4E3C-984D-151CF9776E14,IJclPtrTreeIterator)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO TREE(IJclTree,IJclCollection,B0C658CC-FEF5-4178-A4C5-442C0DEDE207,IJclTreeIterator)}

  {$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO TREE(IJclTree<T>,IJclCollection<T>,3F963AB5-5A75-41F9-A21B-7E7FB541A459,IJclTreeIterator<T>)}
  {$ENDIF SUPPORTS_GENERICS}

{$JPPEXPANDMACRO MAP(IJclIntfIntfMap,IJclContainer,01D05399-4A05-4F3E-92F4-0C236BE77019,const ,IInterface,IJclIntfSet,IJclIntfCollection)}

  (*IJclMultiIntfIntfMap = interface(IJclIntfIntfMap)
    ['{497775A5-D3F1-49FC-A641-15CC9E77F3D0}']
    function GetValues(const Key: IInterface): IJclIntfIterator;
    function Count(const Key: IInterface): Integer;
  end;*)

{$JPPEXPANDMACRO MAP(IJclAnsiStrIntfMap,IJclAnsiStrContainer,A4788A96-281A-4924-AA24-03776DDAAD8A,const ,AnsiString,IJclAnsiStrSet,const ,IInterface,IJclIntfCollection)}

{$JPPEXPANDMACRO MAP(IJclWideStrIntfMap,IJclWideStrContainer,C959AB76-9CF0-4C2C-A2C6-8A1846563FAF,const ,WideString,IJclWideStrSet,const ,IInterface,IJclIntfCollection)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrIntfMap = IJclAnsiStrIntfMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrIntfMap = IJclWideStrIntfMap;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO MAP(IJclIntfAnsiStrMap,IJclAnsiStrContainer,B10E324A-1D98-42FF-B9B4-7F99044591B2,const ,IInterface,IJclIntfSet,const ,AnsiString,IJclAnsiStrCollection)}

{$JPPEXPANDMACRO MAP(IJclIntfWideStrMap,IJclWideStrContainer,D9FD7887-B840-4636-8A8F-E586663E332C,const ,IInterface,IJclIntfSet,const ,WideString,IJclWideStrCollection)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclIntfStrMap = IJclIntfAnsiStrMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclIntfStrMap = IJclIntfWideStrMap;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO MAP(IJclAnsiStrAnsiStrMap,IJclAnsiStrContainer,A4788A96-281A-4924-AA24-03776DDAAD8A,const ,AnsiString,IJclAnsiStrSet,IJclAnsiStrCollection)}

{$JPPEXPANDMACRO MAP(IJclWideStrWideStrMap,IJclWideStrContainer,8E8D2735-C4FB-4F00-8802-B2102BCE3644,const ,WideString,IJclWideStrSet,IJclWideStrCollection)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrStrMap = IJclAnsiStrAnsiStrMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrStrMap = IJclWideStrWideStrMap;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO MAP(IJclSingleIntfMap,IJclSingleContainer,5F5E9E8B-E648-450B-B6C0-0EC65CC2D0BA,const ,Single,IJclSingleSet,const ,IInterface,IJclIntfCollection)}

{$JPPEXPANDMACRO MAP(IJclIntfSingleMap,IJclSingleContainer,234D1618-FB0E-46F5-A70D-5106163A90F7,const ,IInterface,IJclIntfSet,const ,Single,IJclSingleCollection)}

{$JPPEXPANDMACRO MAP(IJclSingleSingleMap,IJclSingleContainer,AEB0008F-F3CF-4055-A7F3-A330D312F03F,const ,Single,IJclSingleSet,IJclSingleCollection)}

{$JPPEXPANDMACRO MAP(IJclDoubleIntfMap,IJclDoubleContainer,08968FFB-36C6-4FBA-BC09-3DCA2B5D7A50,const ,Double,IJclDoubleSet,const ,IInterface,IJclIntfCollection)}

{$JPPEXPANDMACRO MAP(IJclIntfDoubleMap,IJclDoubleContainer,B23DAF6A-6DC5-4DDD-835C-CD4633DDA010,const ,IInterface,IJclIntfSet,const ,Double,IJclDoubleCollection)}

{$JPPEXPANDMACRO MAP(IJclDoubleDoubleMap,IJclDoubleContainer,329A03B8-0B6B-4FE3-87C5-4B63447A5FFD,const ,Double,IJclDoubleSet,IJclDoubleCollection)}

{$JPPEXPANDMACRO MAP(IJclExtendedIntfMap,IJclExtendedContainer,7C0731E0-C9AB-4378-B1B0-8CE3DD60AD41,const ,Extended,IJclExtendedSet,const ,IInterface,IJclIntfCollection)}

{$JPPEXPANDMACRO MAP(IJclIntfExtendedMap,IJclExtendedContainer,479FCE5A-2D8A-44EE-96BC-E8DA3187DBD8,const ,IInterface,IJclIntfSet,const ,Extended,IJclExtendedCollection)}

{$JPPEXPANDMACRO MAP(IJclExtendedExtendedMap,IJclExtendedContainer,962C2B09-8CF5-44E8-A21A-4A7DAFB72A11,const ,Extended,IJclExtendedSet,IJclExtendedCollection)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatIntfMap = IJclExtendedIntfMap;
  IJclIntfFloatMap = IJclIntfExtendedMap;
  IJclFloatFloatMap = IJclExtendedExtendedMap;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatIntfMap = IJclDoubleIntfMap;
  IJclIntfFloatMap = IJclIntfDoubleMap;
  IJclFloatFloatMap = IJclDoubleDoubleMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatIntfMap = IJclSingleIntfMap;
  IJclIntfFloatMap = IJclIntfSingleMap;
  IJclFloatFloatMap = IJclSingleSingleMap;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO MAP(IJclIntegerIntfMap,IJclContainer,E535FE65-AC88-49D3-BEF2-FB30D92C2FA6,,Integer,IJclIntegerSet,const ,IInterface,IJclIntfCollection)}

{$JPPEXPANDMACRO MAP(IJclIntfIntegerMap,IJclContainer,E01DA012-BEE0-4259-8E30-0A7A1A87BED0,const ,IInterface,IJclIntfSet,,Integer,IJclIntegerCollection)}

{$JPPEXPANDMACRO MAP(IJclIntegerIntegerMap,IJclContainer,23A46BC0-DF8D-4BD2-89D2-4DACF1EC73A1,,Integer,IJclIntegerSet,IJclIntegerCollection)}

{$JPPEXPANDMACRO MAP(IJclCardinalIntfMap,IJclContainer,80D39FB1-0D10-49CE-8AF3-1CD98A1D4F6C,,Cardinal,IJclCardinalSet,const ,IInterface,IJclIntfCollection)}

{$JPPEXPANDMACRO MAP(IJclIntfCardinalMap,IJclContainer,E1A724AB-6BDA-45F0-AE21-5E7E789A751B,const ,IInterface,IJclIntfSet,,Cardinal,IJclCardinalCollection)}

{$JPPEXPANDMACRO MAP(IJclCardinalCardinalMap,IJclContainer,1CD3F54C-F92F-4AF4-82B2-0829C08AA83B,,Cardinal,IJclCardinalSet,IJclCardinalCollection)}

{$JPPEXPANDMACRO MAP(IJclInt64IntfMap,IJclContainer,B64FB2D1-8D45-4367-B950-98D3D05AC6A0,const ,Int64,IJclInt64Set,const ,IInterface,IJclIntfCollection)}

{$JPPEXPANDMACRO MAP(IJclIntfInt64Map,IJclContainer,9886BEE3-D15B-45D2-A3FB-4D3A0ADEC8AC,const ,IInterface,IJclIntfSet,const ,Int64,IJclInt64Collection)}

{$JPPEXPANDMACRO MAP(IJclInt64Int64Map,IJclContainer,EF2A2726-408A-4984-9971-DDC1B6EFC9F5,const ,Int64,IJclInt64Set,IJclInt64Collection)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO MAP(IJclPtrIntfMap,IJclContainer,B7C48542-39A0-453F-8F03-8C8CFAB0DCCF,,Pointer,IJclPtrSet,const ,IInterface,IJclIntfCollection)}

{$JPPEXPANDMACRO MAP(IJclIntfPtrMap,IJclContainer,DA51D823-58DB-4D7C-9B8E-07E0FD560B57,const ,IInterface,IJclIntfSet,,Pointer,IJclPtrCollection)}

{$JPPEXPANDMACRO MAP(IJclPtrPtrMap,IJclContainer,1200CB0F-A766-443F-9030-5A804C11B798,,Pointer,IJclPtrSet,IJclPtrCollection)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO MAP(IJclIntfMap,IJclContainer,C70570C6-EDDB-47B4-9003-C637B486731D,const ,IInterface,IJclIntfSet,,TObject,IJclCollection)}

{$JPPEXPANDMACRO MAP(IJclAnsiStrMap,IJclAnsiStrContainer,A7D0A882-6952-496D-A258-23D47DDCCBC4,const ,AnsiString,IJclAnsiStrSet,,TObject,IJclCollection)}

{$JPPEXPANDMACRO MAP(IJclWideStrMap,IJclWideStrContainer,ACE8E6B4-5A56-4753-A2C6-BAE195A56B63,const ,WideString,IJclWideStrSet,,TObject,IJclCollection)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrMap = IJclAnsiStrMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrMap = IJclWideStrMap;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO MAP(IJclSingleMap,IJclSingleContainer,C501920A-F252-4F94-B142-1F05AE06C3D2,const ,Single,IJclSingleSet,,TObject,IJclCollection)}

{$JPPEXPANDMACRO MAP(IJclDoubleMap,IJclDoubleContainer,B1B994AC-49C9-418B-814B-43BAD706F355,const ,Double,IJclDoubleSet,,TObject,IJclCollection)}

{$JPPEXPANDMACRO MAP(IJclExtendedMap,IJclExtendedContainer,3BCC8C87-A186-45E8-9B37-0B8E85120434,const ,Extended,IJclExtendedSet,,TObject,IJclCollection)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatMap = IJclExtendedMap;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatMap = IJclDoubleMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatMap = IJclSingleMap;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO MAP(IJclIntegerMap,IJclContainer,D6FA5D64-A4AF-4419-9981-56BA79BF8770,,Integer,IJclIntegerSet,,TObject,IJclCollection)}

{$JPPEXPANDMACRO MAP(IJclCardinalMap,IJclContainer,A2F92F4F-11CB-4DB2-932F-F10A14237126,,Cardinal,IJclCardinalSet,,TObject,IJclCollection)}

{$JPPEXPANDMACRO MAP(IJclInt64Map,IJclContainer,4C720CE0-7A7C-41D5-BFC1-8D58A47E648F,const ,Int64,IJclInt64Set,,TObject,IJclCollection)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO MAP(IJclPtrMap,IJclContainer,2FE029A9-026C-487D-8204-AD3A28BD2FA2,,Pointer,IJclPtrSet,,TObject,IJclCollection)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO MAP(IJclMap,IJclContainer,A7D0A882-6952-496D-A258-23D47DDCCBC4,,TObject,IJclSet,IJclCollection)}

  {$IFDEF SUPPORTS_GENERICS}
  IHashable = interface
    function GetHashCode: Integer;
  end;

{$JPPEXPANDMACRO MAP(IJclMap<TKey\,TValue>,IJclContainer,22624C43-4828-4A1E-BDD4-4A7FE59AE135,const ,TKey,IJclSet<TKey>,const ,TValue,IJclCollection<TValue>)}
  {$ENDIF SUPPORTS_GENERICS}

{$JPPEXPANDMACRO QUEUE(IJclIntfQueue,IJclContainer,B88756FE-5553-4106-957E-3E33120BFA99,const ,AInterface,IInterface)}

{$JPPEXPANDMACRO QUEUE(IJclAnsiStrQueue,IJclAnsiStrContainer,5BA0ED9A-5AF3-4F79-9D80-34FA7FF15D1F,const ,AString,AnsiString)}

{$JPPEXPANDMACRO QUEUE(IJclWideStrQueue,IJclWideStrContainer,058BBFB7-E9B9-44B5-B676-D5B5B9A79BEF,const ,AString,WideString)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrQueue = IJclAnsiStrQueue;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrQueue = IJclWideStrQueue;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO QUEUE(IJclSingleQueue,IJclSingleContainer,67D74314-9967-4C99-8A48-6E0ADD73EC29,const ,AValue,Single)}

{$JPPEXPANDMACRO QUEUE(IJclDoubleQueue,IJclDoubleContainer,FA1B6D25-3456-4963-87DC-5A2E53B2963F,const ,AValue,Double)}

{$JPPEXPANDMACRO QUEUE(IJclExtendedQueue,IJclExtendedContainer,76F349C0-7681-4BE8-9E94-280C962780D8,const ,AValue,Extended)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatQueue = IJclExtendedQueue;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatQueue = IJclDoubleQueue;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatQueue = IJclSingleQueue;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO QUEUE(IJclIntegerQueue,IJclContainer,4C4E174E-5D19-44CE-A248-B5589A9B68DF,,AValue,Integer)}

{$JPPEXPANDMACRO QUEUE(IJclCardinalQueue,IJclContainer,CC1D4358-E259-4FB0-BA83-5180A0F8A6C0,,AValue,Cardinal)}

{$JPPEXPANDMACRO QUEUE(IJclInt64Queue,IJclContainer,96B620BB-9A90-43D5-82A7-2D818A11C8E1,const ,AValue,Int64)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO QUEUE(IJclPtrQueue,IJclContainer,1052DD37-3035-4C44-A793-54AC4B9C0B29,,APtr,Pointer)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO QUEUE(IJclQueue,IJclContainer,7D0F9DE4-71EA-46EF-B879-88BCFD5D9610,,AObject,TObject)}

  {$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO QUEUE(IJclQueue<T>,IJclContainer,16AB909F-2194-46CF-BD89-B4207AC0CAB8,const ,AItem,T)}
  {$ENDIF SUPPORTS_GENERICS}

{$JPPEXPANDMACRO SORTEDMAP(IJclIntfIntfSortedMap,IJclIntfIntfMap,265A6EB2-4BB3-459F-8813-360FD32A4971,const ,IInterface)}

{$JPPEXPANDMACRO SORTEDMAP(IJclAnsiStrIntfSortedMap,IJclAnsiStrIntfMap,706D1C91-5416-4FDC-B6B1-F4C1E8CFCD38,const ,AnsiString)}

{$JPPEXPANDMACRO SORTEDMAP(IJclWideStrIntfSortedMap,IJclWideStrIntfMap,299FDCFD-2DB7-4D64-BF18-EE3668316430,const ,WideString)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrIntfSortedMap = IJclAnsiStrIntfSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrIntfSortedMap = IJclWideStrIntfSortedMap;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO SORTEDMAP(IJclIntfAnsiStrSortedMap,IJclIntfAnsiStrMap,96E6AC5E-8C40-4795-9C8A-CFD098B58680,const ,IInterface)}

{$JPPEXPANDMACRO SORTEDMAP(IJclIntfWideStrSortedMap,IJclIntfWideStrMap,FBE3AD2E-2781-4DC0-9E80-027027380E21,const ,IInterface)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclIntfStrSortedMap = IJclIntfAnsiStrSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclIntfStrSortedMap = IJclIntfWideStrSortedMap;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO SORTEDMAP(IJclAnsiStrAnsiStrSortedMap,IJclAnsiStrAnsiStrMap,4F457799-5D03-413D-A46C-067DC4200CC3,const ,AnsiString)}

{$JPPEXPANDMACRO SORTEDMAP(IJclWideStrWideStrSortedMap,IJclWideStrWideStrMap,3B0757B2-2290-4AFA-880D-F9BA600E501E,const ,WideString)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrStrSortedMap = IJclAnsiStrAnsiStrSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrStrSortedMap = IJclWideStrWideStrSortedMap;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO SORTEDMAP(IJclSingleIntfSortedMap,IJclSingleIntfMap,83D57068-7B8E-453E-B35B-2AB4B594A7A9,const ,Single)}

{$JPPEXPANDMACRO SORTEDMAP(IJclIntfSingleSortedMap,IJclIntfSingleMap,B07FA192-3466-4F2A-BBF0-2DC0100B08A8,const ,IInterface)}

{$JPPEXPANDMACRO SORTEDMAP(IJclSingleSingleSortedMap,IJclSingleSingleMap,7C6EA0B4-959D-44D5-915F-99DFC1753B00,const ,Single)}

{$JPPEXPANDMACRO SORTEDMAP(IJclDoubleIntfSortedMap,IJclDoubleIntfMap,F36C5F4F-4F8C-4943-AA35-41623D3C21E9,const ,Double)}

{$JPPEXPANDMACRO SORTEDMAP(IJclIntfDoubleSortedMap,IJclIntfDoubleMap,0F16ADAE-F499-4857-B5EA-6F3CC9009DBA,const ,IInterface)}

{$JPPEXPANDMACRO SORTEDMAP(IJclDoubleDoubleSortedMap,IJclDoubleDoubleMap,855C858B-74CF-4338-872B-AF88A02DB537,const ,Double)}

{$JPPEXPANDMACRO SORTEDMAP(IJclExtendedIntfSortedMap,IJclExtendedIntfMap,A30B8835-A319-4776-9A11-D1EEF60B9C26,const ,Extended)}

{$JPPEXPANDMACRO SORTEDMAP(IJclIntfExtendedSortedMap,IJclIntfExtendedMap,3493D6C4-3075-48B6-8E99-CB0000D3978C,const ,IInterface)}

{$JPPEXPANDMACRO SORTEDMAP(IJclExtendedExtendedSortedMap,IJclExtendedExtendedMap,8CAA505C-D9BB-47E7-92EC-6043DC4AF42C,const ,Extended)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatIntfSortedMap = IJclExtendedIntfSortedMap;
  IJclIntfFloatSortedMap = IJclIntfExtendedSortedMap;
  IJclFloatFloatSortedMap = IJclExtendedExtendedSortedMap;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatIntfSortedMap = IJclDoubleIntfSortedMap;
  IJclIntfFloatSortedMap = IJclIntfDoubleSortedMap;
  IJclFloatFloatSortedMap = IJclDoubleDoubleSortedMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatIntfSortedMap = IJclSingleIntfSortedMap;
  IJclIntfFloatSortedMap = IJclIntfSingleSortedMap;
  IJclFloatFloatSortedMap = IJclSingleSingleSortedMap;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO SORTEDMAP(IJclIntegerIntfSortedMap,IJclIntegerIntfMap,8B22802C-61F2-4DA5-B1E9-DBB7840E7996,,Integer)}

{$JPPEXPANDMACRO SORTEDMAP(IJclIntfIntegerSortedMap,IJclIntfIntegerMap,8D3C9B7E-772D-409B-A58C-0CABFAFDEFF0,const ,IInterface)}

{$JPPEXPANDMACRO SORTEDMAP(IJclIntegerIntegerSortedMap,IJclIntegerIntegerMap,8A8BA17A-F468-469C-AF99-77D64C802F7A,,Integer)}

{$JPPEXPANDMACRO SORTEDMAP(IJclCardinalIntfSortedMap,IJclCardinalIntfMap,BAE97425-4F2E-461B-88DD-F83D27657AFA,,Cardinal)}

{$JPPEXPANDMACRO SORTEDMAP(IJclIntfCardinalSortedMap,IJclIntfCardinalMap,BC66BACF-23AE-48C4-9573-EDC3B5110BE7,const ,IInterface)}

{$JPPEXPANDMACRO SORTEDMAP(IJclCardinalCardinalSortedMap,IJclCardinalCardinalMap,182ACDA4-7D74-4D29-BB5C-4C8189DA774E,,Cardinal)}

{$JPPEXPANDMACRO SORTEDMAP(IJclInt64IntfSortedMap,IJclInt64IntfMap,24391756-FB02-4901-81E3-A37738B73DAD,const ,Int64)}

{$JPPEXPANDMACRO SORTEDMAP(IJclIntfInt64SortedMap,IJclIntfInt64Map,6E2AB647-59CC-4609-82E8-6AE75AED80CA,const ,IInterface)}

{$JPPEXPANDMACRO SORTEDMAP(IJclInt64Int64SortedMap,IJclInt64Int64Map,168581D2-9DD3-46D0-934E-EA0CCE5E3C0C,const ,Int64)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO SORTEDMAP(IJclPtrIntfSortedMap,IJclPtrIntfMap,6D7B8042-3CBC-4C8F-98B5-69AFAA104532,,Pointer)}

{$JPPEXPANDMACRO SORTEDMAP(IJclIntfPtrSortedMap,IJclIntfPtrMap,B054BDA2-536F-4C16-B6BB-BB64FA0818B3,const ,IInterface)}

{$JPPEXPANDMACRO SORTEDMAP(IJclPtrPtrSortedMap,IJclPtrPtrMap,F1FAE922-0212-41D0-BB4E-76A8AB2CAB86,,Pointer)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO SORTEDMAP(IJclIntfSortedMap,IJclIntfMap,3CED1477-B958-4109-9BDA-7C84B9E063B2,const ,IInterface)}

{$JPPEXPANDMACRO SORTEDMAP(IJclAnsiStrSortedMap,IJclAnsiStrMap,573F98E3-EBCD-4F28-8F35-96A7366CBF47,const ,AnsiString)}

{$JPPEXPANDMACRO SORTEDMAP(IJclWideStrSortedMap,IJclWideStrMap,B3021EFC-DE25-4B4B-A896-ACE823CD5C01,const ,WideString)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrSortedMap = IJclAnsiStrSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrSortedMap = IJclWideStrSortedMap;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO SORTEDMAP(IJclSingleSortedMap,IJclSingleMap,8C1A12BE-A7F2-4351-90B7-25DB0AAF5F94,const ,Single)}

{$JPPEXPANDMACRO SORTEDMAP(IJclDoubleSortedMap,IJclDoubleMap,8018D66B-AA54-4016-84FC-3E780FFCC38B,const ,Double)}

{$JPPEXPANDMACRO SORTEDMAP(IJclExtendedSortedMap,IJclExtendedMap,2B82C65A-B3EF-477D-BEC0-3D8620A226B1,const ,Extended)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatSortedMap = IJclExtendedSortedMap;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatSortedMap = IJclDoubleSortedMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatSortedMap = IJclSingleSortedMap;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO SORTEDMAP(IJclIntegerSortedMap,IJclIntegerMap,DD7B4C5E-6D51-44CC-9328-B38396A7E1C9,,Integer)}

{$JPPEXPANDMACRO SORTEDMAP(IJclCardinalSortedMap,IJclCardinalMap,4AEAF81F-D72E-4499-B10E-3D017F39915E,,Cardinal)}

{$JPPEXPANDMACRO SORTEDMAP(IJclInt64SortedMap,IJclInt64Map,06C03F90-7DE9-4043-AA56-AAE071D8BD50,const ,Int64)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO SORTEDMAP(IJclPtrSortedMap,IJclPtrMap,578918DB-6A4A-4A9D-B44E-AE3E8FF70818,,Pointer)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO SORTEDMAP(IJclSortedMap,IJclMap,F317A70F-7851-49C2-9DCF-092D8F4D4F98,,TObject)}

  {$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO SORTEDMAP(IJclSortedMap<TKey\,TValue>,IJclMap<TKey\,TValue>,C62B75C4-891B-442E-A5D6-9954E75A5C0C,const ,TKey)}
  {$ENDIF SUPPORTS_GENERICS}

{$JPPEXPANDMACRO SORTEDSET(IJclIntfSortedSet,IJclIntfSet,159BE5A7-7349-42FF-BE55-9CA1B9DBA991,const ,IInterface)}

{$JPPEXPANDMACRO SORTEDSET(IJclAnsiStrSortedSet,IJclAnsiStrSet,03198146-F967-4310-868B-7AD3D52D5CBE,const ,AnsiString)}

{$JPPEXPANDMACRO SORTEDSET(IJclWideStrSortedSet,IJclWideStrSet,ED9567E2-C1D3-4C00-A1D4-90D5C7E27C2D,const ,WideString)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrSortedSet = IJclAnsiStrSortedSet;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrSortedSet = IJclWideStrSortedSet;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO SORTEDSET(IJclSingleSortedSet,IJclSingleSet,65EDA801-9E04-4119-BF9E-D7DD4AF82144,const ,Single)}

{$JPPEXPANDMACRO SORTEDSET(IJclDoubleSortedSet,IJclDoubleSet,DA0E689F-BAFE-4BCE-85E4-C38E780BC84C,const ,Double)}

{$JPPEXPANDMACRO SORTEDSET(IJclExtendedSortedSet,IJclExtendedSet,A9875ED3-81A4-43A3-86BB-3429F51B278B,const ,Extended)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatSortedSet = IJclExtendedSortedSet;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatSortedSet = IJclDoubleSortedSet;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatSortedSet = IJclSingleSortedSet;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO SORTEDSET(IJclIntegerSortedSet,IJclIntegerSet,E086C54B-4FA3-426D-AC4E-FF8E8CA3D663,,Integer)}

{$JPPEXPANDMACRO SORTEDSET(IJclCardinalSortedSet,IJclCardinalSet,2D7995C6-A784-48B6-87E9-55D394A72362,,Cardinal)}

{$JPPEXPANDMACRO SORTEDSET(IJclInt64SortedSet,IJclInt64Set,4C1C3FCA-6169-4A2F-B044-91AC2AA2E954,const ,Int64)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO SORTEDSET(IJclPtrSortedSet,IJclPtrSet,F3A3183C-0820-425C-9446-E0838F0ADAD8,,Pointer)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO SORTEDSET(IJclSortedSet,IJclSet,A3D23E76-ADE9-446C-9B97-F49FCE895D9F,,TObject)}

  {$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO SORTEDSET(IJclSortedSet<T>,IJclSet<T>,30F836E3-2FB1-427E-A499-DFAE201633C8,const ,T)}
  {$ENDIF SUPPORTS_GENERICS}

{$JPPEXPANDMACRO STACK(IJclIntfStack,IJclContainer,CA1DC7A1-8D8F-4A5D-81D1-0FE32E9A4E84,const ,AInterface,IInterface)}

{$JPPEXPANDMACRO STACK(IJclAnsiStrStack,IJclAnsiStrContainer,649BB74C-D7BE-40D9-9F4E-32DDC3F13F3B,const ,AString,AnsiString)}

{$JPPEXPANDMACRO STACK(IJclWideStrStack,IJclWideStrContainer,B2C3B165-33F1-4B7D-A2EC-0B19D12CE33C,const ,AString,WideString)}

  {$IFDEF CONTAINER_ANSISTR}
  IJclStrStack = IJclAnsiStrStack;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  IJclStrStack = IJclWideStrStack;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO STACK(IJclSingleStack,IJclSingleContainer,8DCE45C8-B5B3-43AB-BA08-DAD531CEB9CF,const ,AValue,Single)}

{$JPPEXPANDMACRO STACK(IJclDoubleStack,IJclDoubleContainer,46DF2701-16F0-453C-B938-F04E9C1CEBF8,const ,AValue,Double)}

{$JPPEXPANDMACRO STACK(IJclExtendedStack,IJclExtendedContainer,A2A30585-F561-4757-ABE1-CA511AE72CC5,const ,AValue,Extended)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  IJclFloatStack = IJclExtendedStack;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  IJclFloatStack = IJclDoubleStack;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  IJclFloatStack = IJclSingleStack;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO STACK(IJclIntegerStack,IJclContainer,9190BF0E-5B0C-4D6C-A107-20A933C9B56A,,AValue,Integer)}

{$JPPEXPANDMACRO STACK(IJclCardinalStack,IJclContainer,94F9EDB3-602B-49CE-9990-0AFDAC556F83,,AValue,Cardinal)}

{$JPPEXPANDMACRO STACK(IJclInt64Stack,IJclContainer,D689EB8F-2746-40E9-AD1B-7E656475FC64,const ,AValue,Int64)}

  {$IFNDEF CLR}
{$JPPEXPANDMACRO STACK(IJclPtrStack,IJclContainer,AD11D06C-E0E1-4EDE-AA2F-BC8BDD972B73,,APtr,Pointer)}
  {$ENDIF ~CLR}

{$JPPEXPANDMACRO STACK(IJclStack,IJclContainer,E07E0BD8-A831-41B9-B9A0-7199BD4873B9,,AObject,TObject)}

  {$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO STACK(IJclStack<T>,IJclContainer,2F08EAC9-270D-496E-BE10-5E975918A5F2,const ,AItem,T)}
  {$ENDIF SUPPORTS_GENERICS}

  // Exceptions
  EJclContainerError = class(EJclError);

  EJclOutOfBoundsError = class(EJclContainerError)
  public
    // RsEOutOfBounds
    constructor Create;
  end;

  EJclNoSuchElementError = class(EJclContainerError)
  public
    // RsEValueNotFound
    constructor Create(const Value: string);
  end;

  EJclDuplicateElementError = class(EJclContainerError)
  public
    // RsEDuplicateElement
    constructor Create;
  end;

  EJclIllegalArgumentError = class(EJclContainerError)
  end;

  EJclNoCollectionError = class(EJclIllegalArgumentError)
  public
    // RsENoCollection
    constructor Create;
  end;

  EJclIllegalQueueCapacityError = class(EJclIllegalArgumentError)
  public
    // RsEIllegalQueueCapacity
    constructor Create;
  end;

  EJclOperationNotSupportedError = class(EJclContainerError)
  public
    // RsEOperationNotSupported
    constructor Create;
  end;

  EJclNoEqualityComparerError = class(EJclContainerError)
  public
    // RsENoEqualityComparer
    constructor Create;
  end;

  EJclNoComparerError = class(EJclContainerError)
  public
    // RsENoComparer
    constructor Create;
  end;

  EJclNoHashConverterError = class(EJclContainerError)
  public
    // RsENoHashConverter
    constructor Create;
  end;

  EJclIllegalStateOperationError = class(EJclContainerError)
  public
    // RsEIllegalStateOperation
    constructor Create;
  end;

  EJclAssignError = class(EJclContainerError)
  public
    // RsEAssignError
    constructor Create;
  end;

  EJclReadOnlyError = class(EJclContainerError)
  public
    // RsEReadOnlyError
    constructor Create;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/prototypes/JclContainerIntf.pas $';
    Revision: '$Revision: 2309 $';
    Date: '$Date: 2008-01-15 23:30:26 +0100 (mar., 15 janv. 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils,
  JclResources;

//=== { EJclOutOfBoundsError } ===============================================

constructor EJclOutOfBoundsError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsEOutOfBounds);
  {$ELSE ~CLR}
  inherited CreateRes(@RsEOutOfBounds);
  {$ENDIF ~CLR}
end;

//=== { EJclNoSuchElementError } =============================================

constructor EJclNoSuchElementError.Create(const Value: string);
begin
  {$IFDEF CLR}
  inherited Create(Format(RsEValueNotFound, [Value]));
  {$ELSE ~CLR}
  inherited CreateResFmt(@RsEValueNotFound, [Value]);
  {$ENDIF ~CLR}
end;

//=== { EJclDuplicateElementError } ==========================================

constructor EJclDuplicateElementError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsEDuplicateElement);
  {$ELSE ~CLR}
  inherited CreateRes(@RsEDuplicateElement);
  {$ENDIF ~CLR}
end;

//=== { EJclIllegalQueueCapacityError } ======================================

constructor EJclIllegalQueueCapacityError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsEIllegalQueueCapacity);
  {$ELSE ~CLR}
  inherited CreateRes(@RsEIllegalQueueCapacity);
  {$ENDIF ~CLR}
end;

//=== { EJclNoCollectionError } ==============================================

constructor EJclNoCollectionError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsENoCollection);
  {$ELSE ~CLR}
  inherited CreateRes(@RsENoCollection);
  {$ENDIF ~CLR}
end;

//=== { EJclOperationNotSupportedError } =====================================

constructor EJclOperationNotSupportedError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsEOperationNotSupported);
  {$ELSE ~CLR}
  inherited CreateRes(@RsEOperationNotSupported);
  {$ENDIF ~CLR}
end;

//=== { EJclIllegalStateOperationError } =====================================

constructor EJclIllegalStateOperationError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsEIllegalStateOperation);
  {$ELSE ~CLR}
  inherited CreateRes(@RsEIllegalStateOperation);
  {$ENDIF ~CLR}
end;

//=== { EJclNoComparerError } ================================================

constructor EJclNoComparerError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsENoComparer);
  {$ELSE ~CLR}
  inherited CreateRes(@RsENoComparer);
  {$ENDIF ~CLR}
end;

//=== { EJclNoEqualityComparerError } ========================================

constructor EJclNoEqualityComparerError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsENoEqualityComparer);
  {$ELSE ~CLR}
  inherited CreateRes(@RsENoEqualityComparer);
  {$ENDIF ~CLR}
end;

//=== { EJclNoHashConverterError } ===========================================

constructor EJclNoHashConverterError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsENoHashConverter);
  {$ELSE ~CLR}
  inherited CreateRes(@RsENoHashConverter);
  {$ENDIF ~CLR}
end;

//=== { EJclAssignError } ====================================================

constructor EJclAssignError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsEAssignError);
  {$ELSE ~CLR}
  inherited CreateRes(@RsEAssignError);
  {$ENDIF ~CLR}
end;

//=== { EJclReadOnlyError } ==================================================

constructor EJclReadOnlyError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsEReadOnlyError);
  {$ELSE ~CLR}
  inherited CreateRes(@RsEReadOnlyError);
  {$ENDIF ~CLR}
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

