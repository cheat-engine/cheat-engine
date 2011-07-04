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
{ Last modified: $Date:: 2008-06-05 15:35:37 +0200 (jeu., 05 juin 2008)                         $ }
{ Revision:      $Rev:: 2376                                                                     $ }
{ Author:        $Author:: obones                                                                $ }
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
{$I containers\JclContainerCommon.imp}
{$I containers\JclSortedMaps.imp}
{$I containers\JclSortedMaps.int}
type
(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfIntfSortedEntry,TJclIntfIntfSortedEntryArray,TJclIntfIntfSortedMap,TJclIntfAbstractContainer,IJclIntfIntfMap,IJclIntfIntfSortedMap,IJclIntfSet,IJclIntfCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,,const ,IInterface,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclAnsiStrIntfSortedEntry,TJclAnsiStrIntfSortedEntryArray,TJclAnsiStrIntfSortedMap,TJclAnsiStrAbstractContainer,IJclAnsiStrIntfMap,IJclAnsiStrIntfSortedMap,IJclAnsiStrSet,IJclIntfCollection, IJclStrContainer\, IJclAnsiStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: AnsiString): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,,const ,AnsiString,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfAnsiStrSortedEntry,TJclIntfAnsiStrSortedEntryArray,TJclIntfAnsiStrSortedMap,TJclAnsiStrAbstractContainer,IJclIntfAnsiStrMap,IJclIntfAnsiStrSortedMap,IJclIntfSet,IJclAnsiStrCollection, IJclStrContainer\, IJclAnsiStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: AnsiString): AnsiString;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: AnsiString): Integer;,,,,const ,IInterface,const ,AnsiString)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclAnsiStrAnsiStrSortedEntry,TJclAnsiStrAnsiStrSortedEntryArray,TJclAnsiStrAnsiStrSortedMap,TJclAnsiStrAbstractContainer,IJclAnsiStrAnsiStrMap,IJclAnsiStrAnsiStrSortedMap,IJclAnsiStrSet,IJclAnsiStrCollection, IJclStrContainer\, IJclAnsiStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: AnsiString): AnsiString;
    function KeysCompare(const A\, B: AnsiString): Integer;
    function ValuesCompare(const A\, B: AnsiString): Integer;,,,,const ,AnsiString,const ,AnsiString)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclWideStrIntfSortedEntry,TJclWideStrIntfSortedEntryArray,TJclWideStrIntfSortedMap,TJclWideStrAbstractContainer,IJclWideStrIntfMap,IJclWideStrIntfSortedMap,IJclWideStrSet,IJclIntfCollection, IJclStrContainer\, IJclWideStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: WideString): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,,const ,WideString,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfWideStrSortedEntry,TJclIntfWideStrSortedEntryArray,TJclIntfWideStrSortedMap,TJclWideStrAbstractContainer,IJclIntfWideStrMap,IJclIntfWideStrSortedMap,IJclIntfSet,IJclWideStrCollection, IJclStrContainer\, IJclWideStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: WideString): WideString;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: WideString): Integer;,,,,const ,IInterface,const ,WideString)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclWideStrWideStrSortedEntry,TJclWideStrWideStrSortedEntryArray,TJclWideStrWideStrSortedMap,TJclWideStrAbstractContainer,IJclWideStrWideStrMap,IJclWideStrWideStrSortedMap,IJclWideStrSet,IJclWideStrCollection, IJclStrContainer\, IJclWideStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: WideString): WideString;
    function KeysCompare(const A\, B: WideString): Integer;
    function ValuesCompare(const A\, B: WideString): Integer;,,,,const ,WideString,const ,WideString)*)

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

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclSingleIntfSortedEntry,TJclSingleIntfSortedEntryArray,TJclSingleIntfSortedMap,TJclSingleAbstractContainer,IJclSingleIntfMap,IJclSingleIntfSortedMap,IJclSingleSet,IJclIntfCollection, IJclSingleContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: Single): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,,const ,Single,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfSingleSortedEntry,TJclIntfSingleSortedEntryArray,TJclIntfSingleSortedMap,TJclSingleAbstractContainer,IJclIntfSingleMap,IJclIntfSingleSortedMap,IJclIntfSet,IJclSingleCollection, IJclSingleContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Single): Single;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: Single): Integer;,,,,const ,IInterface,const ,Single)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclSingleSingleSortedEntry,TJclSingleSingleSortedEntryArray,TJclSingleSingleSortedMap,TJclSingleAbstractContainer,IJclSingleSingleMap,IJclSingleSingleSortedMap,IJclSingleSet,IJclSingleCollection, IJclSingleContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function FreeValue(var Value: Single): Single;
    function KeysCompare(const A\, B: Single): Integer;
    function ValuesCompare(const A\, B: Single): Integer;,,,,const ,Single,const ,Single)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclDoubleIntfSortedEntry,TJclDoubleIntfSortedEntryArray,TJclDoubleIntfSortedMap,TJclDoubleAbstractContainer,IJclDoubleIntfMap,IJclDoubleIntfSortedMap,IJclDoubleSet,IJclIntfCollection, IJclDoubleContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: Double): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,,const ,Double,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfDoubleSortedEntry,TJclIntfDoubleSortedEntryArray,TJclIntfDoubleSortedMap,TJclDoubleAbstractContainer,IJclIntfDoubleMap,IJclIntfDoubleSortedMap,IJclIntfSet,IJclDoubleCollection, IJclDoubleContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Double): Double;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: Double): Integer;,,,,const ,IInterface,const ,Double)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclDoubleDoubleSortedEntry,TJclDoubleDoubleSortedEntryArray,TJclDoubleDoubleSortedMap,TJclDoubleAbstractContainer,IJclDoubleDoubleMap,IJclDoubleDoubleSortedMap,IJclDoubleSet,IJclDoubleCollection, IJclDoubleContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function FreeValue(var Value: Double): Double;
    function KeysCompare(const A\, B: Double): Integer;
    function ValuesCompare(const A\, B: Double): Integer;,,,,const ,Double,const ,Double)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclExtendedIntfSortedEntry,TJclExtendedIntfSortedEntryArray,TJclExtendedIntfSortedMap,TJclExtendedAbstractContainer,IJclExtendedIntfMap,IJclExtendedIntfSortedMap,IJclExtendedSet,IJclIntfCollection, IJclExtendedContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: Extended): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,,const ,Extended,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfExtendedSortedEntry,TJclIntfExtendedSortedEntryArray,TJclIntfExtendedSortedMap,TJclExtendedAbstractContainer,IJclIntfExtendedMap,IJclIntfExtendedSortedMap,IJclIntfSet,IJclExtendedCollection, IJclExtendedContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Extended): Extended;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: Extended): Integer;,,,,const ,IInterface,const ,Extended)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclExtendedExtendedSortedEntry,TJclExtendedExtendedSortedEntryArray,TJclExtendedExtendedSortedMap,TJclExtendedAbstractContainer,IJclExtendedExtendedMap,IJclExtendedExtendedSortedMap,IJclExtendedSet,IJclExtendedCollection, IJclExtendedContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function FreeValue(var Value: Extended): Extended;
    function KeysCompare(const A\, B: Extended): Integer;
    function ValuesCompare(const A\, B: Extended): Integer;,,,,const ,Extended,const ,Extended)*)

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

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntegerIntfSortedEntry,TJclIntegerIntfSortedEntryArray,TJclIntegerIntfSortedMap,TJclIntegerAbstractContainer,IJclIntegerIntfMap,IJclIntegerIntfSortedMap,IJclIntegerSet,IJclIntfCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(A\, B: Integer): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,,,Integer,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfIntegerSortedEntry,TJclIntfIntegerSortedEntryArray,TJclIntfIntegerSortedMap,TJclIntegerAbstractContainer,IJclIntfIntegerMap,IJclIntfIntegerSortedMap,IJclIntfSet,IJclIntegerCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Integer): Integer;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(A\, B: Integer): Integer;,,,,const ,IInterface,,Integer)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntegerIntegerSortedEntry,TJclIntegerIntegerSortedEntryArray,TJclIntegerIntegerSortedMap,TJclIntegerAbstractContainer,IJclIntegerIntegerMap,IJclIntegerIntegerSortedMap,IJclIntegerSet,IJclIntegerCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function FreeValue(var Value: Integer): Integer;
    function KeysCompare(A\, B: Integer): Integer;
    function ValuesCompare(A\, B: Integer): Integer;,,,,,Integer,,Integer)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclCardinalIntfSortedEntry,TJclCardinalIntfSortedEntryArray,TJclCardinalIntfSortedMap,TJclCardinalAbstractContainer,IJclCardinalIntfMap,IJclCardinalIntfSortedMap,IJclCardinalSet,IJclIntfCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(A\, B: Cardinal): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,,,Cardinal,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfCardinalSortedEntry,TJclIntfCardinalSortedEntryArray,TJclIntfCardinalSortedMap,TJclCardinalAbstractContainer,IJclIntfCardinalMap,IJclIntfCardinalSortedMap,IJclIntfSet,IJclCardinalCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Cardinal): Cardinal;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(A\, B: Cardinal): Integer;,,,,const ,IInterface,,Cardinal)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclCardinalCardinalSortedEntry,TJclCardinalCardinalSortedEntryArray,TJclCardinalCardinalSortedMap,TJclCardinalAbstractContainer,IJclCardinalCardinalMap,IJclCardinalCardinalSortedMap,IJclCardinalSet,IJclCardinalCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function FreeValue(var Value: Cardinal): Cardinal;
    function KeysCompare(A\, B: Cardinal): Integer;
    function ValuesCompare(A\, B: Cardinal): Integer;,,,,,Cardinal,,Cardinal)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclInt64IntfSortedEntry,TJclInt64IntfSortedEntryArray,TJclInt64IntfSortedMap,TJclInt64AbstractContainer,IJclInt64IntfMap,IJclInt64IntfSortedMap,IJclInt64Set,IJclIntfCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A\, B: Int64): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,,const ,Int64,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfInt64SortedEntry,TJclIntfInt64SortedEntryArray,TJclIntfInt64SortedMap,TJclInt64AbstractContainer,IJclIntfInt64Map,IJclIntfInt64SortedMap,IJclIntfSet,IJclInt64Collection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Int64): Int64;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(const A\, B: Int64): Integer;,,,,const ,IInterface,const ,Int64)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclInt64Int64SortedEntry,TJclInt64Int64SortedEntryArray,TJclInt64Int64SortedMap,TJclInt64AbstractContainer,IJclInt64Int64Map,IJclInt64Int64SortedMap,IJclInt64Set,IJclInt64Collection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function FreeValue(var Value: Int64): Int64;
    function KeysCompare(const A\, B: Int64): Integer;
    function ValuesCompare(const A\, B: Int64): Integer;,,,,const ,Int64,const ,Int64)*)

  {$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclPtrIntfSortedEntry,TJclPtrIntfSortedEntryArray,TJclPtrIntfSortedMap,TJclPtrAbstractContainer,IJclPtrIntfMap,IJclPtrIntfSortedMap,IJclPtrSet,IJclIntfCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(A\, B: Pointer): Integer;
    function ValuesCompare(const A\, B: IInterface): Integer;,,,,,Pointer,const ,IInterface)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfPtrSortedEntry,TJclIntfPtrSortedEntryArray,TJclIntfPtrSortedMap,TJclPtrAbstractContainer,IJclIntfPtrMap,IJclIntfPtrSortedMap,IJclIntfSet,IJclPtrCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Pointer): Pointer;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(A\, B: Pointer): Integer;,,,,const ,IInterface,,Pointer)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclPtrPtrSortedEntry,TJclPtrPtrSortedEntryArray,TJclPtrPtrSortedMap,TJclPtrAbstractContainer,IJclPtrPtrMap,IJclPtrPtrSortedMap,IJclPtrSet,IJclPtrCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function FreeValue(var Value: Pointer): Pointer;
    function KeysCompare(A\, B: Pointer): Integer;
    function ValuesCompare(A\, B: Pointer): Integer;,,,,,Pointer,,Pointer)*)
  {$ENDIF ~CLR}

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntfSortedEntry,TJclIntfSortedEntryArray,TJclIntfSortedMap,TJclIntfAbstractContainer,IJclIntfMap,IJclIntfSortedMap,IJclIntfSet,IJclCollection, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function KeysCompare(const A\, B: IInterface): Integer;
    function ValuesCompare(A\, B: TObject): Integer;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,IInterface,,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclAnsiStrSortedEntry,TJclAnsiStrSortedEntryArray,TJclAnsiStrSortedMap,TJclAnsiStrAbstractContainer,IJclAnsiStrMap,IJclAnsiStrSortedMap,IJclAnsiStrSet,IJclCollection, IJclStrContainer\, IJclAnsiStrContainer\, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function KeysCompare(const A\, B: AnsiString): Integer;
    function ValuesCompare(A\, B: TObject): Integer;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,AnsiString,,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclWideStrSortedEntry,TJclWideStrSortedEntryArray,TJclWideStrSortedMap,TJclWideStrAbstractContainer,IJclWideStrMap,IJclWideStrSortedMap,IJclWideStrSet,IJclCollection, IJclStrContainer\, IJclWideStrContainer\, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function KeysCompare(const A\, B: WideString): Integer;
    function ValuesCompare(A\, B: TObject): Integer;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,WideString,,TObject)*)

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrSortedMap = TJclAnsiStrSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrSortedMap = TJclWideStrSortedMap;
  {$ENDIF CONTAINER_WIDESTR}

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclSingleSortedEntry,TJclSingleSortedEntryArray,TJclSingleSortedMap,TJclSingleAbstractContainer,IJclSingleMap,IJclSingleSortedMap,IJclSingleSet,IJclCollection, IJclSingleContainer\, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function KeysCompare(const A\, B: Single): Integer;
    function ValuesCompare(A\, B: TObject): Integer;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,Single,,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclDoubleSortedEntry,TJclDoubleSortedEntryArray,TJclDoubleSortedMap,TJclDoubleAbstractContainer,IJclDoubleMap,IJclDoubleSortedMap,IJclDoubleSet,IJclCollection, IJclDoubleContainer\, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function KeysCompare(const A\, B: Double): Integer;
    function ValuesCompare(A\, B: TObject): Integer;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,Double,,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclExtendedSortedEntry,TJclExtendedSortedEntryArray,TJclExtendedSortedMap,TJclExtendedAbstractContainer,IJclExtendedMap,IJclExtendedSortedMap,IJclExtendedSet,IJclCollection, IJclExtendedContainer\, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function KeysCompare(const A\, B: Extended): Integer;
    function ValuesCompare(A\, B: TObject): Integer;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,Extended,,TObject)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatSortedMap = TJclExtendedSortedMap;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatSortedMap = TJclDoubleSortedMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatSortedMap = TJclSingleSortedMap;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclIntegerSortedEntry,TJclIntegerSortedEntryArray,TJclIntegerSortedMap,TJclIntegerAbstractContainer,IJclIntegerMap,IJclIntegerSortedMap,IJclIntegerSet,IJclCollection, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function KeysCompare(A\, B: Integer): Integer;
    function ValuesCompare(A\, B: TObject): Integer;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,,Integer,,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclCardinalSortedEntry,TJclCardinalSortedEntryArray,TJclCardinalSortedMap,TJclCardinalAbstractContainer,IJclCardinalMap,IJclCardinalSortedMap,IJclCardinalSet,IJclCollection, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function KeysCompare(A\, B: Cardinal): Integer;
    function ValuesCompare(A\, B: TObject): Integer;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,,Cardinal,,TObject)*)

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclInt64SortedEntry,TJclInt64SortedEntryArray,TJclInt64SortedMap,TJclInt64AbstractContainer,IJclInt64Map,IJclInt64SortedMap,IJclInt64Set,IJclCollection, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function KeysCompare(const A\, B: Int64): Integer;
    function ValuesCompare(A\, B: TObject): Integer;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const ,Int64,,TObject)*)

  {$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclPtrSortedEntry,TJclPtrSortedEntryArray,TJclPtrSortedMap,TJclPtrAbstractContainer,IJclPtrMap,IJclPtrSortedMap,IJclPtrSet,IJclCollection, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function KeysCompare(A\, B: Pointer): Integer;
    function ValuesCompare(A\, B: TObject): Integer;,
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,,Pointer,,TObject)*)
  {$ENDIF ~CLR}

(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclSortedEntry,TJclSortedEntryArray,TJclSortedMap,TJclAbstractContainerBase,IJclMap,IJclSortedMap,IJclSet,IJclCollection, IJclKeyOwner\, IJclValueOwner\,,
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;,
    { IJclKeyOwner }
    function FreeKey(var Key: TObject): TObject;
    function GetOwnsKeys: Boolean;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function KeysCompare(A\, B: TObject): Integer;
    function ValuesCompare(A\, B: TObject): Integer;,
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,,TObject,,TObject)*)

  {$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLSORTEDMAPINT(TJclSortedEntry<TKey\,TValue>,TJclSortedEntryArray<TKey\,TValue>,TJclSortedMap<TKey\,TValue>,TJclAbstractContainerBase,IJclMap<TKey\,TValue>,IJclSortedMap<TKey\,TValue>,IJclSet<TKey>,IJclCollection<TValue>, IJclPairOwner<TKey\,TValue>\,,
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;,
    { IJclPairOwner }
    function FreeKey(var Key: TKey): TKey;
    function FreeValue(var Value: TValue): TValue;
    function GetOwnsKeys: Boolean;
    function GetOwnsValues: Boolean;
    function KeysCompare(const A\, B: TKey): Integer; virtual; abstract;
    function ValuesCompare(const A\, B: TValue): Integer; virtual; abstract;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; virtual; abstract;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; virtual; abstract;,
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,const ,TKey,const ,TValue)*)

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
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/prototypes/JclSortedMaps.pas $';
    Revision: '$Revision: 2376 $';
    Date: '$Date: 2008-06-05 15:35:37 +0200 (jeu., 05 juin 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils,
  JclArrayLists,
  JclArraySets;

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfIntfSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclIntfIntfSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclIntfIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfIntfSortedMap,TJclIntfIntfSortedEntry,IJclIntfIntfMap,IJclIntfIntfSortedMap,IJclIntfSet,IJclIntfIterator,IJclIntfCollection,,,,const ,IInterface,nil,const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclAnsiStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclAnsiStrIntfSortedMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclAnsiStrIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclAnsiStrIntfSortedMap.KeysCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclAnsiStrIntfSortedMap,TJclAnsiStrIntfSortedEntry,IJclAnsiStrIntfMap,IJclAnsiStrIntfSortedMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclIntfCollection,,,,const ,AnsiString,'',const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfAnsiStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfAnsiStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclAnsiStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfAnsiStrSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfAnsiStrSortedMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
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
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclIntfAnsiStrSortedMap.ValuesCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfAnsiStrSortedMap,TJclIntfAnsiStrSortedEntry,IJclIntfAnsiStrMap,IJclIntfAnsiStrSortedMap,IJclIntfSet,IJclIntfIterator,IJclAnsiStrCollection,,,,const ,IInterface,nil,const ,AnsiString,'')}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrAnsiStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrAnsiStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclAnsiStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclAnsiStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclAnsiStrAnsiStrSortedMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclAnsiStrAnsiStrSortedMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclAnsiStrAnsiStrSortedMap.KeysCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclAnsiStrAnsiStrSortedMap.ValuesCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclAnsiStrAnsiStrSortedMap,TJclAnsiStrAnsiStrSortedEntry,IJclAnsiStrAnsiStrMap,IJclAnsiStrAnsiStrSortedMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclAnsiStrCollection,,,,const ,AnsiString,'',const ,AnsiString,'')}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclWideStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclWideStrIntfSortedMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclWideStrIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclWideStrIntfSortedMap.KeysCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclWideStrIntfSortedMap,TJclWideStrIntfSortedEntry,IJclWideStrIntfMap,IJclWideStrIntfSortedMap,IJclWideStrSet,IJclWideStrIterator,IJclIntfCollection,,,,const ,WideString,'',const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfWideStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfWideStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclWideStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfWideStrSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfWideStrSortedMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
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
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclIntfWideStrSortedMap.ValuesCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfWideStrSortedMap,TJclIntfWideStrSortedEntry,IJclIntfWideStrMap,IJclIntfWideStrSortedMap,IJclIntfSet,IJclIntfIterator,IJclWideStrCollection,,,,const ,IInterface,nil,const ,WideString,'')}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrWideStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrWideStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclWideStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclWideStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclWideStrWideStrSortedMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclWideStrWideStrSortedMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclWideStrWideStrSortedMap.KeysCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclWideStrWideStrSortedMap.ValuesCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclWideStrWideStrSortedMap,TJclWideStrWideStrSortedEntry,IJclWideStrWideStrMap,IJclWideStrWideStrSortedMap,IJclWideStrSet,IJclWideStrIterator,IJclWideStrCollection,,,,const ,WideString,'',const ,WideString,'')}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclSingleIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclSingleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclSingleIntfSortedMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclSingleIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclSingleIntfSortedMap.KeysCompare(const A, B: Single): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclSingleIntfSortedMap,TJclSingleIntfSortedEntry,IJclSingleIntfMap,IJclSingleIntfSortedMap,IJclSingleSet,IJclSingleIterator,IJclIntfCollection,,,,const ,Single,0.0,const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfSingleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfSingleSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclSingleArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfSingleSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfSingleSortedMap.FreeValue(var Value: Single): Single;
begin
  Result := Value;
  Value := 0.0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
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
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclIntfSingleSortedMap.ValuesCompare(const A, B: Single): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfSingleSortedMap,TJclIntfSingleSortedEntry,IJclIntfSingleMap,IJclIntfSingleSortedMap,IJclIntfSet,IJclIntfIterator,IJclSingleCollection,,,,const ,IInterface,nil,const ,Single,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclSingleSingleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleSingleSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclSingleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclSingleArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclSingleSingleSortedMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclSingleSingleSortedMap.FreeValue(var Value: Single): Single;
begin
  Result := Value;
  Value := 0.0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclSingleSingleSortedMap.KeysCompare(const A, B: Single): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclSingleSingleSortedMap.ValuesCompare(const A, B: Single): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclSingleSingleSortedMap,TJclSingleSingleSortedEntry,IJclSingleSingleMap,IJclSingleSingleSortedMap,IJclSingleSet,IJclSingleIterator,IJclSingleCollection,,,,const ,Single,0.0,const ,Single,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclDoubleIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclDoubleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclDoubleIntfSortedMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclDoubleIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclDoubleIntfSortedMap.KeysCompare(const A, B: Double): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclDoubleIntfSortedMap,TJclDoubleIntfSortedEntry,IJclDoubleIntfMap,IJclDoubleIntfSortedMap,IJclDoubleSet,IJclDoubleIterator,IJclIntfCollection,,,,const ,Double,0.0,const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfDoubleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfDoubleSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclDoubleArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfDoubleSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfDoubleSortedMap.FreeValue(var Value: Double): Double;
begin
  Result := Value;
  Value := 0.0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
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
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclIntfDoubleSortedMap.ValuesCompare(const A, B: Double): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfDoubleSortedMap,TJclIntfDoubleSortedEntry,IJclIntfDoubleMap,IJclIntfDoubleSortedMap,IJclIntfSet,IJclIntfIterator,IJclDoubleCollection,,,,const ,IInterface,nil,const ,Double,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclDoubleDoubleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleDoubleSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclDoubleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclDoubleArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclDoubleDoubleSortedMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclDoubleDoubleSortedMap.FreeValue(var Value: Double): Double;
begin
  Result := Value;
  Value := 0.0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclDoubleDoubleSortedMap.KeysCompare(const A, B: Double): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclDoubleDoubleSortedMap.ValuesCompare(const A, B: Double): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclDoubleDoubleSortedMap,TJclDoubleDoubleSortedEntry,IJclDoubleDoubleMap,IJclDoubleDoubleSortedMap,IJclDoubleSet,IJclDoubleIterator,IJclDoubleCollection,,,,const ,Double,0.0,const ,Double,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclExtendedIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclExtendedArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclExtendedIntfSortedMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclExtendedIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclExtendedIntfSortedMap.KeysCompare(const A, B: Extended): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclExtendedIntfSortedMap,TJclExtendedIntfSortedEntry,IJclExtendedIntfMap,IJclExtendedIntfSortedMap,IJclExtendedSet,IJclExtendedIterator,IJclIntfCollection,,,,const ,Extended,0.0,const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfExtendedSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfExtendedSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclExtendedArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfExtendedSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfExtendedSortedMap.FreeValue(var Value: Extended): Extended;
begin
  Result := Value;
  Value := 0.0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
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
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclIntfExtendedSortedMap.ValuesCompare(const A, B: Extended): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfExtendedSortedMap,TJclIntfExtendedSortedEntry,IJclIntfExtendedMap,IJclIntfExtendedSortedMap,IJclIntfSet,IJclIntfIterator,IJclExtendedCollection,,,,const ,IInterface,nil,const ,Extended,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclExtendedExtendedSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedExtendedSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclExtendedArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclExtendedArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclExtendedExtendedSortedMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclExtendedExtendedSortedMap.FreeValue(var Value: Extended): Extended;
begin
  Result := Value;
  Value := 0.0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclExtendedExtendedSortedMap.KeysCompare(const A, B: Extended): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclExtendedExtendedSortedMap.ValuesCompare(const A, B: Extended): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclExtendedExtendedSortedMap,TJclExtendedExtendedSortedEntry,IJclExtendedExtendedMap,IJclExtendedExtendedSortedMap,IJclExtendedSet,IJclExtendedIterator,IJclExtendedCollection,,,,const ,Extended,0.0,const ,Extended,0.0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntegerIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntegerArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntegerIntfSortedMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntegerIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclIntegerIntfSortedMap.KeysCompare(A, B: Integer): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntegerIntfSortedMap,TJclIntegerIntfSortedEntry,IJclIntegerIntfMap,IJclIntegerIntfSortedMap,IJclIntegerSet,IJclIntegerIterator,IJclIntfCollection,,,,,Integer,0,const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfIntegerSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntegerSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntegerArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfIntegerSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfIntegerSortedMap.FreeValue(var Value: Integer): Integer;
begin
  Result := Value;
  Value := 0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
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
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclIntfIntegerSortedMap.ValuesCompare(A, B: Integer): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfIntegerSortedMap,TJclIntfIntegerSortedEntry,IJclIntfIntegerMap,IJclIntfIntegerSortedMap,IJclIntfSet,IJclIntfIterator,IJclIntegerCollection,,,,const ,IInterface,nil,,Integer,0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntegerIntegerSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerIntegerSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntegerArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntegerArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntegerIntegerSortedMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntegerIntegerSortedMap.FreeValue(var Value: Integer): Integer;
begin
  Result := Value;
  Value := 0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclIntegerIntegerSortedMap.KeysCompare(A, B: Integer): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclIntegerIntegerSortedMap.ValuesCompare(A, B: Integer): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntegerIntegerSortedMap,TJclIntegerIntegerSortedEntry,IJclIntegerIntegerMap,IJclIntegerIntegerSortedMap,IJclIntegerSet,IJclIntegerIterator,IJclIntegerCollection,,,,,Integer,0,,Integer,0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclCardinalIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclCardinalArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclCardinalIntfSortedMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclCardinalIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclCardinalIntfSortedMap.KeysCompare(A, B: Cardinal): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclCardinalIntfSortedMap,TJclCardinalIntfSortedEntry,IJclCardinalIntfMap,IJclCardinalIntfSortedMap,IJclCardinalSet,IJclCardinalIterator,IJclIntfCollection,,,,,Cardinal,0,const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfCardinalSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfCardinalSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclCardinalArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfCardinalSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfCardinalSortedMap.FreeValue(var Value: Cardinal): Cardinal;
begin
  Result := Value;
  Value := 0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
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
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclIntfCardinalSortedMap.ValuesCompare(A, B: Cardinal): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfCardinalSortedMap,TJclIntfCardinalSortedEntry,IJclIntfCardinalMap,IJclIntfCardinalSortedMap,IJclIntfSet,IJclIntfIterator,IJclCardinalCollection,,,,const ,IInterface,nil,,Cardinal,0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclCardinalCardinalSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalCardinalSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclCardinalArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclCardinalArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclCardinalCardinalSortedMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclCardinalCardinalSortedMap.FreeValue(var Value: Cardinal): Cardinal;
begin
  Result := Value;
  Value := 0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclCardinalCardinalSortedMap.KeysCompare(A, B: Cardinal): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclCardinalCardinalSortedMap.ValuesCompare(A, B: Cardinal): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclCardinalCardinalSortedMap,TJclCardinalCardinalSortedEntry,IJclCardinalCardinalMap,IJclCardinalCardinalSortedMap,IJclCardinalSet,IJclCardinalIterator,IJclCardinalCollection,,,,,Cardinal,0,,Cardinal,0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclInt64IntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64IntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclInt64ArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclInt64IntfSortedMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclInt64IntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclInt64IntfSortedMap.KeysCompare(const A, B: Int64): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclInt64IntfSortedMap,TJclInt64IntfSortedEntry,IJclInt64IntfMap,IJclInt64IntfSortedMap,IJclInt64Set,IJclInt64Iterator,IJclIntfCollection,,,,const ,Int64,0,const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfInt64SortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfInt64SortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclInt64ArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfInt64SortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfInt64SortedMap.FreeValue(var Value: Int64): Int64;
begin
  Result := Value;
  Value := 0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
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
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclIntfInt64SortedMap.ValuesCompare(const A, B: Int64): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfInt64SortedMap,TJclIntfInt64SortedEntry,IJclIntfInt64Map,IJclIntfInt64SortedMap,IJclIntfSet,IJclIntfIterator,IJclInt64Collection,,,,const ,IInterface,nil,const ,Int64,0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclInt64Int64SortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Int64SortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclInt64ArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclInt64ArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclInt64Int64SortedMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclInt64Int64SortedMap.FreeValue(var Value: Int64): Int64;
begin
  Result := Value;
  Value := 0;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclInt64Int64SortedMap.KeysCompare(const A, B: Int64): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclInt64Int64SortedMap.ValuesCompare(const A, B: Int64): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclInt64Int64SortedMap,TJclInt64Int64SortedEntry,IJclInt64Int64Map,IJclInt64Int64SortedMap,IJclInt64Set,IJclInt64Iterator,IJclInt64Collection,,,,const ,Int64,0,const ,Int64,0)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$IFNDEF CLR}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclPtrIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclPtrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclPtrIntfSortedMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclPtrIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclPtrIntfSortedMap.KeysCompare(A, B: Pointer): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclPtrIntfSortedMap,TJclPtrIntfSortedEntry,IJclPtrIntfMap,IJclPtrIntfSortedMap,IJclPtrSet,IJclPtrIterator,IJclIntfCollection,,,,,Pointer,nil,const ,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfPtrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfPtrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclPtrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfPtrSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfPtrSortedMap.FreeValue(var Value: Pointer): Pointer;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
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
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclIntfPtrSortedMap.ValuesCompare(A, B: Pointer): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfPtrSortedMap,TJclIntfPtrSortedEntry,IJclIntfPtrMap,IJclIntfPtrSortedMap,IJclIntfSet,IJclIntfIterator,IJclPtrCollection,,,,const ,IInterface,nil,,Pointer,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclPtrPtrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrPtrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclPtrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclPtrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclPtrPtrSortedMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclPtrPtrSortedMap.FreeValue(var Value: Pointer): Pointer;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclPtrPtrSortedMap.KeysCompare(A, B: Pointer): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
function TJclPtrPtrSortedMap.ValuesCompare(A, B: Pointer): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclPtrPtrSortedMap,TJclPtrPtrSortedEntry,IJclPtrPtrMap,IJclPtrPtrSortedMap,IJclPtrSet,IJclPtrIterator,IJclPtrCollection,,,,,Pointer,nil,,Pointer,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}
{$ENDIF ~CLR}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
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
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclIntfSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO KEYSCOMPARE
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
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntfSortedMap,TJclIntfSortedEntry,IJclIntfMap,IJclIntfSortedMap,IJclIntfSet,IJclIntfIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const ,IInterface,nil,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclAnsiStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclAnsiStrSortedMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
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
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclAnsiStrSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclAnsiStrSortedMap.KeysCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclAnsiStrSortedMap,TJclAnsiStrSortedEntry,IJclAnsiStrMap,IJclAnsiStrSortedMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const ,AnsiString,'',,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclWideStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclWideStrSortedMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
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
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclWideStrSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclWideStrSortedMap.KeysCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclWideStrSortedMap,TJclWideStrSortedEntry,IJclWideStrMap,IJclWideStrSortedMap,IJclWideStrSet,IJclWideStrIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const ,WideString,'',,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclSingleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclSingleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclSingleSortedMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
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
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclSingleSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclSingleSortedMap.KeysCompare(const A, B: Single): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclSingleSortedMap,TJclSingleSortedEntry,IJclSingleMap,IJclSingleSortedMap,IJclSingleSet,IJclSingleIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const ,Single,0.0,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclDoubleSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclDoubleArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclDoubleSortedMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
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
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclDoubleSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclDoubleSortedMap.KeysCompare(const A, B: Double): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclDoubleSortedMap,TJclDoubleSortedEntry,IJclDoubleMap,IJclDoubleSortedMap,IJclDoubleSet,IJclDoubleIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const ,Double,0.0,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclExtendedSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclExtendedArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclExtendedSortedMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
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
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclExtendedSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclExtendedSortedMap.KeysCompare(const A, B: Extended): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclExtendedSortedMap,TJclExtendedSortedEntry,IJclExtendedMap,IJclExtendedSortedMap,IJclExtendedSet,IJclExtendedIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const ,Extended,0.0,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntegerSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntegerArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntegerSortedMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
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
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclIntegerSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclIntegerSortedMap.KeysCompare(A, B: Integer): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclIntegerSortedMap,TJclIntegerSortedEntry,IJclIntegerMap,IJclIntegerSortedMap,IJclIntegerSet,IJclIntegerIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,,Integer,0,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclCardinalSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclCardinalArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclCardinalSortedMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
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
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclCardinalSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclCardinalSortedMap.KeysCompare(A, B: Cardinal): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclCardinalSortedMap,TJclCardinalSortedEntry,IJclCardinalMap,IJclCardinalSortedMap,IJclCardinalSet,IJclCardinalIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,,Cardinal,0,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclInt64SortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64SortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclInt64ArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclInt64SortedMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;
}
{$JPPDEFINEMACRO FREEVALUE
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
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclInt64SortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclInt64SortedMap.KeysCompare(const A, B: Int64): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclInt64SortedMap,TJclInt64SortedEntry,IJclInt64Map,IJclInt64SortedMap,IJclInt64Set,IJclInt64Iterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,const ,Int64,0,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$IFNDEF CLR}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclPtrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclPtrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclPtrSortedMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
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
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclPtrSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO KEYSCOMPARE
function TJclPtrSortedMap.KeysCompare(A, B: Pointer): Integer;
begin
  Result := ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclPtrSortedMap,TJclPtrSortedEntry,IJclPtrMap,IJclPtrSortedMap,IJclPtrSet,IJclPtrIterator,IJclCollection,,; AOwnsValues: Boolean,
  FOwnsValues := AOwnsValues;,,Pointer,nil,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}
{$ENDIF ~CLR}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSortedMap.Create(FSize, False, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclArraySet.Create(Param, False)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
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
}
{$JPPDEFINEMACRO FREEVALUE
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
}
{$JPPDEFINEMACRO GETOWNSKEYS
function TJclSortedMap.GetOWnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;
}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO KEYSCOMPARE
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
}
{$JPPDEFINEMACRO VALUESCOMPARE
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
}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclSortedMap,TJclSortedEntry,IJclMap,IJclSortedMap,IJclSet,IJclIterator,IJclCollection,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;,,TObject,nil,,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

{$IFDEF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)CreateEmptyArraySet(Param, False)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)CreateEmptyArrayList(Param, False)}
{$JPPDEFINEMACRO FREEKEY
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
}
{$JPPDEFINEMACRO FREEVALUE
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
}
{$JPPDEFINEMACRO GETOWNSKEYS
function TJclSortedMap<TKey,TValue>.GetOWnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;
}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclSortedMap<TKey,TValue>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO KEYSCOMPARE}
{$JPPDEFINEMACRO VALUESCOMPARE}
{$JPPEXPANDMACRO JCLSORTEDMAPIMP(TJclSortedMap<TKey\,TValue>,TJclSortedEntry<TKey\,TValue>,IJclMap<TKey\,TValue>,IJclSortedMap<TKey\,TValue>,IJclSet<TKey>,IJclIterator<TKey>,IJclCollection<TValue>,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;,const ,TKey,Default(TKey),const ,TValue,Default(TValue))}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO KEYSCOMPARE}
{$JPPUNDEFMACRO VALUESCOMPARE}

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
