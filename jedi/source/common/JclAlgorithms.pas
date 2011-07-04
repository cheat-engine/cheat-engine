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
{ The Original Code is Algorithms.pas.                                                             }
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
{ Last modified: $Date:: 2008-01-15 23:30:26 +0100 (mar., 15 janv. 2008)                         $ }
{ Revision:      $Rev:: 2309                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclAlgorithms;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase, JclContainerIntf;

// Compare functions
function IntfSimpleCompare(const Obj1, Obj2: IInterface): Integer;
function AnsiStrSimpleCompare(const Obj1, Obj2: AnsiString): Integer;
function WideStrSimpleCompare(const Obj1, Obj2: WideString): Integer;
function StrSimpleCompare(const Obj1, Obj2: string): Integer;
function SingleSimpleCompare(const Obj1, Obj2: Single): Integer;
function DoubleSimpleCompare(const Obj1, Obj2: Double): Integer;
function ExtendedSimpleCompare(const Obj1, Obj2: Extended): Integer;
function FloatSimpleCompare(const Obj1, Obj2: Float): Integer;
function IntegerSimpleCompare(Obj1, Obj2: Integer): Integer;
function CardinalSimpleCompare(Obj1, Obj2: Cardinal): Integer;
function Int64SimpleCompare(const Obj1, Obj2: Int64): Integer;
{$IFNDEF CLR}
function PtrSimpleCompare(Obj1, Obj2: Pointer): Integer;
{$ENDIF ~CLR}
function SimpleCompare(Obj1, Obj2: TObject): Integer;

function IntegerCompare(Obj1, Obj2: TObject): Integer;

// Compare functions for equality
function IntfSimpleEqualityCompare(const Obj1, Obj2: IInterface): Boolean;
function AnsiStrSimpleEqualityCompare(const Obj1, Obj2: AnsiString): Boolean;
function WideStrSimpleEqualityCompare(const Obj1, Obj2: WideString): Boolean;
function StrSimpleEqualityCompare(const Obj1, Obj2: string): Boolean;
function SingleSimpleEqualityCompare(const Obj1, Obj2: Single): Boolean;
function DoubleSimpleEqualityCompare(const Obj1, Obj2: Double): Boolean;
function ExtendedSimpleEqualityCompare(const Obj1, Obj2: Extended): Boolean;
function FloatSimpleEqualityCompare(const Obj1, Obj2: Float): Boolean;
function IntegerSimpleEqualityCompare(Obj1, Obj2: Integer): Boolean;
function CardinalSimpleEqualityCompare(Obj1, Obj2: Cardinal): Boolean;
function Int64SimpleEqualityCompare(const Obj1, Obj2: Int64): Boolean;
{$IFNDEF CLR}
function PtrSimpleEqualityCompare(Obj1, Obj2: Pointer): Boolean;
{$ENDIF ~CLR}
function SimpleEqualityCompare(Obj1, Obj2: TObject): Boolean;

// Apply algorithms
procedure Apply(const First: IJclIntfIterator; Count: Integer; F: TIntfApplyFunction); overload;
procedure Apply(const First: IJclAnsiStrIterator; Count: Integer; F: TAnsiStrApplyFunction); overload;
procedure Apply(const First: IJclWideStrIterator; Count: Integer; F: TWideStrApplyFunction); overload;
procedure Apply(const First: IJclSingleIterator; Count: Integer; F: TSingleApplyFunction); overload;
procedure Apply(const First: IJclDoubleIterator; Count: Integer; F: TDoubleApplyFunction); overload;
procedure Apply(const First: IJclExtendedIterator; Count: Integer; F: TExtendedApplyFunction); overload;
procedure Apply(const First: IJclIntegerIterator; Count: Integer; F: TIntegerApplyFunction); overload;
procedure Apply(const First: IJclCardinalIterator; Count: Integer; F: TCardinalApplyFunction); overload;
procedure Apply(const First: IJclInt64Iterator; Count: Integer; F: TInt64ApplyFunction); overload;
{$IFNDEF CLR}
procedure Apply(const First: IJclPtrIterator; Count: Integer; F: TPtrApplyFunction); overload;
{$ENDIF ~CLR}
procedure Apply(const First: IJclIterator; Count: Integer; F: TApplyFunction); overload;

// Find algorithms
function Find(const First: IJclIntfIterator; Count: Integer; const AInterface: IInterface;
  AComparator: TIntfCompare): IJclIntfIterator; overload;
function Find(const First: IJclIntfIterator; Count: Integer; const AInterface: IInterface;
  AEqualityComparator: TIntfEqualityCompare): IJclIntfIterator; overload;
function Find(const First: IJclAnsiStrIterator; Count: Integer; const AString: AnsiString;
  AComparator: TAnsiStrCompare): IJclAnsiStrIterator; overload;
function Find(const First: IJclAnsiStrIterator; Count: Integer; const AString: AnsiString;
  AEqualityComparator: TAnsiStrEqualityCompare): IJclAnsiStrIterator; overload;
function Find(const First: IJclWideStrIterator; Count: Integer; const AString: WideString;
  AComparator: TWideStrCompare): IJclWideStrIterator; overload;
function Find(const First: IJclWideStrIterator; Count: Integer; const AString: WideString;
  AEqualityComparator: TWideStrEqualityCompare): IJclWideStrIterator; overload;
function Find(const First: IJclSingleIterator; Count: Integer; const AValue: Single;
  AComparator: TSingleCompare): IJclSingleIterator; overload;
function Find(const First: IJclSingleIterator; Count: Integer; const AValue: Single;
  AEqualityComparator: TSingleEqualityCompare): IJclSingleIterator; overload;
function Find(const First: IJclDoubleIterator; Count: Integer; const AValue: Double;
  AComparator: TDoubleCompare): IJclDoubleIterator; overload;
function Find(const First: IJclDoubleIterator; Count: Integer; const AValue: Double;
  AEqualityComparator: TDoubleEqualityCompare): IJclDoubleIterator; overload;
function Find(const First: IJclExtendedIterator; Count: Integer; const AValue: Extended;
  AComparator: TExtendedCompare): IJclExtendedIterator; overload;
function Find(const First: IJclExtendedIterator; Count: Integer; const AValue: Extended;
  AEqualityComparator: TExtendedEqualityCompare): IJclExtendedIterator; overload;
function Find(const First: IJclIntegerIterator; Count: Integer; AValue: Integer;
  AComparator: TIntegerCompare): IJclIntegerIterator; overload;
function Find(const First: IJclIntegerIterator; Count: Integer; AValue: Integer;
  AEqualityComparator: TIntegerEqualityCompare): IJclIntegerIterator; overload;
function Find(const First: IJclCardinalIterator; Count: Integer; AValue: Cardinal;
  AComparator: TCardinalCompare): IJclCardinalIterator; overload;
function Find(const First: IJclCardinalIterator; Count: Integer; AValue: Cardinal;
  AEqualityComparator: TCardinalEqualityCompare): IJclCardinalIterator; overload;
function Find(const First: IJclInt64Iterator; Count: Integer; const AValue: Int64;
  AComparator: TInt64Compare): IJclInt64Iterator; overload;
function Find(const First: IJclInt64Iterator; Count: Integer; const AValue: Int64;
  AEqualityComparator: TInt64EqualityCompare): IJclInt64Iterator; overload;
{$IFNDEF CLR}
function Find(const First: IJclPtrIterator; Count: Integer; APtr: Pointer;
  AComparator: TPtrCompare): IJclPtrIterator; overload;
function Find(const First: IJclPtrIterator; Count: Integer; APtr: Pointer;
  AEqualityComparator: TPtrEqualityCompare): IJclPtrIterator; overload;
{$ENDIF ~CLR}
function Find(const First: IJclIterator; Count: Integer; AObject: TObject;
  AComparator: TCompare): IJclIterator; overload;
function Find(const First: IJclIterator; Count: Integer; AObject: TObject;
  AEqualityComparator: TEqualityCompare): IJclIterator; overload;

// CountObject algorithms
function CountObject(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface; AComparator: TIntfCompare): Integer; overload;
function CountObject(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface; AEqualityComparator: TIntfEqualityCompare): Integer; overload;
function CountObject(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: AnsiString; AComparator: TAnsiStrCompare): Integer; overload;
function CountObject(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: AnsiString; AEqualityComparator: TAnsiStrEqualityCompare): Integer; overload;
function CountObject(const First: IJclWideStrIterator; Count: Integer;
  const AString: WideString; AComparator: TWideStrCompare): Integer; overload;
function CountObject(const First: IJclWideStrIterator; Count: Integer;
  const AString: WideString; AEqualityComparator: TWideStrEqualityCompare): Integer; overload;
function CountObject(const First: IJclSingleIterator; Count: Integer;
  const AValue: Single; AComparator: TSingleCompare): Integer; overload;
function CountObject(const First: IJclSingleIterator; Count: Integer;
  const AValue: Single; AEqualityComparator: TSingleEqualityCompare): Integer; overload;
function CountObject(const First: IJclDoubleIterator; Count: Integer;
  const AValue: Double; AComparator: TDoubleCompare): Integer; overload;
function CountObject(const First: IJclDoubleIterator; Count: Integer;
  const AValue: Double; AEqualityComparator: TDoubleEqualityCompare): Integer; overload;
function CountObject(const First: IJclExtendedIterator; Count: Integer;
  const AValue: Extended; AComparator: TExtendedCompare): Integer; overload;
function CountObject(const First: IJclExtendedIterator; Count: Integer;
  const AValue: Extended; AEqualityComparator: TExtendedEqualityCompare): Integer; overload;
function CountObject(const First: IJclIntegerIterator; Count: Integer;
  AValue: Integer; AComparator: TIntegerCompare): Integer; overload;
function CountObject(const First: IJclIntegerIterator; Count: Integer;
  AValue: Integer; AEqualityComparator: TIntegerEqualityCompare): Integer; overload;
function CountObject(const First: IJclCardinalIterator; Count: Integer;
  AValue: Cardinal; AComparator: TCardinalCompare): Integer; overload;
function CountObject(const First: IJclCardinalIterator; Count: Integer;
  AValue: Cardinal; AEqualityComparator: TCardinalEqualityCompare): Integer; overload;
function CountObject(const First: IJclInt64Iterator; Count: Integer;
  const AValue: Int64; AComparator: TInt64Compare): Integer; overload;
function CountObject(const First: IJclInt64Iterator; Count: Integer;
  const AValue: Int64; AEqualityComparator: TInt64EqualityCompare): Integer; overload;
{$IFNDEF CLR}
function CountObject(const First: IJclPtrIterator; Count: Integer;
  APtr: Pointer; AComparator: TPtrCompare): Integer; overload;
function CountObject(const First: IJclPtrIterator; Count: Integer;
  APtr: Pointer; AEqualityComparator: TPtrEqualityCompare): Integer; overload;
{$ENDIF ~CLR}
function CountObject(const First: IJclIterator; Count: Integer;
  AObject: TObject; AComparator: TCompare): Integer; overload;
function CountObject(const First: IJclIterator; Count: Integer;
  AObject: TObject; AEqualityComparator: TEqualityCompare): Integer; overload;

// Copy algorithms
procedure Copy(const First: IJclIntfIterator; Count: Integer;
  const Output: IJclIntfIterator); overload;
procedure Copy(const First: IJclAnsiStrIterator; Count: Integer;
  const Output: IJclAnsiStrIterator); overload;
procedure Copy(const First: IJclWideStrIterator; Count: Integer;
  const Output: IJclWideStrIterator); overload;
procedure Copy(const First: IJclSingleIterator; Count: Integer;
  const Output: IJclSingleIterator); overload;
procedure Copy(const First: IJclDoubleIterator; Count: Integer;
  const Output: IJclDoubleIterator); overload;
procedure Copy(const First: IJclExtendedIterator; Count: Integer;
  const Output: IJclExtendedIterator); overload;
procedure Copy(const First: IJclIntegerIterator; Count: Integer;
  const Output: IJclIntegerIterator); overload;
procedure Copy(const First: IJclCardinalIterator; Count: Integer;
  const Output: IJclCardinalIterator); overload;
procedure Copy(const First: IJclInt64Iterator; Count: Integer;
  const Output: IJclInt64Iterator); overload;
{$IFNDEF CLR}
procedure Copy(const First: IJclPtrIterator; Count: Integer;
  const Output: IJclPtrIterator); overload;
{$ENDIF ~CLR}
procedure Copy(const First: IJclIterator; Count: Integer;
  const Output: IJclIterator); overload;

// Generate algorithms
procedure Generate(const List: IJclIntfList; Count: Integer; const AInterface: IInterface); overload;
procedure Generate(const List: IJclAnsiStrList; Count: Integer; const AString: AnsiString); overload;
procedure Generate(const List: IJclWideStrList; Count: Integer; const AString: WideString); overload;
procedure Generate(const List: IJclSingleList; Count: Integer; const AValue: Single); overload;
procedure Generate(const List: IJclDoubleList; Count: Integer; const AValue: Double); overload;
procedure Generate(const List: IJclExtendedList; Count: Integer; const AValue: Extended); overload;
procedure Generate(const List: IJclIntegerList; Count: Integer; AValue: Integer); overload;
procedure Generate(const List: IJclCardinalList; Count: Integer; AValue: Cardinal); overload;
procedure Generate(const List: IJclInt64List; Count: Integer; const AValue: Int64); overload;
{$IFNDEF CLR}
procedure Generate(const List: IJclPtrList; Count: Integer; APtr: Pointer); overload;
{$ENDIF CLR}
procedure Generate(const List: IJclList; Count: Integer; AObject: TObject); overload;

// Fill algorithms
procedure Fill(const First: IJclIntfIterator; Count: Integer; const AInterface: IInterface); overload;
procedure Fill(const First: IJclAnsiStrIterator; Count: Integer; const AString: AnsiString); overload;
procedure Fill(const First: IJclWideStrIterator; Count: Integer; const AString: WideString); overload;
procedure Fill(const First: IJclSingleIterator; Count: Integer; const AValue: Single); overload;
procedure Fill(const First: IJclDoubleIterator; Count: Integer; const AValue: Double); overload;
procedure Fill(const First: IJclExtendedIterator; Count: Integer; const AValue: Extended); overload;
procedure Fill(const First: IJclIntegerIterator; Count: Integer; AValue: Integer); overload;
procedure Fill(const First: IJclCardinalIterator; Count: Integer; AValue: Cardinal); overload;
procedure Fill(const First: IJclInt64Iterator; Count: Integer; const AValue: Int64); overload;
{$IFNDEF CLR}
procedure Fill(const First: IJclPtrIterator; Count: Integer; APtr: Pointer); overload;
{$ENDIF ~CLR}
procedure Fill(const First: IJclIterator; Count: Integer; AObject: TObject); overload;

// Reverse algorithms
procedure Reverse(const First, Last: IJclIntfIterator); overload;
procedure Reverse(const First, Last: IJclAnsiStrIterator); overload;
procedure Reverse(const First, Last: IJclWideStrIterator); overload;
procedure Reverse(const First, Last: IJclSingleIterator); overload;
procedure Reverse(const First, Last: IJclDoubleIterator); overload;
procedure Reverse(const First, Last: IJclExtendedIterator); overload;
procedure Reverse(const First, Last: IJclIntegerIterator); overload;
procedure Reverse(const First, Last: IJclCardinalIterator); overload;
procedure Reverse(const First, Last: IJclInt64Iterator); overload;
{$IFNDEF CLR}
procedure Reverse(const First, Last: IJclPtrIterator); overload;
{$ENDIF CLR}
procedure Reverse(const First, Last: IJclIterator); overload;

procedure QuickSort(const AList: IJclIntfList; L, R: Integer; AComparator: TIntfCompare); overload;
procedure QuickSort(const AList: IJclAnsiStrList; L, R: Integer; AComparator: TAnsiStrCompare); overload;
procedure QuickSort(const AList: IJclWideStrList; L, R: Integer; AComparator: TWideStrCompare); overload;
procedure QuickSort(const AList: IJclSingleList; L, R: Integer; AComparator: TSingleCompare); overload;
procedure QuickSort(const AList: IJclDoubleList; L, R: Integer; AComparator: TDoubleCompare); overload;
procedure QuickSort(const AList: IJclExtendedList; L, R: Integer; AComparator: TExtendedCompare); overload;
procedure QuickSort(const AList: IJclIntegerList; L, R: Integer; AComparator: TIntegerCompare); overload;
procedure QuickSort(const AList: IJclCardinalList; L, R: Integer; AComparator: TCardinalCompare); overload;
procedure QuickSort(const AList: IJclInt64List; L, R: Integer; AComparator: TInt64Compare); overload;
{$IFNDEF CLR}
procedure QuickSort(const AList: IJclPtrList; L, R: Integer; AComparator: TPtrCompare); overload;
{$ENDIF ~CLR}
procedure QuickSort(const AList: IJclList; L, R: Integer; AComparator: TCompare); overload;

var
  IntfSortProc: TIntfSortProc = QuickSort;
  AnsiStrSortProc: TAnsiStrSortProc = QuickSort;
  WideStrSortProc: TWideStrSortProc = QuickSort;
  SingleSortProc: TSingleSortProc = QuickSort;
  DoubleSortProc: TDoubleSortProc = QuickSort;
  ExtendedSortProc: TExtendedSortProc = QuickSort;
  IntegerSortProc: TIntegerSortProc = QuickSort;
  CardinalSortProc: TCardinalSortProc = QuickSort;
  Int64SortProc: TInt64SortProc = QuickSort;
  {$IFNDEF CLR}
  PtrSortProc: TPtrSortProc = QuickSort;
  {$ENDIF ~CLR}
  SortProc: TSortProc = QuickSort;

// Sort algorithms
procedure Sort(const AList: IJclIntfList; First, Last: Integer; AComparator: TIntfCompare); overload;
procedure Sort(const AList: IJclAnsiStrList; First, Last: Integer; AComparator: TAnsiStrCompare); overload;
procedure Sort(const AList: IJclWideStrList; First, Last: Integer; AComparator: TWideStrCompare); overload;
procedure Sort(const AList: IJclSingleList; First, Last: Integer; AComparator: TSingleCompare); overload;
procedure Sort(const AList: IJclDoubleList; First, Last: Integer; AComparator: TDoubleCompare); overload;
procedure Sort(const AList: IJclExtendedList; First, Last: Integer; AComparator: TExtendedCompare); overload;
procedure Sort(const AList: IJclIntegerList; First, Last: Integer; AComparator: TIntegerCompare); overload;
procedure Sort(const AList: IJclCardinalList; First, Last: Integer; AComparator: TCardinalCompare); overload;
procedure Sort(const AList: IJclInt64List; First, Last: Integer; AComparator: TInt64Compare); overload;
{$IFNDEF CLR}
procedure Sort(const AList: IJclPtrList; First, Last: Integer; AComparator: TPtrCompare); overload;
{$ENDIF ~CLR}
procedure Sort(const AList: IJclList; First, Last: Integer; AComparator: TCompare); overload;

{$IFDEF SUPPORTS_GENERICS}
type
  // cannot implement generic global functions
  TJclAlgorithms<T> = class
  private
    //FSortProc: TSortProc;
  public
    class procedure Apply(const First: IJclIterator<T>; Count: Integer; F: TApplyFunction<T>);
    class function Find(const First: IJclIterator<T>; Count: Integer; const AItem: T;
  AComparator: TCompare<T>): IJclIterator<T>; overload;
    class function Find(const First: IJclIterator<T>; Count: Integer; const AItem: T;
  AEqualityComparator: TEqualityCompare<T>): IJclIterator<T>; overload;
    class function CountObject(const First: IJclIterator<T>; Count: Integer;
  const AItem: T; AComparator: TCompare<T>): Integer; overload;
    class function CountObject(const First: IJclIterator<T>; Count: Integer;
  const AItem: T; AEqualityComparator: TEqualityCompare<T>): Integer; overload;
    class procedure Copy(const First: IJclIterator<T>; Count: Integer;
  const Output: IJclIterator<T>);
    class procedure Generate(const List: IJclList<T>; Count: Integer; const AItem: T);
    class procedure Fill(const First: IJclIterator<T>; Count: Integer; const AItem: T);
    class procedure Reverse(const First, Last: IJclIterator<T>);
    class procedure QuickSort(const AList: IJclList<T>; L, R: Integer; AComparator: TCompare<T>);
    class procedure Sort(const AList: IJclList<T>; First, Last: Integer; AComparator: TCompare<T>);
    //class property SortProc: TSortProc<T> read FSortProc write FSortProc;
  end;
{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclAlgorithms.pas $';
    Revision: '$Revision: 2309 $';
    Date: '$Date: 2008-01-15 23:30:26 +0100 (mar., 15 janv. 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFNDEF RTL140_UP}
  JclWideStrings,
  {$ENDIF ~RTL140_UP}
  SysUtils;

function IntfSimpleCompare(const Obj1, Obj2: IInterface): Integer;
begin
  if Integer(Obj1) < Integer(Obj2) then
    Result := -1
  else
  if Integer(Obj1) > Integer(Obj2) then
    Result := 1
  else
    Result := 0;
end;

function AnsiStrSimpleCompare(const Obj1, Obj2: AnsiString): Integer;
begin
  // (rom) changed to case sensitive compare
  Result := CompareStr(Obj1, Obj2);
end;

function WideStrSimpleCompare(const Obj1, Obj2: WideString): Integer;
begin
  // (rom) changed to case sensitive compare
  Result := WideCompareStr(Obj1, Obj2);
end;

function StrSimpleCompare(const Obj1, Obj2: string): Integer;
begin
  case SizeOf(Obj1[1]) of
    SizeOf(AnsiChar):
      Result := CompareStr(Obj1, Obj2);
    SizeOf(WideChar):
      Result := WideCompareStr(Obj1, Obj2);
  else
    raise EJclOperationNotSupportedError.Create;
  end;
end;

function SingleSimpleCompare(const Obj1, Obj2: Single): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function DoubleSimpleCompare(const Obj1, Obj2: Double): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function ExtendedSimpleCompare(const Obj1, Obj2: Extended): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function FloatSimpleCompare(const Obj1, Obj2: Float): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function IntegerSimpleCompare(Obj1, Obj2: Integer): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function CardinalSimpleCompare(Obj1, Obj2: Cardinal): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function Int64SimpleCompare(const Obj1, Obj2: Int64): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

{$IFNDEF CLR}
function PtrSimpleCompare(Obj1, Obj2: Pointer): Integer;
begin
  if Integer(Obj1) < Integer(Obj2) then
    Result := -1
  else
  if Integer(Obj1) > Integer(Obj2) then
    Result := 1
  else
    Result := 0;
end;
{$ENDIF ~CLR}

function SimpleCompare(Obj1, Obj2: TObject): Integer;
begin
  if Integer(Obj1) < Integer(Obj2) then
    Result := -1
  else
  if Integer(Obj1) > Integer(Obj2) then
    Result := 1
  else
    Result := 0;
end;

function IntegerCompare(Obj1, Obj2: TObject): Integer;
begin
  if Integer(Obj1) < Integer(Obj2) then
    Result := -1
  else
  if Integer(Obj1) > Integer(Obj2) then
    Result := 1
  else
    Result := 0;
end;

function IntfSimpleEqualityCompare(const Obj1, Obj2: IInterface): Boolean;
begin
  Result := Integer(Obj1) = Integer(Obj2);
end;

function AnsiStrSimpleEqualityCompare(const Obj1, Obj2: AnsiString): Boolean;
begin
  // (rom) changed to case sensitive compare
  Result := CompareStr(Obj1, Obj2) = 0;
end;

function WideStrSimpleEqualityCompare(const Obj1, Obj2: WideString): Boolean;
begin
  // (rom) changed to case sensitive compare
  Result := WideCompareStr(Obj1, Obj2) = 0;
end;

function StrSimpleEqualityCompare(const Obj1, Obj2: string): Boolean;
begin
  case SizeOf(Obj1[1]) of
    SizeOf(AnsiChar):
      Result := CompareStr(Obj1, Obj2) = 0;
    SizeOf(WideChar):
      Result := WideCompareStr(Obj1, Obj2) = 0;
  else
    raise EJclOperationNotSupportedError.Create;
  end;
end;

function SingleSimpleEqualityCompare(const Obj1, Obj2: Single): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function DoubleSimpleEqualityCompare(const Obj1, Obj2: Double): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function ExtendedSimpleEqualityCompare(const Obj1, Obj2: Extended): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function FloatSimpleEqualityCompare(const Obj1, Obj2: Float): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function IntegerSimpleEqualityCompare(Obj1, Obj2: Integer): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function CardinalSimpleEqualityCompare(Obj1, Obj2: Cardinal): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function Int64SimpleEqualityCompare(const Obj1, Obj2: Int64): Boolean;
begin
  Result := Obj1 = Obj2;
end;

{$IFNDEF CLR}
function PtrSimpleEqualityCompare(Obj1, Obj2: Pointer): Boolean;
begin
  Result := Integer(Obj1) = Integer(Obj2);
end;
{$ENDIF ~CLR}

function SimpleEqualityCompare(Obj1, Obj2: TObject): Boolean;
begin
  Result := Integer(Obj1) = Integer(Obj2);
end;

procedure Apply(const First: IJclIntfIterator; Count: Integer; F: TIntfApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetObject(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclAnsiStrIterator; Count: Integer; F: TAnsiStrApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetString(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclWideStrIterator; Count: Integer; F: TWideStrApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetString(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclSingleIterator; Count: Integer; F: TSingleApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetValue(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclDoubleIterator; Count: Integer; F: TDoubleApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetValue(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclExtendedIterator; Count: Integer; F: TExtendedApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetValue(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclIntegerIterator; Count: Integer; F: TIntegerApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetValue(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclCardinalIterator; Count: Integer; F: TCardinalApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetValue(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclInt64Iterator; Count: Integer; F: TInt64ApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetValue(F(First.Next))
    else
      Break;
end;

{$IFNDEF CLR}
procedure Apply(const First: IJclPtrIterator; Count: Integer; F: TPtrApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetPointer(F(First.Next))
    else
      Break;
end;
{$ENDIF ~CLR}

procedure Apply(const First: IJclIterator; Count: Integer; F: TApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetObject(F(First.Next))
    else
      Break;
end;

function Find(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface; AComparator: TIntfCompare): IJclIntfIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AInterface) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface; AEqualityComparator: TIntfEqualityCompare): IJclIntfIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AInterface) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: AnsiString; AComparator: TAnsiStrCompare): IJclAnsiStrIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AString) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: AnsiString; AEqualityComparator: TAnsiStrEqualityCompare): IJclAnsiStrIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AString) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclWideStrIterator; Count: Integer;
  const AString: WideString; AComparator: TWideStrCompare): IJclWideStrIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AString) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclWideStrIterator; Count: Integer;
  const AString: WideString; AEqualityComparator: TWideStrEqualityCompare): IJclWideStrIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AString) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclSingleIterator; Count: Integer;
  const AValue: Single; AComparator: TSingleCompare): IJclSingleIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AValue) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclSingleIterator; Count: Integer;
  const AValue: Single; AEqualityComparator: TSingleEqualityCompare): IJclSingleIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AValue) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclDoubleIterator; Count: Integer;
  const AValue: Double; AComparator: TDoubleCompare): IJclDoubleIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AValue) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclDoubleIterator; Count: Integer;
  const AValue: Double; AEqualityComparator: TDoubleEqualityCompare): IJclDoubleIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AValue) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclExtendedIterator; Count: Integer;
  const AValue: Extended; AComparator: TExtendedCompare): IJclExtendedIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AValue) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclExtendedIterator; Count: Integer;
  const AValue: Extended; AEqualityComparator: TExtendedEqualityCompare): IJclExtendedIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AValue) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclIntegerIterator; Count: Integer;
  AValue: Integer; AComparator: TIntegerCompare): IJclIntegerIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AValue) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclIntegerIterator; Count: Integer;
  AValue: Integer; AEqualityComparator: TIntegerEqualityCompare): IJclIntegerIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AValue) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclCardinalIterator; Count: Integer;
  AValue: Cardinal; AComparator: TCardinalCompare): IJclCardinalIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AValue) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclCardinalIterator; Count: Integer;
  AValue: Cardinal; AEqualityComparator: TCardinalEqualityCompare): IJclCardinalIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AValue) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclInt64Iterator; Count: Integer;
  const AValue: Int64; AComparator: TInt64Compare): IJclInt64Iterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AValue) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclInt64Iterator; Count: Integer;
  const AValue: Int64; AEqualityComparator: TInt64EqualityCompare): IJclInt64Iterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AValue) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

{$IFNDEF CLR}
function Find(const First: IJclPtrIterator; Count: Integer;
  APtr: Pointer; AComparator: TPtrCompare): IJclPtrIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, APtr) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclPtrIterator; Count: Integer;
  APtr: Pointer; AEqualityComparator: TPtrEqualityCompare): IJclPtrIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, APtr) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;
{$ENDIF ~CLR}

function Find(const First: IJclIterator; Count: Integer;
  AObject: TObject; AComparator: TCompare): IJclIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AObject) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclIterator; Count: Integer;
  AObject: TObject; AEqualityComparator: TEqualityCompare): IJclIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AObject) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function CountObject(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface; AComparator: TIntfCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AInterface) = 0))
    else
      Break;
end;

function CountObject(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface; AEqualityComparator: TIntfEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AInterface)))
    else
      Break;
end;

function CountObject(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: AnsiString; AComparator: TAnsiStrCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AString) = 0))
    else
      Break;
end;

function CountObject(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: AnsiString; AEqualityComparator: TAnsiStrEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AString)))
    else
      Break;
end;

function CountObject(const First: IJclWideStrIterator; Count: Integer;
  const AString: WideString; AComparator: TWideStrCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AString) = 0))
    else
      Break;
end;

function CountObject(const First: IJclWideStrIterator; Count: Integer;
  const AString: WideString; AEqualityComparator: TWideStrEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AString)))
    else
      Break;
end;

function CountObject(const First: IJclSingleIterator; Count: Integer;
  const AValue: Single; AComparator: TSingleCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AValue) = 0))
    else
      Break;
end;

function CountObject(const First: IJclSingleIterator; Count: Integer;
  const AValue: Single; AEqualityComparator: TSingleEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AValue)))
    else
      Break;
end;

function CountObject(const First: IJclDoubleIterator; Count: Integer;
  const AValue: Double; AComparator: TDoubleCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AValue) = 0))
    else
      Break;
end;

function CountObject(const First: IJclDoubleIterator; Count: Integer;
  const AValue: Double; AEqualityComparator: TDoubleEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AValue)))
    else
      Break;
end;

function CountObject(const First: IJclExtendedIterator; Count: Integer;
  const AValue: Extended; AComparator: TExtendedCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AValue) = 0))
    else
      Break;
end;

function CountObject(const First: IJclExtendedIterator; Count: Integer;
  const AValue: Extended; AEqualityComparator: TExtendedEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AValue)))
    else
      Break;
end;

function CountObject(const First: IJclIntegerIterator; Count: Integer;
  AValue: Integer; AComparator: TIntegerCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AValue) = 0))
    else
      Break;
end;

function CountObject(const First: IJclIntegerIterator; Count: Integer;
  AValue: Integer; AEqualityComparator: TIntegerEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AValue)))
    else
      Break;
end;

function CountObject(const First: IJclCardinalIterator; Count: Integer;
  AValue: Cardinal; AComparator: TCardinalCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AValue) = 0))
    else
      Break;
end;

function CountObject(const First: IJclCardinalIterator; Count: Integer;
  AValue: Cardinal; AEqualityComparator: TCardinalEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AValue)))
    else
      Break;
end;

function CountObject(const First: IJclInt64Iterator; Count: Integer;
  const AValue: Int64; AComparator: TInt64Compare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AValue) = 0))
    else
      Break;
end;

function CountObject(const First: IJclInt64Iterator; Count: Integer;
  const AValue: Int64; AEqualityComparator: TInt64EqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AValue)))
    else
      Break;
end;

{$IFNDEF CLR}
function CountObject(const First: IJclPtrIterator; Count: Integer;
  APtr: Pointer; AComparator: TPtrCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, APtr) = 0))
    else
      Break;
end;

function CountObject(const First: IJclPtrIterator; Count: Integer;
  APtr: Pointer; AEqualityComparator: TPtrEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, APtr)))
    else
      Break;
end;
{$ENDIF ~CLR}

function CountObject(const First: IJclIterator; Count: Integer;
  AObject: TObject; AComparator: TCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AObject) = 0))
    else
      Break;
end;

function CountObject(const First: IJclIterator; Count: Integer;
  AObject: TObject; AEqualityComparator: TEqualityCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AObject)))
    else
      Break;
end;

procedure Copy(const First: IJclIntfIterator; Count: Integer;
  const Output: IJclIntfIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetObject(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclAnsiStrIterator; Count: Integer;
  const Output: IJclAnsiStrIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetString(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclWideStrIterator; Count: Integer;
  const Output: IJclWideStrIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetString(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclSingleIterator; Count: Integer;
  const Output: IJclSingleIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetValue(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclDoubleIterator; Count: Integer;
  const Output: IJclDoubleIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetValue(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclExtendedIterator; Count: Integer;
  const Output: IJclExtendedIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetValue(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclIntegerIterator; Count: Integer;
  const Output: IJclIntegerIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetValue(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclCardinalIterator; Count: Integer;
  const Output: IJclCardinalIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetValue(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclInt64Iterator; Count: Integer;
  const Output: IJclInt64Iterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetValue(First.Next);
    end
    else
      Break;
end;

{$IFNDEF CLR}
procedure Copy(const First: IJclPtrIterator; Count: Integer;
  const Output: IJclPtrIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetPointer(First.Next);
    end
    else
      Break;
end;
{$ENDIF ~CLR}

procedure Copy(const First: IJclIterator; Count: Integer;
  const Output: IJclIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetObject(First.Next);
    end
    else
      Break;
end;

procedure Generate(const List: IJclIntfList; Count: Integer;
  const AInterface: IInterface);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AInterface);
end;

procedure Generate(const List: IJclAnsiStrList; Count: Integer;
  const AString: AnsiString);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AString);
end;

procedure Generate(const List: IJclWideStrList; Count: Integer;
  const AString: WideString);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AString);
end;

procedure Generate(const List: IJclSingleList; Count: Integer;
  const AValue: Single);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AValue);
end;

procedure Generate(const List: IJclDoubleList; Count: Integer;
  const AValue: Double);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AValue);
end;

procedure Generate(const List: IJclExtendedList; Count: Integer;
  const AValue: Extended);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AValue);
end;

procedure Generate(const List: IJclIntegerList; Count: Integer;
  AValue: Integer);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AValue);
end;

procedure Generate(const List: IJclCardinalList; Count: Integer;
  AValue: Cardinal);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AValue);
end;

procedure Generate(const List: IJclInt64List; Count: Integer;
  const AValue: Int64);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AValue);
end;

{$IFNDEF CLR}
procedure Generate(const List: IJclPtrList; Count: Integer;
  APtr: Pointer);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(APtr);
end;
{$ENDIF ~CLR}

procedure Generate(const List: IJclList; Count: Integer;
  AObject: TObject);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AObject);
end;

procedure Fill(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetObject(AInterface);
    end
    else
      Break;
end;

procedure Fill(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: AnsiString);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetString(AString);
    end
    else
      Break;
end;

procedure Fill(const First: IJclWideStrIterator; Count: Integer;
  const AString: WideString);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetString(AString);
    end
    else
      Break;
end;

procedure Fill(const First: IJclSingleIterator; Count: Integer;
  const AValue: Single);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetValue(AValue);
    end
    else
      Break;
end;

procedure Fill(const First: IJclDoubleIterator; Count: Integer;
  const AValue: Double);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetValue(AValue);
    end
    else
      Break;
end;

procedure Fill(const First: IJclExtendedIterator; Count: Integer;
  const AValue: Extended);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetValue(AValue);
    end
    else
      Break;
end;

procedure Fill(const First: IJclIntegerIterator; Count: Integer;
  AValue: Integer);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetValue(AValue);
    end
    else
      Break;
end;

procedure Fill(const First: IJclCardinalIterator; Count: Integer;
  AValue: Cardinal);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetValue(AValue);
    end
    else
      Break;
end;

procedure Fill(const First: IJclInt64Iterator; Count: Integer;
  const AValue: Int64);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetValue(AValue);
    end
    else
      Break;
end;

{$IFNDEF CLR}
procedure Fill(const First: IJclPtrIterator; Count: Integer;
  APtr: Pointer);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetPointer(APtr);
    end
    else
      Break;
end;
{$ENDIF ~CLR}

procedure Fill(const First: IJclIterator; Count: Integer;
  AObject: TObject);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetObject(AObject);
    end
    else
      Break;
end;

procedure Reverse(const First, Last: IJclIntfIterator);
var
  Obj: IInterface;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetObject(Last.GetObject);
    Last.SetObject(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclAnsiStrIterator);
var
  Obj: AnsiString;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetString(Last.GetString);
    Last.SetString(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclWideStrIterator);
var
  Obj: WideString;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetString(Last.GetString);
    Last.SetString(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclSingleIterator);
var
  Obj: Single;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetValue(Last.GetValue);
    Last.SetValue(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclDoubleIterator);
var
  Obj: Double;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetValue(Last.GetValue);
    Last.SetValue(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclExtendedIterator);
var
  Obj: Extended;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetValue(Last.GetValue);
    Last.SetValue(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclIntegerIterator);
var
  Obj: Integer;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetValue(Last.GetValue);
    Last.SetValue(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclCardinalIterator);
var
  Obj: Cardinal;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetValue(Last.GetValue);
    Last.SetValue(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclInt64Iterator);
var
  Obj: Int64;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetValue(Last.GetValue);
    Last.SetValue(Obj);
  end;
end;

{$IFNDEF CLR}
procedure Reverse(const First, Last: IJclPtrIterator);
var
  Obj: Pointer;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetPointer(Last.GetPointer);
    Last.SetPointer(Obj);
  end;
end;
{$ENDIF ~CLR}

procedure Reverse(const First, Last: IJclIterator);
var
  Obj: TObject;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetObject(Last.GetObject);
    Last.SetObject(Obj);
  end;
end;

procedure QuickSort(const AList: IJclIntfList; L, R: Integer;
  AComparator: TIntfCompare);
var
  I, J, P: Integer;
  Obj: IInterface;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetObject(P);
      while AComparator(AList.GetObject(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetObject(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        Obj := AList.GetObject(I);
        AList.SetObject(I, AList.GetObject(J));
        AList.SetObject(J, Obj);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclAnsiStrList; L, R: Integer;
  AComparator: TAnsiStrCompare);
var
  I, J, P: Integer;
  Obj: AnsiString;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetString(P);
      while AComparator(AList.GetString(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetString(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        Obj := AList.GetString(I);
        AList.SetString(I, AList.GetString(J));
        AList.SetString(J, Obj);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclWideStrList; L, R: Integer;
  AComparator: TWideStrCompare);
var
  I, J, P: Integer;
  Obj: WideString;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetString(P);
      while AComparator(AList.GetString(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetString(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        Obj := AList.GetString(I);
        AList.SetString(I, AList.GetString(J));
        AList.SetString(J, Obj);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclSingleList; L, R: Integer;
  AComparator: TSingleCompare);
var
  I, J, P: Integer;
  Obj: Single;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetValue(P);
      while AComparator(AList.GetValue(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetValue(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        Obj := AList.GetValue(I);
        AList.SetValue(I, AList.GetValue(J));
        AList.SetValue(J, Obj);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclDoubleList; L, R: Integer;
  AComparator: TDoubleCompare);
var
  I, J, P: Integer;
  Obj: Double;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetValue(P);
      while AComparator(AList.GetValue(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetValue(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        Obj := AList.GetValue(I);
        AList.SetValue(I, AList.GetValue(J));
        AList.SetValue(J, Obj);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclExtendedList; L, R: Integer;
  AComparator: TExtendedCompare);
var
  I, J, P: Integer;
  Obj: Extended;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetValue(P);
      while AComparator(AList.GetValue(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetValue(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        Obj := AList.GetValue(I);
        AList.SetValue(I, AList.GetValue(J));
        AList.SetValue(J, Obj);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclIntegerList; L, R: Integer;
  AComparator: TIntegerCompare);
var
  I, J, P: Integer;
  Obj: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetValue(P);
      while AComparator(AList.GetValue(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetValue(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        Obj := AList.GetValue(I);
        AList.SetValue(I, AList.GetValue(J));
        AList.SetValue(J, Obj);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclCardinalList; L, R: Integer;
  AComparator: TCardinalCompare);
var
  I, J, P: Integer;
  Obj: Cardinal;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetValue(P);
      while AComparator(AList.GetValue(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetValue(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        Obj := AList.GetValue(I);
        AList.SetValue(I, AList.GetValue(J));
        AList.SetValue(J, Obj);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclInt64List; L, R: Integer;
  AComparator: TInt64Compare);
var
  I, J, P: Integer;
  Obj: Int64;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetValue(P);
      while AComparator(AList.GetValue(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetValue(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        Obj := AList.GetValue(I);
        AList.SetValue(I, AList.GetValue(J));
        AList.SetValue(J, Obj);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

{$IFNDEF CLR}
procedure QuickSort(const AList: IJclPtrList; L, R: Integer;
  AComparator: TPtrCompare);
var
  I, J, P: Integer;
  Obj: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetPointer(P);
      while AComparator(AList.GetPointer(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetPointer(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        Obj := AList.GetPointer(I);
        AList.SetPointer(I, AList.GetPointer(J));
        AList.SetPointer(J, Obj);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;
{$ENDIF ~CLR}

procedure QuickSort(const AList: IJclList; L, R: Integer;
  AComparator: TCompare);
var
  I, J, P: Integer;
  Obj: TObject;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetObject(P);
      while AComparator(AList.GetObject(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetObject(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        Obj := AList.GetObject(I);
        AList.SetObject(I, AList.GetObject(J));
        AList.SetObject(J, Obj);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure Sort(const AList: IJclIntfList; First, Last: Integer; AComparator: TIntfCompare);
begin
  IntfSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclAnsiStrList; First, Last: Integer; AComparator: TAnsiStrCompare);
begin
  AnsiStrSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclWideStrList; First, Last: Integer; AComparator: TWideStrCompare);
begin
  WideStrSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclSingleList; First, Last: Integer; AComparator: TSingleCompare);
begin
  SingleSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclDoubleList; First, Last: Integer; AComparator: TDoubleCompare);
begin
  DoubleSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclExtendedList; First, Last: Integer; AComparator: TExtendedCompare);
begin
  ExtendedSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclIntegerList; First, Last: Integer; AComparator: TIntegerCompare);
begin
  IntegerSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclCardinalList; First, Last: Integer; AComparator: TCardinalCompare);
begin
  CardinalSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclInt64List; First, Last: Integer; AComparator: TInt64Compare);
begin
  Int64SortProc(AList, First, Last, AComparator);
end;

{$IFNDEF CLR}
procedure Sort(const AList: IJclPtrList; First, Last: Integer; AComparator: TPtrCompare);
begin
  PtrSortProc(AList, First, Last, AComparator);
end;
{$ENDIF ~CLR}

procedure Sort(const AList: IJclList; First, Last: Integer; AComparator: TCompare);
begin
  SortProc(AList, First, Last, AComparator);
end;

{$IFDEF SUPPORTS_GENERICS}
class procedure TJclAlgorithms<T>.Apply(const First: IJclIterator<T>; Count: Integer; F: TApplyFunction<T>);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetItem(F(First.Next))
    else
      Break;
end;

class function TJclAlgorithms<T>.Find(const First: IJclIterator<T>; Count: Integer;
  const AItem: T; AComparator: TCompare<T>): IJclIterator<T>;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AItem) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

class function TJclAlgorithms<T>.Find(const First: IJclIterator<T>; Count: Integer;
  const AItem: T; AEqualityComparator: TEqualityCompare<T>): IJclIterator<T>;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AEqualityComparator(First.Next, AItem) then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

class function TJclAlgorithms<T>.CountObject(const First: IJclIterator<T>; Count: Integer;
  const AItem: T; AComparator: TCompare<T>): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AItem) = 0))
    else
      Break;
end;

class function TJclAlgorithms<T>.CountObject(const First: IJclIterator<T>; Count: Integer;
  const AItem: T; AEqualityComparator: TEqualityCompare<T>): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AEqualityComparator(First.Next, AItem)))
    else
      Break;
end;

class procedure TJclAlgorithms<T>.Copy(const First: IJclIterator<T>; Count: Integer;
  const Output: IJclIterator<T>);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetItem(First.Next);
    end
    else
      Break;
end;

class procedure TJclAlgorithms<T>.Generate(const List: IJclList<T>; Count: Integer;
  const AItem: T);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AItem);
end;

class procedure TJclAlgorithms<T>.Fill(const First: IJclIterator<T>; Count: Integer;
  const AItem: T);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetItem(AItem);
    end
    else
      Break;
end;

class procedure TJclAlgorithms<T>.Reverse(const First, Last: IJclIterator<T>);
var
  Obj: T;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetItem(Last.GetItem);
    Last.SetItem(Obj);
  end;
end;

class procedure TJclAlgorithms<T>.QuickSort(const AList: IJclList<T>; L, R: Integer;
  AComparator: TCompare<T>);
var
  I, J, P: Integer;
  Obj: T;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      Obj := AList.GetItem(P);
      while AComparator(AList.GetItem(I), Obj) < 0 do
        Inc(I);
      while AComparator(AList.GetItem(J), Obj) > 0 do
        Dec(J);
      if I <= J then
      begin
        Obj := AList.GetItem(I);
        AList.SetItem(I, AList.GetItem(J));
        AList.SetItem(J, Obj);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      TJclAlgorithms<T>.QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

class procedure TJclAlgorithms<T>.Sort(const AList: IJclList<T>; First, Last: Integer;
  AComparator: TCompare<T>);
begin
  TJclAlgorithms<T>.QuickSort(AList, First, Last, AComparator);
end;
{$ENDIF SUPPORTS_GENERICS}


{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.