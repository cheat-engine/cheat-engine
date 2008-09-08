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
{$I containers\JclAlgorithms.int}
{$I containers\JclAlgorithms.imp}
interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase, JclContainerIntf;

// Compare functions
{$JPPEXPANDMACRO SIMPLECOMPAREINT(IntfSimpleCompare,const ,IInterface)}
{$JPPEXPANDMACRO SIMPLECOMPAREINT(AnsiStrSimpleCompare,const ,AnsiString)}
{$JPPEXPANDMACRO SIMPLECOMPAREINT(WideStrSimpleCompare,const ,WideString)}
{$JPPEXPANDMACRO SIMPLECOMPAREINT(StrSimpleCompare,const ,string)}
{$JPPEXPANDMACRO SIMPLECOMPAREINT(SingleSimpleCompare,const ,Single)}
{$JPPEXPANDMACRO SIMPLECOMPAREINT(DoubleSimpleCompare,const ,Double)}
{$JPPEXPANDMACRO SIMPLECOMPAREINT(ExtendedSimpleCompare,const ,Extended)}
{$JPPEXPANDMACRO SIMPLECOMPAREINT(FloatSimpleCompare,const ,Float)}
{$JPPEXPANDMACRO SIMPLECOMPAREINT(IntegerSimpleCompare,,Integer)}
{$JPPEXPANDMACRO SIMPLECOMPAREINT(CardinalSimpleCompare,,Cardinal)}
{$JPPEXPANDMACRO SIMPLECOMPAREINT(Int64SimpleCompare,const ,Int64)}
{$IFNDEF CLR}
{$JPPEXPANDMACRO SIMPLECOMPAREINT(PtrSimpleCompare,,Pointer)}
{$ENDIF ~CLR}
{$JPPEXPANDMACRO SIMPLECOMPAREINT(SimpleCompare,,TObject)}

{$JPPEXPANDMACRO SIMPLECOMPAREINT(IntegerCompare,,TObject)}

// Compare functions for equality
{$JPPEXPANDMACRO SIMPLEEQUALITYCOMPAREINT(IntfSimpleEqualityCompare,const ,IInterface)}
{$JPPEXPANDMACRO SIMPLEEQUALITYCOMPAREINT(AnsiStrSimpleEqualityCompare,const ,AnsiString)}
{$JPPEXPANDMACRO SIMPLEEQUALITYCOMPAREINT(WideStrSimpleEqualityCompare,const ,WideString)}
{$JPPEXPANDMACRO SIMPLEEQUALITYCOMPAREINT(StrSimpleEqualityCompare,const ,string)}
{$JPPEXPANDMACRO SIMPLEEQUALITYCOMPAREINT(SingleSimpleEqualityCompare,const ,Single)}
{$JPPEXPANDMACRO SIMPLEEQUALITYCOMPAREINT(DoubleSimpleEqualityCompare,const ,Double)}
{$JPPEXPANDMACRO SIMPLEEQUALITYCOMPAREINT(ExtendedSimpleEqualityCompare,const ,Extended)}
{$JPPEXPANDMACRO SIMPLEEQUALITYCOMPAREINT(FloatSimpleEqualityCompare,const ,Float)}
{$JPPEXPANDMACRO SIMPLEEQUALITYCOMPAREINT(IntegerSimpleEqualityCompare,,Integer)}
{$JPPEXPANDMACRO SIMPLEEQUALITYCOMPAREINT(CardinalSimpleEqualityCompare,,Cardinal)}
{$JPPEXPANDMACRO SIMPLEEQUALITYCOMPAREINT(Int64SimpleEqualityCompare,const ,Int64)}
{$IFNDEF CLR}
{$JPPEXPANDMACRO SIMPLEEQUALITYCOMPAREINT(PtrSimpleEqualityCompare,,Pointer)}
{$ENDIF ~CLR}
{$JPPEXPANDMACRO SIMPLEEQUALITYCOMPAREINT(SimpleEqualityCompare,,TObject)}

// Apply algorithms
{$JPPEXPANDMACRO APPLYINT(Apply,IJclIntfIterator,TIntfApplyFunction)} overload;
{$JPPEXPANDMACRO APPLYINT(Apply,IJclAnsiStrIterator,TAnsiStrApplyFunction)} overload;
{$JPPEXPANDMACRO APPLYINT(Apply,IJclWideStrIterator,TWideStrApplyFunction)} overload;
{$JPPEXPANDMACRO APPLYINT(Apply,IJclSingleIterator,TSingleApplyFunction)} overload;
{$JPPEXPANDMACRO APPLYINT(Apply,IJclDoubleIterator,TDoubleApplyFunction)} overload;
{$JPPEXPANDMACRO APPLYINT(Apply,IJclExtendedIterator,TExtendedApplyFunction)} overload;
{$JPPEXPANDMACRO APPLYINT(Apply,IJclIntegerIterator,TIntegerApplyFunction)} overload;
{$JPPEXPANDMACRO APPLYINT(Apply,IJclCardinalIterator,TCardinalApplyFunction)} overload;
{$JPPEXPANDMACRO APPLYINT(Apply,IJclInt64Iterator,TInt64ApplyFunction)} overload;
{$IFNDEF CLR}
{$JPPEXPANDMACRO APPLYINT(Apply,IJclPtrIterator,TPtrApplyFunction)} overload;
{$ENDIF ~CLR}
{$JPPEXPANDMACRO APPLYINT(Apply,IJclIterator,TApplyFunction)} overload;

// Find algorithms
{$JPPEXPANDMACRO FINDINT(Find,IJclIntfIterator,const ,AInterface,IInterface,TIntfCompare)} overload;
{$JPPEXPANDMACRO FINDEQINT(Find,IJclIntfIterator,const ,AInterface,IInterface,TIntfEqualityCompare)} overload;
{$JPPEXPANDMACRO FINDINT(Find,IJclAnsiStrIterator,const ,AString,AnsiString,TAnsiStrCompare)} overload;
{$JPPEXPANDMACRO FINDEQINT(Find,IJclAnsiStrIterator,const ,AString,AnsiString,TAnsiStrEqualityCompare)} overload;
{$JPPEXPANDMACRO FINDINT(Find,IJclWideStrIterator,const ,AString,WideString,TWideStrCompare)} overload;
{$JPPEXPANDMACRO FINDEQINT(Find,IJclWideStrIterator,const ,AString,WideString,TWideStrEqualityCompare)} overload;
{$JPPEXPANDMACRO FINDINT(Find,IJclSingleIterator,const ,AValue,Single,TSingleCompare)} overload;
{$JPPEXPANDMACRO FINDEQINT(Find,IJclSingleIterator,const ,AValue,Single,TSingleEqualityCompare)} overload;
{$JPPEXPANDMACRO FINDINT(Find,IJclDoubleIterator,const ,AValue,Double,TDoubleCompare)} overload;
{$JPPEXPANDMACRO FINDEQINT(Find,IJclDoubleIterator,const ,AValue,Double,TDoubleEqualityCompare)} overload;
{$JPPEXPANDMACRO FINDINT(Find,IJclExtendedIterator,const ,AValue,Extended,TExtendedCompare)} overload;
{$JPPEXPANDMACRO FINDEQINT(Find,IJclExtendedIterator,const ,AValue,Extended,TExtendedEqualityCompare)} overload;
{$JPPEXPANDMACRO FINDINT(Find,IJclIntegerIterator,,AValue,Integer,TIntegerCompare)} overload;
{$JPPEXPANDMACRO FINDEQINT(Find,IJclIntegerIterator,,AValue,Integer,TIntegerEqualityCompare)} overload;
{$JPPEXPANDMACRO FINDINT(Find,IJclCardinalIterator,,AValue,Cardinal,TCardinalCompare)} overload;
{$JPPEXPANDMACRO FINDEQINT(Find,IJclCardinalIterator,,AValue,Cardinal,TCardinalEqualityCompare)} overload;
{$JPPEXPANDMACRO FINDINT(Find,IJclInt64Iterator,const ,AValue,Int64,TInt64Compare)} overload;
{$JPPEXPANDMACRO FINDEQINT(Find,IJclInt64Iterator,const ,AValue,Int64,TInt64EqualityCompare)} overload;
{$IFNDEF CLR}
{$JPPEXPANDMACRO FINDINT(Find,IJclPtrIterator,,APtr,Pointer,TPtrCompare)} overload;
{$JPPEXPANDMACRO FINDEQINT(Find,IJclPtrIterator,,APtr,Pointer,TPtrEqualityCompare)} overload;
{$ENDIF ~CLR}
{$JPPEXPANDMACRO FINDINT(Find,IJclIterator,,AObject,TObject,TCompare)} overload;
{$JPPEXPANDMACRO FINDEQINT(Find,IJclIterator,,AObject,TObject,TEqualityCompare)} overload;

// CountObject algorithms
{$JPPEXPANDMACRO COUNTOBJECTINT(CountObject,IJclIntfIterator,const ,AInterface,IInterface,TIntfCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTEQINT(CountObject,IJclIntfIterator,const ,AInterface,IInterface,TIntfEqualityCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTINT(CountObject,IJclAnsiStrIterator,const ,AString,AnsiString,TAnsiStrCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTEQINT(CountObject,IJclAnsiStrIterator,const ,AString,AnsiString,TAnsiStrEqualityCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTINT(CountObject,IJclWideStrIterator,const ,AString,WideString,TWideStrCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTEQINT(CountObject,IJclWideStrIterator,const ,AString,WideString,TWideStrEqualityCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTINT(CountObject,IJclSingleIterator,const ,AValue,Single,TSingleCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTEQINT(CountObject,IJclSingleIterator,const ,AValue,Single,TSingleEqualityCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTINT(CountObject,IJclDoubleIterator,const ,AValue,Double,TDoubleCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTEQINT(CountObject,IJclDoubleIterator,const ,AValue,Double,TDoubleEqualityCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTINT(CountObject,IJclExtendedIterator,const ,AValue,Extended,TExtendedCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTEQINT(CountObject,IJclExtendedIterator,const ,AValue,Extended,TExtendedEqualityCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTINT(CountObject,IJclIntegerIterator,,AValue,Integer,TIntegerCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTEQINT(CountObject,IJclIntegerIterator,,AValue,Integer,TIntegerEqualityCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTINT(CountObject,IJclCardinalIterator,,AValue,Cardinal,TCardinalCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTEQINT(CountObject,IJclCardinalIterator,,AValue,Cardinal,TCardinalEqualityCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTINT(CountObject,IJclInt64Iterator,const ,AValue,Int64,TInt64Compare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTEQINT(CountObject,IJclInt64Iterator,const ,AValue,Int64,TInt64EqualityCompare)} overload;
{$IFNDEF CLR}
{$JPPEXPANDMACRO COUNTOBJECTINT(CountObject,IJclPtrIterator,,APtr,Pointer,TPtrCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTEQINT(CountObject,IJclPtrIterator,,APtr,Pointer,TPtrEqualityCompare)} overload;
{$ENDIF ~CLR}
{$JPPEXPANDMACRO COUNTOBJECTINT(CountObject,IJclIterator,,AObject,TObject,TCompare)} overload;
{$JPPEXPANDMACRO COUNTOBJECTEQINT(CountObject,IJclIterator,,AObject,TObject,TEqualityCompare)} overload;

// Copy algorithms
{$JPPEXPANDMACRO COPYINT(Copy,IJclIntfIterator)} overload;
{$JPPEXPANDMACRO COPYINT(Copy,IJclAnsiStrIterator)} overload;
{$JPPEXPANDMACRO COPYINT(Copy,IJclWideStrIterator)} overload;
{$JPPEXPANDMACRO COPYINT(Copy,IJclSingleIterator)} overload;
{$JPPEXPANDMACRO COPYINT(Copy,IJclDoubleIterator)} overload;
{$JPPEXPANDMACRO COPYINT(Copy,IJclExtendedIterator)} overload;
{$JPPEXPANDMACRO COPYINT(Copy,IJclIntegerIterator)} overload;
{$JPPEXPANDMACRO COPYINT(Copy,IJclCardinalIterator)} overload;
{$JPPEXPANDMACRO COPYINT(Copy,IJclInt64Iterator)} overload;
{$IFNDEF CLR}
{$JPPEXPANDMACRO COPYINT(Copy,IJclPtrIterator)} overload;
{$ENDIF ~CLR}
{$JPPEXPANDMACRO COPYINT(Copy,IJclIterator)} overload;

// Generate algorithms
{$JPPEXPANDMACRO GENERATEINT(Generate,IJclIntfList,const ,AInterface,IInterface)} overload;
{$JPPEXPANDMACRO GENERATEINT(Generate,IJclAnsiStrList,const ,AString,AnsiString)} overload;
{$JPPEXPANDMACRO GENERATEINT(Generate,IJclWideStrList,const ,AString,WideString)} overload;
{$JPPEXPANDMACRO GENERATEINT(Generate,IJclSingleList,const ,AValue,Single)} overload;
{$JPPEXPANDMACRO GENERATEINT(Generate,IJclDoubleList,const ,AValue,Double)} overload;
{$JPPEXPANDMACRO GENERATEINT(Generate,IJclExtendedList,const ,AValue,Extended)} overload;
{$JPPEXPANDMACRO GENERATEINT(Generate,IJclIntegerList,,AValue,Integer)} overload;
{$JPPEXPANDMACRO GENERATEINT(Generate,IJclCardinalList,,AValue,Cardinal)} overload;
{$JPPEXPANDMACRO GENERATEINT(Generate,IJclInt64List,const ,AValue,Int64)} overload;
{$IFNDEF CLR}
{$JPPEXPANDMACRO GENERATEINT(Generate,IJclPtrList,,APtr,Pointer)} overload;
{$ENDIF CLR}
{$JPPEXPANDMACRO GENERATEINT(Generate,IJclList,,AObject,TObject)} overload;

// Fill algorithms
{$JPPEXPANDMACRO FILLINT(Fill,IJclIntfIterator,const ,AInterface,IInterface)} overload;
{$JPPEXPANDMACRO FILLINT(Fill,IJclAnsiStrIterator,const ,AString,AnsiString)} overload;
{$JPPEXPANDMACRO FILLINT(Fill,IJclWideStrIterator,const ,AString,WideString)} overload;
{$JPPEXPANDMACRO FILLINT(Fill,IJclSingleIterator,const ,AValue,Single)} overload;
{$JPPEXPANDMACRO FILLINT(Fill,IJclDoubleIterator,const ,AValue,Double)} overload;
{$JPPEXPANDMACRO FILLINT(Fill,IJclExtendedIterator,const ,AValue,Extended)} overload;
{$JPPEXPANDMACRO FILLINT(Fill,IJclIntegerIterator,,AValue,Integer)} overload;
{$JPPEXPANDMACRO FILLINT(Fill,IJclCardinalIterator,,AValue,Cardinal)} overload;
{$JPPEXPANDMACRO FILLINT(Fill,IJclInt64Iterator,const ,AValue,Int64)} overload;
{$IFNDEF CLR}
{$JPPEXPANDMACRO FILLINT(Fill,IJclPtrIterator,,APtr,Pointer)} overload;
{$ENDIF ~CLR}
{$JPPEXPANDMACRO FILLINT(Fill,IJclIterator,,AObject,TObject)} overload;

// Reverse algorithms
{$JPPEXPANDMACRO REVERSEINT(Reverse,IJclIntfIterator)} overload;
{$JPPEXPANDMACRO REVERSEINT(Reverse,IJclAnsiStrIterator)} overload;
{$JPPEXPANDMACRO REVERSEINT(Reverse,IJclWideStrIterator)} overload;
{$JPPEXPANDMACRO REVERSEINT(Reverse,IJclSingleIterator)} overload;
{$JPPEXPANDMACRO REVERSEINT(Reverse,IJclDoubleIterator)} overload;
{$JPPEXPANDMACRO REVERSEINT(Reverse,IJclExtendedIterator)} overload;
{$JPPEXPANDMACRO REVERSEINT(Reverse,IJclIntegerIterator)} overload;
{$JPPEXPANDMACRO REVERSEINT(Reverse,IJclCardinalIterator)} overload;
{$JPPEXPANDMACRO REVERSEINT(Reverse,IJclInt64Iterator)} overload;
{$IFNDEF CLR}
{$JPPEXPANDMACRO REVERSEINT(Reverse,IJclPtrIterator)} overload;
{$ENDIF CLR}
{$JPPEXPANDMACRO REVERSEINT(Reverse,IJclIterator)} overload;

{$JPPEXPANDMACRO SORTINT(QuickSort,IJclIntfList,L,R,TIntfCompare)} overload;
{$JPPEXPANDMACRO SORTINT(QuickSort,IJclAnsiStrList,L,R,TAnsiStrCompare)} overload;
{$JPPEXPANDMACRO SORTINT(QuickSort,IJclWideStrList,L,R,TWideStrCompare)} overload;
{$JPPEXPANDMACRO SORTINT(QuickSort,IJclSingleList,L,R,TSingleCompare)} overload;
{$JPPEXPANDMACRO SORTINT(QuickSort,IJclDoubleList,L,R,TDoubleCompare)} overload;
{$JPPEXPANDMACRO SORTINT(QuickSort,IJclExtendedList,L,R,TExtendedCompare)} overload;
{$JPPEXPANDMACRO SORTINT(QuickSort,IJclIntegerList,L,R,TIntegerCompare)} overload;
{$JPPEXPANDMACRO SORTINT(QuickSort,IJclCardinalList,L,R,TCardinalCompare)} overload;
{$JPPEXPANDMACRO SORTINT(QuickSort,IJclInt64List,L,R,TInt64Compare)} overload;
{$IFNDEF CLR}
{$JPPEXPANDMACRO SORTINT(QuickSort,IJclPtrList,L,R,TPtrCompare)} overload;
{$ENDIF ~CLR}
{$JPPEXPANDMACRO SORTINT(QuickSort,IJclList,L,R,TCompare)} overload;

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
{$JPPEXPANDMACRO SORTINT(Sort,IJclIntfList,First,Last,TIntfCompare)} overload;
{$JPPEXPANDMACRO SORTINT(Sort,IJclAnsiStrList,First,Last,TAnsiStrCompare)} overload;
{$JPPEXPANDMACRO SORTINT(Sort,IJclWideStrList,First,Last,TWideStrCompare)} overload;
{$JPPEXPANDMACRO SORTINT(Sort,IJclSingleList,First,Last,TSingleCompare)} overload;
{$JPPEXPANDMACRO SORTINT(Sort,IJclDoubleList,First,Last,TDoubleCompare)} overload;
{$JPPEXPANDMACRO SORTINT(Sort,IJclExtendedList,First,Last,TExtendedCompare)} overload;
{$JPPEXPANDMACRO SORTINT(Sort,IJclIntegerList,First,Last,TIntegerCompare)} overload;
{$JPPEXPANDMACRO SORTINT(Sort,IJclCardinalList,First,Last,TCardinalCompare)} overload;
{$JPPEXPANDMACRO SORTINT(Sort,IJclInt64List,First,Last,TInt64Compare)} overload;
{$IFNDEF CLR}
{$JPPEXPANDMACRO SORTINT(Sort,IJclPtrList,First,Last,TPtrCompare)} overload;
{$ENDIF ~CLR}
{$JPPEXPANDMACRO SORTINT(Sort,IJclList,First,Last,TCompare)} overload;

{$IFDEF SUPPORTS_GENERICS}
type
  // cannot implement generic global functions
  TJclAlgorithms<T> = class
  private
    //FSortProc: TSortProc;
  public
    class {$JPPEXPANDMACRO APPLYINT(Apply,IJclIterator<T>,TApplyFunction<T>)}
    class {$JPPEXPANDMACRO FINDINT(Find,IJclIterator<T>,const ,AItem,T,TCompare<T>)} overload;
    class {$JPPEXPANDMACRO FINDEQINT(Find,IJclIterator<T>,const ,AItem,T,TEqualityCompare<T>)} overload;
    class {$JPPEXPANDMACRO COUNTOBJECTINT(CountObject,IJclIterator<T>,const ,AItem,T,TCompare<T>)} overload;
    class {$JPPEXPANDMACRO COUNTOBJECTEQINT(CountObject,IJclIterator<T>,const ,AItem,T,TEqualityCompare<T>)} overload;
    class {$JPPEXPANDMACRO COPYINT(Copy,IJclIterator<T>)}
    class {$JPPEXPANDMACRO GENERATEINT(Generate,IJclList<T>,const ,AItem,T)}
    class {$JPPEXPANDMACRO FILLINT(Fill,IJclIterator<T>,const ,AItem,T)}
    class {$JPPEXPANDMACRO REVERSEINT(Reverse,IJclIterator<T>)}
    class {$JPPEXPANDMACRO SORTINT(QuickSort,IJclList<T>,L,R,TCompare<T>)}
    class {$JPPEXPANDMACRO SORTINT(Sort,IJclList<T>,First,Last,TCompare<T>)}
    //class property SortProc: TSortProc<T> read FSortProc write FSortProc;
  end;
{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/prototypes/JclAlgorithms.pas $';
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

{$JPPEXPANDMACRO APPLYIMP(Apply,IJclIntfIterator,TIntfApplyFunction,SetObject)}

{$JPPEXPANDMACRO APPLYIMP(Apply,IJclAnsiStrIterator,TAnsiStrApplyFunction,SetString)}

{$JPPEXPANDMACRO APPLYIMP(Apply,IJclWideStrIterator,TWideStrApplyFunction,SetString)}

{$JPPEXPANDMACRO APPLYIMP(Apply,IJclSingleIterator,TSingleApplyFunction,SetValue)}

{$JPPEXPANDMACRO APPLYIMP(Apply,IJclDoubleIterator,TDoubleApplyFunction,SetValue)}

{$JPPEXPANDMACRO APPLYIMP(Apply,IJclExtendedIterator,TExtendedApplyFunction,SetValue)}

{$JPPEXPANDMACRO APPLYIMP(Apply,IJclIntegerIterator,TIntegerApplyFunction,SetValue)}

{$JPPEXPANDMACRO APPLYIMP(Apply,IJclCardinalIterator,TCardinalApplyFunction,SetValue)}

{$JPPEXPANDMACRO APPLYIMP(Apply,IJclInt64Iterator,TInt64ApplyFunction,SetValue)}

{$IFNDEF CLR}
{$JPPEXPANDMACRO APPLYIMP(Apply,IJclPtrIterator,TPtrApplyFunction,SetPointer)}
{$ENDIF ~CLR}

{$JPPEXPANDMACRO APPLYIMP(Apply,IJclIterator,TApplyFunction,SetObject)}

{$JPPEXPANDMACRO FINDIMP(Find,IJclIntfIterator,const ,AInterface,IInterface,TIntfCompare)}

{$JPPEXPANDMACRO FINDEQIMP(Find,IJclIntfIterator,const ,AInterface,IInterface,TIntfEqualityCompare)}

{$JPPEXPANDMACRO FINDIMP(Find,IJclAnsiStrIterator,const ,AString,AnsiString,TAnsiStrCompare)}

{$JPPEXPANDMACRO FINDEQIMP(Find,IJclAnsiStrIterator,const ,AString,AnsiString,TAnsiStrEqualityCompare)}

{$JPPEXPANDMACRO FINDIMP(Find,IJclWideStrIterator,const ,AString,WideString,TWideStrCompare)}

{$JPPEXPANDMACRO FINDEQIMP(Find,IJclWideStrIterator,const ,AString,WideString,TWideStrEqualityCompare)}

{$JPPEXPANDMACRO FINDIMP(Find,IJclSingleIterator,const ,AValue,Single,TSingleCompare)}

{$JPPEXPANDMACRO FINDEQIMP(Find,IJclSingleIterator,const ,AValue,Single,TSingleEqualityCompare)}

{$JPPEXPANDMACRO FINDIMP(Find,IJclDoubleIterator,const ,AValue,Double,TDoubleCompare)}

{$JPPEXPANDMACRO FINDEQIMP(Find,IJclDoubleIterator,const ,AValue,Double,TDoubleEqualityCompare)}

{$JPPEXPANDMACRO FINDIMP(Find,IJclExtendedIterator,const ,AValue,Extended,TExtendedCompare)}

{$JPPEXPANDMACRO FINDEQIMP(Find,IJclExtendedIterator,const ,AValue,Extended,TExtendedEqualityCompare)}

{$JPPEXPANDMACRO FINDIMP(Find,IJclIntegerIterator,,AValue,Integer,TIntegerCompare)}

{$JPPEXPANDMACRO FINDEQIMP(Find,IJclIntegerIterator,,AValue,Integer,TIntegerEqualityCompare)}

{$JPPEXPANDMACRO FINDIMP(Find,IJclCardinalIterator,,AValue,Cardinal,TCardinalCompare)}

{$JPPEXPANDMACRO FINDEQIMP(Find,IJclCardinalIterator,,AValue,Cardinal,TCardinalEqualityCompare)}

{$JPPEXPANDMACRO FINDIMP(Find,IJclInt64Iterator,const ,AValue,Int64,TInt64Compare)}

{$JPPEXPANDMACRO FINDEQIMP(Find,IJclInt64Iterator,const ,AValue,Int64,TInt64EqualityCompare)}

{$IFNDEF CLR}
{$JPPEXPANDMACRO FINDIMP(Find,IJclPtrIterator,,APtr,Pointer,TPtrCompare)}

{$JPPEXPANDMACRO FINDEQIMP(Find,IJclPtrIterator,,APtr,Pointer,TPtrEqualityCompare)}
{$ENDIF ~CLR}

{$JPPEXPANDMACRO FINDIMP(Find,IJclIterator,,AObject,TObject,TCompare)}

{$JPPEXPANDMACRO FINDEQIMP(Find,IJclIterator,,AObject,TObject,TEqualityCompare)}

{$JPPEXPANDMACRO COUNTOBJECTIMP(CountObject,IJclIntfIterator,const ,AInterface,IInterface,TIntfCompare)}

{$JPPEXPANDMACRO COUNTOBJECTEQIMP(CountObject,IJclIntfIterator,const ,AInterface,IInterface,TIntfEqualityCompare)}

{$JPPEXPANDMACRO COUNTOBJECTIMP(CountObject,IJclAnsiStrIterator,const ,AString,AnsiString,TAnsiStrCompare)}

{$JPPEXPANDMACRO COUNTOBJECTEQIMP(CountObject,IJclAnsiStrIterator,const ,AString,AnsiString,TAnsiStrEqualityCompare)}

{$JPPEXPANDMACRO COUNTOBJECTIMP(CountObject,IJclWideStrIterator,const ,AString,WideString,TWideStrCompare)}

{$JPPEXPANDMACRO COUNTOBJECTEQIMP(CountObject,IJclWideStrIterator,const ,AString,WideString,TWideStrEqualityCompare)}

{$JPPEXPANDMACRO COUNTOBJECTIMP(CountObject,IJclSingleIterator,const ,AValue,Single,TSingleCompare)}

{$JPPEXPANDMACRO COUNTOBJECTEQIMP(CountObject,IJclSingleIterator,const ,AValue,Single,TSingleEqualityCompare)}

{$JPPEXPANDMACRO COUNTOBJECTIMP(CountObject,IJclDoubleIterator,const ,AValue,Double,TDoubleCompare)}

{$JPPEXPANDMACRO COUNTOBJECTEQIMP(CountObject,IJclDoubleIterator,const ,AValue,Double,TDoubleEqualityCompare)}

{$JPPEXPANDMACRO COUNTOBJECTIMP(CountObject,IJclExtendedIterator,const ,AValue,Extended,TExtendedCompare)}

{$JPPEXPANDMACRO COUNTOBJECTEQIMP(CountObject,IJclExtendedIterator,const ,AValue,Extended,TExtendedEqualityCompare)}

{$JPPEXPANDMACRO COUNTOBJECTIMP(CountObject,IJclIntegerIterator,,AValue,Integer,TIntegerCompare)}

{$JPPEXPANDMACRO COUNTOBJECTEQIMP(CountObject,IJclIntegerIterator,,AValue,Integer,TIntegerEqualityCompare)}

{$JPPEXPANDMACRO COUNTOBJECTIMP(CountObject,IJclCardinalIterator,,AValue,Cardinal,TCardinalCompare)}

{$JPPEXPANDMACRO COUNTOBJECTEQIMP(CountObject,IJclCardinalIterator,,AValue,Cardinal,TCardinalEqualityCompare)}

{$JPPEXPANDMACRO COUNTOBJECTIMP(CountObject,IJclInt64Iterator,const ,AValue,Int64,TInt64Compare)}

{$JPPEXPANDMACRO COUNTOBJECTEQIMP(CountObject,IJclInt64Iterator,const ,AValue,Int64,TInt64EqualityCompare)}

{$IFNDEF CLR}
{$JPPEXPANDMACRO COUNTOBJECTIMP(CountObject,IJclPtrIterator,,APtr,Pointer,TPtrCompare)}

{$JPPEXPANDMACRO COUNTOBJECTEQIMP(CountObject,IJclPtrIterator,,APtr,Pointer,TPtrEqualityCompare)}
{$ENDIF ~CLR}

{$JPPEXPANDMACRO COUNTOBJECTIMP(CountObject,IJclIterator,,AObject,TObject,TCompare)}

{$JPPEXPANDMACRO COUNTOBJECTEQIMP(CountObject,IJclIterator,,AObject,TObject,TEqualityCompare)}

{$JPPEXPANDMACRO COPYIMP(Copy,IJclIntfIterator,SetObject)}

{$JPPEXPANDMACRO COPYIMP(Copy,IJclAnsiStrIterator,SetString)}

{$JPPEXPANDMACRO COPYIMP(Copy,IJclWideStrIterator,SetString)}

{$JPPEXPANDMACRO COPYIMP(Copy,IJclSingleIterator,SetValue)}

{$JPPEXPANDMACRO COPYIMP(Copy,IJclDoubleIterator,SetValue)}

{$JPPEXPANDMACRO COPYIMP(Copy,IJclExtendedIterator,SetValue)}

{$JPPEXPANDMACRO COPYIMP(Copy,IJclIntegerIterator,SetValue)}

{$JPPEXPANDMACRO COPYIMP(Copy,IJclCardinalIterator,SetValue)}

{$JPPEXPANDMACRO COPYIMP(Copy,IJclInt64Iterator,SetValue)}

{$IFNDEF CLR}
{$JPPEXPANDMACRO COPYIMP(Copy,IJclPtrIterator,SetPointer)}
{$ENDIF ~CLR}

{$JPPEXPANDMACRO COPYIMP(Copy,IJclIterator,SetObject)}

{$JPPEXPANDMACRO GENERATEIMP(Generate,IJclIntfList,const ,AInterface,IInterface)}

{$JPPEXPANDMACRO GENERATEIMP(Generate,IJclAnsiStrList,const ,AString,AnsiString)}

{$JPPEXPANDMACRO GENERATEIMP(Generate,IJclWideStrList,const ,AString,WideString)}

{$JPPEXPANDMACRO GENERATEIMP(Generate,IJclSingleList,const ,AValue,Single)}

{$JPPEXPANDMACRO GENERATEIMP(Generate,IJclDoubleList,const ,AValue,Double)}

{$JPPEXPANDMACRO GENERATEIMP(Generate,IJclExtendedList,const ,AValue,Extended)}

{$JPPEXPANDMACRO GENERATEIMP(Generate,IJclIntegerList,,AValue,Integer)}

{$JPPEXPANDMACRO GENERATEIMP(Generate,IJclCardinalList,,AValue,Cardinal)}

{$JPPEXPANDMACRO GENERATEIMP(Generate,IJclInt64List,const ,AValue,Int64)}

{$IFNDEF CLR}
{$JPPEXPANDMACRO GENERATEIMP(Generate,IJclPtrList,,APtr,Pointer)}
{$ENDIF ~CLR}

{$JPPEXPANDMACRO GENERATEIMP(Generate,IJclList,,AObject,TObject)}

{$JPPEXPANDMACRO FILLIMP(Fill,IJclIntfIterator,const ,AInterface,IInterface,SetObject)}

{$JPPEXPANDMACRO FILLIMP(Fill,IJclAnsiStrIterator,const ,AString,AnsiString,SetString)}

{$JPPEXPANDMACRO FILLIMP(Fill,IJclWideStrIterator,const ,AString,WideString,SetString)}

{$JPPEXPANDMACRO FILLIMP(Fill,IJclSingleIterator,const ,AValue,Single,SetValue)}

{$JPPEXPANDMACRO FILLIMP(Fill,IJclDoubleIterator,const ,AValue,Double,SetValue)}

{$JPPEXPANDMACRO FILLIMP(Fill,IJclExtendedIterator,const ,AValue,Extended,SetValue)}

{$JPPEXPANDMACRO FILLIMP(Fill,IJclIntegerIterator,,AValue,Integer,SetValue)}

{$JPPEXPANDMACRO FILLIMP(Fill,IJclCardinalIterator,,AValue,Cardinal,SetValue)}

{$JPPEXPANDMACRO FILLIMP(Fill,IJclInt64Iterator,const ,AValue,Int64,SetValue)}

{$IFNDEF CLR}
{$JPPEXPANDMACRO FILLIMP(Fill,IJclPtrIterator,,APtr,Pointer,SetPointer)}
{$ENDIF ~CLR}

{$JPPEXPANDMACRO FILLIMP(Fill,IJclIterator,,AObject,TObject,SetObject)}

{$JPPEXPANDMACRO REVERSEIMP(Reverse,IJclIntfIterator,IInterface,GetObject,SetObject)}

{$JPPEXPANDMACRO REVERSEIMP(Reverse,IJclAnsiStrIterator,AnsiString,GetString,SetString)}

{$JPPEXPANDMACRO REVERSEIMP(Reverse,IJclWideStrIterator,WideString,GetString,SetString)}

{$JPPEXPANDMACRO REVERSEIMP(Reverse,IJclSingleIterator,Single,GetValue,SetValue)}

{$JPPEXPANDMACRO REVERSEIMP(Reverse,IJclDoubleIterator,Double,GetValue,SetValue)}

{$JPPEXPANDMACRO REVERSEIMP(Reverse,IJclExtendedIterator,Extended,GetValue,SetValue)}

{$JPPEXPANDMACRO REVERSEIMP(Reverse,IJclIntegerIterator,Integer,GetValue,SetValue)}

{$JPPEXPANDMACRO REVERSEIMP(Reverse,IJclCardinalIterator,Cardinal,GetValue,SetValue)}

{$JPPEXPANDMACRO REVERSEIMP(Reverse,IJclInt64Iterator,Int64,GetValue,SetValue)}

{$IFNDEF CLR}
{$JPPEXPANDMACRO REVERSEIMP(Reverse,IJclPtrIterator,Pointer,GetPointer,SetPointer)}
{$ENDIF ~CLR}

{$JPPEXPANDMACRO REVERSEIMP(Reverse,IJclIterator,TObject,GetObject,SetObject)}

{$JPPEXPANDMACRO QUICKSORTIMP(QuickSort,IJclIntfList,L,R,TIntfCompare,IInterface,GetObject,SetObject)}

{$JPPEXPANDMACRO QUICKSORTIMP(QuickSort,IJclAnsiStrList,L,R,TAnsiStrCompare,AnsiString,GetString,SetString)}

{$JPPEXPANDMACRO QUICKSORTIMP(QuickSort,IJclWideStrList,L,R,TWideStrCompare,WideString,GetString,SetString)}

{$JPPEXPANDMACRO QUICKSORTIMP(QuickSort,IJclSingleList,L,R,TSingleCompare,Single,GetValue,SetValue)}

{$JPPEXPANDMACRO QUICKSORTIMP(QuickSort,IJclDoubleList,L,R,TDoubleCompare,Double,GetValue,SetValue)}

{$JPPEXPANDMACRO QUICKSORTIMP(QuickSort,IJclExtendedList,L,R,TExtendedCompare,Extended,GetValue,SetValue)}

{$JPPEXPANDMACRO QUICKSORTIMP(QuickSort,IJclIntegerList,L,R,TIntegerCompare,Integer,GetValue,SetValue)}

{$JPPEXPANDMACRO QUICKSORTIMP(QuickSort,IJclCardinalList,L,R,TCardinalCompare,Cardinal,GetValue,SetValue)}

{$JPPEXPANDMACRO QUICKSORTIMP(QuickSort,IJclInt64List,L,R,TInt64Compare,Int64,GetValue,SetValue)}

{$IFNDEF CLR}
{$JPPEXPANDMACRO QUICKSORTIMP(QuickSort,IJclPtrList,L,R,TPtrCompare,Pointer,GetPointer,SetPointer)}
{$ENDIF ~CLR}

{$JPPEXPANDMACRO QUICKSORTIMP(QuickSort,IJclList,L,R,TCompare,TObject,GetObject,SetObject)}

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
class {$JPPEXPANDMACRO APPLYIMP(TJclAlgorithms<T>.Apply,IJclIterator<T>,TApplyFunction<T>,SetItem)}

class {$JPPEXPANDMACRO FINDIMP(TJclAlgorithms<T>.Find,IJclIterator<T>,const ,AItem,T,TCompare<T>)}

class {$JPPEXPANDMACRO FINDEQIMP(TJclAlgorithms<T>.Find,IJclIterator<T>,const ,AItem,T,TEqualityCompare<T>)}

class {$JPPEXPANDMACRO COUNTOBJECTIMP(TJclAlgorithms<T>.CountObject,IJclIterator<T>,const ,AItem,T,TCompare<T>)}

class {$JPPEXPANDMACRO COUNTOBJECTEQIMP(TJclAlgorithms<T>.CountObject,IJclIterator<T>,const ,AItem,T,TEqualityCompare<T>)}

class {$JPPEXPANDMACRO COPYIMP(TJclAlgorithms<T>.Copy,IJclIterator<T>,SetItem)}

class {$JPPEXPANDMACRO GENERATEIMP(TJclAlgorithms<T>.Generate,IJclList<T>,const ,AItem,T)}

class {$JPPEXPANDMACRO FILLIMP(TJclAlgorithms<T>.Fill,IJclIterator<T>,const ,AItem,T,SetItem)}

class {$JPPEXPANDMACRO REVERSEIMP(TJclAlgorithms<T>.Reverse,IJclIterator<T>,T,GetItem,SetItem)}

class {$JPPEXPANDMACRO QUICKSORTIMP(TJclAlgorithms<T>.QuickSort,IJclList<T>,L,R,TCompare<T>,T,GetItem,SetItem)}

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