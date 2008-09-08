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
{ The Original Code is NewStringListUnit.pas.                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is Romullo Sousa.                                     }
{ Portions created by Romullo Sousa are Copyright (C) Romullo Sousa. All rights reserved.          }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{     Romullo Sousa (romullobr)                                                                    }
{     Leo Simas (Leh_U)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains several improvements of the standard TStringList.                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-11-30 20:32:28 +0100 (ven., 30 nov. 2007)                          $ }
{ Revision:      $Rev:: 2245                                                                     $ }
{ Author:        $Author:: ahuser                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStringLists;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF CLR}
  System.Text.RegularExpressions,
  {$ENDIF CLR}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  Classes, SysUtils,
  JclBase;

{$DEFINE HAS_TSTRINGS_COMPARESTRINGS}
{$IFDEF FPC}
 {$UNDEF HAS_TSTRINGS_COMPARESTRINGS}
{$ENDIF FPC}
{$IFDEF COMPILER5}
 {$UNDEF HAS_TSTRINGS_COMPARESTRINGS}
{$ENDIF COMPILER5}

type
  IJclStringList = interface;

  TJclStringListObjectsMode = (omNone, omObjects, omVariants, omInterfaces);

  TJclStringListSortCompare = function(List: IJclStringList; Index1, Index2: Integer): Integer;

  IJclStringList = interface(IInterface)
    ['{8DC5B71C-4756-404D-8636-7872CD299796}']
    { From TStrings/TStringList }
    function Add(const S: string): Integer; overload;
    function AddObject(const S: string; AObject: TObject): Integer;
    function Get(Index: Integer): string;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetObjects(Index: Integer): TObject;
    function GetTextStr: string;
    function GetValue(const Name: string): string;
    function Find(const S: string; var Index: Integer): Boolean;
    function IndexOf(const S: string): Integer;
    {$IFDEF COMPILER6_UP}
    function GetCaseSensitive: Boolean;
    {$ENDIF COMPILER6_UP}
    function GetDuplicates: TDuplicates;
    function GetOnChange: TNotifyEvent;
    function GetOnChanging: TNotifyEvent;
    function GetSorted: Boolean;
    function Equals(Strings: TStrings): Boolean;
    function IndexOfName(const Name: string): Integer;
    function IndexOfObject(AObject: TObject): Integer; 
    function LoadFromFile(const FileName: string): IJclStringList;
    function LoadFromStream(Stream: TStream): IJclStringList;
    function SaveToFile(const FileName: string): IJclStringList;
    function SaveToStream(Stream: TStream): IJclStringList;
    function GetCommaText: string;
    {$IFDEF COMPILER6_UP}
    function GetDelimitedText: string;
    function GetDelimiter: Char;
    {$ENDIF COMPILER6_UP}
    function GetName(Index: Integer): string;
    {$IFDEF COMPILER7_UP}
    function GetNameValueSeparator: Char;
    function GetValueFromIndex(Index: Integer): string;
    {$ENDIF COMPILER7_UP}
    {$IFDEF COMPILER6_UP}
    function GetQuoteChar: Char;
    {$ENDIF COMPILER6_UP}
    procedure SetCommaText(const Value: string);
    {$IFDEF COMPILER6_UP}
    procedure SetDelimitedText(const Value: string);
    procedure SetDelimiter(const Value: Char);
    {$ENDIF COMPILER6_UP}
    {$IFDEF COMPILER7_UP}
    procedure SetNameValueSeparator(const Value: Char);
    procedure SetValueFromIndex(Index: Integer; const Value: string);
    {$ENDIF COMPILER7_UP}
    {$IFDEF COMPILER6_UP}
    procedure SetQuoteChar(const Value: Char);
    {$ENDIF COMPILER6_UP}
    procedure AddStrings(Strings: TStrings); overload;
    procedure SetObjects(Index: Integer; const Value: TObject);
    procedure Put(Index: Integer; const S: string);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetTextStr(const Value: string);
    procedure SetValue(const Name, Value: string);
    {$IFDEF COMPILER6_UP}
    procedure SetCaseSensitive(const Value: Boolean);
    {$ENDIF COMPILER6_UP}
    procedure SetDuplicates(const Value: TDuplicates);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetOnChanging(const Value: TNotifyEvent);
    procedure SetSorted(const Value: Boolean);
    property Count: Integer read GetCount;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Text: string read GetTextStr write SetTextStr;
    property Objects[Index: Integer]: TObject read GetObjects write SetObjects;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Values[const Name: string]: string read GetValue write SetValue;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Sorted: Boolean read GetSorted write SetSorted;
    {$IFDEF COMPILER6_UP}
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    {$ENDIF COMPILER6_UP}
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property OnChanging: TNotifyEvent read GetOnChanging write SetOnChanging;
    {$IFDEF COMPILER6_UP}
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    {$ENDIF COMPILER6_UP}
    property Names[Index: Integer]: string read GetName;
    {$IFDEF COMPILER6_UP}
    property QuoteChar: Char read GetQuoteChar write SetQuoteChar;
    {$ENDIF COMPILER6_UP}
    property CommaText: string read GetCommaText write SetCommaText;
    {$IFDEF COMPILER7_UP}
    property ValueFromIndex[Index: Integer]: string read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: Char read GetNameValueSeparator write SetNameValueSeparator;
    {$ENDIF COMPILER7_UP}
    { New }
    function Assign(Source: TPersistent): IJclStringList;
    function LoadExeParams: IJclStringList;
    function Exists(const S: string): Boolean;
    function ExistsName(const S: string): Boolean;
    function DeleteBlanks: IJclStringList;
    function KeepIntegers: IJclStringList;
    function DeleteIntegers: IJclStringList;
    function ReleaseInterfaces: IJclStringList;
    function FreeObjects(AFreeAndNil: Boolean = False): IJclStringList;
    function Clone: IJclStringList;
    function Insert(Index: Integer; const S: string): IJclStringList;
    function InsertObject(Index: Integer; const S: string; AObject: TObject): IJclStringList;
    function Sort(ACompareFunction: TJclStringListSortCompare = nil): IJclStringList;
    function SortAsInteger: IJclStringList;
    function SortByName: IJclStringList;
    function Delete(AIndex: Integer): IJclStringList; overload;
    function Delete(const AString: string): IJclStringList; overload;
    function Exchange(Index1, Index2: Integer): IJclStringList;
    function Add(const A: array of const): IJclStringList; overload;
    function AddStrings(const A: array of string): IJclStringList; overload;
    function BeginUpdate: IJclStringList;
    function EndUpdate: IJclStringList;
    function Trim: IJclStringList;
    function Join(const ASeparator: string = ''): string;
    function Split(const AText, ASeparator: string; AClearBeforeAdd: Boolean = True): IJclStringList;
    {$IFDEF CLR}
    function ExtractWords(const AText: string): IJclStringList; overload;
    function ExtractWords(const AText: string; const ADelims: TSetOfAnsiChar; AClearBeforeAdd: Boolean = True): IJclStringList; overload;
    {$ELSE}
    function ExtractWords(const AText: string; const ADelims: TSetOfAnsiChar = [#0..' ']; AClearBeforeAdd: Boolean = True): IJclStringList;
    {$ENDIF CLR}
    function Last: string;
    function First: string;
    function LastIndex: Integer;
    function Clear: IJclStringList;
    function DeleteRegEx(const APattern: string): IJclStringList;
    function KeepRegEx(const APattern: string): IJclStringList;
    function Files(const APattern: string = '*'; ARecursive: Boolean = False; const ARegExPattern: string = ''): IJclStringList;
    function Directories(const APattern: string = '*'; ARecursive: Boolean = False; const ARegExPattern: string = ''): IJclStringList;
    function GetStringsRef: TStrings;
    function ConfigAsSet: IJclStringList;
    function Delimit(const ADelimiter: string): IJclStringList;
    function GetInterfaceByIndex(Index: Integer): IInterface;
    function GetLists(Index: Integer): IJclStringList;
    function GetVariants(AIndex: Integer): Variant;
    function GetKeyInterface(const AKey: string): IInterface;
    function GetKeyObject(const AKey: string): TObject;
    function GetKeyVariant(const AKey: string): Variant;
    function GetKeyList(const AKey: string): IJclStringList;
    function GetObjectsMode: TJclStringListObjectsMode;
    procedure SetInterfaceByIndex(Index: Integer; const Value: IInterface);
    procedure SetLists(Index: Integer; const Value: IJclStringList);
    procedure SetVariants(Index: Integer; const Value: Variant);
    procedure SetKeyInterface(const AKey: string; const Value: IInterface);
    procedure SetKeyObject(const AKey: string; const Value: TObject);
    procedure SetKeyVariant(const AKey: string; const Value: Variant);
    procedure SetKeyList(const AKey: string; const Value: IJclStringList);
    property Interfaces[Index: Integer]: IInterface read GetInterfaceByIndex write SetInterfaceByIndex;
    property Lists[Index: Integer]: IJclStringList read GetLists write SetLists;
    property Variants[Index: Integer]: Variant read GetVariants write SetVariants;
    property KeyList[const AKey: string]: IJclStringList read GetKeyList write SetKeyList;
    property KeyObject[const AKey: string]: TObject read GetKeyObject write SetKeyObject;
    property KeyInterface[const AKey: string]: IInterface read GetKeyInterface write SetKeyInterface;
    property KeyVariant[const AKey: string]: Variant read GetKeyVariant write SetKeyVariant;
    property ObjectsMode: TJclStringListObjectsMode read GetObjectsMode;
  end;

function JclStringList: IJclStringList; overload;
function JclStringListStrings(AStrings: TStrings): IJclStringList; overload;
function JclStringListStrings(const A: array of string): IJclStringList; overload;
function JclStringList(const A: array of const): IJclStringList; overload;
function JclStringList(const AText: string): IJclStringList; overload;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclStringLists.pas $';
    Revision: '$Revision: 2245 $';
    Date: '$Date: 2007-11-30 20:32:28 +0100 (ven., 30 nov. 2007) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  TypInfo,
  JclFileUtils,
  {$IFDEF CLR}
  {$ELSE}
  JclPCRE,
  {$ENDIF CLR}
  JclStrings;

type
  TUpdateControl = class(TObject, IInterface)
  private
    FStrings: TStrings;
  {$IFNDEF CLR}
  protected
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  {$ENDIF ~CLR}
  public
    constructor Create(AStrings: TStrings);
  end;

  TVariantWrapper = class(TObject)
  private
    FValue: Variant;
  end;

  TInterfaceWrapper = class(TObject)
  private
    FValue: IInterface;
  end;

  TJclStringListImpl = class(TStringList, IJclStringList)
  private
    FObjectsMode: TJclStringListObjectsMode;
    FSelfAsInterface: IJclStringList;
    {$IFDEF CLR}
    FRegEx: System.Text.RegularExpressions.Regex;
    FCustomSortCompare: TJclStringListSortCompare;
    {$ELSE}
    FLastRegExPattern: string;
    FRegEx: TJclAnsiRegEx;
    {$ENDIF CLR}
    FUpdateControl: TUpdateControl;
    function AutoUpdateControl: IInterface;
    function CanFreeObjects: Boolean;
    function MatchRegEx(const S, APattern: string): Boolean;
    function GetLists(Index: Integer): IJclStringList;
    function GetKeyInterface(const AKey: string): IInterface;
    function GetKeyObject(const AKey: string): TObject;
    function GetKeyVariant(const AKey: string): Variant;
    function GetValue(const Name: string): string;
    function GetVariants(AIndex: Integer): Variant;
    function GetKeyList(const AKey: string): IJclStringList;
    {$IFDEF COMPILER6_UP}
    function GetCaseSensitive: Boolean;
    {$ENDIF COMPILER6_UP}
    function GetDuplicates: TDuplicates;
    function GetOnChange: TNotifyEvent;
    function GetOnChanging: TNotifyEvent;
    function GetSorted: Boolean;
    function GetCommaText: string;
    {$IFDEF COMPILER6_UP}
    function GetDelimitedText: string;
    function GetDelimiter: Char;
    {$ENDIF COMPILER6_UP}
    function GetName(Index: Integer): string;
    {$IFDEF COMPILER7_UP}
    function GetNameValueSeparator: Char;
    function GetValueFromIndex(Index: Integer): string;
    {$ENDIF COMPILER7_UP}
    {$IFDEF COMPILER6_UP}
    function GetQuoteChar: Char;
    {$ENDIF COMPILER6_UP}
    function GetInterfaceByIndex(AIndex: Integer): IInterface;
    function GetObjects(Index: Integer): TObject;
    procedure SetValue(const Name, Value: string);
    procedure SetKeyList(const AKey: string; const Value: IJclStringList);
    procedure SetKeyInterface(const AKey: string; const Value: IInterface);
    procedure SetKeyObject(const AKey: string; const Value: TObject);
    procedure SetKeyVariant(const AKey: string; const Value: Variant);
    procedure SetLists(Index: Integer; const Value: IJclStringList);
    procedure SetVariants(Index: Integer; const Value: Variant);
    {$IFDEF COMPILER6_UP}
    procedure SetCaseSensitive(const Value: Boolean);
    {$ENDIF COMPILER6_UP}
    procedure SetDuplicates(const Value: TDuplicates);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetOnChanging(const Value: TNotifyEvent);
    procedure SetSorted(const Value: Boolean);
    procedure SetCommaText(const Value: string);
    {$IFDEF COMPILER6_UP}
    procedure SetDelimitedText(const Value: string);
    procedure SetDelimiter(const Value: Char);
    {$ENDIF COMPILER6_UP}
    {$IFDEF COMPILER7_UP}
    procedure SetNameValueSeparator(const Value: Char);
    procedure SetValueFromIndex(Index: Integer; const Value: string);
    {$ENDIF COMPILER7_UP}
    {$IFDEF COMPILER6_UP}
    procedure SetQuoteChar(const Value: Char);
    {$ENDIF COMPILER6_UP}
    procedure SetInterfaceByIndex(Index: Integer; const Value: IInterface);
    procedure SetObjects(Index: Integer; const Value: TObject);
    procedure EnsureObjectsMode(AMode: TJclStringListObjectsMode);
    function GetObjectsMode: TJclStringListObjectsMode;
    {$IFDEF CLR}
    function SortByNameCmp(Index1, Index2: Integer): Integer;
    {$ENDIF CLR}
  protected
    {$IFDEF CLR}
    {$ELSE}
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    {$ENDIF CLR}
    {$IFNDEF HAS_TSTRINGS_COMPARESTRINGS}
    function CompareStrings(const S1, S2: string): Integer; virtual;
    {$ENDIF ~HAS_TSTRINGS_COMPARESTRINGS}
  public
    constructor Create;
    destructor Destroy; override;
    function LoadExeParams: IJclStringList;
    function Exists(const S: string): Boolean;
    function ExistsName(const S: string): Boolean;
    function DeleteBlanks: IJclStringList;
    function KeepIntegers: IJclStringList;
    function DeleteIntegers: IJclStringList;
    function ReleaseInterfaces: IJclStringList;
    function FreeObjects(AFreeAndNil: Boolean = False): IJclStringList;
    function Clone: IJclStringList;
    function Add(const A: array of const): IJclStringList; reintroduce; overload;
    function AddStrings(const A: array of string): IJclStringList; reintroduce; overload;
    function BeginUpdate: IJclStringList;
    function EndUpdate: IJclStringList;
    function Trim: IJclStringList;
    function Delimit(const ADelimiter: string): IJclStringList;
    function Join(const ASeparator: string = ''): string;
    function Split(const AText, ASeparator: string; AClearBeforeAdd: Boolean = True): IJclStringList;
    {$IFDEF CLR}
    function ExtractWords(const AText: string): IJclStringList; overload;
    function ExtractWords(const AText: string; const ADelims: TSetOfAnsiChar; AClearBeforeAdd: Boolean = True): IJclStringList; overload;
    {$ELSE}
    function ExtractWords(const AText: string; const ADelims: TSetOfAnsiChar = [#0..' ']; AClearBeforeAdd: Boolean = True): IJclStringList;
    {$ENDIF CLR}
    function Last: string;
    function First: string;
    function LastIndex: Integer;
    function Clear: IJclStringList; reintroduce;
    function DeleteRegEx(const APattern: string): IJclStringList;
    function KeepRegEx(const APattern: string): IJclStringList;
    function Files(const APattern: string = '*'; ARecursive: Boolean = False;
      const ARegExPattern: string = ''): IJclStringList;
    function Directories(const APattern: string = '*'; ARecursive: Boolean = False;
      const ARegExPattern: string = ''): IJclStringList;
    function GetStringsRef: TStrings;
    function ConfigAsSet: IJclStringList;
    function Delete(AIndex: Integer): IJclStringList; reintroduce; overload;
    function Delete(const AString: string): IJclStringList; reintroduce; overload;
    function Exchange(Index1, Index2: Integer): IJclStringList; reintroduce;
    function Sort(ACompareFunction: TJclStringListSortCompare = nil): IJclStringList; reintroduce;
    function SortAsInteger: IJclStringList;
    function SortByName: IJclStringList;
    function Insert(Index: Integer; const S: string): IJclStringList; reintroduce;
    function InsertObject(Index: Integer; const S: string; AObject: TObject): IJclStringList; reintroduce;
    function LoadFromFile(const FileName: string): IJclStringList; reintroduce;
    function LoadFromStream(Stream: TStream): IJclStringList; reintroduce;
    function SaveToFile(const FileName: string): IJclStringList; reintroduce;
    function SaveToStream(Stream: TStream): IJclStringList; reintroduce;
    function Assign(Source: TPersistent): IJclStringList; reintroduce;
    { From TStrings/TStringList }
    property Values[const Name: string]: string read GetValue write SetValue;
    {$IFDEF COMPILER6_UP}
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    {$ENDIF COMPILER6_UP}
    property Names[Index: Integer]: string read GetName;
    {$IFDEF COMPILER6_UP}
    property QuoteChar: Char read GetQuoteChar write SetQuoteChar;
    {$ENDIF COMPILER6_UP}
    property CommaText: string read GetCommaText write SetCommaText;
    {$IFDEF COMPILER7_UP}
    property ValueFromIndex[Index: Integer]: string read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: Char read GetNameValueSeparator write SetNameValueSeparator;
    {$ENDIF COMPILER7_UP}
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Sorted: Boolean read GetSorted write SetSorted;
    {$IFDEF COMPILER6_UP}
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    {$ENDIF COMPILER6_UP}
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property OnChanging: TNotifyEvent read GetOnChanging write SetOnChanging;
    { New }
    property Objects[Index: Integer]: TObject read GetObjects write SetObjects;
    property Interfaces[Index: Integer]: IInterface read GetInterfaceByIndex write SetInterfaceByIndex;
    property Lists[Index: Integer]: IJclStringList read GetLists write SetLists;
    property Variants[Index: Integer]: Variant read GetVariants write SetVariants;
    property KeyList[const AKey: string]: IJclStringList read GetKeyList write SetKeyList;
    property KeyObject[const AKey: string]: TObject read GetKeyObject write SetKeyObject;
    property KeyInterface[const AKey: string]: IInterface read GetKeyInterface write SetKeyInterface;
    property KeyVariant[const AKey: string]: Variant read GetKeyVariant write SetKeyVariant;
    property ObjectsMode: TJclStringListObjectsMode read GetObjectsMode;
  end;

function JclStringList: IJclStringList;
begin
  Result := TJclStringListImpl.Create;
end;

function JclStringList(const AText: string): IJclStringList; overload;
begin
  Result := JclStringList;
  Result.Text := AText;
end;

function JclStringListStrings(AStrings: TStrings): IJclStringList; overload;
begin
  Result := JclStringList;
  Result.AddStrings(AStrings);
end;

function JclStringListStrings(const A: array of string): IJclStringList;
begin
  Result := JclStringList.AddStrings(A);
end;

function JclStringList(const A: array of const): IJclStringList;
begin
  Result := JclStringList.Add(A);
end;

//=== { TJclStringListImpl } =================================================

function TJclStringListImpl.Add(const A: array of const): IJclStringList;
const
  BoolToStr: array [Boolean] of string[5] = ('false', 'true');
var
  I: Integer;
begin
  AutoUpdateControl;
  for I := Low(A) to High(A) do
    {$IFDEF CLR}
    if A[I].GetType = TypeOf(Boolean) then
      Add(BoolToStr[A[I] as Boolean])
    else
      Add(A[I].ToString);
    {$ELSE}
    with A[I] do
      case VType of
        vtInteger:
          Add(IntToStr(VInteger));
        vtBoolean:
          Add(BoolToStr[VBoolean]);
        vtChar:
          Add(VChar);
        vtExtended:
          Add(FloatToStr(VExtended^));
        vtString:
          Add(VString^);
        vtPChar:
          Add(VPChar);
        vtObject:
          Add(VObject.ClassName);
        vtClass:
          Add(VClass.ClassName);
        vtAnsiString:
          Add(string(VAnsiString));
        vtCurrency:
          Add(CurrToStr(VCurrency^));
        vtVariant:
          Add(string(VVariant^));
        vtInt64:
          Add(IntToStr(VInt64^));
      end;
    {$ENDIF CLR}
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.AddStrings(const A: array of string): IJclStringList;
var
  I: Integer;
begin
  AutoUpdateControl;
  for I := Low(A) to High(A) do
    Add(A[I]);
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.BeginUpdate: IJclStringList;
begin
  inherited BeginUpdate;
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.AutoUpdateControl: IInterface;
begin
  Result := FUpdateControl as IInterface;
end;

function TJclStringListImpl.Clear: IJclStringList;
begin
  if CanFreeObjects then
    FreeObjects(False);
  inherited Clear;
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.EndUpdate: IJclStringList;
begin
  inherited EndUpdate;
  Result := FSelfAsInterface;
end;

{$IFDEF CLR}
function TJclStringListImpl.ExtractWords(const AText: string): IJclStringList;
begin
  Result := ExtractWords(AText, [#0..' ']);
end;
{$ENDIF CLR}

function TJclStringListImpl.ExtractWords(const AText: string; const ADelims: TSetOfAnsiChar;
  AClearBeforeAdd: Boolean): IJclStringList;
var
  L, I, X: Integer;
begin
  AutoUpdateControl;
  if AClearBeforeAdd then
    Clear;
  I := 1;
  L := Length(AText);
  while I <= L do
  begin
    while (I <= L) and (AText[I] in ADelims) do
      Inc(I);
    X := I;
    while (I <= L) and not (AText[I] in ADelims) do
      Inc(I);
    if X <> I then
      Add(Copy(AText, X, I - X));
  end;
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.First: string;
begin
  Result := Strings[0];
end;

function TJclStringListImpl.Join(const ASeparator: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to LastIndex - 1 do
    Result := Result + Strings[I] + ASeparator;
  if Count > 0 then
    Result := Result + Last;
end;

function TJclStringListImpl.Last: string;
begin
  Result := Strings[LastIndex];
end;

function TJclStringListImpl.Split(const AText, ASeparator: string;
  AClearBeforeAdd: Boolean = True): IJclStringList;
var
  LStartIndex, LEndIndex: Integer;
  LLengthSeparator: Integer;
begin
  if AText <> '' then
  begin
    AutoUpdateControl;
    if AClearBeforeAdd then
      Clear;
    LLengthSeparator := Length(ASeparator);
    LStartIndex := 1;
    LEndIndex := StrSearch(ASeparator, AText, LStartIndex);
    while LEndIndex > 0 do
    begin
      Add(Copy(AText, LStartIndex, LEndIndex - LStartIndex));
      LStartIndex := LEndIndex + LLengthSeparator;
      LEndIndex := StrSearch(ASeparator, AText, LStartIndex);
    end;
    Add(Copy(AText, LStartIndex, MaxInt));
  end;
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.Trim: IJclStringList;
var
  I: Integer;
begin
  AutoUpdateControl;
  for I := 0 to LastIndex do
    Strings[I] := SysUtils.Trim(Strings[I]);
  Result := FSelfAsInterface;
end;

{$IFNDEF CLR}
function TJclStringListImpl.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TJclStringListImpl._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TJclStringListImpl._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 1 then
  begin
    // When there is only one reference, it is the internal reference,
    // so we release it. The compiler will call _Release again and
    // the object will be destroyed.
    FSelfAsInterface := nil;
  end
  else
  if Result = 0 then
    Destroy;
end;
{$ENDIF ~CLR}

function TJclStringListImpl.DeleteRegEx(const APattern: string): IJclStringList;
var
  I: Integer;
begin
  AutoUpdateControl;
  for I := LastIndex downto 0 do
    if MatchRegEx(Strings[I], APattern) then
      Delete(I);
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.KeepRegEx(const APattern: string): IJclStringList;
var
  I: Integer;
begin
  AutoUpdateControl;
  for I := LastIndex downto 0 do
    if not MatchRegEx(Strings[I], APattern) then
      Delete(I);
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.MatchRegEx(const S, APattern: string): Boolean;
begin
  {$IFDEF CLR}
  if CaseSensitive then
    FRegEx := System.Text.RegularExpressions.Regex.Create(APattern, RegexOptions.None)
  else
    FRegEx := System.Text.RegularExpressions.Regex.Create(APattern, RegexOptions.IgnoreCase);
  { TODO -oAHUser : I don't think this is correct }
  Result := FRegEx.IsMatch(S, APattern);
  {$ELSE}
  if FRegEx = nil then
    FRegEx := TJclAnsiRegEx.Create;
  if FLastRegExPattern <> APattern then
  begin
    {$IFDEF COMPILER6_UP}
    if CaseSensitive then
      FRegEx.Options := FRegEx.Options - [roIgnoreCase]
    else
      FRegEx.Options := FRegEx.Options + [roIgnoreCase];
    {$ENDIF COMPILER6_UP}
    FRegEx.Compile(APattern, False, True);
    FLastRegExPattern := APattern;
  end;
  Result := FRegEx.Match(S);
  {$ENDIF CLR}
end;

destructor TJclStringListImpl.Destroy;
begin
  if CanFreeObjects then
    FreeObjects(False);
  FreeAndNil(FUpdateControl);
  FreeAndNil(FRegEx);
  inherited Destroy;
end;

function TJclStringListImpl.Directories(const APattern: string = '*';
  ARecursive: Boolean = False; const ARegExPattern: string = ''): IJclStringList;

  procedure DoDirectories(const APattern: string);
  var
    LSearchRec: TSearchRec;
    LFullName: string;
    LPath: string;
  begin
    LPath := ExtractFilePath(APattern);
    if FindFirst(APattern, faAnyFile, LSearchRec) = 0 then
      try
        repeat
          if (LSearchRec.Attr and faDirectory = 0) or
             (LSearchRec.Name = '.') or (LSearchRec.Name = '..') then
            Continue;
          LFullName := LPath + LSearchRec.Name;
          if (ARegExPattern = '') or MatchRegEx(LFullName, ARegExPattern) then
            Add(LFullName);
          if ARecursive then
            DoDirectories(PathAddSeparator(LFullName) + ExtractFileName(APattern));
        until FindNext(LSearchRec) <> 0;
      finally
        FindClose(LSearchRec);
      end;
  end;

begin
  AutoUpdateControl;
  if DirectoryExists(APattern) then
    DoDirectories(PathAddSeparator(APattern) + '*')
  else
    DoDirectories(APattern);
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.Files(const APattern: string = '*';
  ARecursive: Boolean = False; const ARegExPattern: string = ''): IJclStringList;

  procedure DoFiles(const APattern: string);
  var
    LSearchRec: TSearchRec;
    LFullName: string;
    LDirectories: IJclStringList;
    LPath: string;
    I: Integer;
  begin
    LPath := ExtractFilePath(APattern);
    if FindFirst(APattern, faAnyFile and not faDirectory, LSearchRec) = 0 then
    begin
      try
        repeat
          if (LSearchRec.Attr and faDirectory <> 0) or
             (LSearchRec.Name = '.') or (LSearchRec.Name = '..') then
            Continue;
          LFullName := LPath + LSearchRec.Name;
          if (ARegExPattern = '') or MatchRegEx(LFullName, ARegExPattern) then
            Add(LFullName);
        until FindNext(LSearchRec) <> 0;
      finally
        FindClose(LSearchRec);
      end;
    end;
    if ARecursive then
    begin
      LDirectories := JclStringList.Directories(LPath + '*', False);
      for I := 0 to LDirectories.LastIndex do
        DoFiles(PathAddSeparator(LDirectories[I]) + ExtractFileName(APattern));
    end;
  end;

begin
  AutoUpdateControl;
  if DirectoryExists(APattern) then
    DoFiles(PathAddSeparator(APattern) + '*')
  else
    DoFiles(APattern);
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.LastIndex: Integer;
begin
  { The code bellow is more optimized than "Result := Count - 1". }
  Result := Count;
  Dec(Result);
end;

constructor TJclStringListImpl.Create;
begin
  inherited Create;
  FUpdateControl := TUpdateControl.Create(Self);
  {$IFDEF CLR}
  FSelfAsInterface := Self;
  {$ELSE}
  if QueryInterface(IJclStringList, FSelfAsInterface) <> 0 then
    {$IFDEF COMPILER5}
    RunError(228 { reIntfCastError });
    {$ELSE}
    System.Error(reIntfCastError);
    {$ENDIF COMPILER5}
  {$ENDIF CLR}
end;

function TJclStringListImpl.GetLists(Index: Integer): IJclStringList;
begin
  Result := Interfaces[Index] as IJclStringList;
  if Result = nil then
  begin
    Result := JclStringList;
    Interfaces[Index] := Result;
  end;
end;

procedure TJclStringListImpl.SetLists(Index: Integer; const Value: IJclStringList);
begin
  Interfaces[Index] := Value;
end;

function TJclStringListImpl.GetStringsRef: TStrings;
begin
  Result := Self;
end;

function TJclStringListImpl.GetKeyInterface(const AKey: string): IInterface;
var
  I: Integer;
begin
  I := IndexOf(AKey);
  if I >= 0 then
    Result := Interfaces[I]
  else
    Result := nil;
end;

function TJclStringListImpl.GetKeyObject(const AKey: string): TObject;
var
  I: Integer;
begin
  I := IndexOf(AKey);
  if I >= 0 then
    Result := Objects[I]
  else
    Result := nil;
end;

procedure TJclStringListImpl.SetKeyInterface(const AKey: string; const Value: IInterface);
var
  I: Integer;
begin
  I := IndexOf(AKey);
  if I < 0 then
    I := Add(AKey);
  Interfaces[I] := Value
end;

procedure TJclStringListImpl.SetKeyObject(const AKey: string; const Value: TObject);
var
  I: Integer;
begin
  I := IndexOf(AKey);
  if I < 0 then
    AddObject(AKey, Value)
  else
    Objects[I] := Value;
end;

function TJclStringListImpl.ConfigAsSet: IJclStringList;
begin
  Sorted := True;
  Duplicates := dupIgnore;
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.GetKeyVariant(const AKey: string): Variant;
var
  I: Integer;
begin
  I := IndexOf(AKey);
  if I >= 0 then
    Result := Variants[I]
  else
    Result := Unassigned;
end;

procedure TJclStringListImpl.SetKeyVariant(const AKey: string; const Value: Variant);
var
  I: Integer;
begin
  I := IndexOf(AKey);
  if I < 0 then
    I := Add(AKey);
  Variants[I] := Value
end;

function TJclStringListImpl.GetValue(const Name: string): string;
begin
  Result := inherited Values[Name];
end;

procedure TJclStringListImpl.SetValue(const Name, Value: string);
begin
  inherited Values[Name] := Value;
end;

function TJclStringListImpl.GetInterfaceByIndex(AIndex: Integer): IInterface;
var
  V: TInterfaceWrapper;
begin
  if FObjectsMode <> omInterfaces then
    EnsureObjectsMode(omInterfaces);
  V := TInterfaceWrapper(inherited Objects[AIndex]);
  if V = nil then
    Result := nil
  else
    Result := V.FValue;
end;

procedure TJclStringListImpl.SetInterfaceByIndex(Index: Integer; const Value: IInterface);
var
  V: TInterfaceWrapper;
begin
  if FObjectsMode <> omInterfaces then
    EnsureObjectsMode(omInterfaces);
  V := TInterfaceWrapper(inherited Objects[Index]);
  if V = nil then
  begin
    V := TInterfaceWrapper.Create;
    inherited Objects[Index] := V;
  end;
  V.FValue := Value;
end;

function TJclStringListImpl.GetObjects(Index: Integer): TObject;
begin
  if FObjectsMode <> omObjects then
    EnsureObjectsMode(omObjects);
  Result := inherited Objects[Index];
end;

procedure TJclStringListImpl.SetObjects(Index: Integer; const Value: TObject);
begin
  if FObjectsMode <> omObjects then
    EnsureObjectsMode(omObjects);
  inherited Objects[Index] := Value;
end;

function TJclStringListImpl.GetVariants(AIndex: Integer): Variant;
var
  V: TVariantWrapper;
begin
  if FObjectsMode <> omVariants then
    EnsureObjectsMode(omVariants);
  V := TVariantWrapper(inherited Objects[AIndex]);
  if V = nil then
    Result := Unassigned
  else
    Result := V.FValue;
end;

procedure TJclStringListImpl.SetVariants(Index: Integer; const Value: Variant);
var
  V: TVariantWrapper;
begin
  if FObjectsMode <> omVariants then
    EnsureObjectsMode(omVariants);
  V := TVariantWrapper(inherited Objects[Index]);
  if V = nil then
  begin
    V := TVariantWrapper.Create;
    inherited Objects[Index] := V;
  end;
  V.FValue := Value;
end;

procedure TJclStringListImpl.EnsureObjectsMode(AMode: TJclStringListObjectsMode);
begin
  if FObjectsMode <> AMode then
  begin
    if FObjectsMode <> omNone then
    begin
      raise Exception.CreateFmt('Objects cannot be used as "%s" because it has been used as "%s".',
        [GetEnumName(TypeInfo(TJclStringListObjectsMode), Ord(AMode)),
        GetEnumName(TypeInfo(TJclStringListObjectsMode), Ord(FObjectsMode))]);
    end;
    FObjectsMode := AMode;
  end;
end;

function TJclStringListImpl.GetKeyList(const AKey: string): IJclStringList;
begin
  Result := KeyInterface[AKey] as IJclStringList;
  if Result = nil then
  begin
    Result := JclStringList;
    KeyInterface[AKey] := Result;
  end;
end;

procedure TJclStringListImpl.SetKeyList(const AKey: string; const Value: IJclStringList);
begin
  KeyInterface[AKey] := Value;
end;

function TJclStringListImpl.Delete(AIndex: Integer): IJclStringList;
begin
  if CanFreeObjects then
    inherited Objects[AIndex].Free;
  inherited Delete(AIndex);
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.Delete(const AString: string): IJclStringList;
begin
  Result := Delete(IndexOf(AString));
end;

function TJclStringListImpl.Exchange(Index1, Index2: Integer): IJclStringList;
begin
  inherited Exchange(Index1, Index2);
  Result := FSelfAsInterface;
end;

{$IFDEF CLR}
function TJclStringListImpl_LocalSort(List: TStringList; Index1, Index2: Integer): Integer;
begin
  with TJclStringListImpl(List) do
    Result := FCustomSortCompare(FSelfAsInterface, Index1, Index2);
end;

function TJclStringListImpl.Sort(ACompareFunction: TJclStringListSortCompare = nil): IJclStringList;
begin
  if not Assigned(ACompareFunction) then
    inherited Sort
  else
  begin
    FCustomSortCompare := ACompareFunction;
    inherited CustomSort(TJclStringListImpl_LocalSort);
    FCustomSortCompare := nil;
  end;
  Result := FSelfAsInterface;
end;

function TJclStringListImpl_LocalSortAsInteger(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := StrToInt(List[Index1]) - StrToInt(List[Index2]);
end;

function TJclStringListImpl.SortAsInteger: IJclStringList;
begin
  inherited CustomSort(TJclStringListImpl_LocalSortAsInteger);
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.SortByNameCmp(Index1, Index2: Integer): Integer;
begin
  Result := CompareStrings(Names[Index1], Names[Index2]);
end;

function TJclStringListImpl_LocalSortByName(List: TStringList; Index1, Index2: Integer): Integer;
begin
  { It is not possible to call TStringList.CompareStrings from here because of
    assembly boundaries. } 
  Result := TJclStringListImpl(List).SortByNameCmp(Index1, Index2);
end;

function TJclStringListImpl.SortByName: IJclStringList;
begin
  inherited CustomSort(TJclStringListImpl_LocalSortByName);
  Result := FSelfAsInterface;
end;

{$ELSE} // CLR

function TJclStringListImpl.Sort(ACompareFunction: TJclStringListSortCompare = nil): IJclStringList;

  function LocalSort(List: TStringList; Index1, Index2: Integer): Integer;
  begin
    Result := ACompareFunction(FSelfAsInterface, Index1, Index2);
  end;

begin
  if not Assigned(ACompareFunction) then
    inherited Sort
  else
    inherited CustomSort(@LocalSort);
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.SortAsInteger: IJclStringList;

  function LocalSortAsInteger(List: TStringList; Index1, Index2: Integer): Integer;
  begin
    Result := StrToInt(List[Index1]) - StrToInt(List[Index2]);
  end;

begin
  inherited CustomSort(@LocalSortAsInteger);
  Result := FSelfAsInterface;
end;

{$IFNDEF HAS_TSTRINGS_COMPARESTRINGS}
function TJclStringListImpl.CompareStrings(const S1, S2: string): Integer;
begin
  Result := AnsiCompareText(S1, S2);
end;
{$ENDIF ~HAS_TSTRINGS_COMPARESTRINGS}

function TJclStringListImpl.SortByName: IJclStringList;

  function LocalSortByName(List: TStringList; Index1, Index2: Integer): Integer;
  begin
    Result := TJclStringListImpl(List).CompareStrings(List.Names[Index1], List.Names[Index2]);
  end;

begin
  inherited CustomSort(@LocalSortByName);
  Result := FSelfAsInterface;
end;
{$ENDIF CLR}

function TJclStringListImpl.Insert(Index: Integer; const S: string): IJclStringList;
begin
  inherited Insert(Index, S);
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.InsertObject(Index: Integer; const S: string; AObject: TObject): IJclStringList;
begin
  inherited InsertObject(Index, S, AObject);
  Result := FSelfAsInterface;
end;

{$IFDEF COMPILER6_UP}
function TJclStringListImpl.GetCaseSensitive: Boolean;
begin
  Result := inherited CaseSensitive;
end;
{$ENDIF COMPILER6_UP}

function TJclStringListImpl.GetDuplicates: TDuplicates;
begin
  Result := inherited Duplicates;
end;

function TJclStringListImpl.GetOnChange: TNotifyEvent;
begin
  Result := inherited OnChange;
end;

function TJclStringListImpl.GetOnChanging: TNotifyEvent;
begin
  Result := inherited OnChanging;
end;

function TJclStringListImpl.GetSorted: Boolean;
begin
  Result := inherited Sorted;
end;

{$IFDEF COMPILER6_UP}
procedure TJclStringListImpl.SetCaseSensitive(const Value: Boolean);
begin
  inherited CaseSensitive := Value;
end;
{$ENDIF COMPILER6_UP}

procedure TJclStringListImpl.SetDuplicates(const Value: TDuplicates);
begin
  inherited Duplicates := Value;
end;

procedure TJclStringListImpl.SetOnChange(const Value: TNotifyEvent);
begin
  inherited OnChange := Value;
end;

procedure TJclStringListImpl.SetOnChanging(const Value: TNotifyEvent);
begin
  inherited OnChanging := Value;
end;

procedure TJclStringListImpl.SetSorted(const Value: Boolean);
begin
  inherited Sorted := Value;
end;

function TJclStringListImpl.LoadFromFile(const FileName: string): IJclStringList;
begin
  inherited LoadFromFile(FileName);
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.LoadFromStream(Stream: TStream): IJclStringList;
begin
  inherited LoadFromStream(Stream);
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.SaveToFile(const FileName: string): IJclStringList;
begin
  inherited SaveToFile(FileName);
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.SaveToStream(Stream: TStream): IJclStringList;
begin
  inherited SaveToStream(Stream);
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.GetCommaText: string;
begin
  Result := inherited CommaText;
end;

{$IFDEF COMPILER6_UP}

function TJclStringListImpl.GetDelimitedText: string;
begin
  Result := inherited DelimitedText;
end;

function TJclStringListImpl.GetDelimiter: Char;
begin
  Result := inherited Delimiter;
end;

{$ENDIF COMPILER6_UP}

function TJclStringListImpl.GetName(Index: Integer): string;
begin
  Result := inherited Names[Index];
end;

{$IFDEF COMPILER7_UP}

function TJclStringListImpl.GetNameValueSeparator: Char;
begin
  Result := inherited NameValueSeparator;
end;

function TJclStringListImpl.GetValueFromIndex(Index: Integer): string;
begin
  Result := inherited ValueFromIndex[Index];
end;

{$ENDIF COMPILER7_UP}

{$IFDEF COMPILER6_UP}
function TJclStringListImpl.GetQuoteChar: Char;
begin
  Result := inherited QuoteChar;
end;
{$ENDIF COMPILER6_UP}

procedure TJclStringListImpl.SetCommaText(const Value: string);
begin
  inherited CommaText := Value;
end;

{$IFDEF COMPILER6_UP}

procedure TJclStringListImpl.SetDelimitedText(const Value: string);
begin
  inherited DelimitedText := Value;
end;

procedure TJclStringListImpl.SetDelimiter(const Value: Char);
begin
  inherited Delimiter := Value;
end;

{$ENDIF COMPILER6_UP}

{$IFDEF COMPILER7_UP}

procedure TJclStringListImpl.SetNameValueSeparator(const Value: Char);
begin
  inherited NameValueSeparator := Value;
end;

procedure TJclStringListImpl.SetValueFromIndex(Index: Integer; const Value: string);
begin
  inherited ValueFromIndex[Index] := Value;
end;

{$ENDIF COMPILER7_UP}

{$IFDEF COMPILER6_UP}
procedure TJclStringListImpl.SetQuoteChar(const Value: Char);
begin
  inherited QuoteChar := Value;
end;
{$ENDIF COMPILER6_UP}

function TJclStringListImpl.Delimit(const ADelimiter: string): IJclStringList;
var
  I: Integer;
begin
  AutoUpdateControl;
  for I := 0 to LastIndex do
    Strings[I] := ADelimiter + Strings[I] + ADelimiter;
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.LoadExeParams: IJclStringList;
var
  I: Integer;
  S: string;
begin
  AutoUpdateControl;
  Clear;
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if S[1] in ['-', '/'] then
      {$IFDEF CLR}Borland.Delphi.{$ENDIF}System.Delete(S, 1, 1);
    Add(S);
  end;
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.Exists(const S: string): Boolean;
begin
  Result := IndexOf(S) >= 0;
end;

function TJclStringListImpl.ExistsName(const S: string): Boolean;
begin
  Result := IndexOfName(S) >= 0;
end;

function TJclStringListImpl.DeleteBlanks: IJclStringList;
var
  I: Integer;
begin
  AutoUpdateControl;
  for I := LastIndex downto 0 do
    if SysUtils.Trim(Strings[I]) = '' then
      Delete(I);
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.KeepIntegers: IJclStringList;
var
  I, X: Integer;
begin
  AutoUpdateControl;
  for I := LastIndex downto 0 do
    if not TryStrToInt(Strings[I], X) then
      Delete(I);
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.DeleteIntegers: IJclStringList;
var
  I, X: Integer;
begin
  AutoUpdateControl;
  for I := LastIndex downto 0 do
    if TryStrToInt(Strings[I], X) then
      Delete(I);
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.FreeObjects(AFreeAndNil: Boolean = False): IJclStringList;
var
  I: Integer;
begin
  if AFreeAndNil then
    AutoUpdateControl;
  for I := 0 to LastIndex do
  begin
    inherited Objects[I].Free;
    if AFreeAndNil then
      inherited Objects[I] := nil;
  end;
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.ReleaseInterfaces: IJclStringList;
var
  I: Integer;
begin
  AutoUpdateControl;
  for I := 0 to LastIndex do
    Interfaces[I] := nil;
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.Clone: IJclStringList;
begin
  Result := JclStringList.Assign(Self);
end;

function TJclStringListImpl.Assign(Source: TPersistent): IJclStringList;
var
  L: TJclStringListImpl;
  I: Integer;
begin
  inherited Assign(Source);
  if Source is TJclStringListImpl then
  begin
    L := TJclStringListImpl(Source);
    FObjectsMode := L.FObjectsMode;
    if not (FObjectsMode in [omNone, omObjects]) then
    begin
      AutoUpdateControl;
      for I := 0 to LastIndex do
      begin
        inherited Objects[I] := nil;
        case FObjectsMode of
          omVariants:
            Variants[I] := L.Variants[I];
          omInterfaces:
            Interfaces[I] := L.Interfaces[I];
        end;
      end;
    end;
  end;
  Result := FSelfAsInterface;
end;

function TJclStringListImpl.CanFreeObjects: Boolean;
begin
  Result := not (FObjectsMode in [omNone, omObjects]);
end;

function TJclStringListImpl.GetObjectsMode: TJclStringListObjectsMode;
begin
  Result := FObjectsMode;
end;

//=== { TUpdateControl } =====================================================

constructor TUpdateControl.Create(AStrings: TStrings);
begin
  inherited Create;
  FStrings := AStrings;
end;

{$IFNDEF CLR}
function TUpdateControl._AddRef: Integer;
begin
  FStrings.BeginUpdate;
  Result := 0;
end;

function TUpdateControl._Release: Integer;
begin
  FStrings.EndUpdate;
  Result := 0;
end;

function TUpdateControl.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;
{$ENDIF ~CLR}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
