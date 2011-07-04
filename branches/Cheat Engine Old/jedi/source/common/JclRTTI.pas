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
{ The Original Code is JclRTTI.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel Bestebroer.                                 }
{ Portions created Marcel Bestebroer are Copyright (C) Marcel Bestebroer. All rights reserved.     }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Theo Bebekis                                                                                   }
{   Marcel Bestebroer (marcelb)                                                                    }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various RunTime Type Information routines. Includes retrieving RTTI information for different    }
{ types, declaring/generating new types, data conversion to user displayable values and 'is'/'as'  }
{ operator hooking.                                                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclRTTI;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_TYPES}
  Types,
  {$IFDEF CLR}
  System.Runtime.InteropServices, System.Reflection, System.ComponentModel,
  Variants,
  {$ELSE}
  {$IFDEF SUPPORTS_INLINE}
  Windows,
  {$ENDIF SUPPORTS_INLINE}
  {$ENDIF CLR}
  {$ELSE}
  Windows,
  {$ENDIF HAS_UNIT_TYPES}
  Classes, SysUtils, TypInfo,
  JclBase;

type
  // TypeInfo writing
  IJclInfoWriter = interface
    ['{7DAD522D-46EA-11D5-B0C0-4854E825F345}']
    function GetWrap: Integer;
    procedure SetWrap(const Value: Integer);
    procedure Write(const S: string);
    procedure Writeln(const S: string = '');
    procedure Indent;
    procedure Outdent;
    property Wrap: Integer read GetWrap write SetWrap;
  end;

  TJclInfoWriter = class(TInterfacedObject, IJclInfoWriter)
  private
    FCurLine: string;
    FIndentLevel: Integer;
    FWrap: Integer;
  protected
    function GetWrap: Integer;
    procedure SetWrap(const Value: Integer);
    procedure DoWrap;
    procedure DoWriteCompleteLines;
    procedure PrimWrite(const S: string); virtual; abstract;

    property CurLine: string read FCurLine write FCurLine;
    property IndentLevel: Integer read FIndentLevel write FIndentLevel;
  public
    constructor Create(const AWrap: Integer = 80);
    destructor Destroy; override;
    procedure Indent;
    procedure Outdent;
    procedure Write(const S: string);
    procedure Writeln(const S: string = '');

    property Wrap: Integer read GetWrap write SetWrap;
  end;

  TJclInfoStringsWriter = class(TJclInfoWriter)
  private
    FStrings: TStrings;
  protected
    procedure PrimWrite(const S: string); override;
  public
    constructor Create(const AStrings: TStrings; const AWrap: Integer = 80);

    property Strings: TStrings read FStrings;
  end;

  // TypeInfo retrieval
  IJclBaseInfo = interface
    procedure WriteTo(const Dest: IJclInfoWriter);
    procedure DeclarationTo(const Dest: IJclInfoWriter);
  end;

  IJclTypeInfo = interface(IJclBaseInfo)
    ['{7DAD5220-46EA-11D5-B0C0-4854E825F345}']
    function GetName: string;
    function GetTypeData: {$IFDEF CLR}TTypeData{$ELSE ~CLR}PTypeData{$ENDIF ~CLR};
    function GetTypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR};
    function GetTypeKind: TTypeKind;

    property Name: string read GetName;
    property TypeData: {$IFDEF CLR}TTypeData{$ELSE ~CLR}PTypeData{$ENDIF ~CLR} read GetTypeData;
    property TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR} read GetTypeInfo;
    property TypeKind: TTypeKind read GetTypeKind;
  end;

  // Ordinal types
  IJclOrdinalTypeInfo = interface(IJclTypeInfo)
    ['{7DAD5221-46EA-11D5-B0C0-4854E825F345}']
    function GetOrdinalType: TOrdType;

    property OrdinalType: TOrdType read GetOrdinalType;
  end;

  IJclOrdinalRangeTypeInfo = interface(IJclOrdinalTypeInfo)
    ['{7DAD5222-46EA-11D5-B0C0-4854E825F345}']
    function GetMinValue: Int64;
    function GetMaxValue: Int64;

    property MinValue: Int64 read GetMinValue;
    property MaxValue: Int64 read GetMaxValue;
  end;

  IJclEnumerationTypeInfo = interface(IJclOrdinalRangeTypeInfo)
    ['{7DAD5223-46EA-11D5-B0C0-4854E825F345}']
    function GetBaseType: IJclEnumerationTypeInfo;
    function GetNames(const I: Integer): string;
    {$IFDEF RTL140_UP}
    function GetUnitName: string;
    {$ENDIF RTL140_UP}

    function IndexOfName(const Name: string): Integer;

    property BaseType: IJclEnumerationTypeInfo read GetBaseType;
    property Names[const I: Integer]: string read GetNames; default;
    {$IFDEF RTL140_UP}
    property UnitName: string read GetUnitName;
    {$ENDIF RTL140_UP}
  end;

  IJclSetTypeInfo = interface(IJclOrdinalTypeInfo)
    ['{7DAD5224-46EA-11D5-B0C0-4854E825F345}']
    function GetBaseType: IJclOrdinalTypeInfo;

    procedure GetAsList(const Value;  const WantRanges: Boolean;
      const Strings: TStrings);
    procedure SetAsList(out Value; const Strings: TStrings);

    property BaseType: IJclOrdinalTypeInfo read GetBaseType;
  end;

  // Float types
  IJclFloatTypeInfo = interface(IJclTypeInfo)
    ['{7DAD5225-46EA-11D5-B0C0-4854E825F345}']
    function GetFloatType: TFloatType;

    property FloatType: TFloatType read GetFloatType;
  end;

  // Short string types
  IJclStringTypeInfo = interface(IJclTypeInfo)
    ['{7DAD5226-46EA-11D5-B0C0-4854E825F345}']
    function GetMaxLength: Integer;

    property MaxLength: Integer read GetMaxLength;
  end;

  // Class types
  TJclPropSpecKind = (pskNone, pskStaticMethod, pskVirtualMethod, pskField,
    pskConstant);

  IJclPropInfo = interface
    ['{7DAD5227-46EA-11D5-B0C0-4854E825F345}']
    function GetPropType: IJclTypeInfo;
    function GetReader: {$IFDEF CLR}MethodInfo{$ELSE}Pointer{$ENDIF};
    function GetWriter: {$IFDEF CLR}MethodInfo{$ELSE}Pointer{$ENDIF};
    function GetStoredProc: {$IFDEF CLR}MethodInfo{$ELSE}Pointer{$ENDIF};
    function GetIndex: Integer;
    function GetDefault: Longint;
    function GetNameIndex: Smallint;
    function GetName: string;
    function GetReaderType: TJclPropSpecKind;
    function GetWriterType: TJclPropSpecKind;
    function GetStoredType: TJclPropSpecKind;
    function GetReaderValue: Integer;
    function GetWriterValue: Integer;
    function GetStoredValue: Integer;

    function IsStored(const AInstance: TObject): Boolean;
    function HasDefault: Boolean;
    function HasIndex: Boolean;

    property PropType: IJclTypeInfo read GetPropType;
    property Reader: {$IFDEF CLR}MethodInfo{$ELSE}Pointer{$ENDIF} read GetReader;
    property Writer: {$IFDEF CLR}MethodInfo{$ELSE}Pointer{$ENDIF} read GetWriter;
    property StoredProc: {$IFDEF CLR}MethodInfo{$ELSE}Pointer{$ENDIF} read GetStoredProc;
    property ReaderType: TJclPropSpecKind read GetReaderType;
    property WriterType: TJclPropSpecKind read GetWriterType;
    property StoredType: TJclPropSpecKind read GetStoredType;
    property ReaderValue: Integer read GetReaderValue;
    property WriterValue: Integer read GetWriterValue;
    property StoredValue: Integer read GetStoredValue;
    property Index: Integer read GetIndex;
    property Default: Longint read GetDefault;
    property NameIndex: Smallint read GetNameIndex;
    property Name: string read GetName;
  end;

  IJclClassTypeInfo = interface(IJclTypeInfo)
    ['{7DAD5228-46EA-11D5-B0C0-4854E825F345}']
    function GetClassRef: TClass;
    function GetParent: IJclClassTypeInfo;
    function GetTotalPropertyCount: Integer;
    function GetPropertyCount: Integer;
    function GetProperties(const PropIdx: Integer): IJclPropInfo;
    function GetPropNames(const Name: string): IJclPropInfo;
    function GetUnitName: string;

    property ClassRef: TClass read GetClassRef;
    property Parent: IJclClassTypeInfo read GetParent;
    property TotalPropertyCount: Integer read GetTotalPropertyCount;
    property PropertyCount: Integer read GetPropertyCount;
    property Properties[const PropIdx: Integer]: IJclPropInfo read GetProperties;
    property PropNames[const Name: string]: IJclPropInfo read GetPropNames;
    property UnitName: string read GetUnitName;
  end;

  // Event types
  IJclEventParamInfo = interface
    ['{7DAD5229-46EA-11D5-B0C0-4854E825F345}']
    function GetFlags: TParamFlags;
    function GetName: string;
    {$IFNDEF CLR}
    function GetRecSize: Integer;
    {$ENDIF ~CLR}
    function GetTypeName: string;
    function GetParam: {$IFDEF CLR}ParameterInfo{$ELSE}Pointer{$ENDIF};

    property Flags: TParamFlags read GetFlags;
    property Name: string read GetName;
    {$IFNDEF CLR}
    property RecSize: Integer read GetRecSize;
    {$ENDIF ~CLR}
    property TypeName: string read GetTypeName;
    property Param: {$IFDEF CLR}ParameterInfo{$ELSE}Pointer{$ENDIF} read GetParam;
  end;

  IJclEventTypeInfo = interface(IJclTypeInfo)
    ['{7DAD522A-46EA-11D5-B0C0-4854E825F345}']
    function GetMethodKind: TMethodKind;
    function GetParameterCount: Integer;
    function GetParameters(const ParamIdx: Integer): IJclEventParamInfo;
    function GetResultTypeName: string;

    property MethodKind: TMethodKind read GetMethodKind;
    property ParameterCount: Integer read GetParameterCount;
    property Parameters[const ParamIdx: Integer]: IJclEventParamInfo
      read GetParameters;
    property ResultTypeName: string read GetResultTypeName;
  end;

  // Interface types
  IJclInterfaceTypeInfo = interface(IJclTypeInfo)
    ['{7DAD522B-46EA-11D5-B0C0-4854E825F345}']
    function GetParent: IJclInterfaceTypeInfo;
    function GetFlags: TIntfFlagsBase;
    function GetGUID: TGUID;
    {$IFDEF RTL140_UP}
    function GetPropertyCount: Integer;
    {$ENDIF RTL140_UP}
    function GetUnitName: string;

    property Parent: IJclInterfaceTypeInfo read GetParent;
    property Flags: TIntfFlagsBase read GetFlags;
    property GUID: TGUID read GetGUID;
    {$IFDEF RTL140_UP}
    property PropertyCount: Integer read GetPropertyCount;
    {$ENDIF RTL140_UP}
    property UnitName: string read GetUnitName;
  end;

  // Int64 types
  IJclInt64TypeInfo = interface(IJclTypeInfo)
    ['{7DAD522C-46EA-11D5-B0C0-4854E825F345}']
    function GetMinValue: Int64;
    function GetMaxValue: Int64;

    property MinValue: Int64 read GetMinValue;
    property MaxValue: Int64 read GetMaxValue;
  end;

  {$IFDEF RTL140_UP}
  // Dynamic array types
  IJclDynArrayTypeInfo = interface(IJclTypeInfo)
    ['{7DAD522E-46EA-11D5-B0C0-4854E825F345}']
    function GetElementSize: Longint;
    function GetElementType: IJclTypeInfo;
    function GetElementsNeedCleanup: Boolean;
    function GetVarType: Integer;
    function GetUnitName: string;

    property ElementSize: Longint read GetElementSize;
    property ElementType: IJclTypeInfo read GetElementType;
    property ElementsNeedCleanup: Boolean read GetElementsNeedCleanup;
    property VarType: Integer read GetVarType;
    property UnitName: string read GetUnitName;
  end;
  {$ENDIF RTL140_UP}

  EJclRTTIError = class(EJclError);

function JclTypeInfo(ATypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR}): IJclTypeInfo;

// Enumeration types
const
  PREFIX_CUT_LOWERCASE = 255;
  PREFIX_CUT_EQUAL     = 254;

  MaxPrefixCut = 250;

function JclEnumValueToIdent(TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR};
  const Value): string;
{$IFNDEF CLR}
function JclGenerateEnumType(const TypeName: ShortString;
  const Literals: array of string): PTypeInfo;
function JclGenerateEnumTypeBasedOn(const TypeName: ShortString;
  BaseType: PTypeInfo; const PrefixCut: Byte): PTypeInfo;
function JclGenerateSubRange(BaseType: PTypeInfo; const TypeName: string;
  const MinValue, MaxValue: Integer): PTypeInfo;
{$ENDIF ~CLR}

// Integer types
function JclStrToTypedInt(Value: string; TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR}): Integer;
function JclTypedIntToStr(Value: Integer; TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR}): string;

// Sets
function JclSetToList(TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR};
  const Value; const WantBrackets: Boolean; const WantRanges: Boolean; const Strings: TStrings): string;
function JclSetToStr(TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR};
  const Value; const WantBrackets: Boolean = False; const WantRanges: Boolean = False): string;
procedure JclStrToSet(TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR};
  var SetVar; const Value: string);
procedure JclIntToSet(TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR};
  var SetVar; const Value: Integer);
function JclSetToInt(TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR};
  const SetVar): Integer;
{$IFNDEF CLR}
function JclGenerateSetType(BaseType: PTypeInfo; const TypeName: ShortString): PTypeInfo;
{$ENDIF ~CLR}

{$IFNDEF CLR}
// User generated type info managment
procedure RemoveTypeInfo(TypeInfo: PTypeInfo);
{$ENDIF ~CLR}

// Is/As hooking
function JclIsClass(const AnObj: TObject; const AClass: TClass): Boolean;
function JclIsClassByName(const AnObj: TObject; const AClass: TClass): Boolean;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclRTTI.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNIT_RTLCONSTS}
  RtlConsts,
  {$ENDIF HAS_UNIT_RTLCONSTS}
  SysConst,
  JclLogic, JclResources, JclStrings, JclSysUtils;

//=== { TJclInfoWriter } =====================================================

constructor TJclInfoWriter.Create(const AWrap: Integer);
begin
  inherited Create;
  Wrap := AWrap;
end;

destructor TJclInfoWriter.Destroy;
begin
  if CurLine <> '' then
    Writeln('');
  inherited Destroy;
end;

function TJclInfoWriter.GetWrap: Integer;
begin
  Result := FWrap;
end;

procedure TJclInfoWriter.SetWrap(const Value: Integer);
begin
  FWrap := Value;
end;

procedure TJclInfoWriter.DoWrap;
const
{$IFDEF CLR}
  WrapChars: array[0..33] of Char = (
     #0, #1, #2, #3, #4, #5, #6, #7, #8, #9, #10, #11, #12, #13, #14, #15,
     #16, #17, #18, #19, #20, #21, #22, #23, #24, #25, #26, #27, #28, #29,
     #30, #31, #32, '-');
{$ELSE}
  WrapChars = [#0..' ', '-'];
{$ENDIF CLR}
var
  TmpLines: TStringList;
  I: Integer;
  TmpLines2: TStringList;
  EndedInCRLF: Boolean;
  LineBreakLength: Integer;
begin
  LineBreakLength := Length(AnsiLineBreak);
  EndedInCRLF := Copy(CurLine, Length(CurLine) - LineBreakLength + 1, LineBreakLength) = AnsiLineBreak;
  TmpLines := TStringList.Create;
  try
    TmpLines.Text := CurLine;
    TmpLines2 := TStringList.Create;
    try
      I := TmpLines.Count-1;
      if not EndedInCRLF then
        Dec(I);
      while I >= 0 do
      begin
        TmpLines[I] := StringOfChar(' ', 2 * IndentLevel) + TmpLines[I];
        if (Wrap > 0) and (Length(TmpLines[I]) > Wrap) then
        begin
          TmpLines2.Text := WrapText(
            TmpLines[I],
            AnsiLineBreak + StringOfChar(' ', 2 * (IndentLevel+1)),
            WrapChars,
            Wrap);
          TmpLines.Delete(I);
          TmpLines.Insert(I, Copy(TmpLines2.Text, 1,
            Length(TmpLines2.Text) - 2));
        end;
        Dec(I);
      end;
      CurLine := TmpLines.Text;
      if not EndedInCRLF then
        Delete(FCurLine, Length(FCurLine) - LineBreakLength + 1, LineBreakLength);
    finally
      TmpLines2.Free;
    end;
  finally
    TmpLines.Free;
  end;
end;

procedure TJclInfoWriter.DoWriteCompleteLines;
var
  CRLFPos: Integer;
begin
  CRLFPos := StrLastPos(AnsiLineBreak, CurLine);
  if CRLFPos > 0 then
  begin
    PrimWrite(Copy(CurLine, 1, CRLFPos-1));
    Delete(FCurLine, 1, CRLFPos+1);
  end;
end;

procedure TJclInfoWriter.Indent;
begin
  IndentLevel := IndentLevel + 1;
end;

procedure TJclInfoWriter.Outdent;
begin
  IndentLevel := IndentLevel - 1;
end;

procedure TJclInfoWriter.Write(const S: string);
begin
  CurLine := CurLine + S;
  DoWrap;
  DoWriteCompleteLines;
end;

procedure TJclInfoWriter.Writeln(const S: string);
begin
  Write(S + AnsiLineBreak);
end;

//=== { TJclInfoStringsWriter } ==============================================

constructor TJclInfoStringsWriter.Create(const AStrings: TStrings;
  const AWrap: Integer);
begin
  inherited Create(AWrap);
  FStrings := AStrings;
end;

procedure TJclInfoStringsWriter.PrimWrite(const S: string);
begin
  Strings.Add(S);
end;

//=== { TJclTypeInfo } =======================================================

type
  TJclTypeInfo = class(TInterfacedObject, IJclTypeInfo)
  private
    FTypeData: {$IFDEF CLR}TTypeData{$ELSE ~CLR}PTypeData{$ENDIF ~CLR};
    FTypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR};
  protected
    function GetName: string;
    function GetTypeData: {$IFDEF CLR}TTypeData{$ELSE ~CLR}PTypeData{$ENDIF ~CLR};
    function GetTypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR};
    function GetTypeKind: TTypeKind;
    procedure WriteTo(const Dest: IJclInfoWriter); virtual;
    procedure DeclarationTo(const Dest: IJclInfoWriter); virtual;
  public
    constructor Create(ATypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR});
    property Name: string read GetName;
    property TypeData: {$IFDEF CLR}TTypeData{$ELSE ~CLR}PTypeData{$ENDIF ~CLR} read GetTypeData;
    property TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR} read GetTypeInfo;
    property TypeKind: TTypeKind read GetTypeKind;
  end;

constructor TJclTypeInfo.Create(ATypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR});
begin
  inherited Create;
  FTypeInfo := ATypeInfo;
  FTypeData := TypInfo.GetTypeData(ATypeInfo);
end;

function TJclTypeInfo.GetName: string;
begin
  Result := TypeInfo.Name;
end;

function TJclTypeInfo.GetTypeData: {$IFDEF CLR}TTypeData{$ELSE ~CLR}PTypeData{$ENDIF ~CLR};
begin
  Result := FTypeData;
end;

function TJclTypeInfo.GetTypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR};
begin
  Result := FTypeInfo;
end;

function TJclTypeInfo.GetTypeKind: TTypeKind;
begin
  Result := {$IFDEF CLR}TypeInfo.TypeKind{$ELSE ~CLR}TypeInfo.Kind{$ENDIF ~CLR};
end;

procedure TJclTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  {$IFDEF CLR}
  Dest.Writeln(RsRTTIName + Name);
  Dest.Writeln(RsRTTITypeKind + JclEnumValueToIdent(Borland.Delphi.System.TypeInfo(TTypeKind),
    TypeInfo.TypeKind));
  Dest.Writeln(Format(RsRTTITypeInfoAt, [TypeInfo]));
  {$ELSE}
  Dest.Writeln(LoadResString(@RsRTTIName) + Name);
  Dest.Writeln(LoadResString(@RsRTTITypeKind) + JclEnumValueToIdent(System.TypeInfo(TTypeKind),
    TypeInfo.Kind));
  Dest.Writeln(Format(LoadResString(@RsRTTITypeInfoAt), [TypeInfo]));
  {$ENDIF CLR}
end;

procedure TJclTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
begin
  {$IFDEF CLR}
  Dest.Write(Format(RsDeclarationFormat, [Name]));
  {$ELSE}
  Dest.Write(Format(LoadResString(@RsDeclarationFormat), [Name]));
  {$ENDIF CLR}
end;

//=== { TJclOrdinalTypeInfo } ================================================

type
  TJclOrdinalTypeInfo = class(TJclTypeInfo, IJclOrdinalTypeInfo)
  protected
    function GetOrdinalType: TOrdType;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
  public
    property OrdinalType: TOrdType read GetOrdinalType;
  end;

function TJclOrdinalTypeInfo.GetOrdinalType: TOrdType;
begin
  Result := TypeData.OrdType;
end;

procedure TJclOrdinalTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  {$IFDEF CLR}
  Dest.Writeln(RsRTTIOrdinalType +
    JclEnumValueToIdent(Borland.Delphi.System.TypeInfo(TOrdType), TypeData.OrdType));
  {$ELSE}
  Dest.Writeln(LoadResString(@RsRTTIOrdinalType) +
    JclEnumValueToIdent(System.TypeInfo(TOrdType), TypeData.OrdType));
  {$ENDIF CLR}
end;

//=== { TJclOrdinalRangeTypeInfo } ===========================================

type
  TJclOrdinalRangeTypeInfo = class(TJclOrdinalTypeInfo, IJclOrdinalRangeTypeInfo)
  protected
    function GetMinValue: Int64;
    function GetMaxValue: Int64;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property MinValue: Int64 read GetMinValue;
    property MaxValue: Int64 read GetMaxValue;
  end;

function TJclOrdinalRangeTypeInfo.GetMinValue: Int64;
begin
  if OrdinalType = otULong then
    Result := Longword(TypeData.MinValue)
  else
    Result := TypeData.MinValue;
end;

function TJclOrdinalRangeTypeInfo.GetMaxValue: Int64;
begin
  if OrdinalType = otULong then
    Result := Longword(TypeData.MaxValue)
  else
    Result := TypeData.MaxValue;
end;

procedure TJclOrdinalRangeTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  {$IFDEF CLR}
  Dest.Writeln(RsRTTIMinValue + IntToStr(MinValue));
  Dest.Writeln(RsRTTIMaxValue + IntToStr(MaxValue));
  {$ELSE}
  Dest.Writeln(LoadResString(@RsRTTIMinValue) + IntToStr(MinValue));
  Dest.Writeln(LoadResString(@RsRTTIMaxValue) + IntToStr(MaxValue));
  {$ENDIF CLR}
end;

procedure TJclOrdinalRangeTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
const
  cRange = '..';
begin
  Dest.Write(Name + ' = ');
  {$IFDEF CLR}
  if TypeInfo.TypeKind in [tkChar, tkWChar] then
  {$ELSE ~CLR}
  if TypeInfo.Kind in [tkChar, tkWChar] then
  {$ENDIF ~CLR}
  begin
    if (MinValue < Ord(' ')) or (MinValue > Ord('~')) then
      Dest.Write('#' + IntToStr(MinValue) + cRange)
    else
      Dest.Write('''' + Chr(Byte(MinValue)) + '''' + cRange);
    if (MaxValue < Ord(' ')) or (MaxValue > Ord('~')) then
      Dest.Write('#' + IntToStr(MaxValue))
    else
      Dest.Write('''' + Chr(Byte(MaxValue)) + '''');
  end
  else
    Dest.Write(IntToStr(MinValue) + '..' + IntToStr(MaxValue));
  {$IFDEF CLR}
  Dest.Writeln('; // ' + JclEnumValueToIdent(Borland.Delphi.System.TypeInfo(TOrdType), TypeData.OrdType));
  {$ELSE}
  Dest.Writeln('; // ' + JclEnumValueToIdent(System.TypeInfo(TOrdType), TypeData.OrdType));
  {$ENDIF CLR}
end;

//=== { TJclEnumerationTypeInfo } ============================================

type
  TJclEnumerationTypeInfo = class(TJclOrdinalRangeTypeInfo, IJclEnumerationTypeInfo)
  protected
    function GetBaseType: IJclEnumerationTypeInfo;
    function GetNames(const I: Integer): string;
    {$IFDEF RTL140_UP}
    function GetUnitName: string;
    {$ENDIF RTL140_UP}
    function IndexOfName(const Name: string): Integer;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property BaseType: IJclEnumerationTypeInfo read GetBaseType;
    property Names[const I: Integer]: string read GetNames; default;
    {$IFDEF RTL140_UP}
    property UnitName: string read GetUnitName;
    {$ENDIF RTL140_UP}
  end;

function TJclEnumerationTypeInfo.GetBaseType: IJclEnumerationTypeInfo;
begin
  {$IFDEF CLR}
  if TypeData.ParentInfo = TypeInfo then
    Result := Self
  else
    Result := TJclEnumerationTypeInfo.Create(TypeData.ParentInfo);
  {$ELSE}
  if TypeData.BaseType^ = TypeInfo then
    Result := Self
  else
    Result := TJclEnumerationTypeInfo.Create(TypeData.BaseType^);
  {$ENDIF CLR}
end;

function TJclEnumerationTypeInfo.GetNames(const I: Integer): string;
var
  Base: IJclEnumerationTypeInfo;
  {$IFNDEF CLR}
  Idx: Integer;
  P: ^ShortString;
  {$ENDIF ~CLR}
begin
  Base := BaseType;
  {$IFDEF CLR}
  if (I >= 0) and (I < Length(Enum.GetNames(Base.TypeInfo))) then
    Result := Enum.GetNames(Base.TypeInfo)[I]
  else
    Result := '';
  {$ELSE}
  Idx := I;
  P := @Base.TypeData.NameList;
  while Idx <> 0 do
  begin
    Inc(Integer(P), Length(P^) + 1);
    Dec(Idx);
  end;
  Result := P^;
  {$ENDIF CLR}
end;

{$IFDEF RTL140_UP}

function TJclEnumerationTypeInfo.GetUnitName: string;
{$IFDEF CLR}
begin
  Result := BaseType.TypeData.EnumUnitName;
end;
{$ELSE}
var
  I: Integer;
  P: ^ShortString;
begin
  if BaseType.TypeInfo = TypeInfo then
  begin
    I := MaxValue - MinValue;
    P := @TypeData.NameList;
    while I >= 0 do
    begin
      Inc(Integer(P), Length(P^) + 1);
      Dec(I);
    end;
    Result := P^;
  end
  else
    Result := TypeData.NameList;
end;
{$ENDIF CLR}

{$ENDIF RTL140_UP}

function TJclEnumerationTypeInfo.IndexOfName(const Name: string): Integer;
begin
  Result := MaxValue;
  while (Result >= MinValue) and
        {$IFDEF CLR}
        not SameText(Name, Names[Result]) do
        {$ELSE}
        not AnsiSameText(Name, Names[Result]) do
        {$ENDIF CLR}
    Dec(Result);
  if Result < MinValue then
    Result := -1;
end;

procedure TJclEnumerationTypeInfo.WriteTo(const Dest: IJclInfoWriter);
var
  Idx: Integer;
  Prefix: string;
begin
  inherited WriteTo(Dest);
  {$IFDEF CLR}
  Dest.Writeln(RsRTTIUnitName + UnitName);
  Dest.Write(RsRTTINameList);
  {$ELSE}
  {$IFDEF RTL140_UP}
  Dest.Writeln(LoadResString(@RsRTTIUnitName) + UnitName);
  {$ENDIF RTL140_UP}
  Dest.Write(LoadResString(@RsRTTINameList));
  {$ENDIF CLR}
  Prefix := '(';
  for Idx := MinValue to MaxValue do
  begin
    Dest.Write(Prefix + Names[Idx]);
    Prefix := ', ';
  end;
  Dest.Writeln(')');
end;

procedure TJclEnumerationTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
var
  Prefix: string;
  I: Integer;
begin
  if Name[1] <> '.' then
    Dest.Write(Name + ' = ');
  if BaseType.TypeInfo = TypeInfo then
  begin
    Dest.Write('(');
    Prefix := '';
    for I := MinValue to MaxValue do
    begin
      Dest.Write(Prefix + Names[I]);
      Prefix := ', ';
    end;
    Dest.Write(')');
  end
  else
    Dest.Write(Names[MinValue] + ' .. ' + Names[MaxValue]);
  if Name[1] <> '.' then
  begin
    {$IFDEF CLR}
    Dest.Write('; // ' + JclEnumValueToIdent(Borland.Delphi.System.TypeInfo(TOrdType), TypeData.OrdType));
    {$ELSE}
    Dest.Write('; // ' + JclEnumValueToIdent(System.TypeInfo(TOrdType), TypeData.OrdType));
    {$ENDIF CLR}
    Dest.Writeln('');
  end;
end;

//=== { TJclSetTypeInfo } ====================================================

type
  TJclSetTypeInfo = class(TJclOrdinalTypeInfo, IJclSetTypeInfo)
  protected
    function GetBaseType: IJclOrdinalTypeInfo;
    procedure GetAsList(const Value; const WantRanges: Boolean;
      const Strings: TStrings);
    procedure SetAsList(out Value; const Strings: TStrings);
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property BaseType: IJclOrdinalTypeInfo read GetBaseType;
  end;

function TJclSetTypeInfo.GetBaseType: IJclOrdinalTypeInfo;
begin
  {$IFDEF CLR}
  Result := JclTypeInfo(TypeData.CompType) as IJclOrdinalTypeInfo;
  {$ELSE}
  Result := JclTypeInfo(TypeData.CompType^) as IJclOrdinalTypeInfo;
  {$ENDIF CLR}
end;

procedure TJclSetTypeInfo.GetAsList(const Value; const WantRanges: Boolean;
  const Strings: TStrings);
var
  BaseInfo: IJclOrdinalRangeTypeInfo;
  FirstBit: Byte;
  LastBit: Byte;
  Bit: Byte;
  StartBit: Integer;

  procedure AddRange;
  var
    FirstOrdNum: Int64;
    LastOrdNum: Int64;
    OrdNum: Int64;
  begin
    FirstOrdNum := (StartBit - FirstBit) + BaseInfo.MinValue;
    LastOrdNum := (Bit - 1 - FirstBit) + BaseInfo.MinValue;
    if WantRanges and (LastOrdNum <> FirstOrdNum) then
    begin
      if BaseInfo.TypeKind = tkEnumeration then
        Strings.Add((BaseInfo as IJclEnumerationTypeInfo).Names[FirstOrdNum] +
          ' .. ' + (BaseInfo as IJclEnumerationTypeInfo).Names[LastOrdNum])
      else
        Strings.Add(IntToStr(FirstOrdNum) + ' .. ' + IntToStr(LastOrdNum));
    end
    else
    begin
      OrdNum := FirstOrdNum;
      while OrdNum <= LastOrdNum do
      begin
        if BaseInfo.TypeKind = tkEnumeration then
          Strings.Add((BaseInfo as IJclEnumerationTypeInfo).Names[OrdNum])
        else
          Strings.Add(IntToStr(OrdNum));
        Inc(OrdNum);
      end;
    end;
  end;

begin
  BaseInfo := BaseType as IJclOrdinalRangeTypeInfo;
  FirstBit := BaseInfo.MinValue mod 8;
  LastBit := BaseInfo.MaxValue - (BaseInfo.MinValue - FirstBit);
  Bit := FirstBit;
  StartBit := -1;
  Strings.BeginUpdate;
  try
    while Bit <= LastBit do
    begin
      if TestBitBuffer(Value, Bit) then
      begin
        if StartBit = -1 then
          StartBit := Bit;
      end
      else
      begin
        if StartBit <> -1 then
        begin
          AddRange;
          StartBit := -1;
        end;
      end;
      Inc(Bit);
    end;
    if StartBit <> -1 then
      AddRange;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJclSetTypeInfo.SetAsList(out Value; const Strings: TStrings);
var
  BaseInfo: IJclOrdinalRangeTypeInfo;
  FirstBit: Integer;
  I: Integer;
  FirstIdent: string;
  LastIdent: string;
  RangePos: Integer;
  FirstOrd: Int64;
  LastOrd: Int64;
  CurOrd: Integer;

  procedure ClearValue;
  var
    LastBit: Integer;
    ByteCount: Integer;
  begin
    LastBit := BaseInfo.MaxValue - BaseInfo.MinValue + 1 + FirstBit;
    ByteCount := (LastBit - FirstBit) div 8;
    if LastBit mod 8 <> 0 then
      Inc(ByteCount);
    {$IFDEF CLR}
    // "set of" is a "array of Byte"
    while ByteCount > 0 do
      TDynByteArray(Value)[ByteCount - 1] := 0;
    {$ELSE}
    FillChar(Value, ByteCount, 0);
    {$ENDIF CLR}
  end;

begin
  BaseInfo := BaseType as IJclOrdinalRangeTypeInfo;
  FirstBit := BaseInfo.MinValue mod 8;
  ClearValue;
  Strings.BeginUpdate;
  try
  for I := 0 to Strings.Count - 1 do
    begin
      if Trim(Strings[I]) <> '' then
      begin
        FirstIdent := Trim(Strings[I]);
        RangePos := Pos('..', FirstIdent);
        if RangePos > 0 then
        begin
          LastIdent := Trim(StrRestOf(FirstIdent, RangePos + 2));
          FirstIdent := Trim(Copy(FirstIdent, 1, RangePos - 1));
        end
        else
          LastIdent := FirstIdent;
        if BaseInfo.TypeKind = tkEnumeration then
        begin
          FirstOrd := (BaseInfo as IJclEnumerationTypeInfo).IndexOfName(FirstIdent);
          LastOrd := (BaseInfo as IJclEnumerationTypeInfo).IndexOfName(LastIdent);
          {$IFDEF CLR}
          if FirstOrd = -1 then
            raise EJclRTTIError.CreateFmt(RsRTTIUnknownIdentifier, [FirstIdent]);
          if LastOrd = -1 then
            raise EJclRTTIError.CreateFmt(RsRTTIUnknownIdentifier, [LastIdent]);
          {$ELSE}
          if FirstOrd = -1 then
            raise EJclRTTIError.CreateResFmt(@RsRTTIUnknownIdentifier, [FirstIdent]);
          if LastOrd = -1 then
            raise EJclRTTIError.CreateResFmt(@RsRTTIUnknownIdentifier, [LastIdent]);
          {$ENDIF CLR}
        end
        else
        begin
          FirstOrd := StrToInt(FirstIdent);
          LastOrd := StrToInt(LastIdent);
        end;
        Dec(FirstOrd, BaseInfo.MinValue);
        Dec(LastOrd, BaseInfo.MinValue);
        for CurOrd := FirstOrd to LastOrd do
          SetBitBuffer(Value, CurOrd + FirstBit);
      end;
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJclSetTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  {$IFDEF CLR}
  Dest.Writeln(RsRTTIBasedOn);
  {$ELSE}
  Dest.Writeln(LoadResString(@RsRTTIBasedOn));
  {$ENDIF CLR}
  Dest.Indent;
  try
    BaseType.WriteTo(Dest);
  finally
    Dest.Outdent;
  end;
end;

procedure TJclSetTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
var
  Base: IJclOrdinalTypeInfo;
  BaseEnum: IJclEnumerationTypeInfo;
begin
  if Name[1] <> '.' then
    Dest.Write(Name + ' = set of ');
  Base := BaseType;

  if Base.Name[1] = '.' then
  begin
    {$IFDEF CLR}
    if Supports(Base, IJclEnumerationTypeInfo, BaseEnum) then
      BaseEnum.DeclarationTo(Dest)
    else
      Dest.Write(RsRTTITypeError);
    {$ELSE}
    if Base.QueryInterface(IJclEnumerationTypeInfo, BaseEnum) = S_OK then
      BaseEnum.DeclarationTo(Dest)
    else
      Dest.Write(LoadResString(@RsRTTITypeError));
    {$ENDIF CLR}
  end
  else
    Dest.Write(Base.Name);
  if Name[1] <> '.' then
  begin
    {$IFDEF CLR}
    Dest.Write('; // ' + JclEnumValueToIdent(Borland.Delphi.System.TypeInfo(TOrdType), TypeData.OrdType));
    {$ELSE}
    Dest.Write('; // ' + JclEnumValueToIdent(System.TypeInfo(TOrdType), TypeData.OrdType));
    {$ENDIF CLR}
    Dest.Writeln('');
  end;
end;

//=== { TJclFloatTypeInfo } ==================================================

type
  TJclFloatTypeInfo = class(TJclTypeInfo, IJclFloatTypeInfo)
  protected
    function GetFloatType: TFloatType;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property FloatType: TFloatType read GetFloatType;
  end;

function TJclFloatTypeInfo.GetFloatType: TFloatType;
begin
  Result := TypeData.FloatType;
end;

procedure TJclFloatTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  {$IFDEF CLR}
  Dest.Writeln(RsRTTIFloatType +
    JclEnumValueToIdent(Borland.Delphi.System.TypeInfo(TFloatType), TypeData.FloatType));
  {$ELSE}
  Dest.Writeln(LoadResString(@RsRTTIFloatType) +
    JclEnumValueToIdent(System.TypeInfo(TFloatType), TypeData.FloatType));
  {$ENDIF CLR}
end;

procedure TJclFloatTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
var
  S: string;
  FT: TFloatType;
begin
  FT := FloatType;
  {$IFDEF CLR}
  S := StrRestOf(JclEnumValueToIdent(Borland.Delphi.System.TypeInfo(TFloatType), FT), 3);
  {$ELSE}
  S := StrRestOf(JclEnumValueToIdent(System.TypeInfo(TFloatType), FT), 3);
  {$ENDIF CLR}
  Dest.Writeln(Name + ' = type ' + S + ';');
end;

//=== { TJclStringTypeInfo } =================================================

type
  TJclStringTypeInfo = class(TJclTypeInfo, IJclStringTypeInfo)
  protected
    function GetMaxLength: Integer;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property MaxLength: Integer read GetMaxLength;
  end;

function TJclStringTypeInfo.GetMaxLength: Integer;
begin
  Result := TypeData.MaxLength;
end;

procedure TJclStringTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  {$IFDEF CLR}
  Dest.Writeln(RsRTTIMaxLen + IntToStr(MaxLength));
  {$ELSE}
  Dest.Writeln(LoadResString(@RsRTTIMaxLen) + IntToStr(MaxLength));
  {$ENDIF CLR}
end;

procedure TJclStringTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
begin
  if Name[1] <> '.' then
    Dest.Write(Name + ' = ');
  Dest.Write('string[' + IntToStr(MaxLength) + ']');
  if Name[1] <> '.' then
    Dest.Writeln(';');
end;

//=== { TJclPropInfo } =======================================================

type
  TJclPropInfo = class(TInterfacedObject, IJclPropInfo)
  private
    FPropInfo: {$IFDEF CLR}TPropInfo{$ELSE ~CLR}PPropInfo{$ENDIF ~CLR};
  protected
    function GetPropInfo: {$IFDEF CLR}TPropInfo{$ELSE ~CLR}PPropInfo{$ENDIF ~CLR};
    function GetPropType: IJclTypeInfo;
    function GetReader: {$IFDEF CLR}MethodInfo{$ELSE}Pointer{$ENDIF};
    function GetWriter: {$IFDEF CLR}MethodInfo{$ELSE}Pointer{$ENDIF};
    function GetStoredProc: {$IFDEF CLR}MethodInfo{$ELSE}Pointer{$ENDIF};
    function GetIndex: Integer;
    function GetDefault: Longint;
    function GetNameIndex: Smallint;
    function GetName: string;
    {$IFDEF CLR}
    function MethodInfoToPropSecpKind(Info: MethodInfo): TJclPropSpecKind;
    {$ENDIF CLR}
    function GetSpecKind(const Value: Integer): TJclPropSpecKind;
    function GetSpecValue(const Value: Integer): Integer;
    function GetReaderType: TJclPropSpecKind;
    function GetWriterType: TJclPropSpecKind;
    function GetStoredType: TJclPropSpecKind;
    function GetReaderValue: Integer;
    function GetWriterValue: Integer;
    function GetStoredValue: Integer;
  public
    constructor Create(const APropInfo: {$IFDEF CLR}TPropInfo{$ELSE ~CLR}PPropInfo{$ENDIF ~CLR});
    function IsStored(const AInstance: TObject): Boolean;
    function HasDefault: Boolean;
    function HasIndex: Boolean;

    property PropInfo: {$IFDEF CLR}TPropInfo{$ELSE ~CLR}PPropInfo{$ENDIF ~CLR} read GetPropInfo;
    property PropType: IJclTypeInfo read GetPropType;
    property Reader: {$IFDEF CLR}MethodInfo{$ELSE}Pointer{$ENDIF} read GetReader;
    property Writer: {$IFDEF CLR}MethodInfo{$ELSE}Pointer{$ENDIF} read GetWriter;
    property StoredProc: {$IFDEF CLR}MethodInfo{$ELSE}Pointer{$ENDIF} read GetStoredProc;
    property ReaderType: TJclPropSpecKind read GetReaderType;
    property WriterType: TJclPropSpecKind read GetWriterType;
    property StoredType: TJclPropSpecKind read GetStoredType;
    property ReaderValue: Integer read GetReaderValue;
    property WriterValue: Integer read GetWriterValue;
    property StoredValue: Integer read GetStoredValue;
    property Index: Integer read GetIndex;
    property Default: Longint read GetDefault;
    property NameIndex: Smallint read GetNameIndex;
    property Name: string read GetName;
  end;

constructor TJclPropInfo.Create(const APropInfo: {$IFDEF CLR}TPropInfo{$ELSE ~CLR}PPropInfo{$ENDIF ~CLR});
begin
  inherited Create;
  FPropInfo := APropInfo;
end;

function TJclPropInfo.GetPropInfo: {$IFDEF CLR}TPropInfo{$ELSE ~CLR}PPropInfo{$ENDIF ~CLR};
begin
  Result := FPropInfo;
end;

function TJclPropInfo.GetPropType: IJclTypeInfo;
begin
  {$IFDEF CLR}
  Result := JclTypeInfo(PropInfo.TypeInfo);
  {$ELSE}
  Result := JclTypeInfo(PropInfo.PropType^);
  {$ENDIF CLR}
end;

function TJclPropInfo.GetReader: {$IFDEF CLR}MethodInfo{$ELSE}Pointer{$ENDIF};
begin
  {$IFDEF CLR}
  Result := (PropInfo as PropertyInfo).GetGetMethod;
  {$ELSE}
  Result := PropInfo.GetProc;
  {$ENDIF CLR}
end;

function TJclPropInfo.GetWriter: {$IFDEF CLR}MethodInfo{$ELSE}Pointer{$ENDIF};
begin
  {$IFDEF CLR}
  Result := (PropInfo as PropertyInfo).GetSetMethod;
  {$ELSE}
  Result := PropInfo.SetProc;
  {$ENDIF CLR}
end;

function TJclPropInfo.GetStoredProc: {$IFDEF CLR}MethodInfo{$ELSE}Pointer{$ENDIF};
{$IFDEF CLR}
var
  I: Integer;
  Accessors: array of MethodInfo;
  Attributes: array of Attribute;
begin
  Result := nil;
  Attributes := Attribute.GetCustomAttributes(PropInfo, True);

  // .NET serializing system: NonSerializedAttribute
  for I := 0 to Length(Attributes) - 1 do
    if Attributes[I] is NonSerializedAttribute then
      Exit;

  // .NET form designer storage: DesignerSerializationVisibilityAttribute
  for I := 0 to Length(Attributes) - 1 do
    if Attributes[I] is DesignerSerializationVisibilityAttribute then
      Exit;

  if PropInfo is PropertyInfo then
  begin
    Accessors := PropertyInfo(PropInfo).GetAccessors;
    for I := 0 to High(Accessors) do
    begin
      if Accessors[I].ReturnType.Equals(TypeOf(System.Boolean)) and
         Accessors[I].Name.StartsWith('stored_') then
      begin
        Result := Accessors[I];
        Break;
      end;
    end;
  end;
end;
{$ELSE}
begin
  Result := PropInfo.StoredProc;
end;
{$ENDIF CLR}

function TJclPropInfo.GetIndex: Integer;
begin
  {$IFDEF CLR}
  Result := Integer($8000000);
  {$ELSE}
  Result := PropInfo.Index;
  {$ENDIF CLR}
end;

function TJclPropInfo.GetDefault: Longint;
begin
  {$IFDEF CLR}
  Result := GetOrdPropDefault(PropInfo);
  {$ELSE}
  Result := PropInfo.Default;
  {$ENDIF CLR}
end;

function TJclPropInfo.GetNameIndex: Smallint;
begin
  {$IFDEF CLR}
  Result := 0;
  {$ELSE}
  Result := PropInfo.NameIndex;
  {$ENDIF CLR}
end;

function TJclPropInfo.GetName: string;
begin
  Result := PropInfo.Name;
end;

{$IFDEF CLR}
function TJclPropInfo.MethodInfoToPropSecpKind(Info: MethodInfo): TJclPropSpecKind;
begin
  if Info.IsStatic then
    Result := pskStaticMethod
  else
  if Info.IsVirtual then
    Result := pskVirtualMethod
  else
    Result := pskNone;
end;
{$ENDIF CLR}

function TJclPropInfo.GetSpecKind(const Value: Integer): TJclPropSpecKind;
var
  P: Integer;
begin
  P := Value shr 24;
  case P of
    $00:
      if Value < 2 then
        Result := pskConstant
      else
        Result := pskStaticMethod;
    $FE:
      Result := pskVirtualMethod;
    $FF:
      Result := pskField;
  else
    Result := pskStaticMethod;
  end;
end;

function TJclPropInfo.GetSpecValue(const Value: Integer): Integer;
begin
  case GetSpecKind(Value) of
    pskStaticMethod, pskConstant:
      Result := Value;
    pskVirtualMethod:
      Result := Smallint(Value and $0000FFFF);
    pskField:
      Result := Value and $00FFFFFF;
  else
    Result := 0;
  end;
end;

function TJclPropInfo.GetReaderType: TJclPropSpecKind;
begin
  {$IFDEF CLR}
  Result := MethodInfoToPropSecpKind(Reader);
  {$ELSE}
  Result := GetSpecKind(Integer(Reader));
  {$ENDIF CLR}
end;

function TJclPropInfo.GetWriterType: TJclPropSpecKind;
begin
  {$IFDEF CLR}
  Result := MethodInfoToPropSecpKind(Writer);
  {$ELSE}
  Result := GetSpecKind(Integer(Writer));
  {$ENDIF CLR}
end;

function TJclPropInfo.GetStoredType: TJclPropSpecKind;
begin
  {$IFDEF CLR}
  Result := MethodInfoToPropSecpKind(StoredProc);
  {$ELSE}
  Result := GetSpecKind(Integer(StoredProc));
  {$ENDIF CLR}
end;

function TJclPropInfo.GetReaderValue: Integer;
begin
  {$IFDEF CLR}
  Result := 0;
  {$ELSE}
  Result := GetSpecValue(Integer(Reader));
  {$ENDIF CLR}
end;

function TJclPropInfo.GetWriterValue: Integer;
begin
  {$IFDEF CLR}
  Result := 0;
  {$ELSE}
  Result := GetSpecValue(Integer(Writer));
  {$ENDIF CLR}
end;

function TJclPropInfo.GetStoredValue: Integer;
begin
  {$IFDEF CLR}
  Result := 0;
  {$ELSE}
  Result := GetSpecValue(Integer(StoredProc));
  {$ENDIF CLR}
end;

function TJclPropInfo.IsStored(const AInstance: TObject): Boolean;
begin
  Result := IsStoredProp(AInstance, FPropInfo);
end;

function TJclPropInfo.HasDefault: Boolean;
begin
  Result := Longword(Default) <> $80000000;
end;

function TJclPropInfo.HasIndex: Boolean;
begin
  Result := Longword(Index) <> $80000000;
end;

//=== { TJclClassTypeInfo } ==================================================

type
  TJclClassTypeInfo = class(TJclTypeInfo, IJclClassTypeInfo)
  protected
    function GetClassRef: TClass;
    function GetParent: IJclClassTypeInfo;
    function GetTotalPropertyCount: Integer;
    function GetPropertyCount: Integer;
    function GetProperties(const PropIdx: Integer): IJclPropInfo;
    function GetPropNames(const Name: string): IJclPropInfo;
    function GetUnitName: string;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property ClassRef: TClass read GetClassRef;
    property Parent: IJclClassTypeInfo read GetParent;
    property TotalPropertyCount: Integer read GetTotalPropertyCount;
    property PropertyCount: Integer read GetPropertyCount;
    property Properties[const PropIdx: Integer]: IJclPropInfo read GetProperties;
    property PropNames[const Name: string]: IJclPropInfo read GetPropNames;
    property UnitName: string read GetUnitName;
  end;

function TJclClassTypeInfo.GetClassRef: TClass;
begin
  Result := TypeData.ClassType;
end;

function TJclClassTypeInfo.GetParent: IJclClassTypeInfo;
begin
  {$IFDEF CLR}
  if (TypeData.ParentInfo <> nil) then
    Result := JclTypeInfo(TypeData.ParentInfo) as IJclClassTypeInfo
  {$ELSE}
  if (TypeData.ParentInfo <> nil) and (TypeData.ParentInfo^ <> nil) then
    Result := JclTypeInfo(TypeData.ParentInfo^) as IJclClassTypeInfo
  {$ENDIF CLR}
  else
    Result := nil;
end;

function TJclClassTypeInfo.GetTotalPropertyCount: Integer;
begin
  Result := TypeData.PropCount;
end;

function TJclClassTypeInfo.GetPropertyCount: Integer;
{$IFDEF CLR}
begin
  Result := TypeData.PropCount;
end;
{$ELSE}
var
  PropData: ^TPropData;
begin
  PropData := @TypeData.UnitName;
  Inc(Integer(PropData), 1 + Length(UnitName));
  Result := PropData.PropCount;
end;
{$ENDIF CLR}

function TJclClassTypeInfo.GetProperties(const PropIdx: Integer): IJclPropInfo;
{$IFDEF CLR}
var
  List: TPropList;
begin
  if PropIdx + 1 > TypeData.PropCount then
    Result := Parent.Properties[PropIdx - TypeData.PropCount]
  else
  begin
    List := GetPropInfos(TypeInfo);
    if PropIdx > 0 then
      Result := TJclPropInfo.Create(List[PropIdx])
    else
      Result := TJclPropInfo.Create(List[0]);
  end;
end;
{$ELSE}
var
  PropData: ^TPropData;
  Prop: PPropInfo;
  Idx: Integer;
  RecSize: Integer;
begin
  PropData := @TypeData.UnitName;
  Inc(Integer(PropData), 1 + Length(UnitName));
  if PropIdx + 1 > PropData.PropCount then
    Result := Parent.Properties[PropIdx - PropData.PropCount]
  else
  begin
    Prop := PPropInfo(PropData);
    Inc(Integer(Prop), 2);
    if PropIdx > 0 then
    begin
      RecSize := SizeOf(TPropInfo) - SizeOf(ShortString);
      Idx := PropIdx;
      while Idx > 0 do
      begin
        Inc(Integer(Prop), RecSize);
        Inc(Integer(Prop), 1 + PByte(Prop)^);
        Dec(Idx);
      end;
    end;
    Result := TJclPropInfo.Create(Prop);
  end;
end;
{$ENDIF CLR}

function TJclClassTypeInfo.GetPropNames(const Name: string): IJclPropInfo;
var
  PropInfo: {$IFDEF CLR}TPropInfo{$ELSE ~CLR}PPropInfo{$ENDIF ~CLR};
begin
  PropInfo := GetPropInfo(TypeInfo, Name);
  if PropInfo <> nil then
    Result := TJclPropInfo.Create(PropInfo)
  else
    Result := nil;
end;

function TJclClassTypeInfo.GetUnitName: string;
begin
  Result := TypeData.UnitName;
end;

procedure TJclClassTypeInfo.WriteTo(const Dest: IJclInfoWriter);
const
{$IFDEF CLR}
  cFmt1 = '[%s %d]';
  cFmt2 = '[%s %s %s]';
  cFmt3 = '[%s=%s]';
  cFmt4 = '[%s=%s %s]';
{$ELSE}
  cFmt1 = '[%s %d]';
  cFmt2 = '[%s %s $%p]';
  cFmt3 = '[%s=%s]';
  cFmt4 = '[%s=%s $%p]';
{$ENDIF CLR}
var
  I: Integer;
  Prop: IJclPropInfo;
begin
  inherited WriteTo(Dest);
  Dest.Writeln(RsRTTIClassName + ClassRef.ClassName);
  Dest.Writeln(RsRTTIParent + Parent.ClassRef.ClassName);
  Dest.Writeln(RsRTTIUnitName + UnitName);
  Dest.Writeln(RsRTTIPropCount + IntToStr(PropertyCount) + ' (' +
    IntToStr(TotalPropertyCount) + ')');
  Dest.Indent;
  try
    for I := 0 to PropertyCount-1 do
    begin
      Prop := Properties[I];
      Dest.Writeln(Prop.Name + ': ' + Prop.PropType.Name);
      Dest.Indent;
      try
        if Prop.HasIndex then
          Dest.Writeln(Format(cFmt1, [RsRTTIIndex, Prop.Index]));
        if Prop.HasDefault then
          Dest.Writeln(Format(cFmt1, [RsRTTIDefault, Prop.Default]));
        case Prop.ReaderType of
          pskStaticMethod:
            Dest.Writeln(Format(cFmt2, [RsRTTIPropRead, RsRTTIStaticMethod,
              {$IFDEF CLR}
              Prop.Reader.ToString()]));
              {$ELSE}
              Pointer(Prop.ReaderValue)]));
              {$ENDIF CLR}
          pskField:
            Dest.Writeln(Format(cFmt2, [RsRTTIPropRead, RsRTTIField,
              {$IFDEF CLR}
              Prop.Reader.ToString()]));
              {$ELSE}
              Pointer(Prop.ReaderValue)]));
              {$ENDIF CLR}
          pskVirtualMethod:
            Dest.Writeln(Format(cFmt2, [RsRTTIPropRead, RsRTTIVirtualMethod,
              {$IFDEF CLR}
              Prop.Reader.ToString()]));
              {$ELSE}
              Pointer(Prop.ReaderValue)]));
              {$ENDIF CLR}
        end;
        case Prop.WriterType of
          pskStaticMethod:
            Dest.Writeln(Format(cFmt2, [RsRTTIPropWrite, RsRTTIStaticMethod,
              {$IFDEF CLR}
              Prop.Writer.ToString()]));
              {$ELSE}
              Pointer(Prop.WriterValue)]));
              {$ENDIF CLR}
          pskField:
            Dest.Writeln(Format(cFmt2, [RsRTTIPropWrite, RsRTTIField,
              {$IFDEF CLR}
              Prop.Writer.ToString()]));
              {$ELSE}
              Pointer(Prop.WriterValue)]));
              {$ENDIF CLR}
          pskVirtualMethod:
            Dest.Writeln(Format(cFmt2, [RsRTTIPropWrite, RsRTTIVirtualMethod,
              {$IFDEF CLR}
              Prop.Writer.ToString()]));
              {$ELSE}
              Pointer(Prop.WriterValue)]));
              {$ENDIF CLR}
        end;
        case Prop.StoredType of
          pskConstant:
            if Boolean(Prop.StoredValue) then
              Dest.Writeln(Format(cFmt3, [RsRTTIPropStored, RsRTTITrue]))
            else
              Dest.Writeln(Format(cFmt3, [RsRTTIPropStored, RsRTTIFalse]));
          pskStaticMethod:
            Dest.Writeln(Format(cFmt4, [RsRTTIPropStored, RsRTTIStaticMethod,
              {$IFDEF CLR}
              Prop.StoredProc.ToString()]));
              {$ELSE}
              Pointer(Prop.StoredValue)]));
              {$ENDIF CLR}
          pskField:
            Dest.Writeln(Format(cFmt4, [RsRTTIPropStored, RsRTTIField,
              {$IFDEF CLR}
              Prop.StoredProc.ToString()]));
              {$ELSE}
              Pointer(Prop.StoredValue)]));
              {$ENDIF CLR}
          pskVirtualMethod:
            Dest.Writeln(Format(cFmt4, [RsRTTIPropStored, RsRTTIVirtualMethod,
              {$IFDEF CLR}
              Prop.StoredProc.ToString()]));
              {$ELSE}
              Pointer(Prop.StoredValue)]));
              {$ENDIF CLR}
        end;
      finally
        Dest.Outdent;
      end;
    end;
  finally
    Dest.Outdent;
  end;
end;

procedure TJclClassTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
var
  {$IFDEF CLR}
  IntfTbl: array of &Type;
  {$ELSE}
  IntfTbl: PInterfaceTable;
  {$ENDIF CLR}
  I: Integer;
  Prop: IJclPropInfo;
begin
  if (Parent <> nil) and
     {$IFDEF CLR}
     not SameText(Parent.Name, 'TObject') then
     {$ELSE}
     not AnsiSameText(Parent.Name, 'TObject') then
     {$ENDIF CLR}
  begin
    Dest.Write(Name + ' = class(' + Parent.Name);
    {$IFDEF CLR}
    IntfTbl := ClassRef.ClassInfo.GetInterfaces;
    if IntfTbl <> nil then
      for I := 0 to High(IntfTbl) do
        Dest.Write(', [''' + JclGUIDToString(IntfTbl[I].TypeData.Guid) + ''']');
    {$ELSE}
    IntfTbl := ClassRef.GetInterfaceTable;
    if IntfTbl <> nil then
      for I := 0 to IntfTbl.EntryCount-1 do
        Dest.Write(', [''' + JclGUIDToString(IntfTbl.Entries[I].IID) + ''']');
    {$ENDIF CLR}
    Dest.Writeln(') // unit ' + UnitName);
  end
  else
    Dest.Writeln(Name + ' = class // unit ' + UnitName);
  if PropertyCount > 0 then
  begin
    Dest.Writeln('published');
    Dest.Indent;
    try
      for I := 0 to PropertyCount-1 do
      begin
        Prop := Properties[I];
        Dest.Write('property ' + Prop.Name + ': ' +  Prop.PropType.Name);
        if Prop.HasIndex then
          Dest.Write(Format(' index %d', [Prop.Index]));

        case Prop.ReaderType of
          {$IFDEF CLR}
          pskStaticMethod:
            Dest.Write(Format(' read [static method %s]', [Prop.Reader.ToString()]));
          pskField:
            Dest.Write(Format(' read [field %s]', [Prop.Reader.ToString()]));
          pskVirtualMethod:
            Dest.Write(Format(' read [virtual method %s]', [Prop.Reader.ToString()]));
          {$ELSE}
          pskStaticMethod:
            Dest.Write(Format(' read [static method $%p]', [Pointer(Prop.ReaderValue)]));
          pskField:
            Dest.Write(Format(' read [field $%p]', [Pointer(Prop.ReaderValue)]));
          pskVirtualMethod:
            Dest.Write(Format(' read [virtual method $%p]', [Pointer(Prop.ReaderValue)]));
          {$ENDIF CLR}
        end;

        case Prop.WriterType of
          {$IFDEF CLR}
          pskStaticMethod:
            Dest.Write(Format(' write [static method %s]', [Prop.Writer.ToString()]));
          pskField:
            Dest.Write(Format(' write [field %s]', [Prop.Writer.ToString()]));
          pskVirtualMethod:
            Dest.Write(Format(' write [virtual method %s]', [Prop.Writer.ToString()]));
          {$ELSE}
          pskStaticMethod:
            Dest.Write(Format(' write [static method $%p]', [Pointer(Prop.WriterValue)]));
          pskField:
            Dest.Write(Format(' write [field $%p]', [Pointer(Prop.WriterValue)]));
          pskVirtualMethod:
            Dest.Write(Format(' write [virtual method $%p]', [Pointer(Prop.WriterValue)]));
          {$ENDIF CLR}
        end;

        case Prop.StoredType of
          pskConstant:
            if Boolean(Prop.StoredValue) then
              Dest.Write(' stored = True')
            else
              Dest.Write(' stored = False');
          {$IFDEF CLR}
          pskStaticMethod:
            Dest.Write(Format(' stored = [static method %s]', [Prop.StoredProc.ToString()]));
          pskField:
            Dest.Write(Format(' stored = [field %s]', [Prop.StoredProc.ToString()]));
          pskVirtualMethod:
            Dest.Write(Format(' stored = [virtual method %s]', [Prop.StoredProc.ToString()]));
          {$ELSE}
          pskStaticMethod:
            Dest.Write(Format(' stored = [static method $%p]', [Pointer(Prop.StoredValue)]));
          pskField:
            Dest.Write(Format(' stored = [field $%p]', [Pointer(Prop.StoredValue)]));
          pskVirtualMethod:
            Dest.Write(Format(' stored = [virtual method $%p]', [Pointer(Prop.StoredValue)]));
          {$ENDIF CLR}
        end;
        if Prop.HasDefault then
          Dest.Write(' default ' + IntToStr(Prop.Default));
        Dest.Writeln(';');
      end;
    finally
      Dest.Outdent;
    end;
  end;
  Dest.Writeln('end;');
end;

//=== { TJclEventParamInfo } =================================================

type
  TJclEventParamInfo = class(TInterfacedObject, IJclEventParamInfo)
  private
    FParam: {$IFDEF CLR}ParameterInfo{$ELSE}Pointer{$ENDIF};
  protected
    function GetFlags: TParamFlags;
    function GetName: string;
    function GetRecSize: Integer;
    function GetTypeName: string;
    function GetParam: {$IFDEF CLR}ParameterInfo{$ELSE}Pointer{$ENDIF};
  public
    constructor Create(const AParam: {$IFDEF CLR}ParameterInfo{$ELSE}Pointer{$ENDIF});

    property Flags: TParamFlags read GetFlags;
    property Name: string read GetName;
    property RecSize: Integer read GetRecSize;
    property TypeName: string read GetTypeName;
    property Param: {$IFDEF CLR}ParameterInfo{$ELSE}Pointer{$ENDIF} read GetParam;
  end;

constructor TJclEventParamInfo.Create(const AParam: {$IFDEF CLR}ParameterInfo{$ELSE}Pointer{$ENDIF});
begin
  inherited Create;
  FParam := AParam;
end;

function TJclEventParamInfo.GetFlags: TParamFlags;
{$IFDEF CLR}
var
  Attr: Attribute;
{$ENDIF CLR}
begin
  {$IFDEF CLR}
  Result := [];
  if FParam.IsOut then
    Result := [pfOut]
  else
  if FParam.ParameterType.IsByRef then
    Result := [pfVar]
  else
  if FindAttribute(FParam.ParameterType, TypeOf(TConstantParamAttribute), Attr) then
    Result := [pfConst];

  with FParam.ParameterType do
    if IsArray or (IsByRef and HasElementType and GetElementType.IsArray) then
       Include(Result, pfArray);
  {$ELSE}
  Result := TParamFlags(PByte(Param)^);
  {$ENDIF CLR}
end;

function TJclEventParamInfo.GetName: string;
{$IFDEF CLR}
begin
  Result := FParam.Name;
end;
{$ELSE}
var
  PName: PShortString;
begin
  PName := Param;
  Inc(Integer(PName));
  Result := PName^;
end;
{$ENDIF CLR}

function TJclEventParamInfo.GetRecSize: Integer;
begin
  Result := 3 + Length(Name) + Length(TypeName);
end;

function TJclEventParamInfo.GetTypeName: string;
{$IFDEF CLR}
begin
  Result := FParam.ParameterType.Name;
end;
{$ELSE}
var
  PName: PShortString;
begin
  PName := Param;
  Inc(Integer(PName));
  Inc(Integer(PName), PByte(PName)^ + 1);
  Result := PName^;
end;
{$ENDIF CLR}

function TJclEventParamInfo.GetParam: {$IFDEF CLR}ParameterInfo{$ELSE}Pointer{$ENDIF};
begin
  Result := FParam;
end;

//=== { TJclEventTypeInfo } ==================================================

type
  TJclEventTypeInfo = class(TJclTypeInfo, IJclEventTypeInfo)
  protected
    function GetMethodKind: TMethodKind;
    function GetParameterCount: Integer;
    function GetParameters(const ParamIdx: Integer): IJclEventParamInfo;
    function GetResultTypeName: string;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property MethodKind: TMethodKind read GetMethodKind;
    property ParameterCount: Integer read GetParameterCount;
    property Parameters[const ParamIdx: Integer]: IJclEventParamInfo
      read GetParameters;
    property ResultTypeName: string read GetResultTypeName;
  end;

function TJclEventTypeInfo.GetMethodKind: TMethodKind;
begin
  Result := TypeData.MethodKind;
end;

function TJclEventTypeInfo.GetParameterCount: Integer;
begin
  Result := TypeData.ParamCount;
end;

function TJclEventTypeInfo.GetParameters(const ParamIdx: Integer): IJclEventParamInfo;
{$IFNDEF CLR}
var
  I: Integer;
  Param: Pointer;
{$ENDIF ~CLR}
begin
  Result := nil;
  {$IFDEF CLR}
  if ParamIdx < TypeData.ParamCount then
    Result := TJclEventParamInfo.Create(TypeData.Params[ParamIdx]);
  {$ELSE}
  Param := @TypeData.ParamList[0];
  I := ParamIdx;
  while I >= 0 do
  begin
    Result := TJclEventParamInfo.Create(Param);
    Inc(Integer(Param), Result.RecSize);
    Dec(I);
  end;
  {$ENDIF CLR}
end;

function TJclEventTypeInfo.GetResultTypeName: string;
{$IFDEF CLR}
begin
  Result := TypeData.ResultTypeName;
end;
{$ELSE}
var
  LastParam: IJclEventParamInfo;
  ResPtr: PShortString;
begin
  if MethodKind = mkFunction then
  begin
    if ParameterCount > 0 then
    begin
      LastParam := Parameters[ParameterCount-1];
      ResPtr := Pointer(Longint(LastParam.Param) + LastParam.RecSize);
    end
    else
      ResPtr := @TypeData.ParamList[0];
    Result := ResPtr^;
  end
  else
    Result := '';
end;
{$ENDIF CLR}

procedure TJclEventTypeInfo.WriteTo(const Dest: IJclInfoWriter);
var
  I: Integer;
  Param: IJclEventParamInfo;
  ParamFlags: TParamFlags;
begin
  inherited WriteTo(Dest);
  {$IFDEF CLR}
  Dest.Writeln(RsRTTIMethodKind +
    JclEnumValueToIdent(Borland.Delphi.System.TypeInfo(TMethodKind), TypeData.MethodKind));
  Dest.Writeln(RsRTTIParamCount + IntToStr(ParameterCount));
  Dest.Indent;
  try
    for I := 0 to ParameterCount-1 do
    begin
      if I > 0 then
        Dest.Writeln('');
      Param := Parameters[I];
      ParamFlags := Param.Flags;
      Dest.Writeln(RsRTTIName + Param.Name);
      Dest.Writeln(RsRTTIType + Param.TypeName);
      Dest.Writeln(RsRTTIFlags +
        JclSetToStr(Borland.Delphi.System.TypeInfo(TParamFlags), ParamFlags, True, False));
    end;
  finally
    Dest.Outdent;
  end;
  if MethodKind = mkFunction then
    Dest.Writeln(RsRTTIReturnType + ResultTypeName);
  {$ELSE}
  Dest.Writeln(LoadResString(@RsRTTIMethodKind) +
    JclEnumValueToIdent(System.TypeInfo(TMethodKind), TypeData.MethodKind));
  Dest.Writeln(LoadResString(@RsRTTIParamCount) + IntToStr(ParameterCount));
  Dest.Indent;
  try
    for I := 0 to ParameterCount-1 do
    begin
      if I > 0 then
        Dest.Writeln('');
      Param := Parameters[I];
      ParamFlags := Param.Flags;
      Dest.Writeln(LoadResString(@RsRTTIName) + Param.Name);
      Dest.Writeln(LoadResString(@RsRTTIType) + Param.TypeName);
      Dest.Writeln(LoadResString(@RsRTTIFlags) +
        JclSetToStr(System.TypeInfo(TParamFlags), ParamFlags, True, False));
    end;
  finally
    Dest.Outdent;
  end;
  if MethodKind = mkFunction then
    Dest.Writeln(LoadResString(@RsRTTIReturnType) + ResultTypeName);
  {$ENDIF CLR}
end;

procedure TJclEventTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
var
  Prefix: string;
  I: Integer;
  Param: IJclEventParamInfo;
begin
  Dest.Write(Name + ' = ');
  if MethodKind = mkFunction then
    Dest.Write('function')
  else
    Dest.Write('procedure');
  Prefix := '(';
  for I := 0 to ParameterCount-1 do
  begin
    Dest.Write(Prefix);
    Prefix := '; ';
    Param := Parameters[I];
    {$IFDEF CLR}
    if pfVar in Param.Flags then
      Dest.Write(RsRTTIVar)
    else
    if pfConst in Param.Flags then
      Dest.Write(RsRTTIConst)
    else
    if pfOut in Param.Flags then
      Dest.Write(RsRTTIOut);
    {$ELSE}
    if pfVar in Param.Flags then
      Dest.Write(LoadResString(@RsRTTIVar))
    else
    if pfConst in Param.Flags then
      Dest.Write(LoadResString(@RsRTTIConst))
    else
    if pfOut in Param.Flags then
      Dest.Write(LoadResString(@RsRTTIOut));
    {$ENDIF CLR}
    Dest.Write(Param.Name);
    if Param.TypeName <> '' then
    begin
      Dest.Write(': ');
      {$IFDEF CLR}
      if pfArray in Param.Flags then
        Dest.Write(RsRTTIArrayOf);
      if WideSameText(Param.TypeName, 'TVarRec') and (pfArray in Param.Flags) then
        Dest.Write(TrimRight(RsRTTIConst))
      {$ELSE}
      if pfArray in Param.Flags then
        Dest.Write(LoadResString(@RsRTTIArrayOf));
      if AnsiSameText(Param.TypeName, 'TVarRec') and (pfArray in Param.Flags) then
        Dest.Write(TrimRight(LoadResString(@RsRTTIConst)))
      {$ENDIF CLR}
      else
        Dest.Write(Param.TypeName);
    end;
  end;
  if ParameterCount <> 0 then
    Dest.Write(')');
  if MethodKind = mkFunction then
    Dest.Write(': ' + ResultTypeName);
  Dest.Writeln(' of object;');
end;

//=== { TJclInterfaceTypeInfo } ==============================================

type
  TJclInterfaceTypeInfo = class(TJclTypeInfo, IJclInterfaceTypeInfo)
  protected
    function GetParent: IJclInterfaceTypeInfo;
    function GetFlags: TIntfFlagsBase;
    function GetGUID: TGUID;
    {$IFDEF RTL140_UP}
    function GetPropertyCount: Integer;
    {$ENDIF RTL140_UP}
    function GetUnitName: string;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property Parent: IJclInterfaceTypeInfo read GetParent;
    property Flags: TIntfFlagsBase read GetFlags;
    property GUID: TGUID read GetGUID;
    {$IFDEF RTL140_UP}
    property PropertyCount: Integer read GetPropertyCount;
    {$ENDIF RTL140_UP}
    property UnitName: string read GetUnitName;
  end;

function TJclInterfaceTypeInfo.GetParent: IJclInterfaceTypeInfo;
begin
  {$IFDEF CLR}
  if TypeInfo.BaseType <> nil then
    Result := JclTypeInfo(TypeInfo.BaseType) as IJclInterfaceTypeInfo
  {$ELSE}
  if (TypeData.IntfParent <> nil) and (TypeData.IntfParent^ <> nil) then
    Result := JclTypeInfo(TypeData.IntfParent^) as IJclInterfaceTypeInfo
  {$ENDIF CLR}
  else
    Result := nil;
end;

function TJclInterfaceTypeInfo.GetFlags: TIntfFlagsBase;
begin
  {$IFDEF CLR}
  Result := [];
  {$ELSE}
  Result := TypeData.IntfFlags;
  {$ENDIF CLR}
end;

const
  NullGUID: TGUID = '{00000000-0000-0000-0000-000000000000}';

function TJclInterfaceTypeInfo.GetGUID: TGUID;
begin
  if ifHasGuid in Flags then
    Result := TypeData.Guid
  else
    Result := NullGUID;
end;

{$IFDEF RTL140_UP}
function TJclInterfaceTypeInfo.GetPropertyCount: Integer;
{$IFDEF CLR}
begin
  Result := TypeData.PropCount;
end;
{$ELSE}
var
  PropData: ^TPropData;
begin
  PropData := @TypeData.IntfUnit;
  Inc(Integer(PropData), 1 + Length(UnitName));
  Result := PropData.PropCount;
end;
{$ENDIF CLR}
{$ENDIF RTL140_UP}

function TJclInterfaceTypeInfo.GetUnitName: string;
begin
  Result := TypeData.IntfUnit;
end;

procedure TJclInterfaceTypeInfo.WriteTo(const Dest: IJclInfoWriter);
var
  IntfFlags: TIntfFlagsBase;
begin
  inherited WriteTo(Dest);
  if ifHasGuid in Flags then
  {$IFDEF CLR}
    Dest.Writeln(RsRTTIGUID + JclGuidToString(GUID));
  IntfFlags := Flags;
  Dest.Writeln(RsRTTIFlags + JclSetToStr(Borland.Delphi.System.TypeInfo(TIntfFlagsBase),
    IntfFlags, True, False));
  Dest.Writeln(RsRTTIUnitName + UnitName);
  if Parent <> nil then
    Dest.Writeln(RsRTTIParent + Parent.Name);
  Dest.Writeln(RsRTTIPropCount + IntToStr(PropertyCount));
  {$ELSE}
    Dest.Writeln(LoadResString(@RsRTTIGUID) + JclGuidToString(GUID));
  IntfFlags := Flags;
  Dest.Writeln(LoadResString(@RsRTTIFlags) + JclSetToStr(System.TypeInfo(TIntfFlagsBase),
    IntfFlags, True, False));
  Dest.Writeln(LoadResString(@RsRTTIUnitName) + UnitName);
  if Parent <> nil then
    Dest.Writeln(LoadResString(@RsRTTIParent) + Parent.Name);
  {$IFDEF RTL140_UP}
  Dest.Writeln(LoadResString(@RsRTTIPropCount) + IntToStr(PropertyCount));
  {$ENDIF RTL140_UP}
  {$ENDIF CLR}
end;

procedure TJclInterfaceTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
begin
  Dest.Write(Name + ' = ');
  if ifDispInterface in Flags then
    Dest.Write('dispinterface')
  else
    Dest.Write('interface');
  {$IFDEF CLR}
  if (Parent <> nil) and not (ifDispInterface in Flags) and not
      WideSameText(Parent.Name, 'IUnknown') then
    Dest.Write('(' + Parent.Name + ')');
  {$ELSE ~CLR}
  if (Parent <> nil) and not (ifDispInterface in Flags) and not
      AnsiSameText(Parent.Name, 'IUnknown') then
    Dest.Write('(' + Parent.Name + ')');
  {$ENDIF ~CLR}
  Dest.Writeln(' // unit ' + UnitName);
  Dest.Indent;
  try
    if ifHasGuid in Flags then
      Dest.Writeln('[''' + JclGuidToString(GUID) + ''']');
  finally
    Dest.Outdent;
    Dest.Writeln('end;');
  end;
end;

//=== { TJclInt64TypeInfo } ==================================================

type
  TJclInt64TypeInfo = class(TJclTypeInfo, IJclInt64TypeInfo)
  protected
    function GetMinValue: Int64;
    function GetMaxValue: Int64;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property MinValue: Int64 read GetMinValue;
    property MaxValue: Int64 read GetMaxValue;
  end;

function TJclInt64TypeInfo.GetMinValue: Int64;
begin
  Result := TypeData.MinInt64Value;
end;

function TJclInt64TypeInfo.GetMaxValue: Int64;
begin
  Result := TypeData.MaxInt64Value;
end;

procedure TJclInt64TypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  {$IFDEF CLR}
  Dest.Writeln(RsRTTIMinValue + IntToStr(MinValue));
  Dest.Writeln(RsRTTIMaxValue + IntToStr(MaxValue));
  {$ELSE}
  Dest.Writeln(LoadResString(@RsRTTIMinValue) + IntToStr(MinValue));
  Dest.Writeln(LoadResString(@RsRTTIMaxValue) + IntToStr(MaxValue));
  {$ENDIF CLR}
end;

procedure TJclInt64TypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
begin
  Dest.Writeln(Name + ' = ' + IntToStr(MinValue) + ' .. ' + IntToStr(MaxValue) + ';');
end;

//=== { TJclDynArrayTypeInfo } ===============================================

{$IFDEF RTL140_UP}

type
  TJclDynArrayTypeInfo = class(TJclTypeInfo, IJclDynArrayTypeInfo)
  protected
    function GetElementSize: Longint;
    function GetElementType: IJclTypeInfo;
    function GetElementsNeedCleanup: Boolean;
    function GetVarType: Integer;
    function GetUnitName: string;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property ElementSize: Longint read GetElementSize;
    property ElementType: IJclTypeInfo read GetElementType;
    property ElementsNeedCleanup: Boolean read GetElementsNeedCleanup;
    property VarType: Integer read GetVarType;
    property UnitName: string read GetUnitName;
  end;

function TJclDynArrayTypeInfo.GetElementSize: Longint;
begin
  {$IFDEF CLR}
  Result := Marshal.SizeOf(TypeInfo.GetElementType);
  {$ELSE}
  Result := TypeData.elSize;
  {$ENDIF CLR}
end;

function TJclDynArrayTypeInfo.GetElementType: IJclTypeInfo;
begin
  {$IFDEF CLR}
  Result := JclTypeInfo(TypeInfo.GetElementType);
  {$ELSE}
  if TypeData.elType = nil then
  begin
    if TypeData.elType2 <> nil then
      Result := JclTypeInfo(TypeData.elType2^)
    else
      Result := nil;
  end
  else
    Result := JclTypeInfo(TypeData.elType^);
  {$ENDIF CLR}
end;

function TJclDynArrayTypeInfo.GetElementsNeedCleanup: Boolean;
begin
  {$IFDEF CLR}
  Result := False;
  {$ELSE}
  Result := TypeData.elType <> nil;
  {$ENDIF CLR}
end;

function TJclDynArrayTypeInfo.GetVarType: Integer;
begin
  {$IFDEF CLR}
  Result := Variant.VarType(TypeInfo);
  {$ELSE}
  Result := TypeData.varType;
  {$ENDIF CLR}
end;

function TJclDynArrayTypeInfo.GetUnitName: string;
begin
  Result := TypeData.DynUnitName;
end;

procedure TJclDynArrayTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  Dest.Writeln(RsRTTIElSize + IntToStr(ElementSize));
  if ElementType = nil then
    Dest.Writeln(RsRTTIElType + RsRTTITypeError)
  else
  if ElementType.Name[1] <> '.' then
    Dest.Writeln(RsRTTIElType + ElementType.Name)
  else
  begin
    Dest.Writeln(RsRTTIElType);
    Dest.Indent;
    try
      ElementType.WriteTo(Dest);
    finally
      Dest.Outdent;
    end;
  end;
  Dest.Write(RsRTTIElNeedCleanup);
  if ElementsNeedCleanup then
    Dest.Writeln(RsRTTITrue)
  else
    Dest.Writeln(RsRTTIFalse);
  Dest.Writeln(RsRTTIVarType + IntToStr(VarType));
  Dest.Writeln(RsRTTIUnitName + UnitName);
end;

procedure TJclDynArrayTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
begin
  if Name[1] <> '.' then
    Dest.Write(Name + ' = ' + RsRTTIArrayOf)
  else
    Dest.Write(RsRTTIArrayOf);
  if ElementType = nil then
    Dest.Write(RsRTTITypeError)
  else
  if ElementType.Name[1] = '.' then
    ElementType.DeclarationTo(Dest)
  else
    Dest.Write(ElementType.Name);
  if Name[1] <> '.' then
    Dest.Writeln('; // Unit ' + UnitName);
end;

{$ENDIF RTL140_UP}

//=== Typeinfo retrieval =====================================================

function JclTypeInfo(ATypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR}): IJclTypeInfo;
begin
  {$IFDEF CLR}
  case ATypeInfo.TypeKind of
  {$ELSE ~CLR}
  case ATypeInfo.Kind of
  {$ENDIF ~CLR}
    tkInteger, tkChar, tkWChar:
      Result := TJclOrdinalRangeTypeInfo.Create(ATypeInfo);
    tkEnumeration:
      Result := TJclEnumerationTypeInfo.Create(ATypeInfo);
    tkSet:
      Result := TJclSetTypeInfo.Create(ATypeInfo);
    tkFloat:
      Result := TJclFloatTypeInfo.Create(ATypeInfo);
    tkString:
      Result := TJclStringTypeInfo.Create(ATypeInfo);
    tkClass:
      Result := TJclClassTypeInfo.Create(ATypeInfo);
    tkMethod:
      Result := TJclEventTypeInfo.Create(ATypeInfo);
    tkInterface:
      Result := TJclInterfaceTypeInfo.Create(ATypeInfo);
    tkInt64:
      Result := TJclInt64TypeInfo.Create(ATypeInfo);
    {$IFDEF RTL140_UP}
    tkDynArray:
      Result := TJclDynArrayTypeInfo.Create(ATypeInfo);
    {$ENDIF RTL140_UP}
  else
    Result := TJclTypeInfo.Create(ATypeInfo);
  end;
end;

//=== User generated type info managment =====================================

{$IFNDEF CLR}
var
  TypeList: TThreadList;

type
  PTypeItem = ^TTypeItem;
  TTypeItem = record
    TypeInfo: PTypeInfo;
    RefCount: Integer;
  end;

procedure FreeTypeData(const TypeInfo: PTypeInfo);
var
  TD: PTypeData;
begin
  TD := GetTypeData(TypeInfo);
  if TypeInfo.Kind = tkSet then
    RemoveTypeInfo(TD^.CompType^)
  else
  if (TypeInfo.Kind = tkEnumeration) and (TD^.BaseType^ <> TypeInfo) then
    RemoveTypeInfo(GetTypeData(TypeInfo)^.BaseType^);
  FreeMem(GetTypeData(TypeInfo)^.BaseType);
  FreeMem(TypeInfo);
end;

procedure AddType(const TypeInfo: PTypeInfo);
var
  Item: PTypeItem;
begin
  New(Item);
  try
    Item.TypeInfo := TypeInfo;
    Item.RefCount := 1;
    TypeList.Add(Item);
  except
    Dispose(Item);
    raise;
  end;
end;

procedure DeleteType(const TypeItem: PTypeItem);
begin
  FreeTypeData(TypeItem.TypeInfo);
  TypeList.Remove(TypeItem);
  Dispose(TypeItem);
end;

procedure DoRefType(const TypeInfo: PTypeInfo; Add: Integer);
var
  I: Integer;
  List: TList;
begin
  List := TypeList.LockList;
  try
    I := List.Count-1;
    while (I >= 0) and (PTypeItem(List[I]).TypeInfo <> TypeInfo) do
      Dec(I);
    if I > -1 then
      Inc(PTypeItem(List[I]).RefCount, Add);
  finally
    TypeList.UnlockList;
  end;
end;

procedure ReferenceType(const TypeInfo: PTypeInfo);
begin
  DoRefType(TypeInfo, 1);
end;

procedure DeReferenceType(const TypeInfo: PTypeInfo);
begin
  DoRefType(TypeInfo, -1);
end;

procedure ClearInfoList;
var
  L: TList;
begin
  L := TypeList.LockList;
  try
    while L.Count > 0 do
      RemoveTypeInfo(PTypeItem(L[L.Count-1])^.TypeInfo);
  finally
    TypeList.UnlockList;
  end;
end;

procedure NewInfoItem(const TypeInfo: PTypeInfo);
begin
  TypeList.Add(TypeInfo);
end;

procedure RemoveTypeInfo(TypeInfo: PTypeInfo);
var
  I: Integer;
  List: TList;
  Item: PTypeItem;
begin
  Item := nil;
  List := TypeList.LockList;
  try
    I := List.Count-1;
    while (I >= 0) and (PTypeItem(List[I]).TypeInfo <> TypeInfo) do
      Dec(I);
    if I > -1 then
      Item := PTypeItem(List[I]);
  finally
    TypeList.UnlockList;
  end;
  if Item <> nil then
  begin
    Dec(Item.RefCount);
    if Item.RefCount <= 0 then
      DeleteType(Item);
  end;
end;
{$ENDIF ~CLR}

//=== Enumerations ===========================================================

function JclEnumValueToIdent(TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR};
  const Value): string;
var
  MinEnum: Integer;
  MaxEnum: Integer;
  EnumVal: Int64;
  OrdType: TOrdType;
begin
  OrdType := GetTypeData(TypeInfo).OrdType;
  MinEnum := GetTypeData(TypeInfo).MinValue;
  MaxEnum := GetTypeData(TypeInfo).MaxValue;
  case OrdType of
    otSByte:
      EnumVal := Smallint(Value);
    otUByte:
      EnumVal := Byte(Value);
    otSWord:
      EnumVal := Shortint(Value);
    otUWord:
      EnumVal := Word(Value);
    otSLong:
      EnumVal := Integer(Value);
    otULong:
      EnumVal := Longword(Value);
  else
    EnumVal := 0;
  end;
  // Check range...
  if (EnumVal < MinEnum) or (EnumVal > MaxEnum) then
    {$IFDEf CLR}
    Result := Format(RsRTTIValueOutOfRange, [RsRTTIOrdinal + IntToStr(EnumVal)])
    {$ELSE}
    Result := Format(LoadResString(@RsRTTIValueOutOfRange),
      [LoadResString(@RsRTTIOrdinal) + IntToStr(EnumVal)])
    {$ENDIF CLR}
  else
    Result := GetEnumName(TypeInfo, EnumVal);
end;

{$IFNDEF CLR}
function JclGenerateEnumType(const TypeName: ShortString;
  const Literals: array of string): PTypeInfo;
type
  PInteger = ^Integer;
var
  StringSize: Integer;
  I: Integer;
  TypeData: PTypeData;
  CurName: PShortString;
begin
  StringSize := 0;
  for I := Low(Literals) to High(Literals) do
    StringSize := StringSize + 1 + Length(Literals[I]);
  Result := AllocMem(SizeOf(TTypeInfo) + SizeOf(TOrdType) +
    (2*SizeOf(Integer)) + SizeOf(PPTypeInfo) +
    StringSize {$IFDEF RTL140_UP}+ 1{$ENDIF RTL140_UP});
  try
    with Result^ do
    begin
      Kind := tkEnumeration;
      Name := TypeName;
    end;
    TypeData := GetTypeData(Result);
    TypeData^.BaseType := AllocMem(SizeOf(Pointer));
    if Length(Literals) < 256 then
      TypeData^.OrdType := otUByte
    else
    if Length(Literals) < 65536 then
      TypeData^.OrdType := otUWord
    else
      TypeData^.OrdType := otULong;
    TypeData^.MinValue := 0;
    TypeData^.MaxValue := Length(Literals)-1;
    TypeData^.BaseType^ := Result;   // No sub-range: basetype points to itself
    CurName := @TypeData^.NameList;
    for I := Low(Literals) to High(Literals) do
    begin
      CurName^ := Literals[I];
      Inc(Integer(CurName), Length(Literals[I])+1);
    end;
    {$IFDEF RTL140_UP}
    CurName^ := ''; // Unit name unknown
    {$ENDIF RTL140_UP}
    AddType(Result);
  except
    try
      ReallocMem(Result, 0);
    except
      Result := nil;
    end;
    raise;
  end;
end;

function JclGenerateEnumTypeBasedOn(const TypeName: ShortString;
  BaseType: PTypeInfo; const PrefixCut: Byte): PTypeInfo;
var
  BaseInfo: IJclTypeInfo;
  BaseKind: TTypeKind;
  Literals: array of string;
  I: Integer;
  S: string;
begin
  BaseInfo := JclTypeInfo(BaseType);
  BaseKind := BaseInfo.TypeKind;
  if BaseInfo.TypeKind <> tkEnumeration then
    raise EJclRTTIError.CreateResFmt(@RsRTTIInvalidBaseType, [BaseInfo.Name,
      JclEnumValueToIdent(System.TypeInfo(TTypeKind), BaseKind)]);
  with BaseInfo as IJclEnumerationTypeInfo do
  begin
    SetLength(Literals, MaxValue - MinValue + 1);
    for I := MinValue to MaxValue do
    begin
      S := Names[I];
      if PrefixCut = PREFIX_CUT_LOWERCASE then
        while (Length(S) > 0) and (S[1] in AnsiLowercaseLetters) do
          Delete(S, 1, 1);
      if (PrefixCut > 0) and (PrefixCut < MaxPrefixCut) then
        Delete(S, 1, PrefixCut);
      if S = '' then
        S := Names[I];
      Literals[I- MinValue] := S;
    end;
    if PrefixCut = PREFIX_CUT_EQUAL then
    begin
      S := Literals[High(Literals)];
      I := High(Literals)-1;
      while (I >= 0) and (S > '') do
      begin
        while Copy(Literals[I], 1, Length(S)) <> S do
          Delete(S, Length(S), 1);
        Dec(I);
      end;
      if S > '' then
        for I := Low(Literals) to High(Literals) do
        begin
          Literals[I] := StrRestOf(Literals[I], Length(S));
          if Literals[I] = '' then
            Literals[I] := Names[I + MinValue];
        end;
    end;
  end;
  Result := JclGenerateEnumType(TypeName, Literals);
end;

function JclGenerateSubRange(BaseType: PTypeInfo; const TypeName: string;
  const MinValue, MaxValue: Integer): PTypeInfo;
var
  TypeData: PTypeData;
begin
  Result := AllocMem(SizeOf(TTypeInfo) + SizeOf(TOrdType) +
    (2*SizeOf(Integer)) + SizeOf(PPTypeInfo));
  try
    with Result^ do
    begin
      Kind := BaseType^.Kind;
      Name := TypeName;
    end;
    TypeData := GetTypeData(Result);
    TypeData^.OrdType := GetTypeData(BaseType)^.OrdType;
    TypeData^.MinValue := MinValue;
    TypeData^.MaxValue := MaxValue;
    TypeData^.BaseType := AllocMem(SizeOf(Pointer));
    TypeData^.BaseType^ := BaseType;
    AddType(Result);
  except
    try
      ReallocMem(Result, 0);
    except
      Result := nil;
    end;
    raise;
  end;
  ReferenceType(BaseType);
end;
{$ENDIF ~CLR}

//=== Integers ===============================================================

function JclStrToTypedInt(Value: string; TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR}): Integer;
var
  Conv: TIdentToInt;
  HaveConversion: Boolean;
  Info: IJclTypeInfo;
  RangeInfo: IJclOrdinalRangeTypeInfo;
  TmpVal: Int64;
begin
  if TypeInfo <> nil then
    Conv := FindIdentToInt(TypeInfo)
  else
    Conv := nil;
  HaveConversion := (@Conv <> nil) and Conv(Value, Result);
  if not HaveConversion then
  begin
    if TypeInfo <> nil then
    begin
      Info := JclTypeInfo(TypeInfo);
      {$IFDEF CLR}
      if not Supports(Info, IJclOrdinalRangeTypeInfo, RangeInfo) then
      {$ELSE}
      if Info.QueryInterface(IJclOrdinalRangeTypeInfo, RangeInfo) <> S_OK then
      {$ENDIF CLR}
        RangeInfo := nil;
      TmpVal := StrToInt64(Value);
      if (RangeInfo <> nil) and ((TmpVal < RangeInfo.MinValue) or
          (TmpVal > RangeInfo.MaxValue)) then
        {$IFDEF CLR}
        raise EConvertError.CreateFmt(SInvalidInteger, [Value]);
        {$ELSE}
        raise EConvertError.CreateResFmt(@SInvalidInteger, [Value]);
        {$ENDIF CLR}
      Result := Integer(TmpVal);
    end
    else
      Result := StrToInt(Value)
  end;
end;

function JclTypedIntToStr(Value: Integer; TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR}): string;
var
  Conv: TIntToIdent;
  HaveConversion: Boolean;
begin
  if TypeInfo <> nil then
    Conv := FindIntToIdent(TypeInfo)
  else
    Conv := nil;
  HaveConversion := (@Conv <> nil) and Conv(Value, Result);
  if not HaveConversion then
  begin
    if (TypeInfo <> nil) and (GetTypeData(TypeInfo).OrdType = otULong) then
      Result := IntToStr(Int64(Cardinal(Value)))
    else
      Result := IntToStr(Value)
  end;
end;

//=== Sets ===================================================================

function JclSetToList(TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR};
  const Value; const WantBrackets: Boolean; const WantRanges: Boolean;
  const Strings: TStrings): string;
var
  SetType: IJclSetTypeInfo;
  I: Integer;
begin
  I := Strings.Count;
  Result := '';
  SetType := JclTypeInfo(TypeInfo) as IJclSetTypeInfo;
  SetType.GetAsList(Value, WantRanges, Strings);
  for I := I to Strings.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ', ' + Strings[I]
    else
      Result := Result + Strings[I];
  end;
  if WantBrackets then
    Result := '[' + Result + ']';
end;

function JclSetToStr(TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR};
  const Value; const WantBrackets: Boolean; const WantRanges: Boolean): string;
var
  Dummy: TStringList;
begin
  Dummy := TStringList.Create;
  try
    Result := JclSetToList(TypeInfo, Value, WantBrackets, WantRanges, Dummy);
  finally
    Dummy.Free;
  end;
end;

procedure JclStrToSet(TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR};
  var SetVar; const Value: string);
var
  SetInfo: IJclSetTypeInfo;
  S: TStringList;
begin
  SetInfo := JclTypeInfo(TypeInfo) as IJclSetTypeInfo;
  S := TStringList.Create;
  try
    StrToStrings(Value, ',', S);
    if S.Count > 0 then
    begin
      if S[0][1] = '[' then
      begin
        S[0] := Copy(S[0], 2, Length(S[0]));
        S[S.Count-1] := Copy(S[S.Count-1], 1,
          Length(S[S.Count-1]) - 1);
      end;
    end;
    SetInfo.SetAsList(SetVar, S);
  finally
    S.Free;
  end;
end;

procedure JclIntToSet(TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR};
  var SetVar; const Value: Integer);
var
  BitShift: Integer;
  TmpInt64: Int64;
  EnumMin: Integer;
  {$IFDEF CLR}
  CompType: TTypeInfo;
  {$ELSE ~CLR}
  EnumMax: Integer;
  ResBytes: Integer;
  CompType: PTypeInfo;
  {$ENDIF ~CLR}
begin
  CompType := GetTypeData(TypeInfo).CompType{$IFNDEF CLR}^{$ENDIF};
  EnumMin := GetTypeData(CompType).MinValue;
  BitShift := EnumMin mod 8;
  TmpInt64 := Longword(Value) shl BitShift;
  {$IFDEF CLR}
  SetVar := BitConverter.GetBytes(TmpInt64);
  {$ELSE}
  EnumMax := GetTypeData(CompType).MaxValue;
  ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
  Move(TmpInt64, SetVar, ResBytes);
  {$ENDIF CLR}
end;

function JclSetToInt(TypeInfo: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR};
  const SetVar): Integer;
var
  BitShift: Integer;
  TmpInt64: Int64;
  EnumMin: Integer;
  EnumMax: Integer;
  ResBytes: Integer;
  CompType: {$IFDEF CLR}TTypeInfo{$ELSE ~CLR}PTypeInfo{$ENDIF ~CLR};
begin
  CompType := GetTypeData(TypeInfo).CompType{$IFNDEF CLR}^{$ENDIF};
  EnumMin := GetTypeData(CompType).MinValue;
  EnumMax := GetTypeData(CompType).MaxValue;
  ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
  BitShift := EnumMin mod 8;
  if (EnumMax - EnumMin) > 32 then
  {$IFDEF CLR}
    raise EJclRTTIError.CreateFmt(RsRTTIValueOutOfRange,
      [IntToStr(EnumMax - EnumMin) + ' ' + RsRTTIBits]);
  TmpInt64 := BitConverter.ToInt64(TDynByteArray(SetVar), 0);
  TmpInt64 := TmpInt64 shr BitShift;
  Result := BitConverter.ToInt32(Copy(BitConverter.GetBytes(TmpInt64), 0, ResBytes), 0);
  {$ELSE}
    raise EJclRTTIError.CreateResFmt(@RsRTTIValueOutOfRange,
      [IntToStr(EnumMax - EnumMin) + ' ' + LoadResString(@RsRTTIBits)]);
  Result := 0;
  TmpInt64 := 0;
  Move(SetVar, TmpInt64, ResBytes + 1);
  TmpInt64 := TmpInt64 shr BitShift;
  Move(TmpInt64, Result, ResBytes);
  {$ENDIF CLR}
end;

{$IFNDEF CLR}

function JclGenerateSetType(BaseType: PTypeInfo;
  const TypeName: ShortString): PTypeInfo;
var
  TypeData: PTypeData;
  ValCount: Integer;
begin
  Result := AllocMem(SizeOf(TTypeInfo) + SizeOf(TOrdType) + SizeOf(PPTypeInfo));
  try
    with Result^ do
    begin
      Kind := tkSet;
      Name := TypeName;
    end;
    with GetTypeData(BaseType)^ do
      ValCount := MaxValue - MinValue + (MinValue mod 8);
    TypeData := GetTypeData(Result);
    case ValCount of
      0..8:
        TypeData^.OrdType := otUByte;
      9..16:
        TypeData^.OrdType := otUWord;
      17..32:
        TypeData^.OrdType := otULong;
      33..64:
        Byte(TypeData^.OrdType) := 8;
      65..128:
        Byte(TypeData^.OrdType) := 16;
      129..256:
        Byte(TypeData^.OrdType) := 32;
    else
      Byte(TypeData^.OrdType) := 255;
    end;
    TypeData^.CompType := AllocMem(SizeOf(Pointer));
    TypeData^.CompType^ := BaseType;
    AddType(Result);
  except
    try
      ReallocMem(Result, 0);
    except
      Result := nil;
    end;
    raise;
  end;
  ReferenceType(BaseType);
end;

//=== Is/As hooking ==========================================================

type
  PReadLoc = ^TReadLoc;
  TReadLoc = packed record
    {$IFDEF OPTIMIZATION_ON}
    Code: array [0..9] of Byte;
    {$ELSE}
    Code: array [0..17] of Byte;
    {$ENDIF OPTIMIZATION_ON}
    OpCode_Call: Byte;
    CallOffset: Longint;
  end;

  PJmp = ^TJmp;
  TJmp = packed record
    case OpCodeJmp: Byte of
      $E9:
        (JmpOffset: Longint);
      $FF:
        (OpCode2: Byte;
         EntryOffset: Longint);
  end;
  
{$ENDIF ~CLR}

// Copied from System.pas (_IsClass function)

function JclIsClass(const AnObj: TObject; const AClass: TClass): Boolean;
{$IFDEF CLR}
begin
  Result := (AnObj <> nil) and (AClass.ClassInfo.IsInstanceOfType(AnObj));
end;
{$ELSE}
asm
        { ->    EAX     left operand (class)    }
        {       EDX VMT of right operand        }
        { <-    AL      left is derived from right      }
        TEST    EAX,EAX
        JE      @@exit
@@loop:
        MOV     EAX,[EAX]
        CMP     EAX,EDX
        JE      @@success
        MOV     EAX,[EAX].vmtParent
        TEST    EAX,EAX
        JNE     @@loop
        JMP     @@exit
@@success:
        MOV     AL,1
@@exit:
end;
{$ENDIF ~CLR}

function JclIsClassByName(const AnObj: TObject; const AClass: TClass): Boolean;
var
  CurClass: TClass;
  CurClass2: TClass;
begin
  Result := AnObj <> nil;
  if Result then
  begin
    CurClass := AnObj.ClassType;
    Result := False;
    while not Result and (CurClass <> nil) do
    begin
      Result := CurClass.ClassNameIs(AClass.ClassName);
      if not Result then
        CurClass := CurClass.ClassParent;
    end;
    if CurClass <> nil then
      CurClass := CurClass.ClassParent;
    CurClass2 := AClass.ClassParent;
    while Result and (CurClass <> nil) and (CurClass2 <> nil) do
    begin
      Result := CurClass.ClassNameIs(CurClass2.ClassName);
      if Result then
      begin
        CurClass := CurClass.ClassParent;
        CurClass2 := CurClass2.ClassParent;
      end;
    end;
    Result := Result and (CurClass = CurClass2);
  end;
end;

function JclAsClass(const AnObj: TObject; const AClass: TClass): TObject;
begin
  if (AnObj = nil) or (AnObj is AClass) then
    Result := AnObj
  else
    {$IFDEF CLR}
    raise EInvalidCast.Create(SInvalidCast);
    {$ELSE}
    raise EInvalidCast.CreateRes(@SInvalidCast);
    {$ENDIF CLR}
end;

{$IFNDEF CLR}
initialization
  TypeList := TThreadList.Create;
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  ClearInfoList;
  FreeAndNil(TypeList);

{$ELSE}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

{$ENDIF ~CLR}

end.
