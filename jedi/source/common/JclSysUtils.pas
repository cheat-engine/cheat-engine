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
{ The Original Code is JclSysUtils.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All rights reserved.  }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Alexander Radchenko,                                                                           }
{   Andreas Hausladen (ahuser)                                                                     }
{   Anthony Steele                                                                                 }
{   Bernhard Berger                                                                                }
{   Heri Bender                                                                                    }
{   Jean-Fabien Connault (cycocrew)                                                                }
{   Jeroen Speldekamp                                                                              }
{   Marcel van Brakel                                                                              }
{   Peter Friese                                                                                   }
{   Petr Vones (pvones)                                                                            }
{   Python                                                                                         }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert R. Marsh                                                                                }
{   Robert Rossmair (rrossmair)                                                                    }
{   Rudy Velthuis                                                                                  }
{   Uwe Schuster (uschuster)                                                                       }
{   Wayne Sherman                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Description: Various pointer and class related routines.                                         }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-01-11 16:48:08 +0100 (ven., 11 janv. 2008)                         $ }
{ Revision:      $Rev:: 2298                                                                     $ }
{ Author:        $Author:: ahuser                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclSysUtils;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF CLR}
  Variants,
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$ENDIF CLR}
  SysUtils, Classes, TypInfo, SyncObjs,
  JclBase;

{$IFNDEF CLR}
// Pointer manipulation
procedure GetAndFillMem(var P: Pointer; const Size: Integer; const Value: Byte);
procedure FreeMemAndNil(var P: Pointer);
function PCharOrNil(const S: string): PChar;
function PAnsiCharOrNil(const S: AnsiString): PAnsiChar;
{$IFDEF SUPPORTS_WIDESTRING}
function PWideCharOrNil(const W: WideString): PWideChar;
{$ENDIF SUPPORTS_WIDESTRING}

function SizeOfMem(const APointer: Pointer): Integer;

function WriteProtectedMemory(BaseAddress, Buffer: Pointer; Size: Cardinal;
  out WrittenBytes: Cardinal): Boolean;

// Guards
type
  ISafeGuard = interface
    function ReleaseItem: Pointer;
    function GetItem: Pointer;
    procedure FreeItem;
    property Item: Pointer read GetItem;
  end;

  IMultiSafeGuard = interface (IInterface)
    function AddItem(Item: Pointer): Pointer;
    procedure FreeItem(Index: Integer);
    function GetCount: Integer;
    function GetItem(Index: Integer): Pointer;
    function ReleaseItem(Index: Integer): Pointer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: Pointer read GetItem;
  end;

function Guard(Mem: Pointer; out SafeGuard: ISafeGuard): Pointer; overload;
function Guard(Obj: TObject; out SafeGuard: ISafeGuard): TObject; overload;

function Guard(Mem: Pointer; var SafeGuard: IMultiSafeGuard): Pointer; overload;
function Guard(Obj: TObject; var SafeGuard: IMultiSafeGuard): TObject; overload;

function GuardGetMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;
function GuardAllocMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;

{ Shared memory between processes functions }

// Functions for the shared memory owner
type
  ESharedMemError = class(EJclError);

{$IFDEF MSWINDOWS}

{ SharedGetMem return ERROR_ALREADY_EXISTS if the shared memory is already
  allocated, otherwise it returns 0.
  Throws ESharedMemError if the Name is invalid. }
function SharedGetMem(var p{: Pointer}; const Name: string; Size: Cardinal;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Integer;

{ SharedAllocMem calls SharedGetMem and then fills the memory with zero if
  it was not already allocated.
  Throws ESharedMemError if the Name is invalid. }
function SharedAllocMem(const Name: string; Size: Cardinal;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Pointer;

{ SharedFreeMem releases the shared memory if it was the last reference. }
function SharedFreeMem(var p{: Pointer}): Boolean;

// Functions for the shared memory user

{ SharedOpenMem returns True if the shared memory was already allocated by
  SharedGetMem or SharedAllocMem. Otherwise it returns False.
  Throws ESharedMemError if the Name is invalid. }

function SharedOpenMem(var p{: Pointer}; const Name: string;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Boolean; overload;

{ SharedOpenMem return nil if the shared memory was not already allocated
  by SharedGetMem or SharedAllocMem.
  Throws ESharedMemError if the Name is invalid. }
function SharedOpenMem(const Name: string;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Pointer; overload;

{ SharedCloseMem releases the shared memory if it was the last reference. }
function SharedCloseMem(var p{: Pointer}): Boolean;

{$ENDIF MSWINDOWS}

// Binary search
function SearchSortedList(List: TList; SortFunc: TListSortCompare; Item: Pointer;
  Nearest: Boolean = False): Integer;

type
  TUntypedSearchCompare = function(Param: Pointer; ItemIndex: Integer; const Value): Integer;

function SearchSortedUntyped(Param: Pointer; ItemCount: Integer; SearchFunc: TUntypedSearchCompare;
  const Value; Nearest: Boolean = False): Integer;

// Dynamic array sort and search routines
type
  TDynArraySortCompare = function (Item1, Item2: Pointer): Integer;

procedure SortDynArray(const ArrayPtr: Pointer; ElementSize: Cardinal; SortFunc: TDynArraySortCompare);
// Usage: SortDynArray(Array, SizeOf(Array[0]), SortFunction);
function SearchDynArray(const ArrayPtr: Pointer; ElementSize: Cardinal; SortFunc: TDynArraySortCompare;
  ValuePtr: Pointer; Nearest: Boolean = False): Integer;
// Usage: SearchDynArray(Array, SizeOf(Array[0]), SortFunction, @SearchedValue);

{ Various compare functions for basic types }

function DynArrayCompareByte(Item1, Item2: Pointer): Integer;
function DynArrayCompareShortInt(Item1, Item2: Pointer): Integer;
function DynArrayCompareWord(Item1, Item2: Pointer): Integer;
function DynArrayCompareSmallInt(Item1, Item2: Pointer): Integer;
function DynArrayCompareInteger(Item1, Item2: Pointer): Integer;
function DynArrayCompareCardinal(Item1, Item2: Pointer): Integer;
function DynArrayCompareInt64(Item1, Item2: Pointer): Integer;

function DynArrayCompareSingle(Item1, Item2: Pointer): Integer;
function DynArrayCompareDouble(Item1, Item2: Pointer): Integer;
function DynArrayCompareExtended(Item1, Item2: Pointer): Integer;
function DynArrayCompareFloat(Item1, Item2: Pointer): Integer;

function DynArrayCompareAnsiString(Item1, Item2: Pointer): Integer;
function DynArrayCompareAnsiText(Item1, Item2: Pointer): Integer;
function DynArrayCompareString(Item1, Item2: Pointer): Integer;
function DynArrayCompareText(Item1, Item2: Pointer): Integer;
{$ENDIF ~CLR}

// Object lists
procedure ClearObjectList(List: TList);
procedure FreeObjectList(var List: TList);

{$IFNDEF CLR}
// Reference memory stream
type
  TJclReferenceMemoryStream = class(TCustomMemoryStream)
  public
    constructor Create(const Ptr: Pointer; Size: Longint);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;
{$ENDIF ~CLR}

// AutoPtr
type
  IAutoPtr = interface
    {$IFNDEF CLR}
    { Returns the object as pointer, so it is easier to assign it to a variable }
    function AsPointer: Pointer;
    {$ENDIF ~CLR}
    { Returns the AutoPtr handled object }
    function AsObject: TObject;
    { Releases the object from the AutoPtr. The AutoPtr looses the control over
      the object. }
    function ReleaseObject: TObject;
  end;

function CreateAutoPtr(Value: TObject): IAutoPtr;

// Replacement for the C ternary conditional operator ? :
function Iff(const Condition: Boolean; const TruePart, FalsePart: string): string; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Char): Char; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Byte): Byte; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Integer): Integer; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Cardinal): Cardinal; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Float): Float; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Boolean): Boolean; overload;
{$IFNDEF CLR}
function Iff(const Condition: Boolean; const TruePart, FalsePart: Pointer): Pointer; overload;
{$ENDIF ~CLR}
function Iff(const Condition: Boolean; const TruePart, FalsePart: Int64): Int64; overload;
{$IFDEF SUPPORTS_VARIANT}
{$IFDEF COMPILER6_UP}  { TODO -cFPC : Check FPC }
// because Compiler 5 can not differentiate between Variant and Byte, Integer, ... in case of overload
function Iff(const Condition: Boolean; const TruePart, FalsePart: Variant): Variant; overload;
{$ENDIF COMPILER6_UP}
{$ENDIF SUPPORTS_VARIANT}

{$IFNDEF CLR}
// Classes information and manipulation
type
  EJclVMTError = class(EJclError);

// Virtual Methods
{$IFNDEF FPC}
function GetVirtualMethodCount(AClass: TClass): Integer;
{$ENDIF ~FPC}
function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;
procedure SetVirtualMethod(AClass: TClass; const Index: Integer; const Method: Pointer);

// Dynamic Methods
type
  TDynamicIndexList = array [0..MaxInt div 16] of Word;
  PDynamicIndexList = ^TDynamicIndexList;
  TDynamicAddressList = array [0..MaxInt div 16] of Pointer;
  PDynamicAddressList = ^TDynamicAddressList;

function GetDynamicMethodCount(AClass: TClass): Integer;
function GetDynamicIndexList(AClass: TClass): PDynamicIndexList;
function GetDynamicAddressList(AClass: TClass): PDynamicAddressList;
function HasDynamicMethod(AClass: TClass; Index: Integer): Boolean;
{$IFNDEF FPC}
function GetDynamicMethod(AClass: TClass; Index: Integer): Pointer;
{$ENDIF ~FPC}

{ init table methods }

function GetInitTable(AClass: TClass): PTypeInfo;

{ field table methods }

type
  PFieldEntry = ^TFieldEntry;
  TFieldEntry = packed record
    OffSet: Integer;
    IDX: Word;
    Name: ShortString;
  end;

  PFieldClassTable = ^TFieldClassTable;
  TFieldClassTable = packed record
    Count: Smallint;
    Classes: array [0..8191] of ^TPersistentClass;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    EntryCount: Word;
    FieldClassTable: PFieldClassTable;
    FirstEntry: TFieldEntry;
   {Entries: array [1..65534] of TFieldEntry;}
  end;

function GetFieldTable(AClass: TClass): PFieldTable;

{ method table }

type
  PMethodEntry = ^TMethodEntry;
  TMethodEntry = packed record
    EntrySize: Word;
    Address: Pointer;
    Name: ShortString;
  end;

  PMethodTable = ^TMethodTable;
  TMethodTable = packed record
    Count: Word;
    FirstEntry: TMethodEntry;
   {Entries: array [1..65534] of TMethodEntry;}
  end;

function GetMethodTable(AClass: TClass): PMethodTable;
function GetMethodEntry(MethodTable: PMethodTable; Index: Integer): PMethodEntry;

// Class Parent
procedure SetClassParent(AClass: TClass; NewClassParent: TClass);
function GetClassParent(AClass: TClass): TClass;

{$IFNDEF FPC}
function IsClass(Address: Pointer): Boolean;
function IsObject(Address: Pointer): Boolean;
{$ENDIF ~FPC}

// Interface information
function GetImplementorOfInterface(const I: IInterface): TObject;
{$ENDIF ~CLR}

// Numeric formatting routines
type
  TDigitCount = 0..255;
  TDigitValue = -1..35;  // invalid, '0'..'9', 'A'..'Z'
  TNumericSystemBase = 2..Succ(High(TDigitValue));

  TJclNumericFormat = class(TObject)
  private
    FWantedPrecision: TDigitCount;
    FPrecision: TDigitCount;
    FNumberOfFractionalDigits: TDigitCount;
    FExpDivision: Integer;
    FDigitBlockSize: TDigitCount;
    FWidth: TDigitCount;
    FSignChars: array [Boolean] of Char;
    FBase: TNumericSystemBase;
    FFractionalPartSeparator: Char;
    FDigitBlockSeparator: Char;
    FShowPositiveSign: Boolean;
    FPaddingChar: Char;
    FMultiplier: string;
    function GetDigitValue(Digit: Char): Integer;
    function GetNegativeSign: Char;
    function GetPositiveSign: Char;
    procedure InvalidDigit(Digit: Char);
    procedure SetPrecision(const Value: TDigitCount);
    procedure SetBase(const Value: TNumericSystemBase);
    procedure SetNegativeSign(const Value: Char);
    procedure SetPositiveSign(const Value: Char);
    procedure SetExpDivision(const Value: Integer);
  protected
    function IntToStr(const Value: Int64; out FirstDigitPos: Integer): string; overload;
    function ShowSign(const Value: Float): Boolean; overload;
    function ShowSign(const Value: Int64): Boolean; overload;
    function SignChar(const Value: Float): Char; overload;
    function SignChar(const Value: Int64): Char; overload;
    property WantedPrecision: TDigitCount read FWantedPrecision;
  public
    constructor Create;
    function Digit(DigitValue: TDigitValue): Char;
    function DigitValue(Digit: Char): TDigitValue;
    function IsDigit(Value: Char): Boolean;
    function Sign(Value: Char): Integer;
    procedure GetMantissaExp(const Value: Float; out Mantissa: string; out Exponent: Integer);
    function FloatToHTML(const Value: Float): string;
    function IntToStr(const Value: Int64): string; overload;
    function FloatToStr(const Value: Float): string; overload;
    function StrToInt(const Value: string): Int64;
    property Base: TNumericSystemBase read FBase write SetBase;
    property Precision: TDigitCount read FPrecision write SetPrecision;
    property NumberOfFractionalDigits: TDigitCount read FNumberOfFractionalDigits write FNumberOfFractionalDigits;
    property ExponentDivision: Integer read FExpDivision write SetExpDivision;
    property DigitBlockSize: TDigitCount read FDigitBlockSize write FDigitBlockSize;
    property DigitBlockSeparator: Char read FDigitBlockSeparator write FDigitBlockSeparator;
    property FractionalPartSeparator: Char read FFractionalPartSeparator write FFractionalPartSeparator;
    property Multiplier: string read FMultiplier write FMultiplier;
    property PaddingChar: Char read FPaddingChar write FPaddingChar;
    property ShowPositiveSign: Boolean read FShowPositiveSign write FShowPositiveSign;
    property Width: TDigitCount read FWidth write FWidth;
    property NegativeSign: Char read GetNegativeSign write SetNegativeSign;
    property PositiveSign: Char read GetPositiveSign write SetPositiveSign;
  end;

function IntToStrZeroPad(Value, Count: Integer): AnsiString;

// Child processes
type
  // e.g. TStrings.Append
  TTextHandler = procedure(const Text: string) of object;
  {$IFDEF FPC}
  PBoolean = System.PBoolean; // as opposed to Windows.PBoolean, which is a pointer to Byte?!
  {$ENDIF FPC}

{$IFNDEF CLR}

const
  ABORT_EXIT_CODE = {$IFDEF MSWINDOWS} ERROR_CANCELLED {$ELSE} 1223 {$ENDIF};

function Execute(const CommandLine: string; OutputLineCallback: TTextHandler; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil): Cardinal; overload;
function Execute(const CommandLine: string; var Output: string; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil): Cardinal; overload;

// Console Utilities
function ReadKey: Char;

// Loading of modules (DLLs)
type
{$IFDEF MSWINDOWS}
  TModuleHandle = HINST;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  TModuleHandle = Pointer;
{$ENDIF LINUX}

const
  INVALID_MODULEHANDLE_VALUE = TModuleHandle(0);

function LoadModule(var Module: TModuleHandle; FileName: string): Boolean;
function LoadModuleEx(var Module: TModuleHandle; FileName: string; Flags: Cardinal): Boolean;
procedure UnloadModule(var Module: TModuleHandle);
function GetModuleSymbol(Module: TModuleHandle; SymbolName: string): Pointer;
function GetModuleSymbolEx(Module: TModuleHandle; SymbolName: string; var Accu: Boolean): Pointer;
function ReadModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
function WriteModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
{$ENDIF ~CLR}

// Conversion Utilities
type
  EJclConversionError = class(EJclError);

function StrToBoolean(const S: string): Boolean;
function BooleanToStr(B: Boolean): string;
function IntToBool(I: Integer): Boolean;
function BoolToInt(B: Boolean): Integer;

const
  {$IFDEF MSWINDOWS}
  ListSeparator = ';';
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ListSeparator = ':';
  {$ENDIF LINUX}

// functions to handle items in a separated list of items
// add items at the end
procedure ListAddItems(var List: string; const Separator, Items: string);
// add items at the end if they are not present
procedure ListIncludeItems(var List: string; const Separator, Items: string);
// delete multiple items
procedure ListRemoveItems(var List: string; const Separator, Items: string);
// delete one item
procedure ListDelItem(var List: string; const Separator: string;
  const Index: Integer);
// return the number of item
function ListItemCount(const List, Separator: string): Integer;
// return the Nth item
function ListGetItem(const List, Separator: string;
  const Index: Integer): string;
// set the Nth item
procedure ListSetItem(var List: string; const Separator: string;
  const Index: Integer; const Value: string);
// return the index of an item
function ListItemIndex(const List, Separator, Item: string): Integer;

// RTL package information
{$IFNDEF CLR}
{$IFNDEF FPC}
function SystemTObjectInstance: LongWord;
function IsCompiledWithPackages: Boolean;
{$ENDIF ~FPC}
{$ENDIF ~CLR}

// GUID
function JclGUIDToString(const GUID: TGUID): string;
function JclStringToGUID(const S: string): TGUID;

// thread safe support

type
  TJclIntfCriticalSection = class(TObject, IInterface)
  {$IFNDEF CLR}
  private
    FCriticalSection: TCriticalSection;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
  {$ENDIF ~CLR}
  end;

{$IFNDEF CLR}
type
  TJclSimpleLog = class (TObject)
  private
    FLogFileHandle: Integer;
    FLogFileName: string;
    FLogWasEmpty: Boolean;
    function GetLogOpen: Boolean;
  protected
    function CreateDefaultFileName: string;
  public
    constructor Create(const ALogFileName: string = '');
    destructor Destroy; override;
    procedure ClearLog;
    procedure CloseLog;
    procedure OpenLog;
    procedure Write(const Text: string; Indent: Integer = 0); overload;
    procedure Write(Strings: TStrings; Indent: Integer = 0); overload;
    //Writes a line to the log file. The current timestamp is written before the line.
    procedure TimeWrite(const Text: string; Indent: Integer = 0); overload;
    procedure TimeWrite(Strings: TStrings; Indent: Integer = 0); overload;
    procedure WriteStamp(SeparatorLen: Integer = 0);
    property LogFileName: string read FLogFileName;
    property LogOpen: Boolean read GetLogOpen;
  end;

// Procedure to initialize the SimpleLog Variable
procedure InitSimpleLog (const ALogFileName: string = '');

// Global Variable to make it easier for an application wide log handling.
// Must be initialized with InitSimpleLog before using
var
  SimpleLog : TJclSimpleLog;

{$ENDIF ~CLR}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclSysUtils.pas $';
    Revision: '$Revision: 2298 $';
    Date: '$Date: 2008-01-11 16:48:08 +0100 (ven., 11 janv. 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNIT_TYPES}
  Types,
  {$ENDIF HAS_UNIT_TYPES}
  {$IFDEF UNIX}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ELSE ~HAS_UNIT_LIBC}
  dl,
  {$ENDIF ~HAS_UNIT_LIBC}
  {$ENDIF UNIX}
  {$IFDEF CLR}
  System.Text,
  {$ELSE}
  {$IFDEF MSWINDOWS}
  JclConsole,
  {$ENDIF MSWINDOWS}
  {$ENDIF CLR}
  Contnrs,
  JclFileUtils, JclMath, JclResources, JclStrings, JclSysInfo;

{$IFNDEF CLR}

// Pointer manipulation
procedure GetAndFillMem(var P: Pointer; const Size: Integer; const Value: Byte);
begin
  GetMem(P, Size);
  FillChar(P^, Size, Value);
end;

procedure FreeMemAndNil(var P: Pointer);
var
  Q: Pointer;
begin
  Q := P;
  P := nil;
  FreeMem(Q);
end;

function PCharOrNil(const S: string): PChar;
begin
  Result := Pointer(S);
end;

function PAnsiCharOrNil(const S: AnsiString): PAnsiChar;
begin
  Result := Pointer(S);
end;

{$IFDEF SUPPORTS_WIDESTRING}

function PWideCharOrNil(const W: WideString): PWideChar;
begin
  Result := Pointer(W);
end;

{$ENDIF SUPPORTS_WIDESTRING}

{$IFDEF MSWINDOWS}
type
  PUsed = ^TUsed;
  TUsed = record
    SizeFlags: Integer;
  end;

const
  cThisUsedFlag = 2;
  cPrevFreeFlag = 1;
  cFillerFlag   = Integer($80000000);
  cFlags        = cThisUsedFlag or cPrevFreeFlag or cFillerFlag;

function SizeOfMem(const APointer: Pointer): Integer;
var
  U: PUsed;
begin
  if IsMemoryManagerSet then
    Result:= -1
  else
  begin
    Result := 0;
    if APointer <> nil then
    begin
      U := APointer;
      U := PUsed(PChar(U) - SizeOf(TUsed));
      if (U.SizeFlags and cThisUsedFlag) <> 0 then
        Result := (U.SizeFlags) and (not cFlags - SizeOf(TUsed));
    end;
  end;
end;
{$ENDIF MSWINDOWS}

{$IFDEF LINUX}
function SizeOfMem(const APointer: Pointer): Integer;
begin
  if IsMemoryManagerSet then
    Result:= -1
  else
  begin
    if APointer <> nil then
      Result := malloc_usable_size(APointer)
    else
      Result := 0;
  end;
end;
{$ENDIF LINUX}

function WriteProtectedMemory(BaseAddress, Buffer: Pointer;
  Size: Cardinal; out WrittenBytes: Cardinal): Boolean;
{$IFDEF MSWINDOWS}
var
  OldProtect, Dummy: Cardinal;
begin
  WrittenBytes := 0;
  if Size > 0 then
  begin
    // (outchy) VirtualProtect for DEP issues
    Result := VirtualProtect(BaseAddress, Size, PAGE_EXECUTE_READWRITE, OldProtect);
    if Result then
    try
      Move(Buffer^, BaseAddress^, Size);
      WrittenBytes := Size;
      if OldProtect in [PAGE_EXECUTE, PAGE_EXECUTE_READ, PAGE_EXECUTE_READWRITE, PAGE_EXECUTE_WRITECOPY] then
        FlushInstructionCache(GetCurrentProcess, BaseAddress, Size);
    finally
      VirtualProtect(BaseAddress, Size, OldProtect, Dummy);
    end;
  end;
  Result := WrittenBytes = Size;
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{ TODO -cHelp : Author: Andreas Hausladen }
{ TODO : Works so far, but causes app to hang on termination }
var
  AlignedAddress: Cardinal;
  PageSize, ProtectSize: Cardinal;
begin
  Result := False;
  WrittenBytes := 0;

  PageSize := Cardinal(getpagesize);
  AlignedAddress := Cardinal(BaseAddress) and not (PageSize - 1); // start memory page
  // get the number of needed memory pages
  ProtectSize := PageSize;
  while Cardinal(BaseAddress) + Size > AlignedAddress + ProtectSize do
    Inc(ProtectSize, PageSize);

  if mprotect(Pointer(AlignedAddress), ProtectSize,
    PROT_READ or PROT_WRITE or PROT_EXEC) = 0 then // obtain write access
  begin
    try
      Move(Buffer^, BaseAddress^, Size); // replace code
      Result := True;
      WrittenBytes := Size;
    finally
      // Is there any function that returns the current page protection?
//    mprotect(p, ProtectSize, PROT_READ or PROT_EXEC); // lock memory page
    end;
  end;
end;

procedure FlushInstructionCache;
{ TODO -cHelp : Author: Andreas Hausladen }
begin
  // do nothing
end;

{$ENDIF LINUX}

// Guards
type
  TSafeGuard = class(TInterfacedObject, ISafeGuard)
  private
    FItem: Pointer;
  public
    constructor Create(Mem: Pointer);
    destructor Destroy; override;
    function ReleaseItem: Pointer;
    function GetItem: Pointer;
    procedure FreeItem; virtual;
  end;

  TObjSafeGuard = class(TSafeGuard, ISafeGuard)
  public
    constructor Create(Obj: TObject);
    procedure FreeItem; override;
  end;

  TMultiSafeGuard = class(TInterfacedObject, IMultiSafeGuard)
  private
    FItems: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddItem(Mem: Pointer): Pointer;
    procedure FreeItem(Index: Integer); virtual;
    function GetCount: Integer;
    function GetItem(Index: Integer): Pointer;
    function ReleaseItem(Index: Integer): Pointer;
  end;

  TObjMultiSafeGuard = class(TMultiSafeGuard, IMultiSafeGuard)
  public
    procedure FreeItem(Index: Integer); override;
  end;

//=== { TSafeGuard } =========================================================

constructor TSafeGuard.Create(Mem: Pointer);
begin
  inherited Create;
  FItem := Mem;
end;

destructor TSafeGuard.Destroy;
begin
  FreeItem;
  inherited Destroy;
end;

function TSafeGuard.ReleaseItem: Pointer;
begin
  Result := FItem;
  FItem := nil;
end;

function TSafeGuard.GetItem: Pointer;
begin
  Result := FItem;
end;

procedure TSafeGuard.FreeItem;
begin
  if FItem <> nil then
    FreeMem(FItem);
  FItem := nil;
end;

//=== { TObjSafeGuard } ======================================================

constructor TObjSafeGuard.Create(Obj: TObject);
begin
  inherited Create(Pointer(Obj));
end;

procedure TObjSafeGuard.FreeItem;
begin
  if FItem <> nil then
  begin
    TObject(FItem).Free;
    FItem := nil;
  end;
end;

//=== { TMultiSafeGuard } ====================================================

constructor TMultiSafeGuard.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TMultiSafeGuard.Destroy;
var
  I: Integer;
begin
  for I := FItems.Count - 1 downto 0 do
    FreeItem(I);
  FItems.Free;
  inherited Destroy;
end;

function TMultiSafeGuard.AddItem(Mem: Pointer): Pointer;
begin
  Result := Mem;
  FItems.Add(Mem);
end;

procedure TMultiSafeGuard.FreeItem(Index: Integer);
begin
  FreeMem(FItems[Index]);
  FItems.Delete(Index);
end;

function TMultiSafeGuard.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TMultiSafeGuard.GetItem(Index: Integer): Pointer;
begin
  Result := FItems[Index];
end;

function TMultiSafeGuard.ReleaseItem(Index: Integer): Pointer;
begin
  Result := FItems[Index];
  FItems.Delete(Index);
end;

function Guard(Mem: Pointer; var SafeGuard: IMultiSafeGuard): Pointer; overload;
begin
  if SafeGuard = nil then
    SafeGuard := TMultiSafeGuard.Create;
  Result := SafeGuard.AddItem(Mem);
end;

//=== { TObjMultiSafeGuard } =================================================

procedure TObjMultiSafeGuard.FreeItem(Index: Integer);
begin
  TObject(FItems[Index]).Free;
  FItems.Delete(Index);
end;

function Guard(Obj: TObject; var SafeGuard: IMultiSafeGuard): TObject; overload;
begin
  if SafeGuard = nil then
    SafeGuard := TObjMultiSafeGuard.Create;
  Result := SafeGuard.AddItem(Obj);
end;

function Guard(Mem: Pointer; out SafeGuard: ISafeGuard): Pointer; overload;
begin
  Result := Mem;
  SafeGuard := TSafeGuard.Create(Mem);
end;

function Guard(Obj: TObject; out SafeGuard: ISafeGuard): TObject; overload;
begin
  Result := Obj;
  SafeGuard := TObjSafeGuard.Create(Obj);
end;

function GuardGetMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;
begin
  GetMem(Result, Size);
  Guard(Result, SafeGuard);
end;

function GuardAllocMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;
begin
  Result := AllocMem(Size);
  Guard(Result, SafeGuard);
end;

//=== Shared memory functions ================================================

type
  PMMFHandleListItem = ^TMMFHandleListItem;
  TMMFHandleListItem = record
    Next: PMMFHandleListItem;
    Memory: Pointer;
    Handle: THandle;
    Name: string;
    References: Integer;
  end;

  PMMFHandleList = PMMFHandleListItem;

var
  MMFHandleList: PMMFHandleList = nil;
  {$IFDEF THREADSAFE}
  GlobalMMFHandleListCS: TJclIntfCriticalSection = nil;
  {$ENDIF THREADSAGE}

{$IFDEF THREADSAFE}
function GetAccessToHandleList: IInterface;
begin
  if not Assigned(GlobalMMFHandleListCS) then
    GlobalMMFHandleListCS := TJclIntfCriticalSection.Create;
  Result := GlobalMMFHandleListCS;
end;
{$ENDIF THREADSAFE}

{$IFDEF MSWINDOWS}

function SharedGetMem(var p{: Pointer}; const Name: string; Size: Cardinal;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Integer;
var
  FileMappingHandle: THandle;
  Iterate, NewListItem: PMMFHandleListItem;
  Protect: Cardinal;
  {$IFDEF THREADSAFE}
  HandleListAccess: IInterface;
  {$ENDIF THREADSAFE}
begin
  Result := 0;
  Pointer(p) := nil;

  if (GetWindowsVersion in [wvUnknown..wvWinNT4]) and
    ((Name = '') or (Pos('\', Name) > 0)) then
      raise ESharedMemError.CreateResFmt(@RsInvalidMMFName, [Name]);

  {$IFDEF THREADSAFE}
  HandleListAccess := GetAccessToHandleList;
  {$ENDIF THREADSAFE}

  // search for same name
  Iterate := MMFHandleList;
  while Iterate <> nil do
  begin
    if CompareText(Iterate^.Name, Name) = 0 then
    begin
      Inc(Iterate^.References);
      Pointer(p) := Iterate^.Memory;
      Result := ERROR_ALREADY_EXISTS;
      Exit;
    end;
    Iterate := Iterate^.Next;
  end;

  // open file mapping
  FileMappingHandle := OpenFileMapping(DesiredAccess, False, PChar(Name));
  if FileMappingHandle = 0 then
  begin
    if Size = 0 then
      raise ESharedMemError.CreateResFmt(@RsInvalidMMFEmpty, [Name]);

    Protect := PAGE_READWRITE;
    if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
       (DesiredAccess = FILE_MAP_COPY) then
      Protect := PAGE_WRITECOPY;

    FileMappingHandle := CreateFileMapping(INVALID_HANDLE_VALUE, nil, Protect,
      0, Size, PChar(Name));
  end
  else
    Result := ERROR_ALREADY_EXISTS;

  case GetLastError of
    ERROR_ALREADY_EXISTS:
      Result := ERROR_ALREADY_EXISTS;
  else
    if FileMappingHandle = 0 then
      {$IFDEF COMPILER6_UP}
      RaiseLastOSError;
      {$ELSE}
      RaiseLastWin32Error;
      {$ENDIF COMPILER6_UP}
  end;

  // map view
  Pointer(p) := MapViewOfFile(FileMappingHandle, DesiredAccess, 0, 0, Size);
  if Pointer(p) = nil then
  begin
    try
      {$IFDEF COMPILER6_UP}
      RaiseLastOSError;
      {$ELSE}
      RaiseLastWin32Error;
      {$ENDIF COMPILER6_UP}
    except
      CloseHandle(FileMappingHandle);
      raise;
    end;
  end;

  // add list item to MMFHandleList
  New(NewListItem);
  NewListItem^.Name := Name;
  NewListItem^.Handle := FileMappingHandle;
  NewListItem^.Memory := Pointer(p);
  NewListItem^.References := 1;

  NewListItem^.Next := MMFHandleList;
  MMFHandleList := NewListItem;
end;

function SharedAllocMem(const Name: string; Size: Cardinal;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Pointer;
begin
  if (SharedGetMem(Result, Name, Size, DesiredAccess) <> ERROR_ALREADY_EXISTS) and
    ((DesiredAccess and (FILE_MAP_WRITE or FILE_MAP_COPY)) <> 0) and
    (Size > 0) and (Result <> nil) then
      FillChar(Pointer(Result)^, Size, 0);
end;

function SharedFreeMem(var p{: Pointer}): Boolean;
var
  n, Iterate: PMMFHandleListItem;
  {$IFDEF THREADSAFE}
  HandleListAccess: IInterface;
  {$ENDIF THREADSAFE}
begin
  if Pointer(p) <> nil then
  begin
    Result := False;
    {$IFDEF THREADSAFE}
    HandleListAccess := GetAccessToHandleList;
    {$ENDIF THREADSAFE}
    Iterate := MMFHandleList;
    n := nil;
    while Iterate <> nil do
    begin
      if Iterate^.Memory = Pointer(p) then
      begin
        if Iterate^.References > 1 then
        begin
          Dec(Iterate^.References);
          Pointer(p) := nil;
          Result := True;
          Exit;
        end;

        UnmapViewOfFile(Iterate^.Memory);
        CloseHandle(Iterate^.Handle);

        if n = nil then
          MMFHandleList := Iterate^.Next
        else
          n^.Next := Iterate^.Next;

        Dispose(Iterate);
        Pointer(p) := nil;
        Result := True;
        Break;
      end;
      n := Iterate;
      Iterate := Iterate^.Next;
    end;
  end
  else
    Result := True;
end;

function SharedOpenMem(var p{: Pointer}; const Name: string;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Boolean;
begin
  Result := SharedGetMem(p, Name, 0, DesiredAccess) = ERROR_ALREADY_EXISTS;
end;

function SharedOpenMem(const Name: string;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Pointer;
begin
  SharedGetMem(Result, Name, 0, DesiredAccess);
end;

function SharedCloseMem(var p{: Pointer}): Boolean;
begin
  Result := SharedFreeMem(p);
end;

procedure FinalizeMMFHandleList;
var
  NextItem, Iterate: PMMFHandleList;
  {$IFDEF THREADSAFE}
  HandleListAccess: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  HandleListAccess := GetAccessToHandleList;
  {$ENDIF THREADSAFE}
  Iterate := MMFHandleList;
  while Iterate <> nil do
  begin
    UnmapViewOfFile(Iterate^.Memory);
    CloseHandle(Iterate^.Handle);

    NextItem := Iterate^.Next;
    Dispose(Iterate);
    Iterate := NextItem;
  end;
end;

{$ENDIF MSWINDOWS}

//=== Binary search ==========================================================

function SearchSortedList(List: TList; SortFunc: TListSortCompare; Item: Pointer; Nearest: Boolean): Integer;
var
  L, H, I, C: Integer;
  B: Boolean;
begin
  Result := -1;
  if List <> nil then
  begin
    L := 0;
    H := List.Count - 1;
    B := False;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := SortFunc(List.List^[I], Item);
      if C < 0 then
        L := I + 1
      else
      begin
        H := I - 1;
        if C = 0 then
        begin
          B := True;
          L := I;
        end;
      end;
    end;
    if B then
      Result := L
    else
    if Nearest and (H >= 0) then
      Result := H;
  end;
end;

function SearchSortedUntyped(Param: Pointer; ItemCount: Integer; SearchFunc: TUntypedSearchCompare;
  const Value; Nearest: Boolean): Integer;
var
  L, H, I, C: Integer;
  B: Boolean;
begin
  Result := -1;
  if ItemCount > 0 then
  begin
    L := 0;
    H := ItemCount - 1;
    B := False;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := SearchFunc(Param, I, Value);
      if C < 0 then
        L := I + 1
      else
      begin
        H := I - 1;
        if C = 0 then
        begin
          B := True;
          L := I;
        end;
      end;
    end;
    if B then
      Result := L
    else
    if Nearest and (H >= 0) then
      Result := H;
  end;
end;

//=== Dynamic array sort and search routines =================================

procedure SortDynArray(const ArrayPtr: Pointer; ElementSize: Cardinal; SortFunc: TDynArraySortCompare);
var
  TempBuf: TDynByteArray;

  function ArrayItemPointer(Item: Integer): Pointer;
  begin
    Result := Pointer(Cardinal(ArrayPtr) + (Cardinal(Item) * ElementSize));
  end;

  procedure QuickSort(L, R: Integer);
  var
    I, J, T: Integer;
    P, IPtr, JPtr: Pointer;
  begin
    repeat
      I := L;
      J := R;
      P := ArrayItemPointer((L + R) shr 1);
      repeat
        while SortFunc(ArrayItemPointer(I), P) < 0 do
          Inc(I);
        while SortFunc(ArrayItemPointer(J), P) > 0 do
          Dec(J);
        if I <= J then
        begin
          IPtr := ArrayItemPointer(I);
          JPtr := ArrayItemPointer(J);
          case ElementSize of
            SizeOf(Byte):
              begin
                T := PByte(IPtr)^;
                PByte(IPtr)^ := PByte(JPtr)^;
                PByte(JPtr)^ := T;
              end;
            SizeOf(Word):
              begin
                T := PWord(IPtr)^;
                PWord(IPtr)^ := PWord(JPtr)^;
                PWord(JPtr)^ := T;
              end;
            SizeOf(Integer):
              begin
                T := PInteger(IPtr)^;
                PInteger(IPtr)^ := PInteger(JPtr)^;
                PInteger(JPtr)^ := T;
              end;
          else
            Move(IPtr^, TempBuf[0], ElementSize);
            Move(JPtr^, IPtr^, ElementSize);
            Move(TempBuf[0], JPtr^, ElementSize);
          end;
          if P = IPtr then
            P := JPtr
          else
          if P = JPtr then
            P := IPtr;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if ArrayPtr <> nil then
  begin
    SetLength(TempBuf, ElementSize);
    QuickSort(0, PInteger(Cardinal(ArrayPtr) - 4)^ - 1);
  end;
end;

function SearchDynArray(const ArrayPtr: Pointer; ElementSize: Cardinal; SortFunc: TDynArraySortCompare;
  ValuePtr: Pointer; Nearest: Boolean): Integer;
var
  L, H, I, C: Integer;
  B: Boolean;
begin
  Result := -1;
  if ArrayPtr <> nil then
  begin
    L := 0;
    H := PInteger(Cardinal(ArrayPtr) - 4)^ - 1;
    B := False;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := SortFunc(Pointer(Cardinal(ArrayPtr) + (Cardinal(I) * ElementSize)), ValuePtr);
      if C < 0 then
        L := I + 1
      else
      begin
        H := I - 1;
        if C = 0 then
        begin
          B := True;
          L := I;
        end;
      end;
    end;
    if B then
      Result := L
    else
    if Nearest and (H >= 0) then
      Result := H;
  end;
end;

{ Various compare functions for basic types }

function DynArrayCompareByte(Item1, Item2: Pointer): Integer;
begin
  Result := PByte(Item1)^ - PByte(Item2)^;
end;

function DynArrayCompareShortInt(Item1, Item2: Pointer): Integer;
begin
  Result := PShortInt(Item1)^ - PShortInt(Item2)^;
end;

function DynArrayCompareWord(Item1, Item2: Pointer): Integer;
begin
  Result := PWord(Item1)^ - PWord(Item2)^;
end;

function DynArrayCompareSmallInt(Item1, Item2: Pointer): Integer;
begin
  Result := PSmallInt(Item1)^ - PSmallInt(Item2)^;
end;

function DynArrayCompareInteger(Item1, Item2: Pointer): Integer;
begin
  Result := PInteger(Item1)^ - PInteger(Item2)^;
end;

function DynArrayCompareCardinal(Item1, Item2: Pointer): Integer;
begin
  Result := PInteger(Item1)^ - PInteger(Item2)^;
end;

function DynArrayCompareInt64(Item1, Item2: Pointer): Integer;
begin
  Result := PInt64(Item1)^ - PInt64(Item2)^;
end;

function DynArrayCompareSingle(Item1, Item2: Pointer): Integer;
begin
  if PSingle(Item1)^ < PSingle(Item2)^ then
    Result := -1
  else
  if PSingle(Item1)^ > PSingle(Item2)^ then
    Result := 1
  else
    Result := 0;
end;

function DynArrayCompareDouble(Item1, Item2: Pointer): Integer;
begin
  if PDouble(Item1)^ < PDouble(Item2)^ then
    Result := -1
  else
  if PDouble(Item1)^ > PDouble(Item2)^ then
    Result := 1
  else
    Result := 0;
end;

function DynArrayCompareExtended(Item1, Item2: Pointer): Integer;
begin
  if PExtended(Item1)^ < PExtended(Item2)^ then
    Result := -1
  else
  if PExtended(Item1)^ > PExtended(Item2)^ then
    Result := 1
  else
    Result := 0;
end;

function DynArrayCompareFloat(Item1, Item2: Pointer): Integer;
begin
  if PFloat(Item1)^ < PFloat(Item2)^ then
    Result := -1
  else
  if PFloat(Item1)^ > PFloat(Item2)^ then
    Result := 1
  else
    Result := 0;
end;

function DynArrayCompareAnsiString(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareStr(PAnsiString(Item1)^, PAnsiString(Item2)^);
end;

function DynArrayCompareAnsiText(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareText(PAnsiString(Item1)^, PAnsiString(Item2)^);
end;

function DynArrayCompareString(Item1, Item2: Pointer): Integer;
begin
  Result := CompareStr(PAnsiString(Item1)^, PAnsiString(Item2)^);
end;

function DynArrayCompareText(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(PAnsiString(Item1)^, PAnsiString(Item2)^);
end;

{$ENDIF ~CLR}

//=== Object lists ===========================================================

procedure ClearObjectList(List: TList);
var
  I: Integer;
begin
  if List <> nil then
  begin
    for I := List.Count - 1 downto 0 do
    begin
      if List[I] <> nil then
      begin
        if TObject(List[I]) is TList then
        begin
          // recursively delete TList sublists
          ClearObjectList(TList(List[I]));
        end;
        TObject(List[I]).Free;
        if (not (List is TComponentList))
          and ((not(List is TObjectList)) or not TObjectList(List).OwnsObjects) then
          List[I] := nil;
      end;
    end;
    List.Clear;
  end;
end;

procedure FreeObjectList(var List: TList);
begin
  if List <> nil then
  begin
    ClearObjectList(List);
    FreeAndNil(List);
  end;
end;

//=== { TJclReferenceMemoryStream } ==========================================

{$IFNDEF CLR}

constructor TJclReferenceMemoryStream.Create(const Ptr: Pointer; Size: Longint);
begin
  {$IFDEF MSWINDOWS}
  Assert(not IsBadReadPtr(Ptr, Size));
  {$ENDIF MSWINDOWS}
  inherited Create;
  SetPointer(Ptr, Size);
end;

function TJclReferenceMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EJclError.CreateRes(@RsCannotWriteRefStream);
end;

{$ENDIF ~CLR}

//=== { TAutoPtr } ===========================================================

type
  TAutoPtr = class(TInterfacedObject, IAutoPtr)
  private
    FValue: TObject;
  public
    constructor Create(AValue: TObject);
    destructor Destroy; override;
    {$IFNDEF CLR}
    function AsPointer: Pointer;
    {$ENDIF ~CLR}
    function AsObject: TObject;
    function ReleaseObject: TObject;
  end;

function CreateAutoPtr(Value: TObject): IAutoPtr;
begin
  Result := TAutoPtr.Create(Value);
end;

constructor TAutoPtr.Create(AValue: TObject);
begin
  inherited Create;
  FValue := AValue;
end;

destructor TAutoPtr.Destroy;
begin
  FValue.Free;
  inherited Destroy;
end;

function TAutoPtr.AsObject: TObject;
begin
  Result := FValue;
end;

{$IFNDEF CLR}
function TAutoPtr.AsPointer: Pointer;
begin
  Result := FValue;
end;
{$ENDIF ~CLR}

function TAutoPtr.ReleaseObject: TObject;
begin
  Result := FValue;
  FValue := nil;
end;

//=== replacement for the C distfix operator ? : =============================

function Iff(const Condition: Boolean; const TruePart, FalsePart: string): string;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(const Condition: Boolean; const TruePart, FalsePart: Char): Char;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(const Condition: Boolean; const TruePart, FalsePart: Byte): Byte;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(const Condition: Boolean; const TruePart, FalsePart: Integer): Integer;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(const Condition: Boolean; const TruePart, FalsePart: Cardinal): Cardinal;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(const Condition: Boolean; const TruePart, FalsePart: Float): Float;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(const Condition: Boolean; const TruePart, FalsePart: Boolean): Boolean;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

{$IFNDEF CLR}
function Iff(const Condition: Boolean; const TruePart, FalsePart: Pointer): Pointer;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;
{$ENDIF ~CLR}

function Iff(const Condition: Boolean; const TruePart, FalsePart: Int64): Int64;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

{$IFDEF SUPPORTS_VARIANT}
{$IFDEF COMPILER6_UP}
function Iff(const Condition: Boolean; const TruePart, FalsePart: Variant): Variant; overload;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;
{$ENDIF COMPILER6_UP}
{$ENDIF SUPPORTS_VARIANT}

{$IFNDEF CLR}

//=== Classes information and manipulation ===================================
// Virtual Methods
// Helper method

procedure SetVMTPointer(AClass: TClass; Offset: Integer; Value: Pointer);
var
  WrittenBytes: DWORD;
  PatchAddress: PPointer;
begin
  PatchAddress := Pointer(Integer(AClass) + Offset);
  if not WriteProtectedMemory(PatchAddress, @Value, SizeOf(Value), WrittenBytes) then
    raise EJclVMTError.CreateResFmt(@RsVMTMemoryWriteError,
      [SysErrorMessage({$IFDEF FPC}GetLastOSError{$ELSE}GetLastError{$ENDIF})]);

  if WrittenBytes <> SizeOf(Pointer) then
    raise EJclVMTError.CreateResFmt(@RsVMTMemoryWriteError, [IntToStr(WrittenBytes)]);

  // make sure that everything keeps working in a dual processor setting
  // (outchy) done by WriteProtectedMemory
  // FlushInstructionCache{$IFDEF MSWINDOWS}(GetCurrentProcess, PatchAddress, SizeOf(Pointer)){$ENDIF};
end;

{$IFNDEF FPC}
function GetVirtualMethodCount(AClass: TClass): Integer;
var
  BeginVMT: Longint;
  EndVMT: Longint;
  TablePointer: Longint;
  I: Integer;
begin
  BeginVMT := Longint(AClass);

  // Scan the offset entries in the class table for the various fields,
  // namely vmtIntfTable, vmtAutoTable, ..., vmtDynamicTable
  // The last entry is always the vmtClassName, so stop once we got there
  // After the last virtual method there is one of these entries.

  EndVMT := PLongint(Longint(AClass) + vmtClassName)^;
  // Set iterator to first item behind VMT table pointer
  I := vmtSelfPtr + SizeOf(Pointer);
  repeat
    TablePointer := PLongint(Longint(AClass) + I)^;
    if (TablePointer <> 0) and (TablePointer >= BeginVMT) and
       (TablePointer < EndVMT) then
      EndVMT := Longint(TablePointer);
    Inc(I, SizeOf(Pointer));
  until I >= vmtClassName;

  Result := (EndVMT - BeginVMT) div SizeOf(Pointer);
end;
{$ENDIF ~FPC}

function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;
begin
  Result := PPointer(Integer(AClass) + Index * SizeOf(Pointer))^;
end;

procedure SetVirtualMethod(AClass: TClass; const Index: Integer; const Method: Pointer);
begin
  SetVMTPointer(AClass, Index * SizeOf(Pointer), Method);
end;

// Dynamic Methods
type
  TvmtDynamicTable = packed record
    Count: Word;
   {IndexList: array [1..Count] of Word;
    AddressList: array [1..Count] of Pointer;}
  end;

function GetDynamicMethodCount(AClass: TClass): Integer; assembler;
asm
        MOV     EAX, [EAX].vmtDynamicTable
        TEST    EAX, EAX
        JE      @@Exit
        MOVZX   EAX, WORD PTR [EAX]
@@Exit:
end;

function GetDynamicIndexList(AClass: TClass): PDynamicIndexList; assembler;
asm
        MOV     EAX, [EAX].vmtDynamicTable
        ADD     EAX, 2
end;

function GetDynamicAddressList(AClass: TClass): PDynamicAddressList; assembler;
asm
        MOV     EAX, [EAX].vmtDynamicTable
        MOVZX   EDX, Word ptr [EAX]
        ADD     EAX, EDX
        ADD     EAX, EDX
        ADD     EAX, 2
end;

function HasDynamicMethod(AClass: TClass; Index: Integer): Boolean; assembler;
// Mainly copied from System.GetDynaMethod
asm
        { ->    EAX     vmt of class            }
        {       DX      dynamic method index    }

        PUSH    EDI
        XCHG    EAX, EDX
        JMP     @@HaveVMT
@@OuterLoop:
        MOV     EDX, [EDX]
@@HaveVMT:
        MOV     EDI, [EDX].vmtDynamicTable
        TEST    EDI, EDI
        JE      @@Parent
        MOVZX   ECX, WORD PTR [EDI]
        PUSH    ECX
        ADD     EDI,2
        REPNE   SCASW
        JE      @@Found
        POP     ECX
@@Parent:
        MOV     EDX,[EDX].vmtParent
        TEST    EDX,EDX
        JNE     @@OuterLoop
        MOV     EAX, 0
        JMP     @@Exit
@@Found:
        POP     EAX
        MOV     EAX, 1
@@Exit:
        POP     EDI
end;

{$IFNDEF FPC}
function GetDynamicMethod(AClass: TClass; Index: Integer): Pointer; assembler;
asm
        CALL    System.@FindDynaClass
end;
{$ENDIF ~FPC}

//=== Interface Table ========================================================

function GetInitTable(AClass: TClass): PTypeInfo; assembler;
asm
        MOV     EAX, [EAX].vmtInitTable
end;

function GetFieldTable(AClass: TClass): PFieldTable; assembler;
asm
        MOV     EAX, [EAX].vmtFieldTable
end;

function GetMethodTable(AClass: TClass): PMethodTable; assembler;
asm
        MOV     EAX, [EAX].vmtMethodTable
end;

function GetMethodEntry(MethodTable: PMethodTable; Index: Integer): PMethodEntry;
begin
  Result := Pointer(Cardinal(MethodTable) + 2);
  for Index := Index downto 1 do
    Inc(Cardinal(Result), Result^.EntrySize);
end;

//=== Class Parent methods ===================================================

procedure SetClassParent(AClass: TClass; NewClassParent: TClass);
var
  WrittenBytes: DWORD;
  PatchAddress: Pointer;
begin
  PatchAddress := PPointer(Integer(AClass) + vmtParent)^;
  if not WriteProtectedMemory(PatchAddress, @NewClassParent, SizeOf(Pointer), WrittenBytes) then
    raise EJclVMTError.CreateResFmt(@RsVMTMemoryWriteError,
      [SysErrorMessage({$IFDEF FPC}GetLastOSError{$ELSE}GetLastError{$ENDIF})]);
  if WrittenBytes <> SizeOf(Pointer) then
    raise EJclVMTError.CreateResFmt(@RsVMTMemoryWriteError, [IntToStr(WrittenBytes)]);
  // make sure that everything keeps working in a dual processor setting
  // (outchy) done by WriteProtectedMemory
  // FlushInstructionCache{$IFDEF MSWINDOWS}(GetCurrentProcess, PatchAddress, SizeOf(Pointer)){$ENDIF};
end;

function GetClassParent(AClass: TClass): TClass; assembler;
asm
        MOV     EAX, [EAX].vmtParent
        TEST    EAX, EAX
        JE      @@Exit
        MOV     EAX, [EAX]
@@Exit:
end;

{$IFNDEF FPC}
function IsClass(Address: Pointer): Boolean; assembler;
asm
        CMP     Address, Address.vmtSelfPtr
        JNZ     @False
        MOV     Result, True
        JMP     @Exit
@False:
        MOV     Result, False
@Exit:
end;
{$ENDIF ~FPC}

{$IFNDEF FPC}
function IsObject(Address: Pointer): Boolean; assembler;
asm
// or IsClass(Pointer(Address^));
        MOV     EAX, [Address]
        CMP     EAX, EAX.vmtSelfPtr
        JNZ     @False
        MOV     Result, True
        JMP     @Exit
@False:
        MOV     Result, False
@Exit:
end;
{$ENDIF ~FPC}

//=== Interface information ==================================================

function GetImplementorOfInterface(const I: IInterface): TObject;
{ TODO -cDOC : Original code by Hallvard Vassbotn }
{ TODO -cTesting : Check the implemetation for any further version of compiler }
const
  AddByte = $04244483; // opcode for ADD DWORD PTR [ESP+4], Shortint
  AddLong = $04244481; // opcode for ADD DWORD PTR [ESP+4], Longint
type
  PAdjustSelfThunk = ^TAdjustSelfThunk;
  TAdjustSelfThunk = packed record
    case AddInstruction: Longint of
      AddByte: (AdjustmentByte: ShortInt);
      AddLong: (AdjustmentLong: Longint);
  end;
  PInterfaceMT = ^TInterfaceMT;
  TInterfaceMT = packed record
    QueryInterfaceThunk: PAdjustSelfThunk;
  end;
  TInterfaceRef = ^PInterfaceMT;
var
  QueryInterfaceThunk: PAdjustSelfThunk;
begin
  try
    Result := Pointer(I);
    if Assigned(Result) then
    begin
      QueryInterfaceThunk := TInterfaceRef(I)^.QueryInterfaceThunk;
      case QueryInterfaceThunk.AddInstruction of
        AddByte:
          Inc(PChar(Result), QueryInterfaceThunk.AdjustmentByte);
        AddLong:
          Inc(PChar(Result), QueryInterfaceThunk.AdjustmentLong);
      else
        Result := nil;
      end;
    end;
  except
    Result := nil;
  end;
end;

{$ENDIF ~CLR}

//=== Numeric formatting routines ============================================

function IntToStrZeroPad(Value, Count: Integer): AnsiString;
begin
  Result := IntToStr(Value);
  if Length(Result) < Count then
    Result := StrFillChar('0', Count - Length(Result)) + Result;
end;

//=== { TJclNumericFormat } ==================================================

{ TODO -cHelp : Author: Robert Rossmair }
{ Digit:         converts a digit value (number) to a digit (char)
  DigitValue:    converts a digit (char) into a number (digit value)
  IntToStr,
  FloatToStr,
  FloatToHTML:   converts a numeric value to a base <Base> numeric representation with formating options
  StrToIn:       converts a base <Base> numeric representation into an integer, if possible
  GetMantisseExponent: similar to AsString, but returns the Exponent separately as an integer
}
const
  {$IFDEF MATH_EXTENDED_PRECISION}
  BinaryPrecision = 64;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  BinaryPrecision = 53;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  BinaryPrecision = 24;
  {$ENDIF MATH_SINGLE_PRECISION}

constructor TJclNumericFormat.Create;
begin
  inherited Create;
  { TODO : Initialize, when possible, from locale info }
  FBase := 10;
  FExpDivision := 1;
  SetPrecision(6);
  FNumberOfFractionalDigits := BinaryPrecision;
  FSignChars[False] := '-';
  FSignChars[True] := '+';
  FPaddingChar := ' ';
  FMultiplier := '×';
  FFractionalPartSeparator := DecimalSeparator{$IFDEF CLR}[1]{$ENDIF};
  FDigitBlockSeparator := ThousandSeparator{$IFDEF CLR}[1]{$ENDIF};
end;

procedure TJclNumericFormat.InvalidDigit(Digit: Char);
begin
  {$IFDEF CLR}
  raise EConvertError.CreateFmt(RsInvalidDigit, [Base, Digit]);
  {$ELSE}
  raise EConvertError.CreateResFmt(@RsInvalidDigit, [Base, Digit]);
  {$ENDIF CLR}
end;

function TJclNumericFormat.Digit(DigitValue: TDigitValue): Char;
begin
  Assert(DigitValue < Base, Format(RsInvalidDigitValue, [Base, DigitValue]));
  if DigitValue > 9 then
    Result := Chr(Ord('A') + DigitValue - 10)
  else
    Result := Chr(Ord('0') + DigitValue);
end;

function TJclNumericFormat.GetDigitValue(Digit: Char): Integer;
begin
  Result := -1;
  {$IFDEF CLR}
  if System.Char.IsDigit(Digit) then
  {$ELSE ~CLR}
  if Digit in AnsiDecDigits then
  {$ENDIF ~CLR}
    Result := Ord(Digit) - Ord('0')
  else
  begin
    Digit := UpCase(Digit);
    {$IFDEF CLR}
    if (Digit >= 'A') and (Digit <= 'Z') then
    {$ELSE ~CLR}
    if Digit in AnsiUppercaseLetters then
    {$ENDIF ~CLR}
      Result := Ord(Digit) - Ord('A') + 10;
  end;
  if Result >= Base then
    Result := -1;
end;

function TJclNumericFormat.DigitValue(Digit: Char): TDigitValue;
begin
  Result := GetDigitValue(Digit);
  if Result = -1 then
    InvalidDigit(Digit);
end;

function TJclNumericFormat.IsDigit(Value: Char): Boolean;
begin
  Result := GetDigitValue(Value) <> -1;
end;

function TJclNumericFormat.FloatToHTML(const Value: Float): string;
var
  Mantissa: string;
  Exponent: Integer;
begin
  GetMantissaExp(Value, Mantissa, Exponent);
  Result := Format('%s %s %d<sup>%d</sup>', [Mantissa, Multiplier, Base, Exponent]);
end;

procedure TJclNumericFormat.GetMantissaExp(const Value: Float;
  out Mantissa: string; out Exponent: Integer);
const
  {$IFDEF FPC}
  InfMantissa: array [Boolean] of string[4] = ('inf', '-inf');
  {$ElSE}
  InfMantissa: array [Boolean] of string = ('inf', '-inf');
  {$ENDIF FPC}
var
  BlockDigits: TDigitCount;
  IntDigits, FracDigits: Integer;
  FirstDigitPos, Prec: Integer;
  I, J, N: Integer;
  K: Int64;
  X: Extended;
  HighDigit: Char;
  {$IFDEF CLR}
  sb: StringBuilder;
  {$ENDIF CLR}

  function GetDigit(X: Extended): Char;
  var
    N: Integer;
  begin
    N := Trunc(X);
    if N > 9 then
      Result := Chr(Ord('A') + N - 10)
    else
      Result := Chr(Ord('0') + N);
  end;

begin
  X := Abs(Value);

  if X > MaxFloatingPoint then
  begin
    Mantissa := InfMantissa[Value < 0];
    Exponent := 1;
    Exit;
  end
  else
  if X < MinFloatingPoint then
  begin
    Mantissa := Format('%.*f', [Precision, 0.0]);
    Exponent := 1;
    Exit;
  end;

  IntDigits := 1;
  Prec := Precision;

  Exponent := Trunc(LogBaseN(Base, X));
  if FExpDivision > 1 then
  begin
    N := Exponent mod FExpDivision;
    Dec(Exponent, N);
    Inc(IntDigits, N);
  end;
  X := X / Power(Base, Exponent);

  if X < 1.0 then
  begin
    Dec(Exponent, FExpDivision);
    X := X * PowerInt(Base, FExpDivision);
    Inc(IntDigits, FExpDivision - 1);
  end;

{ TODO : Here's a problem if X > High(Int64).
It *seems* to surface only if ExponentDivision > 12, but it
has not been investigated if ExponentDivision <= 12 is safe. }
  K := Trunc(X);
  if Value < 0 then
    K := -K;

  {$IFDEF CLR}
  sb := StringBuilder.Create(IntToStr(K, FirstDigitPos));;
  {$ELSE}
  Mantissa := IntToStr(K, FirstDigitPos);
  {$ENDIF CLR}

  FracDigits := Prec - IntDigits;
  if FracDigits > NumberOfFractionalDigits then
    FracDigits := NumberOfFractionalDigits;

  if FracDigits > 0 then
  begin
    {$IFDEF CLR}
    J := sb.Length + 1;
    // allocate sufficient space for point + digits + digit block separators
    sb.Length := FracDigits * 2 + J;
    sb[J - 1] := FractionalPartSeparator;
    {$ELSE}
    J := Length(Mantissa) + 1;
    // allocate sufficient space for point + digits + digit block separators
    SetLength(Mantissa, FracDigits * 2 + J);
    Mantissa[J] := FractionalPartSeparator;
    {$ENDIF CLR}
    I := J + 1;
    BlockDigits := 0;
    while FracDigits > 0 do
    begin
      if (BlockDigits > 0) and (BlockDigits = DigitBlockSize) then
      begin
        {$IFDEF CLR}
        sb[I - 1] := DigitBlockSeparator;
        {$ELSE}
        Mantissa[I] := DigitBlockSeparator;
        {$ENDIF CLR}
        Inc(I);
        BlockDigits := 0;
      end;
      X := Frac(X) * Base;
      {$IFDEF CLR}
      sb[I - 1] := GetDigit(X);
      {$ELSE}
      Mantissa[I] := GetDigit(X);
      {$ENDIF CLR}
      Inc(I);
      Inc(BlockDigits);
      Dec(FracDigits);
    end;
    {$IFDEF CLR}
    sb[I - 1] := #0;
    StrResetLength(sb);
    {$ELSE}
    Mantissa[I] := #0;
    StrResetLength(Mantissa);
    {$ENDIF CLR}
  end;

  if Frac(X) >= 0.5 then
  // round up
  begin
    HighDigit := Digit(Base - 1);
    {$IFDEF CLR}
    for I := sb.Length downto 1 do
    begin
      if sb[I - 1] = HighDigit then
        if (I = FirstDigitPos) then
        begin
          sb[I - 1] := '1';
          Inc(Exponent);
          Break;
        end
        else
          sb[I - 1] := '0'
      else
      if AnsiChar(sb[I - 1]) in [AnsiChar(DigitBlockSeparator), AnsiChar(FractionalPartSeparator)] then
        Continue
      else
      begin
        if sb[I - 1] = '9' then
          sb[I - 1] := 'A'
        else
          sb[I - 1] := Succ(sb[I - 1]);
        Break;
      end;
    end;
    {$ELSE}
    for I := Length(Mantissa) downto 1 do
    begin
      if Mantissa[I] = HighDigit then
        if (I = FirstDigitPos) then
        begin
          Mantissa[I] := '1';
          Inc(Exponent);
          Break;
        end
        else
          Mantissa[I] := '0'
      else
      if Mantissa[I] in [AnsiChar(DigitBlockSeparator), AnsiChar(FractionalPartSeparator)] then
        Continue
      else
      begin
        if Mantissa[I] = '9' then
          Mantissa[I] := 'A'
        else
          Mantissa[I] := Succ(Mantissa[I]);
        Break;
      end;
    end;
    {$ENDIF CLR}
  end;
  {$IFDEF CLR}
  Mantissa := sb.ToString();
  {$ENDIF CLR}
end;

function TJclNumericFormat.FloatToStr(const Value: Float): string;
var
  Mantissa: string;
  Exponent: Integer;
begin
  GetMantissaExp(Value, Mantissa, Exponent);
  Result := Format('%s %s %d^%d', [Mantissa, Multiplier, Base, Exponent]);
end;

function TJclNumericFormat.IntToStr(const Value: Int64): string;
var
  FirstDigitPos: Integer;
begin
  Result := IntToStr(Value, FirstDigitPos);
end;

function TJclNumericFormat.IntToStr(const Value: Int64; out FirstDigitPos: Integer): string;
const
  MaxResultLen = 64 + 63 + 1; // max. digits + max. group separators + sign
var
  Remainder: Int64;
  I, N: Integer;
  Chars, Digits: Cardinal;
  LoopFinished, HasSign, SpacePadding: Boolean;
begin
  SpacePadding := PaddingChar = ' ';
  HasSign := ShowSign(Value);
  Chars := MaxResultLen;
  if Width > Chars then
    Chars := Width;
  {$IFDEF CLR}
  Result := StrFillChar(' ', Chars);
  {$ELSE}
  SetLength(Result, Chars);
  FillChar(Result[1], Chars, ' ');
  {$ENDIF CLR}

  Remainder := Abs(Value);
  Digits := 0;

  Chars := 0;
  if HasSign then
    Chars := 1;

  I := MaxResultLen;

  while True do
  begin
    N := Remainder mod Base;
    Remainder := Remainder div Base;
    if N > 9 then
      Result[I] := Chr(Ord('A') + N - 10)
    else
      Result[I] := Chr(Ord('0') + N);
    Dec(I);
    Inc(Digits);
    Inc(Chars);
    if (Remainder = 0) and (SpacePadding or (Chars >= Width)) then
      Break;
    if (Digits = DigitBlockSize) then
    begin
      Inc(Chars);
      LoopFinished := (Remainder = 0) and (Chars = Width);
      if LoopFinished then
        Result[I] := ' '
      else
        Result[I] := DigitBlockSeparator;
      Dec(I);
      if LoopFinished then
        Break;
      Digits := 0;
    end;
  end;

  FirstDigitPos := I + 1;

  if HasSign then
    Result[I] := SignChar(Value)
  else
    Inc(I);
  N := MaxResultLen - Width + 1;
  if N < I then
    I := N;
  Result := Copy(Result, I, MaxResultLen);
  Dec(FirstDigitPos, I - 1);
end;

procedure TJclNumericFormat.SetBase(const Value: TNumericSystemBase);
begin
  FBase := Value;
  SetPrecision(FWantedPrecision);
end;

procedure TJclNumericFormat.SetExpDivision(const Value: Integer);
begin
  if Value <= 1 then
    FExpDivision := 1
  else
  // see TODO in GetMantissaExp
  if Value > 12 then
    FExpDivision := 12
  else
    FExpDivision := Value;
end;

procedure TJclNumericFormat.SetPrecision(const Value: TDigitCount);
begin
  FWantedPrecision := Value;
  // Do not display more digits than Float precision justifies
  if Base = 2 then
    FPrecision := BinaryPrecision
  else
    FPrecision := Trunc(BinaryPrecision / LogBase2(Base));
  if Value < FPrecision then
    FPrecision := Value;
end;

function TJclNumericFormat.Sign(Value: Char): Integer;
begin
  Result := 0;
  if Value = FSignChars[False] then
    Result := -1;
  if Value = FSignChars[True] then
    Result := +1;
end;

function TJclNumericFormat.StrToInt(const Value: string): Int64;
var
  I, N: Integer;
  C: Char;
begin
  Result := 0;
  N := 0;
  I := 1;
  if (Length(Value) >= I)
  {$IFDEF CLR}
    and ((Value[I] = '+') or (Value[I] = '-')) then
  {$ELSE ~CLR}
    and (Value[I] in AnsiSigns) then
  {$ENDIF ~CLR}
    Inc(I);
  for I := I to Length(Value) do
  begin
    C := Value[I];
    {$IFDEF CLR}
    if System.Char.IsDigit(C) then
    {$ELSE ~CLR}
    if C in AnsiDecDigits then
    {$ENDIF ~CLR}
      N := Ord(C) - Ord('0')
    else
    begin
      C := UpCase(C);
      {$IFDEF CLR}
      if (C >= 'A') and (C <= 'Z') then
      {$ELSE ~CLR}
      if C in AnsiUppercaseLetters then
      {$ENDIF ~CLR}
      begin
        N := Ord(C) - Ord('A') + 10;
        if N >= Base then
          InvalidDigit(C);
      end
      else
        if C = DigitBlockSeparator then
          Continue
      else
        InvalidDigit(C);
    end;
    Result := Result * Base + N;
  end;
  if Value[1] = '-' then
    Result := -Result;
end;

function TJclNumericFormat.ShowSign(const Value: Float): Boolean;
begin
  Result := FShowPositiveSign or (Value < 0);
end;

function TJclNumericFormat.ShowSign(const Value: Int64): Boolean;
begin
  Result := FShowPositiveSign or (Value < 0);
end;

function TJclNumericFormat.SignChar(const Value: Float): Char;
begin
  Result := FSignChars[Value >= 0];
end;

function TJclNumericFormat.SignChar(const Value: Int64): Char;
begin
  Result := FSignChars[Value >= 0];
end;

function TJclNumericFormat.GetNegativeSign: Char;
begin
  Result := FSignChars[False];
end;

function TJclNumericFormat.GetPositiveSign: Char;
begin
  Result := FSignChars[True];
end;

procedure TJclNumericFormat.SetNegativeSign(const Value: Char);
begin
  FSignChars[False] := Value;
end;

procedure TJclNumericFormat.SetPositiveSign(const Value: Char);
begin
  FSignChars[True] := Value;
end;

{$IFNDEF CLR}
//=== Child processes ========================================================

// MuteCRTerminatedLines was "outsourced" from Win32ExecAndRedirectOutput

function MuteCRTerminatedLines(const RawOutput: string): string;
const
  Delta = 1024;
var
  BufPos, OutPos, LfPos, EndPos: Integer;
  C: Char;
begin
  SetLength(Result, Length(RawOutput));
  OutPos := 1;
  LfPos := OutPos;
  EndPos := OutPos;
  for BufPos := 1 to Length(RawOutput) do
  begin
    if OutPos >= Length(Result)-2 then
      SetLength(Result, Length(Result) + Delta);
    C := RawOutput[BufPos];
    case C of
      AnsiCarriageReturn:
        OutPos := LfPos;
      AnsiLineFeed:
        begin
          OutPos := EndPos;
          Result[OutPos] := AnsiCarriageReturn;
          Inc(OutPos);
          Result[OutPos] := C;
          Inc(OutPos);
          EndPos := OutPos;
          LfPos := OutPos;
        end;
    else
      Result[OutPos] := C;
      Inc(OutPos);
      EndPos := OutPos;
    end;
  end;
  SetLength(Result, OutPos - 1);
end;

function InternalExecute(const CommandLine: string; var Output: string; OutputLineCallback: TTextHandler;
  RawOutput: Boolean; AbortPtr: PBoolean): Cardinal;
const
  BufferSize = 255;
var
  Buffer: array [0..BufferSize] of Char;
  TempOutput: string;
  PipeBytesRead: Cardinal;

  procedure ProcessLine(LineEnd: Integer);
  begin
    if RawOutput or (TempOutput[LineEnd] <> AnsiCarriageReturn) then
    begin
      while (LineEnd > 0) and (TempOutput[LineEnd] in [AnsiLineFeed, AnsiCarriageReturn]) do
        Dec(LineEnd);
      OutputLineCallback(Copy(TempOutput, 1, LineEnd));
    end;
  end;

  procedure ProcessBuffer;
  var
    CR, LF: Integer;
  begin
    Buffer[PipeBytesRead] := #0;
    TempOutput := TempOutput + Buffer;
    if Assigned(OutputLineCallback) then
    repeat
      CR := Pos(AnsiCarriageReturn, TempOutput);
      if CR = Length(TempOutput) then
        CR := 0;        // line feed at CR + 1 might be missing
      LF := Pos(AnsiLineFeed, TempOutput);
      if (CR > 0) and ((LF > CR + 1) or (LF = 0)) then
        LF := CR;       // accept CR as line end
      if LF > 0 then
      begin
        ProcessLine(LF);
        Delete(TempOutput, 1, LF);
      end;
    until LF = 0;
  end;

{$IFDEF MSWINDOWS}
// "outsourced" from Win32ExecAndRedirectOutput
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
  PipeRead, PipeWrite: THandle;
begin
  Result := $FFFFFFFF;
  SecurityAttr.nLength := SizeOf(SecurityAttr);
  SecurityAttr.lpSecurityDescriptor := nil;
  SecurityAttr.bInheritHandle := True;
  if not CreatePipe(PipeRead, PipeWrite, @SecurityAttr, 0) then
  begin
    Result := GetLastError;
    Exit;
  end;
  FillChar(StartupInfo, SizeOf(TStartupInfo), #0);
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  StartupInfo.hStdOutput := PipeWrite;
  StartupInfo.hStdError := PipeWrite;
  if CreateProcess(nil, PChar(CommandLine), nil, nil, True, NORMAL_PRIORITY_CLASS,
    nil, nil, StartupInfo, ProcessInfo) then
  begin
    CloseHandle(PipeWrite);
    if AbortPtr <> nil then
      AbortPtr^ := False;
    while ((AbortPtr = nil) or not AbortPtr^) and
      ReadFile(PipeRead, Buffer, BufferSize, PipeBytesRead, nil) and (PipeBytesRead > 0) do
      ProcessBuffer;
    if (AbortPtr <> nil) and AbortPtr^ then
      TerminateProcess(ProcessInfo.hProcess, Cardinal(ABORT_EXIT_CODE));
    if (WaitForSingleObject(ProcessInfo.hProcess, INFINITE) = WAIT_OBJECT_0) and
      not GetExitCodeProcess(ProcessInfo.hProcess, Result) then
        Result := $FFFFFFFF;
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
  end
  else
    CloseHandle(PipeWrite);
  CloseHandle(PipeRead);
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
var
  Pipe: PIOFile;
  Cmd: string;
begin
  Cmd := Format('%s 2>&1', [CommandLine]);
  Pipe := Libc.popen(PChar(Cmd), 'r');
  { TODO : handle Abort }
  repeat
    PipeBytesRead := fread_unlocked(@Buffer, 1, BufferSize, Pipe);
    if PipeBytesRead > 0 then
      ProcessBuffer;
  until PipeBytesRead = 0;
  Result := pclose(Pipe);
  wait(nil);
{$ENDIF UNIX}
  if TempOutput <> '' then
    if Assigned(OutputLineCallback) then
      // output wasn't terminated by a line feed...
      // (shouldn't happen, but you never know)
      ProcessLine(Length(TempOutput))
    else
      if RawOutput then
        Output := Output + TempOutput
      else
        Output := Output + MuteCRTerminatedLines(TempOutput);
end;

{ TODO -cHelp :
RawOutput: Do not process isolated carriage returns (#13).
That is, for RawOutput = False, lines not terminated by a line feed (#10) are deleted from Output. }

function Execute(const CommandLine: string; var Output: string; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil): Cardinal;
begin
  Result := InternalExecute(CommandLine, Output, nil, RawOutput, AbortPtr);
end;

{ TODO -cHelp :
Author: Robert Rossmair
OutputLineCallback called once per line of output. }

function Execute(const CommandLine: string; OutputLineCallback: TTextHandler; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil): Cardinal; overload;
var
  Dummy: string;
begin
  Result := InternalExecute(CommandLine, Dummy, OutputLineCallback, RawOutput, AbortPtr);
end;

//=== Console Utilities ======================================================

function ReadKey: Char;
{$IFDEF MSWINDOWS}
{ TODO -cHelp : Contributor: Robert Rossmair }
var
  Console: TJclConsole;
  InputMode: TJclConsoleInputModes;
begin
  Console := TJclConsole.Default;
  InputMode := Console.Input.Mode;
  Console.Input.Mode := [imProcessed];
  Console.Input.Clear;
  Result := Console.Input.GetEvent.Event.KeyEvent.AsciiChar;
  Console.Input.Mode := InputMode;
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
{ TODO -cHelp : Donator: Wayne Sherman }
var
  ReadFileDescriptor: TFDSet;
  TimeVal: TTimeVal;
  SaveTerminalSettings: TTermIos;
  RawTerminalSettings: TTermIos;
begin
  Result := #0;

  //Save Original Terminal Settings
  tcgetattr(stdin, SaveTerminalSettings);
  tcgetattr(stdin, RawTerminalSettings);

  //Put Terminal in RAW mode
  cfmakeraw(RawTerminalSettings);
  tcsetattr(stdin, TCSANOW, RawTerminalSettings);
  try
    //Setup file I/O descriptor for STDIN
    FD_ZERO(ReadFileDescriptor);
    FD_SET(stdin, ReadFileDescriptor);
    TimeVal.tv_sec := High(LongInt); //wait forever
    TimeVal.tv_usec := 0;

    //clear keyboard buffer first
    TCFlush(stdin, TCIFLUSH);

    //wait for a key to be pressed
    if select(1, @ReadFileDescriptor, nil, nil, @TimeVal) > 0 then
    begin
      //Now read the character
      Result := Char(getchar);
    end
    else
      raise EJclError.CreateRes(@RsReadKeyError);
  finally
    //Restore Original Terminal Settings
    tcsetattr(stdin, TCSANOW, SaveTerminalSettings);
  end;
end;
{$ENDIF UNIX}
{$ENDIF ~CLR}

{$IFNDEF CLR}

//=== Loading of modules (DLLs) ==============================================

function LoadModule(var Module: TModuleHandle; FileName: string): Boolean;
{$IFDEF MSWINDOWS}
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := SafeLoadLibrary(FileName);
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := dlopen(PChar(FileName), RTLD_NOW);
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;
{$ENDIF UNIX}

function LoadModuleEx(var Module: TModuleHandle; FileName: string; Flags: Cardinal): Boolean;
{$IFDEF MSWINDOWS}
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := LoadLibraryEx(PChar(FileName), 0, Flags); // SafeLoadLibrary?
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := dlopen(PChar(FileName), Flags);
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;
{$ENDIF UNIX}

procedure UnloadModule(var Module: TModuleHandle);
{$IFDEF MSWINDOWS}
begin
  if Module <> INVALID_MODULEHANDLE_VALUE then
    FreeLibrary(Module);
  Module := INVALID_MODULEHANDLE_VALUE;
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  if Module <> INVALID_MODULEHANDLE_VALUE then
    dlclose(Pointer(Module));
  Module := INVALID_MODULEHANDLE_VALUE;
end;
{$ENDIF UNIX}

function GetModuleSymbol(Module: TModuleHandle; SymbolName: string): Pointer;
{$IFDEF MSWINDOWS}
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := GetProcAddress(Module, PChar(SymbolName));
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := dlsym(Module, PChar(SymbolName));
end;
{$ENDIF UNIX}

function GetModuleSymbolEx(Module: TModuleHandle; SymbolName: string; var Accu: Boolean): Pointer;
{$IFDEF MSWINDOWS}
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := GetProcAddress(Module, PChar(SymbolName));
  Accu := Accu and (Result <> nil);
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := dlsym(Module, PChar(SymbolName));
  Accu := Accu and (Result <> nil);
end;
{$ENDIF UNIX}

function ReadModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
var
  Sym: Pointer;
begin
  Result := True;
  Sym := GetModuleSymbolEx(Module, SymbolName, Result);
  if Result then
    Move(Sym^, Buffer, Size);
end;

function WriteModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
var
  Sym: Pointer;
begin
  Result := True;
  Sym := GetModuleSymbolEx(Module, SymbolName, Result);
  if Result then
    Move(Buffer, Sym^, Size);
end;

{$ENDIF ~CLR}

//=== Conversion Utilities ===================================================

const
  DefaultTrueBoolStr  = 'True';  // DO NOT LOCALIZE
  DefaultFalseBoolStr = 'False'; // DO NOT LOCALIZE

  DefaultYesBoolStr   = 'Yes';   // DO NOT LOCALIZE
  DefaultNoBoolStr    = 'No';    // DO NOT LOCALIZE

function StrToBoolean(const S: string): Boolean;
var
  LowerCasedText: string;
begin
  { TODO : Possibility to add localized strings, like in Delphi 7 }
  { TODO : Lower case constants }
  LowerCasedText := LowerCase(S);
  Result := ((S = '1') or
    (LowerCasedText = LowerCase(DefaultTrueBoolStr)) or (LowerCasedText = LowerCase(DefaultYesBoolStr))) or
    (LowerCasedText = LowerCase(DefaultTrueBoolStr[1])) or (LowerCasedText = LowerCase(DefaultYesBoolStr[1]));
  if not Result then
  begin
    Result := not ((S = '0') or
      (LowerCasedText = LowerCase(DefaultFalseBoolStr)) or (LowerCasedText = LowerCase(DefaultNoBoolStr)) or
      (LowerCasedText = LowerCase(DefaultFalseBoolStr[1])) or (LowerCasedText = LowerCase(DefaultNoBoolStr[1])));
    if Result then
      {$IFDEF CLR}
      raise EJclConversionError.CreateFmt(RsStringToBoolean, [S]);
      {$ELSE}
      raise EJclConversionError.CreateResFmt(@RsStringToBoolean, [S]);
      {$ENDIF CLR}
  end;
end;

function BooleanToStr(B: Boolean): string;
begin
  if B then
    Result := DefaultTrueBoolStr
  else
    Result := DefaultFalseBoolStr;
end;

function IntToBool(I: Integer): Boolean;
begin
  Result := I <> 0;
end;

function BoolToInt(B: Boolean): Integer;
begin
  Result := Ord(B);
end;

//=== RTL package information ================================================

{$IFNDEF CLR}
{$IFNDEF FPC}

function SystemTObjectInstance: LongWord;
begin
  Result := FindClassHInstance(System.TObject);
end;

function IsCompiledWithPackages: Boolean;
begin
  Result := SystemTObjectInstance <> HInstance;
end;

{$ENDIF ~FPC}
{$ENDIF ~CLR}

//=== GUID ===================================================================

function JclGUIDToString(const GUID: TGUID): string;
begin
  {$IFDEf CLR}
  Result := GUID.ToString();
  {$ELSE}
  Result := Format('{%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x}',
    [GUID.D1, GUID.D2, GUID.D3, GUID.D4[0], GUID.D4[1], GUID.D4[2],
     GUID.D4[3], GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]]);
  {$ENDIF CLR}
end;

function JclStringToGUID(const S: string): TGUID;
begin
  if (Length(S) <> 38) or (S[1] <> '{') or (S[10] <> '-') or (S[15] <> '-') or
    (S[20] <> '-') or (S[25] <> '-') or (S[38] <> '}') then
    {$IFDEF CLR}
    raise EJclConversionError.CreateFmt(RsInvalidGUIDString, [S]);
    {$ELSE}
    raise EJclConversionError.CreateResFmt(@RsInvalidGUIDString, [S]);
    {$ENDIF CLR}

  {$IFDEF CLR}
  Result := System.GUID.Create(
    Integer(StrToInt('$' + Copy(S, 2, 8))),
    Smallint(StrToInt('$' + Copy(S, 11, 4))),
    Smallint(StrToInt('$' + Copy(S, 16, 4))),
    Byte(StrToInt('$' + Copy(S, 21, 2))),
    Byte(StrToInt('$' + Copy(S, 23, 2))),
    Byte(StrToInt('$' + Copy(S, 26, 2))),
    Byte(StrToInt('$' + Copy(S, 28, 2))),
    Byte(StrToInt('$' + Copy(S, 30, 2))),
    Byte(StrToInt('$' + Copy(S, 32, 2))),
    Byte(StrToInt('$' + Copy(S, 34, 2))),
    Byte(StrToInt('$' + Copy(S, 36, 2))));
  {$ELSE}
  Result.D1 := StrToInt('$' + Copy(S, 2, 8));
  Result.D2 := StrToInt('$' + Copy(S, 11, 4));
  Result.D3 := StrToInt('$' + Copy(S, 16, 4));
  Result.D4[0] := StrToInt('$' + Copy(S, 21, 2));
  Result.D4[1] := StrToInt('$' + Copy(S, 23, 2));
  Result.D4[2] := StrToInt('$' + Copy(S, 26, 2));
  Result.D4[3] := StrToInt('$' + Copy(S, 28, 2));
  Result.D4[4] := StrToInt('$' + Copy(S, 30, 2));
  Result.D4[5] := StrToInt('$' + Copy(S, 32, 2));
  Result.D4[6] := StrToInt('$' + Copy(S, 34, 2));
  Result.D4[7] := StrToInt('$' + Copy(S, 36, 2));
  {$ENDIF CLR}
end;

// add items at the end
procedure ListAddItems(var List: string; const Separator, Items: string);
var
  StrList, NewItems: TStringList;
  Index: Integer;
begin
  StrList := TStringList.Create;
  try
    StrToStrings(List, Separator, StrList);

    NewItems := TStringList.Create;
    try
      StrToStrings(Items, Separator, NewItems);

      for Index := 0 to NewItems.Count - 1 do
        StrList.Add(NewItems.Strings[Index]);

      List := StringsToStr(StrList, Separator);
    finally
      NewItems.Free;
    end;
  finally
    StrList.Free;
  end;
end;

// add items at the end if they are not present
procedure ListIncludeItems(var List: string; const Separator, Items: string);
var
  StrList, NewItems: TStringList;
  Index: Integer;
  Item: string;
begin
  StrList := TStringList.Create;
  try
    StrToStrings(List, Separator, StrList);

    NewItems := TStringList.Create;
    try
      StrToStrings(Items, Separator, NewItems);

      for Index := 0 to NewItems.Count - 1 do
      begin
        Item := NewItems.Strings[Index];
        if StrList.IndexOf(Item) = -1 then
          StrList.Add(Item);
      end;

      List := StringsToStr(StrList, Separator);
    finally
      NewItems.Free;
    end;
  finally
    StrList.Free;
  end;
end;

// delete multiple items
procedure ListRemoveItems(var List: string; const Separator, Items: string);
var
  StrList, RemItems: TStringList;
  Index, Position: Integer;
  Item: string;
begin
  StrList := TStringList.Create;
  try
    StrToStrings(List, Separator, StrList);

    RemItems := TStringList.Create;
    try
      StrToStrings(Items, Separator, RemItems);

      for Index := 0 to RemItems.Count - 1 do
      begin
        Item := RemItems.Strings[Index];
        repeat
          Position := StrList.IndexOf(Item);
          if Position >= 0 then
            StrList.Delete(Position);
        until Position < 0;
      end;

      List := StringsToStr(StrList, Separator);
    finally
      RemItems.Free;
    end;
  finally
    StrList.Free;
  end;
end;

// delete one item
procedure ListDelItem(var List: string; const Separator: string; const Index: Integer);
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    StrToStrings(List, Separator, StrList);

    StrList.Delete(Index);

    List := StringsToStr(StrList, Separator);
  finally
    StrList.Free;
  end;
end;

// return the number of item
function ListItemCount(const List, Separator: string): Integer;
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    StrToStrings(List, Separator, StrList);

    Result := StrList.Count;
  finally
    StrList.Free;
  end;
end;

// return the Nth item
function ListGetItem(const List, Separator: string; const Index: Integer): string;
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    StrToStrings(List, Separator, StrList);

    Result := StrList.Strings[Index];
  finally
    StrList.Free;
  end;
end;

// set the Nth item
procedure ListSetItem(var List: string; const Separator: string;
  const Index: Integer; const Value: string);
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    StrToStrings(List, Separator, StrList);

    StrList.Strings[Index] := Value;

    List := StringsToStr(StrList, Separator);
  finally
    StrList.Free;
  end;
end;

// return the index of an item
function ListItemIndex(const List, Separator, Item: string): Integer;
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    StrToStrings(List, Separator, StrList);

    Result := StrList.IndexOf(Item);
  finally
    StrList.Free;
  end;
end;

{$IFNDEF CLR}

//=== { TJclIntfCriticalSection } ============================================

constructor TJclIntfCriticalSection.Create;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TJclIntfCriticalSection.Destroy;
begin
  FCriticalSection.Free;
  inherited Destroy;
end;

function TJclIntfCriticalSection._AddRef: Integer;
begin
  FCriticalSection.Acquire;
  Result := 0;
end;

function TJclIntfCriticalSection._Release: Integer;
begin
  FCriticalSection.Release;
  Result := 0;
end;

function TJclIntfCriticalSection.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

//=== { TJclSimpleLog } ======================================================

{$IFDEF LINUX}
const
  INVALID_HANDLE_VALUE = 0;
{$ENDIF LINUX}

constructor TJclSimpleLog.Create(const ALogFileName: string);
begin
  if ALogFileName = '' then
    FLogFileName := CreateDefaultFileName
  else
    FLogFileName := ALogFileName;
  DWord(FLogFileHandle) := INVALID_HANDLE_VALUE;
end;

function TJclSimpleLog.CreateDefaultFileName: string;
begin
  Result := PathExtractFileDirFixed(ParamStr(0)) +
    PathExtractFileNameNoExt(ParamStr(0)) + '_Err.log';
end;

destructor TJclSimpleLog.Destroy;
begin
  CloseLog;
  inherited Destroy;
end;

procedure TJclSimpleLog.ClearLog;
begin
  CloseLog;
  FLogFileHandle := FileCreate(FLogFileName);
  FLogWasEmpty := True;
end;

procedure TJclSimpleLog.CloseLog;
begin
  if LogOpen then
  begin
    FileClose(FLogFileHandle);
    DWord(FLogFileHandle) := INVALID_HANDLE_VALUE;
    FLogWasEmpty := False;
  end;
end;

function TJclSimpleLog.GetLogOpen: Boolean;
begin
  Result := DWord(FLogFileHandle) <> INVALID_HANDLE_VALUE;
end;

procedure TJclSimpleLog.OpenLog;
begin
  if not LogOpen then
  begin
    FLogFileHandle := FileOpen(FLogFileName, fmOpenWrite or fmShareDenyWrite);
    if not LogOpen then
      FLogFileHandle := FileCreate(FLogFileName);
    if LogOpen then
      FLogWasEmpty := FileSeek(FLogFileHandle, 0, soFromEnd) = 0;
  end
  else
    FLogWasEmpty := False;
end;

procedure TJclSimpleLog.Write(const Text: string; Indent: Integer);
var
  S: string;
  SL: TStringList;
  I: Integer;
begin
  if LogOpen then
  begin
    SL := TStringList.Create;
    try
      SL.Text := Text;
      for I := 0 to SL.Count - 1 do
      begin
        S := StringOfChar(' ', Indent) + StrEnsureSuffix(AnsiCrLf, TrimRight(SL[I]));
        FileWrite(FLogFileHandle, Pointer(S)^, Length(S));
      end;
    finally
      SL.Free;
    end;
  end;
end;

procedure TJclSimpleLog.Write(Strings: TStrings; Indent: Integer);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Write(Strings[I], Indent);
end;

procedure TJclSimpleLog.TimeWrite(const Text: string; Indent: Integer = 0);
var
  S: string;
  SL: TStringList;
  I: Integer;
begin
  if LogOpen then
  begin
    SL := TStringList.Create;
    try
      SL.Text := Text;
      for I := 0 to SL.Count - 1 do
      begin
        S := DateTimeToStr(Now)+' : '+StringOfChar(' ', Indent) + StrEnsureSuffix(AnsiCrLf, TrimRight(SL[I]));
        FileWrite(FLogFileHandle, Pointer(S)^, Length(S));
      end;
    finally
      SL.Free;
    end;
  end;
end;

procedure TJclSimpleLog.TimeWrite(Strings: TStrings; Indent: Integer = 0);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    TimeWrite(Strings[I], Indent);
end;

procedure TJclSimpleLog.WriteStamp(SeparatorLen: Integer);
begin
  if SeparatorLen = 0 then
    SeparatorLen := 40;
  OpenLog;
  if not FLogWasEmpty then
    Write(AnsiCrLf);
  Write(StrRepeat('=', SeparatorLen));
  Write(Format('= %-*s =', [SeparatorLen - 4, DateTimeToStr(Now)]));
  Write(StrRepeat('=', SeparatorLen));
end;

procedure InitSimpleLog (const ALogFileName: string = '');
begin
  if Assigned(SimpleLog) then
    FreeAndNil(SimpleLog);
  SimpleLog := TJclSimpleLog.Create(ALogFileName);
  SimpleLog.OpenLog;
end;

{$ENDIF ~CLR}

initialization
  {$IFNDEF CLR}
  SimpleLog := nil;
  {$IFDEF MSWINDOWS}
  {$IFDEF THREADSAFE}
  if not Assigned(GlobalMMFHandleListCS) then
    GlobalMMFHandleListCS := TJclIntfCriticalSection.Create;
  {$ENDIF THREADSAFE}
  {$ENDIF MSWINDOWS}
  {$ENDIF ~CLR}
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  {$IFNDEF CLR}
  {$IFDEF MSWINDOWS}
  FinalizeMMFHandleList;
  {$IFDEF THREADSAFE}
  FreeAndNil(GlobalMMFHandleListCS);
  {$ENDIF THREADSAFE}
  {$ENDIF MSWINDOWS}
  if Assigned(SimpleLog) then
    FreeAndNil(SimpleLog);
  {$ENDIF ~CLR}
end.
