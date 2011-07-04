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
{ The Original Code is JclLocales.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) Petr Vones. All Rights Reserved.                }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel                                                                              }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains a set of classes which allow you to easily retrieve locale specific           }
{ information such the list of keyboard layouts, names used for dates and characters used for      }
{ formatting numbers and dates.                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclLocales;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF FPC}
  JwaWinNLS,
  {$ENDIF FPC}
  Windows, Classes, SysUtils, Contnrs,
  JclBase, JclWin32;

type
  // System locales
  TJclLocalesDays = 1..7;
  TJclLocalesMonths = 1..13;
  TJclLocaleDateFormats = (ldShort, ldLong, ldYearMonth);

  TJclLocaleInfo = class(TObject)
  private
    FCalendars: TStringList;
    FDateFormats: array [TJclLocaleDateFormats] of TStringList;
    FLocaleID: LCID;
    FTimeFormats: TStringList;
    FUseSystemACP: Boolean;
    FValidCalendars: Boolean;
    FValidDateFormatLists: set of TJclLocaleDateFormats;
    FValidTimeFormatLists: Boolean;
    function GetCalendars: TStrings;
    function GetCalendarIntegerInfo(Calendar: CALID; InfoType: Integer): Integer;
    function GetCalendarStringInfo(Calendar: CALID; InfoType: Integer): string;
    function GetIntegerInfo(InfoType: Integer): Integer;
    function GetStringInfo(InfoType: Integer): string;
    function GetLangID: LANGID;
    function GetSortID: Word;
    function GetLangIDPrimary: Word;
    function GetLangIDSub: Word;
    function GetLongMonthNames(Month: TJclLocalesMonths): string;
    function GetAbbreviatedMonthNames(Month: TJclLocalesMonths): string;
    function GetLongDayNames(Day: TJclLocalesDays): string;
    function GetAbbreviatedDayNames(Day: TJclLocalesDays): string;
    function GetCharInfo(InfoType: Integer): Char;
    function GetTimeFormats: TStrings;
    function GetDateFormats(Format: TJclLocaleDateFormats): TStrings;
    function GetFontCharset: Byte;
    function GetCalTwoDigitYearMax(Calendar: CALID): Integer;
    procedure SetUseSystemACP(const Value: Boolean);
    procedure SetCharInfo(InfoType: Integer; const Value: Char);
    procedure SetIntegerInfo(InfoType: Integer; const Value: Integer);
    procedure SetStringInfo(InfoType: Integer; const Value: string);
  public
    constructor Create(ALocaleID: LCID = LOCALE_SYSTEM_DEFAULT);
    destructor Destroy; override;
    property CharInfo[InfoType: Integer]: Char read GetCharInfo write SetCharInfo;
    property IntegerInfo[InfoType: Integer]: Integer read GetIntegerInfo write SetIntegerInfo;
    property StringInfo[InfoType: Integer]: string read GetStringInfo write SetStringInfo; default;
    property UseSystemACP: Boolean read FUseSystemACP write SetUseSystemACP;
    property FontCharset: Byte read GetFontCharset;
    property LangID: LANGID read GetLangID;
    property LocaleID: LCID read FLocaleID;
    property LangIDPrimary: Word read GetLangIDPrimary;
    property LangIDSub: Word read GetLangIDSub;
    property SortID: Word read GetSortID;
    property DateFormats[Format: TJclLocaleDateFormats]: TStrings read GetDateFormats;
    property TimeFormats: TStrings read GetTimeFormats;
    // Languages
    property LanguageIndentifier: string index LOCALE_ILANGUAGE read GetStringInfo;
    property LocalizedLangName: string index LOCALE_SLANGUAGE read GetStringInfo;
    property EnglishLangName: string index LOCALE_SENGLANGUAGE read GetStringInfo;
    property AbbreviatedLangName: string index LOCALE_SABBREVLANGNAME read GetStringInfo;
    property NativeLangName: string index LOCALE_SNATIVELANGNAME read GetStringInfo;
    property ISOAbbreviatedLangName: string index LOCALE_SISO639LANGNAME read GetStringInfo;
    // Countries
    property CountryCode: Integer index LOCALE_ICOUNTRY read GetIntegerInfo;
    property LocalizedCountryName: string index LOCALE_SCOUNTRY read GetStringInfo;
    property EnglishCountryName: string index LOCALE_SENGCOUNTRY read GetStringInfo;
    property AbbreviatedCountryName: string index LOCALE_SABBREVCTRYNAME read GetStringInfo;
    property NativeCountryName: string index LOCALE_SNATIVECTRYNAME read GetStringInfo;
    property ISOAbbreviatedCountryName: string index LOCALE_SISO3166CTRYNAME read GetStringInfo;
    // Codepages
    property DefaultLanguageId: Integer index LOCALE_IDEFAULTLANGUAGE read GetIntegerInfo;
    property DefaultCountryCode: Integer index LOCALE_IDEFAULTCOUNTRY read GetIntegerInfo;
    property DefaultCodePageEBCDIC: Integer index LOCALE_IDEFAULTEBCDICCODEPAGE read GetIntegerInfo;
    property CodePageOEM: Integer index LOCALE_IDEFAULTCODEPAGE read GetIntegerInfo;
    property CodePageANSI: Integer index LOCALE_IDEFAULTANSICODEPAGE read GetIntegerInfo;
    property CodePageMAC: Integer index LOCALE_IDEFAULTMACCODEPAGE read GetIntegerInfo;
    // Digits
    property ListItemSeparator: Char index LOCALE_SLIST read GetCharInfo write SetCharInfo;
    property Measure: Integer index LOCALE_IMEASURE read GetIntegerInfo write SetIntegerInfo;
    property DecimalSeparator: Char index LOCALE_SDECIMAL read GetCharInfo write SetCharInfo;
    property ThousandSeparator: Char index LOCALE_STHOUSAND read GetCharInfo write SetCharInfo;
    property DigitGrouping: string index LOCALE_SGROUPING read GetStringInfo write SetStringInfo;
    property NumberOfFractionalDigits: Integer index LOCALE_IDIGITS read GetIntegerInfo write SetIntegerInfo;
    property LeadingZeros: Integer index LOCALE_ILZERO read GetIntegerInfo write SetIntegerInfo;
    property NegativeNumberMode: Integer index LOCALE_INEGNUMBER read GetIntegerInfo write SetIntegerInfo;
    property NativeDigits: string index LOCALE_SNATIVEDIGITS read GetStringInfo;
    property DigitSubstitution: Integer index LOCALE_IDIGITSUBSTITUTION read GetIntegerInfo;
    // Monetary
    property MonetarySymbolLocal: string index LOCALE_SCURRENCY read GetStringInfo write SetStringInfo;
    property MonetarySymbolIntl: string index LOCALE_SINTLSYMBOL read GetStringInfo;
    property MonetaryDecimalSeparator: Char index LOCALE_SMONDECIMALSEP read GetCharInfo write SetCharInfo;
    property MonetaryThousandsSeparator: Char index LOCALE_SMONTHOUSANDSEP read GetCharInfo write SetCharInfo;
    property MonetaryGrouping: string index LOCALE_SMONGROUPING read GetStringInfo write SetStringInfo;
    property NumberOfLocalMonetaryDigits: Integer index LOCALE_ICURRDIGITS read GetIntegerInfo write SetIntegerInfo;
    property NumberOfIntlMonetaryDigits: Integer index LOCALE_IINTLCURRDIGITS read GetIntegerInfo;
    property PositiveCurrencyMode: string index LOCALE_ICURRENCY read GetStringInfo write SetStringInfo;
    property NegativeCurrencyMode: string index LOCALE_INEGCURR read GetStringInfo write SetStringInfo;
    property EnglishCurrencyName: string index LOCALE_SENGCURRNAME read GetStringInfo;
    property NativeCurrencyName: string index LOCALE_SNATIVECURRNAME read GetStringInfo;
    // Date and time
    property DateSeparator: Char index LOCALE_SDATE read GetCharInfo write SetCharInfo;
    property TimeSeparator: Char index LOCALE_STIME read GetCharInfo write SetCharInfo;
    property ShortDateFormat: string index LOCALE_SSHORTDATE read GetStringInfo write SetStringInfo;
    property LongDateFormat: string index LOCALE_SLONGDATE read GetStringInfo write SetStringInfo;
    property TimeFormatString: string index LOCALE_STIMEFORMAT read GetStringInfo write SetStringInfo;
    property ShortDateOrdering: Integer index LOCALE_IDATE read GetIntegerInfo;
    property LongDateOrdering: Integer index LOCALE_ILDATE read GetIntegerInfo;
    property TimeFormatSpecifier: Integer index LOCALE_ITIME read GetIntegerInfo write SetIntegerInfo;
    property TimeMarkerPosition: Integer index LOCALE_ITIMEMARKPOSN read GetIntegerInfo;
    property CenturyFormatSpecifier: Integer index LOCALE_ICENTURY read GetIntegerInfo;
    property LeadZerosInTime: Integer index LOCALE_ITLZERO read GetIntegerInfo;
    property LeadZerosInDay: Integer index LOCALE_IDAYLZERO read GetIntegerInfo;
    property LeadZerosInMonth: Integer index LOCALE_IMONLZERO read GetIntegerInfo;
    property AMDesignator: string index LOCALE_S1159 read GetStringInfo write SetStringInfo;
    property PMDesignator: string index LOCALE_S2359 read GetStringInfo write SetStringInfo;
    property YearMonthFormat: string index LOCALE_SYEARMONTH read GetStringInfo write SetStringInfo;
    // Calendar
    property CalendarType: Integer index LOCALE_ICALENDARTYPE read GetIntegerInfo write SetIntegerInfo;
    property AdditionalCaledarTypes: Integer index LOCALE_IOPTIONALCALENDAR read GetIntegerInfo;
    property FirstDayOfWeek: Integer index LOCALE_IFIRSTDAYOFWEEK read GetIntegerInfo write SetIntegerInfo;
    property FirstWeekOfYear: Integer index LOCALE_IFIRSTWEEKOFYEAR read GetIntegerInfo write SetIntegerInfo;
    // Day and month names
    property LongDayNames[Day: TJclLocalesDays]: string read GetLongDayNames;
    property AbbreviatedDayNames[Day: TJclLocalesDays]: string read GetAbbreviatedDayNames;
    property LongMonthNames[Month: TJclLocalesMonths]: string read GetLongMonthNames;
    property AbbreviatedMonthNames[Month: TJclLocalesMonths]: string read GetAbbreviatedMonthNames;
    // Sign
    property PositiveSign: string index LOCALE_SPOSITIVESIGN read GetStringInfo write SetStringInfo;
    property NegativeSign: string index LOCALE_SNEGATIVESIGN read GetStringInfo write SetStringInfo;
    property PositiveSignPos: Integer index LOCALE_IPOSSIGNPOSN read GetIntegerInfo;
    property NegativeSignPos: Integer index LOCALE_INEGSIGNPOSN read GetIntegerInfo;
    property PosOfPositiveMonetarySymbol: Integer index LOCALE_IPOSSYMPRECEDES read GetIntegerInfo;
    property SepOfPositiveMonetarySymbol: Integer index LOCALE_IPOSSEPBYSPACE read GetIntegerInfo;
    property PosOfNegativeMonetarySymbol: Integer index LOCALE_INEGSYMPRECEDES read GetIntegerInfo;
    property SepOfNegativeMonetarySymbol: Integer index LOCALE_INEGSEPBYSPACE read GetIntegerInfo;
    // Misc
    property DefaultPaperSize: Integer index LOCALE_IPAPERSIZE read GetIntegerInfo;
    property FontSignature: string index LOCALE_FONTSIGNATURE read GetStringInfo;
    property LocalizedSortName: string index LOCALE_SSORTNAME read GetStringInfo;
    // Calendar Info
    property Calendars: TStrings read GetCalendars;
    property CalendarIntegerInfo[Calendar: CALID; InfoType: Integer]: Integer read GetCalendarIntegerInfo;
    property CalendarStringInfo[Calendar: CALID; InfoType: Integer]: string read GetCalendarStringInfo;
    property CalTwoDigitYearMax[Calendar: CALID]: Integer read GetCalTwoDigitYearMax;
  end;

  TJclLocalesKind = (lkInstalled, lkSupported);

  TJclLocalesList = class(TObjectList)
  private
    FCodePages: TStringList;
    FKind: TJclLocalesKind;
    function GetItemFromLangID(LangID: LANGID): TJclLocaleInfo;
    function GetItemFromLangIDPrimary(LangIDPrimary: Word): TJclLocaleInfo;
    function GetItemFromLocaleID(LocaleID: LCID): TJclLocaleInfo;
    function GetItems(Index: Integer): TJclLocaleInfo;
    function GetCodePages: TStrings;
  protected
    procedure CreateList;
  public
    constructor Create(AKind: TJclLocalesKind = lkInstalled);
    destructor Destroy; override;
    procedure FillStrings(Strings: TStrings; InfoType: Integer);
    property CodePages: TStrings read GetCodePages;
    property ItemFromLangID[LangID: LANGID]: TJclLocaleInfo read GetItemFromLangID;
    property ItemFromLangIDPrimary[LangIDPrimary: Word]: TJclLocaleInfo read GetItemFromLangIDPrimary;
    property ItemFromLocaleID[LocaleID: LCID]: TJclLocaleInfo read GetItemFromLocaleID;
    property Items[Index: Integer]: TJclLocaleInfo read GetItems; default;
    property Kind: TJclLocalesKind read FKind;
  end;

  // Keyboard layouts
  TJclKeybLayoutFlag = (klReorder, klUnloadPrevious, klSetForProcess,
    klActivate, klNotEllShell, klReplaceLang, klSubstituteOK);

  TJclKeybLayoutFlags = set of TJclKeybLayoutFlag;

  TJclKeyboardLayoutList = class;

  TJclAvailableKeybLayout = class(TObject)
  private
    FIdentifier: DWORD;
    FLayoutID: Word;
    FLayoutFile: string;
    FOwner: TJclKeyboardLayoutList;
    FName: string;
    function GetIdentifierName: string;
    function GetLayoutFileExists: Boolean;
  public
    function Load(const LoadFlags: TJclKeybLayoutFlags): Boolean;
    property Identifier: DWORD read FIdentifier;
    property IdentifierName: string read GetIdentifierName;
    property LayoutID: Word read FLayoutID;
    property LayoutFile: string read FLayoutFile;
    property LayoutFileExists: Boolean read GetLayoutFileExists;
    property Name: string read FName;
  end;

  TJclKeyboardLayout = class(TObject)
  private
    FLayout: HKL;
    FLocaleInfo: TJclLocaleInfo;
    FOwner: TJclKeyboardLayoutList;
    function GetDeviceHandle: Word;
    function GetDisplayName: string;
    function GetLocaleID: Word;
    function GetLocaleInfo: TJclLocaleInfo;
    function GetVariationName: string;
  public
    constructor Create(AOwner: TJclKeyboardLayoutList; ALayout: HKL);
    destructor Destroy; override;
    function Activate(ActivateFlags: TJclKeybLayoutFlags = []): Boolean;
    function Unload: Boolean;
    property DeviceHandle: Word read GetDeviceHandle;
    property DisplayName: string read GetDisplayName;
    property Layout: HKL read FLayout;
    property LocaleID: Word read GetLocaleID;
    property LocaleInfo: TJclLocaleInfo read GetLocaleInfo;
    property VariationName: string read GetVariationName;
  end;

  TJclKeyboardLayoutList = class(TObject)
  private
    FAvailableLayouts: TObjectList;
    FList: TObjectList;
    FOnRefresh: TNotifyEvent;
    function GetCount: Integer;
    function GetItems(Index: Integer): TJclKeyboardLayout;
    function GetActiveLayout: TJclKeyboardLayout;
    function GetItemFromHKL(Layout: HKL): TJclKeyboardLayout;
    function GetLayoutFromLocaleID(LocaleID: Word): TJclKeyboardLayout;
    function GetAvailableLayoutCount: Integer;
    function GetAvailableLayouts(Index: Integer): TJclAvailableKeybLayout;
  protected
    procedure CreateAvailableLayouts;
    procedure DoRefresh; dynamic;
  public
    constructor Create;
    destructor Destroy; override;
    function ActivatePrevLayout(ActivateFlags: TJclKeybLayoutFlags = []): Boolean;
    function ActivateNextLayout(ActivateFlags: TJclKeybLayoutFlags = []): Boolean;
    function LoadLayout(const LayoutName: string; LoadFlags: TJclKeybLayoutFlags): Boolean;
    procedure Refresh;
    property ActiveLayout: TJclKeyboardLayout read GetActiveLayout;
    property AvailableLayouts[Index: Integer]: TJclAvailableKeybLayout read GetAvailableLayouts;
    property AvailableLayoutCount: Integer read GetAvailableLayoutCount;
    property Count: Integer read GetCount;
    property ItemFromHKL[Layout: HKL]: TJclKeyboardLayout read GetItemFromHKL;
    property Items[Index: Integer]: TJclKeyboardLayout read GetItems; default;
    property LayoutFromLocaleID[LocaleID: Word]: TJclKeyboardLayout read GetLayoutFromLocaleID;
    property OnRefresh: TNotifyEvent read FOnRefresh write FOnRefresh;
  end;

// Various routines
procedure JclLocalesInfoList(const Strings: TStrings; InfoType: Integer = LOCALE_SENGCOUNTRY);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclLocales.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF FPC}
  WinSysUt,
  {$ENDIF FPC}
  SysConst, JclFileUtils, JclRegistry, JclStrings, JclSysInfo;

const
  JclMaxKeyboardLayouts = 16;
  LocaleUseAcp: array [Boolean] of DWORD = (0, LOCALE_USE_CP_ACP);

function KeybLayoutFlagsToDWORD(const ActivateFlags: TJclKeybLayoutFlags;
  const LoadMode: Boolean): DWORD;
begin
  Result := 0;
  if klReorder in ActivateFlags then
    Inc(Result, KLF_REORDER);
  if (klUnloadPrevious in ActivateFlags) and IsWinNT then
    Inc(Result, KLF_UNLOADPREVIOUS);
  if (klSetForProcess in ActivateFlags) and IsWin2K then
    Inc(Result, KLF_SETFORPROCESS);
  if LoadMode then
  begin
    if klActivate in ActivateFlags then
      Inc(Result, KLF_ACTIVATE);
    if klNotEllShell in ActivateFlags then
      Inc(Result, KLF_NOTELLSHELL);
    if (klReplaceLang in ActivateFlags) and not IsWinNT3 then
      Inc(Result, KLF_REPLACELANG);
    if klSubstituteOK in ActivateFlags then
      Inc(Result, KLF_SUBSTITUTE_OK);
  end;
end;

// EnumXXX functions helper thread variables
threadvar
  ProcessedLocaleInfoList: TStrings;
  ProcessedLocalesList: TJclLocalesList;

//=== { TJclLocaleInfo } =====================================================

constructor TJclLocaleInfo.Create(ALocaleID: LCID);
begin
  inherited Create;
  FLocaleID := ALocaleID;
  FUseSystemACP := True;
  FValidDateFormatLists := [];
end;

destructor TJclLocaleInfo.Destroy;
var
  DateFormat: TJclLocaleDateFormats;
begin
  FreeAndNil(FCalendars);
  for DateFormat := Low(DateFormat) to High(DateFormat) do
    FreeAndNil(FDateFormats[DateFormat]);
  FreeAndNil(FTimeFormats);
  inherited Destroy;
end;

function TJclLocaleInfo.GetAbbreviatedDayNames(Day: TJclLocalesDays): string;
begin
  Result := GetStringInfo(LOCALE_SABBREVDAYNAME1 + Day - 1);
end;

function TJclLocaleInfo.GetAbbreviatedMonthNames(Month: TJclLocalesMonths): string;
var
  Param: DWORD;
begin
  case Month of
    1..12:
      Param := LOCALE_SABBREVMONTHNAME1 + Month - 1;
    13:
      Param := LOCALE_SABBREVMONTHNAME13;
  else
    raise ERangeError.CreateRes(@SRangeError);
  end;
  Result := GetStringInfo(Param);
end;

function TJclLocaleInfo.GetCalendarIntegerInfo(Calendar: CALID; InfoType: Integer): Integer;
var
  Ret: DWORD;
begin
  InfoType := InfoType or Integer(LocaleUseAcp[FUseSystemACP]) or CAL_RETURN_NUMBER;
  Ret := JclWin32.RtdlGetCalendarInfoW(FLocaleID, Calendar, InfoType, nil, 0, @Result);
  if Ret = 0 then
    Ret := JclWin32.RtdlGetCalendarInfoA(FLocaleID, Calendar, InfoType, nil, 0, @Result);
  if Ret = 0 then
    Result := 0;
end;

function TJclLocaleInfo.GetCalTwoDigitYearMax(Calendar: CALID): Integer;
begin
  Result := GetCalendarIntegerInfo(Calendar, CAL_ITWODIGITYEARMAX);
end;

function EnumCalendarInfoProcEx(lpCalendarInfoString: PChar; Calendar: CALID): BOOL; stdcall;
begin
  ProcessedLocaleInfoList.AddObject(lpCalendarInfoString, Pointer(Calendar));
  Result := True;
end;

function EnumCalendarInfoProcName(lpCalendarInfoString: PChar): BOOL; stdcall;
begin
  ProcessedLocaleInfoList.Add(lpCalendarInfoString);
  Result := True;
end;

function TJclLocaleInfo.GetCalendars: TStrings;
var
  C: CALTYPE;

begin
  if not FValidCalendars then
  begin
    if FCalendars = nil then
      FCalendars := TStringList.Create
    else
      FCalendars.Clear;
    ProcessedLocaleInfoList := FCalendars;
    try
      C := CAL_SCALNAME or LocaleUseAcp[FUseSystemACP];
      if not JclWin32.RtdlEnumCalendarInfoExA(EnumCalendarInfoProcEx, FLocaleID, ENUM_ALL_CALENDARS, C) then
        Windows.EnumCalendarInfo(@EnumCalendarInfoProcName, FLocaleID, ENUM_ALL_CALENDARS, C);
      FValidCalendars := True;
    finally
      ProcessedLocaleInfoList := nil;
    end;
  end;
  Result := FCalendars;
end;

function TJclLocaleInfo.GetCalendarStringInfo(Calendar: CALID; InfoType: Integer): string;
var
  Buffer: Pointer;
  BufferSize: Integer;
  Ret: DWORD;
begin
  Result := '';
  InfoType := InfoType or Integer(LocaleUseAcp[FUseSystemACP]);
  Buffer := nil;
  try
    BufferSize := 128;
    repeat
      ReallocMem(Buffer, BufferSize);
      Ret := RtdlGetCalendarInfoW(FLocaleID, Calendar, InfoType, Buffer, BufferSize, nil);
      if (Ret = 0) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
        BufferSize := RtdlGetCalendarInfoW(FLocaleID, Calendar, InfoType, Buffer, 0, nil) * 2;
    until (Ret > 0) or (GetLastError <> ERROR_INSUFFICIENT_BUFFER);
    if Ret > 0 then
      Result := PWideChar(Buffer)
    else
    begin
      BufferSize := 64;
      repeat
        ReallocMem(Buffer, BufferSize);
        Ret := RtdlGetCalendarInfoA(FLocaleID, Calendar, InfoType, Buffer, BufferSize, nil);
        if (Ret = 0) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
          BufferSize := RtdlGetCalendarInfoA(FLocaleID, Calendar, InfoType, Buffer, 0, nil);
      until (Ret > 0) or (GetLastError <> ERROR_INSUFFICIENT_BUFFER);
      if Ret > 0 then
        Result := PChar(Buffer);
    end;
  finally
    FreeMem(Buffer);
  end;
end;

function TJclLocaleInfo.GetCharInfo(InfoType: Integer): Char;
var
  S: string;
begin
  S := GetStringInfo(InfoType);
  if Length(S) >= 1 then
    Result := S[1]
  else
    Result := ' ';
end;

function TJclLocaleInfo.GetDateFormats(Format: TJclLocaleDateFormats): TStrings;
const
  DateFormats: array [TJclLocaleDateFormats] of DWORD =
    (DATE_SHORTDATE, DATE_LONGDATE, DATE_YEARMONTH);

  function EnumDateFormatsProc(lpDateFormatString: LPSTR): BOOL; stdcall;
  begin
    ProcessedLocaleInfoList.Add(lpDateFormatString);
    DWORD(Result) := 1;
  end;

begin
  if not (Format in FValidDateFormatLists) then
  begin
    if FDateFormats[Format] = nil then
      FDateFormats[Format] := TStringList.Create
    else
      FDateFormats[Format].Clear;
    ProcessedLocaleInfoList := FDateFormats[Format];
    try
      Windows.EnumDateFormats(@EnumDateFormatsProc, FLocaleID, DateFormats[Format] or
        LocaleUseAcp[FUseSystemACP]);
      Include(FValidDateFormatLists, Format);
    finally
      ProcessedLocaleInfoList := nil;
    end;
  end;
  Result := FDateFormats[Format];
end;

function TJclLocaleInfo.GetFontCharset: Byte;
type
  TCharsetEntry = record
    CodePage: Word;
    Charset: Byte;
  end;
const
  CharsetTable: array [1..10] of TCharsetEntry =
   (
    (CodePage: 1252; Charset: ANSI_CHARSET),
    (CodePage: 1250; Charset: EASTEUROPE_CHARSET),
    (CodePage: 1251; Charset: RUSSIAN_CHARSET),
    (CodePage: 1253; Charset: GREEK_CHARSET),
    (CodePage: 1254; Charset: TURKISH_CHARSET),
    (CodePage: 1255; Charset: HEBREW_CHARSET),
    (CodePage: 1256; Charset: ARABIC_CHARSET),
    (CodePage: 1257; Charset: BALTIC_CHARSET),
    (CodePage:  874; Charset: THAI_CHARSET),
    (CodePage:  932; Charset: SHIFTJIS_CHARSET)
   );
var
  I, CpANSI: Integer;
begin
  Result := DEFAULT_CHARSET;
  CpANSI := CodePageANSI;
  for I := Low(CharsetTable) to High(CharsetTable) do
    if CharsetTable[I].CodePage = CpANSI then
    begin
      Result := CharsetTable[I].Charset;
      Break;
    end;
end;

function TJclLocaleInfo.GetIntegerInfo(InfoType: Integer): Integer;
begin
  Result := StrToIntDef(GetStringInfo(InfoType), 0);
end;

function TJclLocaleInfo.GetLangID: LANGID;
begin
  Result := LANGIDFROMLCID(FLocaleID);
end;

function TJclLocaleInfo.GetLangIDPrimary: Word;
begin
  Result := PRIMARYLANGID(LangID);
end;

function TJclLocaleInfo.GetLangIDSub: Word;
begin
  Result := SUBLANGID(LangID);
end;

function TJclLocaleInfo.GetLongDayNames(Day: TJclLocalesDays): string;
begin
  Result := GetStringInfo(LOCALE_SDAYNAME1 + Day - 1);
end;

function TJclLocaleInfo.GetLongMonthNames(Month: TJclLocalesMonths): string;
var
  Param: DWORD;
begin
  if Month = 13 then
    Param := LOCALE_SMONTHNAME13
  else
    Param := LOCALE_SMONTHNAME1 + Month - 1;
  Result := GetStringInfo(Param);
end;

function TJclLocaleInfo.GetSortID: Word;
begin
  Result := SORTIDFROMLCID(FLocaleID);
end;

function TJclLocaleInfo.GetStringInfo(InfoType: Integer): string;
var
  Res: Integer;
  W: PWideChar;
begin
  InfoType := InfoType or Integer(LocaleUseAcp[FUseSystemACP]);
  Res := GetLocaleInfoA(FLocaleID, InfoType, nil, 0);
  if Res > 0 then
  begin
    SetString(Result, nil, Res);
    Res := Windows.GetLocaleInfoA(FLocaleID, InfoType, PChar(Result), Res);
    StrResetLength(Result);
    // Note: GetLocaleInfo returns sometimes incorrect length of string on Win95 (usually plus 1),
    // that's why StrResetLength is called.
  end
  else  // GetLocaleInfoA failed
  if IsWinNT then
  begin
    Res := GetLocaleInfoW(FLocaleID, InfoType, nil, 0);
    if Res > 0 then
    begin
      GetMem(W, Res * SizeOf(WideChar));
      Res := Windows.GetLocaleInfoW(FLocaleID, InfoType, W, Res);
      Result := WideCharToString(W);
      FreeMem(W);
    end;
  end;
  if Res = 0 then
    Result := '';
end;

function TJclLocaleInfo.GetTimeFormats: TStrings;

  function EnumTimeFormatsProc(lpTimeFormatString: LPSTR): BOOL; stdcall;
  begin
    ProcessedLocaleInfoList.Add(lpTimeFormatString);
    DWORD(Result) := 1;
  end;

begin
  if not FValidTimeFormatLists then
  begin
    if FTimeFormats = nil then
      FTimeFormats := TStringList.Create
    else
      FTimeFormats.Clear;
    ProcessedLocaleInfoList := FTimeFormats;
    try
      Windows.EnumTimeFormats(@EnumTimeFormatsProc, FLocaleID, LocaleUseAcp[FUseSystemACP]);
      FValidTimeFormatLists := True;
    finally
      ProcessedLocaleInfoList := nil;
    end;
  end;
  Result := FTimeFormats;
end;

procedure TJclLocaleInfo.SetCharInfo(InfoType: Integer; const Value: Char);
begin
  SetStringInfo(InfoType, Value);
end;

procedure TJclLocaleInfo.SetIntegerInfo(InfoType: Integer; const Value: Integer);
begin
  SetStringInfo(InfoType, IntToStr(Value));
end;

procedure TJclLocaleInfo.SetStringInfo(InfoType: Integer; const Value: string);
begin
  Win32Check(Windows.SetLocaleInfo(FLocaleID, InfoType, PChar(Value)));
end;

procedure TJclLocaleInfo.SetUseSystemACP(const Value: Boolean);
begin
  if FUseSystemACP <> Value then
  begin
    FUseSystemACP := Value;
    FValidCalendars := False;
    FValidDateFormatLists := [];
    FValidTimeFormatLists := False;
  end;
end;

//=== { TJclLocalesList } ====================================================

constructor TJclLocalesList.Create(AKind: TJclLocalesKind);
begin
  inherited Create(True);
  FCodePages := TStringList.Create;
  FKind := AKind;
  CreateList;
end;

destructor TJclLocalesList.Destroy;
begin
  FreeAndNil(FCodePages);
  inherited Destroy;
end;

procedure TJclLocalesList.CreateList;
const
  Flags: array [TJclLocalesKind] of DWORD = (LCID_INSTALLED, LCID_SUPPORTED);

  function EnumLocalesProc(lpLocaleString: LPSTR): BOOL; stdcall;
  var
    LocaleID: LCID;
  begin
    LocaleID := StrToIntDef('$' + Copy(lpLocaleString, 5, 4), 0);
    if LocaleID > 0 then
      ProcessedLocalesList.Add(TJclLocaleInfo.Create(LocaleID));
    DWORD(Result) := 1;
  end;

  function EnumCodePagesProc(lpCodePageString: LPSTR): BOOL; stdcall;
  begin
    ProcessedLocalesList.CodePages.AddObject(lpCodePageString, Pointer(StrToIntDef(lpCodePageString, 0)));
    DWORD(Result) := 1;
  end;

begin
  ProcessedLocalesList := Self;
  try
    Win32Check(Windows.EnumSystemLocales(@EnumLocalesProc, Flags[FKind]));
    Win32Check(Windows.EnumSystemCodePages(@EnumCodePagesProc, Flags[FKind]));
  finally
    ProcessedLocalesList := nil;
  end;
end;

procedure TJclLocalesList.FillStrings(Strings: TStrings; InfoType: Integer);
var
  I: Integer;
begin
  Strings.BeginUpdate;
  try
    for I := 0 to Count - 1 do
      with Items[I] do
        Strings.AddObject(StringInfo[InfoType], Pointer(LocaleId));
  finally
    Strings.EndUpdate;
  end;
end;

function TJclLocalesList.GetCodePages: TStrings;
begin
  Result := FCodePages;
end;

function TJclLocalesList.GetItemFromLangID(LangID: LANGID): TJclLocaleInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].LangID = LangID then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TJclLocalesList.GetItemFromLangIDPrimary(LangIDPrimary: Word): TJclLocaleInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].LangIDPrimary = LangIDPrimary then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TJclLocalesList.GetItemFromLocaleID(LocaleID: LCID): TJclLocaleInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].LocaleID = LocaleID then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TJclLocalesList.GetItems(Index: Integer): TJclLocaleInfo;
begin
  Result := TJclLocaleInfo(inherited Items[Index]);
end;

//=== { TJclAvailableKeybLayout } ============================================

function TJclAvailableKeybLayout.GetIdentifierName: string;
begin
  Result := Format('%.8x', [FIdentifier]);
end;

function TJclAvailableKeybLayout.GetLayoutFileExists: Boolean;
begin
  Result := FileExists(PathAddSeparator(GetWindowsSystemFolder) + LayoutFile);
end;

function TJclAvailableKeybLayout.Load(const LoadFlags: TJclKeybLayoutFlags): Boolean;
begin
  Result := FOwner.LoadLayout(IdentifierName, LoadFlags);
end;

//=== { TJclKeyboardLayout } =================================================

constructor TJclKeyboardLayout.Create(AOwner: TJclKeyboardLayoutList; ALayout: HKL);
begin
  inherited Create;
  FLayout := ALayout;
  FOwner := AOwner;
end;

destructor TJclKeyboardLayout.Destroy;
begin
  FreeAndNil(FLocaleInfo);
  inherited Destroy;
end;

function TJclKeyboardLayout.Activate(ActivateFlags: TJclKeybLayoutFlags): Boolean;
begin
  Result := ActivateKeyboardLayout(FLayout, KeybLayoutFlagsToDWORD(ActivateFlags, False)) {$IFNDEF FPC} <> 0 {$ENDIF};
end;

function TJclKeyboardLayout.GetDeviceHandle: Word;
begin
  Result := HiWord(FLayout);
end;

function TJclKeyboardLayout.GetDisplayName: string;
begin
  Result := LocaleInfo.LocalizedLangName;
  if HiWord(FLayout) <> LoWord(FLayout) then
    Result := Result + ' - ' + VariationName;
end;

function TJclKeyboardLayout.GetLocaleID: Word;
begin
  Result := LoWord(FLayout);
end;

function TJclKeyboardLayout.GetLocaleInfo: TJclLocaleInfo;
begin
  if FLocaleInfo = nil then
    FLocaleInfo := TJclLocaleInfo.Create(MAKELCID(GetLocaleID, SORT_DEFAULT));
  Result := FLocaleInfo;
end;

function TJclKeyboardLayout.GetVariationName: string;
var
  I: Integer;
  Ident: DWORD;
begin
  Result := '';
  if HiWord(FLayout) = LoWord(FLayout) then
    Ident := LoWord(FLayout)
  else
    Ident := FLayout and $0FFFFFFF;
  with FOwner do
    for I := 0 to AvailableLayoutCount - 1 do
      with AvailableLayouts[I] do
        if (LoWord(Identifier) = LoWord(Ident)) and (LayoutID = HiWord(Ident)) then
        begin
          Result := Name;
          Break;
        end;
end;

function TJclKeyboardLayout.Unload: Boolean;
begin
  Result := Windows.UnloadKeyboardLayout(FLayout);
  if Result then
    FOwner.Refresh;
end;

//=== { TJclKeyboardLayoutList } =============================================

constructor TJclKeyboardLayoutList.Create;
begin
  inherited Create;
  FList := TObjectList.Create(True);
  CreateAvailableLayouts;
  Refresh;
end;

destructor TJclKeyboardLayoutList.Destroy;
begin
  FreeAndNil(FAvailableLayouts);
  FreeAndNil(FList);
  inherited Destroy;
end;

function TJclKeyboardLayoutList.ActivateNextLayout(ActivateFlags: TJclKeybLayoutFlags): Boolean;
begin
  Result := ActivateKeyboardLayout(HKL_NEXT, KeybLayoutFlagsToDWORD(ActivateFlags, False)) {$IFNDEF FPC} <> 0 {$ENDIF};
end;

function TJclKeyboardLayoutList.ActivatePrevLayout(
  ActivateFlags: TJclKeybLayoutFlags): Boolean;
begin
  Result := ActivateKeyboardLayout(HKL_PREV, KeybLayoutFlagsToDWORD(ActivateFlags, False)) {$IFNDEF FPC} <> 0 {$ENDIF};
end;

// Documentation:

// HOWTO: How to Find the Available Keyboard Layouts Under Windows NT
// Microsoft Knowledge Base Article - 139571
// http://support.microsoft.com/default.aspx?scid=kb;en-us;139571

// Description of Typical Control Subkeys of the HKLM Registry Key
// Microsoft Knowledge Base Article - 250447
// http://support.microsoft.com/default.aspx?scid=kb;en-us;250447

// http://www.microsoft.com/windows2000/techinfo/reskit/en-us/regentry/28326.asp

procedure TJclKeyboardLayoutList.CreateAvailableLayouts;
const
  cLayoutsKey = 'SYSTEM\CurrentControlSet\Control\Keyboard Layouts';
var
  I: Integer;
  KeyNames: TStringList;
  Item: TJclAvailableKeybLayout;
  Layout: string;
begin
  FAvailableLayouts := TObjectList.Create(True);
  KeyNames := TStringList.Create;
  try
    RegGetKeyNames(HKEY_LOCAL_MACHINE, cLayoutsKey, KeyNames);
    for I := 0 to KeyNames.Count - 1 do
    begin
      Layout := cLayoutsKey + '\' + KeyNames[I];
      Item := TJclAvailableKeybLayout.Create;
      Item.FOwner := Self;
      Item.FIdentifier := StrToIntDef('$' + KeyNames[I], 0);
      Item.FName := RegReadStringDef(HKEY_LOCAL_MACHINE, Layout, 'Layout Text', '');
      Item.FLayoutFile := RegReadStringDef(HKEY_LOCAL_MACHINE, Layout, 'Layout File', '');
      Item.FLayoutID := StrToIntDef('$' + RegReadStringDef(HKEY_LOCAL_MACHINE, Layout, 'Layout Id', ''), 0);
      FAvailableLayouts.Add(Item);
    end;
  finally
    KeyNames.Free;
  end;
end;

procedure TJclKeyboardLayoutList.DoRefresh;
begin
  if Assigned(FOnRefresh) then
    FOnRefresh(Self);
end;

function TJclKeyboardLayoutList.GetActiveLayout: TJclKeyboardLayout;
begin
  Result := ItemFromHKL[GetKeyboardLayout(0)];
end;

function TJclKeyboardLayoutList.GetAvailableLayoutCount: Integer;
begin
  Result := FAvailableLayouts.Count;
end;

function TJclKeyboardLayoutList.GetAvailableLayouts(Index: Integer): TJclAvailableKeybLayout;
begin
  Result := TJclAvailableKeybLayout(FAvailableLayouts[Index]);
end;

function TJclKeyboardLayoutList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJclKeyboardLayoutList.GetItemFromHKL(Layout: HKL): TJclKeyboardLayout;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Layout = Layout then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TJclKeyboardLayoutList.GetItems(Index: Integer): TJclKeyboardLayout;
begin
  Result := TJclKeyboardLayout(FList[Index]);
end;

function TJclKeyboardLayoutList.GetLayoutFromLocaleID(LocaleID: Word): TJclKeyboardLayout;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].LocaleID = LocaleID then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TJclKeyboardLayoutList.LoadLayout(const LayoutName: string;
  LoadFlags: TJclKeybLayoutFlags): Boolean;
begin
  Result := LoadKeyboardLayout(PChar(LayoutName),
    KeybLayoutFlagsToDWORD(LoadFlags, True)) <> 0;
  if Result then
    Refresh;
end;

procedure TJclKeyboardLayoutList.Refresh;
var
  Cnt, I: Integer;
  Layouts: array [1..JclMaxKeyboardLayouts] of HKL;
begin
  Cnt := Windows.GetKeyboardLayoutList(JclMaxKeyboardLayouts, Layouts);
  // Note: GetKeyboardLayoutList doesn't work as expected, when pass 0 to nBuff it always returns 0
  // on Win95.
  FList.Clear;
  for I := 1 to Cnt do
    FList.Add(TJclKeyboardLayout.Create(Self, Layouts[I]));
  DoRefresh;
end;

{ TODO : related MSDN entries, maybe to implement }
// Enabling the Shift Lock Feature on Windows NT 4.0
// Microsoft Knowledge Base Article - 174543
// http://support.microsoft.com/default.aspx?scid=kb;en-us;174543

//=== Various routines =======================================================

procedure JclLocalesInfoList(const Strings: TStrings; InfoType: Integer);
begin
  with TJclLocalesList.Create(lkInstalled) do
  try
    FillStrings(Strings, InfoType);
  finally
    Free;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
