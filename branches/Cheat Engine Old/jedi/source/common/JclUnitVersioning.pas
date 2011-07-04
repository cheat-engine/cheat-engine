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
{ The Original Code is JclUnitVersioning.pas.                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is Andreas Hausladen.                                 }
{ Portions created by Andreas Hausladen are Copyright (C) Andreas Hausladen. All rights reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Andreas Hausladen (ahuser)                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ A unit version information system. It collects information from prepared units by each module.   }
{ It also works with units in DLLs.                                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-01-30 19:32:20 +0100 (mer., 30 janv. 2008)                         $ }
{ Revision:      $Rev:: 2333                                                                     $ }
{ Author:        $Author:: marcovtje                                                             $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclUnitVersioning;

{$I jcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  SysUtils, Contnrs;

type
  PUnitVersionInfo = ^TUnitVersionInfo;
  TUnitVersionInfo = record
    RCSfile: string;  // $'RCSfile$
    Revision: string; // $'Revision$
    Date: string;     // $'Date$     in UTC (GMT)
    LogPath: string;  // logical file path
    Extra: string;    // user defined string
    Data: Pointer;    // user data
  end;

  TUnitVersion = class(TObject)
  private
    FInfo: PUnitVersionInfo;
  public
    constructor Create(AInfo: PUnitVersionInfo);
    function RCSfile: string;
    function Revision: string;
    function Date: string;
    function Extra: string;
    function LogPath: string;
    function Data: Pointer;
    function DateTime: TDateTime;
  end;

  TUnitVersioningModule = class(TObject)
  private
    FInstance: THandle;
    FItems: TObjectList;

    function GetItems(Index: Integer): TUnitVersion;
    function GetCount: Integer;

    procedure Add(Info: PUnitVersionInfo);
    function IndexOfInfo(Info: PUnitVersionInfo): Integer;
  public
    constructor Create(AInstance: THandle);
    destructor Destroy; override;

    function IndexOf(const RCSfile: string; const LogPath: string = '*'): Integer;
    function FindUnit(const RCSfile: string; const LogPath: string = '*'): TUnitVersion;

    property Instance: THandle read FInstance;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TUnitVersion read GetItems; default;
  end;

  TCustomUnitVersioningProvider = class(TObject)
  public
    constructor Create; virtual;
    procedure LoadModuleUnitVersioningInfo(Instance: THandle); virtual;
    procedure ReleaseModuleUnitVersioningInfo(Instance: THandle); virtual;
  end;

  TUnitVersioningProviderClass = class of TCustomUnitVersioningProvider;

  TUnitVersioning = class(TObject)
  private
    FModules: TObjectList;
    FProviders: TObjectList;

    function GetItems(Index: Integer): TUnitVersion;
    function GetCount: Integer;
    function GetModuleCount: Integer;
    function GetModules(Index: Integer): TUnitVersioningModule;

    procedure UnregisterModule(Module: TUnitVersioningModule); overload;
    procedure ValidateModules;
    // These two methods must be virtual because they can be invoked by a DLL.
    // Static linking would mean that the DLL's TUnitVersioning methods handle
    // the call which leads to an access violation.
    procedure Add(Instance: THandle; Info: PUnitVersionInfo); virtual;
    procedure UnregisterModule(Instance: THandle); overload; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterProvider(AProviderClass: TUnitVersioningProviderClass);
    procedure LoadModuleUnitVersioningInfo(Instance: THandle);

    function IndexOf(const RCSfile: string; const LogPath: string = '*'): Integer;
    function FindUnit(const RCSfile: string; const LogPath: string = '*'): TUnitVersion;

    // units by modules
    property ModuleCount: Integer read GetModuleCount;
    property Modules[Index: Integer]: TUnitVersioningModule read GetModules;

    // all units
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TUnitVersion read GetItems; default;
  end;

procedure RegisterUnitVersion(Instance: THandle; const Info: TUnitVersionInfo);
procedure UnregisterUnitVersion(Instance: THandle);

function GetUnitVersioning: TUnitVersioning;

implementation

// Delphi 5 does not know this function //(usc) D6/7 Per does have StartsWith
// a fast version of Pos(SubStr, S) = 1
function StartsWith(const SubStr, S: string): Boolean;
var
  I, Len: Integer;
begin
  Result := False;
  Len := Length(SubStr);
  if Len <= Length(S) then
  begin
    for I := 1 to Len do
      if S[I] <> SubStr[I] then
        Exit;
    Result := True;
  end;
end;

function CompareFilenames(const Fn1, Fn2: string): Integer;
begin
  {$IFDEF MSWINDOWS}
  Result := CompareText(Fn1, Fn2);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Result := CompareStr(Fn1, Fn2);
  {$ENDIF UNIX}
end;

//=== { TUnitVersion } =======================================================

constructor TUnitVersion.Create(AInfo: PUnitVersionInfo);
begin
  inherited Create;
  FInfo := AInfo;
end;

function TUnitVersion.RCSfile: string;
var
  I: Integer;
begin
  Result := Trim(FInfo.RCSfile);
  // the + is to have CVS not touch the string
  if StartsWith('$' + 'RCSfile: ', Result) then // a CVS command
  begin
    Delete(Result, 1, 10);
    Delete(Result, Length(Result) - 1, 2);
    for I := Length(Result) downto 1 do
      if Result[I] = ',' then
      begin
        Delete(Result, I, MaxInt);
        Break;
      end;
  end;
end;

function TUnitVersion.Revision: string;
begin
  Result := Trim(FInfo.Revision);
  if StartsWith('$' + 'Revision: ', Result) then // a CVS command
    Result := Copy(Result, 12, Length(Result) - 11 - 2);
end;

function TUnitVersion.Date: string;
begin
  Result := Trim(FInfo.Date);
  if StartsWith('$' + 'Date: ', Result) then // a CVS command
  begin
    Delete(Result, 1, 7);
    Delete(Result, Length(Result) - 1, 2);
  end;
end;

function TUnitVersion.Data: Pointer;
begin
  Result := FInfo.Data;
end;

function TUnitVersion.Extra: string;
begin
  Result := Trim(FInfo.Extra);
end;

function TUnitVersion.LogPath: string;
begin
  Result := Trim(FInfo.LogPath);
end;

function TUnitVersion.DateTime: TDateTime;
var
  Ps: Integer;
  S: string;
  Error: Integer;
  Year, Month, Day, Hour, Minute, Second: Word;
  TimeSep: Char;
begin
  Result := 0;
  S := Date;

  // date:   yyyy/mm/dd | yyyy-mm-dd | mm/dd/yyyy | mm-dd-yyyy | dd.mm.yyyy
  Ps := Pos('/', S);
  if Ps = 0 then
    Ps := Pos('-', S);
  if Ps <> 0 then
  begin
    if Ps = 5 then
    begin
      // yyyy/mm/dd  |  yyyy-mm-dd
      Val(Copy(S, 1, 4), Year, Error);
      Val(Copy(S, 6, 2), Month, Error);
      Val(Copy(S, 9, 2), Day, Error);
    end
    else
    begin
      // mm/dd/yyyy  |  mm-dd-yyyy
      Val(Copy(S, 1, 2), Month, Error);
      Val(Copy(S, 4, 2), Day, Error);
      Val(Copy(S, 7, 4), Year, Error);
    end;
  end
  else
  begin
    Ps := Pos('.', S);
    if Ps <> 0 then
    begin
      // dd.mm.yyyy
      Val(Copy(S, 1, 2), Day, Error);
      Val(Copy(S, 4, 2), Month, Error);
      Val(Copy(S, 7, 4), Year, Error);
    end
    else
      Exit;
  end;

  // time:   hh:mm:ss  |  hh/mm/ss
  Ps := Pos(' ', S);
  S := Trim(Copy(S, Ps + 1, MaxInt));

  Ps := Pos(':', S);
  if Ps <> 0 then
    TimeSep := ':'
  else
  begin
    Ps := Pos('/', S);
    TimeSep := '/';
  end;
  Val(Copy(S, 1, Ps - 1), Hour, Error);
  Delete(S, 1, Ps);
  Ps := Pos(TimeSep, S);
  Val(Copy(S, 1, Ps - 1), Minute, Error);
  Delete(S, 1, Ps);
  Ps := Pos(TimeSep, S);
  if Ps = 0 then
    Ps := Length(S) + 1;
  Val(Copy(S, 1, Ps - 1), Second, Error);

  Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0);
end;

//=== { TUnitVersioningModule } ==============================================

constructor TUnitVersioningModule.Create(AInstance: THandle);
begin
  inherited Create;
  FInstance := AInstance;
  FItems := TObjectList.Create;
end;

destructor TUnitVersioningModule.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TUnitVersioningModule.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TUnitVersioningModule.GetItems(Index: Integer): TUnitVersion;
begin
  Result := TUnitVersion(FItems[Index]);
end;

procedure TUnitVersioningModule.Add(Info: PUnitVersionInfo);
begin
  FItems.Add(TUnitVersion.Create(Info));
end;

function TUnitVersioningModule.IndexOfInfo(Info: PUnitVersionInfo): Integer;
begin
  for Result := 0 to FItems.Count - 1 do
    if Items[Result].FInfo = Info then
      Exit;
  Result := -1;
end;

function TUnitVersioningModule.FindUnit(const RCSfile: string; const LogPath: string): TUnitVersion;
var
  Index: Integer;
begin
  Index := IndexOf(RCSfile, LogPath);
  if Index <> -1 then
    Result := Items[Index]
  else
    Result := nil;
end;

function TUnitVersioningModule.IndexOf(const RCSfile: string; const LogPath: string): Integer;
begin
  for Result := 0 to FItems.Count - 1 do
    if CompareFilenames(Items[Result].RCSfile, RCSfile) = 0 then
      if LogPath = '*' then
        Exit
      else
      if CompareFilenames(LogPath, Trim(Items[Result].LogPath)) = 0 then
        Exit;
  Result := -1;
end;

//=== { TCustomUnitVersioningProvider } ======================================

constructor TCustomUnitVersioningProvider.Create;
begin
  inherited Create;
end;

procedure TCustomUnitVersioningProvider.LoadModuleUnitVersioningInfo(Instance: THandle);
begin
//
end;

procedure TCustomUnitVersioningProvider.ReleaseModuleUnitVersioningInfo(Instance: THandle);
begin
//
end;

//=== { TUnitVersioning } ====================================================

constructor TUnitVersioning.Create;
begin
  inherited Create;
  FModules := TObjectList.Create;
  FProviders := TObjectList.Create;
end;

destructor TUnitVersioning.Destroy;
begin
  FProviders.Free;
  FModules.Free;
  inherited Destroy;
end;

procedure TUnitVersioning.Add(Instance: THandle; Info: PUnitVersionInfo);
var
  I: Integer;
  Module: TUnitVersioningModule;
begin
  for I := 0 to FModules.Count - 1 do
    if Modules[I].Instance = Instance then
    begin
      if Modules[I].IndexOfInfo(Info) = -1 then
        Modules[I].Add(Info);
      Exit;
    end;
  // create a new module entry
  Module := TUnitVersioningModule.Create(Instance);
  FModules.Add(Module);
  Module.Add(Info);
end;

procedure TUnitVersioning.UnregisterModule(Instance: THandle);
var
  I: Integer;
begin
  for I := FModules.Count - 1 downto 0 do
    if Modules[I].Instance = Instance then
    begin
      FModules.Delete(I);
      Break;
    end;
  for I := 0 to FProviders.Count -1 do
    TCustomUnitVersioningProvider(FProviders[I]).ReleaseModuleUnitVersioningInfo(Instance);
end;

procedure TUnitVersioning.UnregisterModule(Module: TUnitVersioningModule);
begin
  FModules.Remove(Module);
end;

function TUnitVersioning.GetCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  ValidateModules;
  for I := 0 to FModules.Count - 1 do
    Inc(Result, Modules[I].Count);
end;

function TUnitVersioning.GetItems(Index: Integer): TUnitVersion;
var
  Cnt, I: Integer;
begin
  Result := nil;
  ValidateModules;
  Cnt := 0;
  for I := 0 to FModules.Count - 1 do
  begin
    if Index < Cnt + Modules[I].Count then
    begin
      Result := Modules[I].Items[Index - Cnt];
      Break;
    end;
    Inc(Cnt, Modules[I].Count);
  end;
end;

function TUnitVersioning.GetModuleCount: Integer;
begin
  ValidateModules;
  Result := FModules.Count;
end;

function TUnitVersioning.GetModules(Index: Integer): TUnitVersioningModule;
begin
  Result := TUnitVersioningModule(FModules[Index]);
end;

{$UNDEF FPCUNIX}   // Temporary, will move to .inc's in time.
{$IFDEF FPC}
 {$IFDEF UNIX}
 {$DEFIN FPCUNIX}
{$ENDIF}
{$ENDIF}

procedure TUnitVersioning.ValidateModules;
var
  I: Integer;
  Buffer: string;
begin
  for I := FModules.Count - 1 downto 0 do
  begin
    SetLength(Buffer, 1024);
    {$IFDEF FPCUNIX}
    if dlsym(Pointer(Modules[I].Instance), '_init') = nil then
    {$ELSE}
    if GetModuleFileName(Modules[I].Instance, PChar(Buffer), 1024) = 0 then
    {$ENDIF}
      // This module is no more in memory but has not unregistered itself so
      // unregister it here.
      UnregisterModule(Modules[I]);
  end;
end;

function TUnitVersioning.FindUnit(const RCSfile: string; const LogPath: string): TUnitVersion;
var
  I: Integer;
begin
  for I := 0 to FModules.Count - 1 do
  begin
    Result := Modules[I].FindUnit(RCSfile, LogPath);
    if Result <> nil then
      Exit;
  end;
  Result := nil;
end;

function TUnitVersioning.IndexOf(const RCSfile: string; const LogPath: string): Integer;
var
  I, Cnt, Index: Integer;
begin
  Result := -1;
  Cnt := 0;
  for I := 0 to FModules.Count - 1 do
  begin
    Index := Modules[I].IndexOf(RCSfile, LogPath);
    if Index <> -1 then
    begin
      Result := Cnt + Index;
      Break;
    end;
    Inc(Cnt, Modules[I].Count);
  end;
end;

procedure TUnitVersioning.RegisterProvider(AProviderClass: TUnitVersioningProviderClass);
var
  I, Idx: Integer;
begin
  Idx := -1;
  for I := 0 to FProviders.Count - 1 do
    if TObject(FProviders[I]).ClassType = AProviderClass then
    begin
      Idx := I;
      Break;
    end;
  if Idx = -1 then
    FProviders.Add(AProviderClass.Create);
end;

procedure TUnitVersioning.LoadModuleUnitVersioningInfo(Instance: THandle);
var
  I: Integer;
begin
  for I := 0 to FProviders.Count - 1 do
    TCustomUnitVersioningProvider(FProviders[I]).LoadModuleUnitVersioningInfo(Instance);
end;

function GetNamedProcessAddress(const Id: ShortString; out RefCount: Integer): Pointer; forward;
  // Returns a 3820 Bytes large block [= 4096 - 276 = 4096 - (8+256+4+8)]
  // max 20 blocks can be allocated
function ReleaseNamedProcessAddress(P: Pointer): Integer; forward;

// (rom) PAGE_OFFSET is clearly Linux specific
{$IFDEF LINUX}
const
  PAGE_OFFSET = $C0000000; // from linux/include/asm-i386/page.h
{$ENDIF LINUX}

const
  Signature1 = $ABCDEF0123456789;
  Signature2 = $9876543210FEDCBA;

type
  PNPARecord = ^TNPARecord;
  TNPARecord = record
    Signature1: Int64;
    Id: ShortString;
    RefCount: Integer;
    Signature2: Int64;
    Data: record end;
  end;

function GetNamedProcessAddress(const Id: ShortString; out RefCount: Integer): Pointer;
const
  MaxPages = 20;
var
  {$IFDEF MSWINDOWS}
  SysInfo: TSystemInfo;
  MemInfo: TMemoryBasicInformation;
  pid: THandle;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  pid: __pid_t;
  {$ENDIF LINUX}
  Requested, Allocated: PNPARecord;
  Pages: Integer;
  PageSize: Cardinal;
  MaximumApplicationAddress: Pointer;
begin
  RefCount := 0;
  {$IFDEF MSWINDOWS}
  GetSystemInfo(SysInfo);
  PageSize := SysInfo.dwPageSize;
  pid := GetCurrentProcessId;
  MaximumApplicationAddress := SysInfo.lpMaximumApplicationAddress;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  PageSize := getpagesize;
  pid := getpid;
  MaximumApplicationAddress := Pointer(PAGE_OFFSET - 1);
  {$ENDIF UNIX}
  Pages := 0;
  repeat
    Requested := MaximumApplicationAddress;
    Requested := Pointer((Cardinal(Requested) div $10000) * $10000);
    Dec(Cardinal(Requested), Pages * $10000);
    Requested := Pointer((Cardinal(Requested) div PageSize) * PageSize);
    {$IFDEF MSWINDOWS}
    Allocated := VirtualAlloc(Requested, PageSize, MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE);
    if Assigned(Allocated) and (Requested <> Allocated) then
    begin
      // We got relocated (should not happen at all)
      VirtualFree(Allocated, 0, MEM_RELEASE);
      Inc(Pages);
      Continue;
    end;
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    // Do not use MAP_FIXED because it replaces the already allocated map by a
    // new map.
    Allocated := mmap(Requested, PageSize, PROT_READ or PROT_WRITE,
      MAP_PRIVATE or MAP_ANONYMOUS, 0, 0);
    if Allocated = MAP_FAILED then
    begin
      // Prevent SEGV by signature-test code and try the next memory page.
      Inc(Pages);
      Continue;
    end
    else
    if Allocated <> Requested then
    begin
      // It was relocated, means the requested address is already allocated
      munmap(Allocated, PageSize);
      Allocated := nil;
    end;
    {$ENDIF UNIX}

    if Assigned(Allocated) then
      Break // new block allocated
    else
    begin
      {$IFDEF MSWINDOWS}
      VirtualQuery(Requested, MemInfo, SizeOf(MemInfo));
      if (MemInfo.RegionSize >= SizeOf(TNPARecord)) and
        (MemInfo.Protect and PAGE_READWRITE = PAGE_READWRITE) then
      {$ENDIF MSWINDOWS}
      {$IFDEF UNIX}
      try
      {$ENDIF UNIX}
        if (Requested.Signature1 = Signature1 xor pid) and
          (Requested.Signature2 = Signature2 xor pid) and
          (Requested.Id = Id) then
          Break; // Found correct, already existing block.
      {$IFDEF UNIX}
      except
        // ignore
      end;
      {$ENDIF UNIX}
    end;

    Inc(Pages);
    Requested := nil;
  until Pages > MaxPages;

  Result := nil;
  if Allocated <> nil then
  begin
    if Requested = Allocated then
    begin
      // initialize the block
      Requested.Signature1 := Signature1 xor pid;
      Requested.Id := Id;
      Requested.Signature2 := Signature2 xor pid;
      Requested.RefCount := 1;
      Result := @Requested.Data;
      RefCount := 1;
    end;
  end
  else
  if Requested <> nil then
  begin
    Inc(Requested.RefCount);
    Result := @Requested.Data;
    RefCount := Requested.RefCount;
  end;
end;

function ReleaseNamedProcessAddress(P: Pointer): Integer;
var
  Requested: PNPARecord;
begin
  Result := 0;
  if P <> nil then
  begin
    Requested := PNPARecord(Cardinal(P) - SizeOf(TNPARecord));
    Dec(Requested.RefCount);
    Result := Requested.RefCount;
    if Requested.RefCount = 0 then
      {$IFDEF MSWINDOWS}
      VirtualFree(Requested, 0, MEM_RELEASE);
      {$ENDIF MSWINDOWS}
      {$IFDEF UNIX}
      munmap(Requested, getpagesize);
      {$ENDIF UNIX}
  end;
end;

type
  PUnitVersioning = ^TUnitVersioning;

var
  UnitVersioningOwner: Boolean = False;
  GlobalUnitVersioning: TUnitVersioning = nil;
  UnitVersioningNPA: PUnitVersioning = nil;

function GetUnitVersioning: TUnitVersioning;
var
  RefCount: Integer;
begin
  if GlobalUnitVersioning = nil then
  begin
    UnitVersioningNPA := GetNamedProcessAddress('UnitVersioning', RefCount);
    if UnitVersioningNPA <> nil then
    begin
      GlobalUnitVersioning := UnitVersioningNPA^;
      if (GlobalUnitVersioning = nil) or (RefCount = 1) then
      begin
        GlobalUnitVersioning := TUnitVersioning.Create;
        UnitVersioningNPA^ := GlobalUnitVersioning;
        UnitVersioningOwner := True;
      end;
    end
    else
    begin
      GlobalUnitVersioning := TUnitVersioning.Create;
      UnitVersioningOwner := True;
    end;
  end
  else
  if UnitVersioningNPA <> nil then
    GlobalUnitVersioning := UnitVersioningNPA^; // update (maybe the owner has destroyed the instance)
  Result := GlobalUnitVersioning;
end;

procedure FinalizeUnitVersioning;
var
  RefCount: Integer;
begin
  try
    if GlobalUnitVersioning <> nil then
    begin
      RefCount := ReleaseNamedProcessAddress(UnitVersioningNPA);
      if UnitVersioningOwner then
      begin
        if RefCount > 0 then
          UnitVersioningNPA^ := nil;
        GlobalUnitVersioning.Free;
      end;
      GlobalUnitVersioning := nil;
    end;
  except
    // ignore - should never happen
  end;
end;

procedure RegisterUnitVersion(Instance: THandle; const Info: TUnitVersionInfo);
var
  UnitVersioning: TUnitVersioning;
begin
  UnitVersioning := GetUnitVersioning;
  if Assigned(UnitVersioning) then
    UnitVersioning.Add(Instance, @Info);
end;

procedure UnregisterUnitVersion(Instance: THandle);
var
  UnitVersioning: TUnitVersioning;
begin
  UnitVersioning := GetUnitVersioning;
  if Assigned(UnitVersioning) then
    UnitVersioning.UnregisterModule(Instance);
end;

const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclUnitVersioning.pas $';
    Revision: '$Revision: 2333 $';
    Date: '$Date: 2008-01-30 19:32:20 +0100 (mer., 30 janv. 2008) $';
    LogPath: 'JCL\common';
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  FinalizeUnitVersioning;

end.

