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
{ The Original Code is JclCOM.pas.                                                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is Kevin S. Gallagher. Portions created by Kevin S.   }
{ Gallagher are Copyright (C) Kevin S. Gallagher. All Rights Reserved.                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel                                                                              }
{   Robert Marquardt (marquardt)                                                                   }
{   Scott Price (scottprice)                                                                       }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains Various COM (Component Object Model) utility routines.                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclCOM;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, ActiveX, Classes,
  JclBase;

// Various definitions
const
  { Class ID's that may be reused }
  CLSID_StdComponentCategoriesMgr: TGUID = '{0002E005-0000-0000-C000-000000000046}';

  CATID_SafeForInitializing: TGUID = '{7DD95802-9882-11CF-9FA9-00AA006C42C4}';
  CATID_SafeForScripting: TGUID = '{7DD95801-9882-11CF-9FA9-00AA006C42C4}';

  icMAX_CATEGORY_DESC_LEN = 128;

type
  { For use with the Internet Explorer Component Categories Routines.  May be Reused. }
  TArrayCatID = array [0..0] of TGUID;

// Exception classes
type
  EInvalidParam = class(EJclError);

// DCOM and MDAC Related Tests and Utility Routines
function IsDCOMInstalled: Boolean;
function IsDCOMEnabled: Boolean;
function GetDCOMVersion: string;
function GetMDACVersion: string;

// Other Marshalling Routines to complement "CoMarshalInterThreadInterfaceInStream"
{ These routines will provide the ability to marshal an interface for a separate
  process or even for access by a separate machine.  However, to make things
  familiar to users of the existing CoMarshal... routine, I have kept the required
  parameters the same, apart from the "stm" type now being a Var rather than just
  an Out - to allow a little flexibility if the developer wants the destination
  to be a specific stream, otherwise it creates one into the passed variable! }

function MarshalInterThreadInterfaceInVarArray(const iid: TIID;
  unk: IUnknown; var VarArray: OleVariant): HRESULT;
function MarshalInterProcessInterfaceInStream(const iid: TIID;
  unk: IUnknown; var stm: IStream): HRESULT;
function MarshalInterProcessInterfaceInVarArray(const iid: TIID;
  unk: IUnknown; var VarArray: OleVariant): HRESULT;
function MarshalInterMachineInterfaceInStream(const iid: TIID;
  unk: IUnknown; var stm: IStream): HRESULT;
function MarshalInterMachineInterfaceInVarArray(const iid: TIID;
  unk: IUnknown; var VarArray: OleVariant): HRESULT;

// Internet Explorer Component Categories Routines
{ These routines help with the registration of:
    - Safe-Initialization &
    - Safe-for-Scripting
  of ActiveX controls or COM Automation Servers intended to be used in
  HTML pages displayed in Internet Explorer }
{ Conversion of an example found in Microsoft Development Network document:
  MSDN Home >  MSDN Library >  ActiveX Controls >  Overviews/Tutorials
  Safe Initialization and Scripting for ActiveX Controls }

function CreateComponentCategory(const CatID: TGUID; const sDescription: string): HRESULT;
function RegisterCLSIDInCategory(const ClassID: TGUID; const CatID: TGUID): HRESULT;
function UnRegisterCLSIDInCategory(const ClassID: TGUID; const CatID: TGUID): HRESULT;

// Stream Related Routines
{ IDE ISSUE:  These need to be at the bottom of the interface definition as otherwise
              the CTRL+SHIFT+ Up/Down arrows feature no-longer operates }

function ResetIStreamToStart(Stream: IStream): Boolean;
function SizeOfIStreamContents(Stream: IStream): Largeint;

{ Use VarIsEmpty to determine the result of the following XStreamToVariantArray routines!
  VarIsEmptry will return True if VarClear was called - indicating major problem! }

function StreamToVariantArray(Stream: TStream): OleVariant; overload;
function StreamToVariantArray(Stream: IStream): OleVariant; overload;

procedure VariantArrayToStream(VarArray: OleVariant; var Stream: TStream); overload;
procedure VariantArrayToStream(VarArray: OleVariant; var Stream: IStream); overload;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclCOM.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF FPC}
  Types,
  {$ENDIF FPC}
  SysUtils,
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  JclFileUtils, JclRegistry, JclResources, JclSysInfo, JclWin32;

{implementation Constants - may be reused by more than one routine }

const
  pcOLE32 = 'OLE32.dll';

  { TODO : Utility routine here might need to be re-vamped with the
           use of JclUnicode unit in mind. }

function StringToWideString(const Str: string): WideString;
var
  iLen: Integer;
begin
  iLen:= Length(Str) + 1;
  SetLength(Result, (iLen - 1));
  StringToWideChar(Str, PWideChar(Result), iLen);
end;

//=== DCOM and MDAC Related Tests and Utility Routines =======================

function IsDCOMInstalled: Boolean;
var
  OLE32: HMODULE;
begin
  { DCOM is installed by default on all but Windows 95 }
  Result := not (GetWindowsVersion in [wvUnknown, wvWin95, wvWin95OSR2]);
  if not Result then
  begin
    OLE32 := SafeLoadLibrary(pcOLE32);
    if OLE32 > 0 then
    try
      Result := GetProcAddress(OLE32, PChar('CoCreateInstanceEx')) <> nil;
    finally
      FreeLibrary(OLE32);
    end;
  end;
end;

function IsDCOMEnabled: Boolean;
var
  RegValue: string;
begin
  RegValue := RegReadString(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\OLE', 'EnableDCOM');
  Result := (RegValue = 'y') or (RegValue = 'Y');
end;

function GetDCOMVersion: string;
const
  DCOMVersionKey: PChar = 'CLSID\{bdc67890-4fc0-11d0-a805-00aa006d2ea4}\InstalledVersion';
begin
  { NOTE:  This does not work on Windows NT/2000! For a list of DCOM versions:
      http://support.microsoft.com/support/kb/articles/Q235/6/38.ASP }
  Result := '';
  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and IsDCOMEnabled then
    Result := RegReadString(HKEY_CLASSES_ROOT, DCOMVersionKey, '')
  else
    { Possibly from DComExt.dll ‘Product Version’ }
    Result := 'DCOM Version Unknown';
end;

{ NOTE:  Checking whether MDAC is installed at all can be done by querying the
         Software\Microsoft\DataAccess key for the FullInstallVer or
         Fill32InstallVer values. Windows 2000 always installs MDAC 2.5 }

function GetMDACVersion: string;
var
  Key: string;
  DLL: string;
  Version: TJclFileVersionInfo;
begin
  Result := '' ;
  Key := RegReadString(HKEY_CLASSES_ROOT, 'ADODB.Connection\CLSID', '');
  DLL := RegReadString(HKEY_CLASSES_ROOT, 'CLSID\' + Key + '\InprocServer32', '');
  if VersionResourceAvailable(DLL) then
  begin
    Version := TJclFileVersionInfo.Create(DLL);
    try
      Result := Version.ProductVersion;
    finally
      FreeAndNil(Version);
    end;
  end;
end;

// Other Marshalling Routines to complement "CoMarshalInterThreadInterfaceInStream"

function MarshalInterThreadInterfaceInVarArray(const iid: TIID; unk: IUnknown;
  var VarArray: OleVariant): HRESULT;
var
  msData: TMemoryStream;
  itfStream: IStream;
begin
  { TODO -cTest : D4, D5, D6 (CBx ??) }
  try
    { Will need a stream to obtain the data initially before creating the Variant Array }
    msData := TMemoryStream.Create;

    itfStream := (TStreamAdapter.Create(msData, soOwned) as IStream);

    { Probably would never get here in such a condition, but just in case }
    if itfStream = nil then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;

    if itfStream <> nil then
    begin
      { Different Machine }
      Result := CoMarshalInterThreadInterfaceInStream(iid, unk, itfStream);

      if Result <> S_OK then
        Exit;

      VarArray := StreamToVariantArray(itfStream);

      if VarIsNull(VarArray) or VarIsEmpty(VarArray) then
        Result := E_FAIL;
    end
    else
      { TODO : Most likely out of memory, though should not reach here }
      Result := E_POINTER;
  except
    Result := E_UNEXPECTED;
  end;
end;

function MarshalInterProcessInterfaceInStream(const iid: TIID; unk: IUnknown;
  var stm: IStream): HRESULT;
var
  msData: TMemoryStream;
begin
  { TODO -cTest : D4 (CBx ??) }
  try
    { If passed a variable which doesn't contain a valid stream, create and return }
    if stm = nil then
    begin
      msData := TMemoryStream.Create;

      stm := (TStreamAdapter.Create(msData, soOwned) as IStream);

      { Probably would never get here in such a condition, but just in case }
      if stm = nil then
      begin
        Result := E_OUTOFMEMORY;
        Exit;
      end;
    end
    else
      ResetIStreamToStart(stm);

    if stm <> nil then
      { Same Machine, Different Process}
      Result := CoMarshalInterface(stm, iid, unk, MSHCTX_LOCAL, nil, MSHLFLAGS_NORMAL)
    else
      { TODO : Most likely out of memory, though should not reach here }
      Result := E_POINTER;
  except
    Result := E_UNEXPECTED;
  end;
end;

function MarshalInterProcessInterfaceInVarArray(const iid: TIID;
  unk: IUnknown; var VarArray: OleVariant): HRESULT;
var
  itfStream: IStream;
begin
  { TODO -cTest : D4 (CBx ??) }
  Result := MarshalInterProcessInterfaceInStream(iid, unk, itfStream);

  if Result <> S_OK then
    Exit;

  { TODO : Add compiler support for using a VCL Stream instead of an IStream here }
  { Otherwise convert from IStream into Variant Array }
  VarArray := StreamToVariantArray(itfStream);

  if VarIsNull(VarArray) or VarIsEmpty(VarArray) then
    Result := E_FAIL;
end;

function MarshalInterMachineInterfaceInStream(const iid: TIID; unk: IUnknown;
  var stm: IStream): HRESULT;
var
  msData: TMemoryStream;
begin
  { TODO -cTest : D4 (CBx ??) Have no need for it myself at present. }
  try
    { If passed a variable which doesn't contain a valid stream, create and return }
    if stm = nil then
    begin
      msData := TMemoryStream.Create;

      stm := (TStreamAdapter.Create(msData, soOwned) as IStream);

      { Probably would never get here in such a condition, but just in case }
      if stm = nil then
      begin
        Result := E_OUTOFMEMORY;
        Exit;
      end;
    end
    else
      ResetIStreamToStart(stm);

    if stm <> nil then
      { Different Machine }
      Result := CoMarshalInterface(stm, iid, unk, MSHCTX_DIFFERENTMACHINE, nil, MSHLFLAGS_NORMAL)
    else
      { TODO : Most likely out of memory, though should not reach here }
      Result := E_POINTER;
  except
    Result := E_UNEXPECTED;
  end;
end;

function MarshalInterMachineInterfaceInVarArray(const iid: TIID; unk: IUnknown;
  var VarArray: OleVariant): HRESULT;
var
  itfStream: IStream;
begin
  { TODO -cTest : D4 (CBx ??) }
  Result := MarshalInterMachineInterfaceInStream(iid, unk, itfStream);

  if Result <> S_OK then
    Exit;

  { TODO : Add compiler support for using a VCL Stream instead of an IStream here }
  { Otherwise convert from IStream into Variant Array }
  VarArray := StreamToVariantArray(itfStream);

  if VarIsNull(VarArray) or VarIsEmpty(VarArray) then
    Result := E_FAIL;
end;

//=== Internet Explorer Component Categories Routines ========================

function CreateComponentCategory(const CatID: TGUID; const sDescription: string): HRESULT;
var
  CatRegister: ICatRegister;
  hr: HRESULT;
  CatInfo: TCATEGORYINFO;
  iLen: Integer;
  sTemp: string;
  wsTemp: WideString;
begin
  { TODO -cTest : D4 (CBx ??) }
  CatRegister := nil;

  hr := CoCreateInstance(CLSID_StdComponentCategoriesMgr,
          nil, CLSCTX_INPROC_SERVER, ICatRegister, CatRegister);

  if Succeeded(hr) then
    try
      (* Make sure the:
           HKCR\Component Categories\{..catid...}
         key is registered *)
      CatInfo.catid := CatID;
      CatInfo.lcid := MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US); // english

      { Make sure the provided description is not too long.
        Only copy the first 127 characters if it is. }
      iLen := Length(sDescription);
      if iLen > icMAX_CATEGORY_DESC_LEN then
         iLen := icMAX_CATEGORY_DESC_LEN;

      sTemp := Copy(sDescription, 1, iLen);
      wsTemp := StringToWideString(sTemp); 

      Move(Pointer(wsTemp)^, CatInfo.szDescription, (iLen * SizeOf(WideChar)));

      hr := CatRegister.RegisterCategories(1, @CatInfo);
    finally
      CatRegister := nil;
    end;

  { Return the appropriate Result }
  Result := hr;
end;

function RegisterCLSIDInCategory(const ClassID: TGUID; const CatID: TGUID): HRESULT;
var
  CatRegister: ICatRegister;
  hr: HRESULT;
  arCatID: TArrayCatID;
begin
  { TODO -cTest : D4 (CBx ??) }
  { Register your component categories information }
  CatRegister := nil;
  hr := CoCreateInstance(CLSID_StdComponentCategoriesMgr,
          nil, CLSCTX_INPROC_SERVER, ICatRegister, CatRegister);

  if Succeeded(hr) then
    try
      { Register this category as being "implemented" by the class }
      arCatID[0] := CatID;
      hr := CatRegister.RegisterClassImplCategories(ClassID, 1, @arCatID);
    finally
      CatRegister := nil;
    end;

  { Return the appropriate Result }
  Result := hr;
end;

function UnRegisterCLSIDInCategory(const ClassID: TGUID; const CatID: TGUID): HRESULT;
var
  CatRegister: ICatRegister;
  hr: HRESULT;
  arCatID: TArrayCatID;
begin
  { TODO -cTest : D4 (CBx ??) }
  CatRegister := nil;

  hr := CoCreateInstance(CLSID_StdComponentCategoriesMgr,
          nil, CLSCTX_INPROC_SERVER, ICatRegister, CatRegister);

  if Succeeded(hr) then
    try
      { Unregister this category as being "implemented" by the class }
      arCatID[0] := CatID;
      hr := CatRegister.UnRegisterClassImplCategories(ClassID, 1, @arCatID);
    finally
      CatRegister := nil;
    end;

  { Return the appropriate Result }
  Result := hr;
end;

//=== Stream Related Routines ================================================

function ResetIStreamToStart(Stream: IStream): Boolean;
var
  i64Pos: Largeint;
  hrSeek: HRESULT;
begin
  { TODO -cTest : D4 (CBx ??) }
  { Try to get the current stream position, and reset to start if not already there }
  if Succeeded(Stream.Seek(0, STREAM_SEEK_CUR, i64Pos)) then
  begin
    if i64Pos = 0 then
      hrSeek := S_OK
    else
      hrSeek := Stream.Seek(0, STREAM_SEEK_SET, i64Pos);
      { Another possible option was seen as:
        - Stream.Seek(0, STREAM_SEEK_SET, NULL); }

    Result := (hrSeek = S_OK);
  end
  else
    Result := False;
end;

function SizeOfIStreamContents(Stream: IStream): Largeint;
var
  stat: TStatStg;
begin
  { TODO -cTest : D4 (CBx ??) }
  { If we can't determine the size of the Stream, then return -1 for Unattainable }
  if Succeeded(Stream.Stat(stat, STATFLAG_NONAME)) then
    Result := stat.cbSize
  else
    Result := -1;
end;

function StreamToVariantArray(Stream: TStream): OleVariant;
var
  pLocked: Pointer;
begin
  { Use VarIsEmpty to determine the result of this method!
    VarIsEmptry will return True if VarClear was called - indicating major problem! }

  { TODO -cTest : D4 (CBx ??) }
  { Obviously, we must have a valid stream to perform this on }
  if not Assigned(Stream) then
    raise EInvalidParam.CreateRes(@RsComInvalidParam);

  if Stream.Size > 0 then
  begin
    Result := VarArrayCreate([0, Stream.Size - 1], varByte);
    try
      pLocked := VarArrayLock(Result);
      try
        Stream.Position := 0;
        Stream.Read(pLocked^, Stream.Size);
      finally
        VarArrayUnlock(Result);
      end;
    except
      { If we get an exception, clean up the Variant so as not to return incomplete data! }
      VarClear(Result);

      { Alternative:  Re-Raise this Exception
      raise; }
    end;
  end
  else
    { Stream has no data! }
    Result := Null;
end;

function StreamToVariantArray(Stream: IStream): OleVariant;
var
  pLocked: Pointer;
  iSize: Largeint;
  iReadCount: LongInt;
begin
  { Use VarIsEmpty to determine the result of this method!
    VarIsEmptry will return True if VarClear was called - indicating major problem! }

  { TODO -cTest : D4 (CBx ??) }
  { Obviously, we must have a valid stream to perform this on }
  if not Assigned(Stream) then
    raise EInvalidParam.CreateRes(@RsComInvalidParam);

  iSize := SizeOfIStreamContents(Stream);
  if iSize > 0 then
  begin
    if ResetIStreamToStart(Stream) then
    begin
      Result := VarArrayCreate([0, iSize - 1], varByte);
      try
        pLocked := VarArrayLock(Result);
        try
          Stream.Read(pLocked, iSize, @iReadCount);

          if iReadCount <> iSize then
            { Error!  Didn't read all content! }
            raise EInOutError.CreateRes(@RsComFailedStreamRead);
        finally
          VarArrayUnlock(Result);
        end;
      except
        { If we get an exception, clean up the Variant so as not to return incomplete data! }
        VarClear(Result);

        { Alternative:  Re-Raise this Exception
        raise; }
      end;
    end
    else
      { Unable to Reset the Stream to Start!  Return Null Variant }
      Result := Null;
  end
  else
    { Stream has no data! }
    Result := Null;
end;

procedure VariantArrayToStream(VarArray: OleVariant; var Stream: TStream);
var
  pLocked: Pointer;
begin
  { TODO -cTest : D4 (CBx ??) }
  { Check if the Variant is Empty or Null }
  if VarIsEmpty(VarArray) or VarIsNull(VarArray) then
    raise EInvalidParam.CreateRes(@RsComInvalidParam);

  { TODO : Should we allow them to write to the Stream, not matter what position it is at? }
  if Assigned(Stream) then
    Stream.Position := 0
  else
    Stream := TMemoryStream.Create;

  Stream.Size := VarArrayHighBound(VarArray, 1) - VarArrayLowBound(VarArray, 1) + 1;
  pLocked := VarArrayLock(VarArray);
  try
    Stream.Write(pLocked^, Stream.Size);
  finally
    VarArrayUnlock(VarArray);
    Stream.Position := 0;
  end;
end;

procedure VariantArrayToStream(VarArray: OleVariant; var Stream: IStream);
var
  pLocked: Pointer;
  bCreated: Boolean;
  iSize: Largeint;
  iWriteCount: LongInt;
begin
  { TODO -cTest : D4 (CBx ??) }
  { Check if the Variant is Empty or Null }
  if VarIsEmpty(VarArray) or VarIsNull(VarArray) then
    raise EInvalidParam.CreateRes(@RsComInvalidParam);

  bCreated := False;

  { TODO : Should we allow them to write to the Stream, not matter what position it is at? }
  if Assigned(Stream) then
    ResetIStreamToStart(Stream)
  else
  begin
    Stream := (TStreamAdapter.Create(TMemoryStream.Create, soOwned) as IStream);
    bCreated := True;
  end;

  { Check to ensure creation went well, otherwise we might have run out of memory }
  if Stream <> nil then
  begin
    iSize := VarArrayHighBound(VarArray, 1) - VarArrayLowBound(VarArray, 1) + 1;
    try
      Stream.SetSize(iSize);
      pLocked := VarArrayLock(VarArray);
      try
        Stream.Write(pLocked, iSize, @iWriteCount);

        if iWriteCount <> iSize then
          raise EInOutError.CreateRes(@RsComFailedStreamWrite);
      finally
        VarArrayUnlock(VarArray);
        ResetIStreamToStart(Stream);
      end;
    except
      if bCreated then
        Stream := nil;

      raise; { Re-Raise this Exception }
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
