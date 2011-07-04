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
{ The Original Code is JclStructStore.pas.                                                         }
{                                                                                                  }
{ The Initial Developer of the Original Code is Peter Thornqvist.                                  }
{ Portions created by Peter Thornqvist are Copyright (C) Peter Thornqvist. All Rights Reserved.    }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   A. Schmidt (shmia (at) bizerba.de)                                                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ MS Structured storage class wrapper                                                              }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

{
Description:

Wrapper around MS structured storage library to simplify handling compound files
(the filetype used in Word, Excel, newer versions of Access, Project et al).

Note that MS documentation uses the terms "Storage" and "Streams". I've decided to use the
names Folders (for Storages) and Files (for Streams) since that more closely
resembles how the content of a compound file is percieved and used.

Very briefly, a compound (or structured) file is a disk file that contains data organized
in an internal structure. The structure is similar to a normal file system
in that the file can contain folders (storages) and subfiles (streams). Folders
can contain subfolders and files but no data of it's own, files can contain data but no subitems.

This implementation is simplified in a number of ways compared to what can actually be
done with the IStorage implementation in Windows:

* creating a new file with the same name as an existing will silently overwrite
  the existing file, even if it's not a compound file
* SetClassID has not been implemented / surfaced
* STGM_SIMPLE, STGM_PRIORITY, STGM_NOSCRATCH, STGM_FAILIFTHERE and a few other esoteric flags are not supported

BTW, what's the difference between "compound" and "structured"? MS seems a bit confused
themselves on this topic, but it looks like the term "compound file" is used to
describe the actual Microsoft OLE/COM implementation of the theoretical idea
of "structured storage"...

-----------------------------------------------------------------------------}

unit JclStructStorage;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes, SysUtils, ActiveX,
  JclBase;

type
  EJclStructStorageError = class(EJclError);
  TJclStructStorageAccessMode = (smOpenRead, smOpenWrite, smCreate, smShareDenyRead, smShareDenyWrite, smTransacted);
  TJclStructStorageAccessModes = set of TJclStructStorageAccessMode;

  TJclStructStorageFolder = class(TPersistent)
  private
    function GetName: string;
  protected
    FStorage: IStorage;
    FLastError: HRESULT;
    FFileName: string;
    FAccessMode: TJclStructStorageAccessModes;
    FConvertedMode: UINT;
    procedure Check;
    function CheckResult(HR: HRESULT): Boolean;
    // Calls to Dest.Assign will eventually end up here.
    // AssignTo is implemented as a call to IStorage.CopyTo(Dest)
    // This method merges elements contained in the source storage object with
    // those already present in the destination. The layout of the destination
    // storage object may differ from the source storage object.
    // The copy process is recursive, invoking IStorage::CopyTo and IStream::CopyTo
    // on the elements nested inside the source.
    // When copying a stream on top of an existing stream with the same name,
    // the existing stream is first removed and then replaced with the source stream.
    // When copying a storage on top of an existing storage with the same name,
    // the existing storage is not removed. As a result, after the copy operation,
    // the destination IStorage contains older elements, unless they were replaced by
    // newer ones with the same names.
    procedure AssignTo(Dest: TPersistent); override;
  public
    // Returns S_OK if FileName is a compound file
    class function IsStructured(const FileName: string): HRESULT;
    // Converts FileName to a structured file and puts the existing content of the file
    // into a root file stream called 'CONTENTS'
    // Returns S_OK or STG_S_CONVERTED if the file could be converted or if it was already a structured file
    class function Convert(const FileName: string): HRESULT;
    // Copies a sub storage or stream to another storage
    // Before calling this method, the element to be copied must be closed,
    // and the destination storage must be open. Also, the destination object
    // and element cannot be the same storage object/element name as the source
    // of the copy. That is, you cannot copy an element to itself.
    function CopyTo(const OldName, NewName: string; Dest: TJclStructStorageFolder): Boolean;
    // Moves a sub storage or stream to another storage
    // Before calling this method, the element to be moved must be closed,
    // and the destination storage must be open. Also, the destination object
    // and element cannot be the same storage object/element name as the source
    // of the move. That is, you cannot move an element to itself.
    function MoveTo(const OldName, NewName: string; Dest: TJclStructStorageFolder): Boolean;
    // Commits any changes when smTransacted is true
    // When smTransacted  is false, changes are comitted immediately and thus cannot be comitted
    function Commit: Boolean;
    // Reverts any changes when smTransacted is true
    // When smTransacted  is false, changes are comitted immediately and thus cannot be reverted
    function Revert: Boolean;
    // Create a new or open an existing structured file (or subfolder) depending on AccessMode.
    // NOTE that the file will not actually be opened or created until you call
    // one of the methods in this class (except for Destroy). To force a direct open of the file, set OpenDirect to true
    constructor Create(const FileName: string; AccessMode: TJclStructStorageAccessModes;
      OpenDirect: Boolean = False); virtual;
    // Destroys the class instance and releases the compound file (or subfolder)
    destructor Destroy; override;
    // Returns statistics for this storage. The returned structure contains
    // various information about the storage. NOTE that some items may not always be valid or set
    // (f ex the GUID or the date values)
    //
    // NOTE: if you call this function with IncludeName = true, you *must*
    // free the returned Stat by calling FreeStats;
    function GetStats(out Stat: TStatStg; IncludeName: Boolean): Boolean;
    procedure FreeStats(var Stat: TStatStg);
    // Gets the names of all subitems (files or folders depending on the Folders flag) of this storage
    // and puts it in Strings. Strings is cleared before adding the items
    function GetSubItems(Strings: TStrings; Folders: Boolean): Boolean;
    // Adds a new file or folder to this folder. If the file/folder already exists, it is overwritten.
    // NB: Name must be < 31 characters
    function Add(const Name: string; IsFolder: Boolean): Boolean;
    // Deletes a file /folder
    function Delete(const Name: string): Boolean;
    // Renames a file/folder. The element must be closed before calling this method
    // NB: NewName must be < 31 characters
    function Rename(const OldName, NewName: string): Boolean;
    // Returns an existing folder by name. The folder is opened using the same AccessMode
    // as passed into the constructor, except for any smCreate and with sharing set to [smShareDenyRead,smShareDenyWrite]
    // because the MS implementation doesn't support opening the same storage more than once
    // from the same parent storage
    function GetFolder(const Name: string; out Storage: TJclStructStorageFolder): Boolean;
    // Returns an existing file stream by name. The stream is opened using the same AccessMode
    // as passed into the constructor, except for any smCreate and with sharing set to [smShareDenyRead,smShareDenyWrite]
    // because the MS implementation doesn't support opening the same stream more than once
    // from the same parent storage
    function GetFileStream(const Name: string; out Stream: TStream): Boolean;
    // Set the various time fields -a(ccess)time, c(reation)time, m(odified)time - for
    // a stream or storage as specified by Name. Values in Stat that are set to 0 are left
    // unmodified.
    // To set these values for the root storage, pass an empty string in Name.
    // To get the current values, call GetStats on the specific storage or stream
    function SetElementTimes(const Name: string; Stat: TStatStg): Boolean;

    // The name of the storage, either a (sub)folder name or the fully qualified name of the disk file (for the root object)
    property Name: string read GetName;
    // pointer to the IStorage
    property Intf: IStorage read FStorage;
    // last error for this object (can be S_OK)
    property LastError: HRESULT read FLastError;
  end;

  // NOTE: you should not create instances of this class: an instance is created by
  // TJclStructStorageFolder when you call GetFileStream
  TJclStructStorageStream = class(TStream)
  private
    function GetName: string;
  protected
    FStream: IStream;
    FName: string;
    FLastError: HRESULT;
    procedure Check;
    function CheckResult(HR: HRESULT): Boolean;
    procedure SetSize(NewSize: Longint); override;
  public
    destructor Destroy; override;

    // Returns the TStatStg for this stream. This structure contains
    // the name, size and various date/time values for the stream in addition to
    // several other values
    //
    // NOTE: if you call this function with IncludeName = true, you *must*
    // free the returned Stat by calling FreStats or using this type of code:
    //    CoGetMalloc(1,AMalloc);
    //    AMalloc.Free(Stat.pwcsName)
    // where AMalloc is declared as an IMalloc type
    // see also example in TJclStructStorageFolder.GetSubItems above
    function GetStats(out Stat: TStatStg; IncludeName: Boolean): Boolean;
    procedure FreeStats(var Stat: TStatStg);
    // Create a new stream that points to this stream.
    // Returns nil on failure
    // NB! Caller is responsible for freeing this object!
    // To create a copy of a stream, call CopyTo instead
    function Clone: TJclStructStorageStream;
    function CopyTo(Stream: TJclStructStorageStream; Size: Int64): Boolean;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    // name of the stream
    property Name: string read GetName;
    // pointer to the IStream interface
    property Intf: IStream read FStream;
    // the last error for this object (can be S_OK)
    property LastError: HRESULT read FLastError;
  end;

procedure CoMallocFree(P: Pointer);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclStructStorage.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  ComObj,
  JclResources;

var
  FMalloc: IMalloc = nil;

type
  PStgOptions = ^TStgOptions;
  tagSTGOPTIONS = record
    usVersion: Byte;
    reserved: Byte;
    ulSectorSize: DWORD;
    pwcsTemplateFile: POleStr;
  end;
  {$EXTERNALSYM tagSTGOPTIONS}
  TStgOptions = tagSTGOPTIONS;

  TStgCreateStorageExFunc = function(pwcsName: POleStr; grfMode: Longint; StgFmt: Longint; grfAttrs: DWORD; pStgOptions:
    PStgOptions;
    reserved2: Pointer; riid: TIID; out ppObjectOpen: IUnknown): HRESULT; stdcall;
  TStgOpenStorageExFunc = function(pwcsName: POleStr; grfMode: Longint; StgFmt: Longint; grfAttrs: DWORD; pStgOptions:
    PStgOptions;
    reserved2: Pointer; riid: TIID; out ppObjectOpen: IUnknown): HRESULT; stdcall;

var
  // replacements for StgCreateDocFile and StgOpenStorage on Win2k and XP - not currently used
  StgCreateStorageEx: TStgCreateStorageExFunc = nil;
  {$EXTERNALSYM StgCreateStorageEx}
  StgOpenStorageEx: TStgOpenStorageExFunc = nil;
  {$EXTERNALSYM StgOpenStorageEx}

procedure CoMallocFree(P: Pointer);
begin
  if FMalloc = nil then
    OleCheck(CoGetMalloc(1, FMalloc));
  FMalloc.Free(P);
end;

function AccessToMode(AccessMode: TJclStructStorageAccessModes): UINT;
begin
  { NOTE:
    MS has some very specific restrictions when combining the different
    Mode flags and certain combinations will lead to errors. I have mostly resisted the
    temptation to try to consolidate the restrictions here, so you might have to
    read up on the valid combinations on MSDN. Generally, the following rules apply
    when opening a file in non-transacted mode:

     * To create a new file, you must use [smCreate,smRead,smWrite,smShareDenyRead,smShareDenyWrite]
       = STGM_CREATE or STGM_READWRITE or STGM_SHARE_EXCLUSIVE
     * When opening as read-only, you must use [smRead,smShareDenyWrite]
       = STGM_READ or STGM_SHARE_DENY_WRITE
     * when opening for reading and writing, you must use [smRead,smWrite,smShareDenyRead,smShareDenyWrite]
       = STGM_READWRITE or STGM_SHARE_EXCLUSIVE

    These restrictions pretty much exist for transacted files as well with the difference that most
    errors are not reported until a call is made to Commit...
  }

  // creation:
  if smCreate in AccessMode then
  begin
    // only one valid combination, so set up and jump out:
    Result := STGM_CREATE or STGM_READWRITE or STGM_SHARE_EXCLUSIVE;
    Exit;
  end;

  // transactions:
  if smTransacted in AccessMode then
    Result := STGM_TRANSACTED
  else
    Result := STGM_DIRECT;

  // access:
  if AccessMode * [smOpenRead, smOpenWrite] = [smOpenRead, smOpenWrite] then
    Result := Result or STGM_READWRITE // this is *not* the same as (STGM_READ or STGM_WRITE)
  else
  if smOpenWrite in AccessMode then
    Result := Result or STGM_WRITE
  else
  if smOpenRead in AccessMode then // not strictly necessary, since STGM_READ = 0, but makes it more self-documenting
    Result := Result or STGM_READ;

  // sharing:
  if AccessMode * [smShareDenyRead, smShareDenyWrite] = [smShareDenyRead, smShareDenyWrite] then
    Result := Result or STGM_SHARE_EXCLUSIVE // *not* the same as (STGM_SHARE_READ or STGM_SHARE_WRITE)!
  else
  if smShareDenyRead in AccessMode then
    Result := Result or STGM_SHARE_DENY_READ
  else
  if smShareDenyWrite in AccessMode then
    Result := Result or STGM_SHARE_DENY_WRITE
  else
    Result := Result or STGM_SHARE_DENY_NONE;
    // not strictly necessary, since STGM_SHARE_DENY_NONE = 0, but makes it more self-documenting
end;

// simpler and less convoluted than using StringToWideChar

function StrToWChar(const S: string): PWideChar;
begin
  if S = '' then
    Result := nil
  else
  begin
    Result := AllocMem((Length(S)+1) * SizeOf(WideChar));
    MultiByteToWideChar(CP_ACP, 0, PChar(S), Length(S), Result, Length(S));
    // (outchy) length(S) is the number of characters, not the size in bytes
    // (rom) fixed output buffer size (see Win32 help)
    //MultiByteToWideChar(CP_ACP, 0, PChar(S), Length(S), Result, Length(S) div 2);
  end;
end;

procedure FreeWChar(W: PWideChar);
begin
  if Assigned(W) then
    FreeMem(W);
end;

//=== { TJclStructStorageFolder } ============================================

constructor TJclStructStorageFolder.Create(const FileName: string; AccessMode: TJclStructStorageAccessModes;
  OpenDirect: Boolean = False);
begin
  inherited Create;
  FFileName := FileName;
  FAccessMode := AccessMode;
  FConvertedMode := AccessToMode(FAccessMode);
  if FFileName = '' then
    FConvertedMode := FConvertedMode or STGM_DELETEONRELEASE;
  if OpenDirect then
    Check;
end;

destructor TJclStructStorageFolder.Destroy;
begin
  FStorage := nil;
  inherited Destroy;
end;

function TJclStructStorageFolder.Add(const Name: string;
  IsFolder: Boolean): Boolean;
var
  AName: PWideChar;
  Strg: IStorage;
  Stm: IStream;
begin
  Check;
  AName := StrToWChar(Name);
  try
    // always overwrite existing (fails if storage/stream exists and is open)
    if IsFolder then
      Result := CheckResult(FStorage.CreateStorage(AName, STGM_CREATE or STGM_SHARE_EXCLUSIVE, 0, 0, Strg))
    else
      Result := CheckResult(FStorage.CreateStream(AName, STGM_CREATE or STGM_SHARE_EXCLUSIVE, 0, 0, Stm));
  finally
    FreeWChar(AName);
  end;
end;

function TJclStructStorageFolder.Delete(const Name: string): Boolean;
var
  AName: PWideChar;
begin
  Check;
  AName := StrToWChar(Name);
  try
    Result := CheckResult(FStorage.DestroyElement(AName));
  finally
    FreeWChar(AName);
  end;
end;

procedure TJclStructStorageFolder.Check;
var
  AName: PWideChar;
  HR: HRESULT;
begin
  if FStorage = nil then
  begin
    AName := StrToWChar(FFileName);
    try
      if FConvertedMode and STGM_CREATE = STGM_CREATE then
        HR := StgCreateDocfile(AName, FConvertedMode, 0, FStorage)
      else
        HR := StgOpenStorage(AName, nil, FConvertedMode, nil, 0, FStorage);
    finally
      FreeWChar(AName);
    end;
    if not Succeeded(HR) then
      raise EJclStructStorageError.Create(SysErrorMessage(HR));
  end;
end;

function TJclStructStorageFolder.CheckResult(HR: HRESULT): Boolean;
begin
  Result := Succeeded(HR);
  FLastError := HR;
end;

function TJclStructStorageFolder.GetFileStream(const Name: string; out Stream: TStream): Boolean;
var
  AName: PWideChar;
  Stm: IStream;
begin
  Check;
  AName := StrToWChar(Name);
  try
    // Streams don't support transactions, so always create in direct mode
    // Streams only support STGM_SHARE_EXCLUSIVE so add this explicitly
    if Succeeded(FStorage.OpenStream(AName, nil,
      AccessToMode(FAccessMode - [smCreate] + [smShareDenyRead, smShareDenyWrite]), 0, Stm)) then
    begin
      Stream := TJclStructStorageStream.Create;
      TJclStructStorageStream(Stream).FStream := Stm;
      TJclStructStorageStream(Stream).FName := Name;
      Result := True;
    end
    else
    begin
      Result := False;
      Stream := nil;
    end;
  finally
    FreeWChar(AName);
  end;
end;

function TJclStructStorageFolder.GetFolder(const Name: string; out Storage: TJclStructStorageFolder): Boolean;
var
  AName: PWideChar;
  AMode: UINT;
  Strg: IStorage;
begin
  Check;
  AName := StrToWChar(Name);
  try
    // Sub storages only supports STGM_SHARE_EXCLUSIVE, so add explicitly
    AMode := AccessToMode(FAccessMode - [smCreate] + [smShareDenyRead, smShareDenyWrite]);
    if Succeeded(FStorage.OpenStorage(AName, nil,
      AMode, nil, 0, Strg)) then
    begin
      // The parameters here has no real meaning since we set up the private fields directly
      Storage := TJclStructStorageFolder.Create(Name, FAccessMode);
      TJclStructStorageFolder(Storage).FConvertedMode := AMode;
      TJclStructStorageFolder(Storage).FStorage := Strg;
      TJclStructStorageFolder(Storage).FFileName := Name;
      Result := True;
    end
    else
    begin
      Storage := nil;
      Result := False;
    end;
  finally
    FreeWChar(AName);
  end;
end;

function TJclStructStorageFolder.GetSubItems(Strings: TStrings;
  Folders: Boolean): Boolean;
var
  Enum: IEnumSTATSTG;
  Stat: TStatStg;
  NumFetch: Longint;
begin
  Check;
  Strings.BeginUpdate;
  try
    Strings.Clear;
    Result := CheckResult(FStorage.EnumElements(0, nil, 0, Enum));
    if not Result then
      Exit;
    while Succeeded(Enum.Next(1, Stat, @NumFetch)) and (NumFetch = 1) do
    try
      if Folders and (Stat.dwType = STGTY_STORAGE) then
        Strings.Add(WideCharToString(Stat.pwcsName))
      else
      if not Folders and (Stat.dwType = STGTY_STREAM) then
        Strings.Add(WideCharToString(Stat.pwcsName));
    finally
      CoMallocFree(Stat.pwcsName);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

function TJclStructStorageFolder.Rename(const OldName, NewName: string): Boolean;
var
  PWO, PWN: PWideChar;
begin
  Check;
  PWO := StrToWChar(OldName);
  PWN := StrToWChar(NewName);
  try
    // this will fail if the subelement is open
    Result := CheckResult(FStorage.RenameElement(PWO, PWN));
  finally
    FreeWChar(PWO);
    FreeWChar(PWN);
  end;
end;

class function TJclStructStorageFolder.IsStructured(const FileName: string): HRESULT;
var
  AName: PWideChar;
begin
  AName := StrToWChar(FileName);
  try
    Result := StgIsStorageFile(AName);
  finally
    FreeWChar(AName);
  end;
end;

class function TJclStructStorageFolder.Convert(const FileName: string): HRESULT;
var
  Strg: IStorage;
  AName: PWideChar;
begin
  Result := IsStructured(FileName);
  if Succeeded(Result) then
  begin
    AName := StrToWChar(FileName);
    try
      Result := StgCreateDocFile(AName, STGM_READWRITE or STGM_SHARE_EXCLUSIVE or STGM_CONVERT, 0, Strg);
//      Result := (HR = S_OK) or (HR = STG_S_CONVERTED);
    finally
      FreeWChar(AName);
    end;
  end;
end;

function TJclStructStorageFolder.GetStats(out Stat: TStatStg; IncludeName: Boolean): Boolean;
const
  Flags: array [Boolean] of Longint =
    (STATFLAG_NONAME, STATFLAG_DEFAULT);
begin
  Check;
  Result := CheckResult(FStorage.Stat(Stat, Flags[IncludeName]));
end;

function TJclStructStorageFolder.SetElementTimes(const Name: string; Stat: TStatStg): Boolean;
var
  AName: PWideChar;
begin
  Check;
  AName := StrToWChar(Name);
  try
    with Stat do
      Result := CheckResult(FStorage.SetElementTimes(AName, ctime, atime, mtime));
  finally
    FreeWChar(AName);
  end;
end;

function TJclStructStorageFolder.Commit: Boolean;
begin
  Check;
  Result := CheckResult(FStorage.Commit(STGC_DEFAULT)) or
    CheckResult(FStorage.Commit(STGC_OVERWRITE));
end;

function TJclStructStorageFolder.Revert: Boolean;
begin
  Check;
  Result := CheckResult(FStorage.Revert);
end;

function TJclStructStorageFolder.CopyTo(const OldName, NewName: string; Dest: TJclStructStorageFolder): Boolean;
var
  PWO, PWN: PWideChar;
begin
  Result := False;
  if Dest = nil then
    Exit;
  Check;
  Dest.Check;
  PWO := StrToWChar(OldName);
  PWN := StrToWChar(NewName);
  try
    Result := CheckResult(FStorage.MoveElementTo(PWO, Dest.FStorage, PWN, STGMOVE_COPY));
  finally
    FreeWChar(PWO);
    FreeWChar(PWN);
  end;
end;

procedure TJclStructStorageFolder.AssignTo(Dest: TPersistent);
begin
  if Dest is TJclStructStorageFolder then
  begin
    Check;
    TJclStructStorageFolder(Dest).Check;
    CheckResult(FStorage.CopyTo(0, nil, nil, TJclStructStorageFolder(Dest).FStorage));
  end
  else
    inherited AssignTo(Dest);
end;

function TJclStructStorageFolder.MoveTo(const OldName, NewName: string;
  Dest: TJclStructStorageFolder): Boolean;
var
  PWO, PWN: PWideChar;
begin
  Result := False;
  if Dest = nil then
    Exit;
  Check;
  Dest.Check;
  PWO := StrToWChar(OldName);
  PWN := StrToWChar(NewName);
  try
    Result := CheckResult(FStorage.MoveElementTo(PWO, Dest.FStorage, PWN, STGMOVE_MOVE));
  finally
    FreeWChar(PWO);
    FreeWChar(PWN);
  end;
end;

function TJclStructStorageFolder.GetName: string;
var
  Stat: StatStg;
begin
  if (FStorage <> nil) and CheckResult(FStorage.Stat(Stat, STATFLAG_DEFAULT)) then
  begin
    Result := WideCharToString(Stat.pwcsName);
    CoMallocFree(Stat.pwcsName);
  end
  else
    Result := FFileName;
end;

procedure TJclStructStorageFolder.FreeStats(var Stat: TStatStg);
begin
  if Stat.pwcsName <> nil then
    CoMallocFree(Stat.pwcsName);
end;

//=== { TJclStructStorageStream } ============================================

destructor TJclStructStorageStream.Destroy;
begin
  FStream := nil;
  inherited Destroy;
end;

procedure TJclStructStorageStream.Check;
begin
  if FStream = nil then
    raise EJclStructStorageError.CreateRes(@RsIStreamNil);
end;

function TJclStructStorageStream.CheckResult(HR: HRESULT): Boolean;
begin
  Result := Succeeded(HR);
  FlastError := HR;
end;

function TJclStructStorageStream.Clone: TJclStructStorageStream;
var
  Stm: IStream;
begin
  if Succeeded(FStream.Clone(Stm)) then
  begin
    Result := TJclStructStorageStream.Create;
    Result.FStream := Stm;
  end
  else
    Result := nil;
end;

function TJclStructStorageStream.CopyTo(Stream: TJclStructStorageStream;
  Size: Int64): Boolean;
var
  DidRead, DidWrite: Int64;
begin
  DidRead := 0;
  DidWrite := 0;
  Result := Succeeded(FStream.CopyTo(Stream.FStream, Size, DidRead, DidWrite));
end;

procedure TJclStructStorageStream.FreeStats(var Stat: TStatStg);
begin
  if Stat.pwcsName <> nil then
    CoMallocFree(Stat.pwcsName);
end;

function TJclStructStorageStream.GetName: string;
var
  Stat: StatStg;
begin
  if (FStream <> nil) and CheckResult(FStream.Stat(Stat, STATFLAG_DEFAULT)) then
  begin
    Result := WideCharToString(Stat.pwcsName);
    CoMallocFree(Stat.pwcsName);
  end
  else
    Result := Fname;
end;

function TJclStructStorageStream.GetStats(out Stat: TStatStg; IncludeName: Boolean): Boolean;
const
  Flags: array [Boolean] of Longint =
    (STATFLAG_NONAME, STATFLAG_DEFAULT);
begin
  Check;
  Result := CheckResult(FStream.Stat(Stat, Flags[IncludeName]));
end;

function TJclStructStorageStream.Read(var Buffer; Count: Longint): Longint;
begin
  Check;
  if not Succeeded(FStream.Read(@Buffer, Count, @Result)) then
    Result := 0;
end;

function TJclStructStorageStream.Seek(Offset: Integer; Origin: Word): Longint;
var
  N: Int64;
begin
  Check;
  if not Succeeded(FStream.Seek(Offset, Ord(Origin), N)) then
    Result := -1
  else
    Result := N;
end;

procedure TJclStructStorageStream.SetSize(NewSize: Longint);
begin
  Check;
  FStream.SetSize(NewSize);
end;

function TJclStructStorageStream.Write(const Buffer; Count: Longint): Longint;
begin
  Check;
  if not Succeeded(FStream.Write(@Buffer, Count, @Result)) then
    Result := 0;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

