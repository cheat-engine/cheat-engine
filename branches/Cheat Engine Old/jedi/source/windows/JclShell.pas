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
{ The Original Code is JclShell.pas.                                                               }
{                                                                                                  }
{ The Initial Developers of the Original Code are Marcel van Brakel and Petr Vones.                }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Rik Barker (rikbarker)                                                                         }
{   Marcel van Brakel                                                                              }
{   Jean-Fabien Connault (cycocrew)                                                                }
{   Aleksej Kudinov                                                                                }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   kogerbnz                                                                                       }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains routines and classes which makes working with the Windows Shell a bit easier. }
{ Included are routines for working with PIDL's, special folder's, file and folder manipulation    }
{ through shell interfaces, shortcut's and program execution.                                      }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-02-03 19:05:06 +0100 (dim., 03 févr. 2008)                        $ }
{ Revision:      $Rev:: 2338                                                                     $ }
{ Author:        $Author:: marcovtje                                                             $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclShell;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils,
  ShlObj,
  JclWin32, JclSysUtils;

// Files and Folders
type
  TSHDeleteOption  = (doSilent, doAllowUndo, doFilesOnly);
  TSHDeleteOptions = set of TSHDeleteOption;
  TSHRenameOption  = (roSilent, roRenameOnCollision);
  TSHRenameOptions = set of TSHRenameOption;
  TSHCopyOption    = (coSilent, coAllowUndo, coFilesOnly, coNoConfirmation);
  TSHCopyOptions   = set of TSHCopyOption;
  TSHMoveOption    = (moSilent, moAllowUndo, moFilesOnly, moNoConfirmation);
  TSHMoveOptions   = set of TSHMoveOption;

  TUnicodePath     = array [0..MAX_PATH-1] of WideChar;
  TAnsiPath        = array [0..MAX_PATH-1] of char;

function SHDeleteFiles(Parent: THandle; const Files: string; Options: TSHDeleteOptions): Boolean;
function SHDeleteFolder(Parent: THandle; const Folder: string; Options: TSHDeleteOptions): Boolean;
function SHRenameFile(const Src, Dest: string; Options: TSHRenameOptions): Boolean;
function SHCopy(Parent: THandle; const Src, Dest: string; Options: TSHCopyOptions): Boolean;
function SHMove(Parent: THandle; const Src, Dest: string; Options: TSHMoveOptions): Boolean;

type
  TEnumFolderFlag = (efFolders, efNonFolders, efIncludeHidden);
  TEnumFolderFlags = set of TEnumFolderFlag;

  TEnumFolderRec = record
    DisplayName: string;
    Attributes: DWORD;
    IconLarge: HICON;
    IconSmall: HICON;
    Item: PItemIdList;
    EnumIdList: IEnumIdList;
    Folder: IShellFolder;
  end;

function SHEnumFolderFirst(const Folder: string; Flags: TEnumFolderFlags;
  var F: TEnumFolderRec): Boolean;
function SHEnumSpecialFolderFirst(SpecialFolder: DWORD; Flags: TEnumFolderFlags;
  var F: TEnumFolderRec): Boolean;
procedure SHEnumFolderClose(var F: TEnumFolderRec);
function SHEnumFolderNext(var F: TEnumFolderRec): Boolean;

function GetSpecialFolderLocation(const FolderID: Integer): string;

function DisplayPropDialog(const Handle: THandle; const FileName: string): Boolean; overload;
function DisplayPropDialog(const Handle: THandle; Item: PItemIdList): Boolean; overload;

function DisplayContextMenuPidl(const Handle: THandle; const Folder: IShellFolder;
  Item: PItemIdList; Pos: TPoint): Boolean;
function DisplayContextMenu(const Handle: THandle; const FileName: string;
  Pos: TPoint): Boolean;

function OpenFolder(const Path: string; Parent: THandle = 0; Explore: Boolean = False): Boolean;
function OpenSpecialFolder(FolderID: Integer; Parent: THandle = 0; Explore: Boolean = False): Boolean;

// Memory Management
function SHReallocMem(var P: Pointer; Count: Integer): Boolean;
function SHAllocMem(out P: Pointer; Count: Integer): Boolean;
function SHGetMem(var P: Pointer; Count: Integer): Boolean;
function SHFreeMem(var P: Pointer): Boolean;

// Paths and PIDLs
function DriveToPidlBind(const DriveName: string; out Folder: IShellFolder): PItemIdList;
function PathToPidl(const Path: string; Folder: IShellFolder): PItemIdList;
function PathToPidlBind(const FileName: string; out Folder: IShellFolder): PItemIdList;
function PidlBindToParent(IdList: PItemIdList; out Folder: IShellFolder; out Last: PItemIdList): Boolean;
function PidlCompare(Pidl1, Pidl2: PItemIdList): Boolean;
function PidlCopy(Source: PItemIdList; out Dest: PItemIdList): Boolean;
function PidlFree(var IdList: PItemIdList): Boolean;
function PidlGetDepth(Pidl: PItemIdList): Integer;
function PidlGetLength(Pidl: PItemIdList): Integer;
function PidlGetNext(Pidl: PItemIdList): PItemIdList;
function PidlToPath(IdList: PItemIdList): string;

function StrRetFreeMem(StrRet: TStrRet): Boolean;
function StrRetToString(IdList: PItemIdList; StrRet: TStrRet; Free: Boolean): string;

// Shortcuts / Shell link
type
  PShellLink = ^TShellLink;
  TShellLink = record
    Arguments: string;
    ShowCmd: Integer;
    WorkingDirectory: string;
    IdList: PItemIDList;
    Target: string;
    Description: string;
    IconLocation: string;
    IconIndex: Integer;
    HotKey: Word;
  end;

procedure ShellLinkFree(var Link: TShellLink);
function ShellLinkResolve(const FileName: string; var Link: TShellLink): HRESULT; overload;
function ShellLinkResolve(const FileName: string; var Link: TShellLink;
  const ResolveFlags: Cardinal): HRESULT; overload;
function ShellLinkCreate(const Link: TShellLink; const FileName: string): HRESULT;
function ShellLinkCreateSystem(const Link: TShellLink; const Folder: Integer; const FileName: string): HRESULT;
function ShellLinkIcon(const Link: TShellLink): HICON; overload;
function ShellLinkIcon(const FileName: string): HICON; overload;

// Miscellaneous
function SHDllGetVersion(const FileName: string; var Version: TDllVersionInfo): Boolean;

function GetSystemIcon(IconIndex: Integer; Flags: Cardinal): HICON;
function OverlayIcon(var Icon: HICON; Overlay: HICON; Large: Boolean): Boolean;
function OverlayIconShortCut(var Large, Small: HICON): Boolean;
function OverlayIconShared(var Large, Small: HICON): Boolean;
function SHGetItemInfoTip(const Folder: IShellFolder; Item: PItemIdList): string;

function ShellExecEx(const FileName: string; const Parameters: string = ''; const Verb: string = '';
  CmdShow: Integer = SW_SHOWNORMAL): Boolean;
function ShellExec(Wnd: Integer; const Operation, FileName, Parameters, Directory: string; ShowCommand: Integer): Boolean;
function ShellExecAndWait(const FileName: string; const Parameters: string = ''; const Verb: string = '';
  CmdShow: Integer = SW_SHOWNORMAL; const Directory: string = ''): Boolean;

function ShellOpenAs(const FileName: string): Boolean;
function ShellRasDial(const EntryName: string): Boolean;
function ShellRunControlPanel(const NameOrFileName: string; AppletNumber: Integer = 0): Boolean;

function GetFileNameIcon(const FileName: string; Flags: Cardinal = 0): HICON;

type
  TJclFileExeType = (etError, etMsDos, etWin16, etWin32Gui, etWin32Con);

function GetFileExeType(const FileName: TFileName): TJclFileExeType;

function ShellFindExecutable(const FileName, DefaultDir: string): string;

//MSI functions and types used in ShellLinkResolve - copied from JwaMsi.pas
type
  INSTALLSTATE = Longint;
const
  MSILIB = 'msi.dll';
var
  RtdlMsiLibHandle: TModuleHandle = INVALID_MODULEHANDLE_VALUE;
  RtdlMsiGetShortcutTarget: function(szShortcutPath: LPCSTR; szProductCode: LPSTR;
    szFeatureId: LPSTR; szComponentCode: LPSTR): UINT stdcall = nil;

  RtdlMsiGetComponentPath: function(szProduct: LPCSTR; szComponent: LPCSTR;
    lpPathBuf: LPSTR; pcchBuf: LPDWORD): INSTALLSTATE stdcall = nil;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclShell.pas $';
    Revision: '$Revision: 2338 $';
    Date: '$Date: 2008-02-03 19:05:06 +0100 (dim., 03 févr. 2008) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  ActiveX,
  CommCtrl,
  Messages, ShellApi,
  JclFileUtils, JclStrings, JclSysInfo;

const
  cVerbProperties = 'properties';
  cVerbOpen = 'open';
  cVerbExplore = 'explore';
  
//=== Files and Folders ======================================================

// Helper function and constant to map a TSHDeleteOptions set to a Cardinal

const
  FOF_COMPLETELYSILENT = FOF_SILENT or FOF_NOCONFIRMATION or FOF_NOERRORUI or FOF_NOCONFIRMMKDIR;

function DeleteOptionsToCardinal(Options: TSHDeleteOptions): Cardinal;
begin
  Result := 0;
  if doSilent in Options then
    Result := Result or FOF_COMPLETELYSILENT;
  if doAllowUndo in Options then
    Result := Result or FOF_ALLOWUNDO;
  if doFilesOnly in Options then
    Result := Result or FOF_FILESONLY;
end;

function SHDeleteFiles(Parent: THandle; const Files: string;
  Options: TSHDeleteOptions): Boolean;
var
  FileOp: TSHFileOpStruct;
  Source: string;
begin
  FillChar(FileOp, SizeOf(FileOp), #0);
  with FileOp do
  begin
    Wnd := Parent;
    wFunc := FO_DELETE;
    Source := Files + #0#0;
    pFrom := PChar(Source);
    fFlags := DeleteOptionsToCardinal(Options);
  end;
  Result := SHFileOperation(FileOp) = 0;
end;

function SHDeleteFolder(Parent: THandle; const Folder: string;
  Options: TSHDeleteOptions): Boolean;
begin
  Exclude(Options, doFilesOnly);
  Result := SHDeleteFiles(Parent, PathAddSeparator(Folder) + '*.*', Options);
  if Result then
    SHDeleteFiles(Parent, Folder, Options);
end;

// Helper function to map a TSHRenameOptions set to a cardinal

function RenameOptionsToCardinal(Options: TSHRenameOptions): Cardinal;
begin
  Result := 0;
  if roRenameOnCollision in Options then
    Result := Result or FOF_RENAMEONCOLLISION;
  if roSilent in Options then
    Result := Result or FOF_COMPLETELYSILENT;
end;

function SHRenameFile(const Src, Dest: string; Options: TSHRenameOptions): Boolean;
var
  FileOp: TSHFileOpStruct;
  Source, Destination: string;
begin
  FillChar(FileOp, SizeOf(FileOp), #0);
  with FileOp do
  begin
    Wnd := GetDesktopWindow;
    wFunc := FO_RENAME;
    Source := Src + #0#0;
    Destination := Dest + #0#0;
    pFrom := PChar(Source);
    pTo := PChar(Destination);
    fFlags := RenameOptionsToCardinal(Options);
  end;
  Result := SHFileOperation(FileOp) = 0;
end;

function CopyOptionsToCardinal(Options: TSHCopyOptions): Cardinal;
begin
  Result := 0;
  if coSilent in Options then
    Result := Result or FOF_SILENT;
  if coAllowUndo in Options then
    Result := Result or FOF_ALLOWUNDO;
  if coFilesOnly in Options then
    Result := Result or FOF_FILESONLY;
  if coNoConfirmation in Options then
    Result := Result or FOF_NOCONFIRMATION or FOF_NOCONFIRMMKDIR;
end;

function SHCopy(Parent: THandle; const Src, Dest: string; Options: TSHCopyOptions): Boolean;
var
  FileOp: TSHFileOpStruct;
  Source, Destination: string;
begin
  FillChar(FileOp,SizeOf(FileOp),0);
  FileOp.Wnd := Parent;
  FileOp.wFunc := FO_COPY;
  Source := Src + #0#0;
  Destination := Dest + #0#0;
  FileOp.pFrom := PChar(Source);
  FileOp.pTo := PChar(Destination);
  FileOp.fFlags := CopyOptionsToCardinal(Options);
  Result := SHFileOperation(FileOp) = 0;
end;

function MoveOptionsToCardinal(Options: TSHMoveOptions): Cardinal;
begin
  Result := 0;
  if moSilent in Options then
    Result := Result or FOF_SILENT;
  if moAllowUndo in Options then
    Result := Result or FOF_ALLOWUNDO;
  if moFilesOnly in Options then
    Result := Result or FOF_FILESONLY;
  if moNoConfirmation in Options then
    Result := Result or FOF_NOCONFIRMATION;
end;

function SHMove(Parent: THandle; const Src, Dest: string; Options: TSHMoveOptions): Boolean;
var
  FileOp: TSHFileOpStruct;
  Source, Destination: string;
begin
  FillChar(FileOp,SizeOf(FileOp),0);
  FileOp.Wnd := Parent;
  FileOp.wFunc := FO_MOVE;
  Source := Src + #0#0;
  Destination := Dest + #0#0;
  FileOp.pFrom := PChar(Source);
  FileOp.pTo := PChar(Destination);
  FileOp.fFlags := MoveOptionsToCardinal(Options);
  Result := SHFileOperation(FileOp) = 0;
end;

function EnumFolderFlagsToCardinal(Flags: TEnumFolderFlags): Cardinal;
begin
  Result := 0;
  if efFolders in Flags then
    Result := Result or ord(SHCONTF_FOLDERS);
  if efNonFolders in Flags then
    Result := Result or ord(SHCONTF_NONFOLDERS);
  if efIncludeHidden in Flags then
    Result := Result or ord(SHCONTF_INCLUDEHIDDEN);
end;

procedure ClearEnumFolderRec(var F: TEnumFolderRec; const Free, Release: Boolean);
begin
  if Release then
  begin
    F.EnumIdList := nil;
    F.Folder := nil;
  end;
  if Free then
  begin
    PidlFree(F.Item);
    DestroyIcon(F.IconLarge);
    DestroyIcon(F.IconSmall);
  end;
  F.Attributes := 0;
  F.Item := nil;
  F.IconLarge := 0;
  F.IconSmall := 0;
end;

procedure SHEnumFolderClose(var F: TEnumFolderRec);
begin
  ClearEnumFolderRec(F, True, True);
end;

function SHEnumFolderNext(var F: TEnumFolderRec): Boolean;
const
  Attr = Cardinal(SFGAO_CAPABILITYMASK or SFGAO_DISPLAYATTRMASK or SFGAO_CONTENTSMASK);
var
  DisplayNameRet: TStrRet;
  ItemsFetched: ULONG;
  ExtractIcon: IExtractIcon;
  IconFile: TUnicodePath;
  IconIndex: Integer;
  Flags: DWORD;
begin
  Result := False;
  ClearEnumFolderRec(F, True, False);
  if (F.EnumIdList = nil) or (F.Folder = nil) then
    Exit;
  if F.EnumIdList.Next(1, F.Item, ItemsFetched) = NO_ERROR then
  begin
    F.Folder.GetDisplayNameOf(F.Item, SHGDN_INFOLDER, DisplayNameRet);
    F.DisplayName := StrRetToString(F.Item, DisplayNameRet, True);
    F.Attributes := Attr;
    F.Folder.GetAttributesOf(1, F.Item, F.Attributes);
    F.Folder.GetUIObjectOf(0, 1, F.Item, IID_IExtractIconW, nil,
      Pointer(ExtractIcon));
    Flags := 0;
    F.IconLarge := 0;
    F.IconSmall := 0;
    
    if Assigned(ExtractIcon) then
    begin
      ExtractIcon.GetIconLocation(0, @IconFile, MAX_PATH, IconIndex, Flags);
      if (IconIndex < 0) and ((Flags and GIL_NOTFILENAME) = GIL_NOTFILENAME) then
        ExtractIconEx(@IconFile, IconIndex, F.IconLarge, F.IconSmall, 1)
      else
        ExtractIcon.Extract(@IconFile, IconIndex, F.IconLarge, F.IconSmall,
          MakeLong(32, 16));
    end;
          
    Result := True;
  end;
end;

function SHEnumSpecialFolderFirst(SpecialFolder: DWORD; Flags: TEnumFolderFlags;
  var F: TEnumFolderRec): Boolean;
var
  DesktopFolder: IShellFolder;
  FolderPidl: PItemIdList;
begin
  ClearEnumFolderRec(F, False, False);
  SHGetDesktopFolder(DesktopFolder);
  if SpecialFolder = CSIDL_DESKTOP then
    F.Folder := DesktopFolder
  else
  begin
    SHGetSpecialFolderLocation(0, SpecialFolder, FolderPidl);
    try
      DesktopFolder.BindToObject(FolderPidl, nil, IID_IShellFolder, Pointer(F.Folder));
    finally
      PidlFree(FolderPidl);
    end;
  end;
  F.Folder.EnumObjects(0, EnumFolderFlagsToCardinal(Flags), F.EnumIdList);
  Result := SHEnumFolderNext(F);
  if not Result then
    SHEnumFolderClose(F);
end;

function SHEnumFolderFirst(const Folder: string; Flags: TEnumFolderFlags;
  var F: TEnumFolderRec): Boolean;
var
  DesktopFolder: IShellFolder;
  FolderPidl: PItemIdList;
begin
  ClearEnumFolderRec(F, False, False);
  SHGetDesktopFolder(DesktopFolder);
  FolderPidl := PathToPidl(PathAddSeparator(Folder), DesktopFolder);
  try
    DesktopFolder.BindToObject(FolderPidl, nil, IID_IShellFolder, Pointer(F.Folder));
    F.Folder.EnumObjects(0, EnumFolderFlagsToCardinal(Flags), F.EnumIdList);
    Result := SHEnumFolderNext(F);
    if not Result then
      SHEnumFolderClose(F);
  finally
    PidlFree(FolderPidl);
  end;
end;

function GetSpecialFolderLocation(const FolderID: Integer): string;
var
  FolderPidl: PItemIdList;
begin
  if Succeeded(SHGetSpecialFolderLocation(0, FolderID, FolderPidl)) then
  begin
    Result := PidlToPath(FolderPidl);
    PidlFree(FolderPidl);
  end
  else
    Result := '';
end;

function DisplayPropDialog(const Handle: THandle; const FileName: string): Boolean;
var
  Info: TShellExecuteInfo;
begin
  FillChar(Info, SizeOf(Info), #0);
  with Info do
  begin
    cbSize := SizeOf(Info);
    lpFile := PChar(FileName);
    nShow := SW_SHOW;
    fMask := SEE_MASK_INVOKEIDLIST;
    Wnd := Handle;
    lpVerb := cVerbProperties;
  end;
  {$T+}  // need this because ShellExecuteEx is overloaded in FPC -A -W
  Result := ShellExecuteEx(@Info);
  {$T-}
end;

function DisplayPropDialog(const Handle: THandle; Item: PItemIdList): Boolean;
var
  Info: TShellExecuteInfo;
begin
  FillChar(Info, SizeOf(Info), #0);
  with Info do
  begin
    cbSize := SizeOf(Info);
    nShow := SW_SHOW;
    lpIDList := Item;
    fMask := SEE_MASK_INVOKEIDLIST or SEE_MASK_IDLIST;
    Wnd := Handle;
    lpVerb := cVerbProperties;
  end;
  {$T+}
  Result := ShellExecuteEx(@Info);
  {$T-}
end;

// Window procedure for the callback window created by DisplayContextMenu.
// It simply forwards messages to the folder. If you don't do this then the
// system created submenu's will be empty (except for 1 stub item!)
// note: storing the IContextMenu2 pointer in the window's user data was
// 'inspired' by (read: copied from) code by Brad Stowers.

function MenuCallback(Wnd: THandle; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  ContextMenu2: IContextMenu2;
begin
  case Msg of
    WM_CREATE:
      begin
        ContextMenu2 := IContextMenu2(PCreateStruct(lParam).lpCreateParams);
        SetWindowLong(Wnd, GWL_USERDATA, Longint(ContextMenu2));
        Result := DefWindowProc(Wnd, Msg, wParam, lParam);
      end;
    WM_INITMENUPOPUP:
      begin
        ContextMenu2 := IContextMenu2(GetWindowLong(Wnd, GWL_USERDATA));
        ContextMenu2.HandleMenuMsg(Msg, wParam, lParam);
        Result := 0;
      end;
    WM_DRAWITEM, WM_MEASUREITEM:
      begin
        ContextMenu2 := IContextMenu2(GetWindowLong(Wnd, GWL_USERDATA));
        ContextMenu2.HandleMenuMsg(Msg, wParam, lParam);
        Result := 1;
      end;
  else
    Result := DefWindowProc(Wnd, Msg, wParam, lParam);
  end;
end;

// Helper function for DisplayContextMenu, creates the callback window.

function CreateMenuCallbackWnd(const ContextMenu: IContextMenu2): THandle;
const
  IcmCallbackWnd = 'ICMCALLBACKWND';
var
  WndClass: TWndClass;
begin
  FillChar(WndClass, SizeOf(WndClass), #0);
  WndClass.lpszClassName := PChar(IcmCallbackWnd);
  WndClass.lpfnWndProc := @MenuCallback;
  WndClass.hInstance := HInstance;
  Windows.RegisterClass(WndClass);
  Result := CreateWindow(IcmCallbackWnd, IcmCallbackWnd, WS_POPUPWINDOW, 0,
    0, 0, 0, 0, 0, HInstance, Pointer(ContextMenu));
end;

function DisplayContextMenuPidl(const Handle: THandle; const Folder: IShellFolder;
  Item: PItemIdList; Pos: TPoint): Boolean;
var
  Cmd: Cardinal;
  ContextMenu: IContextMenu;
  ContextMenu2: IContextMenu2;
  Menu: HMENU;
  CommandInfo: TCMInvokeCommandInfo;
  CallbackWindow: THandle;
begin
  Result := False;
  if (Item = nil) or (Folder = nil) then
    Exit;
  Folder.GetUIObjectOf(Handle, 1, Item, IID_IContextMenu, nil,
    Pointer(ContextMenu));
  if ContextMenu <> nil then
  begin
    Menu := CreatePopupMenu;
    if Menu <> 0 then
    begin
      if Succeeded(ContextMenu.QueryContextMenu(Menu, 0, 1, $7FFF, CMF_EXPLORE)) then
      begin
        CallbackWindow := 0;
        if Succeeded(ContextMenu.QueryInterface(IContextMenu2, ContextMenu2)) then
        begin
          CallbackWindow := CreateMenuCallbackWnd(ContextMenu2);
        end;
        ClientToScreen(Handle, Pos);
        Cmd := Cardinal(TrackPopupMenu(Menu, TPM_LEFTALIGN or TPM_LEFTBUTTON or
          TPM_RIGHTBUTTON or TPM_RETURNCMD, Pos.X, Pos.Y, 0, CallbackWindow, nil));
        if Cmd <> 0 then
        begin
          FillChar(CommandInfo, SizeOf(CommandInfo), #0);
          CommandInfo.cbSize := SizeOf(TCMInvokeCommandInfo);
          CommandInfo.hwnd := Handle;
          CommandInfo.lpVerb := MakeIntResource(Cmd - 1);
          CommandInfo.nShow := SW_SHOWNORMAL;
          Result := Succeeded(ContextMenu.InvokeCommand(CommandInfo));
        end;
        if CallbackWindow <> 0 then
          DestroyWindow(CallbackWindow);
      end;
      DestroyMenu(Menu);
    end;
  end;
end;

function DisplayContextMenu(const Handle: THandle; const FileName: string;
  Pos: TPoint): Boolean;
var
  ItemIdList: PItemIdList;
  Folder: IShellFolder;
begin
  Result := False;
  ItemIdList := PathToPidlBind(FileName, Folder);
  if ItemIdList <> nil then
  begin
    Result := DisplayContextMenuPidl(Handle, Folder, ItemIdList, Pos);
    PidlFree(ItemIdList);
  end;
end;

function OpenFolder(const Path: string; Parent: THandle; Explore: Boolean): Boolean;
var
  Sei: TShellExecuteInfo;
begin
  Result := False;
  if IsDirectory(Path) then
  begin
    FillChar(Sei, SizeOf(Sei), #0);
    with Sei do
    begin
      cbSize := SizeOf(Sei);
      Wnd := Parent;
      if Explore then
        lpVerb := cVerbExplore
      else
        lpVerb := cVerbOpen;
      lpFile := PChar(Path);
      nShow := SW_SHOWNORMAL;
    end;
    {$T+}
    Result := ShellExecuteEx(@Sei);
    {$T-}
  end;
end;

function OpenSpecialFolder(FolderID: Integer; Parent: THandle; Explore: Boolean): Boolean;
var
  Malloc: IMalloc;
  Pidl: PItemIDList;
  Sei: TShellExecuteInfo;
begin
  Result := False;
  if Succeeded(SHGetMalloc(Malloc)) and
    Succeeded(SHGetSpecialFolderLocation(Parent, FolderID, Pidl)) then
  begin
    FillChar(Sei, SizeOf(Sei), #0);
    with Sei do
    begin
      cbSize := SizeOf(Sei);
      Wnd := Parent;
      fMask := SEE_MASK_INVOKEIDLIST;
      if Explore then
        lpVerb := cVerbExplore
      else
        lpVerb := cVerbOpen;
      lpIDList := Pidl;
      nShow := SW_SHOWNORMAL;
      if PidlToPath(Pidl) = '' then
      begin
        fMask := SEE_MASK_INVOKEIDLIST;
        lpIDList := Pidl;
      end
      else
        lpFile := PChar(PidlToPath(Pidl));
    end;
    {$T+}
    Result := ShellExecuteEx(@Sei);
    {$T-}
    Malloc.Free(Pidl);
  end;
end;

//=== Memory Management ======================================================

function SHAllocMem(out P: Pointer; Count: Integer): Boolean;
var
  Malloc: IMalloc;
begin
  Result := False;
  P := nil;
  if Succeeded(SHGetMalloc(Malloc)) then
  begin
    P := Malloc.Alloc(Count);
    if P <> nil then
    begin
      FillChar(P^, Count, #0);
      Result := True;
    end;
  end;
end;

function SHFreeMem(var P: Pointer): Boolean;
var
  Malloc: IMalloc;
begin
  Result := False;
  if P <> nil then
  begin
    if Succeeded(SHGetMalloc(Malloc)) and (Malloc.DidAlloc(P) > 0) then
    begin
      Malloc.Free(P);
      P := nil;
      Result := True;
    end;
  end;
end;

function SHGetMem(var P: Pointer; Count: Integer): Boolean;
var
  Malloc: IMalloc;
begin
  Result := False;
  if Succeeded(SHGetMalloc(Malloc)) then
  begin
    P := Malloc.Alloc(Count);
    if P <> nil then
      Result := True;
  end;
end;

function SHReallocMem(var P: Pointer; Count: Integer): Boolean;
var
  Malloc: IMalloc;
begin
  Result := False;
  if Succeeded(SHGetMalloc(Malloc)) then
  begin
    if (P <> nil) and (Malloc.DidAlloc(P) <= 0) then
      Exit;
    P := Malloc.ReAlloc(P, Count);
    Result := (P <> nil) or (Count = 0);
  end;
end;

//=== Paths and PIDLs ========================================================

function DriveToPidlBind(const DriveName: string; out Folder: IShellFolder): PItemIdList;
var
  Attr: ULONG;
  Eaten: ULONG;
  DesktopFolder: IShellFolder;
  Drives: PItemIdList;
  Path: TUnicodePath;
begin
  Result := nil;
  if Succeeded(SHGetDesktopFolder(DesktopFolder)) then
  begin
    if Succeeded(SHGetSpecialFolderLocation(0, CSIDL_DRIVES, Drives)) then
    begin
      if Succeeded(DesktopFolder.BindToObject(Drives, nil, IID_IShellFolder,
        Pointer(Folder))) then
      begin
        MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PChar(PathAddSeparator(DriveName)), -1, Path, MAX_PATH);
        if Failed(Folder.ParseDisplayName(0, nil, Path, Eaten, Result, Attr)) then
        begin
          Folder := nil;
          // Failure probably means that this is not a drive. However, do not
          // call PathToPidlBind() because it may cause infinite recursion.
        end;
      end;
    end;
    PidlFree(Drives);
  end;
end;

function PathToPidl(const Path: string; Folder: IShellFolder): PItemIdList;
var
  DesktopFolder: IShellFolder;
  CharsParsed, Attr: ULONG;
  WidePath: TUnicodePath;
begin
  Result := nil;
  MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PChar(Path), -1, WidePath, MAX_PATH);
  if Folder <> nil then
    Folder.ParseDisplayName(0, nil, WidePath, CharsParsed, Result, Attr)
  else
  if Succeeded(SHGetDesktopFolder(DesktopFolder)) then
    DesktopFolder.ParseDisplayName(0, nil, WidePath, CharsParsed, Result, Attr);
end;

function PathToPidlBind(const FileName: string; out Folder: IShellFolder): PItemIdList;
var
  Attr, Eaten: ULONG;
  PathIdList: PItemIdList;
  DesktopFolder: IShellFolder;
  Path, ItemName: TUnicodePath;
begin
  Result := nil;
  MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PChar(ExtractFilePath(FileName)), -1, Path, MAX_PATH);
  MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PChar(ExtractFileName(FileName)), -1, ItemName, MAX_PATH);
  if Succeeded(SHGetDesktopFolder(DesktopFolder)) then
  begin
    if Succeeded(DesktopFolder.ParseDisplayName(0, nil, Path, Eaten, PathIdList,
      Attr)) then
    begin
      if Succeeded(DesktopFolder.BindToObject(PathIdList, nil, IID_IShellFolder,
        Pointer(Folder))) then
      begin
        if Failed(Folder.ParseDisplayName(0, nil, ItemName, Eaten, Result, Attr)) then
        begin
          Folder := nil;
          Result := DriveToPidlBind(FileName, Folder);
        end;
      end;
      PidlFree(PathIdList);
    end
    else
      Result := DriveToPidlBind(FileName, Folder);
  end;
end;

function PidlBindToParent(IdList: PItemIdList; out Folder: IShellFolder; out Last: PItemIdList): Boolean;
var
  Path: string;
begin
  Last := nil;
  Path := PidlToPath(IdList);
  Last := PathToPidlBind(Path, Folder);
  Result := Last <> nil;
  if Last = nil then
    Folder := nil;
end;

function PidlCompare(Pidl1, Pidl2: PItemIdList): Boolean;
var
  L: Integer;
begin
  Result := False;
  L := PidlGetLength(Pidl1);
  if L = PidlGetLength(Pidl2) then
    Result := CompareMem(Pidl1, Pidl2, L);
end;

function PidlCopy(Source: PItemIdList; out Dest: PItemIdList): Boolean;
var
  L: Integer;
begin
  Result := False;
  Dest := Source;
  if Source <> nil then
  begin
    L := PidlGetLength(Source) + 2;
    if SHAllocMem(Pointer(Dest), L) then
    begin
      Move(Source^, Dest^, L);
      Result := True;
    end;
  end;
end;

function PidlFree(var IdList: PItemIdList): Boolean;
var
  Malloc: IMalloc;
begin
  Result := False;
  if IdList = nil then
    Result := True
  else
  begin
    if Succeeded(SHGetMalloc(Malloc)) and (Malloc.DidAlloc(IdList) > 0) then
    begin
      Malloc.Free(IdList);
      IdList := nil;
      Result := True;
    end;
  end;
end;

function PidlGetDepth(Pidl: PItemIdList): Integer;
var
  P: PItemIdList;
begin
  Result := 0;
  if Pidl <> nil then
  begin
    P := Pidl;
    while (P^.mkId.cb <> 0) and (Result < MAX_PATH) do
    begin
      Inc(Result);
      P := PItemIdList(@P^.mkId.abID[P^.mkId.cb - 2]);
    end;
  end;
  if Result = MAX_PATH then
    Result := -1;
end;

function PidlGetLength(Pidl: PItemIdList): Integer;
var
  P: PItemIdList;
  I: Integer;
begin
  Result := 0;
  if Pidl <> nil then
  begin
    I := 0;
    P := Pidl;
    while (P^.mkId.cb <> 0) and (I < MAX_PATH) do
    begin
      Inc(I);
      Inc(Result, P^.mkId.cb);
      P := PItemIdList(@P^.mkId.abID[P^.mkId.cb - 2]);
    end;
    if I = MAX_PATH then
      Result := -1;
  end;
end;

function PidlGetNext(Pidl: PItemIdList): PItemIdList;
begin
  Result := nil;
  if (Pidl <> nil) and (Pidl^.mkid.cb <> 0) then
  begin
    Result := PItemIdList(@Pidl^.mkId.abID[Pidl^.mkId.cb - 2]);
    if Result^.mkid.cb = 0 then
      Result := nil;
  end;
end;

function PidlToPath(IdList: PItemIdList): string;
begin
  SetLength(Result, MAX_PATH);
  if SHGetPathFromIdList(IdList, PChar(Result)) then
    StrResetLength(Result)
  else
    Result := '';
end;

function StrRetFreeMem(StrRet: TStrRet): Boolean;
begin
  Result := False;
  if StrRet.uType = STRRET_WSTR then
    Result := SHFreeMem(Pointer(StrRet.pOleStr));
end;

function StrRetToString(IdList: PItemIdList; StrRet: TStrRet; Free: Boolean): string;
begin
  case StrRet.uType of
    STRRET_WSTR:
      begin
        Result := WideCharToString(StrRet.pOleStr);
        if Free then
          SHFreeMem(Pointer(StrRet.pOleStr));
      end;
    STRRET_OFFSET:
      if IdList <> nil then
        Result := PChar(IdList) + StrRet.uOffset
      else
        Result := '';
    STRRET_CSTR:
      Result := StrRet.cStr;
  else
    Result := '';
  end;
end;

//=== ShortCuts / Shell link =================================================

procedure ShellLinkFree(var Link: TShellLink);
begin
  PidlFree(Link.IdList);
end;

const
  IID_IShellLink: TGUID = { IID_IShellLinkA }
    (D1:$000214EE; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));

function ShellLinkCreateSystem(const Link: TShellLink; const Folder: Integer;
  const FileName: string): HRESULT;
var
  Path: string;
  Pidl: PItemIDList;
begin
  Result := E_INVALIDARG;
  SetLength(Path, MAX_PATH);
  if Succeeded(SHGetSpecialFolderLocation(0, Folder, Pidl)) then
  begin
    Path := PidltoPath(Pidl);
    if Path <> '' then
    begin
      StrResetLength(Path);
      Result := ShellLinkCreate(Link, PathAddSeparator(Path) + FileName);
    end;
  end;
end;

function ShellLinkCreate(const Link: TShellLink; const FileName: string): HRESULT;
var
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
  LinkName: TUnicodePath;
begin
  Result := CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
    IID_IShellLink, ShellLink);
  if Succeeded(Result) then
  begin
    ShellLink.SetArguments(PChar(Link.Arguments));
    ShellLink.SetShowCmd(Link.ShowCmd);
    ShellLink.SetWorkingDirectory(PChar(Link.WorkingDirectory));
    ShellLink.SetPath(PChar(Link.Target));
    ShellLink.SetDescription(PChar(Link.Description));
    ShellLink.SetHotkey(Link.HotKey);
    ShellLink.SetIconLocation(PChar(Link.IconLocation), Link.IconIndex);
    PersistFile := ShellLink as IPersistFile;
    MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PChar(FileName), -1,
      LinkName, MAX_PATH);
    Result := PersistFile.Save(LinkName, True);
  end;
end;

function RtdlLoadMsiFuncs:Boolean;
begin
  Result:=False;
  if LoadModule(rtdlMsiLibHandle,MSILIB) then
  begin
    if not Assigned(RtdlMsiGetShortcutTarget) then
      RtdlMsiGetShortcutTarget:=GetModuleSymbol(rtdlMsiLibHandle,'MsiGetShortcutTargetA');

    if not Assigned(RtdlMsiGetComponentPath) then
      RtdlMsiGetComponentPath:=GetModuleSymbol(rtdlMsiLibHandle,'MsiGetComponentPathA');

    Result:=(Assigned(RtdlMsiGetShortcutTarget)) and (Assigned(RtdlMsiGetComponentPath));
  end;
end;

function ShellLinkResolve(const FileName: string; var Link: TShellLink): HRESULT;
begin
  Result := ShellLinkResolve(FileName, Link, SLR_ANY_MATCH);
end;

function ShellLinkResolve(const FileName: string; var Link: TShellLink;
  const ResolveFlags: Cardinal): HRESULT;
const
  MAX_FEATURE_CHARS = 38;   // maximum chars in MSI feature name
var
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
  LinkName: TUnicodePath;
  Buffer: string;
  Win32FindData: TWin32FindData;
  FullPath: string;
  ProductGuid: array [0..38] of Char;
  FeatureID: array [0..MAX_FEATURE_CHARS] of Char;
  ComponentGUID: array [0..38] of Char;
  TargetFile: array [0..MAX_PATH] of Char;
  PathSize: DWORD;
  TargetResolved: Boolean;
begin
  Result := CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
    IID_IShellLink, ShellLink);

  if Succeeded(Result) then
  begin
    TargetResolved := False;

    // Handle MSI style shortcuts without invoking the Windows installer if
    // the feature was set to "Install on first use"
    if RtdlLoadMsiFuncs then
    begin
      FillChar(ProductGuid, SizeOf(ProductGuid), #0);
      FillChar(FeatureID, SizeOf(FeatureID), #0);
      FillChar(ComponentGuid, SizeOf(ComponentGuid), #0);
      FillChar(TargetFile, SizeOf(TargetFile), #0);

      if RtdlMsiGetShortcutTarget(PAnsiChar(FileName), ProductGuid, FeatureID, ComponentGuid) = ERROR_SUCCESS then
      begin
        PathSize := MAX_PATH + 1;
        RtdlMsiGetComponentPath(ProductGuid, ComponentGuid, TargetFile, @PathSize);

        if TargetFile <> '' then
        begin
          Link.Target := TargetFile;
          TargetResolved := True;
        end;
      end;
    end;

    PersistFile := ShellLink as IPersistFile;
    // PersistFile.Load fails if the filename is not fully qualified
    FullPath := ExpandFileName(FileName);
    MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PChar(FullPath), -1, LinkName, MAX_PATH);
    Result := PersistFile.Load(LinkName, STGM_READ);

    if Succeeded(Result) then
    begin
      Result := ShellLink.Resolve(0, ResolveFlags);

      if Succeeded(Result) then
      begin
        SetLength(Buffer, MAX_PATH);

        if not TargetResolved then
        begin
          ShellLink.GetPath(PChar(Buffer), MAX_PATH, Win32FindData, SLGP_SHORTPATH);
          Link.Target := PChar(Buffer);
        end;

        ShellLink.GetArguments(PChar(Buffer), MAX_PATH);
        Link.Arguments := PChar(Buffer);
        ShellLink.GetShowCmd(Link.ShowCmd);
        ShellLink.GetWorkingDirectory(PChar(Buffer), MAX_PATH);
        Link.WorkingDirectory := PChar(Buffer);
        ShellLink.GetDescription(PChar(Buffer), MAX_PATH);
        Link.Description := PChar(Buffer);
        ShellLink.GetIconLocation(PChar(Buffer), MAX_PATH, Link.IconIndex);
        Link.IconLocation := PChar(Buffer);
        ShellLink.GetHotkey(Link.HotKey);
        ShellLink.GetIDList(Link.IdList);
      end;
    end;
  end;
end;

function ShellLinkIcon(const Link: TShellLink): HICON; overload;
var
  LocExt: string;
  Info: TSHFileInfo;
begin
  Result := 0;
  LocExt := LowerCase(ExtractFileExt(Link.IconLocation));
  // 1. See if IconLocation specifies a valid icon file
  if (LocExt = '.ico') and (FileExists(Link.IconLocation)) then
  begin
    { TODO : Implement loading from an .ico file }
  end;
  // 2. See if IconLocation specifies an executable
  if Result = 0 then
  begin
    if (LocExt = '.dll') or (LocExt = '.exe') then
      Result := ExtractIcon(0, PChar(Link.IconLocation), Link.IconIndex);
  end;
  // 3. See if target specifies a file
  if Result = 0 then
  begin
    if FileExists(Link.Target) then
      Result := ExtractIcon(0, PChar(Link.Target), Link.IconIndex);
  end;
  // 4. See if the target is an object
  if Result = 0 then
  begin
    if Link.IdList <> nil then
    begin
      FillChar(Info, SizeOf(Info), 0);
      if SHGetFileInfo(PChar(Link.IdList), 0, Info, SizeOf(Info), SHGFI_PIDL or SHGFI_ICON) <> 0 then
        Result := Info.hIcon;
    end;
  end;
end;

function ShellLinkIcon(const FileName: string): HICON; overload;
var
  Link: TShellLink;
begin
  if Succeeded(ShellLinkResolve(FileName, Link)) then
  begin
    Result := ShellLinkIcon(Link);
    ShellLinkFree(Link);
  end
  else
    Result := 0;
end;

//=== Miscellaneous ==========================================================

function SHGetItemInfoTip(const Folder: IShellFolder; Item: PItemIdList): string;
var
  QueryInfo: IQueryInfo;
  InfoTip: PWideChar;
begin
  Result := '';
  if (Item = nil) or (Folder = nil) then
    Exit;
  if Succeeded(Folder.GetUIObjectOf(0, 1, Item, IQueryInfo, nil,
    Pointer(QueryInfo))) then
  begin
    if Succeeded(QueryInfo.GetInfoTip(0, InfoTip)) then
    begin
      Result := WideCharToString(InfoTip);
      SHFreeMem(Pointer(InfoTip));
    end;
  end;
end;

function SHDllGetVersion(const FileName: string; var Version: TDllVersionInfo): Boolean;
type
  TDllGetVersionProc = function (var pdvi: TDllVersionInfo): HRESULT; stdcall;
var
  _DllGetVersion: TDllGetVersionProc;
  LibHandle: HINST;
begin
  Result := False;
  LibHandle := SafeLoadLibrary(FileName);
  if LibHandle <> 0 then
  begin
    @_DllGetVersion := GetProcAddress(LibHandle, PChar('DllGetVersion'));
    if @_DllGetVersion <> nil then
    begin
      Version.cbSize := SizeOf(TDllVersionInfo);
      Result := Succeeded(_DllGetVersion(Version));
    end;
    FreeLibrary(LibHandle);
  end;
end;

function OverlayIcon(var Icon: HICON; Overlay: HICON; Large: Boolean): Boolean;
var
  Source, Dest: HIMAGELIST;
  Width, Height: Integer;
begin
  Result := False;
  if Large then
  begin
    Width := GetSystemMetrics(SM_CXICON);
    Height := GetSystemMetrics(SM_CYICON);
    Source := ImageList_Create(Width, Height, ILC_MASK or ILC_COLOR32, 1, 0);
  end
  else
  begin
    Width := GetSystemMetrics(SM_CXSMICON);
    Height := GetSystemMetrics(SM_CYSMICON);
    Source := ImageList_Create(Width, Height, ILC_MASK or ILC_COLOR32, 1, 0);
  end;
  if Source <> 0 then
  begin
    if (ImageList_AddIcon(Source, Icon) <> -1) and
      (ImageList_AddIcon(Source, Overlay) <> -1) then
    begin
      Dest := HIMAGELIST(ImageList_Merge(Source, 0, Source, 1, 0, 0));
      if Dest <> 0 then
      begin
        DestroyIcon(Icon);
        Icon := ImageList_ExtractIcon(0, Dest, 0);
        ImageList_Destroy(Dest);
        Result := True;
      end;
    end;
    ImageList_Destroy(Source);
  end;
end;

function OverlayIconShortCut(var Large, Small: HICON): Boolean;
var
  OvlLarge, OvlSmall: HICON;
begin
  Result := False;
  if ExtractIconEx(PChar('shell32.dll'), 29, OvlLarge, OvlSmall, 1) = 2 then
  begin
    OverlayIcon(Large, OvlLarge, True);
    OverlayIcon(Small, OvlSmall, False);
  end;
end;

function OverlayIconShared(var Large, Small: HICON): Boolean;
var
  OvlLarge, OvlSmall: HICON;
begin
  Result := False;
  if ExtractIconEx(PChar('shell32.dll'), 28, OvlLarge, OvlSmall, 1) = 2 then
  begin
    OverlayIcon(Large, OvlLarge, True);
    OverlayIcon(Small, OvlSmall, False);
  end;
end;

function GetSystemIcon(IconIndex: Integer; Flags: Cardinal): HICON;
var
  FileInfo: TSHFileInfo;
  ImageList: HIMAGELIST;
begin
  FillChar(FileInfo, SizeOf(FileInfo), #0);
  if Flags = 0 then
    Flags := SHGFI_SHELLICONSIZE;
  ImageList := SHGetFileInfo('', 0, FileInfo, SizeOf(FileInfo),
    Flags or SHGFI_SYSICONINDEX);
  Result := ImageList_ExtractIcon(0, ImageList, IconIndex);
end;

function ShellExecEx(const FileName: string; const Parameters: string;
  const Verb: string; CmdShow: Integer): Boolean;
var
  Sei: TShellExecuteInfo;
begin
  FillChar(Sei, SizeOf(Sei), #0);
  Sei.cbSize := SizeOf(Sei);
  Sei.fMask := SEE_MASK_DOENVSUBST or SEE_MASK_FLAG_NO_UI;
  Sei.lpFile := PChar(FileName);
  Sei.lpParameters := PCharOrNil(Parameters);
  Sei.lpVerb := PCharOrNil(Verb);
  Sei.nShow := CmdShow;
  {$T+}
  Result := ShellExecuteEx(@Sei);
  {$T-}
end;

{ TODO -cHelp : author Jean-Fabien Connault note, ShellExecEx() above used to be ShellExec()... }

function ShellExec(Wnd: Integer; const Operation, FileName, Parameters, Directory: string; ShowCommand: Integer): Boolean;
begin
  Result := ShellExecute(Wnd, PChar(Operation), PChar(FileName), PChar(Parameters),
    PChar(Directory), ShowCommand) > 32;
end;

function ShellExecAndWait(const FileName: string; const Parameters: string;
  const Verb: string; CmdShow: Integer; const Directory: string): Boolean;
var
  Sei: TShellExecuteInfo;
  Res: LongBool;
  Msg: tagMSG;
begin
  FillChar(Sei, SizeOf(Sei), #0);
  Sei.cbSize := SizeOf(Sei);
  Sei.fMask := SEE_MASK_DOENVSUBST  or SEE_MASK_FLAG_NO_UI  or SEE_MASK_NOCLOSEPROCESS or
    SEE_MASK_FLAG_DDEWAIT;
  Sei.lpFile := PChar(FileName);
  Sei.lpParameters := PCharOrNil(Parameters);
  Sei.lpVerb := PCharOrNil(Verb);
  Sei.nShow := CmdShow;
  Sei.lpDirectory := PCharOrNil(Directory);
  {$T+}
  Result := ShellExecuteEx(@Sei);
  {$T-}
  if Result then
  begin
    WaitForInputIdle(Sei.hProcess, INFINITE);
    while WaitForSingleObject(Sei.hProcess, 10) = WAIT_TIMEOUT do
      repeat
        Res := PeekMessage(Msg, Sei.Wnd, 0, 0, PM_REMOVE);
        if Res then
        begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
      until not Res;
    CloseHandle(Sei.hProcess);
  end;
end;

function ShellOpenAs(const FileName: string): Boolean;
begin
  Result := ShellExecEx('rundll32', Format('shell32.dll,OpenAs_RunDLL "%s"', [FileName]), '', SW_SHOWNORMAL);
end;

{ TODO: Dynamic linking - move TRasDialDlgA to JclWin32}

type
  TRasDialDlgA = function(lpszPhonebook, lpszEntry, lpszPhoneNumber: PAnsiChar; lpInfo: PRasDialDlg): BOOL; stdcall;

function ShellRasDial(const EntryName: string): Boolean;
var
  Info: TRasDialDlg;
  RasDlg: HModule;
  RasDialDlgA: TRasDialDlgA;
begin
   if IsWinNT then
   begin
     Result := False;
     RasDlg := SafeLoadLibrary('rasdlg.dll');
     if RasDlg <> 0 then
     try
       @RasDialDlgA := GetProcAddress(RasDlg, PChar('RasDialDlgA'));
       if @RasDialDlgA <> nil then
       begin
         FillChar(Info, SizeOf(Info), 0);
         Info.dwSize := SizeOf(Info);
         Result := RasDialDlgA(nil, PChar(EntryName), nil, @Info);
       end;   
     finally   
       FreeLibrary(RasDlg);
     end;   
   end 
   else
     Result := ShellExecEx('rundll32', Format('rnaui.dll,RnaDial "%s"', [EntryName]), '', SW_SHOWNORMAL);
end;

// You can pass simple name of standard system control panel (e.g. 'timedate')
// or full qualified file name (Window 95 only? doesn't work on Win2K!)
// MT: Added support for Windows 98..XP. Have no win95 anymore so I have to
//     trust that the original version works on Windows 95 and Windows 95OSR2.

function ShellRunControlPanel(const NameOrFileName: string; AppletNumber: Integer): Boolean;
var
  FileName: TFileName;
begin
  if ExtractFilePath(NameOrFileName) = '' then
    FileName := ChangeFileExt(PathAddSeparator(GetWindowsSystemFolder) + NameOrFileName, '.cpl')
  else
    FileName := NameOrFileName;
  if FileExists(FileName) then
  begin
    if (IsWin95 or IsWin95OSR2) then
      Result := ShellExecEx('rundll32', Format('shell32.dll,Control_RunDLL "%s", @%d',
        [FileName, AppletNumber]), '', SW_SHOWNORMAL)
    else
      Result := ShellExecEx('rundll32', Format('shell32.dll,Control_RunDLL "%s",,%d',
        [FileName, AppletNumber]), '', SW_SHOWNORMAL)
  end
  else
  begin
    Result := False;
    SetLastError(ERROR_FILE_NOT_FOUND);
  end;
end;

function GetFileExeType(const FileName: TFileName): TJclFileExeType;
var
  FileInfo: TSHFileInfo;
  R: DWORD;
begin
  R := SHGetFileInfo(PChar(FileName), 0, FileInfo, SizeOf(FileInfo), SHGFI_EXETYPE);
  case LoWord(R) of
    IMAGE_DOS_SIGNATURE:
      Result := etMsDos;
    IMAGE_OS2_SIGNATURE:
      Result := etWin16;
    Word(IMAGE_NT_SIGNATURE):
      if HiWord(R) = 0 then
        Result := etWin32Con
      else
        Result := etWin32Gui;
  else
    Result := etError;
  end;
end;

function ShellFindExecutable(const FileName, DefaultDir: string): string;
var
  Res: HINST;
  Buffer: TAnsiPath;
  I: Integer;
begin
  FillChar(Buffer, SizeOf(Buffer), #0);
  Res := FindExecutable(PChar(FileName), PCharOrNil(DefaultDir), Buffer);
  if Res > 32 then
  begin
    // FindExecutable replaces #32 with #0
    for I := Low(Buffer) to High(Buffer) - 1 do
      if Buffer[I] = #0 then
        Buffer[I] := #32;
    Buffer[High(Buffer)] := #0;
    Result := Trim(Buffer);
  end
  else
    Result := '';
end;

function GetFileNameIcon(const FileName: string; Flags: Cardinal = 0): HICON;
var
  FileInfo: TSHFileInfo;
  ImageList: HIMAGELIST;
begin
  FillChar(FileInfo, SizeOf(FileInfo), #0);
  if Flags = 0 then
    Flags := SHGFI_SHELLICONSIZE;
  ImageList := SHGetFileInfo(PChar(FileName), 0, FileInfo, SizeOf(FileInfo),
    Flags or SHGFI_SYSICONINDEX);
  if ImageList <> 0 then
    Result := ImageList_ExtractIcon(0, ImageList, FileInfo.iIcon)
  else
    Result := 0;
end;

initialization
  //We don't load the msi functions until the first attempt to resolve an MSI link

  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  UnloadModule(rtdlMsiLibHandle);

end.


