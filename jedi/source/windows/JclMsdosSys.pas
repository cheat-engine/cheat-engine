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
{ The Original Code is JclMsdosSys.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Robert Marquardt                                   }
{ Portions created by Robert Marquardt are Copyright (C) 2001 Robert Marquardt                     }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributor(s): Robert Rossmair (IJclMsdosSys interface)                                         }
{                                                                                                  }
{ You may retrieve the latest version of this file at the Project JEDI's Code Library home page,   }
{ located at http://sourceforge.net/projects/jcl/                                                  }
{                                                                                                  }
{ Known Issues: None                                                                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclMsdosSys;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, SysUtils;

type
  IJclMsdosSys = interface
  ['{3E1C7E48-49E5-408B-86D2-9924D223B5C5}']
    // Property access methods
    function GetAutoScan: Boolean;
    function GetBootDelay: Cardinal;
    function GetBootGUI: Boolean;
    function GetBootKeys: Boolean;
    function GetBootMenu: Boolean;
    function GetBootMenuDefault: Cardinal;
    function GetBootMenuDelay: Cardinal;
    function GetBootMulti: Boolean;
    function GetBootSafe: Boolean;
    function GetBootWarn: Boolean;
    function GetBootWin: Boolean;
    function GetDBLSpace: Boolean;
    function GetDoubleBuffer: Boolean;
    function GetDRVSpace: Boolean;
    function GetHostWinBootDrv: Char;
    function GetLoadTop: Boolean;
    function GetLogo: Boolean;
    function GetNetwork: Boolean;
    function GetUninstallDir: Char;
    function GetWinBootDir: string;
    function GetWinDir: string;
    function GetWinVer: string;
    procedure SetUninstallDir(AUninstallDir: Char);
    procedure SetWinDir(AWinDir: string);
    procedure SetWinBootDir(AWinBootDir: string);
    procedure SetHostWinBootDrv(AHostWinBootDrv: Char);
    procedure SetAutoScan(AAutoScan: Boolean);
    procedure SetBootDelay(ABootDelay: Cardinal);
    procedure SetBootGUI(ABootGUI: Boolean);
    procedure SetBootKeys(ABootKeys: Boolean);
    procedure SetBootMenu(ABootMenu: Boolean);
    procedure SetBootMenuDefault(ABootMenuDefault: Cardinal);
    procedure SetBootMenuDelay(ABootMenuDelay: Cardinal);
    procedure SetBootMulti(ABootMulti: Boolean);
    procedure SetBootSafe(ABootSafe: Boolean);
    procedure SetBootWarn(ABootWarn: Boolean);
    procedure SetBootWin(ABootWin: Boolean);
    procedure SetDBLSpace(ADBLSpace: Boolean);
    procedure SetDRVSpace(ADRVSpace: Boolean);
    procedure SetDoubleBuffer(ADoubleBuffer: Boolean);
    procedure SetLoadTop(ALoadTop: Boolean);
    procedure SetLogo(ALogo: Boolean);
    procedure SetNetwork(ANetwork: Boolean);
    procedure SetWinVer(AWinVer: string);
    procedure SetBool(var ANew: Boolean; AOld: Boolean);
    procedure SetString(var ANew: string; AOld: string);
    // Properties
    property UninstallDir: Char read GetUninstallDir write SetUninstallDir;
    property WinDir: string read GetWinDir write SetWinDir;
    property WinBootDir: string read GetWinBootDir write SetWinBootDir;
    property HostWinBootDrv: Char read GetHostWinBootDrv write SetHostWinBootDrv;
    property AutoScan: Boolean read GetAutoScan write SetAutoScan;
    property BootDelay: Cardinal read GetBootDelay write SetBootDelay;
    property BootGUI: Boolean read GetBootGUI write SetBootGUI;
    property BootKeys: Boolean read GetBootKeys write SetBootKeys;
    property BootMenu: Boolean read GetBootMenu write SetBootMenu;
    property BootMenuDefault: Cardinal read GetBootMenuDefault write SetBootMenuDefault;
    property BootMenuDelay: Cardinal read GetBootMenuDelay write SetBootMenuDelay;
    property BootMulti: Boolean read GetBootMulti write SetBootMulti;
    property BootSafe: Boolean read GetBootSafe write SetBootSafe;
    property BootWarn: Boolean read GetBootWarn write SetBootWarn;
    property BootWin: Boolean read GetBootWin write SetBootWin;
    property DBLSpace: Boolean read GetDBLSpace write SetDBLSpace;
    property DRVSpace: Boolean read GetDRVSpace write SetDRVSpace;
    property DoubleBuffer: Boolean read GetDoubleBuffer write SetDoubleBuffer;
    property LoadTop: Boolean read GetLoadTop write SetLoadTop;
    property Logo: Boolean read GetLogo write SetLogo;
    property Network: Boolean read GetNetwork write SetNetwork;
    property WinVer: string read GetWinVer write SetWinVer;
  end;

function GetMsdosSys: IJclMsdosSys;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclMsdosSys.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

const
  cMsdosSys = 'C:\MSDOS.SYS';

type
  TJclMsdosSys = class(TInterfacedObject, IJclMsdosSys)
  private
    FUninstallDir: Char;
    FWinDir: string;
    FWinBootDir: string;
    FHostWinBootDrv: Char;
    FAutoScan: Boolean;
    FBootDelay: Cardinal;
    FBootGUI: Boolean;
    FBootKeys: Boolean;
    FBootMenu: Boolean;
    FBootMenuDefault: Cardinal;
    FBootMenuDelay: Cardinal;
    FBootMulti: Boolean;
    FBootSafe: Boolean;
    FBootWarn: Boolean;
    FBootWin: Boolean;
    FDBLSpace: Boolean;
    FDRVSpace: Boolean;
    FDoubleBuffer: Boolean;
    FLoadTop: Boolean;
    FLogo: Boolean;
    FNetwork: Boolean;
    FWinVer: string;
    function GetAutoScan: Boolean;
    function GetBootDelay: Cardinal;
    function GetBootGUI: Boolean;
    function GetBootKeys: Boolean;
    function GetBootMenu: Boolean;
    function GetBootMenuDefault: Cardinal;
    function GetBootMenuDelay: Cardinal;
    function GetBootMulti: Boolean;
    function GetBootSafe: Boolean;
    function GetBootWarn: Boolean;
    function GetBootWin: Boolean;
    function GetDBLSpace: Boolean;
    function GetDoubleBuffer: Boolean;
    function GetDRVSpace: Boolean;
    function GetHostWinBootDrv: Char;
    function GetLoadTop: Boolean;
    function GetLogo: Boolean;
    function GetNetwork: Boolean;
    function GetUninstallDir: Char;
    function GetWinBootDir: string;
    function GetWinDir: string;
    function GetWinVer: string;
    procedure SetUninstallDir(AUninstallDir: Char);
    procedure SetWinDir(AWinDir: string);
    procedure SetWinBootDir(AWinBootDir: string);
    procedure SetHostWinBootDrv(AHostWinBootDrv: Char);
    procedure SetAutoScan(AAutoScan: Boolean);
    procedure SetBootDelay(ABootDelay: Cardinal);
    procedure SetBootGUI(ABootGUI: Boolean);
    procedure SetBootKeys(ABootKeys: Boolean);
    procedure SetBootMenu(ABootMenu: Boolean);
    procedure SetBootMenuDefault(ABootMenuDefault: Cardinal);
    procedure SetBootMenuDelay(ABootMenuDelay: Cardinal);
    procedure SetBootMulti(ABootMulti: Boolean);
    procedure SetBootSafe(ABootSafe: Boolean);
    procedure SetBootWarn(ABootWarn: Boolean);
    procedure SetBootWin(ABootWin: Boolean);
    procedure SetDBLSpace(ADBLSpace: Boolean);
    procedure SetDRVSpace(ADRVSpace: Boolean);
    procedure SetDoubleBuffer(ADoubleBuffer: Boolean);
    procedure SetLoadTop(ALoadTop: Boolean);
    procedure SetLogo(ALogo: Boolean);
    procedure SetNetwork(ANetwork: Boolean);
    procedure SetWinVer(AWinVer: string);
    procedure SetBool(var ANew: Boolean; AOld: Boolean);
    procedure SetString(var ANew: string; AOld: string);
    procedure ReadMsdosSys;
    procedure WriteMsdosSys;
  public
    constructor Create;
    destructor Destroy; override;
    property UninstallDir: Char read GetUninstallDir write SetUninstallDir;
    property WinDir: string read GetWinDir write SetWinDir;
    property WinBootDir: string read GetWinBootDir write SetWinBootDir;
    property HostWinBootDrv: Char read GetHostWinBootDrv write SetHostWinBootDrv;
    property AutoScan: Boolean read GetAutoScan write SetAutoScan;
    property BootDelay: Cardinal read GetBootDelay write SetBootDelay;
    property BootGUI: Boolean read GetBootGUI write SetBootGUI;
    property BootKeys: Boolean read GetBootKeys write SetBootKeys;
    property BootMenu: Boolean read GetBootMenu write SetBootMenu;
    property BootMenuDefault: Cardinal read GetBootMenuDefault write SetBootMenuDefault;
    property BootMenuDelay: Cardinal read GetBootMenuDelay write SetBootMenuDelay;
    property BootMulti: Boolean read GetBootMulti write SetBootMulti;
    property BootSafe: Boolean read GetBootSafe write SetBootSafe;
    property BootWarn: Boolean read GetBootWarn write SetBootWarn;
    property BootWin: Boolean read GetBootWin write SetBootWin;
    property DBLSpace: Boolean read GetDBLSpace write SetDBLSpace;
    property DRVSpace: Boolean read GetDRVSpace write SetDRVSpace;
    property DoubleBuffer: Boolean read GetDoubleBuffer write SetDoubleBuffer;
    property LoadTop: Boolean read GetLoadTop write SetLoadTop;
    property Logo: Boolean read GetLogo write SetLogo;
    property Network: Boolean read GetNetwork write SetNetwork;
    property WinVer: string read GetWinVer write SetWinVer;
  end;

function GetMsdosSys: IJclMsdosSys;
begin
  Result := TJclMsdosSys.Create;
end;

constructor TJclMsdosSys.Create;
begin
  inherited Create;
  ReadMsdosSys;
end;

destructor TJclMsdosSys.Destroy;
begin
  WriteMsdosSys;
  inherited Destroy;
end;

function TJclMsdosSys.GetAutoScan: Boolean;
begin
  Result := FAutoScan;
end;

function TJclMsdosSys.GetBootDelay: Cardinal;
begin
  Result := FBootDelay;
end;

function TJclMsdosSys.GetBootGUI: Boolean;
begin
  Result := FBootGUI;
end;

function TJclMsdosSys.GetBootMenu: Boolean;
begin
  Result := FBootMenu;
end;

function TJclMsdosSys.GetBootKeys: Boolean;
begin
  Result := FBootKeys;
end;

function TJclMsdosSys.GetBootMenuDefault: Cardinal;
begin
  Result := FBootMenuDefault;
end;

function TJclMsdosSys.GetBootMenuDelay: Cardinal;
begin
  Result := FBootMenuDelay;
end;

function TJclMsdosSys.GetBootMulti: Boolean;
begin
  Result := FBootMulti;
end;

function TJclMsdosSys.GetBootSafe: Boolean;
begin
  Result := FBootSafe;
end;

function TJclMsdosSys.GetBootWarn: Boolean;
begin
  Result := FBootWarn;
end;

function TJclMsdosSys.GetBootWin: Boolean;
begin
  Result := FBootWin;
end;

function TJclMsdosSys.GetDBLSpace: Boolean;
begin
  Result := FDBLSpace;
end;

function TJclMsdosSys.GetDoubleBuffer: Boolean;
begin
  Result := FDoubleBuffer;
end;

function TJclMsdosSys.GetDRVSpace: Boolean;
begin
  Result := FDRVSpace;
end;

function TJclMsdosSys.GetHostWinBootDrv: Char;
begin
  Result := FHostWinBootDrv;
end;

function TJclMsdosSys.GetLoadTop: Boolean;
begin
  Result := FLoadTop;
end;

function TJclMsdosSys.GetLogo: Boolean;
begin
  Result := FLogo;
end;

function TJclMsdosSys.GetNetwork: Boolean;
begin
  Result := FNetWork;
end;

function TJclMsdosSys.GetUninstallDir: Char;
begin
  Result := FUninstallDir;
end;

function TJclMsdosSys.GetWinBootDir: string;
begin
  Result := FWinBootDir;
end;

function TJclMsdosSys.GetWinDir: string;
begin
  Result := FWinDir;
end;

function TJclMsdosSys.GetWinVer: string;
begin
  Result := FWinVer;
end;

procedure TJclMsdosSys.SetUninstallDir(AUninstallDir: Char);
begin
  if UninstallDir <> AUninstallDir then
  begin
    FUninstallDir := AUninstallDir;
    WriteMsdosSys;
  end;
end;

procedure TJclMsdosSys.SetWinDir(AWinDir: string);
begin
  SetString(FWinDir, AWinDir);
end;

procedure TJclMsdosSys.SetWinBootDir(AWinBootDir: string);
begin
  SetString(FWinBootDir, AWinBootDir);
end;

procedure TJclMsdosSys.SetHostWinBootDrv(AHostWinBootDrv: Char);
begin
  if HostWinBootDrv <> AHostWinBootDrv then
  begin
    FHostWinBootDrv := AHostWinBootDrv;
    WriteMsdosSys;
  end;
end;

procedure TJclMsdosSys.SetAutoScan(AAutoScan: Boolean);
begin
  SetBool(FAutoScan, AAutoScan);
end;

procedure TJclMsdosSys.SetBootDelay(ABootDelay: Cardinal);
begin
  if BootDelay <> ABootDelay then
  begin
    FBootDelay := ABootDelay;
    WriteMsdosSys;
  end;
end;

procedure TJclMsdosSys.SetBootGUI(ABootGUI: Boolean);
begin
  SetBool(FBootGUI, ABootGUI);
end;

procedure TJclMsdosSys.SetBootKeys(ABootKeys: Boolean);
begin
  SetBool(FBootKeys, ABootKeys);
end;

procedure TJclMsdosSys.SetBootMenu(ABootMenu: Boolean);
begin
  SetBool(FBootMenu, ABootMenu);
end;

procedure TJclMsdosSys.SetBootMenuDefault(ABootMenuDefault: Cardinal);
begin
  if BootMenuDefault <> ABootMenuDefault then
  begin
    FBootMenuDefault := ABootMenuDefault;
    WriteMsdosSys;
  end;
end;

procedure TJclMsdosSys.SetBootMenuDelay(ABootMenuDelay: Cardinal);
begin
  if BootMenuDelay <> ABootMenuDelay then
  begin
    FBootMenuDelay := ABootMenuDelay;
    WriteMsdosSys;
  end;
end;

procedure TJclMsdosSys.SetBootMulti(ABootMulti: Boolean);
begin
  SetBool(FBootMulti, ABootMulti);
end;

procedure TJclMsdosSys.SetBootSafe(ABootSafe: Boolean);
begin
  SetBool(FBootSafe, ABootSafe);
end;

procedure TJclMsdosSys.SetBootWarn(ABootWarn: Boolean);
begin
  SetBool(FBootWarn, ABootWarn);
end;

procedure TJclMsdosSys.SetBootWin(ABootWin: Boolean);
begin
  SetBool(FBootWin, ABootWin);
end;

procedure TJclMsdosSys.SetDBLSpace(ADBLSpace: Boolean);
begin
  SetBool(FDBLSpace, ADBLSpace);
end;

procedure TJclMsdosSys.SetDRVSpace(ADRVSpace: Boolean);
begin
  SetBool(FDRVSpace, ADRVSpace);
end;

procedure TJclMsdosSys.SetDoubleBuffer(ADoubleBuffer: Boolean);
begin
  SetBool(FDoubleBuffer, ADoubleBuffer);
end;

procedure TJclMsdosSys.SetLoadTop(ALoadTop: Boolean);
begin
  SetBool(FLoadTop, ALoadTop);
end;

procedure TJclMsdosSys.SetLogo(ALogo: Boolean);
begin
  SetBool(FLogo, ALogo);
end;

procedure TJclMsdosSys.SetNetwork(ANetwork: Boolean);
begin
  SetBool(FNetwork, ANetwork);
end;

procedure TJclMsdosSys.SetWinVer(AWinVer: string);
begin
  SetString(FWinVer, AWinVer);
end;

procedure TJclMsdosSys.SetBool(var ANew: Boolean; AOld: Boolean);
begin
  if ANew <> AOld then
  begin
    ANew := AOld;
    WriteMsdosSys;
  end;
end;

procedure TJclMsdosSys.SetString(var ANew: string; AOld: string);
begin
  if ANew <> AOld then
  begin
    ANew := AOld;
    WriteMsdosSys;
  end;
end;

procedure TJclMsdosSys.ReadMsdosSys;
var
  List: TStringList;
  Value: string;

  function BoolVal(const Name: string; const Def: Boolean): Boolean;
  var
    Val: string;
  begin
    Result := Def;
    Val := Trim(List.Values[Name]);
    if Val <> '' then
      if Val[1] = '0' then
        Result := False
      else
      if Val[1] = '1' then
        Result := True;
  end;

begin
  FUninstallDir := #0;
  FHostWinBootDrv := #0;
  List := TStringList.Create;
  try
    List.LoadFromFile(cMsDosSys);
    Value := Trim(List.Values['UninstallDir']);
    if Value <> '' then
      FUninstallDir := Value[1];
    FWinDir := Trim(List.Values['WinDir']);
    FWinBootDir := Trim(List.Values['WinBootDir']);
    Value := Trim(List.Values['HostWinBootDrv']);
    if Value <> '' then
      FHostWinBootDrv := Value[1];

    FAutoScan := BoolVal('AutoScan', True);
    FBootDelay := StrToIntDef(Trim(List.Values['BootDelay']), 2);
    FBootGUI := BoolVal('BootGUI', True);
    FBootKeys := BoolVal('BootKeys', True);
    FBootMenu := BoolVal('BootMenu', False);
    FBootMenuDefault := StrToIntDef(Trim(List.Values['BootMenuDefault']), 1);
    FBootMenuDelay := StrToIntDef(Trim(List.Values['BootMenuDelay']), 30);
    FBootMulti := BoolVal('BootMulti', False);
    FBootSafe := BoolVal('BootSafe', False);
    FBootWarn := BoolVal('BootWarn', True);
    FBootWin := BoolVal('BootWin', True);
    FDBLSpace := BoolVal('DBLSpace', True);
    FDRVSpace := BoolVal('DRVSpace', True);
    FDoubleBuffer := BoolVal('DoubleBuffer', False);
    FLoadTop := BoolVal('LoadTop', True);
    FLogo := BoolVal('Logo', True);
    FNetwork := BoolVal('Network', False);
    FWinVer := Trim(List.Values['WinVer']);
  finally
    List.Free;
  end;
end;

procedure TJclMsdosSys.WriteMsdosSys;
var
  Attributes: Integer;
  I: Char;
  Line: string;
begin
  if not FileExists(cMsDosSys) then
    Exit;
  with TStringList.Create do
  try
    Add('[Paths]');
    if UninstallDir <> #0 then
      Add('UninstallDir=' + UninstallDir);
    if WinDir <> '' then
      Add('WinDir=' + WinDir);
    if WinBootDir <> '' then
      Add('WinBootDir=' + WinBootDir);
    if HostWinBootDrv <> #0 then
      Add('HostWinBootDrv=' + HostWinBootDrv);
    Add('');

    Add('[Options]');
    if not AutoScan then
      Add('AutoScan=0');
    if BootDelay <> 2 then
      Add('BootDelay=' + IntToStr(BootDelay));
    if not BootGUI then
      Add('BootGUI=0');
    if not BootKeys then
      Add('BootKeys=0');
    if BootMenu then
      Add('BootMenu=1');
    if BootMenuDefault <> 1 then
      Add('BootMenuDefault=' + IntToStr(BootMenuDefault));
    if BootMenuDelay <> 30 then
      Add('BootMenuDelay=' + IntToStr(BootMenuDelay));
    if BootMulti then
      Add('BootMulti=1');
    if BootSafe then
      Add('BootSafe=1');
    if not BootWarn then
      Add('BootWarn=0');
    if not BootWin then
      Add('BootWin=0');
    if not DBLSpace then
      Add('DBLSpace=0');
    if not DRVSpace then
      Add('DRVSpace=0');
    if DoubleBuffer then
      Add('DoubleBuffer=1');
    if not LoadTop then
      Add('LoadTop=0');
    if not Logo then
      Add('Logo=0');
    if Network then
      Add('Network=1');
    if WinVer <> '' then
      Add('WinVer=' + WinVer);

    Add(';');
    Add(';The following lines are required for compatibility with other programs.');
    Add(';Do not remove them(MSDOS.SYS needs to be >1024 bytes).');
    Line := ';' + StringOfChar('x', 69);
    for I := 'a' to 's' do
      Add(Line+I);
    Attributes := FileGetAttr(cMsDosSys) and not faReadOnly;
    FileSetAttr(cMsDosSys, Attributes);
    SaveToFile(cMsDosSys);
    FileSetAttr(cMsDosSys, Attributes or faReadOnly);
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
