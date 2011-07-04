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
{ The Original Code is JclLANMan.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Peter Friese.                                      }
{ Portions created by Peter Friese are Copyright (C) Peter Friese. All Rights Reserved.            }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Peter Friese                                                                                   }
{   Andreas Hausladen (ahuser)                                                                     }
{   Robert Marquardt (marquardt)                                                                   }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains routines and classes to handle user and group management tasks. As the name   }
{ implies, it uses the LAN Manager API.                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

// Comments to Win9x compatibility of the functions used in this unit

// The following function exist at last since Win95C, but return always
// the error ERROR_CALL_NOT_IMPLEMENTED
//   AllocateAndInitializeSid, LookupAccountSID, FreeSID

unit JclLANMan;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils, Classes;

// User Management
type
  TNetUserFlag = (ufAccountDisable, ufHomedirRequired, ufLockout,
    ufPasswordNotRequired, ufPasswordCantChange, ufDontExpirePassword,
    ufMNSLogonAccount);
  TNetUserFlags = set of TNetUserFlag;
  TNetUserInfoFlag = (uifScript, uifTempDuplicateAccount, uifNormalAccount,
    uifInterdomainTrustAccount, uifWorkstationTrustAccount, uifServerTrustAccount);
  TNetUserInfoFlags = set of TNetUserInfoFlag;
  TNetUserPriv = (upUnknown, upGuest, upUser, upAdmin);
  TNetUserAuthFlag = (afOpPrint, afOpComm, afOpServer, afOpAccounts);
  TNetUserAuthFlags = set of TNetUserAuthFlag;
  TNetWellKnownRID = (wkrAdmins, wkrUsers, wkrGuests, wkrPowerUsers, wkrBackupOPs,
    wkrReplicator, wkrEveryone);

function CreateAccount(const Server, Username, Fullname, Password, Description,
  Homedir, Script: string;
  const PasswordNeverExpires: Boolean = True): Boolean;
function CreateLocalAccount(const Username, Fullname, Password, Description,
  Homedir, Script: string;
  const PasswordNeverExpires: Boolean = True): Boolean;
function DeleteAccount(const Servername, Username: string): Boolean;
function DeleteLocalAccount(Username: string): Boolean;
function CreateLocalGroup(const Server, Groupname, Description: string): Boolean;
function CreateGlobalGroup(const Server, Groupname, Description: string): Boolean;
function DeleteLocalGroup(const Server, Groupname: string): Boolean;

function GetLocalGroups(const Server: string; const Groups: TStrings): Boolean;
function GetGlobalGroups(const Server: string; const Groups: TStrings): Boolean;
function LocalGroupExists(const Group: string): Boolean;
function GlobalGroupExists(const Server, Group: string): Boolean;

function AddAccountToLocalGroup(const Accountname, Groupname: string): Boolean;
function LookupGroupName(const Server: string; const RID: TNetWellKnownRID): string;
procedure ParseAccountName(const QualifiedName: string; var Domain, UserName: string);
function IsLocalAccount(const AccountName: string): Boolean;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclLANMan.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclBase, JclStrings, JclSysInfo, JclWin32;

function CreateAccount(const Server, Username, Fullname, Password, Description,
  Homedir, Script: string; const PasswordNeverExpires: Boolean): Boolean;
var
  wServer, wUsername, wFullname,
  wPassword, wDescription, wHomedir, wScript: WideString;
  Details: USER_INFO_2;
  Err: NET_API_STATUS;
  ParmErr: DWORD;
begin
  wServer := Server;
  wUsername := Username;
  wFullname := Fullname;
  wPassword := Password;
  wDescription := Description;
  wScript := Script;
  wHomedir := Homedir;

  FillChar(Details, SizeOf(Details), #0);
  with Details do
  begin
    usri2_name := PWideChar(wUsername);
    usri2_full_name := PWideChar(wFullname);
    usri2_password := PWideChar(wPassword);
    usri2_comment := PWideChar(wDescription);
    usri2_priv := USER_PRIV_USER;
    usri2_flags := UF_SCRIPT;
    if PassWordNeverExpires then
      usri2_flags := usri2_flags or UF_DONT_EXPIRE_PASSWD;
    usri2_script_path := PWideChar(wScript);
    usri2_home_dir := PWideChar(wHomedir);
    usri2_acct_expires := TIMEQ_FOREVER;
  end;

  Err := RtdlNetUserAdd(PWideChar(wServer), 2, @Details, @ParmErr);
  Result := (Err = NERR_SUCCESS);
end;

function CreateLocalAccount(const Username, Fullname, Password, Description,
  Homedir, Script: string; const PasswordNeverExpires: Boolean): Boolean;
begin
  Result := CreateAccount('', Username, Fullname, Password, Description, Homedir,
    Script, PassWordNeverExpires);
end;

function DeleteAccount(const Servername, Username: string): Boolean;
var
  wServername, wUsername: WideString;
  Err: NET_API_STATUS;
begin
  wServername := Servername;
  wUsername := Username;
  Err := RtdlNetUserDel(PWideChar(wServername), PWideChar(wUsername));
  Result := (Err = NERR_SUCCESS);
end;

function DeleteLocalAccount(Username: string): Boolean;
begin
  Result := DeleteAccount('', Username);
end;

function CreateGlobalGroup(const Server, Groupname, Description: string): Boolean;
var
  wServer, wGroupname, wDescription: WideString;
  Details: GROUP_INFO_1;
  Err: NET_API_STATUS;
  ParmErr: DWORD;
begin
  wServer := Server;
  wGroupname := Groupname;
  wDescription := Description;

  FillChar(Details, SizeOf(Details), #0);
  Details.grpi1_name := PWideChar(wGroupName);
  Details.grpi1_comment := PWideChar(wDescription);

  Err := RtdlNetGroupAdd(PWideChar(wServer), 1, @Details, @ParmErr);
  Result := (Err = NERR_SUCCESS);
end;

function CreateLocalGroup(const Server, Groupname, Description: string): Boolean;
var
  wServer, wGroupname, wDescription: WideString;
  Details: LOCALGROUP_INFO_1;
  Err: NET_API_STATUS;
  ParmErr: DWORD;
begin
  wServer := Server;
  wGroupname := Groupname;
  wDescription := Description;

  FillChar(Details, SizeOf(Details), #0);
  Details.lgrpi1_name := PWideChar(wGroupName);
  Details.lgrpi1_comment := PWideChar(wDescription);

  Err := RtdlNetLocalGroupAdd(PWideChar(wServer), 1, @Details, @ParmErr);
  Result := (Err = NERR_SUCCESS);
end;

function DeleteLocalGroup(const Server, Groupname: string): Boolean;
var
  wServername, wUsername: WideString;
  Err: NET_API_STATUS;
begin
  wServername := Server;
  wUsername := Groupname;
  Err := RtdlNetLocalGroupDel(PWideChar(wServername), PWideChar(wUsername));
  Result := (Err = NERR_SUCCESS);
end;

function GetLocalGroups(const Server: string; const Groups: TStrings): Boolean;
var
  Err: NET_API_STATUS;
  wServername: WideString;
  Buffer: PByte;
  Details: PLocalGroupInfo0;
  EntriesRead, TotalEntries: Cardinal;
  I: Integer;
begin
  wServername := Server;
  Err := RtdlNetLocalGroupEnum(PWideChar(wServername), 0, Buffer, MAX_PREFERRED_LENGTH,
    EntriesRead, TotalEntries, nil);

  if Err = NERR_SUCCESS then
  begin
    Details := PLocalGroupInfo0(Buffer);
    Groups.BeginUpdate;
    try
      for I := 0 to EntriesRead - 1 do
      begin
        Groups.Add(Details^.lgrpi0_name);
        Inc(Details);
      end;
    finally
      Groups.EndUpdate;
    end;
  end;

  RtdlNetApiBufferFree(Buffer);
  Result := (Err = NERR_SUCCESS);
end;

function GetGlobalGroups(const Server: string; const Groups: TStrings): Boolean;
var
  Err: NET_API_STATUS;
  wServername: WideString;
  Buffer: PByte;
  Details: PGroupInfo0;
  EntriesRead, TotalEntries: Cardinal;
  I: Integer;
begin
  wServername := Server;
  Err := RtdlNetGroupEnum(PWideChar(wServername), 0, Buffer, MAX_PREFERRED_LENGTH,
    EntriesRead, TotalEntries, nil);

  if Err = NERR_SUCCESS then
  begin
    Details := PGroupInfo0(Buffer);
    // (rom) is 'None' locale independent?
    if (EntriesRead <> 1) or (Details^.grpi0_name <> 'None') then
    begin
      Groups.BeginUpdate;
      try
        for I := 0 to EntriesRead - 1 do
        begin
          Groups.Add(Details^.grpi0_name);
          Inc(Details);
        end;
      finally
        Groups.EndUpdate;
      end;
    end;
  end
  else
    RaiseLastOSError;

  RtdlNetApiBufferFree(Buffer);
  Result := (Err = NERR_SUCCESS);
end;

function LocalGroupExists(const Group: string): Boolean;
var
  Groups: TStringList;
begin
  Groups := TStringList.Create;
  try
    GetLocalGroups('', Groups);
    Result := (Groups.IndexOf(Group) >= 0);
  finally
    Groups.Free;
  end;
end;

function GlobalGroupExists(const Server, Group: string): Boolean;
var
  Groups: TStringList;
begin
  Groups := TStringList.Create;
  try
    GetGlobalGroups(Server, Groups);
    Result := (Groups.IndexOf(Group) >= 0);
  finally
    Groups.Free;
  end;
end;

function DeleteGlobalGroup(const Server, Groupname: string): Boolean;
var
  wServername, wUsername: WideString;
  Err: NET_API_STATUS;
begin
  wServername := Server;
  wUsername := Groupname;
  Err := RtdlNetGroupDel(PWideChar(wServername), PWideChar(wUsername));
  Result := (Err = NERR_SUCCESS);
end;

function AddAccountToLocalGroup(const Accountname, Groupname: string): Boolean;
var
  Err: NET_API_STATUS;
  wAccountname, wGroupname: WideString;
  Details: LOCALGROUP_MEMBERS_INFO_3;
begin
  wGroupname := Groupname;
  wAccountname := AccountName;

  Details.lgrmi3_domainandname := PWideChar(wAccountname);
  Err := RtdlNetLocalGroupAddMembers(nil, PWideChar(wGroupname), 3, @Details, 1);
  Result := (Err = NERR_SUCCESS);
end;

function RIDToDWORD(const RID: TNetWellKnownRID): DWORD;
begin
  case RID of
    wkrAdmins:
      Result := DOMAIN_ALIAS_RID_ADMINS;
    wkrUsers:
      Result := DOMAIN_ALIAS_RID_USERS;
    wkrGuests:
      Result := DOMAIN_ALIAS_RID_GUESTS;
    wkrPowerUsers:
      Result := DOMAIN_ALIAS_RID_POWER_USERS;
    wkrBackupOPs:
      Result := DOMAIN_ALIAS_RID_BACKUP_OPS;
    wkrReplicator:
      Result := DOMAIN_ALIAS_RID_REPLICATOR;
  else // (wkrEveryone)
    Result := SECURITY_WORLD_RID;
  end;
end;

function DWORDToRID(const RID: DWORD): TNetWellKnownRID;
begin
  case RID of
    DOMAIN_ALIAS_RID_ADMINS:
      Result := wkrAdmins;
    DOMAIN_ALIAS_RID_USERS:
      Result := wkrUsers;
    DOMAIN_ALIAS_RID_GUESTS:
      Result := wkrGuests;
    DOMAIN_ALIAS_RID_POWER_USERS:
      Result := wkrPowerUsers;
    DOMAIN_ALIAS_RID_BACKUP_OPS:
      Result := wkrBackupOPs;
    DOMAIN_ALIAS_RID_REPLICATOR:
      Result := wkrReplicator;
  else // (SECURITY_WORLD_RID)
    Result := wkrEveryone;
  end;
end;

function LookupGroupName(const Server: string; const RID: TNetWellKnownRID): string;
var
  sia: Windows.SID_IDENTIFIER_AUTHORITY;
  rd1, rd2: DWORD;
  ridCount: Integer;
  sd: PSID;
  AccountNameLen, DomainNameLen: DWORD;
  SidNameUse: SID_NAME_USE;
begin
  Result := '';
  rd2 := 0;

  if RID = wkrEveryOne then
  begin
    sia := SECURITY_WORLD_SID_AUTHORITY;
    rd1 := RIDToDWORD(RID);
    ridCount := 1;
  end
  else
  begin
    sia := SECURITY_NT_AUTHORITY;
    rd1 := SECURITY_BUILTIN_DOMAIN_RID;
    rd2 := RIDToDWORD(RID);
    ridCount := 2;
  end;
  if AllocateAndInitializeSid(sia, ridCount, rd1, rd2, 0, 0, 0, 0, 0, 0, sd) then
  try
    AccountNameLen := 0;
    DomainNameLen := 0;
    if not LookupAccountSID(PChar(Server), sd, PChar(Result), AccountNameLen,
      nil, DomainNameLen, SidNameUse) then
      SetLength(Result, AccountNamelen);

    if LookupAccountSID(PChar(Server), sd, PChar(Result), AccountNameLen,
      nil, DomainNameLen, sidNameUse) then
      StrResetLength(Result)
    else
      RaiseLastOSError;
  finally
    FreeSID(sd);
  end;
end;

procedure ParseAccountName(const QualifiedName: string; var Domain, UserName: string);
var
  Parts: TStringList;
begin
  Parts := TStringList.Create;
  try
    StrTokenToStrings(QualifiedName, '\', Parts);
    if Parts.Count = 1 then
      UserName := Parts[0]
    else
    begin
      Domain := Parts[0];
      UserName := Parts[1];
    end;
  finally
    Parts.Free;
  end;
end;

function IsLocalAccount(const AccountName: string): Boolean;
var
  Domain: string;
  UserName: string;
  LocalServerName: string;
begin
  LocalServerName := GetLocalComputerName;
  ParseAccountName(AccountName, Domain, UserName);
  Result := (Domain = '') or (Domain = LocalServerName);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
