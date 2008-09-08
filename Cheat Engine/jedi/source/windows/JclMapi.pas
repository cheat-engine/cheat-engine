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
{ The Original Code is JclMapi.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) Petr Vones. All Rights Reserved.                }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel                                                                              }
{   Robert Marquardt (marquardt)                                                                   }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   Carsten Schuette (schuettecarsten)                                                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various classes and support routines for sending e-mail through Simple MAPI                      }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclMapi;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes, Contnrs, Mapi, SysUtils,
  JclBase;

type
  EJclMapiError = class(EJclError)
  private
    FErrorCode: DWORD;
  public
    property ErrorCode: DWORD read FErrorCode;
  end;

  // Simple MAPI interface
  TJclMapiClient = record
    ClientName: string;
    ClientPath: string;
    RegKeyName: string;
    Valid: Boolean;
  end;

  TJclMapiClientConnect = (ctAutomatic, ctMapi, ctDirect);

  TJclSimpleMapi = class(TObject)
  private
    FAnyClientInstalled: Boolean;
    FBeforeUnloadClient: TNotifyEvent;
    FClients: array of TJclMapiClient;
    FClientConnectKind: TJclMapiClientConnect;
    FClientLibHandle: THandle;
    FDefaultClientIndex: Integer;
    FDefaultProfileName: string;
    FFunctions: array[0..11] of ^Pointer;
    FMapiInstalled: Boolean;
    FMapiVersion: string;
    FProfiles: array of string;
    FSelectedClientIndex: Integer;
    FSimpleMapiInstalled: Boolean;
    { TODO : consider to move this to a internal single instance class with smart linking }
    FMapiAddress: TFNMapiAddress;
    FMapiDeleteMail: TFNMapiDeleteMail;
    FMapiDetails: TFNMapiDetails;
    FMapiFindNext: TFNMapiFindNext;
    FMapiFreeBuffer: TFNMapiFreeBuffer;
    FMapiLogOff: TFNMapiLogOff;
    FMapiLogOn: TFNMapiLogOn;
    FMapiReadMail: TFNMapiReadMail;
    FMapiResolveName: TFNMapiResolveName;
    FMapiSaveMail: TFNMapiSaveMail;
    FMapiSendDocuments: TFNMapiSendDocuments;
    FMapiSendMail: TFNMapiSendMail;
    function GetClientCount: Integer;
    function GetClients(Index: Integer): TJclMapiClient;
    function GetCurrentClientName: string;
    function GetProfileCount: Integer;
    function GetProfiles(Index: Integer): string;
    procedure SetSelectedClientIndex(const Value: Integer);
    procedure SetClientConnectKind(const Value: TJclMapiClientConnect);
    function UseMapi: Boolean;
  protected
    procedure BeforeUnloadClientLib; dynamic;
    procedure CheckListIndex(I, ArrayLength: Integer);
    function GetClientLibName: string;
    class function ProfilesRegKey: string;
    procedure ReadMapiSettings;
  public
    constructor Create;
    destructor Destroy; override;
    function ClientLibLoaded: Boolean;
    procedure LoadClientLib;
    procedure UnloadClientLib;
    property AnyClientInstalled: Boolean read FAnyClientInstalled;
    property ClientConnectKind: TJclMapiClientConnect read FClientConnectKind write SetClientConnectKind;
    property ClientCount: Integer read GetClientCount;
    property Clients[Index: Integer]: TJclMapiClient read GetClients; default;
    property CurrentClientName: string read GetCurrentClientName;
    property DefaultClientIndex: Integer read FDefaultClientIndex;
    property DefaultProfileName: string read FDefaultProfileName;
    property MapiInstalled: Boolean read FMapiInstalled;
    property MapiVersion: string read FMapiVersion;
    property ProfileCount: Integer read GetProfileCount;
    property Profiles[Index: Integer]: string read GetProfiles;
    property SelectedClientIndex: Integer read FSelectedClientIndex write SetSelectedClientIndex;
    property SimpleMapiInstalled: Boolean read FSimpleMapiInstalled;
    property BeforeUnloadClient: TNotifyEvent read FBeforeUnloadClient write FBeforeUnloadClient;
    // Simple MAPI functions
    property MapiAddress: TFNMapiAddress read FMapiAddress;
    property MapiDeleteMail: TFNMapiDeleteMail read FMapiDeleteMail;
    property MapiDetails: TFNMapiDetails read FMapiDetails;
    property MapiFindNext: TFNMapiFindNext read FMapiFindNext;
    property MapiFreeBuffer: TFNMapiFreeBuffer read FMapiFreeBuffer;
    property MapiLogOff: TFNMapiLogOff read FMapiLogOff;
    property MapiLogOn: TFNMapiLogOn read FMapiLogOn;
    property MapiReadMail: TFNMapiReadMail read FMapiReadMail;
    property MapiResolveName: TFNMapiResolveName read FMapiResolveName;
    property MapiSaveMail: TFNMapiSaveMail read FMapiSaveMail;
    property MapiSendDocuments: TFNMapiSendDocuments read FMapiSendDocuments;
    property MapiSendMail: TFNMapiSendMail read FMapiSendMail;
  end;

const
  // Simple email classes
  MapiAddressTypeSMTP = 'SMTP';
  MapiAddressTypeFAX  = 'FAX';
  MapiAddressTypeTLX  = 'TLX';

type
  TJclEmailRecipKind = (rkOriginator, rkTO, rkCC, rkBCC);

  TJclEmailRecip = class(TObject)
  private
    FAddress: string;
    FAddressType: string;
    FKind: TJclEmailRecipKind;
    FName: string;
  private
    procedure SetAddress(Value: string);
  protected
    function SortingName: string;
  public
    function AddressAndName: string;
    class function RecipKindToString(const AKind: TJclEmailRecipKind): string;
    property AddressType: string read FAddressType write FAddressType;
    property Address: string read FAddress write SetAddress;
    property Kind: TJclEmailRecipKind read FKind write FKind;
    property Name: string read FName write FName;
  end;

  TJclEmailRecips = class(TObjectList)
  private
    FAddressesType: string;
    function GetItems(Index: Integer): TJclEmailRecip;
    function GetOriginator: TJclEmailRecip;
  public
    function Add(const Address: string;
      const Name: string = '';
      const Kind: TJclEmailRecipKind = rkTO;
      const AddressType: string = ''): Integer;
    procedure SortRecips;
    property AddressesType: string read FAddressesType write FAddressesType;
    property Items[Index: Integer]: TJclEmailRecip read GetItems; default;
    property Originator: TJclEmailRecip read GetOriginator;
  end;

  TJclEmailFindOption = (foFifo, foUnreadOnly);
  TJclEmailLogonOption = (loLogonUI, loNewSession, loForceDownload);
  TJclEmailReadOption = (roAttachments, roHeaderOnly, roMarkAsRead);

  TJclEmailFindOptions = set of TJclEmailFindOption;
  TJclEmailLogonOptions = set of TJclEmailLogonOption;
  TJclEmailReadOptions = set of TJclEmailReadOption;

  TJclEmailReadMsg = record
    ConversationID: string;
    DateReceived: TDateTime;
    MessageType: string;
    Flags: FLAGS;
  end;

  TJclTaskWindowsList = array of THandle;

  TJclEmail = class(TJclSimpleMapi)
  private
    FAttachments: TStringList;
    FBody: string;
    FFindOptions: TJclEmailFindOptions;
    FHtmlBody: Boolean;
    FLogonOptions: TJclEmailLogonOptions;
    FParentWnd: THandle;
    FParentWndValid: Boolean;
    FReadMsg: TJclEmailReadMsg;
    FRecipients: TJclEmailRecips;
    FSeedMessageID: string;
    FSessionHandle: THandle;
    FSubject: string;
    FTaskWindowList: TJclTaskWindowsList;
    FAttachmentFiles: TStringList;
    function GetAttachments: TStrings;
    function GetAttachmentFiles: TStrings;
    function GetParentWnd: THandle;
    function GetUserLogged: Boolean;
    procedure SetBody(const Value: string);
    procedure SetParentWnd(const Value: THandle);
  protected
    procedure BeforeUnloadClientLib; override;
    procedure DecodeRecips(RecipDesc: PMapiRecipDesc; Count: Integer);
    function InternalSendOrSave(Save: Boolean; ShowDialog: Boolean): Boolean;
    function LogonOptionsToFlags(ShowDialog: Boolean): DWORD;
  public
    constructor Create;
    destructor Destroy; override;
    function Address(const Caption: string = ''; EditFields: Integer = 3): Boolean;
    procedure Clear;
    function Delete(const MessageID: string): Boolean;
    function FindFirstMessage: Boolean;
    function FindNextMessage: Boolean;
    procedure LogOff;
    procedure LogOn(const ProfileName: string = ''; const Password: string = '');
    function MessageReport(Strings: TStrings; MaxWidth: Integer = 80; IncludeAddresses: Boolean = False): Integer;
    function Read(const Options: TJclEmailReadOptions = []): Boolean;
    function ResolveName(var Name, Address: string; ShowDialog: Boolean = False): Boolean;
    procedure RestoreTaskWindows;
    function Save: Boolean;
    procedure SaveTaskWindows;
    function Send(ShowDialog: Boolean = True): Boolean;
    procedure SortAttachments;
    property Attachments: TStrings read GetAttachments;
    property AttachmentFiles: TStrings read GetAttachmentFiles;
    property Body: string read FBody write SetBody;
    property FindOptions: TJclEmailFindOptions read FFindOptions write FFindOptions;
    property HtmlBody: Boolean read FHtmlBody write FHtmlBody;
    property LogonOptions: TJclEmailLogonOptions read FLogonOptions write FLogonOptions;
    property ParentWnd: THandle read GetParentWnd write SetParentWnd;
    property ReadMsg: TJclEmailReadMsg read FReadMsg;
    property Recipients: TJclEmailRecips read FRecipients;
    property SeedMessageID: string read FSeedMessageID write FSeedMessageID;
    property SessionHandle: THandle read FSessionHandle;
    property Subject: string read FSubject write FSubject;
    property UserLogged: Boolean read GetUserLogged;
  end;

// Simple email send function
function JclSimpleSendMail(const Recipient, Name, Subject, Body: string;
  const Attachment: string = ''; ShowDialog: Boolean = True; ParentWND: THandle = 0;
  const ProfileName: string = ''; const Password: string = ''): Boolean;

function JclSimpleSendFax(const Recipient, Name, Subject, Body: string;
  const Attachment: string = ''; ShowDialog: Boolean = True; ParentWND: THandle = 0;
  const ProfileName: string = ''; const Password: string = ''): Boolean;

function JclSimpleBringUpSendMailDialog(const Subject, Body: string;
  const Attachment: string = ''; ParentWND: THandle = 0;
  const ProfileName: string = ''; const Password: string = ''): Boolean;

// MAPI Errors
function MapiCheck(const Res: DWORD; IgnoreUserAbort: Boolean = True): DWORD;

function MapiErrorMessage(const ErrorCode: DWORD): string;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclMapi.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclFileUtils, JclLogic, JclRegistry, JclResources, JclStrings, JclSysInfo, JclSysUtils;

const
  MapiDll = 'mapi32.dll';
  MapiExportNames: array [0..11] of PChar = (
    'MAPIAddress',
    'MAPIDeleteMail',
    'MAPIDetails',
    'MAPIFindNext',
    'MAPIFreeBuffer',
    'MAPILogoff',
    'MAPILogon',
    'MAPIReadMail',
    'MAPIResolveName',
    'MAPISaveMail',
    'MAPISendDocuments',
    'MAPISendMail');
  AddressTypeDelimiter = ':';

//=== MAPI Errors check ======================================================

function MapiCheck(const Res: DWORD; IgnoreUserAbort: Boolean): DWORD;
var
  Error: EJclMapiError;
begin
  if (Res = SUCCESS_SUCCESS) or (IgnoreUserAbort and (Res = MAPI_E_USER_ABORT)) then
    Result := Res
  else
  begin
    Error := EJclMapiError.CreateResFmt(@RsMapiError, [Res, MapiErrorMessage(Res)]);
    Error.FErrorCode := Res;
    raise Error;
  end;
end;

function MapiErrorMessage(const ErrorCode: DWORD): string;
begin
   case ErrorCode of
     MAPI_E_USER_ABORT:
       Result := RsMapiErrUSER_ABORT;
     MAPI_E_FAILURE:
       Result := RsMapiErrFAILURE;
     MAPI_E_LOGIN_FAILURE:
       Result := RsMapiErrLOGIN_FAILURE;
     MAPI_E_DISK_FULL:
       Result := RsMapiErrDISK_FULL;
     MAPI_E_INSUFFICIENT_MEMORY:
       Result := RsMapiErrINSUFFICIENT_MEMORY;
     MAPI_E_ACCESS_DENIED:
       Result := RsMapiErrACCESS_DENIED;
     MAPI_E_TOO_MANY_SESSIONS:
       Result := RsMapiErrTOO_MANY_SESSIONS;
     MAPI_E_TOO_MANY_FILES:
       Result := RsMapiErrTOO_MANY_FILES;
     MAPI_E_TOO_MANY_RECIPIENTS:
       Result := RsMapiErrTOO_MANY_RECIPIENTS;
     MAPI_E_ATTACHMENT_NOT_FOUND:
       Result := RsMapiErrATTACHMENT_NOT_FOUND;
     MAPI_E_ATTACHMENT_OPEN_FAILURE:
       Result := RsMapiErrATTACHMENT_OPEN_FAILURE;
     MAPI_E_ATTACHMENT_WRITE_FAILURE:
       Result := RsMapiErrATTACHMENT_WRITE_FAILURE;
     MAPI_E_UNKNOWN_RECIPIENT:
       Result := RsMapiErrUNKNOWN_RECIPIENT;
     MAPI_E_BAD_RECIPTYPE:
       Result := RsMapiErrBAD_RECIPTYPE;
     MAPI_E_NO_MESSAGES:
       Result := RsMapiErrNO_MESSAGES;
     MAPI_E_INVALID_MESSAGE:
       Result := RsMapiErrINVALID_MESSAGE;
     MAPI_E_TEXT_TOO_LARGE:
       Result := RsMapiErrTEXT_TOO_LARGE;
     MAPI_E_INVALID_SESSION:
       Result := RsMapiErrINVALID_SESSION;
     MAPI_E_TYPE_NOT_SUPPORTED:
       Result := RsMapiErrTYPE_NOT_SUPPORTED;
     MAPI_E_AMBIGUOUS_RECIPIENT:
       Result := RsMapiErrAMBIGUOUS_RECIPIENT;
     MAPI_E_MESSAGE_IN_USE:
       Result := RsMapiErrMESSAGE_IN_USE;
     MAPI_E_NETWORK_FAILURE:
       Result := RsMapiErrNETWORK_FAILURE;
     MAPI_E_INVALID_EDITFIELDS:
       Result := RsMapiErrINVALID_EDITFIELDS;
     MAPI_E_INVALID_RECIPS:
       Result := RsMapiErrINVALID_RECIPS;
     MAPI_E_NOT_SUPPORTED:
       Result := RsMapiErrNOT_SUPPORTED;
   else
     Result := '';
   end;
end;

procedure RestoreTaskWindowsList(const List: TJclTaskWindowsList);
var
  I: Integer;

  function RestoreTaskWnds(Wnd: THandle; List: TJclTaskWindowsList): BOOL; stdcall;
  var
    I: Integer;
    EnableIt: Boolean;
  begin
    if IsWindowVisible(Wnd) then
    begin
      EnableIt := False;
      for I := 1 to Length(List) - 1 do
        if List[I] = Wnd then
        begin
          EnableIt := True;
          Break;
        end;
      EnableWindow(Wnd, EnableIt);
    end;
    Result := True;
  end;

begin
  if Length(List) > 0 then
  begin
    EnumThreadWindows(MainThreadID, @RestoreTaskWnds, Integer(List));
    for I := 0 to Length(List) - 1 do
      EnableWindow(List[I], True);
    SetFocus(List[0]);
  end;
end;

function SaveTaskWindowsList: TJclTaskWindowsList;

  function SaveTaskWnds(Wnd: THandle; var Data: TJclTaskWindowsList): BOOL; stdcall;
  var
    C: Integer;
  begin
    if IsWindowVisible(Wnd) and IsWindowEnabled(Wnd) then
    begin
      C := Length(Data);
      SetLength(Data, C + 1);
      Data[C] := Wnd;
    end;
    Result := True;
  end;

begin
  SetLength(Result, 1);
  Result[0] := GetFocus;
  EnumThreadWindows(MainThreadID, @SaveTaskWnds, Integer(@Result));
end;

//=== { TJclSimpleMapi } =====================================================

constructor TJclSimpleMapi.Create;
begin
  inherited Create;
  FFunctions[0] := @@FMapiAddress;
  FFunctions[1] := @@FMapiDeleteMail;
  FFunctions[2] := @@FMapiDetails;
  FFunctions[3] := @@FMapiFindNext;
  FFunctions[4] := @@FMapiFreeBuffer;
  FFunctions[5] := @@FMapiLogOff;
  FFunctions[6] := @@FMapiLogOn;
  FFunctions[7] := @@FMapiReadMail;
  FFunctions[8] := @@FMapiResolveName;
  FFunctions[9] := @@FMapiSaveMail;
  FFunctions[10] := @@FMapiSendDocuments;
  FFunctions[11] := @@FMapiSendMail;
  FDefaultClientIndex := -1;
  FClientConnectKind := ctAutomatic;
  FSelectedClientIndex := -1;
  ReadMapiSettings;
end;

destructor TJclSimpleMapi.Destroy;
begin
  UnloadClientLib;
  inherited Destroy;
end;

procedure TJclSimpleMapi.BeforeUnloadClientLib;
begin
  if Assigned(FBeforeUnloadClient) then
    FBeforeUnloadClient(Self);
end;

procedure TJclSimpleMapi.CheckListIndex(I, ArrayLength: Integer);
begin
  if (I < 0) or (I >= ArrayLength) then
    raise EJclMapiError.CreateResFmt(@RsMapiInvalidIndex, [I]);
end;

function TJclSimpleMapi.ClientLibLoaded: Boolean;
begin
  Result := FClientLibHandle <> 0;
end;

function TJclSimpleMapi.GetClientCount: Integer;
begin
  Result := Length(FClients);
end;

function TJclSimpleMapi.GetClientLibName: string;
begin
  if UseMapi then
    Result := MapiDll
  else
    Result := FClients[FSelectedClientIndex].ClientPath;
end;

function TJclSimpleMapi.GetClients(Index: Integer): TJclMapiClient;
begin
  CheckListIndex(Index, ClientCount);
  Result := FClients[Index];
end;

function TJclSimpleMapi.GetCurrentClientName: string;
begin
  if UseMapi then
    Result := 'MAPI'
  else
  if ClientCount > 0 then
    Result := Clients[SelectedClientIndex].ClientName
  else
    Result := '';
end;

function TJclSimpleMapi.GetProfileCount: Integer;
begin
  Result := Length(FProfiles);
end;

function TJclSimpleMapi.GetProfiles(Index: Integer): string;
begin
  CheckListIndex(Index, ProfileCount);
  Result := FProfiles[Index];
end;

procedure TJclSimpleMapi.LoadClientLib;
var
  I: Integer;
  P: Pointer;
begin
  if ClientLibLoaded then
    Exit;
  FClientLibHandle := SafeLoadLibrary(GetClientLibName);
  if FClientLibHandle = 0 then
    RaiseLastOSError;
  for I := 0 to Length(FFunctions) - 1 do
  begin
    P := GetProcAddress(FClientLibHandle, PChar(MapiExportNames[I]));
    if P = nil then
    begin
      UnloadClientLib;
      raise EJclMapiError.CreateResFmt(@RsMapiMissingExport, [MapiExportNames[I]]);
    end
    else
      FFunctions[I]^ := P;
  end;
end;

class function TJclSimpleMapi.ProfilesRegKey: string;
begin
  if IsWinNT then
    Result := 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Windows Messaging Subsystem\Profiles'
  else
    Result := 'SOFTWARE\Microsoft\Windows Messaging Subsystem\Profiles';
end;

procedure TJclSimpleMapi.ReadMapiSettings;
const
  MessageSubsytemKey = 'SOFTWARE\Microsoft\Windows Messaging Subsystem';
  MailClientsKey = 'SOFTWARE\Clients\Mail';
var
  DefaultValue, ClientKey: string;
  SL: TStringList;
  I: Integer;

  function CheckValid(var Client: TJclMapiClient): Boolean;
  var
    I: Integer;
    LibHandle: THandle;
  begin
    LibHandle := LoadLibraryEx(PChar(Client.ClientPath), 0, DONT_RESOLVE_DLL_REFERENCES);
    Result := (LibHandle <> 0);
    if Result then
    begin
       for I := Low(MapiExportNames) to High(MapiExportNames) do
        if GetProcAddress(LibHandle, PChar(MapiExportNames[I])) = nil then
        begin
          Result := False;
          Break;
        end;
      FreeLibrary(LibHandle);
    end;
    Client.Valid := Result;
  end;

begin
  FClients := nil;
  FDefaultClientIndex := -1;
  FProfiles := nil;
  FDefaultProfileName := '';
  SL := TStringList.Create;
  try
    if RegKeyExists(HKEY_LOCAL_MACHINE, MessageSubsytemKey) then
    begin
      FMapiInstalled := RegReadStringDef(HKEY_LOCAL_MACHINE, MessageSubsytemKey, 'MAPIX', '') = '1';
      FSimpleMapiInstalled := RegReadStringDef(HKEY_LOCAL_MACHINE, MessageSubsytemKey, 'MAPI', '') = '1';
      FMapiVersion := RegReadStringDef(HKEY_LOCAL_MACHINE, MessageSubsytemKey, 'MAPIXVER', '');
    end;
    FAnyClientInstalled := FMapiInstalled;
    if RegKeyExists(HKEY_LOCAL_MACHINE, MailClientsKey) then
    begin
      DefaultValue := RegReadStringDef(HKEY_LOCAL_MACHINE, MailClientsKey, '', '');
      if RegGetKeyNames(HKEY_LOCAL_MACHINE, MailClientsKey, SL) then
      begin
        SetLength(FClients, SL.Count);
        for I := 0 to SL.Count - 1 do
        begin
          FClients[I].RegKeyName := SL[I];
          FClients[I].Valid := False;
          ClientKey := MailClientsKey + '\' + SL[I];
          if RegKeyExists(HKEY_LOCAL_MACHINE, ClientKey) then
          begin
            FClients[I].ClientName := RegReadStringDef(HKEY_LOCAL_MACHINE, ClientKey, '', '');
            FClients[I].ClientPath := RegReadStringDef(HKEY_LOCAL_MACHINE, ClientKey, 'DLLPathEx', '');
            if FClients[I].ClientPath = '' then
              FClients[I].ClientPath := RegReadStringDef(HKEY_LOCAL_MACHINE, ClientKey, 'DLLPath', '');
            ExpandEnvironmentVar(FClients[I].ClientPath);
            if CheckValid(FClients[I]) then
              FAnyClientInstalled := True;
          end;
        end;
        FDefaultClientIndex := SL.IndexOf(DefaultValue);
        FSelectedClientIndex := FDefaultClientIndex;
      end;
    end;
    if RegKeyExists(HKEY_CURRENT_USER, ProfilesRegKey) then
    begin
      FDefaultProfileName := RegReadStringDef(HKEY_CURRENT_USER, ProfilesRegKey, 'DefaultProfile', '');
      if RegGetKeyNames(HKEY_CURRENT_USER, ProfilesRegKey, SL) then
      begin
        SetLength(FProfiles, SL.Count);
        for I := 0 to SL.Count - 1 do
          FProfiles[I] := SL[I];
      end;
    end;
  finally
    SL.Free;
  end;
end;

procedure TJclSimpleMapi.SetClientConnectKind(const Value: TJclMapiClientConnect);
begin
  if FClientConnectKind <> Value then
  begin
    FClientConnectKind := Value;
    UnloadClientLib;
  end;
end;

procedure TJclSimpleMapi.SetSelectedClientIndex(const Value: Integer);
begin
  CheckListIndex(Value, ClientCount);
  if FSelectedClientIndex <> Value then
  begin
    FSelectedClientIndex := Value;
    UnloadClientLib;
  end;
end;

procedure TJclSimpleMapi.UnloadClientLib;
var
  I: Integer;
begin
  if ClientLibLoaded then
  begin
    BeforeUnloadClientLib;
    FreeLibrary(FClientLibHandle);
    FClientLibHandle := 0;
     for I := 0 to Length(FFunctions) - 1 do
      FFunctions[I]^ := nil;
  end;
end;

function TJclSimpleMapi.UseMapi: Boolean;
begin
  case FClientConnectKind of
    ctAutomatic:
      UseMapi := FSimpleMapiInstalled;
    ctMapi:
      UseMapi := True;
    ctDirect:
      UseMapi := False;
  else
    UseMapi := True;
  end;
end;

//=== { TJclEmailRecip } =====================================================

function TJclEmailRecip.AddressAndName: string;
var
  N: string;
begin
  if Name = '' then
    N := Address
  else
    N := Name;
  Result := Format('"%s" <%s>', [N, Address]);
end;

class function TJclEmailRecip.RecipKindToString(const AKind: TJclEmailRecipKind): string;
const
  Idents: array [TJclEmailRecipKind] of string = (
    RsMapiMailORIG, RsMapiMailTO, RsMapiMailCC, RsMapiMailBCC);
begin
  case AKind of
     rkOriginator:
       Result := RsMapiMailORIG;
     rkTO:
       Result := RsMapiMailTO;
     rkCC:
       Result := RsMapiMailCC;
     rkBCC:
       Result := RsMapiMailBCC;
   end;
end;

procedure TJclEmailRecip.SetAddress(Value: string);
var
  N: Integer;
begin
  Value := Trim(Value);
  N := Pos(AddressTypeDelimiter, Value);
  if N = 0 then
    FAddress := Value
  else
  begin
    FAddress := Copy(Value, N + 1, Length(Value));
    FAddressType := Copy(Value, 1, N - 1);
  end;
end;

function TJclEmailRecip.SortingName: string;
begin
  if FName = '' then
    Result := FAddress
  else
    Result := FName;
end;

//=== { TJclEmailRecips } ====================================================

function TJclEmailRecips.Add(const Address, Name: string;
  const Kind: TJclEmailRecipKind; const AddressType: string): Integer;
var
  Item: TJclEmailRecip;
begin
  Item := TJclEmailRecip.Create;
  try
    Item.Address := Address;
    if AddressType <> '' then
      Item.AddressType := AddressType;
    Item.Name := Name;
    Item.Kind := Kind;
    Result := inherited Add(Item);
  except
    Item.Free;
    raise;
  end;
end;

function TJclEmailRecips.GetItems(Index: Integer): TJclEmailRecip;
begin
  Result := TJclEmailRecip(Get(Index));
end;

function TJclEmailRecips.GetOriginator: TJclEmailRecip;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Kind = rkOriginator then
    begin
      Result := Items[I];
      Break;
    end;
end;

function EmailRecipsCompare(Item1, Item2: Pointer): Integer;
var
  R1, R2: TJclEmailRecip;
begin
  R1 := TJclEmailRecip(Item1);
  R2 := TJclEmailRecip(Item2);
  Result := Integer(R1.Kind) - Integer(R2.Kind);
  if Result = 0 then
    Result := AnsiCompareStr(R1.SortingName, R2.SortingName);
end;

procedure TJclEmailRecips.SortRecips;
begin
  Sort(EmailRecipsCompare);
end;

//=== { TJclEmail } ==========================================================

constructor TJclEmail.Create;
begin
  inherited Create;
  FAttachments := TStringList.Create;
  FAttachmentFiles := TStringList.Create;
  FLogonOptions := [loLogonUI];
  FFindOptions := [foFifo];
  FRecipients := TJclEmailRecips.Create(True);
  FRecipients.AddressesType := MapiAddressTypeSMTP;
end;

destructor TJclEmail.Destroy;
begin
  FreeAndNil(FAttachmentFiles);
  FreeAndNil(FAttachments);
  FreeAndNil(FRecipients);
  inherited Destroy;
end;

function TJclEmail.Address(const Caption: string; EditFields: Integer): Boolean;
var
  NewRecipCount: ULONG;
  NewRecips: PMapiRecipDesc;
  Recips: TMapiRecipDesc;
  Res: DWORD;
begin
  LoadClientLib;
  NewRecips := nil;
  NewRecipCount := 0;
  Res := MapiAddress(FSessionHandle, ParentWnd, PChar(Caption), EditFields, nil,
    0, Recips, LogonOptionsToFlags(False), 0, @NewRecipCount, NewRecips);
  Result := (MapiCheck(Res, True) = SUCCESS_SUCCESS);
  if Result then
  try
    DecodeRecips(NewRecips, NewRecipCount);
  finally
    MapiFreeBuffer(NewRecips);
  end;
end;

procedure TJclEmail.BeforeUnloadClientLib;
begin
  LogOff;
  inherited BeforeUnloadClientLib;
end;

procedure TJclEmail.Clear;
begin
  Attachments.Clear;
  AttachmentFiles.Clear;
  Body := '';
  FSubject := '';
  Recipients.Clear;
  FReadMsg.MessageType := '';
  FReadMsg.DateReceived := 0;
  FReadMsg.ConversationID := '';
  FReadMsg.Flags := 0;
end;

procedure TJclEmail.DecodeRecips(RecipDesc: PMapiRecipDesc; Count: Integer);
var
  S: string;
  N, I: Integer;
  Kind: TJclEmailRecipKind;
begin
  for I := 0 to Count - 1 do
  begin
    if RecipDesc = nil then
      Break;
    Kind := rkOriginator;
    with RecipDesc^ do
    begin
      case ulRecipClass of
         MAPI_ORIG:
           Kind := rkOriginator;
         MAPI_TO:
           Kind := rkTO;
         MAPI_CC:
           Kind := rkCC;
         MAPI_BCC:
           Kind := rkBCC;
        $FFFFFFFF:  // Eudora client version 5.2.0.9 bug
          Kind := rkOriginator;
      else
        MapiCheck(MAPI_E_INVALID_MESSAGE, True);
      end;
      S := lpszAddress;
      N := Pos(AddressTypeDelimiter, S);
      if N = 0 then
        Recipients.Add(S, lpszName, Kind)
      else
        Recipients.Add(Copy(S, N + 1, Length(S)), lpszName, Kind, Copy(S, 1, N - 1));
    end;
    Inc(RecipDesc);
  end;
end;

function TJclEmail.Delete(const MessageID: string): Boolean;
begin
  LoadClientLib;
  Result := MapiCheck(MapiDeleteMail(FSessionHandle, 0, PChar(MessageID), 0, 0),
    False) = SUCCESS_SUCCESS;
end;

function TJclEmail.FindFirstMessage: Boolean;
begin
  SeedMessageID := '';
  Result := FindNextMessage;
end;

function TJclEmail.FindNextMessage: Boolean;
var
  MsgID: array [0..512] of AnsiChar;
  Flags, Res: ULONG;
begin
  Result := False;
  if not UserLogged then
    Exit;
  Flags := MAPI_LONG_MSGID;
  if foFifo in FFindOptions then
    Inc(Flags, MAPI_GUARANTEE_FIFO);
  if foUnreadOnly in FFindOptions then
    Inc(Flags, MAPI_UNREAD_ONLY);
  Res := MapiFindNext(FSessionHandle, 0, nil, PChar(FSeedMessageID), Flags, 0, MsgId);
  Result := (Res = SUCCESS_SUCCESS);
  if Result then
    SeedMessageID := MsgID
  else
  begin
    SeedMessageID := '';
    if Res <> MAPI_E_NO_MESSAGES then
      MapiCheck(Res, True);
  end;
end;

function TJclEmail.GetAttachments: TStrings;
begin
  Result := FAttachments;
end;

function TJclEmail.GetAttachmentFiles: TStrings;
begin
  Result := FAttachmentFiles;
end;

function TJclEmail.GetParentWnd: THandle;
begin
  if FParentWndValid then
    Result := FParentWnd
  else
    Result := GetMainAppWndFromPid(GetCurrentProcessId);
end;

function TJclEmail.GetUserLogged: Boolean;
begin
  Result := (FSessionHandle <> 0);
end;

function TJclEmail.InternalSendOrSave(Save, ShowDialog: Boolean): Boolean;
const
  RecipClasses: array [TJclEmailRecipKind] of DWORD =
    (MAPI_ORIG, MAPI_TO, MAPI_CC, MAPI_BCC);
type
  TSetDllDirectory = function(lpPathName: PAnsiChar): LONGBOOL; stdcall;
  TGetDllDirectory = function(nBufferLength: DWord; lpPathName: PAnsiChar): LONGBOOL; stdcall;
var
  AttachArray: packed array of TMapiFileDesc;
  RecipArray: packed array of TMapiRecipDesc;
  RealAddresses: array of string;
  RealNames: array of string;
  MapiMessage: TMapiMessage;
  Flags, Res: DWORD;
  I: Integer;
  MsgID: array [0..512] of AnsiChar;
  AttachmentFileNames: array of string;
  AttachmentPathNames: array of string;
  HtmlBodyFileName: string;
  SetDllDirectory: TSetDllDirectory;
  GetDllDirectory: TGetDllDirectory;
  DllDirectoryBuffer: array[0..1024] of Char;
begin
  if not AnyClientInstalled then
    raise EJclMapiError.CreateRes(@RsMapiMailNoClient);

  @GetDllDirectory := GetProcAddress(GetModuleHandle(kernel32), 'GetDllDirectoryA');
  @SetDllDirectory := GetProcAddress(GetModuleHandle(kernel32), 'SetDllDirectoryA');
  if Assigned(@GetDllDirectory) and Assigned(@SetDllDirectory) then
  begin
    GetDllDirectory(SizeOf(DllDirectoryBuffer), @DllDirectoryBuffer);
    SetDllDirectory(nil);
  end;
  try
    HtmlBodyFileName := '';
    try
      if FHtmlBody then
      begin
        HtmlBodyFileName := FindUnusedFileName(PathAddSeparator(GetWindowsTempFolder) + 'JclMapi', 'htm', 'Temp');
        Attachments.Insert(0, HtmlBodyFileName);
        AttachmentFiles.Insert(0, '');
        StringToFile(HtmlBodyFileName, Body);
      end;
      // Create attachments
      if Attachments.Count > 0 then
      begin
        SetLength(AttachArray, Attachments.Count);
        SetLength(AttachmentFileNames, Attachments.Count);
        SetLength(AttachmentPathNames, Attachments.Count);
        for I := 0 to Attachments.Count - 1 do
        begin
          FillChar(AttachArray[I], SizeOf(TMapiFileDesc), #0);
          AttachArray[I].nPosition := DWORD(-1);
          if (AttachmentFiles.Count > I) and (AttachmentFiles[I] <> '') then
          begin
            AttachmentFileNames[I] := Attachments[I];
            AttachmentPathNames[I] := ExpandFileName(AttachmentFiles[I]);
          end
          else
          begin
            AttachmentFileNames[I] := ExtractFileName(Attachments[I]);
            AttachmentPathNames[I] := ExpandFileName(Attachments[I]);
          end;
          AttachArray[I].lpszFileName := PAnsiChar(AttachmentFileNames[I]);
          AttachArray[I].lpszPathName := PAnsiChar(AttachmentPathNames[I]);
          if not FileExists(AttachmentPathNames[I]) then
            MapiCheck(MAPI_E_ATTACHMENT_NOT_FOUND, False);
        end;
      end
      else
        AttachArray := nil;
      // Create recipients
      if Recipients.Count > 0 then
      begin
        SetLength(RecipArray, Recipients.Count);
        SetLength(RealAddresses, Recipients.Count);
        SetLength(RealNames, Recipients.Count);
        for I := 0 to Recipients.Count - 1 do
        begin
          FillChar(RecipArray[I], SizeOf(TMapiRecipDesc), #0);
          with RecipArray[I], Recipients[I] do
          begin
            ulRecipClass := RecipClasses[Kind];
            if FName = '' then // some clients requires Name item always filled
            begin
              if FAddress = '' then
                MapiCheck(MAPI_E_INVALID_RECIPS, False);
              RealNames[I] := FAddress;
            end
            else
              RealNames[I] := FName;
            if FAddressType <> '' then
              RealAddresses[I] := FAddressType + AddressTypeDelimiter + FAddress
            else
              if Recipients.AddressesType <> '' then
                RealAddresses[I] := Recipients.AddressesType + AddressTypeDelimiter + FAddress
              else
                RealAddresses[I] := FAddress;
            lpszName := PAnsiChar(RealNames[I]);
            lpszAddress := PAnsiChar(RealAddresses[I]);
          end;
        end;
      end
      else
      begin
        if ShowDialog then
          RecipArray := nil
        else
          MapiCheck(MAPI_E_INVALID_RECIPS, False);
      end;
      // Load MAPI client library
      LoadClientLib;
      // Fill MapiMessage structure
      FillChar(MapiMessage, SizeOf(MapiMessage), #0);
      MapiMessage.lpszSubject := PChar(FSubject);
      if FHtmlBody then
        MapiMessage.lpszNoteText := #0
      else
        MapiMessage.lpszNoteText := PChar(FBody);
      MapiMessage.nRecipCount := Length(RecipArray);
      if MapiMessage.nRecipCount > 0 then
        MapiMessage.lpRecips := PMapiRecipDesc(@RecipArray[0]);
      MapiMessage.nFileCount := Length(AttachArray);
      if MapiMessage.nFileCount > 0 then
        MapiMessage.lpFiles := PMapiFileDesc(@AttachArray[0]);
      Flags := LogonOptionsToFlags(ShowDialog);
      if Save then
      begin
        StrPLCopy(MsgID, SeedMessageID, SizeOf(MsgID));
        Res := MapiSaveMail(FSessionHandle, ParentWND, MapiMessage, Flags, 0, MsgID);
        if Res = SUCCESS_SUCCESS then
          SeedMessageID := MsgID;
      end
      else
        Res := MapiSendMail(FSessionHandle, ParentWND, MapiMessage, Flags, 0);
      Result := (MapiCheck(Res, True) = SUCCESS_SUCCESS);
    finally
      SetLength(AttachArray, 0);
      SetLength(RecipArray, 0);
      SetLength(RealAddresses, 0);
      SetLength(RealNames, 0);
      SetLength(AttachmentFileNames, 0);
      SetLength(AttachmentPathNames, 0);
      if HtmlBodyFileName <> '' then
      begin
        DeleteFile(HtmlBodyFileName);
        Attachments.Delete(0);
        AttachmentFiles.Delete(0);
      end;
    end;
  finally
    if Assigned(@SetDllDirectory) then
      SetDllDirectory(DllDirectoryBuffer);
  end;
end;

procedure TJclEmail.LogOff;
begin
  if UserLogged then
  begin
    MapiCheck(MapiLogOff(FSessionHandle, ParentWND, 0, 0), True);
    FSessionHandle := 0;
  end;
end;

procedure TJclEmail.LogOn(const ProfileName, Password: string);
begin
  if not UserLogged then
  begin
    LoadClientLib;
    MapiCheck(MapiLogOn(ParentWND, PChar(ProfileName), PChar(Password),
      LogonOptionsToFlags(False), 0, @FSessionHandle), True);
  end;
end;

function TJclEmail.LogonOptionsToFlags(ShowDialog: Boolean): DWORD;
begin
  Result := 0;
  if FSessionHandle = 0 then
  begin
    if loLogonUI in FLogonOptions then
      Inc(Result, MAPI_LOGON_UI);
    if loNewSession in FLogonOptions then
      Inc(Result, MAPI_NEW_SESSION);
    if loForceDownload in FLogonOptions then
      Inc(Result, MAPI_FORCE_DOWNLOAD);
  end;
  if ShowDialog then
    Inc(Result, MAPI_DIALOG);
end;

function TJclEmail.MessageReport(Strings: TStrings; MaxWidth: Integer; IncludeAddresses: Boolean): Integer;
const
  NameDelimiter = ', ';
var
  LabelsWidth: Integer;
  NamesList: array [TJclEmailRecipKind] of string;
  ReportKind: TJclEmailRecipKind;
  I, Cnt: Integer;
  BreakStr, S: string;
begin
  Cnt := Strings.Count;
  LabelsWidth := Length(RsMapiMailSubject);
  for ReportKind := Low(ReportKind) to High(ReportKind) do
  begin
    NamesList[ReportKind] := '';
    LabelsWidth := Max(LabelsWidth, Length(TJclEmailRecip.RecipKindToString(ReportKind)));
  end;
  BreakStr := AnsiCrLf + StringOfChar(' ', LabelsWidth + 2);
  for I := 0 to Recipients.Count - 1 do
    with Recipients[I] do
    begin
      if IncludeAddresses then
        S := AddressAndName
      else
        S := Name;
      NamesList[Kind] := NamesList[Kind] + S + NameDelimiter;
    end;

  Strings.BeginUpdate;
  try
    for ReportKind := Low(ReportKind) to High(ReportKind) do
      if NamesList[ReportKind] <> '' then
      begin
        S := StrPadRight(TJclEmailRecip.RecipKindToString(ReportKind), LabelsWidth, AnsiSpace) + ': ' +
          Copy(NamesList[ReportKind], 1, Length(NamesList[ReportKind]) - Length(NameDelimiter));
        Strings.Add(WrapText(S, BreakStr, [AnsiTab, AnsiSpace], MaxWidth));
      end;
    S := RsMapiMailSubject + ': ' + Subject;
    Strings.Add(WrapText(S, BreakStr, [AnsiTab, AnsiSpace], MaxWidth));
    Result := Strings.Count - Cnt;
    Strings.Add('');
    Strings.Add(WrapText(Body, AnsiCrLf, [AnsiTab, AnsiSpace, '-'], MaxWidth));
  finally
    Strings.EndUpdate;
  end;
end;

function TJclEmail.Read(const Options: TJclEmailReadOptions): Boolean;
var
  Flags: ULONG;
  Msg: PMapiMessage;
  I: Integer;
  Files: PMapiFileDesc;

  function CopyAndStrToInt(const S: string; Index, Count: Integer): Integer;
  begin
    Result := StrToIntDef(Copy(S, Index, Count), 0);
  end;

  function MessageDateToDate(const S: string): TDateTime;
  var
    T: TSystemTime;
  begin
    FillChar(T, SizeOf(T), #0);
    with T do
    begin
      wYear := CopyAndStrToInt(S, 1, 4);
      wMonth := CopyAndStrToInt(S, 6, 2);
      wDay := CopyAndStrToInt(S, 9, 2);
      wHour := CopyAndStrToInt(S, 12, 2);
      wMinute := CopyAndStrToInt(S, 15,2);
      Result := EncodeDate(wYear, wMonth, wDay) + EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
    end;
  end;

begin
  Result := False;
  if not UserLogged then
    Exit;
  Clear;
  Flags := 0;
  if roHeaderOnly in Options then
    Inc(Flags, MAPI_ENVELOPE_ONLY);
  if not (roMarkAsRead in Options) then
    Inc(Flags, MAPI_PEEK);
  if not (roAttachments in Options) then
    Inc(Flags, MAPI_SUPPRESS_ATTACH);
  MapiCheck(MapiReadMail(SessionHandle, 0, PChar(FSeedMessageID), Flags, 0, Msg), True);
  if Msg <> nil then
  try
    DecodeRecips(Msg^.lpOriginator, 1);
    DecodeRecips(Msg^.lpRecips, Msg^.nRecipCount);
    FSubject := Msg^.lpszSubject;
    Body := AdjustLineBreaks(Msg^.lpszNoteText);
    Files := Msg^.lpFiles;
    if Files <> nil then
      for I := 0 to Msg^.nFileCount - 1 do
      begin
        if Files^.lpszPathName <> nil then
          Attachments.Add(Files^.lpszPathName)
        else
          Attachments.Add(Files^.lpszFileName);
        Inc(Files);
      end;
    FReadMsg.MessageType := Msg^.lpszMessageType;
    if Msg^.lpszDateReceived <> nil then
      FReadMsg.DateReceived := MessageDateToDate(Msg^.lpszDateReceived);
    FReadMsg.ConversationID := Msg^.lpszConversationID;
    FReadMsg.Flags := Msg^.flFlags;
    Result := True;
  finally
    MapiFreeBuffer(Msg);
  end;
end;

function TJclEmail.ResolveName(var Name, Address: string; ShowDialog: Boolean): Boolean;
var
  Recip: PMapiRecipDesc;
  Res, Flags: DWORD;
begin
  LoadClientLib;
  Flags := LogonOptionsToFlags(ShowDialog) or MAPI_AB_NOMODIFY;
  Recip := nil;
  Res := MapiResolveName(FSessionHandle, ParentWnd, PChar(Name), Flags, 0, Recip);
  Result := (MapiCheck(Res, True) = SUCCESS_SUCCESS) and (Recip <> nil);
  if Result then
  begin
    Address := Recip^.lpszAddress;
    Name := Recip^.lpszName;
    MapiFreeBuffer(Recip);
  end;
end;

procedure TJclEmail.RestoreTaskWindows;
begin
  RestoreTaskWindowsList(FTaskWindowList);
  FTaskWindowList := nil;
end;

function TJclEmail.Save: Boolean;
begin
  Result := InternalSendOrSave(True, False);
end;

procedure TJclEmail.SaveTaskWindows;
begin
  FTaskWindowList := SaveTaskWindowsList;
end;

function TJclEmail.Send(ShowDialog: Boolean): Boolean;
begin
  Result := InternalSendOrSave(False, ShowDialog);
end;

procedure TJclEmail.SetBody(const Value: string);
begin
  if Value = '' then
    FBody := ''
  else
    FBody := StrEnsureSuffix(AnsiCrLf, Value);
end;

procedure TJclEmail.SetParentWnd(const Value: THandle);
begin
  FParentWnd := Value;
  FParentWndValid := True;
end;

procedure TJclEmail.SortAttachments;
var
  S, T, U: TStringList;
  I, Nr: Integer;
begin
  // This is confusing, quick and very dirty.
  S := TStringList.Create;
  try
    S.Capacity := FAttachments.Count;
    for I := 0 to Pred(FAttachments.Count) do
      S.AddObject(FAttachments[I], Pointer(I));
    S.Sort;
    T := TStringList.Create;
    U := TStringList.Create;
    try
      T.Capacity := S.Count;
      U.Capacity := S.Count;
      for I := 0 to Pred(S.Count) do
      begin
        Nr := Integer(S.Objects[I]);
        T.AddObject(FAttachments[Nr], FAttachments.Objects[Nr]);
        U.AddObject(FAttachmentFiles[Nr], FAttachmentFiles.Objects[Nr]);
      end;
      FAttachments.Assign(T);
      FAttachmentFiles.Assign(U);
    finally
      U.Free;
      T.Free;
    end;
  finally
    S.Free;
  end;
end;

//=== Simple email send function =============================================

function SimpleSendHelper(const ARecipient, AName, ASubject, ABody: string; const AAttachment: string;
  AShowDialog: Boolean; AParentWND: THandle; const AProfileName, APassword, AAddressType: string): Boolean;
begin
  with TJclEmail.Create do
  try
    if AParentWND <> 0 then
      ParentWnd := AParentWND;
    if ARecipient <> '' then
      Recipients.Add(ARecipient, AName, rkTO, AAddressType);
    Subject := ASubject;
    Body := ABody;
    if AAttachment <> '' then
      Attachments.Add(AAttachment);
    if AProfileName <> '' then
      LogOn(AProfileName, APassword);
    Result := Send(AShowDialog);
  finally
    Free;
  end;
end;

function JclSimpleSendMail(const Recipient, Name, Subject, Body: string;
  const Attachment: string; ShowDialog: Boolean; ParentWND: THandle;
  const ProfileName: string; const Password: string): Boolean;
begin
  Result := SimpleSendHelper(Recipient, Name, Subject, Body, Attachment, ShowDialog, ParentWND,
    ProfileName, Password, MapiAddressTypeSMTP);
end;

function JclSimpleSendFax(const Recipient, Name, Subject, Body: string;
  const Attachment: string; ShowDialog: Boolean; ParentWND: THandle;
  const ProfileName: string; const Password: string): Boolean;
begin
  Result := SimpleSendHelper(Recipient, Name, Subject, Body, Attachment, ShowDialog, ParentWND,
    ProfileName, Password, MapiAddressTypeFAX);
end;

function JclSimpleBringUpSendMailDialog(const Subject, Body: string;
  const Attachment: string; ParentWND: THandle;
  const ProfileName: string; const Password: string): Boolean;
begin
  Result := SimpleSendHelper('', '', Subject, Body, Attachment, True, ParentWND,
    ProfileName, Password, MapiAddressTypeSMTP);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
