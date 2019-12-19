unit fileaccess;

{$mode delphi}

interface

{$IFDEF windows}
uses
  jwaWindows, windows,Classes, SysUtils;

procedure MakePathAccessible(path: widestring);
{$ENDIF}

implementation

{$IFDEF windows}
resourcestring
  rsNoGetNamedSecurityInfo = 'no GetNamedSecurityInfo';
  rsNoGetSecurityInfo = 'no GetSecurityInfo';
  rsNoSetEntriesInAcl = 'no SetEntriesInAcl';
  rsNoSetNamedSecurityInfo = 'no SetNamedSecurityInfo';

const  SECURITY_WORLD_SID_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 1));

type
  PTRUSTEE_W=^TRUSTEE_W;
  TRUSTEE_W = record
    pMultipleTrustee: PTRUSTEE_W;
    MultipleTrusteeOperation: MULTIPLE_TRUSTEE_OPERATION;
    TrusteeForm: TRUSTEE_FORM;
    TrusteeType: TRUSTEE_TYPE;
    ptstrName: LPWCH;
  end;


type
  EXPLICIT_ACCESS_W = record
    grfAccessPermissions: DWORD;
    grfAccessMode: ACCESS_MODE;
    grfInheritance: DWORD;
    Trustee: TRUSTEE_W;
  end;

  PEXPLICIC_ACCESS_W=^EXPLICIT_ACCESS_W;




var SetNamedSecurityInfo: function(pObjectName: PWideChar; ObjectType: SE_OBJECT_TYPE; SecurityInfo: SECURITY_INFORMATION; ppsidOwner, ppsidGroup: PSID; ppDacl, ppSacl: PACL): DWORD; stdcall;
var GetNamedSecurityInfo: function(pObjectName: PWidechar; ObjectType: SE_OBJECT_TYPE; SecurityInfo: SECURITY_INFORMATION; ppsidOwner, ppsidGroup: PPSID; ppDacl, ppSacl: PPACL; var ppSecurityDescriptor: PSECURITY_DESCRIPTOR): DWORD; stdcall;
var GetSecurityInfo: function(handle: HANDLE; ObjectType: SE_OBJECT_TYPE; SecurityInfo: SECURITY_INFORMATION; ppsidOwner: PPSID; ppsidGroup: PPSID; ppDacl, ppSacl: PPACL; var ppSecurityDescriptor: PSECURITY_DESCRIPTOR): DWORD; stdcall;
var SetEntriesInAcl: function(cCountOfExplicitEntries: ULONG; pListOfExplicitEntries: PEXPLICIT_ACCESS_W; OldAcl: PACL; var NewAcl: PACL): DWORD; stdcall;


procedure MakePathAccessible(path: widestring);
var l: tstringlist;

  z: SID_IDENTIFIER_AUTHORITY;
  sid: pointer;

  sec: PSECURITY_DESCRIPTOR;
  dacl: pacl;

  newdacl: pacl;

  i: dword;

  f: thandle;

  ea: EXPLICIT_ACCESS_W;



  cbName,cbDomain : Cardinal;
  advapi: thandle;
begin
  advapi:=loadlibrary('Advapi32.dll');
  GetNamedSecurityInfo:=getprocaddress(advapi, 'GetNamedSecurityInfoW');
  GetSecurityInfo:=getprocaddress(advapi, 'GetSecurityInfo');
  SetEntriesInAcl:=getprocaddress(advapi, 'SetEntriesInAclW');
  SetNamedSecurityInfo:=getprocaddress(advapi, 'SetNamedSecurityInfoW');
  if not assigned(GetNamedSecurityInfo) then raise exception.create(rsNoGetNamedSecurityInfo);
  if not assigned(GetSecurityInfo) then raise exception.create(rsNoGetSecurityInfo);
  if not assigned(SetEntriesInAcl) then raise exception.create(rsNoSetEntriesInAcl);
  if not assigned(SetNamedSecurityInfo) then raise exception.create(rsNoSetNamedSecurityInfo);
  //  showmessage(advancedoptionsunit.strcouldntrestorecode);
    //function CreateFile(lpFileName:LPCSTR; dwDesiredAccess:DWORD; dwShareMode:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; dwCreationDisposition:DWORD;dwFlagsAndAttributes:DWORD; hTemplateFile:HANDLE):HANDLE; external 'kernel32' name 'CreateFileA';
   // z:=0;
   //f:=CreateFile('C:\temp',0,0, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS,0);
   //if f=INVALID_HANDLE_VALUE then raise exception.create('failed to open dir');

  z:=SECURITY_WORLD_SID_AUTHORITY;
  sid:=nil;

  if AllocateAndInitializeSid(SECURITY_WORLD_SID_AUTHORITY,1,SECURITY_WORLD_RID,0,0,0,0,0,0,0,sid) then //the everyone group
  begin
    sec:=nil;
    dacl:=nil;
    i:=GetNamedSecurityInfo(pwidechar(path), SE_FILE_OBJECT, DACL_SECURITY_INFORMATION, nil,nil, @dacl, nil, sec);
    if i=ERROR_SUCCESS then
    begin
      zeromemory(@ea,sizeof(ea));
      ea.grfAccessPermissions:=GENERIC_ALL;
      ea.grfAccessMode:=GRANT_ACCESS;
      ea.grfInheritance:=SUB_CONTAINERS_AND_OBJECTS_INHERIT;
      ea.Trustee.TrusteeForm:=TRUSTEE_IS_SID;
      ea.Trustee.TrusteeType:=TRUSTEE_IS_UNKNOWN;
      PSID(ea.Trustee.ptstrName) := Sid;


      newdacl:=nil;
      i:=SetEntriesInAcl(1, @ea, dacl, newdacl);
      if i=ERROR_SUCCESS then
      begin
        i:=SetNamedSecurityInfo(pwidechar(path), SE_FILE_OBJECT, DACL_SECURITY_INFORMATION, nil,nil, newdacl, nil);
        //if i<>error_success then
        //  raise exception.create('SetNamedSecurityInfo failed: '+inttostr(i));
      end;
      //else raise exception.create('SetEntriesInAcl failed: '+inttostr(i));
    end;// else raise exception.create('GetNamedSecurityInfo failed: '+inttostr(i));
  end;
  //else raise exception.create('AllocateAndInitializeSid failed');
end;
{$ENDIF}

end.

