unit LuaPipeServer;

//pipe server class specifically made for lua. Only 1 client and 1 server connection at a time

{$mode delphi}

interface

{$ifdef windows}
uses
  jwawindows, windows, Classes, SysUtils, lua, luaclass, luapipe;


procedure initializeLuaPipeServer;
{$endif}

implementation

{$ifdef windows}
uses LuaHandler;

type
  TLuaPipeServer=class(TPipeConnection)
  private
    function isValidPipe: boolean;
  public
    function WaitForClientConnection: boolean;
    constructor create(pipename: string; inputsize, outputsize: integer);
  published
    property valid: boolean read isValidPipe;
  end;

function TLuaPipeServer.isValidPipe;
begin
  result:=pipe<>INVALID_HANDLE_VALUE;
end;

function TLuaPipeServer.WaitForClientConnection;
begin
  fconnected:=ConnectNamedPipe(pipe, nil);

  if not fconnected then
    fconnected:=getlasterror()=ERROR_PIPE_CONNECTED;

  result:=fConnected;
end;

constructor TLuaPipeServer.create(pipename: string; inputsize, outputsize: integer);
var
  a: SECURITY_ATTRIBUTES;
begin
  inherited create;

  a.nLength:=sizeof(a);
  a.bInheritHandle:=TRUE;
  ConvertStringSecurityDescriptorToSecurityDescriptor('D:(D;;FA;;;NU)(A;;0x12019f;;;WD)(A;;0x12019f;;;CO)', SDDL_REVISION_1, a.lpSecurityDescriptor, nil);

  //AllocateAndInitializeSid();
  {
  LPCWSTR LOW_INTEGRITY_SDDL_SACL_W = L"S:(ML;;NW;;;LW)"
  ;
  PSECURITY_DESCRIPTOR securitydescriptor;
  ConvertStringSecurityDescriptorToSecurityDescriptorW(LOW_INTEGRITY_SDDL_SACL_W,SDDL_REVISION_1,&securitydescriptor,NULL);
  sa.nLength = sizeof
  (SECURITY_ATTRIBUTES);
  sa.lpSecurityDescriptor = securitydescriptor;
  sa.bInheritHandle = TRUE;
  }


  pipe:=CreateNamedPipe(pchar('\\.\pipe\'+pipename), PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE or PIPE_READMODE_BYTE or PIPE_WAIT, 1,inputsize, outputsize, INFINITE, @a);

  LocalFree(HLOCAL(a.lpSecurityDescriptor));
end;


function luapipeserver_createNamedPipee(L: PLua_state): integer; cdecl;
var
  pipename: string;
  inputsize: integer;
  outputsize: integer;
  paramcount: integer;
begin
  result:=0;
  paramcount:=lua_gettop(L);
  if paramcount>=1 then
  begin
    inputsize:=4096;
    outputsize:=4096;

    if paramcount>=2 then
      inputsize:=lua_tointeger(L, 2);

    if paramcount>=3 then
      outputsize:=lua_tointeger(L, 3);

    pipename:=lua_tostring(L, 1);
    luaclass_newClass(L, TLuaPipeServer.create(pipename, inputsize, outputsize));
    result:=1;
  end;
end;

function luapipeserver_WaitForClientConnection(L: Plua_State): integer; cdecl;
var p: TLuaPipeServer;
begin
  p:=luaclass_getClassObject(L);
  p.WaitForClientConnection;

  result:=0;
end;

procedure luapipeserver_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  pipecontrol_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'acceptConnection', luapipeserver_WaitForClientConnection);

end;

procedure initializeLuaPipeServer;
begin
  lua_register(LuaVM, 'createPipe', luapipeserver_createNamedPipee);
end;

initialization
  luaclass_register(TLuaPipeServer, luapipeserver_addMetaData );

{$endif}

end.

