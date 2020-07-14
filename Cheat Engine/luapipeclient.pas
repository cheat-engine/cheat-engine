unit LuaPipeClient;


//pipe server class specifically made for lua. Only 1 client and 1 server connection at a time


{$mode delphi}

interface


uses
  {$ifdef windows}
  windows,
  {$endif}
  {$ifdef darwin}
  macport, macpipe,
  {$endif}
  Classes, SysUtils, LuaPipe, lua, LuaClass;

procedure initializeLuaPipeClient;

type
  TLuaPipeClient=class(TPipeConnection)
  private
  public
    constructor create(pipename: string; timeout: integer=0);
  end;


implementation

uses LuaHandler;

constructor TLuaPipeClient.create(pipename: string; timeout: integer=0);
begin
  inherited create;

  ftimeout:=timeout;
  fOverLapped:=true; //timeout>0;

  pipe:=INVALID_HANDLE_VALUE;
  {$ifdef windows}
  if foverlapped then
    pipe:=CreateFile(pchar('\\.\pipe\'+pipename), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING,  FILE_FLAG_OVERLAPPED, 0)
  else
    pipe:=CreateFile(pchar('\\.\pipe\'+pipename), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING,  0, 0);
  {$endif}

  {$ifdef darwin}
  pipe:=connectNamedPipe(pipename, timeout);
  outputdebugstring('pipe='+inttohex(THANDLE(pipe),1));
  outputdebugstring('INVALID_HANDLE_VALUE='+inttohex(THANDLE(INVALID_HANDLE_VALUE),1));
  {$endif}


  fConnected:=THANDLE(pipe)<>THANDLE(INVALID_HANDLE_VALUE);

  if fConnected then
    outputdebugstring('pipe is valid')
  else
    outputdebugstring('pipe is invalid');

end;

function luapipeclient_connectToPipe(L: PLua_state): integer; cdecl;
var pipename: string;

  p: TLuaPipeClient;
  timeout: integer;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    pipename:=lua_tostring(L, 1);

    if pipename='' then exit(0);

    if lua_gettop(L)>=2 then
      timeout:=lua_tointeger(L,2)
    else
      timeout:=0;

    p:=TLuaPipeClient.create(pipename,timeout);
    if p.connected then
    begin
      outputdebugstring('Returning pipe object');
      luaclass_newClass(L, p);
      result:=1;
    end
    else
    begin
      p.free;
      outputdebugstring('Returning nil');
    end;
  end;
end;


procedure initializeLuaPipeClient;
begin
  lua_register(LuaVM, 'connectToPipe', luapipeclient_connectToPipe);
end;


end.

