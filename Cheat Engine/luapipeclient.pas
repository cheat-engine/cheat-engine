unit LuaPipeClient;


//pipe server class specifically made for lua. Only 1 client and 1 server connection at a time


{$mode delphi}

interface

uses
  windows, Classes, SysUtils, LuaPipe, lua, luaclass;

procedure initializeLuaPipeClient;

type
  TLuaPipeClient=class(TPipeConnection)
  private
  public
    constructor create(pipename: string);
  end;

implementation

uses LuaHandler;



constructor TLuaPipeClient.create(pipename: string);
begin
  inherited create;

  pipe:=CreateFile(pchar('\\.\pipe\'+pipename), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  fConnected:=pipe<>INVALID_HANDLE_VALUE;
end;

function luapipeclient_connectToPipe(L: PLua_state): integer; cdecl;
var pipename: string;

  p: TLuaPipeClient;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    pipename:=lua_tostring(L, 1);

    p:=TLuaPipeClient.create(pipename);
    if p.connected then
    begin
      luaclass_newClass(L, p);
      result:=1;
    end
    else
      p.free;
  end;
end;

procedure initializeLuaPipeClient;
begin
  lua_register(LuaVM, 'connectToPipe', luapipeclient_connectToPipe);
end;


end.

