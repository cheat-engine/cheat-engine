unit LuaRemoteThread;

{$mode delphi}

interface

uses
  Classes, SysUtils,lua, lualib, lauxlib,LuaHandler, LCLType;


  procedure initializeLuaRemoteThread;

implementation

uses
  {$ifdef darwin}macport,pthreads, macCreateRemoteThread, {$endif}
  {$ifdef windows}windows,{$endif}
  ProcessHandlerUnit, LuaClass, LuaObject, NewKernelHandler;

type
  TRemoteThread=class
  private
    h: THandle;
    tid: dword;
  public
    function getResult: integer;
    function waitForThread(timeout: integer): integer;
    constructor create(address: ptruint; parameter: ptruint);
  published
    property Result: integer read getResult;
  end;

function TRemoteThread.getResult: integer;
var r: dword;
begin

  if GetExitCodeThread(h, r) then
    result:=r
  else
    result:=-1;
end;


function TRemoteThread.waitForThread(timeout: integer): integer;
begin
  result:=-1; //not yet implemented
  {$ifdef windows}
  result:=WaitForSingleObject(h,timeout);
  {$endif}

  {$ifdef darwin}
  if macWaitForRemoteThread(h, timeout) then
    result:=WAIT_OBJECT_0
  else
    result:=WAIT_TIMEOUT;

  {$endif}



end;

constructor TRemoteThread.create(address: ptruint; parameter: ptruint);
begin
  h:=CreateRemoteThread(processhandle, nil, 0, pointer(address), pointer(parameter), 0, tid);
end;

function remotethread_waitForThread(L: PLua_State): integer; cdecl;
var
  timeout: dword;
  r: integer;
  rt: TRemoteThread;
begin
  result:=0;
  if lua_Gettop(L)>=1 then
    timeout:=lua_tointeger(L,1)
  else
    timeout:=INFINITE;

  rt:=TRemoteThread(luaclass_getClassObject(L));
  r:=rt.waitForThread(timeout);
  if r=WAIT_OBJECT_0 then
  begin
    result:=2;
    lua_pushboolean(L, true);
    lua_pushinteger(L, rt.Result);
  end
  else
  if r=WAIT_TIMEOUT then
  begin
    result:=2;
    lua_pushboolean(L, false);
    lua_pushinteger(L, lua_integer(-2));
  end
  else
  begin
    result:=2;
    lua_pushboolean(L, false);
    lua_pushinteger(L, lua_integer(-3));
  end;
end;

function lua_createRemoteThread(L: PLua_State): integer; cdecl;
var
  rt: TRemoteThread;
  address, parameter: ptruint;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    if lua_isnil(L,1) then exit;

    address:=lua_toaddress(L,1);

    if lua_gettop(L)>=2 then
      parameter:=lua_toaddress(L,2)
    else
      parameter:=0;

    rt:=TRemoteThread.create(address, parameter);

    if rt.h=0 then
    begin
      rt.free;
      exit;
    end
    else
    begin
      luaclass_newClass(L,rt);
      result:=1;
    end;
  end;
end;

procedure remotethread_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'waitForThread', remotethread_waitForThread);
end;

procedure initializeLuaRemoteThread;
begin
  lua_register(LuaVM, 'createRemoteThread', lua_createRemoteThread);
end;

initialization
  luaclass_register(TRemoteThread, remotethread_addMetaData);


end.

