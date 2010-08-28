unit LuaHandler;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, lua, lualib, lauxlib, syncobjs;

var
  LuaVM: Plua_State;
  LuaCS: Tcriticalsection;

implementation

function LuaPanic(L: Plua_State): Integer; cdecl;
begin
  raise exception.create('LUA panic!');
end;

initialization
  LuaCS:=TCriticalSection.create;
  LuaVM:=lua_open();

  if LuaVM<>nil then
  begin
    luaL_openlibs(LuaVM);

    lua_atpanic(LuaVM, LuaPanic);
  end;

finalization
  if LuaCS<>nil then
    LuaCS.free;

  if LuaVM<>nil then
    lua_close(LuaVM);

end.

