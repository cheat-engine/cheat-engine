unit LuaWinControl;

{$mode delphi}

interface

uses
  Classes, SysUtils, controls, lua, lualib, lauxlib,LuaHandler;

procedure initializeLuaWinControl;

implementation

uses LuaCaller;


function wincontrol_getControlCount(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  wincontrol: TWinControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    wincontrol:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, wincontrol.ControlCount);
    result:=1;

  end else lua_pop(L, parameters);
end;

function wincontrol_getControl(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  wincontrol: TWinControl;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    wincontrol:=lua_touserdata(L,-2);
    index:=lua_tointeger(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, wincontrol.Controls[index]);
    result:=1;

  end else lua_pop(L, parameters);
end;

function wincontrol_getControlAtPos(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  wincontrol: TWinControl;
  x,y: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    wincontrol:=lua_touserdata(L,-3);
    x:=lua_tointeger(L,-2);
    y:=lua_tointeger(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, wincontrol.ControlAtPos(point(x,y),[capfOnlyClientAreas, capfAllowWinControls, capfRecursive]));
    result:=1;

  end else lua_pop(L, parameters);
end;


function wincontrol_onEnter(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  wincontrol: TWinControl;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    wincontrol:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(wincontrol.OnEnter));
    wincontrol.OnEnter:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      wincontrol.OnEnter:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      wincontrol.OnEnter:=lc.NotifyEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

function wincontrol_onExit(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  wincontrol: TWinControl;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    wincontrol:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(wincontrol.onExit));
    wincontrol.onExit:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      wincontrol.OnExit:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      wincontrol.OnExit:=lc.NotifyEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

function wincontrol_canFocus(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  wincontrol: TWinControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    wincontrol:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, wincontrol.CanFocus);
    result:=1;

  end else lua_pop(L, parameters);
end;

function wincontrol_focused(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  wincontrol: TWinControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    wincontrol:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, wincontrol.Focused);
    result:=1;

  end else lua_pop(L, parameters);
end;

function wincontrol_setFocus(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  wincontrol: TWinControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    wincontrol:=lua_touserdata(L,-1);
    wincontrol.SetFocus;
  end;

  lua_pop(L, parameters);
end;

procedure initializeLuaWinControl;
begin
  lua_register(LuaVM, 'wincontrol_getControlCount', wincontrol_getControlCount);
  lua_register(LuaVM, 'wincontrol_getControl', wincontrol_getControl);
  lua_register(LuaVM, 'wincontrol_getControlAtPos', wincontrol_getControlAtPos);
  lua_register(LuaVM, 'wincontrol_onEnter', wincontrol_OnEnter);
  lua_register(LuaVM, 'wincontrol_onExit', wincontrol_OnExit);
  lua_register(LuaVM, 'wincontrol_canFocus', wincontrol_canFocus);
  lua_register(LuaVM, 'wincontrol_focused', wincontrol_focused);
  lua_register(LuaVM, 'wincontrol_setFocus', wincontrol_setFocus);
end;

end.

