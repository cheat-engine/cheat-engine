unit LuaCheatComponent;

{$mode delphi}

interface

uses
  Classes, SysUtils,lua, lualib, lauxlib, LuaHandler, LuaCaller,
  ExtCtrls, StdCtrls, ExtraTrainerComponents;

procedure initializeLuaCheatComponent;

implementation



function cheatcomponent_getActive(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    cheatcomponent:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, cheatcomponent.activated);
    result:=1;

  end else lua_pop(L, parameters);
end;


function cheatcomponent_setActive(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=2 then
  begin
    cheatcomponent:=lua_touserdata(L,-parameters);
    cheatcomponent.activated:=lua_toboolean(L,-parameters+1);

    if parameters=3 then
    begin
      deactivatetime:=lua_tointeger(L,-parameters+2);
      if cheatcomponent.activated then
        cheatcomponent.setDeactivateTimer(deactivatetime);

    end;
  end;


  lua_pop(L, parameters);
end;

function cheatcomponent_getDescription(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    cheatcomponent:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushstring(L, cheatcomponent.Description);
    result:=1;

  end else lua_pop(L, parameters);
end;


function cheatcomponent_setDescription(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    cheatcomponent:=lua_touserdata(L,-2);
    cheatcomponent.Description:=Lua_ToString(L,-1);
  end;
  lua_pop(L, parameters);
end;

function cheatcomponent_getHotkey(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    cheatcomponent:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushstring(L, cheatcomponent.Hotkey);
    result:=1;

  end else lua_pop(L, parameters);
end;


function cheatcomponent_setHotkey(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    cheatcomponent:=lua_touserdata(L,-2);
    cheatcomponent.Hotkey:=Lua_ToString(L,-1);
  end;
  lua_pop(L, parameters);
end;

function cheatcomponent_getDescriptionLeft(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    cheatcomponent:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, cheatcomponent.DescriptionLeft);
    result:=1;

  end else lua_pop(L, parameters);
end;


function cheatcomponent_setDescriptionLeft(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    cheatcomponent:=lua_touserdata(L,-2);
    cheatcomponent.DescriptionLeft:=lua_tointeger(L,-1);
  end;
  lua_pop(L, parameters);
end;

function cheatcomponent_getHotkeyLeft(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    cheatcomponent:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, cheatcomponent.HotkeyLeft);
    result:=1;

  end else lua_pop(L, parameters);
end;


function cheatcomponent_setHotkeyLeft(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    cheatcomponent:=lua_touserdata(L,-2);
    cheatcomponent.HotkeyLeft:=lua_tointeger(L,-1);
  end;
  lua_pop(L, parameters);
end;

function cheatcomponent_getEditValue(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    cheatcomponent:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushstring(L, cheatcomponent.EditValue);
    result:=1;

  end else lua_pop(L, parameters);
end;


function cheatcomponent_setEditValue(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    cheatcomponent:=lua_touserdata(L,1);
    if (cheatcomponent=nil) or (not (cheatcomponent is tcheat)) then
      raise exception.create('The provided cheat component is not a valid cheat component');


    cheatcomponent.EditValue:=Lua_ToString(L,2);
  end;
  lua_pop(L, parameters);
end;

procedure initializeLuaCheatComponent;
begin
  Lua_register(LuaVM, 'cheatcomponent_setActive', cheatcomponent_setActive);
  Lua_register(LuaVM, 'cheatcomponent_getActive', cheatcomponent_getActive);
  Lua_register(LuaVM, 'cheatcomponent_setDescription', cheatcomponent_setDescription);
  Lua_register(LuaVM, 'cheatcomponent_getDescription', cheatcomponent_getDescription);
  Lua_register(LuaVM, 'cheatcomponent_setHotkey', cheatcomponent_setHotkey);
  Lua_register(LuaVM, 'cheatcomponent_getHotkey', cheatcomponent_getHotkey);
  Lua_register(LuaVM, 'cheatcomponent_setDescriptionLeft', cheatcomponent_setDescriptionLeft);
  Lua_register(LuaVM, 'cheatcomponent_getDescriptionLeft', cheatcomponent_getDescriptionLeft);
  Lua_register(LuaVM, 'cheatcomponent_setHotkeyLeft', cheatcomponent_setHotkeyLeft);
  Lua_register(LuaVM, 'cheatcomponent_getHotkeyLeft', cheatcomponent_getHotkeyLeft);
  Lua_register(LuaVM, 'cheatcomponent_setEditValue', cheatcomponent_setEditValue);
  Lua_register(LuaVM, 'cheatcomponent_getEditValue', cheatcomponent_getEditValue);
end;

end.

