unit LuaCheatComponent;

{$mode delphi}

interface

uses
  Classes, SysUtils,lua, lualib, lauxlib, LuaHandler, LuaCaller,
  ExtCtrls, StdCtrls, ExtraTrainerComponents;

procedure initializeLuaCheatComponent;

implementation

uses LuaClass, LuaWinControl;

function cheatcomponent_getActive(L: PLua_State): integer; cdecl;
var
  cheatcomponent: TCheat;
begin
  cheatcomponent:=luaclass_getClassObject(L);
  lua_pushboolean(L, cheatcomponent.activated);
  result:=1;
end;


function cheatcomponent_setActive(L: PLua_State): integer; cdecl;
var
  paramstart, paramcount: integer;
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  cheatcomponent:=luaclass_getClassObject(L, @paramstart, @paramcount);


  if paramcount>=1 then
  begin
    cheatcomponent.activated:=lua_toboolean(L,paramstart);

    if paramcount=2 then
    begin
      deactivatetime:=lua_tointeger(L,paramstart+1);
      if cheatcomponent.activated then
        cheatcomponent.setDeactivateTimer(deactivatetime);

    end;
  end;
end;

function cheatcomponent_getDescription(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;
begin
  cheatcomponent:=luaclass_getClassObject(L);
  lua_pushstring(L, cheatcomponent.Description);
  result:=1;
end;


function cheatcomponent_setDescription(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  cheatcomponent:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
    cheatcomponent.Description:=Lua_ToString(L,-1);
end;

function cheatcomponent_getHotkey(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;
begin
  result:=0;
  cheatcomponent:=luaclass_getClassObject(L);
  lua_pushstring(L, cheatcomponent.Hotkey);
  result:=1;
end;


function cheatcomponent_setHotkey(L: PLua_State): integer; cdecl;
var
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  cheatcomponent:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    cheatcomponent.Hotkey:=Lua_ToString(L,-1);
end;

function cheatcomponent_getDescriptionLeft(L: PLua_State): integer; cdecl;
var
  cheatcomponent: TCheat;
begin
  cheatcomponent:=luaclass_getClassObject(L);
  lua_pushinteger(L, cheatcomponent.DescriptionLeft);
  result:=1;
end;


function cheatcomponent_setDescriptionLeft(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  cheatcomponent:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
    cheatcomponent.Descriptionleft:=lua_tointeger(L,-1);
end;


function cheatcomponent_getHotkeyLeft(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;
begin
  result:=0;
  cheatcomponent:=luaclass_getClassObject(L);
  lua_pushinteger(L, cheatcomponent.Hotkeyleft);
  result:=1;
end;


function cheatcomponent_setHotkeyLeft(L: PLua_State): integer; cdecl;
var
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  cheatcomponent:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    cheatcomponent.Hotkeyleft:=lua_tointeger(L,-1);
end;

function cheatcomponent_getEditValue(L: PLua_State): integer; cdecl;
var
  cheatcomponent: TCheat;
begin
  cheatcomponent:=luaclass_getClassObject(L);
  lua_pushstring(L, cheatcomponent.EditValue);
  result:=1;
end;


function cheatcomponent_setEditValue(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  cheatcomponent: TCheat;

  deactivatetime: integer;
  t: TTimer;
begin
  result:=0;
  cheatcomponent:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    cheatcomponent.EditValue:=Lua_ToString(L,-1);
end;

procedure cheatcomponent_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setActive', cheatcomponent_setActive);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getActive', cheatcomponent_getActive);
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

initialization
  luaclass_register(TCheat, cheatcomponent_addMetaData);


end.

