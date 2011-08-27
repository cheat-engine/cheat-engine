unit LuaProgressBar;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, LuaHandler, ceguicomponents,
  pluginexports, controls, ComCtrls;

procedure initializeLuaProgressBar;

implementation


function createProgressBar(L: Plua_State): integer; cdecl;
var
  ProgressBar: TCEProgressBar;
  parameters: integer;
  owner: TWincontrol;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    owner:=lua_touserdata(L, -parameters)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));


  ProgressBar:=TCEProgressBar.Create(owner);
  if owner<>nil then
    ProgressBar.Parent:=owner;

  lua_pushlightuserdata(L, ProgressBar);
  result:=1;
end;

function progressbar_stepIt(L: Plua_State): integer; cdecl;
var parameters: integer;
    progressbar: TCustomProgressBar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    progressbar:=lua_touserdata(L, -1);
    progressbar.StepIt;
  end;
  lua_pop(L, lua_gettop(L));
end;

function progressbar_stepBy(L: Plua_State): integer; cdecl;
var parameters: integer;
    progressbar: TCustomProgressBar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    progressbar:=lua_touserdata(L, -2);
    progressbar.StepBy(lua_tointeger(L, -1));
  end;
  lua_pop(L, lua_gettop(L));
end;


function progressbar_getMax(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  progressbar: Tcustomprogressbar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    progressbar:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, progressbar.max);
    result:=1;

  end else lua_pop(L, parameters);
end;

function progressbar_setMax(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  progressbar: Tcustomprogressbar;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    progressbar:=lua_touserdata(L,-2);
    progressbar.max:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;

function progressbar_getMin(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  progressbar: Tcustomprogressbar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    progressbar:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, progressbar.Min);
    result:=1;

  end else lua_pop(L, parameters);
end;

function progressbar_setMin(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  progressbar: Tcustomprogressbar;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    progressbar:=lua_touserdata(L,-2);
    progressbar.Min:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;


function progressbar_getPosition(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  progressbar: Tcustomprogressbar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    progressbar:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, progressbar.Position);
    result:=1;

  end else lua_pop(L, parameters);
end;

function progressbar_setPosition(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  progressbar: Tcustomprogressbar;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    progressbar:=lua_touserdata(L,-2);
    progressbar.Position:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;


procedure initializeLuaProgressBar;
begin
  lua_register(LuaVM, 'createProgressBar', createProgressBar);
  lua_register(LuaVM, 'progressbar_stepIt', progressbar_stepIt);
  lua_register(LuaVM, 'progressbar_stepBy', progressbar_stepBy);
  lua_register(LuaVM, 'progressbar_getMax', progressbar_getMax);
  lua_register(LuaVM, 'progressbar_setMax', progressbar_setMax);
  lua_register(LuaVM, 'progressbar_getMin', progressbar_getMin);
  lua_register(LuaVM, 'progressbar_setMin', progressbar_setMin);
  lua_register(LuaVM, 'progressbar_getPosition', progressbar_getPosition);
  lua_register(LuaVM, 'progressbar_setPosition', progressbar_setPosition);
end;

end.

