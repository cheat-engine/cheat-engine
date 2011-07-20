unit LuaFont;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics,lua, lualib, lauxlib, LuaHandler;

procedure initializeLuaFont;

implementation

function font_getColor(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  font: Tfont;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    font:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, font.Color);
    result:=1;
  end else lua_pop(L, parameters);
end;

function font_setColor(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  font: Tfont;
  Color: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    font:=lua_touserdata(L,-parameters);
    Color:=lua_tointeger(L, -parameters+1);
    lua_pop(L, parameters);

    font.Color:=Color;
  end else lua_pop(L, parameters);
end;

function font_getSize(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  font: Tfont;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    font:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, font.Size);
    result:=1;
  end else lua_pop(L, parameters);
end;

function font_setSize(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  font: Tfont;
  Size: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    font:=lua_touserdata(L,-parameters);
    size:=lua_tointeger(L, -parameters+1);
    lua_pop(L, parameters);

    font.Size:=Size;
  end else lua_pop(L, parameters);
end;

function font_getName(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  font: Tfont;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    font:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushstring(L, font.Name);
    result:=1;
  end else lua_pop(L, parameters);
end;

function font_setName(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  font: Tfont;
  Name: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    font:=lua_touserdata(L,-parameters);
    name:=Lua_ToString(L, -parameters+1);
    lua_pop(L, parameters);

    font.Name:=Name;
  end else lua_pop(L, parameters);
end;

procedure initializeLuaFont;
begin
  lua_register(LuaVM, 'font_getSize', font_getSize);
  lua_register(LuaVM, 'font_setSize', font_setSize);
  lua_register(LuaVM, 'font_getName', font_getName);
  lua_register(LuaVM, 'font_setName', font_setName);
  lua_register(LuaVM, 'font_getColor', font_getColor);
  lua_register(LuaVM, 'font_setColor', font_setColor);
end;

end.

