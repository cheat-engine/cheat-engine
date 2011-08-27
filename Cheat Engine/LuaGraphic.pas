unit LuaGraphic;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics,lua, lualib, lauxlib,LuaHandler;

procedure initializeLuaGraphic;

implementation

function graphic_getWidth(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  graphic: TGraphic;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    graphic:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, graphic.Width);
    result:=1;
  end else lua_pop(L, parameters);
end;

function graphic_setWidth(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  graphic: TGraphic;
  width: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    graphic:=lua_touserdata(L,-parameters);
    width:=lua_tointeger(L, -parameters+1);
    lua_pop(L, parameters);

    graphic.Width:=width;
  end else lua_pop(L, parameters);
end;

function graphic_getHeight(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  graphic: TGraphic;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    graphic:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, graphic.Height);
    result:=1;
  end else lua_pop(L, parameters);
end;

function graphic_setHeight(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  graphic: TGraphic;
  Height: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    graphic:=lua_touserdata(L,-parameters);
    Height:=lua_tointeger(L, -parameters+1);
    lua_pop(L, parameters);

    graphic.Height:=Height;
  end else lua_pop(L, parameters);
end;

procedure initializeLuaGraphic;
begin
  lua_register(LuaVM, 'graphic_getWidth', graphic_getWidth);
  lua_register(LuaVM, 'graphic_setWidth', graphic_setWidth);
  lua_register(LuaVM, 'graphic_getHeight', graphic_getHeight);
  lua_register(LuaVM, 'graphic_setHeight', graphic_setHeight);

end;

end.

