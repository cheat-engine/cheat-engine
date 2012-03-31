unit LuaRasterImage;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, lua, lualib, lauxlib;

procedure initializeLuaRasterImage;

implementation

uses LuaHandler;

function rasterImage_getCanvas(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TrasterImage;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, c.Canvas);
    result:=1;

  end else lua_pop(L, parameters);
end;

function rasterimage_getPixelFormat(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TrasterImage;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, integer(c.PixelFormat));
    result:=1;

  end else lua_pop(L, parameters);
end;


function rasterimage_setPixelFormat(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TrasterImage;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_touserdata(L,-2);
    c.PixelFormat:=TPixelFormat(lua_tointeger(L,-1));
  end;

  lua_pop(L, parameters);
end;

function rasterimage_getTransparent(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TrasterImage;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, c.Transparent);
    result:=1;

  end else lua_pop(L, parameters);
end;


function rasterimage_setTransparent(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TrasterImage;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_touserdata(L,-2);
    c.Transparent:=lua_toboolean(L,-1);
  end;

  lua_pop(L, parameters);
end;

function rasterimage_getTransparentColor(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TrasterImage;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, integer(c.TransparentColor));
    result:=1;

  end else lua_pop(L, parameters);
end;


function rasterimage_setTransparentColor(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TrasterImage;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_touserdata(L,-2);
    c.TransparentColor:=TColor(lua_tointeger(L,-1));
  end;

  lua_pop(L, parameters);
end;


procedure initializeLuaRasterImage;
begin
  lua_register(LuaVM, 'rasterImage_getCanvas', rasterImage_getCanvas);
  lua_register(LuaVM, 'rasterimage_getPixelFormat', rasterimage_getPixelFormat);
  lua_register(LuaVM, 'rasterimage_setPixelFormat', rasterimage_setPixelFormat);
  lua_register(LuaVM, 'rasterimage_getTransparent', rasterimage_getTransparent);
  lua_register(LuaVM, 'rasterimage_setTransparent', rasterimage_setTransparent);
  lua_register(LuaVM, 'rasterimage_getTransparentColor', rasterimage_getTransparentColor);
  lua_register(LuaVM, 'rasterimage_setTransparentColor', rasterimage_setTransparentColor);
end;


end.

