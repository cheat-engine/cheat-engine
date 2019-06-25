unit LuaRasterImage;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, lua, lualib, lauxlib;

procedure initializeLuaRasterImage;

implementation

uses LuaHandler, luaclass, LuaGraphic;

function rasterImage_getCanvas(L: PLua_State): integer; cdecl;
var
  c: TrasterImage;
begin
  c:=luaclass_getClassObject(L);
  luaclass_newClass(L, c.Canvas);
  result:=1;
end;

function rasterimage_getPixelFormat(L: PLua_State): integer; cdecl;
var
  c: TrasterImage;
begin
  c:=luaclass_getClassObject(L);
  lua_pushinteger(L, integer(c.PixelFormat));
  result:=1;
end;


function rasterimage_setPixelFormat(L: PLua_State): integer; cdecl;
var
  c: TrasterImage;
  a: integer;
begin
  result:=0;
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    c.PixelFormat:=TPixelFormat(lua_tointeger(L,-1));
end;

function rasterimage_getTransparent(L: PLua_State): integer; cdecl; //obsolete
var
  rasterimage: Trasterimage;
begin
  rasterimage:=luaclass_getClassObject(L);
  lua_pushboolean(L, rasterimage.Transparent);
  result:=1;
end;

function rasterimage_setTransparent(L: PLua_State): integer; cdecl;
var
  rasterimage: Trasterimage;
begin
  result:=0;
  rasterimage:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    rasterimage.Transparent:=lua_toboolean(L, -1);
end;


function rasterimage_gettransparentColor(L: PLua_State): integer; cdecl;
var
  c: TrasterImage;
begin
  c:=luaclass_getClassObject(L);
  lua_pushinteger(L, c.transparentColor);
  result:=1;
end;


function rasterimage_settransparentColor(L: PLua_State): integer; cdecl;
var
  c: TrasterImage;
  a: integer;
begin
  result:=0;
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    c.transparentColor:=lua_tointeger(L,-1);
end;

function rasterimage_loadFromStream(L: PLua_State): integer; cdecl;
var
  c: TrasterImage;
begin
  result:=0;
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    c.LoadFromStream(lua_ToCEUserData(L,1));
end;

function rasterimage_saveToStream(L: PLua_State): integer; cdecl;
var
  c: TrasterImage;
begin
  result:=0;
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    c.SaveToStream(lua_ToCEUserData(L,1));
end;

procedure rasterimage_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  graphic_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCanvas', rasterImage_getCanvas);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getPixelFormat', rasterimage_getPixelFormat);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setPixelFormat', rasterimage_setPixelFormat);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getTransparentColor', rasterimage_getTransparentColor);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setTransparentColor', rasterimage_setTransparentColor);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'loadFromStream', rasterimage_loadFromStream);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'saveToStream', rasterimage_saveToStream);

  Luaclass_addPropertyToTable(L, metatable, userdata, 'Canvas', rasterImage_getCanvas, nil);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'PixelFormat', rasterimage_getPixelFormat, rasterimage_setPixelFormat);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'TransparentColor', rasterimage_getTransparentColor, rasterimage_setTransparentColor);
end;


procedure initializeLuaRasterImage;
begin
  lua_register(LuaVM, 'rasterimage_getCanvas', rasterImage_getCanvas);
  lua_register(LuaVM, 'rasterimage_getPixelFormat', rasterimage_getPixelFormat);
  lua_register(LuaVM, 'rasterimage_setPixelFormat', rasterimage_setPixelFormat);
  lua_register(LuaVM, 'rasterimage_getTransparent', rasterimage_getTransparent);  //should be graphic_
  lua_register(LuaVM, 'rasterimage_setTransparent', rasterimage_setTransparent);
  lua_register(LuaVM, 'rasterimage_getTransparentColor', rasterimage_getTransparentColor);
  lua_register(LuaVM, 'rasterimage_setTransparentColor', rasterimage_setTransparentColor);
end;

initialization
  luaclass_register(TrasterImage, rasterImage_addMetaData);



end.

