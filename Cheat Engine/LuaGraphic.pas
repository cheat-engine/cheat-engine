unit LuaGraphic;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics,lua, lualib, lauxlib,LuaHandler;

procedure initializeLuaGraphic;
procedure graphic_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses luaclass, LuaObject;

function graphic_getWidth(L: PLua_State): integer; cdecl;
var
  graphic: TGraphic;
begin
  graphic:=luaclass_getClassObject(L);
  lua_pushinteger(L, graphic.Width);
  result:=1;
end;

function graphic_setWidth(L: PLua_State): integer; cdecl;
var
  graphic: TGraphic;
begin
  result:=0;
  graphic:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    graphic.Width:=lua_tointeger(L, 1);
end;

function graphic_getHeight(L: PLua_State): integer; cdecl;
var
  graphic: TGraphic;
begin
  graphic:=luaclass_getClassObject(L);
  lua_pushinteger(L, graphic.Height);
  result:=1;
end;

function graphic_setHeight(L: PLua_State): integer; cdecl;
var
  graphic: TGraphic;
begin
  result:=0;
  graphic:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    graphic.Height:=lua_tointeger(L, 1);
end;

function graphic_getTransparent(L: PLua_State): integer; cdecl;
var
  graphic: TGraphic;
begin
  graphic:=luaclass_getClassObject(L);
  lua_pushboolean(L, graphic.Transparent);
  result:=1;
end;

function graphic_setTransparent(L: PLua_State): integer; cdecl;
var
  graphic: TGraphic;
begin
  result:=0;
  graphic:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    graphic.Transparent:=lua_toboolean(L, 1);
end;

function graphic_loadFromFile(L: PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>=1 then
    tgraphic(luaclass_getClassObject(L)).LoadFromFile(Lua_ToString(L,1));
end;

function graphic_saveToFile(L: PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>=1 then
    tgraphic(luaclass_getClassObject(L)).SaveToFile(Lua_ToString(L,1));
end;

procedure graphic_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getWidth', graphic_getWidth);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setWidth', graphic_setWidth);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getHeight', graphic_getHeight);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setHeight', graphic_setHeight);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getTransparent', graphic_getTransparent);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setTransparent', graphic_setTransparent);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'loadFromFile', graphic_loadFromFile);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'saveToFile', graphic_saveToFile);

  Luaclass_addPropertyToTable(L, metatable, userdata, 'Width', graphic_getWidth, graphic_setWidth);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Height', graphic_getHeight, graphic_setHeight);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Transparent', graphic_getTransparent, graphic_setTransparent);
end;

procedure initializeLuaGraphic;
begin
  lua_register(LuaVM, 'graphic_getWidth', graphic_getWidth);
  lua_register(LuaVM, 'graphic_setWidth', graphic_setWidth);
  lua_register(LuaVM, 'graphic_getHeight', graphic_getHeight);
  lua_register(LuaVM, 'graphic_setHeight', graphic_setHeight);

end;

initialization
  luaclass_register(TGraphic, graphic_addMetaData);

end.

