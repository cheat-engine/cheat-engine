unit LuaListcolumn;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lauxlib, lualib, ComCtrls;

procedure initializeLuaListcolumn;

implementation

uses luahandler, LuaClass, LuaCollectionItem;

function listcolumn_getAutosize(L: PLua_State): integer; cdecl;
var
  listcolumn: TListColumn;
begin
  listcolumn:=luaclass_getClassObject(L);
  lua_pushboolean(L, listcolumn.AutoSize);
  result:=1;
end;

function listcolumn_setAutosize(L: PLua_State): integer; cdecl;
var
  listcolumn: TListColumn;
begin
  result:=0;
  listcolumn:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
    listcolumn.AutoSize:=lua_toboolean(L,-1);
end;


function listcolumn_getCaption(L: PLua_State): integer; cdecl;
var
  listcolumn: Tlistcolumn;
begin
  listcolumn:=luaclass_getClassObject(L);
  lua_pushstring(L, listcolumn.caption);
  result:=1;
end;

function listcolumn_setCaption(L: PLua_State): integer; cdecl;
var
  listcolumn: Tlistcolumn;
begin
  result:=0;
  listcolumn:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    listcolumn.caption:=Lua_ToString(L,-1);
end;

function listcolumn_getMaxWidth(L: PLua_State): integer; cdecl;
var
  listcolumn: Tlistcolumn;
begin
  listcolumn:=luaclass_getClassObject(L);
  lua_pushinteger(L, listcolumn.MaxWidth);
  result:=1;
end;

function listcolumn_setMaxWidth(L: PLua_State): integer; cdecl;
var
  listcolumn: Tlistcolumn;
begin
  result:=0;
  listcolumn:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    listcolumn.MaxWidth:=lua_tointeger(L,-1);
end;


function listcolumn_getMinWidth(L: PLua_State): integer; cdecl;
var
  listcolumn: Tlistcolumn;
begin
  listcolumn:=luaclass_getClassObject(L);
  lua_pushinteger(L, listcolumn.MinWidth);
  result:=1;
end;

function listcolumn_setMinWidth(L: PLua_State): integer; cdecl;
var
  listcolumn: Tlistcolumn;
begin
  result:=0;
  listcolumn:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    listcolumn.MinWidth:=lua_tointeger(L,-1);
end;

function listcolumn_getWidth(L: PLua_State): integer; cdecl;
var
  listcolumn: Tlistcolumn;
begin
  listcolumn:=luaclass_getClassObject(L);
  lua_pushinteger(L, listcolumn.Width);
  result:=1;
end;

function listcolumn_setWidth(L: PLua_State): integer; cdecl;
var
  listcolumn: Tlistcolumn;
begin
  result:=0;
  listcolumn:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    listcolumn.Width:=lua_tointeger(L,-1);
end;

procedure listcolumn_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  collectionItem_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getAutosize', listcolumn_getAutosize);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setAutosize', listcolumn_setAutosize);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCaption', listcolumn_getCaption);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setCaption', listcolumn_setCaption);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getMaxWidth', listcolumn_getMaxWidth);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setMaxWidth', listcolumn_setMaxWidth);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getMinWidth', listcolumn_getMinWidth);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setMinWidth', listcolumn_setMinWidth);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getWidth', listcolumn_getWidth);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setWidth', listcolumn_setWidth);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Autosize', listcolumn_getAutosize, listcolumn_setAutosize);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Caption', listcolumn_getCaption, listcolumn_setCaption);
  luaclass_addPropertyToTable(L, metatable, userdata, 'MaxWidth', listcolumn_getMaxWidth, listcolumn_setMaxWidth);
  luaclass_addPropertyToTable(L, metatable, userdata, 'MinWidth', listcolumn_getMinWidth, listcolumn_setMinWidth);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Width', listcolumn_getWidth, listcolumn_setWidth);
end;

procedure initializeLuaListcolumn;
begin
  lua_register(LuaVM, 'listcolumn_setAutosize', listcolumn_setAutosize);
  lua_register(LuaVM, 'listcolumn_getCaption', listcolumn_getCaption);
  lua_register(LuaVM, 'listcolumn_setCaption', listcolumn_setCaption);
  lua_register(LuaVM, 'listcolumn_getMaxWidth', listcolumn_getMaxWidth);
  lua_register(LuaVM, 'listcolumn_setMaxWidth', listcolumn_setMaxWidth);
  lua_register(LuaVM, 'listcolumn_getMinWidth', listcolumn_getMinWidth);
  lua_register(LuaVM, 'listcolumn_setMinWidth', listcolumn_setMinWidth);
  lua_register(LuaVM, 'listcolumn_getWidth', listcolumn_getWidth);
  lua_register(LuaVM, 'listcolumn_setWidth', listcolumn_setWidth);
end;

end.

