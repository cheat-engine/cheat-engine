unit LuaFont;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics,lua, lualib, lauxlib, LuaHandler;

procedure initializeLuaFont;

implementation

uses mainunit, LuaClass, LuaObject;

function createFont(L: Plua_State): integer; cdecl;
var f: TFont;
begin
  result:=0;
  lua_pop(L, lua_gettop(L));
  f:=TFont.Create;
  f.assign(mainform.font); //initialize it with the best font there is...
  luaclass_newClass(L, f);
  result:=1;
end;

function Font_getColor(L: PLua_State): integer; cdecl;
var
  Font: TFont;
begin
  Font:=luaclass_getClassObject(L);
  lua_pushinteger(L, Font.Color);
  result:=1;
end;

function Font_setColor(L: PLua_State): integer; cdecl;
var
  Font: TFont;
begin
  Font:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    Font.color:=lua_tointeger(L, -1);
  result:=0;
end;

function Font_getSize(L: PLua_State): integer; cdecl;
var
  Font: TFont;
begin
  Font:=luaclass_getClassObject(L);
  lua_pushinteger(L, Font.Size);
  result:=1;
end;

function Font_setSize(L: PLua_State): integer; cdecl;
var
  Font: TFont;
begin
  Font:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    Font.Size:=lua_tointeger(L, -1);
  result:=0;
end;

function Font_getName(L: PLua_State): integer; cdecl;
var
  Font: TFont;
begin
  Font:=luaclass_getClassObject(L);
  lua_pushstring(L, Font.Name);
  result:=1;
end;

function Font_setName(L: PLua_State): integer; cdecl;
var
  Font: TFont;
begin
  Font:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    Font.Name:=Lua_ToString(L, -1);
  result:=0;
end;

function Font_assign(L: PLua_State): integer; cdecl;
var
  Font: TFont;
begin
  Font:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    Font.Assign(lua_ToCEUserData(L, 1));

  result:=0;
end;

procedure font_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getSize', font_getSize);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setSize', font_setSize);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getName', font_getName);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setName', font_setName);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getColor', font_getColor);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setColor', font_setColor);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'assign', font_assign);

  Luaclass_addPropertyToTable(L, metatable, userdata, 'Size', font_getSize, font_setSize);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Name', font_getName, font_setName);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Color', font_getColor, font_setColor);
end;

procedure initializeLuaFont;
begin

  lua_register(LuaVM, 'createFont', createFont);
  lua_register(LuaVM, 'font_getSize', font_getSize);
  lua_register(LuaVM, 'font_setSize', font_setSize);
  lua_register(LuaVM, 'font_getName', font_getName);
  lua_register(LuaVM, 'font_setName', font_setName);
  lua_register(LuaVM, 'font_getColor', font_getColor);
  lua_register(LuaVM, 'font_setColor', font_setColor);
end;

initialization
  luaclass_register(TFont, font_addMetaData);

end.

