unit LuaPen;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics,lua, lualib, lauxlib,LuaHandler;

procedure initializeLuaPen;

implementation

uses luaclass, LuaObject;

function pen_getColor(L: PLua_State): integer; cdecl;
var
  pen: TPen;
begin
  pen:=luaclass_getClassObject(L);
  lua_pushinteger(L, pen.Color);
  result:=1;
end;

function pen_setColor(L: PLua_State): integer; cdecl;
var
  pen: TPen;
begin
  pen:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    pen.color:=lua_tointeger(L, -1);
  result:=1;
end;

function pen_getWidth(L: PLua_State): integer; cdecl;
var
  pen: TPen;
begin
  pen:=luaclass_getClassObject(L);
  lua_pushinteger(L, pen.Width);
  result:=1;
end;

function pen_setWidth(L: PLua_State): integer; cdecl;
var
  pen: TPen;
begin
  pen:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    pen.Width:=lua_tointeger(L, -1);
  result:=1;
end;

procedure pen_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getColor', pen_getColor);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setColor', pen_setColor);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getWidth', pen_getWidth);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setWidth', pen_setWidth);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Color', pen_getColor, pen_setColor);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Width', pen_getWidth, pen_setWidth);
end;

procedure initializeLuaPen;
begin
  lua_register(LuaVM, 'pen_getColor', pen_getColor);
  lua_register(LuaVM, 'pen_setColor', pen_setColor);
  lua_register(LuaVM, 'pen_getWidth', pen_getWidth);
  lua_register(LuaVM, 'pen_setWidth', pen_setWidth);

end;

initialization
  luaclass_register(TPen, pen_addMetaData);

end.

