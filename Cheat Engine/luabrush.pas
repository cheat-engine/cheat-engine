unit LuaBrush;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics,lua, lualib, lauxlib, LuaHandler;

procedure initializeLuaBrush;

implementation

uses LuaClass, LuaObject;

function Brush_getColor(L: PLua_State): integer; cdecl;
var
  Brush: TBrush;
begin
  Brush:=luaclass_getClassObject(L);
  lua_pushinteger(L, Brush.Color);
  result:=1;
end;

function Brush_setColor(L: PLua_State): integer; cdecl;
var
  Brush: TBrush;
begin
  Brush:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    Brush.color:=lua_tointeger(L, -1);
  result:=1;
end;

procedure brush_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getColor', brush_getColor);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setColor', brush_setColor);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Color', brush_getColor, brush_setColor);
end;

procedure initializeLuaBrush;
begin
  lua_register(LuaVM, 'brush_getColor', brush_getColor);
  lua_register(LuaVM, 'brush_setColor', brush_setColor);
end;

initialization
  luaclass_register(TBrush, brush_addMetaData);

end.

