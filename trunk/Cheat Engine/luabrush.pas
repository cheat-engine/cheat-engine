unit LuaBrush;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics,lua, lualib, lauxlib, LuaHandler;

procedure initializeLuaBrush;

implementation

function brush_getColor(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  brush: Tbrush;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    brush:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, brush.Color);
    result:=1;
  end else lua_pop(L, parameters);
end;

function brush_setColor(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  brush: Tbrush;
  color: TColor;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    brush:=lua_touserdata(L,-parameters);
    color:=lua_tointeger(L, -parameters+1);
    lua_pop(L, parameters);

    brush.Color:=color;
  end else lua_pop(L, parameters);
end;

procedure initializeLuaBrush;
begin
  lua_register(LuaVM, 'brush_getColor', brush_getColor);
  lua_register(LuaVM, 'brush_setColor', brush_setColor);
end;

end.

