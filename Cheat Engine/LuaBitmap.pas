unit LuaBitmap;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, lua, lualib, lauxlib,LuaHandler;   ;

procedure initializeLuaBitmap;

implementation



{
unit LuaPen;

interface

uses
  Classes, SysUtils, Graphics,lua, lualib, lauxlib,LuaHandler;

procedure initializeLuaPen;

implementation

function pen_getColor(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  pen: TPen;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    pen:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, pen.Color);
    result:=1;
  end else lua_pop(L, parameters);
end;

function pen_setColor(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  pen: TPen;
  color: TColor;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    pen:=lua_touserdata(L,-parameters);
    color:=lua_tointeger(L, -parameters+1);
    lua_pop(L, parameters);

    pen.Color:=color;
  end else lua_pop(L, parameters);
end;

function pen_getWidth(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  pen: TPen;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    pen:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, pen.Width);
    result:=1;
  end else lua_pop(L, parameters);
end;

function pen_setWidth(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  pen: TPen;
  width: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    pen:=lua_touserdata(L,-parameters);
    width:=lua_tointeger(L, -parameters+1);
    lua_pop(L, parameters);

    pen.Width:=width;
  end else lua_pop(L, parameters);
end;



}

procedure initializeLuaBitmap;
begin
  lua_register(LuaVM, 'pen_getColor', pen_getColor);
  lua_register(LuaVM, 'pen_setColor', pen_setColor);
  lua_register(LuaVM, 'pen_getWidth', pen_getWidth);
  lua_register(LuaVM, 'pen_setWidth', pen_setWidth);

end;

end.

