unit LuaBinary;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua;

procedure initializeLuaBinary;

implementation

uses luahandler;

function bOr(L: PLua_state): integer; cdecl;
begin
  result:=1;
  if lua_gettop(L)=2 then
    lua_pushinteger(L, lua_tointeger(L, 1) or lua_tointeger(L,2))
  else
    raise exception.create(rsIncorrectNumberOfParameters);
end;

function bXor(L: PLua_state): integer; cdecl;
begin
  result:=1;
  if lua_gettop(L)=2 then
    lua_pushinteger(L, lua_tointeger(L, 1) xor lua_tointeger(L,2))
  else
    raise exception.create(rsIncorrectNumberOfParameters);
end;

function bAnd(L: PLua_state): integer; cdecl;
begin
  result:=1;
  if lua_gettop(L)=2 then
    lua_pushinteger(L, lua_tointeger(L, 1) and lua_tointeger(L,2))
  else
    raise exception.create(rsIncorrectNumberOfParameters);
end;

function bShl(L: PLua_state): integer; cdecl;
begin
  result:=1;
  if lua_gettop(L)=2 then
    lua_pushinteger(L, lua_tointeger(L, 1) shl lua_tointeger(L,2))
  else
    raise exception.create(rsIncorrectNumberOfParameters);
end;

function bShr(L: PLua_state): integer; cdecl;
begin
  result:=1;
  if lua_gettop(L)=2 then
    lua_pushinteger(L, lua_tointeger(L, 1) shr lua_tointeger(L,2))
  else
    raise exception.create(rsIncorrectNumberOfParameters);
end;

function bNot(L: PLua_state): integer; cdecl;
begin
  result:=1;
  if lua_gettop(L)=1 then
    lua_pushinteger(L, not lua_tointeger(L, 1))
  else
    raise exception.create(rsIncorrectNumberOfParameters);
end;

procedure initializeLuaBinary;
begin
  lua_register(LuaVM, 'bOr', bOr);
  lua_register(LuaVM, 'bXor', bXor);
  lua_register(LuaVM, 'bAnd', bAnd);
  lua_register(LuaVM, 'bShl', bShl);
  lua_register(LuaVM, 'bShr', bShr);
  lua_register(LuaVM, 'bNot', bNot);
end;

end.

