(******************************************************************************
 *                                                                            *
 *  File:        lualib.pas                                                   *
 *  Authors:     TeCGraf           (C headers + actual Lua libraries)         *
 *               Lavergne Thomas   (original translation to Pascal)           *
 *               Bram Kuijvenhoven (update to Lua 5.1.1 for FreePascal)       *
 *  Description: Standard Lua libraries                                       *
 *                                                                            *
 ******************************************************************************)
 
(*
** $Id: lualib.h,v 1.28 2003/03/18 12:24:26 roberto Exp $
** Lua standard libraries
** See Copyright Notice in lua.h
*)
(*
** Translated to pascal by Lavergne Thomas
** Bug reports :
**    - thomas.lavergne@laposte.net
**   In french or in english
*)

{$IFDEF FPC}{$MODE OBJFPC}{$H+}{$ENDIF}

unit lualib;

interface

uses
  Lua;

const
  LUA_COLIBNAME = 'coroutine';
  LUA_TABLIBNAME = 'table';
  LUA_IOLIBNAME = 'io';
  LUA_OSLIBNAME = 'os';
  LUA_STRLINAME = 'string';
  LUA_MATHLIBNAME = 'math';
  LUA_DBLIBNAME = 'debug';
  LUA_LOADLIBNAME = 'package';

function luaopen_base(L: Plua_State): LongBool; cdecl;
function luaopen_table(L: Plua_State): LongBool; cdecl;
function luaopen_io(L: Plua_State): LongBool; cdecl;
function luaopen_string(L: Plua_State): LongBool; cdecl;
function luaopen_math(L: Plua_State): LongBool; cdecl;
function luaopen_debug(L: Plua_State): LongBool; cdecl;
function luaopen_package(L: Plua_State): LongBool; cdecl;

(* open all previous libraries *)
procedure luaL_openlibs(L: Plua_State); cdecl;

(* compatibility code *)

function lua_baselibopen(L: Plua_State): LongBool;
function lua_tablibopen(L: Plua_State): LongBool;
function lua_iolibopen(L: Plua_State): LongBool;
function lua_strlibopen(L: Plua_State): LongBool;
function lua_mathlibopen(L: Plua_State): LongBool;
function lua_dblibopen(L: Plua_State): LongBool;

implementation

function luaopen_base(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_table(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_io(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_string(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_math(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_debug(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_package(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;

procedure luaL_openlibs(L: Plua_State); cdecl; external LUA_LIB_NAME;

function lua_baselibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_base(L);
end;

function lua_tablibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_table(L);
end;

function lua_iolibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_io(L);
end;

function lua_strlibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_string(L);
end;

function lua_mathlibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_math(L);
end;

function lua_dblibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_debug(L);
end;

end.
