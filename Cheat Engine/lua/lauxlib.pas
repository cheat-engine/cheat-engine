(******************************************************************************
 *                                                                            *
 *  File:        lauxlib.pas                                                  *
 *  Authors:     TeCGraf           (C headers + actual Lua libraries)         *
 *               Lavergne Thomas   (original translation to Pascal)           *
 *               Bram Kuijvenhoven (update to Lua 5.1.1 for FreePascal)       *
 *  Description: Lua auxiliary library                                        *
 *                                                                            *
 ******************************************************************************)

(*
** $Id: lauxlib.h,v 1.59 2003/03/18 12:25:32 roberto Exp $
** Auxiliary functions for building Lua libraries
** See Copyright Notice in lua.h
*)
(*
** Translated to pascal by Lavergne Thomas
** Notes :
**    - Pointers type was prefixed with 'P'
** Bug reports :
**    - thomas.lavergne@laposte.net
**   In french or in english
*)

{$IFDEF FPC}{$MODE OBJFPC}{$H+}{$ENDIF}

unit lauxlib;

interface

uses
  Lua{$ifdef darwin},mactypes{$endif};

// functions added for Pascal
procedure lua_pushstring(L: Plua_State; const s: string);

// compatibilty macros
function luaL_getn(L: Plua_State; n: Integer): Integer; // calls lua_objlen
procedure luaL_setn(L: Plua_State; t, n: Integer); // does nothing!

type
  luaL_reg = record
    name: PChar;
    func: lua_CFunction;
  end;
  PluaL_reg = ^luaL_reg;

procedure luaL_openlib(L: Plua_State; const libname: PChar; const lr: PluaL_reg; nup: Integer); cdecl;
procedure luaL_register(L: Plua_State; const libname: PChar; const lr: PluaL_reg); cdecl;
function luaL_getmetafield(L: Plua_State; obj: Integer; const e: PChar): Integer; cdecl;
function luaL_callmeta(L: Plua_State; obj: Integer; const e: PChar): Integer; cdecl;
function luaL_argerror(L: Plua_State; numarg: Integer; const extramsg: PChar): Integer; cdecl;
function luaL_checklstring(L: Plua_State; numArg: Integer; l_: Psize_t): PChar; cdecl;
function luaL_optlstring(L: Plua_State; numArg: Integer; const def: PChar; l_: Psize_t): PChar; cdecl;
function luaL_checknumber(L: Plua_State; numArg: Integer): lua_Number; cdecl;
function luaL_optnumber(L: Plua_State; nArg: Integer; def: lua_Number): lua_Number; cdecl;
function luaL_checkinteger(L: Plua_State; numArg: Integer): lua_Integer; cdecl;
function luaL_optinteger(L: Plua_State; nArg: Integer; def: lua_Integer): lua_Integer; cdecl;

procedure luaL_checkstack(L: Plua_State; sz: Integer; const msg: PChar); cdecl;
procedure luaL_checktype(L: Plua_State; narg, t: Integer); cdecl;
procedure luaL_checkany(L: Plua_State; narg: Integer); cdecl;

function luaL_newmetatable(L: Plua_State; const tname: PChar): Integer; cdecl;
function luaL_checkudata(L: Plua_State; ud: Integer; const tname: PChar): Pointer; cdecl;

procedure luaL_where(L: Plua_State; lvl: Integer); cdecl;
//function luaL_error(L: Plua_State; const fmt: PChar; args: array of const): Integer; CDecl; // note: C's ... to array of const conversion is not portable to Delphi

function luaL_checkoption(L: Plua_State; narg: Integer; def: PChar; lst: PPChar): Integer; cdecl;

function luaL_ref(L: Plua_State; t: Integer): Integer; cdecl;
procedure luaL_unref(L: Plua_State; t, ref: Integer); cdecl;

function luaL_loadfile(L: Plua_State; const filename: PChar): Integer; cdecl;
function luaL_loadbuffer(L: Plua_State; const buff: PChar; size: size_t; const name: PChar): Integer; cdecl;
function luaL_loadstring(L: Plua_State; const s: PChar): Integer; cdecl;

function luaL_newstate: Plua_State; cdecl;
function lua_open: Plua_State; // compatibility; moved from unit lua to lauxlib because it needs luaL_newstate

function luaL_gsub(L: Plua_State; const s, p, r: PChar): PChar; cdecl;
function luaL_findtable(L: Plua_State; idx: Integer; const fname: PChar; szhint: Integer): PChar; cdecl;

(*
** ===============================================================
** some useful macros
** ===============================================================
*)

procedure luaL_argcheck(L: Plua_State; cond: Boolean; numarg: Integer; extramsg: PChar);
function luaL_checkstring(L: Plua_State; n: Integer): PChar;
function luaL_optstring(L: Plua_State; n: Integer; d: PChar): PChar;
function luaL_checkint(L: Plua_State; n: Integer): Integer;
function luaL_checklong(L: Plua_State; n: Integer): LongInt;
function luaL_optint(L: Plua_State; n: Integer; d: Double): Integer;
function luaL_optlong(L: Plua_State; n: Integer; d: Double): LongInt;

function luaL_typename(L: Plua_State; i: Integer): PChar;

function lua_dofile(L: Plua_State; const filename: PChar): Integer;
function lua_dostring(L: Plua_State; const str: PChar): Integer;

procedure lua_Lgetmetatable(L: Plua_State; tname: PChar);

// not translated:
// #define luaL_opt(L,f,n,d)	(lua_isnoneornil(L,(n)) ? (d) : f(L,(n)))


(*
** =======================================================
** Generic Buffer manipulation
** =======================================================
*)

const
  // note: this is just arbitrary, as it related to the BUFSIZ defined in stdio.h ...
  LUAL_BUFFERSIZE = 4096;

type
  luaL_Buffer = record
    b: PChar;
    size: size_t;
    n: size_t;
    L: Plua_State;
    buffer: array [0..LUAL_BUFFERSIZE - 1] of Char; // warning: see note above about LUAL_BUFFERSIZE
  end;
  PluaL_Buffer = ^luaL_Buffer;

procedure luaL_addchar(B: PluaL_Buffer; c: Char); // warning: see note above about LUAL_BUFFERSIZE

(* compatibility only (alias for luaL_addchar) *)
procedure luaL_putchar(B: PluaL_Buffer; c: Char); // warning: see note above about LUAL_BUFFERSIZE

procedure luaL_addsize(B: PluaL_Buffer; n: Integer);

procedure luaL_buffinit(L: Plua_State; B: PluaL_Buffer); cdecl;
function luaL_prepbuffer(B: PluaL_Buffer): PChar; cdecl;
procedure luaL_addlstring(B: PluaL_Buffer; const s: PChar; l: size_t); cdecl;
procedure luaL_addstring(B: PluaL_Buffer; const s: PChar); cdecl;
procedure luaL_addvalue(B: PluaL_Buffer); cdecl;
procedure luaL_pushresult(B: PluaL_Buffer); cdecl;


(* compatibility with ref system *)

(* pre-defined references *)
const
  LUA_NOREF  = -2;
  LUA_REFNIL = -1;
  
procedure lua_unref(L: Plua_State; ref: Integer);
procedure lua_getref(L: Plua_State; ref: Integer);

(*
** Compatibility macros and functions
*)

function luaL_check_lstr(L: Plua_State; numArg: Integer; len: Psize_t): PChar;
function luaL_opt_lstr(L: Plua_State; numArg: Integer; const def: PChar; len: Psize_t): PChar;
function luaL_check_number(L: Plua_State; numArg: Integer): lua_Number;
function luaL_opt_number(L: Plua_State; nArg: Integer; def: lua_Number): lua_Number;
procedure luaL_arg_check(L: Plua_State; cond: Boolean; numarg: Integer; extramsg: PChar);
function luaL_check_string(L: Plua_State; n: Integer): PChar;
function luaL_opt_string(L: Plua_State; n: Integer; d: PChar): PChar;
function luaL_check_int(L: Plua_State; n: Integer): Integer;
function luaL_check_long(L: Plua_State; n: Integer): LongInt;
function luaL_opt_int(L: Plua_State; n: Integer; d: Double): Integer;
function luaL_opt_long(L: Plua_State; n: Integer; d: Double): LongInt;

implementation

procedure lua_pushstring(L: Plua_State; const s: string);
begin
  lua_pushlstring(L, PChar(s), Length(s));
end;

function luaL_getn(L: Plua_State; n: Integer): Integer;
begin
  Result := lua_objlen(L, n);
end;

procedure luaL_setn(L: Plua_State; t, n: Integer);
begin
  // does nothing as this operation is deprecated
end;

procedure luaL_openlib(L: Plua_State; const libname: PChar; const lr: PluaL_reg; nup: Integer); cdecl; external LUA_LIB_NAME;
procedure luaL_register(L: Plua_State; const libname: PChar; const lr: PluaL_reg); cdecl;
begin
  luaL_openlib(L, libname, lr,0);
end;

function luaL_getmetafield(L: Plua_State; obj: Integer; const e: PChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_callmeta(L: Plua_State; obj: Integer; const e: PChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_typerror(L: Plua_State; narg: Integer; const tname: PChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_argerror(L: Plua_State; numarg: Integer; const extramsg: PChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_checklstring(L: Plua_State; numArg: Integer; l_: Psize_t): PChar; cdecl; external LUA_LIB_NAME;
function luaL_optlstring(L: Plua_State; numArg: Integer; const def: PChar; l_: Psize_t): PChar; cdecl; external LUA_LIB_NAME;
function luaL_checknumber(L: Plua_State; numArg: Integer): lua_Number; cdecl; external LUA_LIB_NAME;
function luaL_optnumber(L: Plua_State; nArg: Integer; def: lua_Number): lua_Number; cdecl; external LUA_LIB_NAME;
function luaL_checkinteger(L: Plua_State; numArg: Integer): lua_Integer; cdecl; external LUA_LIB_NAME;
function luaL_optinteger(L: Plua_State; nArg: Integer; def: lua_Integer): lua_Integer; cdecl; external LUA_LIB_NAME;

procedure luaL_checkstack(L: Plua_State; sz: Integer; const msg: PChar); cdecl; external LUA_LIB_NAME;
procedure luaL_checktype(L: Plua_State; narg, t: Integer); cdecl; external LUA_LIB_NAME;
procedure luaL_checkany(L: Plua_State; narg: Integer); cdecl; external LUA_LIB_NAME;

function luaL_newmetatable(L: Plua_State; const tname: PChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_checkudata(L: Plua_State; ud: Integer; const tname: PChar): Pointer; cdecl; external LUA_LIB_NAME;

procedure luaL_where(L: Plua_State; lvl: Integer); cdecl; external LUA_LIB_NAME;
function luaL_error(L: Plua_State; const fmt: PChar; args: array of const): Integer; cdecl; external LUA_LIB_NAME;

function luaL_checkoption(L: Plua_State; narg: Integer; def: PChar; lst: PPChar): Integer; cdecl; external LUA_LIB_NAME;

function luaL_ref(L: Plua_State; t: Integer): Integer; cdecl; external LUA_LIB_NAME;
procedure luaL_unref(L: Plua_State; t, ref: Integer); cdecl; external LUA_LIB_NAME;

function luaL_loadfilex(L: Plua_State; const filename: PChar; const mode: pchar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_loadfile(L: Plua_State; const filename: PChar): Integer; cdecl;
begin
  result:=luaL_loadfilex(L,filename,nil);
end;

function luaL_loadbuffer(L: Plua_State; const buff: PChar; size: size_t; const name: PChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_loadstring(L: Plua_State; const s: PChar): Integer; cdecl; external LUA_LIB_NAME;

function luaL_newstate: Plua_State; cdecl; external LUA_LIB_NAME;

function lua_open: Plua_State;
begin
  Result := luaL_newstate;
end;

function luaL_gsub(L: Plua_State; const s, p, r: PChar): PChar; cdecl; external LUA_LIB_NAME;
function luaL_findtable(L: Plua_State; idx: Integer; const fname: PChar; szhint: Integer): PChar; cdecl; external LUA_LIB_NAME;

function luaL_typename(L: Plua_State; i: Integer): PChar;
begin
  Result := lua_typename(L, lua_type(L, i));
end;

function lua_dofile(L: Plua_State; const filename: PChar): Integer;
begin
  Result := luaL_loadfile(L, filename);
  if Result = 0 then
    Result := lua_pcall(L, 0, LUA_MULTRET, 0);
end;

function lua_dostring(L: Plua_State; const str: PChar): Integer;
begin
  Result := luaL_loadstring(L, str);
  if Result = 0 then
    Result := lua_pcall(L, 0, LUA_MULTRET, 0);
end;

procedure lua_Lgetmetatable(L: Plua_State; tname: PChar);
begin
  lua_getfield(L, LUA_REGISTRYINDEX, tname);
end;

procedure luaL_argcheck(L: Plua_State; cond: Boolean; numarg: Integer; extramsg: PChar);
begin
  if not cond then
    luaL_argerror(L, numarg, extramsg)
end;

function luaL_checkstring(L: Plua_State; n: Integer): PChar;
begin
  Result := luaL_checklstring(L, n, nil)
end;

function luaL_optstring(L: Plua_State; n: Integer; d: PChar): PChar;
begin
  Result := luaL_optlstring(L, n, d, nil)
end;

function luaL_checkint(L: Plua_State; n: Integer): Integer;
begin
  Result := Integer(Trunc(luaL_checknumber(L, n)))
end;

function luaL_checklong(L: Plua_State; n: Integer): LongInt;
begin
  Result := LongInt(Trunc(luaL_checknumber(L, n)))
end;

function luaL_optint(L: Plua_State; n: Integer; d: Double): Integer;
begin
  Result := Integer(Trunc(luaL_optnumber(L, n, d)))
end;

function luaL_optlong(L: Plua_State; n: Integer; d: Double): LongInt;
begin
  Result := LongInt(Trunc(luaL_optnumber(L, n, d)))
end;

procedure luaL_addchar(B: PluaL_Buffer; c: Char);
begin
  if ptruint(@(B^.b)) < (ptruint(@(B^.buffer[0])) + LUAL_BUFFERSIZE) then
    luaL_prepbuffer(B);
  B^.b[1] := c;
  B^.b := B^.b + 1;
end;

procedure luaL_putchar(B: PluaL_Buffer; c: Char);
begin
  luaL_addchar(B, c);
end;

procedure luaL_addsize(B: PluaL_Buffer; n: Integer);
begin
  B^.b := B^.b + n;
end;

procedure luaL_buffinit(L: Plua_State ; B: PluaL_Buffer); cdecl; external LUA_LIB_NAME;
function luaL_prepbuffer(B: PluaL_Buffer): PChar; cdecl; external LUA_LIB_NAME;
procedure luaL_addlstring(B: PluaL_Buffer; const s: PChar; l: size_t); cdecl; external LUA_LIB_NAME;
procedure luaL_addstring(B: PluaL_Buffer; const s: PChar); cdecl; external LUA_LIB_NAME;
procedure luaL_addvalue(B: PluaL_Buffer); cdecl; external LUA_LIB_NAME;
procedure luaL_pushresult(B: PluaL_Buffer); cdecl; external LUA_LIB_NAME;

procedure lua_unref(L: Plua_State; ref: Integer);
begin
  luaL_unref(L, LUA_REGISTRYINDEX, ref);
end;

procedure lua_getref(L: Plua_State; ref: Integer);
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, ref);
end;

function luaL_check_lstr(L: Plua_State; numArg: Integer; len: Psize_t): PChar;
begin
  Result := luaL_checklstring(L, numArg, len);
end;

function luaL_opt_lstr(L: Plua_State; numArg: Integer; const def: PChar; len: Psize_t): PChar;
begin
  Result := luaL_optlstring(L, numArg, def, len);
end;

function luaL_check_number(L: Plua_State; numArg: Integer): lua_Number;
begin
  Result := luaL_checknumber(L, numArg);
end;

function luaL_opt_number(L: Plua_State; nArg: Integer; def: lua_Number): lua_Number;
begin
  Result := luaL_optnumber(L, nArg, def);
end;

procedure luaL_arg_check(L: Plua_State; cond: Boolean; numarg: Integer; extramsg: PChar);
begin
  luaL_argcheck(L, cond, numarg, extramsg);
end;

function luaL_check_string(L: Plua_State; n: Integer): PChar;
begin
  Result := luaL_checkstring(L, n);
end;

function luaL_opt_string(L: Plua_State; n: Integer; d: PChar): PChar;
begin
  Result := luaL_optstring(L, n, d);
end;

function luaL_check_int(L: Plua_State; n: Integer): Integer;
begin
  Result := luaL_checkint(L, n);
end;

function luaL_check_long(L: Plua_State; n: Integer): LongInt;
begin
  Result := luaL_checklong(L, n);
end;

function luaL_opt_int(L: Plua_State; n: Integer; d: Double): Integer;
begin
  Result := luaL_optint(L, n, d);
end;

function luaL_opt_long(L: Plua_State; n: Integer; d: Double): LongInt;
begin
  Result := luaL_optlong(L, n, d);
end;

end.
