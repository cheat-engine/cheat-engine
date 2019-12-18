(******************************************************************************
 *                                                                            *
 *  File:        lua.pas                                                      *
 *  Authors:     TeCGraf           (C headers + actual Lua libraries)         *
 *               Lavergne Thomas   (original translation to Pascal)           *
 *               Bram Kuijvenhoven (update to Lua 5.1.1 for FreePascal)       *
 *  Description: Basic Lua library                                            *
 *                                                                            *
 ******************************************************************************)

(*
** $Id: lua.h,v 1.175 2003/03/18 12:31:39 roberto Exp $
** Lua - An Extensible Extension Language
** TeCGraf: Computer Graphics Technology Group, PUC-Rio, Brazil
** http://www.lua.org   mailto:info@lua.org
** See Copyright Notice at the end of this file
*)
(*
** Updated to Lua 5.1.1 by Bram Kuijvenhoven (bram at kuijvenhoven dot net),
**   Hexis BV (http://www.hexis.nl), the Netherlands
** Notes:
**    - Only tested with FPC (FreePascal Compiler)
**    - Using LuaBinaries styled DLL/SO names, which include version names
**    - LUA_YIELD was suffixed by '_' for avoiding name collision
*)
(*
** Translated to pascal by Lavergne Thomas
** Notes :
**    - Pointers type was prefixed with 'P'
**    - lua_upvalueindex constant was transformed to function
**    - Some compatibility function was isolated because with it you must have
**      lualib.
**    - LUA_VERSION was suffixed by '_' for avoiding name collision.
** Bug reports :
**    - thomas.lavergne@laposte.net
**   In french or in english
**
** Updated to Lua 5.3 by Eric Heijnen (cheatengine.org)

*)

{$IFDEF FPC}{$MODE OBJFPC}{$H+}{$ENDIF}

unit lua;

interface

{$ifdef darwin}
uses dialogs, MacOSAll, MacOSXPosix, dynlibs, fileutil;

{$endif}

const
{$IFDEF UNIX}
  {$ifdef darwin}
  LUA_NAME = 'liblua53.dylib';
  LUA_LIB_NAME = 'liblua53.dylib';
  {$else}
  LUA_NAME = 'liblua5.3.so';
  LUA_LIB_NAME = 'liblua5.3.so';
  {$endif}
{$ELSE}
  {$ifdef cpu64}
    LUA_NAME = 'lua53-64.dll';
    LUA_LIB_NAME = 'lua53-64.dll';
  {$else}
  LUA_NAME = 'lua53-32.dll';
  LUA_LIB_NAME = 'lua53-32.dll';
  {$endif}
{$ENDIF}

type
  Psize_t = ^size_t;

const
  LUA_VERSION = 'Lua 5.3';
  LUA_RELEASE = 'Lua 5.3.1';
  LUA_VERSION_NUM = 503;
  LUA_COPYRIGHT = 'Copyright (C) 1994-2015 Lua.org, PUC-Rio';
  LUA_AUTHORS = 'R. Ierusalimschy, L. H. de Figueiredo, W. Celes';

(* option for multiple returns in `lua_pcall' and `lua_call' *)
  LUA_MULTRET = -1;

(*
** pseudo-indices
*)

  LUAI_MAXSTACK=1000000;
  LUAI_FIRSTPSEUDOIDX=(-LUAI_MAXSTACK - 1000);

  LUA_REGISTRYINDEX = LUAI_FIRSTPSEUDOIDX;
  //LUA_ENVIRONINDEX  = -10001;
  //LUA_GLOBALSINDEX  = -10002;

function lua_upvalueindex(I: Integer): Integer;

const
(* thread status; 0 is OK *)
  LUA_YIELD_    = 1;
  LUA_ERRRUN    = 2;
  LUA_ERRSYNTAX = 3;
  LUA_ERRMEM    = 4;
  LUA_ERRGCMM   = 5;
  LUA_ERRERR    = 6;

const
  LUA_OPEQ=	0;
  LUA_OPLT=	1;
  LUA_OPLE=	2;

type
  Plua_State = Pointer;

  lua_CFunction = function(L: Plua_State): Integer; cdecl;

(*
** functions that read/write blocks when loading/dumping Lua chunks
*)
type
  lua_Reader = function(L: Plua_State; ud: Pointer; sz: Psize_t): PChar; cdecl;
  lua_Writer = function(L: Plua_State; const p: Pointer; sz: size_t; ud: Pointer): Integer; cdecl;

(*
** prototype for memory-allocation functions
*)
  lua_Alloc = function(ud, ptr: Pointer; osize, nsize: size_t): Pointer; cdecl;

(*
** basic types
*)
const
  LUA_TNONE          = -1;

  LUA_TNIL           = 0;
  LUA_TBOOLEAN       = 1;
  LUA_TLIGHTUSERDATA = 2;
  LUA_TNUMBER        = 3;
  LUA_TSTRING        = 4;
  LUA_TTABLE         = 5;
  LUA_TFUNCTION      = 6;
  LUA_TUSERDATA      = 7;
  LUA_TTHREAD        = 8;
  LUA_NUMTAGS        = 9;


(* minimum Lua stack available to a C function *)
  LUA_MINSTACK = 20;

type
(* Type of Numbers in Lua *)
  lua_Number = Double;
  lua_Integer = Qword; //with the lnum patch

  //
  lua_KContext = IntPtr;
  lua_KFunction = function(L: plua_state; status: integer; ctx: lua_KContext): integer;

(*
** state manipulation
*)
function lua_newstate(f: lua_Alloc; ud: Pointer): Plua_state; cdecl;
procedure lua_close(L: Plua_State); cdecl;
function lua_newthread(L: Plua_State): Plua_State; cdecl;

function lua_atpanic(L: Plua_State; panicf: lua_CFunction): lua_CFunction; cdecl;

(*
** basic stack manipulation
*)
procedure lua_rotate(L: Plua_State; idx: integer; n: integer); cdecl;
procedure lua_copy(L: Plua_State; fromidx:integer; toidx: integer); cdecl;

function lua_absindex(L: PLua_State; idx: Integer): integer; cdecl;

function lua_gettop(L: Plua_State): Integer; cdecl;
procedure lua_settop(L: Plua_State; idx: Integer); cdecl;
procedure lua_pushvalue(L: Plua_State; Idx: Integer); cdecl;
procedure lua_remove(L: Plua_State; idx: Integer); cdecl;
procedure lua_insert(L: Plua_State; idx: Integer); cdecl;
procedure lua_replace(L: Plua_State; idx: Integer); cdecl;
function lua_checkstack(L: Plua_State; sz: Integer): LongBool; cdecl;

procedure lua_xmove(from, to_: Plua_State; n: Integer); cdecl;

(*
** access functions (stack -> C)
*)
function lua_isnumber(L: Plua_State; idx: Integer): LongBool; cdecl;
function lua_isinteger(L: Plua_State; idx: Integer): LongBool; cdecl;
function lua_isstring(L: Plua_State; idx: Integer): LongBool; cdecl;
function lua_iscfunction(L: Plua_State; idx: Integer): LongBool; cdecl;
function lua_isuserdata(L: Plua_State; idx: Integer): LongBool; cdecl;
function lua_type(L: Plua_State; idx: Integer): Integer; cdecl;
function lua_typename(L: Plua_State; tp: Integer): PChar; cdecl;

function lua_compare(L: Plua_State; idx1,idx2, op: integer): LongBool; cdecl;
function lua_equal(L: Plua_State; idx1, idx2: Integer): LongBool; cdecl;
function lua_rawequal(L: Plua_State; idx1, idx2: Integer): LongBool; cdecl;
function lua_lessthan(L: Plua_State; idx1, idx2: Integer): LongBool; cdecl;

function lua_tonumberx(L: Plua_State; idx: Integer; isnum: pinteger): lua_Number; cdecl;
function lua_tointegerx(L: Plua_State; idx: Integer; isnum: pinteger): lua_Integer; cdecl;

function lua_tonumber(L: Plua_State; idx: Integer): lua_Number; cdecl;
function lua_tointeger(L: Plua_State; idx: Integer): lua_Integer; cdecl;
function lua_toboolean(L: Plua_State; idx: Integer): LongBool; cdecl;
function lua_tolstring(L: Plua_State; idx: Integer; len: Psize_t): PChar; cdecl;
function lua_objlen(L: Plua_State; idx: Integer): size_t; cdecl;
function lua_tocfunction(L: Plua_State; idx: Integer): lua_CFunction; cdecl;
function lua_touserdata(L: Plua_State; idx: Integer): Pointer; cdecl;
function lua_tothread(L: Plua_State; idx: Integer): Plua_State; cdecl;
function lua_topointer(L: Plua_State; idx: Integer): Pointer; cdecl;

(*
** push functions (C -> stack)
*)
procedure lua_pushnil(L: Plua_State); cdecl;
procedure lua_pushnumber(L: Plua_State; n: lua_Number); cdecl;
procedure lua_pushinteger(L: Plua_State; n: lua_Integer); cdecl;
procedure lua_pushlstring(L: Plua_State; const s: PChar; l_: size_t); cdecl;
procedure lua_pushstring(L: Plua_State; const s: PChar); cdecl;
function lua_pushvfstring(L: Plua_State; const fmt: PChar; argp: Pointer): PChar; cdecl;
function lua_pushfstring(L: Plua_State; const fmt: PChar): PChar; cdecl; varargs;
procedure lua_pushcclosure(L: Plua_State; fn: lua_CFunction; n: Integer); cdecl;
procedure lua_pushboolean(L: Plua_State; b: LongBool); cdecl;
procedure lua_pushlightuserdata(L: Plua_State; p: Pointer); cdecl;
procedure lua_pushthread(L: Plua_State); cdecl;

(*
** get functions (Lua -> stack)
*)
procedure lua_gettable(L: Plua_State; idx: Integer); cdecl;
procedure lua_getfield(L: Plua_state; idx: Integer; k: PChar); cdecl;
procedure lua_rawget(L: Plua_State; idx: Integer); cdecl;
function lua_rawgeti(L: Plua_State; idx: integer; n: lua_Integer):integer; cdecl;
procedure lua_createtable(L: Plua_State; narr, nrec: Integer); cdecl;
function lua_newuserdata(L: Plua_State; sz: size_t): Pointer; cdecl; overload;
procedure lua_newuserdata(L: Plua_State; p: pointer); cdecl; overload;
function lua_getmetatable(L: Plua_State; objindex: Integer): Integer; cdecl;
procedure lua_getfenv(L: Plua_State; idx: Integer); cdecl;

(*
** set functions (stack -> Lua)
*)
procedure lua_settable(L: Plua_State; idx: Integer); cdecl;
procedure lua_setfield(L: Plua_State; idx: Integer; k: PChar); cdecl;
procedure lua_rawset(L: Plua_State; idx: Integer); cdecl;
procedure lua_rawseti(L: Plua_State; idx: integer; n: lua_Integer); cdecl;
function lua_setmetatable(L: Plua_State; objindex: Integer): Integer; cdecl;
function lua_setfenv(L: Plua_State; idx: Integer): Integer; cdecl;

(*
** `load' and `call' functions (load and run Lua code)
*)
procedure lua_call(L: Plua_State; nargs, nresults: Integer); cdecl;
function lua_pcall(L: Plua_State; nargs, nresults, errf: Integer): Integer; cdecl;

function lua_cpcall(L: Plua_State; func: lua_CFunction; ud: Pointer): Integer; cdecl;
function lua_load(L: Plua_State; reader: lua_Reader; dt: Pointer; const chunkname: PChar; const mode: pchar): Integer; cdecl;

function lua_dump(L: Plua_State; writer: lua_Writer; data: Pointer; strip: integer): Integer; cdecl;

(*
** coroutine functions
*)
function lua_yieldk(L: Plua_State; nresults: integer; ctx: lua_KContext; h: lua_KFunction): integer; cdecl;
function lua_yield(L: Plua_State; nresults: Integer): Integer; cdecl;
function lua_resume(L: Plua_State; narg: Integer): Integer; cdecl;
function lua_status(L: Plua_State): Integer; cdecl;

(*
** Garbage-collection functions and options
*)
const
  LUA_GCSTOP       = 0;
  LUA_GCRESTART    = 1;
  LUA_GCCOLLECT    = 2;
  LUA_GCCOUNT      = 3;
  LUA_GCCOUNTB     = 4;
  LUA_GCSTEP       = 5;
  LUA_GCSETPAUSE   = 6;
  LUA_GCSETSTEPMUL = 7;

function lua_gc(L: Plua_State; what, data: Integer): Integer; cdecl;

(*
** miscellaneous functions
*)
function lua_error(L: Plua_State): Integer; cdecl;

function lua_next(L: Plua_State; idx: Integer): Integer; cdecl;

procedure lua_concat(L: Plua_State; n: Integer); cdecl;

function lua_getallocf(L: Plua_State; ud: PPointer): lua_Alloc; cdecl;
procedure lua_setallocf(L: Plua_State; f: lua_Alloc; ud: Pointer); cdecl;

(*
** ===============================================================
** some useful macros
** ===============================================================
*)

procedure lua_pop(L: Plua_State; n: Integer);

procedure lua_newtable(L: Plua_state);

procedure lua_register(L: Plua_State; const n: PChar; f: lua_CFunction);
procedure lua_pushcfunction(L: Plua_State; f: lua_CFunction);

function lua_strlen(L: Plua_state; i: Integer): size_t;

function lua_isfunction(L: Plua_State; n: Integer): Boolean;
function lua_istable(L: Plua_State; n: Integer): Boolean;
function lua_islightuserdata(L: Plua_State; n: Integer): Boolean;
function lua_isheavyuserdata(L: Plua_State; n: Integer): Boolean;
function lua_isnil(L: Plua_State; n: Integer): Boolean;
function lua_isboolean(L: Plua_State; n: Integer): Boolean;
function lua_isthread(L: Plua_State; n: Integer): Boolean;
function lua_isnone(L: Plua_State; n: Integer): Boolean;
function lua_isnoneornil(L: Plua_State; n: Integer): Boolean;

procedure lua_pushliteral(L: Plua_State; s: PChar);

procedure lua_setglobal(L: Plua_State; const s: PChar); cdecl;
function lua_getglobal(L: Plua_State; const s: PChar): integer; cdecl;

function lua_tostring(L: Plua_State; i: Integer): PChar;

(*
** compatibility macros and functions
*)

procedure lua_getregistry(L: Plua_State);

function lua_getgccount(L: Plua_State): Integer;

type
  lua_Chunkreader = lua_Reader;
  lua_Chunkwriter = lua_Writer;

(*
** {======================================================================
** Debug API
** =======================================================================
*)

const
  LUA_HOOKCALL    = 0;
  LUA_HOOKRET     = 1;
  LUA_HOOKLINE    = 2;
  LUA_HOOKCOUNT   = 3;
  LUA_HOOKTAILRET = 4;

const
  LUA_MASKCALL  = 1 shl Ord(LUA_HOOKCALL);
  LUA_MASKRET   = 1 shl Ord(LUA_HOOKRET);
  LUA_MASKLINE  = 1 shl Ord(LUA_HOOKLINE);
  LUA_MASKCOUNT = 1 shl Ord(LUA_HOOKCOUNT);

const
  LUA_IDSIZE = 60;

type
  lua_Debug = record           (* activation record *)
    event: Integer;
    name: PChar;               (* (n) *)
    namewhat: PChar;           (* (n) `global', `local', `field', `method' *)
    what: PChar;               (* (S) `Lua', `C', `main', `tail'*)
    source: PChar;             (* (S) *)
    currentline: Integer;      (* (l) *)
    nups: Integer;             (* (u) number of upvalues *)
    linedefined: Integer;      (* (S) *)
    lastlinedefined: Integer;  (* (S) *)
    short_src: array[0..LUA_IDSIZE - 1] of Char; (* (S) *)
    (* private part *)
    i_ci: Integer;              (* active function *)
  end;
  Plua_Debug = ^lua_Debug;

  lua_Hook = procedure(L: Plua_State; ar: Plua_Debug); cdecl;

function lua_getstack(L: Plua_State; level: Integer; ar: Plua_Debug): Integer; cdecl;
function lua_getinfo(L: Plua_State; const what: PChar; ar: Plua_Debug): Integer; cdecl;
function lua_getlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PChar; cdecl;
function lua_setlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PChar; cdecl;
function lua_getupvalue(L: Plua_State; funcindex: Integer; n: Integer): PChar; cdecl;
function lua_setupvalue(L: Plua_State; funcindex: Integer; n: Integer): PChar; cdecl;

function lua_sethook(L: Plua_State; func: lua_Hook; mask: Integer; count: Integer): Integer; cdecl;
//function lua_gethook(L: Plua_State): lua_Hook; cdecl;
function lua_gethookmask(L: Plua_State): Integer; cdecl;
function lua_gethookcount(L: Plua_State): Integer; cdecl;

implementation

uses sysutils;


procedure lua_newuserdata(L: Plua_State; p: pointer); cdecl;
var r: ppointer;
begin
  r:=lua_newuserdata(L, sizeof(p));
  r^:=p;
end;

function lua_upvalueindex(I: Integer): Integer;
begin
  Result := LUA_REGISTRYINDEX - (i);
end;

function lua_newstate(f: lua_Alloc; ud: Pointer): Plua_State; cdecl; external LUA_NAME;
procedure lua_close(L: Plua_State); cdecl; external LUA_NAME;
function lua_newthread(L: Plua_State): Plua_State; cdecl; external LUA_NAME;

function lua_atpanic(L: Plua_State; panicf: lua_CFunction): lua_CFunction; cdecl; external LUA_NAME;

procedure lua_rotate(L: Plua_State; idx: integer; n: integer); cdecl; external LUA_NAME;
procedure lua_copy(L: Plua_State; fromidx:integer; toidx: integer); cdecl; external LUA_NAME;

function lua_rawlen(L: Plua_State; idx: integer):size_t; cdecl; external LUA_NAME;

function lua_absindex(L: PLua_State; idx: Integer): integer; cdecl; external LUA_NAME;
function lua_gettop(L: Plua_State): Integer; cdecl; external LUA_NAME;
procedure lua_settop(L: Plua_State; idx: Integer); cdecl; external LUA_NAME;
procedure lua_pushvalue(L: Plua_State; Idx: Integer); cdecl; external LUA_NAME;
procedure lua_remove(L: Plua_State; idx: Integer); cdecl;
begin
  lua_rotate(L, idx, -1);
  lua_pop(L, 1);
end;

procedure lua_insert(L: Plua_State; idx: Integer); cdecl;
begin
  lua_rotate(L, idx, 1);
end;

procedure lua_replace(L: Plua_State; idx: Integer); cdecl;
begin
  lua_copy(L, -1, idx);
  lua_pop(L, 1);
end;

function lua_checkstack(L: Plua_State; sz: Integer): LongBool; cdecl; external LUA_NAME;
procedure lua_xmove(from, to_: Plua_State; n: Integer); cdecl; external LUA_NAME;

function lua_isnumber(L: Plua_State; idx: Integer): LongBool; cdecl; external LUA_NAME;
function lua_isinteger(L: Plua_State; idx: Integer): LongBool; cdecl; external LUA_NAME;
function lua_isstring(L: Plua_State; idx: Integer): LongBool; cdecl; external LUA_NAME;
function lua_iscfunction(L: Plua_State; idx: Integer): LongBool; cdecl; external LUA_NAME;
function lua_isuserdata(L: Plua_State; idx: Integer): LongBool; cdecl; external LUA_NAME;
function lua_type(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_NAME;
function lua_typename(L: Plua_State; tp: Integer): PChar; cdecl; external LUA_NAME;

function lua_equal(L: Plua_State; idx1, idx2: Integer): LongBool; cdecl;
begin
  result:=lua_compare(L, idx1, idx2, LUA_OPEQ);
end;

function lua_compare(L: Plua_State; idx1,idx2, op: integer): LongBool; cdecl; external LUA_NAME;
function lua_rawequal(L: Plua_State; idx1, idx2: Integer): LongBool; cdecl; external LUA_NAME;

function lua_lessthan(L: Plua_State; idx1, idx2: Integer): LongBool; cdecl;
begin
  result:=lua_compare(L, idx1,idx2, LUA_OPLT);
end;

function lua_tonumberx(L: Plua_State; idx: Integer; isnum: pinteger): lua_Number; cdecl; external LUA_NAME;
function lua_tointegerx(L: Plua_State; idx: Integer; isnum: pinteger): lua_Integer; cdecl; external LUA_NAME;

function lua_tonumber(L: Plua_State; idx: Integer): lua_Number; cdecl;
begin
  result:=lua_tonumberx(L,idx,nil);
end;

function lua_tointeger(L: Plua_State; idx: Integer): lua_Integer; cdecl;
var
  isnum: Integer;
begin
  result:=lua_tointegerx(L,idx,@isnum);
  if isnum=0 then
  begin
    result:=trunc(lua_tonumberx(L,idx,@isnum));
    if isnum=0 then result:=0;
  end;
end;

function lua_toboolean(L: Plua_State; idx: Integer): LongBool; cdecl; external LUA_NAME;
function lua_tolstring(L: Plua_State; idx: Integer; len: Psize_t): PChar; cdecl; external LUA_NAME;
function lua_objlen(L: Plua_State; idx: Integer): size_t; cdecl;
begin
  result:=lua_rawlen(L, idx);
end;

function lua_tocfunction(L: Plua_State; idx: Integer): lua_CFunction; cdecl; external LUA_NAME;
function lua_touserdata(L: Plua_State; idx: Integer): Pointer; cdecl; external LUA_NAME;
function lua_tothread(L: Plua_State; idx: Integer): Plua_State; cdecl; external LUA_NAME;
function lua_topointer(L: Plua_State; idx: Integer): Pointer; cdecl; external LUA_NAME;

procedure lua_pushnil(L: Plua_State); cdecl; external LUA_NAME;
procedure lua_pushnumber(L: Plua_State; n: lua_Number); cdecl; external LUA_NAME;
procedure lua_pushinteger(L: Plua_State; n: lua_Integer); cdecl; external LUA_NAME;
procedure lua_pushlstring(L: Plua_State; const s: PChar; l_: size_t); cdecl; external LUA_NAME;
procedure lua_pushstring(L: Plua_State; const s: PChar); cdecl; external LUA_NAME;
function lua_pushvfstring(L: Plua_State; const fmt: PChar; argp: Pointer): PChar; cdecl; external LUA_NAME;
function lua_pushfstring(L: Plua_State; const fmt: PChar): PChar; cdecl; varargs; external LUA_NAME;
procedure lua_pushcclosure(L: Plua_State; fn: lua_CFunction; n: Integer); cdecl; external LUA_NAME;
procedure lua_pushboolean(L: Plua_State; b: LongBool); cdecl; external LUA_NAME;
procedure lua_pushlightuserdata(L: Plua_State; p: Pointer); cdecl; external LUA_NAME;
procedure lua_pushthread(L: Plua_State); cdecl; external LUA_NAME;

procedure lua_gettable(L: Plua_State; idx: Integer); cdecl; external LUA_NAME;
procedure lua_getfield(L: Plua_state; idx: Integer; k: PChar); cdecl; external LUA_NAME;
procedure lua_rawget(L: Plua_State; idx: Integer); cdecl; external LUA_NAME;
function lua_rawgeti(L: Plua_State; idx: integer; n: lua_Integer):integer; cdecl; external LUA_NAME;
procedure lua_createtable(L: Plua_State; narr, nrec: Integer); cdecl; external LUA_NAME;
function lua_newuserdata(L: Plua_State; sz: size_t): Pointer; cdecl; external LUA_NAME;
function lua_getmetatable(L: Plua_State; objindex: Integer): Integer; cdecl; external LUA_NAME;
procedure lua_getfenv(L: Plua_State; idx: Integer); cdecl; external LUA_NAME;

procedure lua_settable(L: Plua_State; idx: Integer); cdecl; external LUA_NAME;
procedure lua_setfield(L: Plua_State; idx: Integer; k: PChar); cdecl; external LUA_NAME;
procedure lua_rawset(L: Plua_State; idx: Integer); cdecl; external LUA_NAME;
procedure lua_rawseti(L: Plua_State; idx: integer; n: lua_Integer); cdecl; external LUA_NAME;
function lua_setmetatable(L: Plua_State; objindex: Integer): Integer; cdecl; external LUA_NAME;
function lua_setfenv(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_NAME;



procedure lua_callk(L: Plua_State; nargs, nresults: Integer; ctx: lua_KContext; k: lua_KFunction); cdecl; external LUA_NAME;

procedure lua_call(L: Plua_State; nargs, nresults: Integer); cdecl;
begin
  lua_callk(L, nargs, nresults, 0, nil);
end;

function lua_pcallk(L: Plua_State; nargs, nresults, errf: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; cdecl; external LUA_NAME;
function lua_pcall(L: Plua_State; nargs, nresults, errf: Integer): Integer; cdecl;
begin
  result:=lua_pcallk(L, nargs, nresults, errf, 0, nil);
end;

function lua_cpcall(L: Plua_State; func: lua_CFunction; ud: Pointer): Integer; cdecl;
begin
  lua_pushcfunction(L, func);
  lua_pushlightuserdata(L, ud);
  result:=lua_pcall(L,1,0,0);
end;

function lua_load(L: Plua_State; reader: lua_Reader; dt: Pointer; const chunkname: PChar; const mode: PChar): Integer; cdecl; external LUA_NAME;

function lua_dump(L: Plua_State; writer: lua_Writer; data: Pointer; strip: integer): Integer; cdecl; external LUA_NAME;

function lua_yieldk(L: Plua_State; nresults: integer; ctx: lua_KContext; h: lua_KFunction):integer; cdecl; external LUA_NAME;
function lua_yield(L: Plua_State; nresults: Integer): Integer; cdecl;
begin
  result:=lua_yieldk(L, nresults, 0, nil);
end;

function lua_resume(L: Plua_State; narg: Integer): Integer; cdecl; external LUA_NAME;
function lua_status(L: Plua_State): Integer; cdecl; external LUA_NAME;

function lua_gc(L: Plua_State; what, data: Integer): Integer; cdecl; external LUA_NAME;

function lua_error(L: Plua_State): Integer; cdecl; external LUA_NAME;
function lua_next(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_NAME;
procedure lua_concat(L: Plua_State; n: Integer); cdecl; external LUA_NAME;

function lua_getallocf(L: Plua_State; ud: PPointer): lua_Alloc; cdecl; external LUA_NAME;
procedure lua_setallocf(L: Plua_State; f: lua_Alloc; ud: Pointer); cdecl; external LUA_NAME;

procedure lua_pop(L: Plua_State; n: Integer);
begin
  lua_settop(L, -n - 1);
end;

procedure lua_newtable(L: Plua_State);
begin
  lua_createtable(L, 0, 0);
end;

procedure lua_register(L: Plua_State; const n: PChar; f: lua_CFunction);
begin
  lua_pushcfunction(L, f);
  lua_setglobal(L, n);
end;

procedure lua_pushcfunction(L: Plua_State; f: lua_CFunction);
begin
  lua_pushcclosure(L, f, 0);
end;

function lua_strlen(L: Plua_State; i: Integer): size_t;
begin
  Result := lua_objlen(L, i);
end;

function lua_isfunction(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TFUNCTION;
end;

function lua_istable(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TTABLE;
end;

function lua_islightuserdata(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TLIGHTUSERDATA;
end;

function lua_isheavyuserdata(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TUSERDATA;
end;


function lua_isnil(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TNIL;
end;

function lua_isboolean(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TBOOLEAN;
end;

function lua_isthread(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TTHREAD;
end;

function lua_isnone(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TNONE;
end;

function lua_isnoneornil(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) <= 0;
end;

procedure lua_pushliteral(L: Plua_State; s: PChar);
begin
  lua_pushlstring(L, s, Length(s));
end;

procedure lua_setglobal(L: Plua_State; const s: PChar); cdecl; external LUA_NAME;
function lua_getglobal(L: Plua_State; const s: PChar): integer; cdecl; external LUA_NAME;


function lua_tostring(L: Plua_State; i: Integer): PChar;
begin
  Result := lua_tolstring(L, i, nil);
end;


procedure lua_getregistry(L: Plua_State);
begin
  lua_pushvalue(L, LUA_REGISTRYINDEX);
end;

function lua_getgccount(L: Plua_State): Integer;
begin
  Result := lua_gc(L, LUA_GCCOUNT, 0);
end;

(*
** {======================================================================
** Debug API
** =======================================================================
*)

function lua_getstack(L: Plua_State; level: Integer; ar: Plua_Debug): Integer; cdecl; external LUA_NAME;
function lua_getinfo(L: Plua_State; const what: PChar; ar: Plua_Debug): Integer; cdecl; external LUA_NAME;
function lua_getlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PChar; cdecl; external LUA_NAME;
function lua_setlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PChar; cdecl; external LUA_NAME;
function lua_getupvalue(L: Plua_State; funcindex: Integer; n: Integer): PChar; cdecl; external LUA_NAME;
function lua_setupvalue(L: Plua_State; funcindex: Integer; n: Integer): PChar; cdecl; external LUA_NAME;
function lua_sethook(L: Plua_State; func: lua_Hook; mask: Integer; count: Integer): Integer; cdecl; external LUA_NAME;
//function lua_gethook(L: Plua_State): lua_Hook; cdecl; external LUA_NAME;
function lua_gethookmask(L: Plua_State): Integer; cdecl; external LUA_NAME;
function lua_gethookcount(L: Plua_State): Integer; cdecl; external LUA_NAME;


(******************************************************************************
* Copyright (C) 1994-2003 Tecgraf, PUC-Rio.  All rights reserved.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)
end.
