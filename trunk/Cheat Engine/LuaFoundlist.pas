unit LuaFoundlist;

{$mode delphi}

interface

uses
  Classes, SysUtils,lua, lualib, lauxlib, symbolhandler, LuaHandler, cefuncproc,
  memscan, foundlisthelper;

procedure initializeFoundlist;

implementation

function createFoundList(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  foundlist: TFoundlist;
  memscan: TMemScan;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
    memscan:=lua_touserdata(L, -1)
  else
    raise exception.create('createfoundlist(nil)');

  lua_pop(L, lua_gettop(L));

  foundlist:=TFoundList.create(nil, memscan);

  lua_pushlightuserdata(L, foundlist);
  result:=1;
end;

function foundlist_initialize(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  foundlist: Tfoundlist;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    foundlist:=lua_touserdata(L, -parameters);
    lua_pop(L, lua_gettop(L));

    foundlist.Initialize;
  end else lua_pop(L, lua_gettop(L));
end;

function foundlist_deinitialize(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  foundlist: Tfoundlist;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    foundlist:=lua_touserdata(L, -parameters);
    lua_pop(L, lua_gettop(L));

    foundlist.deinitialize;
  end else lua_pop(L, lua_gettop(L));
end;

function foundlist_getCount(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  foundlist: Tfoundlist;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    foundlist:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, foundlist.Count);
    result:=1;
  end else lua_pop(L, parameters);
end;

function foundlist_getAddress(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  foundlist: Tfoundlist;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    foundlist:=lua_touserdata(L,-2);
    index:=lua_tointeger(L,-1);
    lua_pop(L, parameters);


    lua_pushstring(L, inttohex(foundlist.GetAddress(index),8));
    result:=1;
  end else lua_pop(L, parameters);
end;

function foundlist_getValue(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  foundlist: Tfoundlist;
  b: dword;
  value: string;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    foundlist:=lua_touserdata(L,-2);
    index:=lua_tointeger(L,-1);
    lua_pop(L, parameters);

    foundlist.GetAddress(index, b, value);

    lua_pushstring(L, value);
    result:=1;
  end else lua_pop(L, parameters);
end;

procedure InitializeFoundlist;
begin
  Lua_register(LuaVM, 'createFoundList', createFoundList);
  Lua_register(LuaVM, 'foundlist_initialize', foundlist_initialize);
  Lua_register(LuaVM, 'foundlist_deinitialize', foundlist_deinitialize);
  Lua_register(LuaVM, 'foundlist_getCount', foundlist_getCount);
  Lua_register(LuaVM, 'foundlist_getAddress', foundlist_getAddress);
  Lua_register(LuaVM, 'foundlist_getValue', foundlist_getValue);
end;

end.

