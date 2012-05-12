unit LuaAddresslist;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, LuaHandler, cefuncproc;

procedure initializeLuaAddresslist;

implementation

uses memscan, addresslist, MemoryRecordUnit;


function addresslist_getCount(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  addresslist: TAddresslist;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    addresslist:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, addresslist.Count);
    result:=1;

  end else lua_pop(L, parameters);
end;

function addresslist_getSelectedRecords(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  addresslist: TAddresslist;
  i,c: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    addresslist:=lua_touserdata(L,-1);


    lua_newtable(L);
    result:=1;

    c:=1; //seems lua tables prefer to start at 1 instead of 0
    for i:=0 to addresslist.Count-1 do
    begin
      if addresslist[i].isSelected then
      begin
        lua_pushinteger(L, c);
        lua_pushlightuserdata(L, addresslist[i]);
        lua_settable(L, -3);
        inc(c);
      end;
    end;

  end
  else lua_pop(L, parameters);
end;

function addresslist_getMemoryRecord(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  addresslist: TAddresslist;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    addresslist:=lua_touserdata(L,-2);
    index:=lua_tointeger(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, addresslist.MemRecItems[index]);
    result:=1;

  end else lua_pop(L, parameters);
end;

function addresslist_getMemoryRecordByDescription(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  addresslist: TAddresslist;
  description: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    addresslist:=lua_touserdata(L,-2);
    description:=Lua_ToString(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, addresslist.getRecordWithDescription(description));
    result:=1;

  end else lua_pop(L, parameters);
end;

function addresslist_getMemoryRecordByID(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  addresslist: TAddresslist;
  id: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    addresslist:=lua_touserdata(L,-2);
    id:=lua_tointeger(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, addresslist.getRecordWithID(id));
    result:=1;

  end else lua_pop(L, parameters);
end;

function addresslist_createMemoryRecord(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  addresslist: TAddresslist;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    addresslist:=lua_touserdata(L,1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L,   addresslist.addaddress(rsPluginAddress, '0', [], 0, vtDword));
    result:=1;

  end else lua_pop(L, parameters);
end;

function addresslist_doDescriptionChange(L: PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>=1 then
    TAddresslist(lua_touserdata(L,1)).doDescriptionChange;

  lua_pop(L, lua_gettop(L));
end;

function addresslist_doAddressChange(L: PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>=1 then
    TAddresslist(lua_touserdata(L,1)).doAddressChange;

  lua_pop(L, lua_gettop(L));
end;

function addresslist_doTypeChange(L: PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>=1 then
    TAddresslist(lua_touserdata(L,1)).doTypeChange;

  lua_pop(L, lua_gettop(L));
end;

function addresslist_doValueChange(L: PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>=1 then
    TAddresslist(lua_touserdata(L,1)).doValueChange;

  lua_pop(L, lua_gettop(L));
end;

function addresslist_getSelectedRecord(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  addresslist: TAddresslist;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    addresslist:=lua_touserdata(L,1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, addresslist.selectedRecord);
    result:=1;
  end else lua_pop(L, parameters);
end;

function addresslist_setSelectedRecord(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  addresslist: TAddresslist;
  r: TMemoryRecord;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    addresslist:=lua_touserdata(L,1);
    r:=lua_touserdata(L,2);
    lua_pop(L, parameters);

    addresslist.selectedRecord:=r;
  end else lua_pop(L, parameters);
end;

procedure initializeLuaAddresslist;
begin
  Lua_register(LuaVM, 'addresslist_getCount', addresslist_getCount);
  Lua_register(LuaVM, 'addresslist_getMemoryRecord', addresslist_getMemoryRecord);
  Lua_register(LuaVM, 'addresslist_getMemoryRecordByDescription', addresslist_getMemoryRecordByDescription);
  Lua_register(LuaVM, 'addresslist_getMemoryRecordByID', addresslist_getMemoryRecordByID);
  Lua_register(LuaVM, 'addresslist_createMemoryRecord', addresslist_createMemoryRecord);
  Lua_register(LuaVM, 'addresslist_getSelectedRecords', addresslist_getSelectedRecords);

  Lua_register(LuaVM, 'addresslist_doDescriptionChange', addresslist_doDescriptionChange);
  Lua_register(LuaVM, 'addresslist_doAddressChange', addresslist_doAddressChange);
  Lua_register(LuaVM, 'addresslist_doTypeChange', addresslist_doTypeChange);
  Lua_register(LuaVM, 'addresslist_doValueChange', addresslist_doValueChange);
  Lua_register(LuaVM, 'addresslist_getSelectedRecord', addresslist_getSelectedRecord);
  Lua_register(LuaVM, 'addresslist_setSelectedRecord', addresslist_setSelectedRecord);
end;

end.

