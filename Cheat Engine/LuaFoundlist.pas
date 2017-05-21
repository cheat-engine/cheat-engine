unit LuaFoundlist;

{$mode delphi}

interface

uses
  Classes, SysUtils,lua, lualib, lauxlib, symbolhandler, LuaHandler, cefuncproc,
  memscan, foundlisthelper;



procedure initializeFoundlist;

implementation

uses luaclass, luaobject;

resourcestring
  rsCreatefoundlistNeedsAMemscanObjectAsParameter = 'createfoundlist needs a memscan object as parameter';

function createFoundList(L: Plua_State): integer; cdecl;
var
  foundlist: TFoundlist;
  memscan: TMemScan;
begin
  result:=0;
  if lua_gettop(L)=1 then
    memscan:=lua_toceuserdata(L, -1)
  else
    raise exception.create(rsCreatefoundlistNeedsAMemscanObjectAsParameter);

  foundlist:=TFoundList.create(nil, memscan);

  luaclass_newClass(L, foundlist);
  result:=1;
end;

function foundlist_initialize(L: Plua_State): integer; cdecl;
var
  foundlist: Tfoundlist;
begin
  result:=0;
  foundlist:=luaclass_getClassObject(L);
  foundlist.Initialize;
end;

function foundlist_deinitialize(L: Plua_State): integer; cdecl;
var
  foundlist: Tfoundlist;
begin
  result:=0;
  foundlist:=luaclass_getClassObject(L);
  foundlist.Deinitialize;
end;


function foundlist_getCount(L: PLua_State): integer; cdecl;
var
  foundlist: Tfoundlist;
begin
  foundlist:=luaclass_getClassObject(L);
  lua_pushinteger(L, foundlist.count);
  result:=1;
end;

function foundlist_getAddress(L: PLua_State): integer; cdecl;
var
  foundlist: Tfoundlist;
  index: integer;
begin
  result:=0;
  foundlist:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    index:=lua_tointeger(L,-1);
    lua_pushstring(L, inttohex(foundlist.GetAddress(index),8));
    result:=1;
  end;
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
  foundlist:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    index:=lua_tointeger(L,-1);
    foundlist.GetAddress(index, b, value);
    lua_pushstring(L, value);
    result:=1;
  end;
end;

procedure foundlist_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'initialize', foundlist_initialize);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'deinitialize', foundlist_deinitialize);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCount', foundlist_getCount);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getAddress', foundlist_getAddress);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getValue', foundlist_getValue);


  luaclass_addPropertyToTable(L, metatable, userdata, 'Count', foundlist_getCount, nil);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Address', foundlist_getAddress, nil);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Value', foundlist_getValue, nil);

  luaclass_setDefaultArrayProperty(L, metatable, userdata, foundlist_getAddress,nil);
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

initialization
  luaclass_register(TFoundList, foundlist_addMetaData);

end.

