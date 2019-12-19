unit LuaAddresslist;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, LuaHandler, cefuncproc, commonTypeDefs;

procedure initializeLuaAddresslist;

implementation

uses luaclass, memscan, addresslist, MemoryRecordUnit, LuaWinControl;


function addresslist_getCount(L: PLua_State): integer; cdecl;
var
  addresslist: TAddresslist;
begin
  addresslist:=luaclass_getClassObject(L);
  lua_pushinteger(L, addresslist.Count);
  result:=1;
end;

function addresslist_getSelectedRecords(L: PLua_State): integer; cdecl;
var
  addresslist: TAddresslist;
  i: integer;
begin
  result:=0;
  addresslist:=luaclass_getClassObject(L);
  if addresslist.SelCount>0 then
  begin
    lua_newtable(L);
    result:=1;

    for i:=0 to addresslist.Count-1 do
    begin
      if addresslist[i].isSelected then
      begin
        lua_pushinteger(L, i+1);
        luaclass_newClass(L, addresslist[i]);
        lua_settable(L, -3);
      end;
    end;

  end;


end;

function addresslist_getMemoryRecord(L: PLua_State): integer; cdecl;
var
  addresslist: TAddresslist;
  index: integer;
  s: string;
begin
  result:=0;
  addresslist:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    if lua_isnumber(L,-1) then
    begin
      index:=lua_tointeger(L,-1);
      luaclass_newClass(L, addresslist.MemRecItems[index]);
    end
    else
    begin
      s:=Lua_ToString(L,-1);
      luaclass_newClass(L, addresslist.getRecordWithDescription(s));
    end;
    result:=1;
  end;
end;

function addresslist_getMemoryRecordByDescription(L: PLua_State): integer; cdecl;
var
  addresslist: TAddresslist;
  description: string;
begin
  result:=0;
  addresslist:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    description:=Lua_ToString(L,-1);
    luaclass_newClass(L, addresslist.getRecordWithDescription(description));
    result:=1;
  end;
end;

function addresslist_getMemoryRecordByID(L: PLua_State): integer; cdecl;
var
  addresslist: TAddresslist;
  id: integer;
begin
  result:=0;
  addresslist:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    id:=lua_tointeger(L,-1);
    luaclass_newClass(L, addresslist.getRecordWithID(id));
    result:=1;
  end;
end;

function addresslist_createMemoryRecord(L: PLua_State): integer; cdecl;
var
  addresslist: TAddresslist;
begin
  addresslist:=luaclass_getClassObject(L);
  luaclass_newClass(L, addresslist.addaddress(rsPluginAddress, '0', [], 0, vtDword));
  result:=1;
end;

function addresslist_doDescriptionChange(L: PLua_State): integer; cdecl;
begin
  result:=0;
  TAddresslist(luaclass_getClassObject(L)).doDescriptionChange;

end;

function addresslist_doAddressChange(L: PLua_State): integer; cdecl;
begin
  result:=0;
  TAddresslist(luaclass_getClassObject(L)).doAddressChange;
end;

function addresslist_doTypeChange(L: PLua_State): integer; cdecl;
begin
  result:=0;
  TAddresslist(luaclass_getClassObject(L)).doTypeChange;
end;

function addresslist_disableAllWithoutExecute(L: PLua_State): integer; cdecl;
begin
  result:=0;
  TAddresslist(luaclass_getClassObject(L)).disableAllWithoutExecute;
end;

function addresslist_doValueChange(L: PLua_State): integer; cdecl;
begin
  result:=0;
  TAddresslist(luaclass_getClassObject(L)).doValueChange;
end;

function addresslist_getSelectedRecord(L: PLua_State): integer; cdecl;
var
  addresslist: TAddresslist;
begin
  addresslist:=luaclass_getClassObject(L);
  luaclass_newClass(L, addresslist.SelectedRecord);
  result:=1;
end;

function addresslist_setSelectedRecord(L: PLua_State): integer; cdecl;
var
  addresslist: TAddresslist;
  r: TMemoryRecord;
begin
  result:=0;
  addresslist:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    r:=lua_toceuserdata(L,-1);
    addresslist.selectedRecord:=r;
  end;
end;

procedure addresslist_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCount', addresslist_getCount);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getMemoryRecord', addresslist_getMemoryRecord);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getMemoryRecordByDescription', addresslist_getMemoryRecordByDescription);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getMemoryRecordByID', addresslist_getMemoryRecordByID);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'createMemoryRecord', addresslist_createMemoryRecord);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getSelectedRecords', addresslist_getSelectedRecords);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'doDescriptionChange', addresslist_doDescriptionChange);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'doAddressChange', addresslist_doAddressChange);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'doTypeChange', addresslist_doTypeChange);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'doValueChange', addresslist_doValueChange);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getSelectedRecord', addresslist_getSelectedRecord);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setSelectedRecord', addresslist_setSelectedRecord);


  luaclass_addClassFunctionToTable(L, metatable, userdata, 'disableAllWithoutExecute', addresslist_disableAllWithoutExecute);



  luaclass_addPropertyToTable(L, metatable, userdata, 'Count', addresslist_getCount, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'SelectedRecord', addresslist_getSelectedRecord, addresslist_setSelectedRecord);
  luaclass_addPropertyToTable(L, metatable, userdata, 'MemoryRecord', addresslist_getMemoryRecord, nil);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'MemoryRecord', addresslist_getMemoryRecord, nil);
  luaclass_setDefaultArrayProperty(L, metatable, userdata, addresslist_getMemoryRecord, nil);
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

initialization
  luaclass_register(TAddresslist, addresslist_addMetaData);

end.

