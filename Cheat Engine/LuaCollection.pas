unit LuaCollection;

{$mode delphi}

interface

uses
  Classes, SysUtils,Lua, Lualib, lauxlib;

procedure collection_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
procedure initializeLuaCollection;


implementation

uses luaclass, luahandler, LuaObject;

function collection_clear(L: Plua_State): integer; cdecl;
var
  collection: TCollection;
begin
  collection:=luaclass_getClassObject(L);
  collection.clear;
  result:=0;
end;

function collection_getCount(L: PLua_State): integer; cdecl;
var
  collection: TCollection;
begin
  collection:=luaclass_getClassObject(L);
  lua_pushinteger(L, collection.Count);
  result:=1;
end;

function collection_delete(L: Plua_State): integer; cdecl;
var
  collection: TCollection;
begin
  collection:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    collection.Delete(lua_tointeger(L, -1));

  result:=0;
end;

function collection_getItem(L: Plua_State): integer; cdecl;
var
  collection: TCollection;
begin
  result:=0;
  collection:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    luaclass_newClass(L, collection.Items[lua_tointeger(L,1)]);
    exit(1);
  end;


end;

procedure collection_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clear', collection_clear);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCount', collection_getCount);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'delete', collection_delete);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Count', collection_getCount, nil);

  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Items', collection_getItem, nil);
  luaclass_setDefaultArrayProperty(L, metatable, userdata, collection_getItem,nil);
end;

procedure initializeLuaCollection;
begin
  lua_register(LuaVM, 'collection_clear', collection_clear);
  lua_register(LuaVM, 'collection_getCount', collection_getCount);
  lua_register(LuaVM, 'collection_delete', collection_delete);
end;

initialization
  luaclass_register(Tcollection,  collection_addMetaData);

end.

