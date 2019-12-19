unit LuaCollectionItem;

//first new object added since the conversion (no initializeLuaCollectionItem because no backwards compatibility and no create)

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lauxlib, lualib;

procedure collectionItem_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses luahandler, LuaClass, LuaObject;

function collectionItem_getID(L: PLua_state):integer; cdecl;
var ci: TCollectionItem;
begin
  ci:=luaclass_getClassObject(L);
  lua_pushinteger(L, ci.ID);
  result:=1;
end;

function collectionItem_getIndex(L: PLua_state):integer; cdecl;
var ci: TCollectionItem;
begin
  ci:=luaclass_getClassObject(L);
  lua_pushinteger(L, ci.Index);
  result:=1;
end;

function collectionItem_setIndex(L: PLua_state):integer; cdecl;
var ci: TCollectionItem;
begin
  ci:=luaclass_getClassObject(L);
  if lua_gettop(l)=1 then
    ci.index:=lua_tointeger(L,1);

  result:=0;
end;

function collectionItem_getDisplayName(L: PLua_state):integer; cdecl;
var ci: TCollectionItem;
begin
  ci:=luaclass_getClassObject(L);
  lua_pushstring(L, ci.DisplayName);
  result:=1;
end;

function collectionItem_setDisplayName(L: PLua_state):integer; cdecl;
var ci: TCollectionItem;
begin
  ci:=luaclass_getClassObject(L);
  if lua_gettop(l)=1 then
    ci.DisplayName:=Lua_ToString(L,1);

  result:=0;
end;

procedure collectionItem_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getID', collectionItem_getID);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getIndex', collectionItem_getIndex);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setIndex', collectionItem_setIndex);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getDisplayName', collectionItem_getDisplayName);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setDisplayName', collectionItem_setDisplayName);

  luaclass_addPropertyToTable(L, metatable, userdata, 'ID', collectionItem_getID, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Index', collectionItem_getIndex, collectionItem_setIndex);
  luaclass_addPropertyToTable(L, metatable, userdata, 'DisplayName', collectionItem_getDisplayName, collectionItem_setDisplayName);
end;

initialization
  luaclass_register(TCollectionItem, collectionItem_addMetaData);

end.

