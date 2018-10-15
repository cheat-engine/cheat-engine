unit LuaListItems;

{$mode delphi}

interface

uses
  Classes, SysUtils, ComCtrls, lua, lualib, lauxlib;

procedure initializeLuaListItems;

implementation

uses luaclass, luahandler, LuaObject;

function listitems_beginUpdate(L: Plua_State): integer; cdecl;
begin
  result:=0;
  Tlistitems(luaclass_getClassObject(L)).BeginUpdate;
end;

function listitems_endUpdate(L: Plua_State): integer; cdecl;
begin
  result:=0;
  Tlistitems(luaclass_getClassObject(L)).EndUpdate;
end;

function listitems_clear(L: Plua_State): integer; cdecl;
var
  listitems: Tlistitems;
begin
  result:=0;
  listitems:=luaclass_getClassObject(L);
  listitems.clear;
end;

function listitems_getItem(L: PLua_State): integer; cdecl;
var
  listitems: Tlistitems;
  index: integer;
begin
  result:=0;
  listitems:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    index:=lua_tointeger(L,-1);
    luaclass_newClass(L, listitems.Item[index]);
    result:=1;
  end;
end;

function listitems_setItem(L: PLua_State): integer; cdecl;
var
  listitems: Tlistitems;
  index: integer;
  item:TListItem;
begin
  result:=0;
  listitems:=luaclass_getClassObject(L);

  if lua_gettop(L)>=2 then
  begin
    index:=lua_tointeger(L,-2);
    item:=lua_ToCEUserData(L, -1);
    listitems.Item[index]:=item;
  end;
end;

function listitems_setCount(L: PLua_State): integer; cdecl;
var
  listitems: Tlistitems;
begin
  result:=0;
  listitems:=luaclass_getClassObject(L);
  listitems.Count:=lua_tovariant(L, 1);
end;

function listitems_getCount(L: PLua_State): integer; cdecl;
var
  listitems: Tlistitems;
begin
  listitems:=luaclass_getClassObject(L);
  lua_pushvariant(L, listitems.Count);
  result:=1;

end;

function listitems_add(L: PLua_State): integer; cdecl;
var
  listitems: Tlistitems;
begin
  listitems:=luaclass_getClassObject(L);
  luaclass_newClass(L, listitems.add);
  result:=1;
end;

procedure ListItems_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clear', listitems_clear);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCount', listitems_getCount);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getItem', listitems_getItem);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setItem', listitems_setItem);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'add', listitems_add);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'beginUpdate', listitems_beginUpdate);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'endUpdate', listitems_endUpdate);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Count', listitems_getCount, listitems_setCount);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Item', listitems_getItem);
  luaclass_setDefaultArrayProperty(L, metatable, userdata, listitems_getItem, listitems_setItem);

end;

procedure initializeLuaListItems;
begin
  lua_register(LuaVM, 'listitems_clear', listitems_clear);
  lua_register(LuaVM, 'listitems_getCount', listitems_getCount);
  lua_register(LuaVM, 'listitems_getItem', listitems_getItem);
  lua_register(LuaVM, 'listitems_add', listitems_add);
end;

initialization
   luaclass_register(TListItems,  ListItems_addMetaData);

end.

