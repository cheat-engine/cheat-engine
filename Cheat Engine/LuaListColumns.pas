unit LuaListColumns;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, comctrls;

procedure initializeLuaListColumns;

implementation

uses luaclass, luahandler, LuaCollection;

function listcolumns_add(L: PLua_State): integer; cdecl;
var
  listcolumns: TListColumns;
begin
  listcolumns:=luaclass_getClassObject(L);
  luaclass_newclass(L, listcolumns.add);
  result:=1;
end;

function listcolumns_getColumn(L: PLua_State): integer; cdecl;
var
  listcolumns: TListcolumns;
  index: integer;
begin
  result:=0;
  listcolumns:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    index:=lua_toInteger(L,-1);
    luaclass_newClass(L, listcolumns[index]);
    result:=1;
  end;
end;

function listcolumns_setColumn(L: PLua_State): integer; cdecl;
var
  listcolumns: TListcolumns;
  index: integer;
  li: TListColumn;
begin
  result:=0;
  listcolumns:=luaclass_getClassObject(L);

  if lua_gettop(L)=2 then
  begin
    index:=lua_toInteger(L, 1);
    li:=lua_ToCEUserData(L, 2);
    listcolumns[index]:=li;
  end;
end;

procedure listcolumns_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  collection_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'add', listcolumns_add);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getColumn', listcolumns_getColumn);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setColumn', listcolumns_setColumn);

  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Column', listcolumns_getColumn);
  luaclass_setDefaultArrayProperty(L, metatable, userdata, listcolumns_getColumn, listcolumns_setColumn);
end;

procedure initializeLuaListColumns;
begin
  lua_register(LuaVM, 'listcolumns_add', listcolumns_add);
  lua_register(LuaVM, 'listcolumns_getColumn', listcolumns_getColumn);
end;

initialization
  luaclass_register(TListColumns, listcolumns_addMetaData);

end.

