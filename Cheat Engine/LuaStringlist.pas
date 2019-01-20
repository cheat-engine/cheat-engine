unit LuaStringlist;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib;

procedure initializeLuaStringlist;
procedure stringlist_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses luahandler, luaclass, luastrings;

function createStringlist(L: Plua_State): integer; cdecl;
begin
  luaclass_newClass(L, TStringList.Create);
  result:=1;
end;

function stringlist_getDuplicates(L: PLua_State): integer; cdecl;
var
  stringlist: TStringlist;
begin
  stringlist:=luaclass_getClassObject(L);
  lua_pushinteger(L, integer(stringlist.Duplicates));
  result:=1;
end;

function stringlist_setDuplicates(L: PLua_State): integer; cdecl;
var
  stringlist: TStringlist;
  a: integer;
begin
  result:=0;
  stringlist:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    stringlist.Duplicates:=TDuplicates(lua_tointeger(L,-1));
end;


function stringlist_getSorted(L: PLua_State): integer; cdecl;
var
  stringlist: TStringlist;
begin
  stringlist:=luaclass_getClassObject(L);
  lua_pushboolean(L, stringlist.Sorted);
  result:=1;
end;

function stringlist_setSorted(L: PLua_State): integer; cdecl;
var
  stringlist: TStringlist;
  a: integer;
begin
  result:=0;
  stringlist:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    stringlist.sorted:=lua_toboolean(L,-1);
end;

function stringlist_getCaseSensitive(L: PLua_State): integer; cdecl;
var
  stringlist: TStringlist;
begin
  stringlist:=luaclass_getClassObject(L);
  lua_pushboolean(L, stringlist.CaseSensitive);
  result:=1;
end;

function stringlist_setCaseSensitive(L: PLua_State): integer; cdecl;
var
  stringlist: TStringlist;
  a: integer;
begin
  result:=0;
  stringlist:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    stringlist.CaseSensitive:=lua_toboolean(L,-1);
end;

procedure stringlist_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  strings_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getDuplicates', stringlist_getDuplicates);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setDuplicates', stringlist_setDuplicates);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getSorted', stringlist_getSorted);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setSorted', stringlist_setSorted);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCaseSensitive', stringlist_getCaseSensitive);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setCaseSensitive', stringlist_setCaseSensitive);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Duplicates', stringlist_getDuplicates, stringlist_setDuplicates);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Sorted', stringlist_getSorted, stringlist_setSorted);
  luaclass_addPropertyToTable(L, metatable, userdata, 'CaseSensitive', stringlist_getCaseSensitive, stringlist_setCaseSensitive);
end;

procedure initializeLuaStringlist;
begin
  lua_register(LuaVM, 'createStringlist', createStringlist);
  lua_register(LuaVM, 'createStringList', createStringList);
  lua_register(LuaVM, 'stringlist_getDuplicates', stringlist_getDuplicates);
  lua_register(LuaVM, 'stringlist_setDuplicates', stringlist_setDuplicates);
  lua_register(LuaVM, 'stringlist_getSorted', stringlist_getSorted);
  lua_register(LuaVM, 'stringlist_setSorted', stringlist_setSorted);
  lua_register(LuaVM, 'stringlist_getCaseSensitive', stringlist_getCaseSensitive);
  lua_register(LuaVM, 'stringlist_setCaseSensitive', stringlist_setCaseSensitive);
end;

initialization
  luaclass_register(TStringList, stringlist_addMetaData);

end.

