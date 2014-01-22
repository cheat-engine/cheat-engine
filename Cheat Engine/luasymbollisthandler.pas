unit LuaSymbolListHandler;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, LuaClass;

procedure initializeLuaSymbolListHandler;

implementation

uses LuaHandler, SymbolListHandler, LuaObject, symbolhandler;

function createSymbolList(L: Plua_State): integer; cdecl;
begin
  result:=0;
  luaclass_newClass(L, TSymbolListHandler.create);
end;

function SymbolList_clear(L: Plua_State): integer; cdecl;
var
  sl: TSymbolListHandler;
begin
  sl:=luaclass_getClassObject(L);
  sl.clear;
  result:=0;
end;

function SymbolList_register(L: Plua_State): integer; cdecl;
var
  sl: TSymbolListHandler;
begin
  sl:=luaclass_getClassObject(L);
  symhandler.AddSymbolList(sl);
  result:=0;
end;

function SymbolList_unregister(L: Plua_State): integer; cdecl;
var
  sl: TSymbolListHandler;
begin
  sl:=luaclass_getClassObject(L);
  symhandler.RemoveSymbolList(sl);
  result:=0;
end;

procedure pushSymbol(L: PLua_state; si: PCESymbolInfo);
var tableindex: integer;
begin
  lua_newtable(L);
  tableindex:=lua_gettop(L);

  lua_pushstring(L,'modulename');
  lua_pushstring(L,si.module);
  lua_settable(L, tableindex);

  lua_pushstring(L,'searchkey');
  lua_pushstring(L,si.originalstring);
  lua_settable(L, tableindex);

  lua_pushstring(L,'address');
  lua_pushinteger(L,si.address);
  lua_settable(L, tableindex);

  lua_pushstring(L,'symbolsize');
  lua_pushinteger(L,si.size);
  lua_settable(L, tableindex);
end;

function SymbolList_GetSymbolFromAddress(L: Plua_State): integer; cdecl;
var
  sl: TSymbolListHandler;
  si: PCESymbolInfo;
begin
  result:=0;
  sl:=luaclass_getClassObject(L);

  si:=sl.FindAddress(lua_tointeger(L, 1));
  if si<>nil then
  begin
    pushSymbol(L, si);
    result:=1;
  end;
end;

function SymbolList_getSymbolFromString(L: Plua_State): integer; cdecl;
var
  sl: TSymbolListHandler;
  si: PCESymbolInfo;
begin
  result:=0;
  sl:=luaclass_getClassObject(L);

  si:=sl.FindSymbol(lua_tostring(L, 1));
  if si<>nil then
  begin
    pushSymbol(L, si);
    result:=1;
  end;
end;

function SymbolList_addSymbol(L: Plua_State): integer; cdecl;
var
  sl: TSymbolListHandler;
  si: PCESymbolInfo;

  modulename: string;
  searchkey: string;
  address: qword;
  size: integer;
  skipAddressToSymbol: boolean;
begin
  result:=0;
  sl:=luaclass_getClassObject(L);

  if lua_gettop(L)>=4 then //modulename, searchkey, address, symbolsize, skipAddressToSymbol
  begin
    modulename:=lua_tostring(L, 1);
    searchkey:=lua_tostring(L, 2);
    address:=lua_tointeger(L, 3);
    size:=lua_tointeger(L, 4);

    if lua_gettop(L)>=5 then
      skipAddressToSymbol:=lua_toboolean(L, 5)
    else
      skipAddressToSymbol:=false;

    si:=sl.AddSymbol(modulename, searchkey, address, size, skipAddressToSymbol);

    pushSymbol(L, si);
    result:=1;
  end;
end;

function SymbolList_deleteSymbol(L: Plua_State): integer; cdecl;
var
  sl: TSymbolListHandler;
  si: PCESymbolInfo;
begin
  result:=0;
  sl:=luaclass_getClassObject(L);

  if lua_type(L,1)=LUA_TNUMBER then
    sl.DeleteSymbol(lua_tointeger(L, 1))
  else
    sl.DeleteSymbol(lua_tostring(L, 1));
end;

procedure SymbolList_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clear', SymbolList_clear);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getSymbolFromAddress', SymbolList_GetSymbolFromAddress);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getSymbolFromString', SymbolList_getSymbolFromString);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'addSymbol', SymbolList_addSymbol);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'deleteSymbol', SymbolList_deleteSymbol);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'register', SymbolList_register);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'unregister', SymbolList_register);
end;

procedure initializeLuaSymbolListHandler;
begin
  lua_register(LuaVM, 'createSymbolList', createSymbolList);
end;

end.

