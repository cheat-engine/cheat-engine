unit LuaGenericHotkey;

{$mode delphi}

//todo: add table support

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  Classes, SysUtils, lua, lauxlib, lualib, genericHotkey, CEFuncProc,
  math, commonTypeDefs;


procedure initializeLuaGenericHotkey;

implementation

uses luaclass, luaobject, luahandler, luacaller;


function createHotkey(L: Plua_State): integer; cdecl;
var parameters: integer;
  h: TGenericHotkey;
  routine: string;

  lc: TLuaCaller;

  i: integer;
  keys: TKeycombo;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=2 then //(function, key, ...)
  begin
    lc:=TLuaCaller.create;

    if lua_isfunction(L, 1) then
    begin
      lua_pushvalue(L, 1);
      lc.luaroutineindex:=luaL_ref(L,LUA_REGISTRYINDEX)
    end
    else
      lc.luaroutine:=lua_tostring(L,1);

    parameters:=min(parameters,6); //6 because this includes the function

    zeromemory(@keys,sizeof(keys));

    if (parameters=2) and lua_istable(L, 2) then
    begin
      for i:=0 to 4 do
      begin
        lua_pushinteger(L, i+1);
        lua_gettable(L, 2);
        if lua_isnil(L, -1) then  //end of the list
        begin
          lua_pop(L,1);
          break;
        end
        else
        begin
          keys[i]:=lua_tointeger(L,-1);
          lua_pop(L,1);
        end;
      end;

    end
    else
    begin
      for i:=2 to parameters do
        keys[i-2]:=lua_tointeger(L, i);
    end;


    h:=TGenericHotkey.create(lc.NotifyEvent, keys);

    lua_pop(L, lua_gettop(L));

    luaclass_newClass(L, h);
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function GenericHotkey_setKeys(L: PLua_State): integer; cdecl;
var
  GenericHotkey: TGenericHotkey;
  i: integer;
  paramstart, paramcount: integer;
begin
  //test me
  result:=0;
  GenericHotkey:=luaclass_getClassObject(L, @paramstart, @paramcount);

  if paramcount>=1 then
  begin
    zeromemory(@GenericHotkey.keys,sizeof(GenericHotkey.keys));
    for i:=paramstart to paramstart+paramcount-1 do
      GenericHotkey.keys[i-paramstart]:=lua_tointeger(L, i);
  end;
end;

function GenericHotkey_getKeys(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  GenericHotkey: TGenericHotkey;
  i: integer;
begin
  GenericHotkey:=luaclass_getClassObject(L);

  i:=0;
  while (i<5) and (Generichotkey.keys[i]<>0) do
  begin
    lua_pushinteger(L, Generichotkey.keys[i]);
    inc(i);
  end;

  result:=i;
end;

function GenericHotkey_getOnHotkey(L: PLua_State): integer; cdecl;
var
  c: TgenericHotkey;
begin
  c:=luaclass_getClassObject(L);
  LuaCaller_pushMethodProperty(L, TMethod(c.onNotify), 'TNotifyEvent');
  result:=1;
end;

function GenericHotkey_setOnHotkey(L: PLua_State): integer; cdecl;
var
  c: TgenericHotkey;
  m: tmethod;
begin
  if lua_gettop(L)>=1 then
  begin
    c:=luaclass_getClassObject(L);
    m:=tmethod(c.onNotify);
    LuaCaller_setMethodProperty(L, m, 'TNotifyEvent', lua_gettop(L));
    c.onNotify:=tnotifyevent(m);
  end;
  result:=0;
end;

procedure generichotkey_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setKeys', generichotkey_setKeys);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getKeys', generichotkey_getKeys);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOnHotkey', generichotkey_setonHotkey);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOnHotkey', generichotkey_getonHotkey);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'OnHotkey', generichotkey_getonHotkey, generichotkey_setonHotkey);
end;

procedure initializeLuaGenericHotkey;
begin
  lua_register(LuaVM, 'createHotkey', createHotkey);
  lua_register(LuaVM, 'generichotkey_setKeys', generichotkey_setKeys);
  lua_register(LuaVM, 'generichotkey_getKeys', generichotkey_getKeys);
  lua_register(LuaVM, 'generichotkey_onHotkey', generichotkey_setonHotkey);


end;

initialization
  luaclass_register(TGenericHotkey, generichotkey_addMetaData);

end.

