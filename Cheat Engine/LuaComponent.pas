unit LuaComponent;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib;

procedure InitializeLuaComponent;
function component_findComponentByName(L: PLua_state): integer; cdecl; //used by luaclass when a entry can't be resolved
procedure component_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses LuaHandler, LuaObject, LuaClass;

function component_getComponentCount(L: PLua_state): integer; cdecl;
var c: TComponent;
begin
  c:=luaclass_getClassObject(L);
  lua_pushinteger(L, c.ComponentCount);
  result:=1;
end;

function component_findComponentByName(L: PLua_state): integer; cdecl;
var c: TComponent;
  n: string;
begin
  result:=0;
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    n:=Lua_ToString(L, -1);
    luaclass_newClass(L, c.FindComponent(n));
    result:=1;
  end;
end;

function component_getComponent(L: PLua_state): integer; cdecl;
var c: TComponent;
  i: integer;
begin
  result:=0;
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    i:=lua_tointeger(L,-1);
    luaclass_newClass(L, c.Components[i]);
    result:=1;
  end;
end;

function component_getName(L: PLua_state): integer; cdecl;
var c: TComponent;
begin
  c:=luaclass_getClassObject(L);
  lua_pushstring(L, c.Name);
  result:=1;
end;

function component_setName(L: PLua_state): integer; cdecl;
var c: TComponent;
begin
  result:=0;
  c:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
    c.Name:=lua_tostring(L,-1);
end;

function component_getTag(L: PLua_state): integer; cdecl;
var c: TComponent;
begin
  c:=luaclass_getClassObject(L);
  lua_pushinteger(L, c.Tag);
  result:=1;
end;

function component_setTag(L: PLua_state): integer; cdecl;
var c: TComponent;
begin
  result:=0;
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    c.Tag:=lua_tointeger(L, -1);
end;


function component_getOwner(L: PLua_state): integer; cdecl;
var c: TComponent;
begin
  c:=luaclass_getClassObject(L);
  luaclass_newClass(L, c.owner);
  result:=1;
end;

procedure component_addMetaData(L: PLua_state; metatable: integer; userdata: integer);
begin
  object_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getComponentCount', component_getComponentCount);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getComponent', component_getComponent);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'findComponentByName', component_findComponentByName);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getName', component_getName);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setName', component_setName);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getTag', component_getTag);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setTag', component_setTag);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOwner', component_getOwner);

  luaclass_addPropertyToTable(L, metatable, userdata, 'ComponentCount', component_getComponentCount, nil);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Component', component_getComponent);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'ComponentByName', component_findComponentByName);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Name', component_getName, component_setName);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Tag', component_getTag, component_setTag);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Owner', component_getOwner, nil);


end;

procedure InitializeLuaComponent;
begin
  lua_register(LuaVM, 'component_getComponentCount', component_getComponentCount);
  lua_register(LuaVM, 'component_getComponent', component_getComponent);
  lua_register(LuaVM, 'component_findComponentByName', component_findComponentByName);
  lua_register(LuaVM, 'component_getName', component_getName);
  lua_register(LuaVM, 'component_setName', component_setName);
  lua_register(LuaVM, 'component_getTag', component_getTag);
  lua_register(LuaVM, 'component_setTag', component_setTag);
  lua_register(LuaVM, 'component_getOwner', component_getOwner);
end;

initialization
  luaclass_register(TComponent, component_addMetaData);


end.

