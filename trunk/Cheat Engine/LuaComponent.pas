unit LuaComponent;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib;

procedure InitializeComponent;
function component_findComponentByName(L: PLua_state): integer; cdecl;
procedure component_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses LuaHandler, LuaObject, LuaClass;

function component_getComponentCount(L: PLua_state): integer; cdecl;
var c: TComponent;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_toceuserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushinteger(L, c.ComponentCount);
    result:=1;
  end else lua_pop(L, lua_gettop(l));

end;

function component_findComponentByName(L: PLua_state): integer; cdecl;
var c: TComponent;
  n: string;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_toceuserdata(L, 1);
    n:=Lua_ToString(L, 2);
    lua_pop(L, lua_gettop(l));

    luaclass_newClass(L, c.FindComponent(n));


    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function component_getComponent(L: PLua_state): integer; cdecl;
var c: TComponent;
  i: integer;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_toceuserdata(L, -2);
    i:=lua_tointeger(L,-1);
    lua_pop(L, lua_gettop(l));

    lua_pushlightuserdata(L, c.Components[i]);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function component_getName(L: PLua_state): integer; cdecl;
var c: TComponent;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_toceuserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushstring(L, c.Name);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function component_setName(L: PLua_state): integer; cdecl;
var c: TComponent;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_toceuserdata(L, -2);
    c.Name:=lua_tostring(L,-1);
  end;
  lua_pop(L, lua_gettop(l));
end;

function component_getTag(L: PLua_state): integer; cdecl;
var c: TComponent;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_toceuserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushinteger(L, c.Tag);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
end;

function component_setTag(L: PLua_state): integer; cdecl;
var c: TComponent;
  t: integer;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_toceuserdata(L, -2);
    c.Tag:=lua_tointeger(L, -1);
  end;
  lua_pop(L, lua_gettop(l));
end;


function component_getOwner(L: PLua_state): integer; cdecl;
var c: TComponent;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_toceuserdata(L, -1);
    lua_pop(L, lua_gettop(l));

    lua_pushlightuserdata(L, c.Owner);
    result:=1;
  end else lua_pop(L, lua_gettop(l));
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
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Component', component_getComponentCount);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'ComponentByName', component_findComponentByName);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Name', component_getName, component_setName);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Tag', component_getTag, component_setTag);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Owner', component_getOwner, nil);


end;

procedure InitializeComponent;
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

