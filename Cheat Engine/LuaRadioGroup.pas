unit LuaRadioGroup;

{$mode delphi}

interface

uses
  Classes, Controls, ExtCtrls, SysUtils;

procedure initializeLuaRadioGroup;

implementation

uses lua, lualib, lauxlib, LuaHandler, ceguicomponents, LuaCaller;

function createRadioGroup(L: Plua_State): integer; cdecl;
var
  RadioGroup: TCERadioGroup;
  parameters: integer;
  owner: TWincontrol;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    owner:=lua_touserdata(L, -parameters)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));


  RadioGroup:=TCERadioGroup.Create(owner);
  if owner<>nil then
    RadioGroup.Parent:=owner;

  lua_pushlightuserdata(L, RadioGroup);
  result:=1;
end;

function radiogroup_getRows(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  radiogroup: TCustomRadioGroup;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    radiogroup:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, radiogroup.Rows);
    result:=1;

  end else lua_pop(L, parameters);
end;

function radiogroup_getItems(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  radiogroup: TCustomRadioGroup;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    radiogroup:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, radiogroup.items);
    result:=1;

  end else lua_pop(L, parameters);
end;

function radiogroup_getColumns(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  radiogroup: Tcustomradiogroup;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    radiogroup:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, radiogroup.Columns);
    result:=1;

  end else lua_pop(L, parameters);
end;

function radiogroup_setColumns(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  radiogroup: Tcustomradiogroup;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    radiogroup:=lua_touserdata(L,-2);
    radiogroup.Columns:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;


function radiogroup_getItemIndex(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  radiogroup: Tcustomradiogroup;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    radiogroup:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, radiogroup.ItemIndex);
    result:=1;

  end else lua_pop(L, parameters);
end;

function radiogroup_setItemIndex(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  radiogroup: Tcustomradiogroup;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    radiogroup:=lua_touserdata(L,-2);
    radiogroup.ItemIndex:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;

function radiogroup_onClick(L: PLua_State): integer; cdecl; //for some reason the radiogroup has it's own fonclick variable
var
  parameters: integer;
  control: TCustomRadioGroup;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(control.onClick));
    control.onClick:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      control.OnClick:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      control.OnClick:=lc.NotifyEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

procedure initializeLuaRadioGroup;
begin
  lua_register(LuaVM, 'createRadioGroup', createRadioGroup);
  lua_register(LuaVM, 'radiogroup_getRows', radiogroup_getRows);
  lua_register(LuaVM, 'radiogroup_getItems', radioGroup_getItems);
  lua_register(LuaVM, 'radiogroup_getColumns', radiogroup_getColumns);
  lua_register(LuaVM, 'radiogroup_setColumns', radiogroup_setColumns);
  lua_register(LuaVM, 'radiogroup_getItemIndex', radiogroup_getItemIndex);
  lua_register(LuaVM, 'radiogroup_setItemIndex', radiogroup_setItemIndex);
  lua_register(LuaVM, 'radiogroup_onClick', radiogroup_onClick);
end;

end.

