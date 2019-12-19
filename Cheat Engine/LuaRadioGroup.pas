unit LuaRadioGroup;

{$mode delphi}

interface

uses
  Classes, Controls, ExtCtrls, SysUtils;

procedure initializeLuaRadioGroup;

implementation

uses lua, lualib, lauxlib, LuaHandler, ceguicomponents, LuaCaller, LuaGroupbox, LuaClass;

function createRadioGroup(L: Plua_State): integer; cdecl;
var
  RadioGroup: TCERadioGroup;
  parameters: integer;
  owner: TWincontrol;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    owner:=lua_toceuserdata(L, -parameters)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));


  RadioGroup:=TCERadioGroup.Create(owner);
  if owner<>nil then
    RadioGroup.Parent:=owner;

  luaclass_newClass(L, RadioGroup);
  result:=1;
end;

function radiogroup_getRows(L: PLua_State): integer; cdecl;
var
  radiogroup: TCustomRadioGroup;
begin
  radiogroup:=luaclass_getClassObject(L);
  lua_pushinteger(L, radiogroup.Rows);
  result:=1;
end;

function radiogroup_getItems(L: PLua_State): integer; cdecl;
var
  radiogroup: TCustomRadioGroup;
begin
  radiogroup:=luaclass_getClassObject(L);
  luaclass_newClass(L, radiogroup.items);
  result:=1;
end;

function radiogroup_getColumns(L: PLua_State): integer; cdecl;
var
  radiogroup: TCustomRadioGroup;
begin
  radiogroup:=luaclass_getClassObject(L);
  lua_pushinteger(L, radiogroup.Columns);
  result:=1;
end;

function radiogroup_setColumns(L: PLua_State): integer; cdecl;
var
  radiogroup: Tcustomradiogroup;
begin
  result:=0;
  radiogroup:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
    radiogroup.Columns:=lua_tointeger(L,-1);
end;


function radiogroup_getItemIndex(L: PLua_State): integer; cdecl;
var
  radiogroup: TCustomRadioGroup;
begin
  radiogroup:=luaclass_getClassObject(L);
  lua_pushinteger(L, radiogroup.ItemIndex);
  result:=1;
end;

function radiogroup_setItemIndex(L: PLua_State): integer; cdecl;
var
  radiogroup: Tcustomradiogroup;
begin
  result:=0;
  radiogroup:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
    radiogroup.ItemIndex:=lua_tointeger(L,-1);
end;

function radiogroup_getonclick(L: PLua_State): integer; cdecl;
var
  c: TCustomRadioGroup;
begin
  c:=luaclass_getClassObject(L);
  LuaCaller_pushMethodProperty(L, TMethod(c.OnClick), 'TNotifyEvent');
  result:=1;
end;

function radiogroup_setonClick(L: PLua_State): integer; cdecl; //for some reason the radiogroup has it's own fonclick variable
var
  control: TCustomRadioGroup;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  control:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
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
end;

procedure radiogroup_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  groupbox_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getRows', radiogroup_getRows);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getItems', radioGroup_getItems);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getColumns', radiogroup_getColumns);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setColumns', radiogroup_setColumns);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getItemIndex', radiogroup_getItemIndex);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setItemIndex', radiogroup_setItemIndex);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOnClick', radiogroup_setOnClick);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Columns', radiogroup_getColumns, radiogroup_setColumns);
  luaclass_addPropertyToTable(L, metatable, userdata, 'ItemIndex', radiogroup_getItemIndex, radiogroup_setItemIndex);
  luaclass_addPropertyToTable(L, metatable, userdata, 'OnClick', radiogroup_getOnClick, radiogroup_setOnClick);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Items', radioGroup_getItems, nil);

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
  lua_register(LuaVM, 'radiogroup_onClick', radiogroup_setOnClick);
end;

initialization
  luaclass_register(TCustomRadioGroup, radiogroup_addMetaData);

end.

