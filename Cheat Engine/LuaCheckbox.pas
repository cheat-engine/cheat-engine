unit LuaCheckbox;

{$mode delphi}

interface

uses
  Classes, SysUtils,lua, lualib, lauxlib, controls, StdCtrls;

procedure initializeLuaCheckbox;

implementation

uses luahandler, luacaller, luaclass, LuaWinControl, ceguicomponents;

function createCheckBox(L: Plua_State): integer; cdecl;
var
  CheckBox: TCECheckBox;
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


  CheckBox:=TCECheckBox.Create(owner);
  if owner<>nil then
    CheckBox.Parent:=owner;

  luaclass_newClass(L, CheckBox);
  result:=1;
end;

function checkbox_getAllowGrayed(L: PLua_State): integer; cdecl;
var
  checkbox: Tcustomcheckbox;
begin
  checkbox:=luaclass_getClassObject(L);
  lua_pushboolean(L, checkbox.AllowGrayed);
  result:=1;
end;

function checkbox_setAllowGrayed(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  checkbox: Tcustomcheckbox;
begin
  result:=0;
  checkbox:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    checkbox.AllowGrayed:=lua_toboolean(L,-1);
end;

function checkbox_getState(L: PLua_State): integer; cdecl;
var
  checkbox: Tcustomcheckbox;
begin
  checkbox:=luaclass_getClassObject(L);
  lua_pushinteger(L, integer(checkbox.state));
  result:=1;
end;

function checkbox_setState(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  checkbox: Tcustomcheckbox;
begin
  result:=0;
  checkbox:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    checkbox.state:=tcheckboxstate(lua_tointeger(L,-1));
end;

function checkbox_getonChange(L: PLua_State): integer; cdecl;
var
  c: TCustomCheckBox;
begin
  c:=luaclass_getClassObject(L);
  LuaCaller_pushMethodProperty(L, TMethod(c.OnChange), 'TNotifyEvent');
  result:=1;
end;

function checkbox_setonChange(L: PLua_State): integer; cdecl;
var
  control: TCustomCheckBox;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  control:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin

    CleanupLuaCall(tmethod(control.onChange));
    control.onChange:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      control.OnChange:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      control.OnChange:=lc.NotifyEvent;
    end;

  end;
end;

procedure checkbox_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getAllowGrayed', checkbox_getAllowGrayed);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setAllowGrayed', checkbox_setAllowGrayed);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getState', checkbox_getState);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setState', checkbox_setState);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOnChange', checkbox_setonChange);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOnChange', checkbox_getonChange);

  luaclass_addPropertyToTable(L, metatable, userdata, 'AllowGrayed', checkbox_getAllowGrayed, checkbox_setAllowGrayed);
  luaclass_addPropertyToTable(L, metatable, userdata, 'State', checkbox_getState, checkbox_setState);
  luaclass_addPropertyToTable(L, metatable, userdata, 'OnChange', checkbox_getOnChange, checkbox_setOnChange);

end;

procedure initializeLuaCheckbox;
begin
  lua_register(LuaVM, 'createCheckBox', createCheckBox);
  lua_register(LuaVM, 'checkbox_getAllowGrayed', checkbox_getAllowGrayed);
  lua_register(LuaVM, 'checkbox_setAllowGrayed', checkbox_setAllowGrayed);
  lua_register(LuaVM, 'checkbox_getState', checkbox_getState);
  lua_register(LuaVM, 'checkbox_setState', checkbox_setState);
  lua_register(LuaVM, 'checkbox_onChange', checkbox_setonChange);
end;

initialization
  luaclass_register(TCustomCheckbox, checkbox_addMetaData);


end.

