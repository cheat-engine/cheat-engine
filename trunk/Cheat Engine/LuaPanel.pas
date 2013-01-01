unit LuaPanel;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, ExtCtrls;

procedure initializeLuaPanel;
procedure panel_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses luaclass, LuaCustomControl, pluginexports, luahandler;

function createPanel(L: Plua_State): integer; cdecl;
var parameters: integer;
  f,p: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_toceuserdata(L, -1);
    p:=ce_createPanel(f);
    luaclass_newClass(L, p);
    result:=1;
  end;
end;

function panel_getAlignment(L: PLua_State): integer; cdecl;
var
  panel: Tcustompanel;
begin
  panel:=luaclass_getClassObject(L);
  lua_pushinteger(L, integer(panel.Alignment));
  result:=1;
end;

function panel_setAlignment(L: PLua_State): integer; cdecl;
var
  panel: Tcustompanel;
begin
  result:=0;
  panel:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    panel.Alignment:=TAlignment(lua_tointeger(L,-1));
end;

function panel_getBevelInner(L: PLua_State): integer; cdecl;
var
  panel: Tcustompanel;
begin
  panel:=luaclass_getClassObject(L);
  lua_pushinteger(L, integer(panel.BevelInner));
  result:=1;
end;

function panel_setBevelInner(L: PLua_State): integer; cdecl;
var
  panel: Tcustompanel;
begin
  result:=0;
  panel:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    panel.BevelInner:=TPanelBevel(lua_tointeger(L,-1));
end;

function panel_getBevelOuter(L: PLua_State): integer; cdecl;
var
  panel: Tcustompanel;
begin
  panel:=luaclass_getClassObject(L);
  lua_pushinteger(L, integer(panel.BevelOuter));
  result:=1;
end;

function panel_setBevelOuter(L: PLua_State): integer; cdecl;
var
  panel: Tcustompanel;
begin
  result:=0;
  panel:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    panel.BevelOuter:=TPanelBevel(lua_tointeger(L,-1));
end;

function panel_getBevelWidth(L: PLua_State): integer; cdecl;
var
  panel: Tcustompanel;
begin
  panel:=luaclass_getClassObject(L);
  lua_pushinteger(L, integer(panel.BevelWidth));
  result:=1;
end;

function panel_setBevelWidth(L: PLua_State): integer; cdecl;
var
  panel: Tcustompanel;
begin
  result:=0;
  panel:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    panel.BevelWidth:=TBevelWidth(lua_tointeger(L,-1));
end;

function panel_getFullRepaint(L: PLua_State): integer; cdecl;
var
  panel: Tcustompanel;
begin
  panel:=luaclass_getClassObject(L);
  lua_pushboolean(L, panel.FullRepaint);
  result:=1;
end;

function panel_setFullRepaint(L: PLua_State): integer; cdecl;
var
  panel: Tcustompanel;
begin
  result:=0;
  panel:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    panel.FullRepaint:=lua_toboolean(L,-1);
end;

procedure panel_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  customcontrol_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getAlignment', panel_getAlignment);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setAlignment', panel_setAlignment);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getBevelInner', panel_getBevelInner);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setBevelInner', panel_setBevelInner);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getBevelOuter', panel_getBevelOuter);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setBevelOuter', panel_setBevelOuter);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getBevelWidth', panel_getBevelWidth);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setBevelWidth', panel_setBevelWidth);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getFullRepaint', panel_getFullRepaint);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setFullRepaint', panel_setFullRepaint);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Alignment', panel_getAlignment, panel_setAlignment);
  luaclass_addPropertyToTable(L, metatable, userdata, 'BevelInner', panel_getBevelInner, panel_setBevelInner);
  luaclass_addPropertyToTable(L, metatable, userdata, 'BevelOuter', panel_getBevelOuter, panel_setBevelOuter);
  luaclass_addPropertyToTable(L, metatable, userdata, 'BevelWidth', panel_getBevelWidth, panel_setBevelWidth);
  luaclass_addPropertyToTable(L, metatable, userdata, 'FullRepaint', panel_getFullRepaint, panel_setFullRepaint);

end;

procedure initializeLuaPanel;
begin
  lua_register(LuaVM, 'createPanel', createPanel);
  lua_register(LuaVM, 'panel_getAlignment', panel_getAlignment);
  lua_register(LuaVM, 'panel_setAlignment', panel_setAlignment);
  lua_register(LuaVM, 'panel_getBevelInner', panel_getBevelInner);
  lua_register(LuaVM, 'panel_setBevelInner', panel_setBevelInner);
  lua_register(LuaVM, 'panel_getBevelOuter', panel_getBevelOuter);
  lua_register(LuaVM, 'panel_setBevelOuter', panel_setBevelOuter);
  lua_register(LuaVM, 'panel_getBevelWidth', panel_getBevelWidth);
  lua_register(LuaVM, 'panel_setBevelWidth', panel_setBevelWidth);
  lua_register(LuaVM, 'panel_getFullRepaint', panel_getFullRepaint);
  lua_register(LuaVM, 'panel_setFullRepaint', panel_setFullRepaint);

end;

initialization
  luaclass_register(TCustomPanel, panel_addMetaData);

end.

