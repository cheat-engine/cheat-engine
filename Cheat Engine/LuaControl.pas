unit LuaControl;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, Controls, Menus, Graphics;

procedure initializeLuaControl;
procedure control_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses luahandler, pluginexports, LuaCaller, LuaComponent, LuaClass, LuaObject;

function control_repaint(L: PLua_State): integer; cdecl;
begin
  TControl(luaclass_getClassObject(L)).Repaint;
  result:=0;
end;

function control_update(L: PLua_State): integer; cdecl;
begin
  TControl(luaclass_getClassObject(L)).Update;
  result:=0;
end;

function control_getFont(L: PLua_State): integer; cdecl;
var
  c: TControl;
begin
  c:=luaclass_getClassObject(L);
  luaclass_newClass(L, c.font);
  result:=1;
end;

function control_setFont(L: Plua_State): integer; cdecl;
var
  c: tcontrol;
begin
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    c.Font:=lua_ToCEUserData(L, -1);

  result:=0;
end;

function control_setCaption(L: Plua_State): integer; cdecl;
var
  c: tcontrol;
begin
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    c.Caption:=lua_tostring(L, -1);

  result:=0;
end;

function control_getCaption(L: PLua_State): integer; cdecl;
var
  c: TControl;
begin
  c:=luaclass_getClassObject(L);
  lua_pushstring(L, c.caption);
  result:=1;
end;


function control_setLeft(L: Plua_State): integer; cdecl;
var
  c: tcontrol;
begin
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then //6.3+, class only so no need for >= and negative indexes
    c.left:=lua_tointeger(L, 1);

  result:=0;
end;

function control_getLeft(L: Plua_State): integer; cdecl;
var
  c: tcontrol;
begin
  c:=luaclass_getClassObject(L);
  lua_pushinteger(L, c.left);
  result:=1;
end;

function control_setTop(L: Plua_State): integer; cdecl;
var
  c: tcontrol;
begin
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    c.top:=lua_tointeger(L, 1);

  result:=0;
end;

function control_getTop(L: Plua_State): integer; cdecl;
var
  c: tcontrol;
begin
  c:=luaclass_getClassObject(L);
  lua_pushinteger(L, c.top);
  result:=1;
end;

function control_setPosition(L: Plua_State): integer; cdecl;
var
  c: TControl;
begin
  result:=0;
  c:=luaclass_getClassObject(L);

  if lua_gettop(L)>=2 then
  begin
    c.Left:=lua_tointeger(L, -2);
    c.Top:=lua_tointeger(L, -1);
  end;
end;


function control_getPosition(L: PLua_State): integer; cdecl;
var
  c: TControl;
  x,y: integer;
begin
  result:=0;
  c:=luaclass_getClassObject(L);

  lua_pushinteger(L, c.left);
  lua_pushinteger(L, c.top);
  result:=2;
end;

function control_setWidth(L: Plua_State): integer; cdecl;
var
  c: tcontrol;
begin
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    c.Width:=lua_tointeger(L, 1);

  result:=0;
end;

function control_getWidth(L: Plua_State): integer; cdecl;
var
  c: tcontrol;
begin
  c:=luaclass_getClassObject(L);
  lua_pushinteger(L, c.Width);
  result:=1;
end;


function control_setHeight(L: Plua_State): integer; cdecl;
var
  c: tcontrol;
begin
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    c.Height:=lua_tointeger(L, 1);

  result:=0;
end;

function control_getHeight(L: Plua_State): integer; cdecl;
var
  c: tcontrol;
begin
  c:=luaclass_getClassObject(L);
  lua_pushinteger(L, c.Height);
  result:=1;
end;


function control_setClientWidth(L: Plua_State): integer; cdecl;
var
  c: tcontrol;
begin
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    c.ClientWidth:=lua_tointeger(L, 1);

  result:=0;
end;

function control_getClientWidth(L: Plua_State): integer; cdecl;
var
  c: tcontrol;
begin
  c:=luaclass_getClassObject(L);
  lua_pushinteger(L, c.ClientWidth);
  result:=1;
end;


function control_setClientHeight(L: Plua_State): integer; cdecl;
var
  c: tcontrol;
begin
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    c.ClientHeight:=lua_tointeger(L, 1);

  result:=0;
end;

function control_getClientHeight(L: Plua_State): integer; cdecl;
var
  c: tcontrol;
begin
  c:=luaclass_getClassObject(L);
  lua_pushinteger(L, c.ClientHeight);
  result:=1;
end;

function control_setSize(L: Plua_State): integer; cdecl;
var
  c: TControl;
begin
  c:=luaclass_getClassObject(L);


  if lua_gettop(L)>=2 then
  begin
    c.Width:=lua_tointeger(L, -2);
    c.height:=lua_tointeger(L, -1);
  end;
  result:=0;
end;

function control_getSize(L: PLua_State): integer; cdecl;
var
  c: TControl;
begin
  c:=luaclass_getClassObject(L);
  lua_pushinteger(L, c.width);
  lua_pushinteger(L, c.height);
  result:=2;
end;


function control_setAlign(L: PLua_State): integer; cdecl;
var
  c: TControl;
  a: integer;
begin
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    c.Align:=Talign(lua_tointeger(L,-1));

  result:=0;
end;

function control_getAlign(L: PLua_State): integer; cdecl;
var
  c: TControl;
begin
  c:=luaclass_getClassObject(L);
  lua_pushinteger(L, integer(c.Align));
  result:=1;
end;

function control_setEnabled(L: PLua_State): integer; cdecl;
var
  c: TControl;
begin
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    c.Enabled:=lua_toboolean(L,-1);

  result:=0;
end;

function control_getEnabled(L: PLua_State): integer; cdecl;
var
  c: TControl;
begin
  c:=luaclass_getClassObject(L);
  lua_pushboolean(L, c.Enabled);
  result:=1;
end;


function control_setVisible(L: PLua_State): integer; cdecl;
var
  c: TControl;
begin
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    c.Visible:=lua_toboolean(L,-1);

  result:=0;
end;

function control_getVisible(L: PLua_State): integer; cdecl;
var
  c: TControl;
begin
  c:=luaclass_getClassObject(L);
  lua_pushboolean(L, c.Visible);
  result:=1;
end;

function control_setColor(L: PLua_State): integer; cdecl;
var
  c: TControl;
begin
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    c.Color:=Tcolor(lua_tointeger(L,-1));

  result:=0;
end;

function control_getColor(L: PLua_State): integer; cdecl;
var
  c: TControl;
begin
  c:=luaclass_getClassObject(L);
  lua_pushinteger(L, integer(c.color));
  result:=1;
end;


function control_setParent(L: PLua_State): integer; cdecl;
var
  c: TControl;
begin
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    c.Parent:=lua_ToCEUserData(L, -1);

  result:=0;
end;

function control_getParent(L: PLua_State): integer; cdecl;
var
  c: TControl;
begin
  c:=luaclass_getClassObject(L);
  luaclass_newClass(L, c.parent);
  result:=1;
end;

function control_setPopupMenu(L: PLua_State): integer; cdecl;
var
  c: TControl;
begin
  c:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    c.PopupMenu:=lua_ToCEUserData(L, -1);

  result:=0;
end;

function control_getPopupMenu(L: PLua_State): integer; cdecl;
var
  c: TControl;
  i: integer;
begin
  c:=luaclass_getClassObject(L);

  luaclass_newClass(L,c);
  lua_pushstring(L,'PopupMenu');
  lua_getProperty(L);

  if lua_isnil(L,-1) then
    luaclass_newClass(L, c.PopupMenu);

  result:=1;
end;

function control_doClick(L: PLua_State): integer; cdecl;
var
  c: TControl;
begin
  result:=0;
  c:=luaclass_getClassObject(L);
  if assigned(c.OnClick) then
    c.OnClick(c);
end;

function control_getOnClick(L: PLua_State): integer; cdecl;
var
  c: tcontrol;
begin
  c:=luaclass_getClassObject(L);
  LuaCaller_pushMethodProperty(L, TMethod(c.onclick), 'TNotifyEvent');
  result:=1;
end;

function control_setOnClick(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TControl;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  c:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    CleanupLuaCall(tmethod(c.onClick));
    c.onClick:=nil;

    if lua_isfunction(L,-1) then
    begin
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      c.OnClick:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      c.OnClick:=lc.NotifyEvent;
    end;

  end;
end;

function control_bringToFront(L: PLua_State): integer; cdecl;
var
  control: TControl;
begin
  control:=luaclass_getClassObject(L);
  control.BringToFront;
  result:=0
end;

function control_sendToBack(L: PLua_State): integer; cdecl;
var
  control: TControl;
begin
  control:=luaclass_getClassObject(L);
  control.SendToBack;
  result:=0
end;

function control_screenToClient(L: PLua_State): integer; cdecl;
var
  control: TControl;
  p: tpoint;
begin
  result:=0;
  control:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
  begin
    p.x:=lua_tointeger(L,1);
    p.y:=lua_tointeger(L,2);

    p:=control.ScreenToClient(p);
    lua_pushinteger(L,p.x);
    lua_pushinteger(L,p.y);
    result:=2;
  end;
end;

function control_clientToScreen(L: PLua_State): integer; cdecl;
var
  control: TControl;
  p: tpoint;
begin
  result:=0;
  control:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
  begin
    p.x:=lua_tointeger(L,1);
    p.y:=lua_tointeger(L,2);

    p:=control.ClientToScreen(p);
    lua_pushinteger(L,p.x);
    lua_pushinteger(L,p.y);
    result:=2;
  end;
end;

procedure control_addMetaData(L: PLua_state; metatable: integer; userdata: integer);
begin
  component_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setCaption', control_setCaption);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCaption', control_getCaption);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setLeft', control_setLeft);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getLeft', control_getLeft);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setTop', control_setTop);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getTop', control_getTop);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setPosition', control_setPosition);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getPosition', control_getPosition);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setWidth', control_setWidth);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getWidth', control_getWidth);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setHeight', control_setHeight);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getHeight', control_getHeight);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setSize', control_setSize);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getSize', control_getSize);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setAlign', control_setAlign);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getAlign', control_getAlign);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOnClick', control_setOnClick);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'doClick', control_doClick);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setEnabled', control_setEnabled);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getEnabled', control_getEnabled);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setVisible', control_setVisible);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getVisible', control_getVisible);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setColor', control_setColor);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getColor', control_getColor);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setParent', control_setParent);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getParent', control_getParent);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setPopupMenu', control_setPopupMenu);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getPopupMenu', control_getPopupMenu);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getFont', control_getFont);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'repaint', control_repaint);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'update', control_update);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'bringToFront', control_bringToFront);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'sendToBack', control_sendToBack);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'screenToClient', control_screenToClient);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clientToScreen', control_clientToScreen);

  //luaclass_addPropertyToTable(L, metatable, userdata, 'Caption', control_getCaption, control_setCaption);
  //luaclass_addPropertyToTable(L, metatable, userdata, 'Top', control_getTop, control_setTop);
  //luaclass_addPropertyToTable(L, metatable, userdata, 'Left', control_getLeft, control_setLeft);
  //luaclass_addPropertyToTable(L, metatable, userdata, 'Width', control_getWidth, control_setWidth);
  //luaclass_addPropertyToTable(L, metatable, userdata, 'Height', control_getHeight, control_setHeight);
  //luaclass_addPropertyToTable(L, metatable, userdata, 'Align', control_getAlign, control_setAlign);
  //luaclass_addPropertyToTable(L, metatable, userdata, 'Enabled', control_getEnabled, control_setEnabled);
  //luaclass_addPropertyToTable(L, metatable, userdata, 'Visible', control_getVisible, control_setVisible);
  //luaclass_addPropertyToTable(L, metatable, userdata, 'Color', control_getColor, control_setColor);

  luaclass_addPropertyToTable(L, metatable, userdata, 'ClientWidth', control_getClientWidth, control_setClientWidth);
  luaclass_addPropertyToTable(L, metatable, userdata, 'ClientHeight', control_getClientHeight, control_setClientHeight);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Parent', control_getParent, control_setParent);
  luaclass_addPropertyToTable(L, metatable, userdata, 'PopupMenu', control_getPopupMenu, control_setPopupMenu);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Font', control_getFont, control_setFont);
  luaclass_addPropertyToTable(L, metatable, userdata, 'OnClick', control_getOnClick, control_setOnClick);
end;


procedure initializeLuaControl;
begin
  lua_register(LuaVM, 'control_setCaption', control_setCaption);
  lua_register(LuaVM, 'control_getCaption', control_getCaption);
  lua_register(LuaVM, 'control_setPosition', control_setPosition);
  lua_register(LuaVM, 'control_getPosition', control_getPosition);
  lua_register(LuaVM, 'control_setSize', control_setSize);
  lua_register(LuaVM, 'control_getSize', control_getSize);
  lua_register(LuaVM, 'control_setAlign', control_setAlign);
  lua_register(LuaVM, 'control_getAlign', control_getAlign);
  lua_register(LuaVM, 'control_onClick', control_setOnClick);
  lua_register(LuaVM, 'control_doClick', control_doClick);
  lua_register(LuaVM, 'control_setEnabled', control_setEnabled);
  lua_register(LuaVM, 'control_getEnabled', control_getEnabled);
  lua_register(LuaVM, 'control_setVisible', control_setVisible);
  lua_register(LuaVM, 'control_getVisible', control_getVisible);
  lua_register(LuaVM, 'control_setColor', control_setColor);
  lua_register(LuaVM, 'control_getColor', control_getColor);
  lua_register(LuaVM, 'control_setParent', control_setParent);
  lua_register(LuaVM, 'control_getParent', control_getParent);
  lua_register(LuaVM, 'control_setPopupMenu', control_setPopupMenu);
  lua_register(LuaVM, 'control_getPopupMenu', control_getPopupMenu);
  lua_register(LuaVM, 'control_getFont', control_getFont);
end;

initialization
  luaclass_register(TControl, control_addMetaData);

end.

