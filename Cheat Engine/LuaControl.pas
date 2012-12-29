unit LuaControl;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, Controls, Menus;

procedure initializeLuaControl;

implementation

uses luahandler, pluginexports, LuaCaller;

function control_getFont(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TControl;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    c:=lua_toceuserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, c.Font);
    result:=1;

  end else lua_pop(L, parameters);
end;

function control_setCaption(L: Plua_State): integer; cdecl;
var parameters: integer;
  c: pointer;
  caption: pchar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    c:=lua_toceuserdata(L, -2);
    caption:=lua.lua_tostring(L, -1);
    ce_control_setCaption(c,caption);
  end;

  lua_pop(L, lua_gettop(L));
end;

function control_getCaption(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: pchar;
  control: pointer;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_toceuserdata(L,-1);
    lua_pop(L, parameters);

    getmem(d,255);
    try
      if ce_control_getCaption(control, d, 255) then
      begin
        lua_pushstring(L, d);
        result:=1;
      end;


    finally
      freemem(d);
    end;
  end else lua_pop(L, parameters);
end;


function control_setPosition(L: Plua_State): integer; cdecl;
var parameters: integer;
  c: pointer;
  x,y: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    c:=lua_toceuserdata(L, -3);
    x:=lua_tointeger(L, -2);
    y:=lua_tointeger(L, -1);

    ce_control_setPosition(c,x,y);
  end;

  lua_pop(L, lua_gettop(L));
end;


function control_getPosition(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: pointer;
  x,y: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_toceuserdata(L,-1);
    lua_pop(L, parameters);

    x:=ce_control_getX(control);
    y:=ce_control_getY(control);

    lua_pushinteger(L, x);
    lua_pushinteger(L, y);
    result:=2;

  end else lua_pop(L, parameters);
end;

function control_setSize(L: Plua_State): integer; cdecl;
var parameters: integer;
  c: pointer;
  width,height: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    c:=lua_toceuserdata(L, -3);
    width:=lua_tointeger(L, -2);
    height:=lua_tointeger(L, -1);

    ce_control_setSize(c,width,height);
  end;

  lua_pop(L, lua_gettop(L));
end;

function control_getSize(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: pointer;
  width,height: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_toceuserdata(L,-1);
    lua_pop(L, parameters);

    width:=ce_control_getWidth(control);
    height:=ce_control_getHeight(control);

    lua_pushinteger(L, width);
    lua_pushinteger(L, height);
    result:=2;

  end else lua_pop(L, parameters);
end;


function control_setAlign(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: pointer;
  a: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_toceuserdata(L,-2);
    a:=lua_tointeger(L,-1);
    ce_control_setAlign(control,a);
  end;

  lua_pop(L, parameters);
end;

function control_getAlign(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
  align: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_toceuserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, integer(control.Align));
    result:=1;

  end else lua_pop(L, parameters);
end;

function control_setEnabled(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
  Enabled: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_toceuserdata(L,-2);
    control.Enabled:=lua_toboolean(L,-1);
  end;

  lua_pop(L, parameters);
end;

function control_getEnabled(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_toceuserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, control.Enabled);
    result:=1;

  end else lua_pop(L, parameters);
end;


function control_setVisible(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
  visible: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_toceuserdata(L,1);
    control.visible:=lua_toboolean(L,2);
  end;

  lua_pop(L, parameters);
end;

function control_getVisible(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_toceuserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushboolean(L, control.Visible);
    result:=1;

  end else lua_pop(L, parameters);
end;

function control_setColor(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
  Color: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_toceuserdata(L,-2);
    control.Color:=lua_tointeger(L,-1);
  end;

  lua_pop(L, parameters);
end;

function control_getColor(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_toceuserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, control.color);
    result:=1;

  end else lua_pop(L, parameters);
end;


function control_setParent(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
  Parent: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_toceuserdata(L,-2);
    control.Parent:=TWinControl(lua_toceuserdata(L,-1));
  end;

  lua_pop(L, parameters);
end;

function control_getParent(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_toceuserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, control.Parent);
    result:=1;

  end else lua_pop(L, parameters);
end;

function control_setPopupMenu(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
  PopupMenu: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_toceuserdata(L,-2);
    control.PopupMenu:=TPopupMenu(lua_toceuserdata(L,-1));
  end;

  lua_pop(L, parameters);
end;

function control_getPopupMenu(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_toceuserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, control.PopupMenu);
    result:=1;

  end else lua_pop(L, parameters);
end;

function control_doClick(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
  Color: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    control:=lua_toceuserdata(L,1);
    if assigned(control.onclick) then
      control.OnClick(control);
  end;

  lua_pop(L, parameters);
end;

function control_onClick(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  control: TControl;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_toceuserdata(L,-2);

    CleanupLuaCall(tmethod(control.onClick));
    control.onClick:=nil;



    if lua_isfunction(L,-1) then
    begin
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
  lua_register(LuaVM, 'control_onClick', control_onClick);
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

end.

