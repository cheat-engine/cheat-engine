unit LuaTrackbar;

{$mode delphi}

interface

uses
  Classes, SysUtils, controls, comctrls, lua, lualib, lauxlib;

procedure initializeLuaTrackbar;

implementation

uses luahandler, luaclass, luacaller, LuaWinControl;

//trackbar
function createTrackBar(L: Plua_State): integer; cdecl;
var
  TrackBar: TTrackBar;
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


  TrackBar:=TTrackBar.Create(owner);
  if owner<>nil then
    TrackBar.Parent:=owner;

  luaclass_newClass(L, TrackBar);
  result:=1;
end;

function trackbar_getMax(L: PLua_State): integer; cdecl;
var
  trackbar: Tcustomtrackbar;
begin
  trackbar:=luaclass_getClassObject(L);
  lua_pushinteger(L, trackbar.max);
  result:=1;
end;

function trackbar_setMax(L: PLua_State): integer; cdecl;
var
  trackbar: Tcustomtrackbar;
begin
  result:=0;
  trackbar:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
    trackbar.max:=lua_tointeger(L,-1);
end;

function trackbar_getMin(L: PLua_State): integer; cdecl;
var
  trackbar: Tcustomtrackbar;
begin
  trackbar:=luaclass_getClassObject(L);
  lua_pushinteger(L, trackbar.Min);
  result:=1;
end;

function trackbar_setMin(L: PLua_State): integer; cdecl;
var
  trackbar: Tcustomtrackbar;
begin
  result:=0;
  trackbar:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
    trackbar.Min:=lua_tointeger(L,-1);
end;


function trackbar_getPosition(L: PLua_State): integer; cdecl;
var
  trackbar: Tcustomtrackbar;
begin
  trackbar:=luaclass_getClassObject(L);
  lua_pushinteger(L, trackbar.Position);
  result:=1;
end;

function trackbar_setPosition(L: PLua_State): integer; cdecl;
var
  trackbar: Tcustomtrackbar;
begin
  result:=0;
  trackbar:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
    trackbar.Position:=lua_tointeger(L,-1);
end;

function trackbar_getonChange(L: PLua_State): integer; cdecl;
var
  c: TCustomTrackBar;
begin
  c:=luaclass_getClassObject(L);
  LuaCaller_pushMethodProperty(L, TMethod(c.OnChange), 'TNotifyEvent');
  result:=1;
end;

function trackbar_setonChange(L: PLua_State): integer; cdecl;
var
  control: TCustomTrackBar;
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

procedure trackbar_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getMax', trackbar_getMax);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setMax', trackbar_setMax);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getMin', trackbar_getMin);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setMin', trackbar_setMin);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getPosition', trackbar_getPosition);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setPosition', trackbar_setPosition);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOnChange', trackbar_setonChange);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOnChange', trackbar_getonChange);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Max', trackbar_getMax, trackbar_setMax);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Min', trackbar_getMin, trackbar_setMin);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Position', trackbar_getPosition, trackbar_setPosition);
  luaclass_addPropertyToTable(L, metatable, userdata, 'OnChange', trackbar_getonChange, trackbar_setonChange);
end;

procedure initializeLuaTrackbar;
begin
  lua_register(LuaVM, 'createTrackBar', createTrackBar);
  lua_register(LuaVM, 'trackbar_getMax', trackbar_getMax);
  lua_register(LuaVM, 'trackbar_setMax', trackbar_setMax);
  lua_register(LuaVM, 'trackbar_getMin', trackbar_getMin);
  lua_register(LuaVM, 'trackbar_setMin', trackbar_setMin);
  lua_register(LuaVM, 'trackbar_getPosition', trackbar_getPosition);
  lua_register(LuaVM, 'trackbar_setPosition', trackbar_setPosition);
  lua_register(LuaVM, 'trackbar_onChange', trackbar_setonChange);
end;

initialization
  luaclass_register(TCustomTrackbar, trackbar_addMetaData);

end.

