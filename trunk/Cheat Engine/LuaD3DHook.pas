unit LuaD3DHook;

{$mode delphi}

interface

uses
  Classes, SysUtils, graphics, lua, lualib, lauxlib;

procedure initializeLuaD3DHook;

implementation

uses d3dhookUnit, LuaCaller, LuaHandler;

function d3dhook_initializeHook(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  size: integer;
  hookwindow: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
    size:=lua_tointeger(L, -parameters)
  else
    size:=16*1024*1024;

  if parameters>=2 then
    hookwindow:=lua_toboolean(L, -parameters+1)
  else
    hookwindow:=true;


  lua_pop(L, parameters);

  lua_pushboolean(L, safed3dhook(size, hookwindow)<>nil);
  result:=1;
end;

function d3dhook_createOverlay(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  picture: Tpicture;
  x,y: integer;
  d: TD3DHook;
  i: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    picture:=lua_touserdata(L,-parameters);
    x:=lua_tointeger(L, -parameters+1);
    y:=lua_tointeger(L, -parameters+2);
    lua_pop(L, parameters);

    d:=safed3dhook;
    if d<>nil then
    begin
      i:=d.createOverlayFromPicture(picture,x,y);
      lua_pushinteger(L, i);
      result:=1;
    end;

  end else lua_pop(L, parameters);
end;

function d3dhook_updateOverlayImage(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: TD3DHook;
  overlayid: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    overlayid:=lua_tointeger(L, -parameters);
    lua_pop(L, parameters);

    d:=safed3dhook;
    if d<>nil then
      d.updateOverlayImage(overlayid);

  end else lua_pop(L, parameters);
end;

function d3dhook_updateOverlayPosition(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: TD3DHook;
  overlayid,x,y: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    overlayid:=lua_tointeger(L, -parameters);
    x:=lua_tointeger(L, -parameters+1);
    y:=lua_tointeger(L, -parameters+2);

    lua_pop(L, parameters);

    d:=safed3dhook;
    if d<>nil then
      d.updateOverlayPosition(overlayid,x,y);

  end else lua_pop(L, parameters);
end;

function d3dhook_setOverlayVisibility(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: TD3DHook;
  overlayid: integer;
  state: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    overlayid:=lua_tointeger(L, -parameters);
    state:=lua_toboolean(L, -parameters+1);

    lua_pop(L, parameters);

    d:=safed3dhook;
    if d<>nil then
      d.SetOverlayVisibility(overlayid,state);

  end else lua_pop(L, parameters);
end;

function d3dhook_setOverlayAsMouse(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: TD3DHook;
  overlayid: integer;
  state: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    overlayid:=lua_tointeger(L, -parameters);
    lua_pop(L, parameters);

    d:=safed3dhook;
    if d<>nil then
      d.setOverlayAsMouse(overlayid);

  end else lua_pop(L, parameters);
end;


function d3dhook_onClick(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: TD3DHook;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    d:=lua_touserdata(L,-2);

    CleanupLuaCall(TMethod(d.onClick));
    d.onClick:=nil;

    if lua_isfunction(L,-1) then
    begin
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      d.OnClick:=lc.D3DClickEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      d.OnClick:=lc.D3DClickEvent;
    end;

  end;

  lua_pop(L, parameters);
end;


function d3dhook_beginUpdate(L: PLua_State): integer; cdecl;
var
  d: TD3DHook;
begin
  result:=0;
  lua_pop(L, lua_gettop(L));
  d:=safed3dhook;
  if d<>nil then
    d.beginupdate;
end;

function d3dhook_endUpdate(L: PLua_State): integer; cdecl;
var
  d: TD3DHook;
begin
  result:=0;
  lua_pop(L, lua_gettop(L));
  d:=safed3dhook;
  if d<>nil then
    d.endupdate;
end;


procedure initializeLuaD3DHook;
begin
  lua_register(LuaVM, 'd3dhook_initializeHook', d3dhook_initializeHook);
  lua_register(LuaVM, 'd3dhook_createOverlay', d3dhook_createOverlay);
  lua_register(LuaVM, 'd3dhook_updateOverlayImage', d3dhook_updateOverlayImage);
  lua_register(LuaVM, 'd3dhook_updateOverlayPosition', d3dhook_updateOverlayPosition);
  lua_register(LuaVM, 'd3dhook_setOverlayVisibility', d3dhook_setOverlayVisibility);
  lua_register(LuaVM, 'd3dhook_setOverlayAsMouse', d3dhook_setOverlayAsMouse);
  lua_register(LuaVM, 'd3dhook_onClick', d3dhook_onClick);
  lua_register(LuaVM, 'd3dhook_beginUpdate', d3dhook_beginUpdate);
  lua_register(LuaVM, 'd3dhook_endUpdate', d3dhook_endUpdate);
end;

end.

