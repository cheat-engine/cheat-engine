unit LuaD3DHook;

{$mode delphi}

interface

uses
  Classes, SysUtils, graphics, lua, lualib, lauxlib;

procedure initializeLuaD3DHook;

implementation

uses d3dhookUnit, LuaCaller, LuaHandler;

function d3dhook_getWidth(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: TD3DHook;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    d:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    d:=safed3dhook;
    if d<>nil then
    begin
      lua_pushinteger(L, d.getWidth);
      result:=1;
    end;

  end else lua_pop(L, parameters);
end;

function d3dhook_getHeight(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: TD3DHook;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    d:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    d:=safed3dhook;
    if d<>nil then
    begin
      lua_pushinteger(L, d.getHeight);
      result:=1;
    end;

  end else lua_pop(L, parameters);
end;

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
  begin
    d.beginCommandListUpdate;
    d.beginTextureupdate;
  end;
end;

function d3dhook_endUpdate(L: PLua_State): integer; cdecl;
var
  d: TD3DHook;
begin
  result:=0;
  lua_pop(L, lua_gettop(L));
  d:=safed3dhook;
  if d<>nil then
  begin
    d.endTextureupdate;
    d.endCommandListUpdate;
  end;
end;


function d3dhook_setDisabledZBuffer(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: TD3DHook;
  state: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    state:=lua_toboolean(L, -parameters);

    lua_pop(L, parameters);

    d:=safed3dhook;
    if d<>nil then
      d.setDisabledZBuffer(state);

  end else lua_pop(L, parameters);
end;

function d3dhook_setWireframeMode(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: TD3DHook;
  state: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    state:=lua_toboolean(L, -parameters);

    lua_pop(L, parameters);

    d:=safed3dhook;
    if d<>nil then
      d.setWireframeMode(state);

  end else lua_pop(L, parameters);
end;


function d3dhook_setMouseClip(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: TD3DHook;
  state: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    state:=lua_toboolean(L, -parameters);

    lua_pop(L, parameters);

    d:=safed3dhook;
    if d<>nil then
      d.setMouseClip(state);

  end else lua_pop(L, parameters);
end;

procedure initializeLuaD3DHook;
begin
  lua_register(LuaVM, 'd3dhook_initializeHook', d3dhook_initializeHook);
  lua_register(LuaVM, 'd3dhook_onClick', d3dhook_onClick);
  lua_register(LuaVM, 'd3dhook_beginUpdate', d3dhook_beginUpdate);
  lua_register(LuaVM, 'd3dhook_endUpdate', d3dhook_endUpdate);
  lua_register(LuaVM, 'd3dhook_getWidth', d3dhook_getWidth);
  lua_register(LuaVM, 'd3dhook_getHeight', d3dhook_getHeight);
  lua_register(LuaVM, 'd3dhook_setDisabledZBuffer', d3dhook_setDisabledZBuffer);
  lua_register(LuaVM, 'd3dhook_setWireframeMode', d3dhook_setWireframeMode);
  lua_register(LuaVM, 'd3dhook_setMouseClip', d3dhook_setMouseClip);


end;

end.

