unit LuaWinControl;

{$mode delphi}

interface

uses
  {$ifdef windows}windows,{$endif}

  Classes, SysUtils, controls, lua, lualib, lauxlib,LuaHandler, graphics;

procedure initializeLuaWinControl;
procedure wincontrol_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses LuaCaller, luacontrol, luaclass;

function wincontrol_getHandle(L: PLua_State): integer; cdecl;
begin
  lua_pushinteger(L, twincontrol(luaclass_getClassObject(L)).Handle);
  result:=1;
end;

function wincontrol_getDoubleBuffered(L: PLua_State): integer; cdecl;
begin
  lua_pushboolean(L, twincontrol(luaclass_getClassObject(L)).DoubleBuffered);
  result:=1;
end;

function wincontrol_setDoubleBuffered(L: PLua_State): integer; cdecl;
begin
  if lua_gettop(L)=1 then
    twincontrol(luaclass_getClassObject(L)).DoubleBuffered:=lua_toboolean(L, 1);

  result:=0;
end;


function wincontrol_getControlCount(L: PLua_State): integer; cdecl;
begin
  lua_pushinteger(L, twincontrol(luaclass_getClassObject(L)).ControlCount);
  result:=1;
end;

function wincontrol_getControl(L: PLua_State): integer; cdecl;
var
  wincontrol: TWinControl;
  index: integer;
begin
  wincontrol:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    index:=lua_tointeger(L,-1);
    luaclass_newClass(L, wincontrol.Controls[index]);
  end
  else
    lua_pushnil(L);

  result:=1;
end;

function wincontrol_getControlAtPos(L: PLua_State): integer; cdecl;
var
  wincontrol: TWinControl;
  x,y: integer;
  paramstart, paramcount: integer;
begin
  wincontrol:=luaclass_getClassObject(L, @paramstart, @paramcount);
  result:=0;

  if paramcount>=2 then
  begin
    x:=lua_tointeger(L,paramstart);
    y:=lua_tointeger(L,paramstart+1);

    luaclass_newClass(L, wincontrol.ControlAtPos(point(x,y),[capfOnlyClientAreas, capfAllowWinControls, capfRecursive]));
    result:=1;
  end;
end;

function wincontrol_getOnEnter(L: PLua_State): integer; cdecl;
var
  c: twincontrol;
begin
  c:=luaclass_getClassObject(L);
  LuaCaller_pushMethodProperty(L, TMethod(c.OnEnter), 'TNotifyEvent');
  result:=1;
end;

function wincontrol_setOnEnter(L: PLua_State): integer; cdecl;
var
  wincontrol: TWinControl;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  wincontrol:=luaclass_getClassObject(L);
  result:=0;

  if lua_gettop(L)>=1 then
  begin
    CleanupLuaCall(tmethod(wincontrol.OnEnter));
    wincontrol.OnEnter:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      wincontrol.OnEnter:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      wincontrol.OnEnter:=lc.NotifyEvent;
    end;

  end;
end;

function wincontrol_getOnExit(L: PLua_State): integer; cdecl;
var
  c: twincontrol;
begin
  c:=luaclass_getClassObject(L);
  LuaCaller_pushMethodProperty(L, TMethod(c.OnExit), 'TNotifyEvent');
  result:=1;
end;

function wincontrol_setOnExit(L: PLua_State): integer; cdecl;
var
  wincontrol: TWinControl;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  wincontrol:=luaclass_getClassObject(L);
  result:=0;
  if lua_gettop(L)>=1 then
  begin

    CleanupLuaCall(tmethod(wincontrol.onExit));
    wincontrol.onExit:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      wincontrol.OnExit:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      wincontrol.OnExit:=lc.NotifyEvent;
    end;

  end;
end;

function wincontrol_canFocus(L: PLua_State): integer; cdecl;
var
  wincontrol: TWinControl;
begin
  wincontrol:=luaclass_getClassObject(L);
  lua_pushboolean(L, wincontrol.CanFocus);
  result:=1;
end;

function wincontrol_focused(L: PLua_State): integer; cdecl;
var
  wincontrol: TWinControl;
begin
  wincontrol:=luaclass_getClassObject(L);
  lua_pushboolean(L, wincontrol.Focused);
  result:=1;
end;

function wincontrol_setFocus(L: PLua_State): integer; cdecl;
var
  wincontrol: TWinControl;
begin
  wincontrol:=luaclass_getClassObject(L);
  wincontrol.SetFocus;
  result:=0
end;

function wincontrol_setShape(L: PLua_State): integer; cdecl;
var
  wincontrol: TWinControl;
  x: TObject;
begin
  wincontrol:=luaclass_getClassObject(L);
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    x:=lua_toceuserdata(L, -1);

    if (x is TBitmap) then
      wincontrol.SetShape(TBitmap(x))
    else
    if (x is TRegion) then
      wincontrol.SetShape(TRegion(x));
  end;
end;


function wincontrol_setLayeredAttributes(L: PLua_State): integer; cdecl;
var
  h: thandle;
  key: dword;
  alpha: byte;
  flags: byte;
begin
  //only works on forms in windows 7 and earlier, but also works on child components in windows 8 and later
  result:=0;

  {$ifdef windows}

  if lua_gettop(L)>=3 then
  begin
    h:=twincontrol(luaclass_getClassObject(L)).handle;
    if SetWindowLong(h, GWL_EXSTYLE, GetWindowLong(h, GWL_EXSTYLE) or WS_EX_LAYERED)=0 then
    begin
      result:=1;
      lua_pushboolean(L, false);
      exit; //not supported
    end;

    key:=lua_tointeger(L, 1);
    alpha:=lua_tointeger(L, 2);
    flags:=lua_tointeger(L, 3);

    result:=1;
    lua_pushboolean(L, SetLayeredWindowAttributes(h, key, alpha, flags));
  end;
  {$endif}
end;

procedure wincontrol_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  control_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getControlCount', wincontrol_getControlCount);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getControl', wincontrol_getControl);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getControlAtPos', wincontrol_getControlAtPos);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOnEnter', wincontrol_setOnEnter);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOnExit', wincontrol_setOnExit);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'canFocus', wincontrol_canFocus);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'focused', wincontrol_focused);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setFocus', wincontrol_setFocus);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setShape', wincontrol_setShape);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setLayeredAttributes', wincontrol_setLayeredAttributes);

  luaclass_addPropertyToTable(L, metatable, userdata, 'DoubleBuffered', wincontrol_getDoubleBuffered, wincontrol_setDoubleBuffered);
  luaclass_addPropertyToTable(L, metatable, userdata, 'ControlCount', wincontrol_getControlCount, nil);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Control', wincontrol_getControl);
  luaclass_addPropertyToTable(L, metatable, userdata, 'OnEnter', wincontrol_getOnEnter, wincontrol_setOnEnter);
  luaclass_addPropertyToTable(L, metatable, userdata, 'OnExit', wincontrol_getOnExit, wincontrol_setOnExit);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Handle', wincontrol_getHandle, nil);
end;

procedure initializeLuaWinControl;
begin
  lua_register(LuaVM, 'wincontrol_getControlCount', wincontrol_getControlCount);
  lua_register(LuaVM, 'wincontrol_getControl', wincontrol_getControl);
  lua_register(LuaVM, 'wincontrol_getControlAtPos', wincontrol_getControlAtPos);
  lua_register(LuaVM, 'wincontrol_onEnter', wincontrol_setOnEnter);
  lua_register(LuaVM, 'wincontrol_onExit', wincontrol_setOnExit);
  lua_register(LuaVM, 'wincontrol_canFocus', wincontrol_canFocus);
  lua_register(LuaVM, 'wincontrol_focused', wincontrol_focused);
  lua_register(LuaVM, 'wincontrol_setFocus', wincontrol_setFocus);
  lua_register(LuaVM, 'wincontrol_setShape', wincontrol_setShape);

end;

initialization
  luaclass_register(TWinControl, wincontrol_addMetaData );


end.

