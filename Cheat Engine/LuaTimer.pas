unit LuaTimer;

{$mode delphi}

interface

uses
  Classes, SysUtils, ExtCtrls, lua, lualib, lauxlib, CustomTimer;

procedure initializeLuaTimer;

implementation

uses luaclass, luahandler, luacaller, LuaComponent, Clipbrd, luasyntax;

type
  TRunOnceTimer=class(TTimer)
  private
    procedure DoOnTimer; override;
  public
  end;

procedure TRunOnceTimer.DoOnTimer;
begin
  inherited DoOnTimer;

  Enabled:=false;
  free;
end;

function createTimerInternal(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: pointer;

  t: TTimer;
  interval: integer;
  m: tmethod;
begin
  result:=0;
  parameters:=lua_gettop(L);
  f:=nil;

  if parameters>=1 then
  begin
    if lua_isinteger(L,1) then
    begin
      if lua_isfunction(L,2) then  //run once timer
      begin
        interval:=lua_tointeger(L,1);

        t:=TRunOnceTimer.Create(nil);
        t.interval:=interval;

        m:=tmethod(t.ontimer);
        LuaCaller_setMethodProperty(L, m, 'TNotifyEvent', 2);
        t.OnTimer:=tnotifyevent(m);

        luaclass_newClass(L,t); //will selfdestruct when done
        t.enabled:=true;
        exit(1);
      end;
    end
    else
    begin
      f:=lua_toceuserdata(L, 1);
      if (f<>nil) and (not (TObject(f) is TComponent)) then raise exception.create('createTimer: '+TObject(f).ClassName+' is not a Component');
    end;
  end;

  t:=ttimer.create(f);

  if parameters>=2 then
  begin
    if lua_isnil(L,2) then
      t.Enabled:=true
    else
      t.Enabled:=lua_toboolean(L, 2);
  end
  else
    t.enabled:=true;

  luaclass_newClass(L, t);
  result:=1;
end;

function timer_getInterval(L: Plua_State): integer; cdecl;
var
  t: TCustomTimer;
begin
  t:=luaclass_getClassObject(L);
  lua_pushinteger(L, t.Interval);
  result:=1;
end;

function timer_setInterval(L: Plua_State): integer; cdecl;
var
  t: TCustomTimer;
begin
  result:=0;
  t:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    t.Interval:=lua_tointeger(L, -1);
end;

function timer_getOnTimer(L: PLua_State): integer; cdecl;
var
  c: TCustomTimer;
begin
  c:=luaclass_getClassObject(L);
  LuaCaller_pushMethodProperty(L, TMethod(c.OnTimer), 'TNotifyEvent');
  result:=1;
end;

function timer_setonTimer(L: PLua_State): integer; cdecl;
var
  timer: TCustomTimer;
  m: tmethod;
begin
  if lua_gettop(L)>=1 then
  begin
    timer:=luaclass_getClassObject(L);
    m:=tmethod(timer.ontimer);
    LuaCaller_setMethodProperty(L, m, 'TNotifyEvent', lua_gettop(L));
    timer.ontimer:=tnotifyevent(m);
  end;
  result:=0;
end;

function timer_getEnabled(L: Plua_State): integer; cdecl;
var
  t: TCustomTimer;
begin
  t:=luaclass_getClassObject(L);
  lua_pushboolean(L, t.Enabled);
  result:=1;
end;

function timer_setEnabled(L: Plua_State): integer; cdecl;
var
  t: TCustomTimer;
begin
  result:=0;
  t:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    t.Enabled:=lua_toboolean(L, -1);
end;

procedure timer_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  component_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getInterval', timer_getInterval);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setInterval', timer_setInterval);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOnTimer', timer_getonTimer);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOnTimer', timer_setonTimer);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setEnabled', timer_setEnabled);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getEnabled', timer_getEnabled);

  Luaclass_addPropertyToTable(L, metatable, userdata, 'OnTimer', timer_getonTimer, timer_setonTimer);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Enabled', timer_getEnabled, timer_setEnabled);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Interval', timer_getInterval, timer_setInterval);
end;

procedure initializeLuaTimer;
var s: tstringlist;
begin
  lua_register(LuaVM, 'createTimerInternal', createTimerInternal);
  lua_register(LuaVM, 'timer_setInterval', timer_setInterval);
  lua_register(LuaVM, 'timer_onTimer', timer_setonTimer);
  lua_register(LuaVM, 'timer_setEnabled', timer_setEnabled);
  lua_register(LuaVM, 'timer_getEnabled', timer_getEnabled);

  s:=tstringlist.create;
  s.add('function createTimer(p1,p2,...)');
  s.add('  local params=table.pack(...)');
  s.add('  if type(p1)==''number'' and type(p2)==''function'' then');
  s.add('    return createTimerInternal(p1, function()');
  s.add('      p2(table.unpack(params))');
  s.add('    end)');
  s.add('  else');
  s.add('    return createTimerInternal(p1,p2,table.unpack(params))');
  s.add('  end');
  s.add('end');

  LUA_DoScript(s.text);
  if luasyntaxStringHashList<>nil then
  begin
    luasyntaxStringHashList.Add('createTimer');
    luasyntaxStringHashList.Add('CreateTimer');
  end;

  s.free;
end;

initialization
  luaclass_register(TCustomTimer, timer_addMetaData);

end.

