unit LuaCustomControl;

{$mode delphi}

interface

uses
  Classes, SysUtils, Controls, lua, lualib, lauxlib;

procedure customcontrol_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
procedure initializeLuaCustomControl;

implementation

uses luahandler, luaclass, LuaWinControl, LuaCaller;


function customControl_getCanvas(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TCustomControl;
  i: integer;
begin
  c:=luaclass_getClassObject(L);
  if c.Canvas.handle=0 then
    i:=c.Canvas.Pixels[0,0];

  luaclass_newClass(L, c.Canvas);
  result:=1;
end;

function customControl_getOnPaint(L: PLua_State): integer; cdecl;
var
  c: TCustomControl;
begin
  c:=luaclass_getClassObject(L);
  LuaCaller_pushMethodProperty(L, TMethod(c.OnPaint), 'TNotifyEvent');
  result:=1;
end;

function customControl_setOnPaint(L: PLua_State): integer; cdecl;
var
  c: TCustomControl;
  m: tmethod;
begin
  if lua_gettop(L)>=1 then
  begin
    c:=luaclass_getClassObject(L);
    m:=tmethod(c.OnPaint);
    LuaCaller_setMethodProperty(L, m, 'TNotifyEvent', lua_gettop(L));
    c.OnPaint:=tnotifyevent(m);
  end;
  result:=0;
end;


procedure customcontrol_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCanvas', customControl_getCanvas);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Canvas', customControl_getCanvas, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'OnPaint', customControl_getOnPaint, customControl_setOnPaint);
end;

procedure initializeLuaCustomControl;
begin
  lua_register(LuaVM, 'customControl_getCanvas', customControl_getCanvas);
  lua_register(LuaVM, 'customcontrol_getCanvas', customControl_getCanvas);
end;

initialization
  luaclass_register(TCustomControl, customcontrol_addMetaData);


end.

