unit LuaGraphicControl;

{$mode delphi}

interface

uses
  Classes, SysUtils, Controls, lua, lualib, lauxlib;

procedure graphiccontrol_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
procedure initializeGraphicControl;

implementation

uses luahandler, luaclass, LuaControl;

function graphicControl_getCanvas(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  c: TGraphicControl;
  i: integer;
begin
  c:=luaclass_getClassObject(L);
  if c.Canvas.handle=0 then
    i:=c.Canvas.Pixels[0,0];

  luaclass_newClass(L, c.Canvas);
  result:=1;
end;

procedure graphiccontrol_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  control_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCanvas', graphicControl_getCanvas);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Canvas', graphicControl_getCanvas, nil);
end;

procedure initializeGraphicControl;
begin
  lua_register(LuaVM, 'graphicControl_getCanvas', graphicControl_getCanvas);
  lua_register(LuaVM, 'graphiccontrol_getCanvas', graphicControl_getCanvas);
end;


initialization
  luaclass_register(TGraphicControl, graphiccontrol_addMetaData );

end.

