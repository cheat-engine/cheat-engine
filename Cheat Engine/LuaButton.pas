unit LuaButton;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, forms, controls, StdCtrls;

procedure InitializeLuaButton;
procedure button_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses LuaHandler, LuaClass, LuaWinControl;

function createButton(L: Plua_State): integer; cdecl;
var c: TWincontrol;
  b: Tbutton;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    c:=lua_toceuserdata(L, 1);
    b:=TButton.create(c);
    b.Parent:=c;
    luaclass_newClass(L, b);
    result:=1;
  end;
end;

function button_getModalResult(L: PLua_State): integer; cdecl;
var
  button: Tcustombutton;
begin
  button:=luaclass_getClassObject(L);
  lua_pushinteger(L, integer(button.ModalResult));
  result:=1;
end;

function button_setModalResult(L: PLua_State): integer; cdecl;
var
  button: Tcustombutton;
begin
  result:=0;
  button:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    button.ModalResult:=TModalResult(lua_tointeger(L,-1));
end;

procedure button_addMetaData(L: PLua_state; metatable: integer; userdata: integer);
begin
  wincontrol_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getModalResult', button_getModalResult);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setModalResult', button_setModalResult);
  luaclass_addPropertyToTable(L, metatable, userdata, 'ModalResult', button_getModalResult, button_setModalResult);

end;

procedure InitializeLuaButton;
begin
  lua_register(LuaVM, 'createButton', createButton);
  lua_register(LuaVM, 'button_getModalResult', button_getModalResult);
  lua_register(LuaVM, 'button_setModalResult', button_setModalResult);
end;

initialization
  luaclass_register(Tbutton, button_addMetaData);


end.


