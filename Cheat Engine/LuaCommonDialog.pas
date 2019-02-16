unit LuaCommonDialog;

{$mode delphi}

interface

uses
  Classes, SysUtils, dialogs, lua, lauxlib, lualib;

procedure commondialog_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses LuaClass, LuaHandler, LuaComponent;

function commondialog_execute(L: Plua_State): integer; cdecl;
var
  t: TCommonDialog;
begin
  t:=luaclass_getClassObject(L);
  lua_pushboolean(L, t.Execute);
  result:=1;
end;

procedure commondialog_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  //only export "execute" the rest is already published
  component_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'execute', commondialog_execute);
end;

initialization
  luaclass_register(TCommonDialog, commondialog_addMetaData);

end.

