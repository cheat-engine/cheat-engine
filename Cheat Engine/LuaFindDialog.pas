unit LuaFindDialog;

{$mode delphi}

interface

uses
  Classes, SysUtils, dialogs, luaclass, lua, lauxlib, luahandler;

procedure initializeLuaFindDialog;

implementation

function createFindDialog(L: Plua_State): integer; cdecl;
var fd: TFindDialog;
begin
  luaclass_newClass(L, TFindDialog.Create(lua_ToCEUserData(L, -1)));
  result:=1;
end;

procedure initializeLuaFindDialog;
begin
  lua_register(LuaVM, 'createFindDialog', createFindDialog);
end;

end.

