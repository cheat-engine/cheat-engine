unit LuaGroupbox;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, stdctrls;

  procedure initializeLuaGroupbox;
  procedure groupbox_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses luaclass, luahandler, pluginexports, LuaWinControl;

function createGroupBox(L: Plua_State): integer; cdecl;
var parameters: integer;
  f,p: pointer;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    f:=lua_toceuserdata(L, -1);
    p:=ce_createGroupBox(f);

    lua_pop(L, lua_gettop(L));

    luaclass_newClass(L, p);
    result:=1;
  end;
end;

procedure groupbox_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);
end;

procedure initializeLuaGroupbox;
begin
  lua_register(LuaVM, 'createGroupBox', createGroupBox);
end;

initialization
  luaclass_register(TCustomGroupBox, groupbox_addMetaData);

end.

