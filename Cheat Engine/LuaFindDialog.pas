unit LuaFindDialog;

{$mode delphi}

interface

uses
  Classes, SysUtils, dialogs, luaclass, lua, lauxlib, luahandler, betterControls;

procedure initializeLuaFindDialog;

implementation

uses LuaCommonDialog;

function finddialog_getLeft(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, tfinddialog(luaclass_getClassObject(L)).Left);
  result:=1;
end;

function finddialog_setLeft(L: Plua_State): integer; cdecl;
begin
  tfinddialog(luaclass_getClassObject(L)).Left:=lua_tointeger(L,1);
  result:=0;
end;

function finddialog_getTop(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, tfinddialog(luaclass_getClassObject(L)).Top);
  result:=1;
end;

function finddialog_setTop(L: Plua_State): integer; cdecl;
begin
  tfinddialog(luaclass_getClassObject(L)).Top:=lua_tointeger(L,1);
  result:=0;
end;

function finddialog_getWidth(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, tfinddialog(luaclass_getClassObject(L)).Width);
  result:=1;
end;

function finddialog_setWidth(L: Plua_State): integer; cdecl;
begin
  tfinddialog(luaclass_getClassObject(L)).Width:=lua_tointeger(L,1);
  result:=0;
end;

function finddialog_getHeight(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, tfinddialog(luaclass_getClassObject(L)).Height);
  result:=1;
end;

function finddialog_setHeight(L: Plua_State): integer; cdecl;
begin
  tfinddialog(luaclass_getClassObject(L)).Height:=lua_tointeger(L,1);
  result:=0;
end;

function createFindDialog(L: Plua_State): integer; cdecl;
var fd: TFindDialog;
begin
  luaclass_newClass(L, TFindDialog.Create(lua_ToCEUserData(L, -1)));
  result:=1;
end;

procedure finddialog_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  //only export "execute" the rest is already published
  commondialog_addMetaData(L, metatable, userdata);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Left', finddialog_getLeft, finddialog_setLeft);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Top', finddialog_getTop, finddialog_setTop);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Width', finddialog_getWidth, finddialog_setWidth);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Height', finddialog_getHeight, finddialog_setHeight);
end;

procedure initializeLuaFindDialog;
begin
  lua_register(LuaVM, 'createFindDialog', createFindDialog);
end;

initialization
  luaclass_register(TFindDialog, finddialog_addMetaData);

end.

