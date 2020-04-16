unit LuaApplication;
{
This unit will be used to register TCanvas class methods to lua
}

{$mode delphi}

interface

uses
  Classes, SysUtils, forms, lua, lualib, lauxlib, LuaHandler, fpcanvas, LCLType, LCLIntf;


implementation

uses luaclass, luaobject;

function application_bringToFront(L: PLua_State): integer; cdecl;
begin
  TApplication(luaclass_getClassObject(L)).BringToFront;
  result:=0;
end;

function application_processMessages(L: PLua_State): integer; cdecl;
begin
  TApplication(luaclass_getClassObject(L)).ProcessMessages;
  result:=0;
end;

function application_terminate(L: PLua_State): integer; cdecl;
begin
  TApplication(luaclass_getClassObject(L)).Terminate;
  result:=0;
end;

function application_minimize(L: PLua_State): integer; cdecl;
begin
  TApplication(luaclass_getClassObject(L)).Minimize;
  result:=0;
end;

function application_getExeName(L: PLua_State): integer; cdecl;
var
  app: TApplication;
begin
  result:=0;
  app:=luaclass_getClassObject(L);
  lua_pushstring(L, app.ExeName);
  result:=1;
end;

function application_getMainFormOnTaskBar(L: PLua_State): integer; cdecl;
var
  app: TApplication;
begin
  result:=0;
  app:=luaclass_getClassObject(L);
  lua_pushboolean(L, not app.MainFormOnTaskBar);  //bug in laz 2.0.6, it's inverted
  result:=1;
end;

function application_setMainFormOnTaskBar(L: PLua_State): integer; cdecl;
var
  app: TApplication;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    app:=luaclass_getClassObject(L);
    app.MainFormOnTaskBar:=not lua_toboolean(L,1);
  end;
end;



function application_getTitle(L: PLua_State): integer; cdecl;
var
  app: TApplication;
begin
  result:=0;
  app:=luaclass_getClassObject(L);
  lua_pushstring(L, app.Title);
  result:=1;
end;

function application_setTitle(L: PLua_State): integer; cdecl;
begin
  TApplication(luaclass_getClassObject(L)).title:=Lua_ToString(L, 1);
  result:=0;
end;

function application_getIcon(L: PLua_State): integer; cdecl;
begin
  luaclass_newClass(L, TApplication(luaclass_getClassObject(L)).Icon);
  result:=1;
end;

function application_setIcon(L: PLua_State): integer; cdecl;
begin
  result:=0;
  TApplication(luaclass_getClassObject(L)).icon:=lua_ToCEUserData(L, 1);


end;


procedure application_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'bringToFront', application_bringToFront);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'processMessages', application_processMessages);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'terminate', application_terminate);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'minimize', application_minimize);


  Luaclass_addPropertyToTable(L, metatable, userdata, 'Title', application_getTitle, application_setTitle);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Icon', application_getIcon, application_setIcon);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'ExeName', application_getExeName, nil);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'MainFormOnTaskBar', application_getMainFormOnTaskBar, application_setMainFormOnTaskBar);
end;



initialization
  luaclass_register(Tapplication, application_addMetaData);


end.

