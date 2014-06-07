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


procedure application_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'bringToFront', application_bringToFront);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'processMessages', application_processMessages);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'terminate', application_terminate);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'minimize', application_minimize);


  Luaclass_addPropertyToTable(L, metatable, userdata, 'Title', application_getTitle, application_setTitle);

end;



initialization
  luaclass_register(Tapplication, application_addMetaData);


end.

