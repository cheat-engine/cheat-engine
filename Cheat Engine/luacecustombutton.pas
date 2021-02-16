unit LuaCECustomButton;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib;

procedure initializeLuaCECustomButton;

implementation

uses LuaCustomControl, CECustomButton, LuaClass, LuaHandler;



function cecustombutton_startAnimatorTimer(L: PLua_state): integer; cdecl;
begin
  TCECustomButton(luaclass_getClassObject(L)).startAnimatorTimer;
  result:=0;
end;

function cecustombutton_stopAnimatorTimer(L: PLua_state): integer; cdecl;
begin
   TCECustomButton(luaclass_getClassObject(L)).stopAnimatorTimer;
   result:=0;
end;

function lua_createCECustomButton(L: PLua_state): integer;  cdecl;
begin
  if lua_gettop(L)=1 then
  begin
    luaclass_newClass(L, TCECustomButton.create(lua_toceuserdata(L,1)));
    result:=1;
  end
  else
    result:=0;
end;

procedure cecustombutton_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  customcontrol_addMetaData(L,metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'startAnimatorTimer', cecustombutton_startAnimatorTimer);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'stopAnimatorTimer', cecustombutton_stopAnimatorTimer);
end;

procedure initializeLuaCECustomButton;
begin
  lua_register(luavm,'createCECustomButton',lua_createCECustomButton);
end;


initialization
  luaclass_register(TCECustomButton, cecustombutton_addMetaData);



end.

