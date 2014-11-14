unit LuaRipRelativeScanner;

{$mode delphi}

interface

uses
  Classes, SysUtils, RipRelativeScanner;

procedure initializeLuaRipRelativeScanner;

implementation

uses lua, LuaClass, LuaObject, LuaHandler;

function CreateRipRelativeScanner(L:PLua_State): integer; cdecl;
var
  modulename: string;
begin
  result:=0;

  if lua_gettop(L)>=1 then
  begin
    modulename:=Lua_ToString(L, 1);
    luaclass_newClass(L, TRIPRelativeScanner.create(modulename));

    result:=1;
  end;
end;

function RipRelativeScanner_getAddress(L:PLua_State): integer; cdecl;
var
  rrs: TRIPRelativeScanner;
  index: integer;
begin
  result:=0;
  rrs:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    index:=lua_tointeger(L,-1);
    lua_pushinteger(l, rrs.Address[index]);
    result:=1;
  end;
end;

procedure RipRelativeScanner_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Address', RipRelativeScanner_getAddress, nil);
end;

procedure initializeLuaRipRelativeScanner;
begin
  Lua_register(LuaVM, 'createRipRelativeScanner', CreateRipRelativeScanner);
end;


initialization
  luaclass_register(TRIPRelativeScanner, RipRelativeScanner_addMetaData);


end.

