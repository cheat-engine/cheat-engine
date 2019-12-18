unit LuaRIPRelativeScanner;

{$mode delphi}

interface

uses
  Classes, SysUtils, RipRelativeScanner;

procedure initializeLuaRipRelativeScanner;

implementation

uses lua, LuaClass, LuaObject, LuaHandler, symbolhandler;

function CreateRipRelativeScanner(L:PLua_State): integer; cdecl;
var
  modulename: string;

  startaddress, stopaddress: ptruint;
  includelongjumps: boolean;
begin
  result:=0;
  includelongjumps:=false;
  startaddress:=0;
  stopaddress:=0;

  if lua_gettop(L)=1 then
  begin
    modulename:=Lua_ToString(L, 1);
    luaclass_newClass(L, TRIPRelativeScanner.create(modulename, false));
    result:=1;
    exit;
  end;

  if lua_gettop(L)=2 then //could be (modulename, includebooleans), or (startaddress, stopaddress)
  begin
    if lua_isboolean(L,2) then //modulename, includeboolean
    begin
      modulename:=Lua_ToString(L, 1);
      includelongjumps:=lua_toboolean(L, 2);
      luaclass_newClass(L, TRIPRelativeScanner.create(modulename, includelongjumps));
      result:=1;
      exit;
    end
    else //startaddress, stopaddress
    begin
      if lua_type(L, 1)=LUA_TSTRING then
        startaddress:=symhandler.getAddressFromName(Lua_ToString(L, 1))
      else
        startaddress:=lua_tointeger(L, 1);


      if lua_type(L, 2)=LUA_TSTRING then
        stopaddress:=symhandler.getAddressFromName(Lua_ToString(L, 2))
      else
        stopaddress:=lua_tointeger(L, 2);

      luaclass_newClass(L, TRIPRelativeScanner.create(startaddress, stopaddress, false));
      result:=1;
      exit;
    end;
  end;

  if lua_gettop(L)=3 then    //startaddress, stopaddress, includelongjumps
  begin
    if lua_type(L, 1)=LUA_TSTRING then
      startaddress:=symhandler.getaddressFromName(Lua_ToString(L, 1))
    else
      startaddress:=lua_tointeger(L, 1);

    if lua_type(L, 2)=LUA_TSTRING then
      stopaddress:=symhandler.getaddressFromName(Lua_ToString(L, 2))
    else
      stopaddress:=lua_tointeger(L, 2);

    includelongjumps:=lua_toboolean(L, 3);
    luaclass_newClass(L, TRIPRelativeScanner.create(startaddress, stopaddress, includelongjumps));
    result:=1;
    exit;
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

