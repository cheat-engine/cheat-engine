unit LuaCustomType;

{$mode delphi}

interface

uses
  Classes, SysUtils;

procedure initializeLuaCustomType;

implementation

uses CustomTypeHandler, lua, LuaClass, LuaObject, luahandler, LuaByteTable;

function getCustomType(L: Plua_State): integer; cdecl;
var
  s: string;
  ct: TCustomType;
begin
  result:=0;
  if lua_gettop(L)=0 then exit;

  ct:=GetCustomTypeFromName(Lua_ToString(L,1));
  if ct<>nil then
  begin
    luaclass_newClass(L, ct);
    result:=1;
  end;
end;

function customtype_byteTableToValue(L: Plua_State): integer; cdecl;
var
  ct: TCustomType;
  a: ptruint;
  o: pointer;
begin
  result:=0;
  if (lua_gettop(L)=0) or (not lua_istable(L,1)) then exit;

  ct:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
    a:=lua_tointeger(L,2)
  else
    a:=0;

  getmem(o,ct.bytesize);
  try
    readBytesFromTable(L, 1,o,ct.bytesize);

    if ct.scriptUsesFloat then
    begin
      lua_pushnumber(L, ct.ConvertDataToFloat(o,a));
      result:=1;
    end
    else
    begin
      lua_pushinteger(L, ct.ConvertDataToInteger(o,a));
      result:=1;
    end;
  finally
    FreeMemAndNil(o);
  end;
end;

function customtype_valueToByteTable(L: Plua_State): integer; cdecl;
var
  ct: TCustomType;
  f: single;
  i: integer absolute f;
  a: ptruint;

  o: pointer;
begin
  result:=0;
  if lua_gettop(L)=0 then exit;

  ct:=luaclass_getClassObject(L);

  if lua_gettop(L)>=2 then
    a:=lua_tointeger(L,2)
  else
    a:=0;

  getmem(o,ct.bytesize);
  try
    if ct.scriptUsesFloat then
    begin
      f:=lua_tonumber(L, 1);
      ct.ConvertFloatToData(f,o,a);
    end
    else
    begin
      i:=lua_tointeger(L,1);
      ct.ConvertIntegerToData(i,o,a);
    end;


    CreateByteTableFromPointer(L, o,ct.bytesize);
    result:=1;
  finally
    FreeMemAndNil(o);
  end;
end;

procedure customtype_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'byteTableToValue', customtype_byteTableToValue);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'valueToByteTable', customtype_valueToByteTable);
end;

procedure initializeLuaCustomType;
begin
  lua_register(LuaVM, 'getCustomType', getCustomType);
  lua_register(LuaVM, 'registerCustomTypeLua', registerCustomTypeLua);
  lua_register(LuaVM, 'registerCustomTypeAutoAssembler', registerCustomTypeAutoAssembler);


end;

initialization
  luaclass_register(TCustomType, customtype_addMetaData);

end.

