unit LuaDisassembler;

{$mode delphi}

interface

uses
  Classes, SysUtils, disassembler, lua, lauxlib, lualib, symbolhandler;

procedure initializeLuaDisassembler;

implementation

uses LuaHandler, LuaClass, LuaObject;

function disassembler_disassemble(L: PLua_State): integer; cdecl;
var d: TDisassembler;
  s,desc: string;
  address: ptruint;
begin
  result:=0;
  d:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    if lua_type(L,1)=LUA_TSTRING then
      address:=symhandler.getAddressFromNameL(Lua_ToString(L, 1))
    else
      address:=lua_tointeger(L, 1);

    s:=d.disassemble(address, desc);
    lua_pushstring(L, s);

    result:=1;
  end;

end;

function disassembler_getLastDisassembleData(L: PLua_State): integer; cdecl;
var d: TDisassembler;
  t: integer;

  temptable: integer;
  i: integer;
begin
  result:=1;
  d:=luaclass_getClassObject(L);

  lua_newtable(L);
  t:=lua_gettop(L);

  lua_pushstring(L,'address');
  lua_pushinteger(L, d.LastDisassembleData.address);
  lua_settable(L, t);

  lua_pushstring(L,'opcode');
  lua_pushstring(L, d.LastDisassembleData.opcode);
  lua_settable(L, t);

  lua_pushstring(L,'parameters');
  lua_pushstring(L, d.LastDisassembleData.parameters);
  lua_settable(L, t);

  lua_pushstring(L,'description');
  lua_pushstring(L, d.LastDisassembleData.description);
  lua_settable(L, t);

  lua_pushstring(L, 'bytes');
  lua_newtable(L);
  temptable:=lua_gettop(L);
  for i:=0 to length(d.LastDisassembleData.Bytes)-1 do
  begin
    lua_pushinteger(L, i+1);
    lua_pushinteger(L, d.LastDisassembleData.Bytes[i]);
    lua_settable(L, temptable);
  end;
  lua_settable(L, t);

  lua_pushstring(L, 'modrmValueType');
  lua_pushinteger(L, integer(d.LastDisassembleData.modrmValueType));
  lua_settable(L, t);

  lua_pushstring(L,'modrmValue');
  lua_pushinteger(L, d.LastDisassembleData.modrmValue);
  lua_settable(L, t);

  lua_pushstring(L, 'parameterValueType');
  lua_pushinteger(L, integer(d.LastDisassembleData.parameterValueType));
  lua_settable(L, t);

  lua_pushstring(L,'parameterValue');
  lua_pushinteger(L, d.LastDisassembleData.parameterValue);
  lua_settable(L, t);

  lua_pushstring(L,'isJump');
  lua_pushboolean(L, d.LastDisassembleData.isJump);
  lua_settable(L, t);

  lua_pushstring(L,'isCall');
  lua_pushboolean(L, d.LastDisassembleData.isCall);
  lua_settable(L, t);

  lua_pushstring(L,'isRet');
  lua_pushboolean(L, d.LastDisassembleData.isret);
  lua_settable(L, t);

  lua_pushstring(L,'isConditionalJump');
  lua_pushboolean(L, d.LastDisassembleData.isConditionalJump);
  lua_settable(L, t);



end;

function createDisassembler(L: PLua_State): integer; cdecl;
begin
  luaclass_newClass(L, TDisassembler.Create);
  result:=1;
end;

procedure disassembler_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'disassemble', disassembler_disassemble);
  luaclass_addPropertyToTable(L, metatable, userdata, 'LastDisassembleData', disassembler_getLastDisassembleData, nil);

end;

procedure initializeLuaDisassembler;
begin
  lua_register(LuaVM, 'createDisassembler', createDisassembler);
end;

initialization
  luaclass_register(TDisassembler, disassembler_addMetaData);


end.


