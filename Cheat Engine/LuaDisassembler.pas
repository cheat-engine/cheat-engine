unit LuaDisassembler;

{$mode delphi}

interface

uses
  Classes, SysUtils, disassembler, lua, lauxlib, lualib, symbolhandler, LastDisassembleData;

procedure initializeLuaDisassembler;

procedure LastDisassemblerDataToTable(L: PLua_State; t: integer; const ldd: TLastDisassembleData);
procedure LastDisassemblerDataFromTable(L: PLua_State; t: integer; var ldd: TLastDisassembleData);

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

function disassembler_decodeLastParametersToString(L: PLua_State): integer; cdecl;
var d: TDisassembler;
begin
  d:=luaclass_getClassObject(L);
  lua_pushstring(L, d.DecodeLastParametersToString);
  result:=1;
end;

procedure LastDisassemblerDataFromTable(L: PLua_State; t: integer; var ldd: TLastDisassembleData);
var bytestable: integer;
  i, size: integer;
begin
  lua_pushstring(L,'address');
  lua_gettable(L, t);
  ldd.address:=lua_tointeger(L, -1);
  lua_pop(L, 1);

  lua_pushstring(L,'opcode');
  lua_gettable(L, t);
  ldd.opcode:=Lua_ToString(L, -1);
  lua_pop(L, 1);

  lua_pushstring(L,'parameters');
  lua_gettable(L, t);
  ldd.parameters:=Lua_ToString(L, -1);
  lua_pop(L, 1);

  lua_pushstring(L,'description');
  lua_gettable(L, t);
  ldd.description:=Lua_ToString(L, -1);
  lua_pop(L, 1);

  lua_pushstring(L,'commentsoverride');
  lua_gettable(L, t);
  ldd.commentsoverride:=Lua_ToString(L, -1);
  lua_pop(L, 1);

  lua_pushstring(L,'bytes');
  lua_gettable(L, t);
  bytestable:=lua_gettop(L);

  size:=lua_objlen(L, bytestable);
  setlength(ldd.Bytes, size);

  for i:=1 to size do
  begin
    lua_pushinteger(L, i);
    lua_gettable(L, bytestable);
    ldd.bytes[i-1]:=lua_tointeger(L, -1);
    lua_pop(L, 1);
  end;

  lua_pop(L, 1);

  lua_pushstring(L,'modrmValueType');
  lua_gettable(L, t);
  ldd.modrmValueType:=TDisAssemblerValueType(lua_tointeger(L, -1));
  lua_pop(L, 1);

  lua_pushstring(L,'modrmValue');
  lua_gettable(L, t);
  ldd.modrmValue:=lua_tointeger(L, -1);
  lua_pop(L, 1);

  lua_pushstring(L,'parameterValueType');
  lua_gettable(L, t);
  ldd.parameterValueType:=TDisAssemblerValueType(lua_tointeger(L, -1));
  lua_pop(L, 1);

  lua_pushstring(L,'parameterValue');
  lua_gettable(L, t);
  ldd.parameterValue:=lua_tointeger(L, -1);
  lua_pop(L, 1);

  lua_pushstring(L,'isJump');
  lua_gettable(L, t);
  ldd.isjump:=lua_toboolean(L, -1);
  lua_pop(L, 1);

  lua_pushstring(L,'isCall');
  lua_gettable(L, t);
  ldd.isCall:=lua_toboolean(L, -1);
  lua_pop(L, 1);

  lua_pushstring(L,'isRet');
  lua_gettable(L, t);
  ldd.isRet:=lua_toboolean(L, -1);
  lua_pop(L, 1);

  lua_pushstring(L,'isConditionalJump');
  lua_gettable(L, t);
  ldd.isconditionaljump:=lua_toboolean(L, -1);
  lua_pop(L, 1);

end;

procedure LastDisassemblerDataToTable(L: PLua_State; t: integer; const ldd: TLastDisassembleData);
var temptable: integer;
  i: integer;
begin

  lua_pushstring(L,'address');
  lua_pushinteger(L, ldd.address);
  lua_settable(L, t);

  lua_pushstring(L,'opcode');
  lua_pushstring(L, ldd.opcode);
  lua_settable(L, t);

  lua_pushstring(L,'parameters');
  lua_pushstring(L, ldd.parameters);
  lua_settable(L, t);

  lua_pushstring(L,'description');
  lua_pushstring(L, ldd.description);
  lua_settable(L, t);

  lua_pushstring(L,'commentsoverride');
  lua_pushstring(L, ldd.commentsoverride);
  lua_settable(L, t);

  lua_pushstring(L, 'bytes');
  lua_newtable(L);
  temptable:=lua_gettop(L);
  for i:=0 to length(ldd.Bytes)-1 do
  begin
    lua_pushinteger(L, i+1);
    lua_pushinteger(L, ldd.Bytes[i]);
    lua_settable(L, temptable);
  end;
  lua_settable(L, t);

  lua_pushstring(L, 'modrmValueType');
  lua_pushinteger(L, integer(ldd.modrmValueType));
  lua_settable(L, t);

  lua_pushstring(L,'modrmValue');
  lua_pushinteger(L, ldd.modrmValue);
  lua_settable(L, t);

  lua_pushstring(L, 'parameterValueType');
  lua_pushinteger(L, integer(ldd.parameterValueType));
  lua_settable(L, t);

  lua_pushstring(L,'parameterValue');
  lua_pushinteger(L, ldd.parameterValue);
  lua_settable(L, t);

  lua_pushstring(L,'isJump');
  lua_pushboolean(L, ldd.isJump);
  lua_settable(L, t);

  lua_pushstring(L,'isCall');
  lua_pushboolean(L, ldd.isCall);
  lua_settable(L, t);

  lua_pushstring(L,'isRet');
  lua_pushboolean(L, ldd.isret);
  lua_settable(L, t);

  lua_pushstring(L,'isConditionalJump');
  lua_pushboolean(L, ldd.isConditionalJump);
  lua_settable(L, t);
end;

function disassembler_getLastDisassembleData(L: PLua_State): integer; cdecl;
var d: TDisassembler;
  t: integer;


  i: integer;
begin
  result:=1;
  d:=luaclass_getClassObject(L);

  lua_newtable(L);
  t:=lua_gettop(L);
  LastDisassemblerDataToTable(L, t, d.LastDisassembleData);
end;

function createDisassembler(L: PLua_State): integer; cdecl;
begin
  luaclass_newClass(L, TDisassembler.Create);
  result:=1;
end;

function createCR3Disassembler(L: PLua_State): integer; cdecl;
begin
  result:=0;
  {$ifdef windows}
  luaclass_newClass(L, TCR3Disassembler.Create);
  result:=1;
  {$endif}
end;


function getDefaultDisassembler(L: PLua_State): integer; cdecl;
begin
  luaclass_newClass(L, defaultDisassembler);
  result:=1;
end;

function getVisibleDisassembler(L: PLua_State): integer; cdecl;
begin
  luaclass_newClass(L, visibleDisassembler);
  result:=1;
end;

procedure disassembler_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'disassemble', disassembler_disassemble);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'decodeLastParametersToString', disassembler_decodeLastParametersToString);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getLastDisassembleData', disassembler_getLastDisassembleData);
  luaclass_addPropertyToTable(L, metatable, userdata, 'LastDisassembleData', disassembler_getLastDisassembleData, nil);

end;


procedure initializeLuaDisassembler;
begin
  lua_register(LuaVM, 'createDisassembler', createDisassembler);
  lua_register(LuaVM, 'createCR3Disassembler', createCR3Disassembler);

  lua_register(LuaVM, 'getDefaultDisassembler', getDefaultDisassembler);
  lua_register(LuaVM, 'getVisibleDisassembler', getVisibleDisassembler);
end;

initialization
  luaclass_register(TDisassembler, disassembler_addMetaData);


end.


