unit LuaMemscan;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, symbolhandler, LuaHandler, cefuncproc, memscan, foundlisthelper;

procedure initializeMemscan;

implementation

//memscan_firstScan(memscan, scanOption, vartype, roundingtype, input1, input2, startAddress,
//                  stopAddress, protectionflags, alignmenttype, "alignmentparam", isHexadecimalInput,
//                  isNotABinaryString, isunicodescan, iscasesensitive, ispercentagescan);
function memscan_firstScan(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  memscan: Tmemscan;
  scanOption: TScanOption;
  vartype: TVariableType;
  roundingtype: TRoundingType;
  input1: string;
  input2: string;
  startaddress: ptruint;
  stopaddress: ptruint;
  protectionflags: string;
  alignmenttype: TFastScanMethod;
  alignmentparam: String;
  isHexadecimalInput, isNotABinaryString, isunicodescan, iscasesensitive, ispercentagescan: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=15 then
  begin
    memscan:=lua_touserdata(L, 1);
    scanOption:=TScanOption(lua_tointeger(L, 2));
    vartype:=TVariableType(lua_tointeger(L, 3));
    roundingtype:=TRoundingType(lua_tointeger(L, 4));
    input1:=Lua_ToString(L, 5);
    input2:=Lua_ToString(L, 6);

    if lua_type(L,7)=LUA_TSTRING then
      startaddress:=symhandler.getAddressFromNameL(Lua_ToString(L, 7))
    else
      startaddress:=lua_tointeger(L, 7);

    if lua_type(L,8)=LUA_TSTRING then
      stopaddress:=symhandler.getAddressFromNameL(Lua_ToString(L, 8))
    else
      stopaddress:=lua_tointeger(L, 8);

    protectionflags:=Lua_ToString(L, 9);
    alignmenttype:=TFastScanMethod(lua_tointeger(L, 10));
    alignmentparam:=lua_tostring(L, 11);

    isHexadecimalInput:=lua_toboolean(L, 12);
    isNotABinaryString:=lua_toboolean(L, 13);
    isunicodescan:=lua_toboolean(L, 14);
    iscasesensitive:=lua_toboolean(L, 15);

    lua_pop(L, lua_gettop(L));

    memscan.parseProtectionflags(protectionflags);

    memscan.firstscan(scanoption, vartype, roundingtype, input1,input2, startaddress,stopaddress, isHexadecimalInput, isNotABinaryString, isunicodescan, iscasesensitive, alignmenttype, alignmentparam, nil );
  end
  else
  begin
    lua_pop(L, lua_gettop(L));
    lua_pushstring(L, 'Not all parameters have been given');
    lua_error(L);
  end;


end;

function memscan_nextScan(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  memscan: Tmemscan;
  scanOption: TScanOption;
  roundingtype: TRoundingType;
  input1: string;
  input2: string;
  isHexadecimalInput, isNotABinaryString, isunicodescan, iscasesensitive, ispercentagescan: boolean;
  savedscanname: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=10 then
  begin
    memscan:=lua_touserdata(L, -parameters);
    scanOption:=TScanOption(lua_tointeger(L, -parameters+1));
    roundingtype:=TRoundingType(lua_tointeger(L, -parameters+2));
    input1:=Lua_ToString(L, -parameters+3);
    input2:=Lua_ToString(L, -parameters+4);

    isHexadecimalInput:=lua_toboolean(L, -parameters+5);
    isNotABinaryString:=lua_toboolean(L, -parameters+6);
    isunicodescan:=lua_toboolean(L, -parameters+7);
    iscasesensitive:=lua_toboolean(L, -parameters+8);
    ispercentagescan:=lua_toboolean(L, -parameters+9);

    if parameters=11 then
      savedscanname:=Lua_ToString(L, -parameters+10)
    else
      savedscanname:='';

    lua_pop(L, lua_gettop(L));

    memscan.nextscan(scanoption, roundingtype, input1,input2, isHexadecimalInput, isNotABinaryString, isunicodescan, iscasesensitive, ispercentagescan, savedscanname<>'', savedscanname );
  end else lua_pop(L, lua_gettop(L));
end;

function memscan_waitTillDone(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  memscan: Tmemscan;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memscan:=lua_touserdata(L, -parameters);
    lua_pop(L, lua_gettop(L));

    memscan.waittillreallydone;
  end else lua_pop(L, lua_gettop(L));
end;

function memscan_getAttachedFoundlist(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  memscan: Tmemscan;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memscan:=lua_touserdata(L, -parameters);
    lua_pop(L, lua_gettop(L));

    result:=1;
    lua_pushlightuserdata(L,memscan.attachedFoundlist);
  end else lua_pop(L, lua_gettop(L));
end;


function memscan_saveCurrentResults(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  memscan: Tmemscan;
  name: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memscan:=lua_touserdata(L, -parameters);
    name:=Lua_ToString(L, -parameters+1);
    lua_pop(L, lua_gettop(L));

    memscan.saveresults(name);
  end else lua_pop(L, lua_gettop(L));
end;

function memscan_returnOnlyOneResult(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memscan: Tmemscan;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memscan:=lua_touserdata(L,1);
    memscan.OnlyOne:=lua_toboolean(L,2);
  end;

  lua_pop(L, parameters);
end;

function memscan_getOnlyResult(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  memscan: Tmemscan;
  address: ptruint;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memscan:=lua_touserdata(L, -parameters);
    lua_pop(L, lua_gettop(L));

    if memscan.GetOnlyOneResult(address) then
    begin
      result:=1;
      lua_pushinteger(L,address);
    end;


  end else lua_pop(L, lua_gettop(L));
end;


procedure InitializeMemscan;
begin
  Lua_register(LuaVM, 'memscan_firstScan', memscan_firstScan);
  Lua_register(LuaVM, 'memscan_nextScan', memscan_nextScan);
  Lua_register(LuaVM, 'memscan_waitTillDone', memscan_waitTillDone);
  Lua_register(LuaVM, 'memscan_saveCurrentResults', memscan_saveCurrentResults);
  Lua_register(LuaVM, 'memscan_getAttachedFoundlist', memscan_getAttachedFoundlist);
  Lua_register(LuaVM, 'memscan_returnOnlyOneResult', memscan_returnOnlyOneResult);
  Lua_register(LuaVM, 'memscan_getOnlyResult', memscan_getOnlyResult);
end;


end.

