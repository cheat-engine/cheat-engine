unit LuaMemscan;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, symbolhandler, LuaHandler, cefuncproc,
  memscan, foundlisthelper, forms, commonTypeDefs;

procedure initializeMemscan;

implementation

uses luaclass, LuaObject;

resourcestring
  rsNotAllParametersHaveBeenProvided = 'Not all parameters have been provided';

//memscan_firstScan(memscan, scanOption, vartype, roundingtype, input1, input2, startAddress,
//                  stopAddress, protectionflags, alignmenttype, "alignmentparam", isHexadecimalInput,
//                  isNotABinaryString, isunicodescan, iscasesensitive, ispercentagescan);

function memscan_newScan(L: Plua_State): integer; cdecl;
var memscan: TMemscan;
begin
  memscan:=luaclass_getClassObject(L);
  memscan.newscan;
  result:=0;

end;

function memscan_firstScan(L: Plua_State): integer; cdecl;
var
  paramstart, paramcount: integer;
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
  memscan:=luaclass_getClassObject(L, @paramstart, @paramcount);

  if paramcount>=14 then
  begin
    scanOption:=TScanOption(lua_tointeger(L, paramstart));
    vartype:=TVariableType(lua_tointeger(L, paramstart+1));
    roundingtype:=TRoundingType(lua_tointeger(L, paramstart+2));
    input1:=Lua_ToString(L, paramstart+3);
    input2:=Lua_ToString(L, paramstart+4);

    if lua_type(L,paramstart+5)=LUA_TSTRING then
      startaddress:=symhandler.getAddressFromNameL(Lua_ToString(L, paramstart+5))
    else
      startaddress:=lua_tointeger(L, paramstart+5);

    if lua_type(L,paramstart+6)=LUA_TSTRING then
      stopaddress:=symhandler.getAddressFromNameL(Lua_ToString(L, paramstart+6))
    else
      stopaddress:=lua_tointeger(L, paramstart+6);

    protectionflags:=Lua_ToString(L, paramstart+7);
    alignmenttype:=TFastScanMethod(lua_tointeger(L, paramstart+8));
    alignmentparam:=lua_tostring(L, paramstart+9);

    isHexadecimalInput:=lua_toboolean(L, paramstart+10);
    isNotABinaryString:=lua_toboolean(L, paramstart+11);
    isunicodescan:=lua_toboolean(L, paramstart+12);
    iscasesensitive:=lua_toboolean(L, paramstart+13);

    lua_pop(L, lua_gettop(L));

    memscan.parseProtectionflags(protectionflags);

    memscan.firstscan(scanoption, vartype, roundingtype, input1,input2, startaddress,stopaddress, isHexadecimalInput, isNotABinaryString, isunicodescan, iscasesensitive, alignmenttype, alignmentparam, nil );
  end
  else
    memscan.firstscan;
end;


function memscan_nextScan(L: Plua_State): integer; cdecl;
var
  paramcount, paramstart: integer;
  memscan: Tmemscan;
  scanOption: TScanOption;
  roundingtype: TRoundingType;
  input1: string;
  input2: string;
  isHexadecimalInput, isNotABinaryString, isunicodescan, iscasesensitive, ispercentagescan: boolean;
  savedscanname: string;
begin
  result:=0;
  memscan:=luaclass_getClassObject(L, @paramstart, @paramcount);


  if paramcount>=9 then
  begin
    scanOption:=TScanOption(lua_tointeger(L, paramstart+0));
    roundingtype:=TRoundingType(lua_tointeger(L, paramstart+1));
    input1:=Lua_ToString(L, paramstart+2);
    input2:=Lua_ToString(L, paramstart+3);

    isHexadecimalInput:=lua_toboolean(L, paramstart+4);
    isNotABinaryString:=lua_toboolean(L, paramstart+5);
    isunicodescan:=lua_toboolean(L, paramstart+6);
    iscasesensitive:=lua_toboolean(L, paramstart+7);
    ispercentagescan:=lua_toboolean(L, paramstart+8);

    if paramcount>9 then
      savedscanname:=Lua_ToString(L, paramstart+9)
    else
      savedscanname:='';

    lua_pop(L, lua_gettop(L));

    memscan.nextscan(scanoption, roundingtype, input1,input2, isHexadecimalInput, isNotABinaryString, isunicodescan, iscasesensitive, ispercentagescan, savedscanname<>'', savedscanname );
  end
  else
    memscan.nextscan;
end;

function memscan_scan(L: Plua_State): integer; cdecl;
var
  memscan: Tmemscan;
begin
  result:=0;
  lua_pop(L,lua_gettop(L));
  memscan:=luaclass_getClassObject(L);
  if memscan.LastScanType=stNewScan then
    memscan.firstScan
  else
    memscan.nextScan;
end;


function memscan_waitTillDone(L: Plua_State): integer; cdecl;
var
  memscan: Tmemscan;
begin
  result:=0;
  memscan:=luaclass_getClassObject(L);
  if GetCurrentThreadId=MainThreadID then
  begin
    while memscan.waittillreallydone(100)=false do
      CheckSynchronize();
  end
  else
    memscan.waittillreallydone();

end;

function memscan_getAttachedFoundlist(L: Plua_State): integer; cdecl;
var
  memscan: Tmemscan;
begin
  memscan:=luaclass_getClassObject(L);
  luaclass_newclass(L,memscan.attachedFoundlist);
  result:=1;
end;


function memscan_saveCurrentResults(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  memscan: Tmemscan;
  name: string;
begin
  result:=0;
  memscan:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    name:=Lua_ToString(L, -1);
    memscan.saveresults(name);
  end;
end;

function memscan_getreturnOnlyOneResult(L: PLua_State): integer; cdecl;
var
  memscan: Tmemscan;
begin
  memscan:=luaclass_getClassObject(L);
  lua_pushboolean(L,memscan.OnlyOne);
  result:=1;
end;


function memscan_setreturnOnlyOneResult(L: PLua_State): integer; cdecl;
var
  memscan: Tmemscan;
begin
  result:=0;
  memscan:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    memscan.OnlyOne:=lua_toboolean(L,1);
end;

function memscan_getOnlyResult(L: Plua_State): integer; cdecl;
var
  memscan: Tmemscan;
  address: ptruint;
begin
  result:=0;
  memscan:=luaclass_getClassObject(L);

  if memscan.GetOnlyOneResult(address) then
  begin
    result:=1;
    lua_pushinteger(L,address);
  end;
end;

function memscan_getProgress(L: Plua_State): integer; cdecl;
var
  memscan: Tmemscan;
  totaladdressestoscan, currentlyscanned,resultsfound: qword;
begin
  memscan:=luaclass_getClassObject(L);

  memscan.GetProgress(totaladdressestoscan, currentlyscanned,resultsfound);
  lua_newtable(L);
  lua_pushstring(L, 'TotalAddressesToScan');
  lua_pushinteger(L,totaladdressestoscan);
  lua_settable(L,-3);

  lua_pushstring(L, 'CurrentlyScanned');
  lua_pushinteger(L,currentlyscanned);
  lua_settable(L,-3);

  lua_pushstring(L, 'ResultsFound');
  lua_pushinteger(L,resultsfound);
  lua_settable(L,-3);
  result:=1;
end;

procedure memscan_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'scan', memscan_scan);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'firstScan', memscan_firstScan);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'nextScan', memscan_nextScan);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'newScan', memscan_newScan);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'waitTillDone', memscan_waitTillDone);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getProgress', memscan_getProgress);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'saveCurrentResults', memscan_saveCurrentResults);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getAttachedFoundlist', memscan_getAttachedFoundlist);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOnlyOneResult', memscan_setreturnOnlyOneResult);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOnlyResult', memscan_getOnlyResult);

  luaclass_addPropertyToTable(L, metatable, userdata, 'FoundList', memscan_getAttachedFoundlist, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'OnlyOneResult', memscan_getreturnOnlyOneResult, memscan_setreturnOnlyOneResult);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Result', memscan_getOnlyResult, nil);
end;

procedure InitializeMemscan;
begin
  Lua_register(LuaVM, 'memscan_firstScan', memscan_firstScan);
  Lua_register(LuaVM, 'memscan_nextScan', memscan_nextScan);
  Lua_register(LuaVM, 'memscan_waitTillDone', memscan_waitTillDone);
  Lua_register(LuaVM, 'memscan_saveCurrentResults', memscan_saveCurrentResults);
  Lua_register(LuaVM, 'memscan_getAttachedFoundlist', memscan_getAttachedFoundlist);
  Lua_register(LuaVM, 'memscan_returnOnlyOneResult', memscan_setreturnOnlyOneResult);
  Lua_register(LuaVM, 'memscan_getOnlyResult', memscan_getOnlyResult);
end;

initialization
  luaclass_register(TMemScan, memscan_addMetaData);


end.

