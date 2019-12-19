unit LuaMemoryRecord;

{$mode delphi}

interface

uses
  Classes, SysUtils, MemoryRecordUnit, plugin, pluginexports, lua, lualib,
  lauxlib, LuaHandler, LuaCaller, CEFuncProc, ComCtrls, Graphics, commonTypeDefs;

procedure initializeLuaMemoryRecord;

implementation

uses luaclass, LuaObject;

function memoryrecord_getOffsetCount(L: PLUA_State): integer; cdecl;
var
  memrec: TMemoryRecord;
begin
  memrec:=luaclass_getClassObject(L);
  lua_pushinteger(L, memrec.offsetCount);
  result:=1;
end;

function memoryrecord_setOffsetCount(L: PLUA_State): integer; cdecl;
var
  memrec: TMemoryRecord;
begin
  result:=0;
  memrec:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    memrec.offsetCount:=lua_tointeger(L, 1);
end;

function memoryrecord_getOffset(L: PLua_State): integer; cdecl;
var
  memrec: TMemoryRecord;
  index: integer;
begin
  result:=0;
  memrec:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    index:=lua_toInteger(L,1);
    lua_pushinteger(L, memrec.offsets[index].offset);
    result:=1;
  end;
end;

function memoryrecord_setOffset(L: PLua_State): integer; cdecl;
var
  memrec: TMemoryRecord;
  index: integer;
begin
  result:=0;
  memrec:=luaclass_getClassObject(L);
  if lua_gettop(L)=2 then
  begin
    index:=lua_toInteger(L,1);
    memrec.offsets[index].offset:=lua_tointeger(L, 2);
  end;
end;

function memoryrecord_getOffsetText(L: PLua_State): integer; cdecl;
var
  memrec: TMemoryRecord;
  index: integer;
begin
  result:=0;
  memrec:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    index:=lua_toInteger(L,1);
    lua_pushstring(L, memrec.offsets[index].offsetText);
    result:=1;
  end;
end;

function memoryrecord_setOffsetText(L: PLua_State): integer; cdecl;
var
  memrec: TMemoryRecord;
  index: integer;
begin
  result:=0;
  memrec:=luaclass_getClassObject(L);
  if lua_gettop(L)=2 then
  begin
    index:=lua_toInteger(L,1);
    memrec.offsets[index].offsetText:=Lua_ToString(L, 2);
  end;
end;

function memoryrecord_getDropDownValue(L: PLua_State): integer; cdecl;
var
  memrec: TMemoryRecord;
  index: integer;
begin
  result:=0;
  memrec:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    index:=lua_toInteger(L,1);
    lua_pushstring(L, memrec.DropDownValue[index]);
    result:=1;
  end;
end;


function memoryrecord_getDropDownDescription(L: PLua_State): integer; cdecl;
var
  memrec: TMemoryRecord;
  index: integer;
begin
  result:=0;
  memrec:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    index:=lua_toInteger(L,1);
    lua_pushstring(L, memrec.DropDownDescription[index]);
    result:=1;
  end;
end;


function memoryrecord_getchild(L: PLUA_State): integer; cdecl;
var
  memrec: TMemoryRecord;
  index: integer;
begin
  result:=0;
  memrec:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    index:=lua_toInteger(L,1);
    luaclass_newClass(L, memrec.Child[index]);
    result:=1;
  end;
end;

function memoryrecord_setDescription(L: PLUA_State): integer; cdecl;
var
  memrec: TMemoryRecord;
begin
  result:=0;
  memrec:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
    memrec.Description:=lua_tostring(L,-1); //description
end;

function memoryrecord_getDescription(L: PLua_State): integer; cdecl;
var
  memrec: TMemoryRecord;
begin
  memrec:=luaclass_getClassObject(L);
  lua_pushstring(L, memrec.Description);
  result:=1;
end;

function memoryrecord_getCurrentAddress(L: PLua_state): integer; cdecl;
var
  memrec: tmemoryrecord;
begin
  memrec:=luaclass_getClassObject(L);
  lua_pushinteger(L, memrec.GetRealAddress);
  result:=1;
end;

function memoryrecord_getAddress(L: PLua_state): integer; cdecl;
var
  memrec: tmemoryrecord;
  i: integer;
  tabletop: integer;
begin
  memrec:=luaclass_getClassObject(L);
  lua_pushstring(L, memrec.interpretableaddress);
  result:=1;
  if memrec.isPointer then
  begin
    lua_newtable(L);
    tabletop:=lua_gettop(L);

    for i:=0 to memrec.offsetCount-1 do
    begin
      lua_pushinteger(L,i+1);
      lua_pushinteger(L, memrec.offsets[i].offset);
      lua_settable(L, tabletop);
    end;
    result:=2;
  end;
end;

function memoryrecord_setAddress(L: PLua_state): integer; cdecl;
var
  memrec: tmemoryrecord;
  i: integer;
  tabletop: integer;
begin
  result:=0;
  memrec:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    //address
    if lua_type(L,1)=LUA_TNUMBER then
      memrec.interpretableaddress:=inttohex(lua_tointeger(L,1),8)
    else
      memrec.interpretableaddress:=Lua_ToString(L, 1);

    memrec.ReinterpretAddress(true);
    memrec.offsetCount:=0;

    if lua_gettop(L)>=2 then
    begin
      //table
      if lua_istable(L,2) then
      begin
        i:=lua_objlen(L,2);
        if i>512 then exit; //FY

        memrec.offsetCount:=i;
        for i:=0 to memrec.offsetCount-1 do
        begin
          lua_pushinteger(L, i+1); //get the offset
          lua_gettable(L, 2); //from the table    (table[i+1])
          memrec.offsets[i].offset:=lua_tointeger(L,-1);
          lua_pop(L,1);
        end;
      end;
    end;
  end;
end;

function memoryrecord_getAddressOld(L: PLua_state): integer; cdecl;
var
  parameters: integer;
  memrec: pointer;
  address: ptruint;
  offsets: array of dword;
  offsetcount: integer;
  i: integer;
  tabletop: integer;
begin
  result:=0;

  offsetcount:=0;
  setlength(offsets,0);

  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memrec:=lua_toceuserdata(L,-1);
    lua_pop(L, parameters);

    if ce_memrec_getAddress(memrec, @address, nil, 0, @offsetcount) then
    begin
      lua_pushinteger(L,address);
      result:=1;

      if offsetcount>0 then
      begin
        //pointer, return a secondary result (table) which contains the baseaddress and offsets
        setlength(offsets,offsetcount);
        ce_memrec_getAddress(memrec, @address, @offsets[0], length(offsets), @offsetcount);

        lua_newtable(L);
        tabletop:=lua_gettop(L);

        lua_pushinteger(L,1); //index
        lua_pushinteger(L, TMemoryRecord(memrec).getBaseAddress); //value
        lua_settable(L, tabletop);

        for i:=0 to offsetcount-1 do
        begin
          lua_pushinteger(L, i+2);
          lua_pushinteger(L, offsets[i]);
          lua_settable(L, tabletop);
        end;

        inc(result,1); //add the table as a result
      end;

    end;

  end else lua_pop(L, parameters);
end;

function memoryrecord_setAddressOld(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;
  address: pchar;

  s: string;
  offsets: array of dword;
  i,j: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=2 then
  begin
    memrec:=lua_toceuserdata(L, (-parameters));

    if lua_isstring(L, (-parameters)+1) then
      address:=lua.lua_tostring(L, (-parameters)+1)
    else //convert it to a hexadecimal value first
    begin
      s:=inttohex(lua_tointeger(L, (-parameters)+1),8);
      address:=pchar(s);
    end;

    setlength(offsets,parameters-2);
    j:=0;
    for i:=(-parameters)+2 to -1 do
    begin
      offsets[j]:=lua_tointeger(L, i);
      inc(j);
    end;

    lua_pop(L, parameters);

    ce_memrec_setAddress(memrec, address, @offsets[0], length(offsets))
  end else
    lua_pop(L, parameters);
end;

function memoryrecord_getType(L: PLua_State): integer; cdecl;
var
  memrec: TMemoryRecord;
begin
  memrec:=luaclass_getClassObject(L);
  lua_pushinteger(L, integer(memrec.VarType));
  result:=1;
end;

function memoryrecord_setType(L: PLua_State): integer; cdecl;
var
  memrec: TMemoryRecord;
begin
  result:=0;
  memrec:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    memrec.VarType:=TVariableType(lua_tointeger(L, -1)) ;
end;

function memoryrecord_getValue(L: PLua_State): integer; cdecl;
var
  memrec: TMemoryRecord;
begin
  memrec:=luaclass_getClassObject(L);
  lua_pushstring(L, memrec.Value);
  result:=1;
end;

function memoryrecord_setValue(L: PLUA_State): integer; cdecl;
var
  memrec: TMemoryRecord;
begin
  result:=0;
  memrec:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
    memrec.Value:=lua_tostring(L,-1);
end;

function memoryrecord_getScript(L: PLua_State): integer; cdecl;
var
  memrec: TMemoryRecord;
begin
  memrec:=luaclass_getClassObject(L);
  if memrec.AutoAssemblerData.script<>nil then
  begin
    lua_pushstring(L, memrec.AutoAssemblerData.script.Text);
    result:=1;
  end
  else
    result:=0;
end;


function memoryrecord_setScript(L: PLUA_State): integer; cdecl;
var
  memrec: TMemoryRecord;
begin
  result:=0;
  memrec:=luaclass_getClassObject(L);

  if (lua_gettop(L)>=1) and (memrec.AutoAssemblerData.script<>nil) then
    memrec.AutoAssemblerData.script.Text:=lua_tostring(L,-1);
end;

function memoryrecord_isSelected(L: PLua_State): integer; cdecl;
var
  memrec: TMemoryRecord;
begin
  memrec:=luaclass_getClassObject(L);
  lua_pushboolean(L, memrec.isSelected);
  result:=1;
end;

function memoryrecord_disableWithoutExecute(L: PLua_State): integer; cdecl;
begin
  result:=0;
  TMemoryRecord(luaclass_getClassObject(L)).disablewithoutexecute;
end;

function memoryrecord_setActive(L: PLua_State): integer; cdecl;
var
  memrec: TMemoryRecord;
begin
  result:=0;
  memrec:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
    memrec.active:=lua_toboolean(L, 1);
end;

function memoryrecord_getActive(L: PLua_State): integer; cdecl;
var
  memrec: TMemoryRecord;
begin
  memrec:=luaclass_getClassObject(L);
  lua_pushboolean(L, memrec.Active);
  result:=1;
end;

function memoryrecord_freeze(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;
  direction: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    memrec:=lua_toceuserdata(L, -parameters);


    if parameters=2 then
      direction:=lua_tointeger(L, -1)
    else
      direction:=0;

    ce_memrec_freeze(memrec, direction);
  end;

  lua_pop(L, parameters);
end;

function memoryrecord_unfreeze(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
  direction: integer;
begin
  result:=0;
  memoryrecord:=luaclass_getClassObject(L);
  memoryrecord.Active:=false;
end;

function memoryrecord_setColor(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
  color: integer;
begin
  result:=0;
  memoryrecord:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    color:=lua_tointeger(L,-1);
    memoryrecord.Color:=tcolor(color);
  end;
end;

function memoryrecord_appendToEntry(L: PLua_State): integer; cdecl;
var
  memrec1,memrec2: TMemoryRecord;
  parameters: integer;
begin
  result:=0;

  memrec1:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    memrec2:=lua_toceuserdata(L,-1);
    memrec1.treenode.MoveTo(memrec2.treenode, naAddChild);
    memrec2.SetVisibleChildrenState;
  end;
end;

function memoryrecord_delete(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
begin
  result:=0;
  memoryrecord:=luaclass_getClassObject(L);
  memoryrecord.free;
end;

function memoryrecord_reinterpretAddress(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
begin
  result:=0;
  memoryrecord:=luaclass_getClassObject(L);
  memoryrecord.ReinterpretAddress(true);
end;

function memoryrecord_getID(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
begin
  memoryrecord:=luaclass_getClassObject(L);
  lua_pushinteger(L, memoryrecord.id);
  result:=1;
end;

function memoryrecord_createHotkey(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
  hk: TMemoryRecordHotkey;
  keys: TKeyCombo;
  action: TMemrecHotkeyAction;
  value, description: string;
  i: integer;
begin
  result:=0;
  memoryrecord:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
  begin
    if (not lua_istable(L, 1)) or (not lua_isnumber(L, 2)) then exit(0);



    for i:=0 to 4 do
    begin

      lua_pushinteger(L, i+1);
      lua_gettable(L, 1);
      if lua_isnil(L, -1) then  //end of the list
      begin
        keys[i]:=0;
        lua_pop(L,1);
        break;
      end
      else
      begin
        keys[i]:=lua_tointeger(L,-1);
        lua_pop(L,1);
      end;
    end;


    action:=TMemrecHotkeyAction(lua_tointeger(L, 2));

    if lua_gettop(L)>=3 then
      value:=Lua_ToString(L, 3)
    else
      value:='';


    if lua_gettop(L)>=4 then
      description:=Lua_ToString(L, 4)
    else
      description:='';

    hk:=memoryrecord.Addhotkey(keys, action, value, description);
    result:=1;
    luaclass_newClass(L, hk);
  end;

end;

function memoryrecord_getHotkeyCount(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
begin
  memoryrecord:=luaclass_getClassObject(L);
  lua_pushinteger(L, memoryrecord.HotkeyCount);
  result:=1;
end;

function memoryrecord_getHotkey(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
  index: integer;
begin
  result:=0;
  memoryrecord:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    index:=lua_tointeger(L,-1);
    luaclass_newClass(L, memoryrecord.Hotkey[index]);
    result:=1;
  end;
end;

function memoryrecord_getHotkeyByID(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecord: Tmemoryrecord;
  id: integer;
  i: integer;
begin
  result:=0;
  memoryrecord:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    id:=lua_tointeger(L,-1);

    for i:=0 to memoryrecord.Hotkeycount-1 do
      if memoryrecord.Hotkey[i].id=id then
      begin
        luaclass_newClass(L, memoryrecord.Hotkey[i]);
        result:=1;
        exit;
      end;
  end;
end;


function memoryrecord_string_getSize(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
begin
  memoryrecord:=luaclass_getClassObject(L);
  lua_pushinteger(L, memoryrecord.Extra.stringData.length);
  result:=1;
end;

function memoryrecord_string_setSize(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
begin
  result:=0;
  memoryrecord:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    memoryrecord.Extra.stringData.length:=lua_tointeger(L, -1);

end;

function memoryrecord_string_getUnicode(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
begin
  memoryrecord:=luaclass_getClassObject(L);
  lua_pushboolean(L, memoryrecord.Extra.stringData.unicode);
  result:=1;
end;

function memoryrecord_string_setUnicode(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
begin
  result:=0;
  memoryrecord:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    memoryrecord.Extra.stringData.unicode:=lua_toboolean(L, -1);

  if memoryrecord.Extra.stringData.Unicode then
    memoryrecord.Extra.stringData.codepage:=false;
end;

function memoryrecord_string_getCodePage(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
begin
  memoryrecord:=luaclass_getClassObject(L);
  lua_pushboolean(L, memoryrecord.Extra.stringData.CodePage);
  result:=1;
end;

function memoryrecord_string_setCodePage(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
begin
  result:=0;
  memoryrecord:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    memoryrecord.Extra.stringData.CodePage:=lua_toboolean(L, -1);

  if memoryrecord.Extra.stringData.CodePage then
    memoryrecord.Extra.stringData.unicode:=false;
end;

function memoryrecord_binary_getStartbit(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
begin
  memoryrecord:=luaclass_getClassObject(L);
  lua_pushinteger(L, memoryrecord.Extra.bitData.bit);
  result:=1;
end;

function memoryrecord_binary_setStartbit(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
begin
  result:=0;
  memoryrecord:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    memoryrecord.Extra.bitData.bit:=lua_tointeger(L, -1);
end;


function memoryrecord_binary_getSize(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
begin
  memoryrecord:=luaclass_getClassObject(L);
  lua_pushinteger(L, memoryrecord.Extra.bitData.bitlength);
  result:=1;
end;

function memoryrecord_binary_setSize(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
begin
  result:=0;
  memoryrecord:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    memoryrecord.Extra.bitData.bitlength:=lua_tointeger(L, -1);
end;

function memoryrecord_aob_getSize(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
begin
  memoryrecord:=luaclass_getClassObject(L);
  lua_pushinteger(L, memoryrecord.Extra.byteData.bytelength);
  result:=1;
end;

function memoryrecord_aob_setSize(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
begin
  result:=0;
  memoryrecord:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    memoryrecord.Extra.byteData.bytelength:=lua_tointeger(L, -1);
end;


function memoryrecord_onActivate(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  memoryrecord:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin

    CleanupLuaCall(tmethod(memoryrecord.onActivate));
    memoryrecord.onActivate:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      memoryrecord.onActivate:=lc.MemoryRecordActivateEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      memoryrecord.onActivate:=lc.MemoryRecordActivateEvent;
    end;

  end;
end;

function memoryrecord_onDeactivate(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  memoryrecord:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    CleanupLuaCall(tmethod(memoryrecord.onDeactivate));
    memoryrecord.onDeactivate:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      memoryrecord.onDeactivate:=lc.MemoryRecordActivateEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      memoryrecord.onDeactivate:=lc.MemoryRecordActivateEvent;
    end;


  end;
end;

function memoryrecord_onDestroy(L: PLua_State): integer; cdecl;
var
  memoryrecord: Tmemoryrecord;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  memoryrecord:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    CleanupLuaCall(tmethod(memoryrecord.onDestroy));
    memoryrecord.onDestroy:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      memoryrecord.onDestroy:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      memoryrecord.onDestroy:=lc.NotifyEvent;
    end;
  end;
end;

procedure memoryrecord_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
var recordEntry: TRecordEntry;
  recordentries: TRecordEntries;
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setDescription', memoryrecord_setDescription);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getDescription', memoryrecord_getDescription);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getAddress', memoryrecord_getAddress);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setAddress', memoryrecord_setAddress);


  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOffsetCount', memoryrecord_getOffsetCount);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOffsetCount', memoryrecord_setOffsetCount);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOffset', memoryrecord_getOffset);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOffset', memoryrecord_setOffset);


  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCurrentAddress', memoryrecord_getCurrentAddress);


  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getType', memoryrecord_getType);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setType', memoryrecord_setType);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getValue', memoryrecord_getValue);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setValue', memoryrecord_setValue);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getScript', memoryrecord_getScript);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setScript', memoryrecord_setScript);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getActive', memoryrecord_getActive);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setActive', memoryrecord_setActive);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'disableWithoutExecute', memoryrecord_disableWithoutExecute);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getChild', memoryrecord_getChild);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'isSelected', memoryrecord_isSelected);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'appendToEntry', memoryrecord_appendToEntry);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'delete', memoryrecord_delete);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'reinterpret', memoryrecord_reinterpretAddress);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getHotkeyCount', memoryrecord_getHotkeyCount);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getHotkey', memoryrecord_getHotkey);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getHotkeyByID', memoryrecord_getHotkeyByID);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'createHotkey', memoryrecord_createHotkey);



  luaclass_addPropertyToTable(L, metatable, userdata, 'Description', memoryrecord_getDescription, memoryrecord_setDescription);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Address', memoryrecord_getAddress, memoryrecord_setAddress);
  luaclass_addPropertyToTable(L, metatable, userdata, 'CurrentAddress', memoryrecord_getCurrentAddress, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Type', memoryrecord_getType, memoryrecord_setType);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Value', memoryrecord_getValue, memoryrecord_setValue);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Script', memoryrecord_getScript, memoryrecord_setScript);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Active', memoryrecord_getActive, memoryrecord_setActive);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Selected', memoryrecord_isSelected, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'HotkeyCount', memoryrecord_getHotkeyCount, nil);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Hotkey', memoryrecord_getHotkey);



  luaclass_addPropertyToTable(L, metatable, userdata, 'OffsetCount', memoryrecord_getOffsetCount, memoryrecord_setOffsetCount);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Offset', memoryrecord_getOffset, memoryrecord_setOffset);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'OffsetText', memoryrecord_getOffsetText, memoryrecord_setOffsetText);

  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'DropDownValue', memoryrecord_getDropDownValue, nil);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'DropDownDescription', memoryrecord_getDropDownDescription, nil);



  luaclass_addPropertyToTable(L, metatable, userdata, 'Active', memoryrecord_getActive, memoryrecord_setActive);


  recordEntries:=Trecordentries.create;

  recordEntry.name:='Size';
  recordEntry.getf:=memoryrecord_string_getSize;
  recordEntry.setf:=memoryrecord_string_setSize;
  recordEntries.add(recordEntry);

  recordEntry.name:='Unicode';
  recordEntry.getf:=memoryrecord_string_getUnicode;
  recordEntry.setf:=memoryrecord_string_setUnicode;
  recordEntries.add(recordEntry);

  recordEntry.name:='Codepage';
  recordEntry.getf:=memoryrecord_string_getCodepage;
  recordEntry.setf:=memoryrecord_string_setCodepage;
  recordEntries.add(recordEntry);

  luaclass_addRecordPropertyToTable(L, metatable, userdata, 'String', recordEntries);

  recordEntries.clear;
  recordEntry.name:='Startbit';
  recordEntry.getf:=memoryrecord_binary_getStartbit;
  recordEntry.setf:=memoryrecord_binary_setStartbit;
  recordEntries.add(recordEntry);

  recordEntry.name:='Size';
  recordEntry.getf:=memoryrecord_binary_getSize;
  recordEntry.setf:=memoryrecord_binary_setSize;
  recordEntries.add(recordEntry);
  luaclass_addRecordPropertyToTable(L, metatable, userdata, 'Binary', recordEntries);

  recordEntries.clear;
  recordEntry.name:='Size';
  recordEntry.getf:=memoryrecord_aob_getSize;
  recordEntry.setf:=memoryrecord_aob_setSize;
  recordEntries.add(recordEntry);
  luaclass_addRecordPropertyToTable(L, metatable, userdata, 'Aob', recordEntries);
  recordEntries.free;


  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Child', memoryrecord_getchild, nil);
  luaclass_setDefaultArrayProperty(L, metatable, userdata, memoryrecord_getchild, nil);


end;

procedure initializeLuaMemoryRecord;
begin
  lua_register(LuaVM, 'memoryrecord_setDescription', memoryrecord_setDescription);
  lua_register(LuaVM, 'memoryrecord_getDescription', memoryrecord_getDescription);
  lua_register(LuaVM, 'memoryrecord_getAddress', memoryrecord_getAddressOld);
  lua_register(LuaVM, 'memoryrecord_setAddress', memoryrecord_setAddressOld);
  lua_register(LuaVM, 'memoryrecord_getType', memoryrecord_getType);
  lua_register(LuaVM, 'memoryrecord_setType', memoryrecord_setType);
  lua_register(LuaVM, 'memoryrecord_getValue', memoryrecord_getValue);
  lua_register(LuaVM, 'memoryrecord_setValue', memoryrecord_setValue);
  lua_register(LuaVM, 'memoryrecord_getScript', memoryrecord_getScript);
  lua_register(LuaVM, 'memoryrecord_setScript', memoryrecord_setScript);
  lua_register(LuaVM, 'memoryrecord_isActive', memoryrecord_getActive);
  lua_register(LuaVM, 'memoryrecord_isSelected', memoryrecord_isSelected);
  lua_register(LuaVM, 'memoryrecord_freeze', memoryrecord_freeze);
  lua_register(LuaVM, 'memoryrecord_unfreeze', memoryrecord_unfreeze);
  lua_register(LuaVM, 'memoryrecord_setColor', memoryrecord_setColor);
  lua_register(LuaVM, 'memoryrecord_appendToEntry', memoryrecord_appendToEntry);
  lua_register(LuaVM, 'memoryrecord_delete', memoryrecord_delete);

  lua_register(LuaVM, 'memoryrecord_string_getSize', memoryrecord_string_getSize);
  lua_register(LuaVM, 'memoryrecord_string_setSize', memoryrecord_string_setSize);
  lua_register(LuaVM, 'memoryrecord_string_getUnicode', memoryrecord_string_getUnicode);
  lua_register(LuaVM, 'memoryrecord_string_setUnicode', memoryrecord_string_setUnicode);
  lua_register(LuaVM, 'memoryrecord_binary_getStartbit', memoryrecord_binary_getStartbit);
  lua_register(LuaVM, 'memoryrecord_binary_setStartbit', memoryrecord_binary_setStartbit);
  lua_register(LuaVM, 'memoryrecord_binary_getSize', memoryrecord_binary_getSize);
  lua_register(LuaVM, 'memoryrecord_binary_setSize', memoryrecord_binary_setSize);
  lua_register(LuaVM, 'memoryrecord_aob_getSize', memoryrecord_aob_getSize);
  lua_register(LuaVM, 'memoryrecord_aob_setSize', memoryrecord_aob_setSize);

  lua_register(LuaVM, 'memoryrecord_getID', memoryrecord_getID);
  lua_register(LuaVM, 'memoryrecord_getHotkeyCount', memoryrecord_getHotkeyCount);
  lua_register(LuaVM, 'memoryrecord_getHotkey', memoryrecord_getHotkey);
  lua_register(LuaVM, 'memoryrecord_getHotkeyByID', memoryrecord_getHotkeyByID);
  lua_register(LuaVM, 'memoryrecord_onActivate', memoryrecord_onActivate);
  lua_register(LuaVM, 'memoryrecord_onDeactivate', memoryrecord_onDeactivate);
  lua_register(LuaVM, 'memoryrecord_onDestroy', memoryrecord_onDestroy);

end;

initialization
  luaclass_register(TMemoryRecord, memoryrecord_addMetaData);

end.

