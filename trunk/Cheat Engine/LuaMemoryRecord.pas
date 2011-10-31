unit LuaMemoryRecord;

{$mode delphi}

interface

uses
  Classes, SysUtils, MemoryRecordUnit, plugin, pluginexports, lua, lualib,
  lauxlib, LuaHandler, LuaCaller;

procedure initializeLuaMemoryRecord;

implementation

function memoryrecord_setDescription(L: PLUA_State): integer; cdecl;
var
  parameters: integer;
  description: pchar;
  memrec: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memrec:=lua_touserdata(L,-2); //memrec
    description:=lua.lua_tostring(L,-1); //description

    lua_pop(L, parameters);  //clear stack

    ce_memrec_setDescription(memrec, description);
  end;

  lua_pop(L, parameters);  //clear stack

end;

function memoryrecord_getDescription(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  d: pchar;
  memrec: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memrec:=lua_touserdata(L,-1);
    d:=ce_memrec_getDescription(memrec);
    if d<>nil then
    begin
      lua_pushstring(L, d);
      result:=1;
    end;
  end else lua_pop(L, parameters);
end;

function memoryrecord_getAddress(L: PLua_state): integer; cdecl;
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
    memrec:=lua_touserdata(L,-1);
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

function memoryrecord_setAddress(L: PLua_State): integer; cdecl;
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
    memrec:=lua_touserdata(L, (-parameters));

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
  memrec: pointer;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memrec:=lua_touserdata(L, (-parameters));
    lua_pop(L, parameters);

    lua_pushinteger(L, ce_memrec_getType(memrec));
    result:=1;

  end;
end;

function memoryrecord_setType(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  vtype: integer;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memrec:=lua_touserdata(L, -2);
    vtype:=lua_tointeger(L, -1);
    lua_pop(L, parameters);

    ce_memrec_setType(memrec, vtype);
  end
  else
    lua_pop(L, parameters);

end;

function memoryrecord_getValue(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;

  v: pchar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memrec:=lua_touserdata(L, (-parameters));
    lua_pop(L, parameters);


    getmem(v,255);
    try
      if ce_memrec_getValue(memrec, v, 255) then
      begin
        lua_pushstring(L, v);
        result:=1;
      end;

    finally
      freemem(v);
    end;
  end else lua_pop(L, parameters);
end;


function memoryrecord_setValue(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;

  v: pchar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memrec:=lua_touserdata(L, -2);
    v:=lua.lua_tostring(L, -1);


    ce_memrec_setValue(memrec, v);
  end;
  lua_pop(L, parameters);
end;

function memoryrecord_getScript(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;

  v: pchar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memrec:=lua_touserdata(L, -1);
    v:=ce_memrec_getScript(memrec);
    lua_pop(L, parameters);

    if v<>nil then
    begin
      lua_pushstring(L, v);
      result:=1;
    end;

  end else lua_pop(L, parameters);


end;


function memoryrecord_setScript(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;

  v: pchar;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memrec:=lua_touserdata(L, -2);
    v:=lua.lua_tostring(L, -1);

    ce_memrec_setScript(memrec, v);
  end;


  lua_pop(L, parameters);
end;

function memoryrecord_isSelected(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  direction: integer;
  memrec: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memrec:=lua_touserdata(L, -parameters);
    lua_pop(L, parameters);



    lua_pushboolean(L, ce_memrec_isSelected(memrec));
    result:=1;
  end
  else lua_pop(L, parameters);
end;


function memoryrecord_isActive(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  direction: integer;
  memrec: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memrec:=lua_touserdata(L, -parameters);
    lua_pop(L, parameters);

    lua_pushboolean(L, ce_memrec_isFrozen(memrec));
    result:=1;
  end
  else lua_pop(L, parameters);
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
    memrec:=lua_touserdata(L, -parameters);


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
  memrec: pointer;
  parameters: integer;
  direction: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memrec:=lua_touserdata(L, -parameters);
    ce_memrec_unfreeze(memrec);
  end;

  lua_pop(L, parameters);
end;

function memoryrecord_setColor(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;
  color: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memrec:=lua_touserdata(L,-2);
    color:=lua_tointeger(L,-1);
    ce_memrec_setColor(memrec,color);
  end;

  lua_pop(L, parameters);
end;

function memoryrecord_appendToEntry(L: PLua_State): integer; cdecl;
var
  memrec1,memrec2: pointer;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memrec1:=lua_touserdata(L,-2);
    memrec2:=lua_touserdata(L,-1);
    ce_memrec_appendtoentry(memrec1,memrec2);
  end;

  lua_pop(L, parameters);
end;

function memoryrecord_delete(L: PLua_State): integer; cdecl;
var
  memrec: pointer;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memrec:=lua_touserdata(L,-1);
    ce_memrec_delete(memrec);
  end;

  lua_pop(L, parameters);
end;

function memoryrecord_getID(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecord: Tmemoryrecord;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memoryrecord:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, memoryrecord.id);
    result:=1;

  end else lua_pop(L, parameters);
end;

function memoryrecord_getHotkeyCount(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecord: Tmemoryrecord;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    memoryrecord:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, memoryrecord.HotkeyCount);
    result:=1;

  end else lua_pop(L, parameters);
end;

function memoryrecord_getHotkey(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecord: Tmemoryrecord;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memoryrecord:=lua_touserdata(L,-2);
    index:=lua_tointeger(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, memoryrecord.Hotkey[index]);
    result:=1;

  end else lua_pop(L, parameters);
end;

function memoryrecord_getHotkeyByID(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecord: Tmemoryrecord;
  id: integer;
  i: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memoryrecord:=lua_touserdata(L,-2);
    id:=lua_tointeger(L,-1);
    lua_pop(L, parameters);

    for i:=0 to memoryrecord.Hotkeycount-1 do
      if memoryrecord.Hotkey[i].id=id then
      begin
        lua_pushlightuserdata(L, memoryrecord.Hotkey[i]);
        result:=1;
      end;

  end else lua_pop(L, parameters);
end;


function memoryrecord_onActivate(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecord: Tmemoryrecord;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memoryrecord:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(memoryrecord.onActivate));
    memoryrecord.onActivate:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      memoryrecord.onActivate:=lc.ActivateEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      memoryrecord.onActivate:=lc.ActivateEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

function memoryrecord_onDeactivate(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecord: Tmemoryrecord;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memoryrecord:=lua_touserdata(L,-2);

    CleanupLuaCall(tmethod(memoryrecord.onDeactivate));
    memoryrecord.onDeactivate:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      memoryrecord.onDeactivate:=lc.ActivateEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      memoryrecord.onDeactivate:=lc.ActivateEvent;
    end;


  end;

  lua_pop(L, parameters);
end;

function memoryrecord_onDestroy(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  memoryrecord: Tmemoryrecord;
  f: integer;
  routine: string;

  lc: TLuaCaller;

//  clickroutine: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    memoryrecord:=lua_touserdata(L,-2);

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

  lua_pop(L, parameters);
end;

procedure initializeLuaMemoryRecord;
begin
  lua_register(LuaVM, 'memoryrecord_setDescription', memoryrecord_setDescription);
  lua_register(LuaVM, 'memoryrecord_getDescription', memoryrecord_getDescription);
  lua_register(LuaVM, 'memoryrecord_getAddress', memoryrecord_getAddress);
  lua_register(LuaVM, 'memoryrecord_setAddress', memoryrecord_setAddress);
  lua_register(LuaVM, 'memoryrecord_getType', memoryrecord_getType);
  lua_register(LuaVM, 'memoryrecord_setType', memoryrecord_setType);
  lua_register(LuaVM, 'memoryrecord_getValue', memoryrecord_getValue);
  lua_register(LuaVM, 'memoryrecord_setValue', memoryrecord_setValue);
  lua_register(LuaVM, 'memoryrecord_getScript', memoryrecord_getScript);
  lua_register(LuaVM, 'memoryrecord_setScript', memoryrecord_setScript);
  lua_register(LuaVM, 'memoryrecord_isActive', memoryrecord_isActive);
  lua_register(LuaVM, 'memoryrecord_isSelected', memoryrecord_isSelected);
  lua_register(LuaVM, 'memoryrecord_freeze', memoryrecord_freeze);
  lua_register(LuaVM, 'memoryrecord_unfreeze', memoryrecord_unfreeze);
  lua_register(LuaVM, 'memoryrecord_setColor', memoryrecord_setColor);
  lua_register(LuaVM, 'memoryrecord_appendToEntry', memoryrecord_appendToEntry);
  lua_register(LuaVM, 'memoryrecord_delete', memoryrecord_delete);

  lua_register(LuaVM, 'memoryrecord_getID', memoryrecord_getID);
  lua_register(LuaVM, 'memoryrecord_getHotkeyCount', memoryrecord_getHotkeyCount);
  lua_register(LuaVM, 'memoryrecord_getHotkey', memoryrecord_getHotkey);
  lua_register(LuaVM, 'memoryrecord_getHotkeyByID', memoryrecord_getHotkeyByID);
  lua_register(LuaVM, 'memoryrecord_onActivate', memoryrecord_onActivate);
  lua_register(LuaVM, 'memoryrecord_onDeactivate', memoryrecord_onDeactivate);
  lua_register(LuaVM, 'memoryrecord_onDestroy', memoryrecord_onDestroy);
end;

end.

