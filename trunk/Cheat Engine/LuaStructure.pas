unit LuaStructure;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, LuaHandler, LuaCaller, symbolhandler, cefuncproc, newkernelhandler;

procedure initializeLuaStructure;

implementation

uses StructuresFrm2;

function getStructureCount(L: PLua_State): integer; cdecl;
begin
  result:=1;
  lua_tointeger(L, DissectedStructs.Count);
end;

function getStructure(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  i: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    i:=lua_tointeger(L,-parameters);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, DissectedStructs[i]);
    result:=1;
  end else lua_pop(L, parameters);
end;

function createStructure(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  name: string;
  struct: TDissectedStruct;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    name:=Lua_ToString(L,-parameters);
    lua_pop(L, parameters);

    struct:=TDissectedStruct.create(name);

    lua_pushlightuserdata(L, struct);
    result:=1;
  end else lua_pop(L, parameters);
end;

function structure_getName(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    struct:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    lua_pushstring(L, struct.name);
    result:=1;
  end else lua_pop(L, parameters);
end;

function structure_setName(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
  Name: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    struct:=lua_touserdata(L,-parameters);
    name:=Lua_ToString(L, -parameters+1);
    lua_pop(L, parameters);

    struct.Name:=Name;
  end else lua_pop(L, parameters);
end;

function structure_getSize(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    struct:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    lua_pushinteger(L, struct.structuresize);
    result:=1;
  end else lua_pop(L, parameters);
end;

function structure_getElementCount(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    struct:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    lua_pushinteger(L, struct.count);
    result:=1;
  end else lua_pop(L, parameters);
end;

function structure_getElement(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    struct:=lua_touserdata(L,-parameters);
    index:=lua_tointeger(L,-parameters+1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, struct.element[index]);
    result:=1;
  end else lua_pop(L, parameters);
end;

function structure_getElementByOffset(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
  offset: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    struct:=lua_touserdata(L,-parameters);
    offset:=lua_tointeger(L,-parameters+1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, struct.element[struct.getIndexOfOffset(offset)]);
    result:=1;
  end else lua_pop(L, parameters);
end;

function structure_addElement(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    struct:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, struct.addElement);
    result:=1;
  end else lua_pop(L, parameters);
end;

function structure_autoGuess(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
  base: ptruint;
  offset: integer;
  size: integer;
begin
  // structure_autoGuess(structure, baseaddresstoguessfrom, offset, size)

  result:=0;
  parameters:=lua_gettop(L);
  if parameters=4 then
  begin
    struct:=lua_touserdata(L,-parameters);

    if lua_isstring(L, -1) then
      base:=symhandler.getAddressFromName(lua_tostring(L,-parameters+1))
    else
      base:=lua_tointeger(L,-parameters+1);


    offset:=lua_tointeger(L,-parameters+2);
    size:=lua_tointeger(L,-parameters+3);
    lua_pop(L, parameters);

    struct.autoGuessStruct(base, offset, size);
    result:=0;
  end else lua_pop(L, parameters);
end;

function structure_beginUpdate(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    struct:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    struct.beginUpdate;

  end else lua_pop(L, parameters);
end;

function structure_endUpdate(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    struct:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    struct.endUpdate;

  end else lua_pop(L, parameters);
end;

function structure_addToGlobalStructureList(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    struct:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    struct.addToGlobalStructList;

  end else lua_pop(L, parameters);
end;

function structure_removeFromGlobalStructureList(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    struct:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    struct.removeFromGlobalStructList;

  end else lua_pop(L, parameters);
end;

function structureElement_getOwnerStructure(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  se: TStructelement;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    se:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, se.parent);
    result:=1;
  end else lua_pop(L, parameters);
end;

function structureElement_getOffset(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  se: TStructelement;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    se:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    lua_pushinteger(L, se.Offset);
    result:=1;
  end else lua_pop(L, parameters);
end;

function structureElement_setOffset(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  se: TStructelement;
  offset: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    se:=lua_touserdata(L,-parameters);
    offset:=Lua_Tointeger(L, -parameters+1);
    lua_pop(L, parameters);

    se.offset:=offset;
  end else lua_pop(L, parameters);
end;

function structureElement_getName(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  se: TStructelement;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    se:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    lua_pushstring(L, se.name);
    result:=1;
  end else lua_pop(L, parameters);
end;

function structureElement_setName(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  se: TStructelement;
  Name: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    se:=lua_touserdata(L,-parameters);
    name:=Lua_ToString(L, -parameters+1);
    lua_pop(L, parameters);

    se.Name:=Name;
  end else lua_pop(L, parameters);
end;

function structureElement_getVartype(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  se: TStructelement;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    se:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    lua_pushinteger(L, integer(se.Vartype));
    result:=1;
  end else lua_pop(L, parameters);
end;

function structureElement_setVartype(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  se: TStructelement;
  Vartype: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    se:=lua_touserdata(L,-parameters);
    Vartype:=lua_tointeger(L, -parameters+1);
    lua_pop(L, parameters);

    se.Vartype:=Tvariabletype(Vartype);
  end else lua_pop(L, parameters);
end;


function structureElement_getChildStruct(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  se: TStructelement;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    se:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, se.ChildStruct);
    result:=1;
  end else lua_pop(L, parameters);
end;

function structureElement_setChildStruct(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  se: TStructelement;
  Childstruct: TDissectedStruct;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    se:=lua_touserdata(L,-parameters);
    Childstruct:=lua_touserdata(L, -parameters+1);
    lua_pop(L, parameters);

    se.Childstruct:=Childstruct;
  end else lua_pop(L, parameters);
end;

function structureElement_getChildStructStart(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  se: TStructelement;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    se:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    lua_pushinteger(L, se.ChildStructStart);
    result:=1;
  end else lua_pop(L, parameters);
end;

function structureElement_setChildStructStart(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  se: TStructelement;
  start: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    se:=lua_touserdata(L,-parameters);
    start:=Lua_Tointeger(L, -parameters+1);
    lua_pop(L, parameters);

    se.ChildStructStart:=start;
  end else lua_pop(L, parameters);
end;

function structureElement_getByteSize(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  se: TStructelement;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    se:=lua_touserdata(L,-parameters);
    lua_pop(L, parameters);

    lua_pushinteger(L, se.Bytesize);
    result:=1;
  end else lua_pop(L, parameters);
end;

function structureElement_setByteSize(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  se: TStructelement;
  bs: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    se:=lua_touserdata(L,-parameters);
    bs:=Lua_Tointeger(L, -parameters+1);
    lua_pop(L, parameters);

    se.Bytesize:=bs;
  end else lua_pop(L, parameters);
end;

procedure initializeLuaStructure;
begin
  lua_register(LuaVM, 'getStructureCount', getStructureCount);
  lua_register(LuaVM, 'getStructure', getStructure);

  lua_register(LuaVM, 'createStructure', createStructure);

  lua_register(LuaVM, 'structure_getName', structure_getName);
  lua_register(LuaVM, 'structure_setName', structure_setName);
  lua_register(LuaVM, 'structure_getSize', structure_getSize);
  lua_register(LuaVM, 'structure_getElementCount', structure_getElementCount);
  lua_register(LuaVM, 'structure_getElement', structure_getElement);
  lua_register(LuaVM, 'structure_getElementByOffset', structure_getElementByOffset);
  lua_register(LuaVM, 'structure_addElement', structure_addElement);
  lua_register(LuaVM, 'structure_autoGuess', structure_autoGuess);
  lua_register(LuaVM, 'structure_beginUpdate', structure_beginUpdate);
  lua_register(LuaVM, 'structure_endUpdate', structure_endUpdate);

  lua_register(LuaVM, 'structure_addToGlobalStructureList', structure_addToGlobalStructureList);
  lua_register(LuaVM, 'structure_removeFromGlobalStructureList', structure_removeFromGlobalStructureList);


  //structurelement is part of a structure so define here as well
  lua_register(LuaVM, 'structureElement_getOwnerStructure', structureElement_getOwnerStructure);
  lua_register(LuaVM, 'structureElement_getOffset', structureElement_getOffset);
  lua_register(LuaVM, 'structureElement_setOffset', structureElement_setOffset);
  lua_register(LuaVM, 'structureElement_getName', structureElement_getName);
  lua_register(LuaVM, 'structureElement_setName', structureElement_setName);
  lua_register(LuaVM, 'structureElement_getVartype', structureElement_getVartype);
  lua_register(LuaVM, 'structureElement_setVartype', structureElement_setVartype);
  lua_register(LuaVM, 'structureElement_getChildStruct', structureElement_getChildStruct);
  lua_register(LuaVM, 'structureElement_setChildStruct', structureElement_setChildStruct);
  lua_register(LuaVM, 'structureElement_getChildStructStart', structureElement_getChildStructStart);
  lua_register(LuaVM, 'structureElement_setChildStructStart', structureElement_setChildStructStart);
  lua_register(LuaVM, 'structureElement_getBytesize', structureElement_getBytesize);
  lua_register(LuaVM, 'structureElement_setBytesize', structureElement_setBytesize);



end;

end.

