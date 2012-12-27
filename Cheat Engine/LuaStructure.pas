unit LuaStructure;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, LuaHandler, LuaCaller, symbolhandler,
  cefuncproc, newkernelhandler, Dialogs, LuaClass, LuaClassArray;

procedure initializeLuaStructure;

procedure structure_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses StructuresFrm2, LuaObject;



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

    lua_newuserdata(L, DissectedStructs[i]);

//    PDissectedStruct(lua_newuserdata(L, sizeof(TDissectedStruct)))^:=DissectedStructs[i];
    result:=1;
  end else lua_pop(L, parameters);
end;



function structure_getName(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
begin
  result:=0;

  struct:=luaclass_getClassObject(L);

  if struct=nil then exit;

  lua_pushstring(L, struct.name);
  result:=1;
end;

function structure_setName(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
  Name: string;
  i: integer;
begin
  result:=0;

  struct:=luaclass_getClassObject(L);

  if struct=nil then exit;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    name:=Lua_ToString(L, -1); //last parameter

  struct.Name:=Name;

  lua_pop(L, parameters);
end;

function structure_getSize(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
begin
  result:=0;
  parameters:=lua_gettop(L);

  struct:=luaclass_getClassObject(L);

  lua_pushinteger(L, struct.structuresize);
end;

function structure_getElementCount(L: PLua_State): integer; cdecl;
var
  struct: TDissectedStruct;
begin
  result:=0;
  struct:=luaclass_getClassObject(L);

  lua_pushinteger(L, struct.count);
  result:=1;
end;

function structure_getElement(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
  index: integer;
begin
  result:=0;
  struct:=luaclass_getClassObject(L);

  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    index:=lua_tointeger(L,-1);
    if index<struct.count then
    begin
      lua_newuserdata(L, struct.element[index]);
      result:=1;
    end;
  end;
end;


function structure_getElementByOffset(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
  offset: integer;
begin
  result:=0;

  result:=0;
  struct:=luaclass_getClassObject(L);

  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    offset:=lua_tointeger(L,-1);
    lua_newuserdata(L, struct.element[struct.getIndexOfOffset(offset)]);
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

  struct:=luaclass_getClassObject(L);


  lua_newuserdata(L, struct.addElement);
  result:=1;
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

  struct:=luaclass_getClassObject(L);

  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=3 then
  begin
    if lua_isstring(L, -3) then
      base:=symhandler.getAddressFromNameL(lua_tostring(L,-3))
    else
      base:=lua_tointeger(L,-3);


    offset:=lua_tointeger(L,-2);
    size:=lua_tointeger(L,-1);
    struct.autoGuessStruct(base, offset, size);
    result:=0;
  end;
end;

function structure_beginUpdate(L: PLua_State): integer; cdecl;
var
  struct: TDissectedStruct;
begin
  result:=0;
  struct:=luaclass_getClassObject(L);

  struct.beginUpdate;
end;

function structure_endUpdate(L: PLua_State): integer; cdecl;
var
  struct: TDissectedStruct;
begin
  result:=0;
  struct:=luaclass_getClassObject(L);

  struct.endUpdate;
end;

function structure_addToGlobalStructureList(L: PLua_State): integer; cdecl;
var
  struct: TDissectedStruct;
begin
  result:=0;
  struct:=luaclass_getClassObject(L);

  struct.addToGlobalStructList;
end;

function structure_removeFromGlobalStructureList(L: PLua_State): integer; cdecl;
var
  struct: TDissectedStruct;
begin
  result:=0;
  struct:=luaclass_getClassObject(L);

  struct.removeFromGlobalStructList;
end;




procedure structure_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
var i: integer;
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setName', structure_setName);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getName', structure_getName);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Name', structure_getName, structure_setName);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getSize', structure_getSize);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Size', structure_getSize, nil);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getElementCount', structure_getElementCount);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Count', structure_getElementCount, nil);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getElement', structure_getElement);


  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Element', structure_getElement);


  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getElementByOffset', structure_getElementByOffset);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'addElement', structure_addElement);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'autoGuess', structure_autoGuess);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'beginUpdate', structure_beginUpdate);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'endUpdate', structure_endUpdate);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'addToGlobalStructureList', structure_addToGlobalStructureList);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'removeFromGlobalStructureList', structure_removeFromGlobalStructureList);
end;

function createStructure(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  name: string;
  struct: TDissectedStruct;

  metatable: integer;
  m: tmethod;
  i: integer;
  userdata: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    name:=Lua_ToString(L,-parameters);
    lua_pop(L, parameters);

    struct:=TDissectedStruct.create(name);
    lua_newuserdata(L, struct);
    userdata:=lua_gettop(L);

    metatable:=luaclass_createMetaTable(L);
    structure_addMetaData(L, metatable, userdata);
    lua_setmetatable(L, userdata);

    result:=1;

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

