unit LuaStructure;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, LuaHandler, LuaCaller, symbolhandler,
  cefuncproc, newkernelhandler, Dialogs, LuaClass, LuaClassArray, commonTypeDefs;

procedure initializeLuaStructure;

procedure structure_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
procedure structureElement_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses StructuresFrm2, LuaObject, DotNetPipe, symbolhandlerstructs;


resourcestring
  rsInvalidIndex='Invalid index';


function getStructureCount(L: PLua_State): integer; cdecl;
begin
  result:=1;
  lua_pushinteger(L, DissectedStructs.Count);
end;

function getStructure(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  i: integer;
  userdata, metatable: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    i:=lua_tointeger(L,1);
    lua_pop(L, parameters);

    if (i<0) or (i>=DissectedStructs.Count) then
    begin
      lua_pushnil(L);
      lua_pushstring(L,rsInvalidindex);
      exit(2);
    end;

    luaclass_newClass(L, DissectedStructs[i]);

    result:=1;
  end else lua_pop(L, parameters);
end;



function structure_getName(L: PLua_State): integer; cdecl;
var
  struct: TDissectedStruct;
begin
  struct:=luaclass_getClassObject(L);
  lua_pushstring(L, struct.name);
  result:=1;
end;

function structure_setName(L: PLua_State): integer; cdecl;
var
  struct: TDissectedStruct;
begin
  result:=0;

  struct:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    struct.name:=Lua_ToString(L, 1);
end;

function structure_getSize(L: PLua_State): integer; cdecl;
var
  struct: TDissectedStruct;
begin
  struct:=luaclass_getClassObject(L);
  lua_pushinteger(L, struct.structuresize);
  result:=1;
end;

function structure_getElementCount(L: PLua_State): integer; cdecl;
var
  struct: TDissectedStruct;
begin
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
    index:=lua_tointeger(L,1);
    if (index>=0) and (index<struct.count) then
    begin
      luaclass_newclass(L, struct.element[index]);
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
  struct:=luaclass_getClassObject(L);

  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    offset:=lua_tointeger(L,1);
    luaclass_newclass(L, struct.element[struct.getIndexOfOffset(offset)]);
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

  luaclass_newclass(L, struct.addElement);
  result:=1;
end;

function structure_fillFromDotNetAddress(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  struct: TDissectedStruct;
  address: ptruint;
  changename: boolean;

  al: TAddressData;
begin
  struct:=luaclass_getClassObject(L);
  result:=0;
  parameters:=lua_gettop(L);

  changename:=false;
  if parameters>=1 then
  begin
    if lua_isnumber(L, 1) then
      address:=lua_tointeger(L,1)
    else
      address:=symhandler.getAddressFromNameL(lua_tostring(L,1));


    if (parameters>=2) then
      changename:=lua_toboolean(L, 2);


    if symhandler.GetLayoutFromAddress(address, al) then
    begin
      struct.fillFromDotNetAddressData(al);

      if changename then
        struct.setName(al.typedata.classname);
    end;


  end;
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
    if lua_isstring(L, 1) then
      base:=symhandler.getAddressFromNameL(lua_tostring(L,1))
    else
      base:=lua_tointeger(L,1);


    offset:=lua_tointeger(L,2);
    size:=lua_tointeger(L,3);
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
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'fillFromDotNetAddress', structure_fillFromDotNetAddress);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'beginUpdate', structure_beginUpdate);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'endUpdate', structure_endUpdate);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'addToGlobalStructureList', structure_addToGlobalStructureList);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'removeFromGlobalStructureList', structure_removeFromGlobalStructureList);
end;


function createStructureFromName(L: PLua_State): integer; cdecl;
var
  name: string;
  i: integer;
  el: tstringlist;
  e: TDBElementInfo;
  struct: TDissectedStruct;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    name:=Lua_ToString(L,1);

    el:=tstringlist.create;
    try
      symhandler.getStructureElementsFromName(name,el);
      if el.count>0 then
      begin
        struct:=TDissectedStruct.create(name);
        //fill in the struct based on what is in e

        for i:=0 to el.count-1 do
        begin
          e:=TDBElementInfo(el.Objects[i]);

          struct.addElement(el[i],e.offset, e.vartype);

          e.Free;
        end;




        luaclass_newclass(L, struct, structure_addMetaData);
        result:=1;
      end;
    finally
      el.free;
    end;
  end;
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
    luaclass_newclass(L, struct, structure_addMetaData);
    result:=1;
  end else lua_pop(L, parameters);
end;

function structureElement_getOwnerStructure(L: PLua_State): integer; cdecl;
var
  se: TStructelement;
begin
  se:=luaclass_getClassObject(L);
  luaclass_newclass(L, se.parent, structure_addMetaData);
  result:=1;
end;

function structureElement_getOffset(L: PLua_State): integer; cdecl;
var
  se: TStructelement;
begin
  se:=luaclass_getClassObject(L);
  lua_pushinteger(L, se.Offset);
  result:=1;
end;

function structureElement_setOffset(L: PLua_State): integer; cdecl;
var
  se: TStructelement;
  offset: integer;
begin
  result:=0;
  se:=luaclass_getClassObject(L);

  offset:=Lua_Tointeger(L, 1);
  se.offset:=offset;
end;

function structureElement_getValue(L: PLua_State): integer; cdecl;
var
  se: TStructelement;
  address: ptruint;
begin
  se:=luaclass_getClassObject(L);

  address:=lua_toaddress(L,1);

  lua_pushstring(L, se.getValue(address));
  result:=1;
end;

function structureElement_setValue(L: PLua_State): integer; cdecl;
var
  se: TStructelement;
  address: ptruint;
  value: string;
begin
  se:=luaclass_getClassObject(L);

  address:=lua_toaddress(L,1);
  value:=Lua_ToString(L,2);

  se.setValue(address,value);
  result:=1;
end;

function structureElement_getValueFromBase(L: PLua_State): integer; cdecl;
var
  se: TStructelement;
  baseaddress: ptruint;
begin
  se:=luaclass_getClassObject(L);

  baseaddress:=lua_toaddress(L,1);

  lua_pushstring(L, se.getValueFromBase(baseaddress));
  result:=1;
end;

function structureElement_setValueFromBase(L: PLua_State): integer; cdecl;
var
  se: TStructelement;
  baseaddress: ptruint;
  value: string;
begin
  se:=luaclass_getClassObject(L);

  baseaddress:=lua_toaddress(L,1);
  value:=Lua_ToString(L,2);

  se.setValueFromBase(baseaddress,value);
  result:=1;
end;


function structureElement_getName(L: PLua_State): integer; cdecl;
var
  se: TStructelement;
begin
  se:=luaclass_getClassObject(L);
  lua_pushstring(L, se.name);
  result:=1;
end;

function structureElement_setName(L: PLua_State): integer; cdecl;
var
  se: TStructelement;
  Name: string;
begin
  result:=0;
  se:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    name:=Lua_ToString(L, 1);
    se.Name:=Name;
  end;
end;

function structureElement_getVartype(L: PLua_State): integer; cdecl;
var
  se: TStructelement;
begin
  se:=luaclass_getClassObject(L);
  lua_pushinteger(L, integer(se.Vartype));
  result:=1;
end;

function structureElement_setVartype(L: PLua_State): integer; cdecl;
var
  se: TStructelement;
  Vartype: integer;
begin
  result:=0;
  se:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    Vartype:=lua_tointeger(L, 1);
    se.Vartype:=Tvariabletype(Vartype);
  end;
end;


function structureElement_getChildStruct(L: PLua_State): integer; cdecl;
var
  se: TStructelement;
begin
  se:=luaclass_getClassObject(L);
  luaclass_newClass(L, se.ChildStruct, structure_addMetaData);
  result:=1;
end;

function structureElement_setChildStruct(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  se: TStructelement;
  o: tobject;
  Childstruct: TDissectedStruct;
begin
  result:=0;
  se:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    if lua_isnil(L,1) then
      childstruct:=nil
    else
    begin
      o:=lua_ToCEUserData(L, 1);
      if o is TDissectedStruct then
        Childstruct:=TDissectedStruct(o)
      else
        raise EStructureException.create('Invalid child structure object:'+o.ClassName);
    end;

    se.Childstruct:=Childstruct;
  end;
end;

function structureElement_getChildStructStart(L: PLua_State): integer; cdecl;
var
  se: TStructelement;
begin
  se:=luaclass_getClassObject(L);
  lua_pushinteger(L, se.ChildStructStart);
  result:=1;
end;

function structureElement_setChildStructStart(L: PLua_State): integer; cdecl;
var
  se: TStructelement;
begin
  result:=0;
  se:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    se.ChildStructStart:=lua_tointeger(L, 1);
end;

function structureElement_getByteSize(L: PLua_State): integer; cdecl;
var
  se: TStructelement;
begin
  se:=luaclass_getClassObject(L);
  lua_pushinteger(L, se.Bytesize);
  result:=1;
end;

function structureElement_setByteSize(L: PLua_State): integer; cdecl;
var
  se: TStructelement;
begin
  result:=0;
  se:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    se.bytesize:=lua_tointeger(L, 1);
end;

procedure structureElement_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOwnerStructure', structureElement_getOwnerStructure);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Owner', structureElement_getOwnerStructure, nil);


  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOffset', structureElement_getOffset);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOffset', structureElement_setOffset);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Offset', structureElement_getOffset, structureElement_setOffset);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getName', structureElement_getName);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setName', structureElement_setName);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Name', structureElement_getName, structureElement_setName);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getValue', structureElement_getValue);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setValue', structureElement_setValue);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getValueFromBase', structureElement_getValueFromBase);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setValueFromBase', structureElement_setValueFromBase);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getVartype', structureElement_getVartype);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setVartype', structureElement_setVartype);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Vartype', structureElement_getVartype, structureElement_setVartype);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getChildStruct', structureElement_getChildStruct);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setChildStruct', structureElement_setChildStruct);
  luaclass_addPropertyToTable(L, metatable, userdata, 'ChildStruct', structureElement_getChildStruct, structureElement_setChildStruct);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getChildStructStart', structureElement_getChildStructStart);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setChildStructStart', structureElement_setChildStructStart);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getBytesize', structureElement_getBytesize);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setBytesize', structureElement_setBytesize);




end;



procedure initializeLuaStructure;
begin
  lua_register(LuaVM, 'getStructureCount', getStructureCount);
  lua_register(LuaVM, 'getStructure', getStructure);

  lua_register(LuaVM, 'createStructure', createStructure);
  lua_register(LuaVM, 'createStructureFromName', createStructureFromName);



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

initialization
  luaclass_register(TDissectedStruct, structure_addMetaData);
  luaclass_register(TStructelement, structureElement_addMetaData);

end.

