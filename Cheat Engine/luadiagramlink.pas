unit LuaDiagramLink;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib,LuaHandler, diagram, diagramblock,
  diagramlink, diagramtypes, typinfo;

implementation

uses controls, luaclass, LuaCustomControl, LuaObject, LuaDiagram;

function diagramLink_reset(L: PLua_state): integer; cdecl;
var
  dl: TDiagramLink;
begin
  dl:=luaclass_getClassObject(L);
  dl.ResetToDefault;
  result:=0;
end;

function diagramLink_getPointPosition(L: PLua_state): integer; cdecl;
var
  dl: TDiagramLink;
  index: integer;
  p: tpoint;
begin
  result:=0;
  dl:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    index:=lua_tointeger(L,1);
    if (index>=0) and (index<dl.getPointCount) then
    begin
      p:=dl.getPoint(index);
      lua_pushpoint(L,p);
      result:=1;
    end;
  end;
end;

function diagramLink_setPointPosition(L: PLua_state): integer; cdecl;
var
  dl: TDiagramLink;
  index: integer;
  p: tpoint;
begin
  result:=0;
  dl:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
  begin
    index:=lua_tointeger(L,1);
    p:=lua_toPoint(L,2);
    if (index>=0) and (index<dl.getPointCount) then
      dl.updatePointPosition(index,p);
  end;
end;

function diagramLink_hasLinkToBlock(L: PLua_state): integer; cdecl;
var
  dl: TDiagramLink;
  b: TDiagramBlock;
begin
  result:=0;
  dl:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    b:=lua_ToCEUserData(L,1);
    lua_pushboolean(L, dl.hasLinkToBlock(b));
    result:=1;
  end;
end;

function diagramLink_getOriginDescriptor(L: PLua_state): integer; cdecl;
var
  dl: TDiagramLink;
begin
  dl:=luaclass_getClassObject(L);
  lua_pushDiagramBlockSideDescriptor(L,dl.getOriginDescriptor);
  result:=1;
end;

function diagramLink_setOriginDescriptor(L: PLua_state): integer; cdecl;
var
  dl: TDiagramLink;
begin
  result:=0;
  dl:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
    dl.setOriginDescriptor(lua_toDiagramBlockSideDescriptor(L,1));
end;

function diagramLink_getDestinationDescriptor(L: PLua_state): integer; cdecl;
var
  dl: TDiagramLink;
begin
  dl:=luaclass_getClassObject(L);
  lua_pushDiagramBlockSideDescriptor(L,dl.getDestinationDescriptor);
  result:=1;
end;

function diagramLink_setDestinationDescriptor(L: PLua_state): integer; cdecl;
var
  dl: TDiagramLink;
begin
  result:=0;
  dl:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
    dl.setDestinationDescriptor(lua_toDiagramBlockSideDescriptor(L,1));
end;

function diagramLink_updateSide(L: PLua_state): integer; cdecl;
var
  dl: TDiagramLink;
begin
  result:=0;
  dl:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
    dl.updateSide(lua_toDiagramBlockSideDescriptor(L,1));
end;

function diagramLink_getPointIndexAt(L: PLua_state): integer; cdecl;
var
  dl: TDiagramLink;
  x,y: integer;
begin
  result:=0;
  dl:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
  begin
    x:=lua_tointeger(L,1);
    y:=lua_tointeger(L,2);
    lua_pushinteger(L, dl.getPointIndexAt(x,y));
    result:=1;
  end;
end;

function diagramLink_addPoint(L: PLua_state): integer; cdecl;
var
  dl: TDiagramLink;
  x,y,i: integer;
begin
  result:=0;
  dl:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
  begin
    x:=lua_tointeger(L,1);
    y:=lua_tointeger(L,2);
    if lua_gettop(L)>=3 then
      i:=lua_tointeger(L,3)
    else
      i:=-1;

    dl.createPoint(point(x,y),i);
    result:=0;
  end;
end;

function diagramLink_removeAllPoints(L: PLua_state): integer; cdecl;
var
  dl: TDiagramLink;
begin
  result:=0;
  dl:=luaclass_getClassObject(L);
  dl.RemoveAllPlotPoints;
end;

procedure diagramLink_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'reset', diagramLink_reset);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'hasLinkToBlock', diagramLink_hasLinkToBlock);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'updateSide', diagramLink_updateSide);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getPointIndexAt', diagramLink_getPointIndexAt);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'addPoint', diagramLink_addPoint);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'removeAllPoints', diagramLink_removeAllPoints);


  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Points', diagramLink_getPointPosition, diagramLink_setPointPosition);
  luaclass_addPropertyToTable(L, metatable, userdata, 'OriginDescriptor', diagramLink_getOriginDescriptor, diagramLink_setOriginDescriptor);
  luaclass_addPropertyToTable(L, metatable, userdata, 'DestinationDescriptor', diagramLink_getDestinationDescriptor, diagramLink_setDestinationDescriptor);
end;

initialization
  luaclass_register(TDiagramLink, diagramLink_addMetaData);

end.


