unit LuaDiagram;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib,LuaHandler, diagram, diagramblock,
  diagramlink, diagramtypes, typinfo;

procedure initializeLuaDiagram;

function lua_toDiagramBlockSideDescriptor(L: PLua_state; index: integer): TDiagramBlockSideDescriptor;
procedure lua_pushDiagramBlockSideDescriptor(L: PLua_state; dbsd: TDiagramBlockSideDescriptor);

implementation

uses controls, luaclass, LuaCustomControl, LuaObject;

procedure lua_pushDiagramBlockSideDescriptor(L: PLua_state; dbsd: TDiagramBlockSideDescriptor);
var i: integer;
begin
  lua_newtable(L);
  i:=lua_gettop(L);

  lua_pushstring(L, 'Block');
  luaclass_newClass(L, dbsd.block);
  lua_settable(L,i);

  lua_pushstring(L, 'Side');
  lua_pushinteger(L,integer(dbsd.side));
  lua_settable(L,i);

  lua_pushstring(L, 'Position');
  lua_pushinteger(L,dbsd.sideposition);
  lua_settable(L,i);
end;

function lua_toDiagramBlockSideDescriptor(L: PLua_state; index: integer): TDiagramBlockSideDescriptor;
var
  r: TDiagramBlockSideDescriptor;
  i: integer;
begin
  r.block:=nil;
  r.side:=dbsTop;
  r.sideposition:=0;

  i:=lua_absindex(L,index);

  if lua_istable(L,i) then
  begin
    lua_pushstring(L,'Block');
    lua_gettable(L,i);

    if not lua_isnil(L,-1) then
      r.block:=lua_ToCEUserData(L,-1)
    else
    begin
      lua_pop(l,1);
      //try integer (perhaps it's just a block and no side)
      lua_pushinteger(L,1);
      lua_gettable(L,i);

      if lua_isuserdata(L,-1) then
        r.block:=lua_ToCEUserData(L,-1);
    end;
    lua_pop(l,1);

    lua_pushstring(L,'Side');
    lua_gettable(L,i);
    if not lua_isnil(L,-1) then
    begin
      if lua_isinteger(L,-1) then
        r.side:=TDiagramBlockSide(lua_tointeger(L,-1))
      else
      begin
        //string (not documented, but supported)
        r.side:=TDiagramBlockSide(GetEnumValue(typeinfo(TDiagramBlockSide),lua_ToString(L,-1)));
      end;
    end;
    lua_pop(l,1);

    lua_pushstring(L,'Position');
    lua_gettable(L,i);
    r.sideposition:=lua_tointeger(L,-1); //even if nil, result is ok
    lua_pop(l,1);
  end
  else
  if lua_isuserdata(L,i) then
    r.block:=lua_ToCEUserData(L,i);


  result:=r;
end;

function diagram_addConnection(L: PLua_state): integer; cdecl;
var
  diagram: TDiagram;
  origin: TDiagramBlockSideDescriptor;
  destination: TDiagramBlockSideDescriptor;
  link: TDiagramLink;
  ob,db: TDiagramBlock;
begin
  result:=0;
  diagram:=luaclass_getClassObject(L);
  link:=nil;

  if lua_gettop(L)=2 then
  begin
    if lua_istable(L,1) and lua_istable(L,2) then
    begin
      //desc,desc
      origin:=lua_toDiagramBlockSideDescriptor(L,1);
      destination:=lua_toDiagramBlockSideDescriptor(L,2);
      link:=diagram.addConnection(origin,destination);
    end
    else
    if lua_isuserdata(L,1) and lua_isuserdata(L,2) then
    begin
      ob:=lua_ToCEUserData(L,1);
      db:=lua_ToCEUserData(L,2);
      link:=diagram.addConnection(ob,db);
    end;

    if link<>nil then
    begin
      luaclass_newClass(L,link);
      result:=1;
    end;
  end;

end;

function diagram_createBlock(L: PLua_state): integer; cdecl;
var
  diagram: TDiagram;
  b: TDiagramBlock;
begin
  diagram:=luaclass_getClassObject(L);
  b:=diagram.createBlock;

  luaclass_newClass(L,b);
  result:=1;
end;

function diagram_getLink(L: PLua_state): integer; cdecl;
var
  diagram: TDiagram;
  i: integer;
begin
  result:=0;
  diagram:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
  begin
    i:=lua_tointeger(L,1);
    luaclass_newClass(L, diagram.Link[i]);
    result:=1;
  end;
end;

function diagram_getBlock(L: PLua_state): integer; cdecl;
var
  diagram: TDiagram;
  i: integer;
begin
  result:=0;
  diagram:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
  begin
    i:=lua_tointeger(L,1);
    luaclass_newClass(L, diagram.Block[i]);
    result:=1;
  end;
end;

function diagram_saveAsImage(L: PLua_state): integer; cdecl;
var
  diagram: TDiagram;
  filename: string;
begin
  result:=0;
  diagram:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
  begin
    filename:=Lua_ToString(L,1);
    diagram.saveAsImage(filename);
  end;
end;

function diagram_saveToFile(L: PLua_state): integer; cdecl;
var
  diagram: TDiagram;
  filename: string;
begin
  result:=0;
  diagram:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
  begin
    filename:=Lua_ToString(L,1);
    diagram.saveToFile(filename);
  end;
end;

function diagram_loadFromFile(L: PLua_state): integer; cdecl;
var
  diagram: TDiagram;
  filename: string;
begin
  result:=0;
  diagram:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
  begin
    filename:=Lua_ToString(L,1);
    diagram.loadFromFile(filename);
  end;
end;

function diagram_saveToStream(L: PLua_state): integer; cdecl;
var
  diagram: TDiagram;
  s: tstream;
begin
  result:=0;
  diagram:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
  begin
    s:=lua_ToCEUserData(L,1);
    diagram.saveToStream(s);
  end;
end;

function diagram_loadFromStream(L: PLua_state): integer; cdecl;
var
  diagram: TDiagram;
  s: tstream;
begin
  result:=0;
  diagram:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
  begin
    s:=lua_ToCEUserData(L,1);
    diagram.loadFromStream(s);
  end;
end;

function diagram_getObjectAt(L: PLua_state): integer; cdecl;
var
  d: TDiagram;
  p: tpoint;
  o: TObject;
begin
  result:=0;
  d:=luaclass_getClassObject(L);
  if lua_gettop(L)>0 then
  begin
    if lua_gettop(L)>=2 then
    begin
      p.x:=lua_tointeger(L,1);
      p.y:=lua_tointeger(L,2);
    end
    else
      p:=lua_toPoint(L,1);

    o:=d.getObjectAt(p);
    luaclass_newClass(L,o);
    result:=1;
  end;
end;



function createDiagram(L: PLua_state): integer; cdecl;
var
  d: TDiagram;
  owner: TWinControl;
begin
  result:=0;

  if lua_gettop(L)>=1 then
    owner:=lua_toceuserdata(L, 1)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));

  d:=TDiagram.Create(owner);
  d.width:=100;
  d.height:=100;

  if owner<>nil then
    d.Parent:=owner;

  luaclass_newClass(L, d);
  result:=1;
end;



procedure diagram_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  customcontrol_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'createBlock', diagram_createBlock);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'addConnection', diagram_addConnection);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'saveAsImage', diagram_saveAsImage);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'saveToFile', diagram_saveToFile);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'loadFromFile', diagram_loadFromFile);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'saveToStream', diagram_saveToStream);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'loadFromStream', diagram_loadFromStream);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getObjectAt', diagram_getObjectAt);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Link', diagram_getLink);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Block', diagram_getBlock);

end;


procedure initializeLuaDiagram;
var
  i: TDiagramBlockSide;
  ti: pTypeInfo;
begin
  lua_register(LuaVM, 'createDiagram', createDiagram);

  ti:=typeinfo(TDiagramBlockSide);
  for i:=dbsTop to dbsBottomRight do
  begin
    lua_pushinteger(LuaVM,integer(i));
    lua_setglobal(LuaVM,pchar(GetEnumName(ti,integer(i))));
  end;

end;

initialization
  luaclass_register(TDiagram, diagram_addMetaData);

end.

