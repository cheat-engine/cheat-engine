unit LuaVirtualStringTree;

{$mode delphi}

interface

uses
  Classes, SysUtils, ComCtrls, lua, luaclass, Controls, LuaWinControl, laz.VirtualTrees, betterControls;


procedure initializeLuaVirtualStringTree;


implementation

uses LuaHandler, lauxlib, ceguicomponents, TypInfo, LuaByteTable;

function createVirtualStringTree(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  owner: TWincontrol;
begin
  if lua_gettop(L)=1 then
    owner:=TWincontrol(lua_toceuserdata(L, 1))
  else
    owner:=nil;

  tv:=TLazVirtualStringTree.Create(owner);
  if owner<>nil then
    tv.Parent:=owner;

  luaclass_newClass(L, tv);
  result:=1;
end;

function VirtualStringTree_nodeInfoToTable(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  p: PVirtualNode;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)=1 then
  begin
    if lua_islightuserdata(L,1) then
    begin
      p:=lua_touserdata(L, 1);
      try
        if p<>nil then
        begin
          lua_createtable(L, 0, 17);
          lua_pushstring(L, 'self');
          lua_pushlightuserdata(L,p);
          lua_settable(L,-3);

          lua_pushstring(L, 'index');
          lua_pushinteger(L,p^.Index);
          lua_settable(L,-3);

          lua_pushstring(L, 'childcount');
          lua_pushinteger(L,p^.ChildCount);
          lua_settable(L,-3);

          lua_pushstring(L, 'nodeheight');
          lua_pushinteger(L,p^.NodeHeight);
          lua_settable(L,-3);

          lua_pushstring(L, 'states');
          lua_pushstring(L, pchar(SetToString(PTypeInfo(typeinfo(TVirtualNodeStates)), pointer(@p^.States), false)));
          lua_settable(L,-3);

          lua_pushstring(L, 'align');
          lua_pushinteger(L,p^.align);
          lua_settable(L,-3);

          lua_pushstring(L, 'checkstate');
          lua_pushstring(L, GetEnumName(typeinfo(TCheckState), ord(p^.checkstate)));
          lua_settable(L,-3);

          lua_pushstring(L, 'totalcount');
          lua_pushinteger(L,p^.TotalCount);
          lua_settable(L,-3);

          lua_pushstring(L, 'totalheight');
          lua_pushinteger(L,p^.TotalHeight);
          lua_settable(L,-3);

          lua_pushstring(L, 'parent');
          lua_pushlightuserdata(L,p^.parent);
          lua_settable(L,-3);

          lua_pushstring(L, 'prevsibling');
          lua_pushlightuserdata(L,p^.PrevSibling);
          lua_settable(L,-3);

          lua_pushstring(L, 'nextsibling');
          lua_pushlightuserdata(L,p^.NextSibling);
          lua_settable(L,-3);

          lua_pushstring(L, 'firstchild');
          lua_pushlightuserdata(L,p^.firstchild);
          lua_settable(L,-3);

          lua_pushstring(L, 'lastchild');
          lua_pushlightuserdata(L,p^.firstchild);
          lua_settable(L,-3);

          if tv<>nil then
          begin
            lua_pushstring(L, 'data');
            CreateByteTableFromPointer(L,@p^.Data, tv.NodeDataSize);
            lua_settable(L,-3);
          end;

          exit(1);

        end;
      except
        on e: exception do
        begin
          lua_pushnil(L);
          lua_pushstring(L, pchar(e));
          exit(2);
        end;
      end;
    end
    else
    begin
      lua_pushnil(L);
      lua_pushstring(L,'You need to provide the NodeInfo as a userdata object');
      exit(2);
    end;
  end;
end;

function VirtualStringTree_tableToNodeInfo(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  p: PVirtualNode;
  maxsize: integer;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  p:=nil;
  if lua_gettop(L)=1 then
  begin
    if lua_istable(L,1) then
    begin
      lua_pushstring(L,'self');
      lua_gettable(L,1);
      p:=lua_topointer(L,-1);
      lua_pop(L,1);

      lua_pushstring(L, 'index');
      lua_gettable(L,1);
      p^.index:=lua_tointeger(L,-1);
      lua_pop(L,1);

      lua_pushstring(L, 'childcount');
      lua_gettable(L,1);
      p^.childcount:=lua_tointeger(L,-1);
      lua_pop(L,1);

      lua_pushstring(L, 'nodeheight');
      lua_gettable(L,1);
      p^.nodeheight:=lua_tointeger(L,-1);
      lua_pop(L,1);

      lua_pushstring(L, 'states');
      lua_gettable(L,1);
      StringToSet(PTypeInfo(typeinfo(TVirtualNodeStates)), string(Lua_ToString(L,-1)), @p^.States);
      lua_pop(L,1);

      lua_pushstring(L, 'align');
      lua_gettable(L,1);
      p^.align:=lua_tointeger(L,-1);
      lua_pop(L,1);

      lua_pushstring(L, 'checkstate');
      lua_gettable(L,1);
      p^.checkstate:=TCheckState(GetEnumValue(typeinfo(TCheckState), Lua_ToString(L,-1)));
      lua_pop(L,1);

      lua_pushstring(L, 'totalcount');
      lua_gettable(L,1);
      p^.totalcount:=lua_tointeger(L,-1);
      lua_pop(L,1);

      lua_pushstring(L, 'totalheight');
      lua_gettable(L,1);
      p^.totalheight:=lua_tointeger(L,-1);
      lua_pop(L,1);

      lua_pushstring(L, 'parent');
      lua_gettable(L,1);
      p^.parent:=lua_topointer(L,-1);
      lua_pop(L,1);

      lua_pushstring(L, 'prevsibling');
      lua_gettable(L,1);
      p^.prevsibling:=lua_topointer(L,-1);
      lua_pop(L,1);

      lua_pushstring(L, 'firstchild');
      lua_gettable(L,1);
      p^.firstchild:=lua_topointer(L,-1);
      lua_pop(L,1);

      lua_pushstring(L, 'lastchild');
      lua_gettable(L,1);
      p^.lastchild:=lua_topointer(L,-1);
      lua_pop(L,1);


      lua_pushstring(L,'data');
      lua_gettable(L,1);

      maxsize:=lua_objlen(l, -1);
      if tv<>nil then
        maxsize:=tv.NodeDataSize;

      if maxsize>0 then
        readBytesFromTable(L,lua_gettop(L), @p^.Data,maxsize);

      lua_pushlightuserdata(L,p);
      exit(1);
    end;
  end;
end;

function VirtualStringTree_addChild(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  parent: PVirtualNode;
  r: integer;
begin
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  parent:=nil;

  if lua_gettop(L)>=1 then
  begin
    if lua_istable(L,1) then
    begin
      lua_pushstring(L,'self');
      lua_gettable(L,1);
      parent:=lua_topointer(L,-1);
      lua_pop(L,-1);
    end
    else
      parent:=lua_topointer(L,1);
  end;

  lua_pushlightuserdata(L, tv.AddChild(parent));
  result:=1;
end;

procedure virtualstringtree_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'nodeInfoToTable', VirtualStringTree_nodeInfoToTable);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'tableToNodeInfo', VirtualStringTree_tableToNodeInfo);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'addChild', VirtualStringTree_addChild);

end;

function VirtualTreeColumns_add(L: Plua_State): integer; cdecl;
var columns: TVirtualTreeColumns;
begin
  columns:=TVirtualTreeColumns(luaclass_getClassObject(L));
  columns.Header.Options:=columns.Header.Options+[hoVisible];
  columns:=luaclass_getClassObject(L);
  luaclass_newClass(L, columns.Add);
  exit(1);
end;

procedure VirtualTreeColumns_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'add', VirtualTreeColumns_add);
end;


procedure initializeLuaVirtualStringTree;
begin
  lua_register(LuaVM, 'createVirtualStringTree', createVirtualStringTree);

  lua_register(LuaVM, 'nodeInfoToTable', VirtualStringTree_nodeInfoToTable);
  lua_register(LuaVM, 'tableToNodeInfo', VirtualStringTree_tableToNodeInfo);
end;



initialization
   luaclass_register(TVirtualStringTree,  virtualstringtree_addMetaData);
   luaclass_register(TVirtualTreeColumns,  VirtualTreeColumns_addMetaData);

end.

