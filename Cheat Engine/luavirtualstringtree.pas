unit LuaVirtualStringTree;

{$mode delphi}

interface

uses
  Classes, SysUtils, ComCtrls, lua, luaclass, Controls, LuaWinControl,
  laz.VirtualTrees, betterControls;


procedure initializeLuaVirtualStringTree;



implementation

uses LuaHandler, lauxlib, ceguicomponents, TypInfo, LuaByteTable, LuaCollection,
     LuaObject;



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

  tv.Header.Options:=tv.Header.Options-[hoDrag]; //too confusing for new users. Add it if you need it
  if owner<>nil then
    tv.Parent:=owner;

  luaclass_newClass(L, tv);
  result:=1;
end;

function VirtualStringTree_clear(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
begin
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  tv.Clear;
  result:=0;
end;


function VirtualStringTree_beginUpdate(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
begin
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  tv.BeginUpdate;
  result:=0;
end;

function VirtualStringTree_endUpdate(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
begin
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  tv.EndUpdate;
  result:=0;
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
    parent:=lua_topointer(L,1);

  lua_pushlightuserdata(L, tv.AddChild(parent));
  result:=1;
end;


function VirtualStringTree_deleteNode(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
  r: integer;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_topointer(L,1);
    tv.DeleteNode(node);
  end;
end;

function VirtualStringTree_deleteSelectedNodes(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  tv.DeleteSelectedNodes;
end;

function VirtualStringTree_addToSelection(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_topointer(L,1);
    if node<>nil then
      tv.AddToSelection(node);
  end;
end;

function VirtualStringTree_removeFromSelection(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_topointer(L,1);
    if node<>nil then
      tv.RemoveFromSelection(node);
  end;
end;


function VirtualStringTree_getNodeParent(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
  nodeparent: PVirtualNode;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_topointer(L,1);
    if node<>nil then
    begin
      nodeparent:=tv.NodeParent[node];
      if nodeparent=nil then
        lua_pushnil(L)
      else
        lua_pushlightuserdata(L, nodeparent);
      exit(1);
    end;
  end;
end;

function VirtualStringTree_setNodeParent(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node, parent: PVirtualNode;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_topointer(L,1);
    parent:=lua_topointer(L,2);
    if node<>nil then
      tv.NodeParent[node]:=parent;
  end;
end;

function VirtualStringTree_getNodeHeight(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_topointer(L,1);
    if node<>nil then
    begin
      lua_pushinteger(L, tv.NodeHeight[node]);
      exit(1);
    end;
  end;
end;

function VirtualStringTree_setNodeHeight(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
  height: integer;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_topointer(L,1);
    height:=lua_tointeger(L,2);
    if node<>nil then
      tv.NodeHeight[node]:=height;
  end;
end;

function VirtualStringTree_getHasChildren(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_topointer(L,1);
    if node<>nil then
    begin
      lua_pushboolean(L, tv.HasChildren[node]);
      exit(1);
    end;
  end;
end;

function VirtualStringTree_setHasChildren(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
  Children: boolean;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_topointer(L,1);
    Children:=lua_toboolean(L,2);
    if node<>nil then
      tv.HasChildren[node]:=Children;
  end;
end;

function VirtualStringTree_getSelected(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_topointer(L,1);
    if node<>nil then
    begin
      lua_pushboolean(L, tv.Selected[node]);
      exit(1);
    end;
  end;
end;

function VirtualStringTree_setSelected(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
  Selected: boolean;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_topointer(L,1);
    Selected:=lua_toboolean(L,2);
    if node<>nil then
      tv.Selected[node]:=Selected;
  end;
end;

function VirtualStringTree_getChecked(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_topointer(L,1);
    if node<>nil then
    begin
      lua_pushboolean(L, csCheckedNormal=tv.CheckState[node]);
      exit(1);
    end;
  end;
end;

function VirtualStringTree_setChecked(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
  state: boolean;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_topointer(L,1);
    state:=lua_toboolean(L,2);
    if node<>nil then
    begin
      if state then
        tv.CheckState[node]:=csCheckedNormal
      else
        tv.CheckState[node]:=csUncheckedNormal;
    end;
  end;
end;

function VirtualStringTree_getExpanded(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_topointer(L,1);
    if node<>nil then
    begin
      lua_pushboolean(L, tv.Expanded[node]);
      exit(1);
    end;
  end;
end;

function VirtualStringTree_setExpanded(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
  Expanded: boolean;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_topointer(L,1);
    Expanded:=lua_toboolean(L,2);
    if node<>nil then
      tv.Expanded[node]:=Expanded;
  end;
end;

function VirtualStringTree_enumSelectedNodes(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
  e: TVTVirtualNodeEnumeration;
  en: TVTVirtualNodeEnumerator;

  i: integer;
begin
  result:=1;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  e:=tv.SelectedNodes;
  en:=e.GetEnumerator;
  lua_newtable(L);
  result:=1;

  i:=1;
  while en.MoveNext do
  begin
    lua_pushinteger(L,i);
    lua_pushlightuserdata(L, en.current);
    lua_settable(L,-3);

    inc(i);
  end;

  en.free;
end;


function VirtualStringTree_enumCheckedNodes(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
  e: TVTVirtualNodeEnumeration;
  en: TVTVirtualNodeEnumerator;

  i: integer;
begin
  result:=1;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  e:=tv.CheckedNodes;
  en:=e.GetEnumerator;
  lua_newtable(L);
  result:=1;

  i:=1;
  while en.MoveNext do
  begin
    lua_pushinteger(L,i);
    lua_pushlightuserdata(L, en.current);
    lua_settable(L,-3);

    inc(i);
  end;

  en.free;
end;


function VirtualStringTree_getNodeDataAsInteger(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
  p: pointer;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));

  if lua_gettop(L)>=1 then
  begin
    node:=lua_toPointer(L, 1);
    p:=tv.GetNodeData(node);
    if p<>nil then
    begin
      if tv.NodeDataSize>=8 then
        lua_pushinteger(L, pqword(p)^)
      else
      if tv.NodeDataSize>=4 then
        lua_pushinteger(L, pdword(p)^)
      else
      if tv.NodeDataSize>=2 then
        lua_pushinteger(L, pword(p)^)
      else
      if tv.NodeDataSize>=1 then
        lua_pushinteger(L, pbyte(p)^)
    end
    else
    begin
      lua_pushnil(L);
      lua_pushstring(L,'This tree has no datasize assigned for nodes');
      exit(2);
    end;
    exit(1);
  end;
end;

function VirtualStringTree_setNodeDataAsInteger(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
  v: qword;
  p: pointer;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));

  if (lua_gettop(L)>=2) then
  begin
    node:=lua_toPointer(L, 1);
    v:=lua_tointeger(L,2);
    p:=tv.GetNodeData(node);

    if p<>nil then
    begin
      if tv.NodeDataSize>=8 then
        pqword(p)^:=v
      else
      if tv.NodeDataSize>=4 then
        pdword(p)^:=v
      else
      if tv.NodeDataSize>=2 then
        pword(p)^:=v
      else
      if tv.NodeDataSize>=1 then
        pbyte(p)^:=v
    end
    else
    begin
      lua_pushnil(L);
      lua_pushstring(L,'This tree has no datasize assigned for nodes');
      exit(2);
    end;
    lua_pushboolean(L,true);
    exit(1);
  end;
end;


function VirtualStringTree_getNodeDataPointer(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_toPointer(L, 1);
    lua_pushlightuserdata(L, tv.GetNodeData(node));
    exit(1);
  end;
end;

function VirtualStringTree_getRootNode(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
begin
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  lua_pushlightuserdata(L, tv.RootNode);
  result:=1;
end;

function VirtualStringTree_getNodeData(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_toPointer(L, 1);
    CreateByteTableFromPointer(L, tv.GetNodeData(node), tv.NodeDataSize);
    exit(1);
  end;
end;

function VirtualStringTree_setNodeData(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=2 then
  begin
    node:=lua_toPointer(L, 1);
    readBytesFromTable(L, 2, tv.GetNodeData(node), tv.NodeDataSize);
  end;
end;

function virtualstringtree_setFullRowSelect(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    if lua_toboolean(L,1) then
      tv.TreeOptions.SelectionOptions:=tv.TreeOptions.SelectionOptions+[toFullRowSelect]
    else
      tv.TreeOptions.SelectionOptions:=tv.TreeOptions.SelectionOptions-[toFullRowSelect]
  end;
end;

function virtualstringtree_getFullRowSelect(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
begin
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  lua_pushboolean(L, toFullRowSelect in tv.TreeOptions.SelectionOptions);
  result:=1;
end;

function virtualstringtree_setFocusedNode(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
begin
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_Gettop(L)>=1 then
  begin
    node:=lua_topointer(L,1);
    tv.FocusedNode:=node;
  end;
  result:=0;
end;

function virtualstringtree_getFocusedNode(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
begin
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  node:=tv.FocusedNode;
  if node=nil then
    lua_pushnil(L)
  else
    lua_pushlightuserdata(L,node);

  result:=1;
end;

function virtualstringtree_setFocusedColumn(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  c: integer;
begin
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_Gettop(L)>=1 then
  begin
    c:=lua_tointeger(L,1);
    tv.FocusedColumn:=c;
  end;
  result:=0;
end;

function virtualstringtree_getFocusedColumn(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  c: integer;
begin
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  c:=tv.FocusedColumn;
  lua_pushinteger(L,c);
  result:=1;
end;

function VirtualStringTree_getFirstChild(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
  n: PVirtualNode;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_toPointer(L, 1);
    n:=tv.GetFirstChild(node);

    if n=nil then
      lua_pushnil(L)
    else
      lua_pushlightuserdata(L, n);

    result:=1;
  end;

end;

function VirtualStringTree_getNextSibling(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
  n: PVirtualNode;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_toPointer(L, 1);
    n:=tv.GetNextSibling(node);
    if n=nil then
      lua_pushnil(L)
    else
      lua_pushlightuserdata(L, n);

    result:=1;
  end;
end;

function VirtualStringTree_saveToFile(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  fname: string;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    fname:=Lua_ToString(L, 1);
    try
      tv.SaveToFile(fname);
      lua_pushboolean(L,true);
      exit(1);
    except
      on e: exception do
      begin
        lua_pushboolean(L,false);
        lua_pushstring(L,e.message);
        exit(2);
      end;
    end;
  end;
end;

function VirtualStringTree_loadFromFile(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  fname: string;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    fname:=Lua_ToString(L, 1);
    try
      tv.LoadFromFile(fname);
      lua_pushboolean(L,true);
      exit(1);
    except
      on e: exception do
      begin
        lua_pushboolean(L,false);
        lua_pushstring(L,e.message);
        exit(2);
      end;
    end;
  end;
end;

function VirtualStringTree_absoluteIndex(L: Plua_State): integer; cdecl;
var
  tv: TLazVirtualStringTree;
  node: PVirtualNode;
begin
  result:=0;
  tv:=TLazVirtualStringTree(luaclass_getClassObject(L));
  if lua_gettop(L)>=1 then
  begin
    node:=lua_toPointer(L, 1);
    lua_pushinteger(L, tv.AbsoluteIndex(node));
    result:=1;
  end;
end;

procedure virtualstringtree_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clear', VirtualStringTree_clear);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'beginUpdate', VirtualStringTree_beginUpdate);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'endUpdate', VirtualStringTree_endUpdate);


  luaclass_addClassFunctionToTable(L, metatable, userdata, 'addChild', VirtualStringTree_addChild);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'addToSelection', VirtualStringTree_addToSelection);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'removeFromSelection', VirtualStringTree_removeFromSelection);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getNodeParent', VirtualStringTree_getNodeParent);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'enumSelectedNodes', VirtualStringTree_enumSelectedNodes);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'enumCheckedNodes', VirtualStringTree_enumCheckedNodes);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getNodeDataAsInteger', VirtualStringTree_getNodeDataAsInteger);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setNodeDataAsInteger', VirtualStringTree_setNodeDataAsInteger);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getNodeData', VirtualStringTree_getNodeData);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setNodeData', VirtualStringTree_setNodeData);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getNodeDataPointer', VirtualStringTree_getNodeDataPointer);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'deleteNode', VirtualStringTree_deleteNode);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'deleteSelectedNodes', VirtualStringTree_deleteSelectedNodes);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getRootNode', VirtualStringTree_getRootNode);


  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getFirstChild', VirtualStringTree_getFirstChild);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getNextSibling', VirtualStringTree_getNextSibling);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'saveToFile', VirtualStringTree_saveToFile);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'loadFromFile', VirtualStringTree_loadFromFile);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'absoluteIndex', VirtualStringTree_absoluteIndex);




  luaclass_addPropertyToTable(L, metatable, userdata,'FullRowSelect', virtualstringtree_getFullRowSelect, virtualstringtree_setFullRowSelect);
  luaclass_addPropertyToTable(L, metatable, userdata,'FocusedNode', virtualstringtree_getFocusedNode, virtualstringtree_setFocusedNode);
  luaclass_addPropertyToTable(L, metatable, userdata,'FocusedColumn', virtualstringtree_getFocusedColumn, virtualstringtree_setFocusedColumn);


  luaclass_addArrayPropertyToTable(L, metatable, userdata,'NodeParent', VirtualStringTree_getNodeParent, VirtualStringTree_setNodeParent);
  luaclass_addArrayPropertyToTable(L, metatable, userdata,'NodeHeight', VirtualStringTree_getNodeHeight, VirtualStringTree_setNodeHeight);
  luaclass_addArrayPropertyToTable(L, metatable, userdata,'HasChildren', VirtualStringTree_getHasChildren, VirtualStringTree_setHasChildren);
  luaclass_addArrayPropertyToTable(L, metatable, userdata,'Selected', VirtualStringTree_getSelected, VirtualStringTree_setSelected);
  luaclass_addArrayPropertyToTable(L, metatable, userdata,'Checked', VirtualStringTree_getChecked, VirtualStringTree_setChecked);
  luaclass_addArrayPropertyToTable(L, metatable, userdata,'Expanded', VirtualStringTree_getExpanded, VirtualStringTree_setExpanded);



end;

function VirtualTreeColumn_getVisible(L: Plua_State): integer; cdecl;
var
  c: TVirtualTreeColumn;
begin
  c:=TVirtualTreeColumn(luaclass_getClassObject(L));
  lua_pushboolean(L,coVisible in c.Options);
  result:=1;
end;

function VirtualTreeColumn_setVisible(L: Plua_State): integer; cdecl;
var
  c: TVirtualTreeColumn;
begin
  result:=0;
  c:=TVirtualTreeColumn(luaclass_getClassObject(L));
  if lua_gettop(L)=1 then
  begin
    if lua_toboolean(L,1) then
      c.options:=c.options+[coVisible]
    else
      c.options:=c.options-[coVisible];
  end;
end;


procedure VirtualTreeColumn_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addPropertyToTable(L, metatable, userdata,'Visible', VirtualTreeColumn_getVisible, VirtualTreeColumn_setVisible);
end;

function VirtualTreeColumns_add(L: Plua_State): integer; cdecl;
var
  columns: TVirtualTreeColumns;
  c: TVirtualTreeColumn;
begin
  columns:=TVirtualTreeColumns(luaclass_getClassObject(L));
  columns.Header.Options:=columns.Header.Options+[hoVisible];
  columns:=luaclass_getClassObject(L);
  c:=columns.Add;
  luaclass_newClass(L, c);

  if lua_gettop(L)>=1 then
    c.Text:=Lua_ToString(L,1);


  exit(1);
end;

procedure VirtualTreeColumns_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  collection_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'add', VirtualTreeColumns_add);
end;


function vtheader_getautoresize(L: Plua_State): integer; cdecl;
var
  h: TVTHeader;
begin
  h:=TVTHeader(luaclass_getClassObject(L));
  lua_pushboolean(L,hoAutoResize in h.Options);
  result:=1;
end;

function vtheader_setautoresize(L: Plua_State): integer; cdecl;
var
  h: TVTHeader;
begin
  result:=0;
  h:=TVTHeader(luaclass_getClassObject(L));
  if lua_gettop(L)=1 then
  begin
    if lua_toboolean(L,1) then
      h.options:=h.options+[hoAutoResize]
    else
      h.options:=h.options-[hoAutoResize];
  end;
end;


procedure VTHeader_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addPropertyToTable(L, metatable, userdata,'AutoResize', vtheader_getautoresize, vtheader_setautoresize);
end;


procedure initializeLuaVirtualStringTree;
begin
  lua_register(LuaVM, 'createVirtualStringTree', createVirtualStringTree);
end;



initialization
   luaclass_register(TVirtualStringTree,  virtualstringtree_addMetaData);
   luaclass_register(TVirtualTreeColumns, VirtualTreeColumns_addMetaData);
   luaclass_register(TVirtualTreeColumn,  VirtualTreeColumn_addMetaData);
   luaclass_register(TVTHeader,  VTHeader_addMetaData);

end.

