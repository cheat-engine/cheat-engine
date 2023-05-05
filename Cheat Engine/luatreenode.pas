unit LuaTreeNode;

{$mode delphi}

interface

uses
  Classes, SysUtils, ComCtrls, lua, lualib, lauxlib;

implementation

uses luaclass, luahandler, LuaObject;


function treenode_delete(L: Plua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  result:=0;
  treenode:=luaclass_getClassObject(L);

  treenode.Delete;
end;



function treenode_deleteChildren(L: Plua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  result:=0;
  treenode:=luaclass_getClassObject(L);
  treenode.DeleteChildren;
end;

function treenode_expand(L: Plua_State): integer; cdecl;
var
  treenode: Ttreenode;
  recursive: boolean;
begin
  result:=0;
  treenode:=luaclass_getClassObject(L);

  if lua_gettop(L)>0 then
    recursive:=lua_toboolean(L,1)
  else
    recursive:=true;

  treenode.Expand(recursive);
end;

function treenode_collapse(L: Plua_State): integer; cdecl;
var
  treenode: Ttreenode;
  recursive: boolean;
begin
  result:=0;
  treenode:=luaclass_getClassObject(L);

  if lua_gettop(L)>0 then
    recursive:=lua_toboolean(L,1)
  else
    recursive:=true;

  treenode.Collapse(recursive);
end;

function treenode_getItems(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
  index: integer;
begin
  result:=0;
  treenode:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    index:=lua_tointeger(L,-1);
    luaclass_newClass(L, treenode.Items[index]);
    result:=1;
  end;
end;

function treenode_getCount(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  lua_pushvariant(L, treenode.Count);
  result:=1;
end;

function treenode_getIndex(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  lua_pushvariant(L, treenode.Index);
  result:=1;
end;

function treenode_setIndex(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    treenode.Index:=lua_tointeger(L,1);

  result:=1;
end;

function treenode_getLevel(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  lua_pushvariant(L, treenode.Level);
  result:=1;
end;

function treenode_getAbsoluteIndex(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  lua_pushvariant(L, treenode.AbsoluteIndex);
  result:=1;
end;

function treenode_getMultiSelected(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin

  treenode:=luaclass_getClassObject(L);
  lua_pushvariant(L, treenode.MultiSelected);
  result:=1;
end;

function treenode_setMultiSelected(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    treenode.MultiSelected:=lua_tovariant(L, -1);

  result:=0;
end;

function treenode_getSelected(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin

  treenode:=luaclass_getClassObject(L);
  lua_pushvariant(L, treenode.Selected);
  result:=1;
end;

function treenode_setSelected(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    treenode.Selected:=lua_tovariant(L, -1);

  result:=0;
end;

function treenode_getHasChildren(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin

  treenode:=luaclass_getClassObject(L);
  lua_pushvariant(L, treenode.HasChildren);
  result:=1;
end;

function treenode_setHasChildren(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    treenode.HasChildren:=lua_tovariant(L, -1);

  result:=0;
end;

function treenode_getExpanded(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin

  treenode:=luaclass_getClassObject(L);
  lua_pushvariant(L, treenode.Expanded);
  result:=1;
end;

function treenode_setExpanded(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    treenode.Expanded:=lua_toboolean(L, 1);

  result:=0;
end;

function treenode_getVisible(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin

  treenode:=luaclass_getClassObject(L);
  lua_pushvariant(L, treenode.Visible);
  result:=1;
end;

function treenode_setVisible(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    treenode.visible:=lua_toboolean(L, 1);

  result:=0;
end;

function treenode_getParent(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  luaclass_newClass(L, treenode.Parent);
  result:=1;
end;

function treenode_getText(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  lua_pushvariant(L, treenode.Text);
  result:=1;
end;

function treenode_setText(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    treenode.Text:=Lua_ToString(L, 1);

  result:=0;
end;

function treenode_getDisplayRect(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
  textonly: boolean=false;
  r: trect;
begin
  treenode:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    textonly:=lua_toboolean(L,1);

  r:=treenode.DisplayRect(TextOnly);
  lua_pushrect(L,r);
  result:=1;
end;

function treenode_makeVisible(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
  partialok: boolean=false;
begin
  treenode:=luaclass_getClassObject(L);
  treenode.MakeVisible;

  result:=0;
end;

function treenode_getData(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  lua_pushinteger(L, UIntPtr(treenode.Data));
  result:=1;
end;

function treenode_setData(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    treenode.data:=pointer(lua_tointeger(L, 1));

  result:=0;
end;

function treenode_getImageIndex(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  lua_pushinteger(L, treenode.ImageIndex);
  result:=1;
end;

function treenode_setImageIndex(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    treenode.ImageIndex:=lua_tointeger(L, 1);

  result:=0;
end;

function treenode_add(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
  treenodes: Ttreenodes;
  paramcount: integer;
  s: string;
begin
  treenode:=luaclass_getClassObject(L);
  treenodes:=treenode.Owner;

  paramcount:=lua_gettop(L);
  if paramcount>=1 then
    s:=Lua_ToString(L, 1)
  else
    s:='';

  luaclass_newClass(L, treenodes.AddChild(treenode, s));
  result:=1;
end;

function treenode_getNextSibling(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  luaclass_newClass(L, treenode.GetNextSibling);
  result:=1;
end;

procedure treenode_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'add', treenode_add);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getText', treenode_getText);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setText', treenode_setText);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'makeVisible', treenode_makeVisible);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'delete', treenode_delete);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'expand', treenode_expand);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'collapse', treenode_collapse);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'deleteChildren', treenode_deleteChildren);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getNextSibling', treenode_getNextSibling);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getItems', treenode_getItems);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getDisplayRect', treenode_getDisplayRect);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Data', treenode_getData, treenode_setData);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Text', treenode_getText, treenode_setText);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Index', treenode_getIndex, nil);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Level', treenode_getLevel, nil);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Count', treenode_getCount, nil);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'AbsoluteIndex', treenode_getAbsoluteIndex, nil);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'ImageIndex', treenode_getImageIndex, treenode_setImageIndex);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Selected', treenode_getSelected, treenode_setSelected);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'MultiSelected', treenode_getMultiSelected, treenode_setMultiSelected);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Parent', treenode_getParent, nil);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'HasChildren', treenode_getHasChildren, treenode_setHasChildren);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Expanded', treenode_getExpanded, treenode_setExpanded);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Visible', treenode_getVisible, treenode_setVisible);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Items', treenode_getItems);
  luaclass_setDefaultArrayProperty(L, metatable, userdata, treenode_getItems, nil);

end;

initialization
   luaclass_register(TTreenode,  treenode_addMetaData);

end.

