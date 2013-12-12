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

{$ifdef cpu32}
  if treenode.data<>nil then
    freemem(treenode.data);
{$endif}

  treenode.Delete;
end;

function treenode_expand(L: Plua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  result:=0;
  treenode:=luaclass_getClassObject(L);
  treenode.Expand(true);
end;

function treenode_collapse(L: Plua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  result:=0;
  treenode:=luaclass_getClassObject(L);
  treenode.Collapse(true);
end;

function treenode_getIndex(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  lua_pushvariant(L, treenode.Index);
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
    treenode.Text:=lua_tovariant(L, -1);

  result:=0;
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

{$ifdef cpu32}
  if treenode.data=nil then
    lua_pushinteger(L, 0)
  else
  begin
    lua_pushinteger(L, pqword(treenode.data)^);
  end;
{$else}
  lua_pushinteger(L, UIntPtr(treenode.Data));
{$endif}
  result:=1;
end;

function treenode_setData(L: PLua_State): integer; cdecl;
var
  treenode: Ttreenode;
begin
  treenode:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin

{$ifdef cpu32}
    if lua_isnil(L, 1) then
    begin
      //free the data
      freemem(treenode.data);
      treenode.data:=nil;
    end
    else
    begin
      if treenode.data=nil then
        treenode.data:=getmem(8); //enough room for a 64-bit value

      pqword(treenode.data)^:=lua_tointeger(L, 1);
    end;
{$else}
    treenode.data:=pointer(lua_tointeger(L, 1));
{$endif}
  end;

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

  luaclass_addPropertyToTable(L, metatable, userdata, 'Data', treenode_getData, treenode_setData);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Text', treenode_getText, treenode_setText);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Index', treenode_getIndex, nil);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'AbsoluteIndex', treenode_getIndex, nil);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Selected', treenode_getSelected, treenode_setSelected);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'MultiSelected', treenode_getMultiSelected, treenode_setMultiSelected);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Parent', treenode_getParent, nil);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'HasChildren', treenode_getHasChildren, treenode_setHasChildren);

end;

initialization
   luaclass_register(TTreenode,  treenode_addMetaData);

end.

