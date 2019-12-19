unit LuaTreeview;

{$mode delphi}

interface

uses
  Classes, SysUtils, ComCtrls, lua, luaclass, Controls, LuaWinControl;

procedure initializeLuaTreeview;

implementation

uses LuaHandler, ceguicomponents;

function createTreeView(L: Plua_State): integer; cdecl;
var
  Treeview: TCETreeview;
  owner: TWincontrol;
begin
  if lua_gettop(L)=1 then
    owner:=lua_toceuserdata(L, 1)
  else
    owner:=nil;

  Treeview:=TCETreeview.Create(owner);

  if owner<>nil then
    Treeview.Parent:=owner;

  luaclass_newClass(L, Treeview);
  result:=1;

end;

function treeview_getItems(L: PLua_State): integer; cdecl;
var
  Treeview: TCETreeview;
begin
  Treeview:=luaclass_getClassObject(L);
  luaclass_newClass(L, Treeview.Items);
  result:=1;
end;

function treeview_getSelected(L: PLua_State): integer; cdecl;
var
  Treeview: TCETreeview;
begin
  Treeview:=luaclass_getClassObject(L);
  luaclass_newClass(L, Treeview.Selected);
  result:=1;
end;

function treeview_setSelected(L: PLua_State): integer; cdecl;
var
  Treeview: TCETreeview;
begin
  Treeview:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    Treeview.Selected:=lua_ToCEUserData(L, 1);

  result:=0;
end;

function treeview_fullCollapse(L: PLua_State): integer; cdecl;
begin
  TCETreeview(luaclass_getClassObject(L)).FullCollapse;
  result:=0;
end;

function treeview_fullExpand(L: PLua_State): integer; cdecl;
begin
  TCETreeview(luaclass_getClassObject(L)).FullExpand;
  result:=0;
end;

function treeview_beginUpdate(L: PLua_State): integer; cdecl;
begin
  TCETreeview(luaclass_getClassObject(L)).BeginUpdate;
  result:=0;
end;

function treeview_endUpdate(L: PLua_State): integer; cdecl;
begin
  TCETreeview(luaclass_getClassObject(L)).EndUpdate;
  result:=0;
end;


function treeview_saveToFile(L: PLua_State): integer; cdecl;
var
  Treeview: TCETreeview;
  fn: string;
begin
  Treeview:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    treeview.SaveToFile(Lua_ToString(L, 1));

  result:=0;
end;


procedure treeview_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getItems', treeview_getItems);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getSelected', treeview_getSelected);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setSelected', treeview_setSelected);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'fullCollapse', treeview_fullCollapse);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'fullExpand', treeview_fullExpand);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'beginUpdate', treeview_beginUpdate);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'endUpdate', treeview_endUpdate);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'saveToFile', treeview_saveToFile);



  Luaclass_addPropertyToTable(L, metatable, userdata, 'Selected', treeview_getSelected, treeview_setSelected);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Items', treeview_getItems, nil);

end;

procedure initializeLuaTreeview;
begin
  lua_register(LuaVM, 'createTreeView', createTreeView);
  lua_register(LuaVM, 'createTreeview', createTreeView);

end;

initialization
   luaclass_register(TCustomTreeView,  treeview_addMetaData);

end.

