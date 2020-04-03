unit LuaListview;

{$mode delphi}

interface

uses
  Classes, SysUtils, controls, comctrls, lua, lualib, lauxlib;

procedure initializeLuaListview;

implementation

uses luaclass, luahandler, LuaWinControl, ceguicomponents;

function createListView(L: Plua_State): integer; cdecl;
var
  ListView: TCEListView;
  owner: TWincontrol;
begin
  if lua_gettop(L)=1 then
    owner:=lua_toceuserdata(L, 1)
  else
    owner:=nil;

  ListView:=TCEListView.Create(owner);
  ListView.ViewStyle:=vsReport;
  if owner<>nil then
    ListView.Parent:=owner;

  luaclass_newClass(L, ListView);
  result:=1;
end;

function listview_clear(L: Plua_State): integer; cdecl;
var
  listview: TcustomListView;
begin
  result:=0;
  listview:=luaclass_getClassObject(L);
  listview.clear;
end;

function listview_beginupdate(L: Plua_State): integer; cdecl;
var
  listview: TcustomListView;
begin
  result:=0;
  listview:=luaclass_getClassObject(L);
  listview.BeginUpdate;
end;

function listview_endupdate(L: Plua_State): integer; cdecl;
var
  listview: TcustomListView;
begin
  result:=0;
  listview:=luaclass_getClassObject(L);
  listview.EndUpdate;
end;

function listview_getColumns(L: PLua_State): integer; cdecl;
var
  o: Tobject;
  c: TListColumns;
  x: TCustomListView;
begin
  //Columns is a protected property, but defined in both celistview and standard listview
  o:=luaclass_getClassObject(L);
  c:=nil;
  if (o is TListview) then
    c:=Tlistview(o).Columns
  else
  if (o is TCEListView) then
    c:=Tcelistview(o).Columns;

  luaclass_newClass(L, c);
  result:=1;
end;

function listview_getItemAt(L: PLua_State): integer; cdecl;
var
  listview: TCustomListView;
  x,y: integer;

  i: TListItem;
begin
  result:=0;
  if lua_gettop(L)>=2 then
  begin
    listview:=luaclass_getClassObject(L);
    x:=lua_tointeger(L,1);
    y:=lua_tointeger(L,2);
    i:=listview.GetItemAt(x,y);
    luaclass_newClass(L, i);
    result:=1;
  end;
end;


function listview_setItems(L: PLua_State): integer; cdecl;
var
  listview: TCustomListView;
begin
  listview:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    Listview.Items:=lua_ToCEUserData(L, 1);
  result:=0;
end;

function listview_getItems(L: PLua_State): integer; cdecl;
var
  listview: TCustomListView;
begin
  listview:=luaclass_getClassObject(L);
  luaclass_newClass(L, Listview.Items);
  result:=1;
end;

function listview_getItemIndex(L: PLua_State): integer; cdecl;
var
  listview: TCustomListView;
begin
  listview:=luaclass_getClassObject(L);
  lua_pushinteger(L, listview.ItemIndex);
  result:=1;
end;

function listview_setItemIndex(L: PLua_State): integer; cdecl;
var
  listview: TCustomListView;
begin
  listview:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    listview.ItemIndex:=lua_tointeger(L, -1);

  result:=0;
end;

function listview_getSelected(L: PLua_State): integer; cdecl;
var
  listview: TCustomListView;
begin
  listview:=luaclass_getClassObject(L);
  luaclass_newClass(L, listview.Selected);
  result:=1;
end;

function listview_setSelected(L: PLua_State): integer; cdecl;
var
  listview: TCustomListView;
begin
  listview:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    listview.Selected:=lua_ToCEUserData(L, 1);


  result:=0;
end;

function listView_getCanvas(L: PLua_State): integer; cdecl;
var
  listview: TCustomListView;
begin
  listview:=luaclass_getClassObject(L);
  luaclass_newClass(L, Listview.Canvas);
  result:=1;
end;


procedure listview_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);


  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clear', listview_clear);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getColumns', listview_getColumns);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getItemAt', listview_getItemAt);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getItems', listview_getItems);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setItems', listview_setItems);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getItemIndex', listview_getItemIndex);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setItemIndex', listview_setItemIndex);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCanvas', listview_getCanvas);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'beginUpdate', listview_beginUpdate);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'endUpdate', listview_endUpdate);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Columns', listview_getColumns, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Items', listview_getItems, listview_setItems);
  luaclass_addPropertyToTable(L, metatable, userdata, 'ItemIndex', listview_getItemIndex, listview_setItemIndex);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Selected', listview_getSelected, listview_setSelected);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Canvas', listview_getCanvas, nil);


end;

procedure initializeLuaListview;
begin
  lua_register(LuaVM, 'createListView', createListView);
  lua_register(LuaVM, 'createListview', createListView);
  lua_register(LuaVM, 'listview_clear', listview_clear);
  lua_register(LuaVM, 'listview_getColumns', listview_getColumns);
  lua_register(LuaVM, 'listview_getItems', listview_getItems);
  lua_register(LuaVM, 'listview_getItemIndex', listview_getItemIndex);
  lua_register(LuaVM, 'listview_setItemIndex', listview_setItemIndex);
  lua_register(LuaVM, 'listview_getCanvas', listview_getCanvas);
end;

initialization
   luaclass_register(TCustomListView,  listview_addMetaData);

end.

