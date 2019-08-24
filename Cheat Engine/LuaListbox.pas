unit LuaListbox;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, stdctrls, controls;

procedure initializeLuaListbox;
procedure listbox_addMetaData(L: PLua_state; metatable: integer; userdata: integer);

implementation

uses luahandler, luaclass, LuaWinControl;

function createListBox(L: Plua_State): integer; cdecl;
var
  ListBox: TListBox;
  parameters: integer;
  owner: TWincontrol;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    owner:=lua_toceuserdata(L, -parameters)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));


  ListBox:=TListBox.Create(owner);
  if owner<>nil then
    ListBox.Parent:=owner;

  luaclass_newClass(L, ListBox);
  result:=1;
end;

function listbox_clear(L: Plua_State): integer; cdecl;
var
  listbox: tcustomlistbox;
begin
  listbox:=luaclass_getClassObject(L);
  listbox.clear;
  result:=0;
end;

function listbox_clearSelection(L: Plua_State): integer; cdecl;
var
  listbox: tcustomlistbox;
begin
  listbox:=luaclass_getClassObject(L);
  listbox.ClearSelection;
  result:=0;
end;

function listbox_selectAll(L: Plua_State): integer; cdecl;
var
  listbox: tcustomlistbox;
begin
  listbox:=luaclass_getClassObject(L);
  listbox.SelectAll;
  result:=0;
end;


function listbox_getItems(L: PLua_State): integer; cdecl;
var
  listbox: TCustomlistbox;
begin
  listbox:=luaclass_getClassObject(L);
  luaclass_newClass(L, listbox.Items);
  result:=1;
end;

function listbox_setItems(L: PLua_State): integer; cdecl;
var
  listbox: TCustomlistbox;
begin
  listbox:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    listbox.Items:=tstrings(lua_ToCEUserData(L, 1));

  result:=1;
end;

function listbox_getItemIndex(L: PLua_State): integer; cdecl;
var
  listbox: TCustomlistbox;
begin
  listbox:=luaclass_getClassObject(L);
  lua_pushinteger(L, listbox.ItemIndex);
  result:=1;

end;

function listbox_setItemIndex(L: PLua_State): integer; cdecl;
var
  listbox: Tcustomlistbox;
begin
  result:=0;
  listbox:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    listbox.itemindex:=lua_tointeger(L,-1);
end;

function listbox_getCanvas(L: PLua_State): integer; cdecl;
var
  listbox: TCustomlistbox;
begin
  listbox:=luaclass_getClassObject(L);
  luaclass_newClass(L, listbox.Canvas);
  result:=1;
end;

function listbox_getSelected(L: PLua_State): integer; cdecl;
var
  listbox: Tcustomlistbox;
  index: integer;
begin
  result:=0;
  listbox:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    index:=lua_toInteger(L, 1);
    lua_pushboolean(L, listbox.Selected[index]);
    result:=1;
  end;
end;

function listbox_setSelected(L: PLua_State): integer; cdecl;
var
  listbox: Tcustomlistbox;
  index: integer;
begin
  result:=0;
  listbox:=luaclass_getClassObject(L);
  if lua_gettop(L)=2 then
  begin
    index:=lua_toInteger(L, 1);
    listbox.selected[index]:=lua_toboolean(L, 2);
  end;
end;

procedure listbox_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clear', listbox_clear);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clearSelection', listbox_clearSelection);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'selectAll', listbox_selectAll);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getItems', listbox_getItems);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getItemIndex', listbox_getItemIndex);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setItemIndex', listbox_setItemIndex);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCanvas', listbox_getCanvas);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Items', listbox_getItems, listbox_setItems);
  luaclass_addPropertyToTable(L, metatable, userdata, 'ItemIndex', listbox_getItemIndex, listbox_setItemIndex);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Canvas', listbox_getCanvas, nil);

  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Selected', listbox_getSelected, listbox_setSelected);
end;

procedure initializeLuaListbox;
begin
  lua_register(LuaVM, 'createListBox', createListBox);
  lua_register(LuaVM, 'listbox_clear', listbox_clear);
  lua_register(LuaVM, 'listbox_getItems', listbox_getItems);
  lua_register(LuaVM, 'listbox_getItemIndex', listbox_getItemIndex);
  lua_register(LuaVM, 'listbox_setItemIndex', listbox_setItemIndex);
  lua_register(LuaVM, 'listbox_getCanvas', listbox_getCanvas);
end;

initialization
  luaclass_register(TCustomListBox, listbox_addMetaData);

end.

