unit LuaCombobox;

{$mode delphi}

interface

uses
  {$ifdef windows}
  windows,
  {$endif}
  Classes, SysUtils, lua, lualib, lauxlib, controls, StdCtrls, ExtCtrls, LuaWinControl, betterControls;

procedure initializeLuaCombobox;

implementation

uses luahandler, luaclass;
//combobox
function createComboBox(L: Plua_State): integer; cdecl;
var
  ComboBox: TComboBox;
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


  ComboBox:=TComboBox.Create(owner);
  if owner<>nil then
    ComboBox.Parent:=owner;

  luaclass_newClass(L, ComboBox);
  result:=1;
end;

function combobox_clear(L: Plua_State): integer; cdecl;
var
  combobox: tcustomcombobox;
begin
  combobox:=luaclass_getClassObject(L);
  combobox.clear;
  result:=0;
end;


function combobox_getItems(L: PLua_State): integer; cdecl;
var
  combobox: TCustomcombobox;
begin
  combobox:=luaclass_getClassObject(L);
  luaclass_newClass(L, combobox.items);
  result:=1;
end;

function combobox_setItems(L: PLua_State): integer; cdecl;
var
  combobox: TCustomcombobox;
begin
  result:=0;
  combobox:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    combobox.Items:=tstrings(lua_ToCEUserData(L, 1));
end;

function combobox_getItemIndex(L: PLua_State): integer; cdecl;
var
  combobox: TCustomcombobox;
begin
  combobox:=luaclass_getClassObject(L);
  lua_pushinteger(L, combobox.itemindex);
  result:=1;
end;

function combobox_setItemIndex(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  combobox: Tcustomcombobox;
  a: integer;
begin
  result:=0;
  combobox:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    combobox.itemindex:=lua_tointeger(L,-1);
end;


function comboBox_getCanvas(L: PLua_State): integer; cdecl;
var
  combobox: TCustomcombobox;
begin
  combobox:=luaclass_getClassObject(L);
  luaclass_newClass(L, combobox.canvas);
  result:=1;
end;

function combobox_getDroppedDown(L: PLua_State): integer; cdecl;
var
  combobox: TCustomcombobox;
begin
  combobox:=luaclass_getClassObject(L);
  lua_pushboolean(L, combobox.DroppedDown);
  result:=1;
end;

function combobox_setDroppedDown(L: PLua_State): integer; cdecl;
var
  combobox: TCustomcombobox;
  state: boolean;
begin
  result:=0;
  combobox:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    combobox.DroppedDown:=lua_toboolean(L,1);
end;

function combobox_getExtraWidth(L: PLua_State): integer; cdecl;
var
  combobox: TCustomcombobox;
  {$ifdef windows}
  cbi: TComboboxInfo;
  {$endif}
  extrasize: integer;
begin
  combobox:=luaclass_getClassObject(L);

  {$ifdef windows}
  zeromemory(@cbi,sizeof(cbi));
  cbi.cbSize:=sizeof(cbi);
  if GetComboBoxInfo(combobox.handle, @cbi) then
    extrasize:=cbi.rcButton.Right-cbi.rcButton.Left+cbi.rcItem.Left
  else
  {$endif}
    extrasize:=16;

  lua_pushinteger(L, extrasize);
  result:=1;
end;

procedure comboBox_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clear', combobox_clear);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setItems', combobox_setItems);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getItems', combobox_getItems);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getItemIndex', combobox_getItemIndex);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setItemIndex', combobox_setItemIndex);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCanvas', combobox_getCanvas);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getExtraWidth', combobox_getExtraWidth);


  luaclass_addPropertyToTable(L, metatable, userdata, 'Items', combobox_getItems, combobox_setItems);
  luaclass_addPropertyToTable(L, metatable, userdata, 'ItemIndex', combobox_getItemIndex, combobox_setItemIndex);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Canvas', combobox_getCanvas, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'DroppedDown', combobox_getDroppedDown, combobox_setDroppedDown);
end;

procedure initializeLuaCombobox;
begin
  lua_register(LuaVM, 'createComboBox', createComboBox);
  lua_register(LuaVM, 'combobox_clear', combobox_clear);
  lua_register(LuaVM, 'combobox_getItems', combobox_getItems);
  lua_register(LuaVM, 'combobox_getItemIndex', combobox_getItemIndex);
  lua_register(LuaVM, 'combobox_setItemIndex', combobox_setItemIndex);
  lua_register(LuaVM, 'combobox_getCanvas', combobox_getCanvas);
end;

initialization
  luaclass_register(tcustomcombobox, combobox_addmetadata);
end.

