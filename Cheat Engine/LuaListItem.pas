unit LuaListItem;

{$mode delphi}

interface

uses
  Classes, SysUtils, ComCtrls, lua, lualib, lauxlib, typinfo;

procedure initializeLuaListItem;

implementation

uses luaclass, luahandler, LuaObject;

function listitem_delete(L: Plua_State): integer; cdecl;
var
  listitem: Tlistitem;
begin
  result:=0;
  listitem:=luaclass_getClassObject(L);
  listitem.Delete;
end;

function listitem_getIndex(L: PLua_State): integer; cdecl;
var
  listitem: Tlistitem;
begin
  listitem:=luaclass_getClassObject(L);
  lua_pushvariant(L, listitem.Index);
  result:=1;
end;

function listitem_getSelected(L: PLua_State): integer; cdecl;
var
  listitem: Tlistitem;
begin
  listitem:=luaclass_getClassObject(L);
  lua_pushvariant(L, listitem.Selected);
  result:=1;
end;

function listitem_setSelected(L: PLua_State): integer; cdecl;
var
  listitem: Tlistitem;
begin
  listitem:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    listitem.Selected:=lua_tovariant(L, -1);

  result:=0;
end;

function listitem_getData(L: PLua_State): integer; cdecl;
var
  listitem: Tlistitem;
begin
  listitem:=luaclass_getClassObject(L);
  lua_pushinteger(L, ptruint(listitem.Data));
  result:=1;
end;

function listitem_setData(L: PLua_State): integer; cdecl;
var
  listitem: Tlistitem;
begin
  listitem:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    listitem.Data:=pointer(lua_tointeger(L, -1));

  result:=0;
end;

function listitem_getOwner(L: PLua_State): integer; cdecl;
var
  listitem: Tlistitem;
begin
  listitem:=luaclass_getClassObject(L);
  luaclass_newClass(L, listitem.Owner);
  result:=1;
end;

function listitem_getCaption(L: PLua_State): integer; cdecl;
var
  listitem: Tlistitem;
begin
  listitem:=luaclass_getClassObject(L);
  lua_pushvariant(L, listitem.Caption);
  result:=1;
end;

function listitem_setCaption(L: PLua_State): integer; cdecl;
var
  listitem: Tlistitem;
begin
  listitem:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    listitem.Caption:=lua_tovariant(L, -1);

  result:=0;
end;

function listitem_getChecked(L: PLua_State): integer; cdecl;
var
  listitem: Tlistitem;
begin
  listitem:=luaclass_getClassObject(L);
  lua_pushvariant(L, listitem.Checked);
  result:=1;
end;

function listitem_setChecked(L: PLua_State): integer; cdecl;
var
  listitem: Tlistitem;
begin
  listitem:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    listitem.Checked:=lua_tovariant(L, -1);

  result:=0;
end;

function listitem_setSubItems(L: PLua_State): integer; cdecl;
var
  listitem: Tlistitem;
begin
  listitem:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    listitem.SubItems:=lua_ToCEUserData(L, 1);

  result:=1;
end;

function listitem_getSubItems(L: PLua_State): integer; cdecl;
var
  listitem: Tlistitem;
begin
  listitem:=luaclass_getClassObject(L);
  luaclass_newClass(L, listitem.SubItems);
  result:=1;
end;

function listitem_displayRect(L: PLua_State): integer; cdecl;
var
  listitem: Tlistitem;
  r:trect;
begin
  result:=0;
  listitem:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    if lua_isnumber(L,2) then
      r:=listitem.displayrect(TDisplayCode(lua_tointeger(L,1)))
    else
      r:=listitem.displayrect(TDisplayCode(GetEnumValue(typeinfo(TDisplayCode),Lua_ToString(L,1))));

    lua_pushrect(L,r);
    result:=1;
  end;
end;

function listitem_displayRectSubItem(L: PLua_State): integer; cdecl;
var
  listitem: Tlistitem;
  r:trect;
begin
  result:=0;
  listitem:=luaclass_getClassObject(L);
  if lua_gettop(L)=2 then
  begin
    if lua_isnumber(L,2) then
      r:=listitem.DisplayRectSubItem(lua_tointeger(L,1), TDisplayCode(lua_tointeger(L,2)))
    else
      r:=listitem.displayrectSubItem(lua_tointeger(L,1), TDisplayCode(GetEnumValue(typeinfo(TDisplayCode),Lua_ToString(L,2))));

    lua_pushrect(L,r);
    result:=1;
  end;
end;

function listitem_makeVisible(L: PLua_State): integer; cdecl;
var
  listitem: Tlistitem;
  partialok: boolean=false;
begin
  listitem:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    partialok:=lua_toboolean(L, 1);

  listitem.MakeVisible(partialok);

  result:=0;
end;

procedure listitem_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCaption', listitem_getCaption);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setCaption', listitem_setCaption);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getChecked', listitem_getChecked);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setChecked', listitem_setChecked);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setSubItems', listitem_setSubItems);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getSubItems', listitem_getSubItems);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'makeVisible', listitem_makeVisible);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'delete', listitem_delete);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'displayRect', listitem_displayRect);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'displayRectSubItem', listitem_displayRectSubItem);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Caption', listitem_getCaption, listitem_setCaption);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Checked', listitem_getChecked, listitem_setChecked);
  luaclass_addPropertyToTable(L, metatable, userdata, 'SubItems', listitem_getSubItems, listitem_setSubItems);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Index', listitem_getIndex, nil);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Selected', listitem_getSelected, listitem_setSelected);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Owner', listitem_getOwner, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Data', listitem_getData, listitem_setData);

end;

procedure initializeLuaListItem;
begin
  lua_register(LuaVM, 'listitem_delete', listitem_delete);
  lua_register(LuaVM, 'listitem_getCaption', listitem_getCaption);
  lua_register(LuaVM, 'listitem_setCaption', listitem_setCaption);
  lua_register(LuaVM, 'listitem_getChecked', listitem_getChecked);
  lua_register(LuaVM, 'listitem_setChecked', listitem_setChecked);
  lua_register(LuaVM, 'listitem_getSubItems', listitem_getSubItems);
end;

initialization
   luaclass_register(TListItem,  listitem_addMetaData);

end.

