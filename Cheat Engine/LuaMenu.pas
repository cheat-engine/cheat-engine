unit LuaMenu;

{$mode delphi}

interface

uses
  Classes, SysUtils,lua, lualib, lauxlib, LuaHandler,menus, LCLProc;

procedure initializeLuaMenu;

implementation

uses LuaCaller, LuaClass, LuaComponent;

function menu_getItems(L: PLua_State): integer; cdecl;
var
  menu: TMenu;
begin
  menu:=luaclass_getClassObject(L);
  luaclass_newClass(L, menu.Items);
  result:=1;
end;




function createMainMenu(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: TComponent;
  m: TMainMenu;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_ToCEUserData(L, 1);
    m:=TMainMenu.Create(f);
    luaclass_newClass(L, m);
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function createPopupMenu(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: TComponent;
  m: TPopupMenu;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_ToCEUserData(L, -1);
    lua_pop(L, lua_gettop(L));

    m:=TPopupMenu.Create(f);
    luaclass_newClass(L, m);
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function createMenuItem(L: Plua_State): integer; cdecl;
var parameters: integer;
  o: TMenu;
  mi: TMenuitem;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    o:=lua_ToCEUserData(L, -1);
    lua_pop(L, lua_gettop(L));

    mi:=TMenuItem.Create(o);
    luaclass_newClass(L, mi);
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function menuItem_getMenuIndex(L: PLua_State): integer; cdecl;
var
  menuItem: TmenuItem;
begin
  menuitem:=luaclass_getClassObject(L);
  lua_pushinteger(L, menuItem.MenuIndex);
  result:=1;
end;

function menuItem_setMenuIndex(L: PLua_State): integer; cdecl;
var
  menuItem: TmenuItem;
  i: integer;
begin
  menuitem:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    i:=lua_tointeger(L, -1);
    menuItem.MenuIndex:=i;
  end;
  result:=0;
end;


function menuItem_getCaption(L: PLua_State): integer; cdecl;
var
  menuItem: TmenuItem;
begin
  menuitem:=luaclass_getClassObject(L);
  lua_pushstring(L, menuItem.Caption);
  result:=1;
end;

function menuItem_setCaption(L: PLua_State): integer; cdecl;
var
  menuItem: TmenuItem;
  Caption: string;
begin
  menuitem:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    Caption:=Lua_ToString(L, -1);
    menuItem.Caption:=Caption;
  end;
  result:=0;
end;

function menuItem_getShortcut(L: PLua_State): integer; cdecl;
var
  menuItem: TmenuItem;
begin
  menuitem:=luaclass_getClassObject(L);
  lua_pushstring(L, ShortCutToText(menuItem.Shortcut));
  result:=1;
end;

function menuItem_setShortcut(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  menuItem: TmenuItem;
  Shortcut: string;
begin
  menuitem:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    Shortcut:=Lua_ToString(L, -1);
    menuItem.Shortcut:=TextToShortCut(shortcut);
  end;
  result:=0;
end;

function menuItem_getCount(L: PLua_State): integer; cdecl;
var
  menuItem: TmenuItem;
begin
  menuitem:=luaclass_getClassObject(L);
  lua_pushinteger(L, menuItem.Count);
  result:=1;
end;

function menuItem_getParent(L: PLua_State): integer; cdecl;
var
  menuItem: TmenuItem;
begin
  menuitem:=luaclass_getClassObject(L);
  luaclass_newClass(L, menuItem.Parent);
  result:=1;
end;

function menuItem_getMenu(L: PLua_State): integer; cdecl;
var
  menuItem: TmenuItem;
begin
  menuitem:=luaclass_getClassObject(L);
  luaclass_newClass(L, menuItem.Menu);
  result:=1;
end;

function menuItem_getItem(L: PLua_State): integer; cdecl;
var
  menuitem: TMenuItem;
  index: integer;
begin
  result:=0;
  menuitem:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    index:=lua_toInteger(L,-1);
    luaclass_newClass(L, menuitem.Items[index]);
    result:=1;
  end;
end;

function menuItem_add(L: PLua_State): integer; cdecl;
var
  menuitem, menuitem2: TMenuItem;
begin
  result:=0;
  menuitem:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    menuitem2:=lua_toceuserdata(L,-1);
    menuitem.Add(menuitem2);
  end;
end;

function menuItem_insert(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  menuitem, menuitem2: TMenuItem;
  index: integer;
begin
  result:=0;
  menuitem:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
  begin
    index:=lua_tointeger(L, -2);
    menuitem2:=lua_ToCEUserData(L,-1);
    menuitem.Insert(index, menuitem2);
  end;
end;

function menuItem_delete(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  menuitem: TMenuItem;
  index: integer;
begin
  result:=0;
  menuitem:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    index:=lua_toInteger(L,-1);
    menuitem.Delete(index);
  end;
end;

function menuItem_clear(L: PLua_State): integer; cdecl;
var
  menuitem: TMenuItem;
begin
  result:=0;
  menuitem:=luaclass_getClassObject(L);
  menuitem.Clear;
end;

function menuitem_getOnClick(L: PLua_State): integer; cdecl;
var
  c: Tmenuitem;
begin
  c:=luaclass_getClassObject(L);
  LuaCaller_pushMethodProperty(L, TMethod(c.OnClick), 'TNotifyEvent');
  result:=1;
end;

function menuitem_setOnClick(L: PLua_State): integer; cdecl; //for some reason the menuitem has it's own fonclick variable
var
  control: Tmenuitem;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  control:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    CleanupLuaCall(tmethod(control.onClick));
    control.onClick:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      control.OnClick:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      control.OnClick:=lc.NotifyEvent;
    end;

  end;
end;

function menuItem_doClick(L: PLua_State): integer; cdecl;
var
  mi: TMenuItem;
begin
  result:=0;
  mi:=luaclass_getClassObject(L);
  if assigned(mi.onclick) then
    mi.OnClick(mi);
end;

procedure menu_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  component_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getItems', menu_getItems);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Items', menu_getItems, nil);
end;

procedure menuitem_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  component_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCaption', menuItem_getCaption);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setCaption', menuItem_setCaption);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getShortcut', menuItem_getShortcut);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setShortcut', menuItem_setShortcut);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCount', menuItem_getCount);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getItem', menuItem_getItem);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'add', menuItem_add);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'insert', menuItem_insert);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'delete', menuItem_delete);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clear', menuItem_clear);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setOnClick', menuItem_setOnClick);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOnClick', menuItem_getOnClick);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'doClick', menuItem_doClick);

  luaclass_addPropertyToTable(L, metatable, userdata, 'MenuIndex', menuItem_getMenuIndex, menuItem_setMenuIndex);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Caption', menuItem_getCaption, menuItem_setCaption);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Shortcut', menuItem_getShortcut, menuItem_setShortcut);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Parent', menuItem_getParent, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Menu', menuItem_getMenu, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Count', menuItem_getCount, nil);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Item', menuItem_getItem);
  luaclass_setDefaultArrayProperty(L, metatable, userdata, menuItem_getItem, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'OnClick', menuItem_getOnClick, menuItem_setOnClick);
end;


procedure initializeLuaMenu;
begin
  lua_register(LuaVM, 'menu_getItems', menu_getItems);
  lua_register(LuaVM, 'createMainMenu', createMainMenu);
  lua_register(LuaVM, 'createPopupMenu', createPopupMenu);
  lua_register(LuaVM, 'createMenuItem', createMenuItem);

  lua_register(LuaVM, 'menuItem_getCaption', menuItem_getCaption);
  lua_register(LuaVM, 'menuItem_setCaption', menuItem_setCaption);
  lua_register(LuaVM, 'menuItem_getShortcut', menuItem_getShortcut);
  lua_register(LuaVM, 'menuItem_setShortcut', menuItem_setShortcut);
  lua_register(LuaVM, 'menuItem_getCount', menuItem_getCount);
  lua_register(LuaVM, 'menuItem_getItem', menuItem_getItem);
  lua_register(LuaVM, 'menuItem_add', menuItem_add);
  lua_register(LuaVM, 'menuItem_insert', menuItem_insert);
  lua_register(LuaVM, 'menuItem_delete', menuItem_delete);
  lua_register(LuaVM, 'menuItem_onClick', menuItem_setOnClick);
  lua_register(LuaVM, 'menuItem_doClick', menuItem_doClick);
end;

initialization
  luaclass_register(TMenu, Menu_addMetaData);
  luaclass_register(TMenuItem, MenuItem_addMetaData);

end.

