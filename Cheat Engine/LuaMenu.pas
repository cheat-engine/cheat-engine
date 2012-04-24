unit LuaMenu;

{$mode delphi}

interface

uses
  Classes, SysUtils,lua, lualib, lauxlib, LuaHandler,menus, LCLProc;

procedure initializeLuaMenu;

implementation

uses LuaCaller;


function menu_getItems(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  menu: TMenu;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    menu:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushlightuserdata(L, menu.Items);
    result:=1;

  end else lua_pop(L, parameters);
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
    f:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(L));

    m:=TMainMenu.Create(f);
    lua_pushlightuserdata(L, m);
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
    f:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(L));

    m:=TPopupMenu.Create(f);
    lua_pushlightuserdata(L, m);
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
    o:=lua_touserdata(L, -1);
    lua_pop(L, lua_gettop(L));

    mi:=TMenuItem.Create(o);
    lua_pushlightuserdata(L, mi);
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function menuItem_getCaption(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  menuItem: TmenuItem;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    menuItem:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushstring(L, menuItem.Caption);
    result:=1;
  end else lua_pop(L, parameters);
end;

function menuItem_setCaption(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  menuItem: TmenuItem;
  Caption: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    menuItem:=lua_touserdata(L,-parameters);
    Caption:=Lua_ToString(L, -parameters+1);
    lua_pop(L, parameters);

    menuItem.Caption:=Caption;
  end else lua_pop(L, parameters);
end;

function menuItem_getShortcut(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  menuItem: TmenuItem;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    menuItem:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushstring(L, ShortCutToText(menuItem.Shortcut));
    result:=1;
  end else lua_pop(L, parameters);
end;

function menuItem_setShortcut(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  menuItem: TmenuItem;
  Shortcut: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    menuItem:=lua_touserdata(L,-parameters);
    Shortcut:=Lua_ToString(L, -parameters+1);
    lua_pop(L, parameters);

    menuItem.Shortcut:=TextToShortCut(shortcut);
  end else lua_pop(L, parameters);
end;

function menuItem_getCount(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  menuItem: TmenuItem;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    menuItem:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, menuItem.Count);
    result:=1;

  end else lua_pop(L, parameters);
end;

function menuItem_getItem(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  menuitem: TMenuItem;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    menuitem:=lua_touserdata(L,-2);
    index:=lua_toInteger(L,-1);
    lua_pop(L, parameters);


    lua_pushlightuserdata(L, menuitem.Items[index]);
    result:=1;

  end else lua_pop(L, parameters);
end;

function menuItem_add(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  menuitem, menuitem2: TMenuItem;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    menuitem:=lua_touserdata(L,-2);
    menuitem2:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    menuitem.Add(menuitem2);
  end else lua_pop(L, parameters);
end;

function menuItem_insert(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  menuitem, menuitem2: TMenuItem;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    menuitem:=lua_touserdata(L,-3);
    index:=lua_tointeger(L, -2);
    menuitem2:=lua_touserdata(L,-1);
    lua_pop(L, parameters);

    menuitem.Insert(index, menuitem2);
  end else lua_pop(L, parameters);
end;

function menuItem_delete(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  menuitem: TMenuItem;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    menuitem:=lua_touserdata(L,-2);
    index:=lua_toInteger(L,-1);
    lua_pop(L, parameters);

    menuitem.Delete(index);
  end else lua_pop(L, parameters);
end;

function menuitem_onClick(L: PLua_State): integer; cdecl; //for some reason the menuitem has it's own fonclick variable
var
  parameters: integer;
  control: Tmenuitem;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    control:=lua_touserdata(L,-2);

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

  lua_pop(L, parameters);
end;

function menuItem_doClick(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  mi: TMenuItem;
  Color: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    mi:=lua_touserdata(L,1);
    if assigned(mi.onclick) then
      mi.OnClick(mi);
  end;

  lua_pop(L, parameters);
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
  lua_register(LuaVM, 'menuItem_onClick', menuItem_onClick);
  lua_register(LuaVM, 'menuItem_doClick', menuItem_doClick);
end;

end.

