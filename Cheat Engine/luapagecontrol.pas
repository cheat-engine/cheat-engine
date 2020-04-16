unit LuaPageControl;

{$mode delphi}

interface

uses
  Classes, SysUtils, controls, comctrls, lua, lualib, lauxlib;

procedure initializeLuaPageControl;

implementation

uses luahandler, LuaWinControl, luaclass;

function createPageControl(L: Plua_State): integer; cdecl;
var
  PageControl: TPageControl;
  owner: TWincontrol;
begin
  if lua_gettop(L)=1 then
    owner:=lua_toceuserdata(L, 1)
  else
    owner:=nil;

  PageControl:=TPageControl.Create(owner);
  PageControl.Parent:=owner;

  luaclass_newClass(L, PageControl);
  result:=1;
end;

function pagecontrol_addTab(L: Plua_State): integer; cdecl;
var
  pc: TPageControl;
  ts: TTabSheet;
begin
  pc:=luaclass_getClassObject(L);
  luaclass_newClass(L, pc.AddTabSheet);
  result:=1;
end;

function pagecontrol_tabRect(L: Plua_State): integer; cdecl;
var
  pc: TPageControl;
  index: integer;
begin
  if lua_gettop(L)>=1 then
  begin
    index:=lua_tointeger(L,1);
    lua_pushrect(L, TPageControl(luaclass_getClassObject(L)).TabRect(index));
    result:=1;
  end
  else
    result:=0;
end;


function pagecontrol_getPageCount(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, TPageControl(luaclass_getClassObject(L)).PageCount);
  result:=1;
end;

function pagecontrol_getPage(L: Plua_State): integer; cdecl;
var
  pc: TPageControl;
  index: integer;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    pc:=luaclass_getClassObject(L);
    index:=lua_tointeger(L,1);
    if index<pc.PageCount then
    begin
      luaclass_newClass(L, pc.Pages[index]);
      result:=1;
    end;
  end;
end;

procedure pagecontrol_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'addTab', pagecontrol_addTab);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'tabRect', pagecontrol_tabRect);
  luaclass_addPropertyToTable(L, metatable, userdata, 'PageCount', pagecontrol_getPageCount, nil);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Page', pagecontrol_getPage);
end;

function tabsheet_getTabIndex(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, TTabSheet(luaclass_getClassObject(L)).TabIndex);
  result:=1;
end;

procedure tabsheet_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  wincontrol_addMetaData(L, metatable, userdata);
  luaclass_addPropertyToTable(L, metatable, userdata, 'TabIndex', tabsheet_getTabIndex, nil);
end;

procedure initializeLuaPageControl;
begin
  lua_register(LuaVM, 'createPageControl', createPageControl);
end;

initialization
   luaclass_register(TPageControl, pagecontrol_addMetaData);
   luaclass_register(TTabsheet, tabsheet_addMetaData);

end.

