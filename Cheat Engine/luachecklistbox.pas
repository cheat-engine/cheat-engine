unit LuaCheckListBox;

{$mode delphi}

interface

uses
  Classes, SysUtils, controls, CheckLst;

procedure initializeLuaCheckListbox;

implementation

uses LuaListbox, luahandler, luaclass, lua;

function createCheckListBox(L: Plua_State): integer; cdecl;
var
  CheckListBox: TCheckListBox;
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


  CheckListBox:=TCheckListBox.Create(owner);
  if owner<>nil then
    CheckListBox.Parent:=owner;

  luaclass_newClass(L, CheckListBox);
  result:=1;
end;

function checklistbox_getChecked(L: PLua_State): integer; cdecl;
var
  checklistbox: TCustomCheckListBox;
  index: integer;
begin
  result:=0;
  checklistbox:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    index:=lua_toInteger(L, 1);
    lua_pushboolean(L, checklistbox.Checked[index]);
    result:=1;
  end;
end;

function checklistbox_setChecked(L: PLua_State): integer; cdecl;
var
  checklistbox: TCustomCheckListBox;
  index: integer;
begin
  result:=0;
  checklistbox:=luaclass_getClassObject(L);
  if lua_gettop(L)=2 then
  begin
    index:=lua_toInteger(L, 1);
    checklistbox.Checked[index]:=lua_toboolean(L, 2);
  end;
end;

procedure checklistbox_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  listbox_addMetaData(L, metatable, userdata);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Checked', checklistbox_getChecked, checklistbox_setChecked);
end;

procedure initializeLuaCheckListbox;
begin
  lua_register(LuaVM, 'createCheckListBox', createCheckListBox);
end;

initialization
  luaclass_register(TCustomCheckListBox, checklistbox_addMetaData);

end.

