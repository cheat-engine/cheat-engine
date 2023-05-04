unit LuaStructureFrm;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, LuaHandler, LuaCaller, symbolhandler,
  cefuncproc, newkernelhandler, Dialogs, LuaClass, LuaClassArray, commonTypeDefs,
  Forms, StructuresFrm2, LuaStructure, betterControls;
  
  

procedure initializeLuaStructureFrm;
procedure structureFrm_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
procedure structGroup_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
procedure structColumn_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses LuaForm, LuaObject, DotNetPipe, ComCtrls;

resourcestring
  rsGroup1 = 'Group 1';
  rsGroupD = 'Group %d';

function enumStructureForms(L: PLua_State): integer; cdecl;
var i: integer;
begin
  lua_newtable(L);

  for i:=0 to frmStructures2.Count-1 do
  begin
    lua_pushinteger(L,i+1);
    luaclass_newClass(L, frmStructures2[i]);
    lua_settable(L,-3);
  end;

  result:=1;
end;

function createStructureForm(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  address: string;
  form: TfrmStructures2;
  column: TStructColumn;

  metatable: integer;
  m: tmethod;
  i: integer;
  userdata: integer;
  structname: string;
  groupname: string;
  struct: TDissectedStruct;
  group: TStructGroup;

begin
  result:=0;
  parameters:=lua_gettop(L);

  structname:='';
  address:='';
  groupname:=rsGroup1;

  if parameters>=3 then structname:=lua_tostring(L,3);
  if parameters>=2 then groupname:=lua_tostring(L,2);
  if parameters>=1 then address:=lua_tostring(L,1);
  lua_pop(L, parameters);

  form:=TfrmStructures2.create(application);
  luaclass_newclass(L, form);

  if parameters>=1 then
  begin
    group:=TStructGroup.create(form,groupname);
    column:=form.addColumn;
    column.AddressText:=address;
    column.SetProperEditboxPosition;

    if parameters>=3 then
    begin
      for i:=0 to DissectedStructs.count-1 do
      begin
        struct:=TDissectedStruct(DissectedStructs[i]);
        if CompareText(struct.Name,structname) = 0 then
        begin
          form.mainStruct:=struct;
          //form.onFullStructChange(struct);
          break;
        end;
      end;
    end;

  end
  else
  begin
    group:=TStructGroup.create(form,'');
    column:=form.addColumn;
    column.AddressText:='';
    column.SetProperEditboxPosition;
  end;
  group.setPositions;
  form.show;

  luaclass_newClass(L, form);
  result:=1;
end;

function structureForm_addColumn(L: PLua_State): integer; cdecl;
var
  frm: TFrmStructures2;
  column: TStructColumn;
begin
  result:=0;
  frm:=luaclass_getClassObject(L);
  column:=frm.addColumn;
  luaclass_newclass(L, column);
  result:=1;
end;

function structureForm_addGroup(L: PLua_State): integer; cdecl;
var
  frm: TFrmStructures2;
  group: TStructGroup;
  groupname: string;
  parameters: integer;
begin
  result:=0;
  frm:=luaclass_getClassObject(L);
  groupname:=Format(rsGroupD,[frm.groupCount+1]);
  parameters:=lua_gettop(L);
  if (parameters>=1) and (not lua_isnil(L,1)) then
    groupname:=lua_ToString(L, 1);

  lua_pop(L, parameters);
  group:=TStructGroup.create(frm,groupname);
  luaclass_newclass(L, group, structGroup_addMetaData);
  result:=1;
end;

function structureForm_getSelectedStructElement(L: PLua_State): integer; cdecl;
var
  frm: TFrmStructures2;
  struct: TDissectedStruct;
  element: TStructelement;
  structlist: tlist;
  node: TTreenode;
begin
  frm:=luaclass_getClassObject(L);
  result:=0;
  node:=frm.tvStructureView.Selected;
  if node<>nil then
  begin
    struct:=TDissectedStruct(node.parent.Data);
    if struct<>nil then
    begin
      if node.Index<struct.count then
      begin
        element:=struct.element[node.Index];
        luaclass_newClass(L, element);

        lua_newtable(L);
        //build a parent structure list
        node:=node.parent;
        while node.parent<>nil do
        begin
          struct:=TDissectedStruct(node.parent.data);
          if node.Index>=struct.count then exit(0); //corrupt

          element:=struct.element[node.Index];
          lua_pushinteger(L, node.Level);
          lua_newtable(L);

          lua_pushstring(L,'struct');
          luaclass_newClass(L, struct);
          lua_settable(L,-3);

          lua_pushstring(L,'element');
          luaclass_newClass(L, element);
          lua_settable(L,-3);

          lua_settable(L,-3);
          node:=node.parent;
        end;

        exit(2);
      end;
    end;
  end;
end;

function structureForm_structChange(L: PLua_State): integer; cdecl;
var
  frm: TFrmStructures2;
begin
  frm:=luaclass_getClassObject(L);
  frm.mainStruct:=frm.mainStruct;
  result:=0;
end;

function structureForm_getColumn(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  frm: TFrmStructures2;
  index: integer;
begin
  result:=0;
  frm:=luaclass_getClassObject(L);
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    index:=lua_tointeger(L,1);
    luaclass_newclass(L, frm.columns[index]);
    result:=1;
  end else lua_pop(L, parameters);
end;

function structureForm_getGroup(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  frm: TFrmStructures2;
  index: integer;
begin
  result:=0;
  frm:=luaclass_getClassObject(L);
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    index:=lua_tointeger(L,1);
    luaclass_newclass(L, frm.group[index]);
    result:=1;
  end else lua_pop(L, parameters);
end;




function structColumn_focus(L: PLua_State): integer; cdecl;
var
  column: TStructColumn;
begin
  result:=0;
  column:=luaclass_getClassObject(L);
  column.focus;
end;


function structGroup_addColumn(L: PLua_State): integer; cdecl;
var
  frm: TFrmStructures2;
  group: TStructGroup;
  column: TStructColumn;
begin
  result:=0;
  group:=luaclass_getClassObject(L);
  column:=TStructColumn.create(group);
  frm:=group.getParent();
  frm.RefreshVisibleNodes();
  luaclass_newclass(L, column);
  result:=1;
end;

function structGroup_getColumn(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  group: TStructGroup;
  index: integer;
begin
  result:=0;
  group:=luaclass_getClassObject(L);
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    index:=lua_tointeger(L,1);
    luaclass_newclass(L, group.columns[index]);
    result:=1;
  end else lua_pop(L, parameters);
end;

procedure structGroup_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
var i: integer;
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'addColumn', structGroup_addColumn);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Column', structgroup_getColumn, nil);
end;

procedure structColumn_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
var i: integer;
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'focus', structColumn_focus);
end;


procedure structureFrm_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
var i: integer;
begin
  customForm_addMetaData(L, metatable, userdata);
  
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'addColumn', structureForm_addColumn);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'addGroup', structureForm_addGroup);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'structChange', structureForm_structChange);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getSelectedStructElement', structureForm_getSelectedStructElement);


  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Column', structureForm_getColumn, nil);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Group', structureForm_getGroup, nil);

end;

procedure initializeLuaStructureFrm;
begin
  lua_register(LuaVM, 'createStructureForm', createStructureForm);
  lua_register(LuaVM, 'enumStructureForms', enumStructureForms);


end;

initialization
  luaclass_register(TfrmStructures2, structureFrm_addMetaData);
  luaclass_register(TStructGroup, structGroup_addMetaData);
  luaclass_register(TStructColumn, structColumn_addMetaData);

end.
