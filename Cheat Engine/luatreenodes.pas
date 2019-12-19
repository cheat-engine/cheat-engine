unit LuaTreeNodes;

{$mode delphi}

interface

uses
  Classes, SysUtils, ComCtrls, lua, lualib, lauxlib;

implementation

uses luaclass, luahandler, LuaObject;


function treenodes_clear(L: Plua_State): integer; cdecl;
var
  treenodes: TTreeNodes;
  i: integer;
begin
  result:=0;
  treenodes:=luaclass_getClassObject(L);

{$ifdef cpu32}
  //free the allocated memory if there was any
  for i:=0 to treenodes.count-1 do
  begin
    if treenodes[i].Data<>nil then
      freemem(treenodes[i].data)
  end;
{$endif}
  treenodes.Clear;
end;

function treenodes_getItem(L: PLua_State): integer; cdecl;
var
  treenodes: Ttreenodes;
  index: integer;
begin
  result:=0;
  treenodes:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    index:=lua_tointeger(L,-1);
    luaclass_newClass(L, treenodes.Item[index]);
    result:=1;
  end;
end;


function treenodes_getCount(L: PLua_State): integer; cdecl;
var
  treenodes: Ttreenodes;
begin
  treenodes:=luaclass_getClassObject(L);
  lua_pushvariant(L, treenodes.Count);
  result:=1;
end;

function treenodes_add(L: PLua_State): integer; cdecl;
var
  treenodes: Ttreenodes;
  paramcount: integer;
  s: string;
begin
  treenodes:=luaclass_getClassObject(L);

  paramcount:=lua_gettop(L);
  if paramcount>=1 then
    s:=Lua_ToString(L, 1)
  else
    s:='';


  luaclass_newClass(L, treenodes.add(nil, s));
  result:=1;
end;

function treenodes_insertBehind(L: PLua_State): integer; cdecl;
var
  treenodes: Ttreenodes;
  treenode: ttreenode;
  paramcount: integer;
  s: string;
begin
  treenodes:=luaclass_getClassObject(L);

  paramcount:=lua_gettop(L);
  if paramcount>=1 then
    treenode:=lua_ToCEUserData(L, 1)
  else
    treenode:=nil;

  if paramcount>=2 then
    s:=Lua_ToString(L, 2)
  else
    s:='';


  luaclass_newClass(L, treenodes.InsertBehind(treenode, s));
  result:=1;
end;

function treenodes_insert(L: PLua_State): integer; cdecl;
var
  treenodes: Ttreenodes;
  treenode: ttreenode;
  paramcount: integer;
  s: string;
begin
  treenodes:=luaclass_getClassObject(L);

  paramcount:=lua_gettop(L);
  if paramcount>=1 then
    treenode:=lua_ToCEUserData(L, 1)
  else
    treenode:=nil;

  if paramcount>=2 then
    s:=Lua_ToString(L, 2)
  else
    s:='';


  luaclass_newClass(L, treenodes.insert(treenode, s));
  result:=1;
end;

procedure treenodes_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clear', treenodes_clear);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCount', treenodes_getCount);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getItem', treenodes_getItem);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'add', treenodes_add);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'insert', treenodes_insert);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'insertBehind', treenodes_insertBehind);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Count', treenodes_getCount, nil);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Item', treenodes_getItem);
  luaclass_setDefaultArrayProperty(L, metatable, userdata, treenodes_getItem, nil);

end;


initialization
   luaclass_register(TTreeNodes,  treenodes_addMetaData);

end.

