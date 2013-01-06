unit LuaFileDialog;

{$mode delphi}

interface

uses
  Classes, SysUtils, dialogs, lua, lauxlib, lualib;

implementation

uses LuaClass, LuaHandler, LuaComponent;

function filedialog_getFiles(L: Plua_State): integer; cdecl;
var
  t: TFileDialog;
begin
  t:=luaclass_getClassObject(L);
  luaclass_newClass(L, t.Files);
  result:=1;
end;

function filedialog_execute(L: Plua_State): integer; cdecl;
var
  t: TFileDialog;
begin
  t:=luaclass_getClassObject(L);
  lua_pushboolean(L, t.Execute);
  result:=1;
end;

procedure filedialog_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  //only export "execute" and "Files", the rest is already published
  component_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'execute', filedialog_execute);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Files', filedialog_getFiles, nil);
end;

initialization
  luaclass_register(tfiledialog, filedialog_addMetaData);

end.

