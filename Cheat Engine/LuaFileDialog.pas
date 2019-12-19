unit LuaFileDialog;

{$mode delphi}

interface

uses
  Classes, SysUtils, dialogs, lua, lauxlib, lualib;

implementation

uses LuaClass, LuaHandler, LuaCommonDialog;

function filedialog_getFiles(L: Plua_State): integer; cdecl;
var
  t: TFileDialog;
begin
  t:=luaclass_getClassObject(L);
  luaclass_newClass(L, t.Files);
  result:=1;
end;

procedure filedialog_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  //only export "Files", the rest is already published (and Execute is part of commondialog)
  commondialog_addMetaData(L, metatable, userdata);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Files', filedialog_getFiles, nil);
end;

initialization
  luaclass_register(tfiledialog, filedialog_addMetaData);

end.

