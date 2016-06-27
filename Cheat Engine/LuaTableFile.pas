unit LuaTableFile;

{$mode delphi}

interface

uses
  Classes, SysUtils,lua, lauxlib, lualib;

procedure initializeLuaTableFile;

implementation

uses LuaClass, LuaHandler, LuaObject, MainUnit, luafile;

resourcestring
  rsErrorRaisedWithMessage = ' error raised with message: ';
  rsTableDileEntryAlreadyExistsForFilename = 'Table file entry already exists for filename: ';
  rsCreateTableFileRequiresAtLeastOneParameter = 'createTableFile requires at least one parameter';

function indexOfLuaFileByName(filename: string; internal: boolean=false): integer;
var
  i: integer;
  luaFiles: TLuaFileList;
begin
  if internal then
    luaFiles:=mainForm.InternalLuaFiles
  else
    luaFiles:=mainform.LuaFiles;

  result:=-1;
  for i:=0 to luaFiles.count-1 do
    if luaFiles[i].name=filename then
    begin
      result:=i;
      break;
    end;
end;

function pushLuaTableFile(L: Plua_State; index: integer; internal: boolean=false): integer;
var
  lf: TLuafile;
  s: TMemoryStream;
begin
  if internal then
    lf:=MainForm.InternalLuaFiles[index]
  else
    lf:=MainForm.LuaFiles[index];

   s:=lf.stream;
   s.Position:=0;

   result:=1;
   luaclass_newClass(L, lf);
end;

function addLuaTableFile(L: Plua_State; filename: string; filepath: string=''): integer;
var
  i: integer;
  lf: TLuafile;
  s: TMemoryStream;
begin
  result:=0;
  if (indexOfLuaFileByName(filename)=-1) and
     (indexOfLuaFileByName(filename, true)=-1) then
  begin
    try
      s:=TMemorystream.Create;
      try
        if filepath<>'' then
          s.LoadFromFile(filepath);
        lf:=TLuaFile.Create(filename, s);
      finally
        s.Free;
      end;
      i:=mainform.LuaFiles.add(lf);
      result:=pushLuaTableFile(L, i);
      mainform.editedsincelastsave:=true;
    except
      on e : Exception do begin
        lua_pushstring(L, e.className + rsErrorRaisedWithMessage + e.message);
        lua_error(L);
      end;
    end;
  end
  else
  begin
      lua_pushstring(L, rsTableDileEntryAlreadyExistsForFilename + filename);
      lua_error(L);
  end;
end;

function createTableFile(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  filename, filepath: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    filepath:='';
    filename:=Lua_ToString(L, 1);
    if parameters>=2 then
       filepath:=Lua_ToString(L, 2);
    result:=addLuaTableFile(L, filename, filepath);
  end
  else
  begin
    lua_pushstring(L, rsCreateTableFileRequiresAtLeastOneParameter);
    lua_error(L);
  end;
end;

function findTableFile(L: Plua_State): integer; cdecl;
var parameters: integer;
  f: string;
  i: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    f:=Lua_ToString(L, -1);
    lua_pop(L, lua_gettop(L));
    i:=indexOfLuaFileByName(f);
    if i<>-1 then
      result:=pushLuaTableFile(L, i) //return the tableFile, not the stream. To get the stream, use  tablefile_getData
    else
    begin
      i:=indexOfLuaFileByName(f, true); //internal file
      if i<>-1 then
        result:=pushLuaTableFile(L, i, true);
    end;
  end
  else
    lua_pop(L, lua_gettop(L));
end;


function tablefile_delete(L: Plua_State): integer; cdecl;
var
  lf: TLuaFile;
begin
  result:=0;
  lf:=luaclass_getClassObject(L);
  mainform.LuaFiles.Remove(lf);
  mainform.UpdateMenu;
  lf.Free;
  mainform.editedsincelastsave:=true;
end;

function tablefile_saveToFile(L: Plua_State): integer; cdecl;
var
  lf: TLuaFile;
  f: string;
begin
  result:=0;
  lf:=luaclass_getClassObject(L);
  f:=lf.name;
  if lua_gettop(L)>=1 then
    f:=Lua_ToString(L, 1);

  lf.stream.Position:=0;
  lf.stream.SaveToFile(f);
end;

function tablefile_getData(L: Plua_State): integer; cdecl;
var
  lf: TLuaFile;
begin
  result:=1;
  lf:=luaclass_getClassObject(L);
  luaclass_newClass(L, lf.stream);
end;

procedure tablefile_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'delete', tablefile_delete);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'saveToFile', tablefile_saveToFile);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getData', tablefile_getData);
end;

procedure initializeLuaTableFile;
begin
  Lua_register(LuaVM, 'createTableFile', createTableFile);
  Lua_register(LuaVM, 'findTableFile', findTableFile);
  Lua_register(LuaVM, 'tablefile_delete', tablefile_delete);
  Lua_register(LuaVM, 'tablefile_saveToFile', tablefile_saveToFile);
  Lua_register(LuaVM, 'tablefile_getData', tablefile_getData);
end;

initialization
  luaclass_register(TLuafile, tablefile_addMetaData);

end.

