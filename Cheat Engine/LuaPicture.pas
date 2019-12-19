unit LuaPicture;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics,lua, lualib, lauxlib, LuaHandler;

procedure initializeLuaPicture;

implementation

uses luaclass, LuaObject;

function createPicture(L: PLua_State): integer; cdecl;
begin
  luaclass_newClass(L, TPicture.Create);
  result:=1;
end;

function picture_loadFromFile(L: PLua_State): integer; cdecl;
var
  picture: Tpicture;
begin
  result:=0;
  picture:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
    picture.LoadFromFile(Lua_ToString(L, -1));
end;

function picture_saveToFile(L: PLua_State): integer; cdecl;
var
  picture: Tpicture;
begin
  result:=0;
  picture:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
    picture.SaveToFile(Lua_ToString(L, 1));
end;

function picture_loadFromStream(L: PLua_State): integer; cdecl;
var
  paramstart, paramcount: integer;
  picture: Tpicture;
  stream: TStream;
  ext: string;
begin
  result:=0;
  picture:=luaclass_getClassObject(L, @paramstart, @paramcount);

  if paramcount>=1 then
  begin
    stream:=lua_ToCEUserData(L, paramstart);

    if paramstart=2 then //6.2 compat, there i set the position to 0. In 6.3+ I will have the user set it to the position they want first
      stream.Position:=0;

    if paramcount=2 then
    begin
      ext:=Lua_ToString(L, paramstart+1);
      picture.LoadFromStreamWithFileExt(stream,ext);
    end;

    if paramcount=1 then
      picture.LoadFromStream(stream);

  end;
end;

function picture_assign(L: PLua_State): integer; cdecl;
var
  picture, picture2: Tpicture;
begin
  result:=0;
  picture:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    picture2:=lua_ToCEUserData(L, -1);
    picture.Assign(picture2);
  end;
end;

function picture_getIcon(L: PLua_State): integer; cdecl;
begin
  luaclass_newClass(L, TPicture(luaclass_getClassObject(L)).Icon);
  result:=1;
end;

function picture_setIcon(L: PLua_State): integer; cdecl;
begin
  result:=0;
  TPicture(luaclass_getClassObject(L)).Icon:=lua_ToCEUserData(L, 1);
end;


function picture_getGraphic(L: PLua_State): integer; cdecl;
var
  picture: Tpicture;
begin
  picture:=luaclass_getClassObject(L);
  luaclass_newClass(L, picture.Graphic);
  result:=1;
end;

function picture_setGraphic(L: PLua_State): integer; cdecl;
begin
  result:=0;
  TPicture(luaclass_getClassObject(L)).Graphic:=lua_ToCEUserData(L, 1);
end;


function picture_getPNG(L: PLua_State): integer; cdecl;
var
  picture: Tpicture;
begin
  picture:=luaclass_getClassObject(L);
  luaclass_newClass(L, picture.PNG);
  result:=1;
end;

function picture_setPNG(L: PLua_State): integer; cdecl;
begin
  result:=0;
  TPicture(luaclass_getClassObject(L)).PNG:=lua_ToCEUserData(L, 1);
end;


function picture_getBitmap(L: PLua_State): integer; cdecl;
var
  picture: Tpicture;
begin
  picture:=luaclass_getClassObject(L);
  luaclass_newClass(L, picture.Bitmap);
  result:=1;
end;

function picture_setBitmap(L: PLua_State): integer; cdecl;
begin
  result:=0;
  TPicture(luaclass_getClassObject(L)).Bitmap:=lua_ToCEUserData(L, 1);
end;


function picture_getJpeg(L: PLua_State): integer; cdecl;
var
  picture: Tpicture;
begin
  picture:=luaclass_getClassObject(L);
  luaclass_newClass(L, picture.Jpeg);
  result:=1;
end;

function picture_setJpeg(L: PLua_State): integer; cdecl;
begin
  result:=0;
  TPicture(luaclass_getClassObject(L)).Jpeg:=lua_ToCEUserData(L, 1);
end;


procedure picture_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'loadFromFile',picture_loadFromFile);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'saveToFile',picture_saveToFile);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'loadFromStream',picture_loadFromStream);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'assign',picture_assign);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getGraphic',picture_getGraphic);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getPNG',picture_getPNG);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getBitmap',picture_getBitmap);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getJpeg',picture_getJpeg);

  Luaclass_addPropertyToTable(L, metatable, userdata, 'Graphic', picture_getGraphic, picture_setGraphic);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'PNG', picture_getPNG, picture_setPNG);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Bitmap', picture_getBitmap, picture_setBitmap);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Jpeg', picture_getJpeg, picture_setJpeg);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Icon', picture_getIcon, picture_setIcon);
end;

procedure initializeLuaPicture;
var x: TPicture;
begin
  lua_register(LuaVM, 'createPicture', createPicture);
  lua_register(LuaVM, 'picture_loadFromFile',picture_loadFromFile);
  lua_register(LuaVM, 'picture_loadFromStream',picture_loadFromStream);
  lua_register(LuaVM, 'picture_assign',picture_assign);
  lua_register(LuaVM, 'picture_getGraphic',picture_getGraphic);

  lua_register(LuaVM, 'picture_getPNG',picture_getPNG);
  lua_register(LuaVM, 'picture_getBitmap',picture_getBitmap);
  lua_register(LuaVM, 'picture_getJpeg',picture_getJpeg);

end;

initialization
  luaclass_register(TPicture, picture_addMetaData);



end.

