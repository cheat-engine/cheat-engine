unit LuaImage;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib, ExtCtrls;

procedure initializeLuaImage;

implementation

uses luaclass, luahandler, LuaGraphicControl, pluginexports;

function createImage(L: Plua_State): integer; cdecl;
var parameters: integer;
  f,p: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_toceuserdata(L, -1);
    p:=ce_createImage(f);

    lua_pop(L, lua_gettop(L));

    luaclass_newClass(L, p);
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function image_loadImageFromFile(L: Plua_State): integer; cdecl;
var
  i: timage;
  filename: pchar;
begin
  result:=0;
  i:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    filename:=lua.lua_tostring(L, -1);
    try
      i.Picture.LoadFromFile(filename);
      lua_pushboolean(L, true);
    except
      lua_pushboolean(L, false);
    end;
    result:=1;
  end;
end;

function image_getStretch(L: Plua_State): integer; cdecl;
var
  i: timage;
begin
  i:=luaclass_getClassObject(L);
  lua_pushboolean(L, i.Stretch);
  result:=1;
end;

function image_setStretch(L: Plua_State): integer; cdecl;
var
  i: timage;
begin
  result:=0;
  i:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    i.Stretch:=lua_toboolean(L, -1);
end;

function image_getTransparent(L: Plua_State): integer; cdecl;
var
  i: timage;
begin
  i:=luaclass_getClassObject(L);
  lua_pushboolean(L, i.Transparent);
  result:=1;
end;

function image_setTransparent(L: Plua_State): integer; cdecl;
var
  i: timage;
begin
  result:=0;
  i:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    i.Transparent:=lua_toboolean(L, -1);
end;

function Image_getCanvas(L: PLua_State): integer; cdecl;
var
  i: timage;
begin
  i:=luaclass_getClassObject(L);
  luaclass_newClass(L, i.Canvas);
  result:=1;
end;

function Image_getPicture(L: PLua_State): integer; cdecl;
var
  i: timage;
begin
  i:=luaclass_getClassObject(L);
  luaclass_newClass(L, i.Picture);
  result:=1;
end;

function Image_setPicture(L: PLua_State): integer; cdecl;
var
  i: timage;
begin
  i:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    i.Picture:=lua_ToCEUserData(L, 1);

  result:=1;
end;

procedure image_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  graphiccontrol_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'loadImageFromFile', image_loadImageFromFile);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setTransparent', image_settransparent);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getTransparent', image_gettransparent);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setStretch', image_setstretch);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getStretch', image_getstretch);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCanvas', image_getCanvas);  //override
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getPicture', Image_getPicture);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setPicture', Image_setPicture);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Canvas', image_getCanvas, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Transparent', image_gettransparent, image_settransparent);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Stretch', image_getStretch, image_setStretch);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Picture', image_getPicture, image_setPicture);
end;

procedure initializeLuaImage;
begin
  lua_register(LuaVM, 'createImage', createImage);
  lua_register(LuaVM, 'image_loadImageFromFile', image_loadImageFromFile);
  lua_register(LuaVM, 'image_transparent', image_settransparent);
  lua_register(LuaVM, 'image_stretch', image_setstretch);
  lua_register(LuaVM, 'image_getCanvas', image_getCanvas);
  lua_register(LuaVM, 'image_getPicture', Image_getPicture);
end;

initialization
  luaclass_register(TCustomImage, image_addMetaData);

end.

