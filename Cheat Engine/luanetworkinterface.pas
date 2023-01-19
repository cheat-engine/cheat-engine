unit LuaNetworkInterface;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lualib, lauxlib;

procedure initializeLuaNetworkInterface(L: Plua_State);

implementation

uses LuaClass, luahandler, networkInterface, networkInterfaceApi, LuaObject;

function ceconnection_getOption(L: PLua_State): integer; cdecl;
var
  c: TCEConnection;
  optname: string;
begin
  result:=0;
  c:=getconnection;
  if c=nil then exit;

  if lua_gettop(L)=1 then
  begin
    optname:=lua_tostring(L,1);
    lua_pushstring(L, c.getOption(optname));
    result:=1;
  end;
end;

function ceconnection_setOption(L: PLua_State): integer; cdecl;
var
  c: TCEConnection;
  optname: string;
  v: string;
begin
  result:=0;
  c:=getconnection;
  if c=nil then exit;

  if lua_gettop(L)=2 then
  begin
    optname:=lua_tostring(L,1);
    v:=Lua_ToString(L,2);
    c.setOption(optname,v);
  end;
end;

function ceconnection_getOptionList(L: PLua_State): integer; cdecl;
var c: TCEConnection;
  o: TCEServerOptions;
  i: integer;
begin
  result:=0;
  o:=[];
  c:=getconnection;
  if c=nil then exit;

  if c.connected then
  begin
    lua_createtable(L,length(o),0);
    result:=1;

    c.getOptions(o);
    for i:=0 to length(o)-1 do
    begin
      lua_pushinteger(L,i+1);
      lua_createtable(L,0,6);

      //optname, parentoptname, optdescription, acceptablevalue, currentvalue, optiontype(0=not an option, just a label, 1=bool, 2=int, 3=float, 4=double, 5=text)
      lua_pushstring(L,'optname');
      lua_pushstring(L,o[i].optname);
      lua_settable(L,-3);

      lua_pushstring(L,'parentoptname');
      lua_pushstring(L,o[i].parentoptname);
      lua_settable(L,-3);

      lua_pushstring(L,'optdescription');
      lua_pushstring(L,o[i].optdescription);
      lua_settable(L,-3);

      lua_pushstring(L,'acceptablevalues');
      lua_pushstring(L,o[i].acceptablevalues);
      lua_settable(L,-3);

      lua_pushstring(L,'currentvalue');
      lua_pushstring(L,o[i].currentvalue);
      lua_settable(L,-3);

      lua_pushstring(L,'optiontype');
      lua_pushinteger(L,ord(o[i].optiontype));
      lua_settable(L,-3);

      lua_settable(L,-3);
    end;

  end;

end;



function ceconnection_setCurrentPath(L: PLua_State): integer; cdecl;
var c: TCEConnection;
begin
  c:=getconnection;
  if c=nil then exit(0);

  if lua_gettop(L)=1 then
  begin
    lua_pushboolean(L, c.setCurrentPath(Lua_ToString(L,1)));
    exit(1);
  end;
end;

function ceconnection_getCurrentPath(L: PLua_State): integer; cdecl;
var c: TCEConnection;
begin
  c:=getconnection;
  if c=nil then exit(0);

  lua_pushstring(L, c.getCurrentPath);
  exit(1);
end;

function ceconnection_enumFiles(L: PLua_State): integer; cdecl;
var
  path: string;
  c: TCEConnection;
  filelist: tstringlist;
  i: integer;
begin
  c:=getconnection;
  if c=nil then exit(0);

  if lua_gettop(L)>=1 then
    path:=Lua_ToString(L,1)
  else
    path:=c.getCurrentPath;

  filelist:=tstringlist.create;
  c.enumfiles(path, filelist);

  lua_createtable(L,filelist.count,0);

  for i:=0 to filelist.count-1 do
  begin
    lua_pushinteger(L,i+1);
    lua_newtable(L);
    lua_pushstring(L,'name');
    lua_pushstring(L,filelist[i]);
    lua_settable(L,-3);

    lua_pushstring(L,'type');
    lua_pushinteger(L,ptruint(filelist.Objects[i]));
    lua_settable(L,-3);

    lua_settable(L,-3);
  end;

  result:=1;
end;

function ceconnection_createDir(L: PLua_State): integer; cdecl;
var path: string;
  c: TCEConnection;
begin
  result:=0;
  c:=getconnection;
  if c=nil then exit(0);

  if lua_gettop(L)>=1 then
  begin
    path:=Lua_ToString(L,1);
    lua_pushboolean(L, c.createdir(path));
    result:=1;
  end;
end;

function ceconnection_deleteFile(L: PLua_State): integer; cdecl;
var path: string;
  c: TCEConnection;
begin
  result:=0;
  c:=getconnection;
  if c=nil then exit(0);

  if lua_gettop(L)>=1 then
  begin
    path:=Lua_ToString(L,1);
    lua_pushboolean(L, c.deletefile(path));
    result:=1;
  end;
end;

function ceconnection_getFilePermission(L: PLua_State): integer; cdecl;
var
  path: string;
  c: TCEConnection;
  perm: UINT32;
begin
  result:=0;
  c:=getconnection;
  if c=nil then exit(0);

  if lua_gettop(L)>=1 then
  begin
    path:=Lua_ToString(L,1);
    if c.getFilePermissions(path,perm) then
    begin
      lua_pushinteger(L,perm);
      result:=1;
    end;
  end;
end;

function ceconnection_setFilePermission(L: PLua_State): integer; cdecl;
var
  path: string;
  c: TCEConnection;
  perm: UINT32;
begin
  result:=0;
  c:=getconnection;
  if c=nil then exit(0);

  if lua_gettop(L)>=2 then
  begin
    path:=Lua_ToString(L,1);
    perm:=lua_tointeger(L,2);
    result:=1;

    lua_pushboolean(L, c.setfilepermissions(path,perm));
  end;
end;

function ceconnection_getFile(L: PLua_State): integer; cdecl;
var
  c: TCEConnection;
  path: string;
  s: tstream;
begin
  result:=0;
  c:=getconnection;
  if c=nil then exit(0);
  if lua_gettop(L)>=2 then
  begin
    path:=Lua_ToString(L,1);
    s:=lua_ToCEUserData(L,2);

    lua_pushboolean(L, c.getfile(path,s));
  end;
end;

function ceconnection_putFile(L: PLua_State): integer; cdecl;
var
  c: TCEConnection;
  path: string;
  s: tstream;
begin
  result:=0;
  c:=getconnection;
  if c=nil then exit(0);
  if lua_gettop(L)>=2 then
  begin
    path:=Lua_ToString(L,1);
    s:=lua_ToCEUserData(L,2);

    lua_pushboolean(L, c.putfile(path,s));
  end;
end;

function getCEServerInterface(L: PLua_State): integer; cdecl;
var c: TCEConnection;
begin
  result:=0;
  c:=getconnection;
  if c<>nil then
  begin
    luaclass_newClass(L, c);
    exit(1);
  end;
end;

procedure ceconnection_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getOptionList', ceconnection_getOptionList);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setCurrentPath', ceconnection_setCurrentPath);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getCurrentPath', ceconnection_getCurrentPath);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'enumFiles', ceconnection_enumFiles);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'createDir', ceconnection_createDir);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'removeDir', ceconnection_deleteFile);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'deleteFile', ceconnection_deleteFile);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getFilePermission', ceconnection_getFilePermission);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setFilePermission', ceconnection_setFilePermission);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getFile', ceconnection_getFile);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'putFile', ceconnection_putFile);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Option', ceconnection_getOption, ceconnection_setOption);
end;

procedure initializeLuaNetworkInterface(L: Plua_State);
begin
  lua_register(L, 'getCEServerInterface', getCEServerInterface);
end;

initialization
  luaclass_register(TCEConnection, ceconnection_addMetaData);

end.

