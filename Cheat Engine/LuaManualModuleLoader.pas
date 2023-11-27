unit LuaManualModuleLoader;

{$mode delphi}

interface


{$IFDEF windows}
uses
  windows, Classes, SysUtils;

procedure initializeLuaModuleLoader;
{$ENDIF}

implementation

{$IFDEF windows}
uses ManualModuleLoader, lua, lauxlib, lualib, LuaClass, LuaHandler, LuaObject, luafile;

function moduleloader_createModuleLoader(L: PLua_State): integer; cdecl;
var
  ml: TModuleLoader;
  filename: string;
  executeEntryPoint: boolean;
  paramlist: integer;
  i: integer;

  usetimeout: boolean=false;
  timeout: integer;

  paramoffset: integer;

  o: TObject;
  ms: tmemorystream absolute o;
begin
  result:=0;
  paramoffset:=0;
  if lua_gettop(L)>=1 then
  begin
    if lua_isstring(L,1) then
    begin
      filename:=Lua_ToString(L,1);
      try
        ml:=TModuleLoader.create(filename);
        ml.createSymbolListHandler;
      except
        on e: exception do
        begin
          lua_pushnil(L);
          lua_pushstring(L,e.message);
          exit(2);
        end;
      end;
    end
    else
    if lua_isuserdata(L,1) then
    begin
      o:=lua_toceuserdata(L, 1);
      if o is TLuafile then
        o:=tluafile(o).stream;

      if o is TMemoryStream then       //(memstream, filename, executeEntrypoint, timeout)
      begin
        if lua_gettop(L)>=2 then
          filename:=Lua_ToString(L,2);

        ml:=TModuleLoader.create(ms, filename);
        ml.createSymbolListHandler;
        paramoffset:=1;
      end;
    end;

    if lua_gettop(L)>=2+paramoffset then
      executeEntryPoint:=lua_toboolean(L,2+paramoffset)
    else
      executeEntryPoint:=true;

    if lua_gettop(L)>=3+paramoffset then
    begin
      if lua_isnil(L,3+paramoffset) then
        useTimeout:=false
      else
      begin
        useTimeout:=true;
        timeout:=lua_tointeger(L,3+paramoffset);
      end;
    end
    else
      useTimeout:=false;


    if executeEntryPoint then
    begin
      lua_getglobal(L,'executeCodeEx');
      if not lua_isfunction(L,-1) then
      begin
        lua_pop(L,lua_gettop(L));
        lua_pushnil(L);
        lua_pushstring(L,'executeCodeEx invalid');
        exit(2);
      end;


      lua_pushinteger(L,0); //stdcall
      if usetimeout then
        lua_pushinteger(L,timeout) //timeout
      else
        lua_pushnil(L); //timeout:infinite
      lua_pushinteger(L,ml.EntryPoint); //address

      lua_newtable(L); //hinstance
      lua_pushstring(L, 'type');
      lua_pushinteger(L,0); //default int type
      lua_settable(L,-3);
      lua_pushstring(L, 'value');
      lua_pushinteger(L,ml.BaseAddress); //hinstance
      lua_settable(L,-3);

      lua_newtable(L); //dwReason
      lua_pushstring(L, 'type');
      lua_pushinteger(L,0); //default int type
      lua_settable(L,-3);
      lua_pushstring(L, 'value');
      lua_pushinteger(L,DLL_PROCESS_ATTACH);
      lua_settable(L,-3);

      lua_newtable(L); //reserved
      lua_pushstring(L, 'type');
      lua_pushinteger(L,0);
      lua_settable(L,-3);
      lua_pushstring(L, 'value');
      lua_pushinteger(L,0); //init to 0
      lua_settable(L,-3);


      lua_pcall(L,6,1,0);
    end;


    luaclass_newClass(L,ml);
    result:=1;
  end;
end;

function moduleloader_getExports(L: PLua_State): integer; cdecl;
var
  ml: TModuleLoader;
  i: integer;
begin
  ml:=luaclass_getClassObject(L);
  lua_newtable(L);
  for i:=0 to ml.Exporttable.Count-1 do
  begin
    lua_pushstring(L, ml.Exporttable[i]);
    lua_pushinteger(L, ptruint(ml.Exporttable.objects[i]));
    lua_settable(L,-3);
  end;
  result:=1;
end;

procedure moduleloader_addMetadata(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  Luaclass_addPropertyToTable(L, metatable, userdata, 'Exports', moduleloader_getExports, nil);
end;

procedure initializeLuaModuleLoader;
begin
  lua_register(LuaVM, 'loadModule', moduleloader_createModuleLoader);

end;


initialization
  luaclass_register(TModuleLoader, moduleloader_addMetadata);
{$ENDIF}

end.

