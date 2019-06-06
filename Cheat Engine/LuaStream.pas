unit LuaStream;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lauxlib, lualib, math;

procedure stream_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses LuaClass, LuaHandler, LuaObject;

function stream_getSize(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
begin
  stream:=luaclass_getClassObject(L);
  lua_pushinteger(L, stream.Size);
  result:=1;

end;

function stream_setSize(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
begin
  result:=0;
  stream:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    stream.Size:=lua_tointeger(L,-1);
end;

function stream_getPosition(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
begin
  stream:=luaclass_getClassObject(L);
  lua_pushinteger(L, stream.Position);
  result:=1;

end;

function stream_setPosition(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
begin
  result:=0;
  stream:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
    stream.Position:=lua_tointeger(L,-1);
end;

function stream_copyFrom(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
  s: tstream;
  count: integer;
begin
  result:=0;
  stream:=luaclass_getClassObject(L);
  if lua_gettop(L)=2 then
  begin
    s:=lua_ToCEUserData(L, 1);
    count:=lua_tointeger(L, 2);
    stream.CopyFrom(s, count);
  end;
end;

function stream_read(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
  count: integer;

  buf: PByteArray;
  table: integer;
  i: integer;
begin
  result:=0;
  stream:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    if lua_isnumber(L, 1) then
    begin
      count:=lua_tointeger(L, 1);
      getmem(buf, count);
      try
        stream.Read(buf^, count);

        //everything ok, create a table
        lua_newtable(L); //pobably 2
        table:=lua_gettop(L);

        for i:=0 to count-1 do
        begin
          lua_pushinteger(L, i+1);
          lua_pushinteger(L, buf[i]);
          lua_settable(L, table); //set table[i+1]=buf[i]
        end;

        result:=1;

      except
      end;
      FreeMemAndNil(buf);

    end;
  end;
end;

function stream_readByte(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
begin
  stream:=luaclass_getClassObject(L);
  lua_pushinteger(L,stream.ReadByte);
  result:=1;
end;

function stream_writeByte(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
begin
  stream:=luaclass_getClassObject(L);
  stream.WriteByte(lua_tointeger(L,1));
  result:=0;
end;

function stream_readWord(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
begin
  stream:=luaclass_getClassObject(L);
  lua_pushinteger(L,stream.ReadWord);
  result:=1;
end;

function stream_writeWord(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
begin
  stream:=luaclass_getClassObject(L);
  stream.WriteWord(lua_tointeger(L,1));
  result:=0;
end;

function stream_readDword(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
begin
  stream:=luaclass_getClassObject(L);
  lua_pushinteger(L,stream.ReadDword);
  result:=1;
end;

function stream_writeDword(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
begin
  stream:=luaclass_getClassObject(L);
  stream.WriteDword(lua_tointeger(L,1));
  result:=0;
end;

function stream_readQword(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
begin
  stream:=luaclass_getClassObject(L);
  lua_pushinteger(L,stream.ReadQword);
  result:=1;
end;

function stream_writeQword(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
begin
  stream:=luaclass_getClassObject(L);
  stream.WriteQword(lua_tointeger(L,1));
  result:=0;
end;

function stream_readAnsiString(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
begin
  stream:=luaclass_getClassObject(L);
  lua_pushstring(L,stream.ReadAnsiString);
  result:=1;
end;

function stream_writeAnsiString(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
begin
  stream:=luaclass_getClassObject(L);
  stream.WriteAnsiString(Lua_ToString(L,1));
  result:=0;
end;

function stream_write(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
  count: integer;

  buf: PByteArray;
  i: integer;
begin
  result:=0;
  stream:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    //table index is at 1
    if lua_istable(L, 1) then
    begin
      if lua_gettop(L)>=2 then
        count:=min(lua_objlen(L, 1), lua_tointeger(L, 2)) //prevent the length from exeeding the table
      else
        count:=lua_objlen(L, 1);

      getmem(buf, count);
      try
        for i:=0 to count-1 do
        begin
          lua_pushinteger(L, i+1);
          lua_gettable(L, 1); //get table[i+1]
          buf[i]:=lua_tointeger(L, -1);
          lua_pop(L, 1);
        end;
        stream.Write(buf^, count);
      except
      end;
      FreeMemAndNil(buf);

    end;
  end;
end;

procedure stream_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'copyFrom', stream_copyFrom);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'read', stream_read);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'write', stream_write);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readByte', stream_readByte);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeByte', stream_writeByte);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readWord', stream_readWord);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeWord', stream_writeWord);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readDword', stream_readDword);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeDword', stream_writeDword);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readQword', stream_readQword);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeQword', stream_writeQword);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readAnsiString', stream_readAnsiString);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeAnsiString', stream_writeAnsiString);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Size', stream_getSize, stream_setSize);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Position', stream_getPosition, stream_setPosition);
end;

function memorystream_getMemory(L: PLua_State): integer; cdecl;
var ms: Tmemorystream;
begin
  ms:=luaclass_getClassObject(L);
  lua_pushinteger(L, ptruint(ms.Memory));
  result:=1;
end;

function memorystream_loadFromFile(L: PLua_State): integer; cdecl;
var ms: Tmemorystream;
begin
  ms:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    ms.LoadFromFile(Lua_ToString(L,1));

  result:=0;
end;

function memorystream_saveToFile(L: PLua_State): integer; cdecl;
var ms: Tmemorystream;
begin
  ms:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
    ms.SaveToFile(Lua_ToString(L,1));

  result:=0;
end;

procedure memorystream_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  stream_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'loadFromFile', memorystream_loadFromFile);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'saveToFile', memorystream_saveToFile);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Memory', memorystream_getMemory, nil);
end;

function stringstream_getDataString(L: PLua_State): integer; cdecl;
var ss: TStringStream;
  ms: TMemoryStream;
  oldpos: integer;
begin
  ss:=luaclass_getClassObject(L);
  ms:=TMemoryStream.create;
  oldpos:=ss.Position;

  ss.position:=0;
  ms.LoadFromStream(ss);
  lua_pushlstring(L, ms.Memory, ms.Size);
  result:=1;

  ss.position:=oldpos;
  ms.free;
end;

procedure stringstream_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  stream_addMetaData(L, metatable, userdata);
  luaclass_addPropertyToTable(L, metatable, userdata, 'DataString', stringstream_getDataString, nil);
end;


initialization
  luaclass_register(TStream, stream_addMetaData);
  luaclass_register(TMemoryStream, memorystream_addMetaData);
  luaclass_register(TStringStream, stringstream_addMetaData);

end.

