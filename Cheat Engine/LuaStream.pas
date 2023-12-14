unit LuaStream;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lauxlib, lualib, math;

procedure stream_addMetaData(L: PLua_state; metatable: integer; userdata: integer );

implementation

uses LuaClass, LuaHandler, LuaObject{$ifdef darwin},mactypes{$endif};

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
  oldsize: integer;
  newsize: integer;
  diff: integer;
begin
  result:=0;
  stream:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    newsize:=lua_tointeger(L,1);
    if stream is TMemoryStream then
    begin
      oldsize:=stream.size;
      stream.size:=newsize;

      //zero the new bytes
      FillByte(pointer(ptruint(tmemorystream(stream).Memory)+oldsize)^, newsize-oldsize,0);
    end
    else
      stream.Size:=newsize;
  end;
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
  try
    lua_pushinteger(L,stream.ReadByte);
  except
    lua_pushstring(L,'stream error');
    lua_error(L);
  end;
  result:=1;
end;

function stream_writeByte(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
begin
  stream:=luaclass_getClassObject(L);
  try
    stream.WriteByte(lua_tointeger(L,1));

  finally
  end;
  result:=0;
end;

function stream_readWord(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
begin
  stream:=luaclass_getClassObject(L);
  try
    lua_pushinteger(L,stream.ReadWord);
  except
    lua_pushstring(L,'stream error');
    lua_error(L);
  end;
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
  try
    lua_pushinteger(L,stream.ReadDword);
  except
    lua_pushstring(L,'stream error');
    lua_error(L);
  end;
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
  try
    lua_pushinteger(L,stream.ReadQword);
  except
    lua_pushstring(L,'stream error');
    lua_error(L);
  end;
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

function stream_readFloat(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
  f: single;
begin
  stream:=luaclass_getClassObject(L);
  try
    stream.Read(f,sizeof(f));
  except
    lua_pushstring(L,'stream error');
    lua_error(L);
  end;

  lua_pushnumber(L,f);
  result:=1;
end;

function stream_writeFloat(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
  f: single;
begin
  stream:=luaclass_getClassObject(L);
  f:=lua_tonumber(L,1);
  stream.Write(f, sizeof(f));
  result:=0;
end;

function stream_readDouble(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
  d: double;
begin
  stream:=luaclass_getClassObject(L);
  try
    stream.Read(d,sizeof(d));
  except
    lua_pushstring(L,'stream error');
    lua_error(L);
  end;

  lua_pushnumber(L,d);
  result:=1;
end;

function stream_writeDouble(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
  d: double;
begin
  stream:=luaclass_getClassObject(L);
  d:=lua_tonumber(L,1);
  stream.Write(d, sizeof(d));
  result:=0;
end;



function stream_readString(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
  size: integer;
  s: pchar;
begin
  stream:=luaclass_getClassObject(L);
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    size:=lua_tointeger(L,1);
    getmem(s,size+1);
    stream.ReadBuffer(s^,size);
    s[size]:=#0;

    lua_pushstring(L,s);
    result:=1;
  end
  else
  begin
    lua_pushnil(L);
    lua_pushstring(L,rsIncorrectNumberOfParameters);
  end;
end;

function stream_writeString(L: PLua_State): integer; cdecl;
var
  stream: Tstream;
  s: string;
  includeterminator: boolean;
begin
  stream:=luaclass_getClassObject(L);
  result:=0;

  if lua_gettop(L)>=1 then
  begin
    s:=Lua_ToString(L,1);
    if lua_gettop(L)>=2 then
      includeterminator:=lua_toboolean(L,2)
    else
      includeterminator:=false;

    stream.WriteBuffer(s[1],length(s));
    if includeterminator then
      stream.WriteByte(0);
  end
  else
  begin
    lua_pushnil(L);
    lua_pushstring(L,rsIncorrectNumberOfParameters);
  end;
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
      {$if FPC_FULLVERSION < 030200}
        count:=min(int64(lua_objlen(L, 1)), int64(lua_tointeger(L, 2)))
      {$else}
        count:=min(lua_objlen(L, 1), lua_tointeger(L, 2)) //prevent the length from exeeding the table
      {$endif}
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
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readFloat', stream_readFloat);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeFloat', stream_writeFloat);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readDouble', stream_readDouble);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeDouble', stream_writeDouble);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readString', stream_readString);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeString', stream_writeString);
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

function memorystream_loadFromFileNoError(L: PLua_State): integer; cdecl;
var
  ms: Tmemorystream;
  r: boolean=false;
begin
  result:=0;
  ms:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  try
    ms.LoadFromFile(Lua_ToString(L,1));
    lua_pushboolean(L,true);
    result:=1;
  except
    on e:exception do
    begin
      lua_pushboolean(L,false);
      lua_pushstring(L,e.message);
      result:=2;
    end;
  end;
end;

function memorystream_saveToFileNoError(L: PLua_State): integer; cdecl;
var ms: Tmemorystream;
begin
  result:=0;

  ms:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  try
    ms.SaveToFile(Lua_ToString(L,1));
    lua_pushboolean(L,true);
    result:=1;
  except
    on e:exception do
    begin
      lua_pushboolean(L,false);
      lua_pushstring(L,e.message);
      result:=2;
    end;
  end;

end;

function memorystream_clear(L: PLua_State): integer; cdecl;
begin
  tmemorystream(luaclass_getClassObject(L)).Clear;
  result:=0;
end;

procedure memorystream_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  stream_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'loadFromFile', memorystream_loadFromFile);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'saveToFile', memorystream_saveToFile);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'loadFromFileNoError', memorystream_loadFromFileNoError);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'saveToFileNoError', memorystream_saveToFileNoError);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clear', memorystream_clear);
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
  memorystream_addMetaData(L, metatable, userdata);
  luaclass_addPropertyToTable(L, metatable, userdata, 'DataString', stringstream_getDataString, nil);
end;


initialization
  luaclass_register(TStream, stream_addMetaData);
  luaclass_register(TMemoryStream, memorystream_addMetaData);
  luaclass_register(TStringStream, stringstream_addMetaData);

end.

