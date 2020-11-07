unit LuaSettings;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lauxlib, lualib, registry;

procedure initializeLuaSettings;

implementation

uses luahandler, LuaClass, LuaObject, LuaByteTable;

type
  TLuaSettings=class    //A wrapper for the registry object to make access to the cheat engine settings easier and uniform
  private
    freg: Tregistry;
    fpath: string;
    procedure setPath(v: string);
    function getValue(index: string): string;
    procedure setValue(index: string; value: string);
    procedure setBinaryValue(index: string; stream: TStream; size: integer);
    procedure getBinaryValue(index: string; stream: TStream);
  public
    procedure delete(index: string);
    constructor create(initialpath: pchar);
  published
    property path:string read fPath write setPath;
    property value[index: string]: string read getValue write setValue;
  end;

procedure TLuaSettings.setPath(v: string);
begin
  if pos('..',v)>0 then
    exit;

  if freg.OpenKey('\Software\Cheat Engine\'+v, true) then
    fpath:=v;
end;

procedure TLuaSettings.setBinaryValue(index: string; stream: TStream; size: integer);
var
  buffer: pointer;
begin
  if size=0 then
  begin
    stream.Seek(0, soFromBeginning);
    size:=stream.Size;
  end;

  if size>stream.Size then
    size:=stream.size;

  getmem(buffer, size);
  stream.ReadBuffer(buffer^,size);

  try
    freg.WriteBinaryData(index,buffer^, size);
  except
  end;

  freemem(buffer);
end;

procedure TLuaSettings.getBinaryValue(index: string; stream: TStream);
var
  size: integer;
  buf: pointer;
begin
  if freg.ValueExists(index) then
  begin
    size:=freg.GetDataSize(index);
    if size>0 then
    begin
      getmem(buf,size);
      try
        freg.ReadBinaryData(index,buf^,size);
        stream.WriteBuffer(buf^,size);
      except
      end;
      freemem(buf);
    end;
  end;
end;

function TLuaSettings.getValue(index: string): string;
begin
  result:='';
  {$ifdef darwin}
  setpath(fpath);
  {$endif}
  if freg.ValueExists(index) then
  begin
    case freg.GetDataType(index) of
      rdString, rdExpandString: result:=freg.ReadString(index);
      rdInteger: result:=inttostr(freg.ReadInteger(index));
    end;
  end;
end;

procedure TLuaSettings.setValue(index: string; value: string);
begin
  {$ifdef darwin}
  setpath(fpath);
  {$endif}
  if freg.ValueExists(index) then
  begin
    case freg.GetDataType(index) of
      rdString, rdExpandString: freg.WriteString(index, value);
      rdInteger:
      begin
        try
          freg.WriteInteger(index, strtoint(value));
        except
        end;
      end;
    end;
  end
  else
    freg.WriteString(index, value);
end;

procedure TLuaSettings.delete(index: string);
begin
  try
    freg.DeleteValue(index);
  except
  end;
end;

constructor TLuaSettings.create(initialpath: pchar);
begin
  freg:=TRegistry.Create;
  freg.RootKey:=HKEY_CURRENT_USER;

  if initialpath<>nil then
    path:=initialpath
  else
    freg.OpenKey('\Software\Cheat Engine',true);


end;

function  getSettings(L: Plua_State): integer; cdecl;
begin
  if lua_gettop(l)>0 then
    luaclass_newClass(L, TLuaSettings.create(pchar(Lua_ToString(L,1))))
  else
    luaclass_newClass(L, TLuaSettings.create(nil));

  result:=1;
end;

function luasettings_getValue(L: PLua_State): integer; cdecl;
var
  s: TLuaSettings;
  index: string;
begin
  result:=0;
  s:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    index:=lua_toString(L,-1);
    lua_pushstring(L, s.value[index]);
    result:=1;
  end;
end;

function luasettings_setValue(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  s: TLuaSettings;
  index: string;
  newvalue: string;
  paramstart, paramcount: integer;
begin
  result:=0;
  s:=luaclass_getClassObject(L, @paramstart, @paramcount);
  if paramcount>=2 then
  begin
    index:=lua_tostring(L,paramstart);
    if lua_isnil(L,paramstart+1) then //delete
    begin
      s.delete(index);
      exit;
    end
    else
    if lua_isboolean(L, paramstart+1) then
      newvalue:=BoolToStr(lua_toboolean(L,paramstart+1), '1','0')
    else
      newvalue:=lua_tostring(l,paramstart+1);

    s.value[index]:=newvalue;
  end;
end;

function luasettings_getBinaryValue(L: PLua_State): integer; cdecl;
var
  s: TLuaSettings;
  index: string;
  stream: TStream;
  mstream: TMemoryStream;
begin
  result:=0;
  s:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    index:=Lua_ToString(L,1);

    if lua_gettop(L)>=2 then
    begin
      if lua_isuserdata(L,2) then
      begin
        stream:=lua_ToCEUserData(L,2);
        if stream is tstream then
          s.getBinaryValue(index, stream);
      end;
    end
    else
    begin
      //bytetable result
      mstream:=tmemorystream.create;
      s.getBinaryValue(index, mstream);
      CreateByteTableFromPointer(L,mstream.Memory, mstream.Size);
      result:=1;
      mstream.free;
    end;
  end;
end;

function luasettings_setBinaryValue(L: PLua_State): integer; cdecl;
var
  s: TLuaSettings;
  index: string;
  stream: tstream;
  mstream: TMemoryStream;
  size: integer;
  buffer: pointer;
begin
  result:=0;
  s:=luaclass_getClassObject(L);

  if lua_gettop(L)>=2 then
  begin
    index:=Lua_ToString(L,1);

    if lua_isuserdata(L,2) then
    begin
      stream:=lua_ToCEUserData(L,2);
      if stream is tstream then
      begin
        if lua_gettop(L)>=3 then
          size:=lua_tointeger(L,3)
        else
          size:=0;

        s.setBinaryValue(index, stream, size);
      end;
    end
    else
    if lua_istable(L,2) then
    begin
      //bytetable
      size:=lua_objlen(L, 2);
      getmem(buffer, size);
      readBytesFromTable(L,2,buffer, size);

      mstream:=tmemorystream.create;
      mstream.WriteBuffer(buffer^, size);
      s.setBinaryValue(index, mstream, 0);
      mstream.free;
      freemem(buffer);
    end;
  end;
end;

procedure luasettings_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'setBinaryValue', luasettings_setBinaryValue);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getBinaryValue', luasettings_getBinaryValue);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Value', luasettings_getValue, luasettings_setValue);
  luaclass_setDefaultStringArrayProperty(L, metatable, userdata, luasettings_getValue, luasettings_setValue);
end;

procedure initializeLuaSettings;
begin
  lua_register(LuaVM, 'getSettings', getSettings);
end;

initialization
  luaclass_register(TLuaSettings, luasettings_addMetaData);


end.

