unit LuaSettings;

{$mode delphi}

interface

uses
  Classes, SysUtils, lua, lauxlib, lualib, registry;

procedure initializeLuaSettings;

implementation

uses luahandler, LuaClass, LuaObject;

type
  TLuaSettings=class    //A wrapper for the registry object to make access to the cheat engine settings easier and uniform
  private
    freg: Tregistry;
    fpath: string;
    procedure setPath(v: string);
    function getValue(index: string): string;
    procedure setValue(index: string; value: string);
  public
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
    if lua_isboolean(L, paramstart+1) then
      newvalue:=BoolToStr(lua_toboolean(L,paramstart+1), '1','0')
    else
      newvalue:=lua_tostring(l,paramstart+1);

    s.value[index]:=newvalue;
  end;
end;

procedure luasettings_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
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

