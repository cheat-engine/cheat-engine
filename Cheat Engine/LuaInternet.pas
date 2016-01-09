unit LuaInternet;

{$mode delphi}

interface

uses
  Classes, SysUtils, wininet;

procedure initializeLuaInternet;

implementation

uses mainunit2, lua, LuaClass, LuaObject, luahandler;

type
  TWinInternet=class
  private
    internet: HINTERNET;
    fheader: string;
  public
    function getURL(urlstring: string; results: tstream): boolean;
    constructor create(name: string);
    destructor destroy; override;
  published
    property Header: string read fheader write fheader;
  end;

function TWinInternet.getURL(urlstring: string; results: tstream): boolean;
var url: HINTERNET;
  available,actualread: dword;

  buf: PByteArray;
begin
  result:=false;
  if internet<>nil then
  begin
    if header='' then
      url:=InternetOpenUrl(internet, pchar(urlstring),nil,0,0, 0)
    else
      url:=InternetOpenUrl(internet, pchar(urlstring), @fheader[1], length(fheader), 0,0);

    if url=nil then exit;

    while InternetQueryDataAvailable(url, available, 0, 0) do
    begin
      if available=0 then break;

      getmem(buf, available+32);
      try
        if InternetReadFile(url, buf, available+32,actualread)=false then break;
        if actualread>0 then
        begin
          results.WriteBuffer(buf[0], actualread);
          result:=true; //something was read
        end;
      finally
        freemem(buf);
      end;
    end;

    InternetCloseHandle(url);
  end;
end;

constructor TWinInternet.create(name: string);
begin
  internet:=InternetOpen(pchar(name),0, nil, nil,0);
end;

destructor TWinInternet.destroy;
begin
  if internet<>nil then
    InternetCloseHandle(internet);

  inherited destroy;
end;

function getInternet(L: Plua_State): integer; cdecl;
begin
  if lua_gettop(l)>0 then
    luaclass_newClass(L, TWinInternet.create(cename+' : luascript-'+Lua_ToString(L,1)))
  else
    luaclass_newClass(L, TWinInternet.create(cename+' : luascript'));

  result:=1;
end;

function wininternet_getURL(L: Plua_State): integer; cdecl;
var
  i: TWinInternet;
  s: TMemoryStream;
begin
  result:=0;
  i:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    s:=TMemoryStream.create;
    try
      if i.getURL(Lua_ToString(L, -1), s) then
      begin
        lua_pushlstring(L, s.Memory, s.Size);
        result:=1;
      end;
    finally
      s.free;
    end;
  end;
end;

procedure wininternet_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getURL', wininternet_getURL);

end;

procedure initializeLuaInternet;
begin
  lua_register(LuaVM, 'getInternet', getInternet);
end;

var
  test: TWinInternet;
  r: TStringStream;

initialization
  luaclass_register(TWinInternet, wininternet_addMetaData);



end.

