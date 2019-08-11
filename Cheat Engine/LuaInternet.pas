unit LuaInternet;

{$mode delphi}

interface

uses
  Classes, SysUtils, wininet;

procedure initializeLuaInternet;

type
  TWinInternet=class
  private
    internet: HINTERNET;
    fheader: string;
  public
    function getURL(urlstring: string; results: tstream): boolean;
    function postURL(urlstring: string; urlencodedpostdata: string; results: tstream): boolean;
    constructor create(name: string);
    destructor destroy; override;
  published
    property Header: string read fheader write fheader;
  end;

implementation

uses mainunit2, lua, LuaClass, LuaObject, luahandler, URIParser;



function TWinInternet.postURL(urlstring: string; urlencodedpostdata: string; results: tstream): boolean;
var
  url: HINTERNET;

  c,r: HInternet;
  available,actualread: dword;

  buf: PByteArray;

  u: TURI;

  port: dword;

  username, password: pchar;

  h: string;
  requestflags: dword;
  err: integer;
begin
  h:='Content-Type: application/x-www-form-urlencoded';

  result:=false;
  if internet<>nil then
  begin
    //InternetConnect(internet, );

    requestflags:=INTERNET_FLAG_PRAGMA_NOCACHE;

    u:=ParseURI(urlstring);

    port:=INTERNET_DEFAULT_HTTP_PORT;
    if lowercase(u.Protocol)='https' then
    begin
      port:=INTERNET_DEFAULT_HTTPS_PORT;
      requestflags:=requestflags or INTERNET_FLAG_SECURE;
    end
    else
    begin
//      requestflags:=requestflags or INTERNET_FLAG_NO_AUTO_REDIRECT;
    end;

    if u.Port<>0 then
      port:=u.port;

    if u.Username<>'' then
      username:=pchar(u.Username)
    else
      username:=nil;

    if u.Password<>'' then
      password:=pchar(u.Password)
    else
      password:=nil;

    if u.params<>'' then u.params:='?'+u.params;


    c:=InternetConnect(internet, pchar(u.Host), port, username, password, INTERNET_SERVICE_HTTP, 0,0);
    if c<>nil then
    begin
      r:=HttpOpenRequest(c, PChar('POST'), pchar(u.path+u.Document+u.Params), nil, nil, nil, requestflags, 0);
      if r<>nil then
      begin
        if HttpSendRequest(r,pchar(h),length(h),pchar(urlencodedpostdata), length(urlencodedpostdata)) then
        begin

          while InternetQueryDataAvailable(r, available, 0, 0) do
          begin
            if available=0 then break;

            getmem(buf, available+32);
            try
              if InternetReadFile(r, buf, available,actualread)=false then break;
              if actualread>0 then
              begin
                results.WriteBuffer(buf[0], actualread);
                result:=true; //something was read
              end;
            finally
              FreeMemAndNil(buf);
            end;
          end;
        end
        else
        begin
          err:=getLastOSError;

          if err=0 then beep;


        end;

        InternetCloseHandle(r);
      end;

      InternetCloseHandle(c);
    end;

  end;

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
      url:=InternetOpenUrl(internet, pchar(urlstring),nil,0,INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_RESYNCHRONIZE or INTERNET_FLAG_DONT_CACHE, 0)
    else
      url:=InternetOpenUrl(internet, pchar(urlstring), @fheader[1], length(fheader), INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_RESYNCHRONIZE or INTERNET_FLAG_DONT_CACHE,0);

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
        FreeMemAndNil(buf);
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

function wininternet_postURL(L: Plua_State): integer; cdecl;
var
  i: TWinInternet;
  s: TMemoryStream;
begin
  result:=0;
  i:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
  begin
    s:=TMemoryStream.create;
    try
      if i.postURL(Lua_ToString(L, 1), Lua_ToString(L, 2), s) then
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
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'postURL', wininternet_postURL);

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

