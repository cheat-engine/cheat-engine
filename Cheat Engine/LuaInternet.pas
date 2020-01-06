unit LuaInternet;

{$mode delphi}

interface




uses
  Classes, SysUtils
  {$ifdef windows}, wininet
  {$else}
  , fphttpclient,opensslsockets,openssl

  {$endif}
  ;

{$ifndef standalone}
procedure initializeLuaInternet;
{$endif}

type
  TWinInternet=class
  private
    {$ifdef windows}
    internet: HINTERNET;
    fheader: string;
    {$else}
    internet: TFPHTTPClient;
    procedure setHeader(s: string);
    function getHeader: string;
    {$endif}
  public
    function getURL(urlstring: string; results: tstream): boolean;
    function postURL(urlstring: string; urlencodedpostdata: string; results: tstream): boolean;
    constructor create(name: string);
    destructor destroy; override;
  published
    {$ifdef windows}
    property Header: string read fheader write fheader;
    {$else}
    property Header: string read getHeader write setHeader;
    {$endif}
  end;


implementation

uses {$ifndef standalone}MainUnit2, lua, LuaClass, LuaObject, LuaHandler,{$endif} URIParser;

{$ifndef windows}
procedure TWinInternet.setHeader(s: string);
begin
  internet.RequestHeaders.Text:=s;
end;

function TWinInternet.getHeader: string;
begin
  result:=internet.RequestHeaders.Text;
end;

function TWinInternet.postURL(urlstring: string; urlencodedpostdata: string; results: tstream): boolean;
var response: tstrings;
begin
  result:=false;
  response:=tstringlist.Create;
  try
    internet.FormPost(urlstring,urlencodedpostdata,response);
    result:=true;
    results.WriteAnsiString(response.text);
  except
  end;

  freeandnil(response);

end;

function TWinInternet.getURL(urlstring: string; results: tstream): boolean;
begin
  result:=false;
  try
    internet.Get(urlstring, results);
    result:=true;
  except

  end;

end;

{$else}

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
{$endif}

constructor TWinInternet.create(name: string);
begin
  {$ifdef windows}
  internet:=InternetOpen(pchar(name),0, nil, nil,0);
  {$else}

  openssl.DLLVersions[1]:=openssl.DLLVersions[2];
  openssl.DLLVersions[1]:='.46';
  openssl.DLLVersions[2]:='.44';
  InitSSLInterface;
  openssl.ErrClearError;

  internet:=TFPHTTPClient.Create(nil);
  internet.AddHeader('User-Agent',name);
  {$endif}
end;

destructor TWinInternet.destroy;
begin
  if internet<>nil then
  {$ifdef windows}
    InternetCloseHandle(internet);
  {$else}
    freeandnil(internet);
  {$endif}


  inherited destroy;
end;

{$ifndef standalone}

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
{$endif}

end.

