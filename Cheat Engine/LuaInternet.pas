unit LuaInternet;

{$mode delphi}

interface




uses
  Classes, SysUtils
  {$ifdef windows}, wininet
  {$else}
  , fphttpclient,opensslsockets,openssl, StringHashList

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
    fname: string;
    internet: TFPHTTPClient;


    procedure setHeader(s: string);
    function getHeader: string;
    procedure recreateFPHTTPClient;
    procedure storeCookies(urlstring: string);
    procedure loadCookies(urlstring: string);
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

uses {$ifdef darwin}macport, registry,{$endif}{$ifndef standalone}MainUnit2, lua, LuaClass, LuaObject, LuaHandler,{$endif} URIParser, dialogs;

{$ifndef windows}
var cookies: tstringhashlist;
{$endif}

{$ifndef windows}
procedure TWinInternet.setHeader(s: string);
begin
  internet.RequestHeaders.Text:=s;
end;

function TWinInternet.getHeader: string;
begin
  result:=internet.RequestHeaders.Text;
end;

procedure TWinInternet.storeCookies(urlstring: string);
var
  uri: TURI;
  c: tstringlist;
  i,j: integer;
  s: string;
  cname,cname2: string;
  cvalue,cvalue2: string;
  found: boolean;

  p: integer;
begin
  if internet.Cookies.Text='' then exit; //no new cookies

  uri:=ParseURI(urlstring);
  c:=cookies[uri.Host];
  if c=nil then
  begin
    c:=tstringlist.create;
    cookies[uri.Host]:=c;
  end;


  //update the cookies with the new values
  for i:=0 to internet.ResponseHeaders.count-1 do
  begin
    s:=internet.ResponseHeaders[i];
    if (LowerCase(Copy(S,1,10))='set-cookie') then
    begin
      Delete(s,1,Pos(':',S));

      cname:=trim(copy(s,1,pos('=',s)-1));


      cvalue:=trim(copy(s,pos('=',s)+1));
      cvalue:=copy(cvalue,1,pos(';',cvalue)-1);

      found:=false;
      for j:=0 to c.Count-1 do
      begin
        s:=c[j];
        cname2:=trim(copy(s,1,pos('=',s)-1));
        if cname=cname2 then
        begin
          c[j]:=cname+'='+cvalue; //update
          found:=true;
          break;
        end;
      end;

      if not found then
        c.add(cname+'='+cvalue);
    end;
  end;


end;

procedure TWinInternet.loadCookies(urlstring: string);
var
  uri: TURI;
  c: tstringlist;
begin
  uri:=ParseURI(urlstring);

  c:=cookies[uri.Host];
  if c<>nil then
  begin
    internet.Cookies.Text:=c.text;
  end;
end;

function TWinInternet.postURL(urlstring: string; urlencodedpostdata: string; results: tstream): boolean;
begin
  result:=false;
  try
    recreateFPHTTPClient;
    loadCookies(urlstring);

    internet.FormPost(urlstring,urlencodedpostdata,results);

    storeCookies(urlstring);

    result:=true;
  except
    result:=false;
  end;
end;

function TWinInternet.getURL(urlstring: string; results: tstream): boolean;
begin
  result:=false;
  try
    loadCookies(urlstring);
    internet.Get(urlstring, results);

    storeCookies(urlstring);
    result:=true;
  except

  end;

end;

procedure TWinInternet.recreateFPHTTPClient;
begin
  if internet<>nil then
    freeandnil(internet);

  internet:=TFPHTTPClient.Create(nil);
  internet.AddHeader('User-Agent',fname);
  internet.AllowRedirect:=true;
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



  fname:=name;
  openssl.DLLVersions[1]:=openssl.DLLVersions[2];
  openssl.DLLVersions[1]:='.46';
  openssl.DLLVersions[2]:='.44';
  InitSSLInterface;
  openssl.ErrClearError;

  recreateFPHTTPClient;

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

{$ifndef windows}
procedure loadCookiesFromRegistry;
var
  r: TRegistry;
  names: tstringlist;
  i: integer;
  c: tstringlist;
begin
  macPortFixRegPath;
  r:=tregistry.Create;
  r.RootKey:=HKEY_CURRENT_USER;
  try
    if r.OpenKey('Cookies',false) then
    begin
      names:=tstringlist.create;

      try
        r.GetValueNames(names);
        for i:=0 to names.count-1 do
        begin
          c:=tstringlist.create;
          r.ReadStringList(names[i],c);
          cookies[names[i]]:=c;
        end;
      finally
        names.free;
      end;
    end;

  except
  end;

  r.free;
end;

procedure saveCookiesToRegistry;
var
  r: TRegistry;
  i: integer;

  e: PStringHashItem;
  host: string;
begin
  r:=TRegistry.Create;
  r.RootKey:=HKEY_CURRENT_USER;

  if r.OpenKey('Cookies',true) then
  begin
    for i:=0 to cookies.count-1 do
    begin
      e:=cookies.List[i];
      if e<>nil then
      begin
        host:=e^.Key;
        r.WriteStringList(host,tstringlist(e^.Data));
      end;

    end;

  end;
  r.free;
end;
{$endif}

initialization
  {$ifndef windows}
  cookies:=TStringHashList.Create(false);
  loadCookiesFromRegistry;
  {$endif}

  luaclass_register(TWinInternet, wininternet_addMetaData);

  {$ifndef windows}
finalization

  saveCookiesToRegistry;
  {$endif}
{$endif}

end.

