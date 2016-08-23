unit lua_server;
{
This unit will setup a communication to the lua engine
It will be responsible for receiving and dispatching messages

lua server waits for pipe connections and when made spawns a new thread(TLuaServerHandler) which
handles the communication while it itself is going back to listen to new connections.

}

{$mode delphi}

interface

uses
  jwawindows, windows, Classes, SysUtils, lua, lauxlib, lualib, LuaHandler;

type
  TLuaServerHandler=class(TThread)
  private
    pipe: THandle;
    exec: tstringlist;
    result: qword;

    returncount: byte;
    results: array of qword;
    procedure ExecuteLuaScript;
    procedure ExecuteLuaScriptVar;
    //executeLuaFunction
    procedure ExecuteScript;
  protected
    procedure execute; override;

  public
    constructor create(pipe: THandle);
    destructor destroy; override;
  end;

  TLuaServer=class(TThread)
  private
    fname: string;
  protected
    procedure execute; override;
  public
    constructor create(name: string);
    destructor destroy; override;
    property name: string read fname;
  end;

 // TLuaServers =  TFPGList<TLuaServer>;

var luaservers: TList;

function luaserverExists(name: string): boolean;

implementation

resourcestring
  rsALuaserverWithTheName = 'A luaserver with the name ';
  rsAlreadyExists = ' already exists';

function luaserverExists(name: string): boolean;
var i: integer;
begin
  result:=true;
  for i:=0 to luaservers.count-1 do
    if TLuaServer(luaservers[i]).name=name then exit;

  result:=false;
end;



//--------TLuaServerHandler--------

constructor TLuaServerHandler.create(pipe: THandle);
begin
  FreeOnTerminate:=true;
  self.pipe:=pipe;
  exec:=TStringlist.create;
  inherited create(false);
end;

destructor TLuaServerHandler.destroy;
begin
  exec.free;
  inherited destroy;
end;

procedure TLuaServerHandler.ExecuteScript;
var
  i,j: integer;
  top: integer;

begin
  LuaCS.Enter;
  try
    top:=lua_gettop(Luavm);
    i:=luahandler.lua_dostring(luavm, pchar(exec.text));
    if i=0 then
      result:=lua_tointeger(Luavm, -1)
    else
      result:=0;

    if returncount>0 then
    begin
      if length(results)<returncount then
        setlength(results, returncount);

      for i:=0 to returncount-1 do
        results[(returncount-1)-i]:=lua_tointeger(Luavm, -1-i);

    end;

    lua_settop(Luavm, top);

  finally
    luacs.leave;
  end;
end;


{
todo: ExecuteLuaScriptEx
Variable paramcount
setup:
functionref: byte
if functionref=0 then
  functionnamelength: byte
  functionname[functionnamelength]: char
end

paramcount: byte
params[paramcount]: record
    paramtype: byte  - 0=nil, 1=integer64, 2=double, 3=string,  4=table perhaps ?
    value:
      --if paramtype=2 then
      stringlength: word
      string[strinbglength]: char
      --else
      value: 8byte
  end

returncount: byte


--returns:
actualreturncount: byte

}

procedure TLuaServerHandler.ExecuteLuaScriptVar;
{
Same as ExecuteLuaScript but can return more than one return value qword
}
  procedure error;
  begin
    OutputDebugString('Read error');
    terminate;
  end;

var
  scriptsize: integer;
  br: dword;
  script: pchar;

  parameter: qword;
  i: integer;
begin
  if readfile(pipe, scriptsize, sizeof(scriptsize), br, nil) then
  begin
    getmem(script, scriptsize+1);

    try
      if readfile(pipe, script^, scriptsize, br, nil) then
      begin
        script[scriptsize]:=#0;

        if readfile(pipe, parameter, 8, br, nil) then
        begin
          if readfile(pipe, returncount, 1, br, nil) then
          begin
            exec.clear;
            exec.Text:=script;

            exec.Insert(0, 'function _luaservercall'+inttostr(GetCurrentThreadId)+'(parameter)');
            exec.add('end');
            exec.add('return _luaservercall'+inttostr(GetCurrentThreadId)+'('+inttostr(parameter)+')');

            setlength(results, returncount);
            synchronize(executescript);

            for i:=0 to returncount-1 do
              if writefile(pipe, results[i], 8, br, nil)=false then error;

          end;
        end
        else
          error;
      end
      else
        error;

    finally
      freemem(script);
    end;
  end
  else
    error;
end;

procedure TLuaServerHandler.ExecuteLuaScript;
  procedure error;
  begin
    OutputDebugString('Read error');
    terminate;
  end;

var
  scriptsize: integer;
  br: dword;
  script: pchar;

  parameter: qword;
begin
  returncount:=1;
  if readfile(pipe, scriptsize, sizeof(scriptsize), br, nil) then
  begin
    getmem(script, scriptsize+1);

    try
      if readfile(pipe, script^, scriptsize, br, nil) then
      begin
        script[scriptsize]:=#0;

        if readfile(pipe, parameter, 8, br, nil) then
        begin
          exec.clear;
          exec.Text:=script;

          exec.Insert(0, 'function _luaservercall'+inttostr(GetCurrentThreadId)+'(parameter)');
          exec.add('end');
          exec.add('return _luaservercall'+inttostr(GetCurrentThreadId)+'('+inttostr(parameter)+')');

          synchronize(executescript);

          if writefile(pipe, result, 8, br, nil)=false then
            error;
        end
        else
          error;
      end
      else
        error;

    finally
      freemem(script);
    end;
  end
  else
    error;

end;

procedure TLuaServerHandler.execute;
var
  command: byte;
  br: dword;
begin
  try
    while not terminated do
    begin
      ReadFile(pipe, command, sizeof(command), br, nil);
      case command of
        1: ExecuteLuaScript;
        2: ExecuteLuaScriptVar;
        else terminate;
      end;
    end;


  finally
    CloseHandle(pipe);
  end;
end;


//--------TLuaServer--------

procedure TLuaServer.execute;
var
  pipe: THandle;
  a: SECURITY_ATTRIBUTES;
begin
  while not terminated do
  begin
    ZeroMemory(@a, sizeof(a));
    a.nLength:=sizeof(a);
    a.bInheritHandle:=TRUE;

    //got this string from https://www.osronline.com/showThread.CFM?link=204207
    ConvertStringSecurityDescriptorToSecurityDescriptor('D:(D;;FA;;;NU)(A;;0x12019f;;;WD)(A;;0x12019f;;;CO)', SDDL_REVISION_1, a.lpSecurityDescriptor, nil);

    pipe:=CreateNamedPipe(pchar('\\.\pipe\'+name), PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE or PIPE_READMODE_BYTE or PIPE_WAIT, 255, 16, 8192, 0, @a );
    LocalFree(HLOCAL(a.lpSecurityDescriptor));


    if ConnectNamedPipe(pipe, nil) or (GetLastError = ERROR_PIPE_CONNECTED) then
    begin
      //connected
      //send this pipe of to the handler and create a new pipe
      TLuaServerHandler.create(pipe);
    end
    else
    begin
      OutputDebugString('Lua server connect error');
      CloseHandle(pipe); //failure, try again
    end;
  end;
end;

constructor TLuaServer.create(name: string);
var i: integer;
begin
  fname:=name;

  if luaserverExists(name) then
    raise exception.create(rsALuaserverWithTheName+name+rsAlreadyExists);

  luaservers.Add(self);


  inherited create(false);
end;

destructor TLuaServer.destroy;
begin
  inherited destroy;
end;

initialization
  luaservers:=TList.create;


end.

