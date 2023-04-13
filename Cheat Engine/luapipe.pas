unit LuaPipe;

{$mode delphi}

//pipe class specifically made for lua. Only 1 client and 1 server connection at a time

interface

uses
  {$ifdef darwin}
  macport,  macpipe, mactypes,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  Classes, SysUtils, lua, LuaClass, syncobjs, guisafecriticalsection, newkernelhandler;

type
  TPipeConnection=class
  private
    fOnTimeout: TNotifyEvent;
    fOnError: TNotifyEvent;
    procedure CloseConnection(n: TNotifyEvent);

    {$ifdef windows}
    function ProcessOverlappedOperation(o: POVERLAPPED): boolean;
    {$endif}
  protected
    pipe: THandle;
    fconnected: boolean;
    cs: TGuiSafeCriticalSection;
    foverlapped: boolean;
    ftimeout: integer;
    fWarnOnMainThreadLockObtaining: boolean; //to debug mainthread locks
    fErrorOnMainThreadLockObtaining: boolean;

    procedure setTimeout(newtimeout: integer);
  public
    procedure lock;
    procedure unlock;
    function WriteBytes(bytes: pointer; size: integer): boolean;
    function ReadBytes(bytes: pointer; size: integer): boolean;

    function readDouble: double;
    function readFloat: single;

    function readQword: qword;
    function readDword: dword;
    function readWord: word;
    function readByte: byte;

    function readString(size: integer): string;
    function readWideString(size: integer): widestring;

    procedure writeDouble(v: double);
    procedure writeFloat(v: single);
    procedure writeQword(v: qword);
    procedure writeDword(v: dword);
    procedure writeWord(v: word);
    procedure writeByte(v: byte);

    procedure writeString(str: string; include0terminator: boolean);
    procedure writeWideString(str: widestring; include0terminator: boolean);
    constructor create;
    destructor destroy; override;
  published
    property connected: boolean read fConnected;
    property Timeout: Integer read ftimeout write setTimeout;
    property OnTimeout: TNotifyEvent read fOnTimeout write fOnTimeout;
    property OnError: TNotifyEvent read fOnError write fOnError;
    property Handle: THandle read pipe write pipe;
    property WarnOnMainThreadLockObtaining: boolean read fWarnOnMainThreadLockObtaining write fWarnOnMainThreadLockObtaining;
    property ErrorOnMainThreadLockObtaining: boolean read fErrorOnMainThreadLockObtaining write fErrorOnMainThreadLockObtaining;
  end;

procedure pipecontrol_addMetaData(L: PLua_state; metatable: integer; userdata: integer );


implementation


uses LuaObject, LuaByteTable, networkInterface, networkInterfaceApi, LuaHandler;

threadvar WaitEvent: THandle;

destructor TPipeConnection.destroy;
begin
  if (pipe<>0) and (pipe<>INVALID_HANDLE_VALUE) then
    closehandle(pipe);

  if cs<>nil then
    freeandnil(cs);

  inherited destroy;
end;

constructor TPipeConnection.create;
begin
  ftimeout :=5000;
  cs:=TGuiSafeCriticalSection.Create;
end;

procedure TPipeConnection.lock;
begin
  if fErrorOnMainThreadLockObtaining or fWarnOnMainThreadLockObtaining and (MainThreadID=GetCurrentThreadId) then
  begin
    if fErrorOnMainThreadLockObtaining then
    begin
      raise exception.create('No mainthread access to this pipe allowed');
    end;

    lua_getglobal(LuaVM,'print');
    lua_pushstring(LuaVM,'Warning: pipe access from main thread');
    lua_pcall(LuaVM,1,0,0);
  end;

  if ftimeout<>0 then
    cs.Enter(ftimeout)
  else
    cs.enter;
end;

procedure TPipeconnection.unlock;
begin
  cs.leave;
end;

procedure TPipeConnection.setTimeout(newtimeout: integer);
begin
  cs.enter;
  ftimeout:=newtimeout;
  cs.leave;
end;

procedure TPipeConnection.writeDouble(v:double);
begin
  writeBytes(@v, 8);
end;

procedure TPipeConnection.writeFloat(v:single);
begin
  writeBytes(@v, 4);
end;

procedure TPipeConnection.writeQword(v:qword);
begin
  writeBytes(@v, 8);
end;

procedure TPipeConnection.writeDword(v:dword);
begin
  writeBytes(@v, 4);
end;

procedure TPipeConnection.writeWord(v:word);
begin
  writeBytes(@v, 2);
end;

procedure TPipeConnection.writeByte(v:byte);
begin
  writeBytes(@v, 1);
end;

function TPipeConnection.readDouble: double;
begin
  readbytes(@result, 8);
end;

function TPipeConnection.readFloat: single;
begin
  readbytes(@result, 4);
end;

function TPipeConnection.readQword: qword;
begin
  readbytes(@result, 8);
end;

function TPipeConnection.readDword: dword;
begin
  readbytes(@result, 4);
end;

function TPipeConnection.readWord: word;
begin
  readbytes(@result, 2);
end;

function TPipeConnection.readByte: byte;
begin
  readbytes(@result, 1);
end;


procedure TPipeConnection.writeString(str: string; include0terminator: boolean);
begin
  if include0terminator then
    writebytes(@str[1], length(str)+1)
  else
    writebytes(@str[1], length(str));
end;

procedure TPipeConnection.writeWideString(str: widestring; include0terminator: boolean);
begin
  if include0terminator then
    writebytes(@str[1], (length(str)+1)*2)
  else
    writebytes(@str[1], (length(str)+1)*2);
end;

function TPipeConnection.readString(size: integer): string;
var x: pchar;
begin
  getmem(x, size+1);
  readbytes(x, size);
  x[size]:=#0;

  result:=x;

  FreeMemAndNil(x);
end;

function TPipeConnection.readWideString(size: integer): widestring;
var x: pwidechar;
begin
  getmem(x, size+2);
  readbytes(x, size);

  x[size]:=#0;
  x[size+1]:=#0;

  result:=x;
  FreeMemAndNil(x);
end;

{$ifdef windows}

function TPipeConnection.ProcessOverlappedOperation(o: POVERLAPPED): boolean;
var
  starttime: qword;
  i: integer;
  bt, lastbt: dword;
  r: dword;
begin
  starttime:=GetTickCount64;
  bt:=0;
  lastbt:=0;

  while fconnected and ((ftimeout=0) or (gettickcount64<starttime+ftimeout)) do
  begin
    if MainThreadID=GetCurrentThreadId then
    begin
      CheckSynchronize;

      r:=WaitForSingleObject(o^.hEvent, 25);
      case r of
        WAIT_OBJECT_0, WAIT_TIMEOUT: fconnected:=true;
        else
          fconnected:=false;
      end;
      if not fconnected then
      begin
        closeConnection(fOnError);
        exit(false);
      end;
    end
    else
    begin
     // sleep(10);
      r:=WaitForSingleObject(o^.hEvent, ifthen<DWORD>(ftimeout=0, 1000, ftimeout));
      case r of
        WAIT_OBJECT_0, WAIT_TIMEOUT: fconnected:=true;
        else
          fconnected:=false;
      end;
      if not fconnected then
      begin
        closeConnection(fOnError);
        exit(false);
      end;
    end;


    if fconnected and (GetOverlappedResult(pipe, o^, bt,false)=false) then   //todo: check for GetOverlappedResultEx and use that
    begin
      if bt<>lastbt then
        starttime:=GetTickCount64;

      lastbt:=bt;

      i:=getlasterror;
      if ((i=ERROR_IO_PENDING) or (i=ERROR_IO_INCOMPLETE)) then
        continue
      else
      begin
        closeConnection(fOnError);
        exit(false);
      end;
    end
    else
    begin
      o^.Internal:=bt;
      exit(fconnected);
    end;
  end;

  closeConnection(fOnTimeout);
  exit(false);
end;
{$endif}

procedure TPipeConnection.CloseConnection(n: TNotifyEvent);
begin
  fconnected:=false;
  {$ifdef windows}
  CancelIo(pipe);
  closehandle(pipe);
  {$endif}
  {$ifdef darwin}
  closepipe(pipe);
  {$endif}
  pipe:=0;
  if assigned(n) then
    n(self);
end;

function TPipeConnection.WriteBytes(bytes: pointer; size: integer): boolean;

var
  c: TCEConnection;
  {$ifdef windows}
  bw: dword;
  o: OVERLAPPED;
  starttime: qword;
  i: integer;
  overlappedevent: thandle;
  totalwritten: dword;
  {$endif}
begin
  if not fconnected then exit(false);

  if (bytes<>nil) and (size>0) then
  begin
    c:=getConnection;
    if (c<>nil) and c.isNetworkHandle(handle) then
    begin
      fconnected:=c.writePipe(handle, bytes, size, ftimeout);
      if fconnected=false then
        closeConnection(fOnTimeout);
    end
    else
    begin
      {$ifdef windows}
      if foverlapped then
      begin
        totalwritten:=0;
        while fconnected and (totalwritten<size) do
        begin
          zeromemory(@o, sizeof(o));

          if waitevent=0 then
            waitevent:=CreateEvent(nil,false,false,nil);


          o.hEvent:=waitevent;
          resetevent(o.hEvent);

          if writefile(pipe, bytes^, size, bw,@o)=false then
          begin
            if GetLastError=ERROR_IO_PENDING then
            begin
              if ProcessOverlappedOperation(@o) then
              begin
                inc(totalwritten, o.Internal);
                inc(bytes,o.Internal);
              end
              else
                exit(false);
            end
            else
            begin
              closeConnection(fOnError);
              exit(false);
            end;
          end
          else
          begin
            inc(totalwritten,bw);
            inc(bytes,bw);
          end;
        end;
      end
      else
        fconnected:=fconnected and writefile(pipe, bytes^, size, bw, nil);
      {$endif}
      {$ifdef darwin}
      fconnected:=writepipe(pipe, bytes, size, ftimeout);
      if fconnected=false then
        closeConnection(fOnTimeout);
      {$endif}
    end;
  end;

  result:=fconnected;
end;

function TPipeConnection.ReadBytes(bytes: pointer; size: integer): boolean;
var
  c: TCEConnection;
{$ifdef windows}
  br: dword;
  o: OVERLAPPED;
  i: integer;
  starttime: qword;

  totalread: dword;
{$endif}
begin
  if not fconnected then exit(false);

  if (bytes<>nil) and (size>0) then
  begin
    c:=getConnection;
    if (c<>nil) and c.isNetworkHandle(pipe) then
    begin
      fconnected:=c.readPipe(pipe, bytes, size, ftimeout);
      if fconnected=false then
        closeConnection(fOnError);
    end
    else
    begin
      {$ifdef windows}
      starttime:=GetTickCount64;
      totalread:=0;

      if foverlapped then
      begin
        while fconnected and (totalread<size) do
        begin
          zeromemory(@o, sizeof(o));
          if waitevent=0 then
            waitevent:=CreateEvent(nil,false,false,nil);

          o.hEvent:=waitevent;
          resetevent(o.hEvent);
          if Readfile(pipe, bytes^, size, br,@o)=false then
          begin
            if GetLastError=ERROR_IO_PENDING then
            begin
              if ProcessOverlappedOperation(@o) then
              begin
                inc(totalread, o.Internal);
                inc(bytes, o.Internal);
              end
              else
                exit(false);
            end
            else
            begin
              closeConnection(fOnError);
              exit(false);
            end;
          end
          else
          begin
            inc(totalread, br);
            inc(bytes, br);
          end;

        end;
      end
      else
      begin
        while fconnected and (totalread<size) do
        begin
          fconnected:=fconnected and Readfile(pipe, bytes^, size, br, nil);
          inc(totalread,br);
          inc(bytes, br);
        end;
      end;
      {$endif}
      {$ifdef darwin}
      fconnected:=readpipe(pipe,bytes,size,ftimeout);
      if fconnected=false then
        closeConnection(fOnTimeout);
      {$endif}

    end;
  end;


  result:=fconnected;

  if result=false then
  asm
  nop
  end;
end;

function pipecontrol_writeBytes(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;

  paramcount: integer;
  size: integer;

  ba: pbytearray;
begin
  //writeBytes(ByteTable, size OPTIONAL)

  result:=0;

  p:=luaclass_getClassObject(L);
  paramcount:=lua_gettop(L);

  if paramcount>0 then
  begin
    if paramcount>1 then
      size:=lua_tointeger(L,2)
    else
      size:=lua_objlen(L, 1); //get size from the table

    getmem(ba, size);
    readBytesFromTable(L, 1, ba, size);

    if (p.WriteBytes(ba, size)) then
    begin
      lua_pushinteger(L, size);
      result:=1;
    end;

    FreeMemAndNil(ba);
  end;


end;

function pipecontrol_readBytes(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;

  paramcount: integer;
  size: integer;

  ba: pbytearray;
begin
//  readBytes(size: integer): returns a byte table from the pipe, or nil on failure
  result:=0;
  p:=luaclass_getClassObject(L);
  paramcount:=lua_gettop(L);

  if paramcount=1 then
  begin
    size:=lua_tointeger(L, 1);

    getmem(ba, size);
    if p.readBytes(ba, size) then
    begin
      CreateByteTableFromPointer(L, ba, size);
      result:=1;
    end;

    FreeMemAndNil(ba);
  end;


end;

function pipecontrol_readDouble(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
  v: double;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  v:=p.readDouble;
  if p.connected then
  begin
    lua_pushnumber(L, v);
    result:=1;
  end;
end;

function pipecontrol_readFloat(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
  v: Single;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  v:=p.readFloat;
  if p.connected then
  begin
    lua_pushnumber(L, v);
    result:=1;
  end;
end;

function pipecontrol_readQword(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
  v: QWord;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  v:=p.readQword;
  if p.connected then
  begin
    lua_pushinteger(L, v);
    result:=1;
  end;
end;

function pipecontrol_readQwords(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
  v: QWord;
  count: integer;
  results: array of qword;
  i: integer;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    count:=lua_tointeger(L,1);
    setlength(results, count);

    p.ReadBytes(@results[0],count*8);
    if p.connected then
    begin
      lua_createtable(L,count,0);
      for i:=0 to count-1 do
      begin
        lua_pushinteger(L,i+1);
        lua_pushinteger(L,results[i]);
        lua_settable(L,-3);
      end;

      result:=1;
    end;
  end;
end;

function pipecontrol_readDword(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
  v: DWord;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  v:=p.readDword;
  if p.connected then
  begin
    lua_pushinteger(L, v);
    result:=1;
  end;
end;

function pipecontrol_readDwords(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
  v: QWord;
  count: integer;
  results: array of dword;
  i: integer;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    count:=lua_tointeger(L,1);
    setlength(results, count);

    p.ReadBytes(@results[0],count*4);
    if p.connected then
    begin
      lua_createtable(L,count,0);
      for i:=0 to count-1 do
      begin
        lua_pushinteger(L,i+1);
        lua_pushinteger(L,results[i]);
        lua_settable(L,-3);
      end;

      result:=1;
    end;
  end;
end;

function pipecontrol_readWord(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
  v: Word;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  v:=p.readWord;
  if p.connected then
  begin
    lua_pushinteger(L, v);
    result:=1;
  end;
end;

function pipecontrol_readWords(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
  v: QWord;
  count: integer;
  results: array of word;
  i: integer;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    count:=lua_tointeger(L,1);
    setlength(results, count);

    p.ReadBytes(@results[0],count*2);
    if p.connected then
    begin
      lua_createtable(L,count,0);
      for i:=0 to count-1 do
      begin
        lua_pushinteger(L,i+1);
        lua_pushinteger(L,results[i]);
        lua_settable(L,-3);
      end;

      result:=1;
    end;
  end;
end;

function pipecontrol_readByte(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
  v: Byte;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  v:=p.readByte;
  if p.connected then
  begin
    lua_pushinteger(L, v);
    result:=1;
  end;
end;

function pipecontrol_readString(L: PLua_State): integer; cdecl;
//readString(size: integer)
var
  p: TPipeconnection;
  v: QWord;

  paramcount: integer;
  size: integer;
  s: pchar;


begin
  result:=0;
  p:=luaclass_getClassObject(L);
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    size:=lua_tointeger(L, 1);

    getmem(s, size);

    try
      p.ReadBytes(s, size);

      if p.connected then
      begin
        lua_pushlstring(L, s, size);
        result:=1;
      end;
    finally
      FreeMemAndNil(s);
    end;

  end;
end;

function pipecontrol_readWideString(L: PLua_State): integer; cdecl;
//readString(size: integer)
var
  p: TPipeconnection;
  v: QWord;

  paramcount: integer;
  size: integer;
  ws: widestring;
  s: string;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  paramcount:=lua_gettop(L);
  if paramcount=1 then
  begin
    size:=lua_tointeger(L, 1);

    ws:=p.readWideString(size);
    if p.connected then
    begin
      s:=ws;
      lua_pushstring(L, pchar(s));
      result:=1;
    end;
  end;
end;

function pipecontrol_writeDouble(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    p.writeDouble(lua_tonumber(L, 1));
    if p.connected then
    begin
      lua_pushinteger(L, sizeof(double));
      result:=1;
    end;
  end;
end;

function pipecontrol_writeFloat(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    p.writeFloat(lua_tonumber(L, 1));
    if p.connected then
    begin
      lua_pushinteger(L, sizeof(single));
      result:=1;
    end;
  end;
end;

function pipecontrol_writeQword(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    p.writeQword(lua_tointeger(L, 1));
    if p.connected then
    begin
      lua_pushinteger(L, sizeof(QWord));
      result:=1;
    end;
  end;
end;

function pipecontrol_writeDword(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    p.writeDword(lua_tointeger(L, 1));
    if p.connected then
    begin
      lua_pushinteger(L, sizeof(DWord));
      result:=1;
    end;
  end;
end;

function pipecontrol_writeWord(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    p.writeWord(lua_tointeger(L, 1));
    if p.connected then
    begin
      lua_pushinteger(L, sizeof(Word));
      result:=1;
    end;
  end;
end;

function pipecontrol_writeByte(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  if lua_gettop(L)=1 then
  begin
    p.writeByte(lua_tointeger(L, 1));
    if p.connected then
    begin
      lua_pushinteger(L, sizeof(Byte));
      result:=1;
    end;
  end;
end;

function pipecontrol_writeString(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
  paramcount: integer;
  s: pchar;
  slength: size_t;
  include0terminator: boolean;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  paramcount:=lua_gettop(L);
  if paramcount>=1 then
  begin
    if paramcount=2 then
      include0terminator:=lua_toboolean(L, 2)
    else
      include0terminator:=false;

    slength:=0;
    s:=lua_tolstring(L, 1, @slength);

    p.WriteBytes(s, slength);
    if include0terminator then
      p.writeByte(0);

    if p.connected then
    begin
      //return the number of bytes written
      if include0terminator then
        lua_pushinteger(L, slength+1)
      else
        lua_pushinteger(L, slength);

      result:=1;
    end;
  end;
end;

function pipecontrol_writeWideString(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
  paramcount: integer;
  s: string;
  ws: string;
  include0terminator: boolean;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  paramcount:=lua_gettop(L);
  if paramcount>=1 then
  begin
    s:=lua_tostring(L, 1);
    ws:=s;

    if paramcount=2 then
      include0terminator:=lua_toboolean(L, 2)
    else
      include0terminator:=false;

    p.writeWideString(ws, include0terminator);
    if p.connected then
    begin
      if include0terminator then
        lua_pushinteger(L, (length(ws)+1)*2)
      else
        lua_pushinteger(L, length(ws)*2);

      result:=1;
    end;
  end;
end;

function pipecontrol_readIntoStream(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
  stream: tstream;
  size: integer;
  buf: pointer;
begin
  result:=0;
  p:=luaclass_getClassObject(L);

  if lua_gettop(L)>=2 then
  begin
    stream:=lua_ToCEUserData(L,1);
    size:=lua_tointeger(L,2);

    buf:=getmem(size);
    try
      if p.ReadBytes(buf,size) then
      begin
        lua_pushinteger(L, stream.Write(buf^,size));
        result:=1;
      end;
    finally
      freemem(buf);
    end;
  end
end;

function pipecontrol_writeFromStream(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
  stream: tstream;
  size: integer;
  buf: pointer;
begin
  result:=0;
  p:=luaclass_getClassObject(L);

  if lua_gettop(L)>=1 then
  begin
    stream:=lua_ToCEUserData(L,1);

    if lua_gettop(L)>=2 then
      size:=lua_tointeger(L,2)
    else
      size:=stream.Size-stream.Position;

    buf:=getmem(size);
    try
      stream.read(buf^,size);

      if p.WriteBytes(buf,size) then
      begin
        lua_pushinteger(L, stream.Write(buf^,size));
        result:=1;
      end;
    finally
      freemem(buf);
    end;
  end
end;

function pipecontrol_lock(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  p.lock;
end;

function pipecontrol_unlock(L: PLua_State): integer; cdecl;
var
  p: TPipeconnection;
begin
  result:=0;
  p:=luaclass_getClassObject(L);
  p.unlock;
end;


procedure pipecontrol_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'lock', pipecontrol_lock);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'unlock', pipecontrol_unlock);


  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeBytes', pipecontrol_writeBytes);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readBytes', pipecontrol_readBytes);


  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readDouble', pipecontrol_readDouble);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readFloat', pipecontrol_readFloat);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readQword', pipecontrol_readQword);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readQwords', pipecontrol_readQwords);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readDword', pipecontrol_readDword);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readDwords', pipecontrol_readDwords);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readWord', pipecontrol_readWord);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readWords', pipecontrol_readWords);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readByte', pipecontrol_readByte);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readString', pipecontrol_readString);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readWideString', pipecontrol_readWideString);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'readIntoStream', pipecontrol_readIntoStream);



  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeDouble', pipecontrol_writeDouble);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeFloat', pipecontrol_writeFloat);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeQword', pipecontrol_writeQword);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeDword', pipecontrol_writeDword);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeWord', pipecontrol_writeWord);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeByte', pipecontrol_writeByte);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeString', pipecontrol_writeString);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeWideString', pipecontrol_writeWideString);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'writeFromStream', pipecontrol_writeFromStream);

end;

initialization
  luaclass_register(TPipeConnection, pipecontrol_addMetaData );



end.

