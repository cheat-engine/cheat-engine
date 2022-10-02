unit PointerscanConnector;

//thread that will constantly check if it has a connection queue and if so try to connect it and initiate a connection
//it also sets up the basic connection type

{$mode delphi}

interface

uses
  Classes, SysUtils, sockets, resolve, syncobjs, math, {$ifdef windows}winsock2,{$endif} CELazySocket,
  PointerscanNetworkCommands, NewKernelHandler{$ifdef darwin},macport{$endif};


type
  TConnectEntry=record
                 id: integer;
                 deleted: boolean; //when set this entry should get deleted
                 ip: pchar;
                 port: word;
	         password: pchar;
	         becomeparent: boolean;
                 trusted: boolean;
               end;
  PConnectentry=^TConnectentry;

  TConnectEntryArray=array of TConnectentry;

  TPointerscanConnectEvent=procedure(sender: TObject; sockethandle: TSocket; IBecameAParent: boolean; entry: PConnectEntry) of object;
  TPointerScanConnectorLogEvent=procedure(sender: TObject; message: string) of object;

  TPointerscanConnector=class(TThread)
  private
    fOnConnected: TPointerscanConnectEvent;
    fOnLog: TPointerScanConnectorLogEvent;

    nextid: integer; //id to pass on to the next added entry
    lastlog: string;       //debug only
    list: TList;
    listcs: TCriticalSection;

    hr: THostResolver;
    procedure Log(msg: string);
    procedure FreeEntry(entry: PConnectEntry);
    procedure RemoveDeletedEntries; //only called by the thread.
  public
    function AddConnection(ip: string; port: word; password: string; becomeparent: boolean; trusted: boolean=false): integer;
    procedure MarkEntryForDeletion(id: integer); //marks for deletion
    procedure GetList(var l: TConnectEntryArray);

    procedure execute; override;
    constructor create(onconnect: TPointerscanConnectEvent=nil);
    destructor destroy; override;

    property OnConnected: TPointerscanConnectEvent read fOnConnected write fOnConnected;
  end;

implementation

resourcestring
  rsHost = 'host:';
  rsCouldNotBeResolved = ' could not be resolved';
  rsFailureCreatingSocket = 'Failure creating socket';
  rsInvalidResponseFrom = 'invalid response from ';
  rsSomeoneForgotToGiveThisConnector = 'Someone forgot to give this connector an OnConnected event...';
  rsErrorWhileConnecting = 'Error while connecting: ';

procedure TPointerscanConnector.GetList(var l: TConnectEntryArray);
{
Passes a copy of the list of entries to the caller
}
var i: integer;
begin
  listcs.enter;
  try
    setlength(l, list.count);
    for i:=0 to list.count-1 do
      l[i]:=PConnectEntry(list[i])^;
  finally
    listcs.leave;
  end;
end;

procedure TPointerscanConnector.FreeEntry(entry: PConnectEntry);
begin
  if (entry.ip<>nil) then
    StrDispose(entry.ip);

  if entry.password<>nil then
    StrDispose(entry.password);

  Freemem(entry);
end;

procedure TPointerscanConnector.RemoveDeletedEntries;
var
  i: integer;
  entry: PConnectentry;
begin
  listcs.enter;
  try
    i:=0;
    while i<list.count do
    begin
      entry:=list[i];
      if entry.deleted then
      begin
        freeEntry(entry);
        list.Delete(i);
      end
      else
        inc(i);
    end;
  finally
    listcs.leave;
  end;
end;

procedure TPointerscanConnector.MarkEntryForDeletion(id: integer);
var i: integer;
begin
  listcs.enter;
  try
    for i:=0 to list.count-1 do
      if PConnectentry(list[i]).id=id then
        PConnectentry(list[i]).deleted:=true;
  finally
    listcs.leave;
  end;
end;

function TPointerscanConnector.AddConnection(ip: string; port: word; password: string; becomeparent: boolean; trusted: boolean=false): integer;
var
  i: integer;
  entry: PConnectentry;
begin
  if port=0 then exit; //do not allow port 0

  getmem(entry, sizeof(TConnectEntry));
  entry.ip:=strnew(pchar(ip));
  entry.port:=port;
  entry.password:=strnew(pchar(password));
  entry.becomeparent:=becomeparent;
  entry.trusted:=trusted;

  entry.deleted:=false;
  entry.id:=nextid;
  inc(nextid);

  listcs.enter;
  try
    list.Add(entry);
  finally
    listcs.leave;
  end;

  result:=entry.id;
end;

procedure TPointerscanConnector.Log(msg: string);
begin
  lastlog:=msg;
  if assigned(fOnLog) then
    fOnLog(self, msg);
end;

procedure TPointerscanConnector.execute;
var
  i: integer;
  entry: PConnectentry;


  sockaddr: TInetSockAddr;
  sockethandle: TSocket;
  ss: TSocketStream;
  result: byte;


  wait: boolean;
begin
  SetThreadDebugName(handle,'TPointerscanConnector');
  i:=0;
  while not terminated do
  begin
    RemoveDeletedEntries;

    sockethandle:=INVALID_SOCKET;
    entry:=nil;

    try

      wait:=false;
      listcs.enter;

      //check the list
      try
        if i>=list.count  then //start from the beginning
        begin
          wait:=true; //end of the list, wait a bit to prevent hammering
          i:=0;
        end;

        if i<list.count then
          entry:=list[i];

      finally
        listcs.leave;
      end;


      if wait then sleep(1000);

      if terminated then exit;

      if entry<>nil then
      begin
        //connect to this entry
        //first resolve the ip
        if entry.deleted then
        begin
          inc(i);
          continue;
        end;

        sockaddr.sin_addr:=StrToNetAddr(entry.ip);

        if sockaddr.sin_addr.s_bytes[4]=0 then
        begin
          if hr.NameLookup(entry.ip) then
            sockaddr.sin_addr:=hr.NetHostAddress
          else
            raise exception.create(rsHost+entry.ip+rsCouldNotBeResolved);
        end;

        if sockaddr.sin_addr.s_bytes[4]<>0 then
        begin
          //connect
          sockethandle:=fpsocket(AF_INET, SOCK_STREAM, 0);
          if sockethandle=INVALID_SOCKET then
            raise exception.create(rsFailureCreatingSocket);

          sockaddr.sin_family:=AF_INET;
          sockaddr.sin_port:=htons(entry.port);

          if fpconnect(sockethandle, @SockAddr, sizeof(SockAddr))=0 then
          begin
            //connected, do the initial handshake
            //build the message


            ss:=TSocketStream.Create(sockethandle, false); //don't take ownership of this socket

            try
              ss.WriteByte(PSCONNECT_INIT);
              ss.WriteAnsiString8(entry.password);
              ss.WriteByte(ifthen(entry.becomeparent, 0, 1));

              //send it
              ss.flushWrites;

              result:=ss.ReadByte;
            finally
              ss.free;
            end;

            if result<>0 then
              raise exception.create(rsInvalidResponseFrom+entry.ip)
            else
            begin
              if Assigned(fOnConnected) then
              begin
                fOnConnected(self, sockethandle, entry.becomeparent, entry);

                //remove this from the list
                MarkEntryForDeletion(entry.id);
                continue;
              end
              else
                raise exception.create(rsSomeoneForgotToGiveThisConnector);
            end;
          end
          else
          begin
            closeSocket(sockethandle);
            sockethandle:=INVALID_SOCKET;
          end;
        end;

      end;


    except
      on e: exception do
      begin
        if sockethandle<>INVALID_SOCKET then
          closesocket(sockethandle);

        OutputDebugString(rsErrorWhileConnecting+e.message);
        log(e.message);
        sleep(1000);
      end;
    end;
    inc(i);
  end;
end;

destructor TPointerscanConnector.destroy;
begin
  listcs.free;
  list.free;
  hr.free;
  inherited destroy;
end;

constructor TPointerscanConnector.create(onconnect: TPointerscanConnectEvent=nil);
begin
  hr:=THostResolver.Create(nil);
  list:=tlist.create;
  listcs:=tcriticalsection.Create;
  fOnConnected:=onconnect;
  inherited create(false);
end;


end.

