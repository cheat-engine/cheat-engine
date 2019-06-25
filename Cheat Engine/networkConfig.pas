unit networkConfig;

{$mode DELPHI}

interface

uses
  jwawindows, windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus, resolve, Sockets, ctypes,
  registry;

type

  { TfrmNetworkConfig }



  TfrmNetworkConfig = class(TForm)
    btnConnect: TButton;
    Button2: TButton;
    edtHost: TEdit;
    edtPort: TEdit;
    GroupBox1: TGroupBox;
    ctsImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    ListView1: TListView;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    procedure btnConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure MenuItem1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure requestlist;
  end; 

var
  frmNetworkConfig: TfrmNetworkConfig;

var
  host: THostAddr;
  port: integer;

  networkcompression: integer;

procedure CEConnect(hostname: string; p: integer);

implementation

{$R *.lfm}

uses networkInterfaceApi;

resourcestring
  rsHost = 'host:';
  rsCouldNotBeResolved = ' could not be resolved';
  rsFailureCreatingSocket = 'Failure creating socket';
  rsFailedConnectingToTheServer = 'Failed connecting to the server';

type TDiscovery=class(tthread)
  private
    s: cint;

    server: record
      ip: string;
      port: word;
    end;
    procedure addip;
  public
    procedure execute; override;
    procedure Terminate;

    constructor create(suspended: boolean);
    destructor destroy; override;

  end;

var Discovery: TDiscovery;

constructor TDiscovery.create(suspended: boolean);
begin
  s:=fpsocket(PF_INET, SOCK_DGRAM, 0);
  inherited create(suspended);
end;

destructor TDiscovery.destroy;
begin
  if s<>cint(INVALID_SOCKET) then
    closeSocket(s);
end;

procedure TDiscovery.terminate;
var olds: cint;
begin
  if s<>cint(INVALID_SOCKET) then
  begin
    olds:=s;
    s:=cint(INVALID_SOCKET);
    CloseSocket(olds);
  end;

  inherited terminate;
end;

procedure TDiscovery.addip;
var li: tlistitem;
begin
  if frmNetworkConfig<>nil then
  begin
    li:=frmNetworkConfig.ListView1.Items.Add;
    li.Caption:=server.ip;
    li.SubItems.Add(inttostr(server.port));
  end;
end;

procedure TDiscovery.execute;
var
  v: BOOL;
  sin: sockaddr_in;
  Y: word;

  sout: sockaddr_in;
  i: integer;

  //l: socklen_t;

  srecv: sockaddr_in;
  recvsize: integer;

  packet: packed record
    checksum: dword;
    port: word;
  end;

begin
  //send a broadcast asking which devices  (port 3296)


  if s>=0 then
  begin
    v:=true;
    if fpsetsockopt(s, SOL_SOCKET, SO_BROADCAST, @v, sizeof(v)) >=0 then
    begin
      zeromemory(@sin, sizeof(sin));

      sin.sin_family:=PF_INET;
      sin.sin_addr.s_addr:=htonl(INADDR_ANY);
      sin.sin_port:=htons(3296);
      i:=fpbind(s, @sin, sizeof(sin));

      if (i>=0) then
      begin
        zeromemory(@sout, sizeof(sout));
        sout.sin_family:=PF_INET;
        sout.sin_addr.s_addr:=htonl(INADDR_BROADCAST);
        sout.sin_port:=htons(3296);

        packet.checksum:=random(100);
        i:=fpsendto(s, @packet, sizeof(packet),0, @sout, sizeof(sout));

        y:=packet.checksum*$ce;

        if (i>0) then
        repeat
          recvsize:=sizeof(srecv);
          ZeroMemory(@srecv, recvsize);
          i:=fprecvfrom(s, @packet, sizeof(packet), 0, @srecv, @recvsize);
          if (i>0) and (not terminated) then
          begin
           // showmessage('packet.checksum='+inttohex(packet.checksum,8)+' - y='+inttohex(y,8));
            if packet.checksum=y then
            begin
             // showmessage('address='+inttohex(srecv.sin_addr.s_addr,8)+' port='+inttostr(packet.port));
              //add to list
              server.ip:=NetAddrToStr(srecv.sin_addr);
              server.port:=packet.port;
              if not terminated then
                synchronize(addip);
            end;
          end


        until (i<=0) or (terminated);



      end;
    end;
  end;

  if s<>cint(INVALID_SOCKET) then
  begin
    closesocket(s);
    s:=cint(invalid_socket);
  end;

end;

procedure CEconnect(hostname: string; p: integer);
var hr:   THostResolver;
begin
  hr:=THostResolver.Create(nil);
  try

    host:=StrToNetAddr(hostname);

    if host.s_bytes[4]=0 then
    begin
      if hr.NameLookup(hostname) then
        host:=hr.NetHostAddress
      else
        raise exception.create(rsHost+hostname+rsCouldNotBeResolved);

    end;


  finally
    hr.free;
  end;

  port:=ShortHostToNet(p);

  if getConnection=nil then
    raise exception.create(rsFailedConnectingToTheServer);


  InitializeNetworkInterface;
end;

{ TfrmNetworkConfig }

procedure TfrmNetworkConfig.requestlist;
var s: cint;
  v: integer;
begin

  if discovery<>nil then
  begin
    discovery.Terminate;
    discovery.WaitFor;
    freeandnil(discovery);
  end;

  listview1.Clear;

  discovery:=TDiscovery.Create(false);
end;


procedure TfrmNetworkConfig.FormShow(Sender: TObject);
begin
  requestlist;
end;



procedure TfrmNetworkConfig.btnConnectClick(Sender: TObject);
var reg: Tregistry;
begin
  CEconnect(edtHost.text, strtoint(edtPort.text));

  //still here so the connection is made
  reg:=tregistry.create;
  try
    if reg.OpenKey('\Software\Cheat Engine\',false) then
    begin
      reg.WriteString('Last Connect IP', edtHost.text);
      reg.WriteString('Last Connect Port', edtport.text);
    end;
  finally
    reg.free;
  end;

  modalresult:=mrok;
end;

procedure TfrmNetworkConfig.FormCreate(Sender: TObject);
var reg: tregistry;
begin
  reg:=tregistry.create;
  try
    if reg.OpenKey('\Software\Cheat Engine\',false) then
    begin
      if reg.ValueExists('Last Connect IP') then
        edtHost.text:=reg.ReadString('Last Connect IP');

      if reg.ValueExists('Last Connect Port') then
        edtport.text:=reg.ReadString('Last Connect Port');
    end;
  finally
    reg.free;
  end;
end;

procedure TfrmNetworkConfig.ListView1DblClick(Sender: TObject);
begin
  if listview1.selected<>nil then
  begin
    edthost.text:=listview1.selected.caption;
    edtport.text:=listview1.selected.subitems[0];
    btnConnect.click;
  end;
end;

procedure TfrmNetworkConfig.ListView1SelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if selected then
  begin
    edthost.text:=item.caption;
    edtport.text:=item.subitems[0];
  end;
end;

procedure TfrmNetworkConfig.MenuItem1Click(Sender: TObject);
begin
  requestlist;
end;




end.

