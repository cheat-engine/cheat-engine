unit networkConfig;

{$mode DELPHI}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  jwawindows, windows,
  {$endif}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus, resolve, Sockets, ctypes,
  registry, betterControls, Types;

type

  { TfrmNetworkConfig }



  TfrmNetworkConfig = class(TForm)
    btnConnect: TButton;
    Button2: TButton;
    edtFriendlyName: TEdit;
    edtHost: TEdit;
    edtPort: TEdit;
    GroupBox1: TGroupBox;
    ctsImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    lblOptionalName: TLabel;
    lvIPList: TListView;
    miRefresh: TMenuItem;
    miDelete: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    procedure btnConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvIPListDblClick(Sender: TObject);
    procedure lvIPListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure miDeleteClick(Sender: TObject);
    procedure miRefreshClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
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

uses networkInterfaceApi, mainunit2;

resourcestring
  rsHost = 'host:';
  rsCouldNotBeResolved = ' could not be resolved';
  rsFailureCreatingSocket = 'Failure creating socket';
  rsFailedConnectingToTheServer = 'Failed connecting to the server';

type
  THistoryEntry=class
  public
    ip: string;
    port: string;
    name: string;
  end;

  TDiscovery=class(tthread)
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
    li:=frmNetworkConfig.lvIPList.Items.Insert(0); //add to the top
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
  begin
    if host.s_addr<>0 then //it's 0 when it terminated earlier
    begin
      host.s_addr:=0;
      if MainThreadID=GetCurrentThreadId then
        MessageDlg(rsFailedConnectingToTheServer, mtError, [mbOK],0);
    end;

    exit;
  end;


  InitializeNetworkInterface;
end;

{ TfrmNetworkConfig }

procedure TfrmNetworkConfig.requestlist;
var
  i: integer;
begin

  if discovery<>nil then
  begin
    discovery.Terminate;
    discovery.WaitFor;
    freeandnil(discovery);
  end;

  //delete discovered entries
  for i:=lvIPList.items.count-1 downto 0 do
  begin
    if lvIPList.items[i].Data=nil then
      lvIPList.items[i].Delete;

  end;

  discovery:=TDiscovery.Create(false);
end;


procedure TfrmNetworkConfig.FormShow(Sender: TObject);
begin
  requestlist;

  edtHost.Width:=canvas.GetTextWidth('1234.1234.1234.1234');
  edtPort.Width:=canvas.GetTextWidth(' 99999 ');
end;



procedure TfrmNetworkConfig.btnConnectClick(Sender: TObject);
var
  reg: Tregistry;
  i: integer;
  name, ip: string;
  he: THistoryEntry;
  li: TListitem;
begin
  CEconnect(trim(edtHost.text), strtoint(trim(edtPort.text)));

  //check if this is in the list
  he:=nil;
  li:=nil;
  ip:=edtHost.text;
  name:=edtFriendlyName.text;
  for i:=0 to lvIPList.items.count-1 do
  begin
    he:=lvIPList.items[i].data;

    if (name<>'') and (he<>nil) then
    begin
      if he.name=name then
      begin
        li:=lvIPList.items[i];
        break;
      end;
    end
    else
    begin
      if lvIPList.items[i].caption=ip then
      begin
        li:=lvIPList.items[i];
        break;
      end;
    end;
  end;

  if li=nil then
  begin
    li:=lvIPList.items.insert(0);
    li.data:=THistoryEntry.create;
    li.SubItems.add('');
  end;

  if li.data=nil then //update a found entry to history entry
    li.data:=THistoryEntry.create;

  he:=THistoryEntry(li.data);


  if he<>nil then
  begin
    he.ip:=edtHost.text;
    he.port:=edtport.text;
    he.name:=edtFriendlyName.text;

    if he.name<>'' then
      li.Caption:=he.name+' ('+he.ip+')'
    else
      li.caption:=he.ip;

    li.subitems[0]:=he.port;
  end;

  //still here so the connection is made
  reg:=tregistry.create;
  try
    if reg.OpenKey('\Software\'+strCheatEngine+'\',false) then
    begin
      reg.WriteString('Last Connect IP', edtHost.text);
      reg.WriteString('Last Connect Port', edtport.text);
      reg.WriteString('Last Connect Name', edtFriendlyName.text);
    end;
  finally
    reg.free;
  end;

  modalresult:=mrok;
end;

procedure TfrmNetworkConfig.FormCreate(Sender: TObject);
var
  reg: tregistry;
  sl: TStringlist;
  li: tlistitem;
  i: integer;
  ip: string;
  port: string;
  name: string;
  he: THistoryEntry;
begin

  reg:=tregistry.create;
  try
    if reg.OpenKey('\Software\'+strCheatEngine+'\',false) then
    begin
      if reg.ValueExists('Last Connect IP') then
        edtHost.text:=reg.ReadString('Last Connect IP');

      if reg.ValueExists('Last Connect Port') then
        edtport.text:=reg.ReadString('Last Connect Port');

      sl:=tstringlist.create;
      reg.ReadStringList('History',sl);


      for i:=0 to sl.count-1 do
      begin
        case i mod 3 of
          0: ip:=sl[i];
          1: port:=sl[i];
          2:
          begin
            name:=sl[i];
            he:=THistoryEntry.create;
            he.ip:=ip;
            he.port:=port;
            he.name:=name;

            li:=lvIPList.Items.add;
            if name<>'' then
              li.caption:=name+' ('+ip+')'
            else
              li.caption:=ip;

            li.SubItems.add(port);
            li.Data:=he;
          end;
        end;
      end;


      sl.free;
    end;
  finally
    reg.free;
  end;
end;

procedure TfrmNetworkConfig.FormDestroy(Sender: TObject);
var
  i: integer;
  reg: Tregistry;
  he: THistoryEntry;
  sl: tstringlist;
  count: integer;
begin

  reg:=tregistry.create;
  try
    if reg.OpenKey('\Software\'+strCheatEngine+'\',true) then
    begin
      sl:=tstringlist.create;
      count:=0;
      for i:=0 to lvIPList.items.count-1 do
      begin
        if lvIPList.items[i].Data<>nil then
        begin
          he:=lvIPList.items[i].Data;
          inc(count);
          if count<=10 then  //limit to 10 entries
          begin
            sl.add(he.ip);
            sl.add(he.port);
            sl.add(he.name);
          end;

          he.free;
          lvIPList.items[i].Data:=nil;
        end;
      end;

      reg.WriteStringList('History',sl);
      sl.free;
    end;
  finally
    reg.free;
  end;
end;

procedure TfrmNetworkConfig.lvIPListDblClick(Sender: TObject);
begin
  if lvIPList.selected<>nil then
  begin
    if lvIPList.selected.Data<>nil then
    begin
      edthost.text:=THistoryEntry(lvIPList.selected.Data).ip;
      edtFriendlyName.text:=THistoryEntry(lvIPList.selected.Data).name;
    end
    else
      edthost.text:=lvIPList.selected.caption;
    edtport.text:=lvIPList.selected.subitems[0];
    btnConnect.click;
  end;
end;

procedure TfrmNetworkConfig.lvIPListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if selected then
  begin
    if item.data<>nil then
    begin
      edtHost.text:=THistoryEntry(item.data).ip;
      edtFriendlyName.text:=THistoryEntry(item.data).name;

    end
    else
      edthost.text:=item.caption;



    edtport.text:=item.subitems[0];
  end;
end;

procedure TfrmNetworkConfig.miDeleteClick(Sender: TObject);
begin
  if (lvIPList.selected<>nil) and (lvIPList.selected.data<>nil) then
  begin
    THistoryEntry(lvIPList.selected.data).free;
    lvIPList.selected.data:=nil;
    lvIPList.selected.Delete;
  end;
end;

procedure TfrmNetworkConfig.miRefreshClick(Sender: TObject);
begin
  requestlist;
end;

procedure TfrmNetworkConfig.PopupMenu1Popup(Sender: TObject);
begin
  midelete.enabled:=(lvIPList.Selected<>nil) and (lvIPList.Selected.Data<>nil);
end;




end.

