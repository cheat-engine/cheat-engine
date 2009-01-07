unit mainunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, hexedit, StdCtrls, ComCtrls, ExtCtrls, Menus, idwinsock2, winsock;

const
  WM_SEND=wm_user+1;
  WM_RECV=wm_user+2;

type TPacketdata=record
  socket: integer;
  buffer: pbyte;
  buflen: integer;
end;

type
  Tmainform = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ListView1: TListView;
    Button1: TButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Filters1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Newfilter1: TMenuItem;
    N1: TMenuItem;
    Panel4: TPanel;
    edtSocket: TEdit;
    Label1: TLabel;
    edtSize: TEdit;
    Label2: TLabel;
    Button3: TButton;
    Splitter1: TSplitter;
    edtTimes: TEdit;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Newfilter1Click(Sender: TObject);
  private
    { Private declarations }
    hexeditor: Thexeditor;
    procedure getips(socket: TSocket; var sock: string; var peer: string);
    procedure SendEvent(var message: TMessage); message WM_SEND;
    procedure RecvEvent(var message: TMessage); message WM_RECV;

    procedure filter(buffer: pchar; size: integer);
  public
    { Public declarations }
  end;

var
  mainform: Tmainform;

implementation

{$R *.dfm}

uses packetfilter, filterform;

procedure Tmainform.filter(buffer: pchar; size: integer);
var fromstring, tostring: string;
    i,j: integer;
    found: boolean;
begin
  fromstring:=frmFilter.Edit1.Text;
  tostring:=frmfilter.Edit2.Text;

  if fromstring='' then exit;

  for i:=0 to size-length(fromstring)-1 do
  begin
    found:=true;
    for j:=1 to length(fromstring) do
    begin
      if fromstring[j]<>buffer[i+j-1] then
      begin
        found:=false;
        break;
      end;
    end;

    if found then
    begin
      //replace
      for j:=1 to length(tostring) do
      begin
        if i+j>size then exit;
        
        buffer[i+j-1]:=tostring[j];
      end;
    end;
  end;


  
end;

procedure Tmainform.getips(socket: TSocket; var sock: string; var peer: string);
var y: SockAddr_In;
    l: integer;
    i: integer;
begin
  l:=sizeof(TSockAddrIn);

  i:=getsockname(socket,y,l);

  if i=0 then
  begin
    sock:=format('%d.%d.%d.%d:%d',[byte(y.sin_addr.S_un_b.s_b1),byte(y.sin_addr.S_un_b.s_b2), byte(y.sin_addr.S_un_b.s_b3), byte(y.sin_addr.S_un_b.s_b4), ntohs(y.sin_port)]);

    l:=sizeof(TSockAddrIn);
    i:=getpeername(socket,y,l);

    if i=0 then
      peer:=format('%d.%d.%d.%d:%d',[byte(y.sin_addr.S_un_b.s_b1),byte(y.sin_addr.S_un_b.s_b2), byte(y.sin_addr.S_un_b.s_b3), byte(y.sin_addr.S_un_b.s_b4), ntohs(y.sin_port)]);
  end;

end;

procedure Tmainform.RecvEvent(var message: TMessage);
var x: PrecvData;
    li: TListitem;
    packetdata: ^TPacketdata;
    fromaddress,toaddress: string;
    pi: TWSAPROTOCOL_INFO;
    i: integer;
begin
  x:=pointer(message.WParam);
  if frmFilter.CheckBox2.Checked then
    filter(pchar(x.buf), x.bufsize);

  getmem(packetdata,sizeof(TPacketdata));
  packetdata.socket:=x.socket;
  packetdata.buflen:=x.bufsize;
  getmem(packetdata.buffer,packetdata.buflen);
  CopyMemory(packetdata.buffer, x.buf, packetdata.buflen);



 // listview1.Items.BeginUpdate;
  try
    li:=listview1.Items.Add;

    li.caption:='R';
    if x.recvfrom then
      li.caption:=li.Caption+' From';

    li.SubItems.Add(inttostr(x.socket));
    li.SubItems.Add(inttostr(x.requestedsize)+'('+inttostr(x.bufsize)+')');

    getips(x.socket,toaddress,fromaddress);

    if x.recvfrom then
      li.SubItems.Add(format('%d.%d.%d.%d:%d',[byte(x.address.sin_addr.S_un_b.s_b1),byte(x.address.sin_addr.S_un_b.s_b2), byte(x.address.sin_addr.S_un_b.s_b3), byte(x.address.sin_addr.S_un_b.s_b4), ntohs(x.address.sin_port)]))
    else
      li.SubItems.Add(fromaddress);
      
    li.SubItems.Add(toaddress);

    i:=sizeof(pi);
    getsockopt(x.socket,SOL_SOCKET, SO_PROTOCOL_INFO, @pi, i);
    if i>0 then
    begin
      if pi.iSocketType=SOCK_STREAM then
        li.SubItems.Add('TCP')
      else
        li.SubItems.Add('UDP');
    end else li.SubItems.Add('...');
  
    li.Data:=packetdata;
  finally
    //listview1.Items.EndUpdate;
  end;
end;


procedure Tmainform.SendEvent(var message: TMessage);
var x: PsendData;
    li: TListitem;
    packetdata: ^TPacketdata;
    fromaddress,toaddress: string;
    pi: TWSAPROTOCOL_INFO;
    i: integer;
begin
  x:=pointer(message.WParam);
  if frmFilter.CheckBox2.Checked then
    filter(pchar(x.buf^), x.bufsize^);

  getmem(packetdata,sizeof(TPacketdata));
  packetdata.socket:=x.socket;
  packetdata.buflen:=x.bufsize^;
  getmem(packetdata.buffer,packetdata.buflen);
  CopyMemory(packetdata.buffer, x.buf^, packetdata.buflen);

  //listview1.Items.BeginUpdate;
  try
    li:=listview1.Items.Add;
    li.caption:='S';

    if x.sendto then li.caption:=li.caption+' To';
    li.SubItems.Add(inttostr(x.socket));
    li.SubItems.Add(inttostr(x.bufsize^));

    getips(x.socket,fromaddress,toaddress);
    li.SubItems.Add(fromaddress);
    if x.sendto then
      li.SubItems.Add(format('%d.%d.%d.%d:%d',[byte(x.address.sin_addr.S_un_b.s_b1),byte(x.address.sin_addr.S_un_b.s_b2), byte(x.address.sin_addr.S_un_b.s_b3), byte(x.address.sin_addr.S_un_b.s_b4), ntohs(x.address.sin_port)]))
    else
      li.SubItems.Add(toaddress);
    
    i:=sizeof(pi);
    getsockopt(x.socket,SOL_SOCKET, SO_PROTOCOL_INFO, @pi, i);
    if i>0 then
    begin
      if pi.iSocketType=SOCK_STREAM then
        li.SubItems.Add('TCP')
      else
        li.SubItems.Add('UDP');
    end else li.SubItems.Add('...');


    li.Data:=packetdata;
  except
    //listview1.Items.EndUpdate;
  end;
end;

procedure Tmainform.FormCreate(Sender: TObject);
var x: pchar;
    c: integer;
    i: integer;
begin

  frmfilter:=tfrmfilter.Create(self);
  hexeditor:=thexeditor.create(self);
  hexeditor.Align:=alClient;
  hexeditor.Parent:=panel1;
{
  c:=1;
  getmem(x,c*6);
  for i:=0 to c-1 do
    copymemory(@x[i*6],pchar('test12'),6);

  hexeditor.setbuffer(x,c*6);}
end;


type TStartupThread=class(Tthread)
  public
    procedure execute; override;
end;

procedure TStartupThread.execute;
begin
  mainform:=tmainform.Create(nil);
  mainform.ShowModal;
  freeonterminate:=true;
end;


procedure Tmainform.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var packetdata: ^TPacketdata;
begin
  if listview1.ItemIndex<>-1 then
  begin
    packetdata:=listview1.Items[listview1.ItemIndex].Data;
    hexeditor.setbuffer(packetdata.buffer,packetdata.buflen);

    edtSocket.Text:=inttostr(packetdata.socket);
    edtSize.Text:=inttostr(packetdata.buflen);
  end;
end;

procedure Tmainform.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  canclose:=false;
end;

procedure Tmainform.Button3Click(Sender: TObject);
begin
  hexeditor.setSize(strtoint(edtSize.Text));
end;

procedure Tmainform.Button1Click(Sender: TObject);
var socket: integer;
    x: WSABUF;
    bs: dword;
    i: integer;
    t: integer;
begin
  socket:=strtoint(edtSocket.text);

  x.len:=strtoint(edtSize.text);
  x.buf:=hexeditor.getBuffer;

  try
    t:=strtoint(trim(edtTimes.text));
  except
    raise exception.Create('How many times is '+trim(edtTimes.text));
  end;

  while t>0 do
  begin
    ws2WSAsendorig(socket,@x,1,bs,0,nil,nil);
    dec(t);
  end;
end;

procedure Tmainform.Newfilter1Click(Sender: TObject);
begin
  frmfilter.Showmodal;
end;

initialization
  TStartupThread.create(false);

end.
