unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Sockets, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, idwinsock2;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    Button2: TButton;
    TcpClient1: TTcpClient;
    Button3: TButton;
    IdTCPClient1: TIdTCPClient;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var x: thandle;
begin
  x:=loadlibrary('..\cepe.dll');
  if x=0 then showmessage('fuck');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if tcpclient1.Connected then
  begin
    tcpclient1.Disconnect;
  end
  else
  begin
    tcpclient1.RemoteHost:=edit1.text;
    tcpclient1.RemotePort:=edit2.text;
    tcpclient1.Connect;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var x: pchar;
    i: integer;
begin
  getmem(x,100+1);

  memo1.Lines.Clear;


  repeat
    tcpclient1.WaitForData(0);
    i:=tcpclient1.ReceiveBuf(x^,100);

    if i>0 then
    begin
      x[i]:=#0;
      memo1.Text:=memo1.Text+x;
    end; // else showmessage('wait:'+inttostr(i));

  until i=0;
  freemem(x);

end;

procedure TForm1.Button2Click(Sender: TObject);
begin


  tcpclient1.SendBuf(memo1.Lines.GetText^,memo1.gettextlen);
end;

procedure TForm1.Button4Click(Sender: TObject);
var x: integer;
    y: PSockAddr;
    l: integer;
    i: integer;
    fromaddress: string;
    toaddress: string;
begin
  x:=tcpclient1.Handle;
  l:=sizeof(TSockAddrIn);
  getmem(y,l);
  ZeroMemory(y,l);

  i:=getsockname(x,y,l);

  if i=0 then
  begin
    beep;
    y.sin_family:=0;
    fromaddress:=format('%d.%d.%d.%d',[y.sin_addr.S_un_b.s_b1,y.sin_addr.S_un_b.s_b2, y.sin_addr.S_un_b.s_b3, y.sin_addr.S_un_b.s_b4]);

    l:=sizeof(TSockAddrIn);
    i:=getpeername(x,y,l);
    if i=0 then
    begin
      toaddress:=format('%d.%d.%d.%d',[y.sin_addr.S_un_b.s_b1,y.sin_addr.S_un_b.s_b2, y.sin_addr.S_un_b.s_b3, y.sin_addr.S_un_b.s_b4]);
      showmessage(fromaddress+' - '+toaddress);
    end;
  end;

 
end;

end.
