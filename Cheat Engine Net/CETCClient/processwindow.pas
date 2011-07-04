unit processwindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TProcesWindow = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    Button1: TButton;
    btnProcessList: TButton;
    btnWindowList: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnProcessListClick(Sender: TObject);
    procedure btnWindowListClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    opening: string;
    procedure SetProcessWindowState(x: boolean);
  end;

var
  ProcesWindow: TProcesWindow;

implementation

uses CEClient, Unit2;

{$R *.DFM}

procedure TProceswindow.SetProcessWindowState(x: boolean);
begin
  proceswindow.Button1.Enabled:=x;
  proceswindow.Button4.Enabled:=x;
  proceswindow.BtnProcessList.Enabled:=x;
  proceswindow.BtnWindowList.Enabled:=x;
end;

procedure TProcesWindow.Button1Click(Sender: TObject);
var processid: Dword;
    I: Integer;
begin
  if button1.Enabled then
  begin
    with mainform do
    begin
      newscan.enabled:=false;
      nextscanbutton.enabled:=false;
      scantext.enabled:=false;
      scanvalue.enabled:=false;
      label4.enabled:=false;
      scantype.enabled:=false;
      label8.enabled:=false;
      vartype.enabled:=false;
    end;

    if listbox1.ItemIndex<>-1 then
    begin
      mainform.opening:=listbox1.items[listbox1.itemindex];
      mainform.processlabel.caption:='Opening '+opening;

      output[0]:=2;
      val('$'+listbox1.Items[listbox1.itemindex],processid,i);
      pdword(@output[1])^:=processid;
      SendBuf(5);
    end;

     modalresult:=mrok;
  end else showmessage('please wait while the list is transmitted');
end;

procedure TProcesWindow.FormShow(Sender: TObject);
var x: byte;
begin
  listbox1.Clear;
  output[0]:=0;
  SendBuf(1);

//  connectform.waitforcommand(SC_StopProcessList);
//  connectform.HandleCommand(sc_stopprocesslist);

  //connectform.IdTCPClient1.ReadBuffer(x,1);
 // if x=2 then showmessage('yes') else showmessage('no');
end;

procedure TProcesWindow.btnProcessListClick(Sender: TObject);
begin
  SetProcessWindowState(false);

  listbox1.Clear;
  output[0]:=0;
  SendBuf(1);
//  connectform.waitforcommand(SC_StopProcessList);
//  connectform.HandleCommand(sc_stopprocesslist);
end;

procedure TProcesWindow.btnWindowListClick(Sender: TObject);
begin
  SetProcessWindowState(false);

  listbox1.Clear;
  output[0]:=1;
  SendBuf(1);
//  connectform.waitforcommand(SC_StopProcessList);
//  connectform.HandleCommand(sc_stopprocesslist);

end;

procedure TProcesWindow.ListBox1DblClick(Sender: TObject);
begin
  button1.click;
end;

procedure TProcesWindow.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
  proceswindow:=nil;
end;

end.
