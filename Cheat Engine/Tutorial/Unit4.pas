unit Unit4;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources;

type

  { TForm4 }

  TForm4 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Label3: TLabel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation


procedure TForm4.Button1Click(Sender: TObject);
begin
  application.Terminate;
end;

procedure TForm4.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  application.Terminate;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  font.size:=12;
end;

procedure TForm4.FormShow(Sender: TObject);
begin
  label4.Font.Color:=clBlue;
  label4.Font.size:=12;
  label4.Font.Style:=[fsUnderline];
end;

procedure TForm4.Label4Click(Sender: TObject);
begin
  ShellExecute(0, PChar('open'), PChar('http://forum.cheatengine.org/'),PChar(''), PChar(''), SW_MAXIMIZE);
end;

initialization
  {$i Unit4.lrs}
  {$i Unit4.lrs}

end.
