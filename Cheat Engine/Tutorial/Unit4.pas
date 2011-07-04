unit Unit4;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources;

type
  TForm4 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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

initialization
  {$i Unit4.lrs}
  {$i Unit4.lrs}

end.
