unit formScanningUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TFormScanning = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btnCancel: TButton;
    Button1: TButton;

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    closeme: boolean;
  end;

var
  FormScanning: TFormScanning;

implementation

uses CEClient;

{$R *.dfm}

procedure TFormScanning.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  canclose:=closeme;
end;

procedure TFormScanning.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
  FormScanning:=nil;
end;

procedure TFormScanning.btnCancelClick(Sender: TObject);
begin
  closeme:=true;
  modalresult:=mrCancel;

  //send command to stop scanning

  output[0]:=CS_CancelScan;
  sendbuf(1);

  screen.Cursor:=crdefault;
end;

procedure TFormScanning.FormShow(Sender: TObject);
begin
  button1.SetFocus;
end;

end.
